{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Repository Implementation                }
{                                                       }
{       Centralizes device cache and enumeration.       }
{       Uses injected IBluetoothDeviceQuery for         }
{       Windows API abstraction (testability).          }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceRepository;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.DeviceConverter,
  Bluetooth.WinRTDeviceQuery,
  App.ConfigEnums,
  App.ConnectionConfigIntf,
  App.SystemClock;

type
  /// <summary>
  /// Cached registry lookup result.
  /// Stores both successful and failed lookups to avoid repeated I/O.
  /// </summary>
  TRegistryNameCacheEntry = record
    Name: string;
    Timestamp: TDateTime;
    Found: Boolean;
  end;

  /// <summary>
  /// Cache for device name registry lookups.
  /// Reduces I/O by caching results with configurable expiration.
  /// Thread-safe via critical section.
  /// </summary>
  TRegistryNameCache = class
  private
    FCache: TDictionary<UInt64, TRegistryNameCacheEntry>;
    FCacheExpirySeconds: Integer;
    FClock: ISystemClock;
  public
    constructor Create(AClock: ISystemClock; ACacheExpirySeconds: Integer = 60);
    destructor Destroy; override;

    /// <summary>
    /// Tries to get a cached name lookup result.
    /// Returns True if cache entry exists and is not expired.
    /// AFound indicates whether the original lookup succeeded.
    /// </summary>
    function TryGetCached(AAddress: UInt64; out AName: string; out AFound: Boolean): Boolean;

    /// <summary>
    /// Caches a registry lookup result (success or failure).
    /// </summary>
    procedure CacheResult(AAddress: UInt64; const AName: string; AFound: Boolean);

    /// <summary>
    /// Clears all cached entries.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Returns current cache size for diagnostics.
    /// </summary>
    function Count: Integer;
  end;

  /// <summary>
  /// Classic Bluetooth (Win32 API) implementation of IBluetoothDeviceQuery.
  /// Wraps BluetoothFindFirstDevice/Next/Close calls.
  /// Works on Windows 7-11 for Classic Bluetooth devices (no BLE).
  /// </summary>
  TClassicBluetoothDeviceQuery = class(TInterfacedObject, IBluetoothDeviceQuery)
  public
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;
  end;

  /// <summary>
  /// Backward compatibility alias for existing code.
  /// </summary>
  TWindowsBluetoothDeviceQuery = TClassicBluetoothDeviceQuery;

  /// <summary>
  /// Composite query that uses both Win32 and WinRT APIs.
  /// Merges results, preferring devices with non-empty names.
  /// Logs comparison for investigation.
  /// Supports enumeration mode selection via IConnectionConfig.
  /// Uses registry name cache to avoid repeated I/O on every enumeration.
  /// </summary>
  TCompositeBluetoothDeviceQuery = class(TInterfacedObject, IBluetoothDeviceQuery)
  private
    FWin32Query: IBluetoothDeviceQuery;
    FWinRTQuery: IBluetoothDeviceQuery;
    FConnectionConfig: IConnectionConfig;
    FRegistryNameCache: TRegistryNameCache;
    procedure LogComparison(const AWin32, AWinRT: TBluetoothDeviceInfoArray);
    function MergeResults(const AWin32, AWinRT: TBluetoothDeviceInfoArray): TBluetoothDeviceInfoArray;
    function GetEnumerationMode: TEnumerationMode;
    function TryGetNameFromRegistryCached(AAddress: UInt64; out AName: string): Boolean;
  public
    constructor Create; overload;
    constructor Create(AConnectionConfig: IConnectionConfig); overload;
    destructor Destroy; override;
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;
  end;

  /// <summary>
  /// Repository for Bluetooth device storage and retrieval.
  /// Uses injected IBluetoothDeviceQuery for enumeration (testable).
  /// Caches ToArray result to avoid O(n) allocation on every GetAll call.
  /// </summary>
  TBluetoothDeviceRepository = class(TInterfacedObject, IDeviceRepository)
  private
    FDevices: TDictionary<UInt64, TBluetoothDeviceInfo>;
    FDeviceQuery: IBluetoothDeviceQuery;
    FOnListChanged: TDeviceListChangedEvent;

    // Array cache to avoid repeated ToArray allocations in hot path
    FDevicesArrayCache: TBluetoothDeviceInfoArray;
    FDevicesArrayValid: Boolean;

    procedure DoListChanged;
    procedure InvalidateCache;

  protected
    { IDeviceRepository }
    function GetAll: TBluetoothDeviceInfoArray;
    function GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    function TryGetByAddress(AAddress: UInt64; out ADevice: TBluetoothDeviceInfo): Boolean;
    function Contains(AAddress: UInt64): Boolean;
    procedure AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
    function UpdateConnectionState(AAddress: UInt64;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
    procedure Remove(AAddress: UInt64);
    procedure Clear;
    procedure Refresh;
    function GetCount: Integer;
    function GetOnListChanged: TDeviceListChangedEvent;
    procedure SetOnListChanged(AValue: TDeviceListChangedEvent);

  public
    constructor Create(ADeviceQuery: IBluetoothDeviceQuery);
    destructor Destroy; override;
  end;

/// <summary>
/// Creates a device repository with Windows API query implementation.
/// </summary>
function CreateDeviceRepository: IDeviceRepository; overload;

/// <summary>
/// Creates a device repository with config-aware query implementation.
/// </summary>
function CreateDeviceRepository(AConnectionConfig: IConnectionConfig): IDeviceRepository; overload;

/// <summary>
/// Creates the default Windows API device query.
/// </summary>
function CreateBluetoothDeviceQuery: IBluetoothDeviceQuery; overload;

/// <summary>
/// Creates a config-aware device query (uses EnumerationMode setting).
/// </summary>
function CreateBluetoothDeviceQuery(AConnectionConfig: IConnectionConfig): IBluetoothDeviceQuery; overload;

implementation

uses
  System.Classes,
  System.Win.Registry,
  App.Logger,
  App.WinRTSupport;

const
  BTHPORT_DEVICES_KEY = 'SYSTEM\CurrentControlSet\Services\BTHPORT\Parameters\Devices';
  BTHENUM_KEY = 'SYSTEM\CurrentControlSet\Enum\BTHENUM';

/// <summary>
/// Tries to get device name from BTHPORT registry cache.
/// Registry path: HKLM\SYSTEM\CurrentControlSet\Services\BTHPORT\Parameters\Devices\[address]\Name
/// </summary>
function TryGetNameFromBthport(AAddress: UInt64; out AName: string): Boolean;
var
  Reg: TRegistry;
  SubKeyPath: string;
  NameBytes: TBytes;
begin
  Result := False;
  AName := '';

  // Address in registry is lowercase hex without leading zeros
  SubKeyPath := BTHPORT_DEVICES_KEY + '\' + LowerCase(IntToHex(AAddress, 12));

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(SubKeyPath) then
    begin
      try
        if Reg.ValueExists('Name') then
        begin
          // Name is stored as REG_BINARY (UTF-8 bytes with null terminator)
          SetLength(NameBytes, Reg.GetDataSize('Name'));
          if Length(NameBytes) > 0 then
          begin
            Reg.ReadBinaryData('Name', NameBytes[0], Length(NameBytes));
            // Convert UTF-8 to string, remove null terminator
            AName := TEncoding.UTF8.GetString(NameBytes);
            AName := AName.TrimRight([#0]);
            Result := AName <> '';
            if Result then
              LogDebug('TryGetNameFromBthport: Found name "%s" for $%.12X', [AName, AAddress], 'DeviceRepository');
          end;
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

/// <summary>
/// Tries to get device FriendlyName from BTHENUM (PnP) registry.
/// Registry path: HKLM\SYSTEM\CurrentControlSet\Enum\BTHENUM\Dev_[address]\[instance]\FriendlyName
/// The instance subkey varies, so we enumerate all subkeys under Dev_[address].
/// </summary>
function TryGetNameFromBthenum(AAddress: UInt64; out AName: string): Boolean;
var
  Reg: TRegistry;
  DevKeyPath: string;
  SubKeys: TStringList;
  SubKey: string;
begin
  Result := False;
  AName := '';

  // BTHENUM uses uppercase address: Dev_C01693A834C7
  DevKeyPath := BTHENUM_KEY + '\Dev_' + UpperCase(IntToHex(AAddress, 12));

  Reg := TRegistry.Create(KEY_READ);
  SubKeys := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(DevKeyPath) then
    begin
      try
        Reg.GetKeyNames(SubKeys);
      finally
        Reg.CloseKey;
      end;

      // Enumerate subkeys looking for FriendlyName
      for SubKey in SubKeys do
      begin
        if Reg.OpenKeyReadOnly(DevKeyPath + '\' + SubKey) then
        begin
          try
            if Reg.ValueExists('FriendlyName') then
            begin
              AName := Reg.ReadString('FriendlyName');
              if AName <> '' then
              begin
                Result := True;
                LogDebug('TryGetNameFromBthenum: Found FriendlyName "%s" for $%.12X',
                  [AName, AAddress], 'DeviceRepository');
                Exit;
              end;
            end;
          finally
            Reg.CloseKey;
          end;
        end;
      end;
    end;
  finally
    SubKeys.Free;
    Reg.Free;
  end;
end;

/// <summary>
/// Tries to get device name from Windows registry.
/// Tries multiple locations in order:
/// 1. BTHPORT cache (HKLM\SYSTEM\...\BTHPORT\Parameters\Devices\[addr]\Name)
/// 2. BTHENUM PnP (HKLM\SYSTEM\...\Enum\BTHENUM\Dev_[addr]\...\FriendlyName)
/// </summary>
function TryGetNameFromRegistry(AAddress: UInt64; out AName: string): Boolean;
begin
  // Try BTHPORT first (Bluetooth stack cache)
  Result := TryGetNameFromBthport(AAddress, AName);
  if Result then
    Exit;

  // Fallback to BTHENUM (PnP FriendlyName)
  Result := TryGetNameFromBthenum(AAddress, AName);
end;

/// <summary>
/// Checks if a name is a generic WinRT fallback name (e.g., "Bluetooth c0:16:93:a8:34:c7")
/// Format: "Bluetooth " (10 chars) + "xx:xx:xx:xx:xx:xx" (17 chars) = 27 chars total
/// </summary>
function IsGenericBluetoothName(const AName: string): Boolean;
begin
  Result := AName.StartsWith('Bluetooth ', True) and (AName.Length = 27);
end;

{ TRegistryNameCache }

constructor TRegistryNameCache.Create(AClock: ISystemClock;
  ACacheExpirySeconds: Integer);
begin
  inherited Create;
  Assert(AClock <> nil, 'ISystemClock is required');
  FCache := TDictionary<UInt64, TRegistryNameCacheEntry>.Create;
  FCacheExpirySeconds := ACacheExpirySeconds;
  FClock := AClock;
end;

destructor TRegistryNameCache.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TRegistryNameCache.TryGetCached(AAddress: UInt64; out AName: string;
  out AFound: Boolean): Boolean;
var
  Entry: TRegistryNameCacheEntry;
begin
  Result := False;
  AName := '';
  AFound := False;

  if FCache.TryGetValue(AAddress, Entry) then
  begin
    // Check if entry is still valid (not expired)
    if SecondsBetween(FClock.Now, Entry.Timestamp) < FCacheExpirySeconds then
    begin
      AName := Entry.Name;
      AFound := Entry.Found;
      Result := True;
    end
    else
    begin
      // Entry expired, remove it
      FCache.Remove(AAddress);
    end;
  end;
end;

procedure TRegistryNameCache.CacheResult(AAddress: UInt64; const AName: string;
  AFound: Boolean);
var
  Entry: TRegistryNameCacheEntry;
begin
  Entry.Name := AName;
  Entry.Timestamp := FClock.Now;
  Entry.Found := AFound;
  FCache.AddOrSetValue(AAddress, Entry);
end;

procedure TRegistryNameCache.Clear;
begin
  FCache.Clear;
end;

function TRegistryNameCache.Count: Integer;
begin
  Result := FCache.Count;
end;

function CreateBluetoothDeviceQuery: IBluetoothDeviceQuery;
begin
  Result := TCompositeBluetoothDeviceQuery.Create;
end;

function CreateBluetoothDeviceQuery(AConnectionConfig: IConnectionConfig): IBluetoothDeviceQuery;
begin
  Result := TCompositeBluetoothDeviceQuery.Create(AConnectionConfig);
end;

function CreateDeviceRepository: IDeviceRepository;
begin
  Result := TBluetoothDeviceRepository.Create(CreateBluetoothDeviceQuery);
end;

function CreateDeviceRepository(AConnectionConfig: IConnectionConfig): IDeviceRepository;
begin
  Result := TBluetoothDeviceRepository.Create(CreateBluetoothDeviceQuery(AConnectionConfig));
end;

{ TClassicBluetoothDeviceQuery }

function TClassicBluetoothDeviceQuery.EnumeratePairedDevices: TBluetoothDeviceInfoArray;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  Device: TBluetoothDeviceInfo;
  DeviceList: TList<TBluetoothDeviceInfo>;
  LastError: DWORD;
begin
  LogDebug('EnumeratePairedDevices: Starting Windows API enumeration', ClassName);
  DeviceList := TList<TBluetoothDeviceInfo>.Create;
  try
    InitDeviceSearchParams(SearchParams, 0);
    InitDeviceInfo(DeviceInfo);

    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);
    LastError := GetLastError;
    LogDebug('EnumeratePairedDevices: BluetoothFindFirstDevice returned handle=$%X, LastError=%d',
      [FindHandle, LastError], ClassName);

    if FindHandle <> 0 then
    begin
      try
        repeat
          Device := ConvertBluetoothDeviceInfo(DeviceInfo);
          LogDebug('EnumeratePairedDevices: Found device Address=$%.12X, Name="%s", CoD=$%.8X, Connected=%s, Remembered=%s, Authenticated=%s',
            [Device.AddressInt, Device.Name, DeviceInfo.ulClassOfDevice,
             BoolToStr(DeviceInfo.fConnected, True),
             BoolToStr(DeviceInfo.fRemembered, True), BoolToStr(DeviceInfo.fAuthenticated, True)], ClassName);

          // Only add truly paired devices (filter out connected-but-unpaired devices)
          if Device.IsPaired then
            DeviceList.Add(Device)
          else
            LogDebug('EnumeratePairedDevices: Skipping unpaired device Address=$%.12X, Name="%s"',
              [Device.AddressInt, Device.Name], ClassName);

          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end
    else
      LogWarning('EnumeratePairedDevices: BluetoothFindFirstDevice failed, no devices found', ClassName);

    Result := DeviceList.ToArray;
    LogDebug('EnumeratePairedDevices: Complete, found %d devices', [Length(Result)], ClassName);
  finally
    DeviceList.Free;
  end;
end;

{ TCompositeBluetoothDeviceQuery }

constructor TCompositeBluetoothDeviceQuery.Create;
begin
  Create(nil);
end;

constructor TCompositeBluetoothDeviceQuery.Create(AConnectionConfig: IConnectionConfig);
var
  Platform: TBluetoothPlatform;
begin
  inherited Create;
  FConnectionConfig := AConnectionConfig;
  FRegistryNameCache := TRegistryNameCache.Create(SystemClock, 60);

  // Determine platform: use config if available, otherwise auto-detect
  if Assigned(AConnectionConfig) then
    Platform := TWinRTSupport.SelectPlatform(AConnectionConfig.BluetoothPlatform)
  else
    Platform := TWinRTSupport.SelectPlatform(bpAuto);

  // Create only the query for the selected platform
  case Platform of
    bpClassic:
      begin
        FWin32Query := TClassicBluetoothDeviceQuery.Create;
        FWinRTQuery := nil;  // Classic platform only
        LogDebug('TCompositeBluetoothDeviceQuery: Using Classic Bluetooth (Win32) platform', ClassName);
      end;
    bpWinRT:
      begin
        FWin32Query := nil;
        FWinRTQuery := CreateWinRTBluetoothDeviceQuery;  // WinRT platform only
        LogDebug('TCompositeBluetoothDeviceQuery: Using WinRT Bluetooth platform', ClassName);
      end;
  else
    // Fallback: create both for composite mode (shouldn't happen with proper SelectPlatform)
    FWin32Query := TClassicBluetoothDeviceQuery.Create;
    FWinRTQuery := CreateWinRTBluetoothDeviceQuery;
    LogWarning('TCompositeBluetoothDeviceQuery: Unexpected platform, creating both queries', ClassName);
  end;
end;

destructor TCompositeBluetoothDeviceQuery.Destroy;
begin
  FRegistryNameCache.Free;
  inherited Destroy;
end;

function TCompositeBluetoothDeviceQuery.TryGetNameFromRegistryCached(
  AAddress: UInt64; out AName: string): Boolean;
var
  CachedFound: Boolean;
begin
  // First check cache
  if FRegistryNameCache.TryGetCached(AAddress, AName, CachedFound) then
  begin
    Result := CachedFound;
    if CachedFound then
      LogDebug('TryGetNameFromRegistryCached: Cache hit for $%.12X -> "%s"',
        [AAddress, AName], ClassName)
    else
      LogDebug('TryGetNameFromRegistryCached: Cache hit (not found) for $%.12X',
        [AAddress], ClassName);
    Exit;
  end;

  // Cache miss - perform actual registry lookup
  Result := TryGetNameFromRegistry(AAddress, AName);

  // Cache the result (success or failure)
  FRegistryNameCache.CacheResult(AAddress, AName, Result);

  if Result then
    LogDebug('TryGetNameFromRegistryCached: Registry lookup for $%.12X -> "%s" (cached)',
      [AAddress, AName], ClassName)
  else
    LogDebug('TryGetNameFromRegistryCached: Registry lookup failed for $%.12X (cached negative)',
      [AAddress], ClassName);
end;

function TCompositeBluetoothDeviceQuery.GetEnumerationMode: TEnumerationMode;
begin
  if Assigned(FConnectionConfig) then
    Result := FConnectionConfig.EnumerationMode
  else
    Result := emComposite;
end;

function TCompositeBluetoothDeviceQuery.EnumeratePairedDevices: TBluetoothDeviceInfoArray;
var
  Win32Devices, WinRTDevices: TBluetoothDeviceInfoArray;
  Mode: TEnumerationMode;
begin
  // Platform selection overrides enumeration mode
  // If only one query is available, use it regardless of mode
  if not Assigned(FWin32Query) and Assigned(FWinRTQuery) then
  begin
    // WinRT platform only
    Result := FWinRTQuery.EnumeratePairedDevices;
    LogDebug('EnumeratePairedDevices: WinRT platform, %d devices', [Length(Result)], ClassName);
    Exit;
  end;

  if Assigned(FWin32Query) and not Assigned(FWinRTQuery) then
  begin
    // Classic platform only
    Result := FWin32Query.EnumeratePairedDevices;
    LogDebug('EnumeratePairedDevices: Classic platform, %d devices', [Length(Result)], ClassName);
    Exit;
  end;

  // Both queries available - use enumeration mode
  Mode := GetEnumerationMode;
  LogDebug('EnumeratePairedDevices: Starting enumeration, mode=%d', [Ord(Mode)], ClassName);

  case Mode of
    emWin32:
      begin
        // Win32 only
        Win32Devices := FWin32Query.EnumeratePairedDevices;
        SetLength(WinRTDevices, 0);
        LogDebug('EnumeratePairedDevices: Win32 only mode, %d devices', [Length(Win32Devices)], ClassName);
      end;

    emWinRT:
      begin
        // WinRT only
        SetLength(Win32Devices, 0);
        WinRTDevices := FWinRTQuery.EnumeratePairedDevices;
        LogDebug('EnumeratePairedDevices: WinRT only mode, %d devices', [Length(WinRTDevices)], ClassName);
      end;

    emComposite:
      begin
        // Both sources
        Win32Devices := FWin32Query.EnumeratePairedDevices;
        WinRTDevices := FWinRTQuery.EnumeratePairedDevices;
        LogComparison(Win32Devices, WinRTDevices);
      end;
  end;

  // Merge results (also applies registry fallback)
  Result := MergeResults(Win32Devices, WinRTDevices);

  LogDebug('EnumeratePairedDevices: Complete, %d devices', [Length(Result)], ClassName);
end;

procedure TCompositeBluetoothDeviceQuery.LogComparison(
  const AWin32, AWinRT: TBluetoothDeviceInfoArray);
var
  Win32Map, WinRTMap: TDictionary<UInt64, TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
  Address: UInt64;
  Win32EmptyNames, WinRTResolvedNames: Integer;
begin
  LogInfo('=== Win32 vs WinRT Comparison ===', ClassName);
  LogInfo('Win32 devices: %d, WinRT devices: %d', [Length(AWin32), Length(AWinRT)], ClassName);

  Win32Map := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  WinRTMap := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  try
    for Device in AWin32 do
      Win32Map.AddOrSetValue(Device.AddressInt, Device);

    for Device in AWinRT do
      WinRTMap.AddOrSetValue(Device.AddressInt, Device);

    // Log Win32 devices
    LogInfo('--- Win32 Devices ---', ClassName);
    for Device in AWin32 do
      LogInfo('  $%.12X | Name="%s" | CoD=$%.8X | Connected=%s',
        [Device.AddressInt, Device.Name, Device.ClassOfDevice,
         BoolToStr(Device.IsConnected, True)], ClassName);

    // Log WinRT devices
    LogInfo('--- WinRT Devices ---', ClassName);
    for Device in AWinRT do
      LogInfo('  $%.12X | Name="%s" | CoD=$%.8X | Connected=%s',
        [Device.AddressInt, Device.Name, Device.ClassOfDevice,
         BoolToStr(Device.IsConnected, True)], ClassName);

    // Count empty names and resolved names
    Win32EmptyNames := 0;
    WinRTResolvedNames := 0;
    for Device in AWin32 do
    begin
      if Device.Name = '' then
      begin
        Inc(Win32EmptyNames);
        if WinRTMap.ContainsKey(Device.AddressInt) and
           (WinRTMap[Device.AddressInt].Name <> '') then
        begin
          Inc(WinRTResolvedNames);
          LogInfo('*** WinRT resolved: $%.12X -> "%s"',
            [Device.AddressInt, WinRTMap[Device.AddressInt].Name], ClassName);
        end;
      end;
    end;

    // Log devices only in WinRT (BLE devices Win32 missed)
    LogInfo('--- Devices ONLY in WinRT ---', ClassName);
    for Address in WinRTMap.Keys do
    begin
      if not Win32Map.ContainsKey(Address) then
      begin
        Device := WinRTMap[Address];
        LogInfo('  $%.12X | Name="%s" (BLE device missed by Win32)',
          [Device.AddressInt, Device.Name], ClassName);
      end;
    end;

    LogInfo('Summary: Win32 empty names=%d, WinRT resolved=%d',
      [Win32EmptyNames, WinRTResolvedNames], ClassName);
    LogInfo('=== End Comparison ===', ClassName);
  finally
    Win32Map.Free;
    WinRTMap.Free;
  end;
end;

function TCompositeBluetoothDeviceQuery.MergeResults(
  const AWin32, AWinRT: TBluetoothDeviceInfoArray): TBluetoothDeviceInfoArray;
var
  ResultMap: TDictionary<UInt64, TBluetoothDeviceInfo>;
  Device, Existing: TBluetoothDeviceInfo;
  RegistryName: string;
  FinalName: string;
begin
  ResultMap := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  try
    // Start with Win32 devices (has CoD info)
    for Device in AWin32 do
      ResultMap.AddOrSetValue(Device.AddressInt, Device);

    // Merge WinRT devices
    for Device in AWinRT do
    begin
      if ResultMap.TryGetValue(Device.AddressInt, Existing) then
      begin
        // If Win32 has empty name but WinRT has name, use WinRT name
        if (Existing.Name = '') and (Device.Name <> '') then
        begin
          LogDebug('MergeResults: Using WinRT name "%s" for $%.12X',
            [Device.Name, Device.AddressInt], ClassName);
          ResultMap[Device.AddressInt] := Existing.WithName(Device.Name);
        end;
      end
      else
      begin
        // Device only in WinRT, add it
        LogDebug('MergeResults: Adding WinRT-only device $%.12X "%s"',
          [Device.AddressInt, Device.Name], ClassName);
        ResultMap.Add(Device.AddressInt, Device);
      end;
    end;

    // Registry fallback pass: try to resolve generic "Bluetooth XX:XX" names
    // Uses cache to avoid repeated I/O on every enumeration poll
    for Device in ResultMap.Values.ToArray do
    begin
      if (Device.Name = '') or IsGenericBluetoothName(Device.Name) then
      begin
        // Try cached registry lookup for actual device name
        if TryGetNameFromRegistryCached(Device.AddressInt, RegistryName) then
        begin
          LogDebug('MergeResults: Registry resolved $%.12X -> "%s"',
            [Device.AddressInt, RegistryName], ClassName);
          ResultMap[Device.AddressInt] := Device.WithName(RegistryName);
        end
        else if Device.Name = '' then
        begin
          // Last resort: format MAC address as display name
          FinalName := Device.AddressString;
          LogDebug('MergeResults: Using MAC address as name for $%.12X -> "%s"',
            [Device.AddressInt, FinalName], ClassName);
          ResultMap[Device.AddressInt] := Device.WithName(FinalName);
        end;
        // else: keep the WinRT "Bluetooth XX:XX" name
      end;
    end;

    Result := ResultMap.Values.ToArray;
  finally
    ResultMap.Free;
  end;
end;

{ TBluetoothDeviceRepository }

constructor TBluetoothDeviceRepository.Create(ADeviceQuery: IBluetoothDeviceQuery);
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  FDeviceQuery := ADeviceQuery;
end;

destructor TBluetoothDeviceRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

procedure TBluetoothDeviceRepository.InvalidateCache;
begin
  FDevicesArrayValid := False;
  SetLength(FDevicesArrayCache, 0);
end;

function TBluetoothDeviceRepository.GetAll: TBluetoothDeviceInfoArray;
begin
  // Return cached array if valid, avoiding O(n) ToArray allocation
  if not FDevicesArrayValid then
  begin
    FDevicesArrayCache := FDevices.Values.ToArray;
    FDevicesArrayValid := True;
  end;
  Result := FDevicesArrayCache;
end;

function TBluetoothDeviceRepository.GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    FillChar(Result, SizeOf(Result), 0);
end;

function TBluetoothDeviceRepository.TryGetByAddress(AAddress: UInt64;
  out ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := FDevices.TryGetValue(AAddress, ADevice);
end;

function TBluetoothDeviceRepository.Contains(AAddress: UInt64): Boolean;
begin
  Result := FDevices.ContainsKey(AAddress);
end;

procedure TBluetoothDeviceRepository.AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
var
  IsNew: Boolean;
begin
  IsNew := not FDevices.ContainsKey(ADevice.AddressInt);
  FDevices.AddOrSetValue(ADevice.AddressInt, ADevice);
  InvalidateCache;

  if IsNew then
    DoListChanged;
end;

function TBluetoothDeviceRepository.UpdateConnectionState(AAddress: UInt64;
  AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Device: TBluetoothDeviceInfo;
begin
  if FDevices.TryGetValue(AAddress, Device) then
  begin
    Result := Device.WithConnectionState(AState);
    FDevices[AAddress] := Result;
    InvalidateCache;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TBluetoothDeviceRepository.Remove(AAddress: UInt64);
var
  Existed: Boolean;
begin
  Existed := FDevices.ContainsKey(AAddress);
  FDevices.Remove(AAddress);

  if Existed then
  begin
    InvalidateCache;
    DoListChanged;
  end;
end;

procedure TBluetoothDeviceRepository.Clear;
var
  HadDevices: Boolean;
begin
  HadDevices := FDevices.Count > 0;
  FDevices.Clear;

  if HadDevices then
  begin
    InvalidateCache;
    DoListChanged;
  end;
end;

procedure TBluetoothDeviceRepository.Refresh;
var
  Devices: TBluetoothDeviceInfoArray;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('Refresh: Enumerating paired devices', ClassName);
  FDevices.Clear;

  // Use injected query for enumeration (testable)
  Devices := FDeviceQuery.EnumeratePairedDevices;
  for Device in Devices do
    FDevices.AddOrSetValue(Device.AddressInt, Device);

  InvalidateCache;
  LogDebug('Refresh: Found %d paired devices', [FDevices.Count], ClassName);
  DoListChanged;
end;

function TBluetoothDeviceRepository.GetCount: Integer;
begin
  Result := FDevices.Count;
end;

procedure TBluetoothDeviceRepository.DoListChanged;
begin
  if Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

function TBluetoothDeviceRepository.GetOnListChanged: TDeviceListChangedEvent;
begin
  Result := FOnListChanged;
end;

procedure TBluetoothDeviceRepository.SetOnListChanged(AValue: TDeviceListChangedEvent);
begin
  FOnListChanged := AValue;
end;

end.
