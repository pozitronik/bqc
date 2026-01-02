{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Configuration Repository                 }
{                                                       }
{*******************************************************}

/// <summary>
/// Handles per-device configuration storage and retrieval.
/// Separates device configuration from main application settings.
/// </summary>
unit App.DeviceConfigRepository;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.DateUtils,
  System.Generics.Collections,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes;

type
  /// <summary>
  /// Repository for per-device configuration.
  /// Manages device-specific settings with INI persistence.
  /// Handles only storage/retrieval - effective value resolution is done by TAppConfig.
  /// Implements both IDeviceConfigStorage (domain) and IDeviceConfigPersistence (INI).
  /// </summary>
  TDeviceConfigRepository = class(TInterfacedObject,
    IDeviceConfigStorage,
    IDeviceConfigPersistence,
    IDeviceConfigRepository)
  private
    FDevices: TDictionary<UInt64, TDeviceConfig>;
    FModified: Boolean;
    /// <summary>
    /// Checks if a name is a generic Windows format ("Bluetooth XX:XX:XX:XX:XX:XX").
    /// Windows returns this when friendly name cache is missing for offline devices.
    /// </summary>
    function IsGenericWindowsName(const AName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceConfigRepository
    function GetConfig(AAddress: UInt64): TDeviceConfig;
    procedure SetConfig(const AConfig: TDeviceConfig);
    procedure Remove(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
    function GetAllAddresses: TArray<UInt64>;
    function GetAll: TArray<TDeviceConfig>;
    procedure LoadFrom(AIni: TCustomIniFile);
    procedure SaveTo(AIni: TCustomIniFile);
    function IsModified: Boolean;
    procedure ClearModified;
  end;

const
  // Device section prefix
  SEC_DEVICE_PREFIX = 'Device.';

  // Per-device INI keys
  KEY_NAME = 'Name';
  KEY_ALIAS = 'Alias';
  KEY_PINNED = 'Pinned';
  KEY_HIDDEN = 'Hidden';
  KEY_AUTO_CONNECT = 'AutoConnect';
  KEY_CONNECTION_TIMEOUT = 'ConnectionTimeout';
  KEY_CONNECTION_RETRY_COUNT = 'ConnectionRetryCount';
  KEY_NOTIFY_ON_CONNECT = 'NotifyOnConnect';
  KEY_NOTIFY_ON_DISCONNECT = 'NotifyOnDisconnect';
  KEY_NOTIFY_ON_CONNECT_FAILED = 'NotifyOnConnectFailed';
  KEY_NOTIFY_ON_AUTO_CONNECT = 'NotifyOnAutoConnect';
  KEY_DEVICE_TYPE_OVERRIDE = 'DeviceTypeOverride';
  KEY_LAST_SEEN = 'LastSeen';
  // Battery tray icon keys
  KEY_BATTERY_TRAY_ICON = 'BatteryTrayIcon';
  KEY_BATTERY_ICON_COLOR = 'BatteryIconColor';
  KEY_BATTERY_BACKGROUND_COLOR = 'BatteryBackgroundColor';
  KEY_BATTERY_SHOW_NUMERIC = 'BatteryShowNumeric';
  KEY_BATTERY_LOW_THRESHOLD = 'BatteryLowThreshold';
  KEY_BATTERY_NOTIFY_LOW = 'BatteryNotifyLow';
  KEY_BATTERY_NOTIFY_FULL = 'BatteryNotifyFull';
  KEY_SHOW_PROFILES = 'ShowProfiles';

/// <summary>
/// Creates a device configuration repository.
/// </summary>
function CreateDeviceConfigRepository: IDeviceConfigRepository;

implementation

uses
  App.Logger;

function CreateDeviceConfigRepository: IDeviceConfigRepository;
begin
  Result := TDeviceConfigRepository.Create;
end;

{ TDeviceConfigRepository }

constructor TDeviceConfigRepository.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TDeviceConfig>.Create;
  FModified := False;
end;

destructor TDeviceConfigRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

function TDeviceConfigRepository.GetConfig(AAddress: UInt64): TDeviceConfig;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TDeviceConfigRepository.SetConfig(const AConfig: TDeviceConfig);
begin
  FDevices.AddOrSetValue(AConfig.Address, AConfig);
  FModified := True;
end;

procedure TDeviceConfigRepository.Remove(AAddress: UInt64);
begin
  if FDevices.ContainsKey(AAddress) then
  begin
    FDevices.Remove(AAddress);
    FModified := True;
  end;
end;

function TDeviceConfigRepository.IsGenericWindowsName(const AName: string): Boolean;
const
  BLUETOOTH_PREFIX = 'Bluetooth ';
  MAC_LENGTH = 17; // XX:XX:XX:XX:XX:XX
var
  I: Integer;
  MacPart: string;
begin
  // Check if name starts with "Bluetooth " prefix
  if not AName.StartsWith(BLUETOOTH_PREFIX, True) then
    Exit(False);

  // Extract the MAC address part after "Bluetooth "
  MacPart := Copy(AName, Length(BLUETOOTH_PREFIX) + 1, MaxInt);

  // Verify MAC address format: exactly 17 characters (XX:XX:XX:XX:XX:XX)
  if Length(MacPart) <> MAC_LENGTH then
    Exit(False);

  // Verify pattern: hex:hex:hex:hex:hex:hex
  for I := 1 to MAC_LENGTH do
  begin
    case I mod 3 of
      1, 2: // Hex digit positions (1,2,4,5,7,8,10,11,13,14,16,17)
        if not CharInSet(MacPart[I], ['0'..'9', 'A'..'F', 'a'..'f']) then
          Exit(False);
      0: // Colon positions (3,6,9,12,15)
        if MacPart[I] <> ':' then
          Exit(False);
    end;
  end;

  Result := True;
end;

procedure TDeviceConfigRepository.RegisterDevice(AAddress: UInt64;
  const AName: string; ALastSeen: TDateTime);
var
  DeviceConfig: TDeviceConfig;
  IsNew: Boolean;
begin
  IsNew := not FDevices.TryGetValue(AAddress, DeviceConfig);

  if IsNew then
  begin
    // Create new device config with defaults
    DeviceConfig := TDeviceConfig.Default(AAddress);
    DeviceConfig.Name := AName;
    // Only set LastSeen if a valid value is provided (> 0)
    if ALastSeen > 0 then
      DeviceConfig.LastSeen := ALastSeen
    else
      DeviceConfig.LastSeen := 0;
    FDevices.Add(AAddress, DeviceConfig);
    FModified := True;
    LogDebug('RegisterDevice: New device registered: %s ($%.12X)', [AName, AAddress], ClassName);
  end
  else
  begin
    // Update existing device
    // Only update Name if non-empty and different
    // Skip update if new name is generic Windows format but we have a better cached name
    // Only update LastSeen if new value is greater (more recent) AND valid (> 0)
    if (AName <> '') and (DeviceConfig.Name <> AName) then
    begin
      // Don't overwrite good cached name with generic Windows name
      if IsGenericWindowsName(AName) and (DeviceConfig.Name <> '') and not IsGenericWindowsName(DeviceConfig.Name) then
      begin
        LogDebug('RegisterDevice: Skipping generic name "%s" for device $%.12X, keeping cached name "%s"',
          [AName, AAddress, DeviceConfig.Name], ClassName);
      end
      else
      begin
        DeviceConfig.Name := AName;
        FDevices[AAddress] := DeviceConfig;
        FModified := True;
        LogDebug('RegisterDevice: Updated name for device $%.12X: "%s" -> "%s"',
          [AAddress, DeviceConfig.Name, AName], ClassName);
      end;
    end;

    if (ALastSeen > 0) and (ALastSeen > DeviceConfig.LastSeen) then
    begin
      DeviceConfig.LastSeen := ALastSeen;
      FDevices[AAddress] := DeviceConfig;
      FModified := True;
    end;
  end;
end;

function TDeviceConfigRepository.GetAllAddresses: TArray<UInt64>;
begin
  Result := FDevices.Keys.ToArray;
end;

function TDeviceConfigRepository.GetAll: TArray<TDeviceConfig>;
begin
  Result := FDevices.Values.ToArray;
end;

function TDeviceConfigRepository.IsModified: Boolean;
begin
  Result := FModified;
end;

procedure TDeviceConfigRepository.ClearModified;
begin
  FModified := False;
end;

procedure TDeviceConfigRepository.LoadFrom(AIni: TCustomIniFile);
var
  Sections: TStringList;
  Section: string;
  AddressStr: string;
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  LastSeenStr: string;
begin
  FDevices.Clear;
  Sections := TStringList.Create;
  try
    AIni.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
      begin
        AddressStr := Section.Substring(Length(SEC_DEVICE_PREFIX));
        // Support both formats: 581862015DAE and 58:18:62:01:5D:AE
        AddressStr := StringReplace(AddressStr, ':', '', [rfReplaceAll]);
        if TryStrToUInt64('$' + AddressStr, Address) then
        begin
          DeviceConfig := TDeviceConfig.Default(Address);
          DeviceConfig.Name := AIni.ReadString(Section, KEY_NAME, '');
          DeviceConfig.Alias := AIni.ReadString(Section, KEY_ALIAS, '');
          DeviceConfig.Pinned := AIni.ReadBool(Section, KEY_PINNED, False);
          DeviceConfig.Hidden := AIni.ReadBool(Section, KEY_HIDDEN, False);
          DeviceConfig.AutoConnect := AIni.ReadBool(Section, KEY_AUTO_CONNECT, False);
          DeviceConfig.ConnectionTimeout := AIni.ReadInteger(Section, KEY_CONNECTION_TIMEOUT, -1);
          DeviceConfig.ConnectionRetryCount := AIni.ReadInteger(Section, KEY_CONNECTION_RETRY_COUNT, -1);
          // Per-device notification overrides (-1 = use global)
          DeviceConfig.Notifications.OnConnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT, -1);
          DeviceConfig.Notifications.OnDisconnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_DISCONNECT, -1);
          DeviceConfig.Notifications.OnConnectFailed := AIni.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT_FAILED, -1);
          DeviceConfig.Notifications.OnAutoConnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_AUTO_CONNECT, -1);
          // Per-device battery tray overrides (-1 = use global)
          DeviceConfig.BatteryTray.ShowTrayIcon := AIni.ReadInteger(Section, KEY_BATTERY_TRAY_ICON, -1);
          DeviceConfig.BatteryTray.IconColor := AIni.ReadInteger(Section, KEY_BATTERY_ICON_COLOR, -1);
          DeviceConfig.BatteryTray.BackgroundColor := AIni.ReadInteger(Section, KEY_BATTERY_BACKGROUND_COLOR, -1);
          DeviceConfig.BatteryTray.ShowNumericValue := AIni.ReadInteger(Section, KEY_BATTERY_SHOW_NUMERIC, -1);
          DeviceConfig.BatteryTray.LowBatteryThreshold := AIni.ReadInteger(Section, KEY_BATTERY_LOW_THRESHOLD, -1);
          DeviceConfig.BatteryTray.NotifyLowBattery := AIni.ReadInteger(Section, KEY_BATTERY_NOTIFY_LOW, -1);
          DeviceConfig.BatteryTray.NotifyFullyCharged := AIni.ReadInteger(Section, KEY_BATTERY_NOTIFY_FULL, -1);
          DeviceConfig.DeviceTypeOverride := AIni.ReadInteger(Section, KEY_DEVICE_TYPE_OVERRIDE, -1);
          DeviceConfig.ShowProfiles := AIni.ReadInteger(Section, KEY_SHOW_PROFILES, -1);
          // Parse LastSeen as ISO 8601 datetime string
          LastSeenStr := AIni.ReadString(Section, KEY_LAST_SEEN, '');
          if LastSeenStr <> '' then
          begin
            try
              DeviceConfig.LastSeen := ISO8601ToDate(LastSeenStr, False);
            except
              DeviceConfig.LastSeen := 0;
            end;
          end;
          FDevices.Add(Address, DeviceConfig);
        end;
      end;
    end;
  finally
    Sections.Free;
  end;
  FModified := False;
end;

procedure TDeviceConfigRepository.SaveTo(AIni: TCustomIniFile);
var
  Sections: TStringList;
  Section: string;
  Pair: TPair<UInt64, TDeviceConfig>;
  SectionName: string;
begin
  // First, remove all existing device sections
  Sections := TStringList.Create;
  try
    AIni.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
        AIni.EraseSection(Section);
    end;
  finally
    Sections.Free;
  end;

  // Write current device configurations
  for Pair in FDevices do
  begin
    SectionName := SEC_DEVICE_PREFIX + IntToHex(Pair.Key, 12);
    // Always save Name (original device name from Windows)
    AIni.WriteString(SectionName, KEY_NAME, Pair.Value.Name);
    AIni.WriteString(SectionName, KEY_ALIAS, Pair.Value.Alias);
    AIni.WriteBool(SectionName, KEY_PINNED, Pair.Value.Pinned);
    AIni.WriteBool(SectionName, KEY_HIDDEN, Pair.Value.Hidden);
    AIni.WriteBool(SectionName, KEY_AUTO_CONNECT, Pair.Value.AutoConnect);
    // Only save connection settings if they override defaults
    if Pair.Value.ConnectionTimeout >= 0 then
      AIni.WriteInteger(SectionName, KEY_CONNECTION_TIMEOUT, Pair.Value.ConnectionTimeout);
    if Pair.Value.ConnectionRetryCount >= 0 then
      AIni.WriteInteger(SectionName, KEY_CONNECTION_RETRY_COUNT, Pair.Value.ConnectionRetryCount);
    // Only save notification settings if they override globals
    if Pair.Value.Notifications.OnConnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT, Pair.Value.Notifications.OnConnect);
    if Pair.Value.Notifications.OnDisconnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_DISCONNECT, Pair.Value.Notifications.OnDisconnect);
    if Pair.Value.Notifications.OnConnectFailed >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT_FAILED, Pair.Value.Notifications.OnConnectFailed);
    if Pair.Value.Notifications.OnAutoConnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_AUTO_CONNECT, Pair.Value.Notifications.OnAutoConnect);
    // Only save battery tray settings if they override globals
    if Pair.Value.BatteryTray.ShowTrayIcon >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_TRAY_ICON, Pair.Value.BatteryTray.ShowTrayIcon);
    if Pair.Value.BatteryTray.IconColor >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_ICON_COLOR, Pair.Value.BatteryTray.IconColor);
    // Save if custom color (>= 0) or transparent (-2), skip if default (-1)
    if (Pair.Value.BatteryTray.BackgroundColor >= 0) or (Pair.Value.BatteryTray.BackgroundColor = -2) then
      AIni.WriteInteger(SectionName, KEY_BATTERY_BACKGROUND_COLOR, Pair.Value.BatteryTray.BackgroundColor);
    if Pair.Value.BatteryTray.ShowNumericValue >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_SHOW_NUMERIC, Pair.Value.BatteryTray.ShowNumericValue);
    if Pair.Value.BatteryTray.LowBatteryThreshold >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_LOW_THRESHOLD, Pair.Value.BatteryTray.LowBatteryThreshold);
    if Pair.Value.BatteryTray.NotifyLowBattery >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_NOTIFY_LOW, Pair.Value.BatteryTray.NotifyLowBattery);
    if Pair.Value.BatteryTray.NotifyFullyCharged >= 0 then
      AIni.WriteInteger(SectionName, KEY_BATTERY_NOTIFY_FULL, Pair.Value.BatteryTray.NotifyFullyCharged);
    // Only save DeviceTypeOverride if it's set (not auto-detect)
    if Pair.Value.DeviceTypeOverride >= 0 then
      AIni.WriteInteger(SectionName, KEY_DEVICE_TYPE_OVERRIDE, Pair.Value.DeviceTypeOverride);
    // Only save ShowProfiles if it's set (not using global)
    if Pair.Value.ShowProfiles >= 0 then
      AIni.WriteInteger(SectionName, KEY_SHOW_PROFILES, Pair.Value.ShowProfiles);
    // Save LastSeen as ISO 8601 datetime string
    if Pair.Value.LastSeen > 0 then
      AIni.WriteString(SectionName, KEY_LAST_SEEN, DateToISO8601(Pair.Value.LastSeen, False));
  end;
  FModified := False;
end;

end.
