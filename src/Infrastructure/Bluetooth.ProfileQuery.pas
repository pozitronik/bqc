{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Profile Query Implementation                    }
{                                                       }
{       Enumerates and queries Bluetooth profiles       }
{       for paired devices. Implements IProfileQuery    }
{       with optional caching to reduce Windows API     }
{       calls during device list updates.               }
{                                                       }
{*******************************************************}

unit Bluetooth.ProfileQuery;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  App.SystemClock;

type
  /// <summary>
  /// Cached profile entry with timestamp.
  /// </summary>
  TProfileCacheEntry = record
    ProfileInfo: TDeviceProfileInfo;
    CachedAt: TDateTime;
  end;

  /// <summary>
  /// Provides profile enumeration and query functionality.
  /// Implements IProfileQuery with optional caching to reduce
  /// Windows API calls when refreshing device lists.
  /// </summary>
  TProfileQuery = class(TInterfacedObject, IProfileQuery)
  private const
    LOG_SOURCE = 'ProfileQuery';
    MAX_SERVICES = 32;
    // Cache duration: profile info rarely changes, cache for 30 seconds
    CACHE_DURATION_MS = 30000;
  private
    FClock: ISystemClock;
    FCache: TDictionary<UInt64, TProfileCacheEntry>;
    FLock: TObject;

    function IsCacheValid(const AEntry: TProfileCacheEntry): Boolean;
    function QueryProfilesFromWindows(ADeviceAddress: UInt64): TDeviceProfileInfo;
  public
    /// <summary>
    /// Creates a new profile query instance with caching.
    /// </summary>
    /// <param name="AClock">Clock for cache expiration timing.</param>
    constructor Create(AClock: ISystemClock);
    destructor Destroy; override;

    { IProfileQuery }
    function GetDeviceProfiles(ADeviceAddress: UInt64): TDeviceProfileInfo;
    procedure ClearCache;

    /// <summary>
    /// Enumerates enabled service GUIDs for a device.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <returns>Array of enabled service GUIDs.</returns>
    function EnumerateEnabledServices(ADeviceAddress: UInt64): TArray<TGUID>;

    /// <summary>
    /// Logs detailed profile information for debugging.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <param name="ADeviceName">Device name for logging.</param>
    procedure LogDeviceProfiles(ADeviceAddress: UInt64; const ADeviceName: string);
  end;

/// <summary>
/// Creates a profile query instance for production use.
/// </summary>
function CreateProfileQuery: IProfileQuery;

implementation

uses
  System.DateUtils,
  App.Logger;

{ Factory function }

function CreateProfileQuery: IProfileQuery;
begin
  Result := TProfileQuery.Create(SystemClock);
end;

{ TProfileQuery }

constructor TProfileQuery.Create(AClock: ISystemClock);
begin
  inherited Create;
  Assert(AClock <> nil, 'ISystemClock is required');
  FClock := AClock;
  FCache := TDictionary<UInt64, TProfileCacheEntry>.Create;
  FLock := TObject.Create;
  LogDebug('Created with caching enabled (duration=%d ms)', [CACHE_DURATION_MS], LOG_SOURCE);
end;

destructor TProfileQuery.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited Destroy;
end;

function TProfileQuery.IsCacheValid(const AEntry: TProfileCacheEntry): Boolean;
var
  ElapsedMs: Int64;
begin
  ElapsedMs := MilliSecondsBetween(FClock.Now, AEntry.CachedAt);
  Result := ElapsedMs < CACHE_DURATION_MS;
end;

procedure TProfileQuery.ClearCache;
begin
  TMonitor.Enter(FLock);
  try
    FCache.Clear;
    LogDebug('Cache cleared', LOG_SOURCE);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TProfileQuery.EnumerateEnabledServices(
  ADeviceAddress: UInt64): TArray<TGUID>;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ServiceCount: DWORD;
  Services: array[0..MAX_SERVICES - 1] of TGUID;
  ErrorCode: DWORD;
  I: Integer;
begin
  SetLength(Result, 0);

  // Initialize device info with address
  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address.ullLong := ADeviceAddress;

  // First call: get service count
  ServiceCount := 0;
  ErrorCode := BluetoothEnumerateInstalledServices(0, @DeviceInfo, ServiceCount, nil);

  // ERROR_MORE_DATA is expected when pGuidServices is nil
  if (ErrorCode <> ERROR_SUCCESS) and (ErrorCode <> ERROR_MORE_DATA) then
  begin
    LogDebug('EnumerateEnabledServices: Failed to get count, error=%d', [ErrorCode], LOG_SOURCE);
    Exit;
  end;

  if ServiceCount = 0 then
  begin
    LogDebug('EnumerateEnabledServices: No services found for %.12X', [ADeviceAddress], LOG_SOURCE);
    Exit;
  end;

  // Clamp to max
  if ServiceCount > MAX_SERVICES then
    ServiceCount := MAX_SERVICES;

  // Second call: get actual services
  ErrorCode := BluetoothEnumerateInstalledServices(0, @DeviceInfo, ServiceCount, @Services[0]);
  if ErrorCode <> ERROR_SUCCESS then
  begin
    LogDebug('EnumerateEnabledServices: Failed to enumerate, error=%d', [ErrorCode], LOG_SOURCE);
    Exit;
  end;

  // Copy to result
  SetLength(Result, ServiceCount);
  for I := 0 to ServiceCount - 1 do
    Result[I] := Services[I];

  LogDebug('EnumerateEnabledServices: Found %d services for %.12X', [ServiceCount, ADeviceAddress], LOG_SOURCE);
end;

function TProfileQuery.QueryProfilesFromWindows(
  ADeviceAddress: UInt64): TDeviceProfileInfo;
var
  Services: TArray<TGUID>;
  Profiles: TBluetoothProfileArray;
  ProfileTypes: TBluetoothProfileTypes;
  Profile: TBluetoothProfile;
  ProfileType: TBluetoothProfileType;
  I, ProfileCount: Integer;
begin
  Services := EnumerateEnabledServices(ADeviceAddress);

  if Length(Services) = 0 then
  begin
    Result := TDeviceProfileInfo.Empty(ADeviceAddress);
    Exit;
  end;

  // Convert GUIDs to profile types, avoiding duplicates
  ProfileTypes := [];
  SetLength(Profiles, Length(Services));
  ProfileCount := 0;

  for I := 0 to High(Services) do
  begin
    ProfileType := GuidToProfileType(Services[I]);

    // Skip unknown and already-added profile types
    if (ProfileType <> bptUnknown) and not (ProfileType in ProfileTypes) then
    begin
      Include(ProfileTypes, ProfileType);

      Profile := TBluetoothProfile.Create(
        ProfileType,
        Services[I],
        pcsAvailable  // Initial state - actual connection state tracked separately
      );

      Profiles[ProfileCount] := Profile;
      Inc(ProfileCount);
    end;
  end;

  // Trim array to actual count
  SetLength(Profiles, ProfileCount);

  Result := TDeviceProfileInfo.Create(ADeviceAddress, Profiles, FClock.Now);
end;

function TProfileQuery.GetDeviceProfiles(
  ADeviceAddress: UInt64): TDeviceProfileInfo;
var
  Entry: TProfileCacheEntry;
begin
  TMonitor.Enter(FLock);
  try
    // Check cache first
    if FCache.TryGetValue(ADeviceAddress, Entry) then
    begin
      if IsCacheValid(Entry) then
      begin
        LogDebug('GetDeviceProfiles: Cache hit for %.12X', [ADeviceAddress], LOG_SOURCE);
        Result := Entry.ProfileInfo;
        Exit;
      end
      else
      begin
        // Cache expired, remove entry
        FCache.Remove(ADeviceAddress);
        LogDebug('GetDeviceProfiles: Cache expired for %.12X', [ADeviceAddress], LOG_SOURCE);
      end;
    end;
  finally
    TMonitor.Exit(FLock);
  end;

  // Query from Windows API (outside lock to avoid blocking)
  LogDebug('GetDeviceProfiles: Querying Windows API for %.12X', [ADeviceAddress], LOG_SOURCE);
  Result := QueryProfilesFromWindows(ADeviceAddress);

  // Store in cache
  TMonitor.Enter(FLock);
  try
    Entry.ProfileInfo := Result;
    Entry.CachedAt := FClock.Now;
    FCache.AddOrSetValue(ADeviceAddress, Entry);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TProfileQuery.LogDeviceProfiles(ADeviceAddress: UInt64;
  const ADeviceName: string);
var
  ProfileInfo: TDeviceProfileInfo;
  I: Integer;
begin
  ProfileInfo := GetDeviceProfiles(ADeviceAddress);

  LogInfo('Device profiles for %s (%.12X):', [ADeviceName, ADeviceAddress], LOG_SOURCE);
  LogInfo('  Total profiles: %d', [ProfileInfo.Count], LOG_SOURCE);

  for I := 0 to ProfileInfo.Count - 1 do
  begin
    LogInfo('  [%d] %s (%s) - State: %d', [
      I,
      ProfileInfo.Profiles[I].DisplayName,
      ProfileInfo.Profiles[I].ShortName,
      Ord(ProfileInfo.Profiles[I].ConnectionState)
    ], LOG_SOURCE);
  end;
end;

end.
