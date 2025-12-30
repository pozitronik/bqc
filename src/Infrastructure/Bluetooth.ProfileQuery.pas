{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Profile Query Implementation                    }
{                                                       }
{       Enumerates and queries Bluetooth profiles       }
{       for paired devices.                             }
{                                                       }
{*******************************************************}

unit Bluetooth.ProfileQuery;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Provides profile enumeration and query functionality.
  /// </summary>
  TProfileQuery = class
  private const
    LOG_SOURCE = 'ProfileQuery';
    MAX_SERVICES = 32;
  public
    /// <summary>
    /// Enumerates enabled service GUIDs for a device.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <returns>Array of enabled service GUIDs.</returns>
    class function EnumerateEnabledServices(ADeviceAddress: UInt64): TArray<TGUID>;

    /// <summary>
    /// Gets profile information for a device.
    /// Queries enabled services and converts to profile types.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <returns>Device profile information record.</returns>
    class function GetDeviceProfiles(ADeviceAddress: UInt64): TDeviceProfileInfo;

    /// <summary>
    /// Logs detailed profile information for debugging.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <param name="ADeviceName">Device name for logging.</param>
    class procedure LogDeviceProfiles(ADeviceAddress: UInt64; const ADeviceName: string);
  end;

implementation

uses
  App.Logger;

{ TProfileQuery }

class function TProfileQuery.EnumerateEnabledServices(
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

class function TProfileQuery.GetDeviceProfiles(
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

  Result := TDeviceProfileInfo.Create(ADeviceAddress, Profiles, Now);
end;

class procedure TProfileQuery.LogDeviceProfiles(ADeviceAddress: UInt64;
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
