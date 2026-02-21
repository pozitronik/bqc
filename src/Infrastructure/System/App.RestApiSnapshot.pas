{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API JSON Snapshot Builder                  }
{                                                       }
{*******************************************************}

/// <summary>
/// Builds JSON snapshots from device display data.
/// Pure class functions with no state -- fully testable without server dependency.
/// </summary>
unit App.RestApiSnapshot;

interface

uses
  System.SysUtils,
  System.JSON,
  System.DateUtils,
  Bluetooth.Types,
  App.DeviceDisplayTypes;

type
  TRestApiSnapshotBuilder = class
  public
    /// <summary>
    /// Builds the full JSON response for /api/devices.
    /// Contains adapter state, device array, and summary counts.
    /// </summary>
    class function BuildFullSnapshot(
      const AItems: TDeviceDisplayItemArray;
      AAdapterAvailable, AAdapterEnabled: Boolean): string;

    /// <summary>
    /// Builds the status-only JSON response for /api/status.
    /// Contains adapter state and summary counts only (no device details).
    /// </summary>
    class function BuildStatusSnapshot(
      AAdapterAvailable, AAdapterEnabled: Boolean;
      ATotalDevices, AConnectedDevices: Integer): string;

    /// <summary>
    /// Builds a single device JSON object.
    /// </summary>
    class function BuildDeviceJson(
      const AItem: TDeviceDisplayItem): TJSONObject;

    /// <summary>
    /// Finds a device by address string (supports AA:BB:CC:DD:EE:FF and AABBCCDDEEFF).
    /// Returns index in AItems or -1 if not found.
    /// </summary>
    class function FindDeviceByAddress(
      const AItems: TDeviceDisplayItemArray;
      const AAddress: string): Integer;
  private
    class function DeviceTypeToString(AType: TBluetoothDeviceType): string;
    class function ConnectionStateToString(AState: TBluetoothConnectionState): string;
    class function ParseAddressString(const AAddress: string): UInt64;
  end;

implementation

{ TRestApiSnapshotBuilder }

class function TRestApiSnapshotBuilder.BuildFullSnapshot(
  const AItems: TDeviceDisplayItemArray;
  AAdapterAvailable, AAdapterEnabled: Boolean): string;
var
  Root, Adapter, Summary: TJSONObject;
  Devices: TJSONArray;
  I, ConnectedCount: Integer;
begin
  Root := TJSONObject.Create;
  try
    // Adapter state
    Adapter := TJSONObject.Create;
    Adapter.AddPair('available', TJSONBool.Create(AAdapterAvailable));
    Adapter.AddPair('enabled', TJSONBool.Create(AAdapterEnabled));
    Root.AddPair('adapter', Adapter);

    // Device array
    Devices := TJSONArray.Create;
    ConnectedCount := 0;
    for I := 0 to High(AItems) do
    begin
      // Skip action items (scan buttons etc.)
      if AItems[I].Source = dsAction then
        Continue;
      Devices.AddElement(BuildDeviceJson(AItems[I]));
      if AItems[I].Device.IsConnected then
        Inc(ConnectedCount);
    end;
    Root.AddPair('devices', Devices);

    // Summary
    Summary := TJSONObject.Create;
    Summary.AddPair('totalDevices', TJSONNumber.Create(Devices.Count));
    Summary.AddPair('connectedDevices', TJSONNumber.Create(ConnectedCount));
    Root.AddPair('summary', Summary);

    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

class function TRestApiSnapshotBuilder.BuildStatusSnapshot(
  AAdapterAvailable, AAdapterEnabled: Boolean;
  ATotalDevices, AConnectedDevices: Integer): string;
var
  Root, Adapter, Summary: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Adapter := TJSONObject.Create;
    Adapter.AddPair('available', TJSONBool.Create(AAdapterAvailable));
    Adapter.AddPair('enabled', TJSONBool.Create(AAdapterEnabled));
    Root.AddPair('adapter', Adapter);

    Summary := TJSONObject.Create;
    Summary.AddPair('totalDevices', TJSONNumber.Create(ATotalDevices));
    Summary.AddPair('connectedDevices', TJSONNumber.Create(AConnectedDevices));
    Root.AddPair('summary', Summary);

    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

class function TRestApiSnapshotBuilder.BuildDeviceJson(
  const AItem: TDeviceDisplayItem): TJSONObject;
var
  Battery: TJSONObject;
  Profiles: TJSONArray;
  I: Integer;
begin
  Result := TJSONObject.Create;

  Result.AddPair('address', AItem.Device.AddressString);
  Result.AddPair('name', AItem.Device.Name);
  Result.AddPair('displayName', AItem.DisplayName);
  Result.AddPair('type', DeviceTypeToString(AItem.EffectiveDeviceType));
  Result.AddPair('connectionState', ConnectionStateToString(AItem.Device.ConnectionState));
  Result.AddPair('isConnected', TJSONBool.Create(AItem.Device.IsConnected));
  Result.AddPair('isPaired', TJSONBool.Create(AItem.Device.IsPaired));
  Result.AddPair('isPinned', TJSONBool.Create(AItem.IsPinned));

  // Battery
  Battery := TJSONObject.Create;
  if AItem.BatteryStatus.HasLevel then
    Battery.AddPair('level', TJSONNumber.Create(AItem.BatteryStatus.Level))
  else
    Battery.AddPair('level', TJSONNull.Create);
  Battery.AddPair('supported', TJSONBool.Create(AItem.BatteryStatus.IsSupported));
  Battery.AddPair('text', AItem.BatteryText);
  Result.AddPair('battery', Battery);

  // Last seen (ISO 8601)
  if AItem.LastSeen > 0 then
    Result.AddPair('lastSeen', DateToISO8601(AItem.LastSeen, False))
  else
    Result.AddPair('lastSeen', TJSONNull.Create);

  // Profiles
  Profiles := TJSONArray.Create;
  for I := 0 to High(AItem.Profiles) do
    Profiles.Add(AItem.Profiles[I].ShortName);
  Result.AddPair('profiles', Profiles);
end;

class function TRestApiSnapshotBuilder.FindDeviceByAddress(
  const AItems: TDeviceDisplayItemArray;
  const AAddress: string): Integer;
var
  I: Integer;
  TargetAddress: UInt64;
begin
  TargetAddress := ParseAddressString(AAddress);
  if TargetAddress = 0 then
    Exit(-1);

  for I := 0 to High(AItems) do
  begin
    if AItems[I].Source = dsAction then
      Continue;
    if AItems[I].Device.AddressInt = TargetAddress then
      Exit(I);
  end;
  Result := -1;
end;

class function TRestApiSnapshotBuilder.DeviceTypeToString(
  AType: TBluetoothDeviceType): string;
begin
  case AType of
    btUnknown:     Result := 'Unknown';
    btAudioOutput: Result := 'AudioOutput';
    btAudioInput:  Result := 'AudioInput';
    btHeadset:     Result := 'Headset';
    btComputer:    Result := 'Computer';
    btPhone:       Result := 'Phone';
    btKeyboard:    Result := 'Keyboard';
    btMouse:       Result := 'Mouse';
    btGamepad:     Result := 'Gamepad';
    btHID:         Result := 'HID';
  else
    Result := 'Unknown';
  end;
end;

class function TRestApiSnapshotBuilder.ConnectionStateToString(
  AState: TBluetoothConnectionState): string;
begin
  case AState of
    csDisconnected:  Result := 'Disconnected';
    csConnected:     Result := 'Connected';
    csConnecting:    Result := 'Connecting';
    csDisconnecting: Result := 'Disconnecting';
    csUnknown:       Result := 'Unknown';
    csError:         Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

class function TRestApiSnapshotBuilder.ParseAddressString(
  const AAddress: string): UInt64;
var
  Clean: string;
  I: Integer;
  B: Byte;
begin
  // Remove colons and dashes to normalize
  Clean := '';
  for I := 1 to Length(AAddress) do
  begin
    if not CharInSet(AAddress[I], [':', '-']) then
      Clean := Clean + AAddress[I];
  end;

  // Must be exactly 12 hex characters
  if Length(Clean) <> 12 then
    Exit(0);

  // Validate all hex
  for I := 1 to Length(Clean) do
    if not CharInSet(Clean[I], ['0'..'9', 'A'..'F', 'a'..'f']) then
      Exit(0);

  // Parse as big-endian MAC (most significant byte first)
  Result := 0;
  for I := 0 to 5 do
  begin
    B := StrToInt('$' + Copy(Clean, I * 2 + 1, 2));
    Result := Result or (UInt64(B) shl ((5 - I) * 8));
  end;
end;

end.
