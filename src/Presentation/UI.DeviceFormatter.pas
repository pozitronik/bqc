{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Formatting Utilities             }
{                                                       }
{       Provides formatting helpers for device display. }
{       Extracted from TDeviceListBox to follow SRP.    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.DeviceFormatter;

interface

uses
  System.SysUtils,
  Bluetooth.Types,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.AppearanceConfigIntf;

type
  /// <summary>
  /// Static class providing formatting helpers for device display.
  /// All methods are pure functions with no side effects.
  /// </summary>
  TDeviceFormatter = class
  public
    /// <summary>
    /// Formats last seen timestamp in relative format ("2 hours ago", "Yesterday").
    /// </summary>
    /// <param name="ALastSeen">The DateTime value to format. Zero or negative returns "Never".</param>
    /// <returns>Human-readable relative time string.</returns>
    class function FormatLastSeenRelative(ALastSeen: TDateTime): string; static;

    /// <summary>
    /// Formats last seen timestamp in absolute format ("2024-12-22 15:30").
    /// </summary>
    /// <param name="ALastSeen">The DateTime value to format. Zero or negative returns "Never".</param>
    /// <returns>Formatted date-time string.</returns>
    class function FormatLastSeenAbsolute(ALastSeen: TDateTime): string; static;

    /// <summary>
    /// Formats last seen timestamp based on the specified format setting.
    /// </summary>
    /// <param name="ALastSeen">The DateTime value to format.</param>
    /// <param name="AFormat">The format to use (relative or absolute).</param>
    /// <returns>Formatted string according to the specified format.</returns>
    class function FormatLastSeen(ALastSeen: TDateTime;
      AFormat: TLastSeenFormat): string; static;

    /// <summary>
    /// Gets the display name for a device.
    /// Returns alias if set, otherwise returns the device's original name.
    /// </summary>
    /// <param name="ADevice">The Bluetooth device info.</param>
    /// <param name="AConfig">The device-specific configuration.</param>
    /// <returns>The name to display for this device.</returns>
    class function GetDisplayName(const ADevice: TBluetoothDeviceInfo;
      const AConfig: TDeviceConfig): string; static;

    /// <summary>
    /// Gets the effective device type for display.
    /// Returns the override type if set (>= 0), otherwise returns the auto-detected type.
    /// </summary>
    /// <param name="ADevice">The Bluetooth device info with auto-detected type.</param>
    /// <param name="AConfig">The device-specific configuration with potential override.</param>
    /// <returns>The device type to use for display.</returns>
    class function GetEffectiveDeviceType(const ADevice: TBluetoothDeviceInfo;
      const AConfig: TDeviceConfig): TBluetoothDeviceType; static;

    /// <summary>
    /// Calculates the sort group for a device.
    /// Groups: 0 = Pinned, 1 = Connected (not pinned), 2 = Disconnected (not pinned).
    /// </summary>
    /// <param name="ADevice">The Bluetooth device info.</param>
    /// <param name="AConfig">The device-specific configuration.</param>
    /// <returns>Sort group index (0, 1, or 2).</returns>
    class function GetSortGroup(const ADevice: TBluetoothDeviceInfo;
      const AConfig: TDeviceConfig): Integer; static;

    /// <summary>
    /// Formats a connection state as human-readable text.
    /// </summary>
    /// <param name="AState">The connection state to format.</param>
    /// <returns>Human-readable connection state string.</returns>
    class function FormatConnectionState(AState: TBluetoothConnectionState): string; static;

    /// <summary>
    /// Formats a device type as human-readable text.
    /// </summary>
    /// <param name="ADeviceType">The device type to format.</param>
    /// <returns>Human-readable device type string.</returns>
    class function FormatDeviceType(ADeviceType: TBluetoothDeviceType): string; static;

    /// <summary>
    /// Formats battery level as percentage text.
    /// </summary>
    /// <param name="AStatus">The battery status to format.</param>
    /// <returns>Formatted battery text (e.g., "85%") or empty string if not available.</returns>
    class function FormatBatteryLevel(const AStatus: TBatteryStatus): string; static;
  end;

implementation

uses
  System.DateUtils;

{ TDeviceFormatter }

class function TDeviceFormatter.FormatLastSeenRelative(ALastSeen: TDateTime): string;
var
  Diff: TDateTime;
  Days, Hours, Minutes: Integer;
begin
  if ALastSeen <= 0 then
    Exit('Never');

  Diff := Now - ALastSeen;
  Days := Trunc(Diff);
  Hours := HoursBetween(Now, ALastSeen);
  Minutes := MinutesBetween(Now, ALastSeen);

  if Minutes < 1 then
    Result := 'Just now'
  else if Minutes < 60 then
    Result := Format('%d min ago', [Minutes])
  else if Hours < 24 then
    Result := Format('%d hr ago', [Hours])
  else if Days = 1 then
    Result := 'Yesterday'
  else if Days < 7 then
    Result := Format('%d days ago', [Days])
  else if Days < 30 then
    Result := Format('%d weeks ago', [Days div 7])
  else if Days < 365 then
    Result := Format('%d months ago', [Days div 30])
  else
    Result := Format('%d years ago', [Days div 365]);
end;

class function TDeviceFormatter.FormatLastSeenAbsolute(ALastSeen: TDateTime): string;
begin
  if ALastSeen <= 0 then
    Result := ''  // Don't show anything for "never seen" in absolute format
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn', ALastSeen);
end;

class function TDeviceFormatter.FormatLastSeen(ALastSeen: TDateTime;
  AFormat: TLastSeenFormat): string;
begin
  case AFormat of
    lsfRelative:
      Result := FormatLastSeenRelative(ALastSeen);
    lsfAbsolute:
      Result := FormatLastSeenAbsolute(ALastSeen);
  else
    Result := FormatLastSeenRelative(ALastSeen);
  end;
end;

class function TDeviceFormatter.GetDisplayName(const ADevice: TBluetoothDeviceInfo;
  const AConfig: TDeviceConfig): string;
begin
  if AConfig.Alias <> '' then
    Result := AConfig.Alias
  else
    Result := ADevice.Name;
end;

class function TDeviceFormatter.GetEffectiveDeviceType(const ADevice: TBluetoothDeviceInfo;
  const AConfig: TDeviceConfig): TBluetoothDeviceType;
begin
  if AConfig.DeviceTypeOverride >= 0 then
    Result := TBluetoothDeviceType(AConfig.DeviceTypeOverride)
  else
    Result := ADevice.DeviceType;
end;

class function TDeviceFormatter.GetSortGroup(const ADevice: TBluetoothDeviceInfo;
  const AConfig: TDeviceConfig): Integer;
begin
  if AConfig.Pinned then
    Result := 0
  else if ADevice.IsConnected then
    Result := 1
  else
    Result := 2;
end;

class function TDeviceFormatter.FormatConnectionState(AState: TBluetoothConnectionState): string;
begin
  case AState of
    csDisconnected:  Result := 'Disconnected';
    csConnected:     Result := 'Connected';
    csConnecting:    Result := 'Connecting...';
    csDisconnecting: Result := 'Disconnecting...';
    csError:         Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

class function TDeviceFormatter.FormatDeviceType(ADeviceType: TBluetoothDeviceType): string;
begin
  case ADeviceType of
    btAudioOutput: Result := 'Audio Output';
    btAudioInput:  Result := 'Audio Input';
    btHeadset:     Result := 'Headset';
    btComputer:    Result := 'Computer';
    btPhone:       Result := 'Phone';
    btKeyboard:    Result := 'Keyboard';
    btMouse:       Result := 'Mouse';
    btGamepad:     Result := 'Gamepad';
    btHID:         Result := 'Input Device';
  else
    Result := 'Unknown';
  end;
end;

class function TDeviceFormatter.FormatBatteryLevel(const AStatus: TBatteryStatus): string;
begin
  if AStatus.HasLevel then
    Result := Format('%d%%', [AStatus.Level])
  else
    Result := '';
end;

end.
