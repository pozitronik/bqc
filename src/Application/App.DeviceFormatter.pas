{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Formatting Utilities             }
{                                                       }
{       Provides formatting helpers for device display. }
{       Extracted from TDeviceListBox to follow SRP.    }
{                                                       }
{*******************************************************}

unit App.DeviceFormatter;

interface

uses
  System.SysUtils,
  Bluetooth.Types,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.AppearanceConfigIntf,
  App.DeviceConfigTypes,
  App.SystemClock;

type
  /// <summary>
  /// Injectable class providing formatting helpers for device display.
  /// Time-based methods require ISystemClock for testability.
  /// Other methods are pure functions with no side effects.
  /// </summary>
  TDeviceFormatter = class
  private
    FClock: ISystemClock;
  public
    /// <summary>
    /// Creates formatter with injectable clock for testability.
    /// </summary>
    /// <param name="AClock">System clock abstraction. If nil, uses default SystemClock.</param>
    constructor Create(AClock: ISystemClock = nil);

    /// <summary>
    /// Formats last seen timestamp in relative format ("2 hours ago", "Yesterday").
    /// Uses injected clock for current time.
    /// </summary>
    /// <param name="ALastSeen">The DateTime value to format. Zero or negative returns empty string.</param>
    /// <returns>Human-readable relative time string, or empty if never seen.</returns>
    function FormatLastSeenRelative(ALastSeen: TDateTime): string;

    /// <summary>
    /// Formats last seen timestamp in absolute format ("2024-12-22 15:30").
    /// </summary>
    /// <param name="ALastSeen">The DateTime value to format. Zero or negative returns empty string.</param>
    /// <returns>Formatted date-time string, or empty if never seen.</returns>
    function FormatLastSeenAbsolute(ALastSeen: TDateTime): string;

    /// <summary>
    /// Formats last seen timestamp based on the specified format setting.
    /// Creates a temporary formatter instance with default clock.
    /// For clock control, use instance methods directly with injected clock.
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

    /// <summary>
    /// Checks if a device name is in a generic Windows format.
    /// Generic formats include:
    /// - Empty string
    /// - "Bluetooth XX:XX:XX:XX:XX:XX" (27 chars, WinRT fallback format)
    /// - Uppercase MAC without colons "88C9E8A166B4" (12 hex chars)
    /// These indicate the real device name is not available from the Windows API.
    /// </summary>
    /// <param name="AName">The device name to check.</param>
    /// <returns>True if the name is generic/placeholder, False if it's a real device name.</returns>
    class function IsGenericName(const AName: string): Boolean; static;
  end;

implementation

uses
  System.DateUtils,
  System.RegularExpressions;

{ TDeviceFormatter }

constructor TDeviceFormatter.Create(AClock: ISystemClock);
begin
  inherited Create;
  if Assigned(AClock) then
    FClock := AClock
  else
    FClock := App.SystemClock.SystemClock;  // Default: real system clock
end;

function TDeviceFormatter.FormatLastSeenRelative(ALastSeen: TDateTime): string;
var
  CurrentTime: TDateTime;
  Diff: TDateTime;
  Days, Hours, Minutes: Integer;
begin
  if ALastSeen <= 0 then
    Exit('');  // Empty string for never-seen devices (UI will hide last seen)

  CurrentTime := FClock.Now;  // Use injected clock
  Diff := CurrentTime - ALastSeen;
  Days := Trunc(Diff);
  Hours := HoursBetween(CurrentTime, ALastSeen);
  Minutes := MinutesBetween(CurrentTime, ALastSeen);

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

function TDeviceFormatter.FormatLastSeenAbsolute(ALastSeen: TDateTime): string;
begin
  if ALastSeen <= 0 then
    Result := ''  // Don't show anything for "never seen" in absolute format
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn', ALastSeen);
end;

class function TDeviceFormatter.FormatLastSeen(ALastSeen: TDateTime;
  AFormat: TLastSeenFormat): string;
begin
  // Create temporary formatter with default clock for backward compatibility
  with TDeviceFormatter.Create do
  try
    case AFormat of
      lsfRelative:
        Result := FormatLastSeenRelative(ALastSeen);
      lsfAbsolute:
        Result := FormatLastSeenAbsolute(ALastSeen);
    else
      Result := FormatLastSeenRelative(ALastSeen);
    end;
  finally
    Free;
  end;
end;

class function TDeviceFormatter.GetDisplayName(const ADevice: TBluetoothDeviceInfo;
  const AConfig: TDeviceConfig): string;
begin
  // Priority order:
  // 1. User-defined alias (highest priority - explicit user intent)
  // 2. Current device name from Windows API (if non-generic)
  // 3. Cached name from INI config (fallback for generic API names)
  // 4. Current device name even if generic (better than nothing)
  // 5. "Device " + MAC address as last resort
  if AConfig.Alias <> '' then
    Result := AConfig.Alias
  else if (ADevice.Name <> '') and not IsGenericName(ADevice.Name) then
    Result := ADevice.Name  // Fresh non-generic name from API
  else if AConfig.Name <> '' then
    Result := AConfig.Name  // Cached name (may be better than generic)
  else if ADevice.Name <> '' then
    Result := ADevice.Name  // Use generic name if nothing better
  else
    Result := 'Device ' + ADevice.AddressString;
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

class function TDeviceFormatter.IsGenericName(const AName: string): Boolean;
begin
  // Check for Windows generic formats:
  // 1. Empty string - no name available
  // 2. "Bluetooth XX:XX:XX:XX:XX:XX" - WinRT fallback (27 chars, starts with "Bluetooth ")
  // 3. Uppercase MAC without colons "88C9E8A166B4" - Another fallback format (12 hex chars)

  Result := (AName = '') or
            (AName.StartsWith('Bluetooth ', True) and (AName.Length = 27)) or
            ((AName.Length = 12) and TRegEx.IsMatch(AName, '^[0-9A-F]{12}$'));
end;

end.
