{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Item Builder                     }
{                                                       }
{       Builds pre-processed display items from raw     }
{       device data. Implements Information Expert      }
{       pattern by centralizing config access here.     }
{                                                       }
{*******************************************************}

unit UI.DeviceDisplayItemBuilder;

interface

uses
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  App.AppearanceConfigIntf,
  App.ProfileConfigIntf,
  App.DeviceDisplayTypes;

type
  /// <summary>
  /// Builds display items from raw Bluetooth device data.
  /// Responsibilities:
  ///   - Filter hidden devices
  ///   - Resolve aliases and device type overrides
  ///   - Format dates according to appearance settings
  ///   - Calculate sort groups
  ///   - Include battery status when available
  ///   - Sort the result
  /// This follows Information Expert: the builder has all needed
  /// information (configs) to build display items.
  /// </summary>
  TDeviceDisplayItemBuilder = class
  private
    FConfigProvider: IDeviceConfigQuery;
    FAppearanceConfig: IAppearanceConfig;
    FProfileConfig: IProfileConfig;
    FProfileQuery: IProfileQuery;
    FBatteryCache: IBatteryCache;
    /// <summary>
    /// Internal visibility check using pre-loaded config to avoid redundant lookups.
    /// </summary>
    function IsVisibleWithConfig(const ADevice: TBluetoothDeviceInfo;
      const AConfig: TDeviceConfig): Boolean;
    /// <summary>
    /// Internal display item builder using pre-loaded config to avoid redundant lookups.
    /// </summary>
    function BuildDisplayItemWithConfig(const ADevice: TBluetoothDeviceInfo;
      const AConfig: TDeviceConfig): TDeviceDisplayItem;
  public
    /// <summary>
    /// Creates a new builder instance with required dependencies.
    /// </summary>
    /// <param name="AConfigProvider">Provider for device-specific configuration (read-only).</param>
    /// <param name="AAppearanceConfig">Provider for appearance settings.</param>
    /// <param name="AProfileConfig">Provider for profile display settings.</param>
    /// <param name="AProfileQuery">Query for device Bluetooth profiles.</param>
    constructor Create(AConfigProvider: IDeviceConfigQuery;
      AAppearanceConfig: IAppearanceConfig;
      AProfileConfig: IProfileConfig;
      AProfileQuery: IProfileQuery);

    /// <summary>
    /// Sets the battery cache for battery status lookup.
    /// Optional - if not set, battery info will not be displayed.
    /// </summary>
    /// <param name="ABatteryCache">Battery cache instance.</param>
    procedure SetBatteryCache(ABatteryCache: IBatteryCache);

    /// <summary>
    /// Builds display items from an array of raw devices.
    /// Filters hidden devices, processes all data, and sorts the result.
    /// </summary>
    /// <param name="ADevices">Raw Bluetooth device data.</param>
    /// <returns>Sorted array of display items ready for rendering.</returns>
    function BuildDisplayItems(const ADevices: TBluetoothDeviceInfoArray): TDeviceDisplayItemArray;

    /// <summary>
    /// Builds a single display item from a device.
    /// Does not filter - use for updating existing items.
    /// </summary>
    /// <param name="ADevice">Raw Bluetooth device data.</param>
    /// <returns>Display item with all data pre-processed.</returns>
    function BuildDisplayItem(const ADevice: TBluetoothDeviceInfo): TDeviceDisplayItem;

    /// <summary>
    /// Checks if a device should be visible (not hidden and has a name).
    /// </summary>
    /// <param name="ADevice">Device to check.</param>
    /// <returns>True if device should be displayed, False otherwise.</returns>
    function IsVisible(const ADevice: TBluetoothDeviceInfo): Boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  App.Logger,
  UI.DeviceFormatter,
  UI.DeviceSorter;

{ TDeviceDisplayItemBuilder }

constructor TDeviceDisplayItemBuilder.Create(AConfigProvider: IDeviceConfigQuery;
  AAppearanceConfig: IAppearanceConfig;
  AProfileConfig: IProfileConfig;
  AProfileQuery: IProfileQuery);
begin
  inherited Create;
  FConfigProvider := AConfigProvider;
  FAppearanceConfig := AAppearanceConfig;
  FProfileConfig := AProfileConfig;
  FProfileQuery := AProfileQuery;
  FBatteryCache := nil;
end;

procedure TDeviceDisplayItemBuilder.SetBatteryCache(ABatteryCache: IBatteryCache);
begin
  FBatteryCache := ABatteryCache;
end;

function TDeviceDisplayItemBuilder.IsVisibleWithConfig(
  const ADevice: TBluetoothDeviceInfo; const AConfig: TDeviceConfig): Boolean;
var
  EffectiveName: string;
begin
  LogDebug('IsVisible: Checking device Address=$%.12X, Name="%s"', [ADevice.AddressInt, ADevice.Name], ClassName);

  // Calculate effective display name using the same fallback chain as GetDisplayName
  // Priority: Alias > API Name > Cached Name > "Device " + MAC
  EffectiveName := TDeviceFormatter.GetDisplayName(ADevice, AConfig);

  LogDebug('IsVisible: Config loaded - Hidden=%s, Alias="%s", ConfigName="%s", EffectiveName="%s"',
    [BoolToStr(AConfig.Hidden, True), AConfig.Alias, AConfig.Name, EffectiveName], ClassName);

  // Skip hidden devices
  if AConfig.Hidden then
  begin
    LogDebug('IsVisible: FILTERED - device is hidden', ClassName);
    Exit(False);
  end;

  // With MAC fallback, EffectiveName is never empty - but check anyway for safety
  if Trim(EffectiveName) = '' then
  begin
    LogDebug('IsVisible: FILTERED - no effective name available', ClassName);
    Exit(False);
  end;

  LogDebug('IsVisible: VISIBLE', ClassName);
  Result := True;
end;

function TDeviceDisplayItemBuilder.IsVisible(const ADevice: TBluetoothDeviceInfo): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  // Public method loads config and delegates to internal method
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
  Result := IsVisibleWithConfig(ADevice, DeviceConfig);
end;

function TDeviceDisplayItemBuilder.BuildDisplayItemWithConfig(
  const ADevice: TBluetoothDeviceInfo; const AConfig: TDeviceConfig): TDeviceDisplayItem;
var
  LastSeenFormat: TLastSeenFormat;
  BatteryStatus: TBatteryStatus;
  BatteryText: string;
  ProfileInfo: TDeviceProfileInfo;
  Profiles: TBluetoothProfileArray;
  ShowProfiles: Boolean;
begin
  LastSeenFormat := FAppearanceConfig.LastSeenFormat;

  // Get battery status from cache if available and enabled
  if (FBatteryCache <> nil) and FAppearanceConfig.ShowBatteryLevel then
  begin
    BatteryStatus := FBatteryCache.GetBatteryStatus(ADevice.AddressInt);
    BatteryText := TDeviceFormatter.FormatBatteryLevel(BatteryStatus);
  end
  else
  begin
    BatteryStatus := TBatteryStatus.NotSupported;
    BatteryText := '';
  end;

  // Get device profiles if enabled and device is connected
  // Per-device override: -1=Global, 0=No, 1=Yes
  Profiles := nil;
  if ADevice.IsConnected then
  begin
    if AConfig.ShowProfiles = -1 then
      // Use global setting
      ShowProfiles := Assigned(FProfileConfig) and FProfileConfig.ShowProfiles
    else
      // Use per-device setting
      ShowProfiles := AConfig.ShowProfiles = 1;

    if ShowProfiles and Assigned(FProfileQuery) then
    begin
      ProfileInfo := FProfileQuery.GetDeviceProfiles(ADevice.AddressInt);
      // Only include profiles if device has more than one
      if ProfileInfo.Count > 1 then
        Profiles := ProfileInfo.Profiles;
    end;
  end;

  Result := TDeviceDisplayItem.Create(
    ADevice,
    TDeviceFormatter.GetDisplayName(ADevice, AConfig),
    AConfig.Pinned,
    TDeviceFormatter.GetEffectiveDeviceType(ADevice, AConfig),
    TDeviceFormatter.FormatLastSeen(AConfig.LastSeen, LastSeenFormat),
    AConfig.LastSeen,
    TDeviceFormatter.GetSortGroup(ADevice, AConfig),
    BatteryStatus,
    BatteryText,
    Profiles
  );
end;

function TDeviceDisplayItemBuilder.BuildDisplayItem(
  const ADevice: TBluetoothDeviceInfo): TDeviceDisplayItem;
var
  DeviceConfig: TDeviceConfig;
begin
  // Public method loads config and delegates to internal method
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
  Result := BuildDisplayItemWithConfig(ADevice, DeviceConfig);
end;

function TDeviceDisplayItemBuilder.BuildDisplayItems(
  const ADevices: TBluetoothDeviceInfoArray): TDeviceDisplayItemArray;
var
  I: Integer;
  Device: TBluetoothDeviceInfo;
  DeviceConfig: TDeviceConfig;
  VisibleItems: TList<TDeviceDisplayItem>;
begin
  LogDebug('BuildDisplayItems: Input count=%d', [Length(ADevices)], ClassName);

  // Log all input devices for debugging
  for I := 0 to High(ADevices) do
    LogDebug('BuildDisplayItems: Input[%d] Address=$%.12X, Name="%s"',
      [I, ADevices[I].AddressInt, ADevices[I].Name], ClassName);

  // Single pass: load config once per device, check visibility, build item
  VisibleItems := TList<TDeviceDisplayItem>.Create;
  try
    for Device in ADevices do
    begin
      // Load config once per device
      DeviceConfig := FConfigProvider.GetDeviceConfig(Device.AddressInt);

      // Check visibility using pre-loaded config
      if IsVisibleWithConfig(Device, DeviceConfig) then
        // Build display item using same pre-loaded config
        VisibleItems.Add(BuildDisplayItemWithConfig(Device, DeviceConfig));
    end;

    LogDebug('BuildDisplayItems: Visible count=%d', [VisibleItems.Count], ClassName);

    // Convert to array
    Result := VisibleItems.ToArray;
  finally
    VisibleItems.Free;
  end;

  // Sort the result
  TDeviceSorter.Sort(Result);
  LogDebug('BuildDisplayItems: Complete, output count=%d', [Length(Result)], ClassName);
end;

end.
