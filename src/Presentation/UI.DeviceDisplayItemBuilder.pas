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
  UI.DeviceList;

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
    FBatteryCache: IBatteryCache;
  public
    /// <summary>
    /// Creates a new builder instance with required dependencies.
    /// </summary>
    /// <param name="AConfigProvider">Provider for device-specific configuration (read-only).</param>
    /// <param name="AAppearanceConfig">Provider for appearance settings.</param>
    constructor Create(AConfigProvider: IDeviceConfigQuery;
      AAppearanceConfig: IAppearanceConfig);

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
  App.Logger,
  UI.DeviceFormatter,
  UI.DeviceSorter;

{ TDeviceDisplayItemBuilder }

constructor TDeviceDisplayItemBuilder.Create(AConfigProvider: IDeviceConfigQuery;
  AAppearanceConfig: IAppearanceConfig);
begin
  inherited Create;
  FConfigProvider := AConfigProvider;
  FAppearanceConfig := AAppearanceConfig;
  FBatteryCache := nil;
end;

procedure TDeviceDisplayItemBuilder.SetBatteryCache(ABatteryCache: IBatteryCache);
begin
  FBatteryCache := ABatteryCache;
end;

function TDeviceDisplayItemBuilder.IsVisible(const ADevice: TBluetoothDeviceInfo): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  LogDebug('IsVisible: Checking device Address=$%.12X, Name="%s"', [ADevice.AddressInt, ADevice.Name], ClassName);

  // Skip devices with empty names
  if Trim(ADevice.Name) = '' then
  begin
    LogDebug('IsVisible: FILTERED - empty name', ClassName);
    Exit(False);
  end;

  // Skip hidden devices
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
  LogDebug('IsVisible: Config loaded - Hidden=%s, Alias="%s"',
    [BoolToStr(DeviceConfig.Hidden, True), DeviceConfig.Alias], ClassName);

  if DeviceConfig.Hidden then
  begin
    LogDebug('IsVisible: FILTERED - device is hidden', ClassName);
    Exit(False);
  end;

  LogDebug('IsVisible: VISIBLE', ClassName);
  Result := True;
end;

function TDeviceDisplayItemBuilder.BuildDisplayItem(
  const ADevice: TBluetoothDeviceInfo): TDeviceDisplayItem;
var
  DeviceConfig: TDeviceConfig;
  LastSeenFormat: TLastSeenFormat;
  BatteryStatus: TBatteryStatus;
  BatteryText: string;
begin
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
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

  Result := TDeviceDisplayItem.Create(
    ADevice,
    TDeviceFormatter.GetDisplayName(ADevice, DeviceConfig),
    DeviceConfig.Pinned,
    TDeviceFormatter.GetEffectiveDeviceType(ADevice, DeviceConfig),
    TDeviceFormatter.FormatLastSeen(DeviceConfig.LastSeen, LastSeenFormat),
    DeviceConfig.LastSeen,
    TDeviceFormatter.GetSortGroup(ADevice, DeviceConfig),
    BatteryStatus,
    BatteryText
  );
end;

function TDeviceDisplayItemBuilder.BuildDisplayItems(
  const ADevices: TBluetoothDeviceInfoArray): TDeviceDisplayItemArray;
var
  I, Count: Integer;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('BuildDisplayItems: Input count=%d', [Length(ADevices)], ClassName);

  // Log all input devices for debugging
  for I := 0 to High(ADevices) do
    LogDebug('BuildDisplayItems: Input[%d] Address=$%.12X, Name="%s"',
      [I, ADevices[I].AddressInt, ADevices[I].Name], ClassName);

  // First pass: count visible devices
  Count := 0;
  for Device in ADevices do
    if IsVisible(Device) then
      Inc(Count);

  LogDebug('BuildDisplayItems: Visible count=%d', [Count], ClassName);

  // Allocate result array
  SetLength(Result, Count);

  // Second pass: build items for visible devices
  I := 0;
  for Device in ADevices do
  begin
    if IsVisible(Device) then
    begin
      Result[I] := BuildDisplayItem(Device);
      Inc(I);
    end;
  end;

  // Sort the result
  TDeviceSorter.Sort(Result);
  LogDebug('BuildDisplayItems: Complete, output count=%d', [Length(Result)], ClassName);
end;

end.
