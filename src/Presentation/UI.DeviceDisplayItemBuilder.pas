{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Item Builder                     }
{                                                       }
{       Builds pre-processed display items from raw     }
{       device data. Implements Information Expert      }
{       pattern by centralizing config access here.     }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.DeviceDisplayItemBuilder;

interface

uses
  Bluetooth.Types,
  App.ConfigInterfaces,
  UI.DeviceList;

type
  /// <summary>
  /// Builds display items from raw Bluetooth device data.
  /// Responsibilities:
  ///   - Filter hidden devices
  ///   - Resolve aliases and device type overrides
  ///   - Format dates according to appearance settings
  ///   - Calculate sort groups
  ///   - Sort the result
  /// This follows Information Expert: the builder has all needed
  /// information (configs) to build display items.
  /// </summary>
  TDeviceDisplayItemBuilder = class
  private
    FConfigProvider: IDeviceConfigQuery;
    FAppearanceConfig: IAppearanceConfig;
  public
    /// <summary>
    /// Creates a new builder instance with required dependencies.
    /// </summary>
    /// <param name="AConfigProvider">Provider for device-specific configuration (read-only).</param>
    /// <param name="AAppearanceConfig">Provider for appearance settings.</param>
    constructor Create(AConfigProvider: IDeviceConfigQuery;
      AAppearanceConfig: IAppearanceConfig);

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
  UI.DeviceFormatter,
  UI.DeviceSorter;

{ TDeviceDisplayItemBuilder }

constructor TDeviceDisplayItemBuilder.Create(AConfigProvider: IDeviceConfigQuery;
  AAppearanceConfig: IAppearanceConfig);
begin
  inherited Create;
  FConfigProvider := AConfigProvider;
  FAppearanceConfig := AAppearanceConfig;
end;

function TDeviceDisplayItemBuilder.IsVisible(const ADevice: TBluetoothDeviceInfo): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  // Skip devices with empty names
  if Trim(ADevice.Name) = '' then
    Exit(False);

  // Skip hidden devices
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
  if DeviceConfig.Hidden then
    Exit(False);

  Result := True;
end;

function TDeviceDisplayItemBuilder.BuildDisplayItem(
  const ADevice: TBluetoothDeviceInfo): TDeviceDisplayItem;
var
  DeviceConfig: TDeviceConfig;
  LastSeenFormat: TLastSeenFormat;
begin
  DeviceConfig := FConfigProvider.GetDeviceConfig(ADevice.AddressInt);
  LastSeenFormat := FAppearanceConfig.LastSeenFormat;

  Result := TDeviceDisplayItem.Create(
    ADevice,
    TDeviceFormatter.GetDisplayName(ADevice, DeviceConfig),
    DeviceConfig.Pinned,
    TDeviceFormatter.GetEffectiveDeviceType(ADevice, DeviceConfig),
    TDeviceFormatter.FormatLastSeen(DeviceConfig.LastSeen, LastSeenFormat),
    DeviceConfig.LastSeen,
    TDeviceFormatter.GetSortGroup(ADevice, DeviceConfig)
  );
end;

function TDeviceDisplayItemBuilder.BuildDisplayItems(
  const ADevices: TBluetoothDeviceInfoArray): TDeviceDisplayItemArray;
var
  I, Count: Integer;
  Device: TBluetoothDeviceInfo;
begin
  // First pass: count visible devices
  Count := 0;
  for Device in ADevices do
    if IsVisible(Device) then
      Inc(Count);

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
end;

end.
