{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Tray Configuration Interface            }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the battery tray icon configuration interface for global settings.
/// Per-device tray icons showing battery level for connected Bluetooth devices.
/// </summary>
unit App.BatteryTrayConfigIntf;

interface

uses
  Vcl.Graphics,
  App.ConfigEnums;

type
  /// <summary>
  /// Global battery tray icon settings (defaults).
  /// Used by: TBatteryTrayManager, TMainPresenter
  /// </summary>
  IBatteryTrayConfig = interface
    ['{B4773EEA-7EAA-4C0F-9E00-000000000001}']
    function GetShowBatteryTrayIcons: Boolean;
    function GetDefaultIconColor: TColor;
    function GetDefaultBackgroundColor: TColor;
    function GetDefaultShowNumericValue: Boolean;
    function GetDefaultLowBatteryThreshold: Integer;
    function GetDefaultNotifyLowBattery: Boolean;
    function GetDefaultNotifyFullyCharged: Boolean;
    function GetDefaultOutlineColorMode: TOutlineColorMode;
    function GetDefaultCustomOutlineColor: TColor;

    procedure SetShowBatteryTrayIcons(AValue: Boolean);
    procedure SetDefaultIconColor(AValue: TColor);
    procedure SetDefaultBackgroundColor(AValue: TColor);
    procedure SetDefaultShowNumericValue(AValue: Boolean);
    procedure SetDefaultLowBatteryThreshold(AValue: Integer);
    procedure SetDefaultNotifyLowBattery(AValue: Boolean);
    procedure SetDefaultNotifyFullyCharged(AValue: Boolean);
    procedure SetDefaultOutlineColorMode(AValue: TOutlineColorMode);
    procedure SetDefaultCustomOutlineColor(AValue: TColor);

    /// <summary>
    /// Master switch for battery tray icons feature.
    /// </summary>
    property ShowBatteryTrayIcons: Boolean read GetShowBatteryTrayIcons write SetShowBatteryTrayIcons;

    /// <summary>
    /// Default icon fill color for battery level indicator.
    /// Can be overridden per device.
    /// </summary>
    property DefaultIconColor: TColor read GetDefaultIconColor write SetDefaultIconColor;

    /// <summary>
    /// Default icon background color. Use clNone for transparent.
    /// Can be overridden per device.
    /// </summary>
    property DefaultBackgroundColor: TColor read GetDefaultBackgroundColor write SetDefaultBackgroundColor;

    /// <summary>
    /// Whether to show numeric percentage value instead of graphical battery.
    /// </summary>
    property DefaultShowNumericValue: Boolean read GetDefaultShowNumericValue write SetDefaultShowNumericValue;

    /// <summary>
    /// Default threshold (0-100) for low battery notification.
    /// </summary>
    property DefaultLowBatteryThreshold: Integer read GetDefaultLowBatteryThreshold write SetDefaultLowBatteryThreshold;

    /// <summary>
    /// Whether to show balloon notification when battery drops below threshold.
    /// </summary>
    property DefaultNotifyLowBattery: Boolean read GetDefaultNotifyLowBattery write SetDefaultNotifyLowBattery;

    /// <summary>
    /// Whether to show balloon notification when battery reaches 100%.
    /// </summary>
    property DefaultNotifyFullyCharged: Boolean read GetDefaultNotifyFullyCharged write SetDefaultNotifyFullyCharged;

    /// <summary>
    /// Controls how outline color is determined for battery icons.
    /// Auto = follow Windows dark/light mode.
    /// </summary>
    property DefaultOutlineColorMode: TOutlineColorMode read GetDefaultOutlineColorMode write SetDefaultOutlineColorMode;

    /// <summary>
    /// Custom outline color when mode is ocmCustom.
    /// </summary>
    property DefaultCustomOutlineColor: TColor read GetDefaultCustomOutlineColor write SetDefaultCustomOutlineColor;
  end;

implementation

end.
