{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Interfaces                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Central configuration interfaces unit.
/// Re-exports types from domain-specific units for backwards compatibility.
/// New code should consider using specific units directly:
///   - App.ConfigEnums: TWindowMode, TPositionMode, TPollingMode, etc.
///   - App.DeviceConfigTypes: TDeviceConfig, TDeviceNotificationConfig
///   - App.LayoutConfigIntf: ILayoutConfig
///   - App.AppearanceConfigIntf: IAppearanceConfig
///   - App.ConnectionConfigIntf: IConnectionConfig, IPollingConfig
///   - App.NotificationConfigIntf: INotificationConfig
///   - App.LogConfigIntf: ILogConfig, ILogger
/// </summary>
unit App.ConfigInterfaces;

interface

uses
  App.ConfigEnums,
  App.DeviceConfigTypes,
  App.LayoutConfigIntf,
  App.AppearanceConfigIntf,
  App.ConnectionConfigIntf,
  App.NotificationConfigIntf,
  App.LogConfigIntf;

type
  // Re-export enums for backwards compatibility
  TWindowMode = App.ConfigEnums.TWindowMode;
  TPositionMode = App.ConfigEnums.TPositionMode;
  TPollingMode = App.ConfigEnums.TPollingMode;
  TLastSeenFormat = App.ConfigEnums.TLastSeenFormat;
  TNotificationMode = App.ConfigEnums.TNotificationMode;
  TNotificationEvent = App.ConfigEnums.TNotificationEvent;

  // Re-export device config types for backwards compatibility
  TDeviceNotificationConfig = App.DeviceConfigTypes.TDeviceNotificationConfig;
  TDeviceConfig = App.DeviceConfigTypes.TDeviceConfig;

  // Re-export interfaces for backwards compatibility
  ILayoutConfig = App.LayoutConfigIntf.ILayoutConfig;
  IAppearanceConfig = App.AppearanceConfigIntf.IAppearanceConfig;
  IConnectionConfig = App.ConnectionConfigIntf.IConnectionConfig;
  IPollingConfig = App.ConnectionConfigIntf.IPollingConfig;
  INotificationConfig = App.NotificationConfigIntf.INotificationConfig;
  ILogConfig = App.LogConfigIntf.ILogConfig;
  ILogger = App.LogConfigIntf.ILogger;

  // Forward declaration for IAppConfig (used by ISettingsRepository and IDeviceConfigRepository)
  IAppConfig = interface;

  //--------------------------------------------------------------------------
  // Configuration interfaces that remain in this unit
  //--------------------------------------------------------------------------

  /// <summary>
  /// General application settings.
  /// Used by: MainForm, MainPresenter, SettingsPresenter
  /// </summary>
  IGeneralConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000001}']
    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;

    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);

    property WindowMode: TWindowMode read GetWindowMode write SetWindowMode;
    property OnTop: Boolean read GetOnTop write SetOnTop;
    property Autostart: Boolean read GetAutostart write SetAutostart;
  end;

  /// <summary>
  /// Window behavior settings.
  /// Used by: MainForm, SettingsPresenter
  /// </summary>
  IWindowConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000002}']
    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;

    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);

    property MinimizeToTray: Boolean read GetMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read GetCloseToTray write SetCloseToTray;
    property MenuHideOnFocusLoss: Boolean read GetMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;
  end;

  /// <summary>
  /// Window position settings.
  /// Used by: UI.WindowPositioner, SettingsPresenter
  /// </summary>
  IPositionConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000003}']
    function GetPositionMode: TPositionMode;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetPositionW: Integer;
    function GetPositionH: Integer;

    procedure SetPositionMode(AValue: TPositionMode);
    procedure SetPositionX(AValue: Integer);
    procedure SetPositionY(AValue: Integer);
    procedure SetPositionW(AValue: Integer);
    procedure SetPositionH(AValue: Integer);

    property PositionMode: TPositionMode read GetPositionMode write SetPositionMode;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
    property PositionW: Integer read GetPositionW write SetPositionW;
    property PositionH: Integer read GetPositionH write SetPositionH;
  end;

  /// <summary>
  /// Hotkey settings.
  /// Used by: UI.HotkeyManager, SettingsPresenter
  /// </summary>
  IHotkeyConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000004}']
    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;

    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);

    property Hotkey: string read GetHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read GetUseLowLevelHook write SetUseLowLevelHook;
  end;

  /// <summary>
  /// Per-device configuration provider.
  /// Used by: TDeviceListBox, TMainPresenter, TBluetoothService
  /// </summary>
  IDeviceConfigProvider = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000B}']
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);
    procedure RemoveDeviceConfig(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
    function GetConfiguredDeviceAddresses: TArray<UInt64>;

    /// <summary>
    /// Gets effective notification mode for a device and event.
    /// Resolves per-device override or returns global default.
    /// </summary>
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;

    /// <summary>
    /// Gets effective connection timeout for a device.
    /// </summary>
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;

    /// <summary>
    /// Gets effective retry count for a device.
    /// </summary>
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
  end;

  //--------------------------------------------------------------------------
  // Repository Interfaces
  //--------------------------------------------------------------------------

  /// <summary>
  /// Repository interface for application settings persistence.
  /// Separates INI file handling from configuration state.
  /// </summary>
  ISettingsRepository = interface
    ['{B1C2D3E4-2222-3333-4444-555566667777}']

    /// <summary>
    /// Loads settings from storage into the config object.
    /// </summary>
    procedure LoadSettings(AConfig: IAppConfig);

    /// <summary>
    /// Saves settings from the config object to storage.
    /// </summary>
    procedure SaveSettings(AConfig: IAppConfig);

    /// <summary>
    /// Gets the path to the configuration file.
    /// </summary>
    function GetConfigPath: string;

    property ConfigPath: string read GetConfigPath;
  end;

  /// <summary>
  /// Repository interface for per-device configuration.
  /// Separates device config storage from main configuration.
  /// Handles only storage/retrieval - effective value resolution is done by IDeviceConfigProvider.
  /// </summary>
  IDeviceConfigRepository = interface
    ['{C2D3E4F5-3333-4444-5555-666677778888}']

    /// <summary>
    /// Gets configuration for a specific device.
    /// Returns default config if device not found.
    /// </summary>
    function GetConfig(AAddress: UInt64): TDeviceConfig;

    /// <summary>
    /// Sets configuration for a device.
    /// </summary>
    procedure SetConfig(const AConfig: TDeviceConfig);

    /// <summary>
    /// Removes configuration for a device.
    /// </summary>
    procedure Remove(AAddress: UInt64);

    /// <summary>
    /// Registers a newly discovered device.
    /// Creates default config if not exists, updates LastSeen if exists.
    /// </summary>
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);

    /// <summary>
    /// Gets all configured device addresses.
    /// </summary>
    function GetAllAddresses: TArray<UInt64>;

    /// <summary>
    /// Gets all device configurations.
    /// </summary>
    function GetAll: TArray<TDeviceConfig>;

    /// <summary>
    /// Loads device configurations from INI file.
    /// </summary>
    procedure LoadFrom(AIni: TObject);  // TMemIniFile

    /// <summary>
    /// Saves device configurations to INI file.
    /// </summary>
    procedure SaveTo(AIni: TObject);  // TMemIniFile

    /// <summary>
    /// Returns true if any device config has been modified.
    /// </summary>
    function IsModified: Boolean;

    /// <summary>
    /// Clears the modified flag.
    /// </summary>
    procedure ClearModified;
  end;

  //--------------------------------------------------------------------------
  // Aggregate Configuration Interface
  //--------------------------------------------------------------------------

  /// <summary>
  /// Full configuration interface for settings forms and persistence.
  /// Combines all config interfaces. Used by: SettingsForm, App.Config internals
  /// </summary>
  IAppConfig = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000C}']
    function GetConfigPath: string;
    function GetModified: Boolean;

    procedure Load;
    procedure Save;
    procedure SaveIfModified;

    // Aggregate interface access
    function AsGeneralConfig: IGeneralConfig;
    function AsWindowConfig: IWindowConfig;
    function AsPositionConfig: IPositionConfig;
    function AsHotkeyConfig: IHotkeyConfig;
    function AsPollingConfig: IPollingConfig;
    function AsLogConfig: ILogConfig;
    function AsAppearanceConfig: IAppearanceConfig;
    function AsLayoutConfig: ILayoutConfig;
    function AsConnectionConfig: IConnectionConfig;
    function AsNotificationConfig: INotificationConfig;
    function AsDeviceConfigProvider: IDeviceConfigProvider;

    property ConfigPath: string read GetConfigPath;
    property Modified: Boolean read GetModified;
  end;

implementation

end.
