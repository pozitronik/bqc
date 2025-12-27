{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Interfaces                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Core configuration interfaces for the application.
/// Contains interfaces that don't belong to specific domain units:
///   - IGeneralConfig, IWindowConfig, IPositionConfig, IHotkeyConfig
///   - IDeviceConfigQuery, IDeviceConfigMutation, IDeviceConfigProvider
///   - ISettingsRepository, IDeviceConfigStorage, IDeviceConfigPersistence
///   - IDeviceConfigRepository, IAppConfig
/// </summary>
unit App.ConfigInterfaces;

interface

uses
  System.IniFiles,
  App.ConfigEnums,
  App.DeviceConfigTypes,
  App.LayoutConfigIntf,
  App.AppearanceConfigIntf,
  App.ConnectionConfigIntf,
  App.NotificationConfigIntf,
  App.LogConfigIntf,
  App.BatteryTrayConfigIntf;

type
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
  /// Read-only access to device configuration.
  /// Use this for consumers that only need to query device settings.
  /// Used by: TDeviceDisplayItemBuilder, TBluetoothService (read path)
  /// </summary>
  IDeviceConfigQuery = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000D}']
    /// <summary>
    /// Gets configuration for a specific device by address.
    /// Returns default config if device not configured.
    /// </summary>
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;

    /// <summary>
    /// Gets all configured device addresses.
    /// </summary>
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

  /// <summary>
  /// Write access to device configuration.
  /// Use this for consumers that need to modify device settings.
  /// Used by: TSettingsPresenter (device settings), TMainPresenter (device registration)
  /// </summary>
  IDeviceConfigMutation = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000E}']
    /// <summary>
    /// Sets configuration for a device.
    /// </summary>
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);

    /// <summary>
    /// Removes configuration for a device.
    /// </summary>
    procedure RemoveDeviceConfig(AAddress: UInt64);

    /// <summary>
    /// Registers a newly discovered device.
    /// Creates default config if not exists, updates LastSeen if exists.
    /// </summary>
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
  end;

  /// <summary>
  /// Full device configuration provider combining query and mutation.
  /// Used by: TMainPresenter, TSettingsPresenter, components needing both read and write access.
  /// </summary>
  IDeviceConfigProvider = interface(IDeviceConfigQuery)
    ['{A1B2C3D4-1111-1111-1111-00000000000B}']
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);
    procedure RemoveDeviceConfig(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
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
  /// Pure domain interface for device configuration storage.
  /// Used by: TDeviceConfigProvider (only needs domain operations).
  /// Does NOT expose persistence details (ISP-compliant).
  /// </summary>
  IDeviceConfigStorage = interface
    ['{C2D3E4F5-4444-5555-6666-777788889999}']

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
  end;

  /// <summary>
  /// Persistence interface for device configuration.
  /// Used by: TIniSettingsRepository (coordinates INI load/save).
  /// Separated from domain operations per ISP.
  /// </summary>
  IDeviceConfigPersistence = interface
    ['{C2D3E4F5-5555-6666-7777-888899990000}']

    /// <summary>
    /// Loads device configurations from INI file.
    /// </summary>
    procedure LoadFrom(AIni: TCustomIniFile);

    /// <summary>
    /// Saves device configurations to INI file.
    /// </summary>
    procedure SaveTo(AIni: TCustomIniFile);

    /// <summary>
    /// Returns true if any device config has been modified.
    /// </summary>
    function IsModified: Boolean;

    /// <summary>
    /// Clears the modified flag.
    /// </summary>
    procedure ClearModified;
  end;

  /// <summary>
  /// Combined repository interface for per-device configuration.
  /// Extends both IDeviceConfigStorage and IDeviceConfigPersistence.
  /// Used by: Bootstrap (creates single instance implementing both).
  /// For focused access, use IDeviceConfigStorage or IDeviceConfigPersistence directly.
  /// </summary>
  IDeviceConfigRepository = interface(IDeviceConfigStorage)
    ['{C2D3E4F5-3333-4444-5555-666677778888}']

    // Persistence operations (from IDeviceConfigPersistence)
    procedure LoadFrom(AIni: TCustomIniFile);
    procedure SaveTo(AIni: TCustomIniFile);
    function IsModified: Boolean;
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
    function AsBatteryTrayConfig: IBatteryTrayConfig;
    function AsDeviceConfigProvider: IDeviceConfigProvider;

    property ConfigPath: string read GetConfigPath;
    property Modified: Boolean read GetModified;
  end;

implementation

end.
