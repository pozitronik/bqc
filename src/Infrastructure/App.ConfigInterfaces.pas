{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Interfaces                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines layer-specific configuration interfaces for dependency injection.
/// Each interface represents a focused set of configuration concerns,
/// following the Interface Segregation Principle (ISP).
/// </summary>
unit App.ConfigInterfaces;

interface

type
  // Forward declarations
  IAppConfig = interface;

  /// <summary>
  /// Window display mode.
  /// </summary>
  TWindowMode = (
    wmMenu,    // Popup menu style, hides on focus loss (0)
    wmWindow   // Normal window with title bar (1)
  );

  /// <summary>
  /// Position mode for window/menu placement.
  /// </summary>
  TPositionMode = (
    pmCoordinates,   // Use saved X,Y coordinates (0)
    pmNearTray,      // Near system tray icon (1)
    pmNearCursor,    // Near mouse cursor (2)
    pmCenterScreen   // Center of active screen (3)
  );

  /// <summary>
  /// Polling mode for device state detection.
  /// </summary>
  TPollingMode = (
    pmDisabled,  // No polling, event watcher only (0)
    pmFallback,  // Polling as backup if watcher fails (1)
    pmPrimary    // Polling only, no event watcher (2)
  );

  /// <summary>
  /// Last seen timestamp display format.
  /// </summary>
  TLastSeenFormat = (
    lsfRelative,  // "2 hours ago", "Yesterday" (0)
    lsfAbsolute   // "2024-12-21 15:30" (1)
  );

  /// <summary>
  /// Notification mode for events.
  /// </summary>
  TNotificationMode = (
    nmNone,      // No notification
    nmBalloon    // Balloon tip notification
  );

  /// <summary>
  /// Notification event types.
  /// </summary>
  TNotificationEvent = (
    neConnect,        // Device connected
    neDisconnect,     // Device disconnected
    neConnectFailed,  // Connection attempt failed
    neAutoConnect     // Auto-connect triggered
  );

  /// <summary>
  /// Per-device notification settings.
  /// Value of -1 means use global default.
  /// </summary>
  TDeviceNotificationConfig = record
    OnConnect: Integer;        // -1=Global, 0=None, 1=Balloon
    OnDisconnect: Integer;     // -1=Global, 0=None, 1=Balloon
    OnConnectFailed: Integer;  // -1=Global, 0=None, 1=Balloon
    OnAutoConnect: Integer;    // -1=Global, 0=None, 1=Balloon

    class function Default: TDeviceNotificationConfig; static;
  end;

  /// <summary>
  /// Per-device configuration.
  /// </summary>
  TDeviceConfig = record
    Address: UInt64;
    Name: string;
    Alias: string;
    Pinned: Boolean;
    Hidden: Boolean;
    AutoConnect: Boolean;
    ConnectionTimeout: Integer;
    ConnectionRetryCount: Integer;
    Notifications: TDeviceNotificationConfig;
    DeviceTypeOverride: Integer;
    LastSeen: TDateTime;

    class function Default(AAddress: UInt64): TDeviceConfig; static;
  end;

  //--------------------------------------------------------------------------
  // Layer-specific configuration interfaces
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
  /// Polling and event settings.
  /// Used by: TBluetoothService, SettingsPresenter
  /// </summary>
  IPollingConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000005}']
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);

    property PollingMode: TPollingMode read GetPollingMode write SetPollingMode;
    property PollingInterval: Integer read GetPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read GetEventDebounceMs;
  end;

  /// <summary>
  /// Logging settings.
  /// Used by: App.Logger (indirectly via TAppConfig)
  /// </summary>
  ILogConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000006}']
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);

    property LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;
    property LogFilename: string read GetLogFilename write SetLogFilename;
    property LogAppend: Boolean read GetLogAppend write SetLogAppend;
  end;

  /// <summary>
  /// Appearance settings (theme, colors, display options).
  /// Used by: UI.Theme, TDeviceListBox
  /// </summary>
  IAppearanceConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000007}']
    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);

    property ShowAddresses: Boolean read GetShowAddresses write SetShowAddresses;
    property Theme: string read GetTheme write SetTheme;
    property VsfDir: string read GetVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read GetShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read GetLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read GetShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read GetConnectedColor write SetConnectedColor;
  end;

  /// <summary>
  /// Layout settings (dimensions, fonts, spacing).
  /// Used by: TDeviceListBox
  /// </summary>
  ILayoutConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000008}']
    function GetItemHeight: Integer;
    function GetItemPadding: Integer;
    function GetItemMargin: Integer;
    function GetIconSize: Integer;
    function GetCornerRadius: Integer;
    function GetDeviceNameFontSize: Integer;
    function GetStatusFontSize: Integer;
    function GetAddressFontSize: Integer;
    function GetIconFontSize: Integer;
    function GetItemBorderWidth: Integer;
    function GetItemBorderColor: Integer;

    procedure SetItemHeight(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemMargin(AValue: Integer);
    procedure SetIconSize(AValue: Integer);
    procedure SetCornerRadius(AValue: Integer);
    procedure SetDeviceNameFontSize(AValue: Integer);
    procedure SetStatusFontSize(AValue: Integer);
    procedure SetAddressFontSize(AValue: Integer);
    procedure SetIconFontSize(AValue: Integer);
    procedure SetItemBorderWidth(AValue: Integer);
    procedure SetItemBorderColor(AValue: Integer);

    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemPadding: Integer read GetItemPadding write SetItemPadding;
    property ItemMargin: Integer read GetItemMargin write SetItemMargin;
    property IconSize: Integer read GetIconSize write SetIconSize;
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius;
    property DeviceNameFontSize: Integer read GetDeviceNameFontSize write SetDeviceNameFontSize;
    property StatusFontSize: Integer read GetStatusFontSize write SetStatusFontSize;
    property AddressFontSize: Integer read GetAddressFontSize write SetAddressFontSize;
    property IconFontSize: Integer read GetIconFontSize write SetIconFontSize;
    property ItemBorderWidth: Integer read GetItemBorderWidth write SetItemBorderWidth;
    property ItemBorderColor: Integer read GetItemBorderColor write SetItemBorderColor;
  end;

  /// <summary>
  /// Global connection settings (defaults).
  /// Used by: TBluetoothService, connection strategies
  /// </summary>
  IConnectionConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000009}']
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);

    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read GetConnectionRetryCount write SetConnectionRetryCount;
  end;

  /// <summary>
  /// Global notification settings (defaults).
  /// Used by: TMainPresenter
  /// </summary>
  INotificationConfig = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000A}']
    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;

    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

    property NotifyOnConnect: TNotificationMode read GetNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read GetNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read GetNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read GetNotifyOnAutoConnect write SetNotifyOnAutoConnect;
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
  // Repository Interfaces (for persistence separation)
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
  /// </summary>
  IDeviceConfigRepository = interface
    ['{C2D3E4F5-3333-4444-5555-666677778888}']

    /// <summary>
    /// Sets reference to global app config for effective value resolution.
    /// </summary>
    procedure SetGlobalConfig(AConfig: IAppConfig);

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

    /// <summary>
    /// Gets effective notification mode for a device and event.
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
  // Logger interface
  //--------------------------------------------------------------------------

  /// <summary>
  /// Interface for application logging.
  /// Provides structured logging with source identification.
  /// </summary>
  ILogger = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000D}']

    /// <summary>
    /// Logs a message with optional source identifier.
    /// </summary>
    /// <param name="AMessage">The message to log.</param>
    /// <param name="ASource">Source identifier (e.g., 'MainForm', 'Service').</param>
    procedure Log(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Logs a formatted message with optional source identifier.
    /// </summary>
    /// <param name="AFormat">Format string.</param>
    /// <param name="AArgs">Format arguments.</param>
    /// <param name="ASource">Source identifier.</param>
    procedure LogFmt(const AFormat: string; const AArgs: array of const;
      const ASource: string = '');

    /// <summary>
    /// Returns true if logging is currently enabled.
    /// </summary>
    function IsEnabled: Boolean;
  end;

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

{ TDeviceNotificationConfig }

class function TDeviceNotificationConfig.Default: TDeviceNotificationConfig;
begin
  Result.OnConnect := -1;
  Result.OnDisconnect := -1;
  Result.OnConnectFailed := -1;
  Result.OnAutoConnect := -1;
end;

{ TDeviceConfig }

class function TDeviceConfig.Default(AAddress: UInt64): TDeviceConfig;
begin
  Result.Address := AAddress;
  Result.Name := '';
  Result.Alias := '';
  Result.Pinned := False;
  Result.Hidden := False;
  Result.AutoConnect := False;
  Result.ConnectionTimeout := -1;
  Result.ConnectionRetryCount := -1;
  Result.Notifications := TDeviceNotificationConfig.Default;
  Result.DeviceTypeOverride := -1;
  Result.LastSeen := 0;
end;

end.
