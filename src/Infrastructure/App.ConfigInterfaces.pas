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
  /// Used by: MainForm, MainPresenter
  /// </summary>
  IGeneralConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000001}']
    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;

    property WindowMode: TWindowMode read GetWindowMode;
    property OnTop: Boolean read GetOnTop;
    property Autostart: Boolean read GetAutostart;
  end;

  /// <summary>
  /// Window behavior settings.
  /// Used by: MainForm
  /// </summary>
  IWindowConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000002}']
    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;

    property MinimizeToTray: Boolean read GetMinimizeToTray;
    property CloseToTray: Boolean read GetCloseToTray;
    property MenuHideOnFocusLoss: Boolean read GetMenuHideOnFocusLoss;
  end;

  /// <summary>
  /// Window position settings.
  /// Used by: UI.WindowPositioner
  /// </summary>
  IPositionConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000003}']
    function GetPositionMode: TPositionMode;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetPositionW: Integer;
    function GetPositionH: Integer;

    procedure SetPositionX(AValue: Integer);
    procedure SetPositionY(AValue: Integer);
    procedure SetPositionW(AValue: Integer);
    procedure SetPositionH(AValue: Integer);

    property PositionMode: TPositionMode read GetPositionMode;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
    property PositionW: Integer read GetPositionW write SetPositionW;
    property PositionH: Integer read GetPositionH write SetPositionH;
  end;

  /// <summary>
  /// Hotkey settings.
  /// Used by: UI.HotkeyManager
  /// </summary>
  IHotkeyConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000004}']
    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;

    property Hotkey: string read GetHotkey;
    property UseLowLevelHook: Boolean read GetUseLowLevelHook;
  end;

  /// <summary>
  /// Polling and event settings.
  /// Used by: TBluetoothService
  /// </summary>
  IPollingConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000005}']
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    property PollingMode: TPollingMode read GetPollingMode;
    property PollingInterval: Integer read GetPollingInterval;
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

    property LogEnabled: Boolean read GetLogEnabled;
    property LogFilename: string read GetLogFilename;
    property LogAppend: Boolean read GetLogAppend;
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

    property ShowAddresses: Boolean read GetShowAddresses;
    property Theme: string read GetTheme;
    property VsfDir: string read GetVsfDir;
    property ShowLastSeen: Boolean read GetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read GetLastSeenFormat;
    property ShowDeviceIcons: Boolean read GetShowDeviceIcons;
    property ConnectedColor: Integer read GetConnectedColor;
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

    property ItemHeight: Integer read GetItemHeight;
    property ItemPadding: Integer read GetItemPadding;
    property ItemMargin: Integer read GetItemMargin;
    property IconSize: Integer read GetIconSize;
    property CornerRadius: Integer read GetCornerRadius;
    property DeviceNameFontSize: Integer read GetDeviceNameFontSize;
    property StatusFontSize: Integer read GetStatusFontSize;
    property AddressFontSize: Integer read GetAddressFontSize;
    property IconFontSize: Integer read GetIconFontSize;
    property ItemBorderWidth: Integer read GetItemBorderWidth;
    property ItemBorderColor: Integer read GetItemBorderColor;
  end;

  /// <summary>
  /// Global connection settings (defaults).
  /// Used by: TBluetoothService, connection strategies
  /// </summary>
  IConnectionConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000009}']
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;

    property ConnectionTimeout: Integer read GetConnectionTimeout;
    property ConnectionRetryCount: Integer read GetConnectionRetryCount;
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

    property NotifyOnConnect: TNotificationMode read GetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read GetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read GetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read GetNotifyOnAutoConnect;
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
    procedure LoadSettings(AConfig: TObject);

    /// <summary>
    /// Saves settings from the config object to storage.
    /// </summary>
    procedure SaveSettings(AConfig: TObject);

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
