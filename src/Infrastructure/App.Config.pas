{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Configuration                       }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.Config;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Win.Registry,
  System.Generics.Collections,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Application configuration manager.
  /// Singleton class that handles loading/saving settings from INI file.
  /// </summary>
  TAppConfig = class
  private
    FConfigPath: string;
    FModified: Boolean;
    FDevices: TDictionary<UInt64, TDeviceConfig>;

    // [General]
    FWindowMode: TWindowMode;
    FOnTop: Boolean;
    FAutostart: Boolean;

    // [Window]
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;

    // [Menu]
    FMenuHideOnFocusLoss: Boolean;

    // [Hotkey]
    FHotkey: string;
    FUseLowLevelHook: Boolean;

    // [Position]
    FPositionMode: TPositionMode;
    FPositionX: Integer;
    FPositionY: Integer;
    FPositionW: Integer;
    FPositionH: Integer;

    // [Polling]
    FPollingMode: TPollingMode;
    FPollingInterval: Integer;
    FEventDebounceMs: Integer;

    // [Log]
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;

    // [Appearance]
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;
    FShowLastSeen: Boolean;
    FLastSeenFormat: TLastSeenFormat;
    FShowDeviceIcons: Boolean;
    FConnectedColor: Integer;

    // [Layout]
    FItemHeight: Integer;
    FItemPadding: Integer;
    FItemMargin: Integer;
    FIconSize: Integer;
    FCornerRadius: Integer;
    FDeviceNameFontSize: Integer;
    FStatusFontSize: Integer;
    FAddressFontSize: Integer;
    FIconFontSize: Integer;
    FItemBorderWidth: Integer;
    FItemBorderColor: Integer;

    // [Device] - global defaults
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;
    FNotifyOnConnect: TNotificationMode;
    FNotifyOnDisconnect: TNotificationMode;
    FNotifyOnConnectFailed: TNotificationMode;
    FNotifyOnAutoConnect: TNotificationMode;

    procedure SetDefaults;
    procedure LoadDevices(AIni: TMemIniFile);
    procedure SaveDevices(AIni: TMemIniFile);

    // Property setters with modification tracking
    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);
    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);
    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);
    procedure SetPositionMode(AValue: TPositionMode);
    procedure SetPositionX(AValue: Integer);
    procedure SetPositionY(AValue: Integer);
    procedure SetPositionW(AValue: Integer);
    procedure SetPositionH(AValue: Integer);
    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);
    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);
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
    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Loads configuration from INI file. Creates file with defaults if not exists.
    /// </summary>
    procedure Load;

    /// <summary>
    /// Saves configuration to INI file.
    /// </summary>
    procedure Save;

    /// <summary>
    /// Saves configuration only if modified.
    /// </summary>
    procedure SaveIfModified;

    /// <summary>
    /// Gets device-specific configuration.
    /// Returns default config if device not configured.
    /// </summary>
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;

    /// <summary>
    /// Sets device-specific configuration.
    /// </summary>
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);

    /// <summary>
    /// Removes device-specific configuration (resets to defaults).
    /// </summary>
    procedure RemoveDeviceConfig(AAddress: UInt64);

    /// <summary>
    /// Registers a discovered device. Creates new config if not exists,
    /// or updates Name and LastSeen if already registered.
    /// </summary>
    /// <param name="AAddress">Device Bluetooth address.</param>
    /// <param name="AName">Original device name from Windows.</param>
    /// <param name="ALastSeen">Timestamp when device was discovered.</param>
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);

    /// <summary>
    /// Returns all configured device addresses.
    /// </summary>
    function GetConfiguredDeviceAddresses: TArray<UInt64>;

    /// <summary>
    /// Path to the configuration file.
    /// </summary>
    property ConfigPath: string read FConfigPath;

    /// <summary>
    /// True if configuration was modified since last save.
    /// </summary>
    property Modified: Boolean read FModified;

    // [General]
    property WindowMode: TWindowMode read FWindowMode write SetWindowMode;
    property OnTop: Boolean read FOnTop write SetOnTop;
    property Autostart: Boolean read FAutostart write SetAutostart;

    // [Window]
    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;

    // [Menu]
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;

    // [Hotkey]
    property Hotkey: string read FHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read FUseLowLevelHook write SetUseLowLevelHook;

    // [Position]
    property PositionMode: TPositionMode read FPositionMode write SetPositionMode;
    property PositionX: Integer read FPositionX write SetPositionX;
    property PositionY: Integer read FPositionY write SetPositionY;
    property PositionW: Integer read FPositionW write SetPositionW;
    property PositionH: Integer read FPositionH write SetPositionH;

    // [Polling]
    property PollingMode: TPollingMode read FPollingMode write SetPollingMode;
    property PollingInterval: Integer read FPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read FEventDebounceMs write SetEventDebounceMs;

    // [Log]
    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogFilename: string read FLogFilename write SetLogFilename;
    property LogAppend: Boolean read FLogAppend write SetLogAppend;

    // [Appearance]
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    property VsfDir: string read FVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write SetConnectedColor;

    // [Layout]
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ItemPadding: Integer read FItemPadding write SetItemPadding;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property IconSize: Integer read FIconSize write SetIconSize;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius;
    property DeviceNameFontSize: Integer read FDeviceNameFontSize write SetDeviceNameFontSize;
    property StatusFontSize: Integer read FStatusFontSize write SetStatusFontSize;
    property AddressFontSize: Integer read FAddressFontSize write SetAddressFontSize;
    property IconFontSize: Integer read FIconFontSize write SetIconFontSize;
    property ItemBorderWidth: Integer read FItemBorderWidth write SetItemBorderWidth;
    property ItemBorderColor: Integer read FItemBorderColor write SetItemBorderColor;

    // [Device] - global defaults
    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
    property NotifyOnConnect: TNotificationMode read FNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read FNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read FNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read FNotifyOnAutoConnect write SetNotifyOnAutoConnect;

    /// <summary>
    /// Gets effective notification mode for a device and event.
    /// Resolves per-device override or returns global default.
    /// </summary>
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;

    /// <summary>
    /// Gets effective connection timeout for a device.
    /// Resolves per-device override or returns global default.
    /// </summary>
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;

    /// <summary>
    /// Gets effective retry count for a device.
    /// Resolves per-device override or returns global default.
    /// </summary>
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;

    // Compatibility properties (map old names to new)
    property StayOnTop: Boolean read FOnTop write SetOnTop;
  end;

const
  // Layout value limits (shared with UI TUpDown controls)
  MIN_ITEM_HEIGHT = 30;
  MAX_ITEM_HEIGHT = 200;
  DEF_ITEM_HEIGHT = 70;

  MIN_ITEM_PADDING = 0;
  MAX_ITEM_PADDING = 50;
  DEF_ITEM_PADDING = 6;

  MIN_ITEM_MARGIN = 0;
  MAX_ITEM_MARGIN = 30;
  DEF_ITEM_MARGIN = 4;

  MIN_ICON_SIZE = 16;
  MAX_ICON_SIZE = 64;
  DEF_ICON_SIZE = 46;

  MIN_CORNER_RADIUS = 0;
  MAX_CORNER_RADIUS = 30;
  DEF_CORNER_RADIUS = 8;

  MIN_ITEM_BORDER_WIDTH = 0;
  MAX_ITEM_BORDER_WIDTH = 20;
  DEF_ITEM_BORDER_WIDTH = 0;
  DEF_ITEM_BORDER_COLOR = $00808080;  // Gray

  // Font size limits
  MIN_DEVICE_NAME_FONT_SIZE = 6;
  MAX_DEVICE_NAME_FONT_SIZE = 24;
  DEF_DEVICE_NAME_FONT_SIZE = 12;

  MIN_STATUS_FONT_SIZE = 6;
  MAX_STATUS_FONT_SIZE = 24;
  DEF_STATUS_FONT_SIZE = 10;

  MIN_ADDRESS_FONT_SIZE = 6;
  MAX_ADDRESS_FONT_SIZE = 24;
  DEF_ADDRESS_FONT_SIZE = 8;

  MIN_ICON_FONT_SIZE = 8;
  MAX_ICON_FONT_SIZE = 32;
  DEF_ICON_FONT_SIZE = 16;

  // Connection limits
  MIN_CONNECTION_TIMEOUT = 0;
  MAX_CONNECTION_TIMEOUT = 60000;
  DEF_CONNECTION_TIMEOUT = 10000;

  MIN_CONNECTION_RETRY_COUNT = 0;
  MAX_CONNECTION_RETRY_COUNT = 10;
  DEF_CONNECTION_RETRY_COUNT = 2;

  MIN_POLLING_INTERVAL = 0;
  MAX_POLLING_INTERVAL = 10000;
  DEF_POLLING_INTERVAL = 2000;

  // Event debounce limits (filters duplicate events from multiple Windows sources)
  MIN_EVENT_DEBOUNCE_MS = 0;
  MAX_EVENT_DEBOUNCE_MS = 5000;
  DEF_EVENT_DEBOUNCE_MS = 500;

  // Appearance defaults
  DEF_CONNECTED_COLOR = $00008000;  // Dark green (BGR format)

/// <summary>
/// Returns the singleton configuration instance.
/// </summary>
function Config: TAppConfig;

implementation

uses
  System.Math,
  System.DateUtils,
  App.Logger,
  App.Autostart;

const
  // Section names
  SEC_GENERAL = 'General';
  SEC_WINDOW = 'Window';
  SEC_MENU = 'Menu';
  SEC_HOTKEY = 'Hotkey';
  SEC_POSITION = 'Position';
  SEC_POLLING = 'Polling';
  SEC_LOG = 'Log';
  SEC_APPEARANCE = 'Appearance';
  SEC_LAYOUT = 'Layout';
  SEC_DEVICE = 'Device';
  SEC_DEVICE_PREFIX = 'Device.';

  // INI key names - [General]
  KEY_WINDOW = 'Window';
  KEY_ON_TOP = 'OnTop';
  KEY_AUTOSTART = 'Autostart';

  // INI key names - [Window]
  KEY_MINIMIZE_TO_TRAY = 'MinimizeToTray';
  KEY_CLOSE_TO_TRAY = 'CloseToTray';

  // INI key names - [Menu]
  KEY_HIDE_ON_FOCUS_LOSS = 'HideOnFocusLoss';

  // INI key names - [Hotkey]
  KEY_GLOBAL_HOTKEY = 'GlobalHotkey';
  KEY_USE_LOW_LEVEL_HOOK = 'UseLowLevelHook';

  // INI key names - [Position]
  KEY_MODE = 'Mode';
  KEY_X = 'X';
  KEY_Y = 'Y';
  KEY_W = 'W';
  KEY_H = 'H';

  // INI key names - [Polling]
  KEY_INTERVAL = 'Interval';
  KEY_EVENT_DEBOUNCE_MS = 'EventDebounceMs';

  // INI key names - [Log]
  KEY_ENABLED = 'Enabled';
  KEY_FILENAME = 'Filename';
  KEY_APPEND = 'Append';

  // INI key names - [Appearance]
  KEY_SHOW_ADDRESSES = 'ShowAddresses';
  KEY_THEME = 'Theme';
  KEY_VSF_DIR = 'VsfDir';
  KEY_SHOW_LAST_SEEN = 'ShowLastSeen';
  KEY_LAST_SEEN_FORMAT = 'LastSeenFormat';
  KEY_SHOW_DEVICE_ICONS = 'ShowDeviceIcons';
  KEY_CONNECTED_COLOR = 'ConnectedColor';

  // INI key names - [Layout]
  KEY_ITEM_HEIGHT = 'ItemHeight';
  KEY_ITEM_PADDING = 'ItemPadding';
  KEY_ITEM_MARGIN = 'ItemMargin';
  KEY_ICON_SIZE = 'IconSize';
  KEY_CORNER_RADIUS = 'CornerRadius';
  KEY_DEVICE_NAME_FONT_SIZE = 'DeviceNameFontSize';
  KEY_STATUS_FONT_SIZE = 'StatusFontSize';
  KEY_ADDRESS_FONT_SIZE = 'AddressFontSize';
  KEY_ICON_FONT_SIZE = 'IconFontSize';
  KEY_ITEM_BORDER_WIDTH = 'ItemBorderWidth';
  KEY_ITEM_BORDER_COLOR = 'ItemBorderColor';

  // INI key names - [Device] (global defaults and per-device)
  KEY_CONNECTION_TIMEOUT = 'ConnectionTimeout';
  KEY_CONNECTION_RETRY_COUNT = 'ConnectionRetryCount';
  KEY_NOTIFY_ON_CONNECT = 'NotifyOnConnect';
  KEY_NOTIFY_ON_DISCONNECT = 'NotifyOnDisconnect';
  KEY_NOTIFY_ON_CONNECT_FAILED = 'NotifyOnConnectFailed';
  KEY_NOTIFY_ON_AUTO_CONNECT = 'NotifyOnAutoConnect';

  // INI key names - [Device.XXX] (per-device only)
  KEY_NAME = 'Name';
  KEY_ALIAS = 'Alias';
  KEY_PINNED = 'Pinned';
  KEY_HIDDEN = 'Hidden';
  KEY_AUTO_CONNECT = 'AutoConnect';
  KEY_DEVICE_TYPE_OVERRIDE = 'DeviceTypeOverride';
  KEY_LAST_SEEN = 'LastSeen';

  // Default values
  DEF_WINDOW_MODE = wmWindow;
  DEF_ON_TOP = False;
  DEF_AUTOSTART = False;
  DEF_MINIMIZE_TO_TRAY = True;
  DEF_CLOSE_TO_TRAY = True;
  DEF_MENU_HIDE_ON_FOCUS_LOSS = True;
  DEF_HOTKEY = 'Win+K';
  DEF_USE_LOW_LEVEL_HOOK = True;
  DEF_POSITION_MODE = pmCoordinates;
  DEF_POSITION_X = -1;
  DEF_POSITION_Y = -1;
  DEF_POSITION_W = -1;
  DEF_POSITION_H = -1;
  DEF_POLLING_MODE = pmFallback;
  DEF_LOG_ENABLED = False;
  DEF_LOG_FILENAME = 'bqc.log';
  DEF_LOG_APPEND = False;
  DEF_SHOW_ADDRESSES = False;
  DEF_THEME = '';
  DEF_VSF_DIR = 'themes';

  DEF_NOTIFY_ON_CONNECT = nmBalloon;
  DEF_NOTIFY_ON_DISCONNECT = nmBalloon;
  DEF_NOTIFY_ON_CONNECT_FAILED = nmBalloon;
  DEF_NOTIFY_ON_AUTO_CONNECT = nmBalloon;

  // [Appearance] additional defaults
  DEF_SHOW_LAST_SEEN = False;
  DEF_LAST_SEEN_FORMAT = lsfRelative;
  DEF_SHOW_DEVICE_ICONS = True;

var
  GConfig: TAppConfig = nil;

function Config: TAppConfig;
begin
  if GConfig = nil then
  begin
    GConfig := TAppConfig.Create;
    GConfig.Load;
  end;
  Result := GConfig;
end;

{ TAppConfig }

constructor TAppConfig.Create;
var
  ExePath: string;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TDeviceConfig>.Create;

  // Config file next to executable
  ExePath := ExtractFilePath(ParamStr(0));
  FConfigPath := ExePath + 'bqc.ini';

  SetDefaults;
end;

destructor TAppConfig.Destroy;
begin
  SaveIfModified;
  FDevices.Free;
  inherited Destroy;
end;

procedure TAppConfig.SetDefaults;
begin
  // [General]
  FWindowMode := DEF_WINDOW_MODE;
  FOnTop := DEF_ON_TOP;
  FAutostart := DEF_AUTOSTART;

  // [Window]
  FMinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := DEF_CLOSE_TO_TRAY;

  // [Menu]
  FMenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;

  // [Hotkey]
  FHotkey := DEF_HOTKEY;
  FUseLowLevelHook := DEF_USE_LOW_LEVEL_HOOK;

  // [Position]
  FPositionMode := DEF_POSITION_MODE;
  FPositionX := DEF_POSITION_X;
  FPositionY := DEF_POSITION_Y;
  FPositionW := DEF_POSITION_W;
  FPositionH := DEF_POSITION_H;

  // [Polling]
  FPollingMode := DEF_POLLING_MODE;
  FPollingInterval := DEF_POLLING_INTERVAL;
  FEventDebounceMs := DEF_EVENT_DEBOUNCE_MS;

  // [Log]
  FLogEnabled := DEF_LOG_ENABLED;
  FLogFilename := DEF_LOG_FILENAME;
  FLogAppend := DEF_LOG_APPEND;

  // [Appearance]
  FShowAddresses := DEF_SHOW_ADDRESSES;
  FTheme := DEF_THEME;
  FVsfDir := DEF_VSF_DIR;
  FShowLastSeen := DEF_SHOW_LAST_SEEN;
  FLastSeenFormat := DEF_LAST_SEEN_FORMAT;
  FShowDeviceIcons := DEF_SHOW_DEVICE_ICONS;
  FConnectedColor := DEF_CONNECTED_COLOR;

  // [Layout]
  FItemHeight := DEF_ITEM_HEIGHT;
  FItemPadding := DEF_ITEM_PADDING;
  FItemMargin := DEF_ITEM_MARGIN;
  FIconSize := DEF_ICON_SIZE;
  FCornerRadius := DEF_CORNER_RADIUS;
  FDeviceNameFontSize := DEF_DEVICE_NAME_FONT_SIZE;
  FStatusFontSize := DEF_STATUS_FONT_SIZE;
  FAddressFontSize := DEF_ADDRESS_FONT_SIZE;
  FIconFontSize := DEF_ICON_FONT_SIZE;
  FItemBorderWidth := DEF_ITEM_BORDER_WIDTH;
  FItemBorderColor := DEF_ITEM_BORDER_COLOR;

  // [Device]
  FConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  FConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
  FNotifyOnConnect := DEF_NOTIFY_ON_CONNECT;
  FNotifyOnDisconnect := DEF_NOTIFY_ON_DISCONNECT;
  FNotifyOnConnectFailed := DEF_NOTIFY_ON_CONNECT_FAILED;
  FNotifyOnAutoConnect := DEF_NOTIFY_ON_AUTO_CONNECT;

  FDevices.Clear;
  FModified := False;
end;

procedure TAppConfig.Load;
var
  Ini: TMemIniFile;
begin
  if not FileExists(FConfigPath) then
  begin
    // Create default config file
    SetDefaults;
    Save;
    FModified := False;
    // Apply logging setting directly
    App.Logger.SetLoggingEnabled(FLogEnabled, FLogFilename, FLogAppend);
    Exit;
  end;

  Ini := TMemIniFile.Create(FConfigPath);
  try
    // [General]
    FWindowMode := TWindowMode(Ini.ReadInteger(SEC_GENERAL, KEY_WINDOW, Ord(DEF_WINDOW_MODE)));
    FOnTop := Ini.ReadBool(SEC_GENERAL, KEY_ON_TOP, DEF_ON_TOP);
    FAutostart := Ini.ReadBool(SEC_GENERAL, KEY_AUTOSTART, DEF_AUTOSTART);

    // [Window]
    FMinimizeToTray := Ini.ReadBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, DEF_MINIMIZE_TO_TRAY);
    FCloseToTray := Ini.ReadBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, DEF_CLOSE_TO_TRAY);

    // [Menu]
    FMenuHideOnFocusLoss := Ini.ReadBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, DEF_MENU_HIDE_ON_FOCUS_LOSS);

    // [Hotkey]
    FHotkey := Ini.ReadString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, DEF_HOTKEY);
    FUseLowLevelHook := Ini.ReadBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, DEF_USE_LOW_LEVEL_HOOK);

    // [Position]
    FPositionMode := TPositionMode(Ini.ReadInteger(SEC_POSITION, KEY_MODE, Ord(DEF_POSITION_MODE)));
    FPositionX := Ini.ReadInteger(SEC_POSITION, KEY_X, DEF_POSITION_X);
    FPositionY := Ini.ReadInteger(SEC_POSITION, KEY_Y, DEF_POSITION_Y);
    FPositionW := Ini.ReadInteger(SEC_POSITION, KEY_W, DEF_POSITION_W);
    FPositionH := Ini.ReadInteger(SEC_POSITION, KEY_H, DEF_POSITION_H);

    // [Polling]
    FPollingMode := TPollingMode(Ini.ReadInteger(SEC_POLLING, KEY_MODE, Ord(DEF_POLLING_MODE)));
    FPollingInterval := Ini.ReadInteger(SEC_POLLING, KEY_INTERVAL, DEF_POLLING_INTERVAL);
    FEventDebounceMs := Ini.ReadInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, DEF_EVENT_DEBOUNCE_MS);

    // [Log]
    FLogEnabled := Ini.ReadBool(SEC_LOG, KEY_ENABLED, DEF_LOG_ENABLED);
    FLogFilename := Ini.ReadString(SEC_LOG, KEY_FILENAME, DEF_LOG_FILENAME);
    FLogAppend := Ini.ReadBool(SEC_LOG, KEY_APPEND, DEF_LOG_APPEND);

    // [Appearance]
    FShowAddresses := Ini.ReadBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, DEF_SHOW_ADDRESSES);
    FTheme := Ini.ReadString(SEC_APPEARANCE, KEY_THEME, DEF_THEME);
    FVsfDir := Ini.ReadString(SEC_APPEARANCE, KEY_VSF_DIR, DEF_VSF_DIR);
    FShowLastSeen := Ini.ReadBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, DEF_SHOW_LAST_SEEN);
    FLastSeenFormat := TLastSeenFormat(Ini.ReadInteger(SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(DEF_LAST_SEEN_FORMAT)));
    FShowDeviceIcons := Ini.ReadBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, DEF_SHOW_DEVICE_ICONS);
    FConnectedColor := Ini.ReadInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, DEF_CONNECTED_COLOR);

    // [Layout]
    FItemHeight := Ini.ReadInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, DEF_ITEM_HEIGHT);
    FItemPadding := Ini.ReadInteger(SEC_LAYOUT, KEY_ITEM_PADDING, DEF_ITEM_PADDING);
    FItemMargin := Ini.ReadInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, DEF_ITEM_MARGIN);
    FIconSize := Ini.ReadInteger(SEC_LAYOUT, KEY_ICON_SIZE, DEF_ICON_SIZE);
    FCornerRadius := Ini.ReadInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, DEF_CORNER_RADIUS);
    FDeviceNameFontSize := Ini.ReadInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, DEF_DEVICE_NAME_FONT_SIZE);
    FStatusFontSize := Ini.ReadInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, DEF_STATUS_FONT_SIZE);
    FAddressFontSize := Ini.ReadInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, DEF_ADDRESS_FONT_SIZE);
    FIconFontSize := Ini.ReadInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, DEF_ICON_FONT_SIZE);
    FItemBorderWidth := Ini.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, DEF_ITEM_BORDER_WIDTH);
    FItemBorderColor := Ini.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, DEF_ITEM_BORDER_COLOR);

    // [Device] - global defaults
    FConnectionTimeout := Ini.ReadInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, DEF_CONNECTION_TIMEOUT);
    FConnectionRetryCount := Ini.ReadInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, DEF_CONNECTION_RETRY_COUNT);
    FNotifyOnConnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(DEF_NOTIFY_ON_CONNECT)));
    FNotifyOnDisconnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(DEF_NOTIFY_ON_DISCONNECT)));
    FNotifyOnConnectFailed := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(DEF_NOTIFY_ON_CONNECT_FAILED)));
    FNotifyOnAutoConnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(DEF_NOTIFY_ON_AUTO_CONNECT)));

    // Validate numeric values to ensure they're within acceptable ranges
    // (handles corrupted INI files or manual edits with invalid values)
    FPollingInterval := EnsureRange(FPollingInterval, MIN_POLLING_INTERVAL, MAX_POLLING_INTERVAL);
    FEventDebounceMs := EnsureRange(FEventDebounceMs, MIN_EVENT_DEBOUNCE_MS, MAX_EVENT_DEBOUNCE_MS);
    FItemHeight := EnsureRange(FItemHeight, MIN_ITEM_HEIGHT, MAX_ITEM_HEIGHT);
    FItemPadding := EnsureRange(FItemPadding, MIN_ITEM_PADDING, MAX_ITEM_PADDING);
    FItemMargin := EnsureRange(FItemMargin, MIN_ITEM_MARGIN, MAX_ITEM_MARGIN);
    FIconSize := EnsureRange(FIconSize, MIN_ICON_SIZE, MAX_ICON_SIZE);
    FCornerRadius := EnsureRange(FCornerRadius, MIN_CORNER_RADIUS, MAX_CORNER_RADIUS);
    FItemBorderWidth := EnsureRange(FItemBorderWidth, MIN_ITEM_BORDER_WIDTH, MAX_ITEM_BORDER_WIDTH);
    FDeviceNameFontSize := EnsureRange(FDeviceNameFontSize, MIN_DEVICE_NAME_FONT_SIZE, MAX_DEVICE_NAME_FONT_SIZE);
    FStatusFontSize := EnsureRange(FStatusFontSize, MIN_STATUS_FONT_SIZE, MAX_STATUS_FONT_SIZE);
    FAddressFontSize := EnsureRange(FAddressFontSize, MIN_ADDRESS_FONT_SIZE, MAX_ADDRESS_FONT_SIZE);
    FIconFontSize := EnsureRange(FIconFontSize, MIN_ICON_FONT_SIZE, MAX_ICON_FONT_SIZE);
    FConnectionTimeout := EnsureRange(FConnectionTimeout, MIN_CONNECTION_TIMEOUT, MAX_CONNECTION_TIMEOUT);
    FConnectionRetryCount := EnsureRange(FConnectionRetryCount, MIN_CONNECTION_RETRY_COUNT, MAX_CONNECTION_RETRY_COUNT);

    // Device-specific settings
    LoadDevices(Ini);

    FModified := False;
  finally
    Ini.Free;
  end;

  // Apply logging setting
  App.Logger.SetLoggingEnabled(FLogEnabled, FLogFilename, FLogAppend);

  // Ensure registry matches config setting
  TAutostartManager.Apply(FAutostart);
end;

procedure TAppConfig.Save;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FConfigPath);
  try
    // [General]
    Ini.WriteInteger(SEC_GENERAL, KEY_WINDOW, Ord(FWindowMode));
    Ini.WriteBool(SEC_GENERAL, KEY_ON_TOP, FOnTop);
    Ini.WriteBool(SEC_GENERAL, KEY_AUTOSTART, FAutostart);

    // [Window]
    Ini.WriteBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, FMinimizeToTray);
    Ini.WriteBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, FCloseToTray);

    // [Menu]
    Ini.WriteBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, FMenuHideOnFocusLoss);

    // [Hotkey]
    Ini.WriteString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, FHotkey);
    Ini.WriteBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, FUseLowLevelHook);

    // [Position]
    Ini.WriteInteger(SEC_POSITION, KEY_MODE, Ord(FPositionMode));
    Ini.WriteInteger(SEC_POSITION, KEY_X, FPositionX);
    Ini.WriteInteger(SEC_POSITION, KEY_Y, FPositionY);
    Ini.WriteInteger(SEC_POSITION, KEY_W, FPositionW);
    Ini.WriteInteger(SEC_POSITION, KEY_H, FPositionH);

    // [Polling]
    Ini.WriteInteger(SEC_POLLING, KEY_MODE, Ord(FPollingMode));
    Ini.WriteInteger(SEC_POLLING, KEY_INTERVAL, FPollingInterval);
    Ini.WriteInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, FEventDebounceMs);

    // [Log]
    Ini.WriteBool(SEC_LOG, KEY_ENABLED, FLogEnabled);
    Ini.WriteString(SEC_LOG, KEY_FILENAME, FLogFilename);
    Ini.WriteBool(SEC_LOG, KEY_APPEND, FLogAppend);

    // [Appearance]
    Ini.WriteBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, FShowAddresses);
    Ini.WriteString(SEC_APPEARANCE, KEY_THEME, FTheme);
    Ini.WriteString(SEC_APPEARANCE, KEY_VSF_DIR, FVsfDir);
    Ini.WriteBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, FShowLastSeen);
    Ini.WriteInteger(SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(FLastSeenFormat));
    Ini.WriteBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, FShowDeviceIcons);
    Ini.WriteInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, FConnectedColor);

    // [Layout]
    Ini.WriteInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, FItemHeight);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ITEM_PADDING, FItemPadding);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, FItemMargin);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ICON_SIZE, FIconSize);
    Ini.WriteInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, FCornerRadius);
    Ini.WriteInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, FDeviceNameFontSize);
    Ini.WriteInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, FStatusFontSize);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, FAddressFontSize);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, FIconFontSize);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, FItemBorderWidth);
    Ini.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, FItemBorderColor);

    // [Device] - global defaults
    Ini.WriteInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, FConnectionTimeout);
    Ini.WriteInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, FConnectionRetryCount);
    Ini.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(FNotifyOnConnect));
    Ini.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(FNotifyOnDisconnect));
    Ini.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(FNotifyOnConnectFailed));
    Ini.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(FNotifyOnAutoConnect));

    // Device-specific settings
    SaveDevices(Ini);

    Ini.UpdateFile;
    FModified := False;
  finally
    Ini.Free;
  end;
end;

procedure TAppConfig.SaveIfModified;
begin
  if FModified then
    Save;
end;

procedure TAppConfig.LoadDevices(AIni: TMemIniFile);
var
  Sections: TStringList;
  Section: string;
  AddressStr: string;
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  LastSeenStr: string;
begin
  FDevices.Clear;
  Sections := TStringList.Create;
  try
    AIni.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
      begin
        AddressStr := Section.Substring(Length(SEC_DEVICE_PREFIX));
        // Support both formats: 581862015DAE and 58:18:62:01:5D:AE
        AddressStr := StringReplace(AddressStr, ':', '', [rfReplaceAll]);
        if TryStrToUInt64('$' + AddressStr, Address) then
        begin
          DeviceConfig := TDeviceConfig.Default(Address);
          DeviceConfig.Name := AIni.ReadString(Section, KEY_NAME, '');
          DeviceConfig.Alias := AIni.ReadString(Section, KEY_ALIAS, '');
          DeviceConfig.Pinned := AIni.ReadBool(Section, KEY_PINNED, False);
          DeviceConfig.Hidden := AIni.ReadBool(Section, KEY_HIDDEN, False);
          DeviceConfig.AutoConnect := AIni.ReadBool(Section, KEY_AUTO_CONNECT, False);
          DeviceConfig.ConnectionTimeout := AIni.ReadInteger(Section, KEY_CONNECTION_TIMEOUT, -1);
          DeviceConfig.ConnectionRetryCount := AIni.ReadInteger(Section, KEY_CONNECTION_RETRY_COUNT, -1);
          // Per-device notification overrides (-1 = use global)
          DeviceConfig.Notifications.OnConnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT, -1);
          DeviceConfig.Notifications.OnDisconnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_DISCONNECT, -1);
          DeviceConfig.Notifications.OnConnectFailed := AIni.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT_FAILED, -1);
          DeviceConfig.Notifications.OnAutoConnect := AIni.ReadInteger(Section, KEY_NOTIFY_ON_AUTO_CONNECT, -1);
          DeviceConfig.DeviceTypeOverride := AIni.ReadInteger(Section, KEY_DEVICE_TYPE_OVERRIDE, -1);
          // Parse LastSeen as ISO 8601 datetime string
          LastSeenStr := AIni.ReadString(Section, KEY_LAST_SEEN, '');
          if LastSeenStr <> '' then
          begin
            try
              DeviceConfig.LastSeen := ISO8601ToDate(LastSeenStr, False);
            except
              DeviceConfig.LastSeen := 0;
            end;
          end;
          FDevices.Add(Address, DeviceConfig);
        end;
      end;
    end;
  finally
    Sections.Free;
  end;
end;

procedure TAppConfig.SaveDevices(AIni: TMemIniFile);
var
  Sections: TStringList;
  Section: string;
  Pair: TPair<UInt64, TDeviceConfig>;
  SectionName: string;
begin
  // First, remove all existing device sections (except [Device] itself)
  Sections := TStringList.Create;
  try
    AIni.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
        AIni.EraseSection(Section);
    end;
  finally
    Sections.Free;
  end;

  // Write current device configurations
  for Pair in FDevices do
  begin
    SectionName := SEC_DEVICE_PREFIX + IntToHex(Pair.Key, 12);
    // Always save Name (original device name from Windows)
    AIni.WriteString(SectionName, KEY_NAME, Pair.Value.Name);
    AIni.WriteString(SectionName, KEY_ALIAS, Pair.Value.Alias);
    AIni.WriteBool(SectionName, KEY_PINNED, Pair.Value.Pinned);
    AIni.WriteBool(SectionName, KEY_HIDDEN, Pair.Value.Hidden);
    AIni.WriteBool(SectionName, KEY_AUTO_CONNECT, Pair.Value.AutoConnect);
    // Only save connection settings if they override defaults
    if Pair.Value.ConnectionTimeout >= 0 then
      AIni.WriteInteger(SectionName, KEY_CONNECTION_TIMEOUT, Pair.Value.ConnectionTimeout);
    if Pair.Value.ConnectionRetryCount >= 0 then
      AIni.WriteInteger(SectionName, KEY_CONNECTION_RETRY_COUNT, Pair.Value.ConnectionRetryCount);
    // Only save notification settings if they override globals
    if Pair.Value.Notifications.OnConnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT, Pair.Value.Notifications.OnConnect);
    if Pair.Value.Notifications.OnDisconnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_DISCONNECT, Pair.Value.Notifications.OnDisconnect);
    if Pair.Value.Notifications.OnConnectFailed >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT_FAILED, Pair.Value.Notifications.OnConnectFailed);
    if Pair.Value.Notifications.OnAutoConnect >= 0 then
      AIni.WriteInteger(SectionName, KEY_NOTIFY_ON_AUTO_CONNECT, Pair.Value.Notifications.OnAutoConnect);
    // Only save DeviceTypeOverride if it's set (not auto-detect)
    if Pair.Value.DeviceTypeOverride >= 0 then
      AIni.WriteInteger(SectionName, KEY_DEVICE_TYPE_OVERRIDE, Pair.Value.DeviceTypeOverride);
    // Save LastSeen as ISO 8601 datetime string
    if Pair.Value.LastSeen > 0 then
      AIni.WriteString(SectionName, KEY_LAST_SEEN, DateToISO8601(Pair.Value.LastSeen, False));
  end;
end;

function TAppConfig.GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TAppConfig.SetDeviceConfig(const AConfig: TDeviceConfig);
begin
  FDevices.AddOrSetValue(AConfig.Address, AConfig);
  FModified := True;
end;

procedure TAppConfig.RemoveDeviceConfig(AAddress: UInt64);
begin
  if FDevices.ContainsKey(AAddress) then
  begin
    FDevices.Remove(AAddress);
    FModified := True;
  end;
end;

procedure TAppConfig.RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
var
  DeviceConfig: TDeviceConfig;
  IsNew: Boolean;
begin
  IsNew := not FDevices.TryGetValue(AAddress, DeviceConfig);

  if IsNew then
  begin
    // Create new device config with defaults
    DeviceConfig := TDeviceConfig.Default(AAddress);
    DeviceConfig.Name := AName;
    DeviceConfig.LastSeen := ALastSeen;
    FDevices.Add(AAddress, DeviceConfig);
    FModified := True;
    Log('[Config] RegisterDevice: New device registered: %s ($%.12X)', [AName, AAddress]);
  end
  else
  begin
    // Update existing device: always update LastSeen, update Name if changed
    if (DeviceConfig.Name <> AName) or (DeviceConfig.LastSeen < ALastSeen) then
    begin
      if (AName <> '') and (DeviceConfig.Name <> AName) then
        DeviceConfig.Name := AName;
      DeviceConfig.LastSeen := ALastSeen;
      FDevices[AAddress] := DeviceConfig;
      FModified := True;
    end;
  end;
end;

function TAppConfig.GetConfiguredDeviceAddresses: TArray<UInt64>;
begin
  Result := FDevices.Keys.ToArray;
end;

// Property setters with modification tracking

procedure TAppConfig.SetWindowMode(AValue: TWindowMode);
begin
  if FWindowMode <> AValue then
  begin
    FWindowMode := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetOnTop(AValue: Boolean);
begin
  if FOnTop <> AValue then
  begin
    FOnTop := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetAutostart(AValue: Boolean);
begin
  if FAutostart <> AValue then
  begin
    FAutostart := AValue;
    FModified := True;
    // Apply to registry immediately
    TAutostartManager.Apply(AValue);
  end;
end;

procedure TAppConfig.SetMinimizeToTray(AValue: Boolean);
begin
  if FMinimizeToTray <> AValue then
  begin
    FMinimizeToTray := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetCloseToTray(AValue: Boolean);
begin
  if FCloseToTray <> AValue then
  begin
    FCloseToTray := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  if FMenuHideOnFocusLoss <> AValue then
  begin
    FMenuHideOnFocusLoss := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetHotkey(const AValue: string);
begin
  if FHotkey <> AValue then
  begin
    FHotkey := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetUseLowLevelHook(AValue: Boolean);
begin
  if FUseLowLevelHook <> AValue then
  begin
    FUseLowLevelHook := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPositionMode(AValue: TPositionMode);
begin
  if FPositionMode <> AValue then
  begin
    FPositionMode := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPositionX(AValue: Integer);
begin
  if FPositionX <> AValue then
  begin
    FPositionX := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPositionY(AValue: Integer);
begin
  if FPositionY <> AValue then
  begin
    FPositionY := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPositionW(AValue: Integer);
begin
  if FPositionW <> AValue then
  begin
    FPositionW := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPositionH(AValue: Integer);
begin
  if FPositionH <> AValue then
  begin
    FPositionH := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPollingMode(AValue: TPollingMode);
begin
  if FPollingMode <> AValue then
  begin
    FPollingMode := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetPollingInterval(AValue: Integer);
begin
  if FPollingInterval <> AValue then
  begin
    FPollingInterval := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetEventDebounceMs(AValue: Integer);
begin
  if FEventDebounceMs <> AValue then
  begin
    FEventDebounceMs := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetLogEnabled(AValue: Boolean);
begin
  if FLogEnabled <> AValue then
  begin
    FLogEnabled := AValue;
    FModified := True;
    // Apply to logger immediately
    App.Logger.SetLoggingEnabled(AValue, FLogFilename, FLogAppend);
  end;
end;

procedure TAppConfig.SetLogFilename(const AValue: string);
begin
  if FLogFilename <> AValue then
  begin
    FLogFilename := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetLogAppend(AValue: Boolean);
begin
  if FLogAppend <> AValue then
  begin
    FLogAppend := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetShowAddresses(AValue: Boolean);
begin
  if FShowAddresses <> AValue then
  begin
    FShowAddresses := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetTheme(const AValue: string);
begin
  if FTheme <> AValue then
  begin
    FTheme := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetVsfDir(const AValue: string);
begin
  if FVsfDir <> AValue then
  begin
    FVsfDir := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetShowLastSeen(AValue: Boolean);
begin
  if FShowLastSeen <> AValue then
  begin
    FShowLastSeen := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  if FLastSeenFormat <> AValue then
  begin
    FLastSeenFormat := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetShowDeviceIcons(AValue: Boolean);
begin
  if FShowDeviceIcons <> AValue then
  begin
    FShowDeviceIcons := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetConnectedColor(AValue: Integer);
begin
  if FConnectedColor <> AValue then
  begin
    FConnectedColor := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetItemHeight(AValue: Integer);
begin
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetItemPadding(AValue: Integer);
begin
  if FItemPadding <> AValue then
  begin
    FItemPadding := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetItemMargin(AValue: Integer);
begin
  if FItemMargin <> AValue then
  begin
    FItemMargin := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetIconSize(AValue: Integer);
begin
  if FIconSize <> AValue then
  begin
    FIconSize := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetCornerRadius(AValue: Integer);
begin
  if FCornerRadius <> AValue then
  begin
    FCornerRadius := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetDeviceNameFontSize(AValue: Integer);
begin
  if FDeviceNameFontSize <> AValue then
  begin
    FDeviceNameFontSize := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetStatusFontSize(AValue: Integer);
begin
  if FStatusFontSize <> AValue then
  begin
    FStatusFontSize := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetAddressFontSize(AValue: Integer);
begin
  if FAddressFontSize <> AValue then
  begin
    FAddressFontSize := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetIconFontSize(AValue: Integer);
begin
  if FIconFontSize <> AValue then
  begin
    FIconFontSize := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetItemBorderWidth(AValue: Integer);
begin
  if FItemBorderWidth <> AValue then
  begin
    FItemBorderWidth := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetItemBorderColor(AValue: Integer);
begin
  if FItemBorderColor <> AValue then
  begin
    FItemBorderColor := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetConnectionTimeout(AValue: Integer);
begin
  if FConnectionTimeout <> AValue then
  begin
    FConnectionTimeout := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetConnectionRetryCount(AValue: Integer);
begin
  if FConnectionRetryCount <> AValue then
  begin
    FConnectionRetryCount := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetNotifyOnConnect(AValue: TNotificationMode);
begin
  if FNotifyOnConnect <> AValue then
  begin
    FNotifyOnConnect := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetNotifyOnDisconnect(AValue: TNotificationMode);
begin
  if FNotifyOnDisconnect <> AValue then
  begin
    FNotifyOnDisconnect := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetNotifyOnConnectFailed(AValue: TNotificationMode);
begin
  if FNotifyOnConnectFailed <> AValue then
  begin
    FNotifyOnConnectFailed := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetNotifyOnAutoConnect(AValue: TNotificationMode);
begin
  if FNotifyOnAutoConnect <> AValue then
  begin
    FNotifyOnAutoConnect := AValue;
    FModified := True;
  end;
end;

function TAppConfig.GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
var
  DeviceConfig: TDeviceConfig;
  DeviceValue: Integer;
begin
  // Get per-device config if exists
  DeviceConfig := GetDeviceConfig(AAddress);

  // Get the per-device override value for this event
  case AEvent of
    neConnect:
      DeviceValue := DeviceConfig.Notifications.OnConnect;
    neDisconnect:
      DeviceValue := DeviceConfig.Notifications.OnDisconnect;
    neConnectFailed:
      DeviceValue := DeviceConfig.Notifications.OnConnectFailed;
    neAutoConnect:
      DeviceValue := DeviceConfig.Notifications.OnAutoConnect;
  else
    DeviceValue := -1;
  end;

  // If per-device value is set (>= 0), use it; otherwise use global
  if DeviceValue >= 0 then
    Result := TNotificationMode(DeviceValue)
  else
  begin
    // Return global default
    case AEvent of
      neConnect:
        Result := FNotifyOnConnect;
      neDisconnect:
        Result := FNotifyOnDisconnect;
      neConnectFailed:
        Result := FNotifyOnConnectFailed;
      neAutoConnect:
        Result := FNotifyOnAutoConnect;
    else
      Result := nmNone;
    end;
  end;
end;

function TAppConfig.GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetDeviceConfig(AAddress);
  if DeviceConfig.ConnectionTimeout >= 0 then
    Result := DeviceConfig.ConnectionTimeout
  else
    Result := FConnectionTimeout;
end;

function TAppConfig.GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetDeviceConfig(AAddress);
  if DeviceConfig.ConnectionRetryCount >= 0 then
    Result := DeviceConfig.ConnectionRetryCount
  else
    Result := FConnectionRetryCount;
end;

initialization

finalization
  FreeAndNil(GConfig);

end.
