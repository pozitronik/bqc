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
  System.Generics.Collections;

type
  /// <summary>
  /// Notification mode for events.
  /// </summary>
  TNotificationMode = (
    nmNone,      // No notification
    nmBalloon    // Balloon tip notification
    // Future: nmToast for Windows 10+ toast notifications
  );

  /// <summary>
  /// Window display mode.
  /// </summary>
  TWindowMode = (
    wmMenu,    // Popup menu style, hides on focus loss (0)
    wmWindow   // Normal window with title bar (1)
  );

  /// <summary>
  /// Popup menu position when showing in Menu mode.
  /// </summary>
  TMenuPosition = (
    mpNearTray,    // Near system tray icon (default)
    mpNearCursor,  // Near mouse cursor
    mpCenterScreen,// Center of screen
    mpSameAsWindow // Use saved window position
  );

  /// <summary>
  /// Autostart mode for the application.
  /// </summary>
  TAutostartMode = (
    amNone,           // No autostart (0)
    amRegistry        // Start via Registry Run key (1)
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
    Alias: string;
    Pinned: Boolean;
    Hidden: Boolean;
    AutoConnect: Boolean;
    /// <summary>Connection timeout in ms. -1 means use global default.</summary>
    ConnectionTimeout: Integer;
    /// <summary>Connection retry count. -1 means use global default.</summary>
    ConnectionRetryCount: Integer;
    /// <summary>Per-device notification settings.</summary>
    Notifications: TDeviceNotificationConfig;

    class function Default(AAddress: UInt64): TDeviceConfig; static;
  end;

  /// <summary>
  /// Application configuration manager.
  /// Singleton class that handles loading/saving settings from INI file.
  /// </summary>
  TAppConfig = class
  private
    FConfigPath: string;
    FModified: Boolean;
    FDevices: TDictionary<UInt64, TDeviceConfig>;

    // General
    FLoggingEnabled: Boolean;
    FWindowMode: TWindowMode;
    FMenuPosition: TMenuPosition;
    FStayOnTop: Boolean;

    // Hotkey
    FHotkey: string;
    FUseLowLevelHook: Boolean;

    // Polling
    FPollingEnabled: Boolean;
    FPollingInterval: Integer;
    FPollingAsPrimary: Boolean;

    // Appearance
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;

    // Window
    FWindowX: Integer;
    FWindowY: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;

    // Connection
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;

    // Tray
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;

    // Notifications (global defaults)
    FNotifyOnConnect: TNotificationMode;
    FNotifyOnDisconnect: TNotificationMode;
    FNotifyOnConnectFailed: TNotificationMode;
    FNotifyOnAutoConnect: TNotificationMode;

    // Menu behavior
    FMenuHideOnFocusLoss: Boolean;

    // Autostart
    FAutostartMode: TAutostartMode;

    procedure SetDefaults;
    procedure LoadDevices(AIni: TMemIniFile);
    procedure SaveDevices(AIni: TMemIniFile);

    // Property setters with modification tracking
    procedure SetLoggingEnabled(AValue: Boolean);
    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetMenuPosition(AValue: TMenuPosition);
    procedure SetStayOnTop(AValue: Boolean);
    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);
    procedure SetPollingEnabled(AValue: Boolean);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetPollingAsPrimary(AValue: Boolean);
    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetWindowX(AValue: Integer);
    procedure SetWindowY(AValue: Integer);
    procedure SetWindowWidth(AValue: Integer);
    procedure SetWindowHeight(AValue: Integer);
    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);
    procedure SetAutostartMode(AValue: TAutostartMode);

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

    // General settings
    property LoggingEnabled: Boolean read FLoggingEnabled write SetLoggingEnabled;
    property WindowMode: TWindowMode read FWindowMode write SetWindowMode;
    property MenuPosition: TMenuPosition read FMenuPosition write SetMenuPosition;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;

    // Hotkey
    property Hotkey: string read FHotkey write SetHotkey;
    /// <summary>
    /// Use low-level keyboard hook instead of RegisterHotKey.
    /// Allows overriding system hotkeys like Win+K.
    /// </summary>
    property UseLowLevelHook: Boolean read FUseLowLevelHook write SetUseLowLevelHook;

    // Polling
    property PollingEnabled: Boolean read FPollingEnabled write SetPollingEnabled;
    property PollingInterval: Integer read FPollingInterval write SetPollingInterval;
    property PollingAsPrimary: Boolean read FPollingAsPrimary write SetPollingAsPrimary;

    // Appearance
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    /// <summary>
    /// Directory containing .vsf style files. Relative paths are resolved from exe directory.
    /// </summary>
    property VsfDir: string read FVsfDir write SetVsfDir;

    // Window
    property WindowX: Integer read FWindowX write SetWindowX;
    property WindowY: Integer read FWindowY write SetWindowY;
    property WindowWidth: Integer read FWindowWidth write SetWindowWidth;
    property WindowHeight: Integer read FWindowHeight write SetWindowHeight;

    // Connection
    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;

    // Tray
    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;

    // Notifications (global defaults)
    property NotifyOnConnect: TNotificationMode read FNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read FNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read FNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read FNotifyOnAutoConnect write SetNotifyOnAutoConnect;

    // Menu behavior
    /// <summary>
    /// When true, menu popup hides when application loses focus.
    /// Only applies when WindowMode = wmMenu.
    /// </summary>
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;

    // Autostart
    property AutostartMode: TAutostartMode read FAutostartMode write SetAutostartMode;

    /// <summary>
    /// Gets effective notification mode for a device and event.
    /// Resolves per-device override or returns global default.
    /// </summary>
    function GetEffectiveNotification(AAddress: UInt64; AEvent: string): TNotificationMode;
  end;

/// <summary>
/// Returns the singleton configuration instance.
/// </summary>
function Config: TAppConfig;

implementation

uses
  App.Logger,
  App.Autostart;

const
  // Section names
  SEC_GENERAL = 'General';
  SEC_HOTKEY = 'Hotkey';
  SEC_POLLING = 'Polling';
  SEC_APPEARANCE = 'Appearance';
  SEC_WINDOW = 'Window';
  SEC_CONNECTION = 'Connection';
  SEC_TRAY = 'Tray';
  SEC_NOTIFICATIONS = 'Notifications';
  SEC_AUTOSTART = 'Autostart';
  SEC_DEVICE_PREFIX = 'Device.';

  // Default values
  DEF_LOGGING_ENABLED = False;
  DEF_WINDOW_MODE = wmWindow;  // 1 = normal window (default)
  DEF_MENU_POSITION = mpNearCursor;  // Near cursor is more reliable for multi-monitor
  DEF_STAY_ON_TOP = False;
  DEF_HOTKEY = '';
  DEF_USE_LOW_LEVEL_HOOK = True;  // Use low-level hook by default to allow overriding system hotkeys
  DEF_POLLING_ENABLED = True;
  DEF_POLLING_INTERVAL = 2000;
  DEF_POLLING_AS_PRIMARY = False;
  DEF_SHOW_ADDRESSES = False;
  DEF_THEME = 'System';
  DEF_VSF_DIR = 'themes';  // Directory for .vsf style files, relative to exe
  DEF_WINDOW_X = -1;  // -1 means use default/center
  DEF_WINDOW_Y = -1;
  DEF_WINDOW_WIDTH = 320;
  DEF_WINDOW_HEIGHT = 400;
  DEF_CONNECTION_TIMEOUT = 10000;  // 10 seconds
  DEF_CONNECTION_RETRY_COUNT = 2;
  DEF_MINIMIZE_TO_TRAY = True;
  DEF_CLOSE_TO_TRAY = True;  // Default: close hides to tray
  DEF_NOTIFY_ON_CONNECT = nmBalloon;
  DEF_NOTIFY_ON_DISCONNECT = nmBalloon;
  DEF_NOTIFY_ON_CONNECT_FAILED = nmBalloon;
  DEF_NOTIFY_ON_AUTO_CONNECT = nmBalloon;
  DEF_MENU_HIDE_ON_FOCUS_LOSS = True;  // Menu hides when app loses focus (default)
  DEF_AUTOSTART_MODE = amNone;

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

{ TDeviceNotificationConfig }

class function TDeviceNotificationConfig.Default: TDeviceNotificationConfig;
begin
  Result.OnConnect := -1;        // Use global default
  Result.OnDisconnect := -1;     // Use global default
  Result.OnConnectFailed := -1;  // Use global default
  Result.OnAutoConnect := -1;    // Use global default
end;

{ TDeviceConfig }

class function TDeviceConfig.Default(AAddress: UInt64): TDeviceConfig;
begin
  Result.Address := AAddress;
  Result.Alias := '';
  Result.Pinned := False;
  Result.Hidden := False;
  Result.AutoConnect := False;
  Result.ConnectionTimeout := -1;     // Use global default
  Result.ConnectionRetryCount := -1;  // Use global default
  Result.Notifications := TDeviceNotificationConfig.Default;
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
  FLoggingEnabled := DEF_LOGGING_ENABLED;
  FWindowMode := DEF_WINDOW_MODE;
  FMenuPosition := DEF_MENU_POSITION;
  FStayOnTop := DEF_STAY_ON_TOP;
  FHotkey := DEF_HOTKEY;
  FUseLowLevelHook := DEF_USE_LOW_LEVEL_HOOK;
  FPollingEnabled := DEF_POLLING_ENABLED;
  FPollingInterval := DEF_POLLING_INTERVAL;
  FPollingAsPrimary := DEF_POLLING_AS_PRIMARY;
  FShowAddresses := DEF_SHOW_ADDRESSES;
  FTheme := DEF_THEME;
  FVsfDir := DEF_VSF_DIR;
  FWindowX := DEF_WINDOW_X;
  FWindowY := DEF_WINDOW_Y;
  FWindowWidth := DEF_WINDOW_WIDTH;
  FWindowHeight := DEF_WINDOW_HEIGHT;
  FConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  FConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
  FMinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := DEF_CLOSE_TO_TRAY;
  FNotifyOnConnect := DEF_NOTIFY_ON_CONNECT;
  FNotifyOnDisconnect := DEF_NOTIFY_ON_DISCONNECT;
  FNotifyOnConnectFailed := DEF_NOTIFY_ON_CONNECT_FAILED;
  FNotifyOnAutoConnect := DEF_NOTIFY_ON_AUTO_CONNECT;
  FMenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;
  FAutostartMode := DEF_AUTOSTART_MODE;
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
    // Apply logging setting directly (not via property setter)
    App.Logger.SetLoggingEnabled(FLoggingEnabled);
    Exit;
  end;

  Ini := TMemIniFile.Create(FConfigPath);
  try
    // General
    FLoggingEnabled := Ini.ReadBool(SEC_GENERAL, 'LoggingEnabled', DEF_LOGGING_ENABLED);
    FWindowMode := TWindowMode(Ini.ReadInteger(SEC_GENERAL, 'WindowMode', Ord(DEF_WINDOW_MODE)));
    FMenuPosition := TMenuPosition(Ini.ReadInteger(SEC_GENERAL, 'MenuPosition', Ord(DEF_MENU_POSITION)));
    FStayOnTop := Ini.ReadBool(SEC_GENERAL, 'StayOnTop', DEF_STAY_ON_TOP);

    // Hotkey
    FHotkey := Ini.ReadString(SEC_HOTKEY, 'GlobalHotkey', DEF_HOTKEY);
    FUseLowLevelHook := Ini.ReadBool(SEC_HOTKEY, 'UseLowLevelHook', DEF_USE_LOW_LEVEL_HOOK);

    // Polling
    FPollingEnabled := Ini.ReadBool(SEC_POLLING, 'Enabled', DEF_POLLING_ENABLED);
    FPollingInterval := Ini.ReadInteger(SEC_POLLING, 'Interval', DEF_POLLING_INTERVAL);
    FPollingAsPrimary := Ini.ReadBool(SEC_POLLING, 'UsePrimary', DEF_POLLING_AS_PRIMARY);

    // Appearance
    FShowAddresses := Ini.ReadBool(SEC_APPEARANCE, 'ShowAddresses', DEF_SHOW_ADDRESSES);
    FTheme := Ini.ReadString(SEC_APPEARANCE, 'Theme', DEF_THEME);
    FVsfDir := Ini.ReadString(SEC_APPEARANCE, 'VsfDir', DEF_VSF_DIR);

    // Window
    FWindowX := Ini.ReadInteger(SEC_WINDOW, 'X', DEF_WINDOW_X);
    FWindowY := Ini.ReadInteger(SEC_WINDOW, 'Y', DEF_WINDOW_Y);
    FWindowWidth := Ini.ReadInteger(SEC_WINDOW, 'Width', DEF_WINDOW_WIDTH);
    FWindowHeight := Ini.ReadInteger(SEC_WINDOW, 'Height', DEF_WINDOW_HEIGHT);

    // Connection
    FConnectionTimeout := Ini.ReadInteger(SEC_CONNECTION, 'Timeout', DEF_CONNECTION_TIMEOUT);
    FConnectionRetryCount := Ini.ReadInteger(SEC_CONNECTION, 'RetryCount', DEF_CONNECTION_RETRY_COUNT);

    // Tray
    FMinimizeToTray := Ini.ReadBool(SEC_TRAY, 'MinimizeToTray', DEF_MINIMIZE_TO_TRAY);
    FCloseToTray := Ini.ReadBool(SEC_TRAY, 'CloseToTray', DEF_CLOSE_TO_TRAY);

    // Notifications
    FNotifyOnConnect := TNotificationMode(Ini.ReadInteger(SEC_NOTIFICATIONS, 'OnConnect', Ord(DEF_NOTIFY_ON_CONNECT)));
    FNotifyOnDisconnect := TNotificationMode(Ini.ReadInteger(SEC_NOTIFICATIONS, 'OnDisconnect', Ord(DEF_NOTIFY_ON_DISCONNECT)));
    FNotifyOnConnectFailed := TNotificationMode(Ini.ReadInteger(SEC_NOTIFICATIONS, 'OnConnectFailed', Ord(DEF_NOTIFY_ON_CONNECT_FAILED)));
    FNotifyOnAutoConnect := TNotificationMode(Ini.ReadInteger(SEC_NOTIFICATIONS, 'OnAutoConnect', Ord(DEF_NOTIFY_ON_AUTO_CONNECT)));

    // Menu behavior
    FMenuHideOnFocusLoss := Ini.ReadBool(SEC_GENERAL, 'MenuHideOnFocusLoss', DEF_MENU_HIDE_ON_FOCUS_LOSS);

    // Autostart - read from INI and sync with registry
    FAutostartMode := TAutostartMode(Ini.ReadInteger(SEC_AUTOSTART, 'Mode', Ord(DEF_AUTOSTART_MODE)));

    // Device-specific settings
    LoadDevices(Ini);

    FModified := False;
  finally
    Ini.Free;
  end;

  // Apply logging setting directly (not via property setter)
  App.Logger.SetLoggingEnabled(FLoggingEnabled);

  // Ensure registry matches config setting
  // This handles cases where registry was modified externally
  TAutostartManager.Apply(FAutostartMode = amRegistry);
end;

procedure TAppConfig.Save;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FConfigPath);
  try
    // General
    Ini.WriteBool(SEC_GENERAL, 'LoggingEnabled', FLoggingEnabled);
    Ini.WriteInteger(SEC_GENERAL, 'WindowMode', Ord(FWindowMode));
    Ini.WriteInteger(SEC_GENERAL, 'MenuPosition', Ord(FMenuPosition));
    Ini.WriteBool(SEC_GENERAL, 'StayOnTop', FStayOnTop);

    // Hotkey
    Ini.WriteString(SEC_HOTKEY, 'GlobalHotkey', FHotkey);
    Ini.WriteBool(SEC_HOTKEY, 'UseLowLevelHook', FUseLowLevelHook);

    // Polling
    Ini.WriteBool(SEC_POLLING, 'Enabled', FPollingEnabled);
    Ini.WriteInteger(SEC_POLLING, 'Interval', FPollingInterval);
    Ini.WriteBool(SEC_POLLING, 'UsePrimary', FPollingAsPrimary);

    // Appearance
    Ini.WriteBool(SEC_APPEARANCE, 'ShowAddresses', FShowAddresses);
    Ini.WriteString(SEC_APPEARANCE, 'Theme', FTheme);
    Ini.WriteString(SEC_APPEARANCE, 'VsfDir', FVsfDir);

    // Window
    Ini.WriteInteger(SEC_WINDOW, 'X', FWindowX);
    Ini.WriteInteger(SEC_WINDOW, 'Y', FWindowY);
    Ini.WriteInteger(SEC_WINDOW, 'Width', FWindowWidth);
    Ini.WriteInteger(SEC_WINDOW, 'Height', FWindowHeight);

    // Connection
    Ini.WriteInteger(SEC_CONNECTION, 'Timeout', FConnectionTimeout);
    Ini.WriteInteger(SEC_CONNECTION, 'RetryCount', FConnectionRetryCount);

    // Tray
    Ini.WriteBool(SEC_TRAY, 'MinimizeToTray', FMinimizeToTray);
    Ini.WriteBool(SEC_TRAY, 'CloseToTray', FCloseToTray);

    // Notifications
    Ini.WriteInteger(SEC_NOTIFICATIONS, 'OnConnect', Ord(FNotifyOnConnect));
    Ini.WriteInteger(SEC_NOTIFICATIONS, 'OnDisconnect', Ord(FNotifyOnDisconnect));
    Ini.WriteInteger(SEC_NOTIFICATIONS, 'OnConnectFailed', Ord(FNotifyOnConnectFailed));
    Ini.WriteInteger(SEC_NOTIFICATIONS, 'OnAutoConnect', Ord(FNotifyOnAutoConnect));

    // Menu behavior
    Ini.WriteBool(SEC_GENERAL, 'MenuHideOnFocusLoss', FMenuHideOnFocusLoss);

    // Autostart
    Ini.WriteInteger(SEC_AUTOSTART, 'Mode', Ord(FAutostartMode));

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
          DeviceConfig.Alias := AIni.ReadString(Section, 'Alias', '');
          DeviceConfig.Pinned := AIni.ReadBool(Section, 'Pinned', False);
          DeviceConfig.Hidden := AIni.ReadBool(Section, 'Hidden', False);
          DeviceConfig.AutoConnect := AIni.ReadBool(Section, 'AutoConnect', False);
          DeviceConfig.ConnectionTimeout := AIni.ReadInteger(Section, 'ConnectionTimeout', -1);
          DeviceConfig.ConnectionRetryCount := AIni.ReadInteger(Section, 'ConnectionRetryCount', -1);
          // Per-device notification overrides (-1 = use global)
          DeviceConfig.Notifications.OnConnect := AIni.ReadInteger(Section, 'NotifyOnConnect', -1);
          DeviceConfig.Notifications.OnDisconnect := AIni.ReadInteger(Section, 'NotifyOnDisconnect', -1);
          DeviceConfig.Notifications.OnConnectFailed := AIni.ReadInteger(Section, 'NotifyOnConnectFailed', -1);
          DeviceConfig.Notifications.OnAutoConnect := AIni.ReadInteger(Section, 'NotifyOnAutoConnect', -1);
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
  // First, remove all existing device sections
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
    AIni.WriteString(SectionName, 'Alias', Pair.Value.Alias);
    AIni.WriteBool(SectionName, 'Pinned', Pair.Value.Pinned);
    AIni.WriteBool(SectionName, 'Hidden', Pair.Value.Hidden);
    AIni.WriteBool(SectionName, 'AutoConnect', Pair.Value.AutoConnect);
    // Only save connection settings if they override defaults
    if Pair.Value.ConnectionTimeout >= 0 then
      AIni.WriteInteger(SectionName, 'ConnectionTimeout', Pair.Value.ConnectionTimeout);
    if Pair.Value.ConnectionRetryCount >= 0 then
      AIni.WriteInteger(SectionName, 'ConnectionRetryCount', Pair.Value.ConnectionRetryCount);
    // Only save notification settings if they override globals
    if Pair.Value.Notifications.OnConnect >= 0 then
      AIni.WriteInteger(SectionName, 'NotifyOnConnect', Pair.Value.Notifications.OnConnect);
    if Pair.Value.Notifications.OnDisconnect >= 0 then
      AIni.WriteInteger(SectionName, 'NotifyOnDisconnect', Pair.Value.Notifications.OnDisconnect);
    if Pair.Value.Notifications.OnConnectFailed >= 0 then
      AIni.WriteInteger(SectionName, 'NotifyOnConnectFailed', Pair.Value.Notifications.OnConnectFailed);
    if Pair.Value.Notifications.OnAutoConnect >= 0 then
      AIni.WriteInteger(SectionName, 'NotifyOnAutoConnect', Pair.Value.Notifications.OnAutoConnect);
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

function TAppConfig.GetConfiguredDeviceAddresses: TArray<UInt64>;
begin
  Result := FDevices.Keys.ToArray;
end;

// Property setters with modification tracking

procedure TAppConfig.SetLoggingEnabled(AValue: Boolean);
begin
  if FLoggingEnabled <> AValue then
  begin
    FLoggingEnabled := AValue;
    FModified := True;
    // Apply to logger immediately
    App.Logger.SetLoggingEnabled(AValue);
  end;
end;

procedure TAppConfig.SetWindowMode(AValue: TWindowMode);
begin
  if FWindowMode <> AValue then
  begin
    FWindowMode := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetMenuPosition(AValue: TMenuPosition);
begin
  if FMenuPosition <> AValue then
  begin
    FMenuPosition := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetStayOnTop(AValue: Boolean);
begin
  if FStayOnTop <> AValue then
  begin
    FStayOnTop := AValue;
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

procedure TAppConfig.SetPollingEnabled(AValue: Boolean);
begin
  if FPollingEnabled <> AValue then
  begin
    FPollingEnabled := AValue;
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

procedure TAppConfig.SetPollingAsPrimary(AValue: Boolean);
begin
  if FPollingAsPrimary <> AValue then
  begin
    FPollingAsPrimary := AValue;
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

procedure TAppConfig.SetWindowX(AValue: Integer);
begin
  if FWindowX <> AValue then
  begin
    FWindowX := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetWindowY(AValue: Integer);
begin
  if FWindowY <> AValue then
  begin
    FWindowY := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetWindowWidth(AValue: Integer);
begin
  if FWindowWidth <> AValue then
  begin
    FWindowWidth := AValue;
    FModified := True;
  end;
end;

procedure TAppConfig.SetWindowHeight(AValue: Integer);
begin
  if FWindowHeight <> AValue then
  begin
    FWindowHeight := AValue;
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

procedure TAppConfig.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  if FMenuHideOnFocusLoss <> AValue then
  begin
    FMenuHideOnFocusLoss := AValue;
    FModified := True;
  end;
end;

function TAppConfig.GetEffectiveNotification(AAddress: UInt64; AEvent: string): TNotificationMode;
var
  DeviceConfig: TDeviceConfig;
  DeviceValue: Integer;
begin
  // Get per-device config if exists
  DeviceConfig := GetDeviceConfig(AAddress);

  // Get the per-device override value for this event
  DeviceValue := -1;
  if AEvent = 'Connect' then
    DeviceValue := DeviceConfig.Notifications.OnConnect
  else if AEvent = 'Disconnect' then
    DeviceValue := DeviceConfig.Notifications.OnDisconnect
  else if AEvent = 'ConnectFailed' then
    DeviceValue := DeviceConfig.Notifications.OnConnectFailed
  else if AEvent = 'AutoConnect' then
    DeviceValue := DeviceConfig.Notifications.OnAutoConnect;

  // If per-device value is set (>= 0), use it; otherwise use global
  if DeviceValue >= 0 then
    Result := TNotificationMode(DeviceValue)
  else
  begin
    // Return global default
    if AEvent = 'Connect' then
      Result := FNotifyOnConnect
    else if AEvent = 'Disconnect' then
      Result := FNotifyOnDisconnect
    else if AEvent = 'ConnectFailed' then
      Result := FNotifyOnConnectFailed
    else if AEvent = 'AutoConnect' then
      Result := FNotifyOnAutoConnect
    else
      Result := nmNone;
  end;
end;

procedure TAppConfig.SetAutostartMode(AValue: TAutostartMode);
begin
  if FAutostartMode <> AValue then
  begin
    FAutostartMode := AValue;
    FModified := True;
    // Apply to registry immediately
    TAutostartManager.Apply(AValue = amRegistry);
  end;
end;

initialization

finalization
  FreeAndNil(GConfig);

end.
