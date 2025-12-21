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

    // [Log]
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;

    // [Appearance]
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;

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
    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
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

    // [Log]
    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogFilename: string read FLogFilename write SetLogFilename;
    property LogAppend: Boolean read FLogAppend write SetLogAppend;

    // [Appearance]
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    property VsfDir: string read FVsfDir write SetVsfDir;

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
    function GetEffectiveNotification(AAddress: UInt64; AEvent: string): TNotificationMode;

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
  SEC_WINDOW = 'Window';
  SEC_MENU = 'Menu';
  SEC_HOTKEY = 'Hotkey';
  SEC_POSITION = 'Position';
  SEC_POLLING = 'Polling';
  SEC_LOG = 'Log';
  SEC_APPEARANCE = 'Appearance';
  SEC_DEVICE = 'Device';
  SEC_DEVICE_PREFIX = 'Device.';

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
  DEF_POLLING_INTERVAL = 2000;
  DEF_LOG_ENABLED = False;
  DEF_LOG_FILENAME = 'bqc.log';
  DEF_LOG_APPEND = False;
  DEF_SHOW_ADDRESSES = False;
  DEF_THEME = '';
  DEF_VSF_DIR = 'themes';
  DEF_CONNECTION_TIMEOUT = 10000;
  DEF_CONNECTION_RETRY_COUNT = 2;
  DEF_NOTIFY_ON_CONNECT = nmBalloon;
  DEF_NOTIFY_ON_DISCONNECT = nmBalloon;
  DEF_NOTIFY_ON_CONNECT_FAILED = nmBalloon;
  DEF_NOTIFY_ON_AUTO_CONNECT = nmBalloon;

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

  // [Log]
  FLogEnabled := DEF_LOG_ENABLED;
  FLogFilename := DEF_LOG_FILENAME;
  FLogAppend := DEF_LOG_APPEND;

  // [Appearance]
  FShowAddresses := DEF_SHOW_ADDRESSES;
  FTheme := DEF_THEME;
  FVsfDir := DEF_VSF_DIR;

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
    FWindowMode := TWindowMode(Ini.ReadInteger(SEC_GENERAL, 'Window', Ord(DEF_WINDOW_MODE)));
    FOnTop := Ini.ReadBool(SEC_GENERAL, 'OnTop', DEF_ON_TOP);
    FAutostart := Ini.ReadBool(SEC_GENERAL, 'Autostart', DEF_AUTOSTART);

    // [Window]
    FMinimizeToTray := Ini.ReadBool(SEC_WINDOW, 'MinimizeToTray', DEF_MINIMIZE_TO_TRAY);
    FCloseToTray := Ini.ReadBool(SEC_WINDOW, 'CloseToTray', DEF_CLOSE_TO_TRAY);

    // [Menu]
    FMenuHideOnFocusLoss := Ini.ReadBool(SEC_MENU, 'HideOnFocusLoss', DEF_MENU_HIDE_ON_FOCUS_LOSS);

    // [Hotkey]
    FHotkey := Ini.ReadString(SEC_HOTKEY, 'GlobalHotkey', DEF_HOTKEY);
    FUseLowLevelHook := Ini.ReadBool(SEC_HOTKEY, 'UseLowLevelHook', DEF_USE_LOW_LEVEL_HOOK);

    // [Position]
    FPositionMode := TPositionMode(Ini.ReadInteger(SEC_POSITION, 'Mode', Ord(DEF_POSITION_MODE)));
    FPositionX := Ini.ReadInteger(SEC_POSITION, 'X', DEF_POSITION_X);
    FPositionY := Ini.ReadInteger(SEC_POSITION, 'Y', DEF_POSITION_Y);
    FPositionW := Ini.ReadInteger(SEC_POSITION, 'W', DEF_POSITION_W);
    FPositionH := Ini.ReadInteger(SEC_POSITION, 'H', DEF_POSITION_H);

    // [Polling]
    FPollingMode := TPollingMode(Ini.ReadInteger(SEC_POLLING, 'Mode', Ord(DEF_POLLING_MODE)));
    FPollingInterval := Ini.ReadInteger(SEC_POLLING, 'Interval', DEF_POLLING_INTERVAL);

    // [Log]
    FLogEnabled := Ini.ReadBool(SEC_LOG, 'Enabled', DEF_LOG_ENABLED);
    FLogFilename := Ini.ReadString(SEC_LOG, 'Filename', DEF_LOG_FILENAME);
    FLogAppend := Ini.ReadBool(SEC_LOG, 'Append', DEF_LOG_APPEND);

    // [Appearance]
    FShowAddresses := Ini.ReadBool(SEC_APPEARANCE, 'ShowAddresses', DEF_SHOW_ADDRESSES);
    FTheme := Ini.ReadString(SEC_APPEARANCE, 'Theme', DEF_THEME);
    FVsfDir := Ini.ReadString(SEC_APPEARANCE, 'VsfDir', DEF_VSF_DIR);

    // [Device] - global defaults
    FConnectionTimeout := Ini.ReadInteger(SEC_DEVICE, 'ConnectionTimeout', DEF_CONNECTION_TIMEOUT);
    FConnectionRetryCount := Ini.ReadInteger(SEC_DEVICE, 'ConnectionRetryCount', DEF_CONNECTION_RETRY_COUNT);
    FNotifyOnConnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, 'NotifyOnConnect', Ord(DEF_NOTIFY_ON_CONNECT)));
    FNotifyOnDisconnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, 'NotifyOnDisconnect', Ord(DEF_NOTIFY_ON_DISCONNECT)));
    FNotifyOnConnectFailed := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, 'NotifyOnConnectFailed', Ord(DEF_NOTIFY_ON_CONNECT_FAILED)));
    FNotifyOnAutoConnect := TNotificationMode(Ini.ReadInteger(SEC_DEVICE, 'NotifyOnAutoConnect', Ord(DEF_NOTIFY_ON_AUTO_CONNECT)));

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
    Ini.WriteInteger(SEC_GENERAL, 'Window', Ord(FWindowMode));
    Ini.WriteBool(SEC_GENERAL, 'OnTop', FOnTop);
    Ini.WriteBool(SEC_GENERAL, 'Autostart', FAutostart);

    // [Window]
    Ini.WriteBool(SEC_WINDOW, 'MinimizeToTray', FMinimizeToTray);
    Ini.WriteBool(SEC_WINDOW, 'CloseToTray', FCloseToTray);

    // [Menu]
    Ini.WriteBool(SEC_MENU, 'HideOnFocusLoss', FMenuHideOnFocusLoss);

    // [Hotkey]
    Ini.WriteString(SEC_HOTKEY, 'GlobalHotkey', FHotkey);
    Ini.WriteBool(SEC_HOTKEY, 'UseLowLevelHook', FUseLowLevelHook);

    // [Position]
    Ini.WriteInteger(SEC_POSITION, 'Mode', Ord(FPositionMode));
    Ini.WriteInteger(SEC_POSITION, 'X', FPositionX);
    Ini.WriteInteger(SEC_POSITION, 'Y', FPositionY);
    Ini.WriteInteger(SEC_POSITION, 'W', FPositionW);
    Ini.WriteInteger(SEC_POSITION, 'H', FPositionH);

    // [Polling]
    Ini.WriteInteger(SEC_POLLING, 'Mode', Ord(FPollingMode));
    Ini.WriteInteger(SEC_POLLING, 'Interval', FPollingInterval);

    // [Log]
    Ini.WriteBool(SEC_LOG, 'Enabled', FLogEnabled);
    Ini.WriteString(SEC_LOG, 'Filename', FLogFilename);
    Ini.WriteBool(SEC_LOG, 'Append', FLogAppend);

    // [Appearance]
    Ini.WriteBool(SEC_APPEARANCE, 'ShowAddresses', FShowAddresses);
    Ini.WriteString(SEC_APPEARANCE, 'Theme', FTheme);
    Ini.WriteString(SEC_APPEARANCE, 'VsfDir', FVsfDir);

    // [Device] - global defaults
    Ini.WriteInteger(SEC_DEVICE, 'ConnectionTimeout', FConnectionTimeout);
    Ini.WriteInteger(SEC_DEVICE, 'ConnectionRetryCount', FConnectionRetryCount);
    Ini.WriteInteger(SEC_DEVICE, 'NotifyOnConnect', Ord(FNotifyOnConnect));
    Ini.WriteInteger(SEC_DEVICE, 'NotifyOnDisconnect', Ord(FNotifyOnDisconnect));
    Ini.WriteInteger(SEC_DEVICE, 'NotifyOnConnectFailed', Ord(FNotifyOnConnectFailed));
    Ini.WriteInteger(SEC_DEVICE, 'NotifyOnAutoConnect', Ord(FNotifyOnAutoConnect));

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
