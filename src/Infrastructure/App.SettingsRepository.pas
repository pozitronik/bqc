{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Settings Repository - INI Persistence           }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Handles INI file persistence for application settings.
/// Separates persistence concern from configuration state management.
/// </summary>
unit App.SettingsRepository;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Math,
  System.DateUtils,
  System.Generics.Collections,
  App.ConfigInterfaces;

type
  /// <summary>
  /// INI file-based settings repository implementation.
  /// </summary>
  TIniSettingsRepository = class(TInterfacedObject, ISettingsRepository)
  private
    FConfigPath: string;
    FDeviceRepository: IDeviceConfigRepository;

    procedure LoadAppSettings(AIni: TMemIniFile; AConfig: TObject);
    procedure SaveAppSettings(AIni: TMemIniFile; AConfig: TObject);
    procedure ValidateRanges(AConfig: TObject);

  public
    constructor Create(const AConfigPath: string; ADeviceRepository: IDeviceConfigRepository);

    procedure LoadSettings(AConfig: TObject);
    procedure SaveSettings(AConfig: TObject);
    function GetConfigPath: string;
  end;

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

  // INI key names - [Device] (global defaults)
  KEY_CONNECTION_TIMEOUT = 'ConnectionTimeout';
  KEY_CONNECTION_RETRY_COUNT = 'ConnectionRetryCount';
  KEY_NOTIFY_ON_CONNECT = 'NotifyOnConnect';
  KEY_NOTIFY_ON_DISCONNECT = 'NotifyOnDisconnect';
  KEY_NOTIFY_ON_CONNECT_FAILED = 'NotifyOnConnectFailed';
  KEY_NOTIFY_ON_AUTO_CONNECT = 'NotifyOnAutoConnect';

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
  DEF_SHOW_LAST_SEEN = False;
  DEF_LAST_SEEN_FORMAT = lsfRelative;
  DEF_SHOW_DEVICE_ICONS = True;
  DEF_NOTIFY_ON_CONNECT = nmBalloon;
  DEF_NOTIFY_ON_DISCONNECT = nmBalloon;
  DEF_NOTIFY_ON_CONNECT_FAILED = nmBalloon;
  DEF_NOTIFY_ON_AUTO_CONNECT = nmBalloon;

implementation

uses
  App.Config;

{ TIniSettingsRepository }

constructor TIniSettingsRepository.Create(const AConfigPath: string;
  ADeviceRepository: IDeviceConfigRepository);
begin
  inherited Create;
  FConfigPath := AConfigPath;
  FDeviceRepository := ADeviceRepository;
end;

function TIniSettingsRepository.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

procedure TIniSettingsRepository.LoadSettings(AConfig: TObject);
var
  Ini: TMemIniFile;
  Cfg: TAppConfig;
begin
  Cfg := TAppConfig(AConfig);

  if not FileExists(FConfigPath) then
  begin
    // File doesn't exist - defaults already set, just save
    SaveSettings(AConfig);
    Cfg.ClearModified;
    Exit;
  end;

  Ini := TMemIniFile.Create(FConfigPath);
  try
    LoadAppSettings(Ini, AConfig);
    ValidateRanges(AConfig);

    // Load device-specific settings
    if Assigned(FDeviceRepository) then
      FDeviceRepository.LoadFrom(Ini);

    Cfg.ClearModified;
  finally
    Ini.Free;
  end;
end;

procedure TIniSettingsRepository.LoadAppSettings(AIni: TMemIniFile; AConfig: TObject);
var
  Cfg: TAppConfig;
begin
  Cfg := TAppConfig(AConfig);

  // [General]
  Cfg.WindowMode := TWindowMode(AIni.ReadInteger(SEC_GENERAL, KEY_WINDOW, Ord(DEF_WINDOW_MODE)));
  Cfg.OnTop := AIni.ReadBool(SEC_GENERAL, KEY_ON_TOP, DEF_ON_TOP);
  Cfg.Autostart := AIni.ReadBool(SEC_GENERAL, KEY_AUTOSTART, DEF_AUTOSTART);

  // [Window]
  Cfg.MinimizeToTray := AIni.ReadBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, DEF_MINIMIZE_TO_TRAY);
  Cfg.CloseToTray := AIni.ReadBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, DEF_CLOSE_TO_TRAY);

  // [Menu]
  Cfg.MenuHideOnFocusLoss := AIni.ReadBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, DEF_MENU_HIDE_ON_FOCUS_LOSS);

  // [Hotkey]
  Cfg.Hotkey := AIni.ReadString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, DEF_HOTKEY);
  Cfg.UseLowLevelHook := AIni.ReadBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, DEF_USE_LOW_LEVEL_HOOK);

  // [Position]
  Cfg.PositionMode := TPositionMode(AIni.ReadInteger(SEC_POSITION, KEY_MODE, Ord(DEF_POSITION_MODE)));
  Cfg.PositionX := AIni.ReadInteger(SEC_POSITION, KEY_X, DEF_POSITION_X);
  Cfg.PositionY := AIni.ReadInteger(SEC_POSITION, KEY_Y, DEF_POSITION_Y);
  Cfg.PositionW := AIni.ReadInteger(SEC_POSITION, KEY_W, DEF_POSITION_W);
  Cfg.PositionH := AIni.ReadInteger(SEC_POSITION, KEY_H, DEF_POSITION_H);

  // [Polling]
  Cfg.PollingMode := TPollingMode(AIni.ReadInteger(SEC_POLLING, KEY_MODE, Ord(DEF_POLLING_MODE)));
  Cfg.PollingInterval := AIni.ReadInteger(SEC_POLLING, KEY_INTERVAL, DEF_POLLING_INTERVAL);
  Cfg.EventDebounceMs := AIni.ReadInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, DEF_EVENT_DEBOUNCE_MS);

  // [Log]
  Cfg.LogEnabled := AIni.ReadBool(SEC_LOG, KEY_ENABLED, DEF_LOG_ENABLED);
  Cfg.LogFilename := AIni.ReadString(SEC_LOG, KEY_FILENAME, DEF_LOG_FILENAME);
  Cfg.LogAppend := AIni.ReadBool(SEC_LOG, KEY_APPEND, DEF_LOG_APPEND);

  // [Appearance]
  Cfg.ShowAddresses := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, DEF_SHOW_ADDRESSES);
  Cfg.Theme := AIni.ReadString(SEC_APPEARANCE, KEY_THEME, DEF_THEME);
  Cfg.VsfDir := AIni.ReadString(SEC_APPEARANCE, KEY_VSF_DIR, DEF_VSF_DIR);
  Cfg.ShowLastSeen := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, DEF_SHOW_LAST_SEEN);
  Cfg.LastSeenFormat := TLastSeenFormat(AIni.ReadInteger(SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(DEF_LAST_SEEN_FORMAT)));
  Cfg.ShowDeviceIcons := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, DEF_SHOW_DEVICE_ICONS);
  Cfg.ConnectedColor := AIni.ReadInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, DEF_CONNECTED_COLOR);

  // [Layout]
  Cfg.ItemHeight := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, DEF_ITEM_HEIGHT);
  Cfg.ItemPadding := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_PADDING, DEF_ITEM_PADDING);
  Cfg.ItemMargin := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, DEF_ITEM_MARGIN);
  Cfg.IconSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ICON_SIZE, DEF_ICON_SIZE);
  Cfg.CornerRadius := AIni.ReadInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, DEF_CORNER_RADIUS);
  Cfg.DeviceNameFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, DEF_DEVICE_NAME_FONT_SIZE);
  Cfg.StatusFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, DEF_STATUS_FONT_SIZE);
  Cfg.AddressFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, DEF_ADDRESS_FONT_SIZE);
  Cfg.IconFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, DEF_ICON_FONT_SIZE);
  Cfg.ItemBorderWidth := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, DEF_ITEM_BORDER_WIDTH);
  Cfg.ItemBorderColor := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, DEF_ITEM_BORDER_COLOR);

  // [Device] - global defaults
  Cfg.ConnectionTimeout := AIni.ReadInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, DEF_CONNECTION_TIMEOUT);
  Cfg.ConnectionRetryCount := AIni.ReadInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, DEF_CONNECTION_RETRY_COUNT);
  Cfg.NotifyOnConnect := TNotificationMode(AIni.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(DEF_NOTIFY_ON_CONNECT)));
  Cfg.NotifyOnDisconnect := TNotificationMode(AIni.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(DEF_NOTIFY_ON_DISCONNECT)));
  Cfg.NotifyOnConnectFailed := TNotificationMode(AIni.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(DEF_NOTIFY_ON_CONNECT_FAILED)));
  Cfg.NotifyOnAutoConnect := TNotificationMode(AIni.ReadInteger(SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(DEF_NOTIFY_ON_AUTO_CONNECT)));
end;

procedure TIniSettingsRepository.ValidateRanges(AConfig: TObject);
var
  Cfg: TAppConfig;
begin
  Cfg := TAppConfig(AConfig);

  // Validate numeric values to ensure they're within acceptable ranges
  Cfg.PollingInterval := EnsureRange(Cfg.PollingInterval, MIN_POLLING_INTERVAL, MAX_POLLING_INTERVAL);
  Cfg.EventDebounceMs := EnsureRange(Cfg.EventDebounceMs, MIN_EVENT_DEBOUNCE_MS, MAX_EVENT_DEBOUNCE_MS);
  Cfg.ItemHeight := EnsureRange(Cfg.ItemHeight, MIN_ITEM_HEIGHT, MAX_ITEM_HEIGHT);
  Cfg.ItemPadding := EnsureRange(Cfg.ItemPadding, MIN_ITEM_PADDING, MAX_ITEM_PADDING);
  Cfg.ItemMargin := EnsureRange(Cfg.ItemMargin, MIN_ITEM_MARGIN, MAX_ITEM_MARGIN);
  Cfg.IconSize := EnsureRange(Cfg.IconSize, MIN_ICON_SIZE, MAX_ICON_SIZE);
  Cfg.CornerRadius := EnsureRange(Cfg.CornerRadius, MIN_CORNER_RADIUS, MAX_CORNER_RADIUS);
  Cfg.ItemBorderWidth := EnsureRange(Cfg.ItemBorderWidth, MIN_ITEM_BORDER_WIDTH, MAX_ITEM_BORDER_WIDTH);
  Cfg.DeviceNameFontSize := EnsureRange(Cfg.DeviceNameFontSize, MIN_DEVICE_NAME_FONT_SIZE, MAX_DEVICE_NAME_FONT_SIZE);
  Cfg.StatusFontSize := EnsureRange(Cfg.StatusFontSize, MIN_STATUS_FONT_SIZE, MAX_STATUS_FONT_SIZE);
  Cfg.AddressFontSize := EnsureRange(Cfg.AddressFontSize, MIN_ADDRESS_FONT_SIZE, MAX_ADDRESS_FONT_SIZE);
  Cfg.IconFontSize := EnsureRange(Cfg.IconFontSize, MIN_ICON_FONT_SIZE, MAX_ICON_FONT_SIZE);
  Cfg.ConnectionTimeout := EnsureRange(Cfg.ConnectionTimeout, MIN_CONNECTION_TIMEOUT, MAX_CONNECTION_TIMEOUT);
  Cfg.ConnectionRetryCount := EnsureRange(Cfg.ConnectionRetryCount, MIN_CONNECTION_RETRY_COUNT, MAX_CONNECTION_RETRY_COUNT);
end;

procedure TIniSettingsRepository.SaveSettings(AConfig: TObject);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FConfigPath);
  try
    SaveAppSettings(Ini, AConfig);

    // Save device-specific settings
    if Assigned(FDeviceRepository) then
      FDeviceRepository.SaveTo(Ini);

    Ini.UpdateFile;
    TAppConfig(AConfig).ClearModified;
  finally
    Ini.Free;
  end;
end;

procedure TIniSettingsRepository.SaveAppSettings(AIni: TMemIniFile; AConfig: TObject);
var
  Cfg: TAppConfig;
begin
  Cfg := TAppConfig(AConfig);

  // [General]
  AIni.WriteInteger(SEC_GENERAL, KEY_WINDOW, Ord(Cfg.WindowMode));
  AIni.WriteBool(SEC_GENERAL, KEY_ON_TOP, Cfg.OnTop);
  AIni.WriteBool(SEC_GENERAL, KEY_AUTOSTART, Cfg.Autostart);

  // [Window]
  AIni.WriteBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, Cfg.MinimizeToTray);
  AIni.WriteBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, Cfg.CloseToTray);

  // [Menu]
  AIni.WriteBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, Cfg.MenuHideOnFocusLoss);

  // [Hotkey]
  AIni.WriteString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, Cfg.Hotkey);
  AIni.WriteBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, Cfg.UseLowLevelHook);

  // [Position]
  AIni.WriteInteger(SEC_POSITION, KEY_MODE, Ord(Cfg.PositionMode));
  AIni.WriteInteger(SEC_POSITION, KEY_X, Cfg.PositionX);
  AIni.WriteInteger(SEC_POSITION, KEY_Y, Cfg.PositionY);
  AIni.WriteInteger(SEC_POSITION, KEY_W, Cfg.PositionW);
  AIni.WriteInteger(SEC_POSITION, KEY_H, Cfg.PositionH);

  // [Polling]
  AIni.WriteInteger(SEC_POLLING, KEY_MODE, Ord(Cfg.PollingMode));
  AIni.WriteInteger(SEC_POLLING, KEY_INTERVAL, Cfg.PollingInterval);
  AIni.WriteInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, Cfg.EventDebounceMs);

  // [Log]
  AIni.WriteBool(SEC_LOG, KEY_ENABLED, Cfg.LogEnabled);
  AIni.WriteString(SEC_LOG, KEY_FILENAME, Cfg.LogFilename);
  AIni.WriteBool(SEC_LOG, KEY_APPEND, Cfg.LogAppend);

  // [Appearance]
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, Cfg.ShowAddresses);
  AIni.WriteString(SEC_APPEARANCE, KEY_THEME, Cfg.Theme);
  AIni.WriteString(SEC_APPEARANCE, KEY_VSF_DIR, Cfg.VsfDir);
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, Cfg.ShowLastSeen);
  AIni.WriteInteger(SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(Cfg.LastSeenFormat));
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, Cfg.ShowDeviceIcons);
  AIni.WriteInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, Cfg.ConnectedColor);

  // [Layout]
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, Cfg.ItemHeight);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_PADDING, Cfg.ItemPadding);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, Cfg.ItemMargin);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ICON_SIZE, Cfg.IconSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, Cfg.CornerRadius);
  AIni.WriteInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, Cfg.DeviceNameFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, Cfg.StatusFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, Cfg.AddressFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, Cfg.IconFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, Cfg.ItemBorderWidth);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, Cfg.ItemBorderColor);

  // [Device] - global defaults
  AIni.WriteInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, Cfg.ConnectionTimeout);
  AIni.WriteInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, Cfg.ConnectionRetryCount);
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(Cfg.NotifyOnConnect));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(Cfg.NotifyOnDisconnect));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(Cfg.NotifyOnConnectFailed));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(Cfg.NotifyOnAutoConnect));
end;

end.
