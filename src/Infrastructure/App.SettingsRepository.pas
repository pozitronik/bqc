{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Settings Repository - INI Persistence           }
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
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.LogConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.NotificationConfigIntf,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf;

type
  /// <summary>
  /// INI file-based settings repository implementation.
  /// Uses IDeviceConfigPersistence for device config (ISP-compliant - only needs persistence).
  /// </summary>
  TIniSettingsRepository = class(TInterfacedObject, ISettingsRepository)
  private
    FConfigPath: string;
    FDevicePersistence: IDeviceConfigPersistence;

    procedure LoadAppSettings(AIni: TMemIniFile; AConfig: IAppConfig);
    procedure SaveAppSettings(AIni: TMemIniFile; AConfig: IAppConfig);
    procedure ValidateRanges(AConfig: IAppConfig);

  public
    constructor Create(const AConfigPath: string; ADevicePersistence: IDeviceConfigPersistence);
    destructor Destroy; override;

    procedure LoadSettings(AConfig: IAppConfig);
    procedure SaveSettings(AConfig: IAppConfig);
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
  SEC_BATTERY_TRAY = 'BatteryTray';
  SEC_PROFILE = 'Profile';

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
  KEY_CAST_PANEL_HOTKEY = 'CastPanelHotkey';
  KEY_BLUETOOTH_PANEL_HOTKEY = 'BluetoothPanelHotkey';

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
  KEY_LEVEL = 'Level';

  // INI key names - [Appearance]
  KEY_SHOW_ADDRESSES = 'ShowAddresses';
  KEY_THEME = 'Theme';
  KEY_VSF_DIR = 'VsfDir';
  KEY_SHOW_LAST_SEEN = 'ShowLastSeen';
  KEY_LAST_SEEN_FORMAT = 'LastSeenFormat';
  KEY_SHOW_DEVICE_ICONS = 'ShowDeviceIcons';
  KEY_CONNECTED_COLOR = 'ConnectedColor';
  KEY_SHOW_BATTERY_LEVEL = 'ShowBatteryLevel';

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
  KEY_ENUMERATION_MODE = 'EnumerationMode';
  KEY_BLUETOOTH_PLATFORM = 'BluetoothPlatform';
  KEY_NOTIFY_ON_CONNECT = 'NotifyOnConnect';
  KEY_NOTIFY_ON_DISCONNECT = 'NotifyOnDisconnect';
  KEY_NOTIFY_ON_CONNECT_FAILED = 'NotifyOnConnectFailed';
  KEY_NOTIFY_ON_AUTO_CONNECT = 'NotifyOnAutoConnect';

  // INI key names - [BatteryTray]
  KEY_SHOW_BATTERY_TRAY_ICONS = 'ShowBatteryTrayIcons';
  KEY_DEFAULT_ICON_COLOR = 'DefaultIconColor';
  KEY_DEFAULT_BACKGROUND_COLOR = 'DefaultBackgroundColor';
  KEY_DEFAULT_SHOW_NUMERIC_VALUE = 'DefaultShowNumericValue';
  KEY_DEFAULT_LOW_BATTERY_THRESHOLD = 'DefaultLowBatteryThreshold';
  KEY_DEFAULT_NOTIFY_LOW_BATTERY = 'DefaultNotifyLowBattery';
  KEY_DEFAULT_NOTIFY_FULLY_CHARGED = 'DefaultNotifyFullyCharged';
  KEY_DEFAULT_OUTLINE_COLOR_MODE = 'DefaultOutlineColorMode';
  KEY_DEFAULT_CUSTOM_OUTLINE_COLOR = 'DefaultCustomOutlineColor';

  // INI key names - [Profile]
  KEY_SHOW_PROFILES = 'ShowProfiles';
  KEY_PROFILE_FONT_SIZE = 'ProfileFontSize';

  // Default values
  DEF_WINDOW_MODE = wmWindow;
  DEF_ON_TOP = False;
  DEF_AUTOSTART = False;
  DEF_MINIMIZE_TO_TRAY = True;
  DEF_CLOSE_TO_TRAY = True;
  DEF_MENU_HIDE_ON_FOCUS_LOSS = True;
  DEF_HOTKEY = 'Win+K';
  DEF_USE_LOW_LEVEL_HOOK = True;
  DEF_CAST_PANEL_HOTKEY = '';
  DEF_BLUETOOTH_PANEL_HOTKEY = '';
  DEF_POSITION_MODE = pmCoordinates;
  DEF_POSITION_X = -1;
  DEF_POSITION_Y = -1;
  DEF_POSITION_W = -1;
  DEF_POSITION_H = -1;
  DEF_POLLING_MODE = pmFallback;
  DEF_LOG_ENABLED = False;
  DEF_LOG_FILENAME = 'bqc.log';
  DEF_LOG_APPEND = False;
  DEF_LOG_LEVEL = llError;
  DEF_SHOW_ADDRESSES = False;
  DEF_THEME = '';
  DEF_VSF_DIR = 'themes';
  DEF_SHOW_LAST_SEEN = False;
  DEF_LAST_SEEN_FORMAT = lsfRelative;
  DEF_SHOW_DEVICE_ICONS = True;
  DEF_SHOW_BATTERY_LEVEL = True;
  DEF_NOTIFY_ON_CONNECT = nmNone;
  DEF_NOTIFY_ON_DISCONNECT = nmNone;
  DEF_NOTIFY_ON_CONNECT_FAILED = nmNone;
  DEF_NOTIFY_ON_AUTO_CONNECT = nmNone;

  // [Profile] defaults
  DEF_SHOW_PROFILES = False;      // Disabled by default, user opt-in
  DEF_PROFILE_FONT_SIZE = 7;      // Small font for profile tree

implementation

uses
  System.TypInfo,
  App.Config,
  App.ConfigSection.BatteryTray,
  App.Logger;

{ Generic helper for safe enum reading using RTTI }

/// <summary>
/// Reads an enumeration value from INI file with range validation.
/// Uses RTTI to determine valid range and type name for logging.
/// Returns ADefault if value is out of range or missing.
/// </summary>
function SafeReadEnum(AIni: TMemIniFile; const ASection, AKey: string;
  ADefault: Integer; ATypeInfo: PTypeInfo): Integer;
var
  Value: Integer;
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(ATypeInfo);
  Value := AIni.ReadInteger(ASection, AKey, ADefault);
  if (Value >= TypeData.MinValue) and (Value <= TypeData.MaxValue) then
    Result := Value
  else
  begin
    LogWarning('Invalid %s value %d in INI for key [%s]%s, using default',
      [string(ATypeInfo.Name), Value, ASection, AKey], 'SettingsRepository');
    Result := ADefault;
  end;
end;

{ TIniSettingsRepository }

constructor TIniSettingsRepository.Create(const AConfigPath: string;
  ADevicePersistence: IDeviceConfigPersistence);
begin
  inherited Create;
  FConfigPath := AConfigPath;
  FDevicePersistence := ADevicePersistence;
end;

destructor TIniSettingsRepository.Destroy;
begin
  FDevicePersistence := nil;
  inherited Destroy;
end;

function TIniSettingsRepository.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

procedure TIniSettingsRepository.LoadSettings(AConfig: IAppConfig);
var
  Ini: TMemIniFile;
begin
  if not FileExists(FConfigPath) then
  begin
    // File doesn't exist - defaults already set, just save
    SaveSettings(AConfig);
    Exit;
  end;

  Ini := TMemIniFile.Create(FConfigPath);
  try
    LoadAppSettings(Ini, AConfig);
    ValidateRanges(AConfig);

    // Load device-specific settings
    if Assigned(FDevicePersistence) then
      FDevicePersistence.LoadFrom(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TIniSettingsRepository.LoadAppSettings(AIni: TMemIniFile; AConfig: IAppConfig);
var
  GeneralCfg: IGeneralConfig;
  WindowCfg: IWindowConfig;
  PositionCfg: IPositionConfig;
  HotkeyCfg: IHotkeyConfig;
  PollingCfg: IPollingConfig;
  LogCfg: ILogConfig;
  AppearanceCfg: IAppearanceConfig;
  LayoutCfg: ILayoutConfig;
  ConnectionCfg: IConnectionConfig;
  NotificationCfg: INotificationConfig;
  BatteryTrayCfg: IBatteryTrayConfig;
  ProfileCfg: IProfileConfig;
begin
  GeneralCfg := AConfig.AsGeneralConfig;
  WindowCfg := AConfig.AsWindowConfig;
  PositionCfg := AConfig.AsPositionConfig;
  HotkeyCfg := AConfig.AsHotkeyConfig;
  PollingCfg := AConfig.AsPollingConfig;
  LogCfg := AConfig.AsLogConfig;
  AppearanceCfg := AConfig.AsAppearanceConfig;
  LayoutCfg := AConfig.AsLayoutConfig;
  ConnectionCfg := AConfig.AsConnectionConfig;
  NotificationCfg := AConfig.AsNotificationConfig;
  BatteryTrayCfg := AConfig.AsBatteryTrayConfig;
  ProfileCfg := AConfig.AsProfileConfig;

  // [General]
  GeneralCfg.WindowMode := TWindowMode(SafeReadEnum(AIni, SEC_GENERAL, KEY_WINDOW, Ord(DEF_WINDOW_MODE), TypeInfo(TWindowMode)));
  GeneralCfg.OnTop := AIni.ReadBool(SEC_GENERAL, KEY_ON_TOP, DEF_ON_TOP);
  GeneralCfg.Autostart := AIni.ReadBool(SEC_GENERAL, KEY_AUTOSTART, DEF_AUTOSTART);

  // [Window]
  WindowCfg.MinimizeToTray := AIni.ReadBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, DEF_MINIMIZE_TO_TRAY);
  WindowCfg.CloseToTray := AIni.ReadBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, DEF_CLOSE_TO_TRAY);

  // [Menu]
  WindowCfg.MenuHideOnFocusLoss := AIni.ReadBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, DEF_MENU_HIDE_ON_FOCUS_LOSS);

  // [Hotkey]
  HotkeyCfg.Hotkey := AIni.ReadString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, DEF_HOTKEY);
  HotkeyCfg.UseLowLevelHook := AIni.ReadBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, DEF_USE_LOW_LEVEL_HOOK);
  HotkeyCfg.CastPanelHotkey := AIni.ReadString(SEC_HOTKEY, KEY_CAST_PANEL_HOTKEY, DEF_CAST_PANEL_HOTKEY);
  HotkeyCfg.BluetoothPanelHotkey := AIni.ReadString(SEC_HOTKEY, KEY_BLUETOOTH_PANEL_HOTKEY, DEF_BLUETOOTH_PANEL_HOTKEY);

  // [Position]
  PositionCfg.PositionMode := TPositionMode(SafeReadEnum(AIni, SEC_POSITION, KEY_MODE, Ord(DEF_POSITION_MODE), TypeInfo(TPositionMode)));
  PositionCfg.PositionX := AIni.ReadInteger(SEC_POSITION, KEY_X, DEF_POSITION_X);
  PositionCfg.PositionY := AIni.ReadInteger(SEC_POSITION, KEY_Y, DEF_POSITION_Y);
  PositionCfg.PositionW := AIni.ReadInteger(SEC_POSITION, KEY_W, DEF_POSITION_W);
  PositionCfg.PositionH := AIni.ReadInteger(SEC_POSITION, KEY_H, DEF_POSITION_H);

  // [Polling]
  PollingCfg.PollingMode := TPollingMode(SafeReadEnum(AIni, SEC_POLLING, KEY_MODE, Ord(DEF_POLLING_MODE), TypeInfo(TPollingMode)));
  PollingCfg.PollingInterval := AIni.ReadInteger(SEC_POLLING, KEY_INTERVAL, DEF_POLLING_INTERVAL);
  PollingCfg.EventDebounceMs := AIni.ReadInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, DEF_EVENT_DEBOUNCE_MS);

  // [Log]
  LogCfg.LogEnabled := AIni.ReadBool(SEC_LOG, KEY_ENABLED, DEF_LOG_ENABLED);
  LogCfg.LogFilename := AIni.ReadString(SEC_LOG, KEY_FILENAME, DEF_LOG_FILENAME);
  LogCfg.LogAppend := AIni.ReadBool(SEC_LOG, KEY_APPEND, DEF_LOG_APPEND);
  LogCfg.LogLevel := TLogLevel(SafeReadEnum(AIni, SEC_LOG, KEY_LEVEL, Ord(DEF_LOG_LEVEL), TypeInfo(TLogLevel)));

  // [Appearance]
  AppearanceCfg.ShowAddresses := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, DEF_SHOW_ADDRESSES);
  AppearanceCfg.Theme := AIni.ReadString(SEC_APPEARANCE, KEY_THEME, DEF_THEME);
  AppearanceCfg.VsfDir := AIni.ReadString(SEC_APPEARANCE, KEY_VSF_DIR, DEF_VSF_DIR);
  AppearanceCfg.ShowLastSeen := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, DEF_SHOW_LAST_SEEN);
  AppearanceCfg.LastSeenFormat := TLastSeenFormat(SafeReadEnum(AIni, SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(DEF_LAST_SEEN_FORMAT), TypeInfo(TLastSeenFormat)));
  AppearanceCfg.ShowDeviceIcons := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, DEF_SHOW_DEVICE_ICONS);
  AppearanceCfg.ConnectedColor := AIni.ReadInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, DEF_CONNECTED_COLOR);
  AppearanceCfg.ShowBatteryLevel := AIni.ReadBool(SEC_APPEARANCE, KEY_SHOW_BATTERY_LEVEL, DEF_SHOW_BATTERY_LEVEL);

  // [Layout]
  LayoutCfg.ItemHeight := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, DEF_ITEM_HEIGHT);
  LayoutCfg.ItemPadding := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_PADDING, DEF_ITEM_PADDING);
  LayoutCfg.ItemMargin := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, DEF_ITEM_MARGIN);
  LayoutCfg.IconSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ICON_SIZE, DEF_ICON_SIZE);
  LayoutCfg.CornerRadius := AIni.ReadInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, DEF_CORNER_RADIUS);
  LayoutCfg.DeviceNameFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, DEF_DEVICE_NAME_FONT_SIZE);
  LayoutCfg.StatusFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, DEF_STATUS_FONT_SIZE);
  LayoutCfg.AddressFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, DEF_ADDRESS_FONT_SIZE);
  LayoutCfg.IconFontSize := AIni.ReadInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, DEF_ICON_FONT_SIZE);
  LayoutCfg.ItemBorderWidth := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, DEF_ITEM_BORDER_WIDTH);
  LayoutCfg.ItemBorderColor := AIni.ReadInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, DEF_ITEM_BORDER_COLOR);

  // [Device] - global defaults
  ConnectionCfg.ConnectionTimeout := AIni.ReadInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, DEF_CONNECTION_TIMEOUT);
  ConnectionCfg.ConnectionRetryCount := AIni.ReadInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, DEF_CONNECTION_RETRY_COUNT);
  ConnectionCfg.EnumerationMode := TEnumerationMode(SafeReadEnum(AIni, SEC_DEVICE, KEY_ENUMERATION_MODE, Ord(emComposite), TypeInfo(TEnumerationMode)));
  ConnectionCfg.BluetoothPlatform := TBluetoothPlatform(SafeReadEnum(AIni, SEC_DEVICE, KEY_BLUETOOTH_PLATFORM, Ord(bpAuto), TypeInfo(TBluetoothPlatform)));
  NotificationCfg.NotifyOnConnect := TNotificationMode(SafeReadEnum(AIni, SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(DEF_NOTIFY_ON_CONNECT), TypeInfo(TNotificationMode)));
  NotificationCfg.NotifyOnDisconnect := TNotificationMode(SafeReadEnum(AIni, SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(DEF_NOTIFY_ON_DISCONNECT), TypeInfo(TNotificationMode)));
  NotificationCfg.NotifyOnConnectFailed := TNotificationMode(SafeReadEnum(AIni, SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(DEF_NOTIFY_ON_CONNECT_FAILED), TypeInfo(TNotificationMode)));
  NotificationCfg.NotifyOnAutoConnect := TNotificationMode(SafeReadEnum(AIni, SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(DEF_NOTIFY_ON_AUTO_CONNECT), TypeInfo(TNotificationMode)));

  // [BatteryTray] - battery tray icon defaults
  BatteryTrayCfg.ShowBatteryTrayIcons := AIni.ReadBool(SEC_BATTERY_TRAY, KEY_SHOW_BATTERY_TRAY_ICONS, DEF_SHOW_BATTERY_TRAY_ICONS);
  BatteryTrayCfg.DefaultIconColor := AIni.ReadInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_ICON_COLOR, DEF_BATTERY_ICON_COLOR);
  BatteryTrayCfg.DefaultBackgroundColor := AIni.ReadInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_BACKGROUND_COLOR, DEF_BATTERY_BACKGROUND_COLOR);
  BatteryTrayCfg.DefaultShowNumericValue := AIni.ReadBool(SEC_BATTERY_TRAY, KEY_DEFAULT_SHOW_NUMERIC_VALUE, DEF_SHOW_NUMERIC_VALUE);
  BatteryTrayCfg.DefaultLowBatteryThreshold := AIni.ReadInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_LOW_BATTERY_THRESHOLD, DEF_LOW_BATTERY_THRESHOLD);
  BatteryTrayCfg.DefaultNotifyLowBattery := AIni.ReadBool(SEC_BATTERY_TRAY, KEY_DEFAULT_NOTIFY_LOW_BATTERY, DEF_NOTIFY_LOW_BATTERY);
  BatteryTrayCfg.DefaultNotifyFullyCharged := AIni.ReadBool(SEC_BATTERY_TRAY, KEY_DEFAULT_NOTIFY_FULLY_CHARGED, DEF_NOTIFY_FULLY_CHARGED);
  BatteryTrayCfg.DefaultOutlineColorMode := TOutlineColorMode(SafeReadEnum(AIni, SEC_BATTERY_TRAY, KEY_DEFAULT_OUTLINE_COLOR_MODE, Ord(DEF_OUTLINE_COLOR_MODE), TypeInfo(TOutlineColorMode)));
  BatteryTrayCfg.DefaultCustomOutlineColor := AIni.ReadInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_CUSTOM_OUTLINE_COLOR, DEF_CUSTOM_OUTLINE_COLOR);

  // [Profile] section
  ProfileCfg.ShowProfiles := AIni.ReadBool(SEC_PROFILE, KEY_SHOW_PROFILES, DEF_SHOW_PROFILES);
  ProfileCfg.ProfileFontSize := AIni.ReadInteger(SEC_PROFILE, KEY_PROFILE_FONT_SIZE, DEF_PROFILE_FONT_SIZE);
end;

procedure TIniSettingsRepository.ValidateRanges(AConfig: IAppConfig);
var
  PollingCfg: IPollingConfig;
  LayoutCfg: ILayoutConfig;
  ConnectionCfg: IConnectionConfig;
begin
  PollingCfg := AConfig.AsPollingConfig;
  LayoutCfg := AConfig.AsLayoutConfig;
  ConnectionCfg := AConfig.AsConnectionConfig;

  // Validate numeric values to ensure they're within acceptable ranges
  PollingCfg.PollingInterval := EnsureRange(PollingCfg.PollingInterval, MIN_POLLING_INTERVAL, MAX_POLLING_INTERVAL);
  LayoutCfg.ItemHeight := EnsureRange(LayoutCfg.ItemHeight, MIN_ITEM_HEIGHT, MAX_ITEM_HEIGHT);
  LayoutCfg.ItemPadding := EnsureRange(LayoutCfg.ItemPadding, MIN_ITEM_PADDING, MAX_ITEM_PADDING);
  LayoutCfg.ItemMargin := EnsureRange(LayoutCfg.ItemMargin, MIN_ITEM_MARGIN, MAX_ITEM_MARGIN);
  LayoutCfg.IconSize := EnsureRange(LayoutCfg.IconSize, MIN_ICON_SIZE, MAX_ICON_SIZE);
  LayoutCfg.CornerRadius := EnsureRange(LayoutCfg.CornerRadius, MIN_CORNER_RADIUS, MAX_CORNER_RADIUS);
  LayoutCfg.ItemBorderWidth := EnsureRange(LayoutCfg.ItemBorderWidth, MIN_ITEM_BORDER_WIDTH, MAX_ITEM_BORDER_WIDTH);
  LayoutCfg.DeviceNameFontSize := EnsureRange(LayoutCfg.DeviceNameFontSize, MIN_DEVICE_NAME_FONT_SIZE, MAX_DEVICE_NAME_FONT_SIZE);
  LayoutCfg.StatusFontSize := EnsureRange(LayoutCfg.StatusFontSize, MIN_STATUS_FONT_SIZE, MAX_STATUS_FONT_SIZE);
  LayoutCfg.AddressFontSize := EnsureRange(LayoutCfg.AddressFontSize, MIN_ADDRESS_FONT_SIZE, MAX_ADDRESS_FONT_SIZE);
  LayoutCfg.IconFontSize := EnsureRange(LayoutCfg.IconFontSize, MIN_ICON_FONT_SIZE, MAX_ICON_FONT_SIZE);
  ConnectionCfg.ConnectionTimeout := EnsureRange(ConnectionCfg.ConnectionTimeout, MIN_CONNECTION_TIMEOUT, MAX_CONNECTION_TIMEOUT);
  ConnectionCfg.ConnectionRetryCount := EnsureRange(ConnectionCfg.ConnectionRetryCount, MIN_CONNECTION_RETRY_COUNT, MAX_CONNECTION_RETRY_COUNT);
end;

procedure TIniSettingsRepository.SaveSettings(AConfig: IAppConfig);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FConfigPath);
  try
    SaveAppSettings(Ini, AConfig);

    // Save device-specific settings
    if Assigned(FDevicePersistence) then
      FDevicePersistence.SaveTo(Ini);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TIniSettingsRepository.SaveAppSettings(AIni: TMemIniFile; AConfig: IAppConfig);
var
  GeneralCfg: IGeneralConfig;
  WindowCfg: IWindowConfig;
  PositionCfg: IPositionConfig;
  HotkeyCfg: IHotkeyConfig;
  PollingCfg: IPollingConfig;
  LogCfg: ILogConfig;
  AppearanceCfg: IAppearanceConfig;
  LayoutCfg: ILayoutConfig;
  ConnectionCfg: IConnectionConfig;
  NotificationCfg: INotificationConfig;
  BatteryTrayCfg: IBatteryTrayConfig;
  ProfileCfg: IProfileConfig;
begin
  GeneralCfg := AConfig.AsGeneralConfig;
  WindowCfg := AConfig.AsWindowConfig;
  PositionCfg := AConfig.AsPositionConfig;
  HotkeyCfg := AConfig.AsHotkeyConfig;
  PollingCfg := AConfig.AsPollingConfig;
  LogCfg := AConfig.AsLogConfig;
  AppearanceCfg := AConfig.AsAppearanceConfig;
  LayoutCfg := AConfig.AsLayoutConfig;
  ConnectionCfg := AConfig.AsConnectionConfig;
  NotificationCfg := AConfig.AsNotificationConfig;
  BatteryTrayCfg := AConfig.AsBatteryTrayConfig;
  ProfileCfg := AConfig.AsProfileConfig;

  // [General]
  AIni.WriteInteger(SEC_GENERAL, KEY_WINDOW, Ord(GeneralCfg.WindowMode));
  AIni.WriteBool(SEC_GENERAL, KEY_ON_TOP, GeneralCfg.OnTop);
  AIni.WriteBool(SEC_GENERAL, KEY_AUTOSTART, GeneralCfg.Autostart);

  // [Window]
  AIni.WriteBool(SEC_WINDOW, KEY_MINIMIZE_TO_TRAY, WindowCfg.MinimizeToTray);
  AIni.WriteBool(SEC_WINDOW, KEY_CLOSE_TO_TRAY, WindowCfg.CloseToTray);

  // [Menu]
  AIni.WriteBool(SEC_MENU, KEY_HIDE_ON_FOCUS_LOSS, WindowCfg.MenuHideOnFocusLoss);

  // [Hotkey]
  AIni.WriteString(SEC_HOTKEY, KEY_GLOBAL_HOTKEY, HotkeyCfg.Hotkey);
  AIni.WriteBool(SEC_HOTKEY, KEY_USE_LOW_LEVEL_HOOK, HotkeyCfg.UseLowLevelHook);
  AIni.WriteString(SEC_HOTKEY, KEY_CAST_PANEL_HOTKEY, HotkeyCfg.CastPanelHotkey);
  AIni.WriteString(SEC_HOTKEY, KEY_BLUETOOTH_PANEL_HOTKEY, HotkeyCfg.BluetoothPanelHotkey);

  // [Position]
  AIni.WriteInteger(SEC_POSITION, KEY_MODE, Ord(PositionCfg.PositionMode));
  AIni.WriteInteger(SEC_POSITION, KEY_X, PositionCfg.PositionX);
  AIni.WriteInteger(SEC_POSITION, KEY_Y, PositionCfg.PositionY);
  AIni.WriteInteger(SEC_POSITION, KEY_W, PositionCfg.PositionW);
  AIni.WriteInteger(SEC_POSITION, KEY_H, PositionCfg.PositionH);

  // [Polling]
  AIni.WriteInteger(SEC_POLLING, KEY_MODE, Ord(PollingCfg.PollingMode));
  AIni.WriteInteger(SEC_POLLING, KEY_INTERVAL, PollingCfg.PollingInterval);
  AIni.WriteInteger(SEC_POLLING, KEY_EVENT_DEBOUNCE_MS, PollingCfg.EventDebounceMs);

  // [Log]
  AIni.WriteBool(SEC_LOG, KEY_ENABLED, LogCfg.LogEnabled);
  AIni.WriteString(SEC_LOG, KEY_FILENAME, LogCfg.LogFilename);
  AIni.WriteBool(SEC_LOG, KEY_APPEND, LogCfg.LogAppend);
  AIni.WriteInteger(SEC_LOG, KEY_LEVEL, Ord(LogCfg.LogLevel));

  // [Appearance]
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_ADDRESSES, AppearanceCfg.ShowAddresses);
  AIni.WriteString(SEC_APPEARANCE, KEY_THEME, AppearanceCfg.Theme);
  AIni.WriteString(SEC_APPEARANCE, KEY_VSF_DIR, AppearanceCfg.VsfDir);
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_LAST_SEEN, AppearanceCfg.ShowLastSeen);
  AIni.WriteInteger(SEC_APPEARANCE, KEY_LAST_SEEN_FORMAT, Ord(AppearanceCfg.LastSeenFormat));
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_DEVICE_ICONS, AppearanceCfg.ShowDeviceIcons);
  AIni.WriteInteger(SEC_APPEARANCE, KEY_CONNECTED_COLOR, AppearanceCfg.ConnectedColor);
  AIni.WriteBool(SEC_APPEARANCE, KEY_SHOW_BATTERY_LEVEL, AppearanceCfg.ShowBatteryLevel);

  // [Layout]
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_HEIGHT, LayoutCfg.ItemHeight);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_PADDING, LayoutCfg.ItemPadding);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_MARGIN, LayoutCfg.ItemMargin);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ICON_SIZE, LayoutCfg.IconSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_CORNER_RADIUS, LayoutCfg.CornerRadius);
  AIni.WriteInteger(SEC_LAYOUT, KEY_DEVICE_NAME_FONT_SIZE, LayoutCfg.DeviceNameFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_STATUS_FONT_SIZE, LayoutCfg.StatusFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ADDRESS_FONT_SIZE, LayoutCfg.AddressFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ICON_FONT_SIZE, LayoutCfg.IconFontSize);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_WIDTH, LayoutCfg.ItemBorderWidth);
  AIni.WriteInteger(SEC_LAYOUT, KEY_ITEM_BORDER_COLOR, LayoutCfg.ItemBorderColor);

  // [Device] - global defaults
  AIni.WriteInteger(SEC_DEVICE, KEY_CONNECTION_TIMEOUT, ConnectionCfg.ConnectionTimeout);
  AIni.WriteInteger(SEC_DEVICE, KEY_CONNECTION_RETRY_COUNT, ConnectionCfg.ConnectionRetryCount);
  AIni.WriteInteger(SEC_DEVICE, KEY_ENUMERATION_MODE, Ord(ConnectionCfg.EnumerationMode));
  AIni.WriteInteger(SEC_DEVICE, KEY_BLUETOOTH_PLATFORM, Ord(ConnectionCfg.BluetoothPlatform));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT, Ord(NotificationCfg.NotifyOnConnect));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_DISCONNECT, Ord(NotificationCfg.NotifyOnDisconnect));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_CONNECT_FAILED, Ord(NotificationCfg.NotifyOnConnectFailed));
  AIni.WriteInteger(SEC_DEVICE, KEY_NOTIFY_ON_AUTO_CONNECT, Ord(NotificationCfg.NotifyOnAutoConnect));

  // [BatteryTray] - battery tray icon defaults
  AIni.WriteBool(SEC_BATTERY_TRAY, KEY_SHOW_BATTERY_TRAY_ICONS, BatteryTrayCfg.ShowBatteryTrayIcons);
  AIni.WriteInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_ICON_COLOR, BatteryTrayCfg.DefaultIconColor);
  AIni.WriteInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_BACKGROUND_COLOR, BatteryTrayCfg.DefaultBackgroundColor);
  AIni.WriteBool(SEC_BATTERY_TRAY, KEY_DEFAULT_SHOW_NUMERIC_VALUE, BatteryTrayCfg.DefaultShowNumericValue);
  AIni.WriteInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_LOW_BATTERY_THRESHOLD, BatteryTrayCfg.DefaultLowBatteryThreshold);
  AIni.WriteBool(SEC_BATTERY_TRAY, KEY_DEFAULT_NOTIFY_LOW_BATTERY, BatteryTrayCfg.DefaultNotifyLowBattery);
  AIni.WriteBool(SEC_BATTERY_TRAY, KEY_DEFAULT_NOTIFY_FULLY_CHARGED, BatteryTrayCfg.DefaultNotifyFullyCharged);
  AIni.WriteInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_OUTLINE_COLOR_MODE, Ord(BatteryTrayCfg.DefaultOutlineColorMode));
  AIni.WriteInteger(SEC_BATTERY_TRAY, KEY_DEFAULT_CUSTOM_OUTLINE_COLOR, BatteryTrayCfg.DefaultCustomOutlineColor);

  // [Profile] - profile display settings
  AIni.WriteBool(SEC_PROFILE, KEY_SHOW_PROFILES, ProfileCfg.ShowProfiles);
  AIni.WriteInteger(SEC_PROFILE, KEY_PROFILE_FONT_SIZE, ProfileCfg.ProfileFontSize);
end;

end.
