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
  App.ConfigEnums,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Application configuration manager.
  /// Handles loading/saving settings from INI file.
  /// Implements layer-specific configuration interfaces for dependency injection.
  /// Uses reference counting for automatic memory management.
  /// </summary>
  TAppConfig = class(TObject,
    IInterface,
    IGeneralConfig,
    IWindowConfig,
    IPositionConfig,
    IHotkeyConfig,
    IPollingConfig,
    ILogConfig,
    IAppearanceConfig,
    ILayoutConfig,
    IConnectionConfig,
    INotificationConfig,
    IDeviceConfigProvider,
    IAppConfig)
  protected
    FRefCount: Integer;
    // IInterface implementation with reference counting
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    FConfigPath: string;
    FModified: Boolean;
    FDeviceRepository: IDeviceConfigRepository;
    FSettingsRepository: ISettingsRepository;

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

    // Interface getter methods
    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;
    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;
    function GetPositionMode: TPositionMode;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetPositionW: Integer;
    function GetPositionH: Integer;
    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;
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
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;

    // IAppConfig getters
    function GetConfigPath: string;
    function GetModified: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Sets the repositories for persistence and device configuration.
    /// Must be called before Load/Save operations.
    /// </summary>
    procedure SetRepositories(ASettingsRepository: ISettingsRepository;
      ADeviceRepository: IDeviceConfigRepository);

    /// <summary>
    /// Loads configuration from INI file. Creates file with defaults if not exists.
    /// Delegates to ISettingsRepository.
    /// </summary>
    procedure Load;

    /// <summary>
    /// Saves configuration to INI file.
    /// Delegates to ISettingsRepository.
    /// </summary>
    procedure Save;

    /// <summary>
    /// Saves configuration only if modified.
    /// </summary>
    procedure SaveIfModified;

    /// <summary>
    /// Clears the modified flag. Called by repository after save.
    /// </summary>
    procedure ClearModified;

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

    // IAppConfig - aggregate interface access
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

implementation

uses
  System.Math,
  System.DateUtils,
  App.SettingsRepository;

{ TAppConfig }

constructor TAppConfig.Create;
var
  ExePath: string;
begin
  inherited Create;

  // Config file next to executable
  ExePath := ExtractFilePath(ParamStr(0));
  FConfigPath := ExePath + 'bqc.ini';

  SetDefaults;
end;

destructor TAppConfig.Destroy;
begin
  try
    SaveIfModified;
  finally
    // Repositories are interface-based, released automatically
    FSettingsRepository := nil;
    FDeviceRepository := nil;
  end;
  inherited Destroy;
end;

procedure TAppConfig.SetRepositories(ASettingsRepository: ISettingsRepository;
  ADeviceRepository: IDeviceConfigRepository);
begin
  FSettingsRepository := ASettingsRepository;
  FDeviceRepository := ADeviceRepository;
end;

procedure TAppConfig.ClearModified;
begin
  FModified := False;
  if Assigned(FDeviceRepository) then
    FDeviceRepository.ClearModified;
end;

procedure TAppConfig.SetDefaults;
begin
  // [General]
  FWindowMode := App.SettingsRepository.DEF_WINDOW_MODE;
  FOnTop := App.SettingsRepository.DEF_ON_TOP;
  FAutostart := App.SettingsRepository.DEF_AUTOSTART;

  // [Window]
  FMinimizeToTray := App.SettingsRepository.DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := App.SettingsRepository.DEF_CLOSE_TO_TRAY;

  // [Menu]
  FMenuHideOnFocusLoss := App.SettingsRepository.DEF_MENU_HIDE_ON_FOCUS_LOSS;

  // [Hotkey]
  FHotkey := App.SettingsRepository.DEF_HOTKEY;
  FUseLowLevelHook := App.SettingsRepository.DEF_USE_LOW_LEVEL_HOOK;

  // [Position]
  FPositionMode := App.SettingsRepository.DEF_POSITION_MODE;
  FPositionX := App.SettingsRepository.DEF_POSITION_X;
  FPositionY := App.SettingsRepository.DEF_POSITION_Y;
  FPositionW := App.SettingsRepository.DEF_POSITION_W;
  FPositionH := App.SettingsRepository.DEF_POSITION_H;

  // [Polling]
  FPollingMode := App.SettingsRepository.DEF_POLLING_MODE;
  FPollingInterval := DEF_POLLING_INTERVAL;
  FEventDebounceMs := DEF_EVENT_DEBOUNCE_MS;

  // [Log]
  FLogEnabled := App.SettingsRepository.DEF_LOG_ENABLED;
  FLogFilename := App.SettingsRepository.DEF_LOG_FILENAME;
  FLogAppend := App.SettingsRepository.DEF_LOG_APPEND;

  // [Appearance]
  FShowAddresses := App.SettingsRepository.DEF_SHOW_ADDRESSES;
  FTheme := App.SettingsRepository.DEF_THEME;
  FVsfDir := App.SettingsRepository.DEF_VSF_DIR;
  FShowLastSeen := App.SettingsRepository.DEF_SHOW_LAST_SEEN;
  FLastSeenFormat := App.SettingsRepository.DEF_LAST_SEEN_FORMAT;
  FShowDeviceIcons := App.SettingsRepository.DEF_SHOW_DEVICE_ICONS;
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
  FNotifyOnConnect := App.SettingsRepository.DEF_NOTIFY_ON_CONNECT;
  FNotifyOnDisconnect := App.SettingsRepository.DEF_NOTIFY_ON_DISCONNECT;
  FNotifyOnConnectFailed := App.SettingsRepository.DEF_NOTIFY_ON_CONNECT_FAILED;
  FNotifyOnAutoConnect := App.SettingsRepository.DEF_NOTIFY_ON_AUTO_CONNECT;

  FModified := False;
end;

procedure TAppConfig.Load;
begin
  // Delegate to settings repository
  if Assigned(FSettingsRepository) then
    FSettingsRepository.LoadSettings(Self)
  else
    raise EInvalidOperation.Create('Settings repository not set. Call SetRepositories first.');
end;

procedure TAppConfig.Save;
begin
  // Delegate to settings repository
  if Assigned(FSettingsRepository) then
    FSettingsRepository.SaveSettings(Self)
  else
    raise EInvalidOperation.Create('Settings repository not set. Call SetRepositories first.');
end;

procedure TAppConfig.SaveIfModified;
var
  DeviceModified: Boolean;
begin
  DeviceModified := Assigned(FDeviceRepository) and FDeviceRepository.IsModified;
  if FModified or DeviceModified then
    Save;
end;

function TAppConfig.GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
begin
  if Assigned(FDeviceRepository) then
    Result := FDeviceRepository.GetConfig(AAddress)
  else
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TAppConfig.SetDeviceConfig(const AConfig: TDeviceConfig);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.SetConfig(AConfig);
end;

procedure TAppConfig.RemoveDeviceConfig(AAddress: UInt64);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.Remove(AAddress);
end;

procedure TAppConfig.RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.RegisterDevice(AAddress, AName, ALastSeen);
end;

function TAppConfig.GetConfiguredDeviceAddresses: TArray<UInt64>;
begin
  if Assigned(FDeviceRepository) then
    Result := FDeviceRepository.GetAllAddresses
  else
    Result := nil;
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
    // Note: Side effect (registry update) is handled by Bootstrap.ApplySideEffects
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
    // Note: Side effect (logger update) is handled by Bootstrap.ApplySideEffects
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

// IInterface implementation - manual for singleton (no reference counting)

function TAppConfig.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TAppConfig._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TAppConfig._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

// Interface getter implementations

function TAppConfig.GetWindowMode: TWindowMode;
begin
  Result := FWindowMode;
end;

function TAppConfig.GetOnTop: Boolean;
begin
  Result := FOnTop;
end;

function TAppConfig.GetAutostart: Boolean;
begin
  Result := FAutostart;
end;

function TAppConfig.GetMinimizeToTray: Boolean;
begin
  Result := FMinimizeToTray;
end;

function TAppConfig.GetCloseToTray: Boolean;
begin
  Result := FCloseToTray;
end;

function TAppConfig.GetMenuHideOnFocusLoss: Boolean;
begin
  Result := FMenuHideOnFocusLoss;
end;

function TAppConfig.GetPositionMode: TPositionMode;
begin
  Result := FPositionMode;
end;

function TAppConfig.GetPositionX: Integer;
begin
  Result := FPositionX;
end;

function TAppConfig.GetPositionY: Integer;
begin
  Result := FPositionY;
end;

function TAppConfig.GetPositionW: Integer;
begin
  Result := FPositionW;
end;

function TAppConfig.GetPositionH: Integer;
begin
  Result := FPositionH;
end;

function TAppConfig.GetHotkey: string;
begin
  Result := FHotkey;
end;

function TAppConfig.GetUseLowLevelHook: Boolean;
begin
  Result := FUseLowLevelHook;
end;

function TAppConfig.GetPollingMode: TPollingMode;
begin
  Result := FPollingMode;
end;

function TAppConfig.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

function TAppConfig.GetEventDebounceMs: Integer;
begin
  Result := FEventDebounceMs;
end;

function TAppConfig.GetLogEnabled: Boolean;
begin
  Result := FLogEnabled;
end;

function TAppConfig.GetLogFilename: string;
begin
  Result := FLogFilename;
end;

function TAppConfig.GetLogAppend: Boolean;
begin
  Result := FLogAppend;
end;

function TAppConfig.GetShowAddresses: Boolean;
begin
  Result := FShowAddresses;
end;

function TAppConfig.GetTheme: string;
begin
  Result := FTheme;
end;

function TAppConfig.GetVsfDir: string;
begin
  Result := FVsfDir;
end;

function TAppConfig.GetShowLastSeen: Boolean;
begin
  Result := FShowLastSeen;
end;

function TAppConfig.GetLastSeenFormat: TLastSeenFormat;
begin
  Result := FLastSeenFormat;
end;

function TAppConfig.GetShowDeviceIcons: Boolean;
begin
  Result := FShowDeviceIcons;
end;

function TAppConfig.GetConnectedColor: Integer;
begin
  Result := FConnectedColor;
end;

function TAppConfig.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TAppConfig.GetItemPadding: Integer;
begin
  Result := FItemPadding;
end;

function TAppConfig.GetItemMargin: Integer;
begin
  Result := FItemMargin;
end;

function TAppConfig.GetIconSize: Integer;
begin
  Result := FIconSize;
end;

function TAppConfig.GetCornerRadius: Integer;
begin
  Result := FCornerRadius;
end;

function TAppConfig.GetDeviceNameFontSize: Integer;
begin
  Result := FDeviceNameFontSize;
end;

function TAppConfig.GetStatusFontSize: Integer;
begin
  Result := FStatusFontSize;
end;

function TAppConfig.GetAddressFontSize: Integer;
begin
  Result := FAddressFontSize;
end;

function TAppConfig.GetIconFontSize: Integer;
begin
  Result := FIconFontSize;
end;

function TAppConfig.GetItemBorderWidth: Integer;
begin
  Result := FItemBorderWidth;
end;

function TAppConfig.GetItemBorderColor: Integer;
begin
  Result := FItemBorderColor;
end;

function TAppConfig.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TAppConfig.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

function TAppConfig.GetNotifyOnConnect: TNotificationMode;
begin
  Result := FNotifyOnConnect;
end;

function TAppConfig.GetNotifyOnDisconnect: TNotificationMode;
begin
  Result := FNotifyOnDisconnect;
end;

function TAppConfig.GetNotifyOnConnectFailed: TNotificationMode;
begin
  Result := FNotifyOnConnectFailed;
end;

function TAppConfig.GetNotifyOnAutoConnect: TNotificationMode;
begin
  Result := FNotifyOnAutoConnect;
end;

function TAppConfig.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

function TAppConfig.GetModified: Boolean;
begin
  Result := FModified;
end;

// IAppConfig - aggregate interface access

function TAppConfig.AsGeneralConfig: IGeneralConfig;
begin
  Result := Self;
end;

function TAppConfig.AsWindowConfig: IWindowConfig;
begin
  Result := Self;
end;

function TAppConfig.AsPositionConfig: IPositionConfig;
begin
  Result := Self;
end;

function TAppConfig.AsHotkeyConfig: IHotkeyConfig;
begin
  Result := Self;
end;

function TAppConfig.AsPollingConfig: IPollingConfig;
begin
  Result := Self;
end;

function TAppConfig.AsLogConfig: ILogConfig;
begin
  Result := Self;
end;

function TAppConfig.AsAppearanceConfig: IAppearanceConfig;
begin
  Result := Self;
end;

function TAppConfig.AsLayoutConfig: ILayoutConfig;
begin
  Result := Self;
end;

function TAppConfig.AsConnectionConfig: IConnectionConfig;
begin
  Result := Self;
end;

function TAppConfig.AsNotificationConfig: INotificationConfig;
begin
  Result := Self;
end;

function TAppConfig.AsDeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := Self;
end;

end.
