{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Focused Configuration Section Implementations   }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Contains focused configuration section implementations.
/// Each class implements a single configuration interface (SRP).
/// Used by TAppConfig as composition to delegate interface implementations.
/// </summary>
unit App.ConfigSections;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.LayoutConfigIntf,
  App.AppearanceConfigIntf,
  App.ConnectionConfigIntf,
  App.NotificationConfigIntf,
  App.LogConfigIntf;

type
  /// <summary>
  /// Callback type for modification notification.
  /// Called by config sections when any property changes.
  /// </summary>
  TModifiedNotifier = reference to procedure;

  /// <summary>
  /// General application settings implementation.
  /// </summary>
  TGeneralConfigSection = class(TInterfacedObject, IGeneralConfig)
  private
    FWindowMode: TWindowMode;
    FOnTop: Boolean;
    FAutostart: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;

    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);

    procedure SetDefaults;

    property WindowMode: TWindowMode read FWindowMode write SetWindowMode;
    property OnTop: Boolean read FOnTop write SetOnTop;
    property Autostart: Boolean read FAutostart write SetAutostart;
  end;

  /// <summary>
  /// Window behavior settings implementation.
  /// </summary>
  TWindowConfigSection = class(TInterfacedObject, IWindowConfig)
  private
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;
    FMenuHideOnFocusLoss: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;

    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);

    procedure SetDefaults;

    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;
  end;

  /// <summary>
  /// Window position settings implementation.
  /// </summary>
  TPositionConfigSection = class(TInterfacedObject, IPositionConfig)
  private
    FPositionMode: TPositionMode;
    FPositionX: Integer;
    FPositionY: Integer;
    FPositionW: Integer;
    FPositionH: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

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

    procedure SetDefaults;

    property PositionMode: TPositionMode read FPositionMode write SetPositionMode;
    property PositionX: Integer read FPositionX write SetPositionX;
    property PositionY: Integer read FPositionY write SetPositionY;
    property PositionW: Integer read FPositionW write SetPositionW;
    property PositionH: Integer read FPositionH write SetPositionH;
  end;

  /// <summary>
  /// Hotkey settings implementation.
  /// </summary>
  THotkeyConfigSection = class(TInterfacedObject, IHotkeyConfig)
  private
    FHotkey: string;
    FUseLowLevelHook: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;

    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);

    procedure SetDefaults;

    property Hotkey: string read FHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read FUseLowLevelHook write SetUseLowLevelHook;
  end;

  /// <summary>
  /// Polling and event settings implementation.
  /// </summary>
  TPollingConfigSection = class(TInterfacedObject, IPollingConfig)
  private
    FPollingMode: TPollingMode;
    FPollingInterval: Integer;
    FEventDebounceMs: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);

    procedure SetDefaults;

    property PollingMode: TPollingMode read FPollingMode write SetPollingMode;
    property PollingInterval: Integer read FPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read FEventDebounceMs write SetEventDebounceMs;
  end;

  /// <summary>
  /// Logging settings implementation.
  /// </summary>
  TLogConfigSection = class(TInterfacedObject, ILogConfig)
  private
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;
    FLogLevel: TLogLevel;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetLogLevel: TLogLevel;

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetLogLevel(AValue: TLogLevel);

    procedure SetDefaults;

    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogFilename: string read FLogFilename write SetLogFilename;
    property LogAppend: Boolean read FLogAppend write SetLogAppend;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
  end;

  /// <summary>
  /// Appearance settings implementation.
  /// </summary>
  TAppearanceConfigSection = class(TInterfacedObject, IAppearanceConfig)
  private
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;
    FShowLastSeen: Boolean;
    FLastSeenFormat: TLastSeenFormat;
    FShowDeviceIcons: Boolean;
    FConnectedColor: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

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

    procedure SetDefaults;

    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    property VsfDir: string read FVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write SetConnectedColor;
  end;

  /// <summary>
  /// Layout settings implementation.
  /// </summary>
  TLayoutConfigSection = class(TInterfacedObject, ILayoutConfig)
  private
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
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

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

    procedure SetDefaults;

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
  end;

  /// <summary>
  /// Connection settings implementation.
  /// </summary>
  TConnectionConfigSection = class(TInterfacedObject, IConnectionConfig)
  private
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);

    procedure SetDefaults;

    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
  end;

  /// <summary>
  /// Notification settings implementation.
  /// </summary>
  TNotificationConfigSection = class(TInterfacedObject, INotificationConfig)
  private
    FNotifyOnConnect: TNotificationMode;
    FNotifyOnDisconnect: TNotificationMode;
    FNotifyOnConnectFailed: TNotificationMode;
    FNotifyOnAutoConnect: TNotificationMode;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;

    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

    procedure SetDefaults;

    property NotifyOnConnect: TNotificationMode read FNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read FNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read FNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read FNotifyOnAutoConnect write SetNotifyOnAutoConnect;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;  // For layout/connection default constants

{ TGeneralConfigSection }

constructor TGeneralConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TGeneralConfigSection.SetDefaults;
begin
  FWindowMode := DEF_WINDOW_MODE;
  FOnTop := DEF_ON_TOP;
  FAutostart := DEF_AUTOSTART;
end;

function TGeneralConfigSection.GetWindowMode: TWindowMode;
begin
  Result := FWindowMode;
end;

function TGeneralConfigSection.GetOnTop: Boolean;
begin
  Result := FOnTop;
end;

function TGeneralConfigSection.GetAutostart: Boolean;
begin
  Result := FAutostart;
end;

procedure TGeneralConfigSection.SetWindowMode(AValue: TWindowMode);
begin
  if FWindowMode <> AValue then
  begin
    FWindowMode := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TGeneralConfigSection.SetOnTop(AValue: Boolean);
begin
  if FOnTop <> AValue then
  begin
    FOnTop := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TGeneralConfigSection.SetAutostart(AValue: Boolean);
begin
  if FAutostart <> AValue then
  begin
    FAutostart := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TWindowConfigSection }

constructor TWindowConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TWindowConfigSection.SetDefaults;
begin
  FMinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := DEF_CLOSE_TO_TRAY;
  FMenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;
end;

function TWindowConfigSection.GetMinimizeToTray: Boolean;
begin
  Result := FMinimizeToTray;
end;

function TWindowConfigSection.GetCloseToTray: Boolean;
begin
  Result := FCloseToTray;
end;

function TWindowConfigSection.GetMenuHideOnFocusLoss: Boolean;
begin
  Result := FMenuHideOnFocusLoss;
end;

procedure TWindowConfigSection.SetMinimizeToTray(AValue: Boolean);
begin
  if FMinimizeToTray <> AValue then
  begin
    FMinimizeToTray := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TWindowConfigSection.SetCloseToTray(AValue: Boolean);
begin
  if FCloseToTray <> AValue then
  begin
    FCloseToTray := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TWindowConfigSection.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  if FMenuHideOnFocusLoss <> AValue then
  begin
    FMenuHideOnFocusLoss := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TPositionConfigSection }

constructor TPositionConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TPositionConfigSection.SetDefaults;
begin
  FPositionMode := DEF_POSITION_MODE;
  FPositionX := DEF_POSITION_X;
  FPositionY := DEF_POSITION_Y;
  FPositionW := DEF_POSITION_W;
  FPositionH := DEF_POSITION_H;
end;

function TPositionConfigSection.GetPositionMode: TPositionMode;
begin
  Result := FPositionMode;
end;

function TPositionConfigSection.GetPositionX: Integer;
begin
  Result := FPositionX;
end;

function TPositionConfigSection.GetPositionY: Integer;
begin
  Result := FPositionY;
end;

function TPositionConfigSection.GetPositionW: Integer;
begin
  Result := FPositionW;
end;

function TPositionConfigSection.GetPositionH: Integer;
begin
  Result := FPositionH;
end;

procedure TPositionConfigSection.SetPositionMode(AValue: TPositionMode);
begin
  if FPositionMode <> AValue then
  begin
    FPositionMode := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPositionConfigSection.SetPositionX(AValue: Integer);
begin
  if FPositionX <> AValue then
  begin
    FPositionX := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPositionConfigSection.SetPositionY(AValue: Integer);
begin
  if FPositionY <> AValue then
  begin
    FPositionY := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPositionConfigSection.SetPositionW(AValue: Integer);
begin
  if FPositionW <> AValue then
  begin
    FPositionW := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPositionConfigSection.SetPositionH(AValue: Integer);
begin
  if FPositionH <> AValue then
  begin
    FPositionH := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ THotkeyConfigSection }

constructor THotkeyConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure THotkeyConfigSection.SetDefaults;
begin
  FHotkey := DEF_HOTKEY;
  FUseLowLevelHook := DEF_USE_LOW_LEVEL_HOOK;
end;

function THotkeyConfigSection.GetHotkey: string;
begin
  Result := FHotkey;
end;

function THotkeyConfigSection.GetUseLowLevelHook: Boolean;
begin
  Result := FUseLowLevelHook;
end;

procedure THotkeyConfigSection.SetHotkey(const AValue: string);
begin
  if FHotkey <> AValue then
  begin
    FHotkey := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure THotkeyConfigSection.SetUseLowLevelHook(AValue: Boolean);
begin
  if FUseLowLevelHook <> AValue then
  begin
    FUseLowLevelHook := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TPollingConfigSection }

constructor TPollingConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TPollingConfigSection.SetDefaults;
begin
  FPollingMode := DEF_POLLING_MODE;
  FPollingInterval := DEF_POLLING_INTERVAL;
  FEventDebounceMs := DEF_EVENT_DEBOUNCE_MS;
end;

function TPollingConfigSection.GetPollingMode: TPollingMode;
begin
  Result := FPollingMode;
end;

function TPollingConfigSection.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

function TPollingConfigSection.GetEventDebounceMs: Integer;
begin
  Result := FEventDebounceMs;
end;

procedure TPollingConfigSection.SetPollingMode(AValue: TPollingMode);
begin
  if FPollingMode <> AValue then
  begin
    FPollingMode := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPollingConfigSection.SetPollingInterval(AValue: Integer);
begin
  if FPollingInterval <> AValue then
  begin
    FPollingInterval := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TPollingConfigSection.SetEventDebounceMs(AValue: Integer);
begin
  if FEventDebounceMs <> AValue then
  begin
    FEventDebounceMs := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TLogConfigSection }

constructor TLogConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TLogConfigSection.SetDefaults;
begin
  FLogEnabled := DEF_LOG_ENABLED;
  FLogFilename := DEF_LOG_FILENAME;
  FLogAppend := DEF_LOG_APPEND;
  FLogLevel := DEF_LOG_LEVEL;
end;

function TLogConfigSection.GetLogEnabled: Boolean;
begin
  Result := FLogEnabled;
end;

function TLogConfigSection.GetLogFilename: string;
begin
  Result := FLogFilename;
end;

function TLogConfigSection.GetLogAppend: Boolean;
begin
  Result := FLogAppend;
end;

function TLogConfigSection.GetLogLevel: TLogLevel;
begin
  Result := FLogLevel;
end;

procedure TLogConfigSection.SetLogEnabled(AValue: Boolean);
begin
  if FLogEnabled <> AValue then
  begin
    FLogEnabled := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogFilename(const AValue: string);
begin
  if FLogFilename <> AValue then
  begin
    FLogFilename := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogAppend(AValue: Boolean);
begin
  if FLogAppend <> AValue then
  begin
    FLogAppend := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogLevel(AValue: TLogLevel);
begin
  if FLogLevel <> AValue then
  begin
    FLogLevel := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TAppearanceConfigSection }

constructor TAppearanceConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TAppearanceConfigSection.SetDefaults;
begin
  FShowAddresses := DEF_SHOW_ADDRESSES;
  FTheme := DEF_THEME;
  FVsfDir := DEF_VSF_DIR;
  FShowLastSeen := DEF_SHOW_LAST_SEEN;
  FLastSeenFormat := DEF_LAST_SEEN_FORMAT;
  FShowDeviceIcons := DEF_SHOW_DEVICE_ICONS;
  FConnectedColor := DEF_CONNECTED_COLOR;
end;

function TAppearanceConfigSection.GetShowAddresses: Boolean;
begin
  Result := FShowAddresses;
end;

function TAppearanceConfigSection.GetTheme: string;
begin
  Result := FTheme;
end;

function TAppearanceConfigSection.GetVsfDir: string;
begin
  Result := FVsfDir;
end;

function TAppearanceConfigSection.GetShowLastSeen: Boolean;
begin
  Result := FShowLastSeen;
end;

function TAppearanceConfigSection.GetLastSeenFormat: TLastSeenFormat;
begin
  Result := FLastSeenFormat;
end;

function TAppearanceConfigSection.GetShowDeviceIcons: Boolean;
begin
  Result := FShowDeviceIcons;
end;

function TAppearanceConfigSection.GetConnectedColor: Integer;
begin
  Result := FConnectedColor;
end;

procedure TAppearanceConfigSection.SetShowAddresses(AValue: Boolean);
begin
  if FShowAddresses <> AValue then
  begin
    FShowAddresses := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetTheme(const AValue: string);
begin
  if FTheme <> AValue then
  begin
    FTheme := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetVsfDir(const AValue: string);
begin
  if FVsfDir <> AValue then
  begin
    FVsfDir := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetShowLastSeen(AValue: Boolean);
begin
  if FShowLastSeen <> AValue then
  begin
    FShowLastSeen := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  if FLastSeenFormat <> AValue then
  begin
    FLastSeenFormat := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetShowDeviceIcons(AValue: Boolean);
begin
  if FShowDeviceIcons <> AValue then
  begin
    FShowDeviceIcons := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetConnectedColor(AValue: Integer);
begin
  if FConnectedColor <> AValue then
  begin
    FConnectedColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TLayoutConfigSection }

constructor TLayoutConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TLayoutConfigSection.SetDefaults;
begin
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
end;

function TLayoutConfigSection.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TLayoutConfigSection.GetItemPadding: Integer;
begin
  Result := FItemPadding;
end;

function TLayoutConfigSection.GetItemMargin: Integer;
begin
  Result := FItemMargin;
end;

function TLayoutConfigSection.GetIconSize: Integer;
begin
  Result := FIconSize;
end;

function TLayoutConfigSection.GetCornerRadius: Integer;
begin
  Result := FCornerRadius;
end;

function TLayoutConfigSection.GetDeviceNameFontSize: Integer;
begin
  Result := FDeviceNameFontSize;
end;

function TLayoutConfigSection.GetStatusFontSize: Integer;
begin
  Result := FStatusFontSize;
end;

function TLayoutConfigSection.GetAddressFontSize: Integer;
begin
  Result := FAddressFontSize;
end;

function TLayoutConfigSection.GetIconFontSize: Integer;
begin
  Result := FIconFontSize;
end;

function TLayoutConfigSection.GetItemBorderWidth: Integer;
begin
  Result := FItemBorderWidth;
end;

function TLayoutConfigSection.GetItemBorderColor: Integer;
begin
  Result := FItemBorderColor;
end;

procedure TLayoutConfigSection.SetItemHeight(AValue: Integer);
begin
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemPadding(AValue: Integer);
begin
  if FItemPadding <> AValue then
  begin
    FItemPadding := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemMargin(AValue: Integer);
begin
  if FItemMargin <> AValue then
  begin
    FItemMargin := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetIconSize(AValue: Integer);
begin
  if FIconSize <> AValue then
  begin
    FIconSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetCornerRadius(AValue: Integer);
begin
  if FCornerRadius <> AValue then
  begin
    FCornerRadius := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetDeviceNameFontSize(AValue: Integer);
begin
  if FDeviceNameFontSize <> AValue then
  begin
    FDeviceNameFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetStatusFontSize(AValue: Integer);
begin
  if FStatusFontSize <> AValue then
  begin
    FStatusFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetAddressFontSize(AValue: Integer);
begin
  if FAddressFontSize <> AValue then
  begin
    FAddressFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetIconFontSize(AValue: Integer);
begin
  if FIconFontSize <> AValue then
  begin
    FIconFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemBorderWidth(AValue: Integer);
begin
  if FItemBorderWidth <> AValue then
  begin
    FItemBorderWidth := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemBorderColor(AValue: Integer);
begin
  if FItemBorderColor <> AValue then
  begin
    FItemBorderColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TConnectionConfigSection }

constructor TConnectionConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TConnectionConfigSection.SetDefaults;
begin
  FConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  FConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
end;

function TConnectionConfigSection.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TConnectionConfigSection.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

procedure TConnectionConfigSection.SetConnectionTimeout(AValue: Integer);
begin
  if FConnectionTimeout <> AValue then
  begin
    FConnectionTimeout := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TConnectionConfigSection.SetConnectionRetryCount(AValue: Integer);
begin
  if FConnectionRetryCount <> AValue then
  begin
    FConnectionRetryCount := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

{ TNotificationConfigSection }

constructor TNotificationConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TNotificationConfigSection.SetDefaults;
begin
  FNotifyOnConnect := DEF_NOTIFY_ON_CONNECT;
  FNotifyOnDisconnect := DEF_NOTIFY_ON_DISCONNECT;
  FNotifyOnConnectFailed := DEF_NOTIFY_ON_CONNECT_FAILED;
  FNotifyOnAutoConnect := DEF_NOTIFY_ON_AUTO_CONNECT;
end;

function TNotificationConfigSection.GetNotifyOnConnect: TNotificationMode;
begin
  Result := FNotifyOnConnect;
end;

function TNotificationConfigSection.GetNotifyOnDisconnect: TNotificationMode;
begin
  Result := FNotifyOnDisconnect;
end;

function TNotificationConfigSection.GetNotifyOnConnectFailed: TNotificationMode;
begin
  Result := FNotifyOnConnectFailed;
end;

function TNotificationConfigSection.GetNotifyOnAutoConnect: TNotificationMode;
begin
  Result := FNotifyOnAutoConnect;
end;

procedure TNotificationConfigSection.SetNotifyOnConnect(AValue: TNotificationMode);
begin
  if FNotifyOnConnect <> AValue then
  begin
    FNotifyOnConnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnDisconnect(AValue: TNotificationMode);
begin
  if FNotifyOnDisconnect <> AValue then
  begin
    FNotifyOnDisconnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnConnectFailed(AValue: TNotificationMode);
begin
  if FNotifyOnConnectFailed <> AValue then
  begin
    FNotifyOnConnectFailed := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnAutoConnect(AValue: TNotificationMode);
begin
  if FNotifyOnAutoConnect <> AValue then
  begin
    FNotifyOnAutoConnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
