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
  App.ConfigInterfaces,
  App.ConfigSections;

type
  /// <summary>
  /// Application configuration manager.
  /// Handles loading/saving settings from INI file.
  /// Implements layer-specific configuration interfaces via composition.
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

    // Composed configuration sections
    FGeneralSection: TGeneralConfigSection;
    FWindowSection: TWindowConfigSection;
    FPositionSection: TPositionConfigSection;
    FHotkeySection: THotkeyConfigSection;
    FPollingSection: TPollingConfigSection;
    FLogSection: TLogConfigSection;
    FAppearanceSection: TAppearanceConfigSection;
    FLayoutSection: TLayoutConfigSection;
    FConnectionSection: TConnectionConfigSection;
    FNotificationSection: TNotificationConfigSection;

    procedure MarkModified;
    procedure CreateSections;

    // Delegate getters to sections for interface implementation
    // IGeneralConfig
    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;
    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);

    // IWindowConfig
    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;
    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);

    // IPositionConfig
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

    // IHotkeyConfig
    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;
    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);

    // IPollingConfig
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;
    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);

    // ILogConfig
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);

    // IAppearanceConfig
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

    // ILayoutConfig
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

    // IConnectionConfig
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);

    // INotificationConfig
    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;
    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

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

    // [General] - delegated to FGeneralSection
    property WindowMode: TWindowMode read GetWindowMode write SetWindowMode;
    property OnTop: Boolean read GetOnTop write SetOnTop;
    property Autostart: Boolean read GetAutostart write SetAutostart;

    // [Window] - delegated to FWindowSection
    property MinimizeToTray: Boolean read GetMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read GetCloseToTray write SetCloseToTray;

    // [Menu] - delegated to FWindowSection
    property MenuHideOnFocusLoss: Boolean read GetMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;

    // [Hotkey] - delegated to FHotkeySection
    property Hotkey: string read GetHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read GetUseLowLevelHook write SetUseLowLevelHook;

    // [Position] - delegated to FPositionSection
    property PositionMode: TPositionMode read GetPositionMode write SetPositionMode;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
    property PositionW: Integer read GetPositionW write SetPositionW;
    property PositionH: Integer read GetPositionH write SetPositionH;

    // [Polling] - delegated to FPollingSection
    property PollingMode: TPollingMode read GetPollingMode write SetPollingMode;
    property PollingInterval: Integer read GetPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read GetEventDebounceMs write SetEventDebounceMs;

    // [Log] - delegated to FLogSection
    property LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;
    property LogFilename: string read GetLogFilename write SetLogFilename;
    property LogAppend: Boolean read GetLogAppend write SetLogAppend;

    // [Appearance] - delegated to FAppearanceSection
    property ShowAddresses: Boolean read GetShowAddresses write SetShowAddresses;
    property Theme: string read GetTheme write SetTheme;
    property VsfDir: string read GetVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read GetShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read GetLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read GetShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read GetConnectedColor write SetConnectedColor;

    // [Layout] - delegated to FLayoutSection
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

    // [Device] - delegated to FConnectionSection and FNotificationSection
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read GetConnectionRetryCount write SetConnectionRetryCount;
    property NotifyOnConnect: TNotificationMode read GetNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read GetNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read GetNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read GetNotifyOnAutoConnect write SetNotifyOnAutoConnect;

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
    property StayOnTop: Boolean read GetOnTop write SetOnTop;
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

  // Create composed sections with shared modification callback
  CreateSections;
end;

procedure TAppConfig.CreateSections;
var
  Notifier: TModifiedNotifier;
begin
  Notifier := MarkModified;

  FGeneralSection := TGeneralConfigSection.Create(Notifier);
  FWindowSection := TWindowConfigSection.Create(Notifier);
  FPositionSection := TPositionConfigSection.Create(Notifier);
  FHotkeySection := THotkeyConfigSection.Create(Notifier);
  FPollingSection := TPollingConfigSection.Create(Notifier);
  FLogSection := TLogConfigSection.Create(Notifier);
  FAppearanceSection := TAppearanceConfigSection.Create(Notifier);
  FLayoutSection := TLayoutConfigSection.Create(Notifier);
  FConnectionSection := TConnectionConfigSection.Create(Notifier);
  FNotificationSection := TNotificationConfigSection.Create(Notifier);
end;

destructor TAppConfig.Destroy;
begin
  try
    SaveIfModified;
  finally
    // Repositories are interface-based, released automatically
    FSettingsRepository := nil;
    FDeviceRepository := nil;

    // Free composed sections
    FNotificationSection.Free;
    FConnectionSection.Free;
    FLayoutSection.Free;
    FAppearanceSection.Free;
    FLogSection.Free;
    FPollingSection.Free;
    FHotkeySection.Free;
    FPositionSection.Free;
    FWindowSection.Free;
    FGeneralSection.Free;
  end;
  inherited Destroy;
end;

procedure TAppConfig.MarkModified;
begin
  FModified := True;
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

// IGeneralConfig - delegate to FGeneralSection

function TAppConfig.GetWindowMode: TWindowMode;
begin
  Result := FGeneralSection.WindowMode;
end;

procedure TAppConfig.SetWindowMode(AValue: TWindowMode);
begin
  FGeneralSection.WindowMode := AValue;
end;

function TAppConfig.GetOnTop: Boolean;
begin
  Result := FGeneralSection.OnTop;
end;

procedure TAppConfig.SetOnTop(AValue: Boolean);
begin
  FGeneralSection.OnTop := AValue;
end;

function TAppConfig.GetAutostart: Boolean;
begin
  Result := FGeneralSection.Autostart;
end;

procedure TAppConfig.SetAutostart(AValue: Boolean);
begin
  FGeneralSection.Autostart := AValue;
end;

// IWindowConfig - delegate to FWindowSection

function TAppConfig.GetMinimizeToTray: Boolean;
begin
  Result := FWindowSection.MinimizeToTray;
end;

procedure TAppConfig.SetMinimizeToTray(AValue: Boolean);
begin
  FWindowSection.MinimizeToTray := AValue;
end;

function TAppConfig.GetCloseToTray: Boolean;
begin
  Result := FWindowSection.CloseToTray;
end;

procedure TAppConfig.SetCloseToTray(AValue: Boolean);
begin
  FWindowSection.CloseToTray := AValue;
end;

function TAppConfig.GetMenuHideOnFocusLoss: Boolean;
begin
  Result := FWindowSection.MenuHideOnFocusLoss;
end;

procedure TAppConfig.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  FWindowSection.MenuHideOnFocusLoss := AValue;
end;

// IPositionConfig - delegate to FPositionSection

function TAppConfig.GetPositionMode: TPositionMode;
begin
  Result := FPositionSection.PositionMode;
end;

procedure TAppConfig.SetPositionMode(AValue: TPositionMode);
begin
  FPositionSection.PositionMode := AValue;
end;

function TAppConfig.GetPositionX: Integer;
begin
  Result := FPositionSection.PositionX;
end;

procedure TAppConfig.SetPositionX(AValue: Integer);
begin
  FPositionSection.PositionX := AValue;
end;

function TAppConfig.GetPositionY: Integer;
begin
  Result := FPositionSection.PositionY;
end;

procedure TAppConfig.SetPositionY(AValue: Integer);
begin
  FPositionSection.PositionY := AValue;
end;

function TAppConfig.GetPositionW: Integer;
begin
  Result := FPositionSection.PositionW;
end;

procedure TAppConfig.SetPositionW(AValue: Integer);
begin
  FPositionSection.PositionW := AValue;
end;

function TAppConfig.GetPositionH: Integer;
begin
  Result := FPositionSection.PositionH;
end;

procedure TAppConfig.SetPositionH(AValue: Integer);
begin
  FPositionSection.PositionH := AValue;
end;

// IHotkeyConfig - delegate to FHotkeySection

function TAppConfig.GetHotkey: string;
begin
  Result := FHotkeySection.Hotkey;
end;

procedure TAppConfig.SetHotkey(const AValue: string);
begin
  FHotkeySection.Hotkey := AValue;
end;

function TAppConfig.GetUseLowLevelHook: Boolean;
begin
  Result := FHotkeySection.UseLowLevelHook;
end;

procedure TAppConfig.SetUseLowLevelHook(AValue: Boolean);
begin
  FHotkeySection.UseLowLevelHook := AValue;
end;

// IPollingConfig - delegate to FPollingSection

function TAppConfig.GetPollingMode: TPollingMode;
begin
  Result := FPollingSection.PollingMode;
end;

procedure TAppConfig.SetPollingMode(AValue: TPollingMode);
begin
  FPollingSection.PollingMode := AValue;
end;

function TAppConfig.GetPollingInterval: Integer;
begin
  Result := FPollingSection.PollingInterval;
end;

procedure TAppConfig.SetPollingInterval(AValue: Integer);
begin
  FPollingSection.PollingInterval := AValue;
end;

function TAppConfig.GetEventDebounceMs: Integer;
begin
  Result := FPollingSection.EventDebounceMs;
end;

procedure TAppConfig.SetEventDebounceMs(AValue: Integer);
begin
  FPollingSection.EventDebounceMs := AValue;
end;

// ILogConfig - delegate to FLogSection

function TAppConfig.GetLogEnabled: Boolean;
begin
  Result := FLogSection.LogEnabled;
end;

procedure TAppConfig.SetLogEnabled(AValue: Boolean);
begin
  FLogSection.LogEnabled := AValue;
end;

function TAppConfig.GetLogFilename: string;
begin
  Result := FLogSection.LogFilename;
end;

procedure TAppConfig.SetLogFilename(const AValue: string);
begin
  FLogSection.LogFilename := AValue;
end;

function TAppConfig.GetLogAppend: Boolean;
begin
  Result := FLogSection.LogAppend;
end;

procedure TAppConfig.SetLogAppend(AValue: Boolean);
begin
  FLogSection.LogAppend := AValue;
end;

// IAppearanceConfig - delegate to FAppearanceSection

function TAppConfig.GetShowAddresses: Boolean;
begin
  Result := FAppearanceSection.ShowAddresses;
end;

procedure TAppConfig.SetShowAddresses(AValue: Boolean);
begin
  FAppearanceSection.ShowAddresses := AValue;
end;

function TAppConfig.GetTheme: string;
begin
  Result := FAppearanceSection.Theme;
end;

procedure TAppConfig.SetTheme(const AValue: string);
begin
  FAppearanceSection.Theme := AValue;
end;

function TAppConfig.GetVsfDir: string;
begin
  Result := FAppearanceSection.VsfDir;
end;

procedure TAppConfig.SetVsfDir(const AValue: string);
begin
  FAppearanceSection.VsfDir := AValue;
end;

function TAppConfig.GetShowLastSeen: Boolean;
begin
  Result := FAppearanceSection.ShowLastSeen;
end;

procedure TAppConfig.SetShowLastSeen(AValue: Boolean);
begin
  FAppearanceSection.ShowLastSeen := AValue;
end;

function TAppConfig.GetLastSeenFormat: TLastSeenFormat;
begin
  Result := FAppearanceSection.LastSeenFormat;
end;

procedure TAppConfig.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  FAppearanceSection.LastSeenFormat := AValue;
end;

function TAppConfig.GetShowDeviceIcons: Boolean;
begin
  Result := FAppearanceSection.ShowDeviceIcons;
end;

procedure TAppConfig.SetShowDeviceIcons(AValue: Boolean);
begin
  FAppearanceSection.ShowDeviceIcons := AValue;
end;

function TAppConfig.GetConnectedColor: Integer;
begin
  Result := FAppearanceSection.ConnectedColor;
end;

procedure TAppConfig.SetConnectedColor(AValue: Integer);
begin
  FAppearanceSection.ConnectedColor := AValue;
end;

// ILayoutConfig - delegate to FLayoutSection

function TAppConfig.GetItemHeight: Integer;
begin
  Result := FLayoutSection.ItemHeight;
end;

procedure TAppConfig.SetItemHeight(AValue: Integer);
begin
  FLayoutSection.ItemHeight := AValue;
end;

function TAppConfig.GetItemPadding: Integer;
begin
  Result := FLayoutSection.ItemPadding;
end;

procedure TAppConfig.SetItemPadding(AValue: Integer);
begin
  FLayoutSection.ItemPadding := AValue;
end;

function TAppConfig.GetItemMargin: Integer;
begin
  Result := FLayoutSection.ItemMargin;
end;

procedure TAppConfig.SetItemMargin(AValue: Integer);
begin
  FLayoutSection.ItemMargin := AValue;
end;

function TAppConfig.GetIconSize: Integer;
begin
  Result := FLayoutSection.IconSize;
end;

procedure TAppConfig.SetIconSize(AValue: Integer);
begin
  FLayoutSection.IconSize := AValue;
end;

function TAppConfig.GetCornerRadius: Integer;
begin
  Result := FLayoutSection.CornerRadius;
end;

procedure TAppConfig.SetCornerRadius(AValue: Integer);
begin
  FLayoutSection.CornerRadius := AValue;
end;

function TAppConfig.GetDeviceNameFontSize: Integer;
begin
  Result := FLayoutSection.DeviceNameFontSize;
end;

procedure TAppConfig.SetDeviceNameFontSize(AValue: Integer);
begin
  FLayoutSection.DeviceNameFontSize := AValue;
end;

function TAppConfig.GetStatusFontSize: Integer;
begin
  Result := FLayoutSection.StatusFontSize;
end;

procedure TAppConfig.SetStatusFontSize(AValue: Integer);
begin
  FLayoutSection.StatusFontSize := AValue;
end;

function TAppConfig.GetAddressFontSize: Integer;
begin
  Result := FLayoutSection.AddressFontSize;
end;

procedure TAppConfig.SetAddressFontSize(AValue: Integer);
begin
  FLayoutSection.AddressFontSize := AValue;
end;

function TAppConfig.GetIconFontSize: Integer;
begin
  Result := FLayoutSection.IconFontSize;
end;

procedure TAppConfig.SetIconFontSize(AValue: Integer);
begin
  FLayoutSection.IconFontSize := AValue;
end;

function TAppConfig.GetItemBorderWidth: Integer;
begin
  Result := FLayoutSection.ItemBorderWidth;
end;

procedure TAppConfig.SetItemBorderWidth(AValue: Integer);
begin
  FLayoutSection.ItemBorderWidth := AValue;
end;

function TAppConfig.GetItemBorderColor: Integer;
begin
  Result := FLayoutSection.ItemBorderColor;
end;

procedure TAppConfig.SetItemBorderColor(AValue: Integer);
begin
  FLayoutSection.ItemBorderColor := AValue;
end;

// IConnectionConfig - delegate to FConnectionSection

function TAppConfig.GetConnectionTimeout: Integer;
begin
  Result := FConnectionSection.ConnectionTimeout;
end;

procedure TAppConfig.SetConnectionTimeout(AValue: Integer);
begin
  FConnectionSection.ConnectionTimeout := AValue;
end;

function TAppConfig.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionSection.ConnectionRetryCount;
end;

procedure TAppConfig.SetConnectionRetryCount(AValue: Integer);
begin
  FConnectionSection.ConnectionRetryCount := AValue;
end;

// INotificationConfig - delegate to FNotificationSection

function TAppConfig.GetNotifyOnConnect: TNotificationMode;
begin
  Result := FNotificationSection.NotifyOnConnect;
end;

procedure TAppConfig.SetNotifyOnConnect(AValue: TNotificationMode);
begin
  FNotificationSection.NotifyOnConnect := AValue;
end;

function TAppConfig.GetNotifyOnDisconnect: TNotificationMode;
begin
  Result := FNotificationSection.NotifyOnDisconnect;
end;

procedure TAppConfig.SetNotifyOnDisconnect(AValue: TNotificationMode);
begin
  FNotificationSection.NotifyOnDisconnect := AValue;
end;

function TAppConfig.GetNotifyOnConnectFailed: TNotificationMode;
begin
  Result := FNotificationSection.NotifyOnConnectFailed;
end;

procedure TAppConfig.SetNotifyOnConnectFailed(AValue: TNotificationMode);
begin
  FNotificationSection.NotifyOnConnectFailed := AValue;
end;

function TAppConfig.GetNotifyOnAutoConnect: TNotificationMode;
begin
  Result := FNotificationSection.NotifyOnAutoConnect;
end;

procedure TAppConfig.SetNotifyOnAutoConnect(AValue: TNotificationMode);
begin
  FNotificationSection.NotifyOnAutoConnect := AValue;
end;

// Effective value resolution (resolves per-device overrides)

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
    // Return global default from section
    case AEvent of
      neConnect:
        Result := FNotificationSection.NotifyOnConnect;
      neDisconnect:
        Result := FNotificationSection.NotifyOnDisconnect;
      neConnectFailed:
        Result := FNotificationSection.NotifyOnConnectFailed;
      neAutoConnect:
        Result := FNotificationSection.NotifyOnAutoConnect;
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
    Result := FConnectionSection.ConnectionTimeout;
end;

function TAppConfig.GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetDeviceConfig(AAddress);
  if DeviceConfig.ConnectionRetryCount >= 0 then
    Result := DeviceConfig.ConnectionRetryCount
  else
    Result := FConnectionSection.ConnectionRetryCount;
end;

// IInterface implementation with reference counting

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

// IAppConfig getters

function TAppConfig.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

function TAppConfig.GetModified: Boolean;
begin
  Result := FModified;
end;

// IAppConfig - aggregate interface access (returns Self since TAppConfig implements all interfaces)

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
