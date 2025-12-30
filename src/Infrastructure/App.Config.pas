{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Configuration                       }
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
  App.ConnectionConfigIntf,
  App.LogConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.NotificationConfigIntf,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf,
  App.ConfigSectionTypes,
  App.ConfigSection.General,
  App.ConfigSection.Window,
  App.ConfigSection.Position,
  App.ConfigSection.Hotkey,
  App.ConfigSection.Polling,
  App.ConfigSection.Log,
  App.ConfigSection.Appearance,
  App.ConfigSection.Layout,
  App.ConfigSection.Connection,
  App.ConfigSection.Notification,
  App.ConfigSection.BatteryTray,
  App.ConfigSection.Profile;

type
  /// <summary>
  /// Application configuration manager.
  /// Handles loading/saving settings from INI file.
  /// Implements layer-specific configuration interfaces via composition.
  /// Uses reference counting for automatic memory management.
  /// Device configuration is delegated to composed IDeviceConfigProvider (SRP).
  /// </summary>
  TAppConfig = class(TObject,
    IInterface,
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
    FDestroying: Boolean;
    FDeviceRepository: IDeviceConfigRepository;
    FSettingsRepository: ISettingsRepository;
    FDeviceConfigProvider: IDeviceConfigProvider;

    // Composed configuration sections (stored as interfaces for reference counting)
    FGeneralSection: IGeneralConfig;
    FWindowSection: IWindowConfig;
    FPositionSection: IPositionConfig;
    FHotkeySection: IHotkeyConfig;
    FPollingSection: IPollingConfig;
    FLogSection: ILogConfig;
    FAppearanceSection: IAppearanceConfig;
    FLayoutSection: ILayoutConfig;
    FConnectionSection: IConnectionConfig;
    FNotificationSection: INotificationConfig;
    FBatteryTraySection: IBatteryTrayConfig;
    FProfileSection: IProfileConfig;

    procedure MarkModified;
    procedure CreateSections;

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
    /// Path to the configuration file.
    /// </summary>
    property ConfigPath: string read FConfigPath;

    /// <summary>
    /// True if configuration was modified since last save.
    /// </summary>
    property Modified: Boolean read FModified;

    // IAppConfig - aggregate interface access (returns section interfaces)
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
    function AsBatteryTrayConfig: IBatteryTrayConfig;
    function AsProfileConfig: IProfileConfig;
    function AsDeviceConfigProvider: IDeviceConfigProvider;
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
  App.SettingsRepository,
  App.DeviceConfigProvider;

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
  FBatteryTraySection := TBatteryTrayConfigSection.Create(Notifier);
  FProfileSection := TProfileConfigSection.Create(Notifier);
end;

destructor TAppConfig.Destroy;
begin
  // Prevent re-entry from _Release during SaveIfModified
  if FDestroying then
    Exit;
  FDestroying := True;

  try
    SaveIfModified;
  finally
    // All fields are interface-based, released automatically via reference counting
    FDeviceConfigProvider := nil;
    FSettingsRepository := nil;
    FDeviceRepository := nil;
    FGeneralSection := nil;
    FWindowSection := nil;
    FPositionSection := nil;
    FHotkeySection := nil;
    FPollingSection := nil;
    FLogSection := nil;
    FAppearanceSection := nil;
    FLayoutSection := nil;
    FConnectionSection := nil;
    FNotificationSection := nil;
    FBatteryTraySection := nil;
    FProfileSection := nil;
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

  // Create device config provider with dependencies (SRP)
  FDeviceConfigProvider := CreateDeviceConfigProvider(
    FDeviceRepository,
    FNotificationSection,
    FConnectionSection
  );
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
  if (Result = 0) and not FDestroying then
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

// IAppConfig - aggregate interface access (returns composed section interfaces)

function TAppConfig.AsGeneralConfig: IGeneralConfig;
begin
  Result := FGeneralSection;
end;

function TAppConfig.AsWindowConfig: IWindowConfig;
begin
  Result := FWindowSection;
end;

function TAppConfig.AsPositionConfig: IPositionConfig;
begin
  Result := FPositionSection;
end;

function TAppConfig.AsHotkeyConfig: IHotkeyConfig;
begin
  Result := FHotkeySection;
end;

function TAppConfig.AsPollingConfig: IPollingConfig;
begin
  Result := FPollingSection;
end;

function TAppConfig.AsLogConfig: ILogConfig;
begin
  Result := FLogSection;
end;

function TAppConfig.AsAppearanceConfig: IAppearanceConfig;
begin
  Result := FAppearanceSection;
end;

function TAppConfig.AsLayoutConfig: ILayoutConfig;
begin
  Result := FLayoutSection;
end;

function TAppConfig.AsConnectionConfig: IConnectionConfig;
begin
  Result := FConnectionSection;
end;

function TAppConfig.AsNotificationConfig: INotificationConfig;
begin
  Result := FNotificationSection;
end;

function TAppConfig.AsBatteryTrayConfig: IBatteryTrayConfig;
begin
  Result := FBatteryTraySection;
end;

function TAppConfig.AsProfileConfig: IProfileConfig;
begin
  Result := FProfileSection;
end;

function TAppConfig.AsDeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := FDeviceConfigProvider;
end;

end.
