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
    // All fields are interface-based, released automatically via reference counting
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

function TAppConfig.AsDeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := Self;
end;

end.
