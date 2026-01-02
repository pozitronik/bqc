{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Composition Root                    }
{                                                       }
{*******************************************************}

/// <summary>
/// Composition root for dependency injection.
/// Owns and manages the application configuration instance.
/// Provides factory methods for obtaining layer-specific configuration interfaces.
/// </summary>
unit App.Bootstrap;

interface

uses
  App.ConfigInterfaces,
  App.ConfigEnums,
  App.ConnectionConfigIntf,
  App.LogConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.NotificationConfigIntf,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf,
  App.Autostart,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  UI.Theme;

type
  /// <summary>
  /// Application bootstrapper - composition root for DI.
  /// Owns the configuration instance and provides interface-based access.
  /// </summary>
  TAppBootstrap = class
  private
    class var FInstance: TAppBootstrap;
    class function GetInstance: TAppBootstrap; static;
  private
    FConfig: IAppConfig;
    FDeviceRepository: IDeviceConfigRepository;
    FStrategyFactory: IConnectionStrategyFactory;
    FPairingService: IBluetoothPairingService;
    FRadioStateManager: IRadioStateManager;
    FAutostartManager: IAutostartManager;
    function GetConfig: IAppConfig;
    procedure ApplySideEffects;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor Destroy;

    /// <summary>
    /// Gets the singleton bootstrapper instance.
    /// </summary>
    class property Instance: TAppBootstrap read GetInstance;

    /// <summary>
    /// Gets the full application configuration interface.
    /// Use this only when you need access to all config or persistence methods.
    /// Prefer specific interfaces (GeneralConfig, PollingConfig, etc.) for most uses.
    /// </summary>
    function AppConfig: IAppConfig;

    // Layer-specific configuration interfaces
    // Use these to depend only on the config you actually need (ISP)

    function GeneralConfig: IGeneralConfig;
    function WindowConfig: IWindowConfig;
    function PositionConfig: IPositionConfig;
    function HotkeyConfig: IHotkeyConfig;
    function PollingConfig: IPollingConfig;
    function LogConfig: ILogConfig;
    function AppearanceConfig: IAppearanceConfig;
    function LayoutConfig: ILayoutConfig;
    function ConnectionConfig: IConnectionConfig;
    function NotificationConfig: INotificationConfig;
    function BatteryTrayConfig: IBatteryTrayConfig;
    function ProfileConfig: IProfileConfig;
    function DeviceConfigProvider: IDeviceConfigProvider;

    // Services
    function Logger: ILogger;
    function ThemeManager: IThemeManager;

    // Service factories
    function ConnectionStrategyFactory: IConnectionStrategyFactory;
    function PairingService: IBluetoothPairingService;

    // Radio state management
    function RadioStateManager: IRadioStateManager;

    // Autostart management
    function AutostartManager: IAutostartManager;
  end;

/// <summary>
/// Shortcut function to access the bootstrapper instance.
/// Usage: Bootstrap.PollingConfig instead of TAppBootstrap.Instance.PollingConfig
/// </summary>
function Bootstrap: TAppBootstrap;

/// <summary>
/// Explicitly shuts down and frees the Bootstrap singleton.
/// Call this before application exit to ensure clean memory leak detection.
/// Required for test projects that use FastMM5 leak checking.
/// </summary>
procedure ShutdownBootstrap;

implementation

uses
  System.SysUtils,
  App.Config,
  App.SettingsRepository,
  App.DeviceConfigRepository,
  App.Logger,
  Bluetooth.ConnectionStrategies,
  Bluetooth.PairingStrategies,
  Bluetooth.PairingService;

function Bootstrap: TAppBootstrap;
begin
  Result := TAppBootstrap.Instance;
end;

procedure ShutdownBootstrap;
begin
  if TAppBootstrap.FInstance <> nil then
  begin
    TAppBootstrap.FInstance.Free;
    TAppBootstrap.FInstance := nil;
  end;
end;

{ TAppBootstrap }

constructor TAppBootstrap.Create;
begin
  inherited Create;
  FConfig := nil;  // Lazy initialization
end;

destructor TAppBootstrap.Destroy;
begin
  FAutostartManager := nil;
  FRadioStateManager := nil;
  FStrategyFactory := nil;
  FDeviceRepository := nil;
  FConfig := nil;
  inherited Destroy;
end;

class destructor TAppBootstrap.Destroy;
begin
  FInstance.Free;
  FInstance := nil;
end;

class function TAppBootstrap.GetInstance: TAppBootstrap;
begin
  if FInstance = nil then
    FInstance := TAppBootstrap.Create;
  Result := FInstance;
end;

function TAppBootstrap.GetConfig: IAppConfig;
var
  Cfg: TAppConfig;
  SettingsRepo: ISettingsRepository;
  DevicePersistence: IDeviceConfigPersistence;
begin
  if FConfig = nil then
  begin
    Cfg := TAppConfig.Create;

    // Create device repository (implements both IDeviceConfigStorage and IDeviceConfigPersistence)
    FDeviceRepository := CreateDeviceConfigRepository;

    // Query persistence interface for settings repository (ISP-compliant)
    DevicePersistence := FDeviceRepository as IDeviceConfigPersistence;

    // Create settings repository with persistence interface (only needs Load/Save)
    SettingsRepo := TIniSettingsRepository.Create(Cfg.ConfigPath, DevicePersistence);

    // Wire up repositories
    Cfg.SetRepositories(SettingsRepo, FDeviceRepository);

    // Store as interface (must be done after setup to avoid premature release)
    FConfig := Cfg;

    // Load configuration
    FConfig.Load;

    // Apply side effects after loading
    ApplySideEffects;
  end;
  Result := FConfig;
end;

procedure TAppBootstrap.ApplySideEffects;
var
  LogCfg: ILogConfig;
  GeneralCfg: IGeneralConfig;
begin
  LogCfg := FConfig.AsLogConfig;
  GeneralCfg := FConfig.AsGeneralConfig;

  // Initialize logger with settings from config
  App.Logger.SetLoggingEnabled(LogCfg.LogEnabled, LogCfg.LogFilename, LogCfg.LogAppend,
    LogCfg.LogLevel);

  // Ensure autostart registry matches config setting
  AutostartManager.Apply(GeneralCfg.Autostart);
end;

function TAppBootstrap.AppConfig: IAppConfig;
begin
  Result := GetConfig;
end;

function TAppBootstrap.GeneralConfig: IGeneralConfig;
begin
  Result := GetConfig.AsGeneralConfig;
end;

function TAppBootstrap.WindowConfig: IWindowConfig;
begin
  Result := GetConfig.AsWindowConfig;
end;

function TAppBootstrap.PositionConfig: IPositionConfig;
begin
  Result := GetConfig.AsPositionConfig;
end;

function TAppBootstrap.HotkeyConfig: IHotkeyConfig;
begin
  Result := GetConfig.AsHotkeyConfig;
end;

function TAppBootstrap.PollingConfig: IPollingConfig;
begin
  Result := GetConfig.AsPollingConfig;
end;

function TAppBootstrap.LogConfig: ILogConfig;
begin
  Result := GetConfig.AsLogConfig;
end;

function TAppBootstrap.AppearanceConfig: IAppearanceConfig;
begin
  Result := GetConfig.AsAppearanceConfig;
end;

function TAppBootstrap.LayoutConfig: ILayoutConfig;
begin
  Result := GetConfig.AsLayoutConfig;
end;

function TAppBootstrap.ConnectionConfig: IConnectionConfig;
begin
  Result := GetConfig.AsConnectionConfig;
end;

function TAppBootstrap.NotificationConfig: INotificationConfig;
begin
  Result := GetConfig.AsNotificationConfig;
end;

function TAppBootstrap.BatteryTrayConfig: IBatteryTrayConfig;
begin
  Result := GetConfig.AsBatteryTrayConfig;
end;

function TAppBootstrap.ProfileConfig: IProfileConfig;
begin
  Result := GetConfig.AsProfileConfig;
end;

function TAppBootstrap.DeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := GetConfig.AsDeviceConfigProvider;
end;

function TAppBootstrap.Logger: ILogger;
begin
  // Ensure config is loaded (which configures logger)
  GetConfig;
  Result := App.Logger.Logger;
end;

function TAppBootstrap.ThemeManager: IThemeManager;
begin
  Result := UI.Theme.ThemeManager;
end;

function TAppBootstrap.ConnectionStrategyFactory: IConnectionStrategyFactory;
begin
  if FStrategyFactory = nil then
    FStrategyFactory := CreateConnectionStrategyFactory;
  Result := FStrategyFactory;
end;

function TAppBootstrap.PairingService: IBluetoothPairingService;
begin
  if FPairingService = nil then
  begin
    FPairingService := TBluetoothPairingService.Create(
      CreatePairingStrategyFactory,
      nil,  // Repository not used by pairing service
      ConnectionConfig
    );
  end;
  Result := FPairingService;
end;

function TAppBootstrap.RadioStateManager: IRadioStateManager;
var
  Platform: TBluetoothPlatform;
begin
  if FRadioStateManager = nil then
  begin
    // Select platform based on configuration
    Platform := ConnectionConfig.BluetoothPlatform;
    FRadioStateManager := CreateRadioStateManager(Platform);
  end;
  Result := FRadioStateManager;
end;

function TAppBootstrap.AutostartManager: IAutostartManager;
begin
  if FAutostartManager = nil then
    FAutostartManager := TAutostartManager.Create;
  Result := FAutostartManager;
end;

end.
