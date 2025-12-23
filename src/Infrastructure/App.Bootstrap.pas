{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Composition Root                    }
{                                                       }
{       Copyright (c) 2024                              }
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
  Bluetooth.Interfaces;

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
    function DeviceConfigProvider: IDeviceConfigProvider;

    // Services
    function Logger: ILogger;

    // Service factories
    function ConnectionStrategyFactory: IConnectionStrategyFactory;
  end;

/// <summary>
/// Shortcut function to access the bootstrapper instance.
/// Usage: Bootstrap.PollingConfig instead of TAppBootstrap.Instance.PollingConfig
/// </summary>
function Bootstrap: TAppBootstrap;

implementation

uses
  System.SysUtils,
  App.Config,
  App.SettingsRepository,
  App.DeviceConfigRepository,
  App.Logger,
  App.Autostart,
  Bluetooth.ConnectionStrategies;

function Bootstrap: TAppBootstrap;
begin
  Result := TAppBootstrap.Instance;
end;

{ TAppBootstrap }

constructor TAppBootstrap.Create;
begin
  inherited Create;
  FConfig := nil;  // Lazy initialization
end;

destructor TAppBootstrap.Destroy;
begin
  FConfig := nil;  // Release interface reference
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
begin
  if FConfig = nil then
  begin
    Cfg := TAppConfig.Create;

    // Create device repository
    FDeviceRepository := CreateDeviceConfigRepository;

    // Create settings repository with device repository reference
    SettingsRepo := TIniSettingsRepository.Create(Cfg.ConfigPath, FDeviceRepository);

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
  App.Logger.SetLoggingEnabled(LogCfg.LogEnabled, LogCfg.LogFilename, LogCfg.LogAppend);

  // Ensure autostart registry matches config setting
  TAutostartManager.Apply(GeneralCfg.Autostart);
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

function TAppBootstrap.ConnectionStrategyFactory: IConnectionStrategyFactory;
begin
  if FStrategyFactory = nil then
    FStrategyFactory := CreateConnectionStrategyFactory;
  Result := FStrategyFactory;
end;

end.
