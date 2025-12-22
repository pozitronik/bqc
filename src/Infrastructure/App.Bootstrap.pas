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
    FConfig: TObject;  // Actually TAppConfig, but avoids circular reference
    FStrategyFactory: IConnectionStrategyFactory;
    function GetConfig: TObject;
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
  FConfig.Free;
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

function TAppBootstrap.GetConfig: TObject;
begin
  if FConfig = nil then
  begin
    FConfig := TAppConfig.Create;
    TAppConfig(FConfig).Load;
  end;
  Result := FConfig;
end;

function TAppBootstrap.AppConfig: IAppConfig;
begin
  Result := TAppConfig(GetConfig);
end;

function TAppBootstrap.GeneralConfig: IGeneralConfig;
begin
  Result := TAppConfig(GetConfig).AsGeneralConfig;
end;

function TAppBootstrap.WindowConfig: IWindowConfig;
begin
  Result := TAppConfig(GetConfig).AsWindowConfig;
end;

function TAppBootstrap.PositionConfig: IPositionConfig;
begin
  Result := TAppConfig(GetConfig).AsPositionConfig;
end;

function TAppBootstrap.HotkeyConfig: IHotkeyConfig;
begin
  Result := TAppConfig(GetConfig).AsHotkeyConfig;
end;

function TAppBootstrap.PollingConfig: IPollingConfig;
begin
  Result := TAppConfig(GetConfig).AsPollingConfig;
end;

function TAppBootstrap.LogConfig: ILogConfig;
begin
  Result := TAppConfig(GetConfig).AsLogConfig;
end;

function TAppBootstrap.AppearanceConfig: IAppearanceConfig;
begin
  Result := TAppConfig(GetConfig).AsAppearanceConfig;
end;

function TAppBootstrap.LayoutConfig: ILayoutConfig;
begin
  Result := TAppConfig(GetConfig).AsLayoutConfig;
end;

function TAppBootstrap.ConnectionConfig: IConnectionConfig;
begin
  Result := TAppConfig(GetConfig).AsConnectionConfig;
end;

function TAppBootstrap.NotificationConfig: INotificationConfig;
begin
  Result := TAppConfig(GetConfig).AsNotificationConfig;
end;

function TAppBootstrap.DeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := TAppConfig(GetConfig).AsDeviceConfigProvider;
end;

function TAppBootstrap.ConnectionStrategyFactory: IConnectionStrategyFactory;
begin
  if FStrategyFactory = nil then
    FStrategyFactory := CreateConnectionStrategyFactory;
  Result := FStrategyFactory;
end;

end.
