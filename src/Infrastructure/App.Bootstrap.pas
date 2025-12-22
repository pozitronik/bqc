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
/// Provides factory methods for obtaining layer-specific configuration interfaces.
/// During migration, this wraps the existing Config() singleton.
/// After migration completes, the singleton can be replaced with proper DI.
/// </summary>
unit App.Bootstrap;

interface

uses
  App.ConfigInterfaces;

type
  /// <summary>
  /// Application bootstrapper - composition root for DI.
  /// Provides interface-based access to configuration subsystems.
  /// </summary>
  TAppBootstrap = class
  private
    class var FInstance: TAppBootstrap;
    class function GetInstance: TAppBootstrap; static;
  public
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
  end;

/// <summary>
/// Shortcut function to access the bootstrapper instance.
/// Usage: Bootstrap.PollingConfig instead of TAppBootstrap.Instance.PollingConfig
/// </summary>
function Bootstrap: TAppBootstrap;

implementation

uses
  App.Config;

var
  GBootstrap: TAppBootstrap = nil;

function Bootstrap: TAppBootstrap;
begin
  Result := TAppBootstrap.Instance;
end;

{ TAppBootstrap }

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

function TAppBootstrap.AppConfig: IAppConfig;
begin
  // During migration: delegate to existing singleton
  // After migration: this will create/return a managed instance
  Result := Config;
end;

function TAppBootstrap.GeneralConfig: IGeneralConfig;
begin
  Result := Config.AsGeneralConfig;
end;

function TAppBootstrap.WindowConfig: IWindowConfig;
begin
  Result := Config.AsWindowConfig;
end;

function TAppBootstrap.PositionConfig: IPositionConfig;
begin
  Result := Config.AsPositionConfig;
end;

function TAppBootstrap.HotkeyConfig: IHotkeyConfig;
begin
  Result := Config.AsHotkeyConfig;
end;

function TAppBootstrap.PollingConfig: IPollingConfig;
begin
  Result := Config.AsPollingConfig;
end;

function TAppBootstrap.LogConfig: ILogConfig;
begin
  Result := Config.AsLogConfig;
end;

function TAppBootstrap.AppearanceConfig: IAppearanceConfig;
begin
  Result := Config.AsAppearanceConfig;
end;

function TAppBootstrap.LayoutConfig: ILayoutConfig;
begin
  Result := Config.AsLayoutConfig;
end;

function TAppBootstrap.ConnectionConfig: IConnectionConfig;
begin
  Result := Config.AsConnectionConfig;
end;

function TAppBootstrap.NotificationConfig: INotificationConfig;
begin
  Result := Config.AsNotificationConfig;
end;

function TAppBootstrap.DeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := Config.AsDeviceConfigProvider;
end;

end.
