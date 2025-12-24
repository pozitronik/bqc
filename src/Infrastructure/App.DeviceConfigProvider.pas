{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Configuration Provider                   }
{                                                       }
{       Implements IDeviceConfigProvider with effective }
{       value resolution (combines device overrides     }
{       with global defaults).                          }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.DeviceConfigProvider;

interface

uses
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes;

type
  /// <summary>
  /// Device configuration provider implementation.
  /// Combines device-specific settings with global defaults.
  /// Delegates storage to IDeviceConfigRepository.
  /// </summary>
  TDeviceConfigProvider = class(TInterfacedObject,
    IDeviceConfigQuery,
    IDeviceConfigMutation,
    IDeviceConfigProvider)
  private
    FDeviceRepository: IDeviceConfigRepository;
    FNotificationConfig: INotificationConfig;
    FConnectionConfig: IConnectionConfig;
  public
    /// <summary>
    /// Creates a device config provider with required dependencies.
    /// </summary>
    /// <param name="ADeviceRepository">Repository for device config storage.</param>
    /// <param name="ANotificationConfig">Global notification settings for defaults.</param>
    /// <param name="AConnectionConfig">Global connection settings for defaults.</param>
    constructor Create(
      ADeviceRepository: IDeviceConfigRepository;
      ANotificationConfig: INotificationConfig;
      AConnectionConfig: IConnectionConfig
    );

    // IDeviceConfigQuery
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
    function GetConfiguredDeviceAddresses: TArray<UInt64>;
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;

    // IDeviceConfigMutation / IDeviceConfigProvider
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);
    procedure RemoveDeviceConfig(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
  end;

/// <summary>
/// Creates a device config provider instance.
/// </summary>
function CreateDeviceConfigProvider(
  ADeviceRepository: IDeviceConfigRepository;
  ANotificationConfig: INotificationConfig;
  AConnectionConfig: IConnectionConfig
): IDeviceConfigProvider;

implementation

uses
  App.Logger;

function CreateDeviceConfigProvider(
  ADeviceRepository: IDeviceConfigRepository;
  ANotificationConfig: INotificationConfig;
  AConnectionConfig: IConnectionConfig
): IDeviceConfigProvider;
begin
  Result := TDeviceConfigProvider.Create(
    ADeviceRepository,
    ANotificationConfig,
    AConnectionConfig
  );
end;

{ TDeviceConfigProvider }

constructor TDeviceConfigProvider.Create(
  ADeviceRepository: IDeviceConfigRepository;
  ANotificationConfig: INotificationConfig;
  AConnectionConfig: IConnectionConfig
);
begin
  inherited Create;
  FDeviceRepository := ADeviceRepository;
  FNotificationConfig := ANotificationConfig;
  FConnectionConfig := AConnectionConfig;
  LogDebug('Created', ClassName);
end;

function TDeviceConfigProvider.GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
begin
  if Assigned(FDeviceRepository) then
    Result := FDeviceRepository.GetConfig(AAddress)
  else
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TDeviceConfigProvider.SetDeviceConfig(const AConfig: TDeviceConfig);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.SetConfig(AConfig);
end;

procedure TDeviceConfigProvider.RemoveDeviceConfig(AAddress: UInt64);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.Remove(AAddress);
end;

procedure TDeviceConfigProvider.RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
begin
  if Assigned(FDeviceRepository) then
    FDeviceRepository.RegisterDevice(AAddress, AName, ALastSeen);
end;

function TDeviceConfigProvider.GetConfiguredDeviceAddresses: TArray<UInt64>;
begin
  if Assigned(FDeviceRepository) then
    Result := FDeviceRepository.GetAllAddresses
  else
    Result := nil;
end;

function TDeviceConfigProvider.GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
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
    // Return global default from notification config
    case AEvent of
      neConnect:
        Result := FNotificationConfig.NotifyOnConnect;
      neDisconnect:
        Result := FNotificationConfig.NotifyOnDisconnect;
      neConnectFailed:
        Result := FNotificationConfig.NotifyOnConnectFailed;
      neAutoConnect:
        Result := FNotificationConfig.NotifyOnAutoConnect;
    else
      Result := nmNone;
    end;
  end;
end;

function TDeviceConfigProvider.GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetDeviceConfig(AAddress);
  if DeviceConfig.ConnectionTimeout >= 0 then
    Result := DeviceConfig.ConnectionTimeout
  else
    Result := FConnectionConfig.ConnectionTimeout;
end;

function TDeviceConfigProvider.GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetDeviceConfig(AAddress);
  if DeviceConfig.ConnectionRetryCount >= 0 then
    Result := DeviceConfig.ConnectionRetryCount
  else
    Result := FConnectionConfig.ConnectionRetryCount;
end;

end.
