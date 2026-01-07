{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Notification Coordinator                 }
{                                                       }
{       EXTRACTED FROM: App.MainPresenter (god class)   }
{       Handles device state change notifications.      }
{                                                       }
{*******************************************************}

unit App.DeviceNotificationCoordinator;

interface

uses
  Bluetooth.Types,
  App.MainViewInterfaces,
  App.ConfigInterfaces,
  App.ConfigEnums;

type
  /// <summary>
  /// Coordinator for device notification logic.
  /// Extracted from TMainPresenter to separate notification concerns.
  /// Handles showing balloon notifications based on device state and configuration.
  /// </summary>
  TDeviceNotificationCoordinator = class
  private
    FStatusView: IStatusView;
    FDeviceConfigProvider: IDeviceConfigProvider;

    /// <summary>
    /// Gets display name for device (alias from config or device name fallback).
    /// </summary>
    function GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;

  public
    constructor Create(AStatusView: IStatusView; ADeviceConfigProvider: IDeviceConfigProvider);

    /// <summary>
    /// Shows notification for device state change based on configuration.
    /// Checks notification mode from device config and displays balloon if enabled.
    /// </summary>
    procedure ShowNotification(const ADevice: TBluetoothDeviceInfo);
  end;

implementation

uses
  App.DeviceFormatter;

{ TDeviceNotificationCoordinator }

constructor TDeviceNotificationCoordinator.Create(AStatusView: IStatusView;
  ADeviceConfigProvider: IDeviceConfigProvider);
begin
  inherited Create;
  FStatusView := AStatusView;
  FDeviceConfigProvider := ADeviceConfigProvider;
end;

function TDeviceNotificationCoordinator.GetDeviceDisplayName(
  const ADevice: TBluetoothDeviceInfo): string;
begin
  Result := FDeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt).Alias;
  if Result = '' then
    Result := ADevice.Name;
end;

procedure TDeviceNotificationCoordinator.ShowNotification(
  const ADevice: TBluetoothDeviceInfo);
var
  NotifyMode: TNotificationMode;
  DeviceName: string;
begin
  DeviceName := GetDeviceDisplayName(ADevice);

  case ADevice.ConnectionState of
    csConnected:
      begin
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnect);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Connected', nfInfo);
      end;
    csDisconnected:
      begin
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neDisconnect);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Disconnected', nfInfo);
      end;
    csError:
      begin
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnectFailed);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Connection failed', nfError);
      end;
  end;
end;

end.
