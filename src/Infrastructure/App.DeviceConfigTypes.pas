{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Per-Device Configuration Types                  }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines record types for per-device configuration.
/// Used by IDeviceConfigProvider and IDeviceConfigRepository.
/// </summary>
unit App.DeviceConfigTypes;

interface

uses
  App.ConfigEnums;

type
  /// <summary>
  /// Per-device notification settings.
  /// Value of -1 means use global default.
  /// </summary>
  TDeviceNotificationConfig = record
    OnConnect: Integer;        // -1=Global, 0=None, 1=Balloon
    OnDisconnect: Integer;     // -1=Global, 0=None, 1=Balloon
    OnConnectFailed: Integer;  // -1=Global, 0=None, 1=Balloon
    OnAutoConnect: Integer;    // -1=Global, 0=None, 1=Balloon

    class function Default: TDeviceNotificationConfig; static;
  end;

  /// <summary>
  /// Per-device configuration.
  /// </summary>
  TDeviceConfig = record
    Address: UInt64;
    Name: string;
    Alias: string;
    Pinned: Boolean;
    Hidden: Boolean;
    AutoConnect: Boolean;
    ConnectionTimeout: Integer;
    ConnectionRetryCount: Integer;
    Notifications: TDeviceNotificationConfig;
    DeviceTypeOverride: Integer;
    LastSeen: TDateTime;

    class function Default(AAddress: UInt64): TDeviceConfig; static;
  end;

implementation

{ TDeviceNotificationConfig }

class function TDeviceNotificationConfig.Default: TDeviceNotificationConfig;
begin
  Result.OnConnect := -1;
  Result.OnDisconnect := -1;
  Result.OnConnectFailed := -1;
  Result.OnAutoConnect := -1;
end;

{ TDeviceConfig }

class function TDeviceConfig.Default(AAddress: UInt64): TDeviceConfig;
begin
  Result.Address := AAddress;
  Result.Name := '';
  Result.Alias := '';
  Result.Pinned := False;
  Result.Hidden := False;
  Result.AutoConnect := False;
  Result.ConnectionTimeout := -1;
  Result.ConnectionRetryCount := -1;
  Result.Notifications := TDeviceNotificationConfig.Default;
  Result.DeviceTypeOverride := -1;
  Result.LastSeen := 0;
end;

end.
