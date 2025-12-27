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
  /// Per-device battery tray icon settings.
  /// Value of -1 means use global default.
  /// </summary>
  TDeviceBatteryConfig = record
    ShowTrayIcon: Integer;         // -1=Global, 0=No, 1=Yes
    IconColor: Integer;            // -1=Global, or TColor value
    BackgroundColor: Integer;      // -1=Global, or TColor value (clNone for transparent)
    ShowNumericValue: Integer;     // -1=Global, 0=No, 1=Yes
    LowBatteryThreshold: Integer;  // -1=Global, or 0-100
    NotifyLowBattery: Integer;     // -1=Global, 0=No, 1=Yes
    NotifyFullyCharged: Integer;   // -1=Global, 0=No, 1=Yes

    class function Default: TDeviceBatteryConfig; static;
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
    BatteryTray: TDeviceBatteryConfig;
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

{ TDeviceBatteryConfig }

class function TDeviceBatteryConfig.Default: TDeviceBatteryConfig;
begin
  Result.ShowTrayIcon := -1;
  Result.IconColor := -1;
  Result.BackgroundColor := -1;
  Result.ShowNumericValue := -1;
  Result.LowBatteryThreshold := -1;
  Result.NotifyLowBattery := -1;
  Result.NotifyFullyCharged := -1;
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
  Result.BatteryTray := TDeviceBatteryConfig.Default;
  Result.DeviceTypeOverride := -1;
  Result.LastSeen := 0;
end;

end.
