{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Notification Configuration Interface            }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the notification configuration interface for global notification settings.
/// </summary>
unit App.NotificationConfigIntf;

interface

uses
  App.ConfigEnums;

type
  /// <summary>
  /// Global notification settings (defaults).
  /// Used by: TMainPresenter
  /// </summary>
  INotificationConfig = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000A}']
    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;

    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

    property NotifyOnConnect: TNotificationMode read GetNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read GetNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read GetNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read GetNotifyOnAutoConnect write SetNotifyOnAutoConnect;
  end;

implementation

end.
