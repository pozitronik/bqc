{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Notification Configuration Section              }
{                                                       }
{*******************************************************}

/// <summary>
/// Notification settings implementation.
/// </summary>
unit App.ConfigSection.Notification;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.NotificationConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Notification settings implementation.
  /// </summary>
  TNotificationConfigSection = class(TInterfacedObject, INotificationConfig)
  private
    FNotifyOnConnect: TNotificationMode;
    FNotifyOnDisconnect: TNotificationMode;
    FNotifyOnConnectFailed: TNotificationMode;
    FNotifyOnAutoConnect: TNotificationMode;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;

    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

    procedure SetDefaults;

    property NotifyOnConnect: TNotificationMode read FNotifyOnConnect write SetNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read FNotifyOnDisconnect write SetNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read FNotifyOnConnectFailed write SetNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read FNotifyOnAutoConnect write SetNotifyOnAutoConnect;
  end;

implementation

uses
  App.SettingsRepository;

{ TNotificationConfigSection }

constructor TNotificationConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TNotificationConfigSection.SetDefaults;
begin
  FNotifyOnConnect := DEF_NOTIFY_ON_CONNECT;
  FNotifyOnDisconnect := DEF_NOTIFY_ON_DISCONNECT;
  FNotifyOnConnectFailed := DEF_NOTIFY_ON_CONNECT_FAILED;
  FNotifyOnAutoConnect := DEF_NOTIFY_ON_AUTO_CONNECT;
end;

function TNotificationConfigSection.GetNotifyOnConnect: TNotificationMode;
begin
  Result := FNotifyOnConnect;
end;

function TNotificationConfigSection.GetNotifyOnDisconnect: TNotificationMode;
begin
  Result := FNotifyOnDisconnect;
end;

function TNotificationConfigSection.GetNotifyOnConnectFailed: TNotificationMode;
begin
  Result := FNotifyOnConnectFailed;
end;

function TNotificationConfigSection.GetNotifyOnAutoConnect: TNotificationMode;
begin
  Result := FNotifyOnAutoConnect;
end;

procedure TNotificationConfigSection.SetNotifyOnConnect(AValue: TNotificationMode);
begin
  if FNotifyOnConnect <> AValue then
  begin
    FNotifyOnConnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnDisconnect(AValue: TNotificationMode);
begin
  if FNotifyOnDisconnect <> AValue then
  begin
    FNotifyOnDisconnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnConnectFailed(AValue: TNotificationMode);
begin
  if FNotifyOnConnectFailed <> AValue then
  begin
    FNotifyOnConnectFailed := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TNotificationConfigSection.SetNotifyOnAutoConnect(AValue: TNotificationMode);
begin
  if FNotifyOnAutoConnect <> AValue then
  begin
    FNotifyOnAutoConnect := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
