{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Notification Config Section Tests               }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Notification;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.NotificationConfigIntf,
  App.ConfigSection.Notification,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TNotificationConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TNotificationConfigSectionTests = class
  private
    FSection: TNotificationConfigSection;
    FModifiedCalled: Boolean;

    procedure HandleModified;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure Create_InitializesWithDefaults;
    [Test]
    procedure Create_WithNilNotifier_CreatesInstance;

    { Default Values Tests }
    [Test]
    procedure Default_NotifyOnConnect_IsNone;
    [Test]
    procedure Default_NotifyOnDisconnect_IsNone;
    [Test]
    procedure Default_NotifyOnConnectFailed_IsNone;
    [Test]
    procedure Default_NotifyOnAutoConnect_IsNone;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { NotifyOnConnect Tests }
    [Test]
    procedure SetNotifyOnConnect_UpdatesValue;
    [Test]
    procedure SetNotifyOnConnect_SameValue_NoModification;
    [Test]
    procedure SetNotifyOnConnect_DifferentValue_NotifiesModified;

    { NotifyOnDisconnect Tests }
    [Test]
    procedure SetNotifyOnDisconnect_UpdatesValue;
    [Test]
    procedure SetNotifyOnDisconnect_SameValue_NoModification;
    [Test]
    procedure SetNotifyOnDisconnect_DifferentValue_NotifiesModified;

    { NotifyOnConnectFailed Tests }
    [Test]
    procedure SetNotifyOnConnectFailed_UpdatesValue;
    [Test]
    procedure SetNotifyOnConnectFailed_SameValue_NoModification;
    [Test]
    procedure SetNotifyOnConnectFailed_DifferentValue_NotifiesModified;

    { NotifyOnAutoConnect Tests }
    [Test]
    procedure SetNotifyOnAutoConnect_UpdatesValue;
    [Test]
    procedure SetNotifyOnAutoConnect_SameValue_NoModification;
    [Test]
    procedure SetNotifyOnAutoConnect_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsINotificationConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TNotificationConfigSectionTests }

procedure TNotificationConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TNotificationConfigSection.Create(HandleModified);
end;

procedure TNotificationConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TNotificationConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TNotificationConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_CONNECT), Integer(FSection.NotifyOnConnect));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_DISCONNECT), Integer(FSection.NotifyOnDisconnect));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_CONNECT_FAILED), Integer(FSection.NotifyOnConnectFailed));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_AUTO_CONNECT), Integer(FSection.NotifyOnAutoConnect));
end;

procedure TNotificationConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TNotificationConfigSection;
begin
  Section := TNotificationConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TNotificationConfigSectionTests.Default_NotifyOnConnect_IsNone;
begin
  Assert.AreEqual(Integer(nmNone), Integer(FSection.NotifyOnConnect));
end;

procedure TNotificationConfigSectionTests.Default_NotifyOnDisconnect_IsNone;
begin
  Assert.AreEqual(Integer(nmNone), Integer(FSection.NotifyOnDisconnect));
end;

procedure TNotificationConfigSectionTests.Default_NotifyOnConnectFailed_IsNone;
begin
  Assert.AreEqual(Integer(nmNone), Integer(FSection.NotifyOnConnectFailed));
end;

procedure TNotificationConfigSectionTests.Default_NotifyOnAutoConnect_IsNone;
begin
  Assert.AreEqual(Integer(nmNone), Integer(FSection.NotifyOnAutoConnect));
end;

procedure TNotificationConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.NotifyOnConnect := nmBalloon;
  FSection.NotifyOnDisconnect := nmBalloon;
  FSection.NotifyOnConnectFailed := nmBalloon;
  FSection.NotifyOnAutoConnect := nmBalloon;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_CONNECT), Integer(FSection.NotifyOnConnect));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_DISCONNECT), Integer(FSection.NotifyOnDisconnect));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_CONNECT_FAILED), Integer(FSection.NotifyOnConnectFailed));
  Assert.AreEqual(Integer(DEF_NOTIFY_ON_AUTO_CONNECT), Integer(FSection.NotifyOnAutoConnect));
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnect_UpdatesValue;
begin
  FSection.NotifyOnConnect := nmBalloon;
  Assert.AreEqual(Integer(nmBalloon), Integer(FSection.NotifyOnConnect));
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnect_SameValue_NoModification;
begin
  FSection.NotifyOnConnect := DEF_NOTIFY_ON_CONNECT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnect_DifferentValue_NotifiesModified;
begin
  FSection.NotifyOnConnect := nmBalloon;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnDisconnect_UpdatesValue;
begin
  FSection.NotifyOnDisconnect := nmBalloon;
  Assert.AreEqual(Integer(nmBalloon), Integer(FSection.NotifyOnDisconnect));
end;

procedure TNotificationConfigSectionTests.SetNotifyOnDisconnect_SameValue_NoModification;
begin
  FSection.NotifyOnDisconnect := DEF_NOTIFY_ON_DISCONNECT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnDisconnect_DifferentValue_NotifiesModified;
begin
  FSection.NotifyOnDisconnect := nmBalloon;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnectFailed_UpdatesValue;
begin
  FSection.NotifyOnConnectFailed := nmBalloon;
  Assert.AreEqual(Integer(nmBalloon), Integer(FSection.NotifyOnConnectFailed));
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnectFailed_SameValue_NoModification;
begin
  FSection.NotifyOnConnectFailed := DEF_NOTIFY_ON_CONNECT_FAILED;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnConnectFailed_DifferentValue_NotifiesModified;
begin
  FSection.NotifyOnConnectFailed := nmBalloon;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnAutoConnect_UpdatesValue;
begin
  FSection.NotifyOnAutoConnect := nmBalloon;
  Assert.AreEqual(Integer(nmBalloon), Integer(FSection.NotifyOnAutoConnect));
end;

procedure TNotificationConfigSectionTests.SetNotifyOnAutoConnect_SameValue_NoModification;
begin
  FSection.NotifyOnAutoConnect := DEF_NOTIFY_ON_AUTO_CONNECT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TNotificationConfigSectionTests.SetNotifyOnAutoConnect_DifferentValue_NotifiesModified;
begin
  FSection.NotifyOnAutoConnect := nmBalloon;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TNotificationConfigSectionTests.ImplementsINotificationConfig;
begin
  Assert.IsTrue(TNotificationConfigSection.GetInterfaceEntry(INotificationConfig) <> nil,
    'TNotificationConfigSection should implement INotificationConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TNotificationConfigSectionTests);

end.
