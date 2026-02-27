{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       REST API Config Section Tests                   }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.RestApi;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.RestApiConfigIntf,
  App.ConfigSection.RestApi,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TRestApiConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TRestApiConfigSectionTests = class
  private
    FSection: TRestApiConfigSection;
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
    procedure Default_Enabled_IsFalse;
    [Test]
    procedure Default_Port_Is8765;
    [Test]
    procedure Default_BindAddress_IsLocalhost;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { Enabled Tests }
    [Test]
    procedure SetEnabled_True_UpdatesValue;
    [Test]
    procedure SetEnabled_SameValue_NoModification;
    [Test]
    procedure SetEnabled_DifferentValue_NotifiesModified;

    { Port Tests }
    [Test]
    procedure SetPort_UpdatesValue;
    [Test]
    procedure SetPort_SameValue_NoModification;
    [Test]
    procedure SetPort_DifferentValue_NotifiesModified;

    { BindAddress Tests }
    [Test]
    procedure SetBindAddress_UpdatesValue;
    [Test]
    procedure SetBindAddress_SameValue_NoModification;
    [Test]
    procedure SetBindAddress_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIRestApiConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TRestApiConfigSectionTests }

procedure TRestApiConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TRestApiConfigSection.Create(HandleModified);
end;

procedure TRestApiConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TRestApiConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TRestApiConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_API_ENABLED, FSection.Enabled);
  Assert.AreEqual(DEF_API_PORT, FSection.Port);
  Assert.AreEqual(DEF_API_BIND_ADDRESS, FSection.BindAddress);
end;

procedure TRestApiConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TRestApiConfigSection;
begin
  Section := TRestApiConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TRestApiConfigSectionTests.Default_Enabled_IsFalse;
begin
  Assert.IsFalse(FSection.Enabled);
end;

procedure TRestApiConfigSectionTests.Default_Port_Is8765;
begin
  Assert.AreEqual(8765, FSection.Port);
end;

procedure TRestApiConfigSectionTests.Default_BindAddress_IsLocalhost;
begin
  Assert.AreEqual('127.0.0.1', FSection.BindAddress);
end;

procedure TRestApiConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.Enabled := True;
  FSection.Port := 9999;
  FSection.BindAddress := '0.0.0.0';

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_API_ENABLED, FSection.Enabled);
  Assert.AreEqual(DEF_API_PORT, FSection.Port);
  Assert.AreEqual(DEF_API_BIND_ADDRESS, FSection.BindAddress);
end;

procedure TRestApiConfigSectionTests.SetEnabled_True_UpdatesValue;
begin
  FSection.Enabled := True;
  Assert.IsTrue(FSection.Enabled);
end;

procedure TRestApiConfigSectionTests.SetEnabled_SameValue_NoModification;
begin
  FSection.Enabled := DEF_API_ENABLED;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TRestApiConfigSectionTests.SetEnabled_DifferentValue_NotifiesModified;
begin
  FSection.Enabled := not DEF_API_ENABLED;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TRestApiConfigSectionTests.SetPort_UpdatesValue;
begin
  FSection.Port := 9999;
  Assert.AreEqual(9999, FSection.Port);
end;

procedure TRestApiConfigSectionTests.SetPort_SameValue_NoModification;
begin
  FSection.Port := DEF_API_PORT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TRestApiConfigSectionTests.SetPort_DifferentValue_NotifiesModified;
begin
  FSection.Port := 9999;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TRestApiConfigSectionTests.SetBindAddress_UpdatesValue;
begin
  FSection.BindAddress := '0.0.0.0';
  Assert.AreEqual('0.0.0.0', FSection.BindAddress);
end;

procedure TRestApiConfigSectionTests.SetBindAddress_SameValue_NoModification;
begin
  FSection.BindAddress := DEF_API_BIND_ADDRESS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TRestApiConfigSectionTests.SetBindAddress_DifferentValue_NotifiesModified;
begin
  FSection.BindAddress := '0.0.0.0';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TRestApiConfigSectionTests.ImplementsIRestApiConfig;
begin
  Assert.IsTrue(TRestApiConfigSection.GetInterfaceEntry(IRestApiConfig) <> nil,
    'TRestApiConfigSection should implement IRestApiConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TRestApiConfigSectionTests);

end.
