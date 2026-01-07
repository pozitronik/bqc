{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Config Section Tests                 }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Connection;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.ConnectionConfigIntf,
  App.ConfigSection.Connection,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TConnectionConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TConnectionConfigSectionTests = class
  private
    FSection: TConnectionConfigSection;
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
    procedure Default_ConnectionTimeout_Is10000;
    [Test]
    procedure Default_ConnectionRetryCount_Is2;
    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { ConnectionTimeout Tests }
    [Test]
    procedure SetConnectionTimeout_UpdatesValue;
    [Test]
    procedure SetConnectionTimeout_SameValue_NoModification;
    [Test]
    procedure SetConnectionTimeout_DifferentValue_NotifiesModified;

    { ConnectionRetryCount Tests }
    [Test]
    procedure SetConnectionRetryCount_UpdatesValue;
    [Test]
    procedure SetConnectionRetryCount_SameValue_NoModification;
    [Test]
    procedure SetConnectionRetryCount_DifferentValue_NotifiesModified;

    { PairingTimeout Tests }
    [Test]
    procedure Default_PairingTimeout_Is30000;
    [Test]
    procedure SetPairingTimeout_UpdatesValue;
    [Test]
    procedure SetPairingTimeout_SameValue_NoModification;
    [Test]
    procedure SetPairingTimeout_DifferentValue_NotifiesModified;

    { PairingMode Tests }
    [Test]
    procedure Default_PairingMode_IsAutomatic;
    [Test]
    procedure SetPairingMode_UpdatesValue;
    [Test]
    procedure SetPairingMode_SameValue_NoModification;
    [Test]
    procedure SetPairingMode_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIConnectionConfig;
  end;

implementation

uses
  App.Config;

{ TConnectionConfigSectionTests }

procedure TConnectionConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TConnectionConfigSection.Create(HandleModified);
end;

procedure TConnectionConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TConnectionConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TConnectionConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_CONNECTION_TIMEOUT, FSection.ConnectionTimeout);
  Assert.AreEqual(DEF_CONNECTION_RETRY_COUNT, FSection.ConnectionRetryCount);
  Assert.AreEqual(DEF_PAIRING_TIMEOUT, FSection.PairingTimeout);
  Assert.AreEqual(Integer(pmAutomatic), Integer(FSection.PairingMode));
end;

procedure TConnectionConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TConnectionConfigSection;
begin
  Section := TConnectionConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TConnectionConfigSectionTests.Default_ConnectionTimeout_Is10000;
begin
  Assert.AreEqual(10000, FSection.ConnectionTimeout);
end;

procedure TConnectionConfigSectionTests.Default_ConnectionRetryCount_Is2;
begin
  Assert.AreEqual(2, FSection.ConnectionRetryCount);
end;

procedure TConnectionConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.ConnectionTimeout := 5000;
  FSection.ConnectionRetryCount := 5;
  FSection.PairingTimeout := 60000;
  FSection.PairingMode := pmCustom;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_CONNECTION_TIMEOUT, FSection.ConnectionTimeout);
  Assert.AreEqual(DEF_CONNECTION_RETRY_COUNT, FSection.ConnectionRetryCount);
  Assert.AreEqual(DEF_PAIRING_TIMEOUT, FSection.PairingTimeout);
  Assert.AreEqual(Integer(pmAutomatic), Integer(FSection.PairingMode));
end;

procedure TConnectionConfigSectionTests.SetConnectionTimeout_UpdatesValue;
begin
  FSection.ConnectionTimeout := 5000;
  Assert.AreEqual(5000, FSection.ConnectionTimeout);
end;

procedure TConnectionConfigSectionTests.SetConnectionTimeout_SameValue_NoModification;
begin
  FSection.ConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TConnectionConfigSectionTests.SetConnectionTimeout_DifferentValue_NotifiesModified;
begin
  FSection.ConnectionTimeout := 5000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TConnectionConfigSectionTests.SetConnectionRetryCount_UpdatesValue;
begin
  FSection.ConnectionRetryCount := 5;
  Assert.AreEqual(5, FSection.ConnectionRetryCount);
end;

procedure TConnectionConfigSectionTests.SetConnectionRetryCount_SameValue_NoModification;
begin
  FSection.ConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TConnectionConfigSectionTests.SetConnectionRetryCount_DifferentValue_NotifiesModified;
begin
  FSection.ConnectionRetryCount := 5;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TConnectionConfigSectionTests.ImplementsIConnectionConfig;
begin
  Assert.IsTrue(TConnectionConfigSection.GetInterfaceEntry(IConnectionConfig) <> nil,
    'TConnectionConfigSection should implement IConnectionConfig');
end;

procedure TConnectionConfigSectionTests.Default_PairingTimeout_Is30000;
begin
  Assert.AreEqual(30000, FSection.PairingTimeout);
end;

procedure TConnectionConfigSectionTests.SetPairingTimeout_UpdatesValue;
begin
  FSection.PairingTimeout := 60000;
  Assert.AreEqual(60000, FSection.PairingTimeout);
end;

procedure TConnectionConfigSectionTests.SetPairingTimeout_SameValue_NoModification;
begin
  FSection.PairingTimeout := DEF_PAIRING_TIMEOUT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TConnectionConfigSectionTests.SetPairingTimeout_DifferentValue_NotifiesModified;
begin
  FSection.PairingTimeout := 60000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TConnectionConfigSectionTests.Default_PairingMode_IsAutomatic;
begin
  Assert.AreEqual(Integer(pmAutomatic), Integer(FSection.PairingMode));
end;

procedure TConnectionConfigSectionTests.SetPairingMode_UpdatesValue;
begin
  FSection.PairingMode := pmCustom;
  Assert.AreEqual(Integer(pmCustom), Integer(FSection.PairingMode));
end;

procedure TConnectionConfigSectionTests.SetPairingMode_SameValue_NoModification;
begin
  FSection.PairingMode := pmAutomatic;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TConnectionConfigSectionTests.SetPairingMode_DifferentValue_NotifiesModified;
begin
  FSection.PairingMode := pmCustom;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionConfigSectionTests);

end.
