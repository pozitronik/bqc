{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Polling Config Section Tests                    }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Polling;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.ConfigSection.Polling,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TPollingConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TPollingConfigSectionTests = class
  private
    FSection: TPollingConfigSection;
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
    procedure Default_PollingMode_IsFallback;
    [Test]
    procedure Default_PollingInterval_Is2000;
    [Test]
    procedure Default_EventDebounceMs_Is500;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { PollingMode Tests }
    [Test]
    procedure SetPollingMode_UpdatesValue;
    [Test]
    procedure SetPollingMode_SameValue_NoModification;
    [Test]
    procedure SetPollingMode_DifferentValue_NotifiesModified;

    { PollingInterval Tests }
    [Test]
    procedure SetPollingInterval_UpdatesValue;
    [Test]
    procedure SetPollingInterval_SameValue_NoModification;
    [Test]
    procedure SetPollingInterval_DifferentValue_NotifiesModified;

    { EventDebounceMs Tests }
    [Test]
    procedure SetEventDebounceMs_UpdatesValue;
    [Test]
    procedure SetEventDebounceMs_SameValue_NoModification;
    [Test]
    procedure SetEventDebounceMs_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIPollingConfig;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TPollingConfigSectionTests }

procedure TPollingConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TPollingConfigSection.Create(HandleModified);
end;

procedure TPollingConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TPollingConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TPollingConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(Integer(DEF_POLLING_MODE), Integer(FSection.PollingMode));
  Assert.AreEqual(DEF_POLLING_INTERVAL, FSection.PollingInterval);
  Assert.AreEqual(DEF_EVENT_DEBOUNCE_MS, FSection.EventDebounceMs);
end;

procedure TPollingConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TPollingConfigSection;
begin
  Section := TPollingConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TPollingConfigSectionTests.Default_PollingMode_IsFallback;
begin
  Assert.AreEqual(Integer(pmFallback), Integer(FSection.PollingMode));
end;

procedure TPollingConfigSectionTests.Default_PollingInterval_Is2000;
begin
  Assert.AreEqual(2000, FSection.PollingInterval);
end;

procedure TPollingConfigSectionTests.Default_EventDebounceMs_Is500;
begin
  Assert.AreEqual(500, FSection.EventDebounceMs);
end;

procedure TPollingConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.PollingMode := pmPrimary;
  FSection.PollingInterval := 5000;
  FSection.EventDebounceMs := 1000;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(Integer(DEF_POLLING_MODE), Integer(FSection.PollingMode));
  Assert.AreEqual(DEF_POLLING_INTERVAL, FSection.PollingInterval);
  Assert.AreEqual(DEF_EVENT_DEBOUNCE_MS, FSection.EventDebounceMs);
end;

procedure TPollingConfigSectionTests.SetPollingMode_UpdatesValue;
begin
  FSection.PollingMode := pmPrimary;
  Assert.AreEqual(Integer(pmPrimary), Integer(FSection.PollingMode));
end;

procedure TPollingConfigSectionTests.SetPollingMode_SameValue_NoModification;
begin
  FSection.PollingMode := DEF_POLLING_MODE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPollingConfigSectionTests.SetPollingMode_DifferentValue_NotifiesModified;
begin
  FSection.PollingMode := pmPrimary;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPollingConfigSectionTests.SetPollingInterval_UpdatesValue;
begin
  FSection.PollingInterval := 5000;
  Assert.AreEqual(5000, FSection.PollingInterval);
end;

procedure TPollingConfigSectionTests.SetPollingInterval_SameValue_NoModification;
begin
  FSection.PollingInterval := DEF_POLLING_INTERVAL;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPollingConfigSectionTests.SetPollingInterval_DifferentValue_NotifiesModified;
begin
  FSection.PollingInterval := 5000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPollingConfigSectionTests.SetEventDebounceMs_UpdatesValue;
begin
  FSection.EventDebounceMs := 1000;
  Assert.AreEqual(1000, FSection.EventDebounceMs);
end;

procedure TPollingConfigSectionTests.SetEventDebounceMs_SameValue_NoModification;
begin
  FSection.EventDebounceMs := DEF_EVENT_DEBOUNCE_MS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPollingConfigSectionTests.SetEventDebounceMs_DifferentValue_NotifiesModified;
begin
  FSection.EventDebounceMs := 1000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPollingConfigSectionTests.ImplementsIPollingConfig;
begin
  Assert.IsTrue(TPollingConfigSection.GetInterfaceEntry(IPollingConfig) <> nil,
    'TPollingConfigSection should implement IPollingConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TPollingConfigSectionTests);

end.
