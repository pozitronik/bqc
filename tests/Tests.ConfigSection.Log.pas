{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Log Config Section Tests                        }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Log;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.LogConfigIntf,
  App.ConfigSection.Log,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TLogConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TLogConfigSectionTests = class
  private
    FSection: TLogConfigSection;
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
    procedure Default_LogEnabled_IsFalse;
    [Test]
    procedure Default_LogFilename_IsBqcLog;
    [Test]
    procedure Default_LogAppend_IsFalse;
    [Test]
    procedure Default_LogLevel_IsError;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { LogEnabled Tests }
    [Test]
    procedure SetLogEnabled_True_UpdatesValue;
    [Test]
    procedure SetLogEnabled_SameValue_NoModification;
    [Test]
    procedure SetLogEnabled_DifferentValue_NotifiesModified;

    { LogFilename Tests }
    [Test]
    procedure SetLogFilename_UpdatesValue;
    [Test]
    procedure SetLogFilename_SameValue_NoModification;
    [Test]
    procedure SetLogFilename_DifferentValue_NotifiesModified;

    { LogAppend Tests }
    [Test]
    procedure SetLogAppend_True_UpdatesValue;
    [Test]
    procedure SetLogAppend_SameValue_NoModification;
    [Test]
    procedure SetLogAppend_DifferentValue_NotifiesModified;

    { LogLevel Tests }
    [Test]
    procedure SetLogLevel_UpdatesValue;
    [Test]
    procedure SetLogLevel_SameValue_NoModification;
    [Test]
    procedure SetLogLevel_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsILogConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TLogConfigSectionTests }

procedure TLogConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TLogConfigSection.Create(HandleModified);
end;

procedure TLogConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TLogConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TLogConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_LOG_ENABLED, FSection.LogEnabled);
  Assert.AreEqual(DEF_LOG_FILENAME, FSection.LogFilename);
  Assert.AreEqual(DEF_LOG_APPEND, FSection.LogAppend);
  Assert.AreEqual(Integer(DEF_LOG_LEVEL), Integer(FSection.LogLevel));
end;

procedure TLogConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TLogConfigSection;
begin
  Section := TLogConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TLogConfigSectionTests.Default_LogEnabled_IsFalse;
begin
  Assert.IsFalse(FSection.LogEnabled);
end;

procedure TLogConfigSectionTests.Default_LogFilename_IsBqcLog;
begin
  Assert.AreEqual('bqc.log', FSection.LogFilename);
end;

procedure TLogConfigSectionTests.Default_LogAppend_IsFalse;
begin
  Assert.IsFalse(FSection.LogAppend);
end;

procedure TLogConfigSectionTests.Default_LogLevel_IsError;
begin
  Assert.AreEqual(Integer(llError), Integer(FSection.LogLevel));
end;

procedure TLogConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.LogEnabled := True;
  FSection.LogFilename := 'custom.log';
  FSection.LogAppend := True;
  FSection.LogLevel := llDebug;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_LOG_ENABLED, FSection.LogEnabled);
  Assert.AreEqual(DEF_LOG_FILENAME, FSection.LogFilename);
  Assert.AreEqual(DEF_LOG_APPEND, FSection.LogAppend);
  Assert.AreEqual(Integer(DEF_LOG_LEVEL), Integer(FSection.LogLevel));
end;

procedure TLogConfigSectionTests.SetLogEnabled_True_UpdatesValue;
begin
  FSection.LogEnabled := True;
  Assert.IsTrue(FSection.LogEnabled);
end;

procedure TLogConfigSectionTests.SetLogEnabled_SameValue_NoModification;
begin
  FSection.LogEnabled := DEF_LOG_ENABLED;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLogConfigSectionTests.SetLogEnabled_DifferentValue_NotifiesModified;
begin
  FSection.LogEnabled := not DEF_LOG_ENABLED;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLogConfigSectionTests.SetLogFilename_UpdatesValue;
begin
  FSection.LogFilename := 'custom.log';
  Assert.AreEqual('custom.log', FSection.LogFilename);
end;

procedure TLogConfigSectionTests.SetLogFilename_SameValue_NoModification;
begin
  FSection.LogFilename := DEF_LOG_FILENAME;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLogConfigSectionTests.SetLogFilename_DifferentValue_NotifiesModified;
begin
  FSection.LogFilename := 'custom.log';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLogConfigSectionTests.SetLogAppend_True_UpdatesValue;
begin
  FSection.LogAppend := True;
  Assert.IsTrue(FSection.LogAppend);
end;

procedure TLogConfigSectionTests.SetLogAppend_SameValue_NoModification;
begin
  FSection.LogAppend := DEF_LOG_APPEND;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLogConfigSectionTests.SetLogAppend_DifferentValue_NotifiesModified;
begin
  FSection.LogAppend := not DEF_LOG_APPEND;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLogConfigSectionTests.SetLogLevel_UpdatesValue;
begin
  FSection.LogLevel := llDebug;
  Assert.AreEqual(Integer(llDebug), Integer(FSection.LogLevel));
end;

procedure TLogConfigSectionTests.SetLogLevel_SameValue_NoModification;
begin
  FSection.LogLevel := DEF_LOG_LEVEL;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLogConfigSectionTests.SetLogLevel_DifferentValue_NotifiesModified;
begin
  FSection.LogLevel := llDebug;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLogConfigSectionTests.ImplementsILogConfig;
begin
  Assert.IsTrue(TLogConfigSection.GetInterfaceEntry(ILogConfig) <> nil,
    'TLogConfigSection should implement ILogConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TLogConfigSectionTests);

end.
