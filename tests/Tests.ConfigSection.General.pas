{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       General Config Section Tests                    }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.General;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConfigSection.General,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TGeneralConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TGeneralConfigSectionTests = class
  private
    FSection: TGeneralConfigSection;
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
    procedure Default_WindowMode_IsWindow;
    [Test]
    procedure Default_OnTop_IsFalse;
    [Test]
    procedure Default_Autostart_IsFalse;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { WindowMode Tests }
    [Test]
    procedure SetWindowMode_UpdatesValue;
    [Test]
    procedure SetWindowMode_SameValue_NoModification;
    [Test]
    procedure SetWindowMode_DifferentValue_NotifiesModified;

    { OnTop Tests }
    [Test]
    procedure SetOnTop_True_UpdatesValue;
    [Test]
    procedure SetOnTop_SameValue_NoModification;
    [Test]
    procedure SetOnTop_DifferentValue_NotifiesModified;

    { Autostart Tests }
    [Test]
    procedure SetAutostart_True_UpdatesValue;
    [Test]
    procedure SetAutostart_SameValue_NoModification;
    [Test]
    procedure SetAutostart_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIGeneralConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TGeneralConfigSectionTests }

procedure TGeneralConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TGeneralConfigSection.Create(HandleModified);
end;

procedure TGeneralConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TGeneralConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TGeneralConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(Integer(DEF_WINDOW_MODE), Integer(FSection.WindowMode));
  Assert.AreEqual(DEF_ON_TOP, FSection.OnTop);
  Assert.AreEqual(DEF_AUTOSTART, FSection.Autostart);
end;

procedure TGeneralConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TGeneralConfigSection;
begin
  Section := TGeneralConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TGeneralConfigSectionTests.Default_WindowMode_IsWindow;
begin
  Assert.AreEqual(Integer(wmWindow), Integer(FSection.WindowMode));
end;

procedure TGeneralConfigSectionTests.Default_OnTop_IsFalse;
begin
  Assert.IsFalse(FSection.OnTop);
end;

procedure TGeneralConfigSectionTests.Default_Autostart_IsFalse;
begin
  Assert.IsFalse(FSection.Autostart);
end;

procedure TGeneralConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.WindowMode := wmMenu;
  FSection.OnTop := True;
  FSection.Autostart := True;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(Integer(DEF_WINDOW_MODE), Integer(FSection.WindowMode));
  Assert.AreEqual(DEF_ON_TOP, FSection.OnTop);
  Assert.AreEqual(DEF_AUTOSTART, FSection.Autostart);
end;

procedure TGeneralConfigSectionTests.SetWindowMode_UpdatesValue;
begin
  FSection.WindowMode := wmMenu;
  Assert.AreEqual(Integer(wmMenu), Integer(FSection.WindowMode));
end;

procedure TGeneralConfigSectionTests.SetWindowMode_SameValue_NoModification;
begin
  FSection.WindowMode := DEF_WINDOW_MODE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TGeneralConfigSectionTests.SetWindowMode_DifferentValue_NotifiesModified;
begin
  FSection.WindowMode := wmMenu;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TGeneralConfigSectionTests.SetOnTop_True_UpdatesValue;
begin
  FSection.OnTop := True;
  Assert.IsTrue(FSection.OnTop);
end;

procedure TGeneralConfigSectionTests.SetOnTop_SameValue_NoModification;
begin
  FSection.OnTop := DEF_ON_TOP;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TGeneralConfigSectionTests.SetOnTop_DifferentValue_NotifiesModified;
begin
  FSection.OnTop := not DEF_ON_TOP;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TGeneralConfigSectionTests.SetAutostart_True_UpdatesValue;
begin
  FSection.Autostart := True;
  Assert.IsTrue(FSection.Autostart);
end;

procedure TGeneralConfigSectionTests.SetAutostart_SameValue_NoModification;
begin
  FSection.Autostart := DEF_AUTOSTART;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TGeneralConfigSectionTests.SetAutostart_DifferentValue_NotifiesModified;
begin
  FSection.Autostart := not DEF_AUTOSTART;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TGeneralConfigSectionTests.ImplementsIGeneralConfig;
begin
  Assert.IsTrue(TGeneralConfigSection.GetInterfaceEntry(IGeneralConfig) <> nil,
    'TGeneralConfigSection should implement IGeneralConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TGeneralConfigSectionTests);

end.
