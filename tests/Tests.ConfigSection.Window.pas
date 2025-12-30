{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Window Config Section Tests                     }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Window;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigInterfaces,
  App.ConfigSection.Window,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TWindowConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TWindowConfigSectionTests = class
  private
    FSection: TWindowConfigSection;
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
    procedure Default_MinimizeToTray_IsTrue;
    [Test]
    procedure Default_CloseToTray_IsTrue;
    [Test]
    procedure Default_MenuHideOnFocusLoss_IsTrue;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { MinimizeToTray Tests }
    [Test]
    procedure SetMinimizeToTray_False_UpdatesValue;
    [Test]
    procedure SetMinimizeToTray_SameValue_NoModification;
    [Test]
    procedure SetMinimizeToTray_DifferentValue_NotifiesModified;

    { CloseToTray Tests }
    [Test]
    procedure SetCloseToTray_False_UpdatesValue;
    [Test]
    procedure SetCloseToTray_SameValue_NoModification;
    [Test]
    procedure SetCloseToTray_DifferentValue_NotifiesModified;

    { MenuHideOnFocusLoss Tests }
    [Test]
    procedure SetMenuHideOnFocusLoss_False_UpdatesValue;
    [Test]
    procedure SetMenuHideOnFocusLoss_SameValue_NoModification;
    [Test]
    procedure SetMenuHideOnFocusLoss_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIWindowConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TWindowConfigSectionTests }

procedure TWindowConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TWindowConfigSection.Create(HandleModified);
end;

procedure TWindowConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TWindowConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TWindowConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_MINIMIZE_TO_TRAY, FSection.MinimizeToTray);
  Assert.AreEqual(DEF_CLOSE_TO_TRAY, FSection.CloseToTray);
  Assert.AreEqual(DEF_MENU_HIDE_ON_FOCUS_LOSS, FSection.MenuHideOnFocusLoss);
end;

procedure TWindowConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TWindowConfigSection;
begin
  Section := TWindowConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TWindowConfigSectionTests.Default_MinimizeToTray_IsTrue;
begin
  Assert.IsTrue(FSection.MinimizeToTray);
end;

procedure TWindowConfigSectionTests.Default_CloseToTray_IsTrue;
begin
  Assert.IsTrue(FSection.CloseToTray);
end;

procedure TWindowConfigSectionTests.Default_MenuHideOnFocusLoss_IsTrue;
begin
  Assert.IsTrue(FSection.MenuHideOnFocusLoss);
end;

procedure TWindowConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.MinimizeToTray := False;
  FSection.CloseToTray := False;
  FSection.MenuHideOnFocusLoss := False;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_MINIMIZE_TO_TRAY, FSection.MinimizeToTray);
  Assert.AreEqual(DEF_CLOSE_TO_TRAY, FSection.CloseToTray);
  Assert.AreEqual(DEF_MENU_HIDE_ON_FOCUS_LOSS, FSection.MenuHideOnFocusLoss);
end;

procedure TWindowConfigSectionTests.SetMinimizeToTray_False_UpdatesValue;
begin
  FSection.MinimizeToTray := False;
  Assert.IsFalse(FSection.MinimizeToTray);
end;

procedure TWindowConfigSectionTests.SetMinimizeToTray_SameValue_NoModification;
begin
  FSection.MinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TWindowConfigSectionTests.SetMinimizeToTray_DifferentValue_NotifiesModified;
begin
  FSection.MinimizeToTray := not DEF_MINIMIZE_TO_TRAY;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TWindowConfigSectionTests.SetCloseToTray_False_UpdatesValue;
begin
  FSection.CloseToTray := False;
  Assert.IsFalse(FSection.CloseToTray);
end;

procedure TWindowConfigSectionTests.SetCloseToTray_SameValue_NoModification;
begin
  FSection.CloseToTray := DEF_CLOSE_TO_TRAY;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TWindowConfigSectionTests.SetCloseToTray_DifferentValue_NotifiesModified;
begin
  FSection.CloseToTray := not DEF_CLOSE_TO_TRAY;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TWindowConfigSectionTests.SetMenuHideOnFocusLoss_False_UpdatesValue;
begin
  FSection.MenuHideOnFocusLoss := False;
  Assert.IsFalse(FSection.MenuHideOnFocusLoss);
end;

procedure TWindowConfigSectionTests.SetMenuHideOnFocusLoss_SameValue_NoModification;
begin
  FSection.MenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TWindowConfigSectionTests.SetMenuHideOnFocusLoss_DifferentValue_NotifiesModified;
begin
  FSection.MenuHideOnFocusLoss := not DEF_MENU_HIDE_ON_FOCUS_LOSS;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TWindowConfigSectionTests.ImplementsIWindowConfig;
begin
  Assert.IsTrue(TWindowConfigSection.GetInterfaceEntry(IWindowConfig) <> nil,
    'TWindowConfigSection should implement IWindowConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TWindowConfigSectionTests);

end.
