{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Profile Config Section Tests                    }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Profile;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ProfileConfigIntf,
  App.ConfigSection.Profile,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TProfileConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TProfileConfigSectionTests = class
  private
    FSection: TProfileConfigSection;
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
    procedure Default_ShowProfiles_IsFalse;
    [Test]
    procedure Default_ProfileFontSize_Is7;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { ShowProfiles Tests }
    [Test]
    procedure SetShowProfiles_True_UpdatesValue;
    [Test]
    procedure SetShowProfiles_SameValue_NoModification;
    [Test]
    procedure SetShowProfiles_DifferentValue_NotifiesModified;

    { ProfileFontSize Tests }
    [Test]
    procedure SetProfileFontSize_UpdatesValue;
    [Test]
    procedure SetProfileFontSize_SameValue_NoModification;
    [Test]
    procedure SetProfileFontSize_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIProfileConfig;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TProfileConfigSectionTests }

procedure TProfileConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TProfileConfigSection.Create(HandleModified);
end;

procedure TProfileConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TProfileConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TProfileConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_SHOW_PROFILES, FSection.ShowProfiles);
  Assert.AreEqual(DEF_PROFILE_FONT_SIZE, FSection.ProfileFontSize);
end;

procedure TProfileConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TProfileConfigSection;
begin
  Section := TProfileConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TProfileConfigSectionTests.Default_ShowProfiles_IsFalse;
begin
  Assert.IsFalse(FSection.ShowProfiles);
end;

procedure TProfileConfigSectionTests.Default_ProfileFontSize_Is7;
begin
  Assert.AreEqual(7, FSection.ProfileFontSize);
end;

procedure TProfileConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.ShowProfiles := True;
  FSection.ProfileFontSize := 12;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_SHOW_PROFILES, FSection.ShowProfiles);
  Assert.AreEqual(DEF_PROFILE_FONT_SIZE, FSection.ProfileFontSize);
end;

procedure TProfileConfigSectionTests.SetShowProfiles_True_UpdatesValue;
begin
  FSection.ShowProfiles := True;
  Assert.IsTrue(FSection.ShowProfiles);
end;

procedure TProfileConfigSectionTests.SetShowProfiles_SameValue_NoModification;
begin
  FSection.ShowProfiles := DEF_SHOW_PROFILES;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TProfileConfigSectionTests.SetShowProfiles_DifferentValue_NotifiesModified;
begin
  FSection.ShowProfiles := not DEF_SHOW_PROFILES;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TProfileConfigSectionTests.SetProfileFontSize_UpdatesValue;
begin
  FSection.ProfileFontSize := 12;
  Assert.AreEqual(12, FSection.ProfileFontSize);
end;

procedure TProfileConfigSectionTests.SetProfileFontSize_SameValue_NoModification;
begin
  FSection.ProfileFontSize := DEF_PROFILE_FONT_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TProfileConfigSectionTests.SetProfileFontSize_DifferentValue_NotifiesModified;
begin
  FSection.ProfileFontSize := 12;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TProfileConfigSectionTests.ImplementsIProfileConfig;
begin
  Assert.IsTrue(TProfileConfigSection.GetInterfaceEntry(IProfileConfig) <> nil,
    'TProfileConfigSection should implement IProfileConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TProfileConfigSectionTests);

end.
