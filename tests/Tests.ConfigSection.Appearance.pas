{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Appearance Config Section Tests                 }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Appearance;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.AppearanceConfigIntf,
  App.ConfigSection.Appearance,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TAppearanceConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TAppearanceConfigSectionTests = class
  private
    FSection: TAppearanceConfigSection;
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
    procedure Default_ShowAddresses_IsFalse;
    [Test]
    procedure Default_Theme_IsEmpty;
    [Test]
    procedure Default_VsfDir_IsThemes;
    [Test]
    procedure Default_ShowLastSeen_IsFalse;
    [Test]
    procedure Default_LastSeenFormat_IsRelative;
    [Test]
    procedure Default_ShowDeviceIcons_IsTrue;
    [Test]
    procedure Default_ConnectedColor_IsDarkGreen;
    [Test]
    procedure Default_ShowBatteryLevel_IsTrue;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { ShowAddresses Tests }
    [Test]
    procedure SetShowAddresses_True_UpdatesValue;
    [Test]
    procedure SetShowAddresses_SameValue_NoModification;
    [Test]
    procedure SetShowAddresses_DifferentValue_NotifiesModified;

    { Theme Tests }
    [Test]
    procedure SetTheme_UpdatesValue;
    [Test]
    procedure SetTheme_SameValue_NoModification;
    [Test]
    procedure SetTheme_DifferentValue_NotifiesModified;

    { VsfDir Tests }
    [Test]
    procedure SetVsfDir_UpdatesValue;
    [Test]
    procedure SetVsfDir_SameValue_NoModification;
    [Test]
    procedure SetVsfDir_DifferentValue_NotifiesModified;

    { ShowLastSeen Tests }
    [Test]
    procedure SetShowLastSeen_True_UpdatesValue;
    [Test]
    procedure SetShowLastSeen_SameValue_NoModification;
    [Test]
    procedure SetShowLastSeen_DifferentValue_NotifiesModified;

    { LastSeenFormat Tests }
    [Test]
    procedure SetLastSeenFormat_UpdatesValue;
    [Test]
    procedure SetLastSeenFormat_SameValue_NoModification;
    [Test]
    procedure SetLastSeenFormat_DifferentValue_NotifiesModified;

    { ShowDeviceIcons Tests }
    [Test]
    procedure SetShowDeviceIcons_False_UpdatesValue;
    [Test]
    procedure SetShowDeviceIcons_SameValue_NoModification;
    [Test]
    procedure SetShowDeviceIcons_DifferentValue_NotifiesModified;

    { ConnectedColor Tests }
    [Test]
    procedure SetConnectedColor_UpdatesValue;
    [Test]
    procedure SetConnectedColor_SameValue_NoModification;
    [Test]
    procedure SetConnectedColor_DifferentValue_NotifiesModified;

    { ShowBatteryLevel Tests }
    [Test]
    procedure SetShowBatteryLevel_False_UpdatesValue;
    [Test]
    procedure SetShowBatteryLevel_SameValue_NoModification;
    [Test]
    procedure SetShowBatteryLevel_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIAppearanceConfig;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TAppearanceConfigSectionTests }

procedure TAppearanceConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TAppearanceConfigSection.Create(HandleModified);
end;

procedure TAppearanceConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TAppearanceConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TAppearanceConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_SHOW_ADDRESSES, FSection.ShowAddresses);
  Assert.AreEqual(DEF_THEME, FSection.Theme);
  Assert.AreEqual(DEF_VSF_DIR, FSection.VsfDir);
  Assert.AreEqual(DEF_SHOW_LAST_SEEN, FSection.ShowLastSeen);
  Assert.AreEqual(Integer(DEF_LAST_SEEN_FORMAT), Integer(FSection.LastSeenFormat));
  Assert.AreEqual(DEF_SHOW_DEVICE_ICONS, FSection.ShowDeviceIcons);
  Assert.AreEqual(DEF_CONNECTED_COLOR, FSection.ConnectedColor);
  Assert.AreEqual(DEF_SHOW_BATTERY_LEVEL, FSection.ShowBatteryLevel);
end;

procedure TAppearanceConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TAppearanceConfigSection;
begin
  Section := TAppearanceConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TAppearanceConfigSectionTests.Default_ShowAddresses_IsFalse;
begin
  Assert.IsFalse(FSection.ShowAddresses);
end;

procedure TAppearanceConfigSectionTests.Default_Theme_IsEmpty;
begin
  Assert.AreEqual('', FSection.Theme);
end;

procedure TAppearanceConfigSectionTests.Default_VsfDir_IsThemes;
begin
  Assert.AreEqual('themes', FSection.VsfDir);
end;

procedure TAppearanceConfigSectionTests.Default_ShowLastSeen_IsFalse;
begin
  Assert.IsFalse(FSection.ShowLastSeen);
end;

procedure TAppearanceConfigSectionTests.Default_LastSeenFormat_IsRelative;
begin
  Assert.AreEqual(Integer(lsfRelative), Integer(FSection.LastSeenFormat));
end;

procedure TAppearanceConfigSectionTests.Default_ShowDeviceIcons_IsTrue;
begin
  Assert.IsTrue(FSection.ShowDeviceIcons);
end;

procedure TAppearanceConfigSectionTests.Default_ConnectedColor_IsDarkGreen;
begin
  Assert.AreEqual($00008000, FSection.ConnectedColor);
end;

procedure TAppearanceConfigSectionTests.Default_ShowBatteryLevel_IsTrue;
begin
  Assert.IsTrue(FSection.ShowBatteryLevel);
end;

procedure TAppearanceConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.ShowAddresses := True;
  FSection.Theme := 'dark';
  FSection.VsfDir := 'custom';
  FSection.ShowLastSeen := True;
  FSection.LastSeenFormat := lsfAbsolute;
  FSection.ShowDeviceIcons := False;
  FSection.ConnectedColor := $00FF0000;
  FSection.ShowBatteryLevel := False;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_SHOW_ADDRESSES, FSection.ShowAddresses);
  Assert.AreEqual(DEF_THEME, FSection.Theme);
  Assert.AreEqual(DEF_VSF_DIR, FSection.VsfDir);
  Assert.AreEqual(DEF_SHOW_LAST_SEEN, FSection.ShowLastSeen);
  Assert.AreEqual(Integer(DEF_LAST_SEEN_FORMAT), Integer(FSection.LastSeenFormat));
  Assert.AreEqual(DEF_SHOW_DEVICE_ICONS, FSection.ShowDeviceIcons);
  Assert.AreEqual(DEF_CONNECTED_COLOR, FSection.ConnectedColor);
  Assert.AreEqual(DEF_SHOW_BATTERY_LEVEL, FSection.ShowBatteryLevel);
end;

procedure TAppearanceConfigSectionTests.SetShowAddresses_True_UpdatesValue;
begin
  FSection.ShowAddresses := True;
  Assert.IsTrue(FSection.ShowAddresses);
end;

procedure TAppearanceConfigSectionTests.SetShowAddresses_SameValue_NoModification;
begin
  FSection.ShowAddresses := DEF_SHOW_ADDRESSES;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetShowAddresses_DifferentValue_NotifiesModified;
begin
  FSection.ShowAddresses := not DEF_SHOW_ADDRESSES;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetTheme_UpdatesValue;
begin
  FSection.Theme := 'dark';
  Assert.AreEqual('dark', FSection.Theme);
end;

procedure TAppearanceConfigSectionTests.SetTheme_SameValue_NoModification;
begin
  FSection.Theme := DEF_THEME;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetTheme_DifferentValue_NotifiesModified;
begin
  FSection.Theme := 'dark';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetVsfDir_UpdatesValue;
begin
  FSection.VsfDir := 'custom';
  Assert.AreEqual('custom', FSection.VsfDir);
end;

procedure TAppearanceConfigSectionTests.SetVsfDir_SameValue_NoModification;
begin
  FSection.VsfDir := DEF_VSF_DIR;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetVsfDir_DifferentValue_NotifiesModified;
begin
  FSection.VsfDir := 'custom';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetShowLastSeen_True_UpdatesValue;
begin
  FSection.ShowLastSeen := True;
  Assert.IsTrue(FSection.ShowLastSeen);
end;

procedure TAppearanceConfigSectionTests.SetShowLastSeen_SameValue_NoModification;
begin
  FSection.ShowLastSeen := DEF_SHOW_LAST_SEEN;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetShowLastSeen_DifferentValue_NotifiesModified;
begin
  FSection.ShowLastSeen := not DEF_SHOW_LAST_SEEN;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetLastSeenFormat_UpdatesValue;
begin
  FSection.LastSeenFormat := lsfAbsolute;
  Assert.AreEqual(Integer(lsfAbsolute), Integer(FSection.LastSeenFormat));
end;

procedure TAppearanceConfigSectionTests.SetLastSeenFormat_SameValue_NoModification;
begin
  FSection.LastSeenFormat := DEF_LAST_SEEN_FORMAT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetLastSeenFormat_DifferentValue_NotifiesModified;
begin
  FSection.LastSeenFormat := lsfAbsolute;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetShowDeviceIcons_False_UpdatesValue;
begin
  FSection.ShowDeviceIcons := False;
  Assert.IsFalse(FSection.ShowDeviceIcons);
end;

procedure TAppearanceConfigSectionTests.SetShowDeviceIcons_SameValue_NoModification;
begin
  FSection.ShowDeviceIcons := DEF_SHOW_DEVICE_ICONS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetShowDeviceIcons_DifferentValue_NotifiesModified;
begin
  FSection.ShowDeviceIcons := not DEF_SHOW_DEVICE_ICONS;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetConnectedColor_UpdatesValue;
begin
  FSection.ConnectedColor := $00FF0000;
  Assert.AreEqual($00FF0000, FSection.ConnectedColor);
end;

procedure TAppearanceConfigSectionTests.SetConnectedColor_SameValue_NoModification;
begin
  FSection.ConnectedColor := DEF_CONNECTED_COLOR;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetConnectedColor_DifferentValue_NotifiesModified;
begin
  FSection.ConnectedColor := $00FF0000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.SetShowBatteryLevel_False_UpdatesValue;
begin
  FSection.ShowBatteryLevel := False;
  Assert.IsFalse(FSection.ShowBatteryLevel);
end;

procedure TAppearanceConfigSectionTests.SetShowBatteryLevel_SameValue_NoModification;
begin
  FSection.ShowBatteryLevel := DEF_SHOW_BATTERY_LEVEL;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TAppearanceConfigSectionTests.SetShowBatteryLevel_DifferentValue_NotifiesModified;
begin
  FSection.ShowBatteryLevel := not DEF_SHOW_BATTERY_LEVEL;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TAppearanceConfigSectionTests.ImplementsIAppearanceConfig;
begin
  Assert.IsTrue(TAppearanceConfigSection.GetInterfaceEntry(IAppearanceConfig) <> nil,
    'TAppearanceConfigSection should implement IAppearanceConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TAppearanceConfigSectionTests);

end.
