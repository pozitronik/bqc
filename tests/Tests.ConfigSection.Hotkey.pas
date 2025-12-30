{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Hotkey Config Section Tests                     }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Hotkey;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigInterfaces,
  App.ConfigSection.Hotkey,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for THotkeyConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  THotkeyConfigSectionTests = class
  private
    FSection: THotkeyConfigSection;
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
    procedure Default_Hotkey_IsWinK;
    [Test]
    procedure Default_UseLowLevelHook_IsTrue;
    [Test]
    procedure Default_CastPanelHotkey_IsEmpty;
    [Test]
    procedure Default_BluetoothPanelHotkey_IsEmpty;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { Hotkey Tests }
    [Test]
    procedure SetHotkey_UpdatesValue;
    [Test]
    procedure SetHotkey_SameValue_NoModification;
    [Test]
    procedure SetHotkey_DifferentValue_NotifiesModified;

    { UseLowLevelHook Tests }
    [Test]
    procedure SetUseLowLevelHook_False_UpdatesValue;
    [Test]
    procedure SetUseLowLevelHook_SameValue_NoModification;
    [Test]
    procedure SetUseLowLevelHook_DifferentValue_NotifiesModified;

    { CastPanelHotkey Tests }
    [Test]
    procedure SetCastPanelHotkey_UpdatesValue;
    [Test]
    procedure SetCastPanelHotkey_SameValue_NoModification;
    [Test]
    procedure SetCastPanelHotkey_DifferentValue_NotifiesModified;

    { BluetoothPanelHotkey Tests }
    [Test]
    procedure SetBluetoothPanelHotkey_UpdatesValue;
    [Test]
    procedure SetBluetoothPanelHotkey_SameValue_NoModification;
    [Test]
    procedure SetBluetoothPanelHotkey_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIHotkeyConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ THotkeyConfigSectionTests }

procedure THotkeyConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := THotkeyConfigSection.Create(HandleModified);
end;

procedure THotkeyConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure THotkeyConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure THotkeyConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_HOTKEY, FSection.Hotkey);
  Assert.AreEqual(DEF_USE_LOW_LEVEL_HOOK, FSection.UseLowLevelHook);
  Assert.AreEqual(DEF_CAST_PANEL_HOTKEY, FSection.CastPanelHotkey);
  Assert.AreEqual(DEF_BLUETOOTH_PANEL_HOTKEY, FSection.BluetoothPanelHotkey);
end;

procedure THotkeyConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: THotkeyConfigSection;
begin
  Section := THotkeyConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure THotkeyConfigSectionTests.Default_Hotkey_IsWinK;
begin
  Assert.AreEqual('Win+K', FSection.Hotkey);
end;

procedure THotkeyConfigSectionTests.Default_UseLowLevelHook_IsTrue;
begin
  Assert.IsTrue(FSection.UseLowLevelHook);
end;

procedure THotkeyConfigSectionTests.Default_CastPanelHotkey_IsEmpty;
begin
  Assert.AreEqual('', FSection.CastPanelHotkey);
end;

procedure THotkeyConfigSectionTests.Default_BluetoothPanelHotkey_IsEmpty;
begin
  Assert.AreEqual('', FSection.BluetoothPanelHotkey);
end;

procedure THotkeyConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.Hotkey := 'Ctrl+Alt+B';
  FSection.UseLowLevelHook := False;
  FSection.CastPanelHotkey := 'Win+C';
  FSection.BluetoothPanelHotkey := 'Win+B';

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_HOTKEY, FSection.Hotkey);
  Assert.AreEqual(DEF_USE_LOW_LEVEL_HOOK, FSection.UseLowLevelHook);
  Assert.AreEqual(DEF_CAST_PANEL_HOTKEY, FSection.CastPanelHotkey);
  Assert.AreEqual(DEF_BLUETOOTH_PANEL_HOTKEY, FSection.BluetoothPanelHotkey);
end;

procedure THotkeyConfigSectionTests.SetHotkey_UpdatesValue;
begin
  FSection.Hotkey := 'Ctrl+Alt+B';
  Assert.AreEqual('Ctrl+Alt+B', FSection.Hotkey);
end;

procedure THotkeyConfigSectionTests.SetHotkey_SameValue_NoModification;
begin
  FSection.Hotkey := DEF_HOTKEY;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure THotkeyConfigSectionTests.SetHotkey_DifferentValue_NotifiesModified;
begin
  FSection.Hotkey := 'Ctrl+Alt+B';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure THotkeyConfigSectionTests.SetUseLowLevelHook_False_UpdatesValue;
begin
  FSection.UseLowLevelHook := False;
  Assert.IsFalse(FSection.UseLowLevelHook);
end;

procedure THotkeyConfigSectionTests.SetUseLowLevelHook_SameValue_NoModification;
begin
  FSection.UseLowLevelHook := DEF_USE_LOW_LEVEL_HOOK;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure THotkeyConfigSectionTests.SetUseLowLevelHook_DifferentValue_NotifiesModified;
begin
  FSection.UseLowLevelHook := not DEF_USE_LOW_LEVEL_HOOK;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure THotkeyConfigSectionTests.SetCastPanelHotkey_UpdatesValue;
begin
  FSection.CastPanelHotkey := 'Win+C';
  Assert.AreEqual('Win+C', FSection.CastPanelHotkey);
end;

procedure THotkeyConfigSectionTests.SetCastPanelHotkey_SameValue_NoModification;
begin
  FSection.CastPanelHotkey := DEF_CAST_PANEL_HOTKEY;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure THotkeyConfigSectionTests.SetCastPanelHotkey_DifferentValue_NotifiesModified;
begin
  FSection.CastPanelHotkey := 'Win+C';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure THotkeyConfigSectionTests.SetBluetoothPanelHotkey_UpdatesValue;
begin
  FSection.BluetoothPanelHotkey := 'Win+B';
  Assert.AreEqual('Win+B', FSection.BluetoothPanelHotkey);
end;

procedure THotkeyConfigSectionTests.SetBluetoothPanelHotkey_SameValue_NoModification;
begin
  FSection.BluetoothPanelHotkey := DEF_BLUETOOTH_PANEL_HOTKEY;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure THotkeyConfigSectionTests.SetBluetoothPanelHotkey_DifferentValue_NotifiesModified;
begin
  FSection.BluetoothPanelHotkey := 'Win+B';
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure THotkeyConfigSectionTests.ImplementsIHotkeyConfig;
begin
  Assert.IsTrue(THotkeyConfigSection.GetInterfaceEntry(IHotkeyConfig) <> nil,
    'THotkeyConfigSection should implement IHotkeyConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(THotkeyConfigSectionTests);

end.
