{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Tray Config Section Tests               }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.BatteryTray;

interface

uses
  DUnitX.TestFramework,
  Vcl.Graphics,
  System.SysUtils,
  App.BatteryTrayConfigIntf,
  App.ConfigSection.BatteryTray,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TBatteryTrayConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TBatteryTrayConfigSectionTests = class
  private
    FSection: TBatteryTrayConfigSection;
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
    procedure Default_ShowBatteryTrayIcons_IsFalse;
    [Test]
    procedure Default_IconColor_IsGreen;
    [Test]
    procedure Default_BackgroundColor_IsTransparent;
    [Test]
    procedure Default_ShowNumericValue_IsFalse;
    [Test]
    procedure Default_LowBatteryThreshold_Is20;
    [Test]
    procedure Default_NotifyLowBattery_IsTrue;
    [Test]
    procedure Default_NotifyFullyCharged_IsFalse;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { ShowBatteryTrayIcons Tests }
    [Test]
    procedure SetShowBatteryTrayIcons_True_UpdatesValue;
    [Test]
    procedure SetShowBatteryTrayIcons_SameValue_NoModification;
    [Test]
    procedure SetShowBatteryTrayIcons_DifferentValue_NotifiesModified;

    { DefaultIconColor Tests }
    [Test]
    procedure SetDefaultIconColor_UpdatesValue;
    [Test]
    procedure SetDefaultIconColor_SameValue_NoModification;
    [Test]
    procedure SetDefaultIconColor_DifferentValue_NotifiesModified;

    { DefaultBackgroundColor Tests }
    [Test]
    procedure SetDefaultBackgroundColor_UpdatesValue;
    [Test]
    procedure SetDefaultBackgroundColor_TransparentValue_Works;

    { DefaultShowNumericValue Tests }
    [Test]
    procedure SetDefaultShowNumericValue_True_UpdatesValue;
    [Test]
    procedure SetDefaultShowNumericValue_DifferentValue_NotifiesModified;

    { DefaultLowBatteryThreshold Tests }
    [Test]
    procedure SetDefaultLowBatteryThreshold_UpdatesValue;
    [Test]
    procedure SetDefaultLowBatteryThreshold_ValidRange_Works;
    [Test]
    procedure SetDefaultLowBatteryThreshold_DifferentValue_NotifiesModified;

    { DefaultNotifyLowBattery Tests }
    [Test]
    procedure SetDefaultNotifyLowBattery_False_UpdatesValue;
    [Test]
    procedure SetDefaultNotifyLowBattery_DifferentValue_NotifiesModified;

    { DefaultNotifyFullyCharged Tests }
    [Test]
    procedure SetDefaultNotifyFullyCharged_True_UpdatesValue;
    [Test]
    procedure SetDefaultNotifyFullyCharged_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIBatteryTrayConfig;
  end;

implementation

{ TBatteryTrayConfigSectionTests }

procedure TBatteryTrayConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TBatteryTrayConfigSection.Create(HandleModified);
end;

procedure TBatteryTrayConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TBatteryTrayConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TBatteryTrayConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_SHOW_BATTERY_TRAY_ICONS, FSection.ShowBatteryTrayIcons);
  Assert.AreEqual(Integer(DEF_BATTERY_ICON_COLOR), Integer(FSection.DefaultIconColor));
  Assert.AreEqual(Integer(DEF_BATTERY_BACKGROUND_COLOR), Integer(FSection.DefaultBackgroundColor));
  Assert.AreEqual(DEF_SHOW_NUMERIC_VALUE, FSection.DefaultShowNumericValue);
  Assert.AreEqual(DEF_LOW_BATTERY_THRESHOLD, FSection.DefaultLowBatteryThreshold);
  Assert.AreEqual(DEF_NOTIFY_LOW_BATTERY, FSection.DefaultNotifyLowBattery);
  Assert.AreEqual(DEF_NOTIFY_FULLY_CHARGED, FSection.DefaultNotifyFullyCharged);
end;

procedure TBatteryTrayConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TBatteryTrayConfigSection;
begin
  Section := TBatteryTrayConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TBatteryTrayConfigSectionTests.Default_ShowBatteryTrayIcons_IsFalse;
begin
  Assert.IsFalse(FSection.ShowBatteryTrayIcons);
end;

procedure TBatteryTrayConfigSectionTests.Default_IconColor_IsGreen;
begin
  Assert.AreEqual(Integer(DEF_BATTERY_ICON_COLOR), Integer(FSection.DefaultIconColor));
end;

procedure TBatteryTrayConfigSectionTests.Default_BackgroundColor_IsTransparent;
begin
  Assert.AreEqual(Integer(DEF_BATTERY_BACKGROUND_COLOR), Integer(FSection.DefaultBackgroundColor));
end;

procedure TBatteryTrayConfigSectionTests.Default_ShowNumericValue_IsFalse;
begin
  Assert.IsFalse(FSection.DefaultShowNumericValue);
end;

procedure TBatteryTrayConfigSectionTests.Default_LowBatteryThreshold_Is20;
begin
  Assert.AreEqual(20, FSection.DefaultLowBatteryThreshold);
end;

procedure TBatteryTrayConfigSectionTests.Default_NotifyLowBattery_IsTrue;
begin
  Assert.IsTrue(FSection.DefaultNotifyLowBattery);
end;

procedure TBatteryTrayConfigSectionTests.Default_NotifyFullyCharged_IsFalse;
begin
  Assert.IsFalse(FSection.DefaultNotifyFullyCharged);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.ShowBatteryTrayIcons := True;
  FSection.DefaultIconColor := clRed;
  FSection.DefaultBackgroundColor := clBlue;
  FSection.DefaultShowNumericValue := True;
  FSection.DefaultLowBatteryThreshold := 50;
  FSection.DefaultNotifyLowBattery := False;
  FSection.DefaultNotifyFullyCharged := True;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_SHOW_BATTERY_TRAY_ICONS, FSection.ShowBatteryTrayIcons);
  Assert.AreEqual(Integer(DEF_BATTERY_ICON_COLOR), Integer(FSection.DefaultIconColor));
  Assert.AreEqual(Integer(DEF_BATTERY_BACKGROUND_COLOR), Integer(FSection.DefaultBackgroundColor));
  Assert.AreEqual(DEF_SHOW_NUMERIC_VALUE, FSection.DefaultShowNumericValue);
  Assert.AreEqual(DEF_LOW_BATTERY_THRESHOLD, FSection.DefaultLowBatteryThreshold);
  Assert.AreEqual(DEF_NOTIFY_LOW_BATTERY, FSection.DefaultNotifyLowBattery);
  Assert.AreEqual(DEF_NOTIFY_FULLY_CHARGED, FSection.DefaultNotifyFullyCharged);
end;

procedure TBatteryTrayConfigSectionTests.SetShowBatteryTrayIcons_True_UpdatesValue;
begin
  FSection.ShowBatteryTrayIcons := True;
  Assert.IsTrue(FSection.ShowBatteryTrayIcons);
end;

procedure TBatteryTrayConfigSectionTests.SetShowBatteryTrayIcons_SameValue_NoModification;
begin
  FSection.ShowBatteryTrayIcons := DEF_SHOW_BATTERY_TRAY_ICONS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TBatteryTrayConfigSectionTests.SetShowBatteryTrayIcons_DifferentValue_NotifiesModified;
begin
  FSection.ShowBatteryTrayIcons := not DEF_SHOW_BATTERY_TRAY_ICONS;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultIconColor_UpdatesValue;
begin
  FSection.DefaultIconColor := clRed;
  Assert.AreEqual(Integer(clRed), Integer(FSection.DefaultIconColor));
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultIconColor_SameValue_NoModification;
begin
  FSection.DefaultIconColor := DEF_BATTERY_ICON_COLOR;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultIconColor_DifferentValue_NotifiesModified;
begin
  FSection.DefaultIconColor := clBlue;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultBackgroundColor_UpdatesValue;
begin
  FSection.DefaultBackgroundColor := clWhite;
  Assert.AreEqual(Integer(clWhite), Integer(FSection.DefaultBackgroundColor));
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultBackgroundColor_TransparentValue_Works;
begin
  FSection.DefaultBackgroundColor := clNone;
  Assert.AreEqual(Integer(clNone), Integer(FSection.DefaultBackgroundColor));
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultShowNumericValue_True_UpdatesValue;
begin
  FSection.DefaultShowNumericValue := True;
  Assert.IsTrue(FSection.DefaultShowNumericValue);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultShowNumericValue_DifferentValue_NotifiesModified;
begin
  FSection.DefaultShowNumericValue := True;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultLowBatteryThreshold_UpdatesValue;
begin
  FSection.DefaultLowBatteryThreshold := 30;
  Assert.AreEqual(30, FSection.DefaultLowBatteryThreshold);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultLowBatteryThreshold_ValidRange_Works;
begin
  FSection.DefaultLowBatteryThreshold := MIN_LOW_BATTERY_THRESHOLD;
  Assert.AreEqual(MIN_LOW_BATTERY_THRESHOLD, FSection.DefaultLowBatteryThreshold);

  FModifiedCalled := False;
  FSection.DefaultLowBatteryThreshold := MAX_LOW_BATTERY_THRESHOLD;
  Assert.AreEqual(MAX_LOW_BATTERY_THRESHOLD, FSection.DefaultLowBatteryThreshold);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultLowBatteryThreshold_DifferentValue_NotifiesModified;
begin
  FSection.DefaultLowBatteryThreshold := 25;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultNotifyLowBattery_False_UpdatesValue;
begin
  FSection.DefaultNotifyLowBattery := False;
  Assert.IsFalse(FSection.DefaultNotifyLowBattery);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultNotifyLowBattery_DifferentValue_NotifiesModified;
begin
  FSection.DefaultNotifyLowBattery := False;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultNotifyFullyCharged_True_UpdatesValue;
begin
  FSection.DefaultNotifyFullyCharged := True;
  Assert.IsTrue(FSection.DefaultNotifyFullyCharged);
end;

procedure TBatteryTrayConfigSectionTests.SetDefaultNotifyFullyCharged_DifferentValue_NotifiesModified;
begin
  FSection.DefaultNotifyFullyCharged := True;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TBatteryTrayConfigSectionTests.ImplementsIBatteryTrayConfig;
begin
  // Check interface implementation without creating interface reference
  // to avoid ref counting issues with manual Free in TearDown
  Assert.IsTrue(TBatteryTrayConfigSection.GetInterfaceEntry(IBatteryTrayConfig) <> nil,
    'TBatteryTrayConfigSection should implement IBatteryTrayConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TBatteryTrayConfigSectionTests);

end.
