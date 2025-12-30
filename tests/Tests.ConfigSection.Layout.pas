{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Layout Config Section Tests                     }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Layout;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.LayoutConfigIntf,
  App.ConfigSection.Layout,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TLayoutConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TLayoutConfigSectionTests = class
  private
    FSection: TLayoutConfigSection;
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
    procedure Default_ItemHeight_Is70;
    [Test]
    procedure Default_ItemPadding_Is6;
    [Test]
    procedure Default_ItemMargin_Is4;
    [Test]
    procedure Default_IconSize_Is46;
    [Test]
    procedure Default_CornerRadius_Is8;
    [Test]
    procedure Default_DeviceNameFontSize_Is12;
    [Test]
    procedure Default_StatusFontSize_Is10;
    [Test]
    procedure Default_AddressFontSize_Is8;
    [Test]
    procedure Default_IconFontSize_Is16;
    [Test]
    procedure Default_ItemBorderWidth_Is0;
    [Test]
    procedure Default_ItemBorderColor_IsGray;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { ItemHeight Tests }
    [Test]
    procedure SetItemHeight_UpdatesValue;
    [Test]
    procedure SetItemHeight_SameValue_NoModification;
    [Test]
    procedure SetItemHeight_DifferentValue_NotifiesModified;

    { ItemPadding Tests }
    [Test]
    procedure SetItemPadding_UpdatesValue;
    [Test]
    procedure SetItemPadding_SameValue_NoModification;
    [Test]
    procedure SetItemPadding_DifferentValue_NotifiesModified;

    { ItemMargin Tests }
    [Test]
    procedure SetItemMargin_UpdatesValue;
    [Test]
    procedure SetItemMargin_SameValue_NoModification;
    [Test]
    procedure SetItemMargin_DifferentValue_NotifiesModified;

    { IconSize Tests }
    [Test]
    procedure SetIconSize_UpdatesValue;
    [Test]
    procedure SetIconSize_SameValue_NoModification;
    [Test]
    procedure SetIconSize_DifferentValue_NotifiesModified;

    { CornerRadius Tests }
    [Test]
    procedure SetCornerRadius_UpdatesValue;
    [Test]
    procedure SetCornerRadius_SameValue_NoModification;
    [Test]
    procedure SetCornerRadius_DifferentValue_NotifiesModified;

    { DeviceNameFontSize Tests }
    [Test]
    procedure SetDeviceNameFontSize_UpdatesValue;
    [Test]
    procedure SetDeviceNameFontSize_SameValue_NoModification;
    [Test]
    procedure SetDeviceNameFontSize_DifferentValue_NotifiesModified;

    { StatusFontSize Tests }
    [Test]
    procedure SetStatusFontSize_UpdatesValue;
    [Test]
    procedure SetStatusFontSize_SameValue_NoModification;
    [Test]
    procedure SetStatusFontSize_DifferentValue_NotifiesModified;

    { AddressFontSize Tests }
    [Test]
    procedure SetAddressFontSize_UpdatesValue;
    [Test]
    procedure SetAddressFontSize_SameValue_NoModification;
    [Test]
    procedure SetAddressFontSize_DifferentValue_NotifiesModified;

    { IconFontSize Tests }
    [Test]
    procedure SetIconFontSize_UpdatesValue;
    [Test]
    procedure SetIconFontSize_SameValue_NoModification;
    [Test]
    procedure SetIconFontSize_DifferentValue_NotifiesModified;

    { ItemBorderWidth Tests }
    [Test]
    procedure SetItemBorderWidth_UpdatesValue;
    [Test]
    procedure SetItemBorderWidth_SameValue_NoModification;
    [Test]
    procedure SetItemBorderWidth_DifferentValue_NotifiesModified;

    { ItemBorderColor Tests }
    [Test]
    procedure SetItemBorderColor_UpdatesValue;
    [Test]
    procedure SetItemBorderColor_SameValue_NoModification;
    [Test]
    procedure SetItemBorderColor_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsILayoutConfig;
  end;

implementation

uses
  App.Config;

{ TLayoutConfigSectionTests }

procedure TLayoutConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TLayoutConfigSection.Create(HandleModified);
end;

procedure TLayoutConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TLayoutConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TLayoutConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(DEF_ITEM_HEIGHT, FSection.ItemHeight);
  Assert.AreEqual(DEF_ITEM_PADDING, FSection.ItemPadding);
  Assert.AreEqual(DEF_ITEM_MARGIN, FSection.ItemMargin);
  Assert.AreEqual(DEF_ICON_SIZE, FSection.IconSize);
  Assert.AreEqual(DEF_CORNER_RADIUS, FSection.CornerRadius);
  Assert.AreEqual(DEF_DEVICE_NAME_FONT_SIZE, FSection.DeviceNameFontSize);
  Assert.AreEqual(DEF_STATUS_FONT_SIZE, FSection.StatusFontSize);
  Assert.AreEqual(DEF_ADDRESS_FONT_SIZE, FSection.AddressFontSize);
  Assert.AreEqual(DEF_ICON_FONT_SIZE, FSection.IconFontSize);
  Assert.AreEqual(DEF_ITEM_BORDER_WIDTH, FSection.ItemBorderWidth);
  Assert.AreEqual(DEF_ITEM_BORDER_COLOR, FSection.ItemBorderColor);
end;

procedure TLayoutConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TLayoutConfigSection;
begin
  Section := TLayoutConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TLayoutConfigSectionTests.Default_ItemHeight_Is70;
begin
  Assert.AreEqual(70, FSection.ItemHeight);
end;

procedure TLayoutConfigSectionTests.Default_ItemPadding_Is6;
begin
  Assert.AreEqual(6, FSection.ItemPadding);
end;

procedure TLayoutConfigSectionTests.Default_ItemMargin_Is4;
begin
  Assert.AreEqual(4, FSection.ItemMargin);
end;

procedure TLayoutConfigSectionTests.Default_IconSize_Is46;
begin
  Assert.AreEqual(46, FSection.IconSize);
end;

procedure TLayoutConfigSectionTests.Default_CornerRadius_Is8;
begin
  Assert.AreEqual(8, FSection.CornerRadius);
end;

procedure TLayoutConfigSectionTests.Default_DeviceNameFontSize_Is12;
begin
  Assert.AreEqual(12, FSection.DeviceNameFontSize);
end;

procedure TLayoutConfigSectionTests.Default_StatusFontSize_Is10;
begin
  Assert.AreEqual(10, FSection.StatusFontSize);
end;

procedure TLayoutConfigSectionTests.Default_AddressFontSize_Is8;
begin
  Assert.AreEqual(8, FSection.AddressFontSize);
end;

procedure TLayoutConfigSectionTests.Default_IconFontSize_Is16;
begin
  Assert.AreEqual(16, FSection.IconFontSize);
end;

procedure TLayoutConfigSectionTests.Default_ItemBorderWidth_Is0;
begin
  Assert.AreEqual(0, FSection.ItemBorderWidth);
end;

procedure TLayoutConfigSectionTests.Default_ItemBorderColor_IsGray;
begin
  Assert.AreEqual($00808080, FSection.ItemBorderColor);
end;

procedure TLayoutConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.ItemHeight := 100;
  FSection.ItemPadding := 10;
  FSection.ItemMargin := 8;
  FSection.IconSize := 64;
  FSection.CornerRadius := 12;
  FSection.DeviceNameFontSize := 14;
  FSection.StatusFontSize := 12;
  FSection.AddressFontSize := 10;
  FSection.IconFontSize := 20;
  FSection.ItemBorderWidth := 2;
  FSection.ItemBorderColor := $00FF0000;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(DEF_ITEM_HEIGHT, FSection.ItemHeight);
  Assert.AreEqual(DEF_ITEM_PADDING, FSection.ItemPadding);
  Assert.AreEqual(DEF_ITEM_MARGIN, FSection.ItemMargin);
  Assert.AreEqual(DEF_ICON_SIZE, FSection.IconSize);
  Assert.AreEqual(DEF_CORNER_RADIUS, FSection.CornerRadius);
  Assert.AreEqual(DEF_DEVICE_NAME_FONT_SIZE, FSection.DeviceNameFontSize);
  Assert.AreEqual(DEF_STATUS_FONT_SIZE, FSection.StatusFontSize);
  Assert.AreEqual(DEF_ADDRESS_FONT_SIZE, FSection.AddressFontSize);
  Assert.AreEqual(DEF_ICON_FONT_SIZE, FSection.IconFontSize);
  Assert.AreEqual(DEF_ITEM_BORDER_WIDTH, FSection.ItemBorderWidth);
  Assert.AreEqual(DEF_ITEM_BORDER_COLOR, FSection.ItemBorderColor);
end;

procedure TLayoutConfigSectionTests.SetItemHeight_UpdatesValue;
begin
  FSection.ItemHeight := 100;
  Assert.AreEqual(100, FSection.ItemHeight);
end;

procedure TLayoutConfigSectionTests.SetItemHeight_SameValue_NoModification;
begin
  FSection.ItemHeight := DEF_ITEM_HEIGHT;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetItemHeight_DifferentValue_NotifiesModified;
begin
  FSection.ItemHeight := 100;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetItemPadding_UpdatesValue;
begin
  FSection.ItemPadding := 10;
  Assert.AreEqual(10, FSection.ItemPadding);
end;

procedure TLayoutConfigSectionTests.SetItemPadding_SameValue_NoModification;
begin
  FSection.ItemPadding := DEF_ITEM_PADDING;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetItemPadding_DifferentValue_NotifiesModified;
begin
  FSection.ItemPadding := 10;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetItemMargin_UpdatesValue;
begin
  FSection.ItemMargin := 8;
  Assert.AreEqual(8, FSection.ItemMargin);
end;

procedure TLayoutConfigSectionTests.SetItemMargin_SameValue_NoModification;
begin
  FSection.ItemMargin := DEF_ITEM_MARGIN;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetItemMargin_DifferentValue_NotifiesModified;
begin
  FSection.ItemMargin := 8;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetIconSize_UpdatesValue;
begin
  FSection.IconSize := 64;
  Assert.AreEqual(64, FSection.IconSize);
end;

procedure TLayoutConfigSectionTests.SetIconSize_SameValue_NoModification;
begin
  FSection.IconSize := DEF_ICON_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetIconSize_DifferentValue_NotifiesModified;
begin
  FSection.IconSize := 64;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetCornerRadius_UpdatesValue;
begin
  FSection.CornerRadius := 12;
  Assert.AreEqual(12, FSection.CornerRadius);
end;

procedure TLayoutConfigSectionTests.SetCornerRadius_SameValue_NoModification;
begin
  FSection.CornerRadius := DEF_CORNER_RADIUS;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetCornerRadius_DifferentValue_NotifiesModified;
begin
  FSection.CornerRadius := 12;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetDeviceNameFontSize_UpdatesValue;
begin
  FSection.DeviceNameFontSize := 14;
  Assert.AreEqual(14, FSection.DeviceNameFontSize);
end;

procedure TLayoutConfigSectionTests.SetDeviceNameFontSize_SameValue_NoModification;
begin
  FSection.DeviceNameFontSize := DEF_DEVICE_NAME_FONT_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetDeviceNameFontSize_DifferentValue_NotifiesModified;
begin
  FSection.DeviceNameFontSize := 14;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetStatusFontSize_UpdatesValue;
begin
  FSection.StatusFontSize := 12;
  Assert.AreEqual(12, FSection.StatusFontSize);
end;

procedure TLayoutConfigSectionTests.SetStatusFontSize_SameValue_NoModification;
begin
  FSection.StatusFontSize := DEF_STATUS_FONT_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetStatusFontSize_DifferentValue_NotifiesModified;
begin
  FSection.StatusFontSize := 12;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetAddressFontSize_UpdatesValue;
begin
  FSection.AddressFontSize := 10;
  Assert.AreEqual(10, FSection.AddressFontSize);
end;

procedure TLayoutConfigSectionTests.SetAddressFontSize_SameValue_NoModification;
begin
  FSection.AddressFontSize := DEF_ADDRESS_FONT_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetAddressFontSize_DifferentValue_NotifiesModified;
begin
  FSection.AddressFontSize := 10;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetIconFontSize_UpdatesValue;
begin
  FSection.IconFontSize := 20;
  Assert.AreEqual(20, FSection.IconFontSize);
end;

procedure TLayoutConfigSectionTests.SetIconFontSize_SameValue_NoModification;
begin
  FSection.IconFontSize := DEF_ICON_FONT_SIZE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetIconFontSize_DifferentValue_NotifiesModified;
begin
  FSection.IconFontSize := 20;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetItemBorderWidth_UpdatesValue;
begin
  FSection.ItemBorderWidth := 2;
  Assert.AreEqual(2, FSection.ItemBorderWidth);
end;

procedure TLayoutConfigSectionTests.SetItemBorderWidth_SameValue_NoModification;
begin
  FSection.ItemBorderWidth := DEF_ITEM_BORDER_WIDTH;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetItemBorderWidth_DifferentValue_NotifiesModified;
begin
  FSection.ItemBorderWidth := 2;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.SetItemBorderColor_UpdatesValue;
begin
  FSection.ItemBorderColor := $00FF0000;
  Assert.AreEqual($00FF0000, FSection.ItemBorderColor);
end;

procedure TLayoutConfigSectionTests.SetItemBorderColor_SameValue_NoModification;
begin
  FSection.ItemBorderColor := DEF_ITEM_BORDER_COLOR;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TLayoutConfigSectionTests.SetItemBorderColor_DifferentValue_NotifiesModified;
begin
  FSection.ItemBorderColor := $00FF0000;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TLayoutConfigSectionTests.ImplementsILayoutConfig;
begin
  Assert.IsTrue(TLayoutConfigSection.GetInterfaceEntry(ILayoutConfig) <> nil,
    'TLayoutConfigSection should implement ILayoutConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TLayoutConfigSectionTests);

end.
