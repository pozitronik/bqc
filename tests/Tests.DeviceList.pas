{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TDeviceListBox Unit Tests                       }
{                                                       }
{       Tests for device list control including         }
{       O(1) index map lookup behavior.                 }
{                                                       }
{*******************************************************}

unit Tests.DeviceList;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Forms,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  UI.DeviceList,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TDeviceListBox layout caching behavior.
  /// Verifies that config values are cached to avoid repeated interface calls.
  /// </summary>
  [TestFixture]
  TDeviceListLayoutCacheTests = class
  private
    FForm: TForm;
    FDeviceList: TDeviceListBox;
    FLayoutConfig: TMockLayoutConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FProfileConfig: TMockProfileConfig;

    function CreateDisplayItem(AAddress: UInt64; const AName: string;
      AConnectionState: TBluetoothConnectionState): TDeviceDisplayItem;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Layout Cache Tests }
    [Test]
    procedure SetLayoutConfig_RefreshesCache;

    [Test]
    procedure SetLayoutConfig_CachedItemHeightUsedForCalculations;

    [Test]
    procedure SetLayoutConfig_UpdatedConfigValue_ReflectedInCache;

    [Test]
    procedure SetAppearanceConfig_RefreshesCache;

    [Test]
    procedure GetItemRect_UsesCachedItemMargin;
  end;

  /// <summary>
  /// Test fixture for TDeviceListBox profile height calculation.
  /// Verifies consistent height calculation between layout and draw.
  /// </summary>
  [TestFixture]
  TDeviceListProfileHeightTests = class
  private
    FForm: TForm;
    FDeviceList: TDeviceListBox;
    FLayoutConfig: TMockLayoutConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FProfileConfig: TMockProfileConfig;

    function CreateDisplayItemWithProfiles(AAddress: UInt64; const AName: string;
      AProfileCount: Integer): TDeviceDisplayItem;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Profile Height Tests }
    [Test]
    procedure ItemWithNoProfiles_HasBaseHeight;

    [Test]
    procedure ItemWithOneProfile_HasBaseHeight;

    [Test]
    procedure ItemWithMultipleProfiles_HasExpandedHeight;

    [Test]
    procedure ProfileFontSize_AffectsItemHeight;

    [Test]
    procedure ConsistentHeight_BetweenItems;
  end;

  /// <summary>
  /// Test fixture for TDeviceListBox index map behavior.
  /// Verifies O(1) lookup for UpdateDisplayItem.
  /// </summary>
  [TestFixture]
  TDeviceListIndexMapTests = class
  private
    FForm: TForm;  // Parent form required for VCL control
    FDeviceList: TDeviceListBox;
    FLayoutConfig: TMockLayoutConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FProfileConfig: TMockProfileConfig;

    function CreateDisplayItem(AAddress: UInt64; const AName: string;
      AConnectionState: TBluetoothConnectionState): TDeviceDisplayItem;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { SetDisplayItems Tests }
    [Test]
    procedure SetDisplayItems_EmptyArray_ClearsIndexMap;

    [Test]
    procedure SetDisplayItems_SingleItem_BuildsIndexMap;

    [Test]
    procedure SetDisplayItems_MultipleItems_BuildsCompleteIndexMap;

    [Test]
    procedure SetDisplayItems_CalledTwice_RebuildsIndexMap;

    { UpdateDisplayItem Tests }
    [Test]
    procedure UpdateDisplayItem_ExistingDevice_UpdatesItem;

    [Test]
    procedure UpdateDisplayItem_NonExistingDevice_DoesNothing;

    [Test]
    procedure UpdateDisplayItem_FirstItem_UpdatesCorrectly;

    [Test]
    procedure UpdateDisplayItem_LastItem_UpdatesCorrectly;

    [Test]
    procedure UpdateDisplayItem_MiddleItem_UpdatesCorrectly;

    [Test]
    procedure UpdateDisplayItem_PreservesOtherItems;
  end;

implementation

{ TDeviceListLayoutCacheTests }

procedure TDeviceListLayoutCacheTests.Setup;
begin
  FLayoutConfig := TMockLayoutConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FProfileConfig := TMockProfileConfig.Create;

  FForm := TForm.CreateNew(nil);
  FForm.Width := 400;
  FForm.Height := 600;

  FDeviceList := TDeviceListBox.Create(FForm);
  FDeviceList.Parent := FForm;
end;

procedure TDeviceListLayoutCacheTests.TearDown;
begin
  FForm.Free;
end;

function TDeviceListLayoutCacheTests.CreateDisplayItem(AAddress: UInt64;
  const AName: string; AConnectionState: TBluetoothConnectionState): TDeviceDisplayItem;
begin
  Result := Default(TDeviceDisplayItem);
  Result.Device := CreateTestDevice(AAddress, AName, btAudioOutput, AConnectionState);
  Result.DisplayName := AName;
  Result.IsPinned := False;
  Result.SortGroup := 1;
end;

procedure TDeviceListLayoutCacheTests.SetLayoutConfig_RefreshesCache;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Configure layout config with specific ItemHeight
  FLayoutConfig.ItemHeight := 80;
  FLayoutConfig.ItemMargin := 6;

  // Act: Set layout config (triggers cache refresh via setter)
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Test Device', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Assert: GetItemRect should use cached values
  ItemRect := FDeviceList.GetItemRect(0);
  // ItemRect height should be ItemHeight (80), accounting for margin offset
  Assert.AreEqual(80, ItemRect.Bottom - ItemRect.Top);
end;

procedure TDeviceListLayoutCacheTests.SetLayoutConfig_CachedItemHeightUsedForCalculations;
var
  Items: TDeviceDisplayItemArray;
  ItemRect1, ItemRect2: TRect;
begin
  // Arrange: Set config with known ItemHeight and margin
  FLayoutConfig.ItemHeight := 60;
  FLayoutConfig.ItemMargin := 8;

  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;

  SetLength(Items, 2);
  Items[0] := CreateDisplayItem($001, 'Device 1', csConnected);
  Items[1] := CreateDisplayItem($002, 'Device 2', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Get item rects
  ItemRect1 := FDeviceList.GetItemRect(0);
  ItemRect2 := FDeviceList.GetItemRect(1);

  // Assert: Second item should start at ItemHeight + ItemMargin from first
  Assert.AreEqual(ItemRect1.Bottom + FLayoutConfig.ItemMargin, ItemRect2.Top);
end;

procedure TDeviceListLayoutCacheTests.SetLayoutConfig_UpdatedConfigValue_ReflectedInCache;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Initial config
  FLayoutConfig.ItemHeight := 70;
  FLayoutConfig.ItemMargin := 4;
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Device', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Change ItemHeight and re-assign config to trigger refresh
  FLayoutConfig.ItemHeight := 100;
  FDeviceList.LayoutConfig := FLayoutConfig;  // Re-assignment triggers cache refresh
  FDeviceList.SetDisplayItems(Items);  // Recalculate heights

  // Assert: New height should be reflected
  ItemRect := FDeviceList.GetItemRect(0);
  Assert.AreEqual(100, ItemRect.Bottom - ItemRect.Top);
end;

procedure TDeviceListLayoutCacheTests.SetAppearanceConfig_RefreshesCache;
var
  Items: TDeviceDisplayItemArray;
begin
  // Arrange
  FDeviceList.LayoutConfig := FLayoutConfig;
  FAppearanceConfig.ShowDeviceIcons := True;

  // Act: Set appearance config (triggers cache refresh via setter)
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Device', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Assert: No crash, control properly initialized
  Assert.AreEqual(1, FDeviceList.DeviceCount);
end;

procedure TDeviceListLayoutCacheTests.GetItemRect_UsesCachedItemMargin;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Specific margin value
  FLayoutConfig.ItemHeight := 50;
  FLayoutConfig.ItemMargin := 10;
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Device', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act
  ItemRect := FDeviceList.GetItemRect(0);

  // Assert: First item top should be margin offset
  Assert.AreEqual(10, ItemRect.Top);
end;

{ TDeviceListProfileHeightTests }

procedure TDeviceListProfileHeightTests.Setup;
begin
  FLayoutConfig := TMockLayoutConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FProfileConfig := TMockProfileConfig.Create;

  FForm := TForm.CreateNew(nil);
  FForm.Width := 400;
  FForm.Height := 600;

  FDeviceList := TDeviceListBox.Create(FForm);
  FDeviceList.Parent := FForm;
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;
end;

procedure TDeviceListProfileHeightTests.TearDown;
begin
  FForm.Free;
end;

function TDeviceListProfileHeightTests.CreateDisplayItemWithProfiles(
  AAddress: UInt64; const AName: string;
  AProfileCount: Integer): TDeviceDisplayItem;
var
  I: Integer;
  ConnectionState: TProfileConnectionState;
begin
  Result := Default(TDeviceDisplayItem);
  Result.Device := CreateTestDevice(AAddress, AName, btAudioOutput, csConnected);
  Result.DisplayName := AName;
  Result.IsPinned := False;
  Result.SortGroup := 1;

  // Create profiles using TBluetoothProfile.Create
  SetLength(Result.Profiles, AProfileCount);
  for I := 0 to AProfileCount - 1 do
  begin
    if I = 0 then
      ConnectionState := pcsConnected
    else
      ConnectionState := pcsAvailable;
    Result.Profiles[I] := TBluetoothProfile.Create(
      bptA2DPSink, TGUID.Empty, ConnectionState);
  end;
end;

procedure TDeviceListProfileHeightTests.ItemWithNoProfiles_HasBaseHeight;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Enable profiles, create item with no profiles
  FProfileConfig.ShowProfiles := True;
  FLayoutConfig.ItemHeight := 70;
  FDeviceList.LayoutConfig := FLayoutConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItemWithProfiles($001, 'Device', 0);
  FDeviceList.SetDisplayItems(Items);

  // Act
  ItemRect := FDeviceList.GetItemRect(0);

  // Assert: Height should be base height (no profile expansion)
  Assert.AreEqual(70, ItemRect.Bottom - ItemRect.Top);
end;

procedure TDeviceListProfileHeightTests.ItemWithOneProfile_HasBaseHeight;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Single profile does not expand (no tree structure needed)
  FProfileConfig.ShowProfiles := True;
  FLayoutConfig.ItemHeight := 70;
  FDeviceList.LayoutConfig := FLayoutConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItemWithProfiles($001, 'Device', 1);
  FDeviceList.SetDisplayItems(Items);

  // Act
  ItemRect := FDeviceList.GetItemRect(0);

  // Assert: Height should be base height (single profile = no expansion)
  Assert.AreEqual(70, ItemRect.Bottom - ItemRect.Top);
end;

procedure TDeviceListProfileHeightTests.ItemWithMultipleProfiles_HasExpandedHeight;
var
  Items: TDeviceDisplayItemArray;
  ItemRect: TRect;
begin
  // Arrange: Multiple profiles should expand height
  FProfileConfig.ShowProfiles := True;
  FProfileConfig.ProfileFontSize := 7;
  FLayoutConfig.ItemHeight := 70;
  FDeviceList.LayoutConfig := FLayoutConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItemWithProfiles($001, 'Device', 3);
  FDeviceList.SetDisplayItems(Items);

  // Act
  ItemRect := FDeviceList.GetItemRect(0);

  // Assert: Height > base height due to profile section
  // Formula: BaseHeight + ProfileCount * LineHeight + LineHeight/2
  // LineHeight = ProfileFontSize(7) * PROFILE_LINE_HEIGHT_FACTOR(2) = 14
  // Profile section = 3 * 14 + 7 = 49
  // Total = 70 + 49 = 119
  Assert.IsTrue(ItemRect.Bottom - ItemRect.Top > 70,
    'Item with profiles should be taller than base height');
  Assert.AreEqual(119, ItemRect.Bottom - ItemRect.Top);
end;

procedure TDeviceListProfileHeightTests.ProfileFontSize_AffectsItemHeight;
var
  Items: TDeviceDisplayItemArray;
  ItemRect1, ItemRect2: TRect;
  Height1, Height2: Integer;
begin
  // Arrange: Create item with profiles using smaller font
  FProfileConfig.ShowProfiles := True;
  FProfileConfig.ProfileFontSize := 6;
  FLayoutConfig.ItemHeight := 70;
  FDeviceList.LayoutConfig := FLayoutConfig;

  SetLength(Items, 1);
  Items[0] := CreateDisplayItemWithProfiles($001, 'Device', 3);
  FDeviceList.SetDisplayItems(Items);
  ItemRect1 := FDeviceList.GetItemRect(0);
  Height1 := ItemRect1.Bottom - ItemRect1.Top;

  // Act: Change font size to larger
  FProfileConfig.ProfileFontSize := 10;
  FDeviceList.SetDisplayItems(Items);  // Recalculate heights
  ItemRect2 := FDeviceList.GetItemRect(0);
  Height2 := ItemRect2.Bottom - ItemRect2.Top;

  // Assert: Larger font size should result in taller item
  Assert.IsTrue(Height2 > Height1,
    'Larger profile font size should result in taller item');
end;

procedure TDeviceListProfileHeightTests.ConsistentHeight_BetweenItems;
var
  Items: TDeviceDisplayItemArray;
  ItemRect1, ItemRect2: TRect;
begin
  // Arrange: Two items with same number of profiles
  FProfileConfig.ShowProfiles := True;
  FProfileConfig.ProfileFontSize := 7;
  FLayoutConfig.ItemHeight := 70;
  FDeviceList.LayoutConfig := FLayoutConfig;

  SetLength(Items, 2);
  Items[0] := CreateDisplayItemWithProfiles($001, 'Device 1', 2);
  Items[1] := CreateDisplayItemWithProfiles($002, 'Device 2', 2);
  FDeviceList.SetDisplayItems(Items);

  // Act
  ItemRect1 := FDeviceList.GetItemRect(0);
  ItemRect2 := FDeviceList.GetItemRect(1);

  // Assert: Both items should have same height
  Assert.AreEqual(
    ItemRect1.Bottom - ItemRect1.Top,
    ItemRect2.Bottom - ItemRect2.Top,
    'Items with same profile count should have same height'
  );
end;

{ TDeviceListIndexMapTests }

procedure TDeviceListIndexMapTests.Setup;
begin
  FLayoutConfig := TMockLayoutConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FProfileConfig := TMockProfileConfig.Create;

  // Create parent form (required for VCL control to function)
  FForm := TForm.CreateNew(nil);
  FForm.Width := 400;
  FForm.Height := 600;

  FDeviceList := TDeviceListBox.Create(FForm);
  FDeviceList.Parent := FForm;
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := FProfileConfig;
end;

procedure TDeviceListIndexMapTests.TearDown;
begin
  // FDeviceList is owned by FForm, will be freed with it
  FForm.Free;
  // Interfaces are reference-counted
end;

function TDeviceListIndexMapTests.CreateDisplayItem(AAddress: UInt64;
  const AName: string; AConnectionState: TBluetoothConnectionState): TDeviceDisplayItem;
begin
  Result := Default(TDeviceDisplayItem);
  Result.Device := CreateTestDevice(AAddress, AName, btAudioOutput, AConnectionState);
  Result.DisplayName := AName;
  Result.IsPinned := False;
  Result.SortGroup := 1;
end;

{ SetDisplayItems Tests }

procedure TDeviceListIndexMapTests.SetDisplayItems_EmptyArray_ClearsIndexMap;
var
  Items: TDeviceDisplayItemArray;
begin
  // Arrange: First set some items
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Device 1', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Set empty array
  SetLength(Items, 0);
  FDeviceList.SetDisplayItems(Items);

  // Assert: DeviceCount should be 0
  Assert.AreEqual(0, FDeviceList.DeviceCount);
end;

procedure TDeviceListIndexMapTests.SetDisplayItems_SingleItem_BuildsIndexMap;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Original Name', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Update the item (will use index map lookup)
  UpdatedItem := CreateDisplayItem($001, 'Updated Name', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert: Item should be updated (index map worked)
  Assert.AreEqual('Updated Name', FDeviceList.Devices[0].Name);
end;

procedure TDeviceListIndexMapTests.SetDisplayItems_MultipleItems_BuildsCompleteIndexMap;
var
  Items: TDeviceDisplayItemArray;
  I: Integer;
begin
  // Arrange: Create 10 devices
  SetLength(Items, 10);
  for I := 0 to 9 do
    Items[I] := CreateDisplayItem(I + 1, 'Device ' + IntToStr(I + 1), csDisconnected);

  // Act
  FDeviceList.SetDisplayItems(Items);

  // Assert: All devices accessible
  Assert.AreEqual(10, FDeviceList.DeviceCount);
  for I := 0 to 9 do
    Assert.AreEqual(UInt64(I + 1), FDeviceList.Devices[I].AddressInt);
end;

procedure TDeviceListIndexMapTests.SetDisplayItems_CalledTwice_RebuildsIndexMap;
var
  Items1, Items2: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange: Set first list
  SetLength(Items1, 2);
  Items1[0] := CreateDisplayItem($001, 'Device A', csConnected);
  Items1[1] := CreateDisplayItem($002, 'Device B', csConnected);
  FDeviceList.SetDisplayItems(Items1);

  // Act: Set second list with different devices
  SetLength(Items2, 2);
  Items2[0] := CreateDisplayItem($003, 'Device C', csConnected);
  Items2[1] := CreateDisplayItem($004, 'Device D', csConnected);
  FDeviceList.SetDisplayItems(Items2);

  // Assert: Old device addresses should not be found
  // Update with old address should not change anything
  UpdatedItem := CreateDisplayItem($001, 'Should Not Appear', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Verify new devices are there
  Assert.AreEqual(UInt64($003), FDeviceList.Devices[0].AddressInt);
  Assert.AreEqual(UInt64($004), FDeviceList.Devices[1].AddressInt);
end;

{ UpdateDisplayItem Tests }

procedure TDeviceListIndexMapTests.UpdateDisplayItem_ExistingDevice_UpdatesItem;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001122334455, 'Original', csDisconnected);
  FDeviceList.SetDisplayItems(Items);

  // Act
  UpdatedItem := CreateDisplayItem($001122334455, 'Updated', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert
  Assert.AreEqual('Updated', FDeviceList.Devices[0].Name);
  Assert.AreEqual(Ord(csConnected), Ord(FDeviceList.Devices[0].ConnectionState));
end;

procedure TDeviceListIndexMapTests.UpdateDisplayItem_NonExistingDevice_DoesNothing;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Device 1', csConnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Try to update non-existing device
  UpdatedItem := CreateDisplayItem($999, 'Ghost Device', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert: Original item unchanged, no crash
  Assert.AreEqual(1, FDeviceList.DeviceCount);
  Assert.AreEqual('Device 1', FDeviceList.Devices[0].Name);
end;

procedure TDeviceListIndexMapTests.UpdateDisplayItem_FirstItem_UpdatesCorrectly;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 3);
  Items[0] := CreateDisplayItem($001, 'First', csDisconnected);
  Items[1] := CreateDisplayItem($002, 'Second', csDisconnected);
  Items[2] := CreateDisplayItem($003, 'Third', csDisconnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Update first item
  UpdatedItem := CreateDisplayItem($001, 'First Updated', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert
  Assert.AreEqual('First Updated', FDeviceList.Devices[0].Name);
  Assert.AreEqual('Second', FDeviceList.Devices[1].Name);
  Assert.AreEqual('Third', FDeviceList.Devices[2].Name);
end;

procedure TDeviceListIndexMapTests.UpdateDisplayItem_LastItem_UpdatesCorrectly;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 3);
  Items[0] := CreateDisplayItem($001, 'First', csDisconnected);
  Items[1] := CreateDisplayItem($002, 'Second', csDisconnected);
  Items[2] := CreateDisplayItem($003, 'Third', csDisconnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Update last item
  UpdatedItem := CreateDisplayItem($003, 'Third Updated', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert
  Assert.AreEqual('First', FDeviceList.Devices[0].Name);
  Assert.AreEqual('Second', FDeviceList.Devices[1].Name);
  Assert.AreEqual('Third Updated', FDeviceList.Devices[2].Name);
end;

procedure TDeviceListIndexMapTests.UpdateDisplayItem_MiddleItem_UpdatesCorrectly;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
begin
  // Arrange
  SetLength(Items, 5);
  Items[0] := CreateDisplayItem($001, 'First', csDisconnected);
  Items[1] := CreateDisplayItem($002, 'Second', csDisconnected);
  Items[2] := CreateDisplayItem($003, 'Middle', csDisconnected);
  Items[3] := CreateDisplayItem($004, 'Fourth', csDisconnected);
  Items[4] := CreateDisplayItem($005, 'Fifth', csDisconnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Update middle item
  UpdatedItem := CreateDisplayItem($003, 'Middle Updated', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert
  Assert.AreEqual('First', FDeviceList.Devices[0].Name);
  Assert.AreEqual('Second', FDeviceList.Devices[1].Name);
  Assert.AreEqual('Middle Updated', FDeviceList.Devices[2].Name);
  Assert.AreEqual('Fourth', FDeviceList.Devices[3].Name);
  Assert.AreEqual('Fifth', FDeviceList.Devices[4].Name);
end;

procedure TDeviceListIndexMapTests.UpdateDisplayItem_PreservesOtherItems;
var
  Items: TDeviceDisplayItemArray;
  UpdatedItem: TDeviceDisplayItem;
  I: Integer;
begin
  // Arrange: Create 10 devices
  SetLength(Items, 10);
  for I := 0 to 9 do
    Items[I] := CreateDisplayItem(I + 1, 'Device ' + IntToStr(I + 1), csDisconnected);
  FDeviceList.SetDisplayItems(Items);

  // Act: Update device 5 (index 4)
  UpdatedItem := CreateDisplayItem(5, 'Device 5 Updated', csConnected);
  FDeviceList.UpdateDisplayItem(UpdatedItem);

  // Assert: Only device 5 changed, others preserved
  for I := 0 to 9 do
  begin
    if I = 4 then
      Assert.AreEqual('Device 5 Updated', FDeviceList.Devices[I].Name)
    else
      Assert.AreEqual('Device ' + IntToStr(I + 1), FDeviceList.Devices[I].Name);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceListLayoutCacheTests);
  TDUnitX.RegisterTestFixture(TDeviceListProfileHeightTests);
  TDUnitX.RegisterTestFixture(TDeviceListIndexMapTests);

end.
