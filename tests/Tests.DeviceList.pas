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
  Vcl.Forms,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  UI.DeviceList,
  Tests.Mocks;

type
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
  TDUnitX.RegisterTestFixture(TDeviceListIndexMapTests);

end.
