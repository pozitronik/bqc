{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TDeviceSorter Tests                             }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.DeviceSorter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Bluetooth.Types,
  App.ConfigInterfaces,
  UI.DeviceList,
  UI.DeviceSorter,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TDeviceSorter class.
  /// Tests sorting logic for device display items.
  /// </summary>
  [TestFixture]
  TDeviceSorterTests = class
  private
    function CreateDisplayItem(AAddress: UInt64; const AName: string;
      APinned, AConnected: Boolean; ALastSeen: TDateTime): TDeviceDisplayItem;
  public
    { Compare Tests }
    [Test]
    procedure Compare_PinnedVsNotPinned_PinnedFirst;

    [Test]
    procedure Compare_ConnectedVsDisconnected_ConnectedFirst;

    [Test]
    procedure Compare_SameGroup_RecentLastSeenFirst;

    [Test]
    procedure Compare_SameGroupSameLastSeen_AlphabeticalByName;

    [Test]
    procedure Compare_SameGroupSameNameSameLastSeen_ByAddress;

    { Sort Tests }
    [Test]
    procedure Sort_EmptyArray_NoError;

    [Test]
    procedure Sort_SingleItem_NoChange;

    [Test]
    procedure Sort_MultipleItems_CorrectOrder;

    [Test]
    procedure Sort_MixedGroups_PinnedThenConnectedThenDisconnected;

    [Test]
    procedure Sort_SameGroup_SortedByLastSeenThenName;
  end;

implementation

{ TDeviceSorterTests }

function TDeviceSorterTests.CreateDisplayItem(AAddress: UInt64; const AName: string;
  APinned, AConnected: Boolean; ALastSeen: TDateTime): TDeviceDisplayItem;
var
  Device: TBluetoothDeviceInfo;
  SortGroup: Integer;
begin
  if AConnected then
    Device := CreateTestDevice(AAddress, AName, btAudioOutput, csConnected)
  else
    Device := CreateTestDevice(AAddress, AName, btAudioOutput, csDisconnected);

  if APinned then
    SortGroup := 0
  else if AConnected then
    SortGroup := 1
  else
    SortGroup := 2;

  Result := TDeviceDisplayItem.Create(
    Device,
    AName,
    APinned,
    btAudioOutput,
    '',
    ALastSeen,
    SortGroup,
    TBatteryStatus.NotSupported,
    ''
  );
end;

{ Compare Tests }

procedure TDeviceSorterTests.Compare_PinnedVsNotPinned_PinnedFirst;
var
  Pinned, NotPinned: TDeviceDisplayItem;
begin
  Pinned := CreateDisplayItem($001, 'Pinned', True, False, Now);
  NotPinned := CreateDisplayItem($002, 'Not Pinned', False, True, Now);

  // Pinned should come before not pinned
  Assert.IsTrue(TDeviceSorter.Compare(Pinned, NotPinned) < 0);
  Assert.IsTrue(TDeviceSorter.Compare(NotPinned, Pinned) > 0);
end;

procedure TDeviceSorterTests.Compare_ConnectedVsDisconnected_ConnectedFirst;
var
  Connected, Disconnected: TDeviceDisplayItem;
begin
  Connected := CreateDisplayItem($001, 'Connected', False, True, Now);
  Disconnected := CreateDisplayItem($002, 'Disconnected', False, False, Now);

  // Connected (group 1) should come before disconnected (group 2)
  Assert.IsTrue(TDeviceSorter.Compare(Connected, Disconnected) < 0);
  Assert.IsTrue(TDeviceSorter.Compare(Disconnected, Connected) > 0);
end;

procedure TDeviceSorterTests.Compare_SameGroup_RecentLastSeenFirst;
var
  Recent, Older: TDeviceDisplayItem;
begin
  Recent := CreateDisplayItem($001, 'Device A', False, False, IncHour(Now, -1));
  Older := CreateDisplayItem($002, 'Device B', False, False, IncHour(Now, -5));

  // More recent should come first
  Assert.IsTrue(TDeviceSorter.Compare(Recent, Older) < 0);
  Assert.IsTrue(TDeviceSorter.Compare(Older, Recent) > 0);
end;

procedure TDeviceSorterTests.Compare_SameGroupSameLastSeen_AlphabeticalByName;
var
  ItemA, ItemB: TDeviceDisplayItem;
  SameTime: TDateTime;
begin
  SameTime := Now;
  ItemA := CreateDisplayItem($001, 'Alpha', False, False, SameTime);
  ItemB := CreateDisplayItem($002, 'Beta', False, False, SameTime);

  // Alpha should come before Beta
  Assert.IsTrue(TDeviceSorter.Compare(ItemA, ItemB) < 0);
  Assert.IsTrue(TDeviceSorter.Compare(ItemB, ItemA) > 0);
end;

procedure TDeviceSorterTests.Compare_SameGroupSameNameSameLastSeen_ByAddress;
var
  Lower, Higher: TDeviceDisplayItem;
  SameTime: TDateTime;
begin
  SameTime := Now;
  Lower := CreateDisplayItem($001, 'Same Name', False, False, SameTime);
  Higher := CreateDisplayItem($999, 'Same Name', False, False, SameTime);

  // Lower address should come first
  Assert.IsTrue(TDeviceSorter.Compare(Lower, Higher) < 0);
  Assert.IsTrue(TDeviceSorter.Compare(Higher, Lower) > 0);
end;

{ Sort Tests }

procedure TDeviceSorterTests.Sort_EmptyArray_NoError;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 0);
  TDeviceSorter.Sort(Items);  // Should not raise
  Assert.AreEqual<Integer>(0, Length(Items));
end;

procedure TDeviceSorterTests.Sort_SingleItem_NoChange;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($001, 'Single', False, False, Now);

  TDeviceSorter.Sort(Items);

  Assert.AreEqual<Integer>(1, Length(Items));
  Assert.AreEqual('Single', Items[0].DisplayName);
end;

procedure TDeviceSorterTests.Sort_MultipleItems_CorrectOrder;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 3);
  Items[0] := CreateDisplayItem($003, 'Zebra', False, False, Now);
  Items[1] := CreateDisplayItem($001, 'Alpha', False, False, Now);
  Items[2] := CreateDisplayItem($002, 'Beta', False, False, Now);

  TDeviceSorter.Sort(Items);

  // All same group with same time, should be alphabetical
  Assert.AreEqual('Alpha', Items[0].DisplayName);
  Assert.AreEqual('Beta', Items[1].DisplayName);
  Assert.AreEqual('Zebra', Items[2].DisplayName);
end;

procedure TDeviceSorterTests.Sort_MixedGroups_PinnedThenConnectedThenDisconnected;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 4);
  Items[0] := CreateDisplayItem($001, 'Disconnected', False, False, Now);
  Items[1] := CreateDisplayItem($002, 'Pinned', True, False, Now);
  Items[2] := CreateDisplayItem($003, 'Connected', False, True, Now);
  Items[3] := CreateDisplayItem($004, 'Also Pinned', True, True, Now);

  TDeviceSorter.Sort(Items);

  // Pinned items first (group 0), then connected (group 1), then disconnected (group 2)
  Assert.IsTrue(Items[0].IsPinned, 'First item should be pinned');
  Assert.IsTrue(Items[1].IsPinned, 'Second item should be pinned');
  Assert.IsFalse(Items[2].IsPinned, 'Third item should not be pinned');
  Assert.IsTrue(Items[2].Device.IsConnected, 'Third item should be connected');
  Assert.IsFalse(Items[3].Device.IsConnected, 'Fourth item should be disconnected');
end;

procedure TDeviceSorterTests.Sort_SameGroup_SortedByLastSeenThenName;
var
  Items: TDeviceDisplayItemArray;
  RecentTime, OlderTime: TDateTime;
begin
  RecentTime := IncHour(Now, -1);
  OlderTime := IncHour(Now, -5);

  SetLength(Items, 3);
  Items[0] := CreateDisplayItem($001, 'Zebra', False, False, OlderTime);
  Items[1] := CreateDisplayItem($002, 'Alpha', False, False, RecentTime);
  Items[2] := CreateDisplayItem($003, 'Beta', False, False, OlderTime);

  TDeviceSorter.Sort(Items);

  // First: Alpha (most recent)
  // Then: Beta (older, but alphabetically first)
  // Last: Zebra (older, alphabetically last)
  Assert.AreEqual('Alpha', Items[0].DisplayName);
  Assert.AreEqual('Beta', Items[1].DisplayName);
  Assert.AreEqual('Zebra', Items[2].DisplayName);
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceSorterTests);

end.
