{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TDeviceSorter Tests                             }
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
  App.DeviceDisplayTypes,
  App.DeviceSorter,
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
    function CreateDiscoveredItem(AAddress: UInt64; const ADisplayName: string;
      ALastSeen: TDateTime): TDeviceDisplayItem;
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

    { Edge Case Tests }
    [Test]
    procedure Sort_CaseInsensitive_VerifyMixedCase;

    [Test]
    procedure Sort_EqualLastSeen_SortsByName;

    [Test]
    procedure Sort_AllDevicesEqual_StableSort;

    [Test]
    procedure Compare_MaxInt64Address_NoOverflow;

    [Test]
    procedure Sort_SingleItem_NoChange_EdgeCase;

    [Test]
    procedure Sort_TwoIdenticalDevices_Stable;

    { Discovered Devices Sorting Tests }
    [Test]
    procedure Sort_DiscoveredDevices_NamedBeforeUnnamed;

    [Test]
    procedure Sort_DiscoveredDevices_BothNamed_SortByLastSeen;

    [Test]
    procedure Sort_DiscoveredDevices_BothUnnamed_SortByLastSeen;
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
    dsPaired,  // Test helper for paired devices
    AName,
    APinned,
    btAudioOutput,
    '',
    ALastSeen,
    SortGroup,
    TBatteryStatus.NotSupported,
    '',
    []  // Empty profiles array
  );
end;

function TDeviceSorterTests.CreateDiscoveredItem(AAddress: UInt64;
  const ADisplayName: string; ALastSeen: TDateTime): TDeviceDisplayItem;
var
  Device: TBluetoothDeviceInfo;
begin
  // Create unpaired, disconnected device (discovered state)
  Device := CreateTestDevice(AAddress, '', btUnknown, csDisconnected);

  // DisplayName will be used for sorting - can be real name or "Device XX:XX:XX..." pattern
  Result := TDeviceDisplayItem.Create(
    Device,
    dsDiscovered,  // Discovered/unpaired device
    ADisplayName,   // DisplayName - determines if "named" or "unnamed"
    False,          // Not pinned
    btUnknown,      // Unknown device type
    '',             // No status text
    ALastSeen,      // Last seen timestamp
    3,              // SortGroup 3 = Discovered
    TBatteryStatus.NotSupported,
    '',
    []  // Empty profiles array
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

{ TDeviceSorterTests - Edge Cases }

procedure TDeviceSorterTests.Sort_CaseInsensitive_VerifyMixedCase;
var
  Items: TDeviceDisplayItemArray;
  SameTime: TDateTime;
begin
  SameTime := Now;
  SetLength(Items, 4);
  Items[0] := CreateDisplayItem($001, 'ZEBRA', False, False, SameTime);
  Items[1] := CreateDisplayItem($002, 'alpha', False, False, SameTime);
  Items[2] := CreateDisplayItem($003, 'Beta', False, False, SameTime);
  Items[3] := CreateDisplayItem($004, 'ALPHA', False, False, SameTime);

  TDeviceSorter.Sort(Items);

  // Case-insensitive sort: alpha and ALPHA should be adjacent
  // Order should be: alpha, ALPHA, Beta, ZEBRA (or ALPHA, alpha depending on address)
  // CompareText is case-insensitive, so "alpha" = "ALPHA", then sorted by address
  Assert.AreEqual('alpha', Items[0].DisplayName);  // Lower address $002
  Assert.AreEqual('ALPHA', Items[1].DisplayName);  // Higher address $004
  Assert.AreEqual('Beta', Items[2].DisplayName);
  Assert.AreEqual('ZEBRA', Items[3].DisplayName);
end;

procedure TDeviceSorterTests.Sort_EqualLastSeen_SortsByName;
var
  Items: TDeviceDisplayItemArray;
  SameTime: TDateTime;
begin
  SameTime := Now;
  SetLength(Items, 3);
  Items[0] := CreateDisplayItem($003, 'Charlie', False, False, SameTime);
  Items[1] := CreateDisplayItem($001, 'Alpha', False, False, SameTime);
  Items[2] := CreateDisplayItem($002, 'Bravo', False, False, SameTime);

  TDeviceSorter.Sort(Items);

  // All have same LastSeen, should sort alphabetically by name
  Assert.AreEqual('Alpha', Items[0].DisplayName);
  Assert.AreEqual('Bravo', Items[1].DisplayName);
  Assert.AreEqual('Charlie', Items[2].DisplayName);
end;

procedure TDeviceSorterTests.Sort_AllDevicesEqual_StableSort;
var
  Items: TDeviceDisplayItemArray;
  SameTime: TDateTime;
  OriginalAddresses: array[0..2] of UInt64;
  I: Integer;
begin
  SameTime := Now;
  SetLength(Items, 3);
  // All devices have same name, same LastSeen, same group - differ only by address
  Items[0] := CreateDisplayItem($003, 'SameName', False, False, SameTime);
  Items[1] := CreateDisplayItem($001, 'SameName', False, False, SameTime);
  Items[2] := CreateDisplayItem($002, 'SameName', False, False, SameTime);

  // Record original addresses
  for I := 0 to 2 do
    OriginalAddresses[I] := Items[I].Device.AddressInt;

  TDeviceSorter.Sort(Items);

  // When all else is equal, should sort by address (ascending)
  Assert.AreEqual<UInt64>($001, Items[0].Device.AddressInt);
  Assert.AreEqual<UInt64>($002, Items[1].Device.AddressInt);
  Assert.AreEqual<UInt64>($003, Items[2].Device.AddressInt);
end;

procedure TDeviceSorterTests.Compare_MaxInt64Address_NoOverflow;
var
  LowAddr, HighAddr: TDeviceDisplayItem;
  SameTime: TDateTime;
  CompareResult: Integer;
begin
  SameTime := Now;
  // Use very large addresses close to UInt64 max to test for overflow
  LowAddr := CreateDisplayItem($0000000000000001, 'Same', False, False, SameTime);
  HighAddr := CreateDisplayItem($FFFFFFFFFFFF0000, 'Same', False, False, SameTime);

  // This should not overflow - compares using < and > operators, not subtraction
  CompareResult := TDeviceSorter.Compare(LowAddr, HighAddr);

  // Lower address should come first
  Assert.IsTrue(CompareResult < 0, 'Lower address should sort before higher address');

  // Reverse comparison
  CompareResult := TDeviceSorter.Compare(HighAddr, LowAddr);
  Assert.IsTrue(CompareResult > 0, 'Higher address should sort after lower address');
end;

procedure TDeviceSorterTests.Sort_SingleItem_NoChange_EdgeCase;
var
  Items: TDeviceDisplayItemArray;
  OriginalName: string;
  OriginalAddress: UInt64;
begin
  SetLength(Items, 1);
  Items[0] := CreateDisplayItem($123456789, 'Single Device', False, False, Now);
  OriginalName := Items[0].DisplayName;
  OriginalAddress := Items[0].Device.AddressInt;

  TDeviceSorter.Sort(Items);

  // Single item should remain unchanged
  Assert.AreEqual<Integer>(1, Length(Items));
  Assert.AreEqual(OriginalName, Items[0].DisplayName);
  Assert.AreEqual<UInt64>(OriginalAddress, Items[0].Device.AddressInt);
end;

procedure TDeviceSorterTests.Sort_TwoIdenticalDevices_Stable;
var
  Items: TDeviceDisplayItemArray;
  SameTime: TDateTime;
begin
  SameTime := Now;
  SetLength(Items, 2);
  // Two devices that are completely identical except for address
  Items[0] := CreateDisplayItem($002, 'IdenticalName', False, False, SameTime);
  Items[1] := CreateDisplayItem($001, 'IdenticalName', False, False, SameTime);

  TDeviceSorter.Sort(Items);

  // Should be stable - sorted by address when all else is equal
  Assert.AreEqual<UInt64>($001, Items[0].Device.AddressInt);
  Assert.AreEqual<UInt64>($002, Items[1].Device.AddressInt);
end;

{ TDeviceSorterTests - Discovered Devices Sorting }

procedure TDeviceSorterTests.Sort_DiscoveredDevices_NamedBeforeUnnamed;
var
  Items: TDeviceDisplayItemArray;
  SameTime: TDateTime;
begin
  SameTime := Now;
  SetLength(Items, 4);

  // Mix of named and unnamed discovered devices
  Items[0] := CreateDiscoveredItem($001, 'Device AA:BB:CC:DD:EE:01', SameTime);  // Unnamed (generic pattern)
  Items[1] := CreateDiscoveredItem($002, 'Sony Headphones', SameTime);            // Named
  Items[2] := CreateDiscoveredItem($003, 'Device AA:BB:CC:DD:EE:03', SameTime);  // Unnamed (generic pattern)
  Items[3] := CreateDiscoveredItem($004, 'Apple AirPods', SameTime);             // Named

  TDeviceSorter.Sort(Items);

  // Named devices should come before unnamed devices
  Assert.IsTrue(not Items[0].DisplayName.StartsWith('Device '), 'First item should be named');
  Assert.IsTrue(not Items[1].DisplayName.StartsWith('Device '), 'Second item should be named');
  Assert.IsTrue(Items[2].DisplayName.StartsWith('Device '), 'Third item should be unnamed');
  Assert.IsTrue(Items[3].DisplayName.StartsWith('Device '), 'Fourth item should be unnamed');

  // Named devices sorted alphabetically
  Assert.AreEqual('Apple AirPods', Items[0].DisplayName);
  Assert.AreEqual('Sony Headphones', Items[1].DisplayName);
end;

procedure TDeviceSorterTests.Sort_DiscoveredDevices_BothNamed_SortByLastSeen;
var
  Items: TDeviceDisplayItemArray;
  RecentTime, OlderTime: TDateTime;
begin
  RecentTime := IncHour(Now, -1);
  OlderTime := IncHour(Now, -5);

  SetLength(Items, 2);
  Items[0] := CreateDiscoveredItem($001, 'Older Device', OlderTime);   // Named, older
  Items[1] := CreateDiscoveredItem($002, 'Recent Device', RecentTime);  // Named, more recent

  TDeviceSorter.Sort(Items);

  // Within named discovered devices, more recent should come first
  Assert.AreEqual('Recent Device', Items[0].DisplayName);
  Assert.AreEqual('Older Device', Items[1].DisplayName);
end;

procedure TDeviceSorterTests.Sort_DiscoveredDevices_BothUnnamed_SortByLastSeen;
var
  Items: TDeviceDisplayItemArray;
  RecentTime, OlderTime: TDateTime;
begin
  RecentTime := IncHour(Now, -1);
  OlderTime := IncHour(Now, -5);

  SetLength(Items, 2);
  Items[0] := CreateDiscoveredItem($001, 'Device AA:BB:CC:DD:EE:01', OlderTime);   // Unnamed, older
  Items[1] := CreateDiscoveredItem($002, 'Device AA:BB:CC:DD:EE:02', RecentTime);  // Unnamed, more recent

  TDeviceSorter.Sort(Items);

  // Within unnamed discovered devices, more recent should come first
  Assert.AreEqual('Device AA:BB:CC:DD:EE:02', Items[0].DisplayName);
  Assert.AreEqual('Device AA:BB:CC:DD:EE:01', Items[1].DisplayName);
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceSorterTests);

end.
