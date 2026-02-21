{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       REST API Snapshot Builder Tests                 }
{                                                       }
{*******************************************************}

unit Tests.RestApiSnapshot;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  App.RestApiSnapshot;

type
  [TestFixture]
  TTestRestApiSnapshot = class
  private
    function MakeDevice(AAddressInt: UInt64; const AName: string;
      AType: TBluetoothDeviceType; AState: TBluetoothConnectionState;
      AIsPaired: Boolean): TBluetoothDeviceInfo;
    function MakeDisplayItem(const ADevice: TBluetoothDeviceInfo;
      const ADisplayName: string; AIsPinned: Boolean;
      ABattery: TBatteryStatus; const ABatteryText: string;
      ALastSeen: TDateTime): TDeviceDisplayItem;
  public
    [Test]
    procedure EmptyDeviceList_ProducesValidJson;

    [Test]
    procedure SingleConnectedDevice_IncludesBattery;

    [Test]
    procedure MultipleDevices_CorrectCounts;

    [Test]
    procedure AdapterState_Serialized;

    [Test]
    procedure FindDeviceByAddress_MACFormat;

    [Test]
    procedure FindDeviceByAddress_HexFormat;

    [Test]
    procedure FindDeviceByAddress_DashFormat;

    [Test]
    procedure FindDeviceByAddress_NotFound;

    [Test]
    procedure FindDeviceByAddress_InvalidFormat;

    [Test]
    procedure DeviceTypes_SerializedAsStrings;

    [Test]
    procedure ConnectionStates_SerializedAsStrings;

    [Test]
    procedure BatteryNotSupported_LevelIsNull;

    [Test]
    procedure BatteryPending_LevelIsNull;

    [Test]
    procedure StatusSnapshot_HasNoDevicesArray;

    [Test]
    procedure ActionItems_ExcludedFromSnapshot;

    [Test]
    procedure Profiles_SerializedAsShortNames;

    [Test]
    procedure LastSeen_ZeroIsNull;
  end;

implementation

uses
  System.DateUtils;

{ Helper methods }

function TTestRestApiSnapshot.MakeDevice(AAddressInt: UInt64;
  const AName: string; AType: TBluetoothDeviceType;
  AState: TBluetoothConnectionState;
  AIsPaired: Boolean): TBluetoothDeviceInfo;
var
  Addr: TBluetoothAddress;
begin
  Addr := UInt64ToBluetoothAddress(AAddressInt);
  Result := TBluetoothDeviceInfo.Create(
    Addr, AAddressInt, AName, AType, AState,
    AIsPaired, False, 0, Now, 0);
end;

function TTestRestApiSnapshot.MakeDisplayItem(
  const ADevice: TBluetoothDeviceInfo;
  const ADisplayName: string; AIsPinned: Boolean;
  ABattery: TBatteryStatus; const ABatteryText: string;
  ALastSeen: TDateTime): TDeviceDisplayItem;
begin
  Result := TDeviceDisplayItem.Create(
    ADevice, dsPaired, ADisplayName, AIsPinned,
    ADevice.DeviceType, '', ALastSeen, 0,
    ABattery, ABatteryText, nil, False, '');
end;

{ Tests }

procedure TTestRestApiSnapshot.EmptyDeviceList_ProducesValidJson;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 0);
  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);

  Root := TJSONObject.ParseJSONValue(Json);
  try
    Assert.IsNotNull(Root, 'Should produce valid JSON');
    Assert.IsNotNull((Root as TJSONObject).GetValue('adapter'));
    Assert.IsNotNull((Root as TJSONObject).GetValue('devices'));
    Assert.IsNotNull((Root as TJSONObject).GetValue('summary'));

    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    Assert.AreEqual(0, Devices.Count);

    var Summary := (Root as TJSONObject).GetValue('summary') as TJSONObject;
    Assert.AreEqual(0, Summary.GetValue<Integer>('totalDevices'));
    Assert.AreEqual(0, Summary.GetValue<Integer>('connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.SingleConnectedDevice_IncludesBattery;
var
  Items: TDeviceDisplayItemArray;
  Device: TBluetoothDeviceInfo;
  Json: string;
  Root: TJSONValue;
begin
  Device := MakeDevice($AABBCCDDEEFF, 'Headphones', btAudioOutput, csConnected, True);
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(Device, 'My Headphones', True,
    TBatteryStatus.Create(85), '85%', Now);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    Assert.AreEqual(1, Devices.Count);

    var DevObj := Devices.Items[0] as TJSONObject;
    Assert.AreEqual('My Headphones', DevObj.GetValue<string>('displayName'));
    Assert.IsTrue(DevObj.GetValue<Boolean>('isConnected'));
    Assert.IsTrue(DevObj.GetValue<Boolean>('isPinned'));

    var Battery := DevObj.GetValue('battery') as TJSONObject;
    Assert.AreEqual(85, Battery.GetValue<Integer>('level'));
    Assert.IsTrue(Battery.GetValue<Boolean>('supported'));
    Assert.AreEqual('85%', Battery.GetValue<string>('text'));

    var Summary := (Root as TJSONObject).GetValue('summary') as TJSONObject;
    Assert.AreEqual(1, Summary.GetValue<Integer>('connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.MultipleDevices_CorrectCounts;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 3);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev1', btAudioOutput, csConnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', Now);
  Items[1] := MakeDisplayItem(
    MakeDevice(2, 'Dev2', btKeyboard, csDisconnected, True),
    'Dev2', False, TBatteryStatus.NotSupported, '', Now);
  Items[2] := MakeDisplayItem(
    MakeDevice(3, 'Dev3', btMouse, csConnected, True),
    'Dev3', False, TBatteryStatus.NotSupported, '', Now);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Summary := (Root as TJSONObject).GetValue('summary') as TJSONObject;
    Assert.AreEqual(3, Summary.GetValue<Integer>('totalDevices'));
    Assert.AreEqual(2, Summary.GetValue<Integer>('connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.AdapterState_Serialized;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 0);

  // Adapter available but disabled
  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, False);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Adapter := (Root as TJSONObject).GetValue('adapter') as TJSONObject;
    Assert.IsTrue(Adapter.GetValue<Boolean>('available'));
    Assert.IsFalse(Adapter.GetValue<Boolean>('enabled'));
  finally
    Root.Free;
  end;

  // Adapter not available
  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, False, False);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Adapter := (Root as TJSONObject).GetValue('adapter') as TJSONObject;
    Assert.IsFalse(Adapter.GetValue<Boolean>('available'));
    Assert.IsFalse(Adapter.GetValue<Boolean>('enabled'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.FindDeviceByAddress_MACFormat;
var
  Items: TDeviceDisplayItemArray;
  Idx: Integer;
begin
  SetLength(Items, 2);
  Items[0] := MakeDisplayItem(
    MakeDevice($AABBCCDDEEFF, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);
  Items[1] := MakeDisplayItem(
    MakeDevice($112233445566, 'Dev2', btUnknown, csDisconnected, True),
    'Dev2', False, TBatteryStatus.NotSupported, '', 0);

  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, 'AA:BB:CC:DD:EE:FF');
  Assert.AreEqual(0, Idx);

  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, '11:22:33:44:55:66');
  Assert.AreEqual(1, Idx);
end;

procedure TTestRestApiSnapshot.FindDeviceByAddress_HexFormat;
var
  Items: TDeviceDisplayItemArray;
  Idx: Integer;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice($AABBCCDDEEFF, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);

  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, 'AABBCCDDEEFF');
  Assert.AreEqual(0, Idx);
end;

procedure TTestRestApiSnapshot.FindDeviceByAddress_DashFormat;
var
  Items: TDeviceDisplayItemArray;
  Idx: Integer;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice($AABBCCDDEEFF, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);

  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, 'AA-BB-CC-DD-EE-FF');
  Assert.AreEqual(0, Idx);
end;

procedure TTestRestApiSnapshot.FindDeviceByAddress_NotFound;
var
  Items: TDeviceDisplayItemArray;
  Idx: Integer;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice($AABBCCDDEEFF, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);

  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, '00:00:00:00:00:01');
  Assert.AreEqual(-1, Idx);
end;

procedure TTestRestApiSnapshot.FindDeviceByAddress_InvalidFormat;
var
  Items: TDeviceDisplayItemArray;
  Idx: Integer;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice($AABBCCDDEEFF, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);

  // Too short
  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, 'AABB');
  Assert.AreEqual(-1, Idx);

  // Invalid chars
  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, 'GGHHIIJJKKLL');
  Assert.AreEqual(-1, Idx);

  // Empty
  Idx := TRestApiSnapshotBuilder.FindDeviceByAddress(Items, '');
  Assert.AreEqual(-1, Idx);
end;

procedure TTestRestApiSnapshot.DeviceTypes_SerializedAsStrings;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btKeyboard, csDisconnected, True),
    'Dev', False, TBatteryStatus.NotSupported, '', 0);
  // Override effective type
  Items[0].EffectiveDeviceType := btKeyboard;

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var DevObj := Devices.Items[0] as TJSONObject;
    Assert.AreEqual('Keyboard', DevObj.GetValue<string>('type'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.ConnectionStates_SerializedAsStrings;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btUnknown, csConnecting, True),
    'Dev', False, TBatteryStatus.NotSupported, '', 0);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var DevObj := Devices.Items[0] as TJSONObject;
    Assert.AreEqual('Connecting', DevObj.GetValue<string>('connectionState'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.BatteryNotSupported_LevelIsNull;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btUnknown, csDisconnected, True),
    'Dev', False, TBatteryStatus.NotSupported, '', 0);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var Battery := (Devices.Items[0] as TJSONObject).GetValue('battery') as TJSONObject;
    Assert.IsTrue(Battery.GetValue('level') is TJSONNull, 'level should be null');
    Assert.IsFalse(Battery.GetValue<Boolean>('supported'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.BatteryPending_LevelIsNull;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btUnknown, csConnected, True),
    'Dev', False, TBatteryStatus.Pending, '...', 0);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var Battery := (Devices.Items[0] as TJSONObject).GetValue('battery') as TJSONObject;
    // Pending has no valid level, should be null
    Assert.IsTrue(Battery.GetValue('level') is TJSONNull, 'Pending level should be null');
    Assert.IsTrue(Battery.GetValue<Boolean>('supported'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.StatusSnapshot_HasNoDevicesArray;
var
  Json: string;
  Root: TJSONValue;
begin
  Json := TRestApiSnapshotBuilder.BuildStatusSnapshot(True, True, 5, 2);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    Assert.IsNull((Root as TJSONObject).GetValue('devices'),
      'Status snapshot should not contain devices array');
    Assert.IsNotNull((Root as TJSONObject).GetValue('adapter'));
    Assert.IsNotNull((Root as TJSONObject).GetValue('summary'));

    var Summary := (Root as TJSONObject).GetValue('summary') as TJSONObject;
    Assert.AreEqual(5, Summary.GetValue<Integer>('totalDevices'));
    Assert.AreEqual(2, Summary.GetValue<Integer>('connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.ActionItems_ExcludedFromSnapshot;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 2);
  // Normal device
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev1', btUnknown, csDisconnected, True),
    'Dev1', False, TBatteryStatus.NotSupported, '', 0);
  // Action item (scan button)
  Items[1] := TDeviceDisplayItem.Create(
    MakeDevice(0, 'Scan for devices', btUnknown, csDisconnected, False),
    dsAction, 'Scan for devices', False, btUnknown, '', 0, 99,
    TBatteryStatus.NotSupported, '', nil, False, '');

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    Assert.AreEqual(1, Devices.Count, 'Action items should be excluded');

    var Summary := (Root as TJSONObject).GetValue('summary') as TJSONObject;
    Assert.AreEqual(1, Summary.GetValue<Integer>('totalDevices'));
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.Profiles_SerializedAsShortNames;
var
  Items: TDeviceDisplayItemArray;
  Profiles: TBluetoothProfileArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Profiles, 2);
  Profiles[0] := TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsConnected);
  Profiles[1] := TBluetoothProfile.Create(bptHFP, TGUID.Empty, pcsConnected);

  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btAudioOutput, csConnected, True),
    'Dev', False, TBatteryStatus.NotSupported, '', Now);
  Items[0].Profiles := Profiles;

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var ProfileArr := (Devices.Items[0] as TJSONObject).GetValue('profiles') as TJSONArray;
    Assert.AreEqual(2, ProfileArr.Count);
    Assert.AreEqual('A2DP', ProfileArr.Items[0].Value);
    Assert.AreEqual('HFP', ProfileArr.Items[1].Value);
  finally
    Root.Free;
  end;
end;

procedure TTestRestApiSnapshot.LastSeen_ZeroIsNull;
var
  Items: TDeviceDisplayItemArray;
  Json: string;
  Root: TJSONValue;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(1, 'Dev', btUnknown, csDisconnected, True),
    'Dev', False, TBatteryStatus.NotSupported, '', 0);

  Json := TRestApiSnapshotBuilder.BuildFullSnapshot(Items, True, True);
  Root := TJSONObject.ParseJSONValue(Json);
  try
    var Devices := (Root as TJSONObject).GetValue('devices') as TJSONArray;
    var DevObj := Devices.Items[0] as TJSONObject;
    Assert.IsTrue(DevObj.GetValue('lastSeen') is TJSONNull,
      'Zero lastSeen should be null');
  finally
    Root.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRestApiSnapshot);

end.
