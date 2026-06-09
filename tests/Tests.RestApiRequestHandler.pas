{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       REST API Request Handler Tests                  }
{                                                       }
{*******************************************************}

unit Tests.RestApiRequestHandler;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.JSON,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  App.RestApiRequestHandler;

type
  [TestFixture]
  TRestApiRequestHandlerTests = class
  private
    FHandler: TRestApiRequestHandler;

    function MakeDevice(AAddressInt: UInt64; const AName: string;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
    function MakeDisplayItem(const ADevice: TBluetoothDeviceInfo;
      const ADisplayName: string;
      ASource: TDeviceSource = dsPaired): TDeviceDisplayItem;
    /// <summary>Parses a JSON response body; fails the test on invalid JSON.</summary>
    function ParseBody(const AResponse: TRestApiResponse): TJSONObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Routing: defaults }
    [Test]
    procedure Status_Default_ReturnsUnavailableAdapterAndZeroCounts;
    [Test]
    procedure Devices_Default_ReturnsEmptyDeviceList;
    [Test]
    procedure Status_Route_ReturnsStatusSnapshotWithoutDevicesArray;

    { State updates }
    [Test]
    procedure UpdateAdapterState_ReflectedInStatus;
    [Test]
    procedure UpdateDeviceSnapshot_ReflectedInDevices;
    [Test]
    procedure UpdateDeviceSnapshot_SummaryCountsOnlyConnected;
    [Test]
    procedure ActionItems_ExcludedFromSummaryCounts;
    [Test]
    procedure UpdateDeviceSnapshot_TakesCopy_LaterCallerMutationInvisible;

    { Single device endpoint }
    [Test]
    procedure DeviceByAddress_MacFormat_ReturnsDevice;
    [Test]
    procedure DeviceByAddress_HexFormat_ReturnsDevice;
    [Test]
    procedure DeviceByAddress_Unknown_Returns404WithErrorBody;
    [Test]
    procedure DeviceByAddress_EmptyAddress_ReturnsGeneric404;

    { Negative paths }
    [Test]
    procedure UnknownPath_Returns404WithJsonErrorBody;
    [Test]
    procedure PathMatching_IsCaseSensitive;

    { CORS preflight }
    [Test]
    procedure Options_Returns204PreflightWithoutBody;
  end;

implementation

const
  ADDR_HEADPHONES = UInt64($AABBCCDDEEFF);
  ADDR_MOUSE = UInt64($112233445566);

{ Helper methods }

function TRestApiRequestHandlerTests.MakeDevice(AAddressInt: UInt64;
  const AName: string; AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddressInt), AAddressInt, AName,
    btAudioOutput, AState, True, False, 0, Now, 0);
end;

function TRestApiRequestHandlerTests.MakeDisplayItem(
  const ADevice: TBluetoothDeviceInfo; const ADisplayName: string;
  ASource: TDeviceSource): TDeviceDisplayItem;
begin
  Result := TDeviceDisplayItem.Create(
    ADevice, ASource, ADisplayName, False,
    ADevice.DeviceType, '', Now, 0,
    TBatteryStatus.NotSupported, '', nil, False, '');
end;

function TRestApiRequestHandlerTests.ParseBody(
  const AResponse: TRestApiResponse): TJSONObject;
var
  Root: TJSONValue;
begin
  Root := TJSONObject.ParseJSONValue(AResponse.Body);
  Assert.IsNotNull(Root, 'Response body must be valid JSON: ' + AResponse.Body);
  Assert.IsTrue(Root is TJSONObject, 'Response root must be a JSON object');
  Result := Root as TJSONObject;
end;

{ Setup / TearDown }

procedure TRestApiRequestHandlerTests.Setup;
begin
  FHandler := TRestApiRequestHandler.Create;
end;

procedure TRestApiRequestHandlerTests.TearDown;
begin
  FreeAndNil(FHandler);
end;

{ Routing: defaults }

procedure TRestApiRequestHandlerTests.Status_Default_ReturnsUnavailableAdapterAndZeroCounts;
var
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  Response := FHandler.HandleRequest('GET', '/api/status');

  Assert.AreEqual(200, Response.StatusCode);
  Assert.AreEqual('application/json', Response.ContentType);
  Assert.IsFalse(Response.IsPreflight);

  Root := ParseBody(Response);
  try
    Assert.IsFalse(Root.GetValue<Boolean>('adapter.available'));
    Assert.IsFalse(Root.GetValue<Boolean>('adapter.enabled'));
    Assert.AreEqual(0, Root.GetValue<Integer>('summary.totalDevices'));
    Assert.AreEqual(0, Root.GetValue<Integer>('summary.connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.Devices_Default_ReturnsEmptyDeviceList;
var
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  Response := FHandler.HandleRequest('GET', '/api/devices');

  Assert.AreEqual(200, Response.StatusCode);
  Root := ParseBody(Response);
  try
    Assert.AreEqual(0, (Root.GetValue('devices') as TJSONArray).Count);
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.Status_Route_ReturnsStatusSnapshotWithoutDevicesArray;
var
  Root: TJSONObject;
begin
  Root := ParseBody(FHandler.HandleRequest('GET', '/api/status'));
  try
    Assert.IsNull(Root.GetValue('devices'),
      '/api/status must serve the status snapshot, not the full one');
  finally
    Root.Free;
  end;
end;

{ State updates }

procedure TRestApiRequestHandlerTests.UpdateAdapterState_ReflectedInStatus;
var
  Root: TJSONObject;
begin
  FHandler.UpdateAdapterState(True, True);

  Root := ParseBody(FHandler.HandleRequest('GET', '/api/status'));
  try
    Assert.IsTrue(Root.GetValue<Boolean>('adapter.available'));
    Assert.IsTrue(Root.GetValue<Boolean>('adapter.enabled'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.UpdateDeviceSnapshot_ReflectedInDevices;
var
  Items: TDeviceDisplayItemArray;
  Root: TJSONObject;
  Devices: TJSONArray;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'My Headphones');
  FHandler.UpdateDeviceSnapshot(Items);

  Root := ParseBody(FHandler.HandleRequest('GET', '/api/devices'));
  try
    Devices := Root.GetValue('devices') as TJSONArray;
    Assert.AreEqual(1, Devices.Count);
    Assert.AreEqual('My Headphones',
      (Devices.Items[0] as TJSONObject).GetValue<string>('displayName'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.UpdateDeviceSnapshot_SummaryCountsOnlyConnected;
var
  Items: TDeviceDisplayItemArray;
  Root: TJSONObject;
begin
  SetLength(Items, 2);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'Headphones');
  Items[1] := MakeDisplayItem(
    MakeDevice(ADDR_MOUSE, 'Mouse', csDisconnected), 'Mouse');
  FHandler.UpdateDeviceSnapshot(Items);

  Root := ParseBody(FHandler.HandleRequest('GET', '/api/status'));
  try
    Assert.AreEqual(2, Root.GetValue<Integer>('summary.totalDevices'));
    Assert.AreEqual(1, Root.GetValue<Integer>('summary.connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.ActionItems_ExcludedFromSummaryCounts;
var
  Items: TDeviceDisplayItemArray;
  Root: TJSONObject;
begin
  SetLength(Items, 2);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'Headphones');
  Items[1] := MakeDisplayItem(
    MakeDevice(0, 'Scan', csDisconnected), 'Scan for devices', dsAction);
  FHandler.UpdateDeviceSnapshot(Items);

  Root := ParseBody(FHandler.HandleRequest('GET', '/api/status'));
  try
    Assert.AreEqual(1, Root.GetValue<Integer>('summary.totalDevices'));
    Assert.AreEqual(1, Root.GetValue<Integer>('summary.connectedDevices'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.UpdateDeviceSnapshot_TakesCopy_LaterCallerMutationInvisible;
var
  Items: TDeviceDisplayItemArray;
  Root: TJSONObject;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'Original');
  FHandler.UpdateDeviceSnapshot(Items);

  // Mutating the caller's array after the update must not leak into responses
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'Mutated');

  Root := ParseBody(FHandler.HandleRequest('GET', '/api/devices/AA:BB:CC:DD:EE:FF'));
  try
    Assert.AreEqual('Original', Root.GetValue<string>('displayName'));
  finally
    Root.Free;
  end;
end;

{ Single device endpoint }

procedure TRestApiRequestHandlerTests.DeviceByAddress_MacFormat_ReturnsDevice;
var
  Items: TDeviceDisplayItemArray;
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'My Headphones');
  FHandler.UpdateDeviceSnapshot(Items);

  Response := FHandler.HandleRequest('GET', '/api/devices/AA:BB:CC:DD:EE:FF');

  Assert.AreEqual(200, Response.StatusCode);
  Root := ParseBody(Response);
  try
    Assert.AreEqual('My Headphones', Root.GetValue<string>('displayName'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.DeviceByAddress_HexFormat_ReturnsDevice;
var
  Items: TDeviceDisplayItemArray;
  Response: TRestApiResponse;
begin
  SetLength(Items, 1);
  Items[0] := MakeDisplayItem(
    MakeDevice(ADDR_HEADPHONES, 'Headphones', csConnected), 'My Headphones');
  FHandler.UpdateDeviceSnapshot(Items);

  Response := FHandler.HandleRequest('GET', '/api/devices/AABBCCDDEEFF');

  Assert.AreEqual(200, Response.StatusCode);
end;

procedure TRestApiRequestHandlerTests.DeviceByAddress_Unknown_Returns404WithErrorBody;
var
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  Response := FHandler.HandleRequest('GET', '/api/devices/00:00:00:00:00:01');

  Assert.AreEqual(404, Response.StatusCode);
  Assert.AreEqual('application/json', Response.ContentType);
  Root := ParseBody(Response);
  try
    Assert.AreEqual('Device not found', Root.GetValue<string>('error'));
    Assert.AreEqual(404, Root.GetValue<Integer>('code'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.DeviceByAddress_EmptyAddress_ReturnsGeneric404;
var
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  // '/api/devices/' has no address part and must not match the device route
  Response := FHandler.HandleRequest('GET', '/api/devices/');

  Assert.AreEqual(404, Response.StatusCode);
  Root := ParseBody(Response);
  try
    Assert.AreEqual('Not found', Root.GetValue<string>('error'));
  finally
    Root.Free;
  end;
end;

{ Negative paths }

procedure TRestApiRequestHandlerTests.UnknownPath_Returns404WithJsonErrorBody;
var
  Response: TRestApiResponse;
  Root: TJSONObject;
begin
  Response := FHandler.HandleRequest('GET', '/api/nonsense');

  Assert.AreEqual(404, Response.StatusCode);
  Assert.AreEqual('application/json', Response.ContentType);
  Root := ParseBody(Response);
  try
    Assert.AreEqual('Not found', Root.GetValue<string>('error'));
    Assert.AreEqual(404, Root.GetValue<Integer>('code'));
  finally
    Root.Free;
  end;
end;

procedure TRestApiRequestHandlerTests.PathMatching_IsCaseSensitive;
begin
  // Pins current behavior: URL paths are matched case-sensitively
  Assert.AreEqual(404, FHandler.HandleRequest('GET', '/API/status').StatusCode);
  Assert.AreEqual(404, FHandler.HandleRequest('GET', '/Api/Devices').StatusCode);
end;

{ CORS preflight }

procedure TRestApiRequestHandlerTests.Options_Returns204PreflightWithoutBody;
var
  Response: TRestApiResponse;
begin
  Response := FHandler.HandleRequest('OPTIONS', '/api/devices');

  Assert.AreEqual(204, Response.StatusCode);
  Assert.IsTrue(Response.IsPreflight, 'Transport must add CORS preflight headers');
  Assert.AreEqual('', Response.ContentType, 'Preflight must have no body');
  Assert.AreEqual('', Response.Body);
end;

initialization
  TDUnitX.RegisterTestFixture(TRestApiRequestHandlerTests);

end.
