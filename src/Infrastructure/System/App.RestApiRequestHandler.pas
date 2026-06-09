{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API Request Handler                        }
{                                                       }
{*******************************************************}

/// <summary>
/// Transport-free REST API core: routing, snapshot state and response
/// building, extracted from the Indy server so it can be unit-tested.
/// Main thread updates snapshots; HTTP worker threads read under lock.
/// </summary>
unit App.RestApiRequestHandler;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.JSON,
  App.DeviceDisplayTypes,
  App.RestApiSnapshot;

type
  /// <summary>
  /// Transport-agnostic HTTP response produced by the handler.
  /// </summary>
  TRestApiResponse = record
    StatusCode: Integer;
    /// <summary>Response content type; empty means no body (e.g. 204).</summary>
    ContentType: string;
    Body: string;
    /// <summary>True for CORS preflight: transport must add Allow-Methods/Allow-Headers.</summary>
    IsPreflight: Boolean;
  end;

  /// <summary>
  /// Owns API state (device items, adapter state, pre-built JSON snapshots)
  /// and maps method+path to a response. Thread-safe: updates and reads
  /// synchronize on an internal lock.
  /// </summary>
  TRestApiRequestHandler = class
  private
    FLock: TCriticalSection;
    FSnapshot: string;
    FStatusSnapshot: string;
    FItems: TDeviceDisplayItemArray;
    FAdapterAvailable: Boolean;
    FAdapterEnabled: Boolean;

    /// <summary>Rebuilds both JSON snapshots. Caller must hold FLock.</summary>
    procedure RebuildSnapshots;
    class function JsonResponse(const ABody: string;
      ACode: Integer = 200): TRestApiResponse; static;
    class function ErrorResponse(ACode: Integer;
      const AMessage: string): TRestApiResponse; static;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Routes a request to a response. Any method other than OPTIONS is
    /// served as a read (matches previous server behavior where Indy
    /// dispatched GET/POST/HEAD to the same handler).
    /// </summary>
    /// <param name="AMethod">HTTP method (e.g. 'GET', 'OPTIONS').</param>
    /// <param name="APath">Request path (document part, no query string).</param>
    /// <returns>Response record; Body is empty when ContentType is empty.</returns>
    function HandleRequest(const AMethod, APath: string): TRestApiResponse;

    /// <summary>
    /// Updates device snapshot. Called from main thread on device list changes.
    /// </summary>
    procedure UpdateDeviceSnapshot(const AItems: TDeviceDisplayItemArray);

    /// <summary>
    /// Updates adapter state. Called from main thread on toggle changes.
    /// </summary>
    procedure UpdateAdapterState(AAvailable, AEnabled: Boolean);
  end;

implementation

{ TRestApiRequestHandler }

constructor TRestApiRequestHandler.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FAdapterAvailable := False;
  FAdapterEnabled := False;
  FSnapshot := '{"adapter":{"available":false,"enabled":false},"devices":[],"summary":{"totalDevices":0,"connectedDevices":0}}';
  FStatusSnapshot := '{"adapter":{"available":false,"enabled":false},"summary":{"totalDevices":0,"connectedDevices":0}}';
end;

destructor TRestApiRequestHandler.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TRestApiRequestHandler.UpdateDeviceSnapshot(const AItems: TDeviceDisplayItemArray);
begin
  FLock.Acquire;
  try
    FItems := Copy(AItems);
    RebuildSnapshots;
  finally
    FLock.Release;
  end;
end;

procedure TRestApiRequestHandler.UpdateAdapterState(AAvailable, AEnabled: Boolean);
begin
  FLock.Acquire;
  try
    FAdapterAvailable := AAvailable;
    FAdapterEnabled := AEnabled;
    RebuildSnapshots;
  finally
    FLock.Release;
  end;
end;

procedure TRestApiRequestHandler.RebuildSnapshots;
var
  ConnectedCount, TotalCount, I: Integer;
begin
  // Count non-action devices
  TotalCount := 0;
  ConnectedCount := 0;
  for I := 0 to High(FItems) do
  begin
    if FItems[I].Source = dsAction then
      Continue;
    Inc(TotalCount);
    if FItems[I].Device.IsConnected then
      Inc(ConnectedCount);
  end;

  FSnapshot := TRestApiSnapshotBuilder.BuildFullSnapshot(
    FItems, FAdapterAvailable, FAdapterEnabled);
  FStatusSnapshot := TRestApiSnapshotBuilder.BuildStatusSnapshot(
    FAdapterAvailable, FAdapterEnabled, TotalCount, ConnectedCount);
end;

function TRestApiRequestHandler.HandleRequest(const AMethod, APath: string): TRestApiResponse;
var
  LocalSnapshot: string;
  DeviceIndex: Integer;
  LocalItem: TDeviceDisplayItem;
  DeviceJson: TJSONObject;
begin
  // CORS preflight
  if SameText(AMethod, 'OPTIONS') then
  begin
    Result := Default(TRestApiResponse);
    Result.StatusCode := 204;
    Result.IsPreflight := True;
    Exit;
  end;

  // GET /api/status
  if APath = '/api/status' then
  begin
    FLock.Acquire;
    try
      LocalSnapshot := FStatusSnapshot;
    finally
      FLock.Release;
    end;
    Exit(JsonResponse(LocalSnapshot));
  end;

  // GET /api/devices
  if APath = '/api/devices' then
  begin
    FLock.Acquire;
    try
      LocalSnapshot := FSnapshot;
    finally
      FLock.Release;
    end;
    Exit(JsonResponse(LocalSnapshot));
  end;

  // GET /api/devices/{address}
  if APath.StartsWith('/api/devices/') and (Length(APath) > Length('/api/devices/')) then
  begin
    var AddressStr := Copy(APath, Length('/api/devices/') + 1, MaxInt);

    // Copy record under lock (consistent with other endpoints' copy-under-lock pattern)
    FLock.Acquire;
    try
      DeviceIndex := TRestApiSnapshotBuilder.FindDeviceByAddress(FItems, AddressStr);
      if DeviceIndex >= 0 then
        LocalItem := FItems[DeviceIndex];
    finally
      FLock.Release;
    end;

    if DeviceIndex >= 0 then
    begin
      DeviceJson := TRestApiSnapshotBuilder.BuildDeviceJson(LocalItem);
      try
        Exit(JsonResponse(DeviceJson.ToJSON));
      finally
        DeviceJson.Free;
      end;
    end;
    Exit(ErrorResponse(404, 'Device not found'));
  end;

  // Everything else: 404
  Result := ErrorResponse(404, 'Not found');
end;

class function TRestApiRequestHandler.JsonResponse(const ABody: string;
  ACode: Integer): TRestApiResponse;
begin
  Result := Default(TRestApiResponse);
  Result.StatusCode := ACode;
  Result.ContentType := 'application/json';
  Result.Body := ABody;
end;

class function TRestApiRequestHandler.ErrorResponse(ACode: Integer;
  const AMessage: string): TRestApiResponse;
var
  ErrorJson: TJSONObject;
begin
  ErrorJson := TJSONObject.Create;
  try
    ErrorJson.AddPair('error', AMessage);
    ErrorJson.AddPair('code', TJSONNumber.Create(ACode));
    Result := JsonResponse(ErrorJson.ToJSON, ACode);
  finally
    ErrorJson.Free;
  end;
end;

end.
