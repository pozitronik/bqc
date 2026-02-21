{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API HTTP Server                            }
{                                                       }
{*******************************************************}

/// <summary>
/// Lightweight Indy HTTP server serving pre-built JSON snapshots.
/// Main thread updates snapshots; Indy worker threads read under lock.
/// </summary>
unit App.RestApiServer;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.JSON,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer,
  IdSocketHandle,
  App.DeviceDisplayTypes,
  App.RestApiSnapshot;

type
  TRestApiServer = class
  private
    FServer: TIdHTTPServer;
    FLock: TCriticalSection;
    FSnapshot: string;
    FStatusSnapshot: string;
    FItems: TDeviceDisplayItemArray;
    FAdapterAvailable: Boolean;
    FAdapterEnabled: Boolean;
    FRunning: Boolean;

    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure ServeJson(AResponseInfo: TIdHTTPResponseInfo;
      const AJson: string; ACode: Integer = 200);
    procedure ServeError(AResponseInfo: TIdHTTPResponseInfo;
      ACode: Integer; const AMessage: string);
    procedure RebuildSnapshots;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Starts the HTTP server. Raises exception on bind failure.
    /// </summary>
    procedure Start(APort: Integer; const ABindAddress: string);

    /// <summary>
    /// Stops the HTTP server.
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Whether the server is currently running.
    /// </summary>
    function IsRunning: Boolean;

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

uses
  App.Logger;

{ TRestApiServer }

constructor TRestApiServer.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := HandleRequest;
  FRunning := False;
  FAdapterAvailable := False;
  FAdapterEnabled := False;
  FSnapshot := '{"adapter":{"available":false,"enabled":false},"devices":[],"summary":{"totalDevices":0,"connectedDevices":0}}';
  FStatusSnapshot := '{"adapter":{"available":false,"enabled":false},"summary":{"totalDevices":0,"connectedDevices":0}}';
end;

destructor TRestApiServer.Destroy;
begin
  Stop;
  FServer.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRestApiServer.Start(APort: Integer; const ABindAddress: string);
var
  Binding: TIdSocketHandle;
begin
  if FRunning then
    Stop;

  FServer.Bindings.Clear;
  Binding := FServer.Bindings.Add;
  Binding.IP := ABindAddress;
  Binding.Port := APort;

  FServer.Active := True;
  FRunning := True;
  LogInfo('REST API server started on %s:%d', [ABindAddress, APort], ClassName);
end;

procedure TRestApiServer.Stop;
begin
  if not FRunning then
    Exit;
  try
    FServer.Active := False;
  except
    // Ignore shutdown errors
  end;
  FRunning := False;
  LogInfo('REST API server stopped', ClassName);
end;

function TRestApiServer.IsRunning: Boolean;
begin
  Result := FRunning;
end;

procedure TRestApiServer.UpdateDeviceSnapshot(const AItems: TDeviceDisplayItemArray);
begin
  FLock.Acquire;
  try
    FItems := Copy(AItems);
    RebuildSnapshots;
  finally
    FLock.Release;
  end;
end;

procedure TRestApiServer.UpdateAdapterState(AAvailable, AEnabled: Boolean);
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

procedure TRestApiServer.RebuildSnapshots;
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

procedure TRestApiServer.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Path, LocalSnapshot: string;
  DeviceIndex: Integer;
  DeviceJson: TJSONObject;
begin
  Path := ARequestInfo.Document;

  // CORS headers for web-based dashboards
  AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');

  // CORS preflight
  if SameText(ARequestInfo.Command, 'OPTIONS') then
  begin
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Methods', 'GET, OPTIONS');
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Headers', 'Content-Type');
    AResponseInfo.ResponseNo := 204;
    Exit;
  end;

  // GET /api/status
  if Path = '/api/status' then
  begin
    FLock.Acquire;
    try
      LocalSnapshot := FStatusSnapshot;
    finally
      FLock.Release;
    end;
    ServeJson(AResponseInfo, LocalSnapshot);
    Exit;
  end;

  // GET /api/devices
  if Path = '/api/devices' then
  begin
    FLock.Acquire;
    try
      LocalSnapshot := FSnapshot;
    finally
      FLock.Release;
    end;
    ServeJson(AResponseInfo, LocalSnapshot);
    Exit;
  end;

  // GET /api/devices/{address}
  if Path.StartsWith('/api/devices/') and (Length(Path) > Length('/api/devices/')) then
  begin
    var AddressStr := Copy(Path, Length('/api/devices/') + 1, MaxInt);
    FLock.Acquire;
    try
      DeviceIndex := TRestApiSnapshotBuilder.FindDeviceByAddress(FItems, AddressStr);
      if DeviceIndex >= 0 then
      begin
        DeviceJson := TRestApiSnapshotBuilder.BuildDeviceJson(FItems[DeviceIndex]);
        try
          LocalSnapshot := DeviceJson.ToJSON;
        finally
          DeviceJson.Free;
        end;
      end
      else
        LocalSnapshot := '';
    finally
      FLock.Release;
    end;

    if LocalSnapshot <> '' then
      ServeJson(AResponseInfo, LocalSnapshot)
    else
      ServeError(AResponseInfo, 404, 'Device not found');
    Exit;
  end;

  // Everything else: 404
  ServeError(AResponseInfo, 404, 'Not found');
end;

procedure TRestApiServer.ServeJson(AResponseInfo: TIdHTTPResponseInfo;
  const AJson: string; ACode: Integer);
begin
  AResponseInfo.ResponseNo := ACode;
  AResponseInfo.ContentType := 'application/json';
  AResponseInfo.CharSet := 'utf-8';
  AResponseInfo.ContentText := AJson;
end;

procedure TRestApiServer.ServeError(AResponseInfo: TIdHTTPResponseInfo;
  ACode: Integer; const AMessage: string);
var
  ErrorJson: TJSONObject;
begin
  ErrorJson := TJSONObject.Create;
  try
    ErrorJson.AddPair('error', AMessage);
    ErrorJson.AddPair('code', TJSONNumber.Create(ACode));
    ServeJson(AResponseInfo, ErrorJson.ToJSON, ACode);
  finally
    ErrorJson.Free;
  end;
end;

end.
