{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API HTTP Server                            }
{                                                       }
{*******************************************************}

/// <summary>
/// Thin Indy transport shell around TRestApiRequestHandler.
/// All routing, state and response building live in the handler
/// (App.RestApiRequestHandler) so they stay unit-testable.
/// </summary>
unit App.RestApiServer;

interface

uses
  System.SysUtils,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer,
  IdSocketHandle,
  App.DeviceDisplayTypes,
  App.RestApiRequestHandler;

type
  TRestApiServer = class
  private
    FServer: TIdHTTPServer;
    FHandler: TRestApiRequestHandler;
    FRunning: Boolean;

    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
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
  FHandler := TRestApiRequestHandler.Create;
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := HandleCommand;
  // Indy routes only GET/POST/HEAD to OnCommandGet; OPTIONS (CORS preflight)
  // arrives via OnCommandOther and must be wired explicitly
  FServer.OnCommandOther := HandleCommand;
  FRunning := False;
end;

destructor TRestApiServer.Destroy;
begin
  Stop;
  FServer.Free;
  FHandler.Free;
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
  FHandler.UpdateDeviceSnapshot(AItems);
end;

procedure TRestApiServer.UpdateAdapterState(AAvailable, AEnabled: Boolean);
begin
  FHandler.UpdateAdapterState(AAvailable, AEnabled);
end;

procedure TRestApiServer.HandleCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Response: TRestApiResponse;
begin
  Response := FHandler.HandleRequest(ARequestInfo.Command, ARequestInfo.Document);

  // CORS headers for web-based dashboards
  AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');
  if Response.IsPreflight then
  begin
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Methods', 'GET, OPTIONS');
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Headers', 'Content-Type');
  end;

  AResponseInfo.ResponseNo := Response.StatusCode;
  if Response.ContentType <> '' then
  begin
    AResponseInfo.ContentType := Response.ContentType;
    AResponseInfo.CharSet := 'utf-8';
    AResponseInfo.ContentText := Response.Body;
  end;
end;

end.
