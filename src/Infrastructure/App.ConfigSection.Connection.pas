{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Configuration Section                }
{                                                       }
{*******************************************************}

/// <summary>
/// Connection settings implementation.
/// </summary>
unit App.ConfigSection.Connection;

interface

uses
  System.SysUtils,
  App.ConnectionConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Connection settings implementation.
  /// </summary>
  TConnectionConfigSection = class(TInterfacedObject, IConnectionConfig)
  private
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);

    procedure SetDefaults;

    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
  end;

implementation

uses
  App.Config;

{ TConnectionConfigSection }

constructor TConnectionConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TConnectionConfigSection.SetDefaults;
begin
  FConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  FConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
end;

function TConnectionConfigSection.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TConnectionConfigSection.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

procedure TConnectionConfigSection.SetConnectionTimeout(AValue: Integer);
begin
  if FConnectionTimeout <> AValue then
  begin
    FConnectionTimeout := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TConnectionConfigSection.SetConnectionRetryCount(AValue: Integer);
begin
  if FConnectionRetryCount <> AValue then
  begin
    FConnectionRetryCount := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
