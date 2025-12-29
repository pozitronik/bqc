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
  App.ConfigEnums,
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
    FEnumerationMode: TEnumerationMode;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    function GetEnumerationMode: TEnumerationMode;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetEnumerationMode(AValue: TEnumerationMode);

    procedure SetDefaults;

    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
    property EnumerationMode: TEnumerationMode read FEnumerationMode write SetEnumerationMode;
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
  FEnumerationMode := emComposite;
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

function TConnectionConfigSection.GetEnumerationMode: TEnumerationMode;
begin
  Result := FEnumerationMode;
end;

procedure TConnectionConfigSection.SetEnumerationMode(AValue: TEnumerationMode);
begin
  if FEnumerationMode <> AValue then
  begin
    FEnumerationMode := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
