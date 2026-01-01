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
  TConnectionConfigSection = class(TConfigSectionBase, IConnectionConfig)
  private
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;
    FEnumerationMode: TEnumerationMode;
    FBluetoothPlatform: TBluetoothPlatform;
    FAutoScanOnStartup: Boolean;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    function GetEnumerationMode: TEnumerationMode;
    function GetBluetoothPlatform: TBluetoothPlatform;
    function GetAutoScanOnStartup: Boolean;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetEnumerationMode(AValue: TEnumerationMode);
    procedure SetBluetoothPlatform(AValue: TBluetoothPlatform);
    procedure SetAutoScanOnStartup(AValue: Boolean);

    procedure SetDefaults;

    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
    property EnumerationMode: TEnumerationMode read FEnumerationMode write SetEnumerationMode;
    property BluetoothPlatform: TBluetoothPlatform read FBluetoothPlatform write SetBluetoothPlatform;
    property AutoScanOnStartup: Boolean read FAutoScanOnStartup write SetAutoScanOnStartup;
  end;

implementation

uses
  App.Config;

{ TConnectionConfigSection }

constructor TConnectionConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TConnectionConfigSection.SetDefaults;
begin
  FConnectionTimeout := DEF_CONNECTION_TIMEOUT;
  FConnectionRetryCount := DEF_CONNECTION_RETRY_COUNT;
  FEnumerationMode := emComposite;
  FBluetoothPlatform := bpAuto;  // Auto-detect by default
  FAutoScanOnStartup := False;    // Don't scan on startup by default
end;

function TConnectionConfigSection.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TConnectionConfigSection.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

function TConnectionConfigSection.GetEnumerationMode: TEnumerationMode;
begin
  Result := FEnumerationMode;
end;

function TConnectionConfigSection.GetBluetoothPlatform: TBluetoothPlatform;
begin
  Result := FBluetoothPlatform;
end;

procedure TConnectionConfigSection.SetConnectionTimeout(AValue: Integer);
begin
  SetFieldInteger(FConnectionTimeout, AValue);
end;

procedure TConnectionConfigSection.SetConnectionRetryCount(AValue: Integer);
begin
  SetFieldInteger(FConnectionRetryCount, AValue);
end;

procedure TConnectionConfigSection.SetEnumerationMode(AValue: TEnumerationMode);
begin
  if FEnumerationMode <> AValue then
  begin
    FEnumerationMode := AValue;
    NotifyModified;
  end;
end;

procedure TConnectionConfigSection.SetBluetoothPlatform(AValue: TBluetoothPlatform);
begin
  if FBluetoothPlatform <> AValue then
  begin
    FBluetoothPlatform := AValue;
    NotifyModified;
  end;
end;

function TConnectionConfigSection.GetAutoScanOnStartup: Boolean;
begin
  Result := FAutoScanOnStartup;
end;

procedure TConnectionConfigSection.SetAutoScanOnStartup(AValue: Boolean);
begin
  if FAutoScanOnStartup <> AValue then
  begin
    FAutoScanOnStartup := AValue;
    NotifyModified;
  end;
end;

end.
