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
    FPairingStateSyncInterval: Integer;
    FPairingTimeout: Integer;
    FPairingMode: TPairingMode;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    function GetEnumerationMode: TEnumerationMode;
    function GetBluetoothPlatform: TBluetoothPlatform;
    function GetAutoScanOnStartup: Boolean;
    function GetPairingStateSyncInterval: Integer;
    function GetPairingTimeout: Integer;
    function GetPairingMode: TPairingMode;

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetEnumerationMode(AValue: TEnumerationMode);
    procedure SetBluetoothPlatform(AValue: TBluetoothPlatform);
    procedure SetAutoScanOnStartup(AValue: Boolean);
    procedure SetPairingStateSyncInterval(AValue: Integer);
    procedure SetPairingTimeout(AValue: Integer);
    procedure SetPairingMode(AValue: TPairingMode);

    procedure SetDefaults;

    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write SetConnectionRetryCount;
    property EnumerationMode: TEnumerationMode read FEnumerationMode write SetEnumerationMode;
    property BluetoothPlatform: TBluetoothPlatform read FBluetoothPlatform write SetBluetoothPlatform;
    property AutoScanOnStartup: Boolean read FAutoScanOnStartup write SetAutoScanOnStartup;
    property PairingStateSyncInterval: Integer read FPairingStateSyncInterval write SetPairingStateSyncInterval;
    property PairingTimeout: Integer read FPairingTimeout write SetPairingTimeout;
    property PairingMode: TPairingMode read FPairingMode write SetPairingMode;
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
  FPairingStateSyncInterval := 30000;  // 30 seconds default, 0 = disabled
  FPairingTimeout := DEF_PAIRING_TIMEOUT;  // 30 seconds default
  FPairingMode := pmAutomatic;  // Use Windows dialogs by default
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

function TConnectionConfigSection.GetPairingStateSyncInterval: Integer;
begin
  Result := FPairingStateSyncInterval;
end;

procedure TConnectionConfigSection.SetPairingStateSyncInterval(AValue: Integer);
begin
  // Validate range: 0 (disabled) to 300000ms (5 minutes)
  if (AValue < 0) or (AValue > 300000) then
    raise EArgumentOutOfRangeException.CreateFmt(
      'PairingStateSyncInterval must be between 0 and 300000ms, got %d', [AValue]);

  if FPairingStateSyncInterval <> AValue then
  begin
    FPairingStateSyncInterval := AValue;
    NotifyModified;
  end;
end;

function TConnectionConfigSection.GetPairingTimeout: Integer;
begin
  Result := FPairingTimeout;
end;

procedure TConnectionConfigSection.SetPairingTimeout(AValue: Integer);
begin
  SetFieldInteger(FPairingTimeout, AValue);
end;

function TConnectionConfigSection.GetPairingMode: TPairingMode;
begin
  Result := FPairingMode;
end;

procedure TConnectionConfigSection.SetPairingMode(AValue: TPairingMode);
begin
  if FPairingMode <> AValue then
  begin
    FPairingMode := AValue;
    NotifyModified;
  end;
end;

end.
