{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Configuration Interfaces             }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines connection and polling configuration interfaces.
/// </summary>
unit App.ConnectionConfigIntf;

interface

uses
  App.ConfigEnums;

type
  /// <summary>
  /// Global connection settings (defaults).
  /// Used by: TBluetoothService, connection strategies
  /// </summary>
  IConnectionConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000009}']
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

    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read GetConnectionRetryCount write SetConnectionRetryCount;
    property EnumerationMode: TEnumerationMode read GetEnumerationMode write SetEnumerationMode;
    property BluetoothPlatform: TBluetoothPlatform read GetBluetoothPlatform write SetBluetoothPlatform;
    property AutoScanOnStartup: Boolean read GetAutoScanOnStartup write SetAutoScanOnStartup;
    property PairingStateSyncInterval: Integer read GetPairingStateSyncInterval write SetPairingStateSyncInterval;
    property PairingTimeout: Integer read GetPairingTimeout write SetPairingTimeout;
    property PairingMode: TPairingMode read GetPairingMode write SetPairingMode;
  end;

  /// <summary>
  /// Polling and event settings.
  /// Used by: TBluetoothService, SettingsPresenter
  /// </summary>
  IPollingConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000005}']
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);

    property PollingMode: TPollingMode read GetPollingMode write SetPollingMode;
    property PollingInterval: Integer read GetPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read GetEventDebounceMs write SetEventDebounceMs;
  end;

implementation

end.
