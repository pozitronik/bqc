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

    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);

    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ConnectionRetryCount: Integer read GetConnectionRetryCount write SetConnectionRetryCount;
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
