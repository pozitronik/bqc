{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API Configuration Interface                }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the REST API server configuration interface.
/// Used by: App.RestApiServer, App.SettingsPresenter
/// </summary>
unit App.RestApiConfigIntf;

interface

type
  /// <summary>
  /// REST API server settings.
  /// Controls the built-in HTTP server for external monitoring.
  /// </summary>
  IRestApiConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000020}']
    function GetEnabled: Boolean;
    function GetPort: Integer;
    function GetBindAddress: string;

    procedure SetEnabled(AValue: Boolean);
    procedure SetPort(AValue: Integer);
    procedure SetBindAddress(const AValue: string);

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Port: Integer read GetPort write SetPort;
    property BindAddress: string read GetBindAddress write SetBindAddress;
  end;

implementation

end.
