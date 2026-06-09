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
    ['{EFF5E6EC-272E-423C-8DF6-F343C5707344}']
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
