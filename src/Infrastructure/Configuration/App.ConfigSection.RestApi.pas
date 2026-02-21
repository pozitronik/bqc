{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       REST API Configuration Section                  }
{                                                       }
{*******************************************************}

/// <summary>
/// REST API server settings implementation.
/// </summary>
unit App.ConfigSection.RestApi;

interface

uses
  System.SysUtils,
  App.RestApiConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// REST API server settings implementation.
  /// </summary>
  TRestApiConfigSection = class(TConfigSectionBase, IRestApiConfig)
  private
    FEnabled: Boolean;
    FPort: Integer;
    FBindAddress: string;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetEnabled: Boolean;
    function GetPort: Integer;
    function GetBindAddress: string;

    procedure SetEnabled(AValue: Boolean);
    procedure SetPort(AValue: Integer);
    procedure SetBindAddress(const AValue: string);

    procedure SetDefaults;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Port: Integer read FPort write SetPort;
    property BindAddress: string read FBindAddress write SetBindAddress;
  end;

implementation

uses
  App.SettingsRepository;

{ TRestApiConfigSection }

constructor TRestApiConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TRestApiConfigSection.SetDefaults;
begin
  FEnabled := DEF_API_ENABLED;
  FPort := DEF_API_PORT;
  FBindAddress := DEF_API_BIND_ADDRESS;
end;

function TRestApiConfigSection.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TRestApiConfigSection.GetPort: Integer;
begin
  Result := FPort;
end;

function TRestApiConfigSection.GetBindAddress: string;
begin
  Result := FBindAddress;
end;

procedure TRestApiConfigSection.SetEnabled(AValue: Boolean);
begin
  SetFieldBoolean(FEnabled, AValue);
end;

procedure TRestApiConfigSection.SetPort(AValue: Integer);
begin
  SetFieldInteger(FPort, AValue);
end;

procedure TRestApiConfigSection.SetBindAddress(const AValue: string);
begin
  SetFieldString(FBindAddress, AValue);
end;

end.
