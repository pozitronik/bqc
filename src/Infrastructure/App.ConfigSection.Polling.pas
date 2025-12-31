{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Polling Configuration Section                   }
{                                                       }
{*******************************************************}

/// <summary>
/// Polling and event settings implementation.
/// </summary>
unit App.ConfigSection.Polling;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Polling and event settings implementation.
  /// </summary>
  TPollingConfigSection = class(TConfigSectionBase, IPollingConfig)
  private
    FPollingMode: TPollingMode;
    FPollingInterval: Integer;
    FEventDebounceMs: Integer;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);

    procedure SetDefaults;

    property PollingMode: TPollingMode read FPollingMode write SetPollingMode;
    property PollingInterval: Integer read FPollingInterval write SetPollingInterval;
    property EventDebounceMs: Integer read FEventDebounceMs write SetEventDebounceMs;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TPollingConfigSection }

constructor TPollingConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TPollingConfigSection.SetDefaults;
begin
  FPollingMode := DEF_POLLING_MODE;
  FPollingInterval := DEF_POLLING_INTERVAL;
  FEventDebounceMs := DEF_EVENT_DEBOUNCE_MS;
end;

function TPollingConfigSection.GetPollingMode: TPollingMode;
begin
  Result := FPollingMode;
end;

function TPollingConfigSection.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

function TPollingConfigSection.GetEventDebounceMs: Integer;
begin
  Result := FEventDebounceMs;
end;

procedure TPollingConfigSection.SetPollingMode(AValue: TPollingMode);
begin
  if FPollingMode <> AValue then
  begin
    FPollingMode := AValue;
    NotifyModified;
  end;
end;

procedure TPollingConfigSection.SetPollingInterval(AValue: Integer);
begin
  SetFieldInteger(FPollingInterval, AValue);
end;

procedure TPollingConfigSection.SetEventDebounceMs(AValue: Integer);
begin
  SetFieldInteger(FEventDebounceMs, AValue);
end;

end.
