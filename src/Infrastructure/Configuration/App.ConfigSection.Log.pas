{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Log Configuration Section                       }
{                                                       }
{*******************************************************}

/// <summary>
/// Logging settings implementation.
/// </summary>
unit App.ConfigSection.Log;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.LogConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Logging settings implementation.
  /// </summary>
  TLogConfigSection = class(TConfigSectionBase, ILogConfig)
  private
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;
    FLogLevel: TLogLevel;
    FLogSourceFilter: string;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetLogLevel: TLogLevel;
    function GetLogSourceFilter: string;

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetLogLevel(AValue: TLogLevel);
    procedure SetLogSourceFilter(const AValue: string);

    procedure SetDefaults;

    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogFilename: string read FLogFilename write SetLogFilename;
    property LogAppend: Boolean read FLogAppend write SetLogAppend;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
    property LogSourceFilter: string read FLogSourceFilter write SetLogSourceFilter;
  end;

implementation

uses
  App.SettingsRepository;

{ TLogConfigSection }

constructor TLogConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TLogConfigSection.SetDefaults;
begin
  FLogEnabled := DEF_LOG_ENABLED;
  FLogFilename := DEF_LOG_FILENAME;
  FLogAppend := DEF_LOG_APPEND;
  FLogLevel := DEF_LOG_LEVEL;
  FLogSourceFilter := DEF_LOG_SOURCE_FILTER;
end;

function TLogConfigSection.GetLogEnabled: Boolean;
begin
  Result := FLogEnabled;
end;

function TLogConfigSection.GetLogFilename: string;
begin
  Result := FLogFilename;
end;

function TLogConfigSection.GetLogAppend: Boolean;
begin
  Result := FLogAppend;
end;

function TLogConfigSection.GetLogLevel: TLogLevel;
begin
  Result := FLogLevel;
end;

procedure TLogConfigSection.SetLogEnabled(AValue: Boolean);
begin
  SetFieldBoolean(FLogEnabled, AValue);
end;

procedure TLogConfigSection.SetLogFilename(const AValue: string);
begin
  SetFieldString(FLogFilename, AValue);
end;

procedure TLogConfigSection.SetLogAppend(AValue: Boolean);
begin
  SetFieldBoolean(FLogAppend, AValue);
end;

procedure TLogConfigSection.SetLogLevel(AValue: TLogLevel);
begin
  if FLogLevel <> AValue then
  begin
    FLogLevel := AValue;
    NotifyModified;
  end;
end;

function TLogConfigSection.GetLogSourceFilter: string;
begin
  Result := FLogSourceFilter;
end;

procedure TLogConfigSection.SetLogSourceFilter(const AValue: string);
begin
  SetFieldString(FLogSourceFilter, AValue);
end;

end.
