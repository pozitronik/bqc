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
  TLogConfigSection = class(TInterfacedObject, ILogConfig)
  private
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;
    FLogLevel: TLogLevel;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetLogLevel: TLogLevel;

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetLogLevel(AValue: TLogLevel);

    procedure SetDefaults;

    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogFilename: string read FLogFilename write SetLogFilename;
    property LogAppend: Boolean read FLogAppend write SetLogAppend;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
  end;

implementation

uses
  App.SettingsRepository;

{ TLogConfigSection }

constructor TLogConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TLogConfigSection.SetDefaults;
begin
  FLogEnabled := DEF_LOG_ENABLED;
  FLogFilename := DEF_LOG_FILENAME;
  FLogAppend := DEF_LOG_APPEND;
  FLogLevel := DEF_LOG_LEVEL;
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
  if FLogEnabled <> AValue then
  begin
    FLogEnabled := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogFilename(const AValue: string);
begin
  if FLogFilename <> AValue then
  begin
    FLogFilename := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogAppend(AValue: Boolean);
begin
  if FLogAppend <> AValue then
  begin
    FLogAppend := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLogConfigSection.SetLogLevel(AValue: TLogLevel);
begin
  if FLogLevel <> AValue then
  begin
    FLogLevel := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
