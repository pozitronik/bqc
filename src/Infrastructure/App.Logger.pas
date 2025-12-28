{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Logger                              }
{                                                       }
{       Provides structured logging with source         }
{       identification for debugging and diagnostics.   }
{                                                       }
{*******************************************************}

unit App.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.LogConfigIntf;

type
  /// <summary>
  /// Exception raised when logging fails.
  /// </summary>
  ELoggerError = class(Exception);

  /// <summary>
  /// Logger implementation.
  /// Thread-safe file-based logging with severity levels and source identification.
  /// </summary>
  TLogger = class(TInterfacedObject, ILogger)
  private
    FLogFile: string;
    FLock: TCriticalSection;
    FEnabled: Boolean;
    FAppendMode: Boolean;
    FSessionStarted: Boolean;
    FMinLevel: TLogLevel;

    procedure WriteToFile(ALevel: TLogLevel; const AMessage: string;
      const ASource: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Configures the logger.
    /// </summary>
    /// <param name="AEnabled">Enable or disable logging.</param>
    /// <param name="AFilename">Log filename (relative to exe or absolute path).</param>
    /// <param name="AAppend">If true, append to existing file; if false, create new file.</param>
    /// <param name="AMinLevel">Minimum log level to output.</param>
    procedure Configure(AEnabled: Boolean; const AFilename: string = 'bqc.log';
      AAppend: Boolean = False; AMinLevel: TLogLevel = llInfo);

    { ILogger }
    procedure Debug(const AMessage: string; const ASource: string = ''); overload;
    procedure Info(const AMessage: string; const ASource: string = ''); overload;
    procedure Warning(const AMessage: string; const ASource: string = ''); overload;
    procedure Error(const AMessage: string; const ASource: string = ''); overload;
    function IsEnabled: Boolean;
    function GetMinLevel: TLogLevel;

    { Format overloads for convenience }
    procedure Debug(const AFormat: string; const AArgs: array of const;
      const ASource: string = ''); overload;
    procedure Info(const AFormat: string; const AArgs: array of const;
      const ASource: string = ''); overload;
    procedure Warning(const AFormat: string; const AArgs: array of const;
      const ASource: string = ''); overload;
    procedure Error(const AFormat: string; const AArgs: array of const;
      const ASource: string = ''); overload;
  end;

//------------------------------------------------------------------------------
// Global convenience procedures with severity levels
//------------------------------------------------------------------------------

/// <summary>
/// Logs a debug message with optional source.
/// </summary>
procedure LogDebug(const AMessage: string; const ASource: string = ''); overload;

/// <summary>
/// Logs a formatted debug message with optional source.
/// </summary>
procedure LogDebug(const AFormat: string; const AArgs: array of const;
  const ASource: string = ''); overload;

/// <summary>
/// Logs an informational message with optional source.
/// </summary>
procedure LogInfo(const AMessage: string; const ASource: string = ''); overload;

/// <summary>
/// Logs a formatted informational message with optional source.
/// </summary>
procedure LogInfo(const AFormat: string; const AArgs: array of const;
  const ASource: string = ''); overload;

/// <summary>
/// Logs a warning message with optional source.
/// </summary>
procedure LogWarning(const AMessage: string; const ASource: string = ''); overload;

/// <summary>
/// Logs a formatted warning message with optional source.
/// </summary>
procedure LogWarning(const AFormat: string; const AArgs: array of const;
  const ASource: string = ''); overload;

/// <summary>
/// Logs an error message with optional source.
/// </summary>
procedure LogError(const AMessage: string; const ASource: string = ''); overload;

/// <summary>
/// Logs a formatted error message with optional source.
/// </summary>
procedure LogError(const AFormat: string; const AArgs: array of const;
  const ASource: string = ''); overload;

/// <summary>
/// Configures the global logger.
/// </summary>
procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string = 'bqc.log';
  AAppend: Boolean = False; AMinLevel: TLogLevel = llInfo);

/// <summary>
/// Returns true if logging is currently enabled.
/// </summary>
function IsLoggingEnabled: Boolean;

/// <summary>
/// Returns the global logger instance.
/// </summary>
function Logger: ILogger;

implementation

uses
  Winapi.Windows;

var
  GLogger: TLogger = nil;
  GLoggerLock: TCriticalSection = nil;

function GetLogger: TLogger;
var
  NewLogger: TLogger;
  CurrentLogger: Pointer;
begin
  // Atomic read with memory barrier - ensures we see fully constructed object
  CurrentLogger := TInterlocked.CompareExchange(Pointer(GLogger), nil, nil);
  Result := TLogger(CurrentLogger);

  if Result = nil then
  begin
    GLoggerLock.Enter;
    try
      // Re-check inside lock
      if GLogger = nil then
      begin
        NewLogger := TLogger.Create;
        // Atomic write with full memory barrier ensures object is fully
        // constructed before other threads can see the non-nil pointer
        TInterlocked.Exchange(Pointer(GLogger), Pointer(NewLogger));
        Result := NewLogger;
      end
      else
        Result := GLogger;
    finally
      GLoggerLock.Leave;
    end;
  end;
end;

function Logger: ILogger;
begin
  Result := GetLogger;
end;

procedure LogDebug(const AMessage: string; const ASource: string);
begin
  GetLogger.Debug(AMessage, ASource);
end;

procedure LogDebug(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  GetLogger.Debug(Format(AFormat, AArgs), ASource);
end;

procedure LogInfo(const AMessage: string; const ASource: string);
begin
  GetLogger.Info(AMessage, ASource);
end;

procedure LogInfo(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  GetLogger.Info(Format(AFormat, AArgs), ASource);
end;

procedure LogWarning(const AMessage: string; const ASource: string);
begin
  GetLogger.Warning(AMessage, ASource);
end;

procedure LogWarning(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  GetLogger.Warning(Format(AFormat, AArgs), ASource);
end;

procedure LogError(const AMessage: string; const ASource: string);
begin
  GetLogger.Error(AMessage, ASource);
end;

procedure LogError(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  GetLogger.Error(Format(AFormat, AArgs), ASource);
end;

procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string;
  AAppend: Boolean; AMinLevel: TLogLevel);
begin
  GetLogger.Configure(AEnabled, AFilename, AAppend, AMinLevel);
end;

function IsLoggingEnabled: Boolean;
begin
  Result := GetLogger.IsEnabled;
end;

{ TLogger }

constructor TLogger.Create;
var
  ExePath: string;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FEnabled := True;  // Enabled by default for startup diagnostics
  FAppendMode := False;
  FSessionStarted := False;
  FMinLevel := llInfo;  // Default to Info level

  // Default log file next to executable
  ExePath := ExtractFilePath(ParamStr(0));
  FLogFile := ExePath + 'bqc.log';
end;

destructor TLogger.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TLogger.Configure(AEnabled: Boolean; const AFilename: string;
  AAppend: Boolean; AMinLevel: TLogLevel);
var
  ExePath: string;
begin
  FLock.Enter;
  try
    FEnabled := AEnabled;
    FAppendMode := AAppend;
    FMinLevel := AMinLevel;

    // Resolve filename
    if AFilename <> '' then
    begin
      if ExtractFilePath(AFilename) = '' then
      begin
        // Relative path - prepend exe directory
        ExePath := ExtractFilePath(ParamStr(0));
        FLogFile := ExePath + AFilename;
      end
      else
        FLogFile := AFilename;
    end;

    // Reset session flag when settings change
    FSessionStarted := False;
  finally
    FLock.Leave;
  end;
end;

function TLogger.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TLogger.GetMinLevel: TLogLevel;
begin
  Result := FMinLevel;
end;

procedure TLogger.Debug(const AMessage: string; const ASource: string);
begin
  WriteToFile(llDebug, AMessage, ASource);
end;

procedure TLogger.Info(const AMessage: string; const ASource: string);
begin
  WriteToFile(llInfo, AMessage, ASource);
end;

procedure TLogger.Warning(const AMessage: string; const ASource: string);
begin
  WriteToFile(llWarning, AMessage, ASource);
end;

procedure TLogger.Error(const AMessage: string; const ASource: string);
begin
  WriteToFile(llError, AMessage, ASource);
end;

procedure TLogger.Debug(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  WriteToFile(llDebug, Format(AFormat, AArgs), ASource);
end;

procedure TLogger.Info(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  WriteToFile(llInfo, Format(AFormat, AArgs), ASource);
end;

procedure TLogger.Warning(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  WriteToFile(llWarning, Format(AFormat, AArgs), ASource);
end;

procedure TLogger.Error(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  WriteToFile(llError, Format(AFormat, AArgs), ASource);
end;

procedure TLogger.WriteToFile(ALevel: TLogLevel; const AMessage: string;
  const ASource: string);
var
  LogLine: string;
  FileHandle: TextFile;
  ThreadId: Cardinal;
  CreateNew: Boolean;
  SourcePart: string;
begin
  // Check if logging is enabled and level meets threshold
  if (not FEnabled) or (ALevel < FMinLevel) then
    Exit;

  ThreadId := GetCurrentThreadId;

  // Build source part
  if ASource <> '' then
    SourcePart := Format('[%s] ', [ASource])
  else
    SourcePart := '';

  LogLine := Format('[%s] [TID:%d] [%s] %s%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
    ThreadId,
    LogLevelNames[ALevel],
    SourcePart,
    AMessage
  ]);

  FLock.Enter;
  try
    try
      // Determine if we should create a new file or append
      CreateNew := (not FSessionStarted) and (not FAppendMode);

      AssignFile(FileHandle, FLogFile);
      try
        if CreateNew or (not FileExists(FLogFile)) then
        begin
          Rewrite(FileHandle);
          // Write session header
          WriteLn(FileHandle, '=== Bluetooth Quick Connect Log ===');
          WriteLn(FileHandle, Format('=== Session started: %s ===', [
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
          WriteLn(FileHandle, '');
        end
        else
          Append(FileHandle);

        WriteLn(FileHandle, LogLine);
        FSessionStarted := True;
      finally
        CloseFile(FileHandle);
      end;
    except
      on E: Exception do
        raise ELoggerError.CreateFmt('Failed to write to log file "%s": %s',
          [FLogFile, E.Message]);
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  GLoggerLock := TCriticalSection.Create;

finalization
  if Assigned(GLogger) then
    GLogger.Free;
  if Assigned(GLoggerLock) then
    GLoggerLock.Free;

end.
