{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Application Logger                              }
{                                                       }
{       Provides structured logging with source         }
{       identification for debugging and diagnostics.   }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Exception raised when logging fails.
  /// </summary>
  ELoggerError = class(Exception);

  /// <summary>
  /// Logger implementation.
  /// Thread-safe file-based logging with source identification.
  /// </summary>
  TLogger = class(TInterfacedObject, ILogger)
  private
    FLogFile: string;
    FLock: TCriticalSection;
    FEnabled: Boolean;
    FAppendMode: Boolean;
    FSessionStarted: Boolean;

    procedure WriteToFile(const AMessage: string; const ASource: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Configures the logger.
    /// </summary>
    /// <param name="AEnabled">Enable or disable logging.</param>
    /// <param name="AFilename">Log filename (relative to exe or absolute path).</param>
    /// <param name="AAppend">If true, append to existing file; if false, create new file.</param>
    procedure Configure(AEnabled: Boolean; const AFilename: string = 'bqc.log';
      AAppend: Boolean = False);

    { ILogger }
    procedure Log(const AMessage: string; const ASource: string = '');
    procedure LogFmt(const AFormat: string; const AArgs: array of const;
      const ASource: string = '');
    function IsEnabled: Boolean;
  end;

//------------------------------------------------------------------------------
// Global convenience procedures (backward compatible + new signature)
//------------------------------------------------------------------------------

/// <summary>
/// Logs a message with source identifier.
/// </summary>
procedure Log(const AMessage: string; const ASource: string); overload;

/// <summary>
/// Logs a message (legacy - no source).
/// </summary>
procedure Log(const AMessage: string); overload;

/// <summary>
/// Logs a formatted message with source identifier.
/// </summary>
procedure Log(const AFormat: string; const AArgs: array of const;
  const ASource: string); overload;

/// <summary>
/// Logs a formatted message (legacy - no source).
/// </summary>
procedure Log(const AFormat: string; const AArgs: array of const); overload;

/// <summary>
/// Enables or disables logging globally.
/// </summary>
procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string = 'bqc.log';
  AAppend: Boolean = False);

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
begin
  if GLogger = nil then
  begin
    GLoggerLock.Enter;
    try
      if GLogger = nil then
        GLogger := TLogger.Create;
    finally
      GLoggerLock.Leave;
    end;
  end;
  Result := GLogger;
end;

function Logger: ILogger;
begin
  Result := GetLogger;
end;

procedure Log(const AMessage: string; const ASource: string);
begin
  GetLogger.Log(AMessage, ASource);
end;

procedure Log(const AMessage: string);
begin
  GetLogger.Log(AMessage, '');
end;

procedure Log(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  GetLogger.LogFmt(AFormat, AArgs, ASource);
end;

procedure Log(const AFormat: string; const AArgs: array of const);
begin
  GetLogger.LogFmt(AFormat, AArgs, '');
end;

procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string;
  AAppend: Boolean);
begin
  GetLogger.Configure(AEnabled, AFilename, AAppend);
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
  AAppend: Boolean);
var
  ExePath: string;
begin
  FLock.Enter;
  try
    FEnabled := AEnabled;
    FAppendMode := AAppend;

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

procedure TLogger.Log(const AMessage: string; const ASource: string);
begin
  if not FEnabled then
    Exit;

  WriteToFile(AMessage, ASource);
end;

procedure TLogger.LogFmt(const AFormat: string; const AArgs: array of const;
  const ASource: string);
begin
  if not FEnabled then
    Exit;

  WriteToFile(Format(AFormat, AArgs), ASource);
end;

procedure TLogger.WriteToFile(const AMessage: string; const ASource: string);
var
  LogLine: string;
  FileHandle: TextFile;
  ThreadId: Cardinal;
  CreateNew: Boolean;
  SourcePart: string;
begin
  ThreadId := GetCurrentThreadId;

  // Build source part
  if ASource <> '' then
    SourcePart := Format('[%s] ', [ASource])
  else
    SourcePart := '';

  LogLine := Format('[%s] [TID:%d] %s%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
    ThreadId,
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
  GLogger.Free;
  GLoggerLock.Free;

end.
