{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Simple File Logger                              }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

/// <summary>
/// Logs a message to the log file.
/// </summary>
procedure Log(const AMessage: string); overload;

/// <summary>
/// Logs a formatted message to the log file.
/// </summary>
procedure Log(const AFormat: string; const AArgs: array of const); overload;

/// <summary>
/// Enables or disables logging globally.
/// When disabled, Log calls are no-ops.
/// </summary>
/// <param name="AEnabled">Enable or disable logging.</param>
/// <param name="AFilename">Log filename (relative to exe or absolute path).</param>
/// <param name="AAppend">If true, append to existing file; if false, create new file.</param>
procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string = 'bqc.log'; AAppend: Boolean = False);

/// <summary>
/// Returns true if logging is currently enabled.
/// </summary>
function IsLoggingEnabled: Boolean;

implementation

uses
  Winapi.Windows;

var
  GLogFile: string;
  GLock: TCriticalSection;
  GInitialized: Boolean = False;
  GLoggingEnabled: Boolean = True;  // Enabled by default for startup diagnostics
  GAppendMode: Boolean = False;
  GSessionStarted: Boolean = False;

procedure InitLogger;
var
  ExePath: string;
begin
  if GInitialized then
    Exit;

  GLock := TCriticalSection.Create;
  ExePath := ExtractFilePath(ParamStr(0));
  GLogFile := ExePath + 'bqc.log';
  GInitialized := True;
end;

procedure SetLoggingEnabled(AEnabled: Boolean; const AFilename: string; AAppend: Boolean);
var
  ExePath: string;
begin
  if not GInitialized then
    InitLogger;

  GLoggingEnabled := AEnabled;
  GAppendMode := AAppend;

  // Resolve filename
  if AFilename <> '' then
  begin
    if ExtractFilePath(AFilename) = '' then
    begin
      // Relative path - prepend exe directory
      ExePath := ExtractFilePath(ParamStr(0));
      GLogFile := ExePath + AFilename;
    end
    else
      GLogFile := AFilename;
  end;

  // Reset session flag when settings change
  GSessionStarted := False;
end;

function IsLoggingEnabled: Boolean;
begin
  Result := GLoggingEnabled;
end;

procedure Log(const AMessage: string);
var
  LogLine: string;
  FileHandle: TextFile;
  ThreadId: Cardinal;
  CreateNew: Boolean;
begin
  if not GLoggingEnabled then
    Exit;

  if not GInitialized then
    InitLogger;

  ThreadId := GetCurrentThreadId;
  LogLine := Format('[%s] [TID:%d] %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
    ThreadId,
    AMessage
  ]);

  GLock.Enter;
  try
    // Determine if we should create a new file or append
    // On first log of session: create new if not append mode
    // After first log: always append
    CreateNew := (not GSessionStarted) and (not GAppendMode);

    AssignFile(FileHandle, GLogFile);
    try
      if CreateNew or (not FileExists(GLogFile)) then
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
      GSessionStarted := True;
    finally
      CloseFile(FileHandle);
    end;
  except
    // Silently ignore logging errors
  end;
  GLock.Leave;
end;

procedure Log(const AFormat: string; const AArgs: array of const);
begin
  Log(Format(AFormat, AArgs));
end;

initialization
  InitLogger;

finalization
  if GInitialized then
    GLock.Free;

end.
