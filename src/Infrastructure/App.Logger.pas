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
procedure SetLoggingEnabled(AEnabled: Boolean);

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

procedure SetLoggingEnabled(AEnabled: Boolean);
begin
  GLoggingEnabled := AEnabled;
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
    AssignFile(FileHandle, GLogFile);
    try
      if FileExists(GLogFile) then
        Append(FileHandle)
      else
        Rewrite(FileHandle);

      WriteLn(FileHandle, LogLine);
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
