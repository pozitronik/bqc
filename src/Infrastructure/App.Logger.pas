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

implementation

uses
  Winapi.Windows;

var
  GLogFile: string;
  GLock: TCriticalSection;
  GInitialized: Boolean = False;

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

procedure Log(const AMessage: string);
var
  LogLine: string;
  FileHandle: TextFile;
  ThreadId: Cardinal;
begin
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
  Log('=== BQC Logger Started ===');

finalization
  if GInitialized then
  begin
    Log('=== BQC Logger Stopped ===');
    GLock.Free;
  end;

end.
