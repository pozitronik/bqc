{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       System Permissions and Elevation                }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit System.Permissions;

interface

uses
  Winapi.Windows;

/// <summary>
/// Checks if the current process is running with administrator privileges.
/// </summary>
/// <returns>True if running as administrator, False otherwise.</returns>
function IsRunningAsAdmin: Boolean;

/// <summary>
/// Restarts the current application with administrator privileges.
/// Shows UAC prompt to the user.
/// </summary>
/// <returns>True if restart was initiated, False if user cancelled or error occurred.</returns>
function RestartAsAdmin: Boolean;

implementation

uses
  Winapi.ShellAPI,
  System.SysUtils;

function IsRunningAsAdmin: Boolean;
var
  TokenHandle: THandle;
  TokenInformation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;

  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    Exit;

  try
    if GetTokenInformation(TokenHandle, TokenElevation, @TokenInformation,
      SizeOf(TokenInformation), ReturnLength) then
    begin
      Result := TokenInformation.TokenIsElevated <> 0;
    end;
  finally
    CloseHandle(TokenHandle);
  end;
end;

function RestartAsAdmin: Boolean;
var
  ExeName: string;
  SEI: TShellExecuteInfo;
begin
  Result := False;
  ExeName := ParamStr(0);

  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_NOCLOSEPROCESS;
  SEI.Wnd := 0;
  SEI.lpVerb := 'runas';
  SEI.lpFile := PChar(ExeName);
  SEI.lpParameters := nil;
  SEI.lpDirectory := nil;
  SEI.nShow := SW_SHOWNORMAL;

  if ShellExecuteEx(@SEI) then
  begin
    Result := True;
    // Close current instance
    Halt(0);
  end;
end;

end.
