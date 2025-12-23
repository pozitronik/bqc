{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Autostart Management                            }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.Autostart;

interface

type
  /// <summary>
  /// Manages application autostart via Windows Registry.
  /// Uses HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run.
  /// </summary>
  TAutostartManager = class
  public
    /// <summary>
    /// Enables or disables autostart by adding/removing registry entry.
    /// </summary>
    /// <param name="AEnabled">True to enable autostart, False to disable.</param>
    class procedure Apply(AEnabled: Boolean);

    /// <summary>
    /// Checks if autostart is currently enabled in the registry.
    /// </summary>
    /// <returns>True if autostart registry entry exists.</returns>
    class function IsEnabled: Boolean;

    /// <summary>
    /// Returns the path registered for autostart, or empty string if not registered.
    /// </summary>
    class function GetRegisteredPath: string;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Win.Registry,
  App.Logger;

const
  REG_RUN_KEY = 'Software\Microsoft\Windows\CurrentVersion\Run';
  REG_APP_NAME = 'BluetoothQuickConnect';

{ TAutostartManager }

class procedure TAutostartManager.Apply(AEnabled: Boolean);
var
  Reg: TRegistry;
  ExePath: string;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_RUN_KEY, True) then
    begin
      try
        if AEnabled then
        begin
          ExePath := ParamStr(0);
          Reg.WriteString(REG_APP_NAME, '"' + ExePath + '"');
          Log('Enabled autostart, path="%s"', [ExePath], ClassName);
        end
        else
        begin
          if Reg.ValueExists(REG_APP_NAME) then
          begin
            Reg.DeleteValue(REG_APP_NAME);
            Log('Disabled autostart', ClassName);
          end;
        end;
      finally
        Reg.CloseKey;
      end;
    end
    else
      Log('Failed to open registry key', ClassName);
  finally
    Reg.Free;
  end;
end;

class function TAutostartManager.IsEnabled: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(REG_RUN_KEY) then
    begin
      try
        Result := Reg.ValueExists(REG_APP_NAME);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

class function TAutostartManager.GetRegisteredPath: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(REG_RUN_KEY) then
    begin
      try
        if Reg.ValueExists(REG_APP_NAME) then
          Result := Reg.ReadString(REG_APP_NAME);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

end.
