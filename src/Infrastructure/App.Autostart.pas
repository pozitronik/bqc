{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Autostart Management                            }
{                                                       }
{*******************************************************}

unit App.Autostart;

interface

type
  /// <summary>
  /// Interface for managing application autostart.
  /// Abstracts registry access for testability.
  /// </summary>
  IAutostartManager = interface
    ['{C8D9E0F1-3333-4444-5555-666677778888}']

    /// <summary>
    /// Enables or disables autostart.
    /// </summary>
    /// <param name="AEnabled">True to enable autostart, False to disable.</param>
    procedure Apply(AEnabled: Boolean);

    /// <summary>
    /// Checks if autostart is currently enabled.
    /// </summary>
    /// <returns>True if autostart is enabled.</returns>
    function IsEnabled: Boolean;

    /// <summary>
    /// Returns the path registered for autostart, or empty string if not registered.
    /// </summary>
    function GetRegisteredPath: string;
  end;

  /// <summary>
  /// Implementation of IAutostartManager using Windows Registry.
  /// Uses HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run.
  /// </summary>
  TAutostartManager = class(TInterfacedObject, IAutostartManager)
  public
    { IAutostartManager }
    procedure Apply(AEnabled: Boolean);
    function IsEnabled: Boolean;
    function GetRegisteredPath: string;
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

procedure TAutostartManager.Apply(AEnabled: Boolean);
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
          LogInfo('Enabled autostart, path="%s"', [ExePath], ClassName);
        end
        else
        begin
          if Reg.ValueExists(REG_APP_NAME) then
          begin
            Reg.DeleteValue(REG_APP_NAME);
            LogInfo('Disabled autostart', ClassName);
          end;
        end;
      finally
        Reg.CloseKey;
      end;
    end
    else
      LogWarning('Apply: Failed to open registry key for writing', ClassName);
  finally
    Reg.Free;
  end;
end;

function TAutostartManager.IsEnabled: Boolean;
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
    end
    else
      LogWarning('IsEnabled: Failed to open registry key for reading', ClassName);
  finally
    Reg.Free;
  end;
end;

function TAutostartManager.GetRegisteredPath: string;
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
    end
    else
      LogWarning('GetRegisteredPath: Failed to open registry key for reading', ClassName);
  finally
    Reg.Free;
  end;
end;

end.
