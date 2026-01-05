{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Windows System Theme Detector                   }
{                                                       }
{*******************************************************}

/// <summary>
/// Detects Windows system theme (dark/light mode) for adaptive UI.
/// Reads from Windows registry to determine current taskbar appearance.
/// </summary>
unit App.SystemThemeDetector;

interface

type
  /// <summary>
  /// Detected Windows theme mode.
  /// </summary>
  TSystemThemeMode = (
    stmLight,  // Light theme (light taskbar)
    stmDark    // Dark theme (dark taskbar)
  );

  /// <summary>
  /// Detects Windows system theme (dark/light mode).
  /// Reads from registry: HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize
  /// </summary>
  TSystemThemeDetector = class
  public
    /// <summary>
    /// Returns the current Windows taskbar theme mode.
    /// Uses SystemUsesLightTheme registry value.
    /// </summary>
    /// <returns>stmLight or stmDark based on Windows settings.
    /// Returns stmLight if detection fails (safe default for black outline visibility).</returns>
    class function GetTaskbarTheme: TSystemThemeMode;

    /// <summary>
    /// Returns True if Windows is in dark mode (dark taskbar).
    /// </summary>
    class function IsDarkMode: Boolean;
  end;

implementation

uses
  Winapi.Windows,
  System.Win.Registry,
  System.SysUtils,
  App.Logger;

const
  PERSONALIZE_KEY = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  SYSTEM_USES_LIGHT_THEME = 'SystemUsesLightTheme';

{ TSystemThemeDetector }

class function TSystemThemeDetector.GetTaskbarTheme: TSystemThemeMode;
var
  Reg: TRegistry;
  Value: Integer;
begin
  // Safe default: Light mode (black outline visible on light backgrounds)
  Result := stmLight;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(PERSONALIZE_KEY) then
    begin
      try
        if Reg.ValueExists(SYSTEM_USES_LIGHT_THEME) then
        begin
          Value := Reg.ReadInteger(SYSTEM_USES_LIGHT_THEME);
          if Value = 0 then
            Result := stmDark  // 0 = Dark mode
          else
            Result := stmLight; // 1 = Light mode
        end;
      except
        on E: Exception do
          LogWarning('Failed to read system theme: %s', [E.Message], 'SystemThemeDetector');
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function TSystemThemeDetector.IsDarkMode: Boolean;
begin
  Result := GetTaskbarTheme = stmDark;
end;

end.
