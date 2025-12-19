{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       UI Theme Support                                }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.Theme;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics;

type
  /// <summary>
  /// Theme mode enumeration.
  /// </summary>
  TThemeMode = (
    tmSystem,  // Follow Windows system theme
    tmLight,   // Force light theme
    tmDark     // Force dark theme
  );

  /// <summary>
  /// Color scheme for the UI.
  /// </summary>
  TThemeColors = record
    Background: TColor;
    BackgroundSecondary: TColor;
    ItemBackground: TColor;
    ItemBackgroundHover: TColor;
    ItemBackgroundSelected: TColor;
    TextPrimary: TColor;
    TextSecondary: TColor;
    TextDisabled: TColor;
    Accent: TColor;
    AccentHover: TColor;
    Border: TColor;
    IconColor: TColor;
    ConnectedColor: TColor;
    DisconnectedColor: TColor;
  end;

  /// <summary>
  /// Theme manager for the application.
  /// Singleton pattern.
  /// </summary>
  TThemeManager = class
  private class var
    FInstance: TThemeManager;
  private
    FCurrentMode: TThemeMode;
    FColors: TThemeColors;
    FIsDarkMode: Boolean;
    FOnThemeChanged: TNotifyEvent;

    procedure UpdateColors;
    function IsWindowsDarkMode: Boolean;
  public
    class function Instance: TThemeManager;
    class destructor Destroy;

    procedure SetThemeMode(AMode: TThemeMode);
    procedure Refresh;

    property CurrentMode: TThemeMode read FCurrentMode;
    property IsDarkMode: Boolean read FIsDarkMode;
    property Colors: TThemeColors read FColors;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

/// <summary>
/// Returns the global theme manager instance.
/// </summary>
function Theme: TThemeManager; inline;

implementation

uses
  System.Win.Registry;

const
  // Light theme colors (Windows 11 style)
  LightColors: TThemeColors = (
    Background: $00FFFFFF;
    BackgroundSecondary: $00F3F3F3;
    ItemBackground: $00FFFFFF;
    ItemBackgroundHover: $00F5F5F5;
    ItemBackgroundSelected: $00E5E5E5;
    TextPrimary: $00000000;
    TextSecondary: $00606060;
    TextDisabled: $00A0A0A0;
    Accent: $00D77800;        // Windows blue (BGR)
    AccentHover: $00C06A00;
    Border: $00E0E0E0;
    IconColor: $00404040;
    ConnectedColor: $00008000;  // Green
    DisconnectedColor: $00606060;
  );

  // Dark theme colors (Windows 11 style)
  DarkColors: TThemeColors = (
    Background: $00202020;
    BackgroundSecondary: $00282828;
    ItemBackground: $002D2D2D;
    ItemBackgroundHover: $003D3D3D;
    ItemBackgroundSelected: $004D4D4D;
    TextPrimary: $00FFFFFF;
    TextSecondary: $00A0A0A0;
    TextDisabled: $00606060;
    Accent: $00FF9F40;        // Windows accent (BGR)
    AccentHover: $00FFB060;
    Border: $00404040;
    IconColor: $00FFFFFF;
    ConnectedColor: $0060C060;  // Light green
    DisconnectedColor: $00808080;
  );

function Theme: TThemeManager;
begin
  Result := TThemeManager.Instance;
end;

{ TThemeManager }

class function TThemeManager.Instance: TThemeManager;
begin
  if FInstance = nil then
  begin
    FInstance := TThemeManager.Create;
    FInstance.FCurrentMode := tmSystem;
    FInstance.UpdateColors;
  end;
  Result := FInstance;
end;

class destructor TThemeManager.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TThemeManager.IsWindowsDarkMode: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      try
        // AppsUseLightTheme = 0 means dark mode
        Result := Reg.ReadInteger('AppsUseLightTheme') = 0;
      except
        Result := False;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TThemeManager.UpdateColors;
begin
  case FCurrentMode of
    tmSystem:
      FIsDarkMode := IsWindowsDarkMode;
    tmLight:
      FIsDarkMode := False;
    tmDark:
      FIsDarkMode := True;
  end;

  if FIsDarkMode then
    FColors := DarkColors
  else
    FColors := LightColors;
end;

procedure TThemeManager.SetThemeMode(AMode: TThemeMode);
begin
  if FCurrentMode <> AMode then
  begin
    FCurrentMode := AMode;
    UpdateColors;
    if Assigned(FOnThemeChanged) then
      FOnThemeChanged(Self);
  end;
end;

procedure TThemeManager.Refresh;
var
  WasDark: Boolean;
begin
  WasDark := FIsDarkMode;
  UpdateColors;
  if (WasDark <> FIsDarkMode) and Assigned(FOnThemeChanged) then
    FOnThemeChanged(Self);
end;

end.
