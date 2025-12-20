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
  System.IOUtils,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Themes;

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
    FAvailableStyles: TArray<string>;
    FCurrentStyleName: string;

    procedure UpdateColors;
    function IsWindowsDarkMode: Boolean;
    function GetLightStyleName: string;
    function GetDarkStyleName: string;
  public
    class function Instance: TThemeManager;
    class destructor Destroy;

    /// <summary>
    /// Loads .vsf style files from the specified directory.
    /// </summary>
    procedure LoadStylesFromDirectory(const ADirectory: string);

    /// <summary>
    /// Returns list of all available style names (compiled + loaded).
    /// </summary>
    function GetAvailableStyles: TArray<string>;

    /// <summary>
    /// Sets the theme mode and applies the appropriate VCL style.
    /// </summary>
    procedure SetThemeMode(AMode: TThemeMode);

    /// <summary>
    /// Sets a specific VCL style by name.
    /// </summary>
    procedure SetStyle(const AStyleName: string);

    /// <summary>
    /// Refreshes theme based on current mode (useful when Windows theme changes).
    /// </summary>
    procedure Refresh;

    property CurrentMode: TThemeMode read FCurrentMode;
    property IsDarkMode: Boolean read FIsDarkMode;
    property Colors: TThemeColors read FColors;
    property CurrentStyleName: string read FCurrentStyleName;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

/// <summary>
/// Returns the global theme manager instance.
/// </summary>
function Theme: TThemeManager; inline;

implementation

uses
  System.StrUtils,
  System.Win.Registry,
  App.Logger;

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

  // Known dark style names (case-insensitive partial match)
  DarkStylePatterns: array[0..5] of string = (
    'Dark', 'Carbon', 'Onyx', 'Slate', 'Obsidian', 'Charcoal'
  );

function StyleExists(const AStyleName: string): Boolean;
var
  Styles: TArray<string>;
  S: string;
begin
  Result := False;
  Styles := TStyleManager.StyleNames;
  for S in Styles do
  begin
    if SameText(S, AStyleName) then
      Exit(True);
  end;
end;

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
    FInstance.FCurrentStyleName := TStyleManager.ActiveStyle.Name;
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

procedure TThemeManager.LoadStylesFromDirectory(const ADirectory: string);
var
  Files: TArray<string>;
  FilePath: string;
  FullDir: string;
  StyleName: string;
begin
  // Resolve relative path from exe directory
  if TPath.IsRelativePath(ADirectory) then
    FullDir := TPath.Combine(ExtractFilePath(ParamStr(0)), ADirectory)
  else
    FullDir := ADirectory;

  Log('[Theme] LoadStylesFromDirectory: "%s"', [FullDir]);

  if not TDirectory.Exists(FullDir) then
  begin
    Log('[Theme] LoadStylesFromDirectory: Directory does not exist');
    Exit;
  end;

  Files := TDirectory.GetFiles(FullDir, '*.vsf');
  Log('[Theme] LoadStylesFromDirectory: Found %d .vsf files', [Length(Files)]);

  for FilePath in Files do
  begin
    try
      if TStyleManager.IsValidStyle(FilePath) then
      begin
        // Get style name without loading
        StyleName := TPath.GetFileNameWithoutExtension(FilePath);
        // Check if already loaded
        if not StyleExists(StyleName) then
        begin
          TStyleManager.LoadFromFile(FilePath);
          Log('[Theme] LoadStylesFromDirectory: Loaded style "%s"', [StyleName]);
        end
        else
          Log('[Theme] LoadStylesFromDirectory: Style "%s" already loaded', [StyleName]);
      end
      else
        Log('[Theme] LoadStylesFromDirectory: Invalid style file "%s"', [FilePath]);
    except
      on E: Exception do
        Log('[Theme] LoadStylesFromDirectory: Failed to load "%s": %s', [FilePath, E.Message]);
    end;
  end;

  // Update available styles list
  FAvailableStyles := TStyleManager.StyleNames;
  Log('[Theme] LoadStylesFromDirectory: Total available styles: %d', [Length(FAvailableStyles)]);
end;

function TThemeManager.GetAvailableStyles: TArray<string>;
begin
  Result := TStyleManager.StyleNames;
end;

function TThemeManager.GetLightStyleName: string;
var
  Styles: TArray<string>;
  S: string;
begin
  Styles := TStyleManager.StyleNames;

  // Prefer Windows11 or Windows10 for light theme
  for S in Styles do
  begin
    if SameText(S, 'Windows11') or SameText(S, 'Windows 11') then
      Exit(S);
  end;

  for S in Styles do
  begin
    if SameText(S, 'Windows10') or SameText(S, 'Windows 10') then
      Exit(S);
  end;

  // Fallback to 'Windows' or first non-dark style
  for S in Styles do
  begin
    if SameText(S, 'Windows') then
      Exit(S);
  end;

  // Return first style that doesn't contain dark patterns
  for S in Styles do
  begin
    if not ContainsText(S, 'Dark') then
      Exit(S);
  end;

  // Last resort: return default
  Result := 'Windows';
end;

function TThemeManager.GetDarkStyleName: string;
var
  Styles: TArray<string>;
  S: string;
begin
  Styles := TStyleManager.StyleNames;

  // Prefer Windows11 Dark
  for S in Styles do
  begin
    if ContainsText(S, 'Windows11') and ContainsText(S, 'Dark') then
      Exit(S);
    if ContainsText(S, 'Windows 11') and ContainsText(S, 'Dark') then
      Exit(S);
  end;

  // Try Windows10 Dark
  for S in Styles do
  begin
    if ContainsText(S, 'Windows10') and ContainsText(S, 'Dark') then
      Exit(S);
    if ContainsText(S, 'Windows 10') and ContainsText(S, 'Dark') then
      Exit(S);
  end;

  // Any style with 'Dark' in name
  for S in Styles do
  begin
    if ContainsText(S, 'Dark') then
      Exit(S);
  end;

  // Fallback to Carbon or other known dark styles
  for S in Styles do
  begin
    if ContainsText(S, 'Carbon') or ContainsText(S, 'Onyx') or
       ContainsText(S, 'Slate') or ContainsText(S, 'Obsidian') then
      Exit(S);
  end;

  // No dark style found, return light as fallback
  Result := GetLightStyleName;
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
var
  StyleName: string;
begin
  FCurrentMode := AMode;
  UpdateColors;

  // Determine which style to apply
  case AMode of
    tmSystem:
      if FIsDarkMode then
        StyleName := GetDarkStyleName
      else
        StyleName := GetLightStyleName;
    tmLight:
      StyleName := GetLightStyleName;
    tmDark:
      StyleName := GetDarkStyleName;
  end;

  Log('[Theme] SetThemeMode: Mode=%d, IsDark=%s, Style="%s"',
    [Ord(AMode), BoolToStr(FIsDarkMode, True), StyleName]);

  SetStyle(StyleName);
end;

procedure TThemeManager.SetStyle(const AStyleName: string);
begin
  if AStyleName = '' then
    Exit;

  if not StyleExists(AStyleName) then
  begin
    Log('[Theme] SetStyle: Style "%s" not found', [AStyleName]);
    Exit;
  end;

  if not SameText(FCurrentStyleName, AStyleName) then
  begin
    try
      TStyleManager.SetStyle(AStyleName);
      FCurrentStyleName := AStyleName;
      Log('[Theme] SetStyle: Applied style "%s"', [AStyleName]);

      // Update dark mode flag based on style name
      FIsDarkMode := ContainsText(AStyleName, 'Dark') or
                     ContainsText(AStyleName, 'Carbon') or
                     ContainsText(AStyleName, 'Onyx');

      // Update colors to match
      if FIsDarkMode then
        FColors := DarkColors
      else
        FColors := LightColors;

      if Assigned(FOnThemeChanged) then
        FOnThemeChanged(Self);
    except
      on E: Exception do
        Log('[Theme] SetStyle: Failed to apply style "%s": %s', [AStyleName, E.Message]);
    end;
  end;
end;

procedure TThemeManager.Refresh;
var
  WasDark: Boolean;
begin
  WasDark := FIsDarkMode;
  UpdateColors;

  // If in System mode and darkness changed, reapply style
  if (FCurrentMode = tmSystem) and (WasDark <> FIsDarkMode) then
    SetThemeMode(tmSystem)
  else if (WasDark <> FIsDarkMode) and Assigned(FOnThemeChanged) then
    FOnThemeChanged(Self);
end;

end.
