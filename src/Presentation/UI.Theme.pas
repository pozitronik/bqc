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
  /// Theme manager for the application.
  /// Provides simple, explicit style selection without guessing.
  /// Singleton pattern.
  /// </summary>
  TThemeManager = class
  private class var
    FInstance: TThemeManager;
  private
    FCurrentStyleName: string;

    function GetFirstAvailableStyle: string;
  public
    class function Instance: TThemeManager;
    class destructor Destroy;

    /// <summary>
    /// Loads .vsf style files from the specified directory.
    /// Skips already registered styles.
    /// </summary>
    procedure LoadStylesFromDirectory(const ADirectory: string);

    /// <summary>
    /// Returns list of all available style names (embedded + loaded from files).
    /// </summary>
    function GetAvailableStyles: TArray<string>;

    /// <summary>
    /// Sets a specific VCL style by exact name.
    /// Falls back to first embedded style if not found.
    /// </summary>
    procedure SetStyle(const AStyleName: string);

    property CurrentStyleName: string read FCurrentStyleName;
  end;

/// <summary>
/// Returns the global theme manager instance.
/// </summary>
function Theme: TThemeManager; inline;

implementation

uses
  System.StrUtils,
  App.Logger;

const
  // Fallback style name when no styles are available
  FALLBACK_STYLE_NAME = 'Windows';

/// <summary>
/// Safely gets style names, catching exceptions from DiscoverStyleResources
/// when project references styles that don't exist.
/// </summary>
function SafeGetStyleNames: TArray<string>;
begin
  try
    Result := TStyleManager.StyleNames;
  except
    on E: ECustomStyleException do
    begin
      Log('[Theme] SafeGetStyleNames: Failed to discover styles: %s', [E.Message]);
      // Return array with just the active style as fallback
      Result := [TStyleManager.ActiveStyle.Name];
    end;
    on E: Exception do
    begin
      Log('[Theme] SafeGetStyleNames: Unexpected error: %s', [E.Message]);
      Result := [];
    end;
  end;
end;

function StyleExists(const AStyleName: string): Boolean;
var
  Styles: TArray<string>;
  S: string;
begin
  Result := False;
  Styles := SafeGetStyleNames;
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
    FInstance.FCurrentStyleName := TStyleManager.ActiveStyle.Name;
  end;
  Result := FInstance;
end;

class destructor TThemeManager.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TThemeManager.GetFirstAvailableStyle: string;
var
  Styles: TArray<string>;
begin
  Styles := SafeGetStyleNames;
  if Length(Styles) > 0 then
    Result := Styles[0]
  else
    Result := FALLBACK_STYLE_NAME;
end;

procedure TThemeManager.LoadStylesFromDirectory(const ADirectory: string);
var
  Files: TArray<string>;
  FilePath: string;
  FullDir: string;
  FileName: string;
  StyleInfo: TStyleInfo;
  RegisteredStyles: TArray<string>;
  LoadedCount: Integer;

  function IsStyleRegistered(const AName: string): Boolean;
  var
    S: string;
  begin
    Result := False;
    for S in RegisteredStyles do
      if SameText(S, AName) then
        Exit(True);
  end;

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

  // Get already registered styles ONCE before iterating files
  RegisteredStyles := SafeGetStyleNames;

  Files := TDirectory.GetFiles(FullDir, '*.vsf');
  Log('[Theme] LoadStylesFromDirectory: Found %d .vsf files', [Length(Files)]);

  LoadedCount := 0;
  for FilePath in Files do
  begin
    FileName := TPath.GetFileName(FilePath);
    try
      // Get style info from file header
      if TStyleManager.IsValidStyle(FilePath, StyleInfo) then
      begin
        // Only load if not already registered
        if not IsStyleRegistered(StyleInfo.Name) then
        begin
          TStyleManager.LoadFromFile(FilePath);
          Log('[Theme] LoadStylesFromDirectory: Loaded style "%s" from "%s"', [StyleInfo.Name, FileName]);
          Inc(LoadedCount);
        end;
      end
      else
        Log('[Theme] LoadStylesFromDirectory: Invalid style file "%s"', [FileName]);
    except
      on E: Exception do
        Log('[Theme] LoadStylesFromDirectory: Failed to load "%s": %s', [FileName, E.Message]);
    end;
  end;

  Log('[Theme] LoadStylesFromDirectory: Loaded %d new styles, total available: %d',
    [LoadedCount, Length(SafeGetStyleNames)]);
end;

function TThemeManager.GetAvailableStyles: TArray<string>;
begin
  Result := SafeGetStyleNames;
end;

procedure TThemeManager.SetStyle(const AStyleName: string);
var
  TargetStyle: string;
begin
  // Determine target style
  if (AStyleName = '') or not StyleExists(AStyleName) then
  begin
    TargetStyle := GetFirstAvailableStyle;
    if AStyleName <> '' then
      Log('[Theme] SetStyle: Style "%s" not found, falling back to "%s"', [AStyleName, TargetStyle]);
  end
  else
    TargetStyle := AStyleName;

  if TargetStyle = '' then
  begin
    Log('[Theme] SetStyle: No styles available');
    Exit;
  end;

  if not SameText(FCurrentStyleName, TargetStyle) then
  begin
    try
      TStyleManager.SetStyle(TargetStyle);
      FCurrentStyleName := TargetStyle;
      Log('[Theme] SetStyle: Applied style "%s"', [TargetStyle]);
    except
      on E: Exception do
        Log('[Theme] SetStyle: Failed to apply style "%s": %s', [TargetStyle, E.Message]);
    end;
  end;
end;

end.
