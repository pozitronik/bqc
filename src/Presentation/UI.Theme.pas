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
  /// Interface for theme management.
  /// Allows injection and mocking for testability.
  /// </summary>
  IThemeManager = interface
    ['{E8F7A6B5-1234-5678-9ABC-DEF012345678}']

    /// <summary>
    /// Loads .vsf style files from the specified directory.
    /// </summary>
    procedure LoadStylesFromDirectory(const ADirectory: string);

    /// <summary>
    /// Returns list of all available style names.
    /// </summary>
    function GetAvailableStyles: TArray<string>;

    /// <summary>
    /// Sets a specific VCL style by exact name.
    /// </summary>
    procedure SetStyle(const AStyleName: string);

    /// <summary>
    /// Returns the current style name.
    /// </summary>
    function GetCurrentStyleName: string;

    property CurrentStyleName: string read GetCurrentStyleName;
  end;

  /// <summary>
  /// Theme manager for the application.
  /// Provides simple, explicit style selection without guessing.
  /// Implements IThemeManager for dependency injection.
  /// </summary>
  TThemeManager = class(TInterfacedObject, IThemeManager)
  private class var
    FInstance: IThemeManager;
  private
    FCurrentStyleName: string;

    function GetFirstAvailableStyle: string;
    function GetCurrentStyleName: string;
  public
    class function Instance: IThemeManager;
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

    property CurrentStyleName: string read GetCurrentStyleName;
  end;

/// <summary>
/// Returns the global theme manager instance.
/// </summary>
function ThemeManager: IThemeManager;

implementation

uses
  System.StrUtils,
  App.Logger;

const
  // Log source identifier
  SRC = 'Theme';
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
      Log('SafeGetStyleNames: Failed to discover styles: %s', [E.Message], SRC);
      // Return array with just the active style as fallback
      Result := [TStyleManager.ActiveStyle.Name];
    end;
    on E: Exception do
    begin
      Log('SafeGetStyleNames: Unexpected error: %s', [E.Message], SRC);
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

function ThemeManager: IThemeManager;
begin
  Result := TThemeManager.Instance;
end;

{ TThemeManager }

class function TThemeManager.Instance: IThemeManager;
var
  Manager: TThemeManager;
begin
  if FInstance = nil then
  begin
    Manager := TThemeManager.Create;
    Manager.FCurrentStyleName := TStyleManager.ActiveStyle.Name;
    FInstance := Manager;
  end;
  Result := FInstance;
end;

function TThemeManager.GetCurrentStyleName: string;
begin
  Result := FCurrentStyleName;
end;

class destructor TThemeManager.Destroy;
begin
  FInstance := nil;  // Interface reference - will be released automatically
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

  Log('LoadStylesFromDirectory: "%s"', [FullDir], ClassName);

  if not TDirectory.Exists(FullDir) then
  begin
    Log('LoadStylesFromDirectory: Directory does not exist', ClassName);
    Exit;
  end;

  // Get already registered styles ONCE before iterating files
  RegisteredStyles := SafeGetStyleNames;

  Files := TDirectory.GetFiles(FullDir, '*.vsf');
  Log('LoadStylesFromDirectory: Found %d .vsf files', [Length(Files)], ClassName);

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
          Log('LoadStylesFromDirectory: Loaded style "%s" from "%s"', [StyleInfo.Name, FileName], ClassName);
          Inc(LoadedCount);
        end;
      end
      else
        Log('LoadStylesFromDirectory: Invalid style file "%s"', [FileName], ClassName);
    except
      on E: Exception do
        Log('LoadStylesFromDirectory: Failed to load "%s": %s', [FileName, E.Message], ClassName);
    end;
  end;

  Log('LoadStylesFromDirectory: Loaded %d new styles, total available: %d',
    [LoadedCount, Length(SafeGetStyleNames)], ClassName);
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
      Log('SetStyle: Style "%s" not found, falling back to "%s"', [AStyleName, TargetStyle], ClassName);
  end
  else
    TargetStyle := AStyleName;

  if TargetStyle = '' then
  begin
    Log('SetStyle: No styles available', ClassName);
    Exit;
  end;

  if not SameText(FCurrentStyleName, TargetStyle) then
  begin
    try
      TStyleManager.SetStyle(TargetStyle);
      FCurrentStyleName := TargetStyle;
      Log('SetStyle: Applied style "%s"', [TargetStyle], ClassName);
    except
      on E: Exception do
        Log('SetStyle: Failed to apply style "%s": %s', [TargetStyle, E.Message], ClassName);
    end;
  end;
end;

end.
