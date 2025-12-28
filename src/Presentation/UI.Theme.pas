{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       UI Theme Support                                }
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
    /// Returns display name for a style (includes filename if loaded from file).
    /// Format: "StyleName (filename.vsf)" or just "StyleName" for embedded styles.
    /// </summary>
    function GetStyleDisplayName(const AStyleName: string): string;

    /// <summary>
    /// Extracts actual style name from display name.
    /// </summary>
    function GetStyleNameFromDisplay(const ADisplayName: string): string;

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
    FStyleFilenames: TStringList;  // Maps style name -> filename

    function GetFirstAvailableStyle: string;
    function GetCurrentStyleName: string;
  public
    constructor Create;
    destructor Destroy; override;

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
    /// Returns display name for a style (includes filename if loaded from file).
    /// </summary>
    function GetStyleDisplayName(const AStyleName: string): string;

    /// <summary>
    /// Extracts actual style name from display name.
    /// </summary>
    function GetStyleNameFromDisplay(const ADisplayName: string): string;

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
      LogDebug('SafeGetStyleNames: Failed to discover styles: %s', [E.Message], SRC);
      // Return array with just the active style as fallback
      Result := [TStyleManager.ActiveStyle.Name];
    end;
    on E: Exception do
    begin
      LogDebug('SafeGetStyleNames: Unexpected error: %s', [E.Message], SRC);
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

constructor TThemeManager.Create;
begin
  inherited Create;
  FStyleFilenames := TStringList.Create;
  FStyleFilenames.CaseSensitive := False;
  FCurrentStyleName := TStyleManager.ActiveStyle.Name;
end;

destructor TThemeManager.Destroy;
begin
  FStyleFilenames.Free;
  inherited Destroy;
end;

class function TThemeManager.Instance: IThemeManager;
begin
  if FInstance = nil then
    FInstance := TThemeManager.Create;
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

  LogDebug('LoadStylesFromDirectory: "%s"', [FullDir], ClassName);

  if not TDirectory.Exists(FullDir) then
  begin
    LogDebug('LoadStylesFromDirectory: Directory does not exist', ClassName);
    Exit;
  end;

  // Get already registered styles ONCE before iterating files
  RegisteredStyles := SafeGetStyleNames;

  Files := TDirectory.GetFiles(FullDir, '*.vsf');
  LogDebug('LoadStylesFromDirectory: Found %d .vsf files', [Length(Files)], ClassName);

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
          // Track filename for display purposes
          FStyleFilenames.Values[StyleInfo.Name] := FileName;
          LogDebug('LoadStylesFromDirectory: Loaded style "%s" from "%s"', [StyleInfo.Name, FileName], ClassName);
          Inc(LoadedCount);
        end;
      end
      else
        LogDebug('LoadStylesFromDirectory: Invalid style file "%s"', [FileName], ClassName);
    except
      on E: Exception do
        LogDebug('LoadStylesFromDirectory: Failed to load "%s": %s', [FileName, E.Message], ClassName);
    end;
  end;

  LogDebug('LoadStylesFromDirectory: Loaded %d new styles, total available: %d',
    [LoadedCount, Length(SafeGetStyleNames)], ClassName);
end;

function TThemeManager.GetAvailableStyles: TArray<string>;
begin
  Result := SafeGetStyleNames;
end;

function TThemeManager.GetStyleDisplayName(const AStyleName: string): string;
var
  FileName: string;
begin
  FileName := FStyleFilenames.Values[AStyleName];
  if FileName <> '' then
    Result := Format('%s (%s)', [AStyleName, FileName])
  else
    Result := AStyleName;
end;

function TThemeManager.GetStyleNameFromDisplay(const ADisplayName: string): string;
var
  ParenPos: Integer;
begin
  // Extract style name from "StyleName (filename.vsf)" format
  ParenPos := Pos(' (', ADisplayName);
  if ParenPos > 0 then
    Result := Copy(ADisplayName, 1, ParenPos - 1)
  else
    Result := ADisplayName;
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
      LogDebug('SetStyle: Style "%s" not found, falling back to "%s"', [AStyleName, TargetStyle], ClassName);
  end
  else
    TargetStyle := AStyleName;

  if TargetStyle = '' then
  begin
    LogDebug('SetStyle: No styles available', ClassName);
    Exit;
  end;

  if not SameText(FCurrentStyleName, TargetStyle) then
  begin
    try
      TStyleManager.SetStyle(TargetStyle);
      FCurrentStyleName := TargetStyle;
      LogDebug('SetStyle: Applied style "%s"', [TargetStyle], ClassName);
    except
      on E: Exception do
        LogDebug('SetStyle: Failed to apply style "%s": %s', [TargetStyle, E.Message], ClassName);
    end;
  end;
end;

end.
