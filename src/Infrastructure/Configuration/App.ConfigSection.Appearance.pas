{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Appearance Configuration Section                }
{                                                       }
{*******************************************************}

/// <summary>
/// Appearance settings implementation.
/// </summary>
unit App.ConfigSection.Appearance;

interface

uses
  System.SysUtils,
  Vcl.Graphics,
  App.ConfigEnums,
  App.AppearanceConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Appearance settings implementation.
  /// </summary>
  TAppearanceConfigSection = class(TConfigSectionBase, IAppearanceConfig)
  private
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;
    FShowLastSeen: Boolean;
    FLastSeenFormat: TLastSeenFormat;
    FShowDeviceIcons: Boolean;
    FConnectedColor: Integer;
    FShowBatteryLevel: Boolean;
    FListBackgroundSource: TListBackgroundSource;
    FListBackgroundCustomColor: Integer;
    FMainColorSource: TMainColorSource;
    FMainCustomColor: Integer;
    FSecondaryColorSource: TSecondaryColorSource;
    FSecondaryCustomColor: Integer;
    FHoverColorSource: THoverColorSource;
    FHoverCustomColor: Integer;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;
    function GetShowBatteryLevel: Boolean;
    function GetListBackgroundSource: TListBackgroundSource;
    function GetListBackgroundCustomColor: Integer;
    function GetMainColorSource: TMainColorSource;
    function GetMainCustomColor: Integer;
    function GetSecondaryColorSource: TSecondaryColorSource;
    function GetSecondaryCustomColor: Integer;
    function GetHoverColorSource: THoverColorSource;
    function GetHoverCustomColor: Integer;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);
    procedure SetShowBatteryLevel(AValue: Boolean);
    procedure SetListBackgroundSource(AValue: TListBackgroundSource);
    procedure SetListBackgroundCustomColor(AValue: Integer);
    procedure SetMainColorSource(AValue: TMainColorSource);
    procedure SetMainCustomColor(AValue: Integer);
    procedure SetSecondaryColorSource(AValue: TSecondaryColorSource);
    procedure SetSecondaryCustomColor(AValue: Integer);
    procedure SetHoverColorSource(AValue: THoverColorSource);
    procedure SetHoverCustomColor(AValue: Integer);

    procedure SetDefaults;

    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    property VsfDir: string read FVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write SetConnectedColor;
    property ShowBatteryLevel: Boolean read FShowBatteryLevel write SetShowBatteryLevel;
    property ListBackgroundSource: TListBackgroundSource read FListBackgroundSource write SetListBackgroundSource;
    property ListBackgroundCustomColor: Integer read FListBackgroundCustomColor write SetListBackgroundCustomColor;
    property MainColorSource: TMainColorSource read FMainColorSource write SetMainColorSource;
    property MainCustomColor: Integer read FMainCustomColor write SetMainCustomColor;
    property SecondaryColorSource: TSecondaryColorSource read FSecondaryColorSource write SetSecondaryColorSource;
    property SecondaryCustomColor: Integer read FSecondaryCustomColor write SetSecondaryCustomColor;
    property HoverColorSource: THoverColorSource read FHoverColorSource write SetHoverColorSource;
    property HoverCustomColor: Integer read FHoverCustomColor write SetHoverCustomColor;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TAppearanceConfigSection }

constructor TAppearanceConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TAppearanceConfigSection.SetDefaults;
begin
  FShowAddresses := DEF_SHOW_ADDRESSES;
  FTheme := DEF_THEME;
  FVsfDir := DEF_VSF_DIR;
  FShowLastSeen := DEF_SHOW_LAST_SEEN;
  FLastSeenFormat := DEF_LAST_SEEN_FORMAT;
  FShowDeviceIcons := DEF_SHOW_DEVICE_ICONS;
  FConnectedColor := DEF_CONNECTED_COLOR;
  FShowBatteryLevel := DEF_SHOW_BATTERY_LEVEL;
  FListBackgroundSource := DEF_LIST_BACKGROUND_SOURCE;
  FListBackgroundCustomColor := DEF_LIST_BACKGROUND_CUSTOM_COLOR;
  FMainColorSource := DEF_MAIN_COLOR_SOURCE;
  FMainCustomColor := DEF_MAIN_CUSTOM_COLOR;
  FSecondaryColorSource := DEF_SECONDARY_COLOR_SOURCE;
  FSecondaryCustomColor := DEF_SECONDARY_CUSTOM_COLOR;
  FHoverColorSource := DEF_HOVER_COLOR_SOURCE;
  FHoverCustomColor := DEF_HOVER_CUSTOM_COLOR;
end;

function TAppearanceConfigSection.GetShowAddresses: Boolean;
begin
  Result := FShowAddresses;
end;

function TAppearanceConfigSection.GetTheme: string;
begin
  Result := FTheme;
end;

function TAppearanceConfigSection.GetVsfDir: string;
begin
  Result := FVsfDir;
end;

function TAppearanceConfigSection.GetShowLastSeen: Boolean;
begin
  Result := FShowLastSeen;
end;

function TAppearanceConfigSection.GetLastSeenFormat: TLastSeenFormat;
begin
  Result := FLastSeenFormat;
end;

function TAppearanceConfigSection.GetShowDeviceIcons: Boolean;
begin
  Result := FShowDeviceIcons;
end;

function TAppearanceConfigSection.GetConnectedColor: Integer;
begin
  Result := FConnectedColor;
end;

procedure TAppearanceConfigSection.SetShowAddresses(AValue: Boolean);
begin
  SetFieldBoolean(FShowAddresses, AValue);
end;

procedure TAppearanceConfigSection.SetTheme(const AValue: string);
begin
  SetFieldString(FTheme, AValue);
end;

procedure TAppearanceConfigSection.SetVsfDir(const AValue: string);
begin
  SetFieldString(FVsfDir, AValue);
end;

procedure TAppearanceConfigSection.SetShowLastSeen(AValue: Boolean);
begin
  SetFieldBoolean(FShowLastSeen, AValue);
end;

procedure TAppearanceConfigSection.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  if FLastSeenFormat <> AValue then
  begin
    FLastSeenFormat := AValue;
    NotifyModified;
  end;
end;

procedure TAppearanceConfigSection.SetShowDeviceIcons(AValue: Boolean);
begin
  SetFieldBoolean(FShowDeviceIcons, AValue);
end;

procedure TAppearanceConfigSection.SetConnectedColor(AValue: Integer);
begin
  SetFieldInteger(FConnectedColor, AValue);
end;

function TAppearanceConfigSection.GetShowBatteryLevel: Boolean;
begin
  Result := FShowBatteryLevel;
end;

procedure TAppearanceConfigSection.SetShowBatteryLevel(AValue: Boolean);
begin
  SetFieldBoolean(FShowBatteryLevel, AValue);
end;

function TAppearanceConfigSection.GetListBackgroundSource: TListBackgroundSource;
begin
  Result := FListBackgroundSource;
end;

procedure TAppearanceConfigSection.SetListBackgroundSource(AValue: TListBackgroundSource);
begin
  if FListBackgroundSource <> AValue then
  begin
    FListBackgroundSource := AValue;
    NotifyModified;
  end;
end;

function TAppearanceConfigSection.GetListBackgroundCustomColor: Integer;
begin
  Result := FListBackgroundCustomColor;
end;

procedure TAppearanceConfigSection.SetListBackgroundCustomColor(AValue: Integer);
begin
  SetFieldInteger(FListBackgroundCustomColor, AValue);
end;

function TAppearanceConfigSection.GetMainColorSource: TMainColorSource;
begin
  Result := FMainColorSource;
end;

procedure TAppearanceConfigSection.SetMainColorSource(AValue: TMainColorSource);
begin
  if FMainColorSource <> AValue then
  begin
    FMainColorSource := AValue;
    NotifyModified;
  end;
end;

function TAppearanceConfigSection.GetMainCustomColor: Integer;
begin
  Result := FMainCustomColor;
end;

procedure TAppearanceConfigSection.SetMainCustomColor(AValue: Integer);
begin
  SetFieldInteger(FMainCustomColor, AValue);
end;

function TAppearanceConfigSection.GetSecondaryColorSource: TSecondaryColorSource;
begin
  Result := FSecondaryColorSource;
end;

procedure TAppearanceConfigSection.SetSecondaryColorSource(AValue: TSecondaryColorSource);
begin
  if FSecondaryColorSource <> AValue then
  begin
    FSecondaryColorSource := AValue;
    NotifyModified;
  end;
end;

function TAppearanceConfigSection.GetSecondaryCustomColor: Integer;
begin
  Result := FSecondaryCustomColor;
end;

procedure TAppearanceConfigSection.SetSecondaryCustomColor(AValue: Integer);
begin
  SetFieldInteger(FSecondaryCustomColor, AValue);
end;

function TAppearanceConfigSection.GetHoverColorSource: THoverColorSource;
begin
  Result := FHoverColorSource;
end;

procedure TAppearanceConfigSection.SetHoverColorSource(AValue: THoverColorSource);
begin
  if FHoverColorSource <> AValue then
  begin
    FHoverColorSource := AValue;
    NotifyModified;
  end;
end;

function TAppearanceConfigSection.GetHoverCustomColor: Integer;
begin
  Result := FHoverCustomColor;
end;

procedure TAppearanceConfigSection.SetHoverCustomColor(AValue: Integer);
begin
  SetFieldInteger(FHoverCustomColor, AValue);
end;

end.
