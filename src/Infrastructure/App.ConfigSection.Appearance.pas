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

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);
    procedure SetShowBatteryLevel(AValue: Boolean);

    procedure SetDefaults;

    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property Theme: string read FTheme write SetTheme;
    property VsfDir: string read FVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write SetConnectedColor;
    property ShowBatteryLevel: Boolean read FShowBatteryLevel write SetShowBatteryLevel;
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

end.
