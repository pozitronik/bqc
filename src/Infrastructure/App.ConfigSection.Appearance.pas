{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Appearance Configuration Section                }
{                                                       }
{       Copyright (c) 2024                              }
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
  TAppearanceConfigSection = class(TInterfacedObject, IAppearanceConfig)
  private
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;
    FShowLastSeen: Boolean;
    FLastSeenFormat: TLastSeenFormat;
    FShowDeviceIcons: Boolean;
    FConnectedColor: Integer;
    FShowBatteryLevel: Boolean;
    FOnModified: TModifiedNotifier;
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
  inherited Create;
  FOnModified := AOnModified;
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
  if FShowAddresses <> AValue then
  begin
    FShowAddresses := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetTheme(const AValue: string);
begin
  if FTheme <> AValue then
  begin
    FTheme := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetVsfDir(const AValue: string);
begin
  if FVsfDir <> AValue then
  begin
    FVsfDir := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetShowLastSeen(AValue: Boolean);
begin
  if FShowLastSeen <> AValue then
  begin
    FShowLastSeen := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  if FLastSeenFormat <> AValue then
  begin
    FLastSeenFormat := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetShowDeviceIcons(AValue: Boolean);
begin
  if FShowDeviceIcons <> AValue then
  begin
    FShowDeviceIcons := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TAppearanceConfigSection.SetConnectedColor(AValue: Integer);
begin
  if FConnectedColor <> AValue then
  begin
    FConnectedColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

function TAppearanceConfigSection.GetShowBatteryLevel: Boolean;
begin
  Result := FShowBatteryLevel;
end;

procedure TAppearanceConfigSection.SetShowBatteryLevel(AValue: Boolean);
begin
  if FShowBatteryLevel <> AValue then
  begin
    FShowBatteryLevel := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
