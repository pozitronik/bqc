{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Tray Configuration Section              }
{                                                       }
{*******************************************************}

/// <summary>
/// Battery tray icon settings implementation.
/// Controls per-device battery level tray icons.
/// </summary>
unit App.ConfigSection.BatteryTray;

interface

uses
  System.SysUtils,
  Vcl.Graphics,
  App.ConfigEnums,
  App.BatteryTrayConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Battery tray icon settings implementation.
  /// </summary>
  TBatteryTrayConfigSection = class(TConfigSectionBase, IBatteryTrayConfig)
  private
    FShowBatteryTrayIcons: Boolean;
    FDefaultIconColor: TColor;
    FDefaultBackgroundColor: TColor;
    FDefaultShowNumericValue: Boolean;
    FDefaultLowBatteryThreshold: Integer;
    FDefaultNotifyLowBattery: Boolean;
    FDefaultNotifyFullyCharged: Boolean;
    FDefaultOutlineColorMode: TOutlineColorMode;
    FDefaultCustomOutlineColor: TColor;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetShowBatteryTrayIcons: Boolean;
    function GetDefaultIconColor: TColor;
    function GetDefaultBackgroundColor: TColor;
    function GetDefaultShowNumericValue: Boolean;
    function GetDefaultLowBatteryThreshold: Integer;
    function GetDefaultNotifyLowBattery: Boolean;
    function GetDefaultNotifyFullyCharged: Boolean;
    function GetDefaultOutlineColorMode: TOutlineColorMode;
    function GetDefaultCustomOutlineColor: TColor;

    procedure SetShowBatteryTrayIcons(AValue: Boolean);
    procedure SetDefaultIconColor(AValue: TColor);
    procedure SetDefaultBackgroundColor(AValue: TColor);
    procedure SetDefaultShowNumericValue(AValue: Boolean);
    procedure SetDefaultLowBatteryThreshold(AValue: Integer);
    procedure SetDefaultNotifyLowBattery(AValue: Boolean);
    procedure SetDefaultNotifyFullyCharged(AValue: Boolean);
    procedure SetDefaultOutlineColorMode(AValue: TOutlineColorMode);
    procedure SetDefaultCustomOutlineColor(AValue: TColor);

    procedure SetDefaults;

    property ShowBatteryTrayIcons: Boolean read FShowBatteryTrayIcons write SetShowBatteryTrayIcons;
    property DefaultIconColor: TColor read FDefaultIconColor write SetDefaultIconColor;
    property DefaultBackgroundColor: TColor read FDefaultBackgroundColor write SetDefaultBackgroundColor;
    property DefaultShowNumericValue: Boolean read FDefaultShowNumericValue write SetDefaultShowNumericValue;
    property DefaultLowBatteryThreshold: Integer read FDefaultLowBatteryThreshold write SetDefaultLowBatteryThreshold;
    property DefaultNotifyLowBattery: Boolean read FDefaultNotifyLowBattery write SetDefaultNotifyLowBattery;
    property DefaultNotifyFullyCharged: Boolean read FDefaultNotifyFullyCharged write SetDefaultNotifyFullyCharged;
    property DefaultOutlineColorMode: TOutlineColorMode read FDefaultOutlineColorMode write SetDefaultOutlineColorMode;
    property DefaultCustomOutlineColor: TColor read FDefaultCustomOutlineColor write SetDefaultCustomOutlineColor;
  end;

const
  // Default values for battery tray settings
  DEF_SHOW_BATTERY_TRAY_ICONS = False;
  DEF_BATTERY_ICON_COLOR = TColor($00AA00);  // Green
  DEF_BATTERY_BACKGROUND_COLOR = TColor($1FFFFFFF);  // Transparent (clNone equivalent)
  DEF_SHOW_NUMERIC_VALUE = False;
  DEF_LOW_BATTERY_THRESHOLD = 20;
  DEF_NOTIFY_LOW_BATTERY = True;
  DEF_NOTIFY_FULLY_CHARGED = False;
  DEF_OUTLINE_COLOR_MODE = ocmAuto;
  DEF_CUSTOM_OUTLINE_COLOR = TColor($000000);  // Black

  // Validation ranges
  MIN_LOW_BATTERY_THRESHOLD = 5;
  MAX_LOW_BATTERY_THRESHOLD = 50;

implementation

{ TBatteryTrayConfigSection }

constructor TBatteryTrayConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TBatteryTrayConfigSection.SetDefaults;
begin
  FShowBatteryTrayIcons := DEF_SHOW_BATTERY_TRAY_ICONS;
  FDefaultIconColor := DEF_BATTERY_ICON_COLOR;
  FDefaultBackgroundColor := DEF_BATTERY_BACKGROUND_COLOR;
  FDefaultShowNumericValue := DEF_SHOW_NUMERIC_VALUE;
  FDefaultLowBatteryThreshold := DEF_LOW_BATTERY_THRESHOLD;
  FDefaultNotifyLowBattery := DEF_NOTIFY_LOW_BATTERY;
  FDefaultNotifyFullyCharged := DEF_NOTIFY_FULLY_CHARGED;
  FDefaultOutlineColorMode := DEF_OUTLINE_COLOR_MODE;
  FDefaultCustomOutlineColor := DEF_CUSTOM_OUTLINE_COLOR;
end;

function TBatteryTrayConfigSection.GetShowBatteryTrayIcons: Boolean;
begin
  Result := FShowBatteryTrayIcons;
end;

function TBatteryTrayConfigSection.GetDefaultIconColor: TColor;
begin
  Result := FDefaultIconColor;
end;

function TBatteryTrayConfigSection.GetDefaultBackgroundColor: TColor;
begin
  Result := FDefaultBackgroundColor;
end;

function TBatteryTrayConfigSection.GetDefaultShowNumericValue: Boolean;
begin
  Result := FDefaultShowNumericValue;
end;

function TBatteryTrayConfigSection.GetDefaultLowBatteryThreshold: Integer;
begin
  Result := FDefaultLowBatteryThreshold;
end;

function TBatteryTrayConfigSection.GetDefaultNotifyLowBattery: Boolean;
begin
  Result := FDefaultNotifyLowBattery;
end;

function TBatteryTrayConfigSection.GetDefaultNotifyFullyCharged: Boolean;
begin
  Result := FDefaultNotifyFullyCharged;
end;

function TBatteryTrayConfigSection.GetDefaultOutlineColorMode: TOutlineColorMode;
begin
  Result := FDefaultOutlineColorMode;
end;

function TBatteryTrayConfigSection.GetDefaultCustomOutlineColor: TColor;
begin
  Result := FDefaultCustomOutlineColor;
end;

procedure TBatteryTrayConfigSection.SetShowBatteryTrayIcons(AValue: Boolean);
begin
  SetFieldBoolean(FShowBatteryTrayIcons, AValue);
end;

procedure TBatteryTrayConfigSection.SetDefaultIconColor(AValue: TColor);
begin
  if FDefaultIconColor <> AValue then
  begin
    FDefaultIconColor := AValue;
    NotifyModified;
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultBackgroundColor(AValue: TColor);
begin
  if FDefaultBackgroundColor <> AValue then
  begin
    FDefaultBackgroundColor := AValue;
    NotifyModified;
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultShowNumericValue(AValue: Boolean);
begin
  SetFieldBoolean(FDefaultShowNumericValue, AValue);
end;

procedure TBatteryTrayConfigSection.SetDefaultLowBatteryThreshold(AValue: Integer);
begin
  SetFieldInteger(FDefaultLowBatteryThreshold, AValue);
end;

procedure TBatteryTrayConfigSection.SetDefaultNotifyLowBattery(AValue: Boolean);
begin
  SetFieldBoolean(FDefaultNotifyLowBattery, AValue);
end;

procedure TBatteryTrayConfigSection.SetDefaultNotifyFullyCharged(AValue: Boolean);
begin
  SetFieldBoolean(FDefaultNotifyFullyCharged, AValue);
end;

procedure TBatteryTrayConfigSection.SetDefaultOutlineColorMode(AValue: TOutlineColorMode);
begin
  if FDefaultOutlineColorMode <> AValue then
  begin
    FDefaultOutlineColorMode := AValue;
    NotifyModified;
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultCustomOutlineColor(AValue: TColor);
begin
  if FDefaultCustomOutlineColor <> AValue then
  begin
    FDefaultCustomOutlineColor := AValue;
    NotifyModified;
  end;
end;

end.
