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
  App.BatteryTrayConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Battery tray icon settings implementation.
  /// </summary>
  TBatteryTrayConfigSection = class(TInterfacedObject, IBatteryTrayConfig)
  private
    FShowBatteryTrayIcons: Boolean;
    FDefaultIconColor: TColor;
    FDefaultBackgroundColor: TColor;
    FDefaultShowNumericValue: Boolean;
    FDefaultLowBatteryThreshold: Integer;
    FDefaultNotifyLowBattery: Boolean;
    FDefaultNotifyFullyCharged: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetShowBatteryTrayIcons: Boolean;
    function GetDefaultIconColor: TColor;
    function GetDefaultBackgroundColor: TColor;
    function GetDefaultShowNumericValue: Boolean;
    function GetDefaultLowBatteryThreshold: Integer;
    function GetDefaultNotifyLowBattery: Boolean;
    function GetDefaultNotifyFullyCharged: Boolean;

    procedure SetShowBatteryTrayIcons(AValue: Boolean);
    procedure SetDefaultIconColor(AValue: TColor);
    procedure SetDefaultBackgroundColor(AValue: TColor);
    procedure SetDefaultShowNumericValue(AValue: Boolean);
    procedure SetDefaultLowBatteryThreshold(AValue: Integer);
    procedure SetDefaultNotifyLowBattery(AValue: Boolean);
    procedure SetDefaultNotifyFullyCharged(AValue: Boolean);

    procedure SetDefaults;

    property ShowBatteryTrayIcons: Boolean read FShowBatteryTrayIcons write SetShowBatteryTrayIcons;
    property DefaultIconColor: TColor read FDefaultIconColor write SetDefaultIconColor;
    property DefaultBackgroundColor: TColor read FDefaultBackgroundColor write SetDefaultBackgroundColor;
    property DefaultShowNumericValue: Boolean read FDefaultShowNumericValue write SetDefaultShowNumericValue;
    property DefaultLowBatteryThreshold: Integer read FDefaultLowBatteryThreshold write SetDefaultLowBatteryThreshold;
    property DefaultNotifyLowBattery: Boolean read FDefaultNotifyLowBattery write SetDefaultNotifyLowBattery;
    property DefaultNotifyFullyCharged: Boolean read FDefaultNotifyFullyCharged write SetDefaultNotifyFullyCharged;
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

  // Validation ranges
  MIN_LOW_BATTERY_THRESHOLD = 5;
  MAX_LOW_BATTERY_THRESHOLD = 50;

implementation

{ TBatteryTrayConfigSection }

constructor TBatteryTrayConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
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

procedure TBatteryTrayConfigSection.SetShowBatteryTrayIcons(AValue: Boolean);
begin
  if FShowBatteryTrayIcons <> AValue then
  begin
    FShowBatteryTrayIcons := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultIconColor(AValue: TColor);
begin
  if FDefaultIconColor <> AValue then
  begin
    FDefaultIconColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultBackgroundColor(AValue: TColor);
begin
  if FDefaultBackgroundColor <> AValue then
  begin
    FDefaultBackgroundColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultShowNumericValue(AValue: Boolean);
begin
  if FDefaultShowNumericValue <> AValue then
  begin
    FDefaultShowNumericValue := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultLowBatteryThreshold(AValue: Integer);
begin
  if FDefaultLowBatteryThreshold <> AValue then
  begin
    FDefaultLowBatteryThreshold := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultNotifyLowBattery(AValue: Boolean);
begin
  if FDefaultNotifyLowBattery <> AValue then
  begin
    FDefaultNotifyLowBattery := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TBatteryTrayConfigSection.SetDefaultNotifyFullyCharged(AValue: Boolean);
begin
  if FDefaultNotifyFullyCharged <> AValue then
  begin
    FDefaultNotifyFullyCharged := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
