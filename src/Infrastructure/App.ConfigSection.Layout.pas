{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Layout Configuration Section                    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Layout settings implementation.
/// </summary>
unit App.ConfigSection.Layout;

interface

uses
  System.SysUtils,
  App.LayoutConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Layout settings implementation.
  /// </summary>
  TLayoutConfigSection = class(TInterfacedObject, ILayoutConfig)
  private
    FItemHeight: Integer;
    FItemPadding: Integer;
    FItemMargin: Integer;
    FIconSize: Integer;
    FCornerRadius: Integer;
    FDeviceNameFontSize: Integer;
    FStatusFontSize: Integer;
    FAddressFontSize: Integer;
    FIconFontSize: Integer;
    FItemBorderWidth: Integer;
    FItemBorderColor: Integer;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetItemHeight: Integer;
    function GetItemPadding: Integer;
    function GetItemMargin: Integer;
    function GetIconSize: Integer;
    function GetCornerRadius: Integer;
    function GetDeviceNameFontSize: Integer;
    function GetStatusFontSize: Integer;
    function GetAddressFontSize: Integer;
    function GetIconFontSize: Integer;
    function GetItemBorderWidth: Integer;
    function GetItemBorderColor: Integer;

    procedure SetItemHeight(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemMargin(AValue: Integer);
    procedure SetIconSize(AValue: Integer);
    procedure SetCornerRadius(AValue: Integer);
    procedure SetDeviceNameFontSize(AValue: Integer);
    procedure SetStatusFontSize(AValue: Integer);
    procedure SetAddressFontSize(AValue: Integer);
    procedure SetIconFontSize(AValue: Integer);
    procedure SetItemBorderWidth(AValue: Integer);
    procedure SetItemBorderColor(AValue: Integer);

    procedure SetDefaults;

    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ItemPadding: Integer read FItemPadding write SetItemPadding;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property IconSize: Integer read FIconSize write SetIconSize;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius;
    property DeviceNameFontSize: Integer read FDeviceNameFontSize write SetDeviceNameFontSize;
    property StatusFontSize: Integer read FStatusFontSize write SetStatusFontSize;
    property AddressFontSize: Integer read FAddressFontSize write SetAddressFontSize;
    property IconFontSize: Integer read FIconFontSize write SetIconFontSize;
    property ItemBorderWidth: Integer read FItemBorderWidth write SetItemBorderWidth;
    property ItemBorderColor: Integer read FItemBorderColor write SetItemBorderColor;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TLayoutConfigSection }

constructor TLayoutConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TLayoutConfigSection.SetDefaults;
begin
  FItemHeight := DEF_ITEM_HEIGHT;
  FItemPadding := DEF_ITEM_PADDING;
  FItemMargin := DEF_ITEM_MARGIN;
  FIconSize := DEF_ICON_SIZE;
  FCornerRadius := DEF_CORNER_RADIUS;
  FDeviceNameFontSize := DEF_DEVICE_NAME_FONT_SIZE;
  FStatusFontSize := DEF_STATUS_FONT_SIZE;
  FAddressFontSize := DEF_ADDRESS_FONT_SIZE;
  FIconFontSize := DEF_ICON_FONT_SIZE;
  FItemBorderWidth := DEF_ITEM_BORDER_WIDTH;
  FItemBorderColor := DEF_ITEM_BORDER_COLOR;
end;

function TLayoutConfigSection.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TLayoutConfigSection.GetItemPadding: Integer;
begin
  Result := FItemPadding;
end;

function TLayoutConfigSection.GetItemMargin: Integer;
begin
  Result := FItemMargin;
end;

function TLayoutConfigSection.GetIconSize: Integer;
begin
  Result := FIconSize;
end;

function TLayoutConfigSection.GetCornerRadius: Integer;
begin
  Result := FCornerRadius;
end;

function TLayoutConfigSection.GetDeviceNameFontSize: Integer;
begin
  Result := FDeviceNameFontSize;
end;

function TLayoutConfigSection.GetStatusFontSize: Integer;
begin
  Result := FStatusFontSize;
end;

function TLayoutConfigSection.GetAddressFontSize: Integer;
begin
  Result := FAddressFontSize;
end;

function TLayoutConfigSection.GetIconFontSize: Integer;
begin
  Result := FIconFontSize;
end;

function TLayoutConfigSection.GetItemBorderWidth: Integer;
begin
  Result := FItemBorderWidth;
end;

function TLayoutConfigSection.GetItemBorderColor: Integer;
begin
  Result := FItemBorderColor;
end;

procedure TLayoutConfigSection.SetItemHeight(AValue: Integer);
begin
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemPadding(AValue: Integer);
begin
  if FItemPadding <> AValue then
  begin
    FItemPadding := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemMargin(AValue: Integer);
begin
  if FItemMargin <> AValue then
  begin
    FItemMargin := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetIconSize(AValue: Integer);
begin
  if FIconSize <> AValue then
  begin
    FIconSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetCornerRadius(AValue: Integer);
begin
  if FCornerRadius <> AValue then
  begin
    FCornerRadius := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetDeviceNameFontSize(AValue: Integer);
begin
  if FDeviceNameFontSize <> AValue then
  begin
    FDeviceNameFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetStatusFontSize(AValue: Integer);
begin
  if FStatusFontSize <> AValue then
  begin
    FStatusFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetAddressFontSize(AValue: Integer);
begin
  if FAddressFontSize <> AValue then
  begin
    FAddressFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetIconFontSize(AValue: Integer);
begin
  if FIconFontSize <> AValue then
  begin
    FIconFontSize := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemBorderWidth(AValue: Integer);
begin
  if FItemBorderWidth <> AValue then
  begin
    FItemBorderWidth := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TLayoutConfigSection.SetItemBorderColor(AValue: Integer);
begin
  if FItemBorderColor <> AValue then
  begin
    FItemBorderColor := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
