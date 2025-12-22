{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Custom Device List Control                      }
{                                                       }
{       Owner-draw list for Bluetooth devices with      }
{       modern Windows 11 styling.                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.DeviceList;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Themes,
  Bluetooth.Types,
  App.ConfigInterfaces;

type
  TDeviceClickEvent = procedure(Sender: TObject; const ADevice: TBluetoothDeviceInfo) of object;

  /// <summary>
  /// Pre-processed display item for device list rendering.
  /// Contains all data needed for display without further config lookups.
  /// Created by presenter, consumed by view (Information Expert pattern).
  /// </summary>
  TDeviceDisplayItem = record
    /// <summary>Original device data from Bluetooth service.</summary>
    Device: TBluetoothDeviceInfo;

    /// <summary>Display name (alias if set, otherwise device name).</summary>
    DisplayName: string;

    /// <summary>Whether device is pinned to top of list.</summary>
    IsPinned: Boolean;

    /// <summary>Effective device type (override or auto-detected).</summary>
    EffectiveDeviceType: TBluetoothDeviceType;

    /// <summary>Pre-formatted last seen text based on appearance config.</summary>
    LastSeenText: string;

    /// <summary>Raw last seen value for sorting.</summary>
    LastSeen: TDateTime;

    /// <summary>Sort group: 0=Pinned, 1=Connected (not pinned), 2=Disconnected.</summary>
    SortGroup: Integer;

    /// <summary>Creates a display item from device and config data.</summary>
    class function Create(const ADevice: TBluetoothDeviceInfo;
      const ADisplayName: string; AIsPinned: Boolean;
      AEffectiveDeviceType: TBluetoothDeviceType;
      const ALastSeenText: string; ALastSeen: TDateTime;
      ASortGroup: Integer): TDeviceDisplayItem; static;
  end;

  TDeviceDisplayItemArray = TArray<TDeviceDisplayItem>;

  TDeviceDisplayItemClickEvent = procedure(Sender: TObject;
    const AItem: TDeviceDisplayItem) of object;

  /// <summary>
  /// Custom device list control with owner-draw rendering.
  /// Displays pre-processed TDeviceDisplayItem records.
  /// </summary>
  TDeviceListBox = class(TCustomControl)
  private
    FDisplayItems: TDeviceDisplayItemArray;
    FHoverIndex: Integer;
    FSelectedIndex: Integer;
    FScrollPos: Integer;
    FMaxScroll: Integer;
    FShowAddresses: Boolean;
    FOnDeviceClick: TDeviceClickEvent;
    FOnDisplayItemClick: TDeviceDisplayItemClickEvent;
    FOnSelectionChanged: TNotifyEvent;

    // Injected configuration interfaces (for layout/appearance only)
    FLayoutConfig: ILayoutConfig;
    FAppearanceConfig: IAppearanceConfig;

    function GetLayoutConfig: ILayoutConfig;
    function GetAppearanceConfig: IAppearanceConfig;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetSelectedIndex(AValue: Integer);
    function GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
    function GetDeviceCount: Integer;
    function GetItemCount: Integer;
    function GetItemRect(AIndex: Integer): TRect;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure UpdateScrollRange;
    procedure ScrollTo(APos: Integer);

    procedure DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
      const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
    procedure DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
      ADeviceType: TBluetoothDeviceType);
    function GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure UpdateScrollBar;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoEnter; override;
    procedure DoExit; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure SetDisplayItems(const AItems: TDeviceDisplayItemArray);
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);
    function GetSelectedDevice: TBluetoothDeviceInfo;
    function GetSelectedDisplayItem: TDeviceDisplayItem;
    procedure EnsureVisible(AIndex: Integer);

    property Devices[AIndex: Integer]: TBluetoothDeviceInfo read GetDevice;
    property DeviceCount: Integer read GetDeviceCount;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property OnDeviceClick: TDeviceClickEvent read FOnDeviceClick write FOnDeviceClick;
    property OnDisplayItemClick: TDeviceDisplayItemClickEvent read FOnDisplayItemClick write FOnDisplayItemClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;

    // Dependency injection properties (optional - uses Bootstrap fallback if not set)
    property LayoutConfig: ILayoutConfig read GetLayoutConfig write FLayoutConfig;
    property AppearanceConfig: IAppearanceConfig read GetAppearanceConfig write FAppearanceConfig;

  published
    property Align;
    property Anchors;
    property Enabled;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
  end;

implementation

uses
  System.Math,
  App.Bootstrap,
  UI.ListGeometry;

const
  // Font names used for rendering
  FONT_UI = 'Segoe UI';
  FONT_ICONS = 'Segoe MDL2 Assets';

  // Icon characters from Segoe MDL2 Assets
  ICON_PIN = #$E718;
  ICON_HEADPHONE = #$E7F6;
  ICON_MICROPHONE = #$E720;
  ICON_KEYBOARD = #$E765;
  ICON_MOUSE = #$E962;
  ICON_GAMEPAD = #$E7FC;
  ICON_COMPUTER = #$E7F8;
  ICON_PHONE = #$E8EA;
  ICON_INPUT_DEVICE = #$E961;
  ICON_BLUETOOTH = #$E702;

  // Layout spacing constants
  FOCUS_RECT_INSET = 2;        // Pixels to inset focus rectangle from item bounds
  PIN_ICON_WIDTH = 12;         // Width reserved for pin icon
  PIN_ICON_FONT_SIZE = 10;     // Font size for pin icon
  ADDRESS_SPACING = 8;         // Space between device name and address

  // Default control dimensions
  DEFAULT_CONTROL_WIDTH = 300;
  DEFAULT_CONTROL_HEIGHT = 400;

{ TDeviceDisplayItem }

class function TDeviceDisplayItem.Create(const ADevice: TBluetoothDeviceInfo;
  const ADisplayName: string; AIsPinned: Boolean;
  AEffectiveDeviceType: TBluetoothDeviceType;
  const ALastSeenText: string; ALastSeen: TDateTime;
  ASortGroup: Integer): TDeviceDisplayItem;
begin
  Result.Device := ADevice;
  Result.DisplayName := ADisplayName;
  Result.IsPinned := AIsPinned;
  Result.EffectiveDeviceType := AEffectiveDeviceType;
  Result.LastSeenText := ALastSeenText;
  Result.LastSeen := ALastSeen;
  Result.SortGroup := ASortGroup;
end;

{ TDeviceListBox }

constructor TDeviceListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayItems := nil;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FScrollPos := 0;
  FMaxScroll := 0;
  FShowAddresses := False;

  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  DoubleBuffered := True;
  Width := DEFAULT_CONTROL_WIDTH;
  Height := DEFAULT_CONTROL_HEIGHT;
end;

function TDeviceListBox.GetLayoutConfig: ILayoutConfig;
begin
  if FLayoutConfig = nil then
    FLayoutConfig := Bootstrap.LayoutConfig;
  Result := FLayoutConfig;
end;

function TDeviceListBox.GetAppearanceConfig: IAppearanceConfig;
begin
  if FAppearanceConfig = nil then
    FAppearanceConfig := Bootstrap.AppearanceConfig;
  Result := FAppearanceConfig;
end;

procedure TDeviceListBox.SetShowAddresses(AValue: Boolean);
begin
  if FShowAddresses <> AValue then
  begin
    FShowAddresses := AValue;
    Invalidate;
  end;
end;

destructor TDeviceListBox.Destroy;
begin
  inherited Destroy;
end;

procedure TDeviceListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_VSCROLL;
end;

procedure TDeviceListBox.Resize;
begin
  inherited;
  UpdateScrollRange;
end;

procedure TDeviceListBox.Clear;
begin
  FDisplayItems := nil;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FScrollPos := 0;
  UpdateScrollRange;
  Invalidate;
end;

function TDeviceListBox.GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
begin
  if (AIndex >= 0) and (AIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[AIndex].Device
  else
    Result := Default(TBluetoothDeviceInfo);
end;

function TDeviceListBox.GetDeviceCount: Integer;
begin
  Result := Length(FDisplayItems);
end;

function TDeviceListBox.GetSelectedDevice: TBluetoothDeviceInfo;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[FSelectedIndex].Device
  else
    Result := Default(TBluetoothDeviceInfo);
end;

function TDeviceListBox.GetSelectedDisplayItem: TDeviceDisplayItem;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[FSelectedIndex]
  else
    Result := Default(TDeviceDisplayItem);
end;

function TDeviceListBox.GetItemCount: Integer;
begin
  Result := Length(FDisplayItems);
end;

procedure TDeviceListBox.SetDisplayItems(const AItems: TDeviceDisplayItemArray);
begin
  FDisplayItems := AItems;
  FHoverIndex := -1;
  if FSelectedIndex >= Length(FDisplayItems) then
    FSelectedIndex := -1;
  FScrollPos := 0;
  UpdateScrollRange;
  Invalidate;
end;

procedure TDeviceListBox.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
var
  I: Integer;
begin
  for I := 0 to High(FDisplayItems) do
  begin
    if FDisplayItems[I].Device.AddressInt = AItem.Device.AddressInt then
    begin
      FDisplayItems[I] := AItem;
      Invalidate;
      Exit;
    end;
  end;
end;

procedure TDeviceListBox.SetSelectedIndex(AValue: Integer);
var
  Count: Integer;
begin
  Count := GetItemCount;
  if AValue < -1 then AValue := -1;
  if AValue >= Count then AValue := Count - 1;

  if FSelectedIndex <> AValue then
  begin
    FSelectedIndex := AValue;
    if FSelectedIndex >= 0 then
      EnsureVisible(FSelectedIndex);
    Invalidate;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

function TDeviceListBox.GetItemRect(AIndex: Integer): TRect;
begin
  Result := TListGeometry.GetItemRect(
    AIndex,
    LayoutConfig.ItemHeight,
    LayoutConfig.ItemMargin,
    ClientWidth,
    FScrollPos
  );
end;

function TDeviceListBox.ItemAtPos(X, Y: Integer): Integer;
begin
  Result := TListGeometry.ItemAtPos(
    X, Y,
    GetItemCount,
    LayoutConfig.ItemHeight,
    LayoutConfig.ItemMargin,
    ClientWidth,
    FScrollPos
  );
end;

procedure TDeviceListBox.UpdateScrollRange;
begin
  FMaxScroll := TListGeometry.CalculateMaxScroll(
    GetItemCount,
    LayoutConfig.ItemHeight,
    LayoutConfig.ItemMargin,
    ClientHeight
  );
  FScrollPos := TListGeometry.ClampScrollPos(FScrollPos, FMaxScroll);
  UpdateScrollBar;
end;

procedure TDeviceListBox.UpdateScrollBar;
var
  SI: TScrollInfo;
  TotalHeight: Integer;
begin
  if not HandleAllocated then
    Exit;

  TotalHeight := TListGeometry.CalculateTotalHeight(
    GetItemCount,
    LayoutConfig.ItemHeight,
    LayoutConfig.ItemMargin
  );

  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_ALL;
  SI.nMin := 0;
  SI.nMax := TotalHeight;
  SI.nPage := ClientHeight;
  SI.nPos := FScrollPos;
  SI.nTrackPos := 0;

  SetScrollInfo(Handle, SB_VERT, SI, True);
end;

procedure TDeviceListBox.ScrollTo(APos: Integer);
begin
  APos := EnsureRange(APos, 0, FMaxScroll);
  if FScrollPos <> APos then
  begin
    FScrollPos := APos;
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TDeviceListBox.EnsureVisible(AIndex: Integer);
var
  NewScrollPos: Integer;
begin
  if (AIndex < 0) or (AIndex >= GetItemCount) then
    Exit;

  NewScrollPos := TListGeometry.ScrollPosToMakeVisible(
    AIndex,
    FScrollPos,
    LayoutConfig.ItemHeight,
    LayoutConfig.ItemMargin,
    ClientHeight
  );

  if NewScrollPos <> FScrollPos then
    ScrollTo(NewScrollPos);
end;

procedure TDeviceListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; // Prevent flickering
end;

procedure TDeviceListBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TDeviceListBox.WMVScroll(var Message: TWMVScroll);
var
  NewPos: Integer;
  ItemHeight: Integer;
begin
  NewPos := FScrollPos;
  ItemHeight := LayoutConfig.ItemHeight;

  case Message.ScrollCode of
    SB_LINEUP:
      Dec(NewPos, ItemHeight div 2);
    SB_LINEDOWN:
      Inc(NewPos, ItemHeight div 2);
    SB_PAGEUP:
      Dec(NewPos, ClientHeight - ItemHeight);
    SB_PAGEDOWN:
      Inc(NewPos, ClientHeight - ItemHeight);
    SB_THUMBTRACK, SB_THUMBPOSITION:
      NewPos := Message.Pos;
    SB_TOP:
      NewPos := 0;
    SB_BOTTOM:
      NewPos := FMaxScroll;
  end;

  ScrollTo(NewPos);
end;

procedure TDeviceListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TDeviceListBox.Paint;
var
  I, Count: Integer;
  R: TRect;
  IsHover, IsSelected: Boolean;
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  // Background
  Canvas.Brush.Color := Style.GetSystemColor(clWindow);
  Canvas.FillRect(ClientRect);

  // Draw items
  Count := GetItemCount;
  for I := 0 to Count - 1 do
  begin
    R := GetItemRect(I);

    // Skip items outside visible area
    if R.Bottom < 0 then
      Continue;
    if R.Top > ClientHeight then
      Break;

    IsHover := (I = FHoverIndex);
    IsSelected := (I = FSelectedIndex);

    DrawDisplayItem(Canvas, R, FDisplayItems[I], IsHover, IsSelected);
  end;

  // Focus rectangle
  if Focused and (FSelectedIndex >= 0) then
  begin
    R := GetItemRect(FSelectedIndex);
    InflateRect(R, -FOCUS_RECT_INSET, -FOCUS_RECT_INSET);
    Canvas.Pen.Color := Style.GetSystemColor(clHighlight);
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    Canvas.Pen.Style := psSolid;
  end;
end;

procedure TDeviceListBox.DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
  const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
var
  BgColor: TColor;
  IconRect, TextRect: TRect;
  StatusText: string;
  NameLineTop, StatusLineTop: Integer;
  StatusLineHeight: Integer;
  Style: TCustomStyleServices;
  ItemPadding, IconSize, CornerRadius: Integer;
  DeviceNameFontSize, StatusFontSize, AddressFontSize: Integer;
  ShowDeviceIcons, ShowLastSeen: Boolean;
  ItemBorderWidth: Integer;
  ItemBorderColor: TColor;
begin
  Style := TStyleManager.ActiveStyle;

  // Get layout settings from config
  ItemPadding := LayoutConfig.ItemPadding;
  IconSize := LayoutConfig.IconSize;
  CornerRadius := LayoutConfig.CornerRadius;
  DeviceNameFontSize := LayoutConfig.DeviceNameFontSize;
  StatusFontSize := LayoutConfig.StatusFontSize;
  AddressFontSize := LayoutConfig.AddressFontSize;
  ShowDeviceIcons := AppearanceConfig.ShowDeviceIcons;
  ShowLastSeen := AppearanceConfig.ShowLastSeen;
  ItemBorderWidth := LayoutConfig.ItemBorderWidth;
  ItemBorderColor := TColor(LayoutConfig.ItemBorderColor);

  // Calculate status line height for bottom anchoring
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := StatusFontSize;
  StatusLineHeight := ACanvas.TextHeight('Ay');

  NameLineTop := ARect.Top + ItemPadding;
  StatusLineTop := ARect.Bottom - ItemPadding - StatusLineHeight;

  // Determine background color
  if AIsSelected then
    BgColor := Style.GetSystemColor(clHighlight)
  else if AIsHover then
    BgColor := Style.GetSystemColor(clBtnFace)
  else
    BgColor := Style.GetSystemColor(clWindow);

  // Draw rounded rectangle background
  ACanvas.Pen.Color := BgColor;
  ACanvas.Brush.Color := BgColor;
  ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
    CornerRadius, CornerRadius);

  // Draw border if enabled
  if ItemBorderWidth > 0 then
  begin
    ACanvas.Pen.Color := ItemBorderColor;
    ACanvas.Pen.Width := ItemBorderWidth;
    ACanvas.Brush.Style := bsClear;
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
      CornerRadius, CornerRadius);
    ACanvas.Pen.Width := 1;
    ACanvas.Brush.Style := bsSolid;
  end;

  // Icon area (only if ShowDeviceIcons is enabled)
  if ShowDeviceIcons then
  begin
    IconRect.Left := ARect.Left + ItemPadding;
    IconRect.Top := ARect.Top + ((ARect.Bottom - ARect.Top) - IconSize) div 2;
    IconRect.Right := IconRect.Left + IconSize;
    IconRect.Bottom := IconRect.Top + IconSize;

    // Use pre-computed EffectiveDeviceType from display item
    DrawDeviceIcon(ACanvas, IconRect, AItem.EffectiveDeviceType);

    TextRect.Left := IconRect.Right + ItemPadding;
  end
  else
  begin
    TextRect.Left := ARect.Left + ItemPadding;
  end;

  TextRect.Right := ARect.Right - ItemPadding;
  TextRect.Top := ARect.Top;
  TextRect.Bottom := ARect.Bottom;

  // === TOP LINE: Device name, address, pin icon ===
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := DeviceNameFontSize;
  ACanvas.Font.Style := [];
  if AIsSelected then
    ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
  else
    ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
  ACanvas.Brush.Style := bsClear;

  // Draw pin indicator using pre-computed IsPinned
  if AItem.IsPinned then
  begin
    ACanvas.Font.Name := FONT_ICONS;
    ACanvas.Font.Size := PIN_ICON_FONT_SIZE;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    ACanvas.TextOut(ARect.Right - ItemPadding - PIN_ICON_WIDTH, NameLineTop, ICON_PIN);
    ACanvas.Font.Name := FONT_UI;
    ACanvas.Font.Size := DeviceNameFontSize;
    if AIsSelected then
      ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
    else
      ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
  end;

  // Device name (pre-computed DisplayName)
  ACanvas.TextOut(TextRect.Left, NameLineTop, AItem.DisplayName);

  // Address after device name if enabled
  if FShowAddresses then
  begin
    var AddrLeft := TextRect.Left + ACanvas.TextWidth(AItem.DisplayName) + ADDRESS_SPACING;
    var NameHeight := ACanvas.TextHeight('Ay');
    ACanvas.Font.Size := AddressFontSize;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    var AddrOffset := (NameHeight - ACanvas.TextHeight('Ay')) div 2;
    ACanvas.TextOut(AddrLeft, NameLineTop + AddrOffset, '[' + AItem.Device.AddressString + ']');
  end;

  // === BOTTOM LINE: Connection status (left), LastSeen (right) ===
  ACanvas.Font.Size := StatusFontSize;

  // Connection status (left-aligned)
  if AItem.Device.IsConnected then
  begin
    StatusText := AItem.Device.ConnectionStateText;
    ACanvas.Font.Color := TColor(AppearanceConfig.ConnectedColor);
  end
  else
  begin
    StatusText := AItem.Device.ConnectionStateText;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
  end;

  ACanvas.TextOut(TextRect.Left, StatusLineTop, StatusText);

  // LastSeen (right-aligned) - use pre-formatted LastSeenText
  if ShowLastSeen then
  begin
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    var LastSeenWidth := ACanvas.TextWidth(AItem.LastSeenText);
    ACanvas.TextOut(TextRect.Right - LastSeenWidth, StatusLineTop, AItem.LastSeenText);
  end;

  ACanvas.Brush.Style := bsSolid;
end;

procedure TDeviceListBox.DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
  ADeviceType: TBluetoothDeviceType);
var
  IconChar: Char;
  TextSize: TSize;
  X, Y: Integer;
begin
  IconChar := GetDeviceIconChar(ADeviceType);

  // Use icon font for device icons
  ACanvas.Font.Name := FONT_ICONS;
  ACanvas.Font.Size := LayoutConfig.IconFontSize;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindowText);
  ACanvas.Brush.Style := bsClear;

  TextSize := ACanvas.TextExtent(IconChar);
  X := ARect.Left + (ARect.Width - TextSize.cx) div 2;
  Y := ARect.Top + (ARect.Height - TextSize.cy) div 2;

  ACanvas.TextOut(X, Y, IconChar);

  // Restore font
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Brush.Style := bsSolid;
end;

function TDeviceListBox.GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
begin
  case ADeviceType of
    btAudioOutput,
    btHeadset:      Result := ICON_HEADPHONE;
    btAudioInput:   Result := ICON_MICROPHONE;
    btKeyboard:     Result := ICON_KEYBOARD;
    btMouse:        Result := ICON_MOUSE;
    btGamepad:      Result := ICON_GAMEPAD;
    btComputer:     Result := ICON_COMPUTER;
    btPhone:        Result := ICON_PHONE;
    btHID:          Result := ICON_INPUT_DEVICE;
  else
    Result := ICON_BLUETOOTH;
  end;
end;

procedure TDeviceListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited;

  if CanFocus then
    SetFocus;

  if Button = mbLeft then
  begin
    Index := ItemAtPos(X, Y);
    if Index >= 0 then
      SelectedIndex := Index;
  end;
end;

procedure TDeviceListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited;

  Index := ItemAtPos(X, Y);
  if Index <> FHoverIndex then
  begin
    FHoverIndex := Index;
    Invalidate;
  end;
end;

procedure TDeviceListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited;

  if Button = mbLeft then
  begin
    Index := ItemAtPos(X, Y);
    if (Index >= 0) and (Index = FSelectedIndex) then
    begin
      if Assigned(FOnDisplayItemClick) then
        FOnDisplayItemClick(Self, FDisplayItems[Index])
      else if Assigned(FOnDeviceClick) then
        FOnDeviceClick(Self, FDisplayItems[Index].Device);
    end;
  end;
end;

procedure TDeviceListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Count: Integer;
begin
  inherited;

  Count := GetItemCount;

  case Key of
    VK_UP:
      begin
        if FSelectedIndex > 0 then
          SelectedIndex := FSelectedIndex - 1
        else if (FSelectedIndex = -1) and (Count > 0) then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_DOWN:
      begin
        if FSelectedIndex < Count - 1 then
          SelectedIndex := FSelectedIndex + 1
        else if (FSelectedIndex = -1) and (Count > 0) then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_HOME:
      begin
        if Count > 0 then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_END:
      begin
        if Count > 0 then
          SelectedIndex := Count - 1;
        Key := 0;
      end;

    VK_RETURN, VK_SPACE:
      begin
        if FSelectedIndex >= 0 then
        begin
          if Assigned(FOnDisplayItemClick) then
            FOnDisplayItemClick(Self, FDisplayItems[FSelectedIndex])
          else if Assigned(FOnDeviceClick) then
            FOnDeviceClick(Self, FDisplayItems[FSelectedIndex].Device);
        end;
        Key := 0;
      end;
  end;
end;

function TDeviceListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  ScrollTo(FScrollPos - (WheelDelta div 2));
end;

procedure TDeviceListBox.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TDeviceListBox.DoExit;
begin
  inherited;
  Invalidate;
end;

end.
