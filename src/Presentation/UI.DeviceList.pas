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
  /// Context record for item drawing operations.
  /// Holds layout parameters and calculated positions to avoid repeated lookups.
  /// </summary>
  TItemDrawContext = record
    // Layout settings
    ItemPadding: Integer;
    IconSize: Integer;
    CornerRadius: Integer;
    DeviceNameFontSize: Integer;
    StatusFontSize: Integer;
    AddressFontSize: Integer;
    ItemBorderWidth: Integer;
    ItemBorderColor: TColor;
    // Appearance settings
    ShowDeviceIcons: Boolean;
    ShowLastSeen: Boolean;
    ShowAddresses: Boolean;
    ConnectedColor: TColor;
    // Calculated positions
    NameLineTop: Integer;
    StatusLineTop: Integer;
    TextRect: TRect;
    ItemRect: TRect;
    // State
    IsSelected: Boolean;
    IsHover: Boolean;
  end;

  /// <summary>
  /// Cached layout and appearance parameters.
  /// Populated once per paint cycle to avoid repeated config interface queries.
  /// </summary>
  TCachedLayoutParams = record
    // From ILayoutConfig
    ItemPadding: Integer;
    IconSize: Integer;
    CornerRadius: Integer;
    DeviceNameFontSize: Integer;
    StatusFontSize: Integer;
    AddressFontSize: Integer;
    ItemBorderWidth: Integer;
    ItemBorderColor: TColor;
    // From IAppearanceConfig
    ShowDeviceIcons: Boolean;
    ShowLastSeen: Boolean;
    ConnectedColor: TColor;
  end;

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

    /// <summary>Battery status for devices supporting Battery Service.</summary>
    BatteryStatus: TBatteryStatus;

    /// <summary>Pre-formatted battery text (e.g., "85%").</summary>
    BatteryText: string;

    /// <summary>Creates a display item from device and config data.</summary>
    class function Create(const ADevice: TBluetoothDeviceInfo;
      const ADisplayName: string; AIsPinned: Boolean;
      AEffectiveDeviceType: TBluetoothDeviceType;
      const ALastSeenText: string; ALastSeen: TDateTime;
      ASortGroup: Integer; const ABatteryStatus: TBatteryStatus;
      const ABatteryText: string): TDeviceDisplayItem; static;
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

    // Cached layout parameters (refreshed once per paint cycle)
    FCachedLayout: TCachedLayoutParams;

    procedure RefreshLayoutCache;
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
    function CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
      AIsHover, AIsSelected: Boolean): TItemDrawContext;
    procedure DrawItemBackground(ACanvas: TCanvas; const AContext: TItemDrawContext);
    procedure DrawItemTopLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);
    procedure DrawItemBottomLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);
    procedure DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
      ADeviceType: TBluetoothDeviceType);
    function GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
    function GetBatteryIconChar(ALevel: Integer): Char;

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

    // Dependency injection properties (must be set before use)
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
  UI.ListGeometry,
  UI.DeviceFormatter;

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
  ICON_BATTERY_FULL = #$E83F;      // Full battery icon
  ICON_BATTERY_HIGH = #$E859;      // 75% battery
  ICON_BATTERY_MID = #$E85A;       // 50% battery
  ICON_BATTERY_LOW = #$E85B;       // 25% battery
  ICON_BATTERY_EMPTY = #$E850;     // Empty/critical battery

  // Layout spacing constants
  FOCUS_RECT_INSET = 2;        // Pixels to inset focus rectangle from item bounds
  PIN_ICON_WIDTH = 12;         // Width reserved for pin icon
  PIN_ICON_FONT_SIZE = 10;     // Font size for pin icon
  ADDRESS_SPACING = 8;         // Space between device name and address
  BATTERY_SPACING = 4;         // Space between battery text and icon
  BATTERY_FONT_SIZE = 9;       // Font size for battery percentage text
  BATTERY_ICON_FONT_SIZE = 12; // Font size for battery icon

  // Default control dimensions
  DEFAULT_CONTROL_WIDTH = 300;
  DEFAULT_CONTROL_HEIGHT = 400;

{ TDeviceDisplayItem }

{ TODO: Consider refactoring to builder pattern if requirements change.
  Current design analysis (2024-12):

  This factory method takes 7 parameters, which could suggest a builder pattern.
  However, after analysis, the current approach was retained because:

  1. All parameters are REQUIRED - there are no optional fields with defaults.
     A builder pattern primarily benefits when you have optional parameters
     that vary by call site.

  2. Single creation site - TDeviceDisplayItemBuilder.BuildDisplayItem is the
     only production code that calls this method. Test code creates instances
     directly but this is acceptable for testing.

  3. TDeviceDisplayItemBuilder already serves as a conceptual builder - it
     gathers data from multiple sources (device info, config, formatter)
     and produces the display item.

  When to reconsider this decision:
  - If new optional fields are added (e.g., custom icons, badges)
  - If multiple creation sites emerge with different parameter combinations
  - If the parameter list grows beyond 10 parameters

  Alternative patterns considered:
  - Full builder class: TDisplayItemBuilder.ForDevice(D).WithPinned(P)...Build
  - Parameter object record: TDeviceDisplayItemParams with all fields
  - Named parameter simulation via fluent interface

  See also: TDeviceDisplayItemBuilder in UI.DeviceDisplayItemBuilder.pas }
class function TDeviceDisplayItem.Create(const ADevice: TBluetoothDeviceInfo;
  const ADisplayName: string; AIsPinned: Boolean;
  AEffectiveDeviceType: TBluetoothDeviceType;
  const ALastSeenText: string; ALastSeen: TDateTime;
  ASortGroup: Integer; const ABatteryStatus: TBatteryStatus;
  const ABatteryText: string): TDeviceDisplayItem;
begin
  Result.Device := ADevice;
  Result.DisplayName := ADisplayName;
  Result.IsPinned := AIsPinned;
  Result.EffectiveDeviceType := AEffectiveDeviceType;
  Result.LastSeenText := ALastSeenText;
  Result.LastSeen := ALastSeen;
  Result.SortGroup := ASortGroup;
  Result.BatteryStatus := ABatteryStatus;
  Result.BatteryText := ABatteryText;
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
  Result := FLayoutConfig;
end;

function TDeviceListBox.GetAppearanceConfig: IAppearanceConfig;
begin
  Result := FAppearanceConfig;
end;

procedure TDeviceListBox.RefreshLayoutCache;
begin
  // Cache layout config values (avoids repeated interface queries per item)
  if Assigned(FLayoutConfig) then
  begin
    FCachedLayout.ItemPadding := FLayoutConfig.ItemPadding;
    FCachedLayout.IconSize := FLayoutConfig.IconSize;
    FCachedLayout.CornerRadius := FLayoutConfig.CornerRadius;
    FCachedLayout.DeviceNameFontSize := FLayoutConfig.DeviceNameFontSize;
    FCachedLayout.StatusFontSize := FLayoutConfig.StatusFontSize;
    FCachedLayout.AddressFontSize := FLayoutConfig.AddressFontSize;
    FCachedLayout.ItemBorderWidth := FLayoutConfig.ItemBorderWidth;
    FCachedLayout.ItemBorderColor := TColor(FLayoutConfig.ItemBorderColor);
  end;

  // Cache appearance config values
  if Assigned(FAppearanceConfig) then
  begin
    FCachedLayout.ShowDeviceIcons := FAppearanceConfig.ShowDeviceIcons;
    FCachedLayout.ShowLastSeen := FAppearanceConfig.ShowLastSeen;
    FCachedLayout.ConnectedColor := TColor(FAppearanceConfig.ConnectedColor);
  end;
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
  // Refresh cached layout params once per paint (not per item)
  RefreshLayoutCache;

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

function TDeviceListBox.CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
  AIsHover, AIsSelected: Boolean): TItemDrawContext;
var
  StatusLineHeight: Integer;
  IconRect: TRect;
begin
  // Store item rect for reference
  Result.ItemRect := ARect;

  // Use cached layout settings (populated once per paint cycle)
  Result.ItemPadding := FCachedLayout.ItemPadding;
  Result.IconSize := FCachedLayout.IconSize;
  Result.CornerRadius := FCachedLayout.CornerRadius;
  Result.DeviceNameFontSize := FCachedLayout.DeviceNameFontSize;
  Result.StatusFontSize := FCachedLayout.StatusFontSize;
  Result.AddressFontSize := FCachedLayout.AddressFontSize;
  Result.ItemBorderWidth := FCachedLayout.ItemBorderWidth;
  Result.ItemBorderColor := FCachedLayout.ItemBorderColor;

  // Use cached appearance settings
  Result.ShowDeviceIcons := FCachedLayout.ShowDeviceIcons;
  Result.ShowLastSeen := FCachedLayout.ShowLastSeen;
  Result.ConnectedColor := FCachedLayout.ConnectedColor;
  Result.ShowAddresses := FShowAddresses;

  // State
  Result.IsSelected := AIsSelected;
  Result.IsHover := AIsHover;

  // Calculate status line height for bottom anchoring
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := Result.StatusFontSize;
  StatusLineHeight := ACanvas.TextHeight('Ay');

  Result.NameLineTop := ARect.Top + Result.ItemPadding;
  Result.StatusLineTop := ARect.Bottom - Result.ItemPadding - StatusLineHeight;

  // Calculate text rect based on icon visibility
  if Result.ShowDeviceIcons then
  begin
    IconRect.Left := ARect.Left + Result.ItemPadding;
    IconRect.Right := IconRect.Left + Result.IconSize;
    Result.TextRect.Left := IconRect.Right + Result.ItemPadding;
  end
  else
    Result.TextRect.Left := ARect.Left + Result.ItemPadding;

  Result.TextRect.Right := ARect.Right - Result.ItemPadding;
  Result.TextRect.Top := ARect.Top;
  Result.TextRect.Bottom := ARect.Bottom;
end;

procedure TDeviceListBox.DrawItemBackground(ACanvas: TCanvas;
  const AContext: TItemDrawContext);
var
  BgColor: TColor;
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  // Determine background color
  if AContext.IsSelected then
    BgColor := Style.GetSystemColor(clHighlight)
  else if AContext.IsHover then
    BgColor := Style.GetSystemColor(clBtnFace)
  else
    BgColor := Style.GetSystemColor(clWindow);

  // Draw rounded rectangle background
  ACanvas.Pen.Color := BgColor;
  ACanvas.Brush.Color := BgColor;
  ACanvas.RoundRect(
    AContext.ItemRect.Left, AContext.ItemRect.Top,
    AContext.ItemRect.Right, AContext.ItemRect.Bottom,
    AContext.CornerRadius, AContext.CornerRadius);

  // Draw border if enabled
  if AContext.ItemBorderWidth > 0 then
  begin
    ACanvas.Pen.Color := AContext.ItemBorderColor;
    ACanvas.Pen.Width := AContext.ItemBorderWidth;
    ACanvas.Brush.Style := bsClear;
    ACanvas.RoundRect(
      AContext.ItemRect.Left, AContext.ItemRect.Top,
      AContext.ItemRect.Right, AContext.ItemRect.Bottom,
      AContext.CornerRadius, AContext.CornerRadius);
    ACanvas.Pen.Width := 1;
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TDeviceListBox.DrawItemTopLine(ACanvas: TCanvas;
  const AItem: TDeviceDisplayItem; const AContext: TItemDrawContext);
var
  Style: TCustomStyleServices;
  AddrLeft, NameHeight, AddrOffset: Integer;
  RightEdge: Integer;
  BatteryIconChar: Char;
  BatteryIconWidth, BatteryTextWidth: Integer;
  NameLineHeight, BatteryOffset: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  // Set up font for device name
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.DeviceNameFontSize;
  ACanvas.Font.Style := [];
  if AContext.IsSelected then
    ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
  else
    ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
  ACanvas.Brush.Style := bsClear;

  // Calculate name line height for vertical centering of smaller elements
  NameLineHeight := ACanvas.TextHeight('Ay');

  // Start from right edge and work backwards
  RightEdge := AContext.TextRect.Right + AContext.ItemPadding;

  // Draw pin indicator (rightmost)
  if AItem.IsPinned then
  begin
    ACanvas.Font.Name := FONT_ICONS;
    ACanvas.Font.Size := PIN_ICON_FONT_SIZE;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    RightEdge := RightEdge - PIN_ICON_WIDTH;
    ACanvas.TextOut(RightEdge, AContext.NameLineTop, ICON_PIN);
    RightEdge := RightEdge - BATTERY_SPACING;
  end;

  // Draw battery indicator (before pin)
  if AItem.BatteryStatus.HasLevel then
  begin
    // Draw battery icon first (right side of battery display)
    BatteryIconChar := GetBatteryIconChar(AItem.BatteryStatus.Level);
    ACanvas.Font.Name := FONT_ICONS;
    ACanvas.Font.Size := BATTERY_ICON_FONT_SIZE;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    BatteryIconWidth := ACanvas.TextWidth(BatteryIconChar);
    RightEdge := RightEdge - BatteryIconWidth;
    BatteryOffset := (NameLineHeight - ACanvas.TextHeight(BatteryIconChar)) div 2;
    ACanvas.TextOut(RightEdge, AContext.NameLineTop + BatteryOffset, BatteryIconChar);

    // Draw battery percentage text (left of icon)
    ACanvas.Font.Name := FONT_UI;
    ACanvas.Font.Size := BATTERY_FONT_SIZE;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    BatteryTextWidth := ACanvas.TextWidth(AItem.BatteryText);
    RightEdge := RightEdge - BATTERY_SPACING - BatteryTextWidth;
    BatteryOffset := (NameLineHeight - ACanvas.TextHeight(AItem.BatteryText)) div 2;
    ACanvas.TextOut(RightEdge, AContext.NameLineTop + BatteryOffset, AItem.BatteryText);
  end;

  // Restore font for name
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.DeviceNameFontSize;
  if AContext.IsSelected then
    ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
  else
    ACanvas.Font.Color := Style.GetSystemColor(clWindowText);

  // Draw device name
  ACanvas.TextOut(AContext.TextRect.Left, AContext.NameLineTop, AItem.DisplayName);

  // Draw address if enabled
  if AContext.ShowAddresses then
  begin
    AddrLeft := AContext.TextRect.Left + ACanvas.TextWidth(AItem.DisplayName) + ADDRESS_SPACING;
    NameHeight := ACanvas.TextHeight('Ay');
    ACanvas.Font.Size := AContext.AddressFontSize;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    AddrOffset := (NameHeight - ACanvas.TextHeight('Ay')) div 2;
    ACanvas.TextOut(AddrLeft, AContext.NameLineTop + AddrOffset,
      '[' + AItem.Device.AddressString + ']');
  end;
end;

procedure TDeviceListBox.DrawItemBottomLine(ACanvas: TCanvas;
  const AItem: TDeviceDisplayItem; const AContext: TItemDrawContext);
var
  Style: TCustomStyleServices;
  StatusText: string;
  LastSeenWidth: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.StatusFontSize;
  ACanvas.Font.Style := [];

  // Connection status (left-aligned)
  StatusText := TDeviceFormatter.FormatConnectionState(AItem.Device.ConnectionState);
  if AItem.Device.IsConnected then
    ACanvas.Font.Color := AContext.ConnectedColor
  else
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);

  ACanvas.TextOut(AContext.TextRect.Left, AContext.StatusLineTop, StatusText);

  // LastSeen (right-aligned)
  if AContext.ShowLastSeen then
  begin
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    LastSeenWidth := ACanvas.TextWidth(AItem.LastSeenText);
    ACanvas.TextOut(AContext.TextRect.Right - LastSeenWidth,
      AContext.StatusLineTop, AItem.LastSeenText);
  end;
end;

procedure TDeviceListBox.DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
  const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
var
  Context: TItemDrawContext;
  IconRect: TRect;
begin
  // Create context with all layout parameters
  Context := CreateDrawContext(ACanvas, ARect, AIsHover, AIsSelected);

  // Draw background and border
  DrawItemBackground(ACanvas, Context);

  // Draw device icon if enabled
  if Context.ShowDeviceIcons then
  begin
    IconRect.Left := Context.ItemRect.Left + Context.ItemPadding;
    IconRect.Top := Context.ItemRect.Top +
      ((Context.ItemRect.Bottom - Context.ItemRect.Top) - Context.IconSize) div 2;
    IconRect.Right := IconRect.Left + Context.IconSize;
    IconRect.Bottom := IconRect.Top + Context.IconSize;
    DrawDeviceIcon(ACanvas, IconRect, AItem.EffectiveDeviceType);
  end;

  // Draw text content
  DrawItemTopLine(ACanvas, AItem, Context);
  DrawItemBottomLine(ACanvas, AItem, Context);

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

function TDeviceListBox.GetBatteryIconChar(ALevel: Integer): Char;
begin
  if ALevel >= 75 then
    Result := ICON_BATTERY_FULL
  else if ALevel >= 50 then
    Result := ICON_BATTERY_HIGH
  else if ALevel >= 25 then
    Result := ICON_BATTERY_MID
  else if ALevel >= 10 then
    Result := ICON_BATTERY_LOW
  else
    Result := ICON_BATTERY_EMPTY;
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
