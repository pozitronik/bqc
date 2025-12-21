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
  System.Generics.Collections,
  System.Generics.Defaults,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Themes,
  Bluetooth.Types,
  App.Config;

type
  TDeviceClickEvent = procedure(Sender: TObject; const ADevice: TBluetoothDeviceInfo) of object;

  /// <summary>
  /// Custom device list control with owner-draw rendering.
  /// </summary>
  TDeviceListBox = class(TCustomControl)
  private const
    ITEM_HEIGHT = 70;
    ITEM_PADDING = 12;
    ICON_SIZE = 32;
    CORNER_RADIUS = 8;
    ITEM_MARGIN = 4;
  private
    FDevices: TList<TBluetoothDeviceInfo>;
    FHoverIndex: Integer;
    FSelectedIndex: Integer;
    FScrollPos: Integer;
    FMaxScroll: Integer;
    FShowAddresses: Boolean;
    FOnDeviceClick: TDeviceClickEvent;
    FOnSelectionChanged: TNotifyEvent;

    procedure SetShowAddresses(AValue: Boolean);

    procedure SetSelectedIndex(AValue: Integer);
    function GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
    function GetDeviceCount: Integer;
    function GetItemRect(AIndex: Integer): TRect;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure UpdateScrollRange;
    procedure ScrollTo(APos: Integer);

    procedure DrawDevice(ACanvas: TCanvas; const ARect: TRect;
      const ADevice: TBluetoothDeviceInfo; AIsHover, AIsSelected: Boolean);
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
    procedure AddDevice(const ADevice: TBluetoothDeviceInfo);
    procedure UpdateDevice(const ADevice: TBluetoothDeviceInfo);
    procedure SetDevices(const ADevices: TBluetoothDeviceInfoArray);
    function GetSelectedDevice: TBluetoothDeviceInfo;
    procedure EnsureVisible(AIndex: Integer);

    property Devices[AIndex: Integer]: TBluetoothDeviceInfo read GetDevice;
    property DeviceCount: Integer read GetDeviceCount;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property OnDeviceClick: TDeviceClickEvent read FOnDeviceClick write FOnDeviceClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;

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
  System.Math;

{ TDeviceListBox }

constructor TDeviceListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDevices := TList<TBluetoothDeviceInfo>.Create;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FScrollPos := 0;
  FMaxScroll := 0;
  FShowAddresses := False;

  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  DoubleBuffered := True;
  Width := 300;
  Height := 400;
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
  FDevices.Free;
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
  FDevices.Clear;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FScrollPos := 0;
  UpdateScrollRange;
  Invalidate;
end;

procedure TDeviceListBox.AddDevice(const ADevice: TBluetoothDeviceInfo);
begin
  // Skip devices with empty names
  if Trim(ADevice.Name) = '' then
    Exit;

  FDevices.Add(ADevice);
  UpdateScrollRange;
  Invalidate;
end;

procedure TDeviceListBox.UpdateDevice(const ADevice: TBluetoothDeviceInfo);
var
  I: Integer;
begin
  // Skip devices with empty names
  if Trim(ADevice.Name) = '' then
    Exit;

  for I := 0 to FDevices.Count - 1 do
  begin
    if FDevices[I].AddressInt = ADevice.AddressInt then
    begin
      FDevices[I] := ADevice;
      Invalidate;
      Exit;
    end;
  end;
end;

procedure TDeviceListBox.SetDevices(const ADevices: TBluetoothDeviceInfoArray);
var
  Device: TBluetoothDeviceInfo;
  DeviceConfig: TDeviceConfig;
begin
  FDevices.Clear;
  for Device in ADevices do
  begin
    // Skip devices with empty names
    if Trim(Device.Name) = '' then
      Continue;

    // Skip hidden devices
    DeviceConfig := Config.GetDeviceConfig(Device.AddressInt);
    if DeviceConfig.Hidden then
      Continue;

    FDevices.Add(Device);
  end;

  // Sort devices: pinned first, then by name
  FDevices.Sort(TComparer<TBluetoothDeviceInfo>.Construct(
    function(const Left, Right: TBluetoothDeviceInfo): Integer
    var
      LeftConfig, RightConfig: TDeviceConfig;
      LeftPinned, RightPinned: Boolean;
    begin
      LeftConfig := Config.GetDeviceConfig(Left.AddressInt);
      RightConfig := Config.GetDeviceConfig(Right.AddressInt);
      LeftPinned := LeftConfig.Pinned;
      RightPinned := RightConfig.Pinned;

      // Pinned devices come first
      if LeftPinned and not RightPinned then
        Result := -1
      else if not LeftPinned and RightPinned then
        Result := 1
      else
        // Then sort by name
        Result := CompareText(Left.Name, Right.Name);
    end
  ));

  FHoverIndex := -1;
  if FSelectedIndex >= FDevices.Count then
    FSelectedIndex := -1;
  FScrollPos := 0;
  UpdateScrollRange;
  Invalidate;
end;

function TDeviceListBox.GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
begin
  if (AIndex >= 0) and (AIndex < FDevices.Count) then
    Result := FDevices[AIndex]
  else
    Result := Default(TBluetoothDeviceInfo);
end;

function TDeviceListBox.GetDeviceCount: Integer;
begin
  Result := FDevices.Count;
end;

function TDeviceListBox.GetSelectedDevice: TBluetoothDeviceInfo;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < FDevices.Count) then
    Result := FDevices[FSelectedIndex]
  else
    Result := Default(TBluetoothDeviceInfo);
end;

procedure TDeviceListBox.SetSelectedIndex(AValue: Integer);
begin
  if AValue < -1 then AValue := -1;
  if AValue >= FDevices.Count then AValue := FDevices.Count - 1;

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
  Result.Left := ITEM_MARGIN;
  Result.Right := ClientWidth - ITEM_MARGIN;
  Result.Top := ITEM_MARGIN + AIndex * (ITEM_HEIGHT + ITEM_MARGIN) - FScrollPos;
  Result.Bottom := Result.Top + ITEM_HEIGHT;
end;

function TDeviceListBox.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to FDevices.Count - 1 do
  begin
    R := GetItemRect(I);
    if PtInRect(R, Point(X, Y)) then
      Exit(I);
  end;
  Result := -1;
end;

procedure TDeviceListBox.UpdateScrollRange;
var
  TotalHeight, VisibleHeight: Integer;
begin
  TotalHeight := FDevices.Count * (ITEM_HEIGHT + ITEM_MARGIN) + ITEM_MARGIN;
  VisibleHeight := ClientHeight;
  FMaxScroll := Max(0, TotalHeight - VisibleHeight);
  if FScrollPos > FMaxScroll then
    FScrollPos := FMaxScroll;
  UpdateScrollBar;
end;

procedure TDeviceListBox.UpdateScrollBar;
var
  SI: TScrollInfo;
  TotalHeight: Integer;
begin
  if not HandleAllocated then
    Exit;

  TotalHeight := FDevices.Count * (ITEM_HEIGHT + ITEM_MARGIN) + ITEM_MARGIN;

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
  ItemTop, ItemBottom: Integer;
begin
  if (AIndex < 0) or (AIndex >= FDevices.Count) then
    Exit;

  ItemTop := ITEM_MARGIN + AIndex * (ITEM_HEIGHT + ITEM_MARGIN);
  ItemBottom := ItemTop + ITEM_HEIGHT;

  if ItemTop < FScrollPos then
    ScrollTo(ItemTop - ITEM_MARGIN)
  else if ItemBottom > FScrollPos + ClientHeight then
    ScrollTo(ItemBottom - ClientHeight + ITEM_MARGIN);
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
begin
  NewPos := FScrollPos;

  case Message.ScrollCode of
    SB_LINEUP:
      Dec(NewPos, ITEM_HEIGHT div 2);
    SB_LINEDOWN:
      Inc(NewPos, ITEM_HEIGHT div 2);
    SB_PAGEUP:
      Dec(NewPos, ClientHeight - ITEM_HEIGHT);
    SB_PAGEDOWN:
      Inc(NewPos, ClientHeight - ITEM_HEIGHT);
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
  I: Integer;
  R: TRect;
  IsHover, IsSelected: Boolean;
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  // Background
  Canvas.Brush.Color := Style.GetSystemColor(clWindow);
  Canvas.FillRect(ClientRect);

  // Draw items
  for I := 0 to FDevices.Count - 1 do
  begin
    R := GetItemRect(I);

    // Skip items outside visible area
    if R.Bottom < 0 then
      Continue;
    if R.Top > ClientHeight then
      Break;

    IsHover := (I = FHoverIndex);
    IsSelected := (I = FSelectedIndex);

    DrawDevice(Canvas, R, FDevices[I], IsHover, IsSelected);
  end;

  // Focus rectangle
  if Focused and (FSelectedIndex >= 0) then
  begin
    R := GetItemRect(FSelectedIndex);
    InflateRect(R, -2, -2);
    Canvas.Pen.Color := Style.GetSystemColor(clHighlight);
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    Canvas.Pen.Style := psSolid;
  end;
end;

procedure TDeviceListBox.DrawDevice(ACanvas: TCanvas; const ARect: TRect;
  const ADevice: TBluetoothDeviceInfo; AIsHover, AIsSelected: Boolean);
var
  BgColor: TColor;
  IconRect, TextRect: TRect;
  StatusText, DisplayName: string;
  TextTop: Integer;
  DeviceConfig: TDeviceConfig;
  Style: TCustomStyleServices;
  EffectiveDeviceType: TBluetoothDeviceType;
begin
  Style := TStyleManager.ActiveStyle;

  // Get device-specific configuration
  DeviceConfig := Config.GetDeviceConfig(ADevice.AddressInt);

  // Use alias if set, otherwise use device name
  if DeviceConfig.Alias <> '' then
    DisplayName := DeviceConfig.Alias
  else
    DisplayName := ADevice.Name;

  // Determine effective device type: use override if set, otherwise auto-detected
  if DeviceConfig.DeviceTypeOverride >= 0 then
    EffectiveDeviceType := TBluetoothDeviceType(DeviceConfig.DeviceTypeOverride)
  else
    EffectiveDeviceType := ADevice.DeviceType;

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
    CORNER_RADIUS, CORNER_RADIUS);

  // Icon area
  IconRect.Left := ARect.Left + ITEM_PADDING;
  IconRect.Top := ARect.Top + (ITEM_HEIGHT - ICON_SIZE) div 2;
  IconRect.Right := IconRect.Left + ICON_SIZE;
  IconRect.Bottom := IconRect.Top + ICON_SIZE;

  DrawDeviceIcon(ACanvas, IconRect, EffectiveDeviceType);

  // Text area
  TextRect.Left := IconRect.Right + ITEM_PADDING;
  TextRect.Right := ARect.Right - ITEM_PADDING;
  TextRect.Top := ARect.Top;
  TextRect.Bottom := ARect.Bottom;

  // Device name (or alias if configured)
  ACanvas.Font.Name := 'Segoe UI';
  ACanvas.Font.Size := 11;
  ACanvas.Font.Style := [];
  if AIsSelected then
    ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
  else
    ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
  ACanvas.Brush.Style := bsClear;

  TextTop := ARect.Top + ITEM_PADDING;

  // Draw pin indicator in top-right corner for pinned devices
  if DeviceConfig.Pinned then
  begin
    ACanvas.Font.Name := 'Segoe MDL2 Assets';
    ACanvas.Font.Size := 10;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    ACanvas.TextOut(ARect.Right - ITEM_PADDING - 12, ARect.Top + 6, #$E718);  // Pin icon
    ACanvas.Font.Name := 'Segoe UI';
    ACanvas.Font.Size := 11;
    if AIsSelected then
      ACanvas.Font.Color := Style.GetSystemColor(clHighlightText)
    else
      ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
  end;

  ACanvas.TextOut(TextRect.Left, TextTop, DisplayName);

  // Show address after device name if enabled
  if FShowAddresses then
  begin
    // Calculate position while still using name font size
    var AddrLeft := TextRect.Left + ACanvas.TextWidth(DisplayName) + 8;
    ACanvas.Font.Size := 8;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    ACanvas.TextOut(AddrLeft, TextTop + 3, '[' + ADevice.AddressString + ']');
    // Restore font for subsequent drawing
    ACanvas.Font.Size := 11;
  end;

  // Status text
  if ADevice.IsConnected then
  begin
    StatusText := ADevice.ConnectionStateText;
    // Green for connected - use hardcoded color as there's no "success" system color
    ACanvas.Font.Color := $00008000;  // Green
  end
  else
  begin
    StatusText := ADevice.ConnectionStateText;
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
  end;

  ACanvas.Font.Size := 9;
  TextTop := TextTop + ACanvas.TextHeight('Ay') + 4;
  ACanvas.TextOut(TextRect.Left, TextTop, StatusText);

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

  // Use Segoe MDL2 Assets font for icons
  ACanvas.Font.Name := 'Segoe MDL2 Assets';
  ACanvas.Font.Size := 16;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindowText);
  ACanvas.Brush.Style := bsClear;

  TextSize := ACanvas.TextExtent(IconChar);
  X := ARect.Left + (ARect.Width - TextSize.cx) div 2;
  Y := ARect.Top + (ARect.Height - TextSize.cy) div 2;

  ACanvas.TextOut(X, Y, IconChar);

  // Restore font
  ACanvas.Font.Name := 'Segoe UI';
  ACanvas.Brush.Style := bsSolid;
end;

function TDeviceListBox.GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
begin
  case ADeviceType of
    btAudioOutput,
    btHeadset:      Result := #$E7F6;  // Headphone
    btAudioInput:   Result := #$E720;  // Microphone
    btKeyboard:     Result := #$E765;  // Keyboard
    btMouse:        Result := #$E962;  // Mouse
    btGamepad:      Result := #$E7FC;  // Gamepad
    btComputer:     Result := #$E7F8;  // PC
    btPhone:        Result := #$E8EA;  // Phone
    btHID:          Result := #$E961;  // General Input Device
  else
    Result := #$E702;  // Bluetooth
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
    if (Index >= 0) and (Index = FSelectedIndex) and Assigned(FOnDeviceClick) then
      FOnDeviceClick(Self, FDevices[Index]);
  end;
end;

procedure TDeviceListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case Key of
    VK_UP:
      begin
        if FSelectedIndex > 0 then
          SelectedIndex := FSelectedIndex - 1
        else if (FSelectedIndex = -1) and (FDevices.Count > 0) then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_DOWN:
      begin
        if FSelectedIndex < FDevices.Count - 1 then
          SelectedIndex := FSelectedIndex + 1
        else if (FSelectedIndex = -1) and (FDevices.Count > 0) then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_HOME:
      begin
        if FDevices.Count > 0 then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_END:
      begin
        if FDevices.Count > 0 then
          SelectedIndex := FDevices.Count - 1;
        Key := 0;
      end;

    VK_RETURN, VK_SPACE:
      begin
        if (FSelectedIndex >= 0) and Assigned(FOnDeviceClick) then
          FOnDeviceClick(Self, FDevices[FSelectedIndex]);
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
