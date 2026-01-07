{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Custom Device List Control                      }
{                                                       }
{       Owner-draw list for Bluetooth devices with      }
{       modern Windows 11 styling.                      }
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
  Vcl.ExtCtrls,
  Vcl.Themes,
  Vcl.Menus,
  Bluetooth.Types,
  App.ConfigEnums,
  App.LayoutConfigIntf,
  App.AppearanceConfigIntf,
  App.ProfileConfigIntf,
  App.DeviceDisplayTypes,
  App.WinRTSupport,
  UI.DeviceContextMenuBuilder,
  UI.CustomScrollbar,
  UI.ListDataSource,
  UI.DeviceItemRenderer,
  UI.DeviceListTypes;

type
  TDeviceClickEvent = procedure(Sender: TObject; const ADevice: TBluetoothDeviceInfo) of object;
  TDeviceAddressEvent = procedure(Sender: TObject; AAddress: UInt64) of object;

  TDeviceDisplayItemClickEvent = procedure(Sender: TObject;
    const AItem: TDeviceDisplayItem) of object;

  /// <summary>
  /// Custom device list control with owner-draw rendering.
  /// Displays pre-processed TDeviceDisplayItem records.
  /// </summary>
  TDeviceListBox = class(TCustomControl)
  private
    // Data source (extracted) - manages items, heights, selection, hover
    FDataSource: TListDataSource;

    FScrollPos: Integer;
    FMaxScroll: Integer;
    FShowAddresses: Boolean;
    FOnDeviceClick: TDeviceClickEvent;
    FOnDisplayItemClick: TDeviceDisplayItemClickEvent;
    FOnActionClick: TDeviceDisplayItemClickEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnPinToggle: TDeviceAddressEvent;
    FOnCopyName: TDeviceAddressEvent;
    FOnCopyAddress: TDeviceAddressEvent;
    FOnConfigure: TDeviceAddressEvent;
    FOnUnpair: TDeviceAddressEvent;
    FOnHide: TDeviceAddressEvent;

    // Context menu builder (extracted)
    FContextMenuBuilder: TDeviceContextMenuBuilder;

    // Custom scrollbar (extracted)
    FScrollbar: TCustomScrollbar;

    // Item renderer (extracted)
    FRenderer: TDeviceItemRenderer;

    // Injected configuration interfaces (for layout/appearance only)
    FLayoutConfig: ILayoutConfig;
    FAppearanceConfig: IAppearanceConfig;
    FProfileConfig: IProfileConfig;

    // Cached layout parameters (refreshed once per paint cycle)
    FCachedLayout: TCachedLayoutParams;

    // Animation for action button progress
    FAnimationTimer: TTimer;
    FAnimationFrame: Integer;

    // Win7 system icon cache (nil on Win10+ where we use Segoe MDL2 Assets)
    FSystemIconCache: TSystemIconCache;

    procedure HandleAnimationTimer(Sender: TObject);
    procedure ShowContextMenuForItem(AItemIndex: Integer; X, Y: Integer);

    // Data source event handlers
    procedure HandleDataSourceItemsChanged(Sender: TObject);
    procedure HandleDataSourceSelectionChanged(Sender: TObject);

    // Event setters to forward to context menu builder
    procedure SetOnPinToggle(AValue: TDeviceAddressEvent);
    procedure SetOnCopyName(AValue: TDeviceAddressEvent);
    procedure SetOnCopyAddress(AValue: TDeviceAddressEvent);
    procedure SetOnConfigure(AValue: TDeviceAddressEvent);
    procedure SetOnUnpair(AValue: TDeviceAddressEvent);
    procedure SetOnHide(AValue: TDeviceAddressEvent);

    procedure RefreshLayoutCache;
    function GetLayoutConfig: ILayoutConfig;
    function GetAppearanceConfig: IAppearanceConfig;
    function GetProfileConfig: IProfileConfig;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetLayoutConfig(AValue: ILayoutConfig);
    procedure SetAppearanceConfig(AValue: IAppearanceConfig);
    function GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
    function GetDeviceCount: Integer;
    function GetItemCount: Integer;
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(AValue: Integer);
    function ItemAtPos(X, Y: Integer): Integer;
    procedure UpdateScrollRange;
    procedure ScrollTo(APos: Integer);

    procedure HandleScrollbarScrollChanged(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;

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
    function GetItemRect(AIndex: Integer): TRect;

    property Devices[AIndex: Integer]: TBluetoothDeviceInfo read GetDevice;
    property DeviceCount: Integer read GetDeviceCount;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property OnDeviceClick: TDeviceClickEvent read FOnDeviceClick write FOnDeviceClick;
    property OnDisplayItemClick: TDeviceDisplayItemClickEvent read FOnDisplayItemClick write FOnDisplayItemClick;
    property OnActionClick: TDeviceDisplayItemClickEvent read FOnActionClick write FOnActionClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnPinToggle: TDeviceAddressEvent read FOnPinToggle write SetOnPinToggle;
    property OnCopyName: TDeviceAddressEvent read FOnCopyName write SetOnCopyName;
    property OnCopyAddress: TDeviceAddressEvent read FOnCopyAddress write SetOnCopyAddress;
    property OnConfigure: TDeviceAddressEvent read FOnConfigure write SetOnConfigure;
    property OnUnpair: TDeviceAddressEvent read FOnUnpair write SetOnUnpair;
    property OnHide: TDeviceAddressEvent read FOnHide write SetOnHide;

    // Dependency injection properties (must be set before use)
    property LayoutConfig: ILayoutConfig read GetLayoutConfig write SetLayoutConfig;
    property AppearanceConfig: IAppearanceConfig read GetAppearanceConfig write SetAppearanceConfig;
    property ProfileConfig: IProfileConfig read GetProfileConfig write FProfileConfig;

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
  UI.ListGeometry,
  App.Logger;

const
  // Layout spacing constants
  FOCUS_RECT_INSET = 2;        // Pixels to inset focus rectangle from item bounds

  // Default control dimensions
  DEFAULT_CONTROL_WIDTH = 300;
  DEFAULT_CONTROL_HEIGHT = 400;

{ TDeviceListBox }

constructor TDeviceListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollPos := 0;
  FMaxScroll := 0;
  FShowAddresses := False;
  FAnimationFrame := 0;
  // Initialize system icon cache on Win7 (nil on Win10+ where we use Segoe MDL2 Assets)
  if not TWinRTSupport.IsAvailable then
    FSystemIconCache := TSystemIconCache.Create(32)  // 32x32 icons for device list
  else
    FSystemIconCache := nil;

  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  DoubleBuffered := True;
  Width := DEFAULT_CONTROL_WIDTH;
  Height := DEFAULT_CONTROL_HEIGHT;

  // Create data source (extracted from god class)
  FDataSource := TListDataSource.Create;
  FDataSource.OnItemsChanged := HandleDataSourceItemsChanged;
  FDataSource.OnSelectionChanged := HandleDataSourceSelectionChanged;

  // Create animation timer for action button progress indicator
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 50;  // 20 FPS for smooth animation
  FAnimationTimer.OnTimer := HandleAnimationTimer;
  FAnimationTimer.Enabled := True;

  // Create item renderer (extracted from god class)
  FRenderer := TDeviceItemRenderer.Create(FSystemIconCache);

  // Create context menu builder (extracted from god class)
  // Events will be forwarded via property setters when external code sets them
  FContextMenuBuilder := TDeviceContextMenuBuilder.Create(Self);

  // Create custom scrollbar (extracted from god class)
  FScrollbar := TCustomScrollbar.Create;
  FScrollbar.OnScrollChanged := HandleScrollbarScrollChanged;
  // Note: UpdateClientSize will be called in Resize when control gets dimensions
end;

function TDeviceListBox.GetLayoutConfig: ILayoutConfig;
begin
  Result := FLayoutConfig;
end;

function TDeviceListBox.GetAppearanceConfig: IAppearanceConfig;
begin
  Result := FAppearanceConfig;
end;

function TDeviceListBox.GetProfileConfig: IProfileConfig;
begin
  Result := FProfileConfig;
end;

procedure TDeviceListBox.RefreshLayoutCache;
begin
  // Cache layout config values (avoids repeated interface queries per item)
  if Assigned(FLayoutConfig) then
  begin
    FCachedLayout.ItemHeight := FLayoutConfig.ItemHeight;
    FCachedLayout.ItemMargin := FLayoutConfig.ItemMargin;
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
    FCachedLayout.ListBackgroundSource := FAppearanceConfig.ListBackgroundSource;
    FCachedLayout.ListBackgroundCustomColor := FAppearanceConfig.ListBackgroundCustomColor;
    FCachedLayout.MainColorSource := FAppearanceConfig.MainColorSource;
    FCachedLayout.MainCustomColor := FAppearanceConfig.MainCustomColor;
    FCachedLayout.SecondaryColorSource := FAppearanceConfig.SecondaryColorSource;
    FCachedLayout.SecondaryCustomColor := FAppearanceConfig.SecondaryCustomColor;
    FCachedLayout.HoverColorSource := FAppearanceConfig.HoverColorSource;
    FCachedLayout.HoverCustomColor := FAppearanceConfig.HoverCustomColor;
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

procedure TDeviceListBox.SetLayoutConfig(AValue: ILayoutConfig);
begin
  FLayoutConfig := AValue;
  RefreshLayoutCache;
  // Notify data source to recalculate heights
  FDataSource.UpdateConfigs(FLayoutConfig, FProfileConfig);
end;

procedure TDeviceListBox.SetAppearanceConfig(AValue: IAppearanceConfig);
begin
  FAppearanceConfig := AValue;
  RefreshLayoutCache;
end;

destructor TDeviceListBox.Destroy;
begin
  FScrollbar.Free;
  FContextMenuBuilder.Free;
  FDataSource.Free;
  FRenderer.Free;
  FSystemIconCache.Free;  // Safe to free nil
  inherited Destroy;
end;

procedure TDeviceListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_VSCROLL;  // Remove native scrollbar
end;

procedure TDeviceListBox.Resize;
begin
  inherited;
  // Notify scrollbar of size changes
  FScrollbar.UpdateClientSize(ClientWidth, ClientHeight);
  // Refresh layout cache on resize - dimensions may have changed
  RefreshLayoutCache;
  UpdateScrollRange;
end;

procedure TDeviceListBox.Clear;
begin
  FDataSource.Clear;
  FScrollPos := 0;
  UpdateScrollRange;
  Invalidate;
end;

function TDeviceListBox.GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
begin
  Result := FDataSource.GetDevice(AIndex);
end;

function TDeviceListBox.GetDeviceCount: Integer;
begin
  Result := FDataSource.ItemCount;
end;

function TDeviceListBox.GetSelectedDevice: TBluetoothDeviceInfo;
begin
  Result := FDataSource.GetSelectedDevice;
end;

function TDeviceListBox.GetSelectedDisplayItem: TDeviceDisplayItem;
begin
  Result := FDataSource.GetSelectedDisplayItem;
end;

function TDeviceListBox.GetItemCount: Integer;
begin
  Result := FDataSource.ItemCount;
end;

function TDeviceListBox.GetSelectedIndex: Integer;
begin
  Result := FDataSource.SelectedIndex;
end;

procedure TDeviceListBox.SetDisplayItems(const AItems: TDeviceDisplayItemArray);
var
  I: Integer;
  OldScrollPos: Integer;
begin
  // Save current scroll position to preserve it across refreshes
  OldScrollPos := FScrollPos;

  // Delegate to data source (handles items, index map, heights, selection/hover restoration)
  FDataSource.SetDisplayItems(AItems);

  // Restore scroll position (will be clamped by UpdateScrollRange if needed)
  FScrollPos := OldScrollPos;
  UpdateScrollRange;
  Invalidate;

  // If action item is in progress, force immediate update (no delay)
  for I := 0 to High(FDataSource.Items) do
  begin
    if (FDataSource.Items[I].Source = dsAction) and FDataSource.Items[I].IsActionInProgress then
    begin
      UpdateWindow(Handle);
      Break;
    end;
  end;
end;

procedure TDeviceListBox.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
begin
  // Delegate to data source (O(1) lookup, update item, recalculate height)
  FDataSource.UpdateDisplayItem(AItem);
  UpdateScrollRange;
  Invalidate;
end;

procedure TDeviceListBox.SetSelectedIndex(AValue: Integer);
begin
  // Delegate to data source (handles validation and fires OnSelectionChanged event)
  FDataSource.SelectedIndex := AValue;
  // EnsureVisible called in HandleDataSourceSelectionChanged
end;

function TDeviceListBox.GetItemRect(AIndex: Integer): TRect;
begin
  if Length(FDataSource.ItemHeights) > 0 then
    Result := TListGeometry.GetItemRectVariable(
      AIndex,
      FDataSource.ItemHeights,
      FCachedLayout.ItemMargin,
      ClientWidth,
      FScrollPos
    )
  else
    Result := TListGeometry.GetItemRect(
      AIndex,
      FCachedLayout.ItemHeight,
      FCachedLayout.ItemMargin,
      ClientWidth,
      FScrollPos
    );
end;

function TDeviceListBox.ItemAtPos(X, Y: Integer): Integer;
begin
  if Length(FDataSource.ItemHeights) > 0 then
    Result := TListGeometry.ItemAtPosVariable(
      X, Y,
      FDataSource.ItemHeights,
      FCachedLayout.ItemMargin,
      ClientWidth,
      FScrollPos
    )
  else
    Result := TListGeometry.ItemAtPos(
      X, Y,
      GetItemCount,
      FCachedLayout.ItemHeight,
      FCachedLayout.ItemMargin,
      ClientWidth,
      FScrollPos
    );
end;

procedure TDeviceListBox.UpdateScrollRange;
begin
  // Calculate max scroll
  if Length(FDataSource.ItemHeights) > 0 then
    FMaxScroll := TListGeometry.CalculateMaxScrollVariable(
      FDataSource.ItemHeights,
      FCachedLayout.ItemMargin,
      ClientHeight
    )
  else
    FMaxScroll := TListGeometry.CalculateMaxScroll(
      GetItemCount,
      FCachedLayout.ItemHeight,
      FCachedLayout.ItemMargin,
      ClientHeight
    );

  FScrollPos := TListGeometry.ClampScrollPos(FScrollPos, FMaxScroll);

  // Delegate to scrollbar (extracted)
  FScrollbar.UpdateScrollRange(FMaxScroll);
  FScrollbar.ScrollTo(FScrollPos);

  // Hide native scrollbar (we use custom scrollbar)
  if HandleAllocated then
    ShowScrollBar(Handle, SB_VERT, False);

  Invalidate;
end;

procedure TDeviceListBox.ScrollTo(APos: Integer);
begin
  FScrollbar.ScrollTo(APos);
  // FScrollPos will be updated via HandleScrollbarScrollChanged callback
end;

procedure TDeviceListBox.HandleScrollbarScrollChanged(Sender: TObject);
begin
  FScrollPos := FScrollbar.ScrollPos;
  Invalidate;
end;

procedure TDeviceListBox.HandleDataSourceItemsChanged(Sender: TObject);
begin
  UpdateScrollRange;
  Invalidate;
end;

procedure TDeviceListBox.HandleDataSourceSelectionChanged(Sender: TObject);
begin
  if FDataSource.SelectedIndex >= 0 then
    EnsureVisible(FDataSource.SelectedIndex);
  Invalidate;
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TDeviceListBox.EnsureVisible(AIndex: Integer);
var
  NewScrollPos: Integer;
begin
  if (AIndex < 0) or (AIndex >= GetItemCount) then
    Exit;

  if Length(FDataSource.ItemHeights) > 0 then
    NewScrollPos := TListGeometry.ScrollPosToMakeVisibleVariable(
      AIndex,
      FDataSource.ItemHeights,
      FScrollPos,
      FCachedLayout.ItemMargin,
      ClientHeight
    )
  else
    NewScrollPos := TListGeometry.ScrollPosToMakeVisible(
      AIndex,
      FScrollPos,
      FCachedLayout.ItemHeight,
      FCachedLayout.ItemMargin,
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
  ItemHeight := FCachedLayout.ItemHeight;

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
  if FDataSource.HoverIndex <> -1 then
  begin
    FDataSource.HoverIndex := -1;
    Invalidate;
  end;
  // Delegate to scrollbar (returns true if state changed)
  if FScrollbar.HandleMouseLeave then
    Invalidate;
end;

procedure TDeviceListBox.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  // VCL style has changed - refresh layout cache and force repaint
  LogDebug('CMStyleChanged: VCL style changed, refreshing layout cache and invalidating device list', ClassName);
  RefreshLayoutCache;
  Invalidate;
end;

procedure TDeviceListBox.Paint;
var
  I, Count: Integer;
  R: TRect;
  IsHover, IsSelected: Boolean;
  Style: TCustomStyleServices;
begin
  // Layout cache refreshed on resize/style change, not on every paint
  Style := TStyleManager.ActiveStyle;

  // Update renderer cache with current configuration
  FRenderer.UpdateCache(FCachedLayout, FProfileConfig, FShowAddresses, FAnimationFrame);

  // Background (color source from config)
  case FCachedLayout.ListBackgroundSource of
    lbsThemeWindow: Canvas.Brush.Color := Style.GetSystemColor(clWindow);
    lbsThemeForm:   Canvas.Brush.Color := Style.GetSystemColor(clBtnFace);
    lbsCustom:      Canvas.Brush.Color := TColor(FCachedLayout.ListBackgroundCustomColor);
  else
    Canvas.Brush.Color := Style.GetSystemColor(clWindow);  // Fallback
  end;
  Canvas.FillRect(ClientRect);

  // Draw items (delegated to renderer)
  Count := GetItemCount;
  for I := 0 to Count - 1 do
  begin
    R := GetItemRect(I);

    // Skip items outside visible area
    if R.Bottom < 0 then
      Continue;
    if R.Top > ClientHeight then
      Break;

    // Draw separator before first non-paired item (action button or discovered device)
    if (I > 0) and
       (FDataSource.Items[I].Source in [dsAction, dsDiscovered]) and
       (FDataSource.Items[I - 1].Source = dsPaired) then
    begin
      FRenderer.DrawSeparator(Canvas, R);
      // Adjust item rect to start below separator to prevent background overlap
      // Separator is drawn at ARect.Top + (ItemMargin div 2), so offset by ItemMargin
      R.Top := R.Top + FCachedLayout.ItemMargin;
    end;

    IsHover := (I = FDataSource.HoverIndex);
    IsSelected := (I = FDataSource.SelectedIndex);

    FRenderer.DrawDisplayItem(Canvas, R, FDataSource.Items[I], IsHover, IsSelected);
  end;

  // Focus rectangle
  if Focused and (FDataSource.SelectedIndex >= 0) then
  begin
    R := GetItemRect(FDataSource.SelectedIndex);
    InflateRect(R, -FOCUS_RECT_INSET, -FOCUS_RECT_INSET);
    Canvas.Pen.Color := Style.GetSystemColor(clHighlight);
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    Canvas.Pen.Style := psSolid;
  end;

  // Draw custom scrollbar (delegated to extracted component)
  FScrollbar.Render(Canvas);
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
    // Check if scrollbar handled it (delegated to extracted component)
    if FScrollbar.HandleMouseDown(X, Y) then
    begin
      Invalidate;
      Exit;  // Scrollbar handled it
    end;

    // Regular item click
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

  // Notify scrollbar of mouse enter/move (delegated to extracted component)
  FScrollbar.HandleMouseEnter;
  FScrollbar.HandleMouseMove(X, Y);

  // Update item hover state
  Index := ItemAtPos(X, Y);
  if Index <> FDataSource.HoverIndex then
  begin
    FDataSource.HoverIndex := Index;
    Invalidate;
  end;
end;

procedure TDeviceListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited;

  // Notify scrollbar of mouse up (delegated to extracted component)
  FScrollbar.HandleMouseUp(X, Y);

  if Button = mbLeft then
  begin
    Index := ItemAtPos(X, Y);
    if (Index >= 0) and (Index = FDataSource.SelectedIndex) then
    begin
      // Check if action item (scan button, etc.)
      if FDataSource.Items[Index].Source = dsAction then
      begin
        // Don't trigger if action is in progress (e.g., scanning)
        if not FDataSource.Items[Index].IsActionInProgress then
        begin
          if Assigned(FOnActionClick) then
            FOnActionClick(Self, FDataSource.Items[Index]);
        end;
        Exit;  // Don't process as device click
      end;

      // Regular device click
      if Assigned(FOnDisplayItemClick) then
        FOnDisplayItemClick(Self, FDataSource.Items[Index])
      else if Assigned(FOnDeviceClick) then
        FOnDeviceClick(Self, FDataSource.Items[Index].Device);
    end;
  end
  else if Button = mbRight then
  begin
    // Right-click: show context menu
    Index := ItemAtPos(X, Y);
    if (Index >= 0) and (FDataSource.Items[Index].Source <> dsAction) then
    begin
      // Select item if not already selected
      if Index <> FDataSource.SelectedIndex then
        SetSelectedIndex(Index);

      // Show context menu at cursor position
      ShowContextMenuForItem(Index, X, Y);
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
        if FDataSource.SelectedIndex > 0 then
          SelectedIndex := FDataSource.SelectedIndex - 1
        else if (FDataSource.SelectedIndex = -1) and (Count > 0) then
          SelectedIndex := 0;
        Key := 0;
      end;

    VK_DOWN:
      begin
        if FDataSource.SelectedIndex < Count - 1 then
          SelectedIndex := FDataSource.SelectedIndex + 1
        else if (FDataSource.SelectedIndex = -1) and (Count > 0) then
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
        if FDataSource.SelectedIndex >= 0 then
        begin
          // Check if action item (scan button, etc.)
          if FDataSource.Items[FDataSource.SelectedIndex].Source = dsAction then
          begin
            // Don't trigger if action is in progress (e.g., scanning)
            if not FDataSource.Items[FDataSource.SelectedIndex].IsActionInProgress then
            begin
              if Assigned(FOnActionClick) then
                FOnActionClick(Self, FDataSource.Items[FDataSource.SelectedIndex]);
            end;
          end
          else
          begin
            // Regular device activation
            if Assigned(FOnDisplayItemClick) then
              FOnDisplayItemClick(Self, FDataSource.Items[FDataSource.SelectedIndex])
            else if Assigned(FOnDeviceClick) then
              FOnDeviceClick(Self, FDataSource.Items[FDataSource.SelectedIndex].Device);
          end;
        end;
        Key := 0;
      end;

    VK_F10:
      begin
        // Shift+F10: Show context menu for selected device
        if (ssShift in Shift) and (FDataSource.SelectedIndex >= 0) and
           (FDataSource.Items[FDataSource.SelectedIndex].Source <> dsAction) then
        begin
          // Get item rect and show menu at top-left of item
          var ItemRect := GetItemRect(FDataSource.SelectedIndex);
          ShowContextMenuForItem(FDataSource.SelectedIndex, ItemRect.Left, ItemRect.Top);
          Key := 0;
        end;
      end;
  end;
end;

function TDeviceListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  // Delegate to scrollbar (extracted component)
  Result := FScrollbar.HandleMouseWheel(WheelDelta);
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

procedure TDeviceListBox.HandleAnimationTimer(Sender: TObject);
var
  I: Integer;
  HasActionInProgress: Boolean;
  R: TRect;
  WasZero: Boolean;
begin
  // Check if any action item is in progress
  HasActionInProgress := False;
  for I := 0 to High(FDataSource.Items) do
  begin
    if (FDataSource.Items[I].Source = dsAction) and FDataSource.Items[I].IsActionInProgress then
    begin
      HasActionInProgress := True;

      // If just starting (frame was 0), force immediate update
      WasZero := (FAnimationFrame = 0);

      // Increment animation frame for smooth progress bar movement
      Inc(FAnimationFrame);

      // Invalidate only the action item rect to animate progress
      R := GetItemRect(I);
      InvalidateRect(Handle, @R, False);

      // Force immediate update when starting (no delay)
      if WasZero then
        UpdateWindow(Handle);

      Break;
    end;
  end;

  // Reset animation when no action in progress (so next scan starts from 0)
  if not HasActionInProgress then
    FAnimationFrame := 0;
end;

{ Event Setters (forward to context menu builder) }

procedure TDeviceListBox.SetOnPinToggle(AValue: TDeviceAddressEvent);
begin
  FOnPinToggle := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnPinToggle := AValue;
end;

procedure TDeviceListBox.SetOnCopyName(AValue: TDeviceAddressEvent);
begin
  FOnCopyName := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnCopyName := AValue;
end;

procedure TDeviceListBox.SetOnCopyAddress(AValue: TDeviceAddressEvent);
begin
  FOnCopyAddress := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnCopyAddress := AValue;
end;

procedure TDeviceListBox.SetOnConfigure(AValue: TDeviceAddressEvent);
begin
  FOnConfigure := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnConfigure := AValue;
end;

procedure TDeviceListBox.SetOnUnpair(AValue: TDeviceAddressEvent);
begin
  FOnUnpair := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnUnpair := AValue;
end;

procedure TDeviceListBox.SetOnHide(AValue: TDeviceAddressEvent);
begin
  FOnHide := AValue;
  if Assigned(FContextMenuBuilder) then
    FContextMenuBuilder.OnHide := AValue;
end;

{ Context Menu (delegated to TDeviceContextMenuBuilder) }

procedure TDeviceListBox.ShowContextMenuForItem(AItemIndex: Integer; X, Y: Integer);
var
  Item: TDeviceDisplayItem;
  ScreenPt: TPoint;
begin
  if (AItemIndex < 0) or (AItemIndex >= FDataSource.ItemCount) then
    Exit;

  Item := FDataSource.Items[AItemIndex];

  // Convert control coordinates to screen coordinates
  ScreenPt := ClientToScreen(Point(X, Y));

  // Delegate to context menu builder
  FContextMenuBuilder.ShowMenuForItem(Item, ScreenPt.X, ScreenPt.Y);
end;

end.
