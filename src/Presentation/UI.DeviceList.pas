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
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Themes,
  Vcl.Menus,
  Bluetooth.Types,
  App.ConfigInterfaces,
  App.ConfigEnums,
  App.LayoutConfigIntf,
  App.AppearanceConfigIntf,
  App.ProfileConfigIntf,
  App.DeviceDisplayTypes,
  App.WinRTSupport;

type
  TDeviceClickEvent = procedure(Sender: TObject; const ADevice: TBluetoothDeviceInfo) of object;
  TDeviceAddressEvent = procedure(Sender: TObject; AAddress: UInt64) of object;

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
    IsDiscovered: Boolean;  // True for unpaired discovered devices
  end;

  /// <summary>
  /// Cached layout and appearance parameters.
  /// Populated once per paint cycle to avoid repeated config interface queries.
  /// </summary>
  TCachedLayoutParams = record
    // From ILayoutConfig
    ItemHeight: Integer;
    ItemMargin: Integer;
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
    ListBackgroundSource: TListBackgroundSource;
    ListBackgroundCustomColor: Integer;
    MainColorSource: TMainColorSource;
    MainCustomColor: Integer;
    SecondaryColorSource: TSecondaryColorSource;
    SecondaryCustomColor: Integer;
    HoverColorSource: THoverColorSource;
    HoverCustomColor: Integer;
  end;

  TDeviceDisplayItemClickEvent = procedure(Sender: TObject;
    const AItem: TDeviceDisplayItem) of object;

  /// <summary>
  /// System icon cache for Windows 7 compatibility.
  /// Extracts and caches device icons from shell32.dll and imageres.dll.
  /// On Win10+, icons are rendered using Segoe MDL2 Assets font instead.
  /// </summary>
  TSystemIconCache = class
  private
    FIconHandles: TDictionary<TBluetoothDeviceType, HICON>;
    FIconSize: Integer;
    function TryExtractIcon(const AFilename: string; AIndex: Integer): HICON;
    function LoadIconForDeviceType(ADeviceType: TBluetoothDeviceType): HICON;
  public
    constructor Create(AIconSize: Integer);
    destructor Destroy; override;
    /// <summary>
    /// Gets cached icon for device type. Returns 0 if not available.
    /// </summary>
    function GetIcon(ADeviceType: TBluetoothDeviceType): HICON;
  end;

  /// <summary>
  /// Custom device list control with owner-draw rendering.
  /// Displays pre-processed TDeviceDisplayItem records.
  /// </summary>
  TDeviceListBox = class(TCustomControl)
  private
    FDisplayItems: TDeviceDisplayItemArray;
    FDisplayItemIndexMap: TDictionary<UInt64, Integer>;  // Address -> Index for O(1) lookup
    FItemHeights: TArray<Integer>;
    FHoverIndex: Integer;
    FSelectedIndex: Integer;
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

    // Context menu
    FPopupMenu: TPopupMenu;
    FContextMenuDeviceAddress: UInt64;

    // Injected configuration interfaces (for layout/appearance only)
    FLayoutConfig: ILayoutConfig;
    FAppearanceConfig: IAppearanceConfig;
    FProfileConfig: IProfileConfig;

    // Cached layout parameters (refreshed once per paint cycle)
    FCachedLayout: TCachedLayoutParams;

    // Animation for action button progress
    FAnimationTimer: TTimer;
    FAnimationFrame: Integer;

    // Custom scrollbar state
    FScrollbarHover: Boolean;
    FScrollbarDragging: Boolean;
    FScrollbarDragStartY: Integer;
    FScrollbarDragStartScroll: Integer;
    FListHovered: Boolean;  // True when mouse is over the entire control

    // Win7 system icon cache (nil on Win10+ where we use Segoe MDL2 Assets)
    FSystemIconCache: TSystemIconCache;

    procedure HandleAnimationTimer(Sender: TObject);

    procedure BuildContextMenu(AItemIndex: Integer);
    procedure ShowContextMenuForItem(AItemIndex: Integer; X, Y: Integer);
    procedure HandleMenuPinToggle(Sender: TObject);
    procedure HandleMenuCopyName(Sender: TObject);
    procedure HandleMenuCopyAddress(Sender: TObject);
    procedure HandleMenuConfigure(Sender: TObject);
    procedure HandleMenuUnpair(Sender: TObject);
    procedure HandleMenuHide(Sender: TObject);

    procedure RefreshLayoutCache;
    function GetLayoutConfig: ILayoutConfig;
    function GetAppearanceConfig: IAppearanceConfig;
    function GetProfileConfig: IProfileConfig;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetLayoutConfig(AValue: ILayoutConfig);
    procedure SetAppearanceConfig(AValue: IAppearanceConfig);
    procedure SetSelectedIndex(AValue: Integer);
    function GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
    function GetDeviceCount: Integer;
    function GetItemCount: Integer;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure UpdateScrollRange;
    procedure ScrollTo(APos: Integer);

    procedure RecalculateItemHeights;
    function CalculateItemHeight(AIndex: Integer): Integer;
    function GetProfileSectionHeight(AProfileCount: Integer): Integer;
    function GetProfileLineHeight: Integer;

    procedure DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
      const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
    procedure DrawActionButton(ACanvas: TCanvas; const ARect: TRect;
      const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
    function CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
      AIsHover, AIsSelected, AIsDiscovered: Boolean): TItemDrawContext;
    procedure DrawItemBackground(ACanvas: TCanvas; const AContext: TItemDrawContext);
    procedure DrawItemTopLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);
    procedure DrawItemBottomLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);
    procedure DrawProfileSection(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);
    procedure DrawSeparator(ACanvas: TCanvas; const ARect: TRect);
    procedure DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
      ADeviceType: TBluetoothDeviceType; AIsDiscovered: Boolean);
    function GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
    function GetBatteryIconChar(ALevel: Integer): Char;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure UpdateScrollBar;

    // Custom scrollbar helpers
    function GetScrollbarRect: TRect;
    function GetScrollbarTrackRect: TRect;
    function GetScrollbarThumbRect: TRect;
    function IsPointInScrollbar(X, Y: Integer): Boolean;
    function IsPointInScrollbarThumb(X, Y: Integer): Boolean;
    procedure DrawCustomScrollbar(ACanvas: TCanvas);

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
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property ShowAddresses: Boolean read FShowAddresses write SetShowAddresses;
    property OnDeviceClick: TDeviceClickEvent read FOnDeviceClick write FOnDeviceClick;
    property OnDisplayItemClick: TDeviceDisplayItemClickEvent read FOnDisplayItemClick write FOnDisplayItemClick;
    property OnActionClick: TDeviceDisplayItemClickEvent read FOnActionClick write FOnActionClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnPinToggle: TDeviceAddressEvent read FOnPinToggle write FOnPinToggle;
    property OnCopyName: TDeviceAddressEvent read FOnCopyName write FOnCopyName;
    property OnCopyAddress: TDeviceAddressEvent read FOnCopyAddress write FOnCopyAddress;
    property OnConfigure: TDeviceAddressEvent read FOnConfigure write FOnConfigure;
    property OnUnpair: TDeviceAddressEvent read FOnUnpair write FOnUnpair;
    property OnHide: TDeviceAddressEvent read FOnHide write FOnHide;

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
  System.Math,
  UI.ListGeometry,
  UI.DeviceFormatter,
  App.Logger;

const
  // Font names used for rendering
  FONT_UI = 'Segoe UI';
  FONT_ICONS = 'Segoe MDL2 Assets';

  // Profile section constants
  PROFILE_INDENT = 20;           // Indent from left edge
  PROFILE_ICON_SIZE = 10;        // Font size for profile icons
  DEFAULT_PROFILE_FONT_SIZE = 7; // Fallback when config unavailable
  PROFILE_LINE_HEIGHT_FACTOR = 2; // LineHeight = FontSize * Factor

  // Icon characters from Segoe MDL2 Assets (Win10+)
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
  // Battery icons: $E850-$E859 for 0%-90% (10% steps), $E83F for 100%
  ICON_BATTERY_0 = #$E850;         // 0% (empty)
  ICON_BATTERY_100 = #$E83F;       // 100% (full)

  // System icon indices for Win7 compatibility (extracted from ddores.dll)
  // ddores.dll = Device Center resources, contains proper device icons on Win7+
  // Verified indices using IconViewer tool on actual Win7 installation
  SYSICON_HEADPHONE_FILE = 'ddores.dll';
  SYSICON_HEADPHONE_INDEX = 2;        // Headphones/speakers (also: 6=headset, 7=headset variant)
  SYSICON_KEYBOARD_FILE = 'ddores.dll';
  SYSICON_KEYBOARD_INDEX = 26;        // Keyboard icon
  SYSICON_MOUSE_FILE = 'ddores.dll';
  SYSICON_MOUSE_INDEX = 27;           // Mouse icon
  SYSICON_COMPUTER_FILE = 'ddores.dll';
  SYSICON_COMPUTER_INDEX = 12;        // Desktop computer icon
  SYSICON_PHONE_FILE = 'ddores.dll';
  SYSICON_PHONE_INDEX = 11;           // Mobile device icon
  SYSICON_GAMEPAD_FILE = 'ddores.dll';
  SYSICON_GAMEPAD_INDEX = 25;         // Gamepad/controller icon
  SYSICON_GENERIC_FILE = 'ddores.dll';
  SYSICON_GENERIC_INDEX = 0;          // Generic device icon

  // Layout spacing constants
  FOCUS_RECT_INSET = 2;        // Pixels to inset focus rectangle from item bounds
  PIN_ICON_FONT_SIZE = 10;     // Font size for pin icon
  ADDRESS_SPACING = 8;         // Space between device name and address
  BATTERY_SPACING = 4;         // Space between battery text and icon
  BATTERY_FONT_SIZE = 9;       // Font size for battery percentage text
  BATTERY_ICON_FONT_SIZE = 12; // Font size for battery icon

  // Default control dimensions
  DEFAULT_CONTROL_WIDTH = 300;
  DEFAULT_CONTROL_HEIGHT = 400;

{ TSystemIconCache }

constructor TSystemIconCache.Create(AIconSize: Integer);
begin
  inherited Create;
  FIconHandles := TDictionary<TBluetoothDeviceType, HICON>.Create;
  FIconSize := AIconSize;
end;

destructor TSystemIconCache.Destroy;
var
  IconHandle: HICON;
begin
  // Free all cached icon handles
  for IconHandle in FIconHandles.Values do
  begin
    if IconHandle <> 0 then
      DestroyIcon(IconHandle);
  end;
  FIconHandles.Free;
  inherited Destroy;
end;

function TSystemIconCache.TryExtractIcon(const AFilename: string; AIndex: Integer): HICON;
var
  SystemPath: string;
  FullPath: string;
  LargeIcon, SmallIcon: HICON;
  ExtractedCount: UINT;
begin
  Result := 0;

  // Build full path to system DLL
  SetLength(SystemPath, MAX_PATH);
  SetLength(SystemPath, GetSystemDirectory(PChar(SystemPath), MAX_PATH));
  FullPath := IncludeTrailingPathDelimiter(SystemPath) + AFilename;

  // Extract icon at specified size
  // We request both large and small, but only use the one matching our size
  LargeIcon := 0;
  SmallIcon := 0;

  // ExtractIconEx: negative index extracts count, non-negative extracts specific icon
  ExtractedCount := ExtractIconEx(PChar(FullPath), AIndex, LargeIcon, SmallIcon, 1);

  if ExtractedCount > 0 then
  begin
    // Choose icon based on requested size (use small icon for sizes <= 32)
    if FIconSize <= 32 then
    begin
      Result := SmallIcon;
      if LargeIcon <> 0 then
        DestroyIcon(LargeIcon);
    end
    else
    begin
      Result := LargeIcon;
      if SmallIcon <> 0 then
        DestroyIcon(SmallIcon);
    end;
  end;
end;

function TSystemIconCache.LoadIconForDeviceType(ADeviceType: TBluetoothDeviceType): HICON;
begin
  // Try to extract appropriate system icon based on device type
  case ADeviceType of
    btHeadset,
    btAudioOutput:
      Result := TryExtractIcon(SYSICON_HEADPHONE_FILE, SYSICON_HEADPHONE_INDEX);
    btAudioInput:
      Result := TryExtractIcon(SYSICON_HEADPHONE_FILE, SYSICON_HEADPHONE_INDEX);
    btKeyboard:
      Result := TryExtractIcon(SYSICON_KEYBOARD_FILE, SYSICON_KEYBOARD_INDEX);
    btMouse:
      Result := TryExtractIcon(SYSICON_MOUSE_FILE, SYSICON_MOUSE_INDEX);
    btGamepad:
      Result := TryExtractIcon(SYSICON_GAMEPAD_FILE, SYSICON_GAMEPAD_INDEX);
    btComputer:
      Result := TryExtractIcon(SYSICON_COMPUTER_FILE, SYSICON_COMPUTER_INDEX);
    btPhone:
      Result := TryExtractIcon(SYSICON_PHONE_FILE, SYSICON_PHONE_INDEX);
    btHID:
      Result := TryExtractIcon(SYSICON_KEYBOARD_FILE, SYSICON_KEYBOARD_INDEX);
  else
    Result := TryExtractIcon(SYSICON_GENERIC_FILE, SYSICON_GENERIC_INDEX);
  end;

  // Fallback to generic icon if specific icon failed
  if (Result = 0) and (ADeviceType <> btUnknown) then
    Result := TryExtractIcon(SYSICON_GENERIC_FILE, SYSICON_GENERIC_INDEX);
end;

function TSystemIconCache.GetIcon(ADeviceType: TBluetoothDeviceType): HICON;
begin
  // Check cache first
  if not FIconHandles.TryGetValue(ADeviceType, Result) then
  begin
    // Not in cache - load and cache it
    Result := LoadIconForDeviceType(ADeviceType);
    if Result <> 0 then
      FIconHandles.Add(ADeviceType, Result);
  end;
end;

{ TDeviceListBox }

constructor TDeviceListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayItems := nil;
  FDisplayItemIndexMap := TDictionary<UInt64, Integer>.Create;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FScrollPos := 0;
  FMaxScroll := 0;
  FShowAddresses := False;
  FAnimationFrame := 0;
  FScrollbarHover := False;
  FScrollbarDragging := False;
  FScrollbarDragStartY := 0;
  FScrollbarDragStartScroll := 0;
  FListHovered := False;

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

  // Create animation timer for action button progress indicator
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 50;  // 20 FPS for smooth animation
  FAnimationTimer.OnTimer := HandleAnimationTimer;
  FAnimationTimer.Enabled := True;

  // Create context menu (will be populated dynamically)
  FPopupMenu := TPopupMenu.Create(Self);
  FContextMenuDeviceAddress := 0;
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

function TDeviceListBox.GetProfileLineHeight: Integer;
var
  FontSize: Integer;
begin
  if Assigned(FProfileConfig) then
    FontSize := FProfileConfig.ProfileFontSize
  else
    FontSize := DEFAULT_PROFILE_FONT_SIZE;
  Result := FontSize * PROFILE_LINE_HEIGHT_FACTOR;
end;

function TDeviceListBox.GetProfileSectionHeight(AProfileCount: Integer): Integer;
var
  LineHeight: Integer;
begin
  if AProfileCount <= 1 then
    Result := 0
  else
  begin
    LineHeight := GetProfileLineHeight;
    // Profiles start right after status line, add bottom padding
    Result := AProfileCount * LineHeight + LineHeight div 2;
  end;
end;

function TDeviceListBox.CalculateItemHeight(AIndex: Integer): Integer;
const
  ACTION_BUTTON_HEIGHT = 28;  // Thin button height
  ACTION_BUTTON_PADDING = 8;  // Top and bottom padding
var
  BaseHeight: Integer;
  ProfileCount: Integer;
begin
  // Action items (scan button, etc.) have smaller height
  if (AIndex >= 0) and (AIndex < Length(FDisplayItems)) and
     (FDisplayItems[AIndex].Source = dsAction) then
  begin
    Result := ACTION_BUTTON_HEIGHT + (ACTION_BUTTON_PADDING * 2);
    Exit;
  end;

  BaseHeight := FCachedLayout.ItemHeight;

  // Add profile section height if profiles are shown
  if Assigned(FProfileConfig) and FProfileConfig.ShowProfiles and
     (AIndex >= 0) and (AIndex < Length(FDisplayItems)) then
  begin
    ProfileCount := Length(FDisplayItems[AIndex].Profiles);
    Result := BaseHeight + GetProfileSectionHeight(ProfileCount);
  end
  else
    Result := BaseHeight;
end;

procedure TDeviceListBox.RecalculateItemHeights;
var
  I: Integer;
begin
  SetLength(FItemHeights, Length(FDisplayItems));
  for I := 0 to High(FDisplayItems) do
    FItemHeights[I] := CalculateItemHeight(I);
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
end;

procedure TDeviceListBox.SetAppearanceConfig(AValue: IAppearanceConfig);
begin
  FAppearanceConfig := AValue;
  RefreshLayoutCache;
end;

destructor TDeviceListBox.Destroy;
begin
  FDisplayItemIndexMap.Free;
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
  UpdateScrollRange;
end;

procedure TDeviceListBox.Clear;
begin
  FDisplayItems := nil;
  FDisplayItemIndexMap.Clear;  // Clear index map to prevent stale lookups
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
var
  I: Integer;
  OldScrollPos: Integer;
  OldHoverAddress: UInt64;
  OldSelectedAddress: UInt64;
  NewIndex: Integer;
begin
  // Save current scroll position to preserve it across refreshes
  OldScrollPos := FScrollPos;

  // Save addresses of currently hovered/selected items to restore them after rebuild
  if (FHoverIndex >= 0) and (FHoverIndex < Length(FDisplayItems)) then
    OldHoverAddress := FDisplayItems[FHoverIndex].Device.AddressInt
  else
    OldHoverAddress := 0;

  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    OldSelectedAddress := FDisplayItems[FSelectedIndex].Device.AddressInt
  else
    OldSelectedAddress := 0;

  FDisplayItems := AItems;

  // Rebuild index map for O(1) lookup by device address
  FDisplayItemIndexMap.Clear;
  for I := 0 to High(FDisplayItems) do
    FDisplayItemIndexMap.Add(FDisplayItems[I].Device.AddressInt, I);

  RecalculateItemHeights;

  // Restore hover state by looking up the device in the new list
  if OldHoverAddress <> 0 then
  begin
    if FDisplayItemIndexMap.TryGetValue(OldHoverAddress, NewIndex) then
      FHoverIndex := NewIndex
    else
      FHoverIndex := -1;
  end
  else
    FHoverIndex := -1;

  // Restore selection state by looking up the device in the new list
  if OldSelectedAddress <> 0 then
  begin
    if FDisplayItemIndexMap.TryGetValue(OldSelectedAddress, NewIndex) then
      FSelectedIndex := NewIndex
    else
      FSelectedIndex := -1;
  end
  else if FSelectedIndex >= Length(FDisplayItems) then
    FSelectedIndex := -1;

  // Restore scroll position (will be clamped by UpdateScrollRange if needed)
  FScrollPos := OldScrollPos;
  UpdateScrollRange;
  Invalidate;

  // If action item is in progress, force immediate update (no delay)
  for I := 0 to High(FDisplayItems) do
  begin
    if (FDisplayItems[I].Source = dsAction) and FDisplayItems[I].IsActionInProgress then
    begin
      UpdateWindow(Handle);
      Break;
    end;
  end;
end;

procedure TDeviceListBox.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
var
  Index: Integer;
begin
  // O(1) lookup using index map instead of O(n) linear search
  if FDisplayItemIndexMap.TryGetValue(AItem.Device.AddressInt, Index) then
  begin
    FDisplayItems[Index] := AItem;
    // Recalculate height for this item in case profiles changed
    if Index < Length(FItemHeights) then
      FItemHeights[Index] := CalculateItemHeight(Index);
    UpdateScrollRange;
    Invalidate;
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
  if Length(FItemHeights) > 0 then
    Result := TListGeometry.GetItemRectVariable(
      AIndex,
      FItemHeights,
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
  if Length(FItemHeights) > 0 then
    Result := TListGeometry.ItemAtPosVariable(
      X, Y,
      FItemHeights,
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
  if Length(FItemHeights) > 0 then
    FMaxScroll := TListGeometry.CalculateMaxScrollVariable(
      FItemHeights,
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
  UpdateScrollBar;
end;

procedure TDeviceListBox.UpdateScrollBar;
begin
  if not HandleAllocated then
    Exit;

  // Hide native scrollbar (we use custom scrollbar now)
  ShowScrollBar(Handle, SB_VERT, False);

  // Force repaint to show custom scrollbar
  Invalidate;
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

  if Length(FItemHeights) > 0 then
    NewScrollPos := TListGeometry.ScrollPosToMakeVisibleVariable(
      AIndex,
      FItemHeights,
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
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
  if FScrollbarHover then
  begin
    FScrollbarHover := False;
    Invalidate;
  end;
  if FListHovered then
  begin
    FListHovered := False;
    Invalidate;  // Redraw to hide scrollbar
  end;
end;

procedure TDeviceListBox.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  // VCL style has changed, force repaint to update colors
  LogDebug('CMStyleChanged: VCL style changed, invalidating device list', ClassName);
  Invalidate;
end;

function TDeviceListBox.GetScrollbarRect: TRect;
const
  SCROLLBAR_WIDTH = 12;  // Modern thin scrollbar width (px)
begin
  Result := Rect(
    ClientWidth - SCROLLBAR_WIDTH,
    0,
    ClientWidth,
    ClientHeight
  );
end;

function TDeviceListBox.GetScrollbarTrackRect: TRect;
begin
  Result := GetScrollbarRect;
end;

function TDeviceListBox.GetScrollbarThumbRect: TRect;
var
  TrackRect: TRect;
  TrackHeight, ThumbHeight, ThumbTop: Integer;
  ScrollRatio: Double;
begin
  TrackRect := GetScrollbarTrackRect;
  TrackHeight := TrackRect.Height;

  if FMaxScroll <= 0 then
  begin
    // No scrolling needed - full height thumb
    Result := TrackRect;
    Exit;
  end;

  // Thumb height proportional to visible content
  // ThumbHeight = TrackHeight * (ClientHeight / TotalContentHeight)
  ThumbHeight := Round(TrackHeight * (ClientHeight / (ClientHeight + FMaxScroll)));

  // Minimum thumb height for usability
  if ThumbHeight < 20 then
    ThumbHeight := 20;

  // Thumb position based on scroll position
  ScrollRatio := FScrollPos / FMaxScroll;
  ThumbTop := Round(ScrollRatio * (TrackHeight - ThumbHeight));

  Result := Rect(
    TrackRect.Left,
    TrackRect.Top + ThumbTop,
    TrackRect.Right,
    TrackRect.Top + ThumbTop + ThumbHeight
  );
end;

function TDeviceListBox.IsPointInScrollbar(X, Y: Integer): Boolean;
var
  ScrollbarRect: TRect;
begin
  ScrollbarRect := GetScrollbarRect;
  Result := PtInRect(ScrollbarRect, Point(X, Y));
end;

function TDeviceListBox.IsPointInScrollbarThumb(X, Y: Integer): Boolean;
var
  ThumbRect: TRect;
begin
  ThumbRect := GetScrollbarThumbRect;
  Result := PtInRect(ThumbRect, Point(X, Y));
end;

procedure TDeviceListBox.DrawCustomScrollbar(ACanvas: TCanvas);
var
  TrackRect, ThumbRect: TRect;
  Style: TCustomStyleServices;
  TrackColor, ThumbColor: TColor;
  ThumbInset: Integer;
begin
  // Only draw scrollbar when list is hovered or scrollbar is being dragged
  if not (FListHovered or FScrollbarDragging) then
    Exit;

  if FMaxScroll <= 0 then
    Exit; // No scrollbar needed

  Style := TStyleManager.ActiveStyle;
  TrackRect := GetScrollbarTrackRect;
  ThumbRect := GetScrollbarThumbRect;

  // Track background - use VCL theme color
  TrackColor := Style.GetSystemColor(clBtnFace);
  ACanvas.Brush.Color := TrackColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psClear;
  ACanvas.FillRect(TrackRect);

  // Thumb - use theme color, lighter on hover
  if FScrollbarHover or FScrollbarDragging then
    ThumbColor := Style.GetSystemColor(clHighlight)
  else
    ThumbColor := Style.GetSystemColor(clBtnShadow);

  ACanvas.Brush.Color := ThumbColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psClear;

  // Draw rounded thumb centered in track with equal insets on both sides
  ThumbInset := 3;  // Equal margin on left and right for symmetry
  ACanvas.RoundRect(
    ThumbRect.Left + ThumbInset,
    ThumbRect.Top,
    ThumbRect.Right - ThumbInset,
    ThumbRect.Bottom,
    4, 4
  );
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

  // Background (color source from config)
  case FCachedLayout.ListBackgroundSource of
    lbsThemeWindow: Canvas.Brush.Color := Style.GetSystemColor(clWindow);
    lbsThemeForm:   Canvas.Brush.Color := Style.GetSystemColor(clBtnFace);
    lbsCustom:      Canvas.Brush.Color := TColor(FCachedLayout.ListBackgroundCustomColor);
  else
    Canvas.Brush.Color := Style.GetSystemColor(clWindow);  // Fallback
  end;
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

    // Draw separator before first non-paired item (action button or discovered device)
    if (I > 0) and
       (FDisplayItems[I].Source in [dsAction, dsDiscovered]) and
       (FDisplayItems[I - 1].Source = dsPaired) then
    begin
      DrawSeparator(Canvas, R);
      // Adjust item rect to start below separator to prevent background overlap
      // Separator is drawn at ARect.Top + (ItemMargin div 2), so offset by ItemMargin
      R.Top := R.Top + FCachedLayout.ItemMargin;
    end;

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

  // Draw custom scrollbar
  DrawCustomScrollbar(Canvas);
end;

function TDeviceListBox.CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
  AIsHover, AIsSelected, AIsDiscovered: Boolean): TItemDrawContext;
var
  StatusLineHeight: Integer;
  IconRect: TRect;
  BaseHeight: Integer;
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
  Result.IsDiscovered := AIsDiscovered;

  // Calculate status line height for bottom anchoring
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := Result.StatusFontSize;
  StatusLineHeight := ACanvas.TextHeight('Ay');

  // Get base height (without profile section) for proper positioning
  // Status line should be anchored to base height, not expanded rect
  BaseHeight := FCachedLayout.ItemHeight;

  Result.NameLineTop := ARect.Top + Result.ItemPadding;
  // Anchor status line to base height area, not full expanded height
  Result.StatusLineTop := ARect.Top + BaseHeight - Result.ItemPadding - StatusLineHeight;

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
  BgColor, BaseBgColor: TColor;
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  // Determine base background color from config
  case FCachedLayout.ListBackgroundSource of
    lbsThemeWindow: BaseBgColor := Style.GetSystemColor(clWindow);
    lbsThemeForm:   BaseBgColor := Style.GetSystemColor(clBtnFace);
    lbsCustom:      BaseBgColor := TColor(FCachedLayout.ListBackgroundCustomColor);
  else
    BaseBgColor := Style.GetSystemColor(clWindow);  // Fallback
  end;

  // Apply hover effect using configured hover color
  if AContext.IsHover then
  begin
    case FCachedLayout.HoverColorSource of
      hcsThemeWindow: BgColor := Style.GetSystemColor(clWindow);
      hcsThemeForm:   BgColor := Style.GetSystemColor(clBtnFace);
      hcsCustom:      BgColor := TColor(FCachedLayout.HoverCustomColor);
    else
      BgColor := Style.GetSystemColor(clBtnFace);  // Fallback
    end;
  end
  else
    BgColor := BaseBgColor;

  // Draw rounded rectangle background (fill only, no pen outline)
  ACanvas.Brush.Color := BgColor;
  ACanvas.Pen.Style := psClear;
  ACanvas.RoundRect(
    AContext.ItemRect.Left, AContext.ItemRect.Top,
    AContext.ItemRect.Right, AContext.ItemRect.Bottom,
    AContext.CornerRadius, AContext.CornerRadius);
  ACanvas.Pen.Style := psSolid;

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
  PinIconWidth: Integer;
  NameColor: TColor;
  SavedClipRgn: HRGN;
begin
  Style := TStyleManager.ActiveStyle;

  // Determine text color based on state
  if AContext.IsDiscovered then
  begin
    // Use Secondary color for unpaired/discovered devices
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     NameColor := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: NameColor := Style.GetSystemColor(clGrayText);
      scsCustom:        NameColor := TColor(FCachedLayout.SecondaryCustomColor);
    else
      NameColor := Style.GetSystemColor(clGrayText);  // Fallback
    end;
  end
  else if AContext.IsSelected then
    NameColor := Style.GetSystemColor(clHighlightText)
  else
  begin
    // Use Main color for paired devices
    case FCachedLayout.MainColorSource of
      mcsThemeText: NameColor := Style.GetSystemColor(clWindowText);
      mcsCustom:    NameColor := TColor(FCachedLayout.MainCustomColor);
    else
      NameColor := Style.GetSystemColor(clWindowText);  // Fallback
    end;
  end;

  // Set up font for device name
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.DeviceNameFontSize;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := NameColor;
  ACanvas.Brush.Style := bsClear;

  // Start from right edge of text area (already has padding applied)
  RightEdge := AContext.TextRect.Right;

  // Draw pin indicator (rightmost on top line)
  if AItem.IsPinned then
  begin
    ACanvas.Font.Name := FONT_ICONS;
    ACanvas.Font.Size := PIN_ICON_FONT_SIZE;
    // Use Secondary color for pin icon
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
      scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
    end;
    PinIconWidth := ACanvas.TextWidth(ICON_PIN);
    RightEdge := RightEdge - PinIconWidth;
    ACanvas.TextOut(RightEdge, AContext.NameLineTop, ICON_PIN);
  end;

  // Restore font for name
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.DeviceNameFontSize;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := NameColor;

  // Draw device name
  ACanvas.TextOut(AContext.TextRect.Left, AContext.NameLineTop, AItem.DisplayName);

  // Draw address if enabled (with clipping to respect right padding)
  if AContext.ShowAddresses then
  begin
    AddrLeft := AContext.TextRect.Left + ACanvas.TextWidth(AItem.DisplayName) + ADDRESS_SPACING;

    // Only draw if there's space for at least the opening bracket
    if AddrLeft < AContext.TextRect.Right then
    begin
      NameHeight := ACanvas.TextHeight('Ay');
      ACanvas.Font.Size := AContext.AddressFontSize;
      // Use Secondary color for device address
      case FCachedLayout.SecondaryColorSource of
        scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
        scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
        scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
      else
        ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
      end;
      AddrOffset := (NameHeight - ACanvas.TextHeight('Ay')) div 2;

      // Clip address text to TextRect.Right boundary to respect padding
      SavedClipRgn := CreateRectRgn(0, 0, 0, 0);
      if GetClipRgn(ACanvas.Handle, SavedClipRgn) <> 1 then
      begin
        DeleteObject(SavedClipRgn);
        SavedClipRgn := 0;
      end;

      IntersectClipRect(ACanvas.Handle,
        AddrLeft,
        AContext.NameLineTop + AddrOffset,
        AContext.TextRect.Right,
        AContext.NameLineTop + AddrOffset + ACanvas.TextHeight('Ay'));

      ACanvas.TextOut(AddrLeft, AContext.NameLineTop + AddrOffset,
        '[' + AItem.Device.AddressString + ']');

      // Restore original clipping region
      if SavedClipRgn <> 0 then
      begin
        SelectClipRgn(ACanvas.Handle, SavedClipRgn);
        DeleteObject(SavedClipRgn);
      end
      else
        SelectClipRgn(ACanvas.Handle, 0);
    end;
  end;
end;

procedure TDeviceListBox.DrawItemBottomLine(ACanvas: TCanvas;
  const AItem: TDeviceDisplayItem; const AContext: TItemDrawContext);
var
  Style: TCustomStyleServices;
  StatusText: string;
  BatteryIconChar: Char;
  BatteryIconWidth, BatteryTextWidth: Integer;
  StatusLineHeight, BatteryOffset: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.StatusFontSize;
  ACanvas.Font.Style := [];

  StatusLineHeight := ACanvas.TextHeight('Ay');

  // Status line (left-aligned)
  // Priority 1: Custom status text (e.g., pairing progress)
  if AItem.StatusText <> '' then
  begin
    StatusText := AItem.StatusText;
    // Use Secondary color for custom status text
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
      scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
    end;
    ACanvas.TextOut(AContext.TextRect.Left, AContext.StatusLineTop, StatusText);
  end
  // Priority 2: Default status based on device type and state
  else if AContext.IsDiscovered then
  begin
    // Discovered devices: show last seen time if available
    if AContext.ShowLastSeen and (AItem.LastSeenText <> '') then
    begin
      StatusText := AItem.LastSeenText;
      // Use Secondary color for last seen text
      case FCachedLayout.SecondaryColorSource of
        scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
        scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
        scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
      else
        ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
      end;
      ACanvas.TextOut(AContext.TextRect.Left, AContext.StatusLineTop, StatusText);
    end;
  end
  else
  begin
    // Paired devices: show connection status + last seen for disconnected
    StatusText := TDeviceFormatter.FormatConnectionState(AItem.Device.ConnectionState);
    if AContext.ShowLastSeen and (not AItem.Device.IsConnected) and (AItem.LastSeenText <> '') then
      StatusText := StatusText + '. ' + AItem.LastSeenText;

    if AItem.Device.IsConnected then
      ACanvas.Font.Color := AContext.ConnectedColor
    else
    begin
      // Use Secondary color for disconnected status
      case FCachedLayout.SecondaryColorSource of
        scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
        scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
        scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
      else
        ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
      end;
    end;

    ACanvas.TextOut(AContext.TextRect.Left, AContext.StatusLineTop, StatusText);
  end;

  // Battery indicator (right-aligned on bottom line)
  if AItem.BatteryStatus.HasLevel then
  begin
    // Draw battery icon (rightmost)
    BatteryIconChar := GetBatteryIconChar(AItem.BatteryStatus.Level);
    ACanvas.Font.Name := FONT_ICONS;
    ACanvas.Font.Size := BATTERY_ICON_FONT_SIZE;
    // Use Secondary color for battery icon
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
      scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
    end;
    BatteryIconWidth := ACanvas.TextWidth(BatteryIconChar);
    BatteryOffset := (StatusLineHeight - ACanvas.TextHeight(BatteryIconChar)) div 2;
    ACanvas.TextOut(AContext.TextRect.Right - BatteryIconWidth,
      AContext.StatusLineTop + BatteryOffset, BatteryIconChar);

    // Draw battery percentage text (left of icon)
    ACanvas.Font.Name := FONT_UI;
    ACanvas.Font.Size := BATTERY_FONT_SIZE;
    // Use Secondary color for battery text
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
      scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
    end;
    BatteryTextWidth := ACanvas.TextWidth(AItem.BatteryText);
    BatteryOffset := (StatusLineHeight - ACanvas.TextHeight(AItem.BatteryText)) div 2;
    ACanvas.TextOut(AContext.TextRect.Right - BatteryIconWidth - BATTERY_SPACING - BatteryTextWidth,
      AContext.StatusLineTop + BatteryOffset, AItem.BatteryText);
  end;
end;

procedure TDeviceListBox.DrawProfileSection(ACanvas: TCanvas;
  const AItem: TDeviceDisplayItem; const AContext: TItemDrawContext);
var
  Style: TCustomStyleServices;
  I: Integer;
  ProfileTop: Integer;
  ProfileFontSize: Integer;
  LineHeight: Integer;
  ProfileText: string;
  TreeChar: string;
  IsLast: Boolean;
  TreeX: Integer;
begin
  if Length(AItem.Profiles) <= 1 then
    Exit;

  Style := TStyleManager.ActiveStyle;

  // Get font size from config or use default
  if Assigned(FProfileConfig) then
    ProfileFontSize := FProfileConfig.ProfileFontSize
  else
    ProfileFontSize := DEFAULT_PROFILE_FONT_SIZE;

  // Use shared helper to ensure consistency with GetProfileSectionHeight
  LineHeight := GetProfileLineHeight;

  // Calculate profile position - start just below status line
  // Use status font to measure status line height correctly
  ACanvas.Font.Name := FONT_UI;
  ACanvas.Font.Size := AContext.StatusFontSize;
  TreeX := AContext.TextRect.Left;
  // Profiles start immediately after status text
  ProfileTop := AContext.StatusLineTop + ACanvas.TextHeight('A');

  // Now set up font for profiles
  ACanvas.Font.Size := ProfileFontSize;
  ACanvas.Font.Style := [];
  // Use Secondary color for profile names
  case FCachedLayout.SecondaryColorSource of
    scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
    scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
    scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
  else
    ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
  end;
  ACanvas.Brush.Style := bsClear;

  // Draw each profile with tree connectors
  for I := 0 to High(AItem.Profiles) do
  begin
    ProfileText := AItem.Profiles[I].DisplayName;
    IsLast := (I = High(AItem.Profiles));

    // Use box-drawing characters for tree structure
    if IsLast then
      TreeChar := #$2514 + #$2500  // "└─" (corner + horizontal)
    else
      TreeChar := #$251C + #$2500; // "├─" (tee + horizontal)

    // Draw tree connector
    ACanvas.TextOut(TreeX, ProfileTop, TreeChar);

    // Draw profile name (offset by connector width)
    ACanvas.TextOut(
      TreeX + ACanvas.TextWidth(TreeChar) + 4,
      ProfileTop,
      ProfileText
    );

    ProfileTop := ProfileTop + LineHeight;
  end;
end;

procedure TDeviceListBox.DrawSeparator(ACanvas: TCanvas; const ARect: TRect);
var
  Style: TCustomStyleServices;
  SeparatorY: Integer;
  SeparatorColor: TColor;
  LeftMargin, RightMargin: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  // Windows 11 style separator: subtle horizontal line with margins
  SeparatorY := ARect.Top + (FCachedLayout.ItemMargin div 2);
  SeparatorColor := Style.GetSystemColor(clBtnShadow);

  // Add horizontal margins (indent separator slightly)
  LeftMargin := FCachedLayout.ItemPadding;
  RightMargin := FCachedLayout.ItemPadding;

  ACanvas.Pen.Color := SeparatorColor;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  ACanvas.MoveTo(ARect.Left + LeftMargin, SeparatorY);
  ACanvas.LineTo(ARect.Right - RightMargin, SeparatorY);
end;

procedure TDeviceListBox.DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
  const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
var
  Context: TItemDrawContext;
  IconRect: TRect;
  BaseHeight: Integer;
begin
  // Action items (scan button, etc.) use special rendering
  if AItem.Source = dsAction then
  begin
    DrawActionButton(ACanvas, ARect, AItem, AIsHover, AIsSelected);
    Exit;
  end;

  // Create context with all layout parameters
  Context := CreateDrawContext(ACanvas, ARect, AIsHover, AIsSelected, AItem.Source = dsDiscovered);

  // Draw background and border
  DrawItemBackground(ACanvas, Context);

  // Calculate base height (without profile section) for icon centering
  BaseHeight := FCachedLayout.ItemHeight;

  // Draw device icon if enabled (centered in base height area)
  if Context.ShowDeviceIcons then
  begin
    IconRect.Left := Context.ItemRect.Left + Context.ItemPadding;
    IconRect.Top := Context.ItemRect.Top + (BaseHeight - Context.IconSize) div 2;
    IconRect.Right := IconRect.Left + Context.IconSize;
    IconRect.Bottom := IconRect.Top + Context.IconSize;
    DrawDeviceIcon(ACanvas, IconRect, AItem.EffectiveDeviceType, Context.IsDiscovered);
  end;

  // Draw text content
  DrawItemTopLine(ACanvas, AItem, Context);
  DrawItemBottomLine(ACanvas, AItem, Context);

  // Draw profile section if profiles are present
  if Length(AItem.Profiles) > 1 then
    DrawProfileSection(ACanvas, AItem, Context);

  ACanvas.Brush.Style := bsSolid;
end;

procedure TDeviceListBox.DrawActionButton(ACanvas: TCanvas; const ARect: TRect;
  const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);
const
  BUTTON_HEIGHT = 28;  // Thin Windows 11 style button
  TEXT_LEFT_PADDING = 12;  // Left padding for text
  PROGRESS_HEIGHT = 2;  // Thin progress bar
  PROGRESS_SEGMENT_WIDTH = 100;  // Width of moving segment
var
  Style: TCustomStyleServices;
  TextColor, ProgressColor, ProgressBgColor: TColor;
  TextRect, ProgressRect, SegmentRect: TRect;
  ButtonText: string;
  ButtonTop: Integer;
  BarWidth, SegmentPos: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  // Create thin button rect, centered vertically in ARect
  ButtonTop := ARect.Top + (ARect.Bottom - ARect.Top - BUTTON_HEIGHT) div 2;

  if AItem.IsActionInProgress then
  begin
    // Draw animated progress bar instead of text
    BarWidth := ARect.Right - ARect.Left - (FCachedLayout.ItemPadding * 2);

    // Progress bar background
    ProgressRect := Rect(
      ARect.Left + FCachedLayout.ItemPadding,
      ButtonTop + (BUTTON_HEIGHT - PROGRESS_HEIGHT) div 2,
      ARect.Right - FCachedLayout.ItemPadding,
      ButtonTop + (BUTTON_HEIGHT - PROGRESS_HEIGHT) div 2 + PROGRESS_HEIGHT
    );

    ProgressBgColor := Style.GetSystemColor(clBtnFace);
    ACanvas.Brush.Color := ProgressBgColor;
    ACanvas.FillRect(ProgressRect);

    // Animated progress segment (sliding left to right, faster movement)
    SegmentPos := (FAnimationFrame * 8) mod (BarWidth + PROGRESS_SEGMENT_WIDTH);
    SegmentPos := SegmentPos - PROGRESS_SEGMENT_WIDTH;  // Start off-screen left

    SegmentRect := Rect(
      ProgressRect.Left + SegmentPos,
      ProgressRect.Top,
      ProgressRect.Left + SegmentPos + PROGRESS_SEGMENT_WIDTH,
      ProgressRect.Bottom
    );

    // Clip segment to progress bar bounds
    if SegmentRect.Left < ProgressRect.Left then
      SegmentRect.Left := ProgressRect.Left;
    if SegmentRect.Right > ProgressRect.Right then
      SegmentRect.Right := ProgressRect.Right;

    ProgressColor := Style.GetSystemColor(clHighlight);
    ACanvas.Brush.Color := ProgressColor;
    ACanvas.FillRect(SegmentRect);
  end
  else
  begin
    // Draw button text
    ButtonText := AItem.DisplayName;

    // Use Main color for action button text
    case FCachedLayout.MainColorSource of
      mcsThemeText: TextColor := Style.GetSystemColor(clWindowText);
      mcsCustom:    TextColor := TColor(FCachedLayout.MainCustomColor);
    else
      TextColor := Style.GetSystemColor(clWindowText);  // Fallback
    end;

    // Set up font - clean, left-aligned text
    ACanvas.Font.Name := FONT_UI;
    ACanvas.Font.Size := FCachedLayout.StatusFontSize;
    ACanvas.Font.Color := TextColor;
    ACanvas.Font.Style := [];
    ACanvas.Brush.Style := bsClear;

    // Draw left-aligned text with padding
    TextRect := Rect(
      ARect.Left + FCachedLayout.ItemPadding + TEXT_LEFT_PADDING,
      ButtonTop,
      ARect.Right - FCachedLayout.ItemPadding,
      ButtonTop + BUTTON_HEIGHT
    );
    DrawText(ACanvas.Handle, PChar(ButtonText), Length(ButtonText),
      TextRect, DT_LEFT or DT_VCENTER or DT_SINGLELINE);

    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TDeviceListBox.DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
  ADeviceType: TBluetoothDeviceType; AIsDiscovered: Boolean);
var
  IconChar: Char;
  TextSize: TSize;
  X, Y: Integer;
  Style: TCustomStyleServices;
  IconHandle: HICON;
  IconSize: Integer;
begin
  Style := TStyleManager.ActiveStyle;

  // Win7: Use system icons extracted from DLLs
  if Assigned(FSystemIconCache) then
  begin
    IconHandle := FSystemIconCache.GetIcon(ADeviceType);
    if IconHandle <> 0 then
    begin
      // Calculate centered position for icon
      IconSize := ARect.Width;  // Assume square rect
      X := ARect.Left;
      Y := ARect.Top;

      // Draw system icon
      // Note: Icons are colored by the system, we can't apply custom colors
      DrawIconEx(ACanvas.Handle, X, Y, IconHandle, IconSize, IconSize, 0, 0, DI_NORMAL);
      Exit;
    end;
    // If icon extraction failed, fall through to font-based rendering
  end;

  // Win10+: Use Segoe MDL2 Assets font icons (current implementation)
  IconChar := GetDeviceIconChar(ADeviceType);

  // Use icon font for device icons
  ACanvas.Font.Name := FONT_ICONS;
  ACanvas.Font.Size := LayoutConfig.IconFontSize;
  ACanvas.Font.Style := [];

  // Apply Main/Secondary color based on paired/unpaired state
  if AIsDiscovered then
  begin
    // Use Secondary color for unpaired/discovered devices
    case FCachedLayout.SecondaryColorSource of
      scsThemeText:     ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      scsThemeGrayText: ACanvas.Font.Color := Style.GetSystemColor(clGrayText);
      scsCustom:        ACanvas.Font.Color := TColor(FCachedLayout.SecondaryCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clGrayText);  // Fallback
    end;
  end
  else
  begin
    // Use Main color for paired devices
    case FCachedLayout.MainColorSource of
      mcsThemeText: ACanvas.Font.Color := Style.GetSystemColor(clWindowText);
      mcsCustom:    ACanvas.Font.Color := TColor(FCachedLayout.MainCustomColor);
    else
      ACanvas.Font.Color := Style.GetSystemColor(clWindowText);  // Fallback
    end;
  end;

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
var
  IconIndex: Integer;
begin
  // Icons: $E850-$E859 for 0%-90% (10% steps), $E83F for 100%
  if ALevel >= 100 then
    Result := ICON_BATTERY_100
  else if ALevel <= 0 then
    Result := ICON_BATTERY_0
  else
  begin
    // Calculate icon index: 0-9 for levels 0-99%
    // Level 1-9 -> 0, Level 10-19 -> 1, ..., Level 90-99 -> 9
    IconIndex := ALevel div 10;
    if IconIndex > 9 then
      IconIndex := 9;
    Result := Char(Ord(ICON_BATTERY_0) + IconIndex);
  end;
end;

procedure TDeviceListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  ThumbRect, TrackRect: TRect;
  ThumbCenter: Integer;
  PageSize: Integer;
begin
  inherited;

  if CanFocus then
    SetFocus;

  if Button = mbLeft then
  begin
    // Check if clicking on scrollbar
    if IsPointInScrollbar(X, Y) and (FMaxScroll > 0) then
    begin
      ThumbRect := GetScrollbarThumbRect;

      if IsPointInScrollbarThumb(X, Y) then
      begin
        // Start dragging thumb
        FScrollbarDragging := True;
        FScrollbarDragStartY := Y;
        FScrollbarDragStartScroll := FScrollPos;
        FScrollbarHover := True;
        Invalidate;
      end
      else
      begin
        // Click on track - page scroll
        TrackRect := GetScrollbarTrackRect;
        ThumbCenter := (ThumbRect.Top + ThumbRect.Bottom) div 2;

        if Y < ThumbCenter then
        begin
          // Page up
          PageSize := ClientHeight;
          ScrollTo(FScrollPos - PageSize);
        end
        else
        begin
          // Page down
          PageSize := ClientHeight;
          ScrollTo(FScrollPos + PageSize);
        end;
      end;
    end
    else
    begin
      // Regular item click
      Index := ItemAtPos(X, Y);
      if Index >= 0 then
        SelectedIndex := Index;
    end;
  end;
end;

procedure TDeviceListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  WasHover: Boolean;
  TrackRect, ThumbRect: TRect;
  TrackHeight, ThumbHeight: Integer;
  DeltaY, NewScrollPos: Integer;
  ScrollRatio: Double;
begin
  inherited;

  // Track list hover state for scrollbar visibility
  if not FListHovered then
  begin
    FListHovered := True;
    Invalidate;  // Redraw to show scrollbar
  end;

  // Handle scrollbar dragging
  if FScrollbarDragging then
  begin
    TrackRect := GetScrollbarTrackRect;
    ThumbRect := GetScrollbarThumbRect;
    TrackHeight := TrackRect.Height;
    ThumbHeight := ThumbRect.Height;

    DeltaY := Y - FScrollbarDragStartY;

    // Convert pixel delta to scroll position delta
    // ScrollRatio = DeltaY / (TrackHeight - ThumbHeight)
    if TrackHeight > ThumbHeight then
    begin
      ScrollRatio := DeltaY / (TrackHeight - ThumbHeight);
      NewScrollPos := FScrollbarDragStartScroll + Round(ScrollRatio * FMaxScroll);
      ScrollTo(NewScrollPos);
    end;

    Invalidate;
  end
  else
  begin
    // Update scrollbar hover state
    WasHover := FScrollbarHover;
    FScrollbarHover := IsPointInScrollbarThumb(X, Y);

    if WasHover <> FScrollbarHover then
      Invalidate;

    // Update item hover state (only if not over scrollbar)
    if not IsPointInScrollbar(X, Y) then
    begin
      Index := ItemAtPos(X, Y);
      if Index <> FHoverIndex then
      begin
        FHoverIndex := Index;
        Invalidate;
      end;
    end
    else if FHoverIndex <> -1 then
    begin
      FHoverIndex := -1;
      Invalidate;
    end;
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
    // End scrollbar dragging
    if FScrollbarDragging then
    begin
      FScrollbarDragging := False;
      Invalidate;
      Exit;
    end;

    Index := ItemAtPos(X, Y);
    if (Index >= 0) and (Index = FSelectedIndex) then
    begin
      // Check if action item (scan button, etc.)
      if FDisplayItems[Index].Source = dsAction then
      begin
        // Don't trigger if action is in progress (e.g., scanning)
        if not FDisplayItems[Index].IsActionInProgress then
        begin
          if Assigned(FOnActionClick) then
            FOnActionClick(Self, FDisplayItems[Index]);
        end;
        Exit;  // Don't process as device click
      end;

      // Regular device click
      if Assigned(FOnDisplayItemClick) then
        FOnDisplayItemClick(Self, FDisplayItems[Index])
      else if Assigned(FOnDeviceClick) then
        FOnDeviceClick(Self, FDisplayItems[Index].Device);
    end;
  end
  else if Button = mbRight then
  begin
    // Right-click: show context menu
    Index := ItemAtPos(X, Y);
    if (Index >= 0) and (FDisplayItems[Index].Source <> dsAction) then
    begin
      // Select item if not already selected
      if Index <> FSelectedIndex then
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
          // Check if action item (scan button, etc.)
          if FDisplayItems[FSelectedIndex].Source = dsAction then
          begin
            // Don't trigger if action is in progress (e.g., scanning)
            if not FDisplayItems[FSelectedIndex].IsActionInProgress then
            begin
              if Assigned(FOnActionClick) then
                FOnActionClick(Self, FDisplayItems[FSelectedIndex]);
            end;
          end
          else
          begin
            // Regular device activation
            if Assigned(FOnDisplayItemClick) then
              FOnDisplayItemClick(Self, FDisplayItems[FSelectedIndex])
            else if Assigned(FOnDeviceClick) then
              FOnDeviceClick(Self, FDisplayItems[FSelectedIndex].Device);
          end;
        end;
        Key := 0;
      end;

    VK_F10:
      begin
        // Shift+F10: Show context menu for selected device
        if (ssShift in Shift) and (FSelectedIndex >= 0) and
           (FDisplayItems[FSelectedIndex].Source <> dsAction) then
        begin
          // Get item rect and show menu at top-left of item
          var ItemRect := GetItemRect(FSelectedIndex);
          ShowContextMenuForItem(FSelectedIndex, ItemRect.Left, ItemRect.Top);
          Key := 0;
        end;
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

procedure TDeviceListBox.HandleAnimationTimer(Sender: TObject);
var
  I: Integer;
  HasActionInProgress: Boolean;
  R: TRect;
  WasZero: Boolean;
begin
  // Check if any action item is in progress
  HasActionInProgress := False;
  for I := 0 to High(FDisplayItems) do
  begin
    if (FDisplayItems[I].Source = dsAction) and FDisplayItems[I].IsActionInProgress then
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

{ Context Menu }

procedure TDeviceListBox.BuildContextMenu(AItemIndex: Integer);
var
  Item: TDeviceDisplayItem;
  MenuItem: TMenuItem;
begin
  FPopupMenu.Items.Clear;

  if (AItemIndex < 0) or (AItemIndex >= Length(FDisplayItems)) then
    Exit;

  Item := FDisplayItems[AItemIndex];

  // Store device address for menu handlers
  FContextMenuDeviceAddress := Item.Device.AddressInt;

  // For paired devices
  if Item.Device.IsPaired then
  begin
    // Pin/Unpin toggle
    MenuItem := TMenuItem.Create(FPopupMenu);
    if Item.IsPinned then
      MenuItem.Caption := 'Unpin'
    else
      MenuItem.Caption := 'Pin';
    MenuItem.OnClick := HandleMenuPinToggle;
    FPopupMenu.Items.Add(MenuItem);

    // Hide
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Hide from List';
    MenuItem.OnClick := HandleMenuHide;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Copy Name
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Name';
    MenuItem.OnClick := HandleMenuCopyName;
    FPopupMenu.Items.Add(MenuItem);

    // Copy Address
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Address';
    MenuItem.OnClick := HandleMenuCopyAddress;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Configure
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Configure...';
    MenuItem.OnClick := HandleMenuConfigure;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Unpair Device
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Unpair Device';
    MenuItem.OnClick := HandleMenuUnpair;
    FPopupMenu.Items.Add(MenuItem);
  end
  else
  begin
    // For unpaired devices: Copy commands + Hide
    // Copy Name
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Name';
    MenuItem.OnClick := HandleMenuCopyName;
    FPopupMenu.Items.Add(MenuItem);

    // Copy Address
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Address';
    MenuItem.OnClick := HandleMenuCopyAddress;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Hide
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Hide from List';
    MenuItem.OnClick := HandleMenuHide;
    FPopupMenu.Items.Add(MenuItem);
  end;
end;

procedure TDeviceListBox.ShowContextMenuForItem(AItemIndex: Integer; X, Y: Integer);
var
  ScreenPt: TPoint;
begin
  BuildContextMenu(AItemIndex);

  // Convert control coordinates to screen coordinates
  ScreenPt := ClientToScreen(Point(X, Y));

  // Show menu at specified position
  FPopupMenu.Popup(ScreenPt.X, ScreenPt.Y);
end;

procedure TDeviceListBox.HandleMenuPinToggle(Sender: TObject);
begin
  if Assigned(FOnPinToggle) and (FContextMenuDeviceAddress <> 0) then
    FOnPinToggle(Self, FContextMenuDeviceAddress);
end;

procedure TDeviceListBox.HandleMenuCopyName(Sender: TObject);
begin
  if Assigned(FOnCopyName) and (FContextMenuDeviceAddress <> 0) then
    FOnCopyName(Self, FContextMenuDeviceAddress);
end;

procedure TDeviceListBox.HandleMenuCopyAddress(Sender: TObject);
begin
  if Assigned(FOnCopyAddress) and (FContextMenuDeviceAddress <> 0) then
    FOnCopyAddress(Self, FContextMenuDeviceAddress);
end;

procedure TDeviceListBox.HandleMenuConfigure(Sender: TObject);
begin
  if Assigned(FOnConfigure) and (FContextMenuDeviceAddress <> 0) then
    FOnConfigure(Self, FContextMenuDeviceAddress);
end;

procedure TDeviceListBox.HandleMenuUnpair(Sender: TObject);
begin
  if Assigned(FOnUnpair) and (FContextMenuDeviceAddress <> 0) then
    FOnUnpair(Self, FContextMenuDeviceAddress);
end;

procedure TDeviceListBox.HandleMenuHide(Sender: TObject);
begin
  if Assigned(FOnHide) and (FContextMenuDeviceAddress <> 0) then
    FOnHide(Self, FContextMenuDeviceAddress);
end;

end.
