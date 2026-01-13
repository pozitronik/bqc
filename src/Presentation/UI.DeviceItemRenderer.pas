{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Item Renderer                            }
{                                                       }
{       EXTRACTED FROM: UI.DeviceList (god class)       }
{       Handles all item rendering logic - backgrounds, }
{       icons, text, profiles, separators, etc.         }
{                                                       }
{*******************************************************}

unit UI.DeviceItemRenderer;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Themes,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  App.ConfigEnums,
  App.ProfileConfigIntf,
  App.DeviceFormatter,
  UI.DeviceListTypes;

type
  /// <summary>
  /// Device item renderer extracted from TDeviceListBox god class.
  /// Renders device items, action buttons, separators, profiles, icons, etc.
  /// </summary>
  TDeviceItemRenderer = class
  private
    FCachedLayout: TCachedLayoutParams;
    FProfileConfig: IProfileConfig;
    FShowAddresses: Boolean;
    FSystemIconCache: TSystemIconCache;  // For Win7 (nil on Win10+)
    FAnimationFrame: Integer;  // For action button progress animation

    function GetBatteryIconChar(ALevel: Integer): Char;
    function GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
  public
    constructor Create(ASystemIconCache: TSystemIconCache);

    /// <summary>
    /// Updates cached layout parameters and configurations.
    /// Call when config changes or before rendering cycle.
    /// </summary>
    procedure UpdateCache(const ACachedLayout: TCachedLayoutParams;
      AProfileConfig: IProfileConfig; AShowAddresses: Boolean; AAnimationFrame: Integer);

    /// <summary>
    /// Creates drawing context for an item.
    /// </summary>
    function CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
      AIsHover, AIsSelected, AIsDiscovered: Boolean): TItemDrawContext;

    /// <summary>
    /// Renders a complete display item (device or action).
    /// </summary>
    procedure DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
      const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);

    /// <summary>
    /// Renders action button (scan button, etc.).
    /// </summary>
    procedure DrawActionButton(ACanvas: TCanvas; const ARect: TRect;
      const AItem: TDeviceDisplayItem; AIsHover, AIsSelected: Boolean);

    /// <summary>
    /// Renders separator line between sections.
    /// </summary>
    procedure DrawSeparator(ACanvas: TCanvas; const ARect: TRect);

    /// <summary>
    /// Renders item background with rounded corners.
    /// </summary>
    procedure DrawItemBackground(ACanvas: TCanvas; const AContext: TItemDrawContext);

    /// <summary>
    /// Renders top line (device name, battery, pin icon, address).
    /// </summary>
    procedure DrawItemTopLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);

    /// <summary>
    /// Renders bottom line (connection status, profiles, last seen).
    /// </summary>
    procedure DrawItemBottomLine(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);

    /// <summary>
    /// Renders profile section (if profiles are shown).
    /// </summary>
    procedure DrawProfileSection(ACanvas: TCanvas; const AItem: TDeviceDisplayItem;
      const AContext: TItemDrawContext);

    /// <summary>
    /// Renders device icon (Segoe MDL2 on Win10+, system icon on Win7).
    /// </summary>
    procedure DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
      ADeviceType: TBluetoothDeviceType; AIsDiscovered: Boolean);
  end;

const
  // Segoe MDL2 Assets icons (Unicode code points) - exposed for testing
  ICON_BLUETOOTH = #$E702;
  ICON_HEADPHONE = #$E7F6;
  ICON_MICROPHONE = #$E720;
  ICON_COMPUTER = #$E7F8;
  ICON_PHONE = #$E8EA;
  ICON_KEYBOARD = #$E765;
  ICON_MOUSE = #$E962;
  ICON_GAMEPAD = #$E7FC;
  ICON_GENERIC = #$E702;
  ICON_PIN = #$E840;

implementation

uses
  Vcl.Controls,
  Winapi.ShellAPI,
  Winapi.CommCtrl,
  App.WinRTSupport;

const
  // Additional Segoe MDL2 Assets icons (main icons exposed in interface section for testing)
  ICON_AUDIO_HEADSET = #$E95B;
  ICON_AUDIO_HEADPHONES = #$E967;
  ICON_INPUT_DEVICE = #$E961;
  ICON_BATTERY_0 = #$E850;
  ICON_BATTERY_1 = #$E851;
  ICON_BATTERY_2 = #$E852;
  ICON_BATTERY_3 = #$E853;
  ICON_BATTERY_4 = #$E854;
  ICON_BATTERY_5 = #$E855;
  ICON_BATTERY_6 = #$E856;
  ICON_BATTERY_7 = #$E857;
  ICON_BATTERY_8 = #$E858;
  ICON_BATTERY_9 = #$E859;
  ICON_BATTERY_100 = #$E83F;
  ICON_BATTERY_UNKNOWN = #$E996;
  ICON_AWAITING_BATTERY = #$EBE8;  // Empty circle outline

  // Fonts
  FONT_UI = 'Segoe UI';

  // Default values
  DEFAULT_PROFILE_FONT_SIZE = 7;
  PROFILE_LINE_HEIGHT_FACTOR = 2;

  // Icon and spacing constants
  FONT_ICONS = 'Segoe MDL2 Assets';
  PIN_ICON_FONT_SIZE = 10;     // Font size for pin icon
  ADDRESS_SPACING = 8;         // Space between device name and address
  BATTERY_SPACING = 4;         // Space between battery text and icon
  BATTERY_FONT_SIZE = 9;       // Font size for battery percentage text
  BATTERY_ICON_FONT_SIZE = 12; // Font size for battery icon

{ TDeviceItemRenderer }

constructor TDeviceItemRenderer.Create(ASystemIconCache: TSystemIconCache);
begin
  inherited Create;
  FSystemIconCache := ASystemIconCache;
  FAnimationFrame := 0;
end;

procedure TDeviceItemRenderer.UpdateCache(const ACachedLayout: TCachedLayoutParams;
  AProfileConfig: IProfileConfig; AShowAddresses: Boolean; AAnimationFrame: Integer);
begin
  FCachedLayout := ACachedLayout;
  FProfileConfig := AProfileConfig;
  FShowAddresses := AShowAddresses;
  FAnimationFrame := AAnimationFrame;
end;

function TDeviceItemRenderer.GetBatteryIconChar(ALevel: Integer): Char;
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

function TDeviceItemRenderer.CreateDrawContext(ACanvas: TCanvas; const ARect: TRect;
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

procedure TDeviceItemRenderer.DrawItemBackground(ACanvas: TCanvas;
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

procedure TDeviceItemRenderer.DrawItemTopLine(ACanvas: TCanvas;
  const AItem: TDeviceDisplayItem; const AContext: TItemDrawContext);
var
  Style: TCustomStyleServices;
  AddrLeft, NameHeight, AddrOffset: Integer;
  RightEdge: Integer;
  PinIconWidth: Integer;
  PinIconHandle: HICON;
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
    // Win7: Use system icon extracted from imageres.dll
    if Assigned(FSystemIconCache) then
    begin
      PinIconHandle := FSystemIconCache.GetPinIcon;
      if PinIconHandle <> 0 then
      begin
        // Use PIN_ICON_FONT_SIZE as icon size to match font icon size
        PinIconWidth := PIN_ICON_FONT_SIZE;
        RightEdge := RightEdge - PinIconWidth;
        // Draw system icon (system-colored, no custom color support)
        DrawIconEx(ACanvas.Handle, RightEdge, AContext.NameLineTop, PinIconHandle,
          PinIconWidth, PinIconWidth, 0, 0, DI_NORMAL);
      end;
      // If extraction failed, just skip the icon (pin status still obvious from UI)
    end
    else
    begin
      // Win10+: Use Segoe MDL2 Assets font icon
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

procedure TDeviceItemRenderer.DrawItemBottomLine(ACanvas: TCanvas;
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
    // Win10+: Draw battery icon (rightmost, font-based)
    // Win7: Skip icon, percentage text is sufficient
    if not Assigned(FSystemIconCache) then
    begin
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
    end
    else
      BatteryIconWidth := 0;  // No icon on Win7

    // Draw battery percentage text (Win10+: left of icon, Win7: right-aligned)
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
    if BatteryIconWidth > 0 then
      // Win10+: Position text left of icon
      ACanvas.TextOut(AContext.TextRect.Right - BatteryIconWidth - BATTERY_SPACING - BatteryTextWidth,
        AContext.StatusLineTop + BatteryOffset, AItem.BatteryText)
    else
      // Win7: Position text at right edge (no icon)
      ACanvas.TextOut(AContext.TextRect.Right - BatteryTextWidth,
        AContext.StatusLineTop + BatteryOffset, AItem.BatteryText);
  end;
end;

procedure TDeviceItemRenderer.DrawProfileSection(ACanvas: TCanvas;
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

  // Calculate line height (same logic as in TListDataSource)
  LineHeight := ProfileFontSize * PROFILE_LINE_HEIGHT_FACTOR;

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

procedure TDeviceItemRenderer.DrawSeparator(ACanvas: TCanvas; const ARect: TRect);
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

procedure TDeviceItemRenderer.DrawDisplayItem(ACanvas: TCanvas; const ARect: TRect;
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

procedure TDeviceItemRenderer.DrawActionButton(ACanvas: TCanvas; const ARect: TRect;
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

procedure TDeviceItemRenderer.DrawDeviceIcon(ACanvas: TCanvas; const ARect: TRect;
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
  ACanvas.Font.Size := FCachedLayout.IconFontSize;
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

function TDeviceItemRenderer.GetDeviceIconChar(ADeviceType: TBluetoothDeviceType): Char;
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

end.
