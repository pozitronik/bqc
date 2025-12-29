{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Icon Renderer                           }
{                                                       }
{*******************************************************}

/// <summary>
/// Generates battery level icons for system tray using GDI.
/// Creates 16x16 battery-shaped icons with customizable fill color.
/// </summary>
unit UI.BatteryIconRenderer;

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  System.SysUtils,
  System.Types;

type
  /// <summary>
  /// Renders battery level icons using GDI.
  /// Thread-safe: each call creates a new icon.
  /// </summary>
  TBatteryIconRenderer = class
  private
    class procedure DrawBatteryOutline(ACanvas: TCanvas; ARect: TRect);
    class procedure DrawBatteryFill(ACanvas: TCanvas; ARect: TRect;
      ALevel: Integer; AColor: TColor);
    class procedure DrawNumericValue(ACanvas: TCanvas; ARect: TRect;
      ALevel: Integer; AColor: TColor);
    class function IsTransparent(AColor: TColor): Boolean;
  public
    /// <summary>
    /// Creates a battery level icon with graphical display.
    /// </summary>
    /// <param name="ALevel">Battery level percentage (0-100)</param>
    /// <param name="AColor">Fill color for the battery level</param>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateBatteryIcon(ALevel: Integer; AColor: TColor): TIcon; overload;

    /// <summary>
    /// Creates a battery level icon with background color support.
    /// </summary>
    /// <param name="ALevel">Battery level percentage (0-100)</param>
    /// <param name="AColor">Fill color for the battery level</param>
    /// <param name="ABackgroundColor">Background color (use clNone or value > $1000000 for transparent)</param>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateBatteryIcon(ALevel: Integer; AColor: TColor;
      ABackgroundColor: TColor): TIcon; overload;

    /// <summary>
    /// Creates a battery level icon with automatic color selection.
    /// Uses red for low battery (below threshold), provided color otherwise.
    /// </summary>
    /// <param name="ALevel">Battery level percentage (0-100)</param>
    /// <param name="AColor">Normal fill color</param>
    /// <param name="ALowThreshold">Level below which to use red color</param>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateBatteryIconAuto(ALevel: Integer; AColor: TColor;
      ALowThreshold: Integer = 20): TIcon; overload;

    /// <summary>
    /// Creates a battery level icon with full customization.
    /// </summary>
    /// <param name="ALevel">Battery level percentage (0-100)</param>
    /// <param name="AColor">Normal fill color</param>
    /// <param name="ABackgroundColor">Background color (clNone for transparent)</param>
    /// <param name="ALowThreshold">Level below which to use red color</param>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateBatteryIconAuto(ALevel: Integer; AColor: TColor;
      ABackgroundColor: TColor; ALowThreshold: Integer): TIcon; overload;

    /// <summary>
    /// Creates a numeric battery icon showing percentage as digits.
    /// </summary>
    /// <param name="ALevel">Battery level percentage (0-100)</param>
    /// <param name="AColor">Text color</param>
    /// <param name="ABackgroundColor">Background color (clNone for transparent)</param>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateNumericIcon(ALevel: Integer; AColor: TColor;
      ABackgroundColor: TColor): TIcon;

    /// <summary>
    /// Creates a "no battery" or unknown status icon.
    /// </summary>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreateUnknownBatteryIcon: TIcon;

    /// <summary>
    /// Creates a "pending refresh" icon with ellipsis.
    /// Used when device is connected but battery level is being queried.
    /// </summary>
    /// <returns>New TIcon instance. Caller must free.</returns>
    class function CreatePendingBatteryIcon: TIcon;
  end;

const
  // Icon dimensions
  BATTERY_ICON_SIZE = 16;

  // Battery shape parameters - uses full horizontal space
  BATTERY_LEFT = 0;
  BATTERY_TOP = 3;
  BATTERY_WIDTH = 14;
  BATTERY_HEIGHT = 10;
  BATTERY_CAP_WIDTH = 2;
  BATTERY_CAP_HEIGHT = 6;
  BATTERY_PADDING = 1;

  // Colors
  BATTERY_OUTLINE_COLOR = clBlack;
  BATTERY_LOW_COLOR = clRed;
  BATTERY_UNKNOWN_COLOR = clGray;

implementation

{ TBatteryIconRenderer }

class function TBatteryIconRenderer.CreateBatteryIcon(ALevel: Integer;
  AColor: TColor): TIcon;
begin
  // Delegate to overload with transparent background
  Result := CreateBatteryIcon(ALevel, AColor, clNone);
end;

class function TBatteryIconRenderer.CreateBatteryIconAuto(ALevel: Integer;
  AColor: TColor; ALowThreshold: Integer): TIcon;
var
  EffectiveColor: TColor;
begin
  // Use red color for low battery
  if ALevel <= ALowThreshold then
    EffectiveColor := BATTERY_LOW_COLOR
  else
    EffectiveColor := AColor;

  Result := CreateBatteryIcon(ALevel, EffectiveColor);
end;

class function TBatteryIconRenderer.CreateUnknownBatteryIcon: TIcon;
var
  Bitmap: TBitmap;
  MaskBmp: TBitmap;
  BatteryRect: TRect;
  IconInfo: TIconInfo;
begin
  Result := TIcon.Create;
  Bitmap := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  try
    // Setup color bitmap
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    Bitmap.Canvas.Brush.Color := clBlack;
    Bitmap.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    // Setup mask bitmap
    MaskBmp.PixelFormat := pf1bit;
    MaskBmp.Monochrome := True;
    MaskBmp.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    MaskBmp.Canvas.Brush.Color := clWhite;
    MaskBmp.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    BatteryRect := Rect(BATTERY_LEFT, BATTERY_TOP,
      BATTERY_LEFT + BATTERY_WIDTH, BATTERY_TOP + BATTERY_HEIGHT);

    // Draw battery outline
    DrawBatteryOutline(Bitmap.Canvas, BatteryRect);

    // Draw question mark in center
    Bitmap.Canvas.Font.Name := 'Segoe UI';
    Bitmap.Canvas.Font.Size := 7;
    Bitmap.Canvas.Font.Color := BATTERY_UNKNOWN_COLOR;
    Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.TextOut(
      BatteryRect.Left + (BATTERY_WIDTH - Bitmap.Canvas.TextWidth('?')) div 2,
      BatteryRect.Top + (BATTERY_HEIGHT - Bitmap.Canvas.TextHeight('?')) div 2,
      '?'
    );

    // Draw mask
    MaskBmp.Canvas.Brush.Color := clBlack;
    MaskBmp.Canvas.Pen.Color := clBlack;
    MaskBmp.Canvas.Rectangle(BatteryRect);
    MaskBmp.Canvas.Rectangle(
      BatteryRect.Right,
      BatteryRect.Top + (BATTERY_HEIGHT - BATTERY_CAP_HEIGHT) div 2,
      BatteryRect.Right + BATTERY_CAP_WIDTH,
      BatteryRect.Top + (BATTERY_HEIGHT + BATTERY_CAP_HEIGHT) div 2
    );

    // Create icon
    IconInfo.fIcon := True;
    IconInfo.xHotspot := 0;
    IconInfo.yHotspot := 0;
    IconInfo.hbmMask := MaskBmp.Handle;
    IconInfo.hbmColor := Bitmap.Handle;

    Result.Handle := CreateIconIndirect(IconInfo);
  finally
    MaskBmp.Free;
    Bitmap.Free;
  end;
end;

class function TBatteryIconRenderer.CreatePendingBatteryIcon: TIcon;
var
  Bitmap: TBitmap;
  MaskBmp: TBitmap;
  BatteryRect: TRect;
  IconInfo: TIconInfo;
  EllipsisText: string;
  TextWidth, TextHeight: Integer;
begin
  Result := TIcon.Create;
  Bitmap := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  try
    // Setup color bitmap
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    Bitmap.Canvas.Brush.Color := clBlack;
    Bitmap.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    // Setup mask bitmap
    MaskBmp.PixelFormat := pf1bit;
    MaskBmp.Monochrome := True;
    MaskBmp.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    MaskBmp.Canvas.Brush.Color := clWhite;
    MaskBmp.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    BatteryRect := Rect(BATTERY_LEFT, BATTERY_TOP,
      BATTERY_LEFT + BATTERY_WIDTH, BATTERY_TOP + BATTERY_HEIGHT);

    // Draw battery outline
    DrawBatteryOutline(Bitmap.Canvas, BatteryRect);

    // Draw ellipsis in center
    EllipsisText := '...';
    Bitmap.Canvas.Font.Name := 'Segoe UI';
    Bitmap.Canvas.Font.Size := 6;
    Bitmap.Canvas.Font.Color := BATTERY_UNKNOWN_COLOR;
    Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.Brush.Style := bsClear;

    TextWidth := Bitmap.Canvas.TextWidth(EllipsisText);
    TextHeight := Bitmap.Canvas.TextHeight(EllipsisText);

    Bitmap.Canvas.TextOut(
      BatteryRect.Left + (BATTERY_WIDTH - TextWidth) div 2,
      BatteryRect.Top + (BATTERY_HEIGHT - TextHeight) div 2 - 1,  // Slight vertical adjustment
      EllipsisText
    );

    // Draw mask
    MaskBmp.Canvas.Brush.Color := clBlack;
    MaskBmp.Canvas.Pen.Color := clBlack;
    MaskBmp.Canvas.Rectangle(BatteryRect);
    MaskBmp.Canvas.Rectangle(
      BatteryRect.Right,
      BatteryRect.Top + (BATTERY_HEIGHT - BATTERY_CAP_HEIGHT) div 2,
      BatteryRect.Right + BATTERY_CAP_WIDTH,
      BatteryRect.Top + (BATTERY_HEIGHT + BATTERY_CAP_HEIGHT) div 2
    );

    // Create icon
    IconInfo.fIcon := True;
    IconInfo.xHotspot := 0;
    IconInfo.yHotspot := 0;
    IconInfo.hbmMask := MaskBmp.Handle;
    IconInfo.hbmColor := Bitmap.Handle;

    Result.Handle := CreateIconIndirect(IconInfo);
  finally
    MaskBmp.Free;
    Bitmap.Free;
  end;
end;

class procedure TBatteryIconRenderer.DrawBatteryOutline(ACanvas: TCanvas;
  ARect: TRect);
begin
  // Draw main battery body outline
  ACanvas.Pen.Color := BATTERY_OUTLINE_COLOR;
  ACanvas.Pen.Width := 1;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(ARect);

  // Draw battery cap (positive terminal)
  ACanvas.Brush.Color := BATTERY_OUTLINE_COLOR;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(
    ARect.Right,
    ARect.Top + (BATTERY_HEIGHT - BATTERY_CAP_HEIGHT) div 2,
    ARect.Right + BATTERY_CAP_WIDTH,
    ARect.Top + (BATTERY_HEIGHT + BATTERY_CAP_HEIGHT) div 2
  );
end;

class procedure TBatteryIconRenderer.DrawBatteryFill(ACanvas: TCanvas;
  ARect: TRect; ALevel: Integer; AColor: TColor);
var
  FillRect: TRect;
  FillWidth: Integer;
  MaxFillWidth: Integer;
begin
  // Calculate fill area (inside the battery outline with padding)
  FillRect := Rect(
    ARect.Left + BATTERY_PADDING,
    ARect.Top + BATTERY_PADDING,
    ARect.Right - BATTERY_PADDING,
    ARect.Bottom - BATTERY_PADDING
  );

  // Calculate fill width based on battery level
  MaxFillWidth := FillRect.Width;
  FillWidth := MulDiv(MaxFillWidth, ALevel, 100);

  if FillWidth > 0 then
  begin
    FillRect.Right := FillRect.Left + FillWidth;
    ACanvas.Brush.Color := AColor;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Pen.Style := psClear;
    ACanvas.FillRect(FillRect);
    ACanvas.Pen.Style := psSolid;
  end;
end;

class function TBatteryIconRenderer.IsTransparent(AColor: TColor): Boolean;
begin
  // Transparent if clNone or special transparent value ($1FFFFFFF)
  Result := (AColor = clNone) or (AColor > $1000000);
end;

class function TBatteryIconRenderer.CreateBatteryIcon(ALevel: Integer;
  AColor: TColor; ABackgroundColor: TColor): TIcon;
var
  Bitmap: TBitmap;
  MaskBmp: TBitmap;
  BatteryRect: TRect;
  IconInfo: TIconInfo;
  UseTransparent: Boolean;
begin
  Result := TIcon.Create;
  Bitmap := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  try
    UseTransparent := IsTransparent(ABackgroundColor);

    // Setup color bitmap
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    if UseTransparent then
      Bitmap.Canvas.Brush.Color := clBlack
    else
      Bitmap.Canvas.Brush.Color := ABackgroundColor;
    Bitmap.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    // Setup mask bitmap
    MaskBmp.PixelFormat := pf1bit;
    MaskBmp.Monochrome := True;
    MaskBmp.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    if UseTransparent then
      MaskBmp.Canvas.Brush.Color := clWhite  // All transparent by default
    else
      MaskBmp.Canvas.Brush.Color := clBlack; // All opaque
    MaskBmp.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    BatteryRect := Rect(BATTERY_LEFT, BATTERY_TOP,
      BATTERY_LEFT + BATTERY_WIDTH, BATTERY_TOP + BATTERY_HEIGHT);

    // Draw battery components on color bitmap
    DrawBatteryOutline(Bitmap.Canvas, BatteryRect);
    DrawBatteryFill(Bitmap.Canvas, BatteryRect, ALevel, AColor);

    // Draw mask (battery shape = black = opaque)
    if UseTransparent then
    begin
      MaskBmp.Canvas.Brush.Color := clBlack;
      MaskBmp.Canvas.Pen.Color := clBlack;
      // Main battery body
      MaskBmp.Canvas.Rectangle(BatteryRect);
      // Battery cap
      MaskBmp.Canvas.Rectangle(
        BatteryRect.Right,
        BatteryRect.Top + (BATTERY_HEIGHT - BATTERY_CAP_HEIGHT) div 2,
        BatteryRect.Right + BATTERY_CAP_WIDTH,
        BatteryRect.Top + (BATTERY_HEIGHT + BATTERY_CAP_HEIGHT) div 2
      );
    end;

    // Create icon from bitmaps
    IconInfo.fIcon := True;
    IconInfo.xHotspot := 0;
    IconInfo.yHotspot := 0;
    IconInfo.hbmMask := MaskBmp.Handle;
    IconInfo.hbmColor := Bitmap.Handle;

    Result.Handle := CreateIconIndirect(IconInfo);
  finally
    MaskBmp.Free;
    Bitmap.Free;
  end;
end;

class function TBatteryIconRenderer.CreateBatteryIconAuto(ALevel: Integer;
  AColor: TColor; ABackgroundColor: TColor; ALowThreshold: Integer): TIcon;
var
  EffectiveColor: TColor;
begin
  // Use red color for low battery
  if ALevel <= ALowThreshold then
    EffectiveColor := BATTERY_LOW_COLOR
  else
    EffectiveColor := AColor;

  Result := CreateBatteryIcon(ALevel, EffectiveColor, ABackgroundColor);
end;

class procedure TBatteryIconRenderer.DrawNumericValue(ACanvas: TCanvas;
  ARect: TRect; ALevel: Integer; AColor: TColor);
var
  Text: string;
  TextWidth, TextHeight: Integer;
  X, Y: Integer;
begin
  // Format: "100", "99", "0", etc.
  if ALevel >= 100 then
    Text := '99+'
  else
    Text := IntToStr(ALevel);

  ACanvas.Font.Name := 'Segoe UI';
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := AColor;
  ACanvas.Brush.Style := bsClear;

  // Adjust font size based on digit count
  if Length(Text) >= 3 then
    ACanvas.Font.Size := 5
  else
    ACanvas.Font.Size := 7;

  TextWidth := ACanvas.TextWidth(Text);
  TextHeight := ACanvas.TextHeight(Text);

  // Center the text in the icon
  X := (BATTERY_ICON_SIZE - TextWidth) div 2;
  Y := (BATTERY_ICON_SIZE - TextHeight) div 2;

  ACanvas.TextOut(X, Y, Text);
end;

class function TBatteryIconRenderer.CreateNumericIcon(ALevel: Integer;
  AColor: TColor; ABackgroundColor: TColor): TIcon;
var
  Bitmap: TBitmap;
  MaskBmp: TBitmap;
  IconInfo: TIconInfo;
  UseTransparent: Boolean;
  Text: string;
  TextRect: TRect;
begin
  Result := TIcon.Create;
  Bitmap := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  try
    UseTransparent := IsTransparent(ABackgroundColor);

    // Setup color bitmap
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    if UseTransparent then
      Bitmap.Canvas.Brush.Color := clBlack
    else
      Bitmap.Canvas.Brush.Color := ABackgroundColor;
    Bitmap.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    // Setup mask bitmap
    MaskBmp.PixelFormat := pf1bit;
    MaskBmp.Monochrome := True;
    MaskBmp.SetSize(BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);
    if UseTransparent then
      MaskBmp.Canvas.Brush.Color := clWhite  // All transparent by default
    else
      MaskBmp.Canvas.Brush.Color := clBlack; // All opaque
    MaskBmp.Canvas.FillRect(Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE));

    TextRect := Rect(0, 0, BATTERY_ICON_SIZE, BATTERY_ICON_SIZE);

    // Draw numeric value on color bitmap
    DrawNumericValue(Bitmap.Canvas, TextRect, ALevel, AColor);

    // For transparent background, we need to make text area opaque in mask
    if UseTransparent then
    begin
      // Format text same way as DrawNumericValue
      if ALevel >= 100 then
        Text := '99+'
      else
        Text := IntToStr(ALevel);

      MaskBmp.Canvas.Font.Name := 'Segoe UI';
      MaskBmp.Canvas.Font.Style := [fsBold];
      if Length(Text) >= 3 then
        MaskBmp.Canvas.Font.Size := 5
      else
        MaskBmp.Canvas.Font.Size := 7;

      MaskBmp.Canvas.Brush.Color := clBlack;  // Opaque
      MaskBmp.Canvas.Font.Color := clBlack;   // Opaque text
      MaskBmp.Canvas.TextOut(
        (BATTERY_ICON_SIZE - MaskBmp.Canvas.TextWidth(Text)) div 2,
        (BATTERY_ICON_SIZE - MaskBmp.Canvas.TextHeight(Text)) div 2,
        Text
      );
    end;

    // Create icon from bitmaps
    IconInfo.fIcon := True;
    IconInfo.xHotspot := 0;
    IconInfo.yHotspot := 0;
    IconInfo.hbmMask := MaskBmp.Handle;
    IconInfo.hbmColor := Bitmap.Handle;

    Result.Handle := CreateIconIndirect(IconInfo);
  finally
    MaskBmp.Free;
    Bitmap.Free;
  end;
end;

end.
