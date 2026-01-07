{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Custom Scrollbar Control                        }
{                                                       }
{       EXTRACTED FROM: UI.DeviceList (god class)       }
{       Reusable custom scrollbar with modern styling.  }
{       Auto-hiding on mouse leave, smooth dragging.    }
{                                                       }
{*******************************************************}

unit UI.CustomScrollbar;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Themes;

type
  /// <summary>
  /// Custom scrollbar control extracted from TDeviceListBox.
  /// Provides modern, auto-hiding scrollbar with smooth dragging.
  /// </summary>
  TCustomScrollbar = class
  private
    FScrollPos: Integer;
    FMaxScroll: Integer;
    FScrollbarHover: Boolean;
    FScrollbarDragging: Boolean;
    FScrollbarDragStartY: Integer;
    FScrollbarDragStartScroll: Integer;
    FListHovered: Boolean;  // True when mouse is over parent control
    FClientWidth: Integer;
    FClientHeight: Integer;
    FOnScrollChanged: TNotifyEvent;

    function GetScrollbarRect: TRect;
    function GetScrollbarTrackRect: TRect;
    function GetScrollbarThumbRect: TRect;
    function IsPointInScrollbar(X, Y: Integer): Boolean;
    function IsPointInScrollbarThumb(X, Y: Integer): Boolean;
    procedure SetScrollPos(AValue: Integer);
    procedure SetMaxScroll(AValue: Integer);
  public
    constructor Create;

    /// <summary>
    /// Updates client dimensions for scrollbar geometry calculations.
    /// Call when parent control resizes.
    /// </summary>
    procedure UpdateClientSize(AWidth, AHeight: Integer);

    /// <summary>
    /// Updates scroll range. Call when content height changes.
    /// </summary>
    procedure UpdateScrollRange(AMaxScroll: Integer);

    /// <summary>
    /// Scrolls to specified position (clamped to valid range).
    /// </summary>
    procedure ScrollTo(APos: Integer);

    /// <summary>
    /// Handles mouse down event. Returns True if scrollbar handled it.
    /// </summary>
    function HandleMouseDown(X, Y: Integer): Boolean;

    /// <summary>
    /// Handles mouse move event.
    /// </summary>
    procedure HandleMouseMove(X, Y: Integer);

    /// <summary>
    /// Handles mouse up event.
    /// </summary>
    procedure HandleMouseUp(X, Y: Integer);

    /// <summary>
    /// Handles mouse wheel event. Returns True if scrollbar handled it.
    /// </summary>
    function HandleMouseWheel(AWheelDelta: Integer): Boolean;

    /// <summary>
    /// Handles mouse enter (for scrollbar auto-show).
    /// </summary>
    procedure HandleMouseEnter;

    /// <summary>
    /// Handles mouse leave (for scrollbar auto-hide).
    /// </summary>
    function HandleMouseLeave: Boolean;  // Returns true if state changed

    /// <summary>
    /// Renders the scrollbar to specified canvas.
    /// </summary>
    procedure Render(ACanvas: TCanvas);

    property ScrollPos: Integer read FScrollPos write SetScrollPos;
    property MaxScroll: Integer read FMaxScroll write SetMaxScroll;
    property OnScrollChanged: TNotifyEvent read FOnScrollChanged write FOnScrollChanged;
  end;

implementation

const
  SCROLLBAR_WIDTH = 12;  // Modern thin scrollbar width (px)
  MIN_THUMB_HEIGHT = 20; // Minimum thumb height for usability

{ TCustomScrollbar }

constructor TCustomScrollbar.Create;
begin
  inherited Create;
  FScrollPos := 0;
  FMaxScroll := 0;
  FScrollbarHover := False;
  FScrollbarDragging := False;
  FScrollbarDragStartY := 0;
  FScrollbarDragStartScroll := 0;
  FListHovered := False;
  FClientWidth := 0;
  FClientHeight := 0;
end;

procedure TCustomScrollbar.UpdateClientSize(AWidth, AHeight: Integer);
begin
  FClientWidth := AWidth;
  FClientHeight := AHeight;
end;

procedure TCustomScrollbar.UpdateScrollRange(AMaxScroll: Integer);
begin
  FMaxScroll := Max(0, AMaxScroll);
  FScrollPos := EnsureRange(FScrollPos, 0, FMaxScroll);
end;

procedure TCustomScrollbar.ScrollTo(APos: Integer);
begin
  APos := EnsureRange(APos, 0, FMaxScroll);
  if FScrollPos <> APos then
  begin
    FScrollPos := APos;
    if Assigned(FOnScrollChanged) then
      FOnScrollChanged(Self);
  end;
end;

procedure TCustomScrollbar.SetScrollPos(AValue: Integer);
begin
  ScrollTo(AValue);
end;

procedure TCustomScrollbar.SetMaxScroll(AValue: Integer);
begin
  UpdateScrollRange(AValue);
end;

function TCustomScrollbar.GetScrollbarRect: TRect;
begin
  Result := Rect(
    FClientWidth - SCROLLBAR_WIDTH,
    0,
    FClientWidth,
    FClientHeight
  );
end;

function TCustomScrollbar.GetScrollbarTrackRect: TRect;
begin
  Result := GetScrollbarRect;
end;

function TCustomScrollbar.GetScrollbarThumbRect: TRect;
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
  ThumbHeight := Round(TrackHeight * (FClientHeight / (FClientHeight + FMaxScroll)));

  // Minimum thumb height for usability
  if ThumbHeight < MIN_THUMB_HEIGHT then
    ThumbHeight := MIN_THUMB_HEIGHT;

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

function TCustomScrollbar.IsPointInScrollbar(X, Y: Integer): Boolean;
var
  ScrollbarRect: TRect;
begin
  ScrollbarRect := GetScrollbarRect;
  Result := PtInRect(ScrollbarRect, Point(X, Y));
end;

function TCustomScrollbar.IsPointInScrollbarThumb(X, Y: Integer): Boolean;
var
  ThumbRect: TRect;
begin
  ThumbRect := GetScrollbarThumbRect;
  Result := PtInRect(ThumbRect, Point(X, Y));
end;

function TCustomScrollbar.HandleMouseDown(X, Y: Integer): Boolean;
var
  ThumbRect, TrackRect: TRect;
  ThumbCenter: Integer;
  PageSize: Integer;
begin
  Result := False;

  // Check if clicking on scrollbar
  if IsPointInScrollbar(X, Y) and (FMaxScroll > 0) then
  begin
    Result := True;
    ThumbRect := GetScrollbarThumbRect;

    if IsPointInScrollbarThumb(X, Y) then
    begin
      // Start dragging thumb
      FScrollbarDragging := True;
      FScrollbarDragStartY := Y;
      FScrollbarDragStartScroll := FScrollPos;
      FScrollbarHover := True;
    end
    else
    begin
      // Click on track - page scroll
      TrackRect := GetScrollbarTrackRect;
      ThumbCenter := (ThumbRect.Top + ThumbRect.Bottom) div 2;

      if Y < ThumbCenter then
      begin
        // Page up
        PageSize := FClientHeight;
        ScrollTo(FScrollPos - PageSize);
      end
      else
      begin
        // Page down
        PageSize := FClientHeight;
        ScrollTo(FScrollPos + PageSize);
      end;
    end;
  end;
end;

procedure TCustomScrollbar.HandleMouseMove(X, Y: Integer);
var
  TrackRect, ThumbRect: TRect;
  TrackHeight, ThumbHeight: Integer;
  DeltaY, NewScrollPos: Integer;
  ScrollRatio: Double;
begin
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
  end
  else
  begin
    // Update scrollbar hover state
    FScrollbarHover := IsPointInScrollbarThumb(X, Y);
  end;
end;

procedure TCustomScrollbar.HandleMouseUp(X, Y: Integer);
begin
  // End scrollbar dragging
  if FScrollbarDragging then
  begin
    FScrollbarDragging := False;
  end;
end;

function TCustomScrollbar.HandleMouseWheel(AWheelDelta: Integer): Boolean;
begin
  Result := True;
  ScrollTo(FScrollPos - (AWheelDelta div 2));
end;

procedure TCustomScrollbar.HandleMouseEnter;
begin
  if not FListHovered then
  begin
    FListHovered := True;
  end;
end;

function TCustomScrollbar.HandleMouseLeave: Boolean;
begin
  Result := False;
  if FScrollbarHover then
  begin
    FScrollbarHover := False;
    Result := True;  // State changed
  end;
  if FListHovered then
  begin
    FListHovered := False;
    Result := True;  // State changed
  end;
end;

procedure TCustomScrollbar.Render(ACanvas: TCanvas);
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

end.
