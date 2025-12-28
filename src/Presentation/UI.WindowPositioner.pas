{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Window Positioning Utilities                    }
{                                                       }
{       Uses Strategy pattern for extensible            }
{       positioning modes (OCP-compliant).              }
{                                                       }
{*******************************************************}

unit UI.WindowPositioner;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  Vcl.Forms,
  App.ConfigEnums,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Context data passed to position strategies.
  /// Contains all information needed to calculate window position.
  /// </summary>
  TPositionContext = record
    FormWidth: Integer;        // Current form width (source DPI)
    FormHeight: Integer;       // Current form height (source DPI)
    FormPPI: Integer;          // Form's current DPI
    ScaledWidth: Integer;      // Form width scaled for target monitor DPI
    ScaledHeight: Integer;     // Form height scaled for target monitor DPI
    TargetPPI: Integer;        // Target monitor's DPI
    CursorPos: TPoint;
    WorkArea: TRect;
    PositionConfig: IPositionConfig;
    class function Create(AForm: TForm; APositionConfig: IPositionConfig): TPositionContext; static;
  end;

  /// <summary>
  /// Strategy interface for window positioning.
  /// Implement this to add new positioning modes (OCP).
  /// </summary>
  IPositionStrategy = interface
    ['{F1E2D3C4-B5A6-9788-7654-321098765432}']
    /// <summary>
    /// Calculates the top-left position for the window.
    /// </summary>
    /// <param name="AContext">Context with form dimensions, cursor pos, work area.</param>
    /// <returns>Point with calculated Left and Top coordinates.</returns>
    function CalculatePosition(const AContext: TPositionContext): TPoint;
  end;

  /// <summary>
  /// Strategy: Position at saved coordinates or center if not set.
  /// </summary>
  TCoordinatesPositioner = class(TInterfacedObject, IPositionStrategy)
  public
    function CalculatePosition(const AContext: TPositionContext): TPoint;
  end;

  /// <summary>
  /// Strategy: Position near the cursor.
  /// </summary>
  TNearCursorPositioner = class(TInterfacedObject, IPositionStrategy)
  public
    function CalculatePosition(const AContext: TPositionContext): TPoint;
  end;

  /// <summary>
  /// Strategy: Position near the system tray/taskbar.
  /// </summary>
  TNearTrayPositioner = class(TInterfacedObject, IPositionStrategy)
  public
    function CalculatePosition(const AContext: TPositionContext): TPoint;
  end;

  /// <summary>
  /// Strategy: Center on the current monitor.
  /// </summary>
  TCenterScreenPositioner = class(TInterfacedObject, IPositionStrategy)
  public
    function CalculatePosition(const AContext: TPositionContext): TPoint;
  end;

  /// <summary>
  /// Utility class for positioning popup windows relative to taskbar and screen.
  /// Uses strategy pattern for extensible positioning modes.
  /// </summary>
  TWindowPositioner = class
  public
    /// <summary>
    /// Positions a form based on the specified position mode.
    /// Handles taskbar detection and multi-monitor support.
    /// </summary>
    /// <param name="AForm">The form to position.</param>
    /// <param name="APositionConfig">Position configuration (for pmCoordinates mode).</param>
    /// <param name="APosition">The positioning mode.</param>
    class procedure PositionWindow(AForm: TForm; APositionConfig: IPositionConfig;
      APosition: TPositionMode);

    /// <summary>
    /// Ensures a form stays within the work area of its current monitor.
    /// </summary>
    /// <param name="AForm">The form to adjust.</param>
    class procedure EnsureOnScreen(AForm: TForm);

    /// <summary>
    /// Gets the appropriate positioning strategy for a mode.
    /// </summary>
    class function GetStrategy(AMode: TPositionMode): IPositionStrategy;
  end;

implementation

uses
  Vcl.Controls,
  App.Logger;

const
  // Margin between window and screen edge
  WINDOW_EDGE_MARGIN = 10;

{ TPositionContext }

class function TPositionContext.Create(AForm: TForm; APositionConfig: IPositionConfig): TPositionContext;
var
  Mon: TMonitor;
begin
  // Ensure window handle exists so dimensions are accurate
  if not AForm.HandleAllocated then
    AForm.HandleNeeded;

  Result.FormWidth := AForm.Width;
  Result.FormHeight := AForm.Height;
  Result.FormPPI := AForm.CurrentPPI;
  Result.PositionConfig := APositionConfig;

  // Get cursor position and find which monitor it's on
  GetCursorPos(Result.CursorPos);
  Mon := Screen.MonitorFromPoint(Result.CursorPos);
  if Mon <> nil then
  begin
    Result.WorkArea := Mon.WorkareaRect;
    Result.TargetPPI := Mon.PixelsPerInch;
  end
  else
  begin
    Result.WorkArea := Screen.WorkAreaRect;
    Result.TargetPPI := Screen.PixelsPerInch;
  end;

  // Calculate scaled dimensions for target monitor's DPI
  // This handles cross-DPI monitor positioning
  if (Result.FormPPI > 0) and (Result.TargetPPI > 0) and (Result.FormPPI <> Result.TargetPPI) then
  begin
    Result.ScaledWidth := MulDiv(Result.FormWidth, Result.TargetPPI, Result.FormPPI);
    Result.ScaledHeight := MulDiv(Result.FormHeight, Result.TargetPPI, Result.FormPPI);
  end
  else
  begin
    Result.ScaledWidth := Result.FormWidth;
    Result.ScaledHeight := Result.FormHeight;
  end;
end;

{ TCoordinatesPositioner }

function TCoordinatesPositioner.CalculatePosition(const AContext: TPositionContext): TPoint;
begin
  // Use saved coordinates from config
  if (AContext.PositionConfig.PositionX >= 0) and (AContext.PositionConfig.PositionY >= 0) then
  begin
    Result.X := AContext.PositionConfig.PositionX;
    Result.Y := AContext.PositionConfig.PositionY;
    LogDebug('Using saved coordinates X=%d, Y=%d', [Result.X, Result.Y], ClassName);
  end
  else
  begin
    // Default: center on active monitor
    Result.X := AContext.WorkArea.Left + (AContext.WorkArea.Width - AContext.FormWidth) div 2;
    Result.Y := AContext.WorkArea.Top + (AContext.WorkArea.Height - AContext.FormHeight) div 2;
    LogDebug('No saved coordinates, centering on screen', ClassName);
  end;
end;

{ TNearCursorPositioner }

function TNearCursorPositioner.CalculatePosition(const AContext: TPositionContext): TPoint;
begin
  // Position popup above the cursor, centered horizontally
  Result.X := AContext.CursorPos.X - (AContext.FormWidth div 2);
  Result.Y := AContext.CursorPos.Y - AContext.FormHeight - WINDOW_EDGE_MARGIN;
  LogDebug('Near cursor (%d, %d)', [AContext.CursorPos.X, AContext.CursorPos.Y], ClassName);
end;

{ TNearTrayPositioner }

function TNearTrayPositioner.CalculatePosition(const AContext: TPositionContext): TPoint;
var
  Mon: TMonitor;
  Bounds, WorkArea: TRect;
  TrayWnd, SecondaryTrayWnd: HWND;
  TrayRect: TRect;
  HasSecondaryTaskbar: Boolean;
  I: Integer;
  ScaledWidth, ScaledHeight: Integer;
begin
  // Debug: Log all monitors
  LogDebug('MonitorCount=%d, CursorPos=(%d,%d)', [Screen.MonitorCount, AContext.CursorPos.X, AContext.CursorPos.Y], ClassName);
  for I := 0 to Screen.MonitorCount - 1 do
  begin
    LogDebug('Monitor[%d]: Primary=%s, Bounds=(%d,%d,%d,%d), WorkArea=(%d,%d,%d,%d)', [
      I,
      BoolToStr(Screen.Monitors[I].Primary, True),
      Screen.Monitors[I].BoundsRect.Left, Screen.Monitors[I].BoundsRect.Top,
      Screen.Monitors[I].BoundsRect.Right, Screen.Monitors[I].BoundsRect.Bottom,
      Screen.Monitors[I].WorkareaRect.Left, Screen.Monitors[I].WorkareaRect.Top,
      Screen.Monitors[I].WorkareaRect.Right, Screen.Monitors[I].WorkareaRect.Bottom
    ], ClassName);
  end;

  // Check if Windows has taskbar on all displays (secondary taskbars exist)
  // Shell_SecondaryTrayWnd windows only exist when "show taskbar on all displays" is enabled
  SecondaryTrayWnd := FindWindow('Shell_SecondaryTrayWnd', nil);
  HasSecondaryTaskbar := SecondaryTrayWnd <> 0;
  LogDebug('Shell_SecondaryTrayWnd search: Handle=$%x, HasSecondaryTaskbar=%s', [
    SecondaryTrayWnd, BoolToStr(HasSecondaryTaskbar, True)
  ], ClassName);

  // Always find Shell_TrayWnd for logging
  TrayWnd := FindWindow('Shell_TrayWnd', nil);
  if TrayWnd <> 0 then
  begin
    GetWindowRect(TrayWnd, TrayRect);
    LogDebug('Shell_TrayWnd: Handle=$%x, Rect=(%d,%d,%d,%d)', [
      TrayWnd, TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom
    ], ClassName);
  end
  else
  begin
    LogDebug('Shell_TrayWnd: NOT FOUND', ClassName);
  end;

  if HasSecondaryTaskbar then
  begin
    // Taskbar on all displays - use active monitor (where cursor is)
    // Each monitor has its own taskbar, so position near the one user is looking at
    Mon := Screen.MonitorFromPoint(AContext.CursorPos);
    LogDebug('Decision: Taskbar on all displays, using cursor monitor', ClassName);
  end
  else
  begin
    // Taskbar only on one display - find the monitor with Shell_TrayWnd
    // This works with third-party tools (Open-Shell, etc.) that move the taskbar
    if TrayWnd <> 0 then
    begin
      Mon := Screen.MonitorFromRect(TrayRect);
      if Mon <> nil then
        LogDebug('Decision: Using Shell_TrayWnd monitor, MonitorIndex=%d, Primary=%s', [
          Mon.MonitorNum, BoolToStr(Mon.Primary, True)
        ], ClassName)
      else
        LogDebug('Decision: Screen.MonitorFromRect returned nil!', ClassName);
    end
    else
    begin
      // Fallback: use cursor's monitor if tray not found (shouldn't happen)
      Mon := Screen.MonitorFromPoint(AContext.CursorPos);
      LogDebug('Decision: Shell_TrayWnd not found, falling back to cursor monitor', ClassName);
    end;
  end;

  // Fallback chain for edge cases
  if Mon = nil then
  begin
    Mon := Screen.PrimaryMonitor;
    LogDebug('Fallback: Mon was nil, using PrimaryMonitor', ClassName);
  end;
  if Mon = nil then
  begin
    Mon := Screen.Monitors[0];
    LogDebug('Fallback: PrimaryMonitor was nil, using Monitors[0]', ClassName);
  end;

  Bounds := Mon.BoundsRect;
  WorkArea := Mon.WorkareaRect;
  LogDebug('Final monitor: Index=%d, Bounds=(%d,%d,%d,%d), WorkArea=(%d,%d,%d,%d), PPI=%d', [
    Mon.MonitorNum, Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom,
    WorkArea.Left, WorkArea.Top, WorkArea.Right, WorkArea.Bottom, Mon.PixelsPerInch
  ], ClassName);

  // Calculate scaled dimensions for THIS monitor's DPI (may differ from cursor's monitor)
  // AContext.ScaledWidth/Height were calculated for cursor's monitor, we need target monitor's DPI
  if (AContext.FormPPI > 0) and (Mon.PixelsPerInch > 0) and (AContext.FormPPI <> Mon.PixelsPerInch) then
  begin
    ScaledWidth := MulDiv(AContext.FormWidth, Mon.PixelsPerInch, AContext.FormPPI);
    ScaledHeight := MulDiv(AContext.FormHeight, Mon.PixelsPerInch, AContext.FormPPI);
    LogDebug('DPI scale for tray monitor: FormPPI=%d, TargetPPI=%d, %dx%d -> %dx%d',
      [AContext.FormPPI, Mon.PixelsPerInch, AContext.FormWidth, AContext.FormHeight,
       ScaledWidth, ScaledHeight], ClassName);
  end
  else
  begin
    ScaledWidth := AContext.FormWidth;
    ScaledHeight := AContext.FormHeight;
    LogDebug('No DPI scaling needed: FormPPI=%d, TargetPPI=%d, Size=%dx%d',
      [AContext.FormPPI, Mon.PixelsPerInch, ScaledWidth, ScaledHeight], ClassName);
  end;

  // Detect tray corner by comparing monitor bounds to work area
  // Tray is always in a corner: top-right, bottom-right, or bottom-left (never top-left)
  // Use locally calculated ScaledWidth/ScaledHeight for correct positioning on target monitor DPI
  if WorkArea.Top > Bounds.Top then
  begin
    // Taskbar at top -> tray at top-right
    // Position window's top-right corner at work area's top-right
    Result.X := WorkArea.Right - ScaledWidth;
    Result.Y := WorkArea.Top;
    LogDebug('Tray at TOP-RIGHT, window at (%d, %d)', [Result.X, Result.Y], ClassName);
  end
  else if WorkArea.Left > Bounds.Left then
  begin
    // Taskbar at left -> tray at bottom-left
    // Position window's bottom-left corner at work area's bottom-left
    Result.X := WorkArea.Left;
    Result.Y := WorkArea.Bottom - ScaledHeight;
    LogDebug('Tray at BOTTOM-LEFT, window at (%d, %d)', [Result.X, Result.Y], ClassName);
  end
  else
  begin
    // Taskbar at bottom or right -> tray at bottom-right
    // Position window's bottom-right corner at work area's bottom-right
    Result.X := WorkArea.Right - ScaledWidth;
    Result.Y := WorkArea.Bottom - ScaledHeight;
    LogDebug('Tray at BOTTOM-RIGHT, window at (%d, %d)', [Result.X, Result.Y], ClassName);
  end;
end;

{ TCenterScreenPositioner }

function TCenterScreenPositioner.CalculatePosition(const AContext: TPositionContext): TPoint;
begin
  // Center on the monitor where cursor is
  Result.X := AContext.WorkArea.Left + (AContext.WorkArea.Width - AContext.FormWidth) div 2;
  Result.Y := AContext.WorkArea.Top + (AContext.WorkArea.Height - AContext.FormHeight) div 2;
  LogDebug('Center screen', ClassName);
end;

{ TWindowPositioner }

class function TWindowPositioner.GetStrategy(AMode: TPositionMode): IPositionStrategy;
begin
  case AMode of
    pmCoordinates:
      Result := TCoordinatesPositioner.Create;
    pmNearCursor:
      Result := TNearCursorPositioner.Create;
    pmNearTray:
      Result := TNearTrayPositioner.Create;
    pmCenterScreen:
      Result := TCenterScreenPositioner.Create;
  else
    Result := TCenterScreenPositioner.Create; // Default fallback
  end;
end;

class procedure TWindowPositioner.PositionWindow(AForm: TForm;
  APositionConfig: IPositionConfig; APosition: TPositionMode);
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  NewPos: TPoint;
  NewLeft, NewTop: Integer;
  TargetMon: TMonitor;
  TargetWorkArea: TRect;
  ScaledWidth, ScaledHeight: Integer;
begin
  // Build context with all positioning data
  Context := TPositionContext.Create(AForm, APositionConfig);

  LogDebug('PositionWindow: FormWidth=%d, FormHeight=%d, ScaledWidth=%d, ScaledHeight=%d, Mode=%d',
    [Context.FormWidth, Context.FormHeight, Context.ScaledWidth, Context.ScaledHeight,
     Ord(APosition)], ClassName);

  // Get strategy and calculate position
  Strategy := GetStrategy(APosition);
  NewPos := Strategy.CalculatePosition(Context);
  NewLeft := NewPos.X;
  NewTop := NewPos.Y;

  // Find the monitor where the calculated position is located
  // This may be different from cursor's monitor (e.g., "Near Tray" positions on tray's monitor)
  TargetMon := Screen.MonitorFromPoint(NewPos);
  if TargetMon <> nil then
  begin
    TargetWorkArea := TargetMon.WorkareaRect;
    // Recalculate scaled dimensions for the ACTUAL target monitor's DPI
    // Context.ScaledWidth/Height were calculated for cursor's monitor, not target
    if (Context.FormPPI > 0) and (TargetMon.PixelsPerInch > 0) and
       (Context.FormPPI <> TargetMon.PixelsPerInch) then
    begin
      ScaledWidth := MulDiv(Context.FormWidth, TargetMon.PixelsPerInch, Context.FormPPI);
      ScaledHeight := MulDiv(Context.FormHeight, TargetMon.PixelsPerInch, Context.FormPPI);
      LogDebug('DPI rescale for target monitor: FormPPI=%d, TargetPPI=%d, %dx%d -> %dx%d',
        [Context.FormPPI, TargetMon.PixelsPerInch, Context.FormWidth, Context.FormHeight,
         ScaledWidth, ScaledHeight], ClassName);
    end
    else
    begin
      ScaledWidth := Context.FormWidth;
      ScaledHeight := Context.FormHeight;
    end;
  end
  else
  begin
    TargetWorkArea := Context.WorkArea; // Fallback to cursor's monitor
    ScaledWidth := Context.ScaledWidth;
    ScaledHeight := Context.ScaledHeight;
  end;

  LogDebug('Before bounds check: NewLeft=%d, NewTop=%d, TargetWorkArea=(%d,%d,%d,%d), ScaledSize=%dx%d',
    [NewLeft, NewTop, TargetWorkArea.Left, TargetWorkArea.Top,
     TargetWorkArea.Right, TargetWorkArea.Bottom, ScaledWidth, ScaledHeight], ClassName);

  // Ensure popup stays within the TARGET monitor's work area
  // Use recalculated ScaledWidth/ScaledHeight for correct bounds on target monitor DPI
  if NewLeft < TargetWorkArea.Left then
    NewLeft := TargetWorkArea.Left;
  if NewLeft + ScaledWidth > TargetWorkArea.Right then
    NewLeft := TargetWorkArea.Right - ScaledWidth;
  if NewTop < TargetWorkArea.Top then
    NewTop := TargetWorkArea.Top;
  if NewTop + ScaledHeight > TargetWorkArea.Bottom then
    NewTop := TargetWorkArea.Bottom - ScaledHeight;

  LogDebug('Final position: Left=%d, Top=%d', [NewLeft, NewTop], ClassName);

  AForm.Left := NewLeft;
  AForm.Top := NewTop;
end;

class procedure TWindowPositioner.EnsureOnScreen(AForm: TForm);
var
  Mon: TMonitor;
  WorkArea: TRect;
begin
  Mon := Screen.MonitorFromWindow(AForm.Handle);
  if Mon <> nil then
    WorkArea := Mon.WorkareaRect
  else
    WorkArea := Screen.WorkAreaRect;

  if AForm.Left < WorkArea.Left then
    AForm.Left := WorkArea.Left;
  if AForm.Left + AForm.Width > WorkArea.Right then
    AForm.Left := WorkArea.Right - AForm.Width;
  if AForm.Top < WorkArea.Top then
    AForm.Top := WorkArea.Top;
  if AForm.Top + AForm.Height > WorkArea.Bottom then
    AForm.Top := WorkArea.Bottom - AForm.Height;
end;

end.
