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
    FormWidth: Integer;
    FormHeight: Integer;
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

/// <summary>
/// Taskbar utility functions (used by strategies).
/// </summary>
function GetTaskbarRect(out ARect: TRect): Boolean;
function IsTaskbarHorizontal(const ARect: TRect): Boolean;

implementation

uses
  Vcl.Controls,
  App.Logger;

const
  // Windows Shell class name for taskbar
  TASKBAR_CLASS_NAME = 'Shell_TrayWnd';

  // Margin between window and screen/taskbar edge
  WINDOW_EDGE_MARGIN = 10;

{ Taskbar utility functions }

function GetTaskbarRect(out ARect: TRect): Boolean;
var
  TrayWnd: HWND;
begin
  TrayWnd := FindWindow(TASKBAR_CLASS_NAME, nil);
  Result := (TrayWnd <> 0) and GetWindowRect(TrayWnd, ARect);
end;

function IsTaskbarHorizontal(const ARect: TRect): Boolean;
var
  TaskbarWidth, TaskbarHeight: Integer;
begin
  TaskbarWidth := ARect.Right - ARect.Left;
  TaskbarHeight := ARect.Bottom - ARect.Top;
  // Use >= to treat square taskbars as horizontal (edge case)
  Result := TaskbarWidth >= TaskbarHeight;
end;

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
  Result.PositionConfig := APositionConfig;

  // Get cursor position and find which monitor it's on
  GetCursorPos(Result.CursorPos);
  Mon := Screen.MonitorFromPoint(Result.CursorPos);
  if Mon <> nil then
    Result.WorkArea := Mon.WorkareaRect
  else
    Result.WorkArea := Screen.WorkAreaRect;
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
  TrayRect: TRect;
  WorkArea: TRect;
  Mon: TMonitor;
begin
  WorkArea := AContext.WorkArea;

  // Find the actual taskbar/tray position
  if GetTaskbarRect(TrayRect) then
  begin
    LogDebug('Taskbar at L=%d,T=%d,R=%d,B=%d, Horizontal=%s',
      [TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom,
       BoolToStr(IsTaskbarHorizontal(TrayRect), True)], ClassName);

    // Get work area of monitor containing the taskbar
    Mon := Screen.MonitorFromRect(TrayRect);
    if Mon <> nil then
      WorkArea := Mon.WorkareaRect;

    if IsTaskbarHorizontal(TrayRect) then
    begin
      // Horizontal taskbar (top or bottom)
      if TrayRect.Top < WorkArea.Top then
      begin
        // Taskbar at top - position popup below taskbar, near right edge
        Result.X := TrayRect.Right - AContext.FormWidth - WINDOW_EDGE_MARGIN;
        Result.Y := TrayRect.Bottom + WINDOW_EDGE_MARGIN;
        LogDebug('Taskbar at TOP', ClassName);
      end
      else
      begin
        // Taskbar at bottom - position popup above taskbar, near right edge
        Result.X := TrayRect.Right - AContext.FormWidth - WINDOW_EDGE_MARGIN;
        Result.Y := TrayRect.Top - AContext.FormHeight - WINDOW_EDGE_MARGIN;
        LogDebug('Taskbar at BOTTOM', ClassName);
      end;
    end
    else
    begin
      // Vertical taskbar (left or right)
      if TrayRect.Left < WorkArea.Left then
      begin
        // Taskbar at left - position popup to the right of taskbar, near bottom
        Result.X := TrayRect.Right + WINDOW_EDGE_MARGIN;
        Result.Y := TrayRect.Bottom - AContext.FormHeight - WINDOW_EDGE_MARGIN;
        LogDebug('Taskbar at LEFT', ClassName);
      end
      else
      begin
        // Taskbar at right - position popup to the left of taskbar, near bottom
        Result.X := TrayRect.Left - AContext.FormWidth - WINDOW_EDGE_MARGIN;
        Result.Y := TrayRect.Bottom - AContext.FormHeight - WINDOW_EDGE_MARGIN;
        LogDebug('Taskbar at RIGHT', ClassName);
      end;
    end;
  end
  else
  begin
    // Fallback: position at bottom-right of cursor's monitor work area
    Result.X := WorkArea.Right - AContext.FormWidth - WINDOW_EDGE_MARGIN;
    Result.Y := WorkArea.Bottom - AContext.FormHeight - WINDOW_EDGE_MARGIN;
    LogDebug('Near tray (fallback to bottom-right)', ClassName);
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
begin
  // Build context with all positioning data
  Context := TPositionContext.Create(AForm, APositionConfig);

  LogDebug('PositionWindow: FormWidth=%d, FormHeight=%d, Mode=%d',
    [Context.FormWidth, Context.FormHeight, Ord(APosition)], ClassName);

  // Get strategy and calculate position
  Strategy := GetStrategy(APosition);
  NewPos := Strategy.CalculatePosition(Context);
  NewLeft := NewPos.X;
  NewTop := NewPos.Y;

  LogDebug('Before bounds check: NewLeft=%d, NewTop=%d, WorkArea=(%d,%d,%d,%d)',
    [NewLeft, NewTop, Context.WorkArea.Left, Context.WorkArea.Top,
     Context.WorkArea.Right, Context.WorkArea.Bottom], ClassName);

  // Ensure popup stays within work area
  if NewLeft < Context.WorkArea.Left then
    NewLeft := Context.WorkArea.Left;
  if NewLeft + Context.FormWidth > Context.WorkArea.Right then
    NewLeft := Context.WorkArea.Right - Context.FormWidth;
  if NewTop < Context.WorkArea.Top then
    NewTop := Context.WorkArea.Top;
  if NewTop + Context.FormHeight > Context.WorkArea.Bottom then
    NewTop := Context.WorkArea.Bottom - Context.FormHeight;

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
