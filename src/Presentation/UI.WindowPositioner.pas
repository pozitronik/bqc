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
  Mon: TMonitor;
  Bounds, WorkArea: TRect;
begin
  // Find tray on the active monitor (where cursor is)
  // Windows can show taskbar on all displays, so use cursor's monitor
  Mon := Screen.MonitorFromPoint(AContext.CursorPos);
  if Mon = nil then
    Mon := Screen.PrimaryMonitor;
  if Mon = nil then
    Mon := Screen.Monitors[0];

  Bounds := Mon.BoundsRect;
  WorkArea := Mon.WorkareaRect;

  // Detect tray corner by comparing monitor bounds to work area
  // Tray is always in a corner: top-right, bottom-right, or bottom-left (never top-left)
  if WorkArea.Top > Bounds.Top then
  begin
    // Taskbar at top -> tray at top-right
    // Position window's top-right corner at work area's top-right
    Result.X := WorkArea.Right - AContext.FormWidth;
    Result.Y := WorkArea.Top;
    LogDebug('Tray at TOP-RIGHT, window at (%d, %d)', [Result.X, Result.Y], ClassName);
  end
  else if WorkArea.Left > Bounds.Left then
  begin
    // Taskbar at left -> tray at bottom-left
    // Position window's bottom-left corner at work area's bottom-left
    Result.X := WorkArea.Left;
    Result.Y := WorkArea.Bottom - AContext.FormHeight;
    LogDebug('Tray at BOTTOM-LEFT, window at (%d, %d)', [Result.X, Result.Y], ClassName);
  end
  else
  begin
    // Taskbar at bottom or right -> tray at bottom-right
    // Position window's bottom-right corner at work area's bottom-right
    Result.X := WorkArea.Right - AContext.FormWidth;
    Result.Y := WorkArea.Bottom - AContext.FormHeight;
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
