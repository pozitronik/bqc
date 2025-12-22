{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Window Positioning Utilities                    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.WindowPositioner;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms,
  App.Config;

type
  /// <summary>
  /// Utility class for positioning popup windows relative to taskbar and screen.
  /// </summary>
  TWindowPositioner = class
  public
    /// <summary>
    /// Positions a form based on the specified position mode.
    /// Handles taskbar detection and multi-monitor support.
    /// </summary>
    /// <param name="AForm">The form to position.</param>
    /// <param name="APosition">The positioning mode.</param>
    class procedure PositionWindow(AForm: TForm; APosition: TPositionMode);

    /// <summary>
    /// Ensures a form stays within the work area of its current monitor.
    /// </summary>
    /// <param name="AForm">The form to adjust.</param>
    class procedure EnsureOnScreen(AForm: TForm);

  private
    class function GetTaskbarRect(out ARect: TRect): Boolean;
    class function IsTaskbarHorizontal(const ARect: TRect): Boolean;
  end;

implementation

uses
  Vcl.Controls,
  App.Logger;

const
  // Windows Shell class name for taskbar
  TASKBAR_CLASS_NAME = 'Shell_TrayWnd';

  // Margin between window and screen/taskbar edge
  WINDOW_EDGE_MARGIN = 10;

{ TWindowPositioner }

class function TWindowPositioner.GetTaskbarRect(out ARect: TRect): Boolean;
var
  TrayWnd: HWND;
begin
  TrayWnd := FindWindow(TASKBAR_CLASS_NAME, nil);
  Result := (TrayWnd <> 0) and GetWindowRect(TrayWnd, ARect);
end;

class function TWindowPositioner.IsTaskbarHorizontal(const ARect: TRect): Boolean;
var
  TaskbarWidth, TaskbarHeight: Integer;
begin
  TaskbarWidth := ARect.Right - ARect.Left;
  TaskbarHeight := ARect.Bottom - ARect.Top;
  Result := TaskbarWidth > TaskbarHeight;
end;

class procedure TWindowPositioner.PositionWindow(AForm: TForm; APosition: TPositionMode);
var
  CursorPos: TPoint;
  Mon: TMonitor;
  WorkArea: TRect;
  NewLeft, NewTop: Integer;
  TrayRect: TRect;
  FormWidth, FormHeight: Integer;
begin
  // Ensure window handle exists so dimensions are accurate
  if not AForm.HandleAllocated then
    AForm.HandleNeeded;

  // Use actual form dimensions
  FormWidth := AForm.Width;
  FormHeight := AForm.Height;

  Log('[WindowPositioner] PositionWindow: FormWidth=%d, FormHeight=%d, Mode=%d',
    [FormWidth, FormHeight, Ord(APosition)]);

  // Get cursor position and find which monitor it's on
  GetCursorPos(CursorPos);
  Mon := Screen.MonitorFromPoint(CursorPos);
  if Mon <> nil then
    WorkArea := Mon.WorkareaRect
  else
    WorkArea := Screen.WorkAreaRect;

  case APosition of
    pmCoordinates:
      begin
        // Use saved coordinates from config
        if (Config.PositionX >= 0) and (Config.PositionY >= 0) then
        begin
          NewLeft := Config.PositionX;
          NewTop := Config.PositionY;
          Log('[WindowPositioner] Using saved coordinates X=%d, Y=%d', [NewLeft, NewTop]);
        end
        else
        begin
          // Default: center on active monitor
          NewLeft := WorkArea.Left + (WorkArea.Width - FormWidth) div 2;
          NewTop := WorkArea.Top + (WorkArea.Height - FormHeight) div 2;
          Log('[WindowPositioner] No saved coordinates, centering on screen');
        end;
      end;

    pmNearCursor:
      begin
        // Position popup above the cursor, centered horizontally
        NewLeft := CursorPos.X - (FormWidth div 2);
        NewTop := CursorPos.Y - FormHeight - WINDOW_EDGE_MARGIN;
        Log('[WindowPositioner] Near cursor (%d, %d)', [CursorPos.X, CursorPos.Y]);
      end;

    pmNearTray:
      begin
        // Find the actual taskbar/tray position
        if GetTaskbarRect(TrayRect) then
        begin
          Log('[WindowPositioner] Taskbar at L=%d,T=%d,R=%d,B=%d, Horizontal=%s',
            [TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom,
             BoolToStr(IsTaskbarHorizontal(TrayRect), True)]);

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
              NewLeft := TrayRect.Right - FormWidth - WINDOW_EDGE_MARGIN;
              NewTop := TrayRect.Bottom + WINDOW_EDGE_MARGIN;
              Log('[WindowPositioner] Taskbar at TOP');
            end
            else
            begin
              // Taskbar at bottom - position popup above taskbar, near right edge
              NewLeft := TrayRect.Right - FormWidth - WINDOW_EDGE_MARGIN;
              NewTop := TrayRect.Top - FormHeight - WINDOW_EDGE_MARGIN;
              Log('[WindowPositioner] Taskbar at BOTTOM');
            end;
          end
          else
          begin
            // Vertical taskbar (left or right)
            if TrayRect.Left < WorkArea.Left then
            begin
              // Taskbar at left - position popup to the right of taskbar, near bottom
              NewLeft := TrayRect.Right + WINDOW_EDGE_MARGIN;
              NewTop := TrayRect.Bottom - FormHeight - WINDOW_EDGE_MARGIN;
              Log('[WindowPositioner] Taskbar at LEFT');
            end
            else
            begin
              // Taskbar at right - position popup to the left of taskbar, near bottom
              NewLeft := TrayRect.Left - FormWidth - WINDOW_EDGE_MARGIN;
              NewTop := TrayRect.Bottom - FormHeight - WINDOW_EDGE_MARGIN;
              Log('[WindowPositioner] Taskbar at RIGHT');
            end;
          end;
        end
        else
        begin
          // Fallback: position at bottom-right of cursor's monitor work area
          NewLeft := WorkArea.Right - FormWidth - WINDOW_EDGE_MARGIN;
          NewTop := WorkArea.Bottom - FormHeight - WINDOW_EDGE_MARGIN;
          Log('[WindowPositioner] Near tray (fallback to bottom-right)');
        end;
      end;

    pmCenterScreen:
      begin
        // Center on the monitor where cursor is
        NewLeft := WorkArea.Left + (WorkArea.Width - FormWidth) div 2;
        NewTop := WorkArea.Top + (WorkArea.Height - FormHeight) div 2;
        Log('[WindowPositioner] Center screen');
      end;

  else
    NewLeft := AForm.Left;
    NewTop := AForm.Top;
  end;

  Log('[WindowPositioner] Before bounds check: NewLeft=%d, NewTop=%d, WorkArea=(%d,%d,%d,%d)',
    [NewLeft, NewTop, WorkArea.Left, WorkArea.Top, WorkArea.Right, WorkArea.Bottom]);

  // Ensure popup stays within work area
  if NewLeft < WorkArea.Left then
    NewLeft := WorkArea.Left;
  if NewLeft + FormWidth > WorkArea.Right then
    NewLeft := WorkArea.Right - FormWidth;
  if NewTop < WorkArea.Top then
    NewTop := WorkArea.Top;
  if NewTop + FormHeight > WorkArea.Bottom then
    NewTop := WorkArea.Bottom - FormHeight;

  Log('[WindowPositioner] Final position: Left=%d, Top=%d', [NewLeft, NewTop]);

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
