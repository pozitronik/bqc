{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main View Interface Definitions                 }
{                                                       }
{       Defines the contract between MainPresenter      }
{       and the View (MainForm).                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.MainViewInterfaces;

interface

uses
  Winapi.Windows,
  UI.DeviceList;

type
  /// <summary>
  /// Notification flags for balloon notifications.
  /// Matches Vcl.ExtCtrls.TBalloonFlags.
  /// </summary>
  TNotificationFlags = (nfNone, nfInfo, nfWarning, nfError);

  /// <summary>
  /// Interface for the main view (implemented by the Form).
  /// Defines the contract between Presenter and View.
  /// </summary>
  IMainView = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    /// <summary>
    /// Displays pre-processed display items.
    /// Receives ready-to-render items with all formatting and
    /// sorting already applied by presenter.
    /// </summary>
    procedure ShowDisplayItems(const AItems: TDeviceDisplayItemArray);

    /// <summary>
    /// Updates a single display item in the list.
    /// </summary>
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);

    /// <summary>
    /// Clears all devices from the list.
    /// </summary>
    procedure ClearDevices;

    /// <summary>
    /// Sets the Bluetooth toggle switch state.
    /// </summary>
    /// <param name="AEnabled">True for On, False for Off.</param>
    procedure SetToggleState(AEnabled: Boolean);

    /// <summary>
    /// Enables or disables the toggle switch.
    /// </summary>
    procedure SetToggleEnabled(AEnabled: Boolean);

    /// <summary>
    /// Shows a status message.
    /// </summary>
    procedure ShowStatus(const AMessage: string);

    /// <summary>
    /// Shows a balloon notification from the tray icon.
    /// </summary>
    procedure ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);

    /// <summary>
    /// Shows or hides the busy cursor.
    /// </summary>
    procedure SetBusy(ABusy: Boolean);

    /// <summary>
    /// Returns True if the view is currently visible.
    /// </summary>
    function IsVisible: Boolean;

    /// <summary>
    /// Returns True if the view is minimized.
    /// </summary>
    function IsMinimized: Boolean;

    /// <summary>
    /// Returns the window handle for hotkey registration.
    /// </summary>
    function GetWindowHandle: HWND;

    /// <summary>
    /// Shows the view (from tray).
    /// </summary>
    procedure ShowView;

    /// <summary>
    /// Hides the view (to tray).
    /// </summary>
    procedure HideView;

    /// <summary>
    /// Forces the application to close (bypasses CloseToTray).
    /// </summary>
    procedure ForceClose;
  end;

implementation

end.
