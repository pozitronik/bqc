{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       System Tray Icon Manager                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.TrayManager;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Graphics;

type
  /// <summary>
  /// Manages the system tray icon, popup menu, and balloon notifications.
  /// Fires events for user interactions that the owner should handle.
  /// </summary>
  TTrayManager = class
  private
    FTrayIcon: TTrayIcon;
    FTrayMenu: TPopupMenu;
    FOnToggleVisibility: TNotifyEvent;
    FOnExitRequest: TNotifyEvent;

    procedure HandleTrayClick(Sender: TObject);
    procedure HandleMenuShowClick(Sender: TObject);
    procedure HandleMenuExitClick(Sender: TObject);
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    /// <summary>
    /// Creates the tray manager with icon and menu.
    /// </summary>
    /// <param name="AOwner">Component owner for memory management.</param>
    constructor Create(AOwner: TComponent);

    /// <summary>
    /// Shows a balloon notification from the tray icon.
    /// </summary>
    /// <param name="ATitle">The balloon title.</param>
    /// <param name="AMessage">The balloon message text.</param>
    /// <param name="AFlags">The balloon icon type.</param>
    procedure ShowNotification(const ATitle, AMessage: string; AFlags: TBalloonFlags);

    /// <summary>
    /// Updates the Show/Hide menu item caption based on form visibility.
    /// </summary>
    /// <param name="AFormVisible">True if the form is currently visible.</param>
    procedure UpdateMenuCaption(AFormVisible: Boolean);

    /// <summary>
    /// Sets the tray icon image.
    /// </summary>
    /// <param name="AIcon">The icon to display.</param>
    procedure SetIcon(AIcon: TIcon);

    /// <summary>
    /// Sets the tray icon hint text.
    /// </summary>
    /// <param name="AHint">The hint text.</param>
    procedure SetHint(const AHint: string);

    /// <summary>
    /// Gets or sets tray icon visibility.
    /// </summary>
    property Visible: Boolean read GetVisible write SetVisible;

    /// <summary>
    /// Fired when user clicks tray icon or Show/Hide menu item.
    /// Owner should toggle form visibility.
    /// </summary>
    property OnToggleVisibility: TNotifyEvent read FOnToggleVisibility write FOnToggleVisibility;

    /// <summary>
    /// Fired when user clicks Exit menu item.
    /// Owner should close the application.
    /// </summary>
    property OnExitRequest: TNotifyEvent read FOnExitRequest write FOnExitRequest;
  end;

implementation

uses
  Vcl.Forms,
  App.Logger;

{ TTrayManager }

constructor TTrayManager.Create(AOwner: TComponent);
var
  MenuItem: TMenuItem;
begin
  // Create popup menu for tray icon
  FTrayMenu := TPopupMenu.Create(AOwner);

  // Show/Hide menu item
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := 'Show';
  MenuItem.OnClick := HandleMenuShowClick;
  MenuItem.Default := True;
  FTrayMenu.Items.Add(MenuItem);

  // Separator
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := '-';
  FTrayMenu.Items.Add(MenuItem);

  // Exit menu item
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := 'Exit';
  MenuItem.OnClick := HandleMenuExitClick;
  FTrayMenu.Items.Add(MenuItem);

  // Create tray icon
  FTrayIcon := TTrayIcon.Create(AOwner);
  FTrayIcon.Hint := 'Bluetooth Quick Connect';
  FTrayIcon.PopupMenu := FTrayMenu;
  FTrayIcon.OnClick := HandleTrayClick;

  // Use application icon by default
  FTrayIcon.Icon.Assign(Application.Icon);

  // Tray icon is visible by default
  FTrayIcon.Visible := True;

  Log('[TrayManager] Created');
end;

procedure TTrayManager.HandleTrayClick(Sender: TObject);
begin
  if Assigned(FOnToggleVisibility) then
    FOnToggleVisibility(Self);
end;

procedure TTrayManager.HandleMenuShowClick(Sender: TObject);
begin
  if Assigned(FOnToggleVisibility) then
    FOnToggleVisibility(Self);
end;

procedure TTrayManager.HandleMenuExitClick(Sender: TObject);
begin
  Log('[TrayManager] Exit requested');
  if Assigned(FOnExitRequest) then
    FOnExitRequest(Self);
end;

procedure TTrayManager.ShowNotification(const ATitle, AMessage: string; AFlags: TBalloonFlags);
begin
  if FTrayIcon = nil then
    Exit;

  Log('[TrayManager] ShowNotification: Title="%s", Message="%s"', [ATitle, AMessage]);

  FTrayIcon.BalloonTitle := ATitle;
  FTrayIcon.BalloonHint := AMessage;
  FTrayIcon.BalloonFlags := AFlags;
  FTrayIcon.ShowBalloonHint;
end;

procedure TTrayManager.UpdateMenuCaption(AFormVisible: Boolean);
begin
  if FTrayMenu.Items.Count > 0 then
  begin
    if AFormVisible then
      FTrayMenu.Items[0].Caption := 'Hide'
    else
      FTrayMenu.Items[0].Caption := 'Show';
  end;
end;

procedure TTrayManager.SetIcon(AIcon: TIcon);
begin
  FTrayIcon.Icon.Assign(AIcon);
end;

procedure TTrayManager.SetHint(const AHint: string);
begin
  FTrayIcon.Hint := AHint;
end;

function TTrayManager.GetVisible: Boolean;
begin
  Result := FTrayIcon.Visible;
end;

procedure TTrayManager.SetVisible(AValue: Boolean);
begin
  FTrayIcon.Visible := AValue;
end;

end.
