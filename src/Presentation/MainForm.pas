unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.WinXCtrls,
  Vcl.Menus,
  Bluetooth.Types,
  UI.Theme,
  UI.DeviceList,
  UI.TrayManager,
  UI.HotkeyManager,
  App.MainViewInterfaces,
  App.MainPresenter, Vcl.Buttons, System.ImageList, Vcl.ImgList;

const
  WM_DPICHANGED = $02E0;

  // Windows Settings URI for Bluetooth
  WINDOWS_BLUETOOTH_SETTINGS_URI = 'ms-settings:bluetooth';

  // Window size constraints for auto-sizing
  WINDOW_MIN_WIDTH = 280;
  WINDOW_MAX_WIDTH = 500;
  WINDOW_MIN_HEIGHT = 200;
  WINDOW_MAX_HEIGHT = 600;
  WINDOW_DEFAULT_WIDTH = 320;
  WINDOW_DEFAULT_HEIGHT = 400;

type
  /// <summary>
  /// Main application form displaying Bluetooth devices.
  /// Implements IMainView for MVP pattern.
  /// </summary>
  TFormMain = class(TForm, IMainView)
    HeaderPanel: TPanel;
    TitleLabel: TLabel;
    StatusPanel: TPanel;
    SettingsLabel: TLabel;
    StatusLabel: TLabel;
    WindowsSettingsLink: TLabel;
    DevicesPanel: TPanel;
    BluetoothTogglePanel: TPanel;
    BluetoothToggle: TToggleSwitch;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleBluetoothToggle(Sender: TObject);
    procedure HandleSettingsClick(Sender: TObject);
    procedure HandleWindowsSettingsClick(Sender: TObject);
    procedure HandleRefreshClick(Sender: TObject);
    procedure TitleLabelClick(Sender: TObject);
  private
    FPresenter: TMainPresenter;
    FDeviceList: TDeviceListBox;
    FTrayManager: TTrayManager;
    FHotkeyManager: THotkeyManager;
    FForceClose: Boolean;

    { View setup }
    procedure CreateDeviceList;
    procedure ApplyTheme;
    procedure ApplyWindowMode;
    procedure ApplyMenuModeTaskbarHide;
    procedure ApplyWindowPosition;
    procedure ApplyWindowSize;
    procedure CalculateAutoSize(out AWidth, AHeight: Integer);

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleHotkeyTriggered(Sender: TObject);
    procedure HandleTrayToggleVisibility(Sender: TObject);
    procedure HandleTraySettingsRequest(Sender: TObject);
    procedure HandleTrayExitRequest(Sender: TObject);
    procedure HandleApplicationDeactivate(Sender: TObject);
    procedure HandleSettingsApplied(Sender: TObject);

    { IMainView implementation }
    procedure ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);
    procedure ClearDevices;
    procedure SetToggleState(AEnabled: Boolean);
    procedure SetToggleEnabled(AEnabled: Boolean);
    procedure ShowStatus(const AMessage: string);
    procedure ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);
    procedure SetBusy(ABusy: Boolean);
    function IsVisible: Boolean;
    function IsMinimized: Boolean;
    function GetWindowHandle: HWND;
    procedure ShowView;
    procedure HideView;
    procedure IMainView.ForceClose = DoForceClose;
    procedure DoForceClose;

  protected
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotkey(var Msg: TMessage); message WM_HOTKEY;
    procedure WMHotkeyDetected(var Msg: TMessage); message WM_HOTKEY_DETECTED;
    procedure WMDpiChanged(var Msg: TMessage); message WM_DPICHANGED;

  public
    { Public declarations }
    procedure ApplyAllSettings;
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes,
  ShellAPI,
  App.Logger,
  App.ConfigInterfaces,
  App.Bootstrap,
  App.SettingsPresenter,
  UI.WindowPositioner,
  SettingsForm;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Load configuration early via Bootstrap
  Bootstrap;

  Log('FormCreate: Starting', ClassName);

  FForceClose := False;

  // Apply window mode (must be done early before window handle is created)
  ApplyWindowMode;

  // Apply window size first (may be auto-calculated)
  ApplyWindowSize;

  // Apply window position (depends on size being set)
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    // Menu mode: start hidden off-screen, position will be set on show
    Position := poDesigned;
    Left := -10000;
    Top := -10000;
    Log('FormCreate: Menu mode, position will be set on show', ClassName);
  end
  else
  begin
    // Window mode: apply position based on PositionMode
    Position := poDesigned;
    ApplyWindowPosition;
  end;

  // Apply OnTop setting
  if Bootstrap.GeneralConfig.OnTop or (Bootstrap.GeneralConfig.WindowMode = wmMenu) then
  begin
    FormStyle := fsStayOnTop;
    Log('FormCreate: OnTop enabled', ClassName);
  end;

  // Load external VCL styles and apply configured theme
  Theme.LoadStylesFromDirectory(Bootstrap.AppearanceConfig.VsfDir);
  Theme.SetStyle(Bootstrap.AppearanceConfig.Theme);

  // Subscribe to application deactivation
  Application.OnDeactivate := HandleApplicationDeactivate;

  // Create UI components
  CreateDeviceList;

  // Create tray manager
  FTrayManager := TTrayManager.Create(Self);
  FTrayManager.OnToggleVisibility := HandleTrayToggleVisibility;
  FTrayManager.OnSettingsRequest := HandleTraySettingsRequest;
  FTrayManager.OnExitRequest := HandleTrayExitRequest;

  // Apply configuration to device list
  FDeviceList.ShowAddresses := Bootstrap.AppearanceConfig.ShowAddresses;

  // Create and initialize presenter
  FPresenter := TMainPresenter.Create(Self);
  FPresenter.Initialize;

  // Create and register global hotkey
  FHotkeyManager := THotkeyManager.Create;
  FHotkeyManager.OnHotkeyTriggered := HandleHotkeyTriggered;
  FHotkeyManager.Register(Handle, Bootstrap.HotkeyConfig.Hotkey, Bootstrap.HotkeyConfig.UseLowLevelHook);

  // Hide from taskbar in Menu mode
  ApplyMenuModeTaskbarHide;

  // In Menu mode, start hidden
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    Log('FormCreate: Menu mode, starting hidden', ClassName);
    Application.ShowMainForm := False;
    Visible := False;
  end;

  Log('FormCreate: Complete', ClassName);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Save window position and size (always save, applies when PositionMode=0)
  if WindowState = wsNormal then
  begin
    Bootstrap.PositionConfig.PositionX := Left;
    Bootstrap.PositionConfig.PositionY := Top;
    Bootstrap.PositionConfig.PositionW := Width;
    Bootstrap.PositionConfig.PositionH := Height;
    Log('FormDestroy: Saved position X=%d, Y=%d, W=%d, H=%d',
      [Left, Top, Width, Height], ClassName);
  end;

  // Shutdown presenter
  if FPresenter <> nil then
  begin
    FPresenter.Shutdown;
    FPresenter.Free;
    FPresenter := nil;
  end;

  // Free hotkey manager
  FHotkeyManager.Free;

  Application.OnDeactivate := nil;

  Log('FormDestroy: Complete', ClassName);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_F5:
      FPresenter.OnRefreshRequested;
  end;
end;

procedure TFormMain.CreateDeviceList;
begin
  FDeviceList := TDeviceListBox.Create(Self);
  FDeviceList.Parent := DevicesPanel;
  FDeviceList.Align := alClient;
  FDeviceList.OnDeviceClick := HandleDeviceClick;
  FDeviceList.TabOrder := 0;
end;

procedure TFormMain.ApplyTheme;
begin
  // VCL styles handle colors automatically
  // Invalidate and repaint the form to show changes immediately
  Invalidate;
  if Assigned(FDeviceList) then
    FDeviceList.Invalidate;
  Update;
end;

procedure TFormMain.ApplyWindowMode;
begin
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    Log('ApplyWindowMode: Menu mode', ClassName);
  end
  else
  begin
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    Log('ApplyWindowMode: Window mode', ClassName);
  end;
end;

procedure TFormMain.ApplyMenuModeTaskbarHide;
var
  ExStyle: LONG_PTR;
begin
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    ExStyle := GetWindowLongPtr(Handle, GWL_EXSTYLE);
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    ExStyle := ExStyle and (not WS_EX_APPWINDOW);
    SetWindowLongPtr(Handle, GWL_EXSTYLE, ExStyle);
    Log('ApplyMenuModeTaskbarHide: Hidden from taskbar', ClassName);
  end;
end;

procedure TFormMain.ApplyWindowPosition;
begin
  TWindowPositioner.PositionWindow(Self, Bootstrap.PositionConfig.PositionMode);
end;

procedure TFormMain.ApplyWindowSize;
var
  NewWidth, NewHeight: Integer;
begin
  // Check if auto-sizing is requested (-1 means auto)
  if (Bootstrap.PositionConfig.PositionW < 0) or (Bootstrap.PositionConfig.PositionH < 0) then
  begin
    CalculateAutoSize(NewWidth, NewHeight);
    if Bootstrap.PositionConfig.PositionW < 0 then
      Width := NewWidth
    else
      Width := Bootstrap.PositionConfig.PositionW;
    if Bootstrap.PositionConfig.PositionH < 0 then
      Height := NewHeight
    else
      Height := Bootstrap.PositionConfig.PositionH;
    Log('ApplyWindowSize: Auto-calculated W=%d, H=%d', [Width, Height], ClassName);
  end
  else if (Bootstrap.PositionConfig.PositionW > 0) and (Bootstrap.PositionConfig.PositionH > 0) then
  begin
    Width := Bootstrap.PositionConfig.PositionW;
    Height := Bootstrap.PositionConfig.PositionH;
    Log('ApplyWindowSize: Restored W=%d, H=%d', [Width, Height], ClassName);
  end;
  // else: use default form dimensions from DFM
end;

procedure TFormMain.CalculateAutoSize(out AWidth, AHeight: Integer);
begin
  // For now, use reasonable defaults
  // TODO: Could calculate based on actual device count and content
  AWidth := WINDOW_DEFAULT_WIDTH;
  AHeight := WINDOW_DEFAULT_HEIGHT;

  // Apply constraints
  if AWidth < WINDOW_MIN_WIDTH then AWidth := WINDOW_MIN_WIDTH;
  if AWidth > WINDOW_MAX_WIDTH then AWidth := WINDOW_MAX_WIDTH;
  if AHeight < WINDOW_MIN_HEIGHT then AHeight := WINDOW_MIN_HEIGHT;
  if AHeight > WINDOW_MAX_HEIGHT then AHeight := WINDOW_MAX_HEIGHT;

  Log('CalculateAutoSize: Calculated W=%d, H=%d', [AWidth, AHeight], ClassName);
end;

procedure TFormMain.ApplyAllSettings;
var
  ExStyle: LONG_PTR;
begin
  Log('ApplyAllSettings: Applying configuration changes', ClassName);

  // Re-register hotkey (unregister first, then register with new settings)
  FHotkeyManager.Unregister;
  FHotkeyManager.Register(Handle, Bootstrap.HotkeyConfig.Hotkey, Bootstrap.HotkeyConfig.UseLowLevelHook);
  Log('ApplyAllSettings: Hotkey re-registered: %s', [Bootstrap.HotkeyConfig.Hotkey], ClassName);

  // Reload styles from directory (loads any new styles, skips already loaded)
  Theme.LoadStylesFromDirectory(Bootstrap.AppearanceConfig.VsfDir);

  // Apply theme
  Theme.SetStyle(Bootstrap.AppearanceConfig.Theme);
  ApplyTheme;
  Log('ApplyAllSettings: Theme applied: %s', [Bootstrap.AppearanceConfig.Theme], ClassName);

  // Apply window mode (border style and icons)
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    Log('ApplyAllSettings: Applied Menu mode', ClassName);
  end
  else
  begin
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    Log('ApplyAllSettings: Applied Window mode', ClassName);
  end;

  // Apply taskbar visibility based on window mode
  // Must hide window before changing style, then show again to update taskbar
  ExStyle := GetWindowLongPtr(Handle, GWL_EXSTYLE);
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
  begin
    ShowWindow(Handle, SW_HIDE);
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    ExStyle := ExStyle and (not WS_EX_APPWINDOW);
    SetWindowLongPtr(Handle, GWL_EXSTYLE, ExStyle);
    ShowWindow(Handle, SW_SHOW);
  end
  else
  begin
    ShowWindow(Handle, SW_HIDE);
    ExStyle := ExStyle and (not WS_EX_TOOLWINDOW);
    ExStyle := ExStyle or WS_EX_APPWINDOW;
    SetWindowLongPtr(Handle, GWL_EXSTYLE, ExStyle);
    ShowWindow(Handle, SW_SHOW);
  end;
  Log('ApplyAllSettings: Taskbar visibility updated', ClassName);

  // Apply OnTop setting
  if Bootstrap.GeneralConfig.OnTop or (Bootstrap.GeneralConfig.WindowMode = wmMenu) then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
  Log('ApplyAllSettings: OnTop=%s', [BoolToStr(Bootstrap.GeneralConfig.OnTop, True)], ClassName);

  // Apply ShowAddresses to device list
  if FDeviceList <> nil then
  begin
    FDeviceList.ShowAddresses := Bootstrap.AppearanceConfig.ShowAddresses;
    FDeviceList.Invalidate;
  end;

  // Notify presenter to refresh if needed (for polling changes, etc.)
  if FPresenter <> nil then
  begin
    FPresenter.OnSettingsChanged;
    // Refresh device list to apply pinned/hidden changes
    FPresenter.OnRefreshRequested;
  end;

  Log('ApplyAllSettings: Complete', ClassName);
end;

{ Event handlers }

procedure TFormMain.HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
begin
  FPresenter.OnDeviceClicked(ADevice);
end;

procedure TFormMain.HandleBluetoothToggle(Sender: TObject);
begin
  if not FPresenter.IsUpdatingToggle then
    FPresenter.OnToggleChanged(BluetoothToggle.State = tssOn);
end;

procedure TFormMain.HandleSettingsClick(Sender: TObject);
var
  SettingsDialog: TFormSettings;
begin
  Log('HandleSettingsClick: Opening settings dialog', ClassName);
  SettingsDialog := TFormSettings.Create(Self);
  try
    SettingsDialog.OnSettingsApplied := HandleSettingsApplied;
    SettingsDialog.ShowModal;
  finally
    SettingsDialog.Free;
  end;
end;

procedure TFormMain.HandleWindowsSettingsClick(Sender: TObject);
begin
  ShellExecute(0, 'open', WINDOWS_BLUETOOTH_SETTINGS_URI, nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.HandleRefreshClick(Sender: TObject);
begin
  FPresenter.OnRefreshRequested;
end;

procedure TFormMain.TitleLabelClick(Sender: TObject);
begin
  FPresenter.OnRefreshRequested;
end;

procedure TFormMain.HandleSettingsApplied(Sender: TObject);
begin
  ApplyAllSettings;
end;

procedure TFormMain.HandleHotkeyTriggered(Sender: TObject);
begin
  FPresenter.OnVisibilityToggleRequested;
end;

procedure TFormMain.HandleTrayToggleVisibility(Sender: TObject);
begin
  FPresenter.OnVisibilityToggleRequested;
end;

procedure TFormMain.HandleTraySettingsRequest(Sender: TObject);
begin
  HandleSettingsClick(Sender);
end;

procedure TFormMain.HandleTrayExitRequest(Sender: TObject);
begin
  FPresenter.OnExitRequested;
end;

procedure TFormMain.HandleApplicationDeactivate(Sender: TObject);
begin
  if (Bootstrap.GeneralConfig.WindowMode = wmMenu) and Bootstrap.WindowConfig.MenuHideOnFocusLoss and Visible then
  begin
    Log('HandleApplicationDeactivate: Hiding to tray', ClassName);
    HideView;
  end;
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  if (Bootstrap.GeneralConfig.WindowMode = wmMenu) and Bootstrap.WindowConfig.MenuHideOnFocusLoss then
  begin
    Log('FormDeactivate: Hiding to tray', ClassName);
    HideView;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not FForceClose) and (not FPresenter.CanClose) then
  begin
    Log('FormCloseQuery: Hiding to tray instead of closing', ClassName);
    CanClose := False;
    HideView;
  end
  else
  begin
    Log('FormCloseQuery: Allowing close', ClassName);
    CanClose := True;
  end;
end;

{ IMainView implementation }

procedure TFormMain.ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
begin
  FDeviceList.SetDisplayItems(AItems);
end;

procedure TFormMain.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
begin
  FDeviceList.UpdateDisplayItem(AItem);
end;

procedure TFormMain.ClearDevices;
begin
  FDeviceList.Clear;
end;

procedure TFormMain.SetToggleState(AEnabled: Boolean);
begin
  if AEnabled then
    BluetoothToggle.State := tssOn
  else
    BluetoothToggle.State := tssOff;
end;

procedure TFormMain.SetToggleEnabled(AEnabled: Boolean);
begin
  BluetoothToggle.Enabled := AEnabled;
end;

procedure TFormMain.ShowStatus(const AMessage: string);
begin
  StatusLabel.Caption := AMessage;
end;

procedure TFormMain.ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);
var
  BalloonFlags: TBalloonFlags;
begin
  case AFlags of
    nfInfo: BalloonFlags := bfInfo;
    nfWarning: BalloonFlags := bfWarning;
    nfError: BalloonFlags := bfError;
  else
    BalloonFlags := bfNone;
  end;
  FTrayManager.ShowNotification(ATitle, AMessage, BalloonFlags);
end;

procedure TFormMain.SetBusy(ABusy: Boolean);
begin
  if ABusy then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

function TFormMain.IsVisible: Boolean;
begin
  Result := Visible;
end;

function TFormMain.IsMinimized: Boolean;
begin
  Result := WindowState = wsMinimized;
end;

function TFormMain.GetWindowHandle: HWND;
begin
  Result := Handle;
end;

procedure TFormMain.ShowView;
begin
  Log('ShowView', ClassName);

  // Position window based on PositionMode
  // In Menu mode or for modes 1-3 (tray/cursor/center), always reposition
  // In Window mode with mode 0 (coordinates), use saved position
  if (Bootstrap.GeneralConfig.WindowMode = wmMenu) or (Bootstrap.PositionConfig.PositionMode <> pmCoordinates) then
    ApplyWindowPosition;

  Show;
  WindowState := wsNormal;

  Application.BringToFront;
  SetForegroundWindow(Handle);
  BringToFront;

  // Set focus to device list for immediate keyboard navigation
  if (FDeviceList <> nil) and FDeviceList.CanFocus then
    FDeviceList.SetFocus
  else if CanFocus then
    SetFocus;

  FTrayManager.UpdateMenuCaption(True);
end;

procedure TFormMain.HideView;
begin
  Log('HideView', ClassName);
  Hide;
  FTrayManager.UpdateMenuCaption(False);
end;

procedure TFormMain.DoForceClose;
begin
  Log('DoForceClose', ClassName);
  FForceClose := True;
  Close;
end;

{ Windows message handlers }

procedure TFormMain.WMHotkey(var Msg: TMessage);
begin
  FHotkeyManager.HandleWMHotkey(Msg.WParam);
end;

procedure TFormMain.WMHotkeyDetected(var Msg: TMessage);
begin
  FHotkeyManager.HandleHotkeyDetected;
end;

procedure TFormMain.WMDpiChanged(var Msg: TMessage);
begin
  inherited;

  if (Bootstrap.GeneralConfig.WindowMode = wmMenu) and Visible then
  begin
    Log('WMDpiChanged: Repositioning window', ClassName);
    ApplyWindowPosition;
  end;
end;

procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  if (Msg.CmdType and $FFF0) = SC_MINIMIZE then
  begin
    if Bootstrap.WindowConfig.MinimizeToTray then
    begin
      Log('WMSysCommand: Minimizing to tray', ClassName);
      HideView;
      Msg.Result := 0;
      Exit;
    end;
  end;

  inherited;
end;

end.
