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
  App.MainPresenter;

const
  WM_DPICHANGED = $02E0;

type
  /// <summary>
  /// Main application form displaying Bluetooth devices.
  /// Implements IMainView for MVP pattern.
  /// </summary>
  TFormMain = class(TForm, IMainView)
    HeaderPanel: TPanel;
    TitleLabel: TLabel;
    BluetoothToggle: TToggleSwitch;
    StatusPanel: TPanel;
    StatusLabel: TLabel;
    SettingsLink: TLabel;
    DevicesPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleBluetoothToggle(Sender: TObject);
    procedure HandleSettingsClick(Sender: TObject);
    procedure HandleRefreshClick(Sender: TObject);
    procedure TitleLabelClick(Sender: TObject);
    procedure StatusLabelClick(Sender: TObject);
  private
    FPresenter: TMainPresenter;
    FDeviceList: TDeviceListBox;
    FTrayManager: TTrayManager;
    FHotkeyManager: THotkeyManager;
    FForceClose: Boolean;

    { View setup }
    procedure CreateDeviceList;
    procedure ApplyTheme;
    procedure ApplyConfiguredTheme;
    procedure ApplyWindowMode;
    procedure ApplyMenuModeTaskbarHide;
    procedure PositionMenuPopup;

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleThemeChanged(Sender: TObject);
    procedure HandleHotkeyTriggered(Sender: TObject);
    procedure HandleTrayToggleVisibility(Sender: TObject);
    procedure HandleTrayExitRequest(Sender: TObject);
    procedure HandleApplicationDeactivate(Sender: TObject);

    { IMainView implementation }
    procedure ShowDevices(const ADevices: TBluetoothDeviceInfoArray);
    procedure UpdateDevice(const ADevice: TBluetoothDeviceInfo);
    procedure AddDevice(const ADevice: TBluetoothDeviceInfo);
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
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes,
  ShellAPI,
  App.Logger,
  App.Config,
  UI.WindowPositioner,
  SettingsForm;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Load configuration early
  Config;

  Log('[MainForm] FormCreate: Starting');

  FForceClose := False;

  // Apply window mode (must be done early before window handle is created)
  ApplyWindowMode;

  // Restore window position/size from configuration
  if Config.WindowMode = wmMenu then
  begin
    Position := poDesigned;
    Left := -10000;
    Top := -10000;
    Log('[MainForm] FormCreate: Menu mode, position will be set on show');
  end
  else if (Config.WindowX >= 0) and (Config.WindowY >= 0) then
  begin
    Position := poDesigned;
    Left := Config.WindowX;
    Top := Config.WindowY;
    Log('[MainForm] FormCreate: Restored position X=%d, Y=%d', [Left, Top]);
  end;

  // Restore window size (only in Window mode)
  if (Config.WindowMode = wmWindow) and (Config.WindowWidth > 0) and (Config.WindowHeight > 0) then
  begin
    Width := Config.WindowWidth;
    Height := Config.WindowHeight;
    Log('[MainForm] FormCreate: Restored size W=%d, H=%d', [Width, Height]);
  end;

  // Apply StayOnTop setting
  if Config.StayOnTop or (Config.WindowMode = wmMenu) then
  begin
    FormStyle := fsStayOnTop;
    Log('[MainForm] FormCreate: StayOnTop enabled');
  end;

  // Load external VCL styles
  Theme.LoadStylesFromDirectory(Config.VsfDir);
  ApplyConfiguredTheme;

  // Subscribe to theme changes
  Theme.OnThemeChanged := HandleThemeChanged;

  // Subscribe to application deactivation
  Application.OnDeactivate := HandleApplicationDeactivate;

  // Create UI components
  CreateDeviceList;

  // Create tray manager
  FTrayManager := TTrayManager.Create(Self);
  FTrayManager.OnToggleVisibility := HandleTrayToggleVisibility;
  FTrayManager.OnExitRequest := HandleTrayExitRequest;

  // Apply configuration to device list
  FDeviceList.ShowAddresses := Config.ShowAddresses;

  // Apply theme
  ApplyTheme;

  // Create and initialize presenter
  FPresenter := TMainPresenter.Create(Self);
  FPresenter.Initialize;

  // Create and register global hotkey
  FHotkeyManager := THotkeyManager.Create;
  FHotkeyManager.OnHotkeyTriggered := HandleHotkeyTriggered;
  FHotkeyManager.Register(Handle, Config.Hotkey, Config.UseLowLevelHook);

  // Hide from taskbar in Menu mode
  ApplyMenuModeTaskbarHide;

  // In Menu mode, start hidden
  if Config.WindowMode = wmMenu then
  begin
    Log('[MainForm] FormCreate: Menu mode, starting hidden');
    Application.ShowMainForm := False;
    Visible := False;
  end;

  Log('[MainForm] FormCreate: Complete');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Save window position (only in Window mode)
  if (Config.WindowMode = wmWindow) and (WindowState = wsNormal) then
  begin
    Config.WindowX := Left;
    Config.WindowY := Top;
    Config.WindowWidth := Width;
    Config.WindowHeight := Height;
    Log('[MainForm] FormDestroy: Saved position');
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

  Theme.OnThemeChanged := nil;
  Application.OnDeactivate := nil;

  Log('[MainForm] FormDestroy: Complete');
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
var
  Colors: TThemeColors;
begin
  Colors := Theme.Colors;

  Color := Colors.Background;
  HeaderPanel.Color := Colors.Background;
  TitleLabel.Font.Color := Colors.TextPrimary;
  DevicesPanel.Color := Colors.Background;
  StatusPanel.Color := Colors.Background;
  StatusLabel.Font.Color := Colors.TextSecondary;
  SettingsLink.Font.Color := Colors.Accent;

  FDeviceList.Invalidate;
end;

procedure TFormMain.ApplyConfiguredTheme;
var
  ThemeSetting: string;
begin
  ThemeSetting := Config.Theme;
  Log('[MainForm] ApplyConfiguredTheme: Theme="%s"', [ThemeSetting]);

  if SameText(ThemeSetting, 'System') then
    Theme.SetThemeMode(tmSystem)
  else if SameText(ThemeSetting, 'Light') then
    Theme.SetThemeMode(tmLight)
  else if SameText(ThemeSetting, 'Dark') then
    Theme.SetThemeMode(tmDark)
  else
    Theme.SetStyle(ThemeSetting);
end;

procedure TFormMain.ApplyWindowMode;
begin
  if Config.WindowMode = wmMenu then
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    Log('[MainForm] ApplyWindowMode: Menu mode');
  end
  else
  begin
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    Log('[MainForm] ApplyWindowMode: Window mode');
  end;
end;

procedure TFormMain.ApplyMenuModeTaskbarHide;
var
  ExStyle: LONG_PTR;
begin
  if Config.WindowMode = wmMenu then
  begin
    ExStyle := GetWindowLongPtr(Handle, GWL_EXSTYLE);
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    ExStyle := ExStyle and (not WS_EX_APPWINDOW);
    SetWindowLongPtr(Handle, GWL_EXSTYLE, ExStyle);
    Log('[MainForm] ApplyMenuModeTaskbarHide: Hidden from taskbar');
  end;
end;

procedure TFormMain.PositionMenuPopup;
begin
  TWindowPositioner.PositionMenuPopup(Self, Config.MenuPosition);
end;

{ Event handlers }

procedure TFormMain.HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
begin
  FPresenter.OnDeviceClicked(ADevice);
end;

procedure TFormMain.HandleThemeChanged(Sender: TObject);
begin
  ApplyTheme;
end;

procedure TFormMain.HandleBluetoothToggle(Sender: TObject);
begin
  if not FPresenter.IsUpdatingToggle then
    FPresenter.OnToggleChanged(BluetoothToggle.State = tssOn);
end;

procedure TFormMain.HandleSettingsClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'ms-settings:bluetooth', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.HandleRefreshClick(Sender: TObject);
begin
  FPresenter.OnRefreshRequested;
end;

procedure TFormMain.TitleLabelClick(Sender: TObject);
begin
  FPresenter.OnRefreshRequested;
end;

procedure TFormMain.StatusLabelClick(Sender: TObject);
var
  SettingsDialog: TFormSettings;
begin
  Log('[MainForm] StatusLabelClick: Opening settings dialog');
  SettingsDialog := TFormSettings.Create(Self);
  try
    SettingsDialog.ShowModal;
  finally
    SettingsDialog.Free;
  end;
end;

procedure TFormMain.HandleHotkeyTriggered(Sender: TObject);
begin
  FPresenter.OnVisibilityToggleRequested;
end;

procedure TFormMain.HandleTrayToggleVisibility(Sender: TObject);
begin
  FPresenter.OnVisibilityToggleRequested;
end;

procedure TFormMain.HandleTrayExitRequest(Sender: TObject);
begin
  FPresenter.OnExitRequested;
end;

procedure TFormMain.HandleApplicationDeactivate(Sender: TObject);
begin
  if (Config.WindowMode = wmMenu) and Config.MenuHideOnFocusLoss and Visible then
  begin
    Log('[MainForm] HandleApplicationDeactivate: Hiding to tray');
    HideView;
  end;
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  if (Config.WindowMode = wmMenu) and Config.MenuHideOnFocusLoss then
  begin
    Log('[MainForm] FormDeactivate: Hiding to tray');
    HideView;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not FForceClose) and (not FPresenter.CanClose) then
  begin
    Log('[MainForm] FormCloseQuery: Hiding to tray instead of closing');
    CanClose := False;
    HideView;
  end
  else
  begin
    Log('[MainForm] FormCloseQuery: Allowing close');
    CanClose := True;
  end;
end;

{ IMainView implementation }

procedure TFormMain.ShowDevices(const ADevices: TBluetoothDeviceInfoArray);
begin
  FDeviceList.SetDevices(ADevices);
end;

procedure TFormMain.UpdateDevice(const ADevice: TBluetoothDeviceInfo);
begin
  FDeviceList.UpdateDevice(ADevice);
end;

procedure TFormMain.AddDevice(const ADevice: TBluetoothDeviceInfo);
begin
  FDeviceList.AddDevice(ADevice);
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
  Log('[MainForm] ShowView');

  if Config.WindowMode = wmMenu then
    PositionMenuPopup;

  Show;
  WindowState := wsNormal;

  Application.BringToFront;
  SetForegroundWindow(Handle);
  BringToFront;
  if CanFocus then
    SetFocus;

  FTrayManager.UpdateMenuCaption(True);
end;

procedure TFormMain.HideView;
begin
  Log('[MainForm] HideView');
  Hide;
  FTrayManager.UpdateMenuCaption(False);
end;

procedure TFormMain.DoForceClose;
begin
  Log('[MainForm] DoForceClose');
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

  if (Config.WindowMode = wmMenu) and Visible then
  begin
    Log('[MainForm] WMDpiChanged: Repositioning popup');
    PositionMenuPopup;
  end;
end;

procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  if (Msg.CmdType and $FFF0) = SC_MINIMIZE then
  begin
    if Config.MinimizeToTray then
    begin
      Log('[MainForm] WMSysCommand: Minimizing to tray');
      HideView;
      Msg.Result := 0;
      Exit;
    end;
  end;

  inherited;
end;

end.
