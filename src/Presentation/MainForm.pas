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
  Vcl.Buttons,
  System.ImageList,
  Vcl.ImgList,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionStrategies,
  App.ConfigInterfaces,
  App.MainViewInterfaces,
  App.MainPresenter,
  UI.Theme,
  UI.DeviceList,
  UI.TrayManager,
  UI.HotkeyManager;

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
  /// Implements ISP-compliant view interfaces for MVP pattern.
  /// </summary>
  TFormMain = class(TForm, IDeviceListView, IToggleView, IStatusView, IVisibilityView)
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

    { Injected dependencies (set via Setup method) }
    FAppConfig: IAppConfig;
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FPositionConfig: IPositionConfig;
    FHotkeyConfig: IHotkeyConfig;
    FAppearanceConfig: IAppearanceConfig;
    FLayoutConfig: ILayoutConfig;
    FPollingConfig: IPollingConfig;
    FConnectionConfig: IConnectionConfig;
    FStrategyFactory: IConnectionStrategyFactory;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FThemeManager: IThemeManager;

    { View setup }
    procedure CreateDeviceList;
    procedure ApplyTheme;
    procedure ApplyWindowMode;
    procedure ApplyMenuModeTaskbarHide;
    procedure ApplyWindowPosition;
    procedure ApplyWindowSize;
    procedure CalculateAutoSize(out AWidth, AHeight: Integer);

    { Initialization helpers (extracted from FormCreate for SRP) }
    procedure InitializeWindowSettings;
    procedure InitializeUIComponents;
    procedure InitializePresenter;
    procedure InitializeHotkey;
    procedure FinalizeMenuMode;

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleHotkeyTriggered(Sender: TObject);
    procedure HandleTrayToggleVisibility(Sender: TObject);
    procedure HandleTraySettingsRequest(Sender: TObject);
    procedure HandleTrayExitRequest(Sender: TObject);
    procedure HandleApplicationDeactivate(Sender: TObject);
    procedure HandleSettingsApplied(Sender: TObject);

    { View interfaces implementation }
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
    procedure IVisibilityView.ForceClose = DoForceClose;
    procedure DoForceClose;

  protected
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotkey(var Msg: TMessage); message WM_HOTKEY;
    procedure WMHotkeyDetected(var Msg: TMessage); message WM_HOTKEY_DETECTED;
    procedure WMDpiChanged(var Msg: TMessage); message WM_DPICHANGED;

  public
    { Dependency injection - must be called before FormCreate completes }
    procedure Setup(
      AAppConfig: IAppConfig;
      AGeneralConfig: IGeneralConfig;
      AWindowConfig: IWindowConfig;
      APositionConfig: IPositionConfig;
      AHotkeyConfig: IHotkeyConfig;
      AAppearanceConfig: IAppearanceConfig;
      ALayoutConfig: ILayoutConfig;
      APollingConfig: IPollingConfig;
      AConnectionConfig: IConnectionConfig;
      AStrategyFactory: IConnectionStrategyFactory;
      ADeviceConfigProvider: IDeviceConfigProvider;
      AThemeManager: IThemeManager
    );

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
  App.ConfigEnums,
  App.Bootstrap,
  App.SettingsPresenter,
  UI.WindowPositioner,
  SettingsForm;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.Setup(
  AAppConfig: IAppConfig;
  AGeneralConfig: IGeneralConfig;
  AWindowConfig: IWindowConfig;
  APositionConfig: IPositionConfig;
  AHotkeyConfig: IHotkeyConfig;
  AAppearanceConfig: IAppearanceConfig;
  ALayoutConfig: ILayoutConfig;
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  AStrategyFactory: IConnectionStrategyFactory;
  ADeviceConfigProvider: IDeviceConfigProvider;
  AThemeManager: IThemeManager);
begin
  FAppConfig := AAppConfig;
  FGeneralConfig := AGeneralConfig;
  FWindowConfig := AWindowConfig;
  FPositionConfig := APositionConfig;
  FHotkeyConfig := AHotkeyConfig;
  FAppearanceConfig := AAppearanceConfig;
  FLayoutConfig := ALayoutConfig;
  FPollingConfig := APollingConfig;
  FConnectionConfig := AConnectionConfig;
  FStrategyFactory := AStrategyFactory;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FThemeManager := AThemeManager;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Initialize dependencies from Bootstrap (composition root)
  // This centralizes Bootstrap access to FormCreate only
  Setup(
    Bootstrap.AppConfig,
    Bootstrap.GeneralConfig,
    Bootstrap.WindowConfig,
    Bootstrap.PositionConfig,
    Bootstrap.HotkeyConfig,
    Bootstrap.AppearanceConfig,
    Bootstrap.LayoutConfig,
    Bootstrap.PollingConfig,
    Bootstrap.ConnectionConfig,
    Bootstrap.ConnectionStrategyFactory,
    Bootstrap.DeviceConfigProvider,
    Bootstrap.ThemeManager
  );

  Log('FormCreate: Starting', ClassName);

  FForceClose := False;

  // Initialize form using focused helper methods (SRP)
  InitializeWindowSettings;
  InitializeUIComponents;
  InitializePresenter;
  InitializeHotkey;
  FinalizeMenuMode;

  Log('FormCreate: Complete', ClassName);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Save window position and size (always save, applies when PositionMode=0)
  if WindowState = wsNormal then
  begin
    FPositionConfig.PositionX := Left;
    FPositionConfig.PositionY := Top;
    FPositionConfig.PositionW := Width;
    FPositionConfig.PositionH := Height;
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
  // Inject configuration dependencies (eliminates Bootstrap fallback)
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
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
  if FGeneralConfig.WindowMode = wmMenu then
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
  if FGeneralConfig.WindowMode = wmMenu then
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
  TWindowPositioner.PositionWindow(Self, FPositionConfig, FPositionConfig.PositionMode);
end;

procedure TFormMain.ApplyWindowSize;
var
  NewWidth, NewHeight: Integer;
begin
  // Check if auto-sizing is requested (-1 means auto)
  if (FPositionConfig.PositionW < 0) or (FPositionConfig.PositionH < 0) then
  begin
    CalculateAutoSize(NewWidth, NewHeight);
    if FPositionConfig.PositionW < 0 then
      Width := NewWidth
    else
      Width := FPositionConfig.PositionW;
    if FPositionConfig.PositionH < 0 then
      Height := NewHeight
    else
      Height := FPositionConfig.PositionH;
    Log('ApplyWindowSize: Auto-calculated W=%d, H=%d', [Width, Height], ClassName);
  end
  else if (FPositionConfig.PositionW > 0) and (FPositionConfig.PositionH > 0) then
  begin
    Width := FPositionConfig.PositionW;
    Height := FPositionConfig.PositionH;
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

{ Initialization helpers (extracted from FormCreate for SRP) }

procedure TFormMain.InitializeWindowSettings;
begin
  // Apply window mode (must be done early before window handle is created)
  ApplyWindowMode;

  // Apply window size first (may be auto-calculated)
  ApplyWindowSize;

  // Apply window position (depends on size being set)
  if FGeneralConfig.WindowMode = wmMenu then
  begin
    // Menu mode: start hidden off-screen, position will be set on show
    Position := poDesigned;
    Left := -10000;
    Top := -10000;
    Log('InitializeWindowSettings: Menu mode, position will be set on show', ClassName);
  end
  else
  begin
    // Window mode: apply position based on PositionMode
    Position := poDesigned;
    ApplyWindowPosition;
  end;

  // Apply OnTop setting
  if FGeneralConfig.OnTop or (FGeneralConfig.WindowMode = wmMenu) then
  begin
    FormStyle := fsStayOnTop;
    Log('InitializeWindowSettings: OnTop enabled', ClassName);
  end;
end;

procedure TFormMain.InitializeUIComponents;
begin
  // Load external VCL styles and apply configured theme
  FThemeManager.LoadStylesFromDirectory(FAppearanceConfig.VsfDir);
  FThemeManager.SetStyle(FAppearanceConfig.Theme);

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
  FDeviceList.ShowAddresses := FAppearanceConfig.ShowAddresses;
end;

procedure TFormMain.InitializePresenter;
begin
  // Create and initialize presenter with injected dependencies
  // Pass Self as each focused interface (ISP-compliant)
  FPresenter := TMainPresenter.Create(
    Self as IDeviceListView,
    Self as IToggleView,
    Self as IStatusView,
    Self as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FPollingConfig,
    FConnectionConfig,
    FStrategyFactory
  );
  FPresenter.Initialize;
end;

procedure TFormMain.InitializeHotkey;
begin
  // Create and register global hotkey
  FHotkeyManager := THotkeyManager.Create;
  FHotkeyManager.OnHotkeyTriggered := HandleHotkeyTriggered;
  FHotkeyManager.Register(Handle, FHotkeyConfig.Hotkey, FHotkeyConfig.UseLowLevelHook);
end;

procedure TFormMain.FinalizeMenuMode;
begin
  // Hide from taskbar in Menu mode
  ApplyMenuModeTaskbarHide;

  // In Menu mode, start hidden
  if FGeneralConfig.WindowMode = wmMenu then
  begin
    Log('FinalizeMenuMode: Starting hidden', ClassName);
    Application.ShowMainForm := False;
    Visible := False;
  end;
end;

procedure TFormMain.ApplyAllSettings;
var
  ExStyle: LONG_PTR;
begin
  Log('ApplyAllSettings: Applying configuration changes', ClassName);

  // Re-register hotkey (unregister first, then register with new settings)
  FHotkeyManager.Unregister;
  FHotkeyManager.Register(Handle, FHotkeyConfig.Hotkey, FHotkeyConfig.UseLowLevelHook);
  Log('ApplyAllSettings: Hotkey re-registered: %s', [FHotkeyConfig.Hotkey], ClassName);

  // Reload styles from directory (loads any new styles, skips already loaded)
  FThemeManager.LoadStylesFromDirectory(FAppearanceConfig.VsfDir);

  // Apply theme
  FThemeManager.SetStyle(FAppearanceConfig.Theme);
  ApplyTheme;
  Log('ApplyAllSettings: Theme applied: %s', [FAppearanceConfig.Theme], ClassName);

  // Apply window mode (border style and icons)
  if FGeneralConfig.WindowMode = wmMenu then
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
  if FGeneralConfig.WindowMode = wmMenu then
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
  if FGeneralConfig.OnTop or (FGeneralConfig.WindowMode = wmMenu) then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
  Log('ApplyAllSettings: OnTop=%s', [BoolToStr(FGeneralConfig.OnTop, True)], ClassName);

  // Apply ShowAddresses to device list
  if FDeviceList <> nil then
  begin
    FDeviceList.ShowAddresses := FAppearanceConfig.ShowAddresses;
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
    // Inject dependencies from MainForm (not Bootstrap)
    SettingsDialog.Setup(
      FAppConfig,
      FAppConfig.AsLogConfig,
      FDeviceConfigProvider,
      FThemeManager
    );
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
  if (FGeneralConfig.WindowMode = wmMenu) and FWindowConfig.MenuHideOnFocusLoss and Visible then
  begin
    Log('HandleApplicationDeactivate: Hiding to tray', ClassName);
    HideView;
  end;
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  if (FGeneralConfig.WindowMode = wmMenu) and FWindowConfig.MenuHideOnFocusLoss then
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

{ View interfaces implementation }

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
  if (FGeneralConfig.WindowMode = wmMenu) or (FPositionConfig.PositionMode <> pmCoordinates) then
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

  if (FGeneralConfig.WindowMode = wmMenu) and Visible then
  begin
    Log('WMDpiChanged: Repositioning window', ClassName);
    ApplyWindowPosition;
  end;
end;

procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  if (Msg.CmdType and $FFF0) = SC_MINIMIZE then
  begin
    if FWindowConfig.MinimizeToTray then
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
