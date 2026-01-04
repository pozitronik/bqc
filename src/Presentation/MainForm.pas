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
  Vcl.ComCtrls,
  System.ImageList,
  Vcl.ImgList,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionStrategies,
  Bluetooth.RadioControl,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.ConnectionConfigIntf,
  App.BatteryTrayConfigIntf,
  App.MainViewInterfaces,
  App.MainPresenter,
  App.AsyncExecutor,
  App.DeviceDisplayTypes,
  UI.Theme,
  UI.DeviceList,
  UI.TrayManager,
  UI.HotkeyManager,
  UI.BatteryTrayManager;

const
  WM_DPICHANGED = $02E0;
  WM_FOREGROUND_LOST = WM_USER + 200;  // Custom message for foreground loss detection

  // Windows Settings URI for Bluetooth
  WINDOWS_BLUETOOTH_SETTINGS_URI = 'ms-settings:bluetooth';

  // WinEvent constants (not defined in Winapi.Windows)
  EVENT_SYSTEM_FOREGROUND = $0003;
  WINEVENT_OUTOFCONTEXT = $0000;
  WINEVENT_SKIPOWNPROCESS = $0002;

  // Window size constraints for auto-sizing
  WINDOW_MIN_WIDTH = 280;
  WINDOW_MAX_WIDTH = 800;
  WINDOW_MIN_HEIGHT = 300;
  WINDOW_MAX_HEIGHT = 1200;
  WINDOW_DEFAULT_WIDTH = 320;
  WINDOW_DEFAULT_HEIGHT = 400;

type
  HWINEVENTHOOK = THandle;

  TWinEventProc = procedure(hWinEventHook: HWINEVENTHOOK; event: DWORD;
    hwnd: HWND; idObject, idChild: Longint; idEventThread, dwmsEventTime: DWORD); stdcall;

function SetWinEventHook(eventMin, eventMax: DWORD; hmodWinEventProc: HMODULE;
  pfnWinEventProc: TWinEventProc; idProcess, idThread: DWORD;
  dwFlags: DWORD): HWINEVENTHOOK; stdcall; external user32;
function UnhookWinEvent(hWinEventHook: HWINEVENTHOOK): BOOL; stdcall; external user32;

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
    FBatteryTrayManager: TBatteryTrayManager;
    FHotkeyManager: THotkeyManager;
    FCastPanelHotkeyManager: THotkeyManager;
    FBluetoothPanelHotkeyManager: THotkeyManager;
    FForceClose: Boolean;
    FForegroundHook: HWINEVENTHOOK;

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
    FBatteryTrayConfig: IBatteryTrayConfig;
    FStrategyFactory: IConnectionStrategyFactory;
    FRadioStateManager: IRadioStateManager;
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
    procedure HandleActionClick(Sender: TObject; const AItem: TDeviceDisplayItem);
    procedure HandleHotkeyTriggered(Sender: TObject);
    procedure HandleCastPanelHotkeyTriggered(Sender: TObject);
    procedure HandleBluetoothPanelHotkeyTriggered(Sender: TObject);
    procedure HandleTrayToggleVisibility(Sender: TObject);
    procedure HandleTraySettingsRequest(Sender: TObject);
    procedure HandleTrayExitRequest(Sender: TObject);
    procedure HandleApplicationDeactivate(Sender: TObject);
    procedure HandleSettingsApplied(Sender: TObject);
    procedure HandleBatteryNotification(Sender: TObject; AAddress: UInt64;
      const ADeviceName: string; ALevel: Integer; AIsLowBattery: Boolean);

    { View interfaces implementation }
    procedure ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);
    procedure ClearDevices;
    procedure SetToggleState(AEnabled: Boolean);
    procedure SetToggleEnabled(AEnabled: Boolean);
    procedure ShowStatus(const AMessage: string);
    procedure ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);
    procedure SetBusy(ABusy: Boolean);
    procedure SetScanning(AScanning: Boolean);
    function IsVisible: Boolean;
    function IsMinimized: Boolean;
    function GetWindowHandle: HWND;
    procedure ShowView;
    procedure HideView;
    procedure IVisibilityView.ForceClose = DoForceClose;
    procedure DoForceClose;

    { Settings application helpers (extracted from ApplyAllSettings for SRP) }
    procedure ApplyHotkeySettings;
    procedure ApplyThemeSettings;
    procedure ApplyWindowModeSettings;
    procedure ApplyTaskbarVisibility;
    procedure ApplyOnTopSetting;
    procedure ApplyDeviceListSettings;
    procedure NotifyPresenterOfChanges;

    { Foreground tracking for menu mode (more reliable than WM_ACTIVATE for tool windows) }
    procedure InstallForegroundHook;
    procedure UninstallForegroundHook;

  protected
    procedure WMForegroundLost(var Msg: TMessage); message WM_FOREGROUND_LOST;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotkey(var Msg: TMessage); message WM_HOTKEY;
    procedure WMHotkeyDetected(var Msg: TMessage); message WM_HOTKEY_DETECTED;
    procedure WMDpiChanged(var Msg: TMessage); message WM_DPICHANGED;
    procedure WMDisplayChange(var Msg: TMessage); message WM_DISPLAYCHANGE;

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
      ABatteryTrayConfig: IBatteryTrayConfig;
      AStrategyFactory: IConnectionStrategyFactory;
      ARadioStateManager: IRadioStateManager;
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
  Bluetooth.Service,
  Bluetooth.ProfileQuery,
  App.Logger,
  App.Bootstrap,
  App.SettingsPresenter,
  UI.WindowPositioner,
  UI.DeviceDisplayItemBuilder,
  SettingsForm;

{$R *.dfm}

var
  /// <summary>
  /// Global reference to main form for foreground hook callback.
  /// Required because WinEventProc is a standalone procedure, not a method.
  /// </summary>
  GMainFormInstance: TFormMain = nil;

/// <summary>
/// WinEvent callback for EVENT_SYSTEM_FOREGROUND.
/// Called when any window in the system becomes the foreground window.
/// Posts a message to the main form to handle focus loss on the main thread.
/// </summary>
procedure ForegroundEventProc(hWinEventHook: HWINEVENTHOOK; event: DWORD;
  hwnd: HWND; idObject, idChild: Longint; idEventThread, dwmsEventTime: DWORD); stdcall;
var
  WindowProcessId: DWORD;
begin
  // Log every callback invocation for debugging
  LogDebug('ForegroundEventProc: Called, hwnd=$%x, GMainFormInstance=%p', [
    hwnd, Pointer(GMainFormInstance)
  ], 'ForegroundEventProc');

  // Only process if we have a valid form reference and the foreground changed to another window
  if (GMainFormInstance <> nil) and (hwnd <> 0) and (hwnd <> GMainFormInstance.Handle) then
  begin
    // Check if the new foreground window belongs to our process (e.g., Settings dialog)
    // If so, don't hide - we want to stay visible when our own dialogs are shown
    GetWindowThreadProcessId(hwnd, @WindowProcessId);
    LogDebug('ForegroundEventProc: WindowProcessId=%d, CurrentProcessId=%d', [
      WindowProcessId, GetCurrentProcessId
    ], 'ForegroundEventProc');

    if WindowProcessId <> GetCurrentProcessId then
    begin
      // Another process became foreground - post message to hide menu
      LogDebug('ForegroundEventProc: Posting WM_FOREGROUND_LOST', 'ForegroundEventProc');
      PostMessage(GMainFormInstance.Handle, WM_FOREGROUND_LOST, 0, hwnd);
    end
    else
    begin
      LogDebug('ForegroundEventProc: Skipped - same process (our dialog)', 'ForegroundEventProc');
    end;
  end
  else
  begin
    if GMainFormInstance <> nil then
      LogDebug('ForegroundEventProc: Skipped - hwnd=$%x is our handle=$%x', [
        hwnd, GMainFormInstance.Handle
      ], 'ForegroundEventProc')
    else
      LogDebug('ForegroundEventProc: Skipped - GMainFormInstance is nil', 'ForegroundEventProc');
  end;
end;

/// <summary>
/// Checks if the mouse cursor is currently over the taskbar notification area (system tray).
/// Used to detect when user is clicking a tray icon to prevent focus-loss hiding
/// that would interfere with tray icon toggle behavior.
/// </summary>
function IsCursorOverNotificationArea: Boolean;
var
  CursorPos: TPoint;
  TaskbarWnd, TrayWnd, NotifyWnd: HWND;
  TrayRect: TRect;
begin
  Result := False;

  if not GetCursorPos(CursorPos) then
  begin
    LogDebug('IsCursorOverNotificationArea: GetCursorPos failed', 'IsCursorOverNotificationArea');
    Exit;
  end;

  // Find the main taskbar window
  TaskbarWnd := FindWindow('Shell_TrayWnd', nil);
  if TaskbarWnd = 0 then
  begin
    LogDebug('IsCursorOverNotificationArea: Shell_TrayWnd not found', 'IsCursorOverNotificationArea');
    Exit;
  end;

  // Find the notification area container (TrayNotifyWnd)
  TrayWnd := FindWindowEx(TaskbarWnd, 0, 'TrayNotifyWnd', nil);
  if TrayWnd = 0 then
  begin
    LogDebug('IsCursorOverNotificationArea: TrayNotifyWnd not found', 'IsCursorOverNotificationArea');
    Exit;
  end;

  // Get the notification area bounds
  if GetWindowRect(TrayWnd, TrayRect) then
  begin
    Result := PtInRect(TrayRect, CursorPos);
    LogDebug('IsCursorOverNotificationArea: TrayRect=(%d,%d,%d,%d), Cursor=(%d,%d), InRect=%s', [
      TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom,
      CursorPos.X, CursorPos.Y, BoolToStr(Result, True)
    ], 'IsCursorOverNotificationArea');
  end;

  // Also check the overflow notification area (system tray overflow window)
  if not Result then
  begin
    NotifyWnd := FindWindow('NotifyIconOverflowWindow', nil);
    if (NotifyWnd <> 0) and IsWindowVisible(NotifyWnd) then
    begin
      if GetWindowRect(NotifyWnd, TrayRect) then
      begin
        Result := PtInRect(TrayRect, CursorPos);
        LogDebug('IsCursorOverNotificationArea: OverflowRect=(%d,%d,%d,%d), Cursor=(%d,%d), InRect=%s', [
          TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom,
          CursorPos.X, CursorPos.Y, BoolToStr(Result, True)
        ], 'IsCursorOverNotificationArea');
      end;
    end;
  end;
end;

/// <summary>
/// Forces the specified window to the foreground, bypassing Windows restrictions.
/// Windows restricts SetForegroundWindow for background processes. This function
/// temporarily attaches to the foreground thread's input queue, allowing the
/// foreground window change to succeed.
/// </summary>
procedure ForceForegroundWindow(AHandle: HWND);
var
  ForegroundThread, CurrentThread: DWORD;
  ForegroundWnd: HWND;
  SetFgResult: Boolean;
begin
  ForegroundWnd := GetForegroundWindow;
  ForegroundThread := GetWindowThreadProcessId(ForegroundWnd, nil);
  CurrentThread := GetCurrentThreadId;

  LogDebug('ForceForegroundWindow: ForegroundWnd=$%x, ForegroundThread=%d, CurrentThread=%d, SameThread=%s', [
    ForegroundWnd, ForegroundThread, CurrentThread, BoolToStr(ForegroundThread = CurrentThread, True)
  ], 'ForceForegroundWindow');

  if ForegroundThread <> CurrentThread then
  begin
    // Attach to the foreground thread to allow SetForegroundWindow
    AttachThreadInput(CurrentThread, ForegroundThread, True);
    try
      SetFgResult := SetForegroundWindow(AHandle);
      BringWindowToTop(AHandle);
      LogDebug('ForceForegroundWindow: AttachThreadInput path, SetForegroundWindow=%s', [
        BoolToStr(SetFgResult, True)
      ], 'ForceForegroundWindow');
    finally
      AttachThreadInput(CurrentThread, ForegroundThread, False);
    end;
  end
  else
  begin
    SetFgResult := SetForegroundWindow(AHandle);
    BringWindowToTop(AHandle);
    LogDebug('ForceForegroundWindow: SameThread path, SetForegroundWindow=%s', [
      BoolToStr(SetFgResult, True)
    ], 'ForceForegroundWindow');
  end;
end;

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
  ABatteryTrayConfig: IBatteryTrayConfig;
  AStrategyFactory: IConnectionStrategyFactory;
  ARadioStateManager: IRadioStateManager;
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
  FBatteryTrayConfig := ABatteryTrayConfig;
  FStrategyFactory := AStrategyFactory;
  FRadioStateManager := ARadioStateManager;
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
    Bootstrap.BatteryTrayConfig,
    Bootstrap.ConnectionStrategyFactory,
    Bootstrap.RadioStateManager,
    Bootstrap.DeviceConfigProvider,
    Bootstrap.ThemeManager
  );

  LogDebug('FormCreate: Starting', ClassName);

  FForceClose := False;
  FForegroundHook := 0;

  // Initialize form using focused helper methods (SRP)
  InitializeWindowSettings;
  InitializeUIComponents;
  InitializePresenter;
  InitializeHotkey;
  FinalizeMenuMode;

  LogDebug('FormCreate: Complete', ClassName);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Uninstall foreground hook if still active
  UninstallForegroundHook;

  // Save window position and size (always save, applies when PositionMode=0)
  if WindowState = wsNormal then
  begin
    FPositionConfig.PositionX := Left;
    FPositionConfig.PositionY := Top;
    FPositionConfig.PositionW := Width;
    FPositionConfig.PositionH := Height;
    LogDebug('FormDestroy: Saved position X=%d, Y=%d, W=%d, H=%d',
      [Left, Top, Width, Height], ClassName);
  end;

  // Shutdown presenter
  if FPresenter <> nil then
  begin
    FPresenter.Shutdown;
    FPresenter.Free;
    FPresenter := nil;
  end;

  // Free battery tray manager
  if FBatteryTrayManager <> nil then
  begin
    FBatteryTrayManager.ClearAll;
    FBatteryTrayManager.Free;
    FBatteryTrayManager := nil;
  end;

  // Free hotkey managers
  if Assigned(FHotkeyManager) then
  begin
    FHotkeyManager.Free;
    FHotkeyManager := nil;
  end;
  if Assigned(FCastPanelHotkeyManager) then
  begin
    FCastPanelHotkeyManager.Free;
    FCastPanelHotkeyManager := nil;
  end;
  if Assigned(FBluetoothPanelHotkeyManager) then
  begin
    FBluetoothPanelHotkeyManager.Free;
    FBluetoothPanelHotkeyManager := nil;
  end;

  Application.OnDeactivate := nil;

  LogDebug('FormDestroy: Complete', ClassName);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      // Only close on Escape in menu mode
      if FGeneralConfig.WindowMode = wmMenu then
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
  FDeviceList.OnActionClick := HandleActionClick;
  FDeviceList.TabOrder := 0;
  // Inject configuration dependencies (eliminates Bootstrap fallback)
  FDeviceList.LayoutConfig := FLayoutConfig;
  FDeviceList.AppearanceConfig := FAppearanceConfig;
  FDeviceList.ProfileConfig := Bootstrap.ProfileConfig;
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
    LogDebug('ApplyWindowMode: Menu mode', ClassName);
  end
  else
  begin
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    LogDebug('ApplyWindowMode: Window mode', ClassName);
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
    LogDebug('ApplyMenuModeTaskbarHide: Hidden from taskbar', ClassName);
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
    LogDebug('ApplyWindowSize: Auto-calculated W=%d, H=%d', [Width, Height], ClassName);
  end
  else if (FPositionConfig.PositionW > 0) and (FPositionConfig.PositionH > 0) then
  begin
    Width := FPositionConfig.PositionW;
    Height := FPositionConfig.PositionH;
    LogDebug('ApplyWindowSize: Restored W=%d, H=%d', [Width, Height], ClassName);
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

  LogDebug('CalculateAutoSize: Calculated W=%d, H=%d', [AWidth, AHeight], ClassName);
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
    LogDebug('InitializeWindowSettings: Menu mode, position will be set on show', ClassName);
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
    LogDebug('InitializeWindowSettings: OnTop enabled', ClassName);
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

  // Create battery tray manager for per-device battery icons
  FBatteryTrayManager := TBatteryTrayManager.Create(
    Handle,
    FBatteryTrayConfig,
    FDeviceConfigProvider
  );
  FBatteryTrayManager.OnBatteryNotification := HandleBatteryNotification;

  // Apply configuration to device list
  FDeviceList.ShowAddresses := FAppearanceConfig.ShowAddresses;
end;

procedure TFormMain.InitializePresenter;
var
  BluetoothService: IBluetoothService;
  DisplayItemBuilder: IDeviceDisplayItemBuilder;
begin
  // Create Bluetooth service with its dependencies
  BluetoothService := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory
  );

  // Create display item builder with its dependencies
  DisplayItemBuilder := TDeviceDisplayItemBuilder.Create(
    FDeviceConfigProvider,
    FAppearanceConfig,
    Bootstrap.ProfileConfig,
    CreateProfileQuery
  );

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
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    CreateAsyncExecutor,
    BluetoothService,
    Bootstrap.PairingService,
    DisplayItemBuilder
  );
  FPresenter.Initialize;
end;

procedure TFormMain.InitializeHotkey;
begin
  // Create and register global hotkey
  FHotkeyManager := THotkeyManager.Create;
  FHotkeyManager.OnHotkeyTriggered := HandleHotkeyTriggered;
  FHotkeyManager.Register(Handle, FHotkeyConfig.Hotkey, FHotkeyConfig.UseLowLevelHook);

  // Create and register system panel hotkeys (use same hook setting as global hotkey)
  FCastPanelHotkeyManager := THotkeyManager.Create;
  FCastPanelHotkeyManager.OnHotkeyTriggered := HandleCastPanelHotkeyTriggered;
  if FHotkeyConfig.CastPanelHotkey <> '' then
    FCastPanelHotkeyManager.Register(Handle, FHotkeyConfig.CastPanelHotkey, FHotkeyConfig.UseLowLevelHook);

  FBluetoothPanelHotkeyManager := THotkeyManager.Create;
  FBluetoothPanelHotkeyManager.OnHotkeyTriggered := HandleBluetoothPanelHotkeyTriggered;
  if FHotkeyConfig.BluetoothPanelHotkey <> '' then
    FBluetoothPanelHotkeyManager.Register(Handle, FHotkeyConfig.BluetoothPanelHotkey, FHotkeyConfig.UseLowLevelHook);
end;

procedure TFormMain.FinalizeMenuMode;
begin
  // Hide from taskbar in Menu mode
  ApplyMenuModeTaskbarHide;

  // In Menu mode, always start hidden
  if FGeneralConfig.WindowMode = wmMenu then
  begin
    LogDebug('FinalizeMenuMode: Menu mode - starting hidden', ClassName);
    Application.ShowMainForm := False;
    Visible := False;
  end
  // In Window mode, start minimized if configured
  else if (FGeneralConfig.WindowMode = wmWindow) and FWindowConfig.StartMinimized then
  begin
    LogDebug('FinalizeMenuMode: Window mode - starting minimized', ClassName);
    Application.ShowMainForm := False;
    Visible := False;
  end;
end;

{ Settings application helpers }

procedure TFormMain.ApplyHotkeySettings;
begin
  // Re-register main BQC hotkey
  FHotkeyManager.Unregister;
  FHotkeyManager.Register(Handle, FHotkeyConfig.Hotkey, FHotkeyConfig.UseLowLevelHook);
  LogDebug('ApplyHotkeySettings: Hotkey re-registered: %s', [FHotkeyConfig.Hotkey], ClassName);

  // Re-register system panel hotkeys
  FCastPanelHotkeyManager.Unregister;
  if FHotkeyConfig.CastPanelHotkey <> '' then
  begin
    FCastPanelHotkeyManager.Register(Handle, FHotkeyConfig.CastPanelHotkey, FHotkeyConfig.UseLowLevelHook);
    LogDebug('ApplyHotkeySettings: Cast panel hotkey registered: %s', [FHotkeyConfig.CastPanelHotkey], ClassName);
  end;

  FBluetoothPanelHotkeyManager.Unregister;
  if FHotkeyConfig.BluetoothPanelHotkey <> '' then
  begin
    FBluetoothPanelHotkeyManager.Register(Handle, FHotkeyConfig.BluetoothPanelHotkey, FHotkeyConfig.UseLowLevelHook);
    LogDebug('ApplyHotkeySettings: Bluetooth panel hotkey registered: %s', [FHotkeyConfig.BluetoothPanelHotkey], ClassName);
  end;
end;

procedure TFormMain.ApplyThemeSettings;
begin
  FThemeManager.LoadStylesFromDirectory(FAppearanceConfig.VsfDir);
  FThemeManager.SetStyle(FAppearanceConfig.Theme);
  ApplyTheme;
  LogDebug('ApplyThemeSettings: Theme applied: %s', [FAppearanceConfig.Theme], ClassName);
end;

procedure TFormMain.ApplyWindowModeSettings;
begin
  if FGeneralConfig.WindowMode = wmMenu then
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    LogDebug('ApplyWindowModeSettings: Applied Menu mode', ClassName);
  end
  else
  begin
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    LogDebug('ApplyWindowModeSettings: Applied Window mode', ClassName);
  end;
end;

procedure TFormMain.ApplyTaskbarVisibility;
var
  ExStyle: LONG_PTR;
begin
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
  LogDebug('ApplyTaskbarVisibility: Taskbar visibility updated', ClassName);
end;

procedure TFormMain.ApplyOnTopSetting;
begin
  if FGeneralConfig.OnTop or (FGeneralConfig.WindowMode = wmMenu) then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
  LogDebug('ApplyOnTopSetting: OnTop=%s', [BoolToStr(FGeneralConfig.OnTop, True)], ClassName);
end;

procedure TFormMain.ApplyDeviceListSettings;
begin
  if FDeviceList <> nil then
  begin
    FDeviceList.ShowAddresses := FAppearanceConfig.ShowAddresses;
    FDeviceList.Invalidate;
  end;
end;

procedure TFormMain.NotifyPresenterOfChanges;
begin
  if FPresenter <> nil then
  begin
    FPresenter.OnSettingsChanged;
    FPresenter.OnRefreshRequested;
  end;
end;

procedure TFormMain.ApplyAllSettings;
begin
  LogDebug('ApplyAllSettings: Applying configuration changes', ClassName);

  ApplyHotkeySettings;
  ApplyThemeSettings;
  ApplyWindowModeSettings;
  ApplyTaskbarVisibility;
  ApplyOnTopSetting;
  ApplyDeviceListSettings;
  NotifyPresenterOfChanges;

  LogDebug('ApplyAllSettings: Complete', ClassName);
end;

{ Event handlers }

procedure TFormMain.HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
begin
  FPresenter.OnDeviceClicked(ADevice);
end;

procedure TFormMain.HandleActionClick(Sender: TObject; const AItem: TDeviceDisplayItem);
begin
  // Action button clicked (e.g., Scan for devices)
  FPresenter.OnScanRequested;
end;

procedure TFormMain.HandleBluetoothToggle(Sender: TObject);
begin
  if not FPresenter.IsUpdatingToggle then
    FPresenter.OnToggleChanged(BluetoothToggle.State = tssOn);
end;

procedure TFormMain.HandleSettingsClick(Sender: TObject);
var
  SettingsDialog: TFormSettings;
  WasMenuModeVisible: Boolean;
begin
  LogInfo('HandleSettingsClick: Opening settings dialog', ClassName);

  // Remember if menu was visible in menu mode - we'll hide it during Settings
  // This avoids focus tracking issues with modal dialogs
  WasMenuModeVisible := Visible and (FGeneralConfig.WindowMode = wmMenu);

  // Hide menu before opening Settings to avoid focus tracking issues
  // The menu is a transient UI - user can re-open with hotkey after Settings closes
  if WasMenuModeVisible then
  begin
    LogDebug('HandleSettingsClick: Hiding menu before modal dialog', ClassName);
    HideView;
  end;

  SettingsDialog := TFormSettings.Create(Self);
  try
    // Inject dependencies from MainForm (not Bootstrap)
    SettingsDialog.Setup(
      FAppConfig,
      FAppConfig.AsLogConfig,
      FDeviceConfigProvider,
      FBatteryTrayConfig,
      Bootstrap.ProfileConfig,
      FThemeManager
    );
    SettingsDialog.OnSettingsApplied := HandleSettingsApplied;
    SettingsDialog.ShowModal;
  finally
    SettingsDialog.Free;
    // Re-apply hotkey settings in case they were changed
    ApplyHotkeySettings;
  end;
end;

procedure TFormMain.HandleWindowsSettingsClick(Sender: TObject);
var
  Result: HINST;
begin
  Result := ShellExecute(0, 'open', WINDOWS_BLUETOOTH_SETTINGS_URI, nil, nil, SW_SHOWNORMAL);
  if Result <= 32 then
    LogWarning('Failed to open Windows Bluetooth settings, error code: %d', [Result], ClassName);
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

procedure TFormMain.HandleBatteryNotification(Sender: TObject; AAddress: UInt64;
  const ADeviceName: string; ALevel: Integer; AIsLowBattery: Boolean);
var
  Title, Message: string;
begin
  // Show battery notification via tray manager
  if AIsLowBattery then
  begin
    Title := ADeviceName;
    Message := Format('Low battery: %d%%', [ALevel]);
    FTrayManager.ShowNotification(Title, Message, bfWarning);
  end
  else
  begin
    Title := ADeviceName;
    Message := 'Fully charged';
    FTrayManager.ShowNotification(Title, Message, bfInfo);
  end;
end;

procedure TFormMain.HandleHotkeyTriggered(Sender: TObject);
begin
  FPresenter.OnVisibilityToggleRequested;
end;

procedure TFormMain.HandleCastPanelHotkeyTriggered(Sender: TObject);
begin
  // Open Windows Cast panel (same as Win+K)
  ShellExecute(0, 'open', 'ms-actioncenter:controlcenter/cast', nil, nil, SW_SHOWNORMAL);
  LogDebug('Cast panel hotkey triggered', ClassName);
end;

procedure TFormMain.HandleBluetoothPanelHotkeyTriggered(Sender: TObject);
begin
  // Open Windows Bluetooth quick settings panel
  ShellExecute(0, 'open', 'ms-actioncenter:controlcenter/bluetooth', nil, nil, SW_SHOWNORMAL);
  LogDebug('Bluetooth panel hotkey triggered', ClassName);
end;

procedure TFormMain.HandleTrayToggleVisibility(Sender: TObject);
begin
  LogDebug('HandleTrayToggleVisibility: Called, Visible=%s, IsMinimized=%s', [
    BoolToStr(Visible, True), BoolToStr(IsMinimized, True)
  ], ClassName);
  FPresenter.OnVisibilityToggleRequested;
  LogDebug('HandleTrayToggleVisibility: After toggle, Visible=%s', [BoolToStr(Visible, True)], ClassName);
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
  LogDebug('HandleApplicationDeactivate: Called (WindowMode=%d, HideOnFocusLoss=%s, Visible=%s)', [
    Ord(FGeneralConfig.WindowMode),
    BoolToStr(FWindowConfig.MenuHideOnFocusLoss, True),
    BoolToStr(Visible, True)
  ], ClassName);

  if (FGeneralConfig.WindowMode = wmMenu) and FWindowConfig.MenuHideOnFocusLoss and Visible then
  begin
    // Check if cursor is over the notification area (tray icons)
    // If so, user is clicking a tray icon - don't hide, let the click handler toggle
    if IsCursorOverNotificationArea then
    begin
      LogDebug('HandleApplicationDeactivate: Cursor over notification area, skipping hide for tray toggle', ClassName);
      // Don't hide - let the tray click handler decide
    end
    else
    begin
      LogDebug('HandleApplicationDeactivate: Hiding to tray', ClassName);
      HideView;
    end;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not FForceClose) and (not FPresenter.CanClose) then
  begin
    LogDebug('FormCloseQuery: Hiding to tray instead of closing', ClassName);
    CanClose := False;
    HideView;
  end
  else
  begin
    LogDebug('FormCloseQuery: Allowing close', ClassName);
    CanClose := True;
  end;
end;

{ View interfaces implementation }

procedure TFormMain.ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
var
  I: Integer;
  Item: TDeviceDisplayItem;
begin
  FDeviceList.SetDisplayItems(AItems);

  // Update battery tray icons for devices with battery info
  if Assigned(FBatteryTrayManager) then
  begin
    for I := 0 to High(AItems) do
    begin
      Item := AItems[I];
      if Item.Device.IsConnected then
      begin
        if Item.BatteryStatus.IsPending then
          // Show pending icon while battery is being refreshed
          FBatteryTrayManager.UpdateDevicePending(
            Item.Device.AddressInt,
            Item.DisplayName
          )
        else if Item.BatteryStatus.Level >= 0 then
          // Show battery level icon
          FBatteryTrayManager.UpdateDevice(
            Item.Device.AddressInt,
            Item.DisplayName,
            Item.BatteryStatus.Level,
            True
          )
        else
          // Battery not supported or unknown - remove icon
          FBatteryTrayManager.RemoveDevice(Item.Device.AddressInt);
      end
      else
        // Remove tray icon for disconnected devices
        FBatteryTrayManager.RemoveDevice(Item.Device.AddressInt);
    end;
  end;
end;

procedure TFormMain.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
begin
  FDeviceList.UpdateDisplayItem(AItem);

  // Update battery tray icon for this device (same logic as ShowDisplayItems)
  if Assigned(FBatteryTrayManager) then
  begin
    LogDebug('UpdateDisplayItem: Address=$%.12X, IsConnected=%s, IsPending=%s, Level=%d',
      [AItem.Device.AddressInt, BoolToStr(AItem.Device.IsConnected, True),
       BoolToStr(AItem.BatteryStatus.IsPending, True), AItem.BatteryStatus.Level], ClassName);
    if AItem.Device.IsConnected then
    begin
      if AItem.BatteryStatus.IsPending then
        // Show pending icon while battery is being refreshed
        FBatteryTrayManager.UpdateDevicePending(
          AItem.Device.AddressInt,
          AItem.DisplayName
        )
      else if AItem.BatteryStatus.Level >= 0 then
        // Show battery level icon
        FBatteryTrayManager.UpdateDevice(
          AItem.Device.AddressInt,
          AItem.DisplayName,
          AItem.BatteryStatus.Level,
          True
        )
      else
        // Battery not supported or unknown - remove icon
        FBatteryTrayManager.RemoveDevice(AItem.Device.AddressInt);
    end
    else
      // Remove tray icon for disconnected devices
      FBatteryTrayManager.RemoveDevice(AItem.Device.AddressInt);
  end;
end;

procedure TFormMain.ClearDevices;
begin
  FDeviceList.Clear;
  // Also clear battery tray icons
  if Assigned(FBatteryTrayManager) then
    FBatteryTrayManager.ClearAll;
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

procedure TFormMain.SetScanning(AScanning: Boolean);
begin
  // Scanning state is now displayed via the action button in the device list.
  // The action button automatically shows "Scanning..." when IsActionInProgress is True.
  // RefreshDisplayItems (called by presenter after this method) updates the display.
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
var
  ActiveWndBefore, ActiveWndAfter: HWND;
begin
  LogInfo('ShowView', ClassName);

  // Position window based on PositionMode
  // In Menu mode or for modes 1-3 (tray/cursor/center), always reposition
  // In Window mode with mode 0 (coordinates), use saved position
  if (FGeneralConfig.WindowMode = wmMenu) or (FPositionConfig.PositionMode <> pmCoordinates) then
    ApplyWindowPosition;

  ActiveWndBefore := GetActiveWindow;
  LogDebug('ShowView: Before Show, ActiveWindow=$%x, OurHandle=$%x', [ActiveWndBefore, Handle], ClassName);

  Show;
  WindowState := wsNormal;

  // Force foreground activation even from background context (e.g., hotkey)
  // Windows restricts SetForegroundWindow for background processes, so we
  // temporarily attach to the foreground thread to bypass this restriction
  ForceForegroundWindow(Handle);

  ActiveWndAfter := GetActiveWindow;
  LogDebug('ShowView: After ForceForeground, ActiveWindow=$%x, GetForegroundWindow=$%x', [
    ActiveWndAfter, GetForegroundWindow
  ], ClassName);

  // Set focus to device list for immediate keyboard navigation
  if (FDeviceList <> nil) and FDeviceList.CanFocus then
    FDeviceList.SetFocus
  else if CanFocus then
    SetFocus;

  FTrayManager.UpdateMenuCaption(True);

  // Install foreground hook for reliable focus-loss detection in menu mode
  // This is more reliable than WM_ACTIVATE for WS_EX_TOOLWINDOW windows
  InstallForegroundHook;

  // Notify presenter that view is now visible (triggers battery refresh)
  if FPresenter <> nil then
    FPresenter.OnViewShown;
end;

procedure TFormMain.HideView;
begin
  LogInfo('HideView', ClassName);

  // Uninstall foreground hook before hiding
  UninstallForegroundHook;

  Hide;
  FTrayManager.UpdateMenuCaption(False);
end;

procedure TFormMain.DoForceClose;
begin
  LogInfo('DoForceClose', ClassName);
  FForceClose := True;
  Close;
end;

{ Foreground tracking for menu mode }

procedure TFormMain.InstallForegroundHook;
begin
  // Only install in menu mode with hide-on-focus-loss enabled
  if (FGeneralConfig.WindowMode <> wmMenu) or (not FWindowConfig.MenuHideOnFocusLoss) then
    Exit;

  // Already installed?
  if FForegroundHook <> 0 then
    Exit;

  GMainFormInstance := Self;
  FForegroundHook := SetWinEventHook(
    EVENT_SYSTEM_FOREGROUND,
    EVENT_SYSTEM_FOREGROUND,
    0,
    @ForegroundEventProc,
    0,
    0,
    WINEVENT_OUTOFCONTEXT  // Don't use WINEVENT_SKIPOWNPROCESS - we filter by process ID in callback
  );

  if FForegroundHook <> 0 then
    LogDebug('InstallForegroundHook: Installed, handle=$%x', [FForegroundHook], ClassName)
  else
    LogWarning('InstallForegroundHook: Failed to install hook', ClassName);
end;

procedure TFormMain.UninstallForegroundHook;
begin
  if FForegroundHook <> 0 then
  begin
    UnhookWinEvent(FForegroundHook);
    LogDebug('UninstallForegroundHook: Uninstalled, handle=$%x', [FForegroundHook], ClassName);
    FForegroundHook := 0;
  end;
  GMainFormInstance := nil;
end;

{ Windows message handlers }

procedure TFormMain.WMForegroundLost(var Msg: TMessage);
var
  NewForegroundWnd: HWND;
begin
  NewForegroundWnd := HWND(Msg.LParam);
  LogDebug('WMForegroundLost: NewForegroundWnd=$%x, Visible=%s, WindowMode=%d, HideOnFocusLoss=%s', [
    NewForegroundWnd,
    BoolToStr(Visible, True),
    Ord(FGeneralConfig.WindowMode),
    BoolToStr(FWindowConfig.MenuHideOnFocusLoss, True)
  ], ClassName);

  // Hide if we're visible in menu mode with hide-on-focus-loss enabled
  if Visible and
     (FGeneralConfig.WindowMode = wmMenu) and
     FWindowConfig.MenuHideOnFocusLoss then
  begin
    // Check if cursor is over the notification area (tray icons)
    // If so, user is clicking a tray icon - don't hide, let the click handler toggle
    if IsCursorOverNotificationArea then
    begin
      LogDebug('WMForegroundLost: Cursor over notification area, skipping hide for tray toggle', ClassName);
      // Don't hide - let the tray click handler decide
    end
    else
    begin
      LogDebug('WMForegroundLost: Conditions met, hiding menu', ClassName);
      HideView;
    end;
  end;
end;

procedure TFormMain.WMActivate(var Msg: TWMActivate);
var
  ActiveStr: string;
begin
  // Log every WM_ACTIVATE message to trace activation state
  case Msg.Active of
    WA_INACTIVE: ActiveStr := 'WA_INACTIVE';
    WA_ACTIVE: ActiveStr := 'WA_ACTIVE';
    WA_CLICKACTIVE: ActiveStr := 'WA_CLICKACTIVE';
  else
    ActiveStr := Format('Unknown(%d)', [Msg.Active]);
  end;
  LogDebug('WMActivate: Active=%s, WindowMode=%d, HideOnFocusLoss=%s, Visible=%s', [
    ActiveStr,
    Ord(FGeneralConfig.WindowMode),
    BoolToStr(FWindowConfig.MenuHideOnFocusLoss, True),
    BoolToStr(Visible, True)
  ], ClassName);

  inherited;

  // Handle deactivation for menu mode - hide when another window becomes active
  // This is more reliable than Application.OnDeactivate for WS_EX_TOOLWINDOW windows
  if (Msg.Active = WA_INACTIVE) then
  begin
    if (FGeneralConfig.WindowMode = wmMenu) and
       FWindowConfig.MenuHideOnFocusLoss and
       Visible then
    begin
      // Check if cursor is over the notification area (tray icons)
      // If so, user is clicking a tray icon - don't hide, let the click handler toggle
      if IsCursorOverNotificationArea then
      begin
        LogDebug('WMActivate: Cursor over notification area, skipping hide for tray toggle', ClassName);
        // Don't hide - let the tray click handler decide
      end
      else
      begin
        LogDebug('WMActivate: Conditions met, hiding menu', ClassName);
        HideView;
      end;
    end
    else
    begin
      LogDebug('WMActivate: WA_INACTIVE but conditions NOT met (WindowMode=%d, HideOnFocusLoss=%s, Visible=%s)', [
        Ord(FGeneralConfig.WindowMode),
        BoolToStr(FWindowConfig.MenuHideOnFocusLoss, True),
        BoolToStr(Visible, True)
      ], ClassName);
    end;
  end;
end;

procedure TFormMain.WMHotkey(var Msg: TMessage);
begin
  // Check all hotkey managers - each has a unique ID
  if FHotkeyManager.HandleWMHotkey(Msg.WParam) then
    Exit;
  if Assigned(FCastPanelHotkeyManager) and FCastPanelHotkeyManager.HandleWMHotkey(Msg.WParam) then
    Exit;
  if Assigned(FBluetoothPanelHotkeyManager) then
    FBluetoothPanelHotkeyManager.HandleWMHotkey(Msg.WParam);
end;

procedure TFormMain.WMHotkeyDetected(var Msg: TMessage);
begin
  // Check all hotkey managers - wParam contains InstanceId of the triggered hotkey
  if FHotkeyManager.HandleHotkeyDetected(Msg.WParam) then
    Exit;
  if Assigned(FCastPanelHotkeyManager) and FCastPanelHotkeyManager.HandleHotkeyDetected(Msg.WParam) then
    Exit;
  if Assigned(FBluetoothPanelHotkeyManager) then
    FBluetoothPanelHotkeyManager.HandleHotkeyDetected(Msg.WParam);
end;

procedure TFormMain.WMDpiChanged(var Msg: TMessage);
begin
  inherited;

  if (FGeneralConfig.WindowMode = wmMenu) and Visible then
  begin
    LogDebug('WMDpiChanged: Repositioning window', ClassName);
    ApplyWindowPosition;
  end;
end;

procedure TFormMain.WMDisplayChange(var Msg: TMessage);
begin
  inherited;
  // Display configuration changed (resolution, monitor add/remove, taskbar move).
  // Low-level keyboard hooks (WH_KEYBOARD_LL) may become stale during Windows
  // display reconfiguration - refresh them immediately.
  //
  // KNOWN ISSUE: This mitigation doesn't fully solve the problem. Windows may
  // reset hooks at times that don't trigger WM_DISPLAYCHANGE (e.g., certain
  // taskbar movements). In rare cases, Win+K may trigger the system Cast panel
  // instead of this app. Workaround: press the hotkey again to restore the hook.
  LogDebug('WMDisplayChange: Display configuration changed, refreshing hotkey', ClassName);
  ApplyHotkeySettings;
end;

procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  if (Msg.CmdType and $FFF0) = SC_MINIMIZE then
  begin
    if FWindowConfig.MinimizeToTray then
    begin
      LogDebug('WMSysCommand: Minimizing to tray', ClassName);
      HideView;
      Msg.Result := 0;
      Exit;
    end;
  end;

  inherited;
end;

end.
