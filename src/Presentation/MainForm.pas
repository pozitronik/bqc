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
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  UI.Theme,
  UI.DeviceList;

const
  WM_HOTKEY_DETECTED = WM_USER + 100;

type
  /// <summary>
  /// Main application form displaying Bluetooth devices.
  /// </summary>
  TFormMain = class(TForm)
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
  private
    FBluetoothService: IBluetoothService;
    FDevices: TBluetoothDeviceInfoArray;
    FDeviceList: TDeviceListBox;
    FRadioWatcher: TBluetoothRadioWatcher;
    FUpdatingToggle: Boolean;
    FDelayedLoadTimer: TTimer;
    FTrayIcon: TTrayIcon;
    FTrayMenu: TPopupMenu;
    FForceClose: Boolean;
    FHotkeyRegistered: Boolean;
    FHotkeyId: Integer;
    FUsingLowLevelHook: Boolean;

    procedure CreateDeviceList;
    procedure CreateTrayIcon;
    procedure RegisterGlobalHotkey;
    procedure UnregisterGlobalHotkey;
    function ParseHotkeyString(const AHotkey: string; out AModifiers: Cardinal; out AVirtualKey: Cardinal): Boolean;
    procedure SetToggleState(AState: TToggleSwitchState);
    procedure ApplyTheme;
    procedure LoadDevices;
    procedure LoadDevicesDelayed;
    procedure AutoConnectDevices;
    procedure UpdateStatus(const AMessage: string);
    procedure ShowFromTray;
    procedure HideToTray;
    procedure PositionMenuPopup;
    procedure ApplyWindowMode;
    procedure ApplyMenuModeTaskbarHide;

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleThemeChanged(Sender: TObject);
    procedure HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
    procedure HandleDelayedLoadTimer(Sender: TObject);
    procedure HandleTrayIconClick(Sender: TObject);
    procedure HandleTrayMenuShowClick(Sender: TObject);
    procedure HandleTrayMenuExitClick(Sender: TObject);

  protected
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotkey(var Msg: TMessage); message WM_HOTKEY;
    procedure WMHotkeyDetected(var Msg: TMessage); message WM_HOTKEY_DETECTED;

  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes,
  ShellAPI,
  Bluetooth.Service,
  App.Logger,
  App.Config;

{$R *.dfm}

const
  // Low-level keyboard hook constants
  WH_KEYBOARD_LL = 13;
  LLKHF_UP = $80;

type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

var
  // Global state for low-level keyboard hook
  GKeyboardHook: HHOOK = 0;
  GHotkeyModifiers: Cardinal = 0;
  GHotkeyVirtualKey: Cardinal = 0;
  GHotkeyFormHandle: HWND = 0;
  GCurrentModifiers: Cardinal = 0;

function LowLevelKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KbdStruct: PKBDLLHOOKSTRUCT;
  VK: Cardinal;
  IsKeyDown: Boolean;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(GKeyboardHook, nCode, wParam, lParam);
    Exit;
  end;

  KbdStruct := PKBDLLHOOKSTRUCT(lParam);
  VK := KbdStruct^.vkCode;
  IsKeyDown := (KbdStruct^.flags and LLKHF_UP) = 0;

  // Track modifier key states
  case VK of
    VK_LWIN, VK_RWIN:
      if IsKeyDown then
        GCurrentModifiers := GCurrentModifiers or MOD_WIN
      else
        GCurrentModifiers := GCurrentModifiers and (not MOD_WIN);
    VK_LCONTROL, VK_RCONTROL:
      if IsKeyDown then
        GCurrentModifiers := GCurrentModifiers or MOD_CONTROL
      else
        GCurrentModifiers := GCurrentModifiers and (not MOD_CONTROL);
    VK_LMENU, VK_RMENU:
      if IsKeyDown then
        GCurrentModifiers := GCurrentModifiers or MOD_ALT
      else
        GCurrentModifiers := GCurrentModifiers and (not MOD_ALT);
    VK_LSHIFT, VK_RSHIFT:
      if IsKeyDown then
        GCurrentModifiers := GCurrentModifiers or MOD_SHIFT
      else
        GCurrentModifiers := GCurrentModifiers and (not MOD_SHIFT);
  end;

  // Check if our hotkey is triggered (on key down, not modifiers themselves)
  if IsKeyDown and (GHotkeyVirtualKey <> 0) then
  begin
    // Check if this is the main key (not a modifier) and modifiers match
    if (VK = GHotkeyVirtualKey) and (GCurrentModifiers = GHotkeyModifiers) then
    begin
      // Post message to form and consume the key
      PostMessage(GHotkeyFormHandle, WM_HOTKEY_DETECTED, 0, 0);
      Result := 1;  // Consume the key, don't pass to system
      Exit;
    end;
  end;

  Result := CallNextHookEx(GKeyboardHook, nCode, wParam, lParam);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  RadioEnabled: Boolean;
begin
  // Load configuration early (this also applies LoggingEnabled setting)
  Config;

  Log('[MainForm] FormCreate: Starting');

  FForceClose := False;
  FHotkeyRegistered := False;
  FHotkeyId := GlobalAddAtom('BQC_GlobalHotkey');
  FUsingLowLevelHook := False;

  // Apply window mode (must be done early before window handle is created)
  ApplyWindowMode;

  // Restore window position/size from configuration
  // In Menu mode, position is handled by PositionMenuPopup when showing
  if Config.WindowMode = wmMenu then
  begin
    // Menu mode: always use poDesigned to prevent VCL from centering the form
    // Actual position will be set by PositionMenuPopup when showing
    Position := poDesigned;
    // Set initial position off-screen to avoid flicker if somehow shown before positioned
    Left := -10000;
    Top := -10000;
    Log('[MainForm] FormCreate: Menu mode, position will be set on show');
  end
  else if (Config.WindowX >= 0) and (Config.WindowY >= 0) then
  begin
    // Window mode with saved position
    Position := poDesigned;
    Left := Config.WindowX;
    Top := Config.WindowY;
    Log('[MainForm] FormCreate: Restored position X=%d, Y=%d', [Left, Top]);
  end;

  // Restore window size from configuration (only in Window mode)
  // In Menu mode, use design-time dimensions to ensure consistent sizing
  if (Config.WindowMode = wmWindow) and (Config.WindowWidth > 0) and (Config.WindowHeight > 0) then
  begin
    Width := Config.WindowWidth;
    Height := Config.WindowHeight;
    Log('[MainForm] FormCreate: Restored size W=%d, H=%d', [Width, Height]);
  end
  else
  begin
    Log('[MainForm] FormCreate: Using design size W=%d, H=%d', [Width, Height]);
  end;

  // Apply StayOnTop setting (Menu mode is always on top)
  if Config.StayOnTop or (Config.WindowMode = wmMenu) then
  begin
    FormStyle := fsStayOnTop;
    Log('[MainForm] FormCreate: StayOnTop enabled');
  end;

  FUpdatingToggle := False;
  FRadioWatcher := nil;

  // Create delayed load timer (for when BT is enabled externally)
  FDelayedLoadTimer := TTimer.Create(Self);
  FDelayedLoadTimer.Enabled := False;
  FDelayedLoadTimer.Interval := 500;
  FDelayedLoadTimer.OnTimer := HandleDelayedLoadTimer;

  // Subscribe to theme changes
  Theme.OnThemeChanged := HandleThemeChanged;

  // Create custom device list control
  CreateDeviceList;

  // Create tray icon
  CreateTrayIcon;

  // Apply configuration settings to device list
  FDeviceList.ShowAddresses := Config.ShowAddresses;

  // Apply current theme
  ApplyTheme;

  // Create Bluetooth service
  Log('[MainForm] FormCreate: Creating Bluetooth service');
  FBluetoothService := CreateBluetoothService;
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;
  Log('[MainForm] FormCreate: Bluetooth service created, event handlers assigned');

  // Check Bluetooth radio state using WinRT API
  if GetBluetoothRadioState(RadioEnabled) then
  begin
    Log('[MainForm] FormCreate: Radio state: Enabled=%s', [BoolToStr(RadioEnabled, True)]);
    // Adapter exists, set toggle based on current state
    if RadioEnabled then
    begin
      SetToggleState(tssOn);
      UpdateStatus('Loading devices...');
      LoadDevices;
      AutoConnectDevices;
    end
    else
    begin
      SetToggleState(tssOff);
      UpdateStatus('Bluetooth is off');
      FDeviceList.Clear;
    end;

    // Start watching for radio state changes (from Windows Settings etc.)
    FRadioWatcher := TBluetoothRadioWatcher.Create;
    FRadioWatcher.OnStateChanged := HandleRadioStateChanged;
    FRadioWatcher.Start;
    Log('[MainForm] FormCreate: Radio watcher started');
  end
  else
  begin
    Log('[MainForm] FormCreate: No Bluetooth adapter found');
    // No Bluetooth adapter found
    SetToggleState(tssOff);
    BluetoothToggle.Enabled := False;
    UpdateStatus('No Bluetooth adapter found');
  end;

  // Register global hotkey if configured
  RegisterGlobalHotkey;

  // Hide from taskbar in Menu mode (must be after handle is created)
  ApplyMenuModeTaskbarHide;

  // In Menu mode, start hidden - user will show via hotkey or tray click
  if Config.WindowMode = wmMenu then
  begin
    Log('[MainForm] FormCreate: Menu mode, starting hidden');
    // Don't call HideToTray here as it would interfere with form creation
    // Just make sure we don't show the form
    Application.ShowMainForm := False;
    Visible := False;
  end;

  Log('[MainForm] FormCreate: Complete');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Save window position and size to configuration (only in Window mode)
  if (Config.WindowMode = wmWindow) and (WindowState = wsNormal) then
  begin
    Config.WindowX := Left;
    Config.WindowY := Top;
    Config.WindowWidth := Width;
    Config.WindowHeight := Height;
    Log('[MainForm] FormDestroy: Saved position X=%d, Y=%d, W=%d, H=%d', [Left, Top, Width, Height]);
  end;

  // Unregister global hotkey
  UnregisterGlobalHotkey;
  if FHotkeyId <> 0 then
    GlobalDeleteAtom(FHotkeyId);

  Theme.OnThemeChanged := nil;

  // Stop and free radio watcher
  if FRadioWatcher <> nil then
  begin
    FRadioWatcher.Stop;
    FRadioWatcher.Free;
    FRadioWatcher := nil;
  end;

  FBluetoothService := nil;
  FDevices := nil;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_F5:
      HandleRefreshClick(nil);
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

procedure TFormMain.CreateTrayIcon;
var
  MenuItem: TMenuItem;
begin
  // Create popup menu for tray icon
  FTrayMenu := TPopupMenu.Create(Self);

  // Show/Hide menu item
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := 'Show';
  MenuItem.OnClick := HandleTrayMenuShowClick;
  MenuItem.Default := True;
  FTrayMenu.Items.Add(MenuItem);

  // Separator
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := '-';
  FTrayMenu.Items.Add(MenuItem);

  // Exit menu item
  MenuItem := TMenuItem.Create(FTrayMenu);
  MenuItem.Caption := 'Exit';
  MenuItem.OnClick := HandleTrayMenuExitClick;
  FTrayMenu.Items.Add(MenuItem);

  // Create tray icon
  FTrayIcon := TTrayIcon.Create(Self);
  FTrayIcon.Hint := 'Bluetooth Quick Connect';
  FTrayIcon.PopupMenu := FTrayMenu;
  FTrayIcon.OnClick := HandleTrayIconClick;

  // Use application icon
  FTrayIcon.Icon.Assign(Application.Icon);

  // Tray icon is always visible
  FTrayIcon.Visible := True;

  Log('[MainForm] CreateTrayIcon: Tray icon created');
end;

function TFormMain.ParseHotkeyString(const AHotkey: string;
  out AModifiers: Cardinal; out AVirtualKey: Cardinal): Boolean;
var
  Parts: TArray<string>;
  Part: string;
  KeyPart: string;
  I: Integer;
begin
  Result := False;
  AModifiers := 0;
  AVirtualKey := 0;

  if Trim(AHotkey) = '' then
    Exit;

  // Split by '+' and process each part
  Parts := AHotkey.Split(['+']);
  if Length(Parts) = 0 then
    Exit;

  // Last part is the key, rest are modifiers
  KeyPart := Trim(Parts[High(Parts)]).ToUpper;

  // Process modifiers
  for I := 0 to High(Parts) - 1 do
  begin
    Part := Trim(Parts[I]).ToUpper;
    if (Part = 'CTRL') or (Part = 'CONTROL') then
      AModifiers := AModifiers or MOD_CONTROL
    else if Part = 'ALT' then
      AModifiers := AModifiers or MOD_ALT
    else if Part = 'SHIFT' then
      AModifiers := AModifiers or MOD_SHIFT
    else if (Part = 'WIN') or (Part = 'WINDOWS') then
      AModifiers := AModifiers or MOD_WIN
    else
    begin
      Log('[MainForm] ParseHotkeyString: Unknown modifier "%s"', [Part]);
      Exit;
    end;
  end;

  // Parse the key
  if Length(KeyPart) = 1 then
  begin
    // Single character: A-Z or 0-9
    if (KeyPart[1] >= 'A') and (KeyPart[1] <= 'Z') then
      AVirtualKey := Ord(KeyPart[1])
    else if (KeyPart[1] >= '0') and (KeyPart[1] <= '9') then
      AVirtualKey := Ord(KeyPart[1])
    else
    begin
      Log('[MainForm] ParseHotkeyString: Unknown key "%s"', [KeyPart]);
      Exit;
    end;
  end
  else if KeyPart.StartsWith('F') and (Length(KeyPart) <= 3) then
  begin
    // Function keys F1-F12
    I := StrToIntDef(KeyPart.Substring(1), 0);
    if (I >= 1) and (I <= 12) then
      AVirtualKey := VK_F1 + I - 1
    else
    begin
      Log('[MainForm] ParseHotkeyString: Unknown function key "%s"', [KeyPart]);
      Exit;
    end;
  end
  else if KeyPart = 'SPACE' then
    AVirtualKey := VK_SPACE
  else if KeyPart = 'ENTER' then
    AVirtualKey := VK_RETURN
  else if KeyPart = 'TAB' then
    AVirtualKey := VK_TAB
  else if KeyPart = 'ESCAPE' then
    AVirtualKey := VK_ESCAPE
  else if KeyPart = 'BACKSPACE' then
    AVirtualKey := VK_BACK
  else if KeyPart = 'DELETE' then
    AVirtualKey := VK_DELETE
  else if KeyPart = 'INSERT' then
    AVirtualKey := VK_INSERT
  else if KeyPart = 'HOME' then
    AVirtualKey := VK_HOME
  else if KeyPart = 'END' then
    AVirtualKey := VK_END
  else if KeyPart = 'PAGEUP' then
    AVirtualKey := VK_PRIOR
  else if KeyPart = 'PAGEDOWN' then
    AVirtualKey := VK_NEXT
  else if KeyPart = 'UP' then
    AVirtualKey := VK_UP
  else if KeyPart = 'DOWN' then
    AVirtualKey := VK_DOWN
  else if KeyPart = 'LEFT' then
    AVirtualKey := VK_LEFT
  else if KeyPart = 'RIGHT' then
    AVirtualKey := VK_RIGHT
  else
  begin
    Log('[MainForm] ParseHotkeyString: Unknown key "%s"', [KeyPart]);
    Exit;
  end;

  // Need at least one modifier for global hotkeys
  if AModifiers = 0 then
  begin
    Log('[MainForm] ParseHotkeyString: No modifiers specified');
    Exit;
  end;

  Result := True;
  Log('[MainForm] ParseHotkeyString: Parsed "%s" -> Modifiers=$%X, VK=$%X', [AHotkey, AModifiers, AVirtualKey]);
end;

procedure TFormMain.RegisterGlobalHotkey;
var
  Modifiers, VirtualKey: Cardinal;
begin
  if FHotkeyRegistered then
    UnregisterGlobalHotkey;

  if Trim(Config.Hotkey) = '' then
  begin
    Log('[MainForm] RegisterGlobalHotkey: No hotkey configured');
    Exit;
  end;

  if not ParseHotkeyString(Config.Hotkey, Modifiers, VirtualKey) then
  begin
    Log('[MainForm] RegisterGlobalHotkey: Failed to parse hotkey "%s"', [Config.Hotkey]);
    Exit;
  end;

  if Config.UseLowLevelHook then
  begin
    // Use low-level keyboard hook (can override system hotkeys)
    GHotkeyModifiers := Modifiers;
    GHotkeyVirtualKey := VirtualKey;
    GHotkeyFormHandle := Handle;
    GCurrentModifiers := 0;

    GKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);
    if GKeyboardHook <> 0 then
    begin
      FHotkeyRegistered := True;
      FUsingLowLevelHook := True;
      Log('[MainForm] RegisterGlobalHotkey: Installed low-level hook for "%s"', [Config.Hotkey]);
    end
    else
    begin
      Log('[MainForm] RegisterGlobalHotkey: Failed to install low-level hook (Error=%d)', [GetLastError]);
    end;
  end
  else
  begin
    // Use standard RegisterHotKey (fallback, cannot override system hotkeys)
    // Add MOD_NOREPEAT to prevent repeated triggers when holding key
    if RegisterHotKey(Handle, FHotkeyId, Modifiers or MOD_NOREPEAT, VirtualKey) then
    begin
      FHotkeyRegistered := True;
      FUsingLowLevelHook := False;
      Log('[MainForm] RegisterGlobalHotkey: Registered hotkey "%s" (RegisterHotKey)', [Config.Hotkey]);
    end
    else
    begin
      Log('[MainForm] RegisterGlobalHotkey: Failed to register hotkey "%s" (Error=%d)', [Config.Hotkey, GetLastError]);
    end;
  end;
end;

procedure TFormMain.UnregisterGlobalHotkey;
begin
  if FHotkeyRegistered then
  begin
    if FUsingLowLevelHook then
    begin
      // Uninstall low-level hook
      if GKeyboardHook <> 0 then
      begin
        UnhookWindowsHookEx(GKeyboardHook);
        GKeyboardHook := 0;
        GHotkeyModifiers := 0;
        GHotkeyVirtualKey := 0;
        GHotkeyFormHandle := 0;
        Log('[MainForm] UnregisterGlobalHotkey: Uninstalled low-level hook');
      end;
    end
    else
    begin
      // Unregister standard hotkey
      UnregisterHotKey(Handle, FHotkeyId);
      Log('[MainForm] UnregisterGlobalHotkey: Unregistered hotkey (RegisterHotKey)');
    end;
    FHotkeyRegistered := False;
    FUsingLowLevelHook := False;
  end;
end;

procedure TFormMain.WMHotkey(var Msg: TMessage);
begin
  // Handler for RegisterHotKey method
  if Msg.WParam = Cardinal(FHotkeyId) then
  begin
    Log('[MainForm] WMHotkey: Global hotkey triggered (RegisterHotKey)');
    // Toggle visibility (same as tray icon click)
    if Visible and (WindowState <> wsMinimized) then
      HideToTray
    else
      ShowFromTray;
  end;
end;

procedure TFormMain.WMHotkeyDetected(var Msg: TMessage);
begin
  // Handler for low-level keyboard hook method
  Log('[MainForm] WMHotkeyDetected: Global hotkey triggered (low-level hook)');
  // Toggle visibility (same as tray icon click)
  if Visible and (WindowState <> wsMinimized) then
    HideToTray
  else
    ShowFromTray;
end;

procedure TFormMain.SetToggleState(AState: TToggleSwitchState);
begin
  FUpdatingToggle := True;
  try
    BluetoothToggle.State := AState;
  finally
    FUpdatingToggle := False;
  end;
end;

procedure TFormMain.TitleLabelClick(Sender: TObject);
begin
  UpdateStatus('Refreshing...');
  LoadDevices;
end;

procedure TFormMain.ApplyTheme;
var
  Colors: TThemeColors;
begin
  Colors := Theme.Colors;

  // Form
  Color := Colors.Background;

  // Header
  HeaderPanel.Color := Colors.Background;
  TitleLabel.Font.Color := Colors.TextPrimary;


  // Devices
  DevicesPanel.Color := Colors.Background;

  // Status
  StatusPanel.Color := Colors.Background;
  StatusLabel.Font.Color := Colors.TextSecondary;

  // Settings link
  SettingsLink.Font.Color := Colors.Accent;

  // Refresh device list to apply theme
  FDeviceList.Invalidate;
end;

procedure TFormMain.LoadDevices;
var
  I: Integer;
begin
  Log('[MainForm] LoadDevices: Starting');
  Screen.Cursor := crHourGlass;
  try
    FDevices := FBluetoothService.GetPairedDevices;
    Log('[MainForm] LoadDevices: Got %d devices', [Length(FDevices)]);

    for I := 0 to High(FDevices) do
      Log('[MainForm] LoadDevices: Device[%d] Address=$%.12X, Name="%s", Connected=%s', [
        I, FDevices[I].AddressInt, FDevices[I].Name, BoolToStr(FDevices[I].IsConnected, True)
      ]);

    FDeviceList.SetDevices(FDevices);

    if Length(FDevices) = 0 then
      UpdateStatus('No paired devices')
    else
      UpdateStatus(Format('%d device(s)', [Length(FDevices)]));
  finally
    Screen.Cursor := crDefault;
  end;
  Log('[MainForm] LoadDevices: Complete');
end;

procedure TFormMain.AutoConnectDevices;

  procedure ConnectDeviceAsync(const ADevice: TBluetoothDeviceInfo; AService: IBluetoothService);
  var
    LDevice: TBluetoothDeviceInfo;
  begin
    // Copy device info to local variable before creating thread
    LDevice := ADevice;
    TThread.CreateAnonymousThread(
      procedure
      begin
        AService.Connect(LDevice);
      end
    ).Start;
  end;

var
  I: Integer;
  DeviceConfig: TDeviceConfig;
  Device: TBluetoothDeviceInfo;
begin
  Log('[MainForm] AutoConnectDevices: Starting');

  for I := 0 to High(FDevices) do
  begin
    Device := FDevices[I];
    DeviceConfig := Config.GetDeviceConfig(Device.AddressInt);

    // Skip if AutoConnect not enabled for this device
    if not DeviceConfig.AutoConnect then
      Continue;

    // Skip if already connected
    if Device.IsConnected then
    begin
      Log('[MainForm] AutoConnectDevices: %s already connected, skipping', [Device.Name]);
      Continue;
    end;

    Log('[MainForm] AutoConnectDevices: Auto-connecting %s (Address=$%.12X)', [Device.Name, Device.AddressInt]);
    UpdateStatus(Format('Auto-connecting %s...', [Device.Name]));

    // Use helper procedure to properly capture device value
    ConnectDeviceAsync(Device, FBluetoothService);
  end;

  Log('[MainForm] AutoConnectDevices: Complete');
end;

procedure TFormMain.UpdateStatus(const AMessage: string);
begin
  StatusLabel.Caption := AMessage;
end;

procedure TFormMain.HandleDeviceClick(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
begin
  Log('[MainForm] HandleDeviceClick: Address=$%.12X, Name="%s", ConnectionState=%d', [
    ADevice.AddressInt, ADevice.Name, Ord(ADevice.ConnectionState)
  ]);

  if ADevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    Log('[MainForm] HandleDeviceClick: Operation in progress, ignoring');
    UpdateStatus('Operation in progress...');
    Exit;
  end;

  if ADevice.IsConnected then
  begin
    Log('[MainForm] HandleDeviceClick: Device is connected, will disconnect');
    UpdateStatus(Format('Disconnecting %s...', [ADevice.Name]));
  end
  else
  begin
    Log('[MainForm] HandleDeviceClick: Device is disconnected, will connect');
    UpdateStatus(Format('Connecting %s...', [ADevice.Name]));
  end;

  // Make a local copy for the anonymous procedure to capture
  LDevice := ADevice;

  // In Menu mode, hide immediately after device click
  if Config.WindowMode = wmMenu then
  begin
    Log('[MainForm] HandleDeviceClick: Menu mode, hiding to tray');
    HideToTray;
  end;

  // Toggle connection in background thread
  // Note: We don't call LoadDevices after toggle because:
  // 1. ConnectWithStrategy already fires DoDeviceStateChanged with the correct state
  // 2. Calling LoadDevices immediately may get stale data from Windows (race condition)
  // 3. The device watcher will catch any external state changes
  TThread.CreateAnonymousThread(
    procedure
    begin
      Log('[MainForm] HandleDeviceClick (thread): Calling ToggleConnection');
      FBluetoothService.ToggleConnection(LDevice);
      Log('[MainForm] HandleDeviceClick (thread): ToggleConnection completed');
    end
  ).Start;
end;

procedure TFormMain.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
begin
  Log('[MainForm] HandleDeviceStateChanged: Address=$%.12X, Name="%s", ConnectionState=%d', [
    ADevice.AddressInt, ADevice.Name, Ord(ADevice.ConnectionState)
  ]);

  // Skip devices with empty names (invalid events)
  if Trim(ADevice.Name) = '' then
  begin
    Log('[MainForm] HandleDeviceStateChanged: Empty name, skipping');
    Exit;
  end;

  // Make a local copy for the anonymous procedure to capture
  // (const parameters are passed by reference and may be destroyed before queue executes)
  LDevice := ADevice;

  TThread.Queue(nil,
    procedure
    var
      I: Integer;
      Found: Boolean;
    begin
      Log('[MainForm] HandleDeviceStateChanged (queued): Processing Name="%s", ConnectionState=%d', [
        LDevice.Name, Ord(LDevice.ConnectionState)
      ]);

      Found := False;

      // Update local cache
      for I := 0 to High(FDevices) do
      begin
        if FDevices[I].AddressInt = LDevice.AddressInt then
        begin
          Log('[MainForm] HandleDeviceStateChanged (queued): Found at index %d, updating', [I]);
          FDevices[I] := LDevice;
          Found := True;
          Break;
        end;
      end;

      // If device not in cache, add it (for newly connected devices)
      if not Found then
      begin
        Log('[MainForm] HandleDeviceStateChanged (queued): Not found, adding new device');
        SetLength(FDevices, Length(FDevices) + 1);
        FDevices[High(FDevices)] := LDevice;
        FDeviceList.AddDevice(LDevice);
      end
      else
      begin
        // Update UI for existing device
        Log('[MainForm] HandleDeviceStateChanged (queued): Calling FDeviceList.UpdateDevice');
        FDeviceList.UpdateDevice(LDevice);
      end;

      // Show status
      UpdateStatus(Format('%s: %s', [LDevice.Name, LDevice.ConnectionStateText]));
    end
  );
end;

procedure TFormMain.HandleDeviceListChanged(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      LoadDevices;
    end
  );
end;

procedure TFormMain.HandleError(Sender: TObject; const AMessage: string;
  AErrorCode: Cardinal);
begin
  TThread.Queue(nil,
    procedure
    begin
      if AErrorCode <> 0 then
        UpdateStatus(Format('Error: %s (%d)', [AMessage, AErrorCode]))
      else
        UpdateStatus(Format('Error: %s', [AMessage]));
    end
  );
end;

procedure TFormMain.HandleThemeChanged(Sender: TObject);
begin
  ApplyTheme;
end;

procedure TFormMain.HandleSettingsClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'ms-settings:bluetooth', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.HandleBluetoothToggle(Sender: TObject);
var
  EnableBluetooth: Boolean;
begin
  // Prevent re-entrancy
  if FUpdatingToggle then
    Exit;

  EnableBluetooth := (BluetoothToggle.State = tssOn);

  if EnableBluetooth then
    UpdateStatus('Enabling Bluetooth...')
  else
    UpdateStatus('Disabling Bluetooth...');

  // Perform radio control in background thread
  TThread.CreateAnonymousThread(
    procedure
    var
      LResult: TRadioControlResult;
      LEnableBT: Boolean;
    begin
      LEnableBT := EnableBluetooth;
      LResult := SetBluetoothRadioState(LEnableBT);

      TThread.Queue(nil,
        procedure
        begin
          case LResult of
            rcSuccess:
              begin
                if LEnableBT then
                begin
                  UpdateStatus('Bluetooth enabled');
                  LoadDevices;
                end
                else
                begin
                  UpdateStatus('Bluetooth disabled');
                  FDeviceList.Clear;
                end;
              end;
            rcAccessDenied:
              begin
                UpdateStatus('Access denied - check Windows settings');
                // Revert toggle state
                if LEnableBT then
                  SetToggleState(tssOff)
                else
                  SetToggleState(tssOn);
              end;
            rcDeviceNotFound:
              begin
                UpdateStatus('Bluetooth adapter not found');
                SetToggleState(tssOff);
                BluetoothToggle.Enabled := False;
              end;
          else
            begin
              UpdateStatus('Failed to change Bluetooth state');
              // Revert toggle state
              if LEnableBT then
                SetToggleState(tssOff)
              else
                SetToggleState(tssOn);
            end;
          end;
        end
      );
    end
  ).Start;
end;

procedure TFormMain.HandleRefreshClick(Sender: TObject);
begin
  UpdateStatus('Refreshing...');
  LoadDevices;
end;

procedure TFormMain.HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
begin
  // Update toggle to reflect new state (changed from Windows Settings or elsewhere)
  if AEnabled then
  begin
    SetToggleState(tssOn);
    UpdateStatus('Loading devices...');
    // Use delayed load to give Bluetooth stack time to initialize
    LoadDevicesDelayed;
  end
  else
  begin
    SetToggleState(tssOff);
    UpdateStatus('Bluetooth disabled');
    FDelayedLoadTimer.Enabled := False;
    FDeviceList.Clear;
  end;
end;

procedure TFormMain.LoadDevicesDelayed;
begin
  // Reset and start the delayed load timer
  FDelayedLoadTimer.Enabled := False;
  FDelayedLoadTimer.Enabled := True;
end;

procedure TFormMain.HandleDelayedLoadTimer(Sender: TObject);
begin
  FDelayedLoadTimer.Enabled := False;
  LoadDevices;
end;

{ Tray icon support }

procedure TFormMain.ShowFromTray;
begin
  Log('[MainForm] ShowFromTray');

  // Position popup in Menu mode
  if Config.WindowMode = wmMenu then
    PositionMenuPopup;

  Show;
  WindowState := wsNormal;
  Application.BringToFront;
  SetForegroundWindow(Handle);

  // Update tray menu item caption
  if FTrayMenu.Items.Count > 0 then
    FTrayMenu.Items[0].Caption := 'Hide';
end;

procedure TFormMain.HideToTray;
begin
  Log('[MainForm] HideToTray');
  Hide;

  // Update tray menu item caption
  if FTrayMenu.Items.Count > 0 then
    FTrayMenu.Items[0].Caption := 'Show';
end;

procedure TFormMain.ApplyWindowMode;
begin
  if Config.WindowMode = wmMenu then
  begin
    // Borderless popup style
    BorderStyle := bsNone;
    BorderIcons := [];

    // Hide from taskbar - must be done after handle is created
    // We'll apply WS_EX_TOOLWINDOW in FormCreate after handle exists
    Log('[MainForm] ApplyWindowMode: Menu mode (borderless popup)');
  end
  else
  begin
    // Normal window style
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize];
    Log('[MainForm] ApplyWindowMode: Window mode (normal window)');
  end;
end;

procedure TFormMain.ApplyMenuModeTaskbarHide;
var
  ExStyle: LONG_PTR;
begin
  // Hide from taskbar by adding WS_EX_TOOLWINDOW style
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
var
  CursorPos: TPoint;
  Mon: TMonitor;
  WorkArea: TRect;
  NewLeft, NewTop: Integer;
  TrayWnd: HWND;
  TrayRect: TRect;
  TaskbarWidth, TaskbarHeight: Integer;
  IsHorizontal: Boolean;
  FormWidth, FormHeight: Integer;
begin
  // Ensure window handle exists so dimensions are accurate
  if not HandleAllocated then
    HandleNeeded;

  // Use actual form dimensions
  FormWidth := Width;
  FormHeight := Height;

  Log('[MainForm] PositionMenuPopup: Using FormWidth=%d, FormHeight=%d', [FormWidth, FormHeight]);

  // Get cursor position and find which monitor it's on
  GetCursorPos(CursorPos);
  Mon := Screen.MonitorFromPoint(CursorPos);
  if Mon <> nil then
    WorkArea := Mon.WorkareaRect
  else
    WorkArea := Screen.WorkAreaRect;

  case Config.MenuPosition of
    mpNearCursor:
      begin
        // Position popup above the cursor, centered horizontally
        NewLeft := CursorPos.X - (FormWidth div 2);
        NewTop := CursorPos.Y - FormHeight - 10;
        Log('[MainForm] PositionMenuPopup: Near cursor (%d, %d)', [CursorPos.X, CursorPos.Y]);
      end;

    mpNearTray:
      begin
        // Find the actual taskbar/tray position
        TrayWnd := FindWindow('Shell_TrayWnd', nil);
        if (TrayWnd <> 0) and GetWindowRect(TrayWnd, TrayRect) then
        begin
          TaskbarWidth := TrayRect.Right - TrayRect.Left;
          TaskbarHeight := TrayRect.Bottom - TrayRect.Top;
          IsHorizontal := TaskbarWidth > TaskbarHeight;

          Log('[MainForm] PositionMenuPopup: Taskbar at L=%d,T=%d,R=%d,B=%d, Horizontal=%s',
            [TrayRect.Left, TrayRect.Top, TrayRect.Right, TrayRect.Bottom, BoolToStr(IsHorizontal, True)]);

          // Get work area of monitor containing the taskbar
          Mon := Screen.MonitorFromRect(TrayRect);
          if Mon <> nil then
            WorkArea := Mon.WorkareaRect;

          if IsHorizontal then
          begin
            // Horizontal taskbar (top or bottom)
            if TrayRect.Top < WorkArea.Top then
            begin
              // Taskbar at top - position popup below taskbar, near right edge
              NewLeft := TrayRect.Right - FormWidth - 10;
              NewTop := TrayRect.Bottom + 10;
              Log('[MainForm] PositionMenuPopup: Taskbar at TOP');
            end
            else
            begin
              // Taskbar at bottom - position popup above taskbar, near right edge
              NewLeft := TrayRect.Right - FormWidth - 10;
              NewTop := TrayRect.Top - FormHeight - 10;
              Log('[MainForm] PositionMenuPopup: Taskbar at BOTTOM');
            end;
          end
          else
          begin
            // Vertical taskbar (left or right)
            if TrayRect.Left < WorkArea.Left then
            begin
              // Taskbar at left - position popup to the right of taskbar, near bottom
              NewLeft := TrayRect.Right + 10;
              NewTop := TrayRect.Bottom - FormHeight - 10;
              Log('[MainForm] PositionMenuPopup: Taskbar at LEFT');
            end
            else
            begin
              // Taskbar at right - position popup to the left of taskbar, near bottom
              NewLeft := TrayRect.Left - FormWidth - 10;
              NewTop := TrayRect.Bottom - FormHeight - 10;
              Log('[MainForm] PositionMenuPopup: Taskbar at RIGHT');
            end;
          end;
        end
        else
        begin
          // Fallback: position at bottom-right of cursor's monitor work area
          NewLeft := WorkArea.Right - FormWidth - 10;
          NewTop := WorkArea.Bottom - FormHeight - 10;
          Log('[MainForm] PositionMenuPopup: Near tray (fallback to bottom-right)');
        end;
      end;

    mpCenterScreen:
      begin
        // Center on the monitor where cursor is
        NewLeft := WorkArea.Left + (WorkArea.Width - FormWidth) div 2;
        NewTop := WorkArea.Top + (WorkArea.Height - FormHeight) div 2;
        Log('[MainForm] PositionMenuPopup: Center screen');
      end;

    mpSameAsWindow:
      begin
        // Use saved position (already applied in FormCreate)
        NewLeft := Left;
        NewTop := Top;
        Log('[MainForm] PositionMenuPopup: Same as window');
      end;
  else
    NewLeft := Left;
    NewTop := Top;
  end;

  Log('[MainForm] PositionMenuPopup: Before bounds check: NewLeft=%d, NewTop=%d, WorkArea=(%d,%d,%d,%d)',
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

  Log('[MainForm] PositionMenuPopup: Final position: Left=%d, Top=%d (Bottom=%d)',
    [NewLeft, NewTop, NewTop + FormHeight]);

  Left := NewLeft;
  Top := NewTop;
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  // In Menu mode, hide when focus is lost (clicking outside)
  if Config.WindowMode = wmMenu then
  begin
    Log('[MainForm] FormDeactivate: Menu mode, hiding to tray');
    HideToTray;
  end;
end;

procedure TFormMain.HandleTrayIconClick(Sender: TObject);
begin
  if Visible then
    HideToTray
  else
    ShowFromTray;
end;

procedure TFormMain.HandleTrayMenuShowClick(Sender: TObject);
begin
  if Visible then
    HideToTray
  else
    ShowFromTray;
end;

procedure TFormMain.HandleTrayMenuExitClick(Sender: TObject);
begin
  Log('[MainForm] HandleTrayMenuExitClick: Forcing close');
  FForceClose := True;
  Close;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // If CloseToTray is enabled and not forcing close, hide to tray instead
  if Config.CloseToTray and (not FForceClose) then
  begin
    Log('[MainForm] FormCloseQuery: CloseToTray enabled, hiding to tray');
    CanClose := False;
    HideToTray;
  end
  else
  begin
    Log('[MainForm] FormCloseQuery: Allowing close (ForceClose=%s, CloseToTray=%s)',
      [BoolToStr(FForceClose, True), BoolToStr(Config.CloseToTray, True)]);
    CanClose := True;
  end;
end;

procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  // Intercept minimize command
  if (Msg.CmdType and $FFF0) = SC_MINIMIZE then
  begin
    if Config.MinimizeToTray then
    begin
      Log('[MainForm] WMSysCommand: MinimizeToTray enabled, hiding to tray');
      HideToTray;
      Msg.Result := 0;
      Exit;
    end;
  end;

  inherited;
end;

end.
