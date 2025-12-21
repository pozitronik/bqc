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
  UI.DeviceList,
  UI.TrayManager,
  UI.HotkeyManager;

const
  WM_DPICHANGED = $02E0;

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
    FTrayManager: TTrayManager;
    FHotkeyManager: THotkeyManager;
    FForceClose: Boolean;

    procedure CreateDeviceList;
    procedure HandleHotkeyTriggered(Sender: TObject);
    procedure SetToggleState(AState: TToggleSwitchState);
    procedure ApplyTheme;
    procedure ApplyConfiguredTheme;
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
    procedure HandleTrayToggleVisibility(Sender: TObject);
    procedure HandleTrayExitRequest(Sender: TObject);
    procedure HandleApplicationDeactivate(Sender: TObject);

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
  Bluetooth.Service,
  App.Logger,
  App.Config,
  UI.WindowPositioner;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  RadioEnabled: Boolean;
begin
  // Load configuration early (this also applies LoggingEnabled setting)
  Config;

  Log('[MainForm] FormCreate: Starting');

  FForceClose := False;

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

  // Load external VCL styles from VsfDir
  Theme.LoadStylesFromDirectory(Config.VsfDir);

  // Apply configured theme
  ApplyConfiguredTheme;

  // Subscribe to theme changes
  Theme.OnThemeChanged := HandleThemeChanged;

  // Subscribe to application deactivation for menu hide-on-focus-loss
  Application.OnDeactivate := HandleApplicationDeactivate;

  // Create custom device list control
  CreateDeviceList;

  // Create tray manager
  FTrayManager := TTrayManager.Create(Self);
  FTrayManager.OnToggleVisibility := HandleTrayToggleVisibility;
  FTrayManager.OnExitRequest := HandleTrayExitRequest;

  // Apply configuration settings to device list
  FDeviceList.ShowAddresses := Config.ShowAddresses;

  // Apply theme colors to custom controls
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

  // Create and register global hotkey if configured
  FHotkeyManager := THotkeyManager.Create;
  FHotkeyManager.OnHotkeyTriggered := HandleHotkeyTriggered;
  FHotkeyManager.Register(Handle, Config.Hotkey, Config.UseLowLevelHook);

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

  // Free hotkey manager (unregisters hotkey automatically)
  FHotkeyManager.Free;

  Theme.OnThemeChanged := nil;
  Application.OnDeactivate := nil;

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

procedure TFormMain.HandleHotkeyTriggered(Sender: TObject);
begin
  // Toggle visibility when global hotkey is triggered
  if Visible and (WindowState <> wsMinimized) then
    HideToTray
  else
    ShowFromTray;
end;

procedure TFormMain.WMHotkey(var Msg: TMessage);
begin
  // Delegate to hotkey manager
  FHotkeyManager.HandleWMHotkey(Msg.WParam);
end;

procedure TFormMain.WMHotkeyDetected(var Msg: TMessage);
begin
  // Delegate to hotkey manager
  FHotkeyManager.HandleHotkeyDetected;
end;

procedure TFormMain.WMDpiChanged(var Msg: TMessage);
var
  NewDPI: Word;
begin
  NewDPI := LoWord(Msg.WParam);
  Log('[MainForm] WMDpiChanged: New DPI=%d, current size W=%d, H=%d', [NewDPI, Width, Height]);

  // Let VCL handle the DPI change (resize form, scale controls)
  inherited;

  Log('[MainForm] WMDpiChanged: After inherited, size W=%d, H=%d', [Width, Height]);

  // After DPI change and form resize, reposition if in Menu mode
  if (Config.WindowMode = wmMenu) and Visible then
  begin
    Log('[MainForm] WMDpiChanged: Repositioning popup after DPI change');
    PositionMenuPopup;
  end;
end;

{ TODO: Multi-monitor DPI scaling issue
  ============================================================================
  KNOWN ISSUE:
  When monitors have different DPI scales (e.g., 150% and 200%), the menu popup
  may be positioned incorrectly on its FIRST appearance after the taskbar moves
  to a different monitor. Subsequent appearances work correctly.

  ROOT CAUSE:
  The form dimensions are calculated at the DPI of the monitor where the form
  was last shown. When positioning for a different DPI monitor, we use stale
  dimensions. WM_DPICHANGED fires AFTER Show, so the initial position is wrong.

  ATTEMPTED SOLUTIONS:
  1. HandleNeeded before positioning - doesn't trigger DPI update
  2. SetBounds with config dimensions - wrong dimensions for Menu mode
  3. WM_DPICHANGED handler with reposition - fires too late, flicker still occurs

  POTENTIAL SOLUTIONS TO TRY:
  1. Use PostMessage in WMDpiChanged to defer repositioning until after all
     DPI processing is complete

  2. Pre-calculate scaled dimensions before positioning:
     - Use GetDpiForMonitor() to get target monitor DPI
     - Use GetDpiForWindow() to get current form DPI
     - Scale Width/Height by (TargetDPI / CurrentDPI) ratio

  3. Use SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2)
     around positioning code to ensure correct DPI context

  4. Show form off-screen first, wait for DPI change, then position and show:
     - Set position to (-10000, -10000)
     - Call Show
     - In WM_DPICHANGED or via PostMessage, position correctly
     - Bring to foreground

  5. Query Windows for the "would-be" size at target DPI without showing

  WORKAROUND FOR USERS:
  The issue only occurs on first show after taskbar moves between monitors
  with different DPI. Triggering the hotkey twice (hide then show) will
  display the menu at the correct position.
  ============================================================================
}

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

procedure TFormMain.ApplyConfiguredTheme;
var
  ThemeSetting: string;
begin
  ThemeSetting := Config.Theme;
  Log('[MainForm] ApplyConfiguredTheme: Theme setting="%s"', [ThemeSetting]);

  // Handle special theme modes
  if SameText(ThemeSetting, 'System') then
    Theme.SetThemeMode(tmSystem)
  else if SameText(ThemeSetting, 'Light') then
    Theme.SetThemeMode(tmLight)
  else if SameText(ThemeSetting, 'Dark') then
    Theme.SetThemeMode(tmDark)
  else
  begin
    // Treat as specific style name
    Theme.SetStyle(ThemeSetting);
  end;
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
      NotifyMode: TNotificationMode;
      DeviceName: string;
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

      // Get device display name (alias if set, otherwise device name)
      DeviceName := Config.GetDeviceConfig(LDevice.AddressInt).Alias;
      if DeviceName = '' then
        DeviceName := LDevice.Name;

      // Show balloon notification based on connection state
      case LDevice.ConnectionState of
        csConnected:
          begin
            NotifyMode := Config.GetEffectiveNotification(LDevice.AddressInt, 'Connect');
            if NotifyMode = nmBalloon then
              FTrayManager.ShowNotification(DeviceName, 'Connected', bfInfo);
          end;
        csDisconnected:
          begin
            NotifyMode := Config.GetEffectiveNotification(LDevice.AddressInt, 'Disconnect');
            if NotifyMode = nmBalloon then
              FTrayManager.ShowNotification(DeviceName, 'Disconnected', bfInfo);
          end;
        csError:
          begin
            NotifyMode := Config.GetEffectiveNotification(LDevice.AddressInt, 'ConnectFailed');
            if NotifyMode = nmBalloon then
              FTrayManager.ShowNotification(DeviceName, 'Connection failed', bfError);
          end;
      end;
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

  // Ensure form gets focus - especially important in Menu mode
  Application.BringToFront;
  SetForegroundWindow(Handle);
  // Explicitly activate and focus the form
  BringToFront;
  if CanFocus then
    SetFocus;

  // Update tray menu item caption
  FTrayManager.UpdateMenuCaption(True);
end;

procedure TFormMain.HideToTray;
begin
  Log('[MainForm] HideToTray');
  Hide;

  // Update tray menu item caption
  FTrayManager.UpdateMenuCaption(False);
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
begin
  TWindowPositioner.PositionMenuPopup(Self, Config.MenuPosition);
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  // Form deactivation is less reliable for fsStayOnTop forms.
  // Application.OnDeactivate (HandleApplicationDeactivate) is the primary mechanism.
  // This handler is kept as a backup for some edge cases.
  if (Config.WindowMode = wmMenu) and Config.MenuHideOnFocusLoss then
  begin
    Log('[MainForm] FormDeactivate: Menu mode with MenuHideOnFocusLoss, hiding to tray');
    HideToTray;
  end;
end;

procedure TFormMain.HandleApplicationDeactivate(Sender: TObject);
begin
  // Application lost focus - more reliable than Form.OnDeactivate for fsStayOnTop
  if (Config.WindowMode = wmMenu) and Config.MenuHideOnFocusLoss and Visible then
  begin
    Log('[MainForm] HandleApplicationDeactivate: Menu mode with MenuHideOnFocusLoss, hiding to tray');
    HideToTray;
  end;
end;

procedure TFormMain.HandleTrayToggleVisibility(Sender: TObject);
begin
  if Visible then
    HideToTray
  else
    ShowFromTray;
end;

procedure TFormMain.HandleTrayExitRequest(Sender: TObject);
begin
  Log('[MainForm] HandleTrayExitRequest: Forcing close');
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
