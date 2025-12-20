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
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  UI.Theme,
  UI.DeviceList;

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

    procedure CreateDeviceList;
    procedure SetToggleState(AState: TToggleSwitchState);
    procedure ApplyTheme;
    procedure LoadDevices;
    procedure LoadDevicesDelayed;
    procedure AutoConnectDevices;
    procedure UpdateStatus(const AMessage: string);

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleThemeChanged(Sender: TObject);
    procedure HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
    procedure HandleDelayedLoadTimer(Sender: TObject);

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

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  RadioEnabled: Boolean;
begin
  // Load configuration early (this also applies LoggingEnabled setting)
  Config;

  Log('[MainForm] FormCreate: Starting');

  // Restore window position from configuration
  if (Config.WindowX >= 0) and (Config.WindowY >= 0) then
  begin
    Position := poDesigned;
    Left := Config.WindowX;
    Top := Config.WindowY;
    Log('[MainForm] FormCreate: Restored position X=%d, Y=%d', [Left, Top]);
  end;

  // Restore window size from configuration
  if (Config.WindowWidth > 0) and (Config.WindowHeight > 0) then
  begin
    Width := Config.WindowWidth;
    Height := Config.WindowHeight;
    Log('[MainForm] FormCreate: Restored size W=%d, H=%d', [Width, Height]);
  end;

  // Apply StayOnTop setting
  if Config.StayOnTop then
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

  Log('[MainForm] FormCreate: Complete');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Save window position and size to configuration
  if WindowState = wsNormal then
  begin
    Config.WindowX := Left;
    Config.WindowY := Top;
    Config.WindowWidth := Width;
    Config.WindowHeight := Height;
    Log('[MainForm] FormDestroy: Saved position X=%d, Y=%d, W=%d, H=%d', [Left, Top, Width, Height]);
  end;

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

end.
