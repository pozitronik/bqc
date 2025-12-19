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
    procedure UpdateStatus(const AMessage: string);

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleThemeChanged(Sender: TObject);
    procedure HandleRefreshClick(Sender: TObject);
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
  Bluetooth.Service;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  RadioEnabled: Boolean;
begin
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

  // Apply current theme
  ApplyTheme;

  // Create Bluetooth service
  FBluetoothService := CreateBluetoothService;
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;

  // Check Bluetooth radio state using WinRT API
  if GetBluetoothRadioState(RadioEnabled) then
  begin
    // Adapter exists, set toggle based on current state
    if RadioEnabled then
    begin
      SetToggleState(tssOn);
      UpdateStatus('Loading devices...');
      LoadDevices;
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
  end
  else
  begin
    // No Bluetooth adapter found
    SetToggleState(tssOff);
    BluetoothToggle.Enabled := False;
    UpdateStatus('No Bluetooth adapter found');
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
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
begin
  Screen.Cursor := crHourGlass;
  try
    FDevices := FBluetoothService.GetPairedDevices;
    FDeviceList.SetDevices(FDevices);

    if Length(FDevices) = 0 then
      UpdateStatus('No paired devices')
    else
      UpdateStatus(Format('%d device(s)', [Length(FDevices)]));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.UpdateStatus(const AMessage: string);
begin
  StatusLabel.Caption := AMessage;
end;

procedure TFormMain.HandleDeviceClick(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  if ADevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    UpdateStatus('Operation in progress...');
    Exit;
  end;

  if ADevice.IsConnected then
    UpdateStatus(Format('Disconnecting %s...', [ADevice.Name]))
  else
    UpdateStatus(Format('Connecting %s...', [ADevice.Name]));

  // Toggle connection in background thread
  TThread.CreateAnonymousThread(
    procedure
    begin
      FBluetoothService.ToggleConnection(ADevice);

      TThread.Queue(nil,
        procedure
        begin
          LoadDevices;
        end
      );
    end
  ).Start;
end;

procedure TFormMain.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  TThread.Queue(nil,
    procedure
    var
      I: Integer;
    begin
      // Update local cache
      for I := 0 to High(FDevices) do
      begin
        if FDevices[I].AddressInt = ADevice.AddressInt then
        begin
          FDevices[I] := ADevice;
          Break;
        end;
      end;

      // Update UI
      FDeviceList.UpdateDevice(ADevice);
      UpdateStatus(Format('%s: %s', [ADevice.Name, ADevice.ConnectionStateText]));
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
