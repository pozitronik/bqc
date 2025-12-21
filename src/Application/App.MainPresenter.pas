{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Presenter (MVP Pattern)                    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.MainPresenter;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.ExtCtrls,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl;

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
    /// Displays the list of Bluetooth devices.
    /// </summary>
    procedure ShowDevices(const ADevices: TBluetoothDeviceInfoArray);

    /// <summary>
    /// Updates a single device in the list.
    /// </summary>
    procedure UpdateDevice(const ADevice: TBluetoothDeviceInfo);

    /// <summary>
    /// Adds a new device to the list.
    /// </summary>
    procedure AddDevice(const ADevice: TBluetoothDeviceInfo);

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

  /// <summary>
  /// Main presenter handling business logic for the Bluetooth device management.
  /// Coordinates between the View (Form) and the Model (Services).
  /// </summary>
  TMainPresenter = class
  private
    FView: IMainView;
    FBluetoothService: IBluetoothService;
    FRadioWatcher: TBluetoothRadioWatcher;
    FDevices: TBluetoothDeviceInfoArray;
    FDelayedLoadTimer: TTimer;
    FUpdatingToggle: Boolean;

    { Service event handlers }
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
    procedure HandleDelayedLoadTimer(Sender: TObject);

    { Internal methods }
    procedure LoadDevices;
    procedure LoadDevicesDelayed;
    procedure AutoConnectDevices;
    procedure ConnectDeviceAsync(const ADevice: TBluetoothDeviceInfo);
    procedure ToggleConnectionAsync(const ADevice: TBluetoothDeviceInfo);
    procedure SetRadioStateAsync(AEnable: Boolean);

    function GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;
    procedure ShowDeviceNotification(const ADevice: TBluetoothDeviceInfo);

  public
    /// <summary>
    /// Creates the presenter with a reference to the view.
    /// </summary>
    /// <param name="AView">The view interface (implemented by Form).</param>
    constructor Create(AView: IMainView);

    /// <summary>
    /// Destroys the presenter and releases resources.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    /// Initializes the presenter after the view is ready.
    /// Creates services, checks radio state, starts watchers.
    /// </summary>
    procedure Initialize;

    /// <summary>
    /// Shuts down the presenter before the view closes.
    /// Stops watchers, releases services.
    /// </summary>
    procedure Shutdown;

    /// <summary>
    /// Called when user clicks a device in the list.
    /// Toggles connection state.
    /// </summary>
    procedure OnDeviceClicked(const ADevice: TBluetoothDeviceInfo);

    /// <summary>
    /// Called when user toggles the Bluetooth switch.
    /// Enables or disables Bluetooth radio.
    /// </summary>
    procedure OnToggleChanged(AEnabled: Boolean);

    /// <summary>
    /// Called when user requests a refresh.
    /// Reloads the device list.
    /// </summary>
    procedure OnRefreshRequested;

    /// <summary>
    /// Called when visibility toggle is requested (hotkey, tray click).
    /// Shows or hides the view.
    /// </summary>
    procedure OnVisibilityToggleRequested;

    /// <summary>
    /// Called when user requests to exit the application.
    /// </summary>
    procedure OnExitRequested;

    /// <summary>
    /// Checks if the view can close or should hide to tray.
    /// </summary>
    /// <returns>True if view can close, False if it should hide.</returns>
    function CanClose: Boolean;

    /// <summary>
    /// Returns True if Bluetooth toggle is being updated programmatically.
    /// </summary>
    property IsUpdatingToggle: Boolean read FUpdatingToggle;
  end;

implementation

uses
  App.Logger,
  App.Config,
  Bluetooth.Service;

{ TMainPresenter }

constructor TMainPresenter.Create(AView: IMainView);
begin
  inherited Create;
  FView := AView;
  FBluetoothService := nil;
  FRadioWatcher := nil;
  FDevices := nil;
  FDelayedLoadTimer := nil;
  FUpdatingToggle := False;
  Log('[MainPresenter] Created');
end;

destructor TMainPresenter.Destroy;
begin
  Shutdown;
  Log('[MainPresenter] Destroyed');
  inherited;
end;

procedure TMainPresenter.Initialize;
var
  RadioEnabled: Boolean;
begin
  Log('[MainPresenter] Initialize: Starting');

  // Create delayed load timer
  FDelayedLoadTimer := TTimer.Create(nil);
  FDelayedLoadTimer.Enabled := False;
  FDelayedLoadTimer.Interval := 500;
  FDelayedLoadTimer.OnTimer := HandleDelayedLoadTimer;

  // Create Bluetooth service
  Log('[MainPresenter] Initialize: Creating Bluetooth service');
  FBluetoothService := CreateBluetoothService;
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;
  Log('[MainPresenter] Initialize: Bluetooth service created');

  // Check Bluetooth radio state
  if GetBluetoothRadioState(RadioEnabled) then
  begin
    Log('[MainPresenter] Initialize: Radio state: Enabled=%s', [BoolToStr(RadioEnabled, True)]);

    if RadioEnabled then
    begin
      FView.SetToggleState(True);
      FView.ShowStatus('Loading devices...');
      LoadDevices;
      AutoConnectDevices;
    end
    else
    begin
      FView.SetToggleState(False);
      FView.ShowStatus('Bluetooth is off');
      FView.ClearDevices;
    end;

    // Start watching for radio state changes
    FRadioWatcher := TBluetoothRadioWatcher.Create;
    FRadioWatcher.OnStateChanged := HandleRadioStateChanged;
    FRadioWatcher.Start;
    Log('[MainPresenter] Initialize: Radio watcher started');
  end
  else
  begin
    Log('[MainPresenter] Initialize: No Bluetooth adapter found');
    FView.SetToggleState(False);
    FView.SetToggleEnabled(False);
    FView.ShowStatus('No Bluetooth adapter found');
  end;

  Log('[MainPresenter] Initialize: Complete');
end;

procedure TMainPresenter.Shutdown;
begin
  Log('[MainPresenter] Shutdown: Starting');

  // Stop delayed load timer
  if FDelayedLoadTimer <> nil then
  begin
    FDelayedLoadTimer.Enabled := False;
    FDelayedLoadTimer.Free;
    FDelayedLoadTimer := nil;
  end;

  // Stop and free radio watcher
  if FRadioWatcher <> nil then
  begin
    FRadioWatcher.Stop;
    FRadioWatcher.Free;
    FRadioWatcher := nil;
  end;

  // Release service
  FBluetoothService := nil;
  FDevices := nil;

  Log('[MainPresenter] Shutdown: Complete');
end;

procedure TMainPresenter.LoadDevices;
var
  I: Integer;
begin
  Log('[MainPresenter] LoadDevices: Starting');
  FView.SetBusy(True);
  try
    FDevices := FBluetoothService.GetPairedDevices;
    Log('[MainPresenter] LoadDevices: Got %d devices', [Length(FDevices)]);

    for I := 0 to High(FDevices) do
      Log('[MainPresenter] LoadDevices: Device[%d] Address=$%.12X, Name="%s", Connected=%s', [
        I, FDevices[I].AddressInt, FDevices[I].Name, BoolToStr(FDevices[I].IsConnected, True)
      ]);

    FView.ShowDevices(FDevices);

    if Length(FDevices) = 0 then
      FView.ShowStatus('No paired devices')
    else
      FView.ShowStatus(Format('%d device(s)', [Length(FDevices)]));
  finally
    FView.SetBusy(False);
  end;
  Log('[MainPresenter] LoadDevices: Complete');
end;

procedure TMainPresenter.LoadDevicesDelayed;
begin
  FDelayedLoadTimer.Enabled := False;
  FDelayedLoadTimer.Enabled := True;
end;

procedure TMainPresenter.AutoConnectDevices;
var
  I: Integer;
  DeviceConfig: TDeviceConfig;
  Device: TBluetoothDeviceInfo;
begin
  Log('[MainPresenter] AutoConnectDevices: Starting');

  for I := 0 to High(FDevices) do
  begin
    Device := FDevices[I];
    DeviceConfig := Config.GetDeviceConfig(Device.AddressInt);

    if not DeviceConfig.AutoConnect then
      Continue;

    if Device.IsConnected then
    begin
      Log('[MainPresenter] AutoConnectDevices: %s already connected, skipping', [Device.Name]);
      Continue;
    end;

    Log('[MainPresenter] AutoConnectDevices: Auto-connecting %s', [Device.Name]);
    FView.ShowStatus(Format('Auto-connecting %s...', [Device.Name]));
    ConnectDeviceAsync(Device);
  end;

  Log('[MainPresenter] AutoConnectDevices: Complete');
end;

procedure TMainPresenter.ConnectDeviceAsync(const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
  LService: IBluetoothService;
begin
  LDevice := ADevice;
  LService := FBluetoothService;
  TThread.CreateAnonymousThread(
    procedure
    begin
      LService.Connect(LDevice);
    end
  ).Start;
end;

procedure TMainPresenter.ToggleConnectionAsync(const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
  LService: IBluetoothService;
begin
  LDevice := ADevice;
  LService := FBluetoothService;
  TThread.CreateAnonymousThread(
    procedure
    begin
      Log('[MainPresenter] ToggleConnectionAsync: Calling ToggleConnection');
      LService.ToggleConnection(LDevice);
      Log('[MainPresenter] ToggleConnectionAsync: Complete');
    end
  ).Start;
end;

procedure TMainPresenter.SetRadioStateAsync(AEnable: Boolean);
var
  LView: IMainView;
begin
  LView := FView;
  TThread.CreateAnonymousThread(
    procedure
    var
      LResult: TRadioControlResult;
    begin
      LResult := SetBluetoothRadioState(AEnable);

      TThread.Queue(nil,
        procedure
        begin
          case LResult of
            rcSuccess:
              begin
                if AEnable then
                begin
                  LView.ShowStatus('Bluetooth enabled');
                  LoadDevices;
                end
                else
                begin
                  LView.ShowStatus('Bluetooth disabled');
                  LView.ClearDevices;
                end;
              end;
            rcAccessDenied:
              begin
                LView.ShowStatus('Access denied - check Windows settings');
                FUpdatingToggle := True;
                try
                  LView.SetToggleState(not AEnable);
                finally
                  FUpdatingToggle := False;
                end;
              end;
            rcDeviceNotFound:
              begin
                LView.ShowStatus('Bluetooth adapter not found');
                FUpdatingToggle := True;
                try
                  LView.SetToggleState(False);
                finally
                  FUpdatingToggle := False;
                end;
                LView.SetToggleEnabled(False);
              end;
          else
            begin
              LView.ShowStatus('Failed to change Bluetooth state');
              FUpdatingToggle := True;
              try
                LView.SetToggleState(not AEnable);
              finally
                FUpdatingToggle := False;
              end;
            end;
          end;
        end
      );
    end
  ).Start;
end;

function TMainPresenter.GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;
begin
  Result := Config.GetDeviceConfig(ADevice.AddressInt).Alias;
  if Result = '' then
    Result := ADevice.Name;
end;

procedure TMainPresenter.ShowDeviceNotification(const ADevice: TBluetoothDeviceInfo);
var
  NotifyMode: TNotificationMode;
  DeviceName: string;
begin
  DeviceName := GetDeviceDisplayName(ADevice);

  case ADevice.ConnectionState of
    csConnected:
      begin
        NotifyMode := Config.GetEffectiveNotification(ADevice.AddressInt, 'Connect');
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Connected', nfInfo);
      end;
    csDisconnected:
      begin
        NotifyMode := Config.GetEffectiveNotification(ADevice.AddressInt, 'Disconnect');
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Disconnected', nfInfo);
      end;
    csError:
      begin
        NotifyMode := Config.GetEffectiveNotification(ADevice.AddressInt, 'ConnectFailed');
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Connection failed', nfError);
      end;
  end;
end;

{ Service event handlers }

procedure TMainPresenter.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
begin
  Log('[MainPresenter] HandleDeviceStateChanged: Address=$%.12X, Name="%s", State=%d', [
    ADevice.AddressInt, ADevice.Name, Ord(ADevice.ConnectionState)
  ]);

  if Trim(ADevice.Name) = '' then
  begin
    Log('[MainPresenter] HandleDeviceStateChanged: Empty name, skipping');
    Exit;
  end;

  LDevice := ADevice;

  TThread.Queue(nil,
    procedure
    var
      I: Integer;
      Found: Boolean;
    begin
      Log('[MainPresenter] HandleDeviceStateChanged (queued): Processing %s', [LDevice.Name]);

      Found := False;

      // Update local cache
      for I := 0 to High(FDevices) do
      begin
        if FDevices[I].AddressInt = LDevice.AddressInt then
        begin
          FDevices[I] := LDevice;
          Found := True;
          Break;
        end;
      end;

      // Add if not found
      if not Found then
      begin
        SetLength(FDevices, Length(FDevices) + 1);
        FDevices[High(FDevices)] := LDevice;
        FView.AddDevice(LDevice);
      end
      else
      begin
        FView.UpdateDevice(LDevice);
      end;

      FView.ShowStatus(Format('%s: %s', [LDevice.Name, LDevice.ConnectionStateText]));
      ShowDeviceNotification(LDevice);
    end
  );
end;

procedure TMainPresenter.HandleDeviceListChanged(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      LoadDevices;
    end
  );
end;

procedure TMainPresenter.HandleError(Sender: TObject; const AMessage: string;
  AErrorCode: Cardinal);
begin
  TThread.Queue(nil,
    procedure
    begin
      if AErrorCode <> 0 then
        FView.ShowStatus(Format('Error: %s (%d)', [AMessage, AErrorCode]))
      else
        FView.ShowStatus(Format('Error: %s', [AMessage]));
    end
  );
end;

procedure TMainPresenter.HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
begin
  Log('[MainPresenter] HandleRadioStateChanged: Enabled=%s', [BoolToStr(AEnabled, True)]);

  FUpdatingToggle := True;
  try
    FView.SetToggleState(AEnabled);
  finally
    FUpdatingToggle := False;
  end;

  if AEnabled then
  begin
    FView.ShowStatus('Loading devices...');
    LoadDevicesDelayed;
  end
  else
  begin
    FView.ShowStatus('Bluetooth disabled');
    if FDelayedLoadTimer <> nil then
      FDelayedLoadTimer.Enabled := False;
    FView.ClearDevices;
  end;
end;

procedure TMainPresenter.HandleDelayedLoadTimer(Sender: TObject);
begin
  FDelayedLoadTimer.Enabled := False;
  LoadDevices;
end;

{ Public methods called by View }

procedure TMainPresenter.OnDeviceClicked(const ADevice: TBluetoothDeviceInfo);
begin
  Log('[MainPresenter] OnDeviceClicked: %s, State=%d', [ADevice.Name, Ord(ADevice.ConnectionState)]);

  if ADevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    Log('[MainPresenter] OnDeviceClicked: Operation in progress, ignoring');
    FView.ShowStatus('Operation in progress...');
    Exit;
  end;

  if ADevice.IsConnected then
    FView.ShowStatus(Format('Disconnecting %s...', [ADevice.Name]))
  else
    FView.ShowStatus(Format('Connecting %s...', [ADevice.Name]));

  // In Menu mode, hide immediately after device click
  if Config.WindowMode = wmMenu then
  begin
    Log('[MainPresenter] OnDeviceClicked: Menu mode, hiding view');
    FView.HideView;
  end;

  ToggleConnectionAsync(ADevice);
end;

procedure TMainPresenter.OnToggleChanged(AEnabled: Boolean);
begin
  if FUpdatingToggle then
    Exit;

  Log('[MainPresenter] OnToggleChanged: Enabled=%s', [BoolToStr(AEnabled, True)]);

  if AEnabled then
    FView.ShowStatus('Enabling Bluetooth...')
  else
    FView.ShowStatus('Disabling Bluetooth...');

  SetRadioStateAsync(AEnabled);
end;

procedure TMainPresenter.OnRefreshRequested;
begin
  Log('[MainPresenter] OnRefreshRequested');
  FView.ShowStatus('Refreshing...');
  LoadDevices;
end;

procedure TMainPresenter.OnVisibilityToggleRequested;
begin
  if FView.IsVisible and (not FView.IsMinimized) then
    FView.HideView
  else
    FView.ShowView;
end;

procedure TMainPresenter.OnExitRequested;
begin
  Log('[MainPresenter] OnExitRequested');
  FView.ForceClose;
end;

function TMainPresenter.CanClose: Boolean;
begin
  Result := not Config.CloseToTray;
end;

end.
