{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Presenter (MVP Pattern)                    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.MainPresenter;

{ TODO: MainPresenter splitting analysis (2024-12)

  This file was analyzed for potential splitting into focused presenters.
  After analysis, the current monolithic structure was retained because:

  1. ALREADY WELL-DELEGATED: The presenter properly delegates to:
     - FBluetoothService for device operations
     - FDisplayItemBuilder for building display items
     - Bootstrap.* providers for configuration access
     The presenter itself is a thin coordinator, not a "god class".

  2. COHESIVE RESPONSIBILITIES: All methods relate to a single concern -
     coordinating between IMainView and Bluetooth services. This is exactly
     what a main presenter in MVP should do.

  3. REASONABLE SIZE: 515 lines of implementation (excluding interface) is
     acceptable for a main coordinator. Most methods are short and focused.

  4. SPLITTING WOULD OVER-ENGINEER: Potential extractions would create
     very small classes with minimal benefit:
     - TRadioController: ~60 lines (SetRadioStateAsync, HandleRadioStateChanged)
     - TNotificationService: ~30 lines (ShowDeviceNotification)
     - TDeviceAutoConnector: ~25 lines (AutoConnectDevices)
     This adds indirection without significant testability or maintainability gain.

  When to reconsider this decision:
  - If the file grows beyond ~1000 lines of implementation
  - If new major features are added (e.g., device grouping, profiles)
  - If testing requires mocking specific presenter behaviors
  - If multiple views need to share subset of presenter logic

  Potential future improvements:
  - Consider command pattern for async operations if they grow complex
  - Add presenter tests for state management logic }

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.ExtCtrls,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  App.MainViewInterfaces,
  UI.DeviceList,
  UI.DeviceDisplayItemBuilder;

type
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
    FDisplayItems: TDeviceDisplayItemArray;
    FDisplayItemBuilder: TDeviceDisplayItemBuilder;
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
    procedure RefreshDisplayItems;

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
    /// Called when settings have been changed.
    /// Restarts Bluetooth monitoring if polling settings changed.
    /// </summary>
    procedure OnSettingsChanged;

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
  App.ConfigInterfaces,
  App.Bootstrap,
  Bluetooth.Service;

{ TMainPresenter }

constructor TMainPresenter.Create(AView: IMainView);
begin
  inherited Create;
  FView := AView;
  FBluetoothService := nil;
  FRadioWatcher := nil;
  FDevices := nil;
  FDisplayItems := nil;
  FDisplayItemBuilder := TDeviceDisplayItemBuilder.Create(
    Bootstrap.DeviceConfigProvider,
    Bootstrap.AppearanceConfig
  );
  FDelayedLoadTimer := nil;
  FUpdatingToggle := False;
  Log('[MainPresenter] Created');
end;

destructor TMainPresenter.Destroy;
begin
  Shutdown;
  FDisplayItemBuilder.Free;
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

    // Register all discovered devices to persistent config
    for I := 0 to High(FDevices) do
    begin
      Log('[MainPresenter] LoadDevices: Device[%d] Address=$%.12X, Name="%s", Connected=%s', [
        I, FDevices[I].AddressInt, FDevices[I].Name, BoolToStr(FDevices[I].IsConnected, True)
      ]);
      // Register device with current timestamp
      Bootstrap.DeviceConfigProvider.RegisterDevice(FDevices[I].AddressInt, FDevices[I].Name, Now);
    end;

    // Save config if any new devices were registered
    Bootstrap.AppConfig.SaveIfModified;

    // Build display items and send to view
    RefreshDisplayItems;

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
    DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Device.AddressInt);

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
  Result := Bootstrap.DeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt).Alias;
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
        NotifyMode := Bootstrap.DeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnect);
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Connected', nfInfo);
      end;
    csDisconnected:
      begin
        NotifyMode := Bootstrap.DeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neDisconnect);
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Disconnected', nfInfo);
      end;
    csError:
      begin
        NotifyMode := Bootstrap.DeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnectFailed);
        if NotifyMode = nmBalloon then
          FView.ShowNotification(DeviceName, 'Connection failed', nfError);
      end;
  end;
end;

procedure TMainPresenter.RefreshDisplayItems;
begin
  FDisplayItems := FDisplayItemBuilder.BuildDisplayItems(FDevices);
  FView.ShowDisplayItems(FDisplayItems);
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

      // Add to local cache if not found
      if not Found then
      begin
        SetLength(FDevices, Length(FDevices) + 1);
        FDevices[High(FDevices)] := LDevice;
      end;

      // Update LastSeen timestamp in persistent config
      Bootstrap.DeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, Now);
      Bootstrap.AppConfig.SaveIfModified;

      // Rebuild display items (device may have moved groups due to state change)
      RefreshDisplayItems;

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
  if Bootstrap.GeneralConfig.WindowMode = wmMenu then
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

procedure TMainPresenter.OnSettingsChanged;
begin
  Log('[MainPresenter] OnSettingsChanged');
  // Note: Polling mode changes require application restart to take effect.
  // Other settings (theme, hotkey, etc.) are applied immediately by MainForm.
end;

function TMainPresenter.CanClose: Boolean;
begin
  Result := not Bootstrap.WindowConfig.CloseToTray;
end;

end.
