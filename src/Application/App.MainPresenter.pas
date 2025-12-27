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
     - Injected config interfaces for configuration access
     The presenter itself is a thin coordinator, not a "god class".

  2. COHESIVE RESPONSIBILITIES: All methods relate to a single concern -
     coordinating between view interfaces and Bluetooth services. This is exactly
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
  App.ConfigInterfaces,
  App.AppearanceConfigIntf,
  UI.DeviceList,
  UI.DeviceDisplayItemBuilder;

type
  /// <summary>
  /// Main presenter handling business logic for the Bluetooth device management.
  /// Coordinates between the View (Form) and the Model (Services).
  /// Dependencies are injected via constructor for testability.
  /// </summary>
  TMainPresenter = class
  private
    { View interfaces (ISP-compliant) }
    FDeviceListView: IDeviceListView;
    FToggleView: IToggleView;
    FStatusView: IStatusView;
    FVisibilityView: IVisibilityView;

    FBluetoothService: IBluetoothService;

    { Injected configuration dependencies }
    FAppConfig: IAppConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FAppearanceConfig: IAppearanceConfig;
    FPollingConfig: IPollingConfig;
    FConnectionConfig: IConnectionConfig;
    FStrategyFactory: IConnectionStrategyFactory;
    FRadioStateManager: IRadioStateManager;
    FBatteryCache: IBatteryCache;
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
    procedure HandleBatteryQueryCompleted(Sender: TObject; ADeviceAddress: UInt64;
      const AStatus: TBatteryStatus);

    procedure RefreshBatteryForConnectedDevices;

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

    /// <summary>
    /// Updates existing device or adds new one using copy-on-write pattern.
    /// Creates a new array to avoid in-place modification issues.
    /// </summary>
    procedure UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);

  public
    /// <summary>
    /// Creates the presenter with injected dependencies.
    /// </summary>
    /// <param name="ADeviceListView">View for device list operations.</param>
    /// <param name="AToggleView">View for toggle switch operations.</param>
    /// <param name="AStatusView">View for status and notifications.</param>
    /// <param name="AVisibilityView">View for window visibility.</param>
    /// <param name="AAppConfig">Application configuration for persistence.</param>
    /// <param name="ADeviceConfigProvider">Device configuration provider.</param>
    /// <param name="AGeneralConfig">General settings (window mode).</param>
    /// <param name="AWindowConfig">Window settings (close to tray).</param>
    /// <param name="AAppearanceConfig">Appearance settings for display items.</param>
    /// <param name="APollingConfig">Polling settings for device monitor.</param>
    /// <param name="AConnectionConfig">Connection settings.</param>
    /// <param name="AStrategyFactory">Connection strategy factory.</param>
    /// <param name="ARadioStateManager">Bluetooth radio state manager.</param>
    constructor Create(
      ADeviceListView: IDeviceListView;
      AToggleView: IToggleView;
      AStatusView: IStatusView;
      AVisibilityView: IVisibilityView;
      AAppConfig: IAppConfig;
      ADeviceConfigProvider: IDeviceConfigProvider;
      AGeneralConfig: IGeneralConfig;
      AWindowConfig: IWindowConfig;
      AAppearanceConfig: IAppearanceConfig;
      APollingConfig: IPollingConfig;
      AConnectionConfig: IConnectionConfig;
      AStrategyFactory: IConnectionStrategyFactory;
      ARadioStateManager: IRadioStateManager
    );

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
    /// Called when the view becomes visible.
    /// Triggers battery level refresh for connected devices.
    /// </summary>
    procedure OnViewShown;

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
  App.ConfigEnums,
  Bluetooth.Service,
  Bluetooth.BatteryQuery,
  UI.DeviceFormatter;

{ TMainPresenter }

constructor TMainPresenter.Create(
  ADeviceListView: IDeviceListView;
  AToggleView: IToggleView;
  AStatusView: IStatusView;
  AVisibilityView: IVisibilityView;
  AAppConfig: IAppConfig;
  ADeviceConfigProvider: IDeviceConfigProvider;
  AGeneralConfig: IGeneralConfig;
  AWindowConfig: IWindowConfig;
  AAppearanceConfig: IAppearanceConfig;
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  AStrategyFactory: IConnectionStrategyFactory;
  ARadioStateManager: IRadioStateManager
);
begin
  inherited Create;

  // Store view interfaces
  FDeviceListView := ADeviceListView;
  FToggleView := AToggleView;
  FStatusView := AStatusView;
  FVisibilityView := AVisibilityView;

  // Store injected dependencies
  FAppConfig := AAppConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FGeneralConfig := AGeneralConfig;
  FWindowConfig := AWindowConfig;
  FAppearanceConfig := AAppearanceConfig;
  FPollingConfig := APollingConfig;
  FConnectionConfig := AConnectionConfig;
  FStrategyFactory := AStrategyFactory;
  FRadioStateManager := ARadioStateManager;

  FBluetoothService := nil;
  FBatteryCache := nil;
  FDevices := nil;
  FDisplayItems := nil;
  FDisplayItemBuilder := TDeviceDisplayItemBuilder.Create(
    FDeviceConfigProvider,
    FAppearanceConfig
  );
  FDelayedLoadTimer := nil;
  FUpdatingToggle := False;
  LogDebug('Created', ClassName);
end;

destructor TMainPresenter.Destroy;
begin
  Shutdown;
  FBatteryCache := nil;
  FDisplayItemBuilder.Free;
  LogDebug('Destroyed', ClassName);
  inherited;
end;

procedure TMainPresenter.Initialize;
var
  RadioEnabled: Boolean;
begin
  LogDebug('Initialize: Starting', ClassName);

  // Create delayed load timer
  FDelayedLoadTimer := TTimer.Create(nil);
  FDelayedLoadTimer.Enabled := False;
  FDelayedLoadTimer.Interval := 500;
  FDelayedLoadTimer.OnTimer := HandleDelayedLoadTimer;

  // Create battery cache if battery display is enabled
  if FAppearanceConfig.ShowBatteryLevel then
  begin
    LogDebug('Initialize: Creating battery cache', ClassName);
    FBatteryCache := CreateBatteryCache(CreateBatteryQuery);
    FBatteryCache.OnQueryCompleted := HandleBatteryQueryCompleted;
    FDisplayItemBuilder.SetBatteryCache(FBatteryCache);
  end;

  // Create Bluetooth service
  LogDebug('Initialize: Creating Bluetooth service', ClassName);
  FBluetoothService := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory
  );
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;
  LogDebug('Initialize: Bluetooth service created', ClassName);

  // Check Bluetooth radio state
  if FRadioStateManager.GetState(RadioEnabled) then
  begin
    LogDebug('Initialize: Radio state: Enabled=%s', [BoolToStr(RadioEnabled, True)], ClassName);

    if RadioEnabled then
    begin
      FToggleView.SetToggleState(True);
      FStatusView.ShowStatus('Loading devices...');
      LoadDevices;
      AutoConnectDevices;
    end
    else
    begin
      FToggleView.SetToggleState(False);
      FStatusView.ShowStatus('Bluetooth is off');
      FDeviceListView.ClearDevices;
    end;

    // Start watching for radio state changes
    FRadioStateManager.OnStateChanged := HandleRadioStateChanged;
    FRadioStateManager.StartWatching;
    LogDebug('Initialize: Radio watcher started', ClassName);
  end
  else
  begin
    LogDebug('Initialize: No Bluetooth adapter found', ClassName);
    FToggleView.SetToggleState(False);
    FToggleView.SetToggleEnabled(False);
    FStatusView.ShowStatus('No Bluetooth adapter found');
  end;

  LogDebug('Initialize: Complete', ClassName);
end;

procedure TMainPresenter.Shutdown;
begin
  LogDebug('Shutdown: Starting', ClassName);

  // Stop delayed load timer
  if FDelayedLoadTimer <> nil then
  begin
    FDelayedLoadTimer.Enabled := False;
    FDelayedLoadTimer.Free;
    FDelayedLoadTimer := nil;
  end;

  // Stop radio state watching
  if FRadioStateManager <> nil then
    FRadioStateManager.StopWatching;

  // Release service
  FBluetoothService := nil;
  FDevices := nil;

  LogDebug('Shutdown: Complete', ClassName);
end;

procedure TMainPresenter.LoadDevices;
var
  I: Integer;
begin
  LogDebug('LoadDevices: Starting', ClassName);
  FStatusView.SetBusy(True);
  try
    FDevices := FBluetoothService.GetPairedDevices;
    LogDebug('LoadDevices: Got %d devices', [Length(FDevices)], ClassName);

    // Register all discovered devices to persistent config
    for I := 0 to High(FDevices) do
    begin
      LogDebug('LoadDevices: Device[%d] Address=$%.12X, Name="%s", Connected=%s', [
        I, FDevices[I].AddressInt, FDevices[I].Name, BoolToStr(FDevices[I].IsConnected, True)
      ], ClassName);
      // Register device but don't update LastSeen (pass 0)
      // LastSeen is only updated when device actually connects
      FDeviceConfigProvider.RegisterDevice(FDevices[I].AddressInt, FDevices[I].Name, 0);
    end;

    // Save config if any new devices were registered
    FAppConfig.SaveIfModified;

    // Build display items and send to view
    RefreshDisplayItems;

    if Length(FDevices) = 0 then
      FStatusView.ShowStatus('No paired devices')
    else
      FStatusView.ShowStatus(Format('%d device(s)', [Length(FDevices)]));

    // Trigger battery level refresh for connected devices
    RefreshBatteryForConnectedDevices;
  finally
    FStatusView.SetBusy(False);
  end;
  LogDebug('LoadDevices: Complete', ClassName);
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
  LogDebug('AutoConnectDevices: Starting', ClassName);

  for I := 0 to High(FDevices) do
  begin
    Device := FDevices[I];
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(Device.AddressInt);

    if not DeviceConfig.AutoConnect then
      Continue;

    if Device.IsConnected then
    begin
      LogDebug('AutoConnectDevices: %s already connected, skipping', [Device.Name], ClassName);
      Continue;
    end;

    LogDebug('AutoConnectDevices: Auto-connecting %s', [Device.Name], ClassName);
    FStatusView.ShowStatus(Format('Auto-connecting %s...', [Device.Name]));
    ConnectDeviceAsync(Device);
  end;

  LogDebug('AutoConnectDevices: Complete', ClassName);
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
      LogDebug('ToggleConnectionAsync: Calling ToggleConnection', ClassName);
      LService.ToggleConnection(LDevice);
      LogDebug('ToggleConnectionAsync: Complete', ClassName);
    end
  ).Start;
end;

procedure TMainPresenter.SetRadioStateAsync(AEnable: Boolean);
var
  LDeviceListView: IDeviceListView;
  LToggleView: IToggleView;
  LStatusView: IStatusView;
  LRadioStateManager: IRadioStateManager;
begin
  // Capture interface references for async thread
  LDeviceListView := FDeviceListView;
  LToggleView := FToggleView;
  LStatusView := FStatusView;
  LRadioStateManager := FRadioStateManager;

  TThread.CreateAnonymousThread(
    procedure
    var
      LResult: TRadioControlResult;
    begin
      LResult := LRadioStateManager.SetState(AEnable);

      TThread.Queue(nil,
        procedure
        begin
          case LResult of
            rcSuccess:
              begin
                if AEnable then
                begin
                  LStatusView.ShowStatus('Bluetooth enabled');
                  LoadDevices;
                end
                else
                begin
                  LStatusView.ShowStatus('Bluetooth disabled');
                  LDeviceListView.ClearDevices;
                end;
              end;
            rcAccessDenied:
              begin
                LStatusView.ShowStatus('Access denied - check Windows settings');
                FUpdatingToggle := True;
                try
                  LToggleView.SetToggleState(not AEnable);
                finally
                  FUpdatingToggle := False;
                end;
              end;
            rcDeviceNotFound:
              begin
                LStatusView.ShowStatus('Bluetooth adapter not found');
                FUpdatingToggle := True;
                try
                  LToggleView.SetToggleState(False);
                finally
                  FUpdatingToggle := False;
                end;
                LToggleView.SetToggleEnabled(False);
              end;
          else
            begin
              LStatusView.ShowStatus('Failed to change Bluetooth state');
              FUpdatingToggle := True;
              try
                LToggleView.SetToggleState(not AEnable);
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
  Result := FDeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt).Alias;
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
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnect);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Connected', nfInfo);
      end;
    csDisconnected:
      begin
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neDisconnect);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Disconnected', nfInfo);
      end;
    csError:
      begin
        NotifyMode := FDeviceConfigProvider.GetEffectiveNotification(ADevice.AddressInt, neConnectFailed);
        if NotifyMode = nmBalloon then
          FStatusView.ShowNotification(DeviceName, 'Connection failed', nfError);
      end;
  end;
end;

procedure TMainPresenter.RefreshDisplayItems;
begin
  FDisplayItems := FDisplayItemBuilder.BuildDisplayItems(FDevices);
  FDeviceListView.ShowDisplayItems(FDisplayItems);
end;

procedure TMainPresenter.UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);
var
  I, J: Integer;
  NewDevices: TBluetoothDeviceInfoArray;
begin
  // Copy-on-write pattern: create new array to avoid in-place modification
  // This ensures FDevices is always in a consistent state
  //
  // IMPORTANT: Do NOT use Move() for copying records with managed types (strings).
  // Move() does raw byte copy without incrementing reference counts, causing
  // use-after-free when the source array is deallocated.

  // First, check if device exists and update
  for I := 0 to High(FDevices) do
  begin
    if FDevices[I].AddressInt = ADevice.AddressInt then
    begin
      // Create copy with updated element using proper assignment
      SetLength(NewDevices, Length(FDevices));
      for J := 0 to High(FDevices) do
        NewDevices[J] := FDevices[J];
      NewDevices[I] := ADevice;
      FDevices := NewDevices;
      LogDebug('UpdateOrAddDevice: Updated device at index %d', [I], ClassName);
      Exit;
    end;
  end;

  // Device not found, append to new array using proper assignment
  SetLength(NewDevices, Length(FDevices) + 1);
  for J := 0 to High(FDevices) do
    NewDevices[J] := FDevices[J];
  NewDevices[High(NewDevices)] := ADevice;
  FDevices := NewDevices;
  LogDebug('UpdateOrAddDevice: Added new device, total=%d', [Length(FDevices)], ClassName);
end;

{ Service event handlers }

procedure TMainPresenter.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
begin
  LogDebug('HandleDeviceStateChanged: Address=$%.12X, Name="%s", State=%d', [
    ADevice.AddressInt, ADevice.Name, Ord(ADevice.ConnectionState)
  ], ClassName);

  if Trim(ADevice.Name) = '' then
  begin
    LogDebug('HandleDeviceStateChanged: Empty name, skipping', ClassName);
    Exit;
  end;

  LDevice := ADevice;

  TThread.Queue(nil,
    procedure
    begin
      LogDebug('HandleDeviceStateChanged (queued): Processing %s', [LDevice.Name], ClassName);

      // Update local cache using copy-on-write pattern
      UpdateOrAddDevice(LDevice);

      // Update device in persistent config
      // Only update LastSeen when device actually connects - this is when the app "sees" it
      // For other state changes, pass 0 to preserve existing LastSeen value
      if LDevice.ConnectionState = csConnected then
      begin
        FDeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, Now);
        // Refresh battery level for newly connected device
        if (FBatteryCache <> nil) and FAppearanceConfig.ShowBatteryLevel then
          FBatteryCache.RequestRefresh(LDevice.AddressInt);
      end
      else
        FDeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, 0);
      FAppConfig.SaveIfModified;

      // Rebuild display items (device may have moved groups due to state change)
      RefreshDisplayItems;

      FStatusView.ShowStatus(Format('%s: %s', [LDevice.Name, TDeviceFormatter.FormatConnectionState(LDevice.ConnectionState)]));
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
        FStatusView.ShowStatus(Format('Error: %s (%d)', [AMessage, AErrorCode]))
      else
        FStatusView.ShowStatus(Format('Error: %s', [AMessage]));
    end
  );
end;

procedure TMainPresenter.HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
begin
  LogInfo('HandleRadioStateChanged: Enabled=%s', [BoolToStr(AEnabled, True)], ClassName);

  FUpdatingToggle := True;
  try
    FToggleView.SetToggleState(AEnabled);
  finally
    FUpdatingToggle := False;
  end;

  if AEnabled then
  begin
    FStatusView.ShowStatus('Loading devices...');
    LoadDevicesDelayed;
  end
  else
  begin
    FStatusView.ShowStatus('Bluetooth disabled');
    if FDelayedLoadTimer <> nil then
      FDelayedLoadTimer.Enabled := False;
    FDeviceListView.ClearDevices;
  end;
end;

procedure TMainPresenter.HandleDelayedLoadTimer(Sender: TObject);
begin
  FDelayedLoadTimer.Enabled := False;
  LoadDevices;
end;

procedure TMainPresenter.HandleBatteryQueryCompleted(Sender: TObject;
  ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
begin
  // Rebuild display items to show updated battery level
  RefreshDisplayItems;
end;

procedure TMainPresenter.RefreshBatteryForConnectedDevices;
var
  Addresses: TArray<UInt64>;
  I, Count: Integer;
  Device: TBluetoothDeviceInfo;
begin
  if FBatteryCache = nil then
    Exit;

  if not FAppearanceConfig.ShowBatteryLevel then
    Exit;

  // Collect addresses of connected devices
  Count := 0;
  SetLength(Addresses, Length(FDevices));
  for I := 0 to High(FDevices) do
  begin
    Device := FDevices[I];
    if Device.IsConnected then
    begin
      Addresses[Count] := Device.AddressInt;
      Inc(Count);
    end;
  end;
  SetLength(Addresses, Count);

  if Count > 0 then
  begin
    LogDebug('RefreshBatteryForConnectedDevices: Refreshing %d devices', [Count], ClassName);
    FBatteryCache.RequestRefreshAll(Addresses);
  end;
end;

{ Public methods called by View }

procedure TMainPresenter.OnDeviceClicked(const ADevice: TBluetoothDeviceInfo);
begin
  LogInfo('OnDeviceClicked: %s, State=%d', [ADevice.Name, Ord(ADevice.ConnectionState)], ClassName);

  if ADevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    LogInfo('OnDeviceClicked: Operation in progress, ignoring', ClassName);
    FStatusView.ShowStatus('Operation in progress...');
    Exit;
  end;

  if ADevice.IsConnected then
    FStatusView.ShowStatus(Format('Disconnecting %s...', [ADevice.Name]))
  else
    FStatusView.ShowStatus(Format('Connecting %s...', [ADevice.Name]));

  // In Menu mode, hide immediately after device click
  if FGeneralConfig.WindowMode = wmMenu then
  begin
    LogInfo('OnDeviceClicked: Menu mode, hiding view', ClassName);
    FVisibilityView.HideView;
  end;

  ToggleConnectionAsync(ADevice);
end;

procedure TMainPresenter.OnToggleChanged(AEnabled: Boolean);
begin
  if FUpdatingToggle then
    Exit;

  LogInfo('OnToggleChanged: Enabled=%s', [BoolToStr(AEnabled, True)], ClassName);

  if AEnabled then
    FStatusView.ShowStatus('Enabling Bluetooth...')
  else
    FStatusView.ShowStatus('Disabling Bluetooth...');

  SetRadioStateAsync(AEnabled);
end;

procedure TMainPresenter.OnRefreshRequested;
begin
  LogInfo('OnRefreshRequested', ClassName);
  FStatusView.ShowStatus('Refreshing...');
  LoadDevices;
end;

procedure TMainPresenter.OnVisibilityToggleRequested;
begin
  if FVisibilityView.IsVisible and (not FVisibilityView.IsMinimized) then
    FVisibilityView.HideView
  else
    FVisibilityView.ShowView;
end;

procedure TMainPresenter.OnExitRequested;
begin
  LogInfo('OnExitRequested', ClassName);
  FVisibilityView.ForceClose;
end;

procedure TMainPresenter.OnSettingsChanged;
begin
  LogInfo('OnSettingsChanged', ClassName);
  // Note: Polling mode changes require application restart to take effect.
  // Other settings (theme, hotkey, etc.) are applied immediately by MainForm.
end;

procedure TMainPresenter.OnViewShown;
begin
  RefreshBatteryForConnectedDevices;
end;

function TMainPresenter.CanClose: Boolean;
begin
  Result := not FWindowConfig.CloseToTray;
end;

end.
