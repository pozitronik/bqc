{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Presenter (MVP Pattern)                    }
{                                                       }
{*******************************************************}

unit App.MainPresenter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  App.MainViewInterfaces,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.AsyncExecutor,
  App.DeviceDisplayTypes,
  App.DeviceDisplayItemBuilder,
  App.DeviceNotificationCoordinator,
  App.DeviceBatteryCoordinator,
  App.DeviceDiscoveryCoordinator;

type
  /// <summary>
  /// Main presenter handling business logic for the Bluetooth device management.
  /// Coordinates between the View (Form) and the Model (Services).
  /// Dependencies are injected via constructor for testability.
  /// </summary>
  /// <remarks>
  /// ARCHITECTURAL NOTE: The constructor has many parameters by design.
  /// Each parameter is a focused interface (ISP), making dependencies explicit
  /// and testable. Alternatives like config aggregates or parameter objects
  /// would hide dependencies and reduce clarity. The parameter count reflects
  /// real complexity - this is the main application coordinator. Tests confirm
  /// the design works well with mock injection.
  /// </remarks>
  TMainPresenter = class
  private
    { View interfaces (ISP-compliant) }
    FDeviceListView: IDeviceListView;
    FToggleView: IToggleView;
    FStatusView: IStatusView;
    FVisibilityView: IVisibilityView;

    FBluetoothService: IBluetoothService;
    FPairingService: IBluetoothPairingService;

    { Coordinators (extracted from god class) }
    FNotificationCoordinator: TDeviceNotificationCoordinator;
    FBatteryCoordinator: TDeviceBatteryCoordinator;
    FDiscoveryCoordinator: TDeviceDiscoveryCoordinator;

    { Injected configuration dependencies }
    FAppConfig: IAppConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FAppearanceConfig: IAppearanceConfig;
    FLayoutConfig: ILayoutConfig;
    FConnectionConfig: IConnectionConfig;
    FRadioStateManager: IRadioStateManager;
    FAsyncExecutor: IAsyncExecutor;
    FBatteryCache: IBatteryCache;
    FDeviceList: TList<TBluetoothDeviceInfo>;
    FDeviceIndexMap: TDictionary<UInt64, Integer>;  // Address -> Index for O(1) lookup
    FDevicesArrayCache: TBluetoothDeviceInfoArray;
    FDevicesArrayValid: Boolean;
    FDisplayItems: TDeviceDisplayItemArray;
    FDisplayItemBuilder: IDeviceDisplayItemBuilder;
    FDelayedLoadGeneration: Integer;  // Generation counter for delayed load cancellation
    FUpdatingToggle: Boolean;
    FIsShutdown: Boolean;  // Lifetime safety flag for async callbacks
    FIsPairing: Boolean;  // True when pairing operation is in progress
    FDeviceStatusMessages: TDictionary<UInt64, string>;  // Persistent device status messages

    { Service event handlers }
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);

    { Internal methods }
    function GetConnectedDeviceAddresses: TArray<UInt64>;
    procedure LoadDevices;
    procedure LoadDevicesDelayed;
    procedure CancelDelayedLoad;
    procedure AutoConnectDevices;
    procedure ConnectDeviceAsync(const ADevice: TBluetoothDeviceInfo);
    procedure ToggleConnectionAsync(const ADevice: TBluetoothDeviceInfo);
    procedure SetRadioStateAsync(AEnable: Boolean);

    /// <summary>
    /// Initiates asynchronous pairing with a Bluetooth device.
    /// Uses strategy pattern via FPairingService to support different pairing methods.
    /// Displays Windows pairing dialog and provides progress feedback via status view.
    /// On completion, calls HandlePairingResult on main thread.
    /// </summary>
    /// <param name="ADevice">Device to pair with.</param>
    procedure PairDeviceAsync(const ADevice: TBluetoothDeviceInfo);

    procedure HandlePairingResult(const ADevice: TBluetoothDeviceInfo; const AResult: TPairingResult);

    /// <summary>
    /// Clears custom status text for a device and refreshes its display.
    /// Used to remove pairing progress messages after completion.
    /// </summary>
    procedure ClearDeviceStatus(ADeviceAddress: UInt64);

    /// <summary>
    /// Sets custom status text for a device and refreshes its display.
    /// Used to show persistent error/info messages in device status line.
    /// </summary>
    procedure SetDeviceStatus(ADeviceAddress: UInt64; const AMessage: string);

    /// <summary>
    /// Synchronizes paired device list with Windows Bluetooth state.
    /// Removes devices from FDeviceList that were unpaired externally via Windows Settings.
    /// Runs periodically based on PairingStateSyncInterval config.
    /// Optimized with O(1) dictionary lookup instead of linear search.
    /// </summary>
    procedure SyncPairedDeviceList;

    procedure ScheduleNextPairingSync;
    procedure RemoveDeviceFromList(ADeviceAddress: UInt64);

    function GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;
    procedure RefreshDisplayItems;

    /// <summary>
    /// Applies persistent status messages to display items after they're built.
    /// Preserves custom status text across display list rebuilds.
    /// </summary>
    procedure ApplyStatusMessages;

    /// <summary>
    /// Returns devices as array, building from list if cache is invalid.
    /// Used by BuildDisplayItems which requires array input.
    /// </summary>
    function GetDevicesArray: TBluetoothDeviceInfoArray;

    /// <summary>
    /// Invalidates the devices array cache.
    /// Must be called after any modification to FDeviceList.
    /// </summary>
    procedure InvalidateDevicesArrayCache;

    /// <summary>
    /// Sets toggle state while preventing recursive event handling.
    /// Wraps the state change with FUpdatingToggle guard.
    /// </summary>
    procedure SetToggleStateSafe(AState: Boolean);

    /// <summary>
    /// Queues a procedure to run on the main thread with shutdown guard.
    /// Centralizes the FIsShutdown check pattern to prevent async callbacks
    /// from accessing freed state after presenter shutdown.
    /// </summary>
    procedure QueueIfNotShutdown(AProc: TProc);

    /// <summary>
    /// Creates a scan action display item (embedded scan button).
    /// Returns an action item with appropriate text based on scanning state.
    /// </summary>
    function CreateScanActionItem: TDeviceDisplayItem;

  protected
    /// <summary>
    /// Updates existing device or adds new one.
    /// Uses O(1) dictionary lookup + O(1) list update/append.
    /// </summary>
    procedure UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);

    /// <summary>
    /// Finds a device in the internal device list by address.
    /// Uses O(1) dictionary lookup. Returns empty record (AddressInt=0) if not found.
    /// </summary>
    function FindDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;

    /// <summary>
    /// Returns the number of devices in the internal list.
    /// Exposed for testing to verify index map consistency.
    /// </summary>
    function GetDeviceCount: Integer;

    /// <summary>
    /// Returns the number of connected devices in the cache.
    /// Exposed for testing to verify connected addresses cache.
    /// </summary>
    function GetConnectedDeviceCount: Integer;

    /// <summary>
    /// Returns the battery cache instance.
    /// Exposed for testing to trigger battery query events.
    /// </summary>
    function GetBatteryCache: IBatteryCache;

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
    /// <param name="ALayoutConfig">Layout settings for UI dimensions and unpaired device display.</param>
    /// <param name="AConnectionConfig">Connection and discovery settings.</param>
    /// <param name="ARadioStateManager">Bluetooth radio state manager.</param>
    /// <param name="AAsyncExecutor">Async executor for background operations.</param>
    /// <param name="ABluetoothService">Bluetooth service for device operations.</param>
    /// <param name="APairingService">Bluetooth pairing service for pairing operations.</param>
    /// <param name="ADisplayItemBuilder">Builder for creating display items from devices.</param>
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
      ALayoutConfig: ILayoutConfig;
      AConnectionConfig: IConnectionConfig;
      ARadioStateManager: IRadioStateManager;
      AAsyncExecutor: IAsyncExecutor;
      ABluetoothService: IBluetoothService;
      APairingService: IBluetoothPairingService;
      ADisplayItemBuilder: IDeviceDisplayItemBuilder
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
    /// Called when user toggles pin state via context menu.
    /// </summary>
    procedure OnDevicePinToggled(ADeviceAddress: UInt64);

    /// <summary>
    /// Called when user selects "Copy Name" from context menu.
    /// </summary>
    procedure OnCopyDeviceName(ADeviceAddress: UInt64);

    /// <summary>
    /// Called when user selects "Copy Address" from context menu.
    /// </summary>
    procedure OnCopyDeviceAddress(ADeviceAddress: UInt64);

    /// <summary>
    /// Called when user selects "Hide from List" from context menu.
    /// Sets device Hidden flag via DeviceConfigProvider.
    /// </summary>
    procedure OnDeviceHidden(ADeviceAddress: UInt64);

    /// <summary>
    /// Called when user selects "Unpair Device" from context menu.
    /// </summary>
    procedure OnUnpairDevice(ADeviceAddress: UInt64);

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
    /// Called when user requests a scan for nearby devices.
    /// Initiates async Bluetooth discovery scan.
    /// </summary>
    procedure OnScanRequested;

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
    /// Called when user requests to open Windows Bluetooth settings.
    /// Opens appropriate settings UI based on OS version.
    /// </summary>
    procedure OnBluetoothSettingsRequested;

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
  Winapi.Windows,
  Winapi.ShellAPI,
  Vcl.Clipbrd,
  App.Logger,
  App.ConfigEnums,
  App.DeviceConfigTypes,
  App.WinRTSupport,
  Bluetooth.WinAPI,
  Bluetooth.BatteryQuery,
  Bluetooth.ProfileQuery,
  App.DeviceFormatter,
  App.DeviceSorter;

const
  // Debounces rapid device list updates after connection state changes
  DELAYED_LOAD_INTERVAL_MS = 500;
  // Windows battery drivers need time to update after device reconnects
  BATTERY_REFRESH_DELAY_MS = 10000;
  // Delay to ensure device repository is updated after pairing before auto-connect
  PAIRING_REFRESH_DELAY_MS = 500;

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
  ALayoutConfig: ILayoutConfig;
  AConnectionConfig: IConnectionConfig;
  ARadioStateManager: IRadioStateManager;
  AAsyncExecutor: IAsyncExecutor;
  ABluetoothService: IBluetoothService;
  APairingService: IBluetoothPairingService;
  ADisplayItemBuilder: IDeviceDisplayItemBuilder
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
  FLayoutConfig := ALayoutConfig;
  FConnectionConfig := AConnectionConfig;
  FRadioStateManager := ARadioStateManager;
  FAsyncExecutor := AAsyncExecutor;
  FBluetoothService := ABluetoothService;
  FPairingService := APairingService;
  FDisplayItemBuilder := ADisplayItemBuilder;
  FBatteryCache := nil;

  // Create coordinators (extracted from god class)
  FNotificationCoordinator := TDeviceNotificationCoordinator.Create(FStatusView, FDeviceConfigProvider);

  FDeviceList := TList<TBluetoothDeviceInfo>.Create;
  FDeviceIndexMap := TDictionary<UInt64, Integer>.Create;
  FDeviceStatusMessages := TDictionary<UInt64, string>.Create;
  FDevicesArrayCache := nil;
  FDevicesArrayValid := False;
  FDisplayItems := nil;
  FDelayedLoadGeneration := 0;
  FUpdatingToggle := False;
  FIsShutdown := False;
  FIsPairing := False;
  LogDebug('Created', ClassName);
end;

destructor TMainPresenter.Destroy;
begin
  Shutdown;
  FDiscoveryCoordinator.Free;
  FBatteryCoordinator.Free;
  FNotificationCoordinator.Free;
  FBatteryCache := nil;
  FDeviceStatusMessages.Free;
  FDeviceIndexMap.Free;
  FDeviceList.Free;
  // FDisplayItemBuilder is interface - reference counted, no Free needed
  LogDebug('Destroyed', ClassName);
  inherited;
end;

procedure TMainPresenter.Initialize;
var
  RadioEnabled: Boolean;
begin
  LogDebug('Initialize: Starting', ClassName);

  // Create battery cache - use real cache when enabled, null object when disabled
  // Null Object pattern eliminates null checks throughout the codebase
  if FAppearanceConfig.ShowBatteryLevel then
  begin
    LogDebug('Initialize: Creating battery cache', ClassName);
    FBatteryCache := CreateBatteryCache(CreateBatteryQuery);
  end
  else
  begin
    LogDebug('Initialize: Using null battery cache (feature disabled)', ClassName);
    FBatteryCache := CreateNullBatteryCache;
  end;
  FDisplayItemBuilder.SetBatteryCache(FBatteryCache);

  // Create battery coordinator (extracted from god class)
  FBatteryCoordinator := TDeviceBatteryCoordinator.Create(
    FBatteryCache,
    FDisplayItemBuilder,
    FDeviceListView,
    FAsyncExecutor,
    FindDeviceByAddress,
    GetConnectedDeviceAddresses
  );
  FBatteryCoordinator.Initialize;

  // Create discovery coordinator (extracted from god class)
  FDiscoveryCoordinator := TDeviceDiscoveryCoordinator.Create(
    FBluetoothService,
    FStatusView,
    FAsyncExecutor,
    RefreshDisplayItems,
    ClearDeviceStatus,
    QueueIfNotShutdown
  );
  FDiscoveryCoordinator.Initialize;

  // Wire up Bluetooth service event handlers
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;
  LogDebug('Initialize: Bluetooth service event handlers wired', ClassName);

  // Check Bluetooth radio state
  if FRadioStateManager.GetState(RadioEnabled) then
  begin
    LogDebug('Initialize: Radio state: Enabled=%s', [BoolToStr(RadioEnabled, True)], ClassName);

    if RadioEnabled then
    begin
      SetToggleStateSafe(True);
      FStatusView.ShowStatus('Loading devices...');
      LoadDevices;
      AutoConnectDevices;
    end
    else
    begin
      SetToggleStateSafe(False);
      FStatusView.ShowStatus('Bluetooth is off');
      FDeviceListView.ClearDevices;
    end;

    // Disable toggle if radio control is not supported (e.g., Win7 Classic mode)
    if not FRadioStateManager.SupportsStateChange then
    begin
      FToggleView.SetToggleEnabled(False);
      LogDebug('Initialize: Radio control not supported, toggle disabled', ClassName);
    end;

    // Start watching for radio state changes
    FRadioStateManager.OnStateChanged := HandleRadioStateChanged;
    FRadioStateManager.StartWatching;
    LogDebug('Initialize: Radio watcher started', ClassName);
  end
  else
  begin
    LogDebug('Initialize: No Bluetooth adapter found', ClassName);
    SetToggleStateSafe(False);
    FToggleView.SetToggleEnabled(False);
    FStatusView.ShowStatus('No Bluetooth adapter found');
  end;

  // Setup periodic sync for pairing state (detects devices unpaired from Windows)
  if FConnectionConfig.PairingStateSyncInterval > 0 then
  begin
    LogInfo('Initialize: Setting up periodic pairing state sync (interval=%d ms)',
      [FConnectionConfig.PairingStateSyncInterval], ClassName);

    // Schedule periodic sync using delayed execution
    ScheduleNextPairingSync;
  end
  else
    LogDebug('Initialize: Periodic pairing sync disabled (interval=0)', ClassName);

  LogDebug('Initialize: Complete', ClassName);
end;

procedure TMainPresenter.Shutdown;
begin
  LogDebug('Shutdown: Starting', ClassName);

  // Set shutdown flag first to prevent async callbacks from accessing freed state
  FIsShutdown := True;

  // Shutdown coordinators
  if FBatteryCoordinator <> nil then
    FBatteryCoordinator.Shutdown;

  // Cancel any pending delayed load
  CancelDelayedLoad;

  // Stop radio state watching
  if FRadioStateManager <> nil then
    FRadioStateManager.StopWatching;

  // Save config before shutdown to persist LastSeen timestamps
  FAppConfig.SaveIfModified;
  LogDebug('Shutdown: Config saved', ClassName);

  // Release service
  FBluetoothService := nil;
  FDeviceIndexMap.Clear;
  FDeviceList.Clear;
  InvalidateDevicesArrayCache;

  LogDebug('Shutdown: Complete', ClassName);
end;

procedure TMainPresenter.LoadDevices;
var
  I: Integer;
  Devices: TBluetoothDeviceInfoArray;
begin
  LogDebug('LoadDevices: Starting', ClassName);
  FStatusView.SetBusy(True);
  try
    Devices := FBluetoothService.GetPairedDevices;
    LogDebug('LoadDevices: Got %d devices', [Length(Devices)], ClassName);

    // Replace list contents with new devices and rebuild index map
    FDeviceIndexMap.Clear;
    FDeviceList.Clear;
    for I := 0 to High(Devices) do
    begin
      FDeviceList.Add(Devices[I]);
      FDeviceIndexMap.Add(Devices[I].AddressInt, I);
    end;
    InvalidateDevicesArrayCache;

    // Register all discovered devices to persistent config
    for I := 0 to FDeviceList.Count - 1 do
    begin
      LogDebug('LoadDevices: Device[%d] Address=$%.12X, Name="%s", Connected=%s', [
        I, FDeviceList[I].AddressInt, FDeviceList[I].Name, BoolToStr(FDeviceList[I].IsConnected, True)
      ], ClassName);
      // Register device and set LastSeen to Now if currently connected
      // This ensures connected devices have a valid timestamp for when they disconnect
      if FDeviceList[I].IsConnected then
        FDeviceConfigProvider.RegisterDevice(FDeviceList[I].AddressInt, FDeviceList[I].Name, Now)
      else
        FDeviceConfigProvider.RegisterDevice(FDeviceList[I].AddressInt, FDeviceList[I].Name, 0);
    end;

    // Save config if any new devices were registered
    FAppConfig.SaveIfModified;

    // Build display items and send to view
    RefreshDisplayItems;

    if FDeviceList.Count = 0 then
      FStatusView.ShowStatus('No paired devices')
    else
      FStatusView.ShowStatus(Format('%d device(s)', [FDeviceList.Count]));

    // Trigger battery level refresh for connected devices (delegated to coordinator)
    FBatteryCoordinator.RefreshBatteryForConnectedDevices;
  finally
    FStatusView.SetBusy(False);
  end;
  LogDebug('LoadDevices: Complete', ClassName);
end;

procedure TMainPresenter.LoadDevicesDelayed;
var
  CapturedGeneration: Integer;
begin
  // Increment generation to invalidate any pending delayed loads (debounce)
  Inc(FDelayedLoadGeneration);
  CapturedGeneration := FDelayedLoadGeneration;
  LogDebug('LoadDevicesDelayed: Scheduling with generation=%d', [CapturedGeneration], ClassName);

  FAsyncExecutor.RunDelayed(
    procedure
    begin
      QueueIfNotShutdown(
        procedure
        begin
          // Only execute if generation hasn't changed (not cancelled/superseded)
          if FDelayedLoadGeneration = CapturedGeneration then
          begin
            LogDebug('LoadDevicesDelayed: Executing generation=%d', [CapturedGeneration], ClassName);
            LoadDevices;
          end
          else
            LogDebug('LoadDevicesDelayed: Skipped generation=%d (current=%d)',
              [CapturedGeneration, FDelayedLoadGeneration], ClassName);
        end
      );
    end,
    DELAYED_LOAD_INTERVAL_MS
  );
end;

procedure TMainPresenter.CancelDelayedLoad;
begin
  // Increment generation to invalidate all pending delayed loads
  Inc(FDelayedLoadGeneration);
  LogDebug('CancelDelayedLoad: New generation=%d', [FDelayedLoadGeneration], ClassName);
end;

procedure TMainPresenter.AutoConnectDevices;
var
  I: Integer;
  DeviceConfig: TDeviceConfig;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('AutoConnectDevices: Starting', ClassName);

  for I := 0 to FDeviceList.Count - 1 do
  begin
    Device := FDeviceList[I];
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
  FAsyncExecutor.RunAsync(
    procedure
    begin
      LService.Connect(LDevice);
    end
  );
end;

procedure TMainPresenter.ToggleConnectionAsync(const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
  LService: IBluetoothService;
begin
  LDevice := ADevice;
  LService := FBluetoothService;
  FAsyncExecutor.RunAsync(
    procedure
    begin
      LogDebug('ToggleConnectionAsync: Calling ToggleConnection', ClassName);
      LService.ToggleConnection(LDevice);
      LogDebug('ToggleConnectionAsync: Complete', ClassName);
    end
  );
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

  FAsyncExecutor.RunAsync(
    procedure
    var
      LResult: TRadioControlResult;
    begin
      LResult := LRadioStateManager.SetState(AEnable);

      QueueIfNotShutdown(
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
                SetToggleStateSafe(not AEnable);
              end;
            rcDeviceNotFound:
              begin
                LStatusView.ShowStatus('Bluetooth adapter not found');
                SetToggleStateSafe(False);
                LToggleView.SetToggleEnabled(False);
              end;
          else
            begin
              LStatusView.ShowStatus('Failed to change Bluetooth state');
              SetToggleStateSafe(not AEnable);
            end;
          end;
        end
      );
    end
  );
end;

procedure TMainPresenter.PairDeviceAsync(const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
  LPairingService: IBluetoothPairingService;
  LStatusView: IStatusView;
  DeviceName: string;
begin
  // Prevent concurrent pairing operations
  if FIsPairing then
  begin
    LogWarning('PairDeviceAsync: Pairing already in progress, ignoring request', ClassName);
    Exit;
  end;

  DeviceName := GetDeviceDisplayName(ADevice);
  LDevice := ADevice;
  LPairingService := FPairingService;
  LStatusView := FStatusView;

  LogInfo('PairDeviceAsync: Initiating pairing for device %s ($%.12X)', [DeviceName, ADevice.AddressInt], ClassName);

  // Mark pairing as in progress to prevent concurrent attempts
  FIsPairing := True;

  FAsyncExecutor.RunAsync(
    procedure
    var
      PairingResult: TPairingResult;
      ProgressCallback: TPairingProgressCallback;
      DeviceAddress: UInt64;
    begin
      try
        // Capture device address for progress callback
        DeviceAddress := LDevice.AddressInt;

        // Progress callback to update device status line instead of global status
        ProgressCallback := procedure(const AMessage: string)
        begin
          QueueIfNotShutdown(
            procedure
            begin
              // Store status in dictionary and update display
              SetDeviceStatus(DeviceAddress, AMessage);
              LogDebug('PairDeviceAsync: Updated device status - %s', [AMessage], ClassName);
            end
          );
        end;

        LogDebug('PairDeviceAsync: Calling PairDevice', ClassName);
        PairingResult := LPairingService.PairDevice(LDevice, ProgressCallback);
        LogInfo('PairDeviceAsync: Pairing complete, Status=%d (%s)',
          [Ord(PairingResult.Status), PairingResult.ErrorMessage], ClassName);

        // Handle result on main thread
        QueueIfNotShutdown(
          procedure
          begin
            HandlePairingResult(LDevice, PairingResult);
          end
        );
      except
        on E: Exception do
        begin
          LogError('PairDeviceAsync: Exception during pairing - %s: %s', [E.ClassName, E.Message], ClassName);

          // Clear pairing flag and show error in device status on main thread
          QueueIfNotShutdown(
            procedure
            begin
              FIsPairing := False;
              SetDeviceStatus(DeviceAddress, Format('Pairing failed: %s', [E.Message]));
            end
          );
        end;
      end;
    end
  );
end;

procedure TMainPresenter.HandlePairingResult(const ADevice: TBluetoothDeviceInfo;
  const AResult: TPairingResult);
var
  DeviceName: string;
begin
  // Clear pairing flag to allow new pairing attempts
  FIsPairing := False;

  DeviceName := GetDeviceDisplayName(ADevice);

  case AResult.Status of
    prsSuccess:
      begin
        LogInfo('HandlePairingResult: Success - refreshing device list and auto-connecting', ClassName);

        // Clear device status - device will be refreshed from repository
        ClearDeviceStatus(ADevice.AddressInt);

        // Refresh device repository to get newly paired device
        FBluetoothService.RefreshAllDevices;

        // Auto-connect after successful pairing
        FAsyncExecutor.RunAsync(
          procedure
          var
            RefreshedDevice: TBluetoothDeviceInfo;
          begin
            // Small delay to ensure repository is updated
            Sleep(PAIRING_REFRESH_DELAY_MS);

            QueueIfNotShutdown(
              procedure
              begin
                // Find the newly paired device in the updated list
                RefreshedDevice := FindDeviceByAddress(ADevice.AddressInt);
                if RefreshedDevice.AddressInt <> 0 then
                begin
                  LogInfo('HandlePairingResult: Auto-connecting to newly paired device', ClassName);
                  ToggleConnectionAsync(RefreshedDevice);
                end
                else
                  LogWarning('HandlePairingResult: Device not found after pairing', ClassName);
              end
            );
          end
        );
      end;

    prsCancelled:
      begin
        LogInfo('HandlePairingResult: User cancelled pairing', ClassName);
        SetDeviceStatus(ADevice.AddressInt, 'Pairing cancelled');
      end;

    prsAlreadyPaired:
      begin
        LogInfo('HandlePairingResult: Device already paired - refreshing list', ClassName);
        SetDeviceStatus(ADevice.AddressInt, 'Already paired');
        FBluetoothService.RefreshAllDevices;
      end;

    prsTimeout:
      begin
        LogWarning('HandlePairingResult: Pairing timed out', ClassName);
        SetDeviceStatus(ADevice.AddressInt, 'Pairing timed out');
      end;

    prsNotSupported:
      begin
        LogError('HandlePairingResult: Pairing not supported - %s', [AResult.ErrorMessage], ClassName);
        SetDeviceStatus(ADevice.AddressInt, AResult.ErrorMessage);
      end;

    prsFailed:
      begin
        LogError('HandlePairingResult: Pairing failed - %s (code %d)',
          [AResult.ErrorMessage, AResult.ErrorCode], ClassName);
        SetDeviceStatus(ADevice.AddressInt, Format('Pairing failed: %s', [AResult.ErrorMessage]));
      end;
  end;
end;

procedure TMainPresenter.ClearDeviceStatus(ADeviceAddress: UInt64);
begin
  SetDeviceStatus(ADeviceAddress, '');
end;

procedure TMainPresenter.SetDeviceStatus(ADeviceAddress: UInt64; const AMessage: string);
var
  Device: TBluetoothDeviceInfo;
  DisplayItem: TDeviceDisplayItem;
  IsDiscovered: Boolean;
begin
  // Store in persistent dictionary so it survives display list rebuilds
  if AMessage <> '' then
    FDeviceStatusMessages.AddOrSetValue(ADeviceAddress, AMessage)
  else
    FDeviceStatusMessages.Remove(ADeviceAddress);

  // Try to find device in paired devices list
  Device := FindDeviceByAddress(ADeviceAddress);
  IsDiscovered := False;

  if Device.AddressInt = 0 then
  begin
    // Try to find in unpaired devices (discovered devices)
    Device := FDiscoveryCoordinator.FindUnpairedDevice(ADeviceAddress);
    if Device.AddressInt <> 0 then
    begin
      IsDiscovered := True;
    end
    else
    begin
      LogDebug('SetDeviceStatus: Device $%.12X not found (will be applied when device appears)', [ADeviceAddress], ClassName);
      // Don't exit - status is stored and will be applied when device appears
      Exit;
    end;
  end;

  // Build display item using appropriate method for device type
  if IsDiscovered then
    DisplayItem := FDisplayItemBuilder.BuildDiscoveredDeviceDisplayItem(Device)
  else
    DisplayItem := FDisplayItemBuilder.BuildDisplayItem(Device);

  DisplayItem.StatusText := AMessage;

  // Update single item in UI
  FDeviceListView.UpdateDisplayItem(DisplayItem);

  if AMessage <> '' then
    LogDebug('SetDeviceStatus: Set status for device $%.12X - "%s"', [ADeviceAddress, AMessage], ClassName)
  else
    LogDebug('SetDeviceStatus: Cleared status for device $%.12X', [ADeviceAddress], ClassName);
end;

procedure TMainPresenter.SyncPairedDeviceList;
var
  PairedAddresses: TArray<UInt64>;
  PairedAddressSet: TDictionary<UInt64, Boolean>;
  DeviceAddress: UInt64;
  PairedAddr: UInt64;
  I: Integer;
  RemovedCount: Integer;
  DeviceToRemove: TBluetoothDeviceInfo;
begin
  if FIsShutdown then
    Exit;

  LogDebug('SyncPairedDeviceList: Starting periodic sync', ClassName);

  // Get current paired device addresses from Windows
  PairedAddresses := FPairingService.GetPairedDeviceAddresses;

  // Convert array to dictionary for O(1) lookups instead of O(n) linear search
  PairedAddressSet := TDictionary<UInt64, Boolean>.Create(Length(PairedAddresses));
  try
    for PairedAddr in PairedAddresses do
      PairedAddressSet.Add(PairedAddr, True);

    RemovedCount := 0;

    // Check each device in our list against Windows paired devices
    I := FDeviceList.Count - 1;
    while I >= 0 do
    begin
      DeviceAddress := FDeviceList[I].AddressInt;

      // Check if device is still paired in Windows (O(1) lookup)
      if not PairedAddressSet.ContainsKey(DeviceAddress) then
      begin
        // Device was unpaired from Windows - remove it
        DeviceToRemove := FDeviceList[I];
        LogInfo('SyncPairedDeviceList: Device $%.12X ("%s") was unpaired externally, removing from list',
          [DeviceAddress, DeviceToRemove.Name], ClassName);

        // Disconnect if connected
        if DeviceToRemove.IsConnected then
        begin
          LogInfo('SyncPairedDeviceList: Disconnecting device before removal', ClassName);
          FBluetoothService.Disconnect(DeviceToRemove);
        end;

        RemoveDeviceFromList(DeviceAddress);
        Inc(RemovedCount);
      end;

      Dec(I);
    end;

    if RemovedCount > 0 then
    begin
      LogInfo('SyncPairedDeviceList: Removed %d unpaired device(s)', [RemovedCount], ClassName);
      RefreshDisplayItems;
      FStatusView.ShowStatus(Format('%d device(s) removed (unpaired from Windows)', [RemovedCount]));
    end
    else
      LogDebug('SyncPairedDeviceList: No changes detected', ClassName);
  finally
    PairedAddressSet.Free;
  end;
end;

procedure TMainPresenter.ScheduleNextPairingSync;
var
  SyncInterval: Integer;
begin
  SyncInterval := FConnectionConfig.PairingStateSyncInterval;

  if (SyncInterval > 0) and not FIsShutdown then
  begin
    FAsyncExecutor.RunDelayed(
      procedure
      begin
        QueueIfNotShutdown(
          procedure
          begin
            SyncPairedDeviceList;
            ScheduleNextPairingSync;  // Reschedule for next interval
          end
        );
      end,
      SyncInterval
    );
  end;
end;

procedure TMainPresenter.RemoveDeviceFromList(ADeviceAddress: UInt64);
var
  Index: Integer;
  I: Integer;
begin
  if FDeviceIndexMap.TryGetValue(ADeviceAddress, Index) then
  begin
    LogDebug('RemoveDeviceFromList: Removing device at index %d (Address=$%.12X)',
      [Index, ADeviceAddress], ClassName);

    // Remove from list
    FDeviceList.Delete(Index);

    // Rebuild index map since all indices after removed item have shifted
    FDeviceIndexMap.Clear;
    for I := 0 to FDeviceList.Count - 1 do
      FDeviceIndexMap.Add(FDeviceList[I].AddressInt, I);

    // Invalidate cache
    InvalidateDevicesArrayCache;

    LogDebug('RemoveDeviceFromList: Device removed, list count now %d', [FDeviceList.Count], ClassName);
  end
  else
    LogWarning('RemoveDeviceFromList: Device $%.12X not found in index map', [ADeviceAddress], ClassName);
end;

function TMainPresenter.GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;
begin
  Result := FDeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt).Alias;
  if Result = '' then
    Result := ADevice.Name;
end;

function TMainPresenter.CreateScanActionItem: TDeviceDisplayItem;
var
  DummyDevice: TBluetoothDeviceInfo;
  ButtonText: string;
  ZeroAddress: TBluetoothAddress;
begin
  // Create a sentinel device record (not a real device)
  // Use AddressInt = 0 as sentinel value to indicate this is not a real device
  ZeroAddress := Default(TBluetoothAddress);  // All zeros
  DummyDevice := TBluetoothDeviceInfo.Create(
    ZeroAddress,            // Sentinel MAC address (00:00:00:00:00:00)
    0,                      // Sentinel AddressInt
    '',                     // Empty name
    btUnknown,              // Unknown type
    csDisconnected,         // Not connected
    False,                  // Not paired
    False,                  // Not authenticated
    0,                      // No class of device
    0,                      // No last seen
    0                       // No last used
  );

  // Button text depends on scanning state
  if FDiscoveryCoordinator.IsScanning then
    ButtonText := 'Scanning...'
  else
    ButtonText := 'Scan for devices';

  // Create action display item with special properties
  Result := TDeviceDisplayItem.Create(
    DummyDevice,
    dsAction,                    // Action item source type
    ButtonText,                  // Display text
    False,                       // Not pinned
    btUnknown,                   // No device type
    '',                          // No last seen text
    0,                           // No timestamp
    -1,                          // Special sort group (positions before discovered devices)
    TBatteryStatus.NotSupported, // No battery
    '',                          // No battery text
    [],                          // No profiles
    FDiscoveryCoordinator.IsScanning  // Action in progress flag
  );
end;

procedure TMainPresenter.RefreshDisplayItems;
var
  PairedItems: TDeviceDisplayItemArray;
  UnpairedItems: TList<TDeviceDisplayItem>;
  I: Integer;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('RefreshDisplayItems: Starting', ClassName);

  // Build display items for paired devices
  PairedItems := FDisplayItemBuilder.BuildDisplayItems(GetDevicesArray);
  LogDebug('RefreshDisplayItems: Paired=%d', [Length(PairedItems)], ClassName);

  // Build display items for unpaired devices if ShowUnpairedDevices is enabled
  if FLayoutConfig.ShowUnpairedDevices then
  begin
    // Scan button appears when ShowUnpairedDevices is enabled, regardless of whether there are unpaired devices
    LogDebug('RefreshDisplayItems: Building unpaired section, count=%d', [FDiscoveryCoordinator.UnpairedDevices.Count], ClassName);
    UnpairedItems := TList<TDeviceDisplayItem>.Create;
    try
      // Build unpaired device items (if any)
      for I := 0 to FDiscoveryCoordinator.UnpairedDevices.Count - 1 do
      begin
        Device := FDiscoveryCoordinator.UnpairedDevices[I];
        // Skip devices that have become paired (they're now in FDeviceIndexMap)
        if FDeviceIndexMap.ContainsKey(Device.AddressInt) then
        begin
          LogDebug('RefreshDisplayItems: Skipping $%.12X (now in paired list)', [Device.AddressInt], ClassName);
          Continue;
        end;

        // Skip devices marked as hidden
        if FDeviceConfigProvider.GetDeviceConfig(Device.AddressInt).Hidden then
        begin
          LogDebug('RefreshDisplayItems: Skipping $%.12X (marked as hidden)', [Device.AddressInt], ClassName);
          Continue;
        end;

        // Filter unidentified devices if ShowUnidentifiedDevices is disabled.
        // Only filters truly empty names (Device.Name = ''), NOT generic names like "Bluetooth XX:XX:XX..."
        // Generic names are considered better than nothing and remain visible.
        if not FLayoutConfig.ShowUnidentifiedDevices then
        begin
          if Device.Name = '' then  // Only truly empty names are filtered
          begin
            LogDebug('RefreshDisplayItems: Filtering unidentified device $%.12X (empty name)',
              [Device.AddressInt], ClassName);
            Continue;
          end;
        end;

        LogDebug('RefreshDisplayItems: Adding unpaired device $%.12X, Name="%s"',
          [Device.AddressInt, Device.Name], ClassName);
        UnpairedItems.Add(FDisplayItemBuilder.BuildDiscoveredDeviceDisplayItem(Device));
      end;

      // Merge paired, scan button, and unpaired items
      SetLength(FDisplayItems, Length(PairedItems) + 1 + UnpairedItems.Count);

      // Copy paired items
      for I := 0 to High(PairedItems) do
        FDisplayItems[I] := PairedItems[I];

      // Insert scan action button after paired items (always shown when ShowUnpairedDevices is enabled)
      FDisplayItems[Length(PairedItems)] := CreateScanActionItem;

      // Copy unpaired items after scan button (may be empty if no devices discovered yet)
      for I := 0 to UnpairedItems.Count - 1 do
        FDisplayItems[Length(PairedItems) + 1 + I] := UnpairedItems[I];

      LogDebug('RefreshDisplayItems: Total=%d (Paired=%d, Scan=1, Unpaired=%d)',
        [Length(FDisplayItems), Length(PairedItems), UnpairedItems.Count], ClassName);
    finally
      UnpairedItems.Free;
    end;
  end
  else
  begin
    // ShowUnpairedDevices is disabled - no scan button, just paired items
    FDisplayItems := PairedItems;
    LogDebug('RefreshDisplayItems: Total=%d (Paired only)', [Length(FDisplayItems)], ClassName);
  end;

  // Apply persistent status messages after building display items
  ApplyStatusMessages;

  FDeviceListView.ShowDisplayItems(FDisplayItems);
end;

procedure TMainPresenter.ApplyStatusMessages;
var
  I: Integer;
  DeviceAddress: UInt64;
  StatusMessage: string;
begin
  // Apply stored status messages to display items
  for I := 0 to High(FDisplayItems) do
  begin
    DeviceAddress := FDisplayItems[I].Device.AddressInt;
    if FDeviceStatusMessages.TryGetValue(DeviceAddress, StatusMessage) then
    begin
      FDisplayItems[I].StatusText := StatusMessage;
      LogDebug('ApplyStatusMessages: Applied status to device $%.12X - "%s"',
        [DeviceAddress, StatusMessage], ClassName);
    end;
  end;
end;

function TMainPresenter.GetDevicesArray: TBluetoothDeviceInfoArray;
var
  I: Integer;
begin
  if not FDevicesArrayValid then
  begin
    SetLength(FDevicesArrayCache, FDeviceList.Count);
    for I := 0 to FDeviceList.Count - 1 do
      FDevicesArrayCache[I] := FDeviceList[I];
    FDevicesArrayValid := True;
  end;
  Result := FDevicesArrayCache;
end;

procedure TMainPresenter.InvalidateDevicesArrayCache;
begin
  FDevicesArrayValid := False;
end;

procedure TMainPresenter.UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);
var
  Index: Integer;
begin
  // O(1) lookup via dictionary, O(1) update at index or O(1) amortized append
  if FDeviceIndexMap.TryGetValue(ADevice.AddressInt, Index) then
  begin
    // Device exists - update in-place (index doesn't change)
    FDeviceList[Index] := ADevice;
    InvalidateDevicesArrayCache;
    LogDebug('UpdateOrAddDevice: Updated device at index %d', [Index], ClassName);
  end
  else
  begin
    // Device not found - append and add to index map
    Index := FDeviceList.Count;
    FDeviceList.Add(ADevice);
    FDeviceIndexMap.Add(ADevice.AddressInt, Index);
    InvalidateDevicesArrayCache;
    LogDebug('UpdateOrAddDevice: Added new device, total=%d', [FDeviceList.Count], ClassName);
  end;
end;

function TMainPresenter.FindDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
var
  Index: Integer;
begin
  // O(1) lookup via dictionary
  if FDeviceIndexMap.TryGetValue(AAddress, Index) then
    Result := FDeviceList[Index]
  else
    Result := Default(TBluetoothDeviceInfo);  // Empty record with AddressInt=0
end;

function TMainPresenter.GetDeviceCount: Integer;
begin
  Result := FDeviceList.Count;
end;

function TMainPresenter.GetConnectedDeviceCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FDeviceList.Count - 1 do
    if FDeviceList[I].IsConnected then
      Inc(Result);
end;

function TMainPresenter.GetConnectedDeviceAddresses: TArray<UInt64>;
var
  I, Count: Integer;
begin
  // Skip if battery display is disabled
  if not FAppearanceConfig.ShowBatteryLevel then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Compute connected addresses on-demand - O(n) but n is small (~10 devices)
  // and this is called infrequently (view shown, devices loaded)
  SetLength(Result, FDeviceList.Count);
  Count := 0;
  for I := 0 to FDeviceList.Count - 1 do
    if FDeviceList[I].IsConnected then
    begin
      Result[Count] := FDeviceList[I].AddressInt;
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function TMainPresenter.GetBatteryCache: IBatteryCache;
begin
  Result := FBatteryCache;
end;

procedure TMainPresenter.SetToggleStateSafe(AState: Boolean);
begin
  FUpdatingToggle := True;
  try
    FToggleView.SetToggleState(AState);
  finally
    FUpdatingToggle := False;
  end;
end;

procedure TMainPresenter.QueueIfNotShutdown(AProc: TProc);
begin
  TThread.Queue(nil,
    procedure
    begin
      if not FIsShutdown then
        AProc();
    end
  );
end;

{ Service event handlers }

procedure TMainPresenter.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  LDevice: TBluetoothDeviceInfo;
begin
  LogInfo('HandleDeviceStateChanged: Address=$%.12X, Name="%s", State=%d (%s)', [
    ADevice.AddressInt, ADevice.Name, Ord(ADevice.ConnectionState),
    GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(ADevice.ConnectionState))
  ], ClassName);

  if Trim(ADevice.Name) = '' then
  begin
    LogDebug('HandleDeviceStateChanged: Empty name, skipping', ClassName);
    Exit;
  end;

  LDevice := ADevice;

  QueueIfNotShutdown(
    procedure
    var
      RadioEnabled: Boolean;
    begin
      LogInfo('HandleDeviceStateChanged (queued): Updating device list for %s, State=%d (%s)', [
        LDevice.Name, Ord(LDevice.ConnectionState),
        GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(LDevice.ConnectionState))
      ], ClassName);

      // Ignore device state changes when radio is disabled to prevent race condition where
      // async device state events arrive after ClearDevices was called during radio shutdown
      if not FRadioStateManager.GetState(RadioEnabled) or not RadioEnabled then
      begin
        LogDebug('HandleDeviceStateChanged (queued): Radio disabled, ignoring event', ClassName);
        Exit;
      end;

      // Update local cache using copy-on-write pattern
      UpdateOrAddDevice(LDevice);
      LogDebug('HandleDeviceStateChanged (queued): Device list updated, count=%d', [FDeviceList.Count], ClassName);

      // Update device in persistent config
      // Always update LastSeen on ANY state change - if we're getting events, the device is active/seen
      // This ensures immediate updates without waiting for terminal states (csConnected/csDisconnected)
      FDeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, Now);
      LogDebug('HandleDeviceStateChanged: Updated LastSeen for device $%.12X, state=%d',
        [LDevice.AddressInt, Ord(LDevice.ConnectionState)], ClassName);

      // Handle state-specific logic
      if LDevice.ConnectionState = csConnected then
      begin
        // Set battery status to "pending" only if we don't have a cached value.
        // If we already have a battery level from before (e.g., device reconnecting after BT toggle),
        // keep showing the cached value instead of replacing it with pending icon.
        // Do NOT request immediate refresh because Windows device properties (used by
        // SetupAPI) take several seconds to update after device reconnects.
        // Schedule delayed refresh to get accurate battery level.
        if FAppearanceConfig.ShowBatteryLevel then
        begin
          if not FBatteryCache.HasCachedStatus(LDevice.AddressInt) then
            FBatteryCache.SetBatteryStatus(LDevice.AddressInt, TBatteryStatus.Pending);
          FBatteryCoordinator.ScheduleDelayedBatteryRefresh(LDevice.AddressInt, BATTERY_REFRESH_DELAY_MS);
        end;
      end
      else if LDevice.ConnectionState = csDisconnected then
      begin
        // Invalidate battery cache so next connect won't show stale value
        FBatteryCache.Remove(LDevice.AddressInt);
      end;
      FAppConfig.SaveIfModified;

      // Rebuild display items (device may have moved groups due to state change)
      RefreshDisplayItems;

      FStatusView.ShowStatus(Format('%s: %s', [LDevice.Name, TDeviceFormatter.FormatConnectionState(LDevice.ConnectionState)]));
      FNotificationCoordinator.ShowNotification(LDevice);
    end
  );
end;

procedure TMainPresenter.HandleDeviceListChanged(Sender: TObject);
begin
  QueueIfNotShutdown(
    procedure
    begin
      LoadDevices;
    end
  );
end;

procedure TMainPresenter.HandleError(Sender: TObject; const AMessage: string;
  AErrorCode: Cardinal);
begin
  QueueIfNotShutdown(
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
  // Skip if presenter is shutting down
  if FIsShutdown then
    Exit;

  LogInfo('HandleRadioStateChanged: Enabled=%s', [BoolToStr(AEnabled, True)], ClassName);

  SetToggleStateSafe(AEnabled);

  if AEnabled then
  begin
    FStatusView.ShowStatus('Loading devices...');
    LoadDevicesDelayed;
  end
  else
  begin
    FStatusView.ShowStatus('Bluetooth disabled');
    CancelDelayedLoad;
    FDeviceListView.ClearDevices;
  end;
end;

{ Public methods called by View }

procedure TMainPresenter.OnDeviceClicked(const ADevice: TBluetoothDeviceInfo);
var
  CurrentDevice: TBluetoothDeviceInfo;
begin
  LogInfo('OnDeviceClicked: Device clicked - Name="%s", Address=$%.12X',
    [ADevice.Name, ADevice.AddressInt], ClassName);

  // Look up current state from internal cache, not stale UI copy
  CurrentDevice := FindDeviceByAddress(ADevice.AddressInt);

  if CurrentDevice.AddressInt = 0 then
  begin
    // Device not in paired list - it's an unpaired discovered device
    LogInfo('OnDeviceClicked: Unpaired discovered device (Address=$%.12X), initiating pairing', [ADevice.AddressInt], ClassName);
    PairDeviceAsync(ADevice);
    Exit;
  end;

  LogInfo('OnDeviceClicked: Found in cache: CurrentState=%d (%s), IsConnected=%s',
    [Ord(CurrentDevice.ConnectionState),
     GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(CurrentDevice.ConnectionState)),
     BoolToStr(CurrentDevice.IsConnected, True)], ClassName);

  if CurrentDevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    LogInfo('OnDeviceClicked: Operation in progress, ignoring', ClassName);
    FStatusView.ShowStatus('Operation in progress...');
    Exit;
  end;

  if CurrentDevice.IsConnected then
  begin
    LogInfo('OnDeviceClicked: ACTION=DISCONNECT (IsConnected=True)', ClassName);
    FStatusView.ShowStatus(Format('Disconnecting %s...', [CurrentDevice.Name]));
  end
  else
  begin
    LogInfo('OnDeviceClicked: ACTION=CONNECT (IsConnected=False)', ClassName);
    FStatusView.ShowStatus(Format('Connecting %s...', [CurrentDevice.Name]));
  end;

  // Menu stays open - user can see connection progress and click more devices
  // Menu will close on focus loss or explicit hotkey/close

  ToggleConnectionAsync(CurrentDevice);
end;

procedure TMainPresenter.OnDevicePinToggled(ADeviceAddress: UInt64);
var
  Device: TBluetoothDeviceInfo;
  DeviceConfig: TDeviceConfig;
begin
  LogInfo('OnDevicePinToggled: Address=$%.12X', [ADeviceAddress], ClassName);

  Device := FindDeviceByAddress(ADeviceAddress);
  if Device.AddressInt = 0 then
  begin
    LogWarning('OnDevicePinToggled: Device not found', ClassName);
    Exit;
  end;

  // Toggle pin state
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(ADeviceAddress);
  DeviceConfig.Pinned := not DeviceConfig.Pinned;
  FDeviceConfigProvider.SetDeviceConfig(DeviceConfig);

  LogInfo('OnDevicePinToggled: Pin state changed to %s', [BoolToStr(DeviceConfig.Pinned, True)], ClassName);

  // Refresh display to show updated pin state
  RefreshDisplayItems;
end;

procedure TMainPresenter.OnCopyDeviceName(ADeviceAddress: UInt64);
var
  Device: TBluetoothDeviceInfo;
begin
  Device := FindDeviceByAddress(ADeviceAddress);
  if Device.AddressInt = 0 then
  begin
    // Try unpaired devices
    Device := FDiscoveryCoordinator.FindUnpairedDevice(ADeviceAddress);
  end;

  if Device.AddressInt <> 0 then
  begin
    Vcl.Clipbrd.Clipboard.AsText := Device.Name;
    LogInfo('OnCopyDeviceName: Copied "%s" to clipboard', [Device.Name], ClassName);
  end;
end;

procedure TMainPresenter.OnCopyDeviceAddress(ADeviceAddress: UInt64);
var
  AddressStr: string;
begin
  AddressStr := Format('%.12X', [ADeviceAddress]);
  Vcl.Clipbrd.Clipboard.AsText := AddressStr;
  LogInfo('OnCopyDeviceAddress: Copied "%s" to clipboard', [AddressStr], ClassName);
end;

procedure TMainPresenter.OnDeviceHidden(ADeviceAddress: UInt64);
var
  Device: TBluetoothDeviceInfo;
  DeviceConfig: TDeviceConfig;
begin
  LogInfo('OnDeviceHidden: Address=$%.12X', [ADeviceAddress], ClassName);

  Device := FindDeviceByAddress(ADeviceAddress);
  if Device.AddressInt = 0 then
  begin
    // Try unpaired devices
    Device := FDiscoveryCoordinator.FindUnpairedDevice(ADeviceAddress);
  end;

  if Device.AddressInt <> 0 then
  begin
    // Set hidden flag
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(ADeviceAddress);
    DeviceConfig.Hidden := True;
    FDeviceConfigProvider.SetDeviceConfig(DeviceConfig);

    LogInfo('OnDeviceHidden: Device hidden', ClassName);

    // Refresh display to remove from list
    RefreshDisplayItems;
  end
  else
  begin
    LogWarning('OnDeviceHidden: Device not found', ClassName);
  end;
end;

procedure TMainPresenter.OnUnpairDevice(ADeviceAddress: UInt64);
var
  Device: TBluetoothDeviceInfo;
  PairingResult: TPairingResult;
begin
  LogInfo('OnUnpairDevice: Address=$%.12X', [ADeviceAddress], ClassName);

  Device := FindDeviceByAddress(ADeviceAddress);
  if Device.AddressInt = 0 then
  begin
    LogWarning('OnUnpairDevice: Device not found', ClassName);
    Exit;
  end;

  FStatusView.ShowStatus(Format('Unpairing %s...', [Device.Name]));

  // Call unpair synchronously
  PairingResult := FPairingService.UnpairDevice(ADeviceAddress);

  if PairingResult.IsSuccess then
  begin
    LogInfo('OnUnpairDevice: Successfully unpaired', ClassName);
    FStatusView.ShowStatus(Format('Unpaired %s', [Device.Name]));

    // Disconnect if connected
    if Device.IsConnected then
    begin
      LogInfo('OnUnpairDevice: Disconnecting device before removal', ClassName);
      FBluetoothService.Disconnect(Device);
    end;

    // Remove device from repository to prevent re-adding when Windows cache still has it
    FBluetoothService.RemoveDevice(ADeviceAddress);

    // Remove device from presenter's list immediately (Windows cache may not reflect unpair yet)
    RemoveDeviceFromList(ADeviceAddress);

    // Refresh display
    RefreshDisplayItems;
  end
  else
  begin
    LogError('OnUnpairDevice: Failed - %s', [PairingResult.ErrorMessage], ClassName);
    FStatusView.ShowStatus(Format('Failed to unpair %s: %s', [Device.Name, PairingResult.ErrorMessage]));
  end;
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
  // Clear unpaired devices cache on refresh (volatile discovery data)
  FDiscoveryCoordinator.ClearUnpairedDevices;

  FStatusView.ShowStatus('Refreshing...');
  LoadDevices;
end;

procedure TMainPresenter.OnScanRequested;
begin
  FDiscoveryCoordinator.StartScan;
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

procedure TMainPresenter.OnBluetoothSettingsRequested;
const
  WINDOWS_BLUETOOTH_SETTINGS_URI = 'ms-settings:bluetooth';
var
  Result: HINST;
begin
  LogInfo('OnBluetoothSettingsRequested: Opening Bluetooth settings', ClassName);

  // Win7 uses classic Control Panel, Win8+ uses modern Settings app
  if TWinRTSupport.IsAvailable then
  begin
    LogDebug('OnBluetoothSettingsRequested: Opening modern Settings app', ClassName);
    Result := ShellExecute(0, 'open', WINDOWS_BLUETOOTH_SETTINGS_URI, nil, nil, SW_SHOWNORMAL);
  end
  else
  begin
    LogDebug('OnBluetoothSettingsRequested: Opening classic Control Panel', ClassName);
    Result := ShellExecute(0, 'open', 'control.exe', 'bthprops.cpl', nil, SW_SHOWNORMAL);
  end;

  if Result <= 32 then
    LogWarning('OnBluetoothSettingsRequested: Failed to open Bluetooth settings, error code: %d', [Result], ClassName);
end;

procedure TMainPresenter.OnSettingsChanged;
begin
  LogInfo('OnSettingsChanged', ClassName);
  // Note: Polling mode changes require application restart to take effect.
  // Other settings (theme, hotkey, etc.) are applied immediately by MainForm.
end;

procedure TMainPresenter.OnViewShown;
begin
  FBatteryCoordinator.RefreshBatteryForConnectedDevices;
end;

function TMainPresenter.CanClose: Boolean;
begin
  Result := not FWindowConfig.CloseToTray;
end;

end.
