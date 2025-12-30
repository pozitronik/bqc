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
  App.AppearanceConfigIntf,
  App.AsyncExecutor,
  App.DeviceDisplayTypes,
  UI.DeviceDisplayItemBuilder;

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

    { Injected configuration dependencies }
    FAppConfig: IAppConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FAppearanceConfig: IAppearanceConfig;
    FRadioStateManager: IRadioStateManager;
    FAsyncExecutor: IAsyncExecutor;
    FBatteryCache: IBatteryCache;
    FDeviceList: TList<TBluetoothDeviceInfo>;
    FDeviceIndexMap: TDictionary<UInt64, Integer>;  // Address -> Index for O(1) lookup
    FDevicesArrayCache: TBluetoothDeviceInfoArray;
    FDevicesArrayValid: Boolean;
    FDisplayItems: TDeviceDisplayItemArray;
    FDisplayItemBuilder: TDeviceDisplayItemBuilder;
    FDelayedLoadGeneration: Integer;  // Generation counter for delayed load cancellation
    FUpdatingToggle: Boolean;

    { Service event handlers }
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleRadioStateChanged(Sender: TObject; AEnabled: Boolean);
    procedure HandleBatteryQueryCompleted(Sender: TObject; ADeviceAddress: UInt64;
      const AStatus: TBatteryStatus);

    procedure RefreshBatteryForConnectedDevices;
    procedure ScheduleDelayedBatteryRefresh(AAddress: UInt64; ADelayMs: Integer);

    { Internal methods }
    procedure LoadDevices;
    procedure LoadDevicesDelayed;
    procedure CancelDelayedLoad;
    procedure AutoConnectDevices;
    procedure ConnectDeviceAsync(const ADevice: TBluetoothDeviceInfo);
    procedure ToggleConnectionAsync(const ADevice: TBluetoothDeviceInfo);
    procedure SetRadioStateAsync(AEnable: Boolean);

    function GetDeviceDisplayName(const ADevice: TBluetoothDeviceInfo): string;
    procedure ShowDeviceNotification(const ADevice: TBluetoothDeviceInfo);
    procedure RefreshDisplayItems;

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
    /// <param name="ARadioStateManager">Bluetooth radio state manager.</param>
    /// <param name="AAsyncExecutor">Async executor for background operations.</param>
    /// <param name="ABluetoothService">Bluetooth service for device operations.</param>
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
      ARadioStateManager: IRadioStateManager;
      AAsyncExecutor: IAsyncExecutor;
      ABluetoothService: IBluetoothService
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
  App.DeviceConfigTypes,
  Bluetooth.BatteryQuery,
  Bluetooth.ProfileQuery,
  UI.DeviceFormatter;

const
  // Debounces rapid device list updates after connection state changes
  DELAYED_LOAD_INTERVAL_MS = 500;
  // Windows battery drivers need time to update after device reconnects
  BATTERY_REFRESH_DELAY_MS = 10000;

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
  ARadioStateManager: IRadioStateManager;
  AAsyncExecutor: IAsyncExecutor;
  ABluetoothService: IBluetoothService
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
  FRadioStateManager := ARadioStateManager;
  FAsyncExecutor := AAsyncExecutor;
  FBluetoothService := ABluetoothService;
  FBatteryCache := nil;
  FDeviceList := TList<TBluetoothDeviceInfo>.Create;
  FDeviceIndexMap := TDictionary<UInt64, Integer>.Create;
  FDevicesArrayCache := nil;
  FDevicesArrayValid := False;
  FDisplayItems := nil;
  FDisplayItemBuilder := TDeviceDisplayItemBuilder.Create(
    FDeviceConfigProvider,
    FAppearanceConfig,
    FAppConfig.AsProfileConfig,
    CreateProfileQuery
  );
  FDelayedLoadGeneration := 0;
  FUpdatingToggle := False;
  LogDebug('Created', ClassName);
end;

destructor TMainPresenter.Destroy;
begin
  Shutdown;
  FBatteryCache := nil;
  FDeviceIndexMap.Free;
  FDeviceList.Free;
  FDisplayItemBuilder.Free;
  LogDebug('Destroyed', ClassName);
  inherited;
end;

procedure TMainPresenter.Initialize;
var
  RadioEnabled: Boolean;
begin
  LogDebug('Initialize: Starting', ClassName);

  // Create battery cache if battery display is enabled
  if FAppearanceConfig.ShowBatteryLevel then
  begin
    LogDebug('Initialize: Creating battery cache', ClassName);
    FBatteryCache := CreateBatteryCache(CreateBatteryQuery);
    FBatteryCache.OnQueryCompleted := HandleBatteryQueryCompleted;
    FDisplayItemBuilder.SetBatteryCache(FBatteryCache);
  end;

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

  // Cancel any pending delayed load
  CancelDelayedLoad;

  // Stop radio state watching
  if FRadioStateManager <> nil then
    FRadioStateManager.StopWatching;

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
      // Register device but don't update LastSeen (pass 0)
      // LastSeen is only updated when device actually connects
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

    // Trigger battery level refresh for connected devices
    RefreshBatteryForConnectedDevices;
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
      TThread.Queue(nil,
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
  FDisplayItems := FDisplayItemBuilder.BuildDisplayItems(GetDevicesArray);
  FDeviceListView.ShowDisplayItems(FDisplayItems);
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

procedure TMainPresenter.SetToggleStateSafe(AState: Boolean);
begin
  FUpdatingToggle := True;
  try
    FToggleView.SetToggleState(AState);
  finally
    FUpdatingToggle := False;
  end;
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

  TThread.Queue(nil,
    procedure
    begin
      LogInfo('HandleDeviceStateChanged (queued): Updating device list for %s, State=%d (%s)', [
        LDevice.Name, Ord(LDevice.ConnectionState),
        GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(LDevice.ConnectionState))
      ], ClassName);

      // Update local cache using copy-on-write pattern
      UpdateOrAddDevice(LDevice);
      LogDebug('HandleDeviceStateChanged (queued): Device list updated, count=%d', [FDeviceList.Count], ClassName);

      // Update device in persistent config
      // Update LastSeen when device connects or disconnects - the moment we last "saw" it active
      // For other state changes (connecting, disconnecting), pass 0 to preserve existing value
      if LDevice.ConnectionState = csConnected then
      begin
        FDeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, Now);
        // Set battery status to "pending" - shows ellipsis icon while refreshing.
        // Do NOT request immediate refresh because Windows device properties (used by
        // SetupAPI) take several seconds to update after device reconnects.
        // Schedule delayed refresh to get accurate battery level.
        if (FBatteryCache <> nil) and FAppearanceConfig.ShowBatteryLevel then
        begin
          FBatteryCache.SetBatteryStatus(LDevice.AddressInt, TBatteryStatus.Pending);
          ScheduleDelayedBatteryRefresh(LDevice.AddressInt, BATTERY_REFRESH_DELAY_MS);
        end;
      end
      else if LDevice.ConnectionState = csDisconnected then
      begin
        // Update LastSeen on disconnect - this is the last time device was active
        FDeviceConfigProvider.RegisterDevice(LDevice.AddressInt, LDevice.Name, Now);
        // Invalidate battery cache so next connect won't show stale value
        if FBatteryCache <> nil then
          FBatteryCache.Remove(LDevice.AddressInt);
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

procedure TMainPresenter.HandleBatteryQueryCompleted(Sender: TObject;
  ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
begin
  LogDebug('HandleBatteryQueryCompleted: Address=$%.12X, Level=%d', [ADeviceAddress, AStatus.Level], ClassName);
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
  SetLength(Addresses, FDeviceList.Count);
  for I := 0 to FDeviceList.Count - 1 do
  begin
    Device := FDeviceList[I];
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

procedure TMainPresenter.ScheduleDelayedBatteryRefresh(AAddress: UInt64; ADelayMs: Integer);
var
  LAddress: UInt64;
  LBatteryCache: IBatteryCache;
begin
  if FBatteryCache = nil then
    Exit;

  LAddress := AAddress;
  LBatteryCache := FBatteryCache;

  FAsyncExecutor.RunDelayed(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        begin
          if LBatteryCache <> nil then
            LBatteryCache.RequestRefresh(LAddress);
        end
      );
    end,
    ADelayMs
  );
end;

{ Public methods called by View }

procedure TMainPresenter.OnDeviceClicked(const ADevice: TBluetoothDeviceInfo);
var
  CurrentDevice: TBluetoothDeviceInfo;
  I: Integer;
begin
  LogInfo('=== OnDeviceClicked START ===', ClassName);
  LogInfo('OnDeviceClicked: UI passed device: Name="%s", Address=$%.12X, UIState=%d (%s)',
    [ADevice.Name, ADevice.AddressInt, Ord(ADevice.ConnectionState),
     GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(ADevice.ConnectionState))], ClassName);

  // Dump all devices in FDeviceList for debugging
  LogDebug('OnDeviceClicked: FDeviceList count=%d', [FDeviceList.Count], ClassName);
  for I := 0 to FDeviceList.Count - 1 do
    LogDebug('  FDeviceList[%d]: Address=$%.12X, Name="%s", State=%d (%s)',
      [I, FDeviceList[I].AddressInt, FDeviceList[I].Name, Ord(FDeviceList[I].ConnectionState),
       GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(FDeviceList[I].ConnectionState))], ClassName);

  // Look up current state from internal cache, not stale UI copy
  CurrentDevice := FindDeviceByAddress(ADevice.AddressInt);
  if CurrentDevice.AddressInt = 0 then
  begin
    // Device not in internal list - use passed device (rare case)
    LogWarning('OnDeviceClicked: Device NOT FOUND in cache (Address=$%.12X), using UI state', [ADevice.AddressInt], ClassName);
    CurrentDevice := ADevice;
  end
  else
    LogInfo('OnDeviceClicked: Found in cache: CurrentState=%d (%s), IsConnected=%s',
      [Ord(CurrentDevice.ConnectionState),
       GetEnumName(TypeInfo(TBluetoothConnectionState), Ord(CurrentDevice.ConnectionState)),
       BoolToStr(CurrentDevice.IsConnected, True)], ClassName);

  if CurrentDevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    LogInfo('OnDeviceClicked: Operation in progress, ignoring. === END ===', ClassName);
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

  LogInfo('OnDeviceClicked: Calling ToggleConnectionAsync. === END ===', ClassName);
  ToggleConnectionAsync(CurrentDevice);
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
