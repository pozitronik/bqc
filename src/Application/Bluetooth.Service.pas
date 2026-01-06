{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Bluetooth Service Implementation           }
{                                                       }
{       Implements IBluetoothService facade.            }
{       Follows Single Responsibility by delegating     }
{       to specialized components.                      }
{                                                       }
{*******************************************************}

unit Bluetooth.Service;

interface

uses
  System.SysUtils,
  System.Classes,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionStrategies,
  App.Logger,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  App.ConnectionConfigIntf;

type
  /// <summary>
  /// Main Bluetooth service implementation.
  /// Facade pattern: Provides unified interface to Bluetooth subsystem.
  /// Implements granular interfaces for ISP compliance.
  /// </summary>
  TBluetoothService = class(TInterfacedObject, IBluetoothService,
    IBluetoothDeviceEnumerator, IBluetoothConnectionManager)
  private
    FOnDeviceStateChanged: TDeviceStateChangedEvent;
    FOnDeviceDiscovered: TDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    FOnError: TBluetoothErrorEvent;

    { Injected dependencies }
    FConnectionConfig: IConnectionConfig;
    FDeviceConfigProvider: IDeviceConfigQuery;
    FStrategyFactory: IConnectionStrategyFactory;
    FDeviceMonitor: IDeviceMonitor;
    FDeviceRepository: IDeviceRepository;
    FConnectionExecutor: IConnectionExecutor;
    FAdapterQuery: IBluetoothAdapterQuery;
    FEventDebouncer: IEventDebouncer;

    procedure DoDeviceStateChanged(const ADevice: TBluetoothDeviceInfo);
    procedure DoDeviceDiscovered(const ADevice: TBluetoothDeviceInfo);
    procedure DoDeviceOutOfRange(const ADeviceAddress: UInt64);
    procedure DoError(const AMessage: string; AErrorCode: Cardinal);

    function ConnectWithStrategy(
      const ADevice: TBluetoothDeviceInfo;
      AEnable: Boolean
    ): Boolean;

    { Device monitor event handlers }
    procedure HandleMonitorDeviceStateChanged(Sender: TObject;
      const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
    procedure HandleMonitorDeviceDiscovered(Sender: TObject;
      const ADevice: TBluetoothDeviceInfo);
    procedure HandleMonitorDeviceOutOfRange(Sender: TObject;
      const ADeviceAddress: UInt64);
    procedure HandleMonitorError(Sender: TObject;
      const AMessage: string; AErrorCode: Cardinal);

  protected
    { IBluetoothService / IBluetoothDeviceEnumerator / IBluetoothConnectionManager }
    function IsAdapterAvailable: Boolean;
    function GetPairedDevices: TBluetoothDeviceInfoArray;
    function RefreshAllDevices: TBluetoothDeviceInfoArray;
    function RefreshDeviceStatus(const ADevice: TBluetoothDeviceInfo): TBluetoothDeviceInfo;
    function Connect(const ADevice: TBluetoothDeviceInfo): Boolean;
    function Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;
    function ToggleConnection(const ADevice: TBluetoothDeviceInfo): Boolean;
    procedure RemoveDevice(ADeviceAddress: UInt64);

    function GetOnDeviceStateChanged: TDeviceStateChangedEvent;
    procedure SetOnDeviceStateChanged(AValue: TDeviceStateChangedEvent);
    function GetOnDeviceListChanged: TDeviceListChangedEvent;
    procedure SetOnDeviceListChanged(AValue: TDeviceListChangedEvent);
    function GetOnDeviceDiscovered: TDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TDeviceDiscoveredEvent);
    function GetOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TDeviceOutOfRangeEvent);
    function GetOnError: TBluetoothErrorEvent;
    procedure SetOnError(AValue: TBluetoothErrorEvent);
    procedure TriggerDiscoveryScan;
    procedure ScanForNearbyDevices;

  public
    constructor Create(
      AConnectionConfig: IConnectionConfig;
      ADeviceConfigProvider: IDeviceConfigQuery;
      AStrategyFactory: IConnectionStrategyFactory;
      ADeviceMonitor: IDeviceMonitor;
      ADeviceRepository: IDeviceRepository;
      AConnectionExecutor: IConnectionExecutor;
      AAdapterQuery: IBluetoothAdapterQuery;
      AEventDebouncer: IEventDebouncer
    );
    destructor Destroy; override;

    { Read-only access to injected dependencies }
    property ConnectionConfig: IConnectionConfig read FConnectionConfig;
    property DeviceConfigProvider: IDeviceConfigQuery read FDeviceConfigProvider;
    property StrategyFactory: IConnectionStrategyFactory read FStrategyFactory;
    property DeviceMonitor: IDeviceMonitor read FDeviceMonitor;
    property DeviceRepository: IDeviceRepository read FDeviceRepository;
    property ConnectionExecutor: IConnectionExecutor read FConnectionExecutor;
    property AdapterQuery: IBluetoothAdapterQuery read FAdapterQuery;
    property EventDebouncer: IEventDebouncer read FEventDebouncer;
  end;

/// <summary>
/// Creates the default Bluetooth service instance.
/// Factory function with optional dependency overrides for testability.
/// When nil is passed for optional parameters, default implementations are created.
/// </summary>
function CreateBluetoothService(
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigQuery;
  AStrategyFactory: IConnectionStrategyFactory;
  // Optional dependencies - nil means use default implementation
  ADeviceMonitor: IDeviceMonitor = nil;
  ADeviceRepository: IDeviceRepository = nil;
  AConnectionExecutor: IConnectionExecutor = nil;
  AAdapterQuery: IBluetoothAdapterQuery = nil;
  AEventDebouncer: IEventDebouncer = nil
): IBluetoothService;

implementation

uses
  Winapi.Windows,
  Bluetooth.WinAPI,
  Bluetooth.DeviceConverter,
  Bluetooth.DeviceMonitors,
  Bluetooth.DeviceRepository,
  Bluetooth.ConnectionExecutor,
  Bluetooth.AdapterQuery,
  Bluetooth.EventDebouncer,
  App.SystemClock;

const
  // Bluetooth inquiry timeout multiplier for device discovery
  // Each unit = 1.28 seconds, so 10 = ~12.8 seconds (standard duration)
  INQUIRY_TIMEOUT_MULTIPLIER = 10;

function CreateBluetoothService(
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigQuery;
  AStrategyFactory: IConnectionStrategyFactory;
  ADeviceMonitor: IDeviceMonitor;
  ADeviceRepository: IDeviceRepository;
  AConnectionExecutor: IConnectionExecutor;
  AAdapterQuery: IBluetoothAdapterQuery;
  AEventDebouncer: IEventDebouncer
): IBluetoothService;
var
  LDeviceMonitor: IDeviceMonitor;
  LDeviceRepository: IDeviceRepository;
  LConnectionExecutor: IConnectionExecutor;
  LAdapterQuery: IBluetoothAdapterQuery;
  LEventDebouncer: IEventDebouncer;
  MonitorFactory: IDeviceMonitorFactory;
begin
  // Use provided dependencies or create defaults
  if ADeviceMonitor <> nil then
    LDeviceMonitor := ADeviceMonitor
  else
  begin
    MonitorFactory := TDeviceMonitorFactory.Create(APollingConfig);
    LDeviceMonitor := MonitorFactory.CreateMonitor;
  end;

  if ADeviceRepository <> nil then
    LDeviceRepository := ADeviceRepository
  else
    LDeviceRepository := CreateDeviceRepository(AConnectionConfig);

  if AConnectionExecutor <> nil then
    LConnectionExecutor := AConnectionExecutor
  else
    LConnectionExecutor := CreateConnectionExecutor;

  if AAdapterQuery <> nil then
    LAdapterQuery := AAdapterQuery
  else
    LAdapterQuery := CreateAdapterQuery;

  if AEventDebouncer <> nil then
    LEventDebouncer := AEventDebouncer
  else
    LEventDebouncer := TDeviceEventDebouncer.Create(SystemClock, 500);

  Result := TBluetoothService.Create(
    AConnectionConfig,
    ADeviceConfigProvider,
    AStrategyFactory,
    LDeviceMonitor,
    LDeviceRepository,
    LConnectionExecutor,
    LAdapterQuery,
    LEventDebouncer
  );
end;

{ TBluetoothService }

constructor TBluetoothService.Create(
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigQuery;
  AStrategyFactory: IConnectionStrategyFactory;
  ADeviceMonitor: IDeviceMonitor;
  ADeviceRepository: IDeviceRepository;
  AConnectionExecutor: IConnectionExecutor;
  AAdapterQuery: IBluetoothAdapterQuery;
  AEventDebouncer: IEventDebouncer
);
begin
  inherited Create;

  // Store injected dependencies
  FConnectionConfig := AConnectionConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FStrategyFactory := AStrategyFactory;
  FDeviceMonitor := ADeviceMonitor;
  FDeviceRepository := ADeviceRepository;
  FConnectionExecutor := AConnectionExecutor;
  FAdapterQuery := AAdapterQuery;
  FEventDebouncer := AEventDebouncer;

  // Configure and start device monitor
  FDeviceMonitor.OnDeviceStateChanged := HandleMonitorDeviceStateChanged;
  FDeviceMonitor.SetOnDeviceDiscovered(HandleMonitorDeviceDiscovered);
  FDeviceMonitor.SetOnDeviceOutOfRange(HandleMonitorDeviceOutOfRange);
  FDeviceMonitor.OnError := HandleMonitorError;

  if FDeviceMonitor.Start then
    LogDebug('Create: Device monitor started successfully', ClassName)
  else
    LogDebug('Create: Device monitor failed to start', ClassName);

  // Initial device enumeration
  FDeviceRepository.Refresh;
  LogDebug('Create: Initial device enumeration complete, %d devices found', [
    FDeviceRepository.Count
  ], ClassName);
end;

destructor TBluetoothService.Destroy;
begin
  if FDeviceMonitor <> nil then
    FDeviceMonitor.Stop;
  inherited Destroy;
end;

function TBluetoothService.IsAdapterAvailable: Boolean;
begin
  Result := FAdapterQuery.IsAdapterAvailable;
end;

function TBluetoothService.GetPairedDevices: TBluetoothDeviceInfoArray;
begin
  Result := FDeviceRepository.GetAll;
end;

function TBluetoothService.RefreshAllDevices: TBluetoothDeviceInfoArray;
begin
  FDeviceRepository.Refresh;
  Result := FDeviceRepository.GetAll;
end;

function TBluetoothService.RefreshDeviceStatus(
  const ADevice: TBluetoothDeviceInfo): TBluetoothDeviceInfo;
begin
  // Repository is kept up-to-date by the device monitor
  // Return current cached state, or original if not found
  if not FDeviceRepository.TryGetByAddress(ADevice.AddressInt, Result) then
    Result := ADevice;
end;

function TBluetoothService.Connect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := ConnectWithStrategy(ADevice, True);
end;

function TBluetoothService.Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := ConnectWithStrategy(ADevice, False);
end;

function TBluetoothService.ToggleConnection(
  const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  if ADevice.IsConnected then
    Result := Disconnect(ADevice)
  else
    Result := Connect(ADevice);
end;

procedure TBluetoothService.RemoveDevice(ADeviceAddress: UInt64);
begin
  LogDebug('RemoveDevice: Removing device $%.12X from repository', [ADeviceAddress], ClassName);
  FDeviceRepository.Remove(ADeviceAddress);
end;

function TBluetoothService.ConnectWithStrategy(
  const ADevice: TBluetoothDeviceInfo;
  AEnable: Boolean
): Boolean;
var
  Strategy: IConnectionStrategy;
  ServiceGuids: TArray<TGUID>;
  UpdatedDevice: TBluetoothDeviceInfo;
  RetryCount: Integer;
  DeviceConfig: TDeviceConfig;
  ExecResult: TConnectionResult;
begin
  Result := False;

  // Get device-specific configuration
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt);

  // Get retry count: device-specific if set, otherwise global
  if DeviceConfig.ConnectionRetryCount >= 0 then
    RetryCount := DeviceConfig.ConnectionRetryCount
  else
    RetryCount := FConnectionConfig.ConnectionRetryCount;

  LogDebug('ConnectWithStrategy: Device=%s, RetryCount=%d (device-specific=%s)', [
    ADevice.Name, RetryCount, BoolToStr(DeviceConfig.ConnectionRetryCount >= 0, True)
  ], ClassName);

  // Get appropriate strategy for this device type
  Strategy := FStrategyFactory.GetStrategy(ADevice.DeviceType);
  if Strategy = nil then
  begin
    DoError('No connection strategy found for device type', 0);
    Exit;
  end;

  // Get service GUIDs from strategy
  ServiceGuids := Strategy.GetServiceGuids;
  if Length(ServiceGuids) = 0 then
  begin
    DoError('No service GUIDs defined for device', 0);
    Exit;
  end;

  // Notify state change (connecting/disconnecting)
  if AEnable then
    UpdatedDevice := ADevice.WithConnectionState(csConnecting)
  else
    UpdatedDevice := ADevice.WithConnectionState(csDisconnecting);
  DoDeviceStateChanged(UpdatedDevice);

  // Execute connection via executor
  ExecResult := FConnectionExecutor.Execute(ADevice, ServiceGuids, AEnable, RetryCount);
  Result := ExecResult.Success;

  // Update final state
  if Result then
  begin
    if AEnable then
      UpdatedDevice := ADevice.WithConnectionState(csConnected)
    else
      UpdatedDevice := ADevice.WithConnectionState(csDisconnected);
  end
  else
  begin
    UpdatedDevice := ADevice.WithConnectionState(csError);
    DoError('Failed to change connection state', ExecResult.ErrorCode);
  end;

  // Update repository and notify
  FDeviceRepository.AddOrUpdate(UpdatedDevice);
  DoDeviceStateChanged(UpdatedDevice);
end;

procedure TBluetoothService.DoDeviceStateChanged(
  const ADevice: TBluetoothDeviceInfo);
begin
  LogDebug('DoDeviceStateChanged: Address=$%.12X, Name="%s", ConnectionState=%d, Handler assigned=%s', [
    ADevice.AddressInt,
    ADevice.Name,
    Ord(ADevice.ConnectionState),
    BoolToStr(Assigned(FOnDeviceStateChanged), True)
  ], ClassName);
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, ADevice);
end;

procedure TBluetoothService.DoDeviceDiscovered(
  const ADevice: TBluetoothDeviceInfo);
begin
  LogDebug('DoDeviceDiscovered: Address=$%.12X, Name="%s", Handler assigned=%s', [
    ADevice.AddressInt,
    ADevice.Name,
    BoolToStr(Assigned(FOnDeviceDiscovered), True)
  ], ClassName);
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADevice);
end;

procedure TBluetoothService.DoDeviceOutOfRange(const ADeviceAddress: UInt64);
begin
  LogDebug('DoDeviceOutOfRange: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceOutOfRange), True)
  ], ClassName);
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TBluetoothService.DoError(const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

function TBluetoothService.GetOnDeviceStateChanged: TDeviceStateChangedEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TBluetoothService.SetOnDeviceStateChanged(
  AValue: TDeviceStateChangedEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TBluetoothService.GetOnDeviceListChanged: TDeviceListChangedEvent;
begin
  Result := FDeviceRepository.OnListChanged;
end;

procedure TBluetoothService.SetOnDeviceListChanged(
  AValue: TDeviceListChangedEvent);
begin
  FDeviceRepository.OnListChanged := AValue;
end;

function TBluetoothService.GetOnDeviceDiscovered: TDeviceDiscoveredEvent;
begin
  Result := FOnDeviceDiscovered;
end;

procedure TBluetoothService.SetOnDeviceDiscovered(AValue: TDeviceDiscoveredEvent);
begin
  FOnDeviceDiscovered := AValue;
end;

function TBluetoothService.GetOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
begin
  Result := FOnDeviceOutOfRange;
end;

procedure TBluetoothService.SetOnDeviceOutOfRange(AValue: TDeviceOutOfRangeEvent);
begin
  FOnDeviceOutOfRange := AValue;
end;

function TBluetoothService.GetOnError: TBluetoothErrorEvent;
begin
  Result := FOnError;
end;

procedure TBluetoothService.SetOnError(AValue: TBluetoothErrorEvent);
begin
  FOnError := AValue;
end;

{ Device Monitor Event Handlers }

procedure TBluetoothService.HandleMonitorDeviceStateChanged(Sender: TObject;
  const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
var
  Device: TBluetoothDeviceInfo;
  UpdatedDevice: TBluetoothDeviceInfo;
  EventType: TDeviceEventType;
begin
  LogDebug('HandleMonitorDeviceStateChanged: Address=$%.12X, NewState=%d', [
    ADeviceAddress, Ord(ANewState)
  ], ClassName);

  // Determine event type based on new state
  case ANewState of
    csConnected: EventType := detConnect;
    csDisconnected: EventType := detDisconnect;
  else
    EventType := detAttributeChange;
  end;

  // Check with debouncer if event should be processed
  if Assigned(FEventDebouncer) then
  begin
    if not FEventDebouncer.ShouldProcess(ADeviceAddress, EventType, ANewState) then
    begin
      LogDebug('HandleMonitorDeviceStateChanged: Event filtered by debouncer', ClassName);
      Exit;
    end;
  end;

  // Check if device is in repository
  if FDeviceRepository.TryGetByAddress(ADeviceAddress, Device) then
  begin
    // Update device with new state
    UpdatedDevice := Device.WithConnectionState(ANewState);
    FDeviceRepository.AddOrUpdate(UpdatedDevice);
    DoDeviceStateChanged(UpdatedDevice);
  end
  else
  begin
    // Device not in repository - refresh from Windows and try again
    FDeviceRepository.Refresh;
    if FDeviceRepository.TryGetByAddress(ADeviceAddress, Device) then
    begin
      UpdatedDevice := Device.WithConnectionState(ANewState);
      FDeviceRepository.AddOrUpdate(UpdatedDevice);
      DoDeviceStateChanged(UpdatedDevice);
    end;
  end;
end;

procedure TBluetoothService.HandleMonitorDeviceDiscovered(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  LogDebug('HandleMonitorDeviceDiscovered: Address=$%.12X, Name="%s"', [
    ADevice.AddressInt,
    ADevice.Name
  ], ClassName);
  // Delegate to event handler
  DoDeviceDiscovered(ADevice);
end;

procedure TBluetoothService.HandleMonitorDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
begin
  LogDebug('HandleMonitorDeviceOutOfRange: Address=$%.12X', [ADeviceAddress], ClassName);
  // Delegate to event handler
  DoDeviceOutOfRange(ADeviceAddress);
end;

procedure TBluetoothService.HandleMonitorError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  LogWarning('HandleMonitorError: %s (code %d)', [AMessage, AErrorCode], ClassName);
  DoError(AMessage, AErrorCode);
end;

procedure TBluetoothService.TriggerDiscoveryScan;
begin
  LogInfo('TriggerDiscoveryScan: Restarting device watcher to refresh discovery', ClassName);

  // Restart the device monitor to trigger fresh discovery events
  if FDeviceMonitor.IsRunning then
  begin
    LogDebug('TriggerDiscoveryScan: Stopping monitor', ClassName);
    FDeviceMonitor.Stop;
  end;

  LogDebug('TriggerDiscoveryScan: Starting monitor', ClassName);
  if FDeviceMonitor.Start then
    LogDebug('TriggerDiscoveryScan: Monitor restarted successfully', ClassName)
  else
    LogWarning('TriggerDiscoveryScan: Failed to restart monitor', ClassName);

  // Also perform active inquiry to find nearby devices
  ScanForNearbyDevices;
end;

procedure TBluetoothService.ScanForNearbyDevices;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  Device: TBluetoothDeviceInfo;
begin
  LogInfo('ScanForNearbyDevices: Starting active Bluetooth inquiry (Classic BT only)', ClassName);

  // NOTE: This implementation only scans for Classic Bluetooth devices.
  // Future enhancement: Add BLE device scanning using WinRT BluetoothLEAdvertisementWatcher
  // when bpWinRT platform is selected. BLE devices should also fire OnDeviceDiscovered
  // events to populate the unpaired devices cache.

  // Initialize search parameters for unpaired devices (Classic BT)
  FillChar(SearchParams, SizeOf(SearchParams), 0);
  SearchParams.dwSize := SizeOf(BLUETOOTH_DEVICE_SEARCH_PARAMS);
  SearchParams.fReturnAuthenticated := False;  // Don't return paired devices
  SearchParams.fReturnRemembered := False;     // Don't return remembered devices
  SearchParams.fReturnConnected := False;      // Don't return connected devices
  SearchParams.fReturnUnknown := True;         // DO return unknown/unpaired devices
  SearchParams.fIssueInquiry := True;          // Perform active Bluetooth inquiry
  SearchParams.cTimeoutMultiplier := INQUIRY_TIMEOUT_MULTIPLIER;  // Standard inquiry timeout (~12.8s)
  SearchParams.hRadio := 0;                    // Use default radio

  DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);

  FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);
  if FindHandle <> 0 then
  begin
    try
      repeat
        // Convert and fire discovered event
        Device := ConvertBluetoothDeviceInfo(DeviceInfo);
        LogDebug('ScanForNearbyDevices: Found device Address=$%.12X, Name="%s", Paired=%s',
          [Device.AddressInt, Device.Name, BoolToStr(Device.IsPaired, True)], ClassName);

        // Only fire event for unpaired devices
        if not Device.IsPaired then
          DoDeviceDiscovered(Device);

        DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
      until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
    finally
      BluetoothFindDeviceClose(FindHandle);
    end;
    LogInfo('ScanForNearbyDevices: Inquiry complete', ClassName);
  end
  else
    LogDebug('ScanForNearbyDevices: No unpaired devices found (error=%d)', [GetLastError], ClassName);
end;

end.
