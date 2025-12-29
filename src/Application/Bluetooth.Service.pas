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
    procedure DoError(const AMessage: string; AErrorCode: Cardinal);

    function ConnectWithStrategy(
      const ADevice: TBluetoothDeviceInfo;
      AEnable: Boolean
    ): Boolean;

    { Device monitor event handlers }
    procedure HandleMonitorDeviceStateChanged(Sender: TObject;
      const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
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

    function GetOnDeviceStateChanged: TDeviceStateChangedEvent;
    procedure SetOnDeviceStateChanged(AValue: TDeviceStateChangedEvent);
    function GetOnDeviceListChanged: TDeviceListChangedEvent;
    procedure SetOnDeviceListChanged(AValue: TDeviceListChangedEvent);
    function GetOnError: TBluetoothErrorEvent;
    procedure SetOnError(AValue: TBluetoothErrorEvent);

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
/// Factory function for dependency injection.
/// </summary>
function CreateBluetoothService(
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigQuery;
  AStrategyFactory: IConnectionStrategyFactory
): IBluetoothService;

implementation

uses
  Bluetooth.DeviceMonitors,
  Bluetooth.DeviceRepository,
  Bluetooth.ConnectionExecutor,
  Bluetooth.AdapterQuery,
  Bluetooth.EventDebouncer;

function CreateBluetoothService(
  APollingConfig: IPollingConfig;
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigQuery;
  AStrategyFactory: IConnectionStrategyFactory
): IBluetoothService;
var
  MonitorFactory: IDeviceMonitorFactory;
  Debouncer: IEventDebouncer;
begin
  MonitorFactory := TDeviceMonitorFactory.Create(APollingConfig);
  Debouncer := TDeviceEventDebouncer.Create(500); // 500ms default debounce interval
  Result := TBluetoothService.Create(
    AConnectionConfig,
    ADeviceConfigProvider,
    AStrategyFactory,
    MonitorFactory.CreateMonitor,
    CreateDeviceRepository(AConnectionConfig),
    CreateConnectionExecutor,
    CreateAdapterQuery,
    Debouncer
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

procedure TBluetoothService.HandleMonitorError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  LogWarning('HandleMonitorError: %s (code %d)', [AMessage, AErrorCode], ClassName);
  DoError(AMessage, AErrorCode);
end;

end.
