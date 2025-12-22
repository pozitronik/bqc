{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Bluetooth Service Implementation           }
{                                                       }
{       Implements IBluetoothService facade.            }
{       Follows Single Responsibility by delegating     }
{       to specialized components.                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.Service;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.ConnectionStrategies,
  App.Logger,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Main Bluetooth service implementation.
  /// Facade pattern: Provides unified interface to Bluetooth subsystem.
  /// </summary>
  TBluetoothService = class(TInterfacedObject, IBluetoothService)
  private
    FOnDeviceStateChanged: TDeviceStateChangedEvent;
    FOnError: TBluetoothErrorEvent;
    FRadioHandle: THandle;

    { Injected dependencies }
    FConnectionConfig: IConnectionConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FStrategyFactory: IConnectionStrategyFactory;
    FDeviceMonitor: IDeviceMonitor;
    FDeviceRepository: IDeviceRepository;

    procedure CloseRadio;
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
    { IBluetoothService }
    function IsAdapterAvailable: Boolean;
    function GetPairedDevices: TBluetoothDeviceInfoArray;
    function RefreshAllDevices: TBluetoothDeviceInfoArray;
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
      ADeviceConfigProvider: IDeviceConfigProvider;
      AStrategyFactory: IConnectionStrategyFactory;
      ADeviceMonitor: IDeviceMonitor;
      ADeviceRepository: IDeviceRepository
    );
    destructor Destroy; override;

    { Read-only access to injected dependencies }
    property ConnectionConfig: IConnectionConfig read FConnectionConfig;
    property DeviceConfigProvider: IDeviceConfigProvider read FDeviceConfigProvider;
    property StrategyFactory: IConnectionStrategyFactory read FStrategyFactory;
    property DeviceMonitor: IDeviceMonitor read FDeviceMonitor;
    property DeviceRepository: IDeviceRepository read FDeviceRepository;
  end;

/// <summary>
/// Creates the default Bluetooth service instance.
/// Factory function for dependency injection.
/// </summary>
function CreateBluetoothService: IBluetoothService;

implementation

uses
  App.Bootstrap,
  Bluetooth.DeviceMonitors,
  Bluetooth.DeviceRepository;

function CreateBluetoothService: IBluetoothService;
var
  MonitorFactory: IDeviceMonitorFactory;
begin
  MonitorFactory := TDeviceMonitorFactory.Create(Bootstrap.PollingConfig);
  Result := TBluetoothService.Create(
    Bootstrap.ConnectionConfig,
    Bootstrap.DeviceConfigProvider,
    Bootstrap.ConnectionStrategyFactory,
    MonitorFactory.CreateMonitor,
    CreateDeviceRepository
  );
end;

{ TBluetoothService }

constructor TBluetoothService.Create(
  AConnectionConfig: IConnectionConfig;
  ADeviceConfigProvider: IDeviceConfigProvider;
  AStrategyFactory: IConnectionStrategyFactory;
  ADeviceMonitor: IDeviceMonitor;
  ADeviceRepository: IDeviceRepository
);
begin
  inherited Create;

  // Store injected dependencies
  FConnectionConfig := AConnectionConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FStrategyFactory := AStrategyFactory;
  FDeviceMonitor := ADeviceMonitor;
  FDeviceRepository := ADeviceRepository;

  FRadioHandle := 0;

  // Configure and start device monitor
  FDeviceMonitor.OnDeviceStateChanged := HandleMonitorDeviceStateChanged;
  FDeviceMonitor.OnError := HandleMonitorError;

  if FDeviceMonitor.Start then
    Log('[Service] Create: Device monitor started successfully')
  else
    Log('[Service] Create: Device monitor failed to start');
end;

destructor TBluetoothService.Destroy;
begin
  if FDeviceMonitor <> nil then
    FDeviceMonitor.Stop;
  CloseRadio;
  inherited Destroy;
end;

procedure TBluetoothService.CloseRadio;
begin
  if FRadioHandle <> 0 then
  begin
    CloseHandle(FRadioHandle);
    FRadioHandle := 0;
  end;
end;

function TBluetoothService.IsAdapterAvailable: Boolean;
var
  FindParams: BLUETOOTH_FIND_RADIO_PARAMS;
  FindHandle: HBLUETOOTH_RADIO_FIND;
  RadioHandle: THandle;
begin
  FindParams.dwSize := SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  FindHandle := BluetoothFindFirstRadio(@FindParams, RadioHandle);

  Result := FindHandle <> 0;

  if Result then
  begin
    CloseHandle(RadioHandle);
    BluetoothFindRadioClose(FindHandle);
  end;
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
  ServiceGuid: TGUID;
  WinDeviceInfo: BLUETOOTH_DEVICE_INFO;
  ServiceFlag: DWORD;
  ErrorCode: DWORD;
  UpdatedDevice: TBluetoothDeviceInfo;
  AnySuccess: Boolean;
  RetryCount, Attempt: Integer;
  DeviceConfig: TDeviceConfig;
begin
  Result := False;
  AnySuccess := False;
  ErrorCode := ERROR_SUCCESS;

  // Get device-specific configuration
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(ADevice.AddressInt);

  // Get retry count: device-specific if set, otherwise global
  if DeviceConfig.ConnectionRetryCount >= 0 then
    RetryCount := DeviceConfig.ConnectionRetryCount
  else
    RetryCount := FConnectionConfig.ConnectionRetryCount;

  // Bounds checking
  if RetryCount < 0 then
    RetryCount := 0;
  if RetryCount > 10 then
    RetryCount := 10;  // Cap at 10 retries

  Log('[Service] ConnectWithStrategy: Device=%s, RetryCount=%d (device-specific=%s)', [
    ADevice.Name, RetryCount, BoolToStr(DeviceConfig.ConnectionRetryCount >= 0, True)
  ]);

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

  // Prepare device info structure
  InitDeviceInfo(WinDeviceInfo);
  WinDeviceInfo.Address.ullLong := ADevice.AddressInt;
  Move(ADevice.Address[0], WinDeviceInfo.Address.rgBytes[0], 6);

  // Notify state change (connecting/disconnecting)
  if AEnable then
    UpdatedDevice := ADevice.WithConnectionState(csConnecting)
  else
    UpdatedDevice := ADevice.WithConnectionState(csDisconnecting);
  DoDeviceStateChanged(UpdatedDevice);

  // Set service flag
  if AEnable then
    ServiceFlag := BLUETOOTH_SERVICE_ENABLE
  else
    ServiceFlag := BLUETOOTH_SERVICE_DISABLE;

  // Try connection with retries
  for Attempt := 0 to RetryCount do
  begin
    if Attempt > 0 then
    begin
      Log('[Service] ConnectWithStrategy: Retry attempt %d of %d', [Attempt, RetryCount]);
      Sleep(500);  // Brief pause between retries
    end;

    AnySuccess := False;
    ErrorCode := ERROR_SUCCESS;

    // Try each service GUID
    for ServiceGuid in ServiceGuids do
    begin
      ErrorCode := BluetoothSetServiceState(
        0,  // Use first available radio
        @WinDeviceInfo,
        @ServiceGuid,
        ServiceFlag
      );

      if ErrorCode = ERROR_SUCCESS then
        AnySuccess := True;
    end;

    if AnySuccess then
      Break;  // Success, no need for more retries
  end;

  Result := AnySuccess;

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
    DoError('Failed to change connection state', ErrorCode);
  end;

  // Update repository and notify
  FDeviceRepository.AddOrUpdate(UpdatedDevice);
  DoDeviceStateChanged(UpdatedDevice);
end;

procedure TBluetoothService.DoDeviceStateChanged(
  const ADevice: TBluetoothDeviceInfo);
begin
  Log('[Service] DoDeviceStateChanged: Address=$%.12X, Name="%s", ConnectionState=%d, Handler assigned=%s', [
    ADevice.AddressInt,
    ADevice.Name,
    Ord(ADevice.ConnectionState),
    BoolToStr(Assigned(FOnDeviceStateChanged), True)
  ]);
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
begin
  Log('[Service] HandleMonitorDeviceStateChanged: Address=$%.12X, NewState=%d', [
    ADeviceAddress, Ord(ANewState)
  ]);

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
  Log('[Service] HandleMonitorError: %s (code %d)', [AMessage, AErrorCode]);
  DoError(AMessage, AErrorCode);
end;

end.
