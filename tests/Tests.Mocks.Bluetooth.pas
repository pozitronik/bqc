{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations - Bluetooth                }
{                                                       }
{*******************************************************}

unit Tests.Mocks.Bluetooth;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.EventDebouncer,
  Bluetooth.RadioControl;

type
  /// <summary>
  /// Mock implementation of IConnectionStrategy for testing.
  /// </summary>
  TMockConnectionStrategy = class(TInterfacedObject, IConnectionStrategy)
  private
    FPriority: Integer;
    FSupportedTypes: TArray<TBluetoothDeviceType>;
    FServiceGuids: TArray<TGUID>;
    FCanHandleResult: Boolean;
  public
    constructor Create; overload;
    constructor Create(APriority: Integer; ASupportedTypes: TArray<TBluetoothDeviceType>); overload;

    // IConnectionStrategy
    function CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
    function GetServiceGuids: TArray<TGUID>;
    function GetPriority: Integer;

    // Test configuration
    property Priority: Integer read FPriority write FPriority;
    property SupportedTypes: TArray<TBluetoothDeviceType> read FSupportedTypes write FSupportedTypes;
    property ServiceGuids: TArray<TGUID> read FServiceGuids write FServiceGuids;
    property CanHandleResult: Boolean read FCanHandleResult write FCanHandleResult;
  end;

  /// <summary>
  /// Mock implementation of IConnectionStrategyFactory for testing.
  /// </summary>
  TMockConnectionStrategyFactory = class(TInterfacedObject, IConnectionStrategyFactory)
  private
    FStrategies: TList<IConnectionStrategy>;
    FGetStrategyCallCount: Integer;
    FLastRequestedDeviceType: TBluetoothDeviceType;
  public
    constructor Create;
    destructor Destroy; override;

    // IConnectionStrategyFactory
    function GetStrategy(ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
    procedure RegisterStrategy(AStrategy: IConnectionStrategy);
    function GetAllStrategies: TArray<IConnectionStrategy>;

    // Test helpers
    procedure Clear;
    property GetStrategyCallCount: Integer read FGetStrategyCallCount;
    property LastRequestedDeviceType: TBluetoothDeviceType read FLastRequestedDeviceType;
  end;

  /// <summary>
  /// Mock implementation of IDeviceMonitor for testing.
  /// </summary>
  TMockDeviceMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FRunning: Boolean;
    FStartResult: Boolean;
    FStartCallCount: Integer;
    FStopCallCount: Integer;
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnError: TMonitorErrorEvent;
  public
    constructor Create;

    // IDeviceMonitor
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);

    // Test helpers
    procedure SimulateDeviceStateChanged(AAddress: UInt64; AState: TBluetoothConnectionState);
    procedure SimulateError(const AMessage: string; AErrorCode: Cardinal);

    property StartResult: Boolean read FStartResult write FStartResult;
    property StartCallCount: Integer read FStartCallCount;
    property StopCallCount: Integer read FStopCallCount;
    property Running: Boolean read FRunning write FRunning;
    property OnDeviceStateChanged: TMonitorDeviceStateEvent read FOnDeviceStateChanged write FOnDeviceStateChanged;
    property OnError: TMonitorErrorEvent read FOnError write FOnError;
  end;

  /// <summary>
  /// Mock implementation of IDeviceRepository for testing.
  /// </summary>
  TMockDeviceRepository = class(TInterfacedObject, IDeviceRepository)
  private
    FDevices: TDictionary<UInt64, TBluetoothDeviceInfo>;
    FOnListChanged: TDeviceListChangedEvent;
    FRefreshCallCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceRepository
    function GetAll: TBluetoothDeviceInfoArray;
    function GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    function TryGetByAddress(AAddress: UInt64; out ADevice: TBluetoothDeviceInfo): Boolean;
    function Contains(AAddress: UInt64): Boolean;
    procedure AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
    function UpdateConnectionState(AAddress: UInt64; AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
    procedure Remove(AAddress: UInt64);
    procedure Clear;
    procedure Refresh;
    function GetCount: Integer;
    function GetOnListChanged: TDeviceListChangedEvent;
    procedure SetOnListChanged(AValue: TDeviceListChangedEvent);

    // Test helpers
    property Devices: TDictionary<UInt64, TBluetoothDeviceInfo> read FDevices;
    property RefreshCallCount: Integer read FRefreshCallCount;
    property Count: Integer read GetCount;
    property OnListChanged: TDeviceListChangedEvent read FOnListChanged write FOnListChanged;
  end;

  /// <summary>
  /// Mock implementation of IConnectionExecutor for testing.
  /// </summary>
  TMockConnectionExecutor = class(TInterfacedObject, IConnectionExecutor)
  private
    FExecuteResult: TConnectionResult;
    FExecuteCallCount: Integer;
    FLastDevice: TBluetoothDeviceInfo;
    FLastEnable: Boolean;
    FLastRetryCount: Integer;
  public
    constructor Create;

    // IConnectionExecutor
    function Execute(
      const ADevice: TBluetoothDeviceInfo;
      const AServiceGuids: TArray<TGUID>;
      AEnable: Boolean;
      ARetryCount: Integer
    ): TConnectionResult;

    // Test configuration & verification
    property ExecuteResult: TConnectionResult read FExecuteResult write FExecuteResult;
    property ExecuteCallCount: Integer read FExecuteCallCount;
    property LastDevice: TBluetoothDeviceInfo read FLastDevice;
    property LastEnable: Boolean read FLastEnable;
    property LastRetryCount: Integer read FLastRetryCount;
  end;

  /// <summary>
  /// Mock implementation of IBluetoothAdapterQuery for testing.
  /// </summary>
  TMockAdapterQuery = class(TInterfacedObject, IBluetoothAdapterQuery)
  private
    FAdapterAvailable: Boolean;
    FAdapterName: string;
    FIsAdapterAvailableCallCount: Integer;
    FGetAdapterNameCallCount: Integer;
  public
    constructor Create;

    // IBluetoothAdapterQuery
    function IsAdapterAvailable: Boolean;
    function GetAdapterName: string;

    // Test configuration
    property AdapterAvailable: Boolean read FAdapterAvailable write FAdapterAvailable;
    property AdapterName: string read FAdapterName write FAdapterName;
    property IsAdapterAvailableCallCount: Integer read FIsAdapterAvailableCallCount;
    property GetAdapterNameCallCount: Integer read FGetAdapterNameCallCount;
  end;

  /// <summary>
  /// Mock implementation of IBluetoothDeviceQuery for testing.
  /// </summary>
  TMockBluetoothDeviceQuery = class(TInterfacedObject, IBluetoothDeviceQuery)
  private
    FDevices: TBluetoothDeviceInfoArray;
    FEnumerateCallCount: Integer;
  public
    constructor Create;

    // IBluetoothDeviceQuery
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;

    // Test helpers
    procedure SetDevices(const ADevices: TBluetoothDeviceInfoArray);
    procedure AddDevice(const ADevice: TBluetoothDeviceInfo);
    procedure ClearDevices;

    property EnumerateCallCount: Integer read FEnumerateCallCount;
  end;

  /// <summary>
  /// Mock implementation of IEventDebouncer for testing.
  /// </summary>
  TMockEventDebouncer = class(TInterfacedObject, IEventDebouncer)
  private
    FDebounceMs: Integer;
    FShouldProcessResult: Boolean;
    FShouldProcessCallCount: Integer;
    FClearCallCount: Integer;
    FLastAddress: UInt64;
    FLastEventType: TDeviceEventType;
    FLastConnectionState: TBluetoothConnectionState;
  public
    constructor Create;

    // IEventDebouncer
    function ShouldProcess(AAddress: UInt64; AEventType: TDeviceEventType;
      AConnectionState: TBluetoothConnectionState): Boolean;
    procedure Clear;
    function GetDebounceMs: Integer;
    procedure SetDebounceMs(AValue: Integer);

    // Test configuration
    property ShouldProcessResult: Boolean read FShouldProcessResult write FShouldProcessResult;
    property ShouldProcessCallCount: Integer read FShouldProcessCallCount;
    property ClearCallCount: Integer read FClearCallCount;
    property LastAddress: UInt64 read FLastAddress;
    property LastEventType: TDeviceEventType read FLastEventType;
    property LastConnectionState: TBluetoothConnectionState read FLastConnectionState;
    property DebounceMs: Integer read FDebounceMs write FDebounceMs;
  end;

  /// <summary>
  /// Mock implementation of IRadioStateManager for testing.
  /// </summary>
  TMockRadioStateManager = class(TInterfacedObject, IRadioStateManager)
  private
    FRadioEnabled: Boolean;
    FRadioAvailable: Boolean;
    FSetStateResult: TRadioControlResult;
    FGetStateCallCount: Integer;
    FSetStateCallCount: Integer;
    FStartWatchingCallCount: Integer;
    FStopWatchingCallCount: Integer;
    FLastSetStateValue: Boolean;
    FOnStateChanged: TRadioStateChangedEvent;
  public
    constructor Create;

    // IRadioStateManager
    function GetState(out AEnabled: Boolean): Boolean;
    function SetState(AEnable: Boolean): TRadioControlResult;
    function SetStateEx(AEnable: Boolean): TRadioControlResultEx;
    procedure StartWatching;
    procedure StopWatching;
    function GetOnStateChanged: TRadioStateChangedEvent;
    procedure SetOnStateChanged(AValue: TRadioStateChangedEvent);

    // Test helpers
    procedure SimulateStateChanged(AEnabled: Boolean);

    property RadioEnabled: Boolean read FRadioEnabled write FRadioEnabled;
    property RadioAvailable: Boolean read FRadioAvailable write FRadioAvailable;
    property SetStateResult: TRadioControlResult read FSetStateResult write FSetStateResult;
    property GetStateCallCount: Integer read FGetStateCallCount;
    property SetStateCallCount: Integer read FSetStateCallCount;
    property StartWatchingCallCount: Integer read FStartWatchingCallCount;
    property StopWatchingCallCount: Integer read FStopWatchingCallCount;
    property LastSetStateValue: Boolean read FLastSetStateValue;
  end;

  /// <summary>
  /// Mock implementation of IBatteryQueryStrategy for testing.
  /// </summary>
  TMockBatteryQueryStrategy = class(TInterfacedObject, IBatteryQueryStrategy)
  private
    FPriority: Integer;
    FName: string;
    FTryQueryResult: Boolean;
    FBatteryStatus: TBatteryStatus;
    FTryQueryCallCount: Integer;
    FLastQueryAddress: UInt64;
    FLastQueryTimeout: Cardinal;
  public
    constructor Create(APriority: Integer = 100; const AName: string = 'Mock');

    // IBatteryQueryStrategy
    function TryQuery(ADeviceAddress: UInt64; ATimeoutMs: Cardinal;
      out AStatus: TBatteryStatus): Boolean;
    function GetPriority: Integer;
    function GetName: string;

    // Test configuration
    property TryQueryResult: Boolean read FTryQueryResult write FTryQueryResult;
    property BatteryStatus: TBatteryStatus read FBatteryStatus write FBatteryStatus;
    property TryQueryCallCount: Integer read FTryQueryCallCount;
    property LastQueryAddress: UInt64 read FLastQueryAddress;
    property LastQueryTimeout: Cardinal read FLastQueryTimeout;
    property Priority: Integer read FPriority write FPriority;
    property Name: string read FName write FName;
  end;

  /// <summary>
  /// Mock implementation of IBatteryQuery for testing.
  /// </summary>
  TMockBatteryQuery = class(TInterfacedObject, IBatteryQuery)
  private
    FBatteryStatus: TBatteryStatus;
    FGetBatteryLevelCallCount: Integer;
    FLastQueryAddress: UInt64;
  public
    constructor Create;

    // IBatteryQuery
    function GetBatteryLevel(ADeviceAddress: UInt64): TBatteryStatus;
    function GetBatteryLevelWithTimeout(ADeviceAddress: UInt64;
      ATimeoutMs: Cardinal): TBatteryStatus;

    // Test configuration
    property BatteryStatus: TBatteryStatus read FBatteryStatus write FBatteryStatus;
    property GetBatteryLevelCallCount: Integer read FGetBatteryLevelCallCount;
    property LastQueryAddress: UInt64 read FLastQueryAddress;
  end;

  /// <summary>
  /// Mock implementation of IBatteryCache for testing.
  /// </summary>
  TMockBatteryCache = class(TInterfacedObject, IBatteryCache)
  private
    FCache: TDictionary<UInt64, TBatteryStatus>;
    FOnQueryCompleted: TBatteryQueryCompletedEvent;
    FRequestRefreshCallCount: Integer;
    FRequestRefreshAllCallCount: Integer;
    FClearCallCount: Integer;
    FLastRefreshAddress: UInt64;
  public
    constructor Create;
    destructor Destroy; override;

    // IBatteryCache
    function GetBatteryStatus(ADeviceAddress: UInt64): TBatteryStatus;
    function HasCachedStatus(ADeviceAddress: UInt64): Boolean;
    procedure RequestRefresh(ADeviceAddress: UInt64);
    procedure RequestRefreshAll(const ADeviceAddresses: TArray<UInt64>);
    procedure Clear;
    procedure Remove(ADeviceAddress: UInt64);
    function GetOnQueryCompleted: TBatteryQueryCompletedEvent;
    procedure SetOnQueryCompleted(AValue: TBatteryQueryCompletedEvent);

    // Test helpers
    procedure SetBatteryStatus(ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
    procedure SimulateQueryCompleted(AAddress: UInt64; const AStatus: TBatteryStatus);

    property RequestRefreshCallCount: Integer read FRequestRefreshCallCount;
    property RequestRefreshAllCallCount: Integer read FRequestRefreshAllCallCount;
    property ClearCallCount: Integer read FClearCallCount;
    property LastRefreshAddress: UInt64 read FLastRefreshAddress;
  end;

  /// <summary>
  /// Mock implementation of IBluetoothService for testing.
  /// </summary>
  TMockBluetoothService = class(TInterfacedObject, IBluetoothService)
  private
    FDevices: TBluetoothDeviceInfoArray;
    FConnectResult: TConnectionResult;
    FDisconnectResult: TConnectionResult;
    FAdapterAvailable: Boolean;
    FAdapterName: string;
    FOnDeviceStateChanged: TDeviceStateChangedEvent;
    FOnDeviceListChanged: TDeviceListChangedEvent;
    FOnError: TBluetoothErrorEvent;

    FConnectCallCount: Integer;
    FDisconnectCallCount: Integer;
    FToggleConnectionCallCount: Integer;
    FRefreshAllDevicesCallCount: Integer;
    FGetDevicesCallCount: Integer;
    FIsAdapterAvailableCallCount: Integer;
    FLastConnectDevice: TBluetoothDeviceInfo;
    FLastDisconnectDevice: TBluetoothDeviceInfo;
    FLastToggleDevice: TBluetoothDeviceInfo;
  public
    constructor Create;

    // IBluetoothService
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

    // Test helpers
    procedure SimulateDeviceStateChanged(const ADevice: TBluetoothDeviceInfo);
    procedure SimulateDeviceListChanged;
    procedure SimulateError(const AMessage: string; AErrorCode: Cardinal);

    // Test configuration
    property Devices: TBluetoothDeviceInfoArray read FDevices write FDevices;
    property ConnectResult: TConnectionResult read FConnectResult write FConnectResult;
    property DisconnectResult: TConnectionResult read FDisconnectResult write FDisconnectResult;
    property AdapterAvailable: Boolean read FAdapterAvailable write FAdapterAvailable;
    property AdapterName: string read FAdapterName write FAdapterName;

    // Call counts
    property ConnectCallCount: Integer read FConnectCallCount;
    property DisconnectCallCount: Integer read FDisconnectCallCount;
    property ToggleConnectionCallCount: Integer read FToggleConnectionCallCount;
    property RefreshAllDevicesCallCount: Integer read FRefreshAllDevicesCallCount;
    property GetDevicesCallCount: Integer read FGetDevicesCallCount;
    property IsAdapterAvailableCallCount: Integer read FIsAdapterAvailableCallCount;
    property LastConnectDevice: TBluetoothDeviceInfo read FLastConnectDevice;
    property LastDisconnectDevice: TBluetoothDeviceInfo read FLastDisconnectDevice;
    property LastToggleDevice: TBluetoothDeviceInfo read FLastToggleDevice;
  end;

  /// <summary>
  /// Helper function to create test device info records.
  /// </summary>
function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;

implementation

{ Helper Functions }

function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddressInt),
    AAddressInt,
    AName,
    ADeviceType,
    AConnectionState,
    True,   // IsPaired
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

{ TMockConnectionStrategy }

constructor TMockConnectionStrategy.Create;
begin
  inherited Create;
  FPriority := 0;
  FSupportedTypes := [];
  FServiceGuids := [];
  FCanHandleResult := True;
end;

constructor TMockConnectionStrategy.Create(APriority: Integer;
  ASupportedTypes: TArray<TBluetoothDeviceType>);
begin
  inherited Create;
  FPriority := APriority;
  FSupportedTypes := ASupportedTypes;
  FServiceGuids := [];
  FCanHandleResult := True;
end;

function TMockConnectionStrategy.CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
var
  DevType: TBluetoothDeviceType;
begin
  // If FCanHandleResult is False, always return False
  if not FCanHandleResult then
    Exit(False);

  // If no specific types configured, use FCanHandleResult
  if Length(FSupportedTypes) = 0 then
    Exit(FCanHandleResult);

  // Check if device type is in supported list
  for DevType in FSupportedTypes do
    if DevType = ADeviceType then
      Exit(True);

  Result := False;
end;

function TMockConnectionStrategy.GetServiceGuids: TArray<TGUID>;
begin
  Result := FServiceGuids;
end;

function TMockConnectionStrategy.GetPriority: Integer;
begin
  Result := FPriority;
end;

{ TMockConnectionStrategyFactory }

constructor TMockConnectionStrategyFactory.Create;
begin
  inherited Create;
  FStrategies := TList<IConnectionStrategy>.Create;
  FGetStrategyCallCount := 0;
  FLastRequestedDeviceType := btUnknown;
end;

destructor TMockConnectionStrategyFactory.Destroy;
begin
  FStrategies.Free;
  inherited Destroy;
end;

function TMockConnectionStrategyFactory.GetStrategy(
  ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
var
  Strategy: IConnectionStrategy;
  BestStrategy: IConnectionStrategy;
  BestPriority: Integer;
begin
  Inc(FGetStrategyCallCount);
  FLastRequestedDeviceType := ADeviceType;

  BestStrategy := nil;
  BestPriority := -1;

  for Strategy in FStrategies do
  begin
    if Strategy.CanHandle(ADeviceType) and (Strategy.GetPriority > BestPriority) then
    begin
      BestStrategy := Strategy;
      BestPriority := Strategy.GetPriority;
    end;
  end;

  Result := BestStrategy;
end;

procedure TMockConnectionStrategyFactory.RegisterStrategy(
  AStrategy: IConnectionStrategy);
begin
  FStrategies.Add(AStrategy);
end;

function TMockConnectionStrategyFactory.GetAllStrategies: TArray<IConnectionStrategy>;
begin
  Result := FStrategies.ToArray;
end;

procedure TMockConnectionStrategyFactory.Clear;
begin
  FStrategies.Clear;
  FGetStrategyCallCount := 0;
end;

{ TMockDeviceMonitor }

constructor TMockDeviceMonitor.Create;
begin
  inherited Create;
  FRunning := False;
  FStartResult := True;
  FStartCallCount := 0;
  FStopCallCount := 0;
end;

function TMockDeviceMonitor.Start: Boolean;
begin
  Inc(FStartCallCount);
  if FStartResult then
    FRunning := True;
  Result := FStartResult;
end;

procedure TMockDeviceMonitor.Stop;
begin
  Inc(FStopCallCount);
  FRunning := False;
end;

function TMockDeviceMonitor.IsRunning: Boolean;
begin
  Result := FRunning;
end;

function TMockDeviceMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TMockDeviceMonitor.SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TMockDeviceMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TMockDeviceMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

procedure TMockDeviceMonitor.SimulateDeviceStateChanged(AAddress: UInt64;
  AState: TBluetoothConnectionState);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, AAddress, AState);
end;

procedure TMockDeviceMonitor.SimulateError(const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

{ TMockDeviceRepository }

constructor TMockDeviceRepository.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  FRefreshCallCount := 0;
end;

destructor TMockDeviceRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

function TMockDeviceRepository.GetAll: TBluetoothDeviceInfoArray;
begin
  Result := FDevices.Values.ToArray;
end;

function TMockDeviceRepository.GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    FillChar(Result, SizeOf(Result), 0);
end;

function TMockDeviceRepository.TryGetByAddress(AAddress: UInt64;
  out ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := FDevices.TryGetValue(AAddress, ADevice);
end;

function TMockDeviceRepository.Contains(AAddress: UInt64): Boolean;
begin
  Result := FDevices.ContainsKey(AAddress);
end;

procedure TMockDeviceRepository.AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
var
  IsNew: Boolean;
begin
  IsNew := not FDevices.ContainsKey(ADevice.AddressInt);
  FDevices.AddOrSetValue(ADevice.AddressInt, ADevice);
  if IsNew and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

function TMockDeviceRepository.UpdateConnectionState(AAddress: UInt64;
  AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Device: TBluetoothDeviceInfo;
begin
  if FDevices.TryGetValue(AAddress, Device) then
  begin
    Result := Device.WithConnectionState(AState);
    FDevices[AAddress] := Result;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TMockDeviceRepository.Remove(AAddress: UInt64);
var
  Existed: Boolean;
begin
  Existed := FDevices.ContainsKey(AAddress);
  FDevices.Remove(AAddress);
  if Existed and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

procedure TMockDeviceRepository.Clear;
var
  HadDevices: Boolean;
begin
  HadDevices := FDevices.Count > 0;
  FDevices.Clear;
  if HadDevices and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

procedure TMockDeviceRepository.Refresh;
begin
  Inc(FRefreshCallCount);
  // Mock refresh does nothing - test code adds devices directly
end;

function TMockDeviceRepository.GetCount: Integer;
begin
  Result := FDevices.Count;
end;

function TMockDeviceRepository.GetOnListChanged: TDeviceListChangedEvent;
begin
  Result := FOnListChanged;
end;

procedure TMockDeviceRepository.SetOnListChanged(AValue: TDeviceListChangedEvent);
begin
  FOnListChanged := AValue;
end;

{ TMockConnectionExecutor }

constructor TMockConnectionExecutor.Create;
begin
  inherited Create;
  FExecuteResult := TConnectionResult.Ok;
  FExecuteCallCount := 0;
  FLastEnable := False;
  FLastRetryCount := 0;
end;

function TMockConnectionExecutor.Execute(
  const ADevice: TBluetoothDeviceInfo;
  const AServiceGuids: TArray<TGUID>;
  AEnable: Boolean;
  ARetryCount: Integer
): TConnectionResult;
begin
  Inc(FExecuteCallCount);
  FLastDevice := ADevice;
  FLastEnable := AEnable;
  FLastRetryCount := ARetryCount;
  Result := FExecuteResult;
end;

{ TMockAdapterQuery }

constructor TMockAdapterQuery.Create;
begin
  inherited Create;
  FAdapterAvailable := True;
  FAdapterName := 'Mock Bluetooth Adapter';
  FIsAdapterAvailableCallCount := 0;
  FGetAdapterNameCallCount := 0;
end;

function TMockAdapterQuery.IsAdapterAvailable: Boolean;
begin
  Inc(FIsAdapterAvailableCallCount);
  Result := FAdapterAvailable;
end;

function TMockAdapterQuery.GetAdapterName: string;
begin
  Inc(FGetAdapterNameCallCount);
  Result := FAdapterName;
end;

{ TMockBluetoothDeviceQuery }

constructor TMockBluetoothDeviceQuery.Create;
begin
  inherited Create;
  SetLength(FDevices, 0);
  FEnumerateCallCount := 0;
end;

function TMockBluetoothDeviceQuery.EnumeratePairedDevices: TBluetoothDeviceInfoArray;
begin
  Inc(FEnumerateCallCount);
  Result := FDevices;
end;

procedure TMockBluetoothDeviceQuery.SetDevices(const ADevices: TBluetoothDeviceInfoArray);
begin
  FDevices := ADevices;
end;

procedure TMockBluetoothDeviceQuery.AddDevice(const ADevice: TBluetoothDeviceInfo);
begin
  SetLength(FDevices, Length(FDevices) + 1);
  FDevices[High(FDevices)] := ADevice;
end;

procedure TMockBluetoothDeviceQuery.ClearDevices;
begin
  SetLength(FDevices, 0);
end;

{ TMockEventDebouncer }

constructor TMockEventDebouncer.Create;
begin
  inherited Create;
  FDebounceMs := 500;
  FShouldProcessResult := True;  // Default: allow all events
  FShouldProcessCallCount := 0;
  FClearCallCount := 0;
  FLastAddress := 0;
  FLastEventType := detConnect;
  FLastConnectionState := csDisconnected;
end;

function TMockEventDebouncer.ShouldProcess(AAddress: UInt64;
  AEventType: TDeviceEventType; AConnectionState: TBluetoothConnectionState): Boolean;
begin
  Inc(FShouldProcessCallCount);
  FLastAddress := AAddress;
  FLastEventType := AEventType;
  FLastConnectionState := AConnectionState;
  Result := FShouldProcessResult;
end;

procedure TMockEventDebouncer.Clear;
begin
  Inc(FClearCallCount);
end;

function TMockEventDebouncer.GetDebounceMs: Integer;
begin
  Result := FDebounceMs;
end;

procedure TMockEventDebouncer.SetDebounceMs(AValue: Integer);
begin
  FDebounceMs := AValue;
end;

{ TMockRadioStateManager }

constructor TMockRadioStateManager.Create;
begin
  inherited Create;
  FRadioEnabled := True;
  FRadioAvailable := True;
  FSetStateResult := rcSuccess;
  FGetStateCallCount := 0;
  FSetStateCallCount := 0;
  FStartWatchingCallCount := 0;
  FStopWatchingCallCount := 0;
  FLastSetStateValue := False;
end;

function TMockRadioStateManager.GetState(out AEnabled: Boolean): Boolean;
begin
  Inc(FGetStateCallCount);
  AEnabled := FRadioEnabled;
  Result := FRadioAvailable;
end;

function TMockRadioStateManager.SetState(AEnable: Boolean): TRadioControlResult;
begin
  Inc(FSetStateCallCount);
  FLastSetStateValue := AEnable;
  if FSetStateResult = rcSuccess then
    FRadioEnabled := AEnable;
  Result := FSetStateResult;
end;

function TMockRadioStateManager.SetStateEx(AEnable: Boolean): TRadioControlResultEx;
begin
  Result.Result := SetState(AEnable);
  Result.ErrorCode := 0;
end;

procedure TMockRadioStateManager.StartWatching;
begin
  Inc(FStartWatchingCallCount);
end;

procedure TMockRadioStateManager.StopWatching;
begin
  Inc(FStopWatchingCallCount);
end;

function TMockRadioStateManager.GetOnStateChanged: TRadioStateChangedEvent;
begin
  Result := FOnStateChanged;
end;

procedure TMockRadioStateManager.SetOnStateChanged(AValue: TRadioStateChangedEvent);
begin
  FOnStateChanged := AValue;
end;

procedure TMockRadioStateManager.SimulateStateChanged(AEnabled: Boolean);
begin
  FRadioEnabled := AEnabled;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, AEnabled);
end;

{ TMockBatteryQueryStrategy }

constructor TMockBatteryQueryStrategy.Create(APriority: Integer; const AName: string);
begin
  inherited Create;
  FPriority := APriority;
  FName := AName;
  FTryQueryResult := False;
  FBatteryStatus := TBatteryStatus.NotSupported;
  FTryQueryCallCount := 0;
  FLastQueryAddress := 0;
  FLastQueryTimeout := 0;
end;

function TMockBatteryQueryStrategy.TryQuery(ADeviceAddress: UInt64; ATimeoutMs: Cardinal;
  out AStatus: TBatteryStatus): Boolean;
begin
  Inc(FTryQueryCallCount);
  FLastQueryAddress := ADeviceAddress;
  FLastQueryTimeout := ATimeoutMs;
  AStatus := FBatteryStatus;
  Result := FTryQueryResult;
end;

function TMockBatteryQueryStrategy.GetPriority: Integer;
begin
  Result := FPriority;
end;

function TMockBatteryQueryStrategy.GetName: string;
begin
  Result := FName;
end;

{ TMockBatteryQuery }

constructor TMockBatteryQuery.Create;
begin
  inherited Create;
  FBatteryStatus := TBatteryStatus.NotSupported;
  FGetBatteryLevelCallCount := 0;
  FLastQueryAddress := 0;
end;

function TMockBatteryQuery.GetBatteryLevel(ADeviceAddress: UInt64): TBatteryStatus;
begin
  Inc(FGetBatteryLevelCallCount);
  FLastQueryAddress := ADeviceAddress;
  Result := FBatteryStatus;
end;

function TMockBatteryQuery.GetBatteryLevelWithTimeout(ADeviceAddress: UInt64;
  ATimeoutMs: Cardinal): TBatteryStatus;
begin
  Inc(FGetBatteryLevelCallCount);
  FLastQueryAddress := ADeviceAddress;
  Result := FBatteryStatus;
end;

{ TMockBatteryCache }

constructor TMockBatteryCache.Create;
begin
  inherited Create;
  FCache := TDictionary<UInt64, TBatteryStatus>.Create;
  FRequestRefreshCallCount := 0;
  FRequestRefreshAllCallCount := 0;
  FClearCallCount := 0;
  FLastRefreshAddress := 0;
end;

destructor TMockBatteryCache.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TMockBatteryCache.GetBatteryStatus(ADeviceAddress: UInt64): TBatteryStatus;
begin
  if not FCache.TryGetValue(ADeviceAddress, Result) then
    Result := TBatteryStatus.NotSupported;
end;

procedure TMockBatteryCache.SetBatteryStatus(ADeviceAddress: UInt64;
  const AStatus: TBatteryStatus);
begin
  FCache.AddOrSetValue(ADeviceAddress, AStatus);
end;

function TMockBatteryCache.HasCachedStatus(ADeviceAddress: UInt64): Boolean;
begin
  Result := FCache.ContainsKey(ADeviceAddress);
end;

procedure TMockBatteryCache.RequestRefresh(ADeviceAddress: UInt64);
begin
  Inc(FRequestRefreshCallCount);
  FLastRefreshAddress := ADeviceAddress;
end;

procedure TMockBatteryCache.RequestRefreshAll(const ADeviceAddresses: TArray<UInt64>);
begin
  Inc(FRequestRefreshAllCallCount);
end;

procedure TMockBatteryCache.Clear;
begin
  Inc(FClearCallCount);
  FCache.Clear;
end;

procedure TMockBatteryCache.Remove(ADeviceAddress: UInt64);
begin
  FCache.Remove(ADeviceAddress);
end;

function TMockBatteryCache.GetOnQueryCompleted: TBatteryQueryCompletedEvent;
begin
  Result := FOnQueryCompleted;
end;

procedure TMockBatteryCache.SetOnQueryCompleted(AValue: TBatteryQueryCompletedEvent);
begin
  FOnQueryCompleted := AValue;
end;

procedure TMockBatteryCache.SimulateQueryCompleted(AAddress: UInt64;
  const AStatus: TBatteryStatus);
begin
  FCache.AddOrSetValue(AAddress, AStatus);
  if Assigned(FOnQueryCompleted) then
    FOnQueryCompleted(Self, AAddress, AStatus);
end;

{ TMockBluetoothService }

constructor TMockBluetoothService.Create;
begin
  inherited Create;
  // Default to successful results
  FConnectResult := TConnectionResult.Ok;
  FDisconnectResult := TConnectionResult.Ok;
  FAdapterAvailable := True;
  FAdapterName := 'Mock Adapter';
  // Initialize arrays
  SetLength(FDevices, 0);
  // Initialize counters
  FConnectCallCount := 0;
  FDisconnectCallCount := 0;
  FToggleConnectionCallCount := 0;
  FRefreshAllDevicesCallCount := 0;
  FGetDevicesCallCount := 0;
  FIsAdapterAvailableCallCount := 0;
end;

function TMockBluetoothService.IsAdapterAvailable: Boolean;
begin
  Inc(FIsAdapterAvailableCallCount);
  Result := FAdapterAvailable;
end;

function TMockBluetoothService.GetPairedDevices: TBluetoothDeviceInfoArray;
begin
  Inc(FGetDevicesCallCount);
  Result := FDevices;
end;

function TMockBluetoothService.RefreshAllDevices: TBluetoothDeviceInfoArray;
begin
  Inc(FRefreshAllDevicesCallCount);
  Result := FDevices;
end;

function TMockBluetoothService.Connect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Inc(FConnectCallCount);
  FLastConnectDevice := ADevice;
  Result := FConnectResult.Success;
end;

function TMockBluetoothService.Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Inc(FDisconnectCallCount);
  FLastDisconnectDevice := ADevice;
  Result := FDisconnectResult.Success;
end;

function TMockBluetoothService.ToggleConnection(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Inc(FToggleConnectionCallCount);
  FLastToggleDevice := ADevice;
  // Toggle based on current state - if connected, use disconnect result, otherwise use connect
  if ADevice.ConnectionState = csConnected then
    Result := FDisconnectResult.Success
  else
    Result := FConnectResult.Success;
end;

function TMockBluetoothService.GetOnDeviceStateChanged: TDeviceStateChangedEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TMockBluetoothService.SetOnDeviceStateChanged(AValue: TDeviceStateChangedEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TMockBluetoothService.GetOnDeviceListChanged: TDeviceListChangedEvent;
begin
  Result := FOnDeviceListChanged;
end;

procedure TMockBluetoothService.SetOnDeviceListChanged(AValue: TDeviceListChangedEvent);
begin
  FOnDeviceListChanged := AValue;
end;

function TMockBluetoothService.GetOnError: TBluetoothErrorEvent;
begin
  Result := FOnError;
end;

procedure TMockBluetoothService.SetOnError(AValue: TBluetoothErrorEvent);
begin
  FOnError := AValue;
end;

procedure TMockBluetoothService.SimulateDeviceStateChanged(const ADevice: TBluetoothDeviceInfo);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, ADevice);
end;

procedure TMockBluetoothService.SimulateDeviceListChanged;
begin
  if Assigned(FOnDeviceListChanged) then
    FOnDeviceListChanged(Self);
end;

procedure TMockBluetoothService.SimulateError(const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

end.
