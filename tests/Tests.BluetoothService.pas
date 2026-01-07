{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TBluetoothService Unit Tests                    }
{                                                       }
{       Tests interface segregation (ISP) compliance    }
{       and core service functionality.                 }
{                                                       }
{*******************************************************}

unit Tests.BluetoothService;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.Service,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  Tests.Mocks;

type
  [TestFixture]
  TBluetoothServiceTests = class
  private
    FService: IBluetoothService;
    FConnectionConfig: IConnectionConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FStrategyFactory: IConnectionStrategyFactory;
    FDeviceMonitor: IDeviceMonitor;
    FDeviceRepository: IDeviceRepository;
    FConnectionExecutor: IConnectionExecutor;
    FAdapterQuery: IBluetoothAdapterQuery;
    FEventDebouncer: IEventDebouncer;
    // Keep object references for test setup
    FMockDeviceRepository: TMockDeviceRepository;
    FMockConnectionExecutor: TMockConnectionExecutor;
    FMockStrategyFactory: TMockConnectionStrategyFactory;
    FMockEventDebouncer: TMockEventDebouncer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Interface Segregation Tests }
    [Test]
    procedure SupportsIBluetoothService;
    [Test]
    procedure SupportsIBluetoothDeviceEnumerator;
    [Test]
    procedure SupportsIBluetoothConnectionManager;

    { IBluetoothDeviceEnumerator Tests }
    [Test]
    procedure GetPairedDevices_ReturnsDevicesFromRepository;
    [Test]
    procedure GetPairedDevices_EmptyRepository_ReturnsEmptyArray;
    [Test]
    procedure RefreshDeviceStatus_DeviceInRepository_ReturnsUpdatedDevice;
    [Test]
    procedure RefreshDeviceStatus_DeviceNotInRepository_ReturnsOriginalDevice;

    { IBluetoothConnectionManager Tests }
    [Test]
    procedure Connect_CallsConnectionExecutor;
    [Test]
    procedure Disconnect_CallsConnectionExecutor;
    [Test]
    procedure ToggleConnection_ConnectedDevice_Disconnects;
    [Test]
    procedure ToggleConnection_DisconnectedDevice_Connects;
  end;

  /// <summary>
  /// Tests for CreateBluetoothService factory function.
  /// Verifies that optional dependencies can be injected for testability.
  /// </summary>
  [TestFixture]
  TCreateBluetoothServiceFactoryTests = class
  private
    FPollingConfig: IPollingConfig;
    FConnectionConfig: IConnectionConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FStrategyFactory: IConnectionStrategyFactory;
    FMockStrategyFactory: TMockConnectionStrategyFactory;
    // Interface references for proper cleanup
    FDeviceMonitor: IDeviceMonitor;
    FDeviceRepository: IDeviceRepository;
    FConnectionExecutor: IConnectionExecutor;
    FAdapterQuery: IBluetoothAdapterQuery;
    FEventDebouncer: IEventDebouncer;
    // Object references for test setup
    FMockDeviceMonitor: TMockDeviceMonitor;
    FMockDeviceRepository: TMockDeviceRepository;
    FMockConnectionExecutor: TMockConnectionExecutor;
    FMockAdapterQuery: TMockAdapterQuery;
    FMockEventDebouncer: TMockEventDebouncer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Factory with default dependencies }
    [Test]
    procedure Factory_WithNilDependencies_CreatesService;

    { Factory with injected dependencies }
    [Test]
    procedure Factory_WithInjectedRepository_UsesProvidedRepository;
    [Test]
    procedure Factory_WithInjectedExecutor_UsesProvidedExecutor;
    [Test]
    procedure Factory_WithInjectedAdapterQuery_UsesProvidedQuery;
    [Test]
    procedure Factory_WithAllMocks_UsesAllProvidedDependencies;
  end;

implementation

{ TBluetoothServiceTests }

procedure TBluetoothServiceTests.Setup;
var
  Strategy: TMockConnectionStrategy;
begin
  FConnectionConfig := TMockConnectionConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FMockStrategyFactory := TMockConnectionStrategyFactory.Create;
  FStrategyFactory := FMockStrategyFactory;
  FDeviceMonitor := TMockDeviceMonitor.Create;
  FMockDeviceRepository := TMockDeviceRepository.Create;
  FDeviceRepository := FMockDeviceRepository;
  FMockConnectionExecutor := TMockConnectionExecutor.Create;
  FConnectionExecutor := FMockConnectionExecutor;
  FAdapterQuery := TMockAdapterQuery.Create;
  FMockEventDebouncer := TMockEventDebouncer.Create;
  FEventDebouncer := FMockEventDebouncer;

  // Register a default strategy with service GUIDs
  Strategy := TMockConnectionStrategy.Create;
  Strategy.ServiceGuids := [StringToGUID('{00001108-0000-1000-8000-00805F9B34FB}')]; // Headset AG
  Strategy.CanHandleResult := True;
  FMockStrategyFactory.RegisterStrategy(Strategy);

  FService := TBluetoothService.Create(
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FDeviceMonitor,
    FDeviceRepository,
    FConnectionExecutor,
    FAdapterQuery,
    FEventDebouncer,
    nil  // AVerificationStrategy - not needed for these tests
  );
end;

procedure TBluetoothServiceTests.TearDown;
begin
  // All interfaces are reference-counted, will be freed automatically
  FService := nil;
  FConnectionConfig := nil;
  FDeviceConfigProvider := nil;
  FStrategyFactory := nil;
  FDeviceMonitor := nil;
  FDeviceRepository := nil;
  FConnectionExecutor := nil;
  FAdapterQuery := nil;
  FEventDebouncer := nil;
  FMockDeviceRepository := nil;
  FMockConnectionExecutor := nil;
  FMockStrategyFactory := nil;
  FMockEventDebouncer := nil;
end;

{ Interface Segregation Tests }

procedure TBluetoothServiceTests.SupportsIBluetoothService;
var
  Intf: IBluetoothService;
begin
  Assert.IsTrue(Supports(FService, IBluetoothService, Intf),
    'TBluetoothService should support IBluetoothService');
end;

procedure TBluetoothServiceTests.SupportsIBluetoothDeviceEnumerator;
var
  Intf: IBluetoothDeviceEnumerator;
begin
  Assert.IsTrue(Supports(FService, IBluetoothDeviceEnumerator, Intf),
    'TBluetoothService should support IBluetoothDeviceEnumerator');
end;

procedure TBluetoothServiceTests.SupportsIBluetoothConnectionManager;
var
  Intf: IBluetoothConnectionManager;
begin
  Assert.IsTrue(Supports(FService, IBluetoothConnectionManager, Intf),
    'TBluetoothService should support IBluetoothConnectionManager');
end;

{ IBluetoothDeviceEnumerator Tests }

procedure TBluetoothServiceTests.GetPairedDevices_ReturnsDevicesFromRepository;
var
  Device1, Device2: TBluetoothDeviceInfo;
  Enumerator: IBluetoothDeviceEnumerator;
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange
  Device1 := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  Device2 := CreateTestDevice($AABBCCDDEEFF, 'Device2', btKeyboard, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device1);
  FMockDeviceRepository.AddOrUpdate(Device2);

  Supports(FService, IBluetoothDeviceEnumerator, Enumerator);

  // Act
  Devices := Enumerator.GetPairedDevices;

  // Assert
  Assert.IsTrue(Length(Devices) = 2, 'Should return 2 devices');
end;

procedure TBluetoothServiceTests.GetPairedDevices_EmptyRepository_ReturnsEmptyArray;
var
  Enumerator: IBluetoothDeviceEnumerator;
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange
  Supports(FService, IBluetoothDeviceEnumerator, Enumerator);

  // Act
  Devices := Enumerator.GetPairedDevices;

  // Assert
  Assert.IsTrue(Length(Devices) = 0, 'Should return empty array');
end;

procedure TBluetoothServiceTests.RefreshDeviceStatus_DeviceInRepository_ReturnsUpdatedDevice;
var
  OriginalDevice, UpdatedDevice, ResultDevice: TBluetoothDeviceInfo;
  Enumerator: IBluetoothDeviceEnumerator;
begin
  // Arrange
  OriginalDevice := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  UpdatedDevice := OriginalDevice.WithConnectionState(csConnected);
  FMockDeviceRepository.AddOrUpdate(UpdatedDevice);

  Supports(FService, IBluetoothDeviceEnumerator, Enumerator);

  // Act
  ResultDevice := Enumerator.RefreshDeviceStatus(OriginalDevice);

  // Assert
  Assert.AreEqual(csConnected, ResultDevice.ConnectionState,
    'Should return device with updated connection state from repository');
end;

procedure TBluetoothServiceTests.RefreshDeviceStatus_DeviceNotInRepository_ReturnsOriginalDevice;
var
  OriginalDevice, ResultDevice: TBluetoothDeviceInfo;
  Enumerator: IBluetoothDeviceEnumerator;
begin
  // Arrange
  OriginalDevice := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  // Don't add to repository

  Supports(FService, IBluetoothDeviceEnumerator, Enumerator);

  // Act
  ResultDevice := Enumerator.RefreshDeviceStatus(OriginalDevice);

  // Assert
  Assert.AreEqual(OriginalDevice.AddressInt, ResultDevice.AddressInt,
    'Should return original device when not in repository');
  Assert.AreEqual(OriginalDevice.ConnectionState, ResultDevice.ConnectionState,
    'Should preserve original connection state');
end;

{ IBluetoothConnectionManager Tests }

procedure TBluetoothServiceTests.Connect_CallsConnectionExecutor;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.Connect(Device);

  // Assert
  Assert.AreEqual(1, FMockConnectionExecutor.ExecuteCallCount, 'Should call executor once');
  Assert.IsTrue(FMockConnectionExecutor.LastEnable, 'Should call with Enable=True');
end;

procedure TBluetoothServiceTests.Disconnect_CallsConnectionExecutor;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.Disconnect(Device);

  // Assert
  Assert.AreEqual(1, FMockConnectionExecutor.ExecuteCallCount, 'Should call executor once');
  Assert.IsFalse(FMockConnectionExecutor.LastEnable, 'Should call with Enable=False');
end;

procedure TBluetoothServiceTests.ToggleConnection_ConnectedDevice_Disconnects;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.ToggleConnection(Device);

  // Assert
  Assert.IsFalse(FMockConnectionExecutor.LastEnable,
    'Toggle on connected device should disconnect (Enable=False)');
end;

procedure TBluetoothServiceTests.ToggleConnection_DisconnectedDevice_Connects;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.ToggleConnection(Device);

  // Assert
  Assert.IsTrue(FMockConnectionExecutor.LastEnable,
    'Toggle on disconnected device should connect (Enable=True)');
end;

{ TCreateBluetoothServiceFactoryTests }

procedure TCreateBluetoothServiceFactoryTests.Setup;
var
  Strategy: TMockConnectionStrategy;
begin
  FPollingConfig := TMockPollingConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FMockStrategyFactory := TMockConnectionStrategyFactory.Create;
  FStrategyFactory := FMockStrategyFactory;

  // Register a default strategy
  Strategy := TMockConnectionStrategy.Create;
  Strategy.ServiceGuids := [StringToGUID('{00001108-0000-1000-8000-00805F9B34FB}')];
  Strategy.CanHandleResult := True;
  FMockStrategyFactory.RegisterStrategy(Strategy);

  // Create mocks for optional dependencies
  // Store both object and interface references for proper refcount management
  FMockDeviceMonitor := TMockDeviceMonitor.Create;
  FDeviceMonitor := FMockDeviceMonitor;
  FMockDeviceRepository := TMockDeviceRepository.Create;
  FDeviceRepository := FMockDeviceRepository;
  FMockConnectionExecutor := TMockConnectionExecutor.Create;
  FConnectionExecutor := FMockConnectionExecutor;
  FMockAdapterQuery := TMockAdapterQuery.Create;
  FAdapterQuery := FMockAdapterQuery;
  FMockEventDebouncer := TMockEventDebouncer.Create;
  FEventDebouncer := FMockEventDebouncer;
end;

procedure TCreateBluetoothServiceFactoryTests.TearDown;
begin
  // Clear interface references first to release objects
  FDeviceMonitor := nil;
  FDeviceRepository := nil;
  FConnectionExecutor := nil;
  FAdapterQuery := nil;
  FEventDebouncer := nil;
  // Clear object references (already freed via interface)
  FMockDeviceMonitor := nil;
  FMockDeviceRepository := nil;
  FMockConnectionExecutor := nil;
  FMockAdapterQuery := nil;
  FMockEventDebouncer := nil;
  // Clear other interface references
  FPollingConfig := nil;
  FConnectionConfig := nil;
  FDeviceConfigProvider := nil;
  FStrategyFactory := nil;
  FMockStrategyFactory := nil;
end;

procedure TCreateBluetoothServiceFactoryTests.Factory_WithNilDependencies_CreatesService;
var
  Service: IBluetoothService;
begin
  // Act - call factory with all nil optional dependencies (uses defaults)
  Service := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    nil,  // DeviceMonitor
    nil,  // DeviceRepository
    nil,  // ConnectionExecutor
    nil,  // AdapterQuery
    nil   // EventDebouncer
  );

  // Assert
  Assert.IsNotNull(Service, 'Factory should create service with default dependencies');
end;

procedure TCreateBluetoothServiceFactoryTests.Factory_WithInjectedRepository_UsesProvidedRepository;
var
  Service: IBluetoothService;
  Device: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange - add device to mock repository
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csConnected);
  FMockDeviceRepository.AddOrUpdate(Device);

  // Act - create service with injected repository
  Service := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FMockDeviceMonitor,
    FMockDeviceRepository,
    nil,
    FMockAdapterQuery,
    FMockEventDebouncer
  );

  // Assert - service should use our mock repository
  Devices := Service.GetPairedDevices;
  Assert.AreEqual(Integer(1), Integer(Length(Devices)),
    'Service should use injected repository containing our test device');
  Assert.AreEqual('TestDevice', Devices[0].Name);
end;

procedure TCreateBluetoothServiceFactoryTests.Factory_WithInjectedExecutor_UsesProvidedExecutor;
var
  Service: IBluetoothService;
  ConnManager: IBluetoothConnectionManager;
  Device: TBluetoothDeviceInfo;
begin
  // Arrange
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Create service with injected executor
  Service := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FMockDeviceMonitor,
    FMockDeviceRepository,
    FMockConnectionExecutor,
    FMockAdapterQuery,
    FMockEventDebouncer
  );

  Supports(Service, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.Connect(Device);

  // Assert - our mock executor should have been called
  Assert.AreEqual(1, FMockConnectionExecutor.ExecuteCallCount,
    'Service should use injected connection executor');
end;

procedure TCreateBluetoothServiceFactoryTests.Factory_WithInjectedAdapterQuery_UsesProvidedQuery;
var
  Service: IBluetoothService;
begin
  // Arrange - set adapter as unavailable
  FMockAdapterQuery.AdapterAvailable := False;

  // Act - create service with injected adapter query
  Service := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FMockDeviceMonitor,
    FMockDeviceRepository,
    FMockConnectionExecutor,
    FMockAdapterQuery,
    FMockEventDebouncer
  );

  // Assert - service should use our mock adapter query
  Assert.IsFalse(Service.IsAdapterAvailable,
    'Service should use injected adapter query returning unavailable');

  // Change mock state and verify
  FMockAdapterQuery.AdapterAvailable := True;
  Assert.IsTrue(Service.IsAdapterAvailable,
    'Service should reflect updated mock adapter state');
end;

procedure TCreateBluetoothServiceFactoryTests.Factory_WithAllMocks_UsesAllProvidedDependencies;
var
  Service: IBluetoothService;
  ConnManager: IBluetoothConnectionManager;
  Device: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange - setup all mocks
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device);
  FMockAdapterQuery.AdapterAvailable := True;
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act - create service with all injected dependencies
  Service := CreateBluetoothService(
    FPollingConfig,
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FMockDeviceMonitor,
    FMockDeviceRepository,
    FMockConnectionExecutor,
    FMockAdapterQuery,
    FMockEventDebouncer
  );

  // Assert - verify all mocks are used
  Assert.IsTrue(Service.IsAdapterAvailable, 'Should use mock adapter query');

  Devices := Service.GetPairedDevices;
  Assert.AreEqual(Integer(1), Integer(Length(Devices)), 'Should use mock repository');

  Supports(Service, IBluetoothConnectionManager, ConnManager);
  ConnManager.Connect(Device);
  Assert.AreEqual(1, FMockConnectionExecutor.ExecuteCallCount, 'Should use mock executor');
end;

initialization
  TDUnitX.RegisterTestFixture(TBluetoothServiceTests);
  TDUnitX.RegisterTestFixture(TCreateBluetoothServiceFactoryTests);

end.
