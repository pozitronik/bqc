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
  Bluetooth.ConnectionVerification,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.DeviceConfigTypes,
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
    FMockAdapterQuery: TMockAdapterQuery;
    FMockEventDebouncer: TMockEventDebouncer;
    FMockDeviceMonitor: TMockDeviceMonitor;
    // Tracking fields for event handler verification
    FStateChangedFired: Boolean;
    FLastStateChangedDevice: TBluetoothDeviceInfo;
    FDiscoveredFired: Boolean;
    FLastDiscoveredDevice: TBluetoothDeviceInfo;
    FOutOfRangeFired: Boolean;
    FLastOutOfRangeAddress: UInt64;
    FErrorFired: Boolean;
    FLastErrorMessage: string;
    FLastErrorCode: Cardinal;
    FListChangedFired: Boolean;
    // Event handlers that set tracking fields
    procedure TrackDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure TrackDeviceListChanged(Sender: TObject);
    procedure TrackDeviceDiscovered(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure TrackDeviceOutOfRange(Sender: TObject; const ADeviceAddress: UInt64);
    procedure TrackError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    // Helper: creates a service with verification strategy for specific tests
    function CreateServiceWithVerification(
      AVerification: IConnectionVerificationStrategy): IBluetoothService;
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

    { RemoveDevice Tests }
    [Test]
    procedure RemoveDevice_RemovesFromRepository;
    [Test]
    procedure RemoveDevice_NonExisting_NoError;

    { RefreshAllDevices Tests }
    [Test]
    procedure RefreshAllDevices_CallsRepositoryRefresh;

    { IsAdapterAvailable Tests }
    [Test]
    procedure IsAdapterAvailable_DelegatesToAdapterQuery;

    { Event Handler Property Tests }
    [Test]
    procedure OnDeviceStateChanged_SetAndGet_RoundTrips;
    [Test]
    procedure OnDeviceListChanged_SetAndGet_RoundTrips;
    [Test]
    procedure OnDeviceDiscovered_SetAndGet_RoundTrips;
    [Test]
    procedure OnDeviceOutOfRange_SetAndGet_RoundTrips;
    [Test]
    procedure OnError_SetAndGet_RoundTrips;
    [Test]
    procedure EventHandlers_InitiallyNil;

    { ConnectWithStrategy edge cases }
    [Test]
    procedure Connect_NilStrategy_ReturnsFalse;
    [Test]
    procedure Connect_EmptyServiceGuids_ReturnsFalse;

    { ConnectWithStrategy verification paths }
    [Test]
    procedure Connect_VerificationSucceeds_ReturnsTrue;
    [Test]
    procedure Connect_VerificationFails_ReturnsFalse;
    [Test]
    procedure Connect_NoVerification_TrustsWindowsState;
    [Test]
    procedure Connect_ExecutorFails_SetsErrorState;
    [Test]
    procedure Disconnect_SkipsVerification;
    [Test]
    procedure Connect_UsesDeviceSpecificRetryCount;

    { Monitor event handler forwarding }
    [Test]
    procedure Monitor_DeviceStateChanged_UpdatesRepository;
    [Test]
    procedure Monitor_DeviceStateChanged_FilteredByDebouncer;
    [Test]
    procedure Monitor_DeviceStateChanged_RefreshesWhenNotInRepo;
    [Test]
    procedure Monitor_DeviceStateChanged_VerifiesConnection;
    [Test]
    procedure Monitor_DeviceStateChanged_RejectsUnverifiedConnection;
    [Test]
    procedure Monitor_DeviceDiscovered_ForwardsToHandler;
    [Test]
    procedure Monitor_DeviceOutOfRange_ForwardsToHandler;
    [Test]
    procedure Monitor_Error_ForwardsToHandler;

    { TriggerDiscoveryScan }
    [Test]
    procedure TriggerDiscoveryScan_StopsAndRestartsMonitor;
    [Test]
    procedure TriggerDiscoveryScan_MonitorNotRunning_JustStarts;
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
  FMockAdapterQuery := nil;
  FMockEventDebouncer := nil;
  FMockDeviceMonitor := nil;
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

{ Tracking event handlers - set fields for test assertions }

procedure TBluetoothServiceTests.TrackDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  FStateChangedFired := True;
  FLastStateChangedDevice := ADevice;
end;

procedure TBluetoothServiceTests.TrackDeviceListChanged(Sender: TObject);
begin
  FListChangedFired := True;
end;

procedure TBluetoothServiceTests.TrackDeviceDiscovered(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  FDiscoveredFired := True;
  FLastDiscoveredDevice := ADevice;
end;

procedure TBluetoothServiceTests.TrackDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
begin
  FOutOfRangeFired := True;
  FLastOutOfRangeAddress := ADeviceAddress;
end;

procedure TBluetoothServiceTests.TrackError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  FErrorFired := True;
  FLastErrorMessage := AMessage;
  FLastErrorCode := AErrorCode;
end;

{ RemoveDevice Tests }

procedure TBluetoothServiceTests.RemoveDevice_RemovesFromRepository;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FMockDeviceRepository.AddOrUpdate(Device);
  Assert.IsTrue(FMockDeviceRepository.Contains($112233445566),
    'Precondition: device should be in repository');

  // Act
  FService.RemoveDevice($112233445566);

  // Assert
  Assert.IsFalse(FMockDeviceRepository.Contains($112233445566),
    'Device should be removed from repository');
end;

procedure TBluetoothServiceTests.RemoveDevice_NonExisting_NoError;
begin
  // Act & Assert - should not raise exception
  FService.RemoveDevice($FFFFFFFFFFFF);
  Assert.Pass('RemoveDevice for non-existing address should not raise');
end;

{ RefreshAllDevices Tests }

procedure TBluetoothServiceTests.RefreshAllDevices_CallsRepositoryRefresh;
var
  InitialRefreshCount: Integer;
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange - constructor already calls Refresh once
  InitialRefreshCount := FMockDeviceRepository.RefreshCallCount;

  // Act
  Devices := FService.RefreshAllDevices;

  // Assert
  Assert.AreEqual(InitialRefreshCount + 1, FMockDeviceRepository.RefreshCallCount,
    'RefreshAllDevices should call repository Refresh');
end;

{ IsAdapterAvailable Tests }

procedure TBluetoothServiceTests.IsAdapterAvailable_DelegatesToAdapterQuery;
begin
  // Arrange & Act & Assert - True case
  FMockAdapterQuery.AdapterAvailable := True;
  Assert.IsTrue(FService.IsAdapterAvailable,
    'Should return True when adapter is available');

  // Arrange & Act & Assert - False case
  FMockAdapterQuery.AdapterAvailable := False;
  Assert.IsFalse(FService.IsAdapterAvailable,
    'Should return False when adapter is unavailable');
end;

{ Event Handler Property Tests }

procedure TBluetoothServiceTests.OnDeviceStateChanged_SetAndGet_RoundTrips;
begin
  // Act
  FService.OnDeviceStateChanged := TrackDeviceStateChanged;

  // Assert
  Assert.IsTrue(Assigned(FService.OnDeviceStateChanged),
    'OnDeviceStateChanged should be assigned after setting');
end;

procedure TBluetoothServiceTests.OnDeviceListChanged_SetAndGet_RoundTrips;
begin
  // Act - OnDeviceListChanged delegates to repository.OnListChanged
  FService.OnDeviceListChanged := TrackDeviceListChanged;

  // Assert
  Assert.IsTrue(Assigned(FService.OnDeviceListChanged),
    'OnDeviceListChanged should be assigned after setting');
end;

procedure TBluetoothServiceTests.OnDeviceDiscovered_SetAndGet_RoundTrips;
begin
  // Act
  FService.OnDeviceDiscovered := TrackDeviceDiscovered;

  // Assert
  Assert.IsTrue(Assigned(FService.OnDeviceDiscovered),
    'OnDeviceDiscovered should be assigned after setting');
end;

procedure TBluetoothServiceTests.OnDeviceOutOfRange_SetAndGet_RoundTrips;
begin
  // Act
  FService.OnDeviceOutOfRange := TrackDeviceOutOfRange;

  // Assert
  Assert.IsTrue(Assigned(FService.OnDeviceOutOfRange),
    'OnDeviceOutOfRange should be assigned after setting');
end;

procedure TBluetoothServiceTests.OnError_SetAndGet_RoundTrips;
begin
  // Act
  FService.OnError := TrackError;

  // Assert
  Assert.IsTrue(Assigned(FService.OnError),
    'OnError should be assigned after setting');
end;

procedure TBluetoothServiceTests.EventHandlers_InitiallyNil;
begin
  // Assert - service event handlers should be nil after construction
  // (constructor only sets monitor handlers, not service-level handlers)
  Assert.IsFalse(Assigned(FService.OnDeviceStateChanged),
    'OnDeviceStateChanged should be nil initially');
  Assert.IsFalse(Assigned(FService.OnDeviceListChanged),
    'OnDeviceListChanged should be nil initially');
  Assert.IsFalse(Assigned(FService.OnDeviceDiscovered),
    'OnDeviceDiscovered should be nil initially');
  Assert.IsFalse(Assigned(FService.OnDeviceOutOfRange),
    'OnDeviceOutOfRange should be nil initially');
  Assert.IsFalse(Assigned(FService.OnError),
    'OnError should be nil initially');
end;

{ ConnectWithStrategy edge cases }

procedure TBluetoothServiceTests.Connect_NilStrategy_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange - clear all strategies so factory returns nil
  FMockStrategyFactory.Clear;
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);

  // Act & Assert
  Assert.IsFalse(FService.Connect(Device),
    'Connect should return False when no strategy found');
  Assert.AreEqual(0, FMockConnectionExecutor.ExecuteCallCount,
    'Executor should not be called when strategy is nil');
end;

procedure TBluetoothServiceTests.Connect_EmptyServiceGuids_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
  Strategy: TMockConnectionStrategy;
begin
  // Arrange - strategy with empty GUIDs
  FMockStrategyFactory.Clear;
  Strategy := TMockConnectionStrategy.Create;
  Strategy.CanHandleResult := True;
  Strategy.ServiceGuids := [];
  FMockStrategyFactory.RegisterStrategy(Strategy);

  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);

  // Act & Assert
  Assert.IsFalse(FService.Connect(Device),
    'Connect should return False with empty service GUIDs');
  Assert.AreEqual(0, FMockConnectionExecutor.ExecuteCallCount,
    'Executor should not be called with empty GUIDs');
end;

{ Helper to create service with verification strategy }

function TBluetoothServiceTests.CreateServiceWithVerification(
  AVerification: IConnectionVerificationStrategy): IBluetoothService;
begin
  // Reuses the same mock dependencies already set up in Setup
  Result := TBluetoothService.Create(
    FConnectionConfig,
    FDeviceConfigProvider,
    FStrategyFactory,
    FDeviceMonitor,
    FDeviceRepository,
    FConnectionExecutor,
    FAdapterQuery,
    FEventDebouncer,
    AVerification
  );
end;

{ ConnectWithStrategy verification paths }

procedure TBluetoothServiceTests.Connect_VerificationSucceeds_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
  MockVerify: TMockConnectionVerificationStrategy;
  Svc: IBluetoothService;
begin
  // Arrange
  MockVerify := TMockConnectionVerificationStrategy.Create;
  MockVerify.VerifyResult := True;
  Svc := CreateServiceWithVerification(MockVerify);
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act
  Assert.IsTrue(Svc.Connect(Device),
    'Connect should return True when verification succeeds');

  // Assert
  Assert.AreEqual(1, MockVerify.VerifyCallCount,
    'Verification should be called once');
  Assert.AreEqual(UInt64($112233445566), MockVerify.LastVerifyAddress,
    'Verification should receive correct device address');
end;

procedure TBluetoothServiceTests.Connect_VerificationFails_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
  MockVerify: TMockConnectionVerificationStrategy;
  Svc: IBluetoothService;
begin
  // Arrange - executor succeeds but verification fails
  MockVerify := TMockConnectionVerificationStrategy.Create;
  MockVerify.VerifyResult := False;
  Svc := CreateServiceWithVerification(MockVerify);
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act
  Assert.IsFalse(Svc.Connect(Device),
    'Connect should return False when verification fails');

  // Assert - device should be updated to disconnected in repository
  Assert.IsTrue(FMockDeviceRepository.Contains($112233445566),
    'Device should be in repository after failed verification');
  Assert.AreEqual(csDisconnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should be Disconnected after failed verification');
end;

procedure TBluetoothServiceTests.Connect_NoVerification_TrustsWindowsState;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange - FService has nil verification strategy (default Setup)
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act
  Assert.IsTrue(FService.Connect(Device),
    'Connect should return True when no verification strategy (trusts Windows)');

  // Assert - device should be updated to connected in repository
  Assert.AreEqual(csConnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should be Connected when no verification (trusts Windows)');
end;

procedure TBluetoothServiceTests.Connect_ExecutorFails_SetsErrorState;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Fail(42);
  FErrorFired := False;
  FService.OnError := TrackError;

  // Act
  Assert.IsFalse(FService.Connect(Device),
    'Connect should return False when executor fails');

  // Assert - device should be in error state
  Assert.AreEqual(csError,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should be in Error state after executor failure');
  Assert.IsTrue(FErrorFired, 'OnError should fire on executor failure');
end;

procedure TBluetoothServiceTests.Disconnect_SkipsVerification;
var
  Device: TBluetoothDeviceInfo;
  MockVerify: TMockConnectionVerificationStrategy;
  Svc: IBluetoothService;
begin
  // Arrange
  MockVerify := TMockConnectionVerificationStrategy.Create;
  MockVerify.VerifyResult := True;
  Svc := CreateServiceWithVerification(MockVerify);
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act
  Assert.IsTrue(Svc.Disconnect(Device),
    'Disconnect should return True on success');

  // Assert - verification should NOT be called for disconnect
  Assert.AreEqual(0, MockVerify.VerifyCallCount,
    'Verification should not be called during disconnect');
  Assert.AreEqual(csDisconnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should be Disconnected after successful disconnect');
end;

procedure TBluetoothServiceTests.Connect_UsesDeviceSpecificRetryCount;
var
  Device: TBluetoothDeviceInfo;
  DevConfig: TDeviceConfig;
begin
  // Arrange - set device-specific retry count
  DevConfig := TDeviceConfig.Default($112233445566);
  DevConfig.ConnectionRetryCount := 5;
  (FDeviceConfigProvider as TMockDeviceConfigProvider).AddDeviceConfig($112233445566, DevConfig);

  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  // Act
  FService.Connect(Device);

  // Assert - executor should receive device-specific retry count (5), not global (2)
  Assert.AreEqual(5, FMockConnectionExecutor.LastRetryCount,
    'Should use device-specific retry count (5) instead of global (2)');
end;

{ Monitor event handler forwarding }

procedure TBluetoothServiceTests.Monitor_DeviceStateChanged_UpdatesRepository;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange - put device in repository
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device);
  FStateChangedFired := False;
  FService.OnDeviceStateChanged := TrackDeviceStateChanged;

  // Act - simulate monitor event
  FMockDeviceMonitor.SimulateDeviceStateChanged($112233445566, csConnected);

  // Assert
  Assert.AreEqual(csConnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Repository should be updated with new state');
  Assert.IsTrue(FStateChangedFired, 'OnDeviceStateChanged handler should fire');
end;

procedure TBluetoothServiceTests.Monitor_DeviceStateChanged_FilteredByDebouncer;
var
  Device: TBluetoothDeviceInfo;
begin
  // Arrange - debouncer filters the event
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device);
  FMockEventDebouncer.ShouldProcessResult := False;
  FStateChangedFired := False;
  FService.OnDeviceStateChanged := TrackDeviceStateChanged;

  // Act
  FMockDeviceMonitor.SimulateDeviceStateChanged($112233445566, csConnected);

  // Assert - event should be filtered
  Assert.IsFalse(FStateChangedFired,
    'Handler should NOT fire when debouncer filters event');
  Assert.AreEqual(csDisconnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Repository should NOT be updated when debouncer filters event');
end;

procedure TBluetoothServiceTests.Monitor_DeviceStateChanged_RefreshesWhenNotInRepo;
var
  InitialRefreshCount: Integer;
begin
  // Arrange - device NOT in repository
  InitialRefreshCount := FMockDeviceRepository.RefreshCallCount;

  // Act
  FMockDeviceMonitor.SimulateDeviceStateChanged($AABBCCDDEEFF, csConnected);

  // Assert - should have triggered a repository refresh
  Assert.AreEqual(InitialRefreshCount + 1, FMockDeviceRepository.RefreshCallCount,
    'Should refresh repository when device not found');
end;

procedure TBluetoothServiceTests.Monitor_DeviceStateChanged_VerifiesConnection;
var
  Device: TBluetoothDeviceInfo;
  MockVerify: TMockConnectionVerificationStrategy;
  Svc: IBluetoothService;
begin
  // Arrange - service with verification strategy
  MockVerify := TMockConnectionVerificationStrategy.Create;
  MockVerify.VerifyResult := True;
  Svc := CreateServiceWithVerification(MockVerify);
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device);

  // Act - simulate monitor reporting connected
  FMockDeviceMonitor.SimulateDeviceStateChanged($112233445566, csConnected);

  // Assert - verification should be called
  Assert.AreEqual(1, MockVerify.VerifyCallCount,
    'Verification should be called for connected state changes');
  Assert.AreEqual(csConnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should be Connected after verification succeeds');
end;

procedure TBluetoothServiceTests.Monitor_DeviceStateChanged_RejectsUnverifiedConnection;
var
  Device: TBluetoothDeviceInfo;
  MockVerify: TMockConnectionVerificationStrategy;
  Svc: IBluetoothService;
begin
  // Arrange - verification fails
  MockVerify := TMockConnectionVerificationStrategy.Create;
  MockVerify.VerifyResult := False;
  Svc := CreateServiceWithVerification(MockVerify);
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FMockDeviceRepository.AddOrUpdate(Device);

  // Act - simulate monitor reporting connected (but device is actually off)
  FMockDeviceMonitor.SimulateDeviceStateChanged($112233445566, csConnected);

  // Assert - state should be overridden to disconnected
  Assert.AreEqual(1, MockVerify.VerifyCallCount,
    'Verification should be called');
  Assert.AreEqual(csDisconnected,
    FMockDeviceRepository.GetByAddress($112233445566).ConnectionState,
    'Device should remain Disconnected when verification fails');
end;

procedure TBluetoothServiceTests.Monitor_DeviceDiscovered_ForwardsToHandler;
var
  DiscoveredDevice: TBluetoothDeviceInfo;
begin
  // Arrange
  DiscoveredDevice := CreateTestDevice($AABBCCDDEEFF, 'NewDevice', btAudioOutput, csDisconnected);
  FDiscoveredFired := False;
  FService.OnDeviceDiscovered := TrackDeviceDiscovered;

  // Act
  FMockDeviceMonitor.SimulateDeviceDiscovered(DiscoveredDevice);

  // Assert
  Assert.IsTrue(FDiscoveredFired, 'OnDeviceDiscovered handler should fire');
  Assert.AreEqual('NewDevice', FLastDiscoveredDevice.Name,
    'Handler should receive the discovered device');
end;

procedure TBluetoothServiceTests.Monitor_DeviceOutOfRange_ForwardsToHandler;
begin
  // Arrange
  FOutOfRangeFired := False;
  FLastOutOfRangeAddress := 0;
  FService.OnDeviceOutOfRange := TrackDeviceOutOfRange;

  // Act
  FMockDeviceMonitor.SimulateDeviceOutOfRange($112233445566);

  // Assert
  Assert.IsTrue(FOutOfRangeFired, 'OnDeviceOutOfRange handler should fire');
  Assert.AreEqual(UInt64($112233445566), FLastOutOfRangeAddress,
    'Handler should receive correct device address');
end;

procedure TBluetoothServiceTests.Monitor_Error_ForwardsToHandler;
begin
  // Arrange
  FErrorFired := False;
  FLastErrorMessage := '';
  FLastErrorCode := 0;
  FService.OnError := TrackError;

  // Act
  FMockDeviceMonitor.SimulateError('Test error', 42);

  // Assert
  Assert.IsTrue(FErrorFired, 'OnError handler should fire');
  Assert.AreEqual('Test error', FLastErrorMessage,
    'Handler should receive correct error message');
  Assert.AreEqual(Cardinal(42), FLastErrorCode,
    'Handler should receive correct error code');
end;

{ TriggerDiscoveryScan }

procedure TBluetoothServiceTests.TriggerDiscoveryScan_StopsAndRestartsMonitor;
var
  InitialStopCount, InitialStartCount: Integer;
begin
  // Arrange - monitor is running after Setup
  Assert.IsTrue(FMockDeviceMonitor.Running,
    'Precondition: monitor should be running');
  InitialStopCount := FMockDeviceMonitor.StopCallCount;
  InitialStartCount := FMockDeviceMonitor.StartCallCount;

  // Act
  FService.TriggerDiscoveryScan;

  // Assert
  Assert.AreEqual(InitialStopCount + 1, FMockDeviceMonitor.StopCallCount,
    'Should stop monitor before restarting');
  Assert.AreEqual(InitialStartCount + 1, FMockDeviceMonitor.StartCallCount,
    'Should restart monitor');
end;

procedure TBluetoothServiceTests.TriggerDiscoveryScan_MonitorNotRunning_JustStarts;
var
  InitialStopCount, InitialStartCount: Integer;
begin
  // Arrange - stop monitor first
  FMockDeviceMonitor.Stop;
  Assert.IsFalse(FMockDeviceMonitor.Running,
    'Precondition: monitor should not be running');
  InitialStopCount := FMockDeviceMonitor.StopCallCount;
  InitialStartCount := FMockDeviceMonitor.StartCallCount;

  // Act
  FService.TriggerDiscoveryScan;

  // Assert - should NOT call Stop since monitor isn't running
  Assert.AreEqual(InitialStopCount, FMockDeviceMonitor.StopCallCount,
    'Should not call Stop when monitor is not running');
  Assert.AreEqual(InitialStartCount + 1, FMockDeviceMonitor.StartCallCount,
    'Should call Start');
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
