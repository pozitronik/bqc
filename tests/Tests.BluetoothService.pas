{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TBluetoothService Unit Tests                    }
{                                                       }
{       Tests interface segregation (ISP) compliance    }
{       and core service functionality.                 }
{                                                       }
{       Copyright (c) 2024                              }
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
    // Keep object references for test setup
    FMockDeviceRepository: TMockDeviceRepository;
    FMockConnectionExecutor: TMockConnectionExecutor;
    FMockStrategyFactory: TMockConnectionStrategyFactory;
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
    FAdapterQuery
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
  FMockDeviceRepository := nil;
  FMockConnectionExecutor := nil;
  FMockStrategyFactory := nil;
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

initialization
  TDUnitX.RegisterTestFixture(TBluetoothServiceTests);

end.
