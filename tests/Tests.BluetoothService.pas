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
  Tests.Mocks;

type
  [TestFixture]
  TBluetoothServiceTests = class
  private
    FService: TBluetoothService;
    FConnectionConfig: TMockConnectionConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FStrategyFactory: TMockConnectionStrategyFactory;
    FDeviceMonitor: TMockDeviceMonitor;
    FDeviceRepository: TMockDeviceRepository;
    FConnectionExecutor: TMockConnectionExecutor;
    FAdapterQuery: TMockAdapterQuery;
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
begin
  FConnectionConfig := TMockConnectionConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FStrategyFactory := TMockConnectionStrategyFactory.Create;
  FDeviceMonitor := TMockDeviceMonitor.Create;
  FDeviceRepository := TMockDeviceRepository.Create;
  FConnectionExecutor := TMockConnectionExecutor.Create;
  FAdapterQuery := TMockAdapterQuery.Create;

  // Register a default strategy
  FStrategyFactory.RegisterStrategy(TMockConnectionStrategy.Create);

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
  FService.Free;
  // Mocks are reference-counted, no need to free
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
  FDeviceRepository.AddOrUpdate(Device1);
  FDeviceRepository.AddOrUpdate(Device2);

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
  FDeviceRepository.AddOrUpdate(UpdatedDevice);

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
  FConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.Connect(Device);

  // Assert
  Assert.AreEqual(1, FConnectionExecutor.ExecuteCallCount, 'Should call executor once');
  Assert.IsTrue(FConnectionExecutor.LastEnable, 'Should call with Enable=True');
end;

procedure TBluetoothServiceTests.Disconnect_CallsConnectionExecutor;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.Disconnect(Device);

  // Assert
  Assert.AreEqual(1, FConnectionExecutor.ExecuteCallCount, 'Should call executor once');
  Assert.IsFalse(FConnectionExecutor.LastEnable, 'Should call with Enable=False');
end;

procedure TBluetoothServiceTests.ToggleConnection_ConnectedDevice_Disconnects;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csConnected);
  FConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.ToggleConnection(Device);

  // Assert
  Assert.IsFalse(FConnectionExecutor.LastEnable,
    'Toggle on connected device should disconnect (Enable=False)');
end;

procedure TBluetoothServiceTests.ToggleConnection_DisconnectedDevice_Connects;
var
  Device: TBluetoothDeviceInfo;
  ConnManager: IBluetoothConnectionManager;
begin
  // Arrange
  Device := CreateTestDevice($112233445566, 'Device1', btAudioOutput, csDisconnected);
  FConnectionExecutor.ExecuteResult := TConnectionResult.Ok;

  Supports(FService, IBluetoothConnectionManager, ConnManager);

  // Act
  ConnManager.ToggleConnection(Device);

  // Assert
  Assert.IsTrue(FConnectionExecutor.LastEnable,
    'Toggle on disconnected device should connect (Enable=True)');
end;

initialization
  TDUnitX.RegisterTestFixture(TBluetoothServiceTests);

end.
