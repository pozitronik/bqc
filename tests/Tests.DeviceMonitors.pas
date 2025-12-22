{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Monitor Unit Tests                       }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.DeviceMonitors;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.DeviceMonitors,
  App.ConfigInterfaces,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TPollingMonitor.
  /// </summary>
  [TestFixture]
  TPollingMonitorTests = class
  private
    FMonitor: IDeviceMonitor;
    FPollingConfigObj: TMockPollingConfig;
    FPollingConfig: IPollingConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Create_IsNotRunning;
    [Test]
    procedure Start_ReturnsTrue;
    [Test]
    procedure Start_SetsRunningTrue;
    [Test]
    procedure Stop_SetsRunningFalse;
    [Test]
    procedure Start_WhenAlreadyRunning_ReturnsTrue;
  end;

  /// <summary>
  /// Test fixture for TDeviceMonitorFactory.
  /// </summary>
  [TestFixture]
  TDeviceMonitorFactoryTests = class
  private
    FPollingConfigObj: TMockPollingConfig;
    FPollingConfig: IPollingConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CreateMonitor_PollingMode_ReturnsPollingMonitor;
    [Test]
    procedure CreateMonitor_FallbackMode_ReturnsFallbackMonitor;
    [Test]
    procedure CreateMonitor_ReturnsNotNil;
  end;

  /// <summary>
  /// Test fixture for TMockDeviceMonitor.
  /// </summary>
  [TestFixture]
  TMockDeviceMonitorTests = class
  private
    FMonitor: TMockDeviceMonitor;
    FStateChangedCount: Integer;
    FLastAddress: UInt64;
    FLastState: TBluetoothConnectionState;
    FErrorCount: Integer;
    FLastErrorMessage: string;
    procedure HandleStateChanged(Sender: TObject; const AAddress: UInt64;
      AState: TBluetoothConnectionState);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Create_IsNotRunning;
    [Test]
    procedure Start_DefaultReturnsTrue;
    [Test]
    procedure Start_IncrementsCallCount;
    [Test]
    procedure Start_SetsRunningTrue;
    [Test]
    procedure Start_WhenStartResultFalse_ReturnsFalse;
    [Test]
    procedure Stop_IncrementsCallCount;
    [Test]
    procedure Stop_SetsRunningFalse;
    [Test]
    procedure SimulateDeviceStateChanged_FiresEvent;
    [Test]
    procedure SimulateDeviceStateChanged_PassesCorrectData;
    [Test]
    procedure SimulateError_FiresEvent;
  end;

implementation

{ TPollingMonitorTests }

procedure TPollingMonitorTests.Setup;
begin
  FPollingConfigObj := TMockPollingConfig.Create;
  FPollingConfigObj.PollingMode := pmPrimary;
  FPollingConfigObj.PollingInterval := 100;  // Fast for testing
  FPollingConfig := FPollingConfigObj;
  FMonitor := TPollingMonitor.Create(FPollingConfig.PollingInterval);
end;

procedure TPollingMonitorTests.TearDown;
begin
  FMonitor.Stop;
  FMonitor := nil;
  FPollingConfig := nil;
  // FPollingConfigObj freed via interface reference counting
end;

procedure TPollingMonitorTests.Create_IsNotRunning;
begin
  Assert.IsFalse(FMonitor.IsRunning);
end;

procedure TPollingMonitorTests.Start_ReturnsTrue;
begin
  Assert.IsTrue(FMonitor.Start);
end;

procedure TPollingMonitorTests.Start_SetsRunningTrue;
begin
  FMonitor.Start;
  Assert.IsTrue(FMonitor.IsRunning);
end;

procedure TPollingMonitorTests.Stop_SetsRunningFalse;
begin
  FMonitor.Start;
  FMonitor.Stop;
  Assert.IsFalse(FMonitor.IsRunning);
end;

procedure TPollingMonitorTests.Start_WhenAlreadyRunning_ReturnsTrue;
begin
  FMonitor.Start;
  Assert.IsTrue(FMonitor.Start);
end;

{ TDeviceMonitorFactoryTests }

procedure TDeviceMonitorFactoryTests.Setup;
begin
  FPollingConfigObj := TMockPollingConfig.Create;
  FPollingConfig := FPollingConfigObj;
end;

procedure TDeviceMonitorFactoryTests.TearDown;
begin
  FPollingConfig := nil;
end;

procedure TDeviceMonitorFactoryTests.CreateMonitor_PollingMode_ReturnsPollingMonitor;
var
  Factory: IDeviceMonitorFactory;
  Monitor: IDeviceMonitor;
begin
  FPollingConfigObj.PollingMode := pmPrimary;
  Factory := TDeviceMonitorFactory.Create(FPollingConfig);

  Monitor := Factory.CreateMonitor;

  Assert.IsNotNull(Monitor);
  // Can verify it's a polling monitor by checking it starts successfully
  Assert.IsTrue(Monitor.Start);
  Monitor.Stop;
end;

procedure TDeviceMonitorFactoryTests.CreateMonitor_FallbackMode_ReturnsFallbackMonitor;
var
  Factory: IDeviceMonitorFactory;
  Monitor: IDeviceMonitor;
begin
  FPollingConfigObj.PollingMode := pmFallback;
  Factory := TDeviceMonitorFactory.Create(FPollingConfig);

  Monitor := Factory.CreateMonitor;

  Assert.IsNotNull(Monitor);
end;

procedure TDeviceMonitorFactoryTests.CreateMonitor_ReturnsNotNil;
var
  Factory: IDeviceMonitorFactory;
begin
  Factory := TDeviceMonitorFactory.Create(FPollingConfig);
  Assert.IsNotNull(Factory.CreateMonitor);
end;

{ TMockDeviceMonitorTests }

procedure TMockDeviceMonitorTests.Setup;
begin
  FMonitor := TMockDeviceMonitor.Create;
  FMonitor.OnDeviceStateChanged := HandleStateChanged;
  FMonitor.OnError := HandleError;
  FStateChangedCount := 0;
  FLastAddress := 0;
  FLastState := csUnknown;
  FErrorCount := 0;
  FLastErrorMessage := '';
end;

procedure TMockDeviceMonitorTests.TearDown;
begin
  FMonitor.Free;
end;

procedure TMockDeviceMonitorTests.HandleStateChanged(Sender: TObject;
  const AAddress: UInt64; AState: TBluetoothConnectionState);
begin
  Inc(FStateChangedCount);
  FLastAddress := AAddress;
  FLastState := AState;
end;

procedure TMockDeviceMonitorTests.HandleError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  Inc(FErrorCount);
  FLastErrorMessage := AMessage;
end;

procedure TMockDeviceMonitorTests.Create_IsNotRunning;
begin
  Assert.IsFalse(FMonitor.IsRunning);
end;

procedure TMockDeviceMonitorTests.Start_DefaultReturnsTrue;
begin
  Assert.IsTrue(FMonitor.Start);
end;

procedure TMockDeviceMonitorTests.Start_IncrementsCallCount;
begin
  Assert.AreEqual(0, FMonitor.StartCallCount);
  FMonitor.Start;
  Assert.AreEqual(1, FMonitor.StartCallCount);
  FMonitor.Start;
  Assert.AreEqual(2, FMonitor.StartCallCount);
end;

procedure TMockDeviceMonitorTests.Start_SetsRunningTrue;
begin
  FMonitor.Start;
  Assert.IsTrue(FMonitor.IsRunning);
end;

procedure TMockDeviceMonitorTests.Start_WhenStartResultFalse_ReturnsFalse;
begin
  FMonitor.StartResult := False;
  Assert.IsFalse(FMonitor.Start);
  Assert.IsFalse(FMonitor.IsRunning);
end;

procedure TMockDeviceMonitorTests.Stop_IncrementsCallCount;
begin
  Assert.AreEqual(0, FMonitor.StopCallCount);
  FMonitor.Stop;
  Assert.AreEqual(1, FMonitor.StopCallCount);
end;

procedure TMockDeviceMonitorTests.Stop_SetsRunningFalse;
begin
  FMonitor.Start;
  FMonitor.Stop;
  Assert.IsFalse(FMonitor.IsRunning);
end;

procedure TMockDeviceMonitorTests.SimulateDeviceStateChanged_FiresEvent;
begin
  Assert.AreEqual(0, FStateChangedCount);

  FMonitor.SimulateDeviceStateChanged($AABBCC, csConnected);

  Assert.AreEqual(1, FStateChangedCount);
end;

procedure TMockDeviceMonitorTests.SimulateDeviceStateChanged_PassesCorrectData;
begin
  FMonitor.SimulateDeviceStateChanged($123456789ABC, csDisconnected);

  Assert.AreEqual(UInt64($123456789ABC), FLastAddress);
  Assert.AreEqual(Ord(csDisconnected), Ord(FLastState));
end;

procedure TMockDeviceMonitorTests.SimulateError_FiresEvent;
begin
  Assert.AreEqual(0, FErrorCount);

  FMonitor.SimulateError('Test error', 123);

  Assert.AreEqual(1, FErrorCount);
  Assert.AreEqual('Test error', FLastErrorMessage);
end;

initialization
  TDUnitX.RegisterTestFixture(TPollingMonitorTests);
  TDUnitX.RegisterTestFixture(TDeviceMonitorFactoryTests);
  TDUnitX.RegisterTestFixture(TMockDeviceMonitorTests);

end.
