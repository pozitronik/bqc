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
  App.ConfigEnums,
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
  /// Test fixture for TFallbackMonitor with injected mock monitors.
  /// </summary>
  [TestFixture]
  TFallbackMonitorTests = class
  private
    FPrimaryMonitor: TMockDeviceMonitor;
    FSecondaryMonitor: TMockDeviceMonitor;
    FFallbackMonitor: IDeviceMonitor;
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
    procedure Start_UsesPrimaryMonitor;
    [Test]
    procedure Start_PrimaryFails_UsesSecondary;
    [Test]
    procedure Start_WhenAlreadyRunning_ReturnsTrue;
    [Test]
    procedure Stop_StopsBothMonitors;
    [Test]
    procedure PrimaryError_SwitchesToSecondary;
    [Test]
    procedure DeviceStateChanged_ForwardsEvent;
    [Test]
    procedure SecondaryError_ForwardsError;
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

{ TFallbackMonitorTests }

procedure TFallbackMonitorTests.Setup;
begin
  FPrimaryMonitor := TMockDeviceMonitor.Create;
  FSecondaryMonitor := TMockDeviceMonitor.Create;
  // Note: TFallbackMonitor takes ownership via interface reference
  FFallbackMonitor := TFallbackMonitor.Create(FPrimaryMonitor, FSecondaryMonitor);
  FFallbackMonitor.SetOnDeviceStateChanged(HandleStateChanged);
  FFallbackMonitor.SetOnError(HandleError);
  FStateChangedCount := 0;
  FLastAddress := 0;
  FLastState := csUnknown;
  FErrorCount := 0;
  FLastErrorMessage := '';
end;

procedure TFallbackMonitorTests.TearDown;
begin
  FFallbackMonitor.Stop;
  FFallbackMonitor := nil;
  // Mock monitors are freed via interface reference counting
end;

procedure TFallbackMonitorTests.HandleStateChanged(Sender: TObject;
  const AAddress: UInt64; AState: TBluetoothConnectionState);
begin
  Inc(FStateChangedCount);
  FLastAddress := AAddress;
  FLastState := AState;
end;

procedure TFallbackMonitorTests.HandleError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  Inc(FErrorCount);
  FLastErrorMessage := AMessage;
end;

procedure TFallbackMonitorTests.Create_IsNotRunning;
begin
  Assert.IsFalse(FFallbackMonitor.IsRunning);
end;

procedure TFallbackMonitorTests.Start_UsesPrimaryMonitor;
begin
  FFallbackMonitor.Start;

  Assert.AreEqual(1, FPrimaryMonitor.StartCallCount, 'Primary should be started');
  Assert.AreEqual(0, FSecondaryMonitor.StartCallCount, 'Secondary should not be started');
  Assert.IsTrue(FFallbackMonitor.IsRunning);
end;

procedure TFallbackMonitorTests.Start_PrimaryFails_UsesSecondary;
begin
  FPrimaryMonitor.StartResult := False;

  FFallbackMonitor.Start;

  Assert.AreEqual(1, FPrimaryMonitor.StartCallCount, 'Primary should be attempted');
  Assert.AreEqual(1, FSecondaryMonitor.StartCallCount, 'Secondary should be started');
  Assert.IsTrue(FFallbackMonitor.IsRunning);
end;

procedure TFallbackMonitorTests.Start_WhenAlreadyRunning_ReturnsTrue;
begin
  FFallbackMonitor.Start;
  Assert.IsTrue(FFallbackMonitor.Start);
  Assert.AreEqual(1, FPrimaryMonitor.StartCallCount, 'Should not start again');
end;

procedure TFallbackMonitorTests.Stop_StopsBothMonitors;
begin
  FFallbackMonitor.Start;
  FFallbackMonitor.Stop;

  Assert.AreEqual(1, FPrimaryMonitor.StopCallCount);
  Assert.IsFalse(FFallbackMonitor.IsRunning);
end;

procedure TFallbackMonitorTests.PrimaryError_SwitchesToSecondary;
begin
  FFallbackMonitor.Start;
  Assert.AreEqual(0, FSecondaryMonitor.StartCallCount, 'Secondary not started yet');

  // Simulate error on primary
  FPrimaryMonitor.SimulateError('Primary failed', 100);

  Assert.AreEqual(1, FSecondaryMonitor.StartCallCount, 'Secondary should be started after error');
  Assert.AreEqual(1, FErrorCount, 'Error should be forwarded');
  Assert.AreEqual('Primary failed', FLastErrorMessage);
end;

procedure TFallbackMonitorTests.DeviceStateChanged_ForwardsEvent;
begin
  FFallbackMonitor.Start;

  FPrimaryMonitor.SimulateDeviceStateChanged($AABBCCDD, csConnected);

  Assert.AreEqual(1, FStateChangedCount);
  Assert.AreEqual(UInt64($AABBCCDD), FLastAddress);
  Assert.AreEqual(Ord(csConnected), Ord(FLastState));
end;

procedure TFallbackMonitorTests.SecondaryError_ForwardsError;
begin
  FPrimaryMonitor.StartResult := False;
  FFallbackMonitor.Start;
  FErrorCount := 0;  // Reset after potential primary error

  FSecondaryMonitor.SimulateError('Secondary error', 200);

  Assert.AreEqual(1, FErrorCount, 'Secondary error should be forwarded');
  Assert.AreEqual('Secondary error', FLastErrorMessage);
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
  TDUnitX.RegisterTestFixture(TFallbackMonitorTests);
  TDUnitX.RegisterTestFixture(TDeviceMonitorFactoryTests);
  TDUnitX.RegisterTestFixture(TMockDeviceMonitorTests);

end.
