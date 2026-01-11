{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Battery Coordinator Tests                }
{                                                       }
{*******************************************************}

unit Tests.DeviceBatteryCoordinator;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.MainViewInterfaces,
  App.DeviceBatteryCoordinator,
  App.DeviceDisplayTypes,
  App.DeviceDisplayItemBuilder,
  App.AsyncExecutor,
  Tests.Mocks,
  Tests.Mocks.Bluetooth,
  Tests.Mocks.View,
  Tests.Mocks.Infrastructure;

type
  /// <summary>
  /// Tests for TDeviceBatteryCoordinator focusing on battery
  /// refresh logic, event handling, and shutdown safety.
  /// </summary>
  [TestFixture]
  TDeviceBatteryCoordinatorTests = class
  private
    FCoordinator: TDeviceBatteryCoordinator;
    FBatteryCache: TMockBatteryCache;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FDeviceListView: TMockMainView;
    FAsyncExecutor: TMockAsyncExecutor;

    // Device lookup state
    FFindDeviceResult: TBluetoothDeviceInfo;
    FConnectedAddresses: TArray<UInt64>;

    function FindDeviceFunc(AAddress: UInt64): TBluetoothDeviceInfo;
    function GetConnectedAddressesFunc: TArray<UInt64>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation tests
    [Test]
    procedure Create_InitializesCorrectly;

    // Initialize tests
    [Test]
    procedure Initialize_WiresUpEventHandler;

    // HandleBatteryQueryCompleted tests (triggered via cache event)
    [Test]
    procedure BatteryQueryCompleted_ShuttingDown_SkipsUpdate;
    [Test]
    procedure BatteryQueryCompleted_DeviceNotFound_SkipsUpdate;
    [Test]
    procedure BatteryQueryCompleted_DeviceFound_UpdatesDisplayItem;

    // RefreshBatteryForConnectedDevices tests
    [Test]
    procedure RefreshBatteryForConnectedDevices_NoDevices_NoRefresh;
    [Test]
    procedure RefreshBatteryForConnectedDevices_WithDevices_RequestsRefreshAll;

    // ScheduleDelayedBatteryRefresh tests
    [Test]
    procedure ScheduleDelayedBatteryRefresh_NotShuttingDown_RequestsRefresh;
    [Test]
    procedure ScheduleDelayedBatteryRefresh_ShuttingDown_SkipsRefresh;
    [Test]
    procedure ScheduleDelayedBatteryRefresh_UsesCorrectDelay;

    // Shutdown tests
    [Test]
    procedure Shutdown_PreventsSubsequentUpdates;

    // BatteryCache property test
    [Test]
    procedure BatteryCache_ReturnsInjectedInstance;
  end;

implementation

{ TDeviceBatteryCoordinatorTests }

procedure TDeviceBatteryCoordinatorTests.Setup;
begin
  FBatteryCache := TMockBatteryCache.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FDeviceListView := TMockMainView.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FAsyncExecutor.Synchronous := True;  // Execute immediately for deterministic tests

  // Default: device not found (empty record)
  FFindDeviceResult := Default(TBluetoothDeviceInfo);
  FConnectedAddresses := nil;

  FCoordinator := TDeviceBatteryCoordinator.Create(
    FBatteryCache,
    FDisplayItemBuilder,
    FDeviceListView as IDeviceListView,
    FAsyncExecutor,
    FindDeviceFunc,
    GetConnectedAddressesFunc
  );
end;

procedure TDeviceBatteryCoordinatorTests.TearDown;
begin
  FCoordinator.Free;
  FCoordinator := nil;

  // Interface references are released when coordinator is freed
  FAsyncExecutor := nil;
  FDeviceListView := nil;
  FDisplayItemBuilder := nil;
  FBatteryCache := nil;
end;

function TDeviceBatteryCoordinatorTests.FindDeviceFunc(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  // Return the configured result if address matches, otherwise empty
  if (FFindDeviceResult.AddressInt <> 0) and (FFindDeviceResult.AddressInt = AAddress) then
    Result := FFindDeviceResult
  else
    Result := Default(TBluetoothDeviceInfo);
end;

function TDeviceBatteryCoordinatorTests.GetConnectedAddressesFunc: TArray<UInt64>;
begin
  Result := FConnectedAddresses;
end;

{ Creation tests }

procedure TDeviceBatteryCoordinatorTests.Create_InitializesCorrectly;
begin
  Assert.IsNotNull(FCoordinator, 'Coordinator should be created');
  Assert.IsNotNull(FCoordinator.BatteryCache, 'BatteryCache should be accessible');
end;

{ Initialize tests }

procedure TDeviceBatteryCoordinatorTests.Initialize_WiresUpEventHandler;
var
  Status: TBatteryStatus;
begin
  // Before Initialize, event handler is not wired
  FCoordinator.Initialize;

  // Configure a device to be found
  FFindDeviceResult := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csConnected);

  // Simulate battery query completion - this should trigger the handler
  Status := TBatteryStatus.Create(75);
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);

  // Verify the handler was called (it should have called BuildDisplayItem)
  Assert.AreEqual(1, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Initialize should wire up event handler that calls BuildDisplayItem');
end;

{ HandleBatteryQueryCompleted tests }

procedure TDeviceBatteryCoordinatorTests.BatteryQueryCompleted_ShuttingDown_SkipsUpdate;
var
  Status: TBatteryStatus;
begin
  FCoordinator.Initialize;

  // Configure a device to be found
  FFindDeviceResult := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csConnected);

  // Shutdown coordinator
  FCoordinator.Shutdown;

  // Simulate battery query completion
  Status := TBatteryStatus.Create(75);
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);

  // Verify no update was made
  Assert.AreEqual(0, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should skip update when shutting down');
  Assert.AreEqual(0, FDeviceListView.UpdateDisplayItemCount,
    'Should not call UpdateDisplayItem when shutting down');
end;

procedure TDeviceBatteryCoordinatorTests.BatteryQueryCompleted_DeviceNotFound_SkipsUpdate;
var
  Status: TBatteryStatus;
begin
  FCoordinator.Initialize;

  // Leave FFindDeviceResult as default (empty - device not found)

  // Simulate battery query completion
  Status := TBatteryStatus.Create(75);
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);

  // Verify no update was made
  Assert.AreEqual(0, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should skip BuildDisplayItem when device not found');
  Assert.AreEqual(0, FDeviceListView.UpdateDisplayItemCount,
    'Should not call UpdateDisplayItem when device not found');
end;

procedure TDeviceBatteryCoordinatorTests.BatteryQueryCompleted_DeviceFound_UpdatesDisplayItem;
var
  Status: TBatteryStatus;
begin
  FCoordinator.Initialize;

  // Configure a device to be found
  FFindDeviceResult := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csConnected);

  // Simulate battery query completion
  Status := TBatteryStatus.Create(75);
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);

  // Verify display item was built and view was updated
  Assert.AreEqual(1, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should call BuildDisplayItem when device found');
  Assert.AreEqual(UInt64($AABBCCDDEEFF), FDisplayItemBuilder.LastDevice.AddressInt,
    'Should build display item for correct device');
  Assert.AreEqual(1, FDeviceListView.UpdateDisplayItemCount,
    'Should call UpdateDisplayItem on view');
end;

{ RefreshBatteryForConnectedDevices tests }

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_NoDevices_NoRefresh;
begin
  // No connected devices
  FConnectedAddresses := nil;

  FCoordinator.RefreshBatteryForConnectedDevices;

  Assert.AreEqual(0, FBatteryCache.RequestRefreshAllCallCount,
    'Should not call RequestRefreshAll when no connected devices');
end;

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_WithDevices_RequestsRefreshAll;
begin
  // Configure connected devices
  SetLength(FConnectedAddresses, 3);
  FConnectedAddresses[0] := $111111111111;
  FConnectedAddresses[1] := $222222222222;
  FConnectedAddresses[2] := $333333333333;

  FCoordinator.RefreshBatteryForConnectedDevices;

  Assert.AreEqual(1, FBatteryCache.RequestRefreshAllCallCount,
    'Should call RequestRefreshAll once');
end;

{ ScheduleDelayedBatteryRefresh tests }

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_NotShuttingDown_RequestsRefresh;
begin
  FCoordinator.ScheduleDelayedBatteryRefresh($AABBCCDDEEFF, 1000);

  Assert.AreEqual(1, FAsyncExecutor.RunDelayedCallCount,
    'Should call RunDelayed');
  Assert.AreEqual(1, FBatteryCache.RequestRefreshCallCount,
    'Should call RequestRefresh (executor is synchronous)');
  Assert.AreEqual(UInt64($AABBCCDDEEFF), FBatteryCache.LastRefreshAddress,
    'Should request refresh for correct address');
end;

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_ShuttingDown_SkipsRefresh;
begin
  // Shutdown first
  FCoordinator.Shutdown;

  FCoordinator.ScheduleDelayedBatteryRefresh($AABBCCDDEEFF, 1000);

  Assert.AreEqual(1, FAsyncExecutor.RunDelayedCallCount,
    'Should still call RunDelayed (async is scheduled)');
  Assert.AreEqual(0, FBatteryCache.RequestRefreshCallCount,
    'Should NOT call RequestRefresh when shutting down');
end;

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_UsesCorrectDelay;
begin
  FCoordinator.ScheduleDelayedBatteryRefresh($AABBCCDDEEFF, 2500);

  Assert.AreEqual(2500, FAsyncExecutor.LastDelayMs,
    'Should pass correct delay to executor');
end;

{ Shutdown tests }

procedure TDeviceBatteryCoordinatorTests.Shutdown_PreventsSubsequentUpdates;
var
  Status: TBatteryStatus;
begin
  FCoordinator.Initialize;

  // Configure a device to be found
  FFindDeviceResult := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', btAudioOutput, csConnected);

  // First query should work
  Status := TBatteryStatus.Create(75);
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);
  Assert.AreEqual(1, FDisplayItemBuilder.BuildDisplayItemCallCount, 'First query should work');

  // Shutdown
  FCoordinator.Shutdown;

  // Second query should be skipped
  FBatteryCache.SimulateQueryCompleted($AABBCCDDEEFF, Status);
  Assert.AreEqual(1, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Second query should be skipped after shutdown');
end;

{ BatteryCache property test }

procedure TDeviceBatteryCoordinatorTests.BatteryCache_ReturnsInjectedInstance;
begin
  // Verify BatteryCache property returns the injected instance
  Assert.AreSame(FBatteryCache as IBatteryCache, FCoordinator.BatteryCache,
    'BatteryCache property should return injected instance');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceBatteryCoordinatorTests);

end.
