{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Battery Coordinator Tests                }
{                                                       }
{       Tests for TDeviceBatteryCoordinator covering:   }
{       - Battery cache event wiring                    }
{       - Battery query completion handling             }
{       - Connected device battery refresh              }
{       - Delayed battery refresh scheduling            }
{       - Shutdown safety (async callback guards)       }
{                                                       }
{*******************************************************}

unit Tests.DeviceBatteryCoordinator;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.MainViewInterfaces,
  App.DeviceDisplayTypes,
  App.DeviceDisplayItemBuilder,
  App.AsyncExecutor,
  App.DeviceBatteryCoordinator,
  Tests.Mocks.Bluetooth,
  Tests.Mocks.View,
  Tests.Mocks.Infrastructure;

type
  /// <summary>
  /// Tests for TDeviceBatteryCoordinator focusing on battery monitoring,
  /// cache integration, and async refresh scheduling.
  /// </summary>
  [TestFixture]
  TDeviceBatteryCoordinatorTests = class
  private
    FCoordinator: TDeviceBatteryCoordinator;
    FBatteryCache: TMockBatteryCache;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FDeviceListView: TMockMainView;
    FAsyncExecutor: TMockAsyncExecutor;

    // Test tracking for injected functions
    FFindDeviceCallCount: Integer;
    FLastFindDeviceAddress: UInt64;
    FFindDeviceResult: TBluetoothDeviceInfo;

    FGetConnectedAddressesCallCount: Integer;
    FConnectedAddresses: TArray<UInt64>;

    /// <summary>
    /// Injected find device function for testing.
    /// </summary>
    function FindDeviceFunc(AAddress: UInt64): TBluetoothDeviceInfo;

    /// <summary>
    /// Injected get connected addresses function for testing.
    /// </summary>
    function GetConnectedAddressesFunc: TArray<UInt64>;

    /// <summary>
    /// Creates a test device with specified address.
    /// </summary>
    function CreateTestDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation tests
    [Test]
    procedure Create_StoresBatteryCache;
    [Test]
    procedure Create_NotShuttingDownByDefault;

    // Initialize tests
    [Test]
    procedure Initialize_WiresBatteryQueryCompletedHandler;

    // Shutdown tests
    [Test]
    procedure Shutdown_SetsShuttingDownFlag;
    [Test]
    procedure Shutdown_PreventsHandlerExecution;

    // HandleBatteryQueryCompleted tests (via cache event)
    [Test]
    procedure HandleBatteryQueryCompleted_CallsFindDeviceFunc;
    [Test]
    procedure HandleBatteryQueryCompleted_SkipsWhenDeviceNotFound;
    [Test]
    procedure HandleBatteryQueryCompleted_BuildsDisplayItem;
    [Test]
    procedure HandleBatteryQueryCompleted_UpdatesViewWithDisplayItem;
    [Test]
    procedure HandleBatteryQueryCompleted_SkipsWhenShuttingDown;

    // RefreshBatteryForConnectedDevices tests
    [Test]
    procedure RefreshBatteryForConnectedDevices_CallsGetConnectedAddresses;
    [Test]
    procedure RefreshBatteryForConnectedDevices_RequestsRefreshAll;
    [Test]
    procedure RefreshBatteryForConnectedDevices_SkipsWhenNoConnectedDevices;
    [Test]
    procedure RefreshBatteryForConnectedDevices_PassesAllAddresses;

    // ScheduleDelayedBatteryRefresh tests
    [Test]
    procedure ScheduleDelayedBatteryRefresh_UsesRunDelayed;
    [Test]
    procedure ScheduleDelayedBatteryRefresh_PassesCorrectDelay;
    [Test]
    procedure ScheduleDelayedBatteryRefresh_CallbackRequestsRefresh;
    [Test]
    procedure ScheduleDelayedBatteryRefresh_CallbackSkipsWhenShuttingDown;
  end;

implementation

{ TDeviceBatteryCoordinatorTests }

procedure TDeviceBatteryCoordinatorTests.Setup;
begin
  FBatteryCache := TMockBatteryCache.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FDeviceListView := TMockMainView.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FAsyncExecutor.Synchronous := False;  // Don't execute immediately by default

  // Reset tracking
  FFindDeviceCallCount := 0;
  FLastFindDeviceAddress := 0;
  FFindDeviceResult := Default(TBluetoothDeviceInfo);

  FGetConnectedAddressesCallCount := 0;
  FConnectedAddresses := [];

  FCoordinator := TDeviceBatteryCoordinator.Create(
    FBatteryCache as IBatteryCache,
    FDisplayItemBuilder as IDeviceDisplayItemBuilder,
    FDeviceListView as IDeviceListView,
    FAsyncExecutor as IAsyncExecutor,
    FindDeviceFunc,
    GetConnectedAddressesFunc
  );
end;

procedure TDeviceBatteryCoordinatorTests.TearDown;
begin
  FCoordinator.Free;
  FCoordinator := nil;
  // Interfaces are reference-counted
  FAsyncExecutor := nil;
  FDeviceListView := nil;
  FDisplayItemBuilder := nil;
  FBatteryCache := nil;
end;

function TDeviceBatteryCoordinatorTests.FindDeviceFunc(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  Inc(FFindDeviceCallCount);
  FLastFindDeviceAddress := AAddress;
  Result := FFindDeviceResult;
end;

function TDeviceBatteryCoordinatorTests.GetConnectedAddressesFunc: TArray<UInt64>;
begin
  Inc(FGetConnectedAddressesCallCount);
  Result := FConnectedAddresses;
end;

function TDeviceBatteryCoordinatorTests.CreateTestDevice(AAddress: UInt64;
  const AName: string): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddress),
    AAddress,
    AName,
    btAudioOutput,
    csConnected,
    True,   // IsPaired
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

{ Creation tests }

procedure TDeviceBatteryCoordinatorTests.Create_StoresBatteryCache;
begin
  Assert.IsNotNull(FCoordinator.BatteryCache,
    'BatteryCache property should return injected cache');
end;

procedure TDeviceBatteryCoordinatorTests.Create_NotShuttingDownByDefault;
begin
  // Verify by triggering an event - it should NOT be skipped
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($001, 'Test');

  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(75));

  Assert.AreEqual(1, FFindDeviceCallCount,
    'FindDeviceFunc should be called when not shutting down');
end;

{ Initialize tests }

procedure TDeviceBatteryCoordinatorTests.Initialize_WiresBatteryQueryCompletedHandler;
begin
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($AABBCCDD, 'Test Device');

  // Trigger event through mock
  FBatteryCache.SimulateQueryCompleted($AABBCCDD, TBatteryStatus.Create(50));

  // Handler should have been called
  Assert.AreEqual(1, FFindDeviceCallCount,
    'Handler should be wired and called when event fires');
end;

{ Shutdown tests }

procedure TDeviceBatteryCoordinatorTests.Shutdown_SetsShuttingDownFlag;
begin
  FCoordinator.Initialize;
  FCoordinator.Shutdown;

  // Verify by triggering event - it should be skipped
  FFindDeviceResult := CreateTestDevice($001, 'Test');
  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(50));

  Assert.AreEqual(0, FFindDeviceCallCount,
    'Handler should skip execution after shutdown');
end;

procedure TDeviceBatteryCoordinatorTests.Shutdown_PreventsHandlerExecution;
begin
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($001, 'Test');

  // First call before shutdown - should work
  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(50));
  Assert.AreEqual(1, FFindDeviceCallCount, 'Should work before shutdown');

  // Shutdown
  FCoordinator.Shutdown;

  // Second call after shutdown - should be skipped
  FBatteryCache.SimulateQueryCompleted($002, TBatteryStatus.Create(75));
  Assert.AreEqual(1, FFindDeviceCallCount,
    'Should NOT call FindDeviceFunc after shutdown');
end;

{ HandleBatteryQueryCompleted tests }

procedure TDeviceBatteryCoordinatorTests.HandleBatteryQueryCompleted_CallsFindDeviceFunc;
var
  ExpectedAddress: UInt64;
begin
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($AABBCCDD, 'Test');

  FBatteryCache.SimulateQueryCompleted($AABBCCDD, TBatteryStatus.Create(80));

  Assert.AreEqual(1, FFindDeviceCallCount, 'Should call FindDeviceFunc');
  ExpectedAddress := $AABBCCDD;
  Assert.AreEqual(ExpectedAddress, FLastFindDeviceAddress,
    'Should pass correct address to FindDeviceFunc');
end;

procedure TDeviceBatteryCoordinatorTests.HandleBatteryQueryCompleted_SkipsWhenDeviceNotFound;
begin
  FCoordinator.Initialize;
  // FFindDeviceResult is default (AddressInt = 0) - device not found

  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(50));

  // Should call FindDeviceFunc but not proceed further
  Assert.AreEqual(1, FFindDeviceCallCount, 'Should call FindDeviceFunc');
  Assert.AreEqual(0, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should NOT call BuildDisplayItem when device not found');
  Assert.AreEqual(0, FDeviceListView.UpdateDisplayItemCount,
    'Should NOT call UpdateDisplayItem when device not found');
end;

procedure TDeviceBatteryCoordinatorTests.HandleBatteryQueryCompleted_BuildsDisplayItem;
begin
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($001, 'My Headphones');

  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(65));

  Assert.AreEqual(1, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should call BuildDisplayItem');
  Assert.AreEqual('My Headphones', FDisplayItemBuilder.LastDevice.Name,
    'Should pass correct device to BuildDisplayItem');
end;

procedure TDeviceBatteryCoordinatorTests.HandleBatteryQueryCompleted_UpdatesViewWithDisplayItem;
begin
  FCoordinator.Initialize;
  FFindDeviceResult := CreateTestDevice($001, 'Test Device');

  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(90));

  Assert.AreEqual(1, FDeviceListView.UpdateDisplayItemCount,
    'Should call UpdateDisplayItem on view');
end;

procedure TDeviceBatteryCoordinatorTests.HandleBatteryQueryCompleted_SkipsWhenShuttingDown;
begin
  FCoordinator.Initialize;
  FCoordinator.Shutdown;
  FFindDeviceResult := CreateTestDevice($001, 'Test');

  FBatteryCache.SimulateQueryCompleted($001, TBatteryStatus.Create(50));

  Assert.AreEqual(0, FFindDeviceCallCount,
    'Should NOT call FindDeviceFunc when shutting down');
  Assert.AreEqual(0, FDisplayItemBuilder.BuildDisplayItemCallCount,
    'Should NOT call BuildDisplayItem when shutting down');
  Assert.AreEqual(0, FDeviceListView.UpdateDisplayItemCount,
    'Should NOT call UpdateDisplayItem when shutting down');
end;

{ RefreshBatteryForConnectedDevices tests }

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_CallsGetConnectedAddresses;
begin
  FConnectedAddresses := [$001, $002];

  FCoordinator.RefreshBatteryForConnectedDevices;

  Assert.AreEqual(1, FGetConnectedAddressesCallCount,
    'Should call GetConnectedAddressesFunc');
end;

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_RequestsRefreshAll;
begin
  FConnectedAddresses := [$001, $002, $003];

  FCoordinator.RefreshBatteryForConnectedDevices;

  Assert.AreEqual(1, FBatteryCache.RequestRefreshAllCallCount,
    'Should call RequestRefreshAll on battery cache');
end;

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_SkipsWhenNoConnectedDevices;
begin
  FConnectedAddresses := [];  // No connected devices

  FCoordinator.RefreshBatteryForConnectedDevices;

  Assert.AreEqual(1, FGetConnectedAddressesCallCount,
    'Should still call GetConnectedAddressesFunc');
  Assert.AreEqual(0, FBatteryCache.RequestRefreshAllCallCount,
    'Should NOT call RequestRefreshAll when no devices connected');
end;

procedure TDeviceBatteryCoordinatorTests.RefreshBatteryForConnectedDevices_PassesAllAddresses;
var
  Addr1, Addr2: UInt64;
begin
  Addr1 := $AABBCCDD;
  Addr2 := $11223344;
  FConnectedAddresses := [Addr1, Addr2];

  FCoordinator.RefreshBatteryForConnectedDevices;

  // RequestRefreshAll was called - verify through call count
  // (TMockBatteryCache doesn't track the addresses passed, but we verified it's called)
  Assert.AreEqual(1, FBatteryCache.RequestRefreshAllCallCount,
    'Should call RequestRefreshAll with connected addresses');
end;

{ ScheduleDelayedBatteryRefresh tests }

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_UsesRunDelayed;
begin
  FCoordinator.ScheduleDelayedBatteryRefresh($001, 5000);

  Assert.AreEqual(1, FAsyncExecutor.RunDelayedCallCount,
    'Should call RunDelayed on async executor');
end;

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_PassesCorrectDelay;
begin
  FCoordinator.ScheduleDelayedBatteryRefresh($001, 10000);

  Assert.AreEqual(10000, FAsyncExecutor.LastDelayMs,
    'Should pass correct delay value');
end;

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_CallbackRequestsRefresh;
var
  ExpectedAddress: UInt64;
begin
  ExpectedAddress := $AABBCCDD;

  FCoordinator.ScheduleDelayedBatteryRefresh(ExpectedAddress, 5000);

  // Execute the pending callback
  FAsyncExecutor.ExecutePending;

  Assert.AreEqual(1, FBatteryCache.RequestRefreshCallCount,
    'Callback should call RequestRefresh');
  Assert.AreEqual(ExpectedAddress, FBatteryCache.LastRefreshAddress,
    'Callback should request refresh for correct address');
end;

procedure TDeviceBatteryCoordinatorTests.ScheduleDelayedBatteryRefresh_CallbackSkipsWhenShuttingDown;
begin
  FCoordinator.ScheduleDelayedBatteryRefresh($001, 5000);

  // Shutdown before callback executes
  FCoordinator.Shutdown;

  // Now execute the pending callback
  FAsyncExecutor.ExecutePending;

  Assert.AreEqual(0, FBatteryCache.RequestRefreshCallCount,
    'Callback should NOT call RequestRefresh after shutdown');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceBatteryCoordinatorTests);

end.
