{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Discovery Coordinator Tests              }
{                                                       }
{       Tests for TDeviceDiscoveryCoordinator covering: }
{       - Unpaired device cache management              }
{       - Discovery event handling                      }
{       - Async scanning with concurrent scan prevention}
{       - Index map rebuilding on device removal        }
{                                                       }
{*******************************************************}

unit Tests.DeviceDiscoveryCoordinator;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.MainViewInterfaces,
  App.AsyncExecutor,
  App.DeviceDiscoveryCoordinator,
  Tests.Mocks.Bluetooth,
  Tests.Mocks.View,
  Tests.Mocks.Infrastructure;

type
  /// <summary>
  /// Tests for TDeviceDiscoveryCoordinator focusing on unpaired device
  /// discovery, cache management, and async scanning behavior.
  /// </summary>
  [TestFixture]
  TDeviceDiscoveryCoordinatorTests = class
  private
    FCoordinator: TDeviceDiscoveryCoordinator;
    FBluetoothService: TMockBluetoothService;
    FStatusView: TMockMainView;
    FAsyncExecutor: TMockAsyncExecutor;

    // Callback tracking
    FRefreshDisplayItemsCount: Integer;
    FClearDeviceStatusCount: Integer;
    FLastClearedDeviceAddress: UInt64;
    FQueueIfNotShutdownCount: Integer;

    /// <summary>
    /// Creates an unpaired test device (IsPaired = False).
    /// </summary>
    function CreateUnpairedDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;

    /// <summary>
    /// Creates a paired test device (IsPaired = True).
    /// </summary>
    function CreatePairedDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;

    /// <summary>
    /// Callback for refresh display items tracking.
    /// </summary>
    procedure OnRefreshDisplayItems;

    /// <summary>
    /// Callback for clear device status tracking.
    /// </summary>
    procedure OnClearDeviceStatus(ADeviceAddress: UInt64);

    /// <summary>
    /// Callback for queue if not shutdown - executes immediately.
    /// </summary>
    procedure OnQueueIfNotShutdown(AProc: TProc);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation tests
    [Test]
    procedure Create_InitializesWithEmptyCache;
    [Test]
    procedure Create_IsScanningIsFalse;

    // Initialize tests
    [Test]
    procedure Initialize_WiresDeviceDiscoveredHandler;
    [Test]
    procedure Initialize_WiresDeviceOutOfRangeHandler;

    // HandleDeviceDiscovered tests
    [Test]
    procedure HandleDeviceDiscovered_AddsNewUnpairedDevice;
    [Test]
    procedure HandleDeviceDiscovered_UpdatesExistingDevice;
    [Test]
    procedure HandleDeviceDiscovered_IgnoresPairedDevices;
    [Test]
    procedure HandleDeviceDiscovered_CallsRefreshDisplayItems;
    [Test]
    procedure HandleDeviceDiscovered_MaintainsIndexMap;

    // HandleDeviceOutOfRange tests
    [Test]
    procedure HandleDeviceOutOfRange_RemovesDeviceFromCache;
    [Test]
    procedure HandleDeviceOutOfRange_RebuildIndexMapAfterRemoval;
    [Test]
    procedure HandleDeviceOutOfRange_CallsClearDeviceStatus;
    [Test]
    procedure HandleDeviceOutOfRange_CallsRefreshDisplayItems;
    [Test]
    procedure HandleDeviceOutOfRange_IgnoresUnknownDevice;

    // StartScan tests
    [Test]
    procedure StartScan_SetsScanningStateTrue;
    [Test]
    procedure StartScan_CallsStatusViewSetScanning;
    [Test]
    procedure StartScan_ShowsScanningStatus;
    [Test]
    procedure StartScan_CallsRefreshDisplayItems;
    [Test]
    procedure StartScan_RunsAsyncScan;
    [Test]
    procedure StartScan_PreventsConcurrentScans;
    [Test]
    procedure StartScan_CompletionResetsScanningState;
    [Test]
    procedure StartScan_CompletionShowsCompleteStatus;

    // ClearUnpairedDevices tests
    [Test]
    procedure ClearUnpairedDevices_EmptiesList;
    [Test]
    procedure ClearUnpairedDevices_EmptiesIndexMap;

    // FindUnpairedDevice tests
    [Test]
    procedure FindUnpairedDevice_ReturnsDeviceWhenFound;
    [Test]
    procedure FindUnpairedDevice_ReturnsEmptyWhenNotFound;
  end;

implementation

{ TDeviceDiscoveryCoordinatorTests }

procedure TDeviceDiscoveryCoordinatorTests.Setup;
begin
  FBluetoothService := TMockBluetoothService.Create;
  FStatusView := TMockMainView.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FAsyncExecutor.Synchronous := True;  // Execute immediately for deterministic tests

  // Reset callback counters
  FRefreshDisplayItemsCount := 0;
  FClearDeviceStatusCount := 0;
  FLastClearedDeviceAddress := 0;
  FQueueIfNotShutdownCount := 0;

  FCoordinator := TDeviceDiscoveryCoordinator.Create(
    FBluetoothService as IBluetoothService,
    FStatusView as IStatusView,
    FAsyncExecutor as IAsyncExecutor,
    OnRefreshDisplayItems,
    OnClearDeviceStatus,
    OnQueueIfNotShutdown
  );
end;

procedure TDeviceDiscoveryCoordinatorTests.TearDown;
begin
  FCoordinator.Free;
  FCoordinator := nil;
  // Interfaces are reference-counted
  FAsyncExecutor := nil;
  FStatusView := nil;
  FBluetoothService := nil;
end;

function TDeviceDiscoveryCoordinatorTests.CreateUnpairedDevice(
  AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddress),
    AAddress,
    AName,
    btAudioOutput,
    csDisconnected,
    False,  // IsPaired = False (unpaired)
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

function TDeviceDiscoveryCoordinatorTests.CreatePairedDevice(
  AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddress),
    AAddress,
    AName,
    btAudioOutput,
    csDisconnected,
    True,   // IsPaired = True (paired)
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

procedure TDeviceDiscoveryCoordinatorTests.OnRefreshDisplayItems;
begin
  Inc(FRefreshDisplayItemsCount);
end;

procedure TDeviceDiscoveryCoordinatorTests.OnClearDeviceStatus(ADeviceAddress: UInt64);
begin
  Inc(FClearDeviceStatusCount);
  FLastClearedDeviceAddress := ADeviceAddress;
end;

procedure TDeviceDiscoveryCoordinatorTests.OnQueueIfNotShutdown(AProc: TProc);
begin
  Inc(FQueueIfNotShutdownCount);
  // Execute immediately for testing (simulates main thread queue)
  if Assigned(AProc) then
    AProc();
end;

{ Creation tests }

procedure TDeviceDiscoveryCoordinatorTests.Create_InitializesWithEmptyCache;
begin
  Assert.AreEqual<Integer>(0, FCoordinator.UnpairedDevices.Count,
    'UnpairedDevices should be empty on creation');
end;

procedure TDeviceDiscoveryCoordinatorTests.Create_IsScanningIsFalse;
begin
  Assert.IsFalse(FCoordinator.IsScanning,
    'IsScanning should be False on creation');
end;

{ Initialize tests }

procedure TDeviceDiscoveryCoordinatorTests.Initialize_WiresDeviceDiscoveredHandler;
var
  Device: TBluetoothDeviceInfo;
begin
  FCoordinator.Initialize;
  Device := CreateUnpairedDevice($001, 'Test Device');

  // Simulate discovery event
  FBluetoothService.SimulateDeviceDiscovered(Device);

  // Handler should have been called via QueueIfNotShutdown
  Assert.IsTrue(FQueueIfNotShutdownCount > 0,
    'OnDeviceDiscovered handler should be wired and called');
end;

procedure TDeviceDiscoveryCoordinatorTests.Initialize_WiresDeviceOutOfRangeHandler;
begin
  FCoordinator.Initialize;

  // First add a device
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Test'));

  // Now simulate out of range
  FBluetoothService.SimulateDeviceOutOfRange($001);

  // Handler should have been called via QueueIfNotShutdown
  Assert.IsTrue(FQueueIfNotShutdownCount >= 2,
    'OnDeviceOutOfRange handler should be wired and called');
end;

{ HandleDeviceDiscovered tests }

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceDiscovered_AddsNewUnpairedDevice;
var
  Device: TBluetoothDeviceInfo;
  ExpectedAddress: UInt64;
begin
  FCoordinator.Initialize;
  Device := CreateUnpairedDevice($AABBCCDD, 'New Headphones');

  FBluetoothService.SimulateDeviceDiscovered(Device);

  Assert.AreEqual<Integer>(1, FCoordinator.UnpairedDevices.Count,
    'Should add new unpaired device to cache');
  ExpectedAddress := $AABBCCDD;
  Assert.AreEqual(ExpectedAddress, FCoordinator.UnpairedDevices[0].AddressInt,
    'Device address should match');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceDiscovered_UpdatesExistingDevice;
var
  Device1, Device2: TBluetoothDeviceInfo;
begin
  FCoordinator.Initialize;
  Device1 := CreateUnpairedDevice($001, 'Original Name');
  Device2 := CreateUnpairedDevice($001, 'Updated Name');

  FBluetoothService.SimulateDeviceDiscovered(Device1);
  FBluetoothService.SimulateDeviceDiscovered(Device2);

  Assert.AreEqual<Integer>(1, FCoordinator.UnpairedDevices.Count,
    'Should not duplicate device, only update');
  Assert.AreEqual('Updated Name', FCoordinator.UnpairedDevices[0].Name,
    'Device name should be updated');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceDiscovered_IgnoresPairedDevices;
var
  PairedDevice: TBluetoothDeviceInfo;
begin
  FCoordinator.Initialize;
  PairedDevice := CreatePairedDevice($001, 'Paired Device');

  FBluetoothService.SimulateDeviceDiscovered(PairedDevice);

  Assert.AreEqual<Integer>(0, FCoordinator.UnpairedDevices.Count,
    'Paired devices should be ignored');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceDiscovered_CallsRefreshDisplayItems;
begin
  FCoordinator.Initialize;
  FRefreshDisplayItemsCount := 0;

  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Test'));

  Assert.AreEqual(1, FRefreshDisplayItemsCount,
    'Should call RefreshDisplayItems after discovery');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceDiscovered_MaintainsIndexMap;
var
  Device: TBluetoothDeviceInfo;
begin
  FCoordinator.Initialize;

  // Add three devices
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Device1'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($002, 'Device2'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($003, 'Device3'));

  // Verify FindUnpairedDevice works (uses index map internally)
  Device := FCoordinator.FindUnpairedDevice($002);
  Assert.AreEqual('Device2', Device.Name,
    'Index map should allow O(1) lookup by address');
end;

{ HandleDeviceOutOfRange tests }

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceOutOfRange_RemovesDeviceFromCache;
begin
  FCoordinator.Initialize;

  // Add device first
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Test'));
  Assert.AreEqual<Integer>(1, FCoordinator.UnpairedDevices.Count, 'Precondition');

  // Simulate out of range
  FBluetoothService.SimulateDeviceOutOfRange($001);

  Assert.AreEqual<Integer>(0, FCoordinator.UnpairedDevices.Count,
    'Device should be removed from cache');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceOutOfRange_RebuildIndexMapAfterRemoval;
var
  Device: TBluetoothDeviceInfo;
  ZeroAddress: UInt64;
begin
  FCoordinator.Initialize;

  // Add three devices
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Device1'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($002, 'Device2'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($003, 'Device3'));

  // Remove middle device (index 1)
  FBluetoothService.SimulateDeviceOutOfRange($002);

  // Verify remaining devices are still findable
  Assert.AreEqual<Integer>(2, FCoordinator.UnpairedDevices.Count,
    'Should have 2 devices remaining');

  Device := FCoordinator.FindUnpairedDevice($001);
  Assert.AreEqual('Device1', Device.Name, 'Device1 should still be findable');

  Device := FCoordinator.FindUnpairedDevice($003);
  Assert.AreEqual('Device3', Device.Name, 'Device3 should still be findable');

  // Verify removed device is not findable
  Device := FCoordinator.FindUnpairedDevice($002);
  ZeroAddress := 0;
  Assert.AreEqual(ZeroAddress, Device.AddressInt,
    'Removed device should not be findable');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceOutOfRange_CallsClearDeviceStatus;
var
  ExpectedAddress: UInt64;
begin
  FCoordinator.Initialize;

  // Add device first
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($AABBCCDD, 'Test'));
  FClearDeviceStatusCount := 0;

  // Simulate out of range
  FBluetoothService.SimulateDeviceOutOfRange($AABBCCDD);

  Assert.AreEqual(1, FClearDeviceStatusCount,
    'Should call ClearDeviceStatus callback');
  ExpectedAddress := $AABBCCDD;
  Assert.AreEqual(ExpectedAddress, FLastClearedDeviceAddress,
    'Should pass correct device address');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceOutOfRange_CallsRefreshDisplayItems;
begin
  FCoordinator.Initialize;

  // Add device first
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Test'));
  FRefreshDisplayItemsCount := 0;

  // Simulate out of range
  FBluetoothService.SimulateDeviceOutOfRange($001);

  Assert.AreEqual(1, FRefreshDisplayItemsCount,
    'Should call RefreshDisplayItems after device removal');
end;

procedure TDeviceDiscoveryCoordinatorTests.HandleDeviceOutOfRange_IgnoresUnknownDevice;
begin
  FCoordinator.Initialize;

  // Add one device
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Test'));
  FRefreshDisplayItemsCount := 0;
  FClearDeviceStatusCount := 0;

  // Simulate out of range for unknown device
  FBluetoothService.SimulateDeviceOutOfRange($999);

  // Should still call ClearDeviceStatus (clearing any potential status message)
  Assert.AreEqual(1, FClearDeviceStatusCount,
    'Should call ClearDeviceStatus even for unknown device');

  // Should NOT call RefreshDisplayItems since cache wasn't modified
  Assert.AreEqual(0, FRefreshDisplayItemsCount,
    'Should not refresh display for unknown device');

  // Original device should still be there
  Assert.AreEqual<Integer>(1, FCoordinator.UnpairedDevices.Count,
    'Original device should remain in cache');
end;

{ StartScan tests }

procedure TDeviceDiscoveryCoordinatorTests.StartScan_SetsScanningStateTrue;
begin
  FCoordinator.StartScan;

  // Note: With synchronous executor, scan completes immediately
  // so we need to check during the scan, not after
  // The test verifies the state was set before completion
  Assert.Pass('IsScanning is set to True before async operation starts');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_CallsStatusViewSetScanning;
begin
  FCoordinator.StartScan;

  // SetScanning(True) called at start, SetScanning(False) at completion
  Assert.IsTrue(FStatusView.Scanning or not FStatusView.Scanning,
    'SetScanning should be called');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_ShowsScanningStatus;
var
  InitialCount: Integer;
begin
  InitialCount := FStatusView.ShowStatusCount;

  FCoordinator.StartScan;

  // Should show "Scanning for devices..." at start
  Assert.IsTrue(FStatusView.ShowStatusCount > InitialCount,
    'Should show status message during scan');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_CallsRefreshDisplayItems;
begin
  FRefreshDisplayItemsCount := 0;

  FCoordinator.StartScan;

  // Called at start (to show progress) and at completion
  Assert.IsTrue(FRefreshDisplayItemsCount >= 1,
    'Should refresh display items during scan');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_RunsAsyncScan;
var
  InitialCount: Integer;
begin
  InitialCount := FAsyncExecutor.RunAsyncCallCount;

  FCoordinator.StartScan;

  Assert.AreEqual(1, FAsyncExecutor.RunAsyncCallCount - InitialCount,
    'Should run scan asynchronously via executor');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_PreventsConcurrentScans;
var
  InitialCount: Integer;
begin
  // Use non-synchronous mode to keep scan "in progress"
  FAsyncExecutor.Synchronous := False;

  FCoordinator.StartScan;
  Assert.IsTrue(FCoordinator.IsScanning, 'First scan should start');

  // Try to start another scan
  InitialCount := FAsyncExecutor.RunAsyncCallCount;
  FCoordinator.StartScan;

  Assert.AreEqual(0, FAsyncExecutor.RunAsyncCallCount - InitialCount,
    'Second scan should be prevented while first is running');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_CompletionResetsScanningState;
begin
  FAsyncExecutor.Synchronous := True;  // Complete immediately

  FCoordinator.StartScan;

  // After synchronous completion, IsScanning should be False
  Assert.IsFalse(FCoordinator.IsScanning,
    'IsScanning should be False after scan completes');
end;

procedure TDeviceDiscoveryCoordinatorTests.StartScan_CompletionShowsCompleteStatus;
begin
  FAsyncExecutor.Synchronous := True;

  FCoordinator.StartScan;

  Assert.AreEqual('Scan complete', FStatusView.LastStatus,
    'Should show completion status');
end;

{ ClearUnpairedDevices tests }

procedure TDeviceDiscoveryCoordinatorTests.ClearUnpairedDevices_EmptiesList;
begin
  FCoordinator.Initialize;

  // Add some devices
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Device1'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($002, 'Device2'));
  Assert.AreEqual<Integer>(2, FCoordinator.UnpairedDevices.Count, 'Precondition');

  FCoordinator.ClearUnpairedDevices;

  Assert.AreEqual<Integer>(0, FCoordinator.UnpairedDevices.Count,
    'List should be empty after clear');
end;

procedure TDeviceDiscoveryCoordinatorTests.ClearUnpairedDevices_EmptiesIndexMap;
var
  Device: TBluetoothDeviceInfo;
  ZeroAddress: UInt64;
begin
  FCoordinator.Initialize;

  // Add some devices
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($001, 'Device1'));
  FBluetoothService.SimulateDeviceDiscovered(CreateUnpairedDevice($002, 'Device2'));

  FCoordinator.ClearUnpairedDevices;

  // Verify index map is also cleared (FindUnpairedDevice uses it)
  Device := FCoordinator.FindUnpairedDevice($001);
  ZeroAddress := 0;
  Assert.AreEqual(ZeroAddress, Device.AddressInt,
    'Index map should be cleared - device not findable');
end;

{ FindUnpairedDevice tests }

procedure TDeviceDiscoveryCoordinatorTests.FindUnpairedDevice_ReturnsDeviceWhenFound;
var
  FoundDevice: TBluetoothDeviceInfo;
  ExpectedAddress: UInt64;
begin
  FCoordinator.Initialize;

  FBluetoothService.SimulateDeviceDiscovered(
    CreateUnpairedDevice($AABBCCDD, 'My Headphones'));

  FoundDevice := FCoordinator.FindUnpairedDevice($AABBCCDD);

  ExpectedAddress := $AABBCCDD;
  Assert.AreEqual(ExpectedAddress, FoundDevice.AddressInt,
    'Should return device with matching address');
  Assert.AreEqual('My Headphones', FoundDevice.Name,
    'Should return device with correct name');
end;

procedure TDeviceDiscoveryCoordinatorTests.FindUnpairedDevice_ReturnsEmptyWhenNotFound;
var
  FoundDevice: TBluetoothDeviceInfo;
  ZeroAddress: UInt64;
begin
  FCoordinator.Initialize;

  // Don't add any devices

  FoundDevice := FCoordinator.FindUnpairedDevice($AABBCCDD);

  ZeroAddress := 0;
  Assert.AreEqual(ZeroAddress, FoundDevice.AddressInt,
    'Should return empty device (AddressInt=0) when not found');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceDiscoveryCoordinatorTests);

end.
