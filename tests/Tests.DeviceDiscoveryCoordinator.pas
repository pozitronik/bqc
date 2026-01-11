{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Discovery Coordinator Tests              }
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
  App.DeviceDiscoveryCoordinator,
  App.AsyncExecutor,
  Tests.Mocks,
  Tests.Mocks.Bluetooth,
  Tests.Mocks.View;

type
  /// <summary>
  /// Tests for TDeviceDiscoveryCoordinator focusing on
  /// unpaired device list management and index map correctness.
  /// Tests verify behavior via FindUnpairedDevice and UnpairedDevices.Count
  /// rather than accessing internal state directly.
  /// </summary>
  [TestFixture]
  TDeviceDiscoveryCoordinatorTests = class
  private
    FCoordinator: TDeviceDiscoveryCoordinator;
    FBluetoothService: TMockBluetoothService;
    FStatusView: TMockMainView;
    FAsyncExecutor: TSynchronousExecutor;
    FRefreshDisplayItemsCallCount: Integer;
    FClearDeviceStatusCallCount: Integer;
    FLastClearedAddress: UInt64;

    function CreateTestDevice(AAddress: UInt64; const AName: string;
      AIsPaired: Boolean = False): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation test
    [Test]
    procedure Create_InitializesCorrectly;

    // Basic functionality tests
    [Test]
    procedure DeviceDiscovered_UnpairedDevice_AddsToList;
    [Test]
    procedure DeviceDiscovered_PairedDevice_IgnoresDevice;
    [Test]
    procedure DeviceDiscovered_DuplicateDevice_UpdatesExisting;

    // Device out of range tests
    [Test]
    procedure DeviceOutOfRange_ExistingDevice_RemovesFromList;
    [Test]
    procedure DeviceOutOfRange_NonExistingDevice_NoChange;
    [Test]
    procedure DeviceOutOfRange_CallsClearDeviceStatus;

    // Index map correctness tests (critical for optimization)
    [Test]
    procedure DeviceOutOfRange_RemoveFirst_UpdatesAllIndices;
    [Test]
    procedure DeviceOutOfRange_RemoveMiddle_UpdatesSubsequentIndices;
    [Test]
    procedure DeviceOutOfRange_RemoveLast_NoIndexShift;
    [Test]
    procedure DeviceOutOfRange_MultipleRemovals_IndicesStayCorrect;

    // FindUnpairedDevice tests
    [Test]
    procedure FindUnpairedDevice_ExistingDevice_ReturnsDevice;
    [Test]
    procedure FindUnpairedDevice_NonExistingDevice_ReturnsEmpty;

    // Clear tests
    [Test]
    procedure ClearUnpairedDevices_ClearsBothListAndMap;
  end;

implementation

uses
  App.MainViewInterfaces;

{ TDeviceDiscoveryCoordinatorTests }

procedure TDeviceDiscoveryCoordinatorTests.Setup;
var
  RefreshDisplayItems: TRefreshDisplayItemsProc;
  ClearDeviceStatus: TClearDeviceStatusProc;
  QueueIfNotShutdown: TQueueIfNotShutdownProc;
begin
  FBluetoothService := TMockBluetoothService.Create;
  FStatusView := TMockMainView.Create;
  FAsyncExecutor := TSynchronousExecutor.Create;
  FRefreshDisplayItemsCallCount := 0;
  FClearDeviceStatusCallCount := 0;
  FLastClearedAddress := 0;

  // Capture Self explicitly in local variables to avoid closure issues
  RefreshDisplayItems :=
    procedure
    begin
      Inc(Self.FRefreshDisplayItemsCallCount);
    end;

  ClearDeviceStatus :=
    procedure(AAddress: UInt64)
    begin
      Inc(Self.FClearDeviceStatusCallCount);
      Self.FLastClearedAddress := AAddress;
    end;

  QueueIfNotShutdown :=
    procedure(AProc: TProc)
    begin
      // Execute immediately for synchronous testing
      if Assigned(AProc) then
        AProc();
    end;

  FCoordinator := TDeviceDiscoveryCoordinator.Create(
    FBluetoothService,
    FStatusView as IStatusView,
    FAsyncExecutor,
    RefreshDisplayItems,
    ClearDeviceStatus,
    QueueIfNotShutdown
  );

  // Wire up event handlers
  FCoordinator.Initialize;
end;

procedure TDeviceDiscoveryCoordinatorTests.TearDown;
begin
  // Free coordinator first - it holds interface references to the mocks
  FCoordinator.Free;
  FCoordinator := nil;

  // The mocks are TInterfacedObject descendants. After coordinator is freed,
  // the interface references are released, but since we created the objects
  // directly (not via interface variables), we still need to manage them.
  // However, the interface release may have already freed them if refcount hit 0.
  // To be safe, we nil the fields without freeing - the coordinator's interface
  // references were the only things keeping them alive.
  FAsyncExecutor := nil;
  FStatusView := nil;
  FBluetoothService := nil;
end;

function TDeviceDiscoveryCoordinatorTests.CreateTestDevice(AAddress: UInt64;
  const AName: string; AIsPaired: Boolean): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address[0] := AAddress and $FF;
  Address[1] := (AAddress shr 8) and $FF;
  Address[2] := (AAddress shr 16) and $FF;
  Address[3] := (AAddress shr 24) and $FF;
  Address[4] := (AAddress shr 32) and $FF;
  Address[5] := (AAddress shr 40) and $FF;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AAddress,
    AName,
    btAudioOutput,
    csDisconnected,
    AIsPaired,  // IsPaired
    True,       // IsRemembered
    0,          // BatteryLevel
    Now,        // LastSeen
    Now         // LastConnected
  );
end;

{ Creation test }

procedure TDeviceDiscoveryCoordinatorTests.Create_InitializesCorrectly;
begin
  // Verify coordinator was created
  Assert.IsNotNull(FCoordinator, 'Coordinator should be created');

  // Verify UnpairedDevices list is accessible
  Assert.IsNotNull(FCoordinator.UnpairedDevices, 'UnpairedDevices should not be nil');

  // Verify initial state
  Assert.AreEqual(0, Integer(FCoordinator.UnpairedDevices.Count),
    'Initial list should be empty');

  Assert.IsFalse(FCoordinator.IsScanning, 'Should not be scanning initially');
end;

{ Basic functionality tests }

procedure TDeviceDiscoveryCoordinatorTests.DeviceDiscovered_UnpairedDevice_AddsToList;
var
  Device: TBluetoothDeviceInfo;
begin
  // First verify basic state
  Assert.AreEqual(0, Integer(FCoordinator.UnpairedDevices.Count),
    'Initial list should be empty');

  Device := CreateTestDevice($AABBCCDDEEFF, 'TestHeadphones', False);

  // Trigger the event through the mock service
  FBluetoothService.SimulateDeviceDiscovered(Device);

  Assert.AreEqual(1, Integer(FCoordinator.UnpairedDevices.Count),
    'Unpaired device should be added to list');
  Assert.AreEqual('TestHeadphones', FCoordinator.UnpairedDevices[0].Name);
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceDiscovered_PairedDevice_IgnoresDevice;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'PairedDevice', True);

  FBluetoothService.SimulateDeviceDiscovered(Device);

  Assert.AreEqual(0, Integer(FCoordinator.UnpairedDevices.Count),
    'Paired device should NOT be added to unpaired list');
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceDiscovered_DuplicateDevice_UpdatesExisting;
var
  Device1, Device2: TBluetoothDeviceInfo;
begin
  Device1 := CreateTestDevice($AABBCCDDEEFF, 'OldName', False);
  Device2 := CreateTestDevice($AABBCCDDEEFF, 'NewName', False);

  FBluetoothService.SimulateDeviceDiscovered(Device1);
  Assert.AreEqual(1, Integer(FCoordinator.UnpairedDevices.Count), 'First discovery');

  FBluetoothService.SimulateDeviceDiscovered(Device2);

  Assert.AreEqual(1, Integer(FCoordinator.UnpairedDevices.Count),
    'Should still have 1 device after update');
  Assert.AreEqual('NewName', FCoordinator.UnpairedDevices[0].Name,
    'Device name should be updated');
end;

{ Device out of range tests }

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_ExistingDevice_RemovesFromList;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', False);
  FBluetoothService.SimulateDeviceDiscovered(Device);
  Assert.AreEqual(1, Integer(FCoordinator.UnpairedDevices.Count), 'Precondition: device added');

  FBluetoothService.SimulateDeviceOutOfRange($AABBCCDDEEFF);

  Assert.AreEqual(0, Integer(FCoordinator.UnpairedDevices.Count),
    'Device should be removed from list');
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_NonExistingDevice_NoChange;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', False);
  FBluetoothService.SimulateDeviceDiscovered(Device);

  // Try to remove a different device
  FBluetoothService.SimulateDeviceOutOfRange($111111111111);

  Assert.AreEqual(1, Integer(FCoordinator.UnpairedDevices.Count),
    'Original device should remain in list');
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_CallsClearDeviceStatus;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', False);
  FBluetoothService.SimulateDeviceDiscovered(Device);

  FBluetoothService.SimulateDeviceOutOfRange($AABBCCDDEEFF);

  Assert.AreEqual(1, FClearDeviceStatusCallCount,
    'ClearDeviceStatus callback should be called once');
  Assert.AreEqual(UInt64($AABBCCDDEEFF), FLastClearedAddress,
    'ClearDeviceStatus should receive correct address');
end;

{ Index map correctness tests - CRITICAL for optimization }

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_RemoveFirst_UpdatesAllIndices;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  FoundDevice: TBluetoothDeviceInfo;
begin
  // Add 3 devices: indices 0, 1, 2
  Device1 := CreateTestDevice($111111111111, 'Device1', False);
  Device2 := CreateTestDevice($222222222222, 'Device2', False);
  Device3 := CreateTestDevice($333333333333, 'Device3', False);

  FBluetoothService.SimulateDeviceDiscovered(Device1);  // Index 0
  FBluetoothService.SimulateDeviceDiscovered(Device2);  // Index 1
  FBluetoothService.SimulateDeviceDiscovered(Device3);  // Index 2

  Assert.AreEqual(3, Integer(FCoordinator.UnpairedDevices.Count), 'Precondition: 3 devices');

  // Remove first device (index 0) - all remaining should shift
  FBluetoothService.SimulateDeviceOutOfRange($111111111111);

  Assert.AreEqual(2, Integer(FCoordinator.UnpairedDevices.Count), 'Should have 2 devices');

  // Verify Device2 is findable via index map
  FoundDevice := FCoordinator.FindUnpairedDevice($222222222222);
  Assert.AreEqual('Device2', FoundDevice.Name,
    'Device2 should be findable after removal of first');

  // Verify Device3 is findable via index map
  FoundDevice := FCoordinator.FindUnpairedDevice($333333333333);
  Assert.AreEqual('Device3', FoundDevice.Name,
    'Device3 should be findable after removal of first');

  // Verify list order
  Assert.AreEqual('Device2', FCoordinator.UnpairedDevices[0].Name,
    'Device2 should now be at index 0');
  Assert.AreEqual('Device3', FCoordinator.UnpairedDevices[1].Name,
    'Device3 should now be at index 1');
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_RemoveMiddle_UpdatesSubsequentIndices;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  FoundDevice: TBluetoothDeviceInfo;
begin
  // Add 3 devices
  Device1 := CreateTestDevice($111111111111, 'Device1', False);
  Device2 := CreateTestDevice($222222222222, 'Device2', False);
  Device3 := CreateTestDevice($333333333333, 'Device3', False);

  FBluetoothService.SimulateDeviceDiscovered(Device1);  // Index 0
  FBluetoothService.SimulateDeviceDiscovered(Device2);  // Index 1
  FBluetoothService.SimulateDeviceDiscovered(Device3);  // Index 2

  // Remove middle device (index 1)
  FBluetoothService.SimulateDeviceOutOfRange($222222222222);

  Assert.AreEqual(2, Integer(FCoordinator.UnpairedDevices.Count), 'Should have 2 devices');

  // Device1 should still be at index 0 (unchanged) and findable
  FoundDevice := FCoordinator.FindUnpairedDevice($111111111111);
  Assert.AreEqual('Device1', FoundDevice.Name,
    'Device1 should be findable (index unchanged)');
  Assert.AreEqual('Device1', FCoordinator.UnpairedDevices[0].Name,
    'Device1 should still be at index 0');

  // Device3 should now be at index 1 (shifted down) and findable
  FoundDevice := FCoordinator.FindUnpairedDevice($333333333333);
  Assert.AreEqual('Device3', FoundDevice.Name,
    'Device3 should be findable after middle removal');
  Assert.AreEqual('Device3', FCoordinator.UnpairedDevices[1].Name,
    'Device3 should now be at index 1');
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_RemoveLast_NoIndexShift;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  FoundDevice: TBluetoothDeviceInfo;
begin
  // Add 3 devices
  Device1 := CreateTestDevice($111111111111, 'Device1', False);
  Device2 := CreateTestDevice($222222222222, 'Device2', False);
  Device3 := CreateTestDevice($333333333333, 'Device3', False);

  FBluetoothService.SimulateDeviceDiscovered(Device1);  // Index 0
  FBluetoothService.SimulateDeviceDiscovered(Device2);  // Index 1
  FBluetoothService.SimulateDeviceDiscovered(Device3);  // Index 2

  // Remove last device (index 2) - no shifting needed for others
  FBluetoothService.SimulateDeviceOutOfRange($333333333333);

  Assert.AreEqual(2, Integer(FCoordinator.UnpairedDevices.Count), 'Should have 2 devices');

  // Device1 should still be at index 0 and findable
  FoundDevice := FCoordinator.FindUnpairedDevice($111111111111);
  Assert.AreEqual('Device1', FoundDevice.Name);
  Assert.AreEqual('Device1', FCoordinator.UnpairedDevices[0].Name);

  // Device2 should still be at index 1 and findable
  FoundDevice := FCoordinator.FindUnpairedDevice($222222222222);
  Assert.AreEqual('Device2', FoundDevice.Name);
  Assert.AreEqual('Device2', FCoordinator.UnpairedDevices[1].Name);
end;

procedure TDeviceDiscoveryCoordinatorTests.DeviceOutOfRange_MultipleRemovals_IndicesStayCorrect;
var
  Devices: array[0..4] of TBluetoothDeviceInfo;
  I: Integer;
  FoundDevice: TBluetoothDeviceInfo;
begin
  // Add 5 devices
  for I := 0 to 4 do
  begin
    Devices[I] := CreateTestDevice(UInt64(I + 1) * $111111111111, 'Device' + IntToStr(I + 1), False);
    FBluetoothService.SimulateDeviceDiscovered(Devices[I]);
  end;

  Assert.AreEqual(5, Integer(FCoordinator.UnpairedDevices.Count), 'Precondition: 5 devices');

  // Remove devices in various order: 2nd, 4th, 1st
  FBluetoothService.SimulateDeviceOutOfRange($222222222222);  // Remove Device2
  Assert.AreEqual(4, Integer(FCoordinator.UnpairedDevices.Count), 'After removing Device2');

  FBluetoothService.SimulateDeviceOutOfRange($444444444444);  // Remove Device4
  Assert.AreEqual(3, Integer(FCoordinator.UnpairedDevices.Count), 'After removing Device4');

  FBluetoothService.SimulateDeviceOutOfRange($111111111111);  // Remove Device1
  Assert.AreEqual(2, Integer(FCoordinator.UnpairedDevices.Count), 'After removing Device1');

  // Verify remaining devices (Device3 and Device5) are findable
  FoundDevice := FCoordinator.FindUnpairedDevice($333333333333);
  Assert.AreEqual('Device3', FoundDevice.Name, 'Device3 should be findable');

  FoundDevice := FCoordinator.FindUnpairedDevice($555555555555);
  Assert.AreEqual('Device5', FoundDevice.Name, 'Device5 should be findable');

  // Verify removed devices are NOT findable (return empty)
  FoundDevice := FCoordinator.FindUnpairedDevice($111111111111);
  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt, 'Device1 should not be found');

  FoundDevice := FCoordinator.FindUnpairedDevice($222222222222);
  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt, 'Device2 should not be found');

  FoundDevice := FCoordinator.FindUnpairedDevice($444444444444);
  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt, 'Device4 should not be found');
end;

{ FindUnpairedDevice tests }

procedure TDeviceDiscoveryCoordinatorTests.FindUnpairedDevice_ExistingDevice_ReturnsDevice;
var
  Device: TBluetoothDeviceInfo;
  FoundDevice: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice', False);
  FBluetoothService.SimulateDeviceDiscovered(Device);

  FoundDevice := FCoordinator.FindUnpairedDevice($AABBCCDDEEFF);

  Assert.AreEqual('TestDevice', FoundDevice.Name);
  Assert.AreEqual(UInt64($AABBCCDDEEFF), FoundDevice.AddressInt);
end;

procedure TDeviceDiscoveryCoordinatorTests.FindUnpairedDevice_NonExistingDevice_ReturnsEmpty;
var
  FoundDevice: TBluetoothDeviceInfo;
begin
  FoundDevice := FCoordinator.FindUnpairedDevice($AABBCCDDEEFF);

  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt,
    'Non-existing device should return empty record with AddressInt=0');
end;

{ Clear tests }

procedure TDeviceDiscoveryCoordinatorTests.ClearUnpairedDevices_ClearsBothListAndMap;
var
  Device1, Device2: TBluetoothDeviceInfo;
  FoundDevice: TBluetoothDeviceInfo;
begin
  Device1 := CreateTestDevice($111111111111, 'Device1', False);
  Device2 := CreateTestDevice($222222222222, 'Device2', False);

  FBluetoothService.SimulateDeviceDiscovered(Device1);
  FBluetoothService.SimulateDeviceDiscovered(Device2);
  Assert.AreEqual(2, Integer(FCoordinator.UnpairedDevices.Count), 'Precondition: 2 devices');

  FCoordinator.ClearUnpairedDevices;

  Assert.AreEqual(0, Integer(FCoordinator.UnpairedDevices.Count),
    'List should be empty after clear');

  // Verify index map is also cleared (FindUnpairedDevice should return empty)
  FoundDevice := FCoordinator.FindUnpairedDevice($111111111111);
  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt,
    'Device1 should not be findable after clear');

  FoundDevice := FCoordinator.FindUnpairedDevice($222222222222);
  Assert.AreEqual(UInt64(0), FoundDevice.AddressInt,
    'Device2 should not be findable after clear');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceDiscoveryCoordinatorTests);

end.
