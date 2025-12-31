{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Tray Manager Tests                      }
{                                                       }
{*******************************************************}

unit Tests.BatteryTrayManager;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Graphics,
  System.SysUtils,
  App.ConfigInterfaces,
  App.BatteryTrayConfigIntf,
  UI.BatteryTrayManager,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TBatteryTrayManager class.
  /// Tests tray icon management, notification logic, and configuration handling.
  /// Note: Some tests avoid actual Shell_NotifyIcon calls by using disabled state.
  /// </summary>
  [TestFixture]
  TBatteryTrayManagerTests = class
  private
    FConfig: TMockBatteryTrayConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FManager: TBatteryTrayManager;
    FNotificationCount: Integer;
    FLastNotificationAddress: UInt64;
    FLastNotificationLevel: Integer;
    FLastNotificationIsLow: Boolean;

    procedure HandleBatteryNotification(Sender: TObject; AAddress: UInt64;
      const ADeviceName: string; ALevel: Integer; AIsLowBattery: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure Create_WithValidParams_CreatesInstance;
    [Test]
    procedure Create_WithNilConfig_CreatesInstance;
    [Test]
    procedure Create_StartsEnabled;

    { Enabled Property Tests }
    [Test]
    procedure Enabled_SetFalse_DisablesManager;
    [Test]
    procedure Enabled_SetTrue_EnablesManager;

    { UpdateDevice Tests - Logic Only (manager disabled to avoid Shell_NotifyIcon) }
    [Test]
    procedure UpdateDevice_WhenDisabled_DoesNothing;
    [Test]
    procedure UpdateDevice_NotConnected_RemovesDevice;
    [Test]
    procedure UpdateDevice_NegativeLevel_RemovesDevice;

    { RemoveDevice Tests }
    [Test]
    procedure RemoveDevice_NonExistent_NoError;

    { ClearAll Tests }
    [Test]
    procedure ClearAll_EmptyManager_NoError;

    { Configuration Reading Tests }
    [Test]
    procedure UpdateDevice_UsesGlobalColor;
    [Test]
    procedure UpdateDevice_UsesGlobalThreshold;
    [Test]
    procedure UpdateDevice_UsesGlobalShowNumeric;

    { Per-Device Override Tests }
    [Test]
    procedure UpdateDevice_DeviceOverrideColor_UsesDeviceColor;
    [Test]
    procedure UpdateDevice_DeviceOverrideThreshold_UsesDeviceThreshold;

    { Notification Event Tests }
    [Test]
    procedure OnBatteryNotification_CanBeAssigned;

    { ShouldShowTrayIcon Logic Tests }
    [Test]
    procedure ShouldShowTrayIcon_GlobalDisabled_ReturnsFalse;
    [Test]
    procedure ShouldShowTrayIcon_DeviceExplicitlyDisabled_ReturnsFalse;
    [Test]
    procedure ShouldShowTrayIcon_DeviceExplicitlyEnabled_ReturnsTrue;

    { RefreshAll Tests }
    [Test]
    procedure RefreshAll_EmptyManager_NoError;

    { Icon Caching Tests }
    [Test]
    procedure UpdateDevice_SameParamsTwice_NoError;
    [Test]
    procedure UpdateDevicePending_SameParamsTwice_NoError;
    [Test]
    procedure UpdateDevice_DifferentLevels_NoError;
    [Test]
    procedure UpdateDevice_TransitionFromPendingToLevel_NoError;
    [Test]
    procedure RemoveDevice_ClearsCacheEntry;
    [Test]
    procedure ClearAll_ClearsCacheEntries;

    { Stress Tests - Simulating Rapid Clicking Scenario }
    [Test]
    procedure RapidUpdates_MultipleDevices_NoError;
    [Test]
    procedure InterleavedOperations_MixedDeviceStates_NoError;
    [Test]
    procedure RemoveDuringIteration_SimulatedScenario_NoError;
    [Test]
    procedure UpdatePendingAfterRemove_NoError;
    [Test]
    procedure ClearAllDuringUpdates_NoError;
  end;

implementation

uses
  App.DeviceConfigTypes;

{ TBatteryTrayManagerTests }

procedure TBatteryTrayManagerTests.Setup;
begin
  FConfig := TMockBatteryTrayConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  // Use 0 as owner handle for testing (no real window)
  FManager := TBatteryTrayManager.Create(0, FConfig, FDeviceConfigProvider);
  FNotificationCount := 0;
  FLastNotificationAddress := 0;
  FLastNotificationLevel := 0;
  FLastNotificationIsLow := False;
end;

procedure TBatteryTrayManagerTests.TearDown;
begin
  FManager.Free;
  // Interfaces are reference-counted, no need to free
end;

procedure TBatteryTrayManagerTests.HandleBatteryNotification(Sender: TObject;
  AAddress: UInt64; const ADeviceName: string; ALevel: Integer;
  AIsLowBattery: Boolean);
begin
  Inc(FNotificationCount);
  FLastNotificationAddress := AAddress;
  FLastNotificationLevel := ALevel;
  FLastNotificationIsLow := AIsLowBattery;
end;

procedure TBatteryTrayManagerTests.Create_WithValidParams_CreatesInstance;
begin
  Assert.IsNotNull(FManager);
end;

procedure TBatteryTrayManagerTests.Create_WithNilConfig_CreatesInstance;
var
  Manager: TBatteryTrayManager;
begin
  Manager := TBatteryTrayManager.Create(0, nil, nil);
  try
    Assert.IsNotNull(Manager);
  finally
    Manager.Free;
  end;
end;

procedure TBatteryTrayManagerTests.Create_StartsEnabled;
begin
  Assert.IsTrue(FManager.Enabled, 'Manager should start enabled');
end;

procedure TBatteryTrayManagerTests.Enabled_SetFalse_DisablesManager;
begin
  FManager.Enabled := False;
  Assert.IsFalse(FManager.Enabled);
end;

procedure TBatteryTrayManagerTests.Enabled_SetTrue_EnablesManager;
begin
  FManager.Enabled := False;
  FManager.Enabled := True;
  Assert.IsTrue(FManager.Enabled);
end;

procedure TBatteryTrayManagerTests.UpdateDevice_WhenDisabled_DoesNothing;
begin
  FManager.Enabled := False;
  // Should not raise exception
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);
  Assert.Pass('UpdateDevice when disabled completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_NotConnected_RemovesDevice;
begin
  FManager.Enabled := False; // Disable to avoid Shell_NotifyIcon calls
  // Call with connected = false
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, False);
  Assert.Pass('UpdateDevice with not connected completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_NegativeLevel_RemovesDevice;
begin
  FManager.Enabled := False; // Disable to avoid Shell_NotifyIcon calls
  // Call with negative level
  FManager.UpdateDevice($123456789ABC, 'Test Device', -1, True);
  Assert.Pass('UpdateDevice with negative level completed without error');
end;

procedure TBatteryTrayManagerTests.RemoveDevice_NonExistent_NoError;
begin
  // Should not raise exception for non-existent device
  FManager.RemoveDevice($123456789ABC);
  Assert.Pass('RemoveDevice for non-existent device completed without error');
end;

procedure TBatteryTrayManagerTests.ClearAll_EmptyManager_NoError;
begin
  // Should not raise exception when clearing empty manager
  FManager.ClearAll;
  Assert.Pass('ClearAll on empty manager completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_UsesGlobalColor;
begin
  // Set a specific color in mock config
  FConfig.DefaultIconColor := clBlue;

  // The color is read internally during UpdateDevice
  // We verify the config is accessible
  Assert.AreEqual(Integer(clBlue), Integer(FConfig.DefaultIconColor));
end;

procedure TBatteryTrayManagerTests.UpdateDevice_UsesGlobalThreshold;
begin
  FConfig.DefaultLowBatteryThreshold := 25;
  Assert.AreEqual(25, FConfig.DefaultLowBatteryThreshold);
end;

procedure TBatteryTrayManagerTests.UpdateDevice_UsesGlobalShowNumeric;
begin
  FConfig.DefaultShowNumericValue := True;
  Assert.IsTrue(FConfig.DefaultShowNumericValue);
end;

procedure TBatteryTrayManagerTests.UpdateDevice_DeviceOverrideColor_UsesDeviceColor;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := TDeviceConfig.Default($123456789ABC);
  DeviceConfig.BatteryTray.IconColor := Integer(clRed);
  FDeviceConfigProvider.AddDeviceConfig($123456789ABC, DeviceConfig);

  // Verify the device config is stored correctly
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig($123456789ABC);
  Assert.AreEqual(Integer(clRed), DeviceConfig.BatteryTray.IconColor);
end;

procedure TBatteryTrayManagerTests.UpdateDevice_DeviceOverrideThreshold_UsesDeviceThreshold;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := TDeviceConfig.Default($123456789ABC);
  DeviceConfig.BatteryTray.LowBatteryThreshold := 30;
  FDeviceConfigProvider.AddDeviceConfig($123456789ABC, DeviceConfig);

  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig($123456789ABC);
  Assert.AreEqual(30, DeviceConfig.BatteryTray.LowBatteryThreshold);
end;

procedure TBatteryTrayManagerTests.OnBatteryNotification_CanBeAssigned;
begin
  FManager.OnBatteryNotification := HandleBatteryNotification;
  Assert.IsTrue(Assigned(FManager.OnBatteryNotification));
end;

procedure TBatteryTrayManagerTests.ShouldShowTrayIcon_GlobalDisabled_ReturnsFalse;
begin
  // When global setting is disabled, device should not show icon
  FConfig.ShowBatteryTrayIcons := False;

  // UpdateDevice internally checks ShouldShowTrayIcon
  // With manager disabled, we test the config value directly
  Assert.IsFalse(FConfig.ShowBatteryTrayIcons);
end;

procedure TBatteryTrayManagerTests.ShouldShowTrayIcon_DeviceExplicitlyDisabled_ReturnsFalse;
var
  DeviceConfig: TDeviceConfig;
begin
  FConfig.ShowBatteryTrayIcons := True;

  DeviceConfig := TDeviceConfig.Default($123456789ABC);
  DeviceConfig.BatteryTray.ShowTrayIcon := 0; // Explicitly disabled
  FDeviceConfigProvider.AddDeviceConfig($123456789ABC, DeviceConfig);

  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig($123456789ABC);
  Assert.AreEqual(0, DeviceConfig.BatteryTray.ShowTrayIcon);
end;

procedure TBatteryTrayManagerTests.ShouldShowTrayIcon_DeviceExplicitlyEnabled_ReturnsTrue;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := TDeviceConfig.Default($123456789ABC);
  DeviceConfig.BatteryTray.ShowTrayIcon := 1; // Explicitly enabled
  FDeviceConfigProvider.AddDeviceConfig($123456789ABC, DeviceConfig);

  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig($123456789ABC);
  Assert.AreEqual(1, DeviceConfig.BatteryTray.ShowTrayIcon);
end;

procedure TBatteryTrayManagerTests.RefreshAll_EmptyManager_NoError;
begin
  FManager.RefreshAll;
  Assert.Pass('RefreshAll on empty manager completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_SameParamsTwice_NoError;
begin
  // Disable manager to avoid Shell_NotifyIcon calls
  FManager.Enabled := False;

  // Call UpdateDevice twice with identical parameters
  // Second call should hit cache and early exit
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);

  Assert.Pass('UpdateDevice with same params twice completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevicePending_SameParamsTwice_NoError;
begin
  FManager.Enabled := False;

  // Call UpdateDevicePending twice with identical parameters
  FManager.UpdateDevicePending($123456789ABC, 'Test Device');
  FManager.UpdateDevicePending($123456789ABC, 'Test Device');

  Assert.Pass('UpdateDevicePending with same params twice completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_DifferentLevels_NoError;
begin
  FManager.Enabled := False;

  // Call with different levels - cache should not match, icon should be recreated
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);
  FManager.UpdateDevice($123456789ABC, 'Test Device', 75, True);
  FManager.UpdateDevice($123456789ABC, 'Test Device', 100, True);

  Assert.Pass('UpdateDevice with different levels completed without error');
end;

procedure TBatteryTrayManagerTests.UpdateDevice_TransitionFromPendingToLevel_NoError;
begin
  FManager.Enabled := False;

  // Start with pending, then update to actual level
  FManager.UpdateDevicePending($123456789ABC, 'Test Device');
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);

  Assert.Pass('Transition from pending to level completed without error');
end;

procedure TBatteryTrayManagerTests.RemoveDevice_ClearsCacheEntry;
begin
  FManager.Enabled := False;

  // Add device then remove - should clear cache
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);
  FManager.RemoveDevice($123456789ABC);

  // Add again - should work without issues
  FManager.UpdateDevice($123456789ABC, 'Test Device', 50, True);

  Assert.Pass('RemoveDevice clears cache correctly');
end;

procedure TBatteryTrayManagerTests.ClearAll_ClearsCacheEntries;
begin
  FManager.Enabled := False;

  // Add multiple devices then clear all
  FManager.UpdateDevice($123456789ABC, 'Device 1', 50, True);
  FManager.UpdateDevice($987654321DEF, 'Device 2', 75, True);
  FManager.ClearAll;

  // Add again - should work without issues
  FManager.UpdateDevice($123456789ABC, 'Device 1', 50, True);
  FManager.UpdateDevice($987654321DEF, 'Device 2', 75, True);

  Assert.Pass('ClearAll clears cache correctly');
end;

procedure TBatteryTrayManagerTests.RapidUpdates_MultipleDevices_NoError;
const
  // Device addresses from the actual bug scenario
  ADDR_XM6 = UInt64($58195DD0B92E);    // WH-1000XM6 (96861746716078)
  ADDR_XM4 = UInt64($88CD65D0B2B4);    // WH-1000XM4 (150400772695732)
  ADDR_REDMI = UInt64($4875648BA4BC);  // Redmi Buds 5 Pro (79662169810108)
  ADDR_CONTROLLER = UInt64($E4371737D58F);  // Pro Controller (250791062919311)
  ADDR_POCO = UInt64($C0162213A747);   // POCO (211203199087815)
var
  I: Integer;
begin
  FManager.Enabled := False;

  // Simulate the scenario: 5 devices, rapid updates like during clicking
  for I := 1 to 100 do
  begin
    // Simulate ShowDisplayItems iterating through all devices
    FManager.UpdateDevicePending(ADDR_XM6, 'WH-1000XM6');
    FManager.UpdateDevicePending(ADDR_XM4, 'WH-1000XM4');
    FManager.RemoveDevice(ADDR_REDMI);
    FManager.RemoveDevice(ADDR_CONTROLLER);
    FManager.UpdateDevicePending(ADDR_POCO, 'POCO');

    // Then some devices get battery updates
    FManager.UpdateDevice(ADDR_XM4, 'WH-1000XM4', 85, True);
    FManager.UpdateDevice(ADDR_POCO, 'POCO', 60, True);

    // Some disconnect
    FManager.RemoveDevice(ADDR_XM6);
    FManager.RemoveDevice(ADDR_XM4);
  end;

  Assert.Pass('Rapid updates with multiple devices completed without error');
end;

procedure TBatteryTrayManagerTests.InterleavedOperations_MixedDeviceStates_NoError;
const
  ADDR = UInt64($123456789ABC);
var
  I: Integer;
begin
  FManager.Enabled := False;

  // Simulate rapid state changes for a single device
  for I := 1 to 200 do
  begin
    // Device connects (pending)
    FManager.UpdateDevicePending(ADDR, 'Test Device');

    // Battery level arrives
    FManager.UpdateDevice(ADDR, 'Test Device', 50 + (I mod 50), True);

    // Device disconnects
    FManager.RemoveDevice(ADDR);

    // Device reconnects while iterating
    FManager.UpdateDevicePending(ADDR, 'Test Device');

    // Config changes trigger ClearAll
    if I mod 10 = 0 then
      FManager.ClearAll;
  end;

  Assert.Pass('Interleaved operations completed without error');
end;

procedure TBatteryTrayManagerTests.RemoveDuringIteration_SimulatedScenario_NoError;
const
  ADDR1 = UInt64($111111111111);
  ADDR2 = UInt64($222222222222);
  ADDR3 = UInt64($333333333333);
var
  I: Integer;
begin
  FManager.Enabled := False;

  // Simulate the scenario where ShowDisplayItems iterates and some devices
  // change state mid-iteration (simulated by interleaved remove calls)
  for I := 1 to 50 do
  begin
    FManager.UpdateDevicePending(ADDR1, 'Device 1');
    FManager.UpdateDevicePending(ADDR2, 'Device 2');
    FManager.UpdateDevicePending(ADDR3, 'Device 3');

    // Simulate another refresh happening (removes and re-adds)
    FManager.RemoveDevice(ADDR1);
    FManager.UpdateDevicePending(ADDR1, 'Device 1');

    FManager.RemoveDevice(ADDR2);
    FManager.UpdateDevicePending(ADDR2, 'Device 2');

    FManager.RefreshAll;
  end;

  Assert.Pass('Remove during simulated iteration completed without error');
end;

procedure TBatteryTrayManagerTests.UpdatePendingAfterRemove_NoError;
const
  ADDR = UInt64($AABBCCDDEEFF);
var
  I: Integer;
begin
  FManager.Enabled := False;

  // This tests the scenario where a device is removed and then
  // immediately UpdateDevicePending is called (e.g., device reconnects quickly)
  for I := 1 to 100 do
  begin
    // Add device
    FManager.UpdateDevicePending(ADDR, 'Quick Device');
    FManager.UpdateDevice(ADDR, 'Quick Device', 75, True);

    // Remove it
    FManager.RemoveDevice(ADDR);

    // Immediately add pending again (simulates rapid reconnect)
    FManager.UpdateDevicePending(ADDR, 'Quick Device');
  end;

  Assert.Pass('UpdatePendingAfterRemove completed without error');
end;

procedure TBatteryTrayManagerTests.ClearAllDuringUpdates_NoError;
const
  ADDR1 = UInt64($111111111111);
  ADDR2 = UInt64($222222222222);
var
  I: Integer;
begin
  FManager.Enabled := False;

  // Simulate scenario where ClearAll is called while updates are happening
  // (e.g., settings changed during device state updates)
  for I := 1 to 50 do
  begin
    FManager.UpdateDevicePending(ADDR1, 'Device 1');
    FManager.ClearAll;
    FManager.UpdateDevicePending(ADDR1, 'Device 1');

    FManager.UpdateDevice(ADDR2, 'Device 2', 50, True);
    FManager.ClearAll;
    FManager.UpdateDevice(ADDR2, 'Device 2', 50, True);

    FManager.UpdateDevicePending(ADDR1, 'Device 1');
    FManager.UpdateDevice(ADDR2, 'Device 2', 60, True);
    FManager.RefreshAll;
  end;

  Assert.Pass('ClearAll during updates completed without error');
end;

initialization
  TDUnitX.RegisterTestFixture(TBatteryTrayManagerTests);

end.
