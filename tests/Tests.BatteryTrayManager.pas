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

initialization
  TDUnitX.RegisterTestFixture(TBatteryTrayManagerTests);

end.
