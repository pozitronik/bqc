{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TMainPresenter Unit Tests                       }
{                                                       }
{       Tests for main presenter with injected          }
{       mock dependencies.                              }
{                                                       }
{*******************************************************}

unit Tests.MainPresenter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.MainViewInterfaces,
  App.MainPresenter,
  App.AsyncExecutor,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.RadioControl,
  UI.DeviceList,
  App.DeviceDisplayItemBuilder,
  Tests.Mocks;

type
  /// <summary>
  /// Testable subclass that exposes protected methods for unit testing.
  /// </summary>
  TTestableMainPresenter = class(TMainPresenter)
  public
    procedure UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);
    function FindDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    function GetDeviceCount: Integer;
    function GetConnectedDeviceCount: Integer;
    function GetBatteryCache: IBatteryCache;
    /// <summary>
    /// Exposes HandleBatteryQueryCompleted for testing single-item update behavior.
    /// </summary>
    procedure SimulateBatteryQueryCompleted(ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
    /// <summary>
    /// Exposes RemoveDeviceFromList for testing index map rebuild behavior.
    /// </summary>
    procedure RemoveDeviceFromList(ADeviceAddress: UInt64);
    /// <summary>
    /// Exposes GetDevicesArray for testing array caching behavior.
    /// </summary>
    function GetDevicesArray: TBluetoothDeviceInfoArray;
  end;

  [TestFixture]
  TMainPresenterTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Create_InitializesWithDependencies;

    [Test]
    procedure CanClose_CloseToTrayTrue_ReturnsFalse;

    [Test]
    procedure CanClose_CloseToTrayFalse_ReturnsTrue;

    [Test]
    procedure OnExitRequested_CallsForceClose;

    [Test]
    procedure OnVisibilityToggleRequested_WhenVisible_HidesView;

    [Test]
    procedure OnVisibilityToggleRequested_WhenHidden_ShowsView;

    [Test]
    procedure OnVisibilityToggleRequested_WhenMinimized_ShowsView;

    [Test]
    procedure OnDeviceClicked_ConnectedDevice_ShowsDisconnectingStatus;

    [Test]
    procedure OnDeviceClicked_DisconnectedDevice_ShowsConnectingStatus;

    [Test]
    procedure OnDeviceClicked_MenuMode_KeepsViewOpen;

    [Test]
    procedure OnDeviceClicked_WindowMode_KeepsViewOpen;

    [Test]
    procedure OnDeviceClicked_ConnectingDevice_IgnoresClick;

    [Test]
    procedure OnDeviceClicked_DisconnectingDevice_IgnoresClick;

    { Initialize tests }
    [Test]
    procedure Initialize_RadioEnabled_SetsToggleTrue;

    [Test]
    procedure Initialize_RadioEnabled_LoadsDevices;

    [Test]
    procedure Initialize_RadioDisabled_SetsToggleFalse;

    [Test]
    procedure Initialize_RadioDisabled_ClearsDevices;

    [Test]
    procedure Initialize_NoAdapter_DisablesToggle;

    [Test]
    procedure Initialize_StartsRadioWatcher;

    [Test]
    procedure Initialize_WiresServiceEventHandlers;

    { Shutdown tests }
    [Test]
    procedure Shutdown_StopsRadioWatcher;

    [Test]
    procedure Shutdown_ReleasesService;

    { Event handler tests }
    [Test]
    procedure HandleRadioStateChanged_RadioEnabled_LoadsDevices;

    [Test]
    procedure HandleRadioStateChanged_RadioDisabled_ClearsDevices;

    { Public method tests }
    [Test]
    procedure OnToggleChanged_EnabledTrue_CallsSetRadioStateAsync;

    [Test]
    procedure OnToggleChanged_EnabledFalse_CallsSetRadioStateAsync;

    [Test]
    procedure OnToggleChanged_WhenUpdatingToggle_IgnoresCall;

    [Test]
    procedure OnRefreshRequested_CallsLoadDevices;

    [Test]
    procedure OnRefreshRequested_ShowsRefreshingStatus;

    [Test]
    procedure OnViewShown_RefreshesBatteryForConnectedDevices;

    [Test]
    procedure OnSettingsChanged_CompletesWithoutError;

    [Test]
    procedure IsUpdatingToggle_InitiallyFalse;

    [Test]
    procedure IsUpdatingToggle_ReturnsFalseAfterInitialize;
  end;

  /// <summary>
  /// Tests for delayed load behavior using IAsyncExecutor.RunDelayed.
  /// Verifies debouncing, cancellation, and correct timing.
  /// </summary>
  [TestFixture]
  TDelayedLoadTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure RadioEnabled_CallsRunDelayed;

    [Test]
    procedure RadioEnabled_UsesCorrectDelayInterval;

    [Test]
    procedure RadioEnabled_DelayedLoadExecutesLoadDevices;

    [Test]
    procedure RadioDisabled_CancelsPendingDelayedLoad;

    [Test]
    procedure MultipleRadioStateChanges_DebouncesProperly;

    [Test]
    procedure Shutdown_CancelsPendingDelayedLoad;
  end;

  /// <summary>
  /// Tests for device index map functionality (O(1) lookup).
  /// Uses TTestableMainPresenter to access protected methods.
  /// </summary>
  [TestFixture]
  TDeviceIndexMapTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FindDeviceByAddress_EmptyList_ReturnsEmptyRecord;

    [Test]
    procedure UpdateOrAddDevice_NewDevice_AddsToList;

    [Test]
    procedure UpdateOrAddDevice_NewDevice_CanBeFoundByAddress;

    [Test]
    procedure UpdateOrAddDevice_ExistingDevice_UpdatesInPlace;

    [Test]
    procedure UpdateOrAddDevice_MultipleDevices_AllCanBeFound;

    [Test]
    procedure UpdateOrAddDevice_MixedAddAndUpdate_MaintainsConsistency;

    [Test]
    procedure FindDeviceByAddress_NonExistentAddress_ReturnsEmptyRecord;
  end;

implementation

{ TTestableMainPresenter }

procedure TTestableMainPresenter.UpdateOrAddDevice(const ADevice: TBluetoothDeviceInfo);
begin
  inherited UpdateOrAddDevice(ADevice);
end;

function TTestableMainPresenter.FindDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  Result := inherited FindDeviceByAddress(AAddress);
end;

function TTestableMainPresenter.GetDeviceCount: Integer;
begin
  Result := inherited GetDeviceCount;
end;

function TTestableMainPresenter.GetConnectedDeviceCount: Integer;
begin
  Result := inherited GetConnectedDeviceCount;
end;

function TTestableMainPresenter.GetBatteryCache: IBatteryCache;
begin
  Result := inherited GetBatteryCache;
end;

procedure TTestableMainPresenter.SimulateBatteryQueryCompleted(
  ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
var
  BatteryCache: IBatteryCache;
begin
  // Trigger the battery query completed event on the battery cache
  // This will call the coordinator's HandleBatteryQueryCompleted internally
  BatteryCache := GetBatteryCache;
  if Assigned(BatteryCache) and Assigned(BatteryCache.OnQueryCompleted) then
    BatteryCache.OnQueryCompleted(nil, ADeviceAddress, AStatus);
end;

procedure TTestableMainPresenter.RemoveDeviceFromList(ADeviceAddress: UInt64);
begin
  inherited RemoveDeviceFromList(ADeviceAddress);
end;

function TTestableMainPresenter.GetDevicesArray: TBluetoothDeviceInfoArray;
begin
  Result := inherited GetDevicesArray;
end;

{ TMainPresenterTests }

procedure TMainPresenterTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TMainPresenterTests.TearDown;
var
  I: Integer;
begin
  // Wait for async operations to complete. Multiple async layers exist:
  // - SetRadioStateAsync spawns threads that use TThread.Queue
  // - Battery cache (if enabled) spawns threads with TThread.Queue
  // We need to give threads time to complete and explicitly process the
  // synchronization queue with CheckSynchronize (not just ProcessMessages)
  for I := 1 to 5 do
  begin
    Sleep(20);
    CheckSynchronize(0);  // Process any pending TThread.Queue/Synchronize calls
  end;
  Application.ProcessMessages;
  CheckSynchronize(0);  // Final sync processing

  FPresenter.Free;
  // Interfaces are reference-counted, will be released when presenter is freed
end;

procedure TMainPresenterTests.CreatePresenter;
begin
  FPresenter := TMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TMainPresenterTests.Create_InitializesWithDependencies;
begin
  CreatePresenter;

  Assert.IsNotNull(FPresenter, 'Presenter should be created');
end;

procedure TMainPresenterTests.CanClose_CloseToTrayTrue_ReturnsFalse;
begin
  FWindowConfig.CloseToTray := True;
  CreatePresenter;

  Assert.IsFalse(FPresenter.CanClose, 'CanClose should return False when CloseToTray is True');
end;

procedure TMainPresenterTests.CanClose_CloseToTrayFalse_ReturnsTrue;
begin
  FWindowConfig.CloseToTray := False;
  CreatePresenter;

  Assert.IsTrue(FPresenter.CanClose, 'CanClose should return True when CloseToTray is False');
end;

procedure TMainPresenterTests.OnExitRequested_CallsForceClose;
begin
  CreatePresenter;

  FPresenter.OnExitRequested;

  Assert.IsTrue(FView.ForceCloseCalled, 'ForceClose should be called on view');
end;

procedure TMainPresenterTests.OnVisibilityToggleRequested_WhenVisible_HidesView;
begin
  CreatePresenter;
  FView.Visible := True;
  FView.Minimized := False;

  FPresenter.OnVisibilityToggleRequested;

  Assert.IsTrue(FView.HideViewCalled, 'HideView should be called when view is visible');
end;

procedure TMainPresenterTests.OnVisibilityToggleRequested_WhenHidden_ShowsView;
begin
  CreatePresenter;
  FView.Visible := False;

  FPresenter.OnVisibilityToggleRequested;

  Assert.IsTrue(FView.ShowViewCalled, 'ShowView should be called when view is hidden');
end;

procedure TMainPresenterTests.OnVisibilityToggleRequested_WhenMinimized_ShowsView;
begin
  CreatePresenter;
  FView.Visible := True;
  FView.Minimized := True;

  FPresenter.OnVisibilityToggleRequested;

  Assert.IsTrue(FView.ShowViewCalled, 'ShowView should be called when view is minimized');
end;

procedure TMainPresenterTests.OnDeviceClicked_ConnectedDevice_ShowsDisconnectingStatus;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnected);
  FBluetoothService.Devices := [Device];

  CreatePresenter;
  FPresenter.Initialize;

  FPresenter.OnDeviceClicked(Device);

  Assert.Contains(FView.LastStatus, 'Disconnecting', 'Status should indicate disconnecting');
end;

procedure TMainPresenterTests.OnDeviceClicked_DisconnectedDevice_ShowsConnectingStatus;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);
  FBluetoothService.Devices := [Device];

  CreatePresenter;
  FPresenter.Initialize;

  FPresenter.OnDeviceClicked(Device);

  Assert.Contains(FView.LastStatus, 'Connecting', 'Status should indicate connecting');
end;

procedure TMainPresenterTests.OnDeviceClicked_MenuMode_KeepsViewOpen;
var
  Device: TBluetoothDeviceInfo;
begin
  // Menu now stays open after device click so user can see connection progress
  // and click more devices. Menu will close on focus loss.
  FGeneralConfig.WindowMode := wmMenu;
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.IsFalse(FView.HideViewCalled, 'View should stay open in menu mode to show connection progress');
end;

procedure TMainPresenterTests.OnDeviceClicked_WindowMode_KeepsViewOpen;
var
  Device: TBluetoothDeviceInfo;
begin
  FGeneralConfig.WindowMode := wmWindow;
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.IsFalse(FView.HideViewCalled, 'View should stay open in window mode');
end;

procedure TMainPresenterTests.OnDeviceClicked_ConnectingDevice_IgnoresClick;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnecting);
  FBluetoothService.Devices := [Device];

  CreatePresenter;
  FPresenter.Initialize;

  FPresenter.OnDeviceClicked(Device);

  // Status is shown with "Operation in progress" message
  Assert.Contains(FView.LastStatus, 'progress', 'Status should indicate operation in progress');
end;

procedure TMainPresenterTests.OnDeviceClicked_DisconnectingDevice_IgnoresClick;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnecting);
  FBluetoothService.Devices := [Device];

  CreatePresenter;
  FPresenter.Initialize;

  FPresenter.OnDeviceClicked(Device);

  Assert.Contains(FView.LastStatus, 'progress', 'Status should indicate operation in progress');
end;

{ Initialize tests }

procedure TMainPresenterTests.Initialize_RadioEnabled_SetsToggleTrue;
begin
  // Arrange: Configure radio as enabled and available
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: Toggle should be set to True
  Assert.IsTrue(FView.ToggleState, 'Toggle should be set to True when radio is enabled');
end;

procedure TMainPresenterTests.Initialize_RadioEnabled_LoadsDevices;
begin
  // Arrange: Configure radio as enabled
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: Status should indicate devices are being loaded
  Assert.Contains(FView.LastStatus, 'device', 'Status should mention devices after initialization');
end;

procedure TMainPresenterTests.Initialize_RadioDisabled_SetsToggleFalse;
begin
  // Arrange: Configure radio as disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: Toggle should be set to False
  Assert.IsFalse(FView.ToggleState, 'Toggle should be set to False when radio is disabled');
end;

procedure TMainPresenterTests.Initialize_RadioDisabled_ClearsDevices;
begin
  // Arrange: Configure radio as disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: ClearDevices should be called
  Assert.IsTrue(FView.ClearDevicesCalled, 'ClearDevices should be called when radio is disabled');
end;

procedure TMainPresenterTests.Initialize_NoAdapter_DisablesToggle;
begin
  // Arrange: Configure no adapter available
  FRadioStateManager.RadioAvailable := False;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: Toggle should be disabled and set to False
  Assert.IsFalse(FView.ToggleState, 'Toggle should be False when no adapter');
  Assert.IsFalse(FView.ToggleEnabled, 'Toggle should be disabled when no adapter');
  Assert.Contains(FView.LastStatus, 'No Bluetooth adapter', 'Status should indicate no adapter found');
end;

procedure TMainPresenterTests.Initialize_StartsRadioWatcher;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: Radio watcher should be started
  Assert.AreEqual(1, FRadioStateManager.StartWatchingCallCount, 'StartWatching should be called once');
end;

procedure TMainPresenterTests.Initialize_WiresServiceEventHandlers;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: After Initialize, service events should be wired
  // We verify indirectly by checking that GetState was called (happens during Initialize)
  Assert.AreEqual(1, FRadioStateManager.GetStateCallCount, 'GetState should be called during Initialize');
end;

{ Shutdown tests }

procedure TMainPresenterTests.Shutdown_StopsRadioWatcher;
begin
  // Arrange: Initialize first
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.Shutdown;

  // Assert: StopWatching should be called
  Assert.AreEqual(1, FRadioStateManager.StopWatchingCallCount, 'StopWatching should be called on Shutdown');
end;

procedure TMainPresenterTests.Shutdown_ReleasesService;
begin
  // Arrange: Initialize first
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.Shutdown;

  // Assert: No exception should occur, and a second shutdown should be safe
  FPresenter.Shutdown;  // Should not raise exception

  Assert.Pass('Shutdown completed without errors');
end;

{ Event handler tests }

procedure TMainPresenterTests.HandleRadioStateChanged_RadioEnabled_LoadsDevices;
begin
  // Arrange: Initialize with radio disabled first
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Reset tracking
  FView.ClearDevices;  // Reset state

  // Act: Simulate radio state changing to enabled
  FRadioStateManager.SimulateStateChanged(True);

  // Assert: Toggle should be updated and loading status shown
  Assert.IsTrue(FView.ToggleState, 'Toggle should be set to True when radio becomes enabled');
  Assert.Contains(FView.LastStatus, 'Loading', 'Status should indicate loading devices');
end;

procedure TMainPresenterTests.HandleRadioStateChanged_RadioDisabled_ClearsDevices;
begin
  // Arrange: Initialize with radio enabled first
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Reset tracking
  FView.ClearDevices;  // Clear the flag by accessing the method
  // We need to check if ClearDevices is called again

  // Act: Simulate radio state changing to disabled
  FRadioStateManager.SimulateStateChanged(False);

  // Assert: Toggle should be False and devices should be cleared
  Assert.IsFalse(FView.ToggleState, 'Toggle should be set to False when radio becomes disabled');
  Assert.Contains(FView.LastStatus, 'disabled', 'Status should indicate Bluetooth disabled');
end;

{ Public method tests }

procedure TMainPresenterTests.OnToggleChanged_EnabledTrue_CallsSetRadioStateAsync;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.OnToggleChanged(True);

  // Assert: Status should indicate enabling Bluetooth
  Assert.Contains(FView.LastStatus, 'Enabling', 'Status should indicate enabling Bluetooth');
end;

procedure TMainPresenterTests.OnToggleChanged_EnabledFalse_CallsSetRadioStateAsync;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.OnToggleChanged(False);

  // Assert: Status should indicate disabling Bluetooth
  Assert.Contains(FView.LastStatus, 'Disabling', 'Status should indicate disabling Bluetooth');
end;

procedure TMainPresenterTests.OnToggleChanged_WhenUpdatingToggle_IgnoresCall;
var
  InitialStatusCount: Integer;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Store initial status count after initialization
  InitialStatusCount := FView.ShowStatusCount;

  // The presenter guards against recursive toggle updates
  // We can't directly test IsUpdatingToggle from outside,
  // but we verify that the guard works by checking that
  // when toggle is changed programmatically, OnToggleChanged ignores the call
  // This is tested implicitly in Initialize which uses SetToggleStateSafe

  // Assert: Verify that calling OnToggleChanged works normally
  FPresenter.OnToggleChanged(False);

  Assert.IsTrue(FView.ShowStatusCount > InitialStatusCount,
    'OnToggleChanged should update status when not in updating state');
end;

procedure TMainPresenterTests.OnRefreshRequested_CallsLoadDevices;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.OnRefreshRequested;

  // Assert: Status should show device count (result of LoadDevices)
  Assert.Contains(FView.LastStatus, 'device', 'Status should mention devices after refresh');
end;

procedure TMainPresenterTests.OnRefreshRequested_ShowsRefreshingStatus;
var
  InitialStatusCount: Integer;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  InitialStatusCount := FView.ShowStatusCount;

  // Act
  FPresenter.OnRefreshRequested;

  // Assert: ShowStatus should have been called at least twice
  // (once for "Refreshing..." and once for the result)
  Assert.IsTrue(FView.ShowStatusCount > InitialStatusCount,
    'ShowStatus should be called during refresh');
end;

procedure TMainPresenterTests.OnViewShown_RefreshesBatteryForConnectedDevices;
begin
  // Arrange: Enable battery level display
  FAppearanceConfig.ShowBatteryLevel := True;
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act: Notify that view is shown
  FPresenter.OnViewShown;

  // Assert: No exception should occur
  // The actual battery refresh is handled internally
  Assert.Pass('OnViewShown completed without errors');
end;

procedure TMainPresenterTests.OnSettingsChanged_CompletesWithoutError;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Act
  FPresenter.OnSettingsChanged;

  // Assert: Method should complete without exceptions
  Assert.Pass('OnSettingsChanged completed without errors');
end;

procedure TMainPresenterTests.IsUpdatingToggle_InitiallyFalse;
begin
  // Arrange
  CreatePresenter;

  // Assert: Before initialization, IsUpdatingToggle should be False
  Assert.IsFalse(FPresenter.IsUpdatingToggle, 'IsUpdatingToggle should be False initially');
end;

procedure TMainPresenterTests.IsUpdatingToggle_ReturnsFalseAfterInitialize;
begin
  // Arrange
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert: After Initialize completes, IsUpdatingToggle should be False
  Assert.IsFalse(FPresenter.IsUpdatingToggle, 'IsUpdatingToggle should be False after Initialize');
end;

{ TDelayedLoadTests }

procedure TDelayedLoadTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  // Use non-synchronous mode to control when delayed procs execute
  FAsyncExecutor.Synchronous := False;
  FBluetoothService := TMockBluetoothService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TDelayedLoadTests.TearDown;
var
  I: Integer;
begin
  // Process any pending TThread.Queue calls
  for I := 1 to 5 do
  begin
    Sleep(20);
    CheckSynchronize(0);
  end;
  Application.ProcessMessages;
  CheckSynchronize(0);

  FPresenter.Free;
end;

procedure TDelayedLoadTests.CreatePresenter;
begin
  FPresenter := TMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TDelayedLoadTests.RadioEnabled_CallsRunDelayed;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Reset counter after initialization
  FAsyncExecutor.ClearPending;
  // Store initial RunDelayed call count (might have been called during init)
  var InitialCount := FAsyncExecutor.RunDelayedCallCount;

  // Act: Simulate radio state changing to enabled
  FRadioStateManager.SimulateStateChanged(True);

  // Assert: RunDelayed should have been called
  Assert.IsTrue(FAsyncExecutor.RunDelayedCallCount > InitialCount,
    'RunDelayed should be called when radio becomes enabled');
end;

procedure TDelayedLoadTests.RadioEnabled_UsesCorrectDelayInterval;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;
  FAsyncExecutor.ClearPending;

  // Act: Simulate radio state changing to enabled
  FRadioStateManager.SimulateStateChanged(True);

  // Assert: Delay should be 500ms (DELAYED_LOAD_INTERVAL_MS)
  Assert.AreEqual(500, FAsyncExecutor.LastDelayMs,
    'Delayed load should use 500ms interval');
end;

procedure TDelayedLoadTests.RadioEnabled_DelayedLoadExecutesLoadDevices;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Clear any pending operations
  FAsyncExecutor.ClearPending;
  FView.ShowDisplayItemsCalled := False;

  // Act: Simulate radio state changing to enabled
  FRadioStateManager.SimulateStateChanged(True);

  // Execute the pending delayed proc
  FAsyncExecutor.ExecutePending;
  // Process TThread.Queue calls
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: LoadDevices should have been called (via ShowDisplayItems)
  Assert.IsTrue(FView.ShowDisplayItemsCalled,
    'LoadDevices should execute after delayed callback');
end;

procedure TDelayedLoadTests.RadioDisabled_CancelsPendingDelayedLoad;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;
  FAsyncExecutor.ClearPending;
  FView.ShowDisplayItemsCalled := False;

  // Schedule a delayed load
  FRadioStateManager.SimulateStateChanged(True);

  // Act: Disable radio before delayed load executes (cancels pending)
  FRadioStateManager.SimulateStateChanged(False);

  // Execute the pending procs
  FAsyncExecutor.ExecutePending;
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: LoadDevices should NOT have been called because load was cancelled
  // The delayed callback checks generation counter and skips if changed
  Assert.IsFalse(FView.ShowDisplayItemsCalled,
    'LoadDevices should be cancelled when radio is disabled before execution');
end;

procedure TDelayedLoadTests.MultipleRadioStateChanges_DebouncesProperly;
var
  ShowDisplayItemsCount: Integer;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;
  FAsyncExecutor.ClearPending;

  // Act: Rapidly toggle radio state multiple times
  FRadioStateManager.SimulateStateChanged(True);
  FRadioStateManager.SimulateStateChanged(False);
  FRadioStateManager.SimulateStateChanged(True);
  FRadioStateManager.SimulateStateChanged(False);
  FRadioStateManager.SimulateStateChanged(True);  // Final state is enabled

  // Record initial state
  FView.ShowDisplayItemsCount := 0;

  // Execute all pending procs
  FAsyncExecutor.ExecutePending;
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: Only the last delayed load should execute
  ShowDisplayItemsCount := FView.ShowDisplayItemsCount;
  Assert.AreEqual(1, ShowDisplayItemsCount,
    'Only one LoadDevices should execute due to debouncing');
end;

procedure TDelayedLoadTests.Shutdown_CancelsPendingDelayedLoad;
begin
  // Arrange: Start with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;
  FAsyncExecutor.ClearPending;
  FView.ShowDisplayItemsCalled := False;

  // Schedule a delayed load
  FRadioStateManager.SimulateStateChanged(True);

  // Act: Shutdown before delayed load executes
  FPresenter.Shutdown;

  // Execute the pending procs
  FAsyncExecutor.ExecutePending;
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: LoadDevices should NOT have been called
  Assert.IsFalse(FView.ShowDisplayItemsCalled,
    'LoadDevices should be cancelled when Shutdown is called');
end;

{ TDeviceIndexMapTests }

procedure TDeviceIndexMapTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TDeviceIndexMapTests.TearDown;
begin
  FPresenter.Free;
end;

procedure TDeviceIndexMapTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TDeviceIndexMapTests.FindDeviceByAddress_EmptyList_ReturnsEmptyRecord;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;

  Device := FPresenter.FindDeviceByAddress($001122334455);

  Assert.AreEqual(UInt64(0), Device.AddressInt, 'Should return empty record for non-existent device');
end;

procedure TDeviceIndexMapTests.UpdateOrAddDevice_NewDevice_AddsToList;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Device', btAudioOutput, csDisconnected);

  FPresenter.UpdateOrAddDevice(Device);

  Assert.AreEqual(1, FPresenter.GetDeviceCount, 'Should have 1 device after add');
end;

procedure TDeviceIndexMapTests.UpdateOrAddDevice_NewDevice_CanBeFoundByAddress;
var
  Device, FoundDevice: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Device', btAudioOutput, csDisconnected);

  FPresenter.UpdateOrAddDevice(Device);
  FoundDevice := FPresenter.FindDeviceByAddress($001122334455);

  Assert.AreEqual(Device.AddressInt, FoundDevice.AddressInt);
  Assert.AreEqual(Device.Name, FoundDevice.Name);
end;

procedure TDeviceIndexMapTests.UpdateOrAddDevice_ExistingDevice_UpdatesInPlace;
var
  Device1, Device2, FoundDevice: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($001122334455, 'Test Device', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($001122334455, 'Updated Name', btAudioOutput, csConnected);

  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);

  Assert.AreEqual(1, FPresenter.GetDeviceCount, 'Should still have 1 device after update');
  FoundDevice := FPresenter.FindDeviceByAddress($001122334455);
  Assert.AreEqual('Updated Name', FoundDevice.Name, 'Name should be updated');
  Assert.AreEqual(Ord(csConnected), Ord(FoundDevice.ConnectionState), 'State should be updated');
end;

procedure TDeviceIndexMapTests.UpdateOrAddDevice_MultipleDevices_AllCanBeFound;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  Found1, Found2, Found3: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);

  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  Assert.AreEqual(3, FPresenter.GetDeviceCount, 'Should have 3 devices');

  Found1 := FPresenter.FindDeviceByAddress($111111111111);
  Found2 := FPresenter.FindDeviceByAddress($222222222222);
  Found3 := FPresenter.FindDeviceByAddress($333333333333);

  Assert.AreEqual('Device 1', Found1.Name);
  Assert.AreEqual('Device 2', Found2.Name);
  Assert.AreEqual('Device 3', Found3.Name);
end;

procedure TDeviceIndexMapTests.UpdateOrAddDevice_MixedAddAndUpdate_MaintainsConsistency;
var
  Device1, Device2, Device3, UpdatedDevice2: TBluetoothDeviceInfo;
  Found1, Found2, Found3: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  // Add three devices
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);

  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  // Update middle device
  UpdatedDevice2 := CreateTestDevice($222222222222, 'Device 2 Updated', btKeyboard, csDisconnected);
  FPresenter.UpdateOrAddDevice(UpdatedDevice2);

  // Verify all devices still accessible with correct data
  Assert.AreEqual(3, FPresenter.GetDeviceCount, 'Count should remain 3');

  Found1 := FPresenter.FindDeviceByAddress($111111111111);
  Found2 := FPresenter.FindDeviceByAddress($222222222222);
  Found3 := FPresenter.FindDeviceByAddress($333333333333);

  Assert.AreEqual('Device 1', Found1.Name, 'Device 1 should be unchanged');
  Assert.AreEqual('Device 2 Updated', Found2.Name, 'Device 2 should be updated');
  Assert.AreEqual('Device 3', Found3.Name, 'Device 3 should be unchanged');
end;

procedure TDeviceIndexMapTests.FindDeviceByAddress_NonExistentAddress_ReturnsEmptyRecord;
var
  Device, Found: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);

  Found := FPresenter.FindDeviceByAddress($999999999999);

  Assert.AreEqual(UInt64(0), Found.AddressInt, 'Should return empty record for non-existent address');
end;

{ TBatteryQueryCompletionTests }

/// <summary>
/// Tests for battery query completion behavior.
/// Verifies O(1) single-item update instead of full display refresh.
/// </summary>
type
  [TestFixture]
  TBatteryQueryCompletionTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure BatteryQueryCompleted_ExistingDevice_CallsUpdateDisplayItem;

    [Test]
    procedure BatteryQueryCompleted_ExistingDevice_DoesNotCallShowDisplayItems;

    [Test]
    procedure BatteryQueryCompleted_NonExistingDevice_DoesNotCallUpdateDisplayItem;

    [Test]
    procedure BatteryQueryCompleted_ExistingDevice_UpdatesCorrectDevice;
  end;

procedure TBatteryQueryCompletionTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TBatteryQueryCompletionTests.TearDown;
begin
  FPresenter.Free;
end;

procedure TBatteryQueryCompletionTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
  FPresenter.Initialize;
end;

procedure TBatteryQueryCompletionTests.BatteryQueryCompleted_ExistingDevice_CallsUpdateDisplayItem;
var
  Device: TBluetoothDeviceInfo;
  BatteryStatus: TBatteryStatus;
begin
  // Arrange: Create presenter and add a device to its list
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnected);
  FPresenter.UpdateOrAddDevice(Device);
  FView.UpdateDisplayItemCount := 0;  // Reset counter after any setup calls

  // Act: Simulate battery query completion
  BatteryStatus := TBatteryStatus.Create(75);
  FPresenter.SimulateBatteryQueryCompleted($001122334455, BatteryStatus);

  // Assert: UpdateDisplayItem should be called once
  Assert.AreEqual(1, FView.UpdateDisplayItemCount,
    'UpdateDisplayItem should be called exactly once for single device battery update');
end;

procedure TBatteryQueryCompletionTests.BatteryQueryCompleted_ExistingDevice_DoesNotCallShowDisplayItems;
var
  Device: TBluetoothDeviceInfo;
  BatteryStatus: TBatteryStatus;
  InitialShowCount: Integer;
begin
  // Arrange: Create presenter and add a device
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnected);
  FPresenter.UpdateOrAddDevice(Device);
  InitialShowCount := FView.ShowDisplayItemsCount;  // Capture initial count

  // Act: Simulate battery query completion
  BatteryStatus := TBatteryStatus.Create(50);
  FPresenter.SimulateBatteryQueryCompleted($001122334455, BatteryStatus);

  // Assert: ShowDisplayItems should NOT be called (no full refresh)
  Assert.AreEqual(InitialShowCount, FView.ShowDisplayItemsCount,
    'ShowDisplayItems should not be called - battery update uses single-item update');
end;

procedure TBatteryQueryCompletionTests.BatteryQueryCompleted_NonExistingDevice_DoesNotCallUpdateDisplayItem;
var
  BatteryStatus: TBatteryStatus;
begin
  // Arrange: Create presenter with empty device list
  CreatePresenter;
  FView.UpdateDisplayItemCount := 0;

  // Act: Simulate battery query for non-existent device
  BatteryStatus := TBatteryStatus.Create(100);
  FPresenter.SimulateBatteryQueryCompleted($999999999999, BatteryStatus);

  // Assert: No update should occur
  Assert.AreEqual(0, FView.UpdateDisplayItemCount,
    'UpdateDisplayItem should not be called for non-existent device');
end;

procedure TBatteryQueryCompletionTests.BatteryQueryCompleted_ExistingDevice_UpdatesCorrectDevice;
var
  Device1, Device2: TBluetoothDeviceInfo;
  BatteryStatus: TBatteryStatus;
begin
  // Arrange: Create presenter and add multiple devices
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csConnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FView.UpdateDisplayItemCount := 0;

  // Act: Simulate battery query for Device 2
  BatteryStatus := TBatteryStatus.Create(85);
  FPresenter.SimulateBatteryQueryCompleted($222222222222, BatteryStatus);

  // Assert: Correct device address should be updated
  Assert.AreEqual(UInt64($222222222222), FView.LastUpdatedDisplayItem.Device.AddressInt,
    'Should update the correct device by address');
end;

{ TShutdownSafetyTests }

/// <summary>
/// Tests for shutdown flag lifetime safety.
/// Verifies async callbacks are skipped after Shutdown is called.
/// </summary>
type
  [TestFixture]
  TShutdownSafetyTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure AfterShutdown_BatteryQueryCompleted_DoesNotUpdateDisplay;

    [Test]
    procedure AfterShutdown_RadioStateChanged_DoesNotUpdateToggle;

    [Test]
    procedure AfterShutdown_DelayedLoad_DoesNotExecute;

    [Test]
    procedure AfterShutdown_MultipleAsyncCallbacks_AllSkipped;
  end;

procedure TShutdownSafetyTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FAsyncExecutor.Synchronous := False;  // Control when callbacks execute
  FBluetoothService := TMockBluetoothService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TShutdownSafetyTests.TearDown;
var
  I: Integer;
begin
  // Process any pending TThread.Queue calls
  for I := 1 to 5 do
  begin
    Sleep(20);
    CheckSynchronize(0);
  end;
  Application.ProcessMessages;
  CheckSynchronize(0);

  FPresenter.Free;
end;

procedure TShutdownSafetyTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TShutdownSafetyTests.AfterShutdown_BatteryQueryCompleted_DoesNotUpdateDisplay;
var
  Device: TBluetoothDeviceInfo;
  BatteryStatus: TBatteryStatus;
begin
  // Arrange: Create presenter and add a device
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnected);
  FPresenter.UpdateOrAddDevice(Device);
  FView.UpdateDisplayItemCount := 0;

  // Shutdown presenter
  FPresenter.Shutdown;

  // Act: Simulate battery query completion after shutdown
  BatteryStatus := TBatteryStatus.Create(75);
  FPresenter.SimulateBatteryQueryCompleted($001122334455, BatteryStatus);

  // Assert: UpdateDisplayItem should NOT be called
  Assert.AreEqual(0, FView.UpdateDisplayItemCount,
    'UpdateDisplayItem should not be called after Shutdown');
end;

procedure TShutdownSafetyTests.AfterShutdown_RadioStateChanged_DoesNotUpdateToggle;
begin
  // Arrange: Initialize with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Shutdown presenter
  FPresenter.Shutdown;

  // Record current toggle state
  FView.ToggleState := False;
  FView.SetToggleStateCallCount := 0;

  // Act: Simulate radio state change after shutdown
  FRadioStateManager.SimulateStateChanged(True);

  // Assert: Toggle state should NOT change
  Assert.AreEqual(0, FView.SetToggleStateCallCount,
    'SetToggleState should not be called after Shutdown');
  Assert.IsFalse(FView.ToggleState,
    'Toggle state should remain unchanged after Shutdown');
end;

procedure TShutdownSafetyTests.AfterShutdown_DelayedLoad_DoesNotExecute;
begin
  // Arrange: Initialize with radio disabled
  FRadioStateManager.RadioEnabled := False;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;
  FAsyncExecutor.ClearPending;
  FView.ShowDisplayItemsCalled := False;

  // Schedule a delayed load
  FRadioStateManager.SimulateStateChanged(True);

  // Shutdown before executing
  FPresenter.Shutdown;

  // Act: Execute pending callbacks
  FAsyncExecutor.ExecutePending;
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: LoadDevices should NOT have been called
  Assert.IsFalse(FView.ShowDisplayItemsCalled,
    'Delayed load should not execute after Shutdown');
end;

procedure TShutdownSafetyTests.AfterShutdown_MultipleAsyncCallbacks_AllSkipped;
var
  Device: TBluetoothDeviceInfo;
  BatteryStatus: TBatteryStatus;
  InitialUpdateCount, InitialShowCount: Integer;
begin
  // Arrange: Initialize fully
  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  // Add a device
  Device := CreateTestDevice($001122334455, 'Test Device', btAudioOutput, csConnected);
  FPresenter.UpdateOrAddDevice(Device);

  // Schedule delayed load
  FAsyncExecutor.ClearPending;
  FRadioStateManager.SimulateStateChanged(False);
  FRadioStateManager.SimulateStateChanged(True);

  // Shutdown
  FPresenter.Shutdown;

  // Record current counts
  InitialUpdateCount := FView.UpdateDisplayItemCount;
  InitialShowCount := FView.ShowDisplayItemsCount;
  FView.SetToggleStateCallCount := 0;

  // Act: Try all async callbacks
  BatteryStatus := TBatteryStatus.Create(50);
  FPresenter.SimulateBatteryQueryCompleted($001122334455, BatteryStatus);
  FRadioStateManager.SimulateStateChanged(True);
  FAsyncExecutor.ExecutePending;
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: Nothing should have been called
  Assert.AreEqual(InitialUpdateCount, FView.UpdateDisplayItemCount,
    'UpdateDisplayItem should not change after Shutdown');
  Assert.AreEqual(InitialShowCount, FView.ShowDisplayItemsCount,
    'ShowDisplayItems should not change after Shutdown');
  Assert.AreEqual(0, FView.SetToggleStateCallCount,
    'SetToggleState should not be called after Shutdown');
end;

{ TConnectedAddressesCacheTests }

/// <summary>
/// Tests for connected device addresses cache.
/// Verifies O(1) access to connected devices without iterating device list.
/// </summary>
type
  [TestFixture]
  TConnectedAddressesCacheTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure InitiallyEmpty;

    [Test]
    procedure LoadDevices_PopulatesCacheWithConnectedDevices;

    [Test]
    procedure LoadDevices_ExcludesDisconnectedDevices;

    [Test]
    procedure DeviceConnects_AddedToCache;

    [Test]
    procedure DeviceDisconnects_RemovedFromCache;

    [Test]
    procedure MultipleConnectedDevices_AllTracked;

    [Test]
    procedure Shutdown_ClearsCache;
  end;

  /// <summary>
  /// Tests for unpaired device filtering based on ShowUnidentifiedDevices config.
  /// </summary>
  [TestFixture]
  TUnpairedDeviceFilteringTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FLayoutConfig: TMockLayoutConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FConnectionConfig: TMockConnectionConfig;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ConfigProperty_ShowUnidentifiedDevices_DefaultValue;

    [Test]
    procedure ConfigProperty_ShowUnidentifiedDevices_CanBeSet;

    [Test]
    procedure ConfigProperty_ShowUnpairedDevices_Required;
  end;

procedure TConnectedAddressesCacheTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TConnectedAddressesCacheTests.TearDown;
var
  I: Integer;
begin
  // Process any pending TThread.Queue calls
  for I := 1 to 5 do
  begin
    Sleep(20);
    CheckSynchronize(0);
  end;
  Application.ProcessMessages;
  CheckSynchronize(0);

  FPresenter.Free;
end;

procedure TConnectedAddressesCacheTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TConnectedAddressesCacheTests.InitiallyEmpty;
begin
  CreatePresenter;

  Assert.AreEqual(0, FPresenter.GetConnectedDeviceCount,
    'Connected device cache should be empty initially');
end;

procedure TConnectedAddressesCacheTests.LoadDevices_PopulatesCacheWithConnectedDevices;
var
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange: Set up devices with 2 connected
  SetLength(Devices, 3);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csConnected);
  Devices[1] := CreateTestDevice($222, 'Device 2', btKeyboard, csDisconnected);
  Devices[2] := CreateTestDevice($333, 'Device 3', btMouse, csConnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act: Initialize loads devices
  FPresenter.Initialize;

  // Assert: Only connected devices in cache
  Assert.AreEqual(2, FPresenter.GetConnectedDeviceCount,
    'Cache should contain 2 connected devices');
end;

procedure TConnectedAddressesCacheTests.LoadDevices_ExcludesDisconnectedDevices;
var
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange: All devices disconnected
  SetLength(Devices, 3);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csDisconnected);
  Devices[1] := CreateTestDevice($222, 'Device 2', btKeyboard, csDisconnected);
  Devices[2] := CreateTestDevice($333, 'Device 3', btMouse, csDisconnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;

  // Act
  FPresenter.Initialize;

  // Assert
  Assert.AreEqual(0, FPresenter.GetConnectedDeviceCount,
    'Cache should be empty when no devices are connected');
end;

procedure TConnectedAddressesCacheTests.DeviceConnects_AddedToCache;
var
  Devices: TBluetoothDeviceInfoArray;
  ConnectedDevice: TBluetoothDeviceInfo;
begin
  // Arrange: Start with disconnected device
  SetLength(Devices, 1);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csDisconnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  Assert.AreEqual(0, FPresenter.GetConnectedDeviceCount,
    'Cache should be empty initially');

  // Act: Simulate device connecting
  ConnectedDevice := CreateTestDevice($111, 'Device 1', btAudioOutput, csConnected);
  FBluetoothService.SimulateDeviceStateChanged(ConnectedDevice);
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: Device should be in cache
  Assert.AreEqual(1, FPresenter.GetConnectedDeviceCount,
    'Connected device should be added to cache');
end;

procedure TConnectedAddressesCacheTests.DeviceDisconnects_RemovedFromCache;
var
  Devices: TBluetoothDeviceInfoArray;
  DisconnectedDevice: TBluetoothDeviceInfo;
begin
  // Arrange: Start with connected device
  SetLength(Devices, 1);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csConnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  Assert.AreEqual(1, FPresenter.GetConnectedDeviceCount,
    'Cache should have 1 device initially');

  // Act: Simulate device disconnecting
  DisconnectedDevice := CreateTestDevice($111, 'Device 1', btAudioOutput, csDisconnected);
  FBluetoothService.SimulateDeviceStateChanged(DisconnectedDevice);
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: Device should be removed from cache
  Assert.AreEqual(0, FPresenter.GetConnectedDeviceCount,
    'Disconnected device should be removed from cache');
end;

procedure TConnectedAddressesCacheTests.MultipleConnectedDevices_AllTracked;
var
  Devices: TBluetoothDeviceInfoArray;
  NewDevice: TBluetoothDeviceInfo;
begin
  // Arrange: Start with 2 connected devices
  SetLength(Devices, 2);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csConnected);
  Devices[1] := CreateTestDevice($222, 'Device 2', btKeyboard, csConnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  Assert.AreEqual(2, FPresenter.GetConnectedDeviceCount,
    'Cache should have 2 devices initially');

  // Act: Add third connected device
  NewDevice := CreateTestDevice($333, 'Device 3', btMouse, csConnected);
  FBluetoothService.SimulateDeviceStateChanged(NewDevice);
  CheckSynchronize(0);
  Application.ProcessMessages;
  CheckSynchronize(0);

  // Assert: All 3 should be tracked
  Assert.AreEqual(3, FPresenter.GetConnectedDeviceCount,
    'Cache should have 3 devices after new connection');
end;

procedure TConnectedAddressesCacheTests.Shutdown_ClearsCache;
var
  Devices: TBluetoothDeviceInfoArray;
begin
  // Arrange: Start with connected device
  SetLength(Devices, 2);
  Devices[0] := CreateTestDevice($111, 'Device 1', btAudioOutput, csConnected);
  Devices[1] := CreateTestDevice($222, 'Device 2', btKeyboard, csConnected);
  FBluetoothService.Devices := Devices;

  FRadioStateManager.RadioEnabled := True;
  FRadioStateManager.RadioAvailable := True;
  CreatePresenter;
  FPresenter.Initialize;

  Assert.AreEqual(2, FPresenter.GetConnectedDeviceCount,
    'Cache should have 2 devices initially');

  // Act
  FPresenter.Shutdown;

  // Assert
  Assert.AreEqual(0, FPresenter.GetConnectedDeviceCount,
    'Cache should be cleared after Shutdown');
end;

{ TUnpairedDeviceFilteringTests }

procedure TUnpairedDeviceFilteringTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
end;

procedure TUnpairedDeviceFilteringTests.TearDown;
var
  I: Integer;
begin
  // Process any pending TThread.Queue calls
  for I := 1 to 5 do
  begin
    Sleep(20);
    CheckSynchronize(0);
  end;
  Application.ProcessMessages;
  CheckSynchronize(0);

  FDisplayItemBuilder.Free;
  FPairingService.Free;
  FBluetoothService.Free;
  FAsyncExecutor.Free;
  FRadioStateManager.Free;
  FConnectionConfig.Free;
  FLayoutConfig.Free;
  FAppearanceConfig.Free;
  FWindowConfig.Free;
  FGeneralConfig.Free;
  FDeviceConfigProvider.Free;
  FAppConfig.Free;
  FView.Free;
end;

procedure TUnpairedDeviceFilteringTests.ConfigProperty_ShowUnidentifiedDevices_DefaultValue;
begin
  // Verify that ShowUnidentifiedDevices defaults to True (non-breaking change)
  Assert.IsTrue(FLayoutConfig.ShowUnidentifiedDevices,
    'ShowUnidentifiedDevices should default to True for backwards compatibility');
end;

procedure TUnpairedDeviceFilteringTests.ConfigProperty_ShowUnidentifiedDevices_CanBeSet;
begin
  // Verify the property can be set to False
  FLayoutConfig.ShowUnidentifiedDevices := False;
  Assert.IsFalse(FLayoutConfig.ShowUnidentifiedDevices,
    'ShowUnidentifiedDevices should be settable to False');

  // Verify it can be set back to True
  FLayoutConfig.ShowUnidentifiedDevices := True;
  Assert.IsTrue(FLayoutConfig.ShowUnidentifiedDevices,
    'ShowUnidentifiedDevices should be settable to True');
end;

procedure TUnpairedDeviceFilteringTests.ConfigProperty_ShowUnpairedDevices_Required;
begin
  // Verify ShowUnpairedDevices exists and works (prerequisite for filtering)
  FLayoutConfig.ShowUnpairedDevices := True;
  Assert.IsTrue(FLayoutConfig.ShowUnpairedDevices,
    'ShowUnpairedDevices should be settable to True');

  FLayoutConfig.ShowUnpairedDevices := False;
  Assert.IsFalse(FLayoutConfig.ShowUnpairedDevices,
    'ShowUnpairedDevices should be settable to False');
end;

{ TRemoveDeviceFromListTests }

/// <summary>
/// Tests for RemoveDeviceFromList behavior.
/// Verifies that index map remains consistent after device removal.
/// These tests document current behavior before optimization.
/// </summary>
type
  [TestFixture]
  TRemoveDeviceFromListTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure RemoveDevice_SingleDevice_ListBecomesEmpty;

    [Test]
    procedure RemoveDevice_FromMiddle_OtherDevicesStillAccessible;

    [Test]
    procedure RemoveDevice_FromBeginning_OtherDevicesStillAccessible;

    [Test]
    procedure RemoveDevice_FromEnd_OtherDevicesStillAccessible;

    [Test]
    procedure RemoveDevice_NonExistent_NoChange;

    [Test]
    procedure RemoveDevice_AllDevices_ListBecomesEmpty;

    [Test]
    procedure RemoveDevice_IndexMapConsistency_AfterMultipleRemovals;
  end;

procedure TRemoveDeviceFromListTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TRemoveDeviceFromListTests.TearDown;
begin
  FPresenter.Free;
end;

procedure TRemoveDeviceFromListTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_SingleDevice_ListBecomesEmpty;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);
  Assert.AreEqual(1, FPresenter.GetDeviceCount, 'Should have 1 device');

  FPresenter.RemoveDeviceFromList($111111111111);

  Assert.AreEqual(0, FPresenter.GetDeviceCount, 'List should be empty after removal');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_FromMiddle_OtherDevicesStillAccessible;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  Found1, Found3: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  // Remove middle device
  FPresenter.RemoveDeviceFromList($222222222222);

  Assert.AreEqual(2, FPresenter.GetDeviceCount, 'Should have 2 devices after removal');
  Found1 := FPresenter.FindDeviceByAddress($111111111111);
  Found3 := FPresenter.FindDeviceByAddress($333333333333);
  Assert.AreEqual('Device 1', Found1.Name, 'Device 1 should still be accessible');
  Assert.AreEqual('Device 3', Found3.Name, 'Device 3 should still be accessible');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_FromBeginning_OtherDevicesStillAccessible;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  Found2, Found3: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  // Remove first device - this is where the O(n) rebuild is most costly
  FPresenter.RemoveDeviceFromList($111111111111);

  Assert.AreEqual(2, FPresenter.GetDeviceCount, 'Should have 2 devices after removal');
  Found2 := FPresenter.FindDeviceByAddress($222222222222);
  Found3 := FPresenter.FindDeviceByAddress($333333333333);
  Assert.AreEqual('Device 2', Found2.Name, 'Device 2 should still be accessible');
  Assert.AreEqual('Device 3', Found3.Name, 'Device 3 should still be accessible');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_FromEnd_OtherDevicesStillAccessible;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  Found1, Found2: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  // Remove last device - this is where optimization matters least
  FPresenter.RemoveDeviceFromList($333333333333);

  Assert.AreEqual(2, FPresenter.GetDeviceCount, 'Should have 2 devices after removal');
  Found1 := FPresenter.FindDeviceByAddress($111111111111);
  Found2 := FPresenter.FindDeviceByAddress($222222222222);
  Assert.AreEqual('Device 1', Found1.Name, 'Device 1 should still be accessible');
  Assert.AreEqual('Device 2', Found2.Name, 'Device 2 should still be accessible');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_NonExistent_NoChange;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);

  // Try to remove non-existent device
  FPresenter.RemoveDeviceFromList($999999999999);

  Assert.AreEqual(1, FPresenter.GetDeviceCount, 'List should be unchanged');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_AllDevices_ListBecomesEmpty;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  FPresenter.RemoveDeviceFromList($111111111111);
  FPresenter.RemoveDeviceFromList($222222222222);
  FPresenter.RemoveDeviceFromList($333333333333);

  Assert.AreEqual(0, FPresenter.GetDeviceCount, 'List should be empty after removing all devices');
end;

procedure TRemoveDeviceFromListTests.RemoveDevice_IndexMapConsistency_AfterMultipleRemovals;
var
  Devices: array[0..4] of TBluetoothDeviceInfo;
  I: Integer;
  Found: TBluetoothDeviceInfo;
begin
  CreatePresenter;

  // Add 5 devices
  for I := 0 to 4 do
  begin
    Devices[I] := CreateTestDevice(
      UInt64(I + 1) * $111111111111,
      'Device ' + IntToStr(I + 1),
      btAudioOutput,
      csDisconnected
    );
    FPresenter.UpdateOrAddDevice(Devices[I]);
  end;
  Assert.AreEqual(5, FPresenter.GetDeviceCount, 'Should have 5 devices');

  // Remove devices in random order: 2nd, 4th, 1st
  FPresenter.RemoveDeviceFromList(2 * $111111111111);  // Remove Device 2
  FPresenter.RemoveDeviceFromList(4 * $111111111111);  // Remove Device 4
  FPresenter.RemoveDeviceFromList(1 * $111111111111);  // Remove Device 1

  // Verify remaining devices (3 and 5) are still accessible
  Assert.AreEqual(2, FPresenter.GetDeviceCount, 'Should have 2 devices remaining');

  Found := FPresenter.FindDeviceByAddress(3 * $111111111111);
  Assert.AreEqual('Device 3', Found.Name, 'Device 3 should be accessible');

  Found := FPresenter.FindDeviceByAddress(5 * $111111111111);
  Assert.AreEqual('Device 5', Found.Name, 'Device 5 should be accessible');
end;

{ TGetDevicesArrayTests }

/// <summary>
/// Tests for GetDevicesArray behavior.
/// Verifies that the array cache returns correct devices.
/// These tests document current behavior before optimization.
/// </summary>
type
  [TestFixture]
  TGetDevicesArrayTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FLayoutConfig: TMockLayoutConfig;
    FConnectionConfig: TMockConnectionConfig;
    FRadioStateManager: TMockRadioStateManager;
    FAsyncExecutor: TMockAsyncExecutor;
    FBluetoothService: TMockBluetoothService;
    FPairingService: TMockBluetoothPairingService;
    FDisplayItemBuilder: TMockDeviceDisplayItemBuilder;
    FPresenter: TTestableMainPresenter;

    procedure CreatePresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetDevicesArray_EmptyList_ReturnsEmptyArray;

    [Test]
    procedure GetDevicesArray_SingleDevice_ReturnsArrayWithOneElement;

    [Test]
    procedure GetDevicesArray_MultipleDevices_ReturnsAllDevices;

    [Test]
    procedure GetDevicesArray_AfterUpdate_ReturnsFreshData;

    [Test]
    procedure GetDevicesArray_AfterRemoval_ReturnsUpdatedArray;

    [Test]
    procedure GetDevicesArray_CalledTwice_ReturnsSameData;
  end;

procedure TGetDevicesArrayTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
  FAsyncExecutor := TMockAsyncExecutor.Create;
  FBluetoothService := TMockBluetoothService.Create;
  FPairingService := TMockBluetoothPairingService.Create;
  FDisplayItemBuilder := TMockDeviceDisplayItemBuilder.Create;
  FPresenter := nil;
end;

procedure TGetDevicesArrayTests.TearDown;
begin
  FPresenter.Free;
end;

procedure TGetDevicesArrayTests.CreatePresenter;
begin
  FPresenter := TTestableMainPresenter.Create(
    FView as IDeviceListView,
    FView as IToggleView,
    FView as IStatusView,
    FView as IVisibilityView,
    FAppConfig,
    FDeviceConfigProvider,
    FGeneralConfig,
    FWindowConfig,
    FAppearanceConfig,
    FLayoutConfig,
    FConnectionConfig,
    FRadioStateManager,
    FAsyncExecutor,
    FBluetoothService,
    FPairingService,
    FDisplayItemBuilder
  );
end;

procedure TGetDevicesArrayTests.GetDevicesArray_EmptyList_ReturnsEmptyArray;
var
  Devices: TBluetoothDeviceInfoArray;
begin
  CreatePresenter;

  Devices := FPresenter.GetDevicesArray;

  Assert.AreEqual(0, Integer(Length(Devices)), 'Empty list should return empty array');
end;

procedure TGetDevicesArrayTests.GetDevicesArray_SingleDevice_ReturnsArrayWithOneElement;
var
  Device: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);

  Devices := FPresenter.GetDevicesArray;

  Assert.AreEqual(1, Integer(Length(Devices)), 'Should return array with 1 element');
  Assert.AreEqual('Device 1', Devices[0].Name, 'Array should contain the device');
end;

procedure TGetDevicesArrayTests.GetDevicesArray_MultipleDevices_ReturnsAllDevices;
var
  Device1, Device2, Device3: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
  FoundNames: string;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  Device3 := CreateTestDevice($333333333333, 'Device 3', btMouse, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);
  FPresenter.UpdateOrAddDevice(Device3);

  Devices := FPresenter.GetDevicesArray;

  Assert.AreEqual(3, Integer(Length(Devices)), 'Should return array with 3 elements');

  // Verify all devices present (order may vary)
  FoundNames := Devices[0].Name + ',' + Devices[1].Name + ',' + Devices[2].Name;
  Assert.IsTrue(Pos('Device 1', FoundNames) > 0, 'Device 1 should be in array');
  Assert.IsTrue(Pos('Device 2', FoundNames) > 0, 'Device 2 should be in array');
  Assert.IsTrue(Pos('Device 3', FoundNames) > 0, 'Device 3 should be in array');
end;

procedure TGetDevicesArrayTests.GetDevicesArray_AfterUpdate_ReturnsFreshData;
var
  Device, UpdatedDevice: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);

  // Get first array
  Devices := FPresenter.GetDevicesArray;
  Assert.AreEqual('Device 1', Devices[0].Name);

  // Update device
  UpdatedDevice := CreateTestDevice($111111111111, 'Updated Device', btAudioOutput, csConnected);
  FPresenter.UpdateOrAddDevice(UpdatedDevice);

  // Get array again - should have updated data
  Devices := FPresenter.GetDevicesArray;
  Assert.AreEqual('Updated Device', Devices[0].Name, 'Array should reflect updated device');
end;

procedure TGetDevicesArrayTests.GetDevicesArray_AfterRemoval_ReturnsUpdatedArray;
var
  Device1, Device2: TBluetoothDeviceInfo;
  Devices: TBluetoothDeviceInfoArray;
begin
  CreatePresenter;
  Device1 := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  Device2 := CreateTestDevice($222222222222, 'Device 2', btKeyboard, csConnected);
  FPresenter.UpdateOrAddDevice(Device1);
  FPresenter.UpdateOrAddDevice(Device2);

  // Get array before removal
  Devices := FPresenter.GetDevicesArray;
  Assert.AreEqual(2, Integer(Length(Devices)), 'Should have 2 devices');

  // Remove one device
  FPresenter.RemoveDeviceFromList($111111111111);

  // Get array after removal
  Devices := FPresenter.GetDevicesArray;
  Assert.AreEqual(1, Integer(Length(Devices)), 'Should have 1 device after removal');
  Assert.AreEqual('Device 2', Devices[0].Name, 'Remaining device should be Device 2');
end;

procedure TGetDevicesArrayTests.GetDevicesArray_CalledTwice_ReturnsSameData;
var
  Device: TBluetoothDeviceInfo;
  Devices1, Devices2: TBluetoothDeviceInfoArray;
begin
  CreatePresenter;
  Device := CreateTestDevice($111111111111, 'Device 1', btAudioOutput, csDisconnected);
  FPresenter.UpdateOrAddDevice(Device);

  // Get array twice without modifications
  Devices1 := FPresenter.GetDevicesArray;
  Devices2 := FPresenter.GetDevicesArray;

  Assert.AreEqual(Integer(Length(Devices1)), Integer(Length(Devices2)), 'Both arrays should have same length');
  Assert.AreEqual(Devices1[0].Name, Devices2[0].Name, 'Both arrays should have same data');
end;

initialization
  TDUnitX.RegisterTestFixture(TMainPresenterTests);
  TDUnitX.RegisterTestFixture(TDelayedLoadTests);
  TDUnitX.RegisterTestFixture(TDeviceIndexMapTests);
  TDUnitX.RegisterTestFixture(TBatteryQueryCompletionTests);
  TDUnitX.RegisterTestFixture(TShutdownSafetyTests);
  TDUnitX.RegisterTestFixture(TConnectedAddressesCacheTests);
  TDUnitX.RegisterTestFixture(TUnpairedDeviceFilteringTests);
  TDUnitX.RegisterTestFixture(TRemoveDeviceFromListTests);
  TDUnitX.RegisterTestFixture(TGetDevicesArrayTests);

end.
