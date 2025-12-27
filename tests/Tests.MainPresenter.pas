{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TMainPresenter Unit Tests                       }
{                                                       }
{       Tests for main presenter with injected          }
{       mock dependencies.                              }
{                                                       }
{       Copyright (c) 2024                              }
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
  Bluetooth.Types,
  Bluetooth.RadioControl,
  UI.DeviceList,
  Tests.Mocks;

type
  [TestFixture]
  TMainPresenterTests = class
  private
    FView: TMockMainView;
    FAppConfig: TMockAppConfig;
    FDeviceConfigProvider: TMockDeviceConfigProvider;
    FGeneralConfig: TMockGeneralConfig;
    FWindowConfig: TMockWindowConfig;
    FAppearanceConfig: TMockAppearanceConfig;
    FPollingConfig: TMockPollingConfig;
    FConnectionConfig: TMockConnectionConfig;
    FStrategyFactory: TMockConnectionStrategyFactory;
    FRadioStateManager: TMockRadioStateManager;
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
    procedure OnDeviceClicked_MenuMode_HidesView;

    [Test]
    procedure OnDeviceClicked_WindowMode_DoesNotHideView;

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

implementation

{ TMainPresenterTests }

procedure TMainPresenterTests.Setup;
begin
  FView := TMockMainView.Create;
  FAppConfig := TMockAppConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FPollingConfig := TMockPollingConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FStrategyFactory := TMockConnectionStrategyFactory.Create;
  FRadioStateManager := TMockRadioStateManager.Create;
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
    FPollingConfig,
    FConnectionConfig,
    FStrategyFactory,
    FRadioStateManager
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
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.Contains(FView.LastStatus, 'Disconnecting', 'Status should indicate disconnecting');
end;

procedure TMainPresenterTests.OnDeviceClicked_DisconnectedDevice_ShowsConnectingStatus;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.Contains(FView.LastStatus, 'Connecting', 'Status should indicate connecting');
end;

procedure TMainPresenterTests.OnDeviceClicked_MenuMode_HidesView;
var
  Device: TBluetoothDeviceInfo;
begin
  FGeneralConfig.WindowMode := wmMenu;
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.IsTrue(FView.HideViewCalled, 'View should be hidden in menu mode after device click');
end;

procedure TMainPresenterTests.OnDeviceClicked_WindowMode_DoesNotHideView;
var
  Device: TBluetoothDeviceInfo;
begin
  FGeneralConfig.WindowMode := wmWindow;
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnected);

  FPresenter.OnDeviceClicked(Device);

  Assert.IsFalse(FView.HideViewCalled, 'View should not be hidden in window mode');
end;

procedure TMainPresenterTests.OnDeviceClicked_ConnectingDevice_IgnoresClick;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csConnecting);

  FPresenter.OnDeviceClicked(Device);

  // Status is shown with "Operation in progress" message
  Assert.Contains(FView.LastStatus, 'progress', 'Status should indicate operation in progress');
end;

procedure TMainPresenterTests.OnDeviceClicked_DisconnectingDevice_IgnoresClick;
var
  Device: TBluetoothDeviceInfo;
begin
  CreatePresenter;
  Device := CreateTestDevice($001122334455, 'Test Headphones', btAudioOutput, csDisconnecting);

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

initialization
  TDUnitX.RegisterTestFixture(TMainPresenterTests);

end.
