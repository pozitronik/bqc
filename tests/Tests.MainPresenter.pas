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
begin
  FPresenter.Free;
  // Interfaces are reference-counted, will be released automatically
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

initialization
  TDUnitX.RegisterTestFixture(TMainPresenterTests);

end.
