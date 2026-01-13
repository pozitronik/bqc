{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Notification Coordinator Tests           }
{                                                       }
{*******************************************************}

unit Tests.DeviceNotificationCoordinator;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  App.MainViewInterfaces,
  App.ConfigInterfaces,
  App.ConfigEnums,
  App.DeviceConfigTypes,
  App.DeviceNotificationCoordinator,
  Tests.Mocks,
  Tests.Mocks.Config,
  Tests.Mocks.View;

type
  /// <summary>
  /// Tests for TDeviceNotificationCoordinator focusing on
  /// notification display logic based on device state and configuration.
  /// </summary>
  [TestFixture]
  TDeviceNotificationCoordinatorTests = class
  private
    FCoordinator: TDeviceNotificationCoordinator;
    FStatusView: TMockMainView;
    FConfigProvider: TMockDeviceConfigProvider;

    /// <summary>
    /// Creates a test device with specified state.
    /// </summary>
    function CreateDevice(AAddress: UInt64; const AName: string;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;

    /// <summary>
    /// Configures notification settings for a device.
    /// </summary>
    procedure ConfigureNotification(AAddress: UInt64; const AAlias: string;
      AOnConnect, AOnDisconnect, AOnConnectFailed: TNotificationMode);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation tests
    [Test]
    procedure Create_InitializesCorrectly;

    // Display name tests
    [Test]
    procedure ShowNotification_WithAlias_UsesAlias;
    [Test]
    procedure ShowNotification_NoAlias_UsesDeviceName;

    // Connected state tests
    [Test]
    procedure ShowNotification_Connected_BalloonMode_ShowsNotification;
    [Test]
    procedure ShowNotification_Connected_NoneMode_NoNotification;

    // Disconnected state tests
    [Test]
    procedure ShowNotification_Disconnected_BalloonMode_ShowsNotification;
    [Test]
    procedure ShowNotification_Disconnected_NoneMode_NoNotification;

    // Error state tests
    [Test]
    procedure ShowNotification_Error_BalloonMode_ShowsErrorNotification;
    [Test]
    procedure ShowNotification_Error_NoneMode_NoNotification;

    // Non-notifiable states tests
    [Test]
    procedure ShowNotification_Connecting_NoNotification;
    [Test]
    procedure ShowNotification_Disconnecting_NoNotification;
    [Test]
    procedure ShowNotification_Unknown_NoNotification;

    // Notification content tests
    [Test]
    procedure ShowNotification_Connected_CorrectMessage;
    [Test]
    procedure ShowNotification_Disconnected_CorrectMessage;
    [Test]
    procedure ShowNotification_Error_CorrectMessageAndFlags;
  end;

implementation

{ TDeviceNotificationCoordinatorTests }

procedure TDeviceNotificationCoordinatorTests.Setup;
begin
  FStatusView := TMockMainView.Create;
  FConfigProvider := TMockDeviceConfigProvider.Create;
  FCoordinator := TDeviceNotificationCoordinator.Create(
    FStatusView as IStatusView,
    FConfigProvider as IDeviceConfigProvider
  );
end;

procedure TDeviceNotificationCoordinatorTests.TearDown;
begin
  FCoordinator.Free;
  FCoordinator := nil;
  // Interfaces are reference-counted
  FConfigProvider := nil;
  FStatusView := nil;
end;

function TDeviceNotificationCoordinatorTests.CreateDevice(AAddress: UInt64;
  const AName: string; AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
begin
  Result := CreateTestDevice(AAddress, AName, btAudioOutput, AState);
end;

procedure TDeviceNotificationCoordinatorTests.ConfigureNotification(
  AAddress: UInt64; const AAlias: string;
  AOnConnect, AOnDisconnect, AOnConnectFailed: TNotificationMode);
var
  Config: TDeviceConfig;
begin
  Config := Default(TDeviceConfig);
  Config.Address := AAddress;
  Config.Alias := AAlias;
  Config.Notifications.OnConnect := Ord(AOnConnect);
  Config.Notifications.OnDisconnect := Ord(AOnDisconnect);
  Config.Notifications.OnConnectFailed := Ord(AOnConnectFailed);
  FConfigProvider.AddDeviceConfig(AAddress, Config);
end;

{ Creation tests }

procedure TDeviceNotificationCoordinatorTests.Create_InitializesCorrectly;
begin
  Assert.IsNotNull(FCoordinator, 'Coordinator should be created');
end;

{ Display name tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_WithAlias_UsesAlias;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($AABBCCDDEEFF, 'My Headphones', nmBalloon, nmNone, nmNone);
  Device := CreateDevice($AABBCCDDEEFF, 'WH-1000XM4', csConnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(1, FStatusView.ShowNotificationCount, 'Should show notification');
  Assert.AreEqual('My Headphones', FStatusView.LastNotificationTitle,
    'Should use alias as title');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_NoAlias_UsesDeviceName;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($AABBCCDDEEFF, '', nmBalloon, nmNone, nmNone);
  Device := CreateDevice($AABBCCDDEEFF, 'WH-1000XM4', csConnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(1, FStatusView.ShowNotificationCount, 'Should show notification');
  Assert.AreEqual('WH-1000XM4', FStatusView.LastNotificationTitle,
    'Should use device name when no alias');
end;

{ Connected state tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Connected_BalloonMode_ShowsNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmBalloon, nmNone, nmNone);
  Device := CreateDevice($001, 'Device', csConnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(1, FStatusView.ShowNotificationCount, 'Should show notification');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Connected_NoneMode_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmNone, nmNone, nmNone);
  Device := CreateDevice($001, 'Device', csConnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Should NOT show notification when mode is None');
end;

{ Disconnected state tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Disconnected_BalloonMode_ShowsNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmNone, nmBalloon, nmNone);
  Device := CreateDevice($001, 'Device', csDisconnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(1, FStatusView.ShowNotificationCount, 'Should show notification');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Disconnected_NoneMode_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmNone, nmNone, nmNone);
  Device := CreateDevice($001, 'Device', csDisconnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Should NOT show notification when mode is None');
end;

{ Error state tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Error_BalloonMode_ShowsErrorNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmNone, nmNone, nmBalloon);
  Device := CreateDevice($001, 'Device', csError);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(1, FStatusView.ShowNotificationCount, 'Should show notification');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Error_NoneMode_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmNone, nmNone, nmNone);
  Device := CreateDevice($001, 'Device', csError);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Should NOT show notification when mode is None');
end;

{ Non-notifiable states tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Connecting_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmBalloon, nmBalloon, nmBalloon);
  Device := CreateDevice($001, 'Device', csConnecting);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Connecting state should not trigger notification');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Disconnecting_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmBalloon, nmBalloon, nmBalloon);
  Device := CreateDevice($001, 'Device', csDisconnecting);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Disconnecting state should not trigger notification');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Unknown_NoNotification;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'Device', nmBalloon, nmBalloon, nmBalloon);
  Device := CreateDevice($001, 'Device', csUnknown);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual(0, FStatusView.ShowNotificationCount,
    'Unknown state should not trigger notification');
end;

{ Notification content tests }

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Connected_CorrectMessage;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'My Device', nmBalloon, nmNone, nmNone);
  Device := CreateDevice($001, 'My Device', csConnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual('Connected', FStatusView.LastNotificationMessage);
  Assert.AreEqual(Ord(nfInfo), Ord(FStatusView.LastNotificationFlags),
    'Connected should use Info flags');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Disconnected_CorrectMessage;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'My Device', nmNone, nmBalloon, nmNone);
  Device := CreateDevice($001, 'My Device', csDisconnected);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual('Disconnected', FStatusView.LastNotificationMessage);
  Assert.AreEqual(Ord(nfInfo), Ord(FStatusView.LastNotificationFlags),
    'Disconnected should use Info flags');
end;

procedure TDeviceNotificationCoordinatorTests.ShowNotification_Error_CorrectMessageAndFlags;
var
  Device: TBluetoothDeviceInfo;
begin
  ConfigureNotification($001, 'My Device', nmNone, nmNone, nmBalloon);
  Device := CreateDevice($001, 'My Device', csError);

  FCoordinator.ShowNotification(Device);

  Assert.AreEqual('Connection failed', FStatusView.LastNotificationMessage);
  Assert.AreEqual(Ord(nfError), Ord(FStatusView.LastNotificationFlags),
    'Error should use Error flags');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceNotificationCoordinatorTests);

end.
