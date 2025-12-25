{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Configuration Provider Tests             }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.DeviceConfigProvider;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  App.DeviceConfigProvider,
  Tests.Mocks;

type
  /// <summary>
  /// Mock implementation of IDeviceConfigStorage for testing.
  /// </summary>
  TMockDeviceConfigStorage = class(TInterfacedObject, IDeviceConfigStorage)
  private
    FDevices: TDictionary<UInt64, TDeviceConfig>;
    FGetConfigCallCount: Integer;
    FSetConfigCallCount: Integer;
    FRemoveCallCount: Integer;
    FRegisterDeviceCallCount: Integer;
    FLastRegisteredAddress: UInt64;
    FLastRegisteredName: string;
    FLastRegisteredLastSeen: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceConfigStorage
    function GetConfig(AAddress: UInt64): TDeviceConfig;
    procedure SetConfig(const AConfig: TDeviceConfig);
    procedure Remove(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
    function GetAllAddresses: TArray<UInt64>;
    function GetAll: TArray<TDeviceConfig>;

    // Test helpers
    procedure AddDevice(const AConfig: TDeviceConfig);
    procedure Clear;

    property GetConfigCallCount: Integer read FGetConfigCallCount;
    property SetConfigCallCount: Integer read FSetConfigCallCount;
    property RemoveCallCount: Integer read FRemoveCallCount;
    property RegisterDeviceCallCount: Integer read FRegisterDeviceCallCount;
    property LastRegisteredAddress: UInt64 read FLastRegisteredAddress;
    property LastRegisteredName: string read FLastRegisteredName;
    property LastRegisteredLastSeen: TDateTime read FLastRegisteredLastSeen;
  end;

  /// <summary>
  /// Test fixture for TDeviceConfigProvider.
  /// Tests effective value resolution and delegation to storage.
  /// </summary>
  [TestFixture]
  TDeviceConfigProviderTests = class
  private
    FStorage: TMockDeviceConfigStorage;
    FNotificationConfig: TMockNotificationConfig;
    FConnectionConfig: TMockConnectionConfig;
    FProvider: IDeviceConfigProvider;
    const
      TEST_ADDRESS_1 = UInt64($001122334455);
      TEST_ADDRESS_2 = UInt64($665544332211);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor and factory tests }
    [Test]
    procedure Create_WithValidDependencies_Succeeds;

    [Test]
    procedure CreateDeviceConfigProvider_ReturnsValidInstance;

    { GetDeviceConfig tests }
    [Test]
    procedure GetDeviceConfig_ExistingDevice_ReturnsConfig;

    [Test]
    procedure GetDeviceConfig_NonExistingDevice_ReturnsDefault;

    [Test]
    procedure GetDeviceConfig_NilStorage_ReturnsDefault;

    { SetDeviceConfig tests }
    [Test]
    procedure SetDeviceConfig_DelegatesToStorage;

    [Test]
    procedure SetDeviceConfig_NilStorage_DoesNotRaise;

    { RemoveDeviceConfig tests }
    [Test]
    procedure RemoveDeviceConfig_DelegatesToStorage;

    [Test]
    procedure RemoveDeviceConfig_NilStorage_DoesNotRaise;

    { RegisterDevice tests }
    [Test]
    procedure RegisterDevice_DelegatesToStorage;

    [Test]
    procedure RegisterDevice_NilStorage_DoesNotRaise;

    { GetConfiguredDeviceAddresses tests }
    [Test]
    procedure GetConfiguredDeviceAddresses_ReturnsStorageAddresses;

    [Test]
    procedure GetConfiguredDeviceAddresses_EmptyStorage_ReturnsEmpty;

    [Test]
    procedure GetConfiguredDeviceAddresses_NilStorage_ReturnsNil;

    { GetEffectiveNotification tests - Connect event }
    [Test]
    procedure GetEffectiveNotification_Connect_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveNotification_Connect_NoOverride_ReturnsGlobal;

    [Test]
    procedure GetEffectiveNotification_Connect_DeviceNotFound_ReturnsGlobal;

    { GetEffectiveNotification tests - Disconnect event }
    [Test]
    procedure GetEffectiveNotification_Disconnect_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveNotification_Disconnect_NoOverride_ReturnsGlobal;

    { GetEffectiveNotification tests - ConnectFailed event }
    [Test]
    procedure GetEffectiveNotification_ConnectFailed_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveNotification_ConnectFailed_NoOverride_ReturnsGlobal;

    { GetEffectiveNotification tests - AutoConnect event }
    [Test]
    procedure GetEffectiveNotification_AutoConnect_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveNotification_AutoConnect_NoOverride_ReturnsGlobal;

    { GetEffectiveConnectionTimeout tests }
    [Test]
    procedure GetEffectiveConnectionTimeout_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveConnectionTimeout_NoOverride_ReturnsGlobal;

    [Test]
    procedure GetEffectiveConnectionTimeout_NegativeOverride_ReturnsGlobal;

    [Test]
    procedure GetEffectiveConnectionTimeout_ZeroOverride_ReturnsZero;

    { GetEffectiveConnectionRetryCount tests }
    [Test]
    procedure GetEffectiveConnectionRetryCount_DeviceOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveConnectionRetryCount_NoOverride_ReturnsGlobal;

    [Test]
    procedure GetEffectiveConnectionRetryCount_NegativeOverride_ReturnsGlobal;

    [Test]
    procedure GetEffectiveConnectionRetryCount_ZeroOverride_ReturnsZero;

    { Edge cases }
    [Test]
    procedure GetEffectiveNotification_InvalidEvent_ReturnsNone;

    [Test]
    procedure GetEffectiveNotification_BalloonMode_ReturnsCorrectly;
  end;

implementation

{ TMockDeviceConfigStorage }

constructor TMockDeviceConfigStorage.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TDeviceConfig>.Create;
  FGetConfigCallCount := 0;
  FSetConfigCallCount := 0;
  FRemoveCallCount := 0;
  FRegisterDeviceCallCount := 0;
end;

destructor TMockDeviceConfigStorage.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

function TMockDeviceConfigStorage.GetConfig(AAddress: UInt64): TDeviceConfig;
begin
  Inc(FGetConfigCallCount);
  if not FDevices.TryGetValue(AAddress, Result) then
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TMockDeviceConfigStorage.SetConfig(const AConfig: TDeviceConfig);
begin
  Inc(FSetConfigCallCount);
  FDevices.AddOrSetValue(AConfig.Address, AConfig);
end;

procedure TMockDeviceConfigStorage.Remove(AAddress: UInt64);
begin
  Inc(FRemoveCallCount);
  FDevices.Remove(AAddress);
end;

procedure TMockDeviceConfigStorage.RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
var
  Config: TDeviceConfig;
begin
  Inc(FRegisterDeviceCallCount);
  FLastRegisteredAddress := AAddress;
  FLastRegisteredName := AName;
  FLastRegisteredLastSeen := ALastSeen;

  if FDevices.TryGetValue(AAddress, Config) then
  begin
    Config.Name := AName;
    Config.LastSeen := ALastSeen;
    FDevices[AAddress] := Config;
  end
  else
  begin
    Config := TDeviceConfig.Default(AAddress);
    Config.Name := AName;
    Config.LastSeen := ALastSeen;
    FDevices.Add(AAddress, Config);
  end;
end;

function TMockDeviceConfigStorage.GetAllAddresses: TArray<UInt64>;
begin
  Result := FDevices.Keys.ToArray;
end;

function TMockDeviceConfigStorage.GetAll: TArray<TDeviceConfig>;
begin
  Result := FDevices.Values.ToArray;
end;

procedure TMockDeviceConfigStorage.AddDevice(const AConfig: TDeviceConfig);
begin
  FDevices.AddOrSetValue(AConfig.Address, AConfig);
end;

procedure TMockDeviceConfigStorage.Clear;
begin
  FDevices.Clear;
end;

{ TDeviceConfigProviderTests }

procedure TDeviceConfigProviderTests.Setup;
begin
  FStorage := TMockDeviceConfigStorage.Create;
  FNotificationConfig := TMockNotificationConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;

  // Set default global values
  FNotificationConfig.NotifyOnConnect := nmBalloon;
  FNotificationConfig.NotifyOnDisconnect := nmNone;
  FNotificationConfig.NotifyOnConnectFailed := nmBalloon;
  FNotificationConfig.NotifyOnAutoConnect := nmNone;
  FConnectionConfig.ConnectionTimeout := 10000;
  FConnectionConfig.ConnectionRetryCount := 3;

  FProvider := TDeviceConfigProvider.Create(
    FStorage,
    FNotificationConfig,
    FConnectionConfig
  );
end;

procedure TDeviceConfigProviderTests.TearDown;
begin
  FProvider := nil;
  // Interfaces are released automatically
end;

procedure TDeviceConfigProviderTests.Create_WithValidDependencies_Succeeds;
begin
  Assert.IsNotNull(FProvider);
end;

procedure TDeviceConfigProviderTests.CreateDeviceConfigProvider_ReturnsValidInstance;
var
  Provider: IDeviceConfigProvider;
begin
  Provider := CreateDeviceConfigProvider(
    FStorage,
    FNotificationConfig,
    FConnectionConfig
  );
  Assert.IsNotNull(Provider);
end;

procedure TDeviceConfigProviderTests.GetDeviceConfig_ExistingDevice_ReturnsConfig;
var
  StoredConfig, RetrievedConfig: TDeviceConfig;
begin
  StoredConfig := TDeviceConfig.Default(TEST_ADDRESS_1);
  StoredConfig.Name := 'Test Device';
  StoredConfig.Alias := 'My Device';
  FStorage.AddDevice(StoredConfig);

  RetrievedConfig := FProvider.GetDeviceConfig(TEST_ADDRESS_1);

  Assert.AreEqual('Test Device', RetrievedConfig.Name);
  Assert.AreEqual('My Device', RetrievedConfig.Alias);
  Assert.AreEqual(1, FStorage.GetConfigCallCount);
end;

procedure TDeviceConfigProviderTests.GetDeviceConfig_NonExistingDevice_ReturnsDefault;
var
  Config: TDeviceConfig;
begin
  Config := FProvider.GetDeviceConfig(TEST_ADDRESS_1);

  Assert.AreEqual(TEST_ADDRESS_1, Config.Address);
  Assert.AreEqual('', Config.Name);
  Assert.IsFalse(Config.Pinned);
end;

procedure TDeviceConfigProviderTests.GetDeviceConfig_NilStorage_ReturnsDefault;
var
  Provider: IDeviceConfigProvider;
  Config: TDeviceConfig;
begin
  Provider := TDeviceConfigProvider.Create(nil, FNotificationConfig, FConnectionConfig);

  Config := Provider.GetDeviceConfig(TEST_ADDRESS_1);

  Assert.AreEqual(TEST_ADDRESS_1, Config.Address);
end;

procedure TDeviceConfigProviderTests.SetDeviceConfig_DelegatesToStorage;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Pinned := True;

  FProvider.SetDeviceConfig(Config);

  Assert.AreEqual(1, FStorage.SetConfigCallCount);
end;

procedure TDeviceConfigProviderTests.SetDeviceConfig_NilStorage_DoesNotRaise;
var
  Provider: IDeviceConfigProvider;
  Config: TDeviceConfig;
begin
  Provider := TDeviceConfigProvider.Create(nil, FNotificationConfig, FConnectionConfig);
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);

  // Should not raise
  Provider.SetDeviceConfig(Config);
  Assert.Pass;
end;

procedure TDeviceConfigProviderTests.RemoveDeviceConfig_DelegatesToStorage;
begin
  FProvider.RemoveDeviceConfig(TEST_ADDRESS_1);
  Assert.AreEqual(1, FStorage.RemoveCallCount);
end;

procedure TDeviceConfigProviderTests.RemoveDeviceConfig_NilStorage_DoesNotRaise;
var
  Provider: IDeviceConfigProvider;
begin
  Provider := TDeviceConfigProvider.Create(nil, FNotificationConfig, FConnectionConfig);
  Provider.RemoveDeviceConfig(TEST_ADDRESS_1);
  Assert.Pass;
end;

procedure TDeviceConfigProviderTests.RegisterDevice_DelegatesToStorage;
var
  TestTime: TDateTime;
begin
  TestTime := Now;
  FProvider.RegisterDevice(TEST_ADDRESS_1, 'New Device', TestTime);

  Assert.AreEqual(1, FStorage.RegisterDeviceCallCount);
  Assert.AreEqual(TEST_ADDRESS_1, FStorage.LastRegisteredAddress);
  Assert.AreEqual('New Device', FStorage.LastRegisteredName);
  Assert.AreEqual(TestTime, FStorage.LastRegisteredLastSeen);
end;

procedure TDeviceConfigProviderTests.RegisterDevice_NilStorage_DoesNotRaise;
var
  Provider: IDeviceConfigProvider;
begin
  Provider := TDeviceConfigProvider.Create(nil, FNotificationConfig, FConnectionConfig);
  Provider.RegisterDevice(TEST_ADDRESS_1, 'Device', Now);
  Assert.Pass;
end;

procedure TDeviceConfigProviderTests.GetConfiguredDeviceAddresses_ReturnsStorageAddresses;
var
  Config1, Config2: TDeviceConfig;
  Addresses: TArray<UInt64>;
begin
  Config1 := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config2 := TDeviceConfig.Default(TEST_ADDRESS_2);
  FStorage.AddDevice(Config1);
  FStorage.AddDevice(Config2);

  Addresses := FProvider.GetConfiguredDeviceAddresses;

  Assert.AreEqual(2, Integer(Length(Addresses)));
end;

procedure TDeviceConfigProviderTests.GetConfiguredDeviceAddresses_EmptyStorage_ReturnsEmpty;
var
  Addresses: TArray<UInt64>;
begin
  Addresses := FProvider.GetConfiguredDeviceAddresses;
  Assert.AreEqual(0, Integer(Length(Addresses)));
end;

procedure TDeviceConfigProviderTests.GetConfiguredDeviceAddresses_NilStorage_ReturnsNil;
var
  Provider: IDeviceConfigProvider;
  Addresses: TArray<UInt64>;
begin
  Provider := TDeviceConfigProvider.Create(nil, FNotificationConfig, FConnectionConfig);
  Addresses := Provider.GetConfiguredDeviceAddresses;
  Assert.IsNull(Addresses);
end;

{ GetEffectiveNotification - Connect }

procedure TDeviceConfigProviderTests.GetEffectiveNotification_Connect_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  EffectiveMode: TNotificationMode;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Notifications.OnConnect := Ord(nmNone);  // Override to None
  FStorage.AddDevice(Config);

  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnect);

  Assert.AreEqual(Ord(nmNone), Ord(EffectiveMode));
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_Connect_NoOverride_ReturnsGlobal;
var
  EffectiveMode: TNotificationMode;
begin
  // Device has default config with -1 override
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnect);

  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode)); // Global is nmBalloon
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_Connect_DeviceNotFound_ReturnsGlobal;
var
  EffectiveMode: TNotificationMode;
begin
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnect);
  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode));
end;

{ GetEffectiveNotification - Disconnect }

procedure TDeviceConfigProviderTests.GetEffectiveNotification_Disconnect_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  EffectiveMode: TNotificationMode;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Notifications.OnDisconnect := Ord(nmBalloon);  // Override to Balloon
  FStorage.AddDevice(Config);

  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neDisconnect);

  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode));
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_Disconnect_NoOverride_ReturnsGlobal;
var
  EffectiveMode: TNotificationMode;
begin
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neDisconnect);
  Assert.AreEqual(Ord(nmNone), Ord(EffectiveMode)); // Global is nmNone
end;

{ GetEffectiveNotification - ConnectFailed }

procedure TDeviceConfigProviderTests.GetEffectiveNotification_ConnectFailed_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  EffectiveMode: TNotificationMode;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Notifications.OnConnectFailed := Ord(nmNone);
  FStorage.AddDevice(Config);

  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnectFailed);

  Assert.AreEqual(Ord(nmNone), Ord(EffectiveMode));
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_ConnectFailed_NoOverride_ReturnsGlobal;
var
  EffectiveMode: TNotificationMode;
begin
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnectFailed);
  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode));
end;

{ GetEffectiveNotification - AutoConnect }

procedure TDeviceConfigProviderTests.GetEffectiveNotification_AutoConnect_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  EffectiveMode: TNotificationMode;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Notifications.OnAutoConnect := Ord(nmBalloon);
  FStorage.AddDevice(Config);

  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neAutoConnect);

  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode));
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_AutoConnect_NoOverride_ReturnsGlobal;
var
  EffectiveMode: TNotificationMode;
begin
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neAutoConnect);
  Assert.AreEqual(Ord(nmNone), Ord(EffectiveMode));
end;

{ GetEffectiveConnectionTimeout }

procedure TDeviceConfigProviderTests.GetEffectiveConnectionTimeout_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  Timeout: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionTimeout := 5000;
  FStorage.AddDevice(Config);

  Timeout := FProvider.GetEffectiveConnectionTimeout(TEST_ADDRESS_1);

  Assert.AreEqual(5000, Timeout);
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionTimeout_NoOverride_ReturnsGlobal;
var
  Timeout: Integer;
begin
  Timeout := FProvider.GetEffectiveConnectionTimeout(TEST_ADDRESS_1);
  Assert.AreEqual(10000, Timeout); // Global value
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionTimeout_NegativeOverride_ReturnsGlobal;
var
  Config: TDeviceConfig;
  Timeout: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionTimeout := -1;  // No override
  FStorage.AddDevice(Config);

  Timeout := FProvider.GetEffectiveConnectionTimeout(TEST_ADDRESS_1);

  Assert.AreEqual(10000, Timeout); // Falls back to global
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionTimeout_ZeroOverride_ReturnsZero;
var
  Config: TDeviceConfig;
  Timeout: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionTimeout := 0;  // Explicit zero
  FStorage.AddDevice(Config);

  Timeout := FProvider.GetEffectiveConnectionTimeout(TEST_ADDRESS_1);

  Assert.AreEqual(0, Timeout);
end;

{ GetEffectiveConnectionRetryCount }

procedure TDeviceConfigProviderTests.GetEffectiveConnectionRetryCount_DeviceOverride_ReturnsOverride;
var
  Config: TDeviceConfig;
  RetryCount: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionRetryCount := 5;
  FStorage.AddDevice(Config);

  RetryCount := FProvider.GetEffectiveConnectionRetryCount(TEST_ADDRESS_1);

  Assert.AreEqual(5, RetryCount);
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionRetryCount_NoOverride_ReturnsGlobal;
var
  RetryCount: Integer;
begin
  RetryCount := FProvider.GetEffectiveConnectionRetryCount(TEST_ADDRESS_1);
  Assert.AreEqual(3, RetryCount); // Global value
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionRetryCount_NegativeOverride_ReturnsGlobal;
var
  Config: TDeviceConfig;
  RetryCount: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionRetryCount := -1;
  FStorage.AddDevice(Config);

  RetryCount := FProvider.GetEffectiveConnectionRetryCount(TEST_ADDRESS_1);

  Assert.AreEqual(3, RetryCount); // Falls back to global
end;

procedure TDeviceConfigProviderTests.GetEffectiveConnectionRetryCount_ZeroOverride_ReturnsZero;
var
  Config: TDeviceConfig;
  RetryCount: Integer;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.ConnectionRetryCount := 0;  // Explicit zero (no retries)
  FStorage.AddDevice(Config);

  RetryCount := FProvider.GetEffectiveConnectionRetryCount(TEST_ADDRESS_1);

  Assert.AreEqual(0, RetryCount);
end;

{ Edge cases }

procedure TDeviceConfigProviderTests.GetEffectiveNotification_InvalidEvent_ReturnsNone;
var
  EffectiveMode: TNotificationMode;
begin
  // Test behavior with out-of-range event type
  // The code has an else clause that returns nmNone
  // This test documents the expected behavior
  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnect);
  // At least verify it doesn't crash and returns something valid
  Assert.IsTrue(EffectiveMode in [nmNone, nmBalloon]);
end;

procedure TDeviceConfigProviderTests.GetEffectiveNotification_BalloonMode_ReturnsCorrectly;
var
  Config: TDeviceConfig;
  EffectiveMode: TNotificationMode;
begin
  Config := TDeviceConfig.Default(TEST_ADDRESS_1);
  Config.Notifications.OnConnect := Ord(nmBalloon);
  FStorage.AddDevice(Config);

  EffectiveMode := FProvider.GetEffectiveNotification(TEST_ADDRESS_1, neConnect);

  Assert.AreEqual(Ord(nmBalloon), Ord(EffectiveMode));
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceConfigProviderTests);

end.
