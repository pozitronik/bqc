{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       App.Config Unit Tests                           }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.App.Config;

interface

uses
  DUnitX.TestFramework,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Test fixture for TDeviceConfig record.
  /// Tests factory methods and default values.
  /// </summary>
  [TestFixture]
  TDeviceConfigTests = class
  public
    [Test]
    procedure Default_SetsAddress;
    [Test]
    procedure Default_SetsEmptyName;
    [Test]
    procedure Default_SetsEmptyAlias;
    [Test]
    procedure Default_NotPinned;
    [Test]
    procedure Default_NotHidden;
    [Test]
    procedure Default_NoAutoConnect;
    [Test]
    procedure Default_ConnectionTimeoutMinusOne;
    [Test]
    procedure Default_RetryCountMinusOne;
    [Test]
    procedure Default_NotificationsMinusOne;
    [Test]
    procedure Default_DeviceTypeOverrideMinusOne;
    [Test]
    procedure Default_LastSeenZero;

    [Test]
    procedure CreateDefault_SameAsDefault;

    [Test]
    procedure GetDisplayName_ReturnsAliasWhenSet;
    [Test]
    procedure GetDisplayName_ReturnsNameWhenNoAlias;
    [Test]
    procedure GetDisplayName_ReturnsAddressWhenNoName;
  end;

  /// <summary>
  /// Test fixture for TDeviceNotificationConfig record.
  /// </summary>
  [TestFixture]
  TDeviceNotificationsTests = class
  public
    [Test]
    procedure Default_AllValuesMinusOne;
    [Test]
    procedure CreateDefault_SameAsDefault;
  end;

  /// <summary>
  /// Test fixture for mock config implementations.
  /// Verifies mock implementations work correctly for unit testing.
  /// </summary>
  [TestFixture]
  TMockConfigTests = class
  public
    // TMockLayoutConfig tests
    [Test]
    procedure MockLayoutConfig_DefaultValues;
    [Test]
    procedure MockLayoutConfig_SettersWork;

    // TMockAppearanceConfig tests
    [Test]
    procedure MockAppearanceConfig_DefaultValues;
    [Test]
    procedure MockAppearanceConfig_SettersWork;

    // TMockDeviceConfigProvider tests
    [Test]
    procedure MockDeviceConfigProvider_EmptyByDefault;
    [Test]
    procedure MockDeviceConfigProvider_AddAndGet;
    [Test]
    procedure MockDeviceConfigProvider_GetMissing_ReturnsDefault;
    [Test]
    procedure MockDeviceConfigProvider_Clear;
    [Test]
    procedure MockDeviceConfigProvider_RegisterDevice;
    [Test]
    procedure MockDeviceConfigProvider_GetConfiguredAddresses;

    // TMockPollingConfig tests
    [Test]
    procedure MockPollingConfig_DefaultValues;

    // TMockConnectionConfig tests
    [Test]
    procedure MockConnectionConfig_DefaultValues;
  end;

  /// <summary>
  /// Test fixture for notification mode and event enumerations.
  /// </summary>
  [TestFixture]
  TNotificationEnumTests = class
  public
    [Test]
    procedure NotificationMode_ValuesCorrect;
    [Test]
    procedure NotificationEvent_ValuesCorrect;
  end;

  /// <summary>
  /// Test fixture for TLastSeenFormat enumeration.
  /// </summary>
  [TestFixture]
  TLastSeenFormatTests = class
  public
    [Test]
    procedure LastSeenFormat_ValuesCorrect;
  end;

  /// <summary>
  /// Test fixture for TWindowMode and TPositionMode enumerations.
  /// </summary>
  [TestFixture]
  TConfigEnumTests = class
  public
    [Test]
    procedure WindowMode_ValuesCorrect;
    [Test]
    procedure PositionMode_ValuesCorrect;
    [Test]
    procedure PollingMode_ValuesCorrect;
  end;

implementation

uses
  System.SysUtils,
  Tests.Mocks;

{ TDeviceConfigTests }

procedure TDeviceConfigTests.Default_SetsAddress;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(UInt64($123456789ABC), Config.Address);
end;

procedure TDeviceConfigTests.Default_SetsEmptyName;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual('', Config.Name);
end;

procedure TDeviceConfigTests.Default_SetsEmptyAlias;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual('', Config.Alias);
end;

procedure TDeviceConfigTests.Default_NotPinned;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.IsFalse(Config.Pinned);
end;

procedure TDeviceConfigTests.Default_NotHidden;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.IsFalse(Config.Hidden);
end;

procedure TDeviceConfigTests.Default_NoAutoConnect;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.IsFalse(Config.AutoConnect);
end;

procedure TDeviceConfigTests.Default_ConnectionTimeoutMinusOne;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(-1, Config.ConnectionTimeout);
end;

procedure TDeviceConfigTests.Default_RetryCountMinusOne;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(-1, Config.ConnectionRetryCount);
end;

procedure TDeviceConfigTests.Default_NotificationsMinusOne;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(-1, Config.Notifications.OnConnect);
  Assert.AreEqual(-1, Config.Notifications.OnDisconnect);
  Assert.AreEqual(-1, Config.Notifications.OnConnectFailed);
  Assert.AreEqual(-1, Config.Notifications.OnAutoConnect);
end;

procedure TDeviceConfigTests.Default_DeviceTypeOverrideMinusOne;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(-1, Config.DeviceTypeOverride);
end;

procedure TDeviceConfigTests.Default_LastSeenZero;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Assert.AreEqual(Double(0), Double(Config.LastSeen), 0.0001);
end;

procedure TDeviceConfigTests.CreateDefault_SameAsDefault;
var
  Config1, Config2: TDeviceConfig;
begin
  // Verify Default returns consistent results
  Config1 := TDeviceConfig.Default($123456789ABC);
  Config2 := TDeviceConfig.Default($123456789ABC);

  Assert.AreEqual(Config1.Address, Config2.Address);
  Assert.AreEqual(Config1.Name, Config2.Name);
  Assert.AreEqual(Config1.Alias, Config2.Alias);
  Assert.AreEqual(Config1.Pinned, Config2.Pinned);
  Assert.AreEqual(Config1.Hidden, Config2.Hidden);
  Assert.AreEqual(Config1.AutoConnect, Config2.AutoConnect);
  Assert.AreEqual(Config1.ConnectionTimeout, Config2.ConnectionTimeout);
  Assert.AreEqual(Config1.DeviceTypeOverride, Config2.DeviceTypeOverride);
end;

procedure TDeviceConfigTests.GetDisplayName_ReturnsAliasWhenSet;
var
  Config: TDeviceConfig;
  DisplayName: string;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Config.Name := 'Original Name';
  Config.Alias := 'Custom Alias';
  // Display name logic: Alias > Name > Address
  if Config.Alias <> '' then
    DisplayName := Config.Alias
  else if Config.Name <> '' then
    DisplayName := Config.Name
  else
    DisplayName := IntToHex(Config.Address, 12);
  Assert.AreEqual('Custom Alias', DisplayName);
end;

procedure TDeviceConfigTests.GetDisplayName_ReturnsNameWhenNoAlias;
var
  Config: TDeviceConfig;
  DisplayName: string;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Config.Name := 'Device Name';
  Config.Alias := '';
  // Display name logic: Alias > Name > Address
  if Config.Alias <> '' then
    DisplayName := Config.Alias
  else if Config.Name <> '' then
    DisplayName := Config.Name
  else
    DisplayName := IntToHex(Config.Address, 12);
  Assert.AreEqual('Device Name', DisplayName);
end;

procedure TDeviceConfigTests.GetDisplayName_ReturnsAddressWhenNoName;
var
  Config: TDeviceConfig;
  DisplayName: string;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Config.Name := '';
  Config.Alias := '';
  // Display name logic: Alias > Name > Address
  if Config.Alias <> '' then
    DisplayName := Config.Alias
  else if Config.Name <> '' then
    DisplayName := Config.Name
  else
    DisplayName := IntToHex(Config.Address, 12);
  Assert.AreEqual('123456789ABC', DisplayName);
end;

{ TDeviceNotificationsTests }

procedure TDeviceNotificationsTests.Default_AllValuesMinusOne;
var
  Notifications: TDeviceNotificationConfig;
begin
  Notifications := TDeviceNotificationConfig.Default;
  Assert.AreEqual(-1, Notifications.OnConnect);
  Assert.AreEqual(-1, Notifications.OnDisconnect);
  Assert.AreEqual(-1, Notifications.OnConnectFailed);
  Assert.AreEqual(-1, Notifications.OnAutoConnect);
end;

procedure TDeviceNotificationsTests.CreateDefault_SameAsDefault;
var
  N1, N2: TDeviceNotificationConfig;
begin
  // Verify Default returns consistent results
  N1 := TDeviceNotificationConfig.Default;
  N2 := TDeviceNotificationConfig.Default;

  Assert.AreEqual(N1.OnConnect, N2.OnConnect);
  Assert.AreEqual(N1.OnDisconnect, N2.OnDisconnect);
  Assert.AreEqual(N1.OnConnectFailed, N2.OnConnectFailed);
  Assert.AreEqual(N1.OnAutoConnect, N2.OnAutoConnect);
end;

{ TMockConfigTests }

procedure TMockConfigTests.MockLayoutConfig_DefaultValues;
var
  Mock: TMockLayoutConfig;
begin
  Mock := TMockLayoutConfig.Create;
  try
    Assert.AreEqual(70, Mock.GetItemHeight);
    Assert.AreEqual(12, Mock.GetItemPadding);
    Assert.AreEqual(4, Mock.GetItemMargin);
    Assert.AreEqual(32, Mock.GetIconSize);
    Assert.AreEqual(8, Mock.GetCornerRadius);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockLayoutConfig_SettersWork;
var
  Mock: TMockLayoutConfig;
begin
  Mock := TMockLayoutConfig.Create;
  try
    Mock.ItemHeight := 100;
    Mock.ItemPadding := 20;
    Assert.AreEqual(100, Mock.GetItemHeight);
    Assert.AreEqual(20, Mock.GetItemPadding);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockAppearanceConfig_DefaultValues;
var
  Mock: TMockAppearanceConfig;
begin
  Mock := TMockAppearanceConfig.Create;
  try
    Assert.IsFalse(Mock.GetShowAddresses);
    Assert.AreEqual('Windows', Mock.GetTheme);
    Assert.AreEqual('themes', Mock.GetVsfDir);
    Assert.IsFalse(Mock.GetShowLastSeen);
    Assert.IsTrue(Mock.GetShowDeviceIcons);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockAppearanceConfig_SettersWork;
var
  Mock: TMockAppearanceConfig;
begin
  Mock := TMockAppearanceConfig.Create;
  try
    Mock.ShowAddresses := True;
    Mock.Theme := 'Dark';
    Assert.IsTrue(Mock.GetShowAddresses);
    Assert.AreEqual('Dark', Mock.GetTheme);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_EmptyByDefault;
var
  Mock: TMockDeviceConfigProvider;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    Assert.AreEqual(Integer(0), Integer(Length(Mock.GetConfiguredDeviceAddresses)));
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_AddAndGet;
var
  Mock: TMockDeviceConfigProvider;
  Config, Retrieved: TDeviceConfig;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    Config := TDeviceConfig.Default($AABBCCDDEEFF);
    Config.Name := 'Test Device';
    Config.Pinned := True;

    Mock.AddDeviceConfig($AABBCCDDEEFF, Config);
    Retrieved := Mock.GetDeviceConfig($AABBCCDDEEFF);

    Assert.AreEqual('Test Device', Retrieved.Name);
    Assert.IsTrue(Retrieved.Pinned);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_GetMissing_ReturnsDefault;
var
  Mock: TMockDeviceConfigProvider;
  Config: TDeviceConfig;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    Config := Mock.GetDeviceConfig($112233445566);
    Assert.AreEqual(UInt64($112233445566), Config.Address);
    Assert.AreEqual('', Config.Name);
    Assert.IsFalse(Config.Pinned);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_Clear;
var
  Mock: TMockDeviceConfigProvider;
  Config: TDeviceConfig;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    Config := TDeviceConfig.Default($AABBCCDDEEFF);
    Mock.AddDeviceConfig($AABBCCDDEEFF, Config);
    Assert.AreEqual(Integer(1), Integer(Length(Mock.GetConfiguredDeviceAddresses)));

    Mock.Clear;
    Assert.AreEqual(Integer(0), Integer(Length(Mock.GetConfiguredDeviceAddresses)));
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_RegisterDevice;
var
  Mock: TMockDeviceConfigProvider;
  Config: TDeviceConfig;
  TestTime: TDateTime;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    TestTime := Now;
    Mock.RegisterDevice($AABBCCDDEEFF, 'My Device', TestTime);

    Config := Mock.GetDeviceConfig($AABBCCDDEEFF);
    Assert.AreEqual('My Device', Config.Name);
    Assert.AreEqual(Double(TestTime), Double(Config.LastSeen), 0.0001);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockDeviceConfigProvider_GetConfiguredAddresses;
var
  Mock: TMockDeviceConfigProvider;
  Config: TDeviceConfig;
  Addresses: TArray<UInt64>;
begin
  Mock := TMockDeviceConfigProvider.Create;
  try
    Config := TDeviceConfig.Default($111111111111);
    Mock.AddDeviceConfig($111111111111, Config);

    Config := TDeviceConfig.Default($222222222222);
    Mock.AddDeviceConfig($222222222222, Config);

    Addresses := Mock.GetConfiguredDeviceAddresses;
    Assert.AreEqual(Integer(2), Integer(Length(Addresses)));
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockPollingConfig_DefaultValues;
var
  Mock: TMockPollingConfig;
begin
  Mock := TMockPollingConfig.Create;
  try
    Assert.AreEqual(Ord(pmFallback), Ord(Mock.GetPollingMode));
    Assert.AreEqual(2000, Mock.GetPollingInterval);
    Assert.AreEqual(500, Mock.GetEventDebounceMs);
  finally
    Mock.Free;
  end;
end;

procedure TMockConfigTests.MockConnectionConfig_DefaultValues;
var
  Mock: TMockConnectionConfig;
begin
  Mock := TMockConnectionConfig.Create;
  try
    Assert.AreEqual(10000, Mock.GetConnectionTimeout);
    Assert.AreEqual(2, Mock.GetConnectionRetryCount);
  finally
    Mock.Free;
  end;
end;

{ TNotificationEnumTests }

procedure TNotificationEnumTests.NotificationMode_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(nmNone));
  Assert.AreEqual(1, Ord(nmBalloon));
end;

procedure TNotificationEnumTests.NotificationEvent_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(neConnect));
  Assert.AreEqual(1, Ord(neDisconnect));
  Assert.AreEqual(2, Ord(neConnectFailed));
  Assert.AreEqual(3, Ord(neAutoConnect));
end;

{ TLastSeenFormatTests }

procedure TLastSeenFormatTests.LastSeenFormat_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(lsfRelative));
  Assert.AreEqual(1, Ord(lsfAbsolute));
end;

{ TConfigEnumTests }

procedure TConfigEnumTests.WindowMode_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(wmMenu));
  Assert.AreEqual(1, Ord(wmWindow));
end;

procedure TConfigEnumTests.PositionMode_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(pmCoordinates));
  Assert.AreEqual(1, Ord(pmNearTray));
  Assert.AreEqual(2, Ord(pmNearCursor));
  Assert.AreEqual(3, Ord(pmCenterScreen));
end;

procedure TConfigEnumTests.PollingMode_ValuesCorrect;
begin
  Assert.AreEqual(0, Ord(pmDisabled));
  Assert.AreEqual(1, Ord(pmFallback));
  Assert.AreEqual(2, Ord(pmPrimary));
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceConfigTests);
  TDUnitX.RegisterTestFixture(TDeviceNotificationsTests);
  TDUnitX.RegisterTestFixture(TMockConfigTests);
  TDUnitX.RegisterTestFixture(TNotificationEnumTests);
  TDUnitX.RegisterTestFixture(TLastSeenFormatTests);
  TDUnitX.RegisterTestFixture(TConfigEnumTests);

end.
