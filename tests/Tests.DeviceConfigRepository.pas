{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Config Repository Unit Tests             }
{                                                       }
{*******************************************************}

unit Tests.DeviceConfigRepository;

interface

uses
  DUnitX.TestFramework,
  System.IniFiles,
  App.ConfigInterfaces,
  App.DeviceConfigTypes;

type
  /// <summary>
  /// Test fixture for TDeviceConfigRepository.
  /// Tests device configuration storage and retrieval.
  /// </summary>
  [TestFixture]
  TDeviceConfigRepositoryTests = class
  private
    FRepository: IDeviceConfigRepository;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Basic CRUD tests
    [Test]
    procedure GetConfig_UnknownDevice_ReturnsDefault;
    [Test]
    procedure SetConfig_AndGetConfig_ReturnsValue;
    [Test]
    procedure SetConfig_SetsModifiedFlag;
    [Test]
    procedure Remove_ExistingDevice_RemovesIt;
    [Test]
    procedure Remove_NonExisting_NoError;

    // RegisterDevice tests
    [Test]
    procedure RegisterDevice_NewDevice_CreatesConfig;
    [Test]
    procedure RegisterDevice_ExistingDevice_UpdatesLastSeen;
    [Test]
    procedure RegisterDevice_ExistingDevice_UpdatesName;
    [Test]
    procedure RegisterDevice_GenericName_KeepsCachedName;
    [Test]
    procedure RegisterDevice_GenericName_UpdatesIfNoCache;
    [Test]
    procedure RegisterDevice_GoodName_OverwritesGenericCache;

    // GetAllAddresses tests
    [Test]
    procedure GetAllAddresses_Empty_ReturnsEmpty;
    [Test]
    procedure GetAllAddresses_WithDevices_ReturnsAll;

    // GetAll tests
    [Test]
    procedure GetAll_Empty_ReturnsEmpty;
    [Test]
    procedure GetAll_WithDevices_ReturnsAll;

    // Modified flag tests
    [Test]
    procedure IsModified_Initially_False;
    [Test]
    procedure ClearModified_ResetsFlag;
  end;

  /// <summary>
  /// Test fixture for INI persistence (LoadFrom/SaveTo).
  /// Uses TMemIniFile for in-memory round-trip testing without file system I/O.
  /// </summary>
  [TestFixture]
  TDeviceConfigPersistenceTests = class
  private
    FRepository: IDeviceConfigRepository;
    FIni: TMemIniFile;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // LoadFrom tests
    [Test]
    procedure LoadFrom_EmptyIni_LoadsNothing;
    [Test]
    procedure LoadFrom_BasicDevice_LoadsNameAndAlias;
    [Test]
    procedure LoadFrom_BooleanFields_LoadsPinnedAndHidden;
    [Test]
    procedure LoadFrom_ConnectionSettings_LoadsTimeoutAndRetry;
    [Test]
    procedure LoadFrom_NotificationOverrides_LoadsAllFields;
    [Test]
    procedure LoadFrom_BatteryTrayOverrides_LoadsAllFields;
    [Test]
    procedure LoadFrom_DeviceTypeOverride_LoadsValue;
    [Test]
    procedure LoadFrom_ShowProfiles_LoadsValue;
    [Test]
    procedure LoadFrom_LastSeen_ParsesISO8601;
    [Test]
    procedure LoadFrom_LastSeen_InvalidDate_SetsZero;
    [Test]
    procedure LoadFrom_LastSeen_Empty_SetsZero;
    [Test]
    procedure LoadFrom_ColonMACFormat_ParsesCorrectly;
    [Test]
    procedure LoadFrom_NonDeviceSection_Ignored;
    [Test]
    procedure LoadFrom_InvalidAddress_Ignored;
    [Test]
    procedure LoadFrom_MultipleDevices_LoadsAll;
    [Test]
    procedure LoadFrom_ClearsExistingDevices;
    [Test]
    procedure LoadFrom_ClearsModifiedFlag;

    // SaveTo tests
    [Test]
    procedure SaveTo_BasicDevice_WritesNameAndAlias;
    [Test]
    procedure SaveTo_BooleanFields_WritesPinnedAndHidden;
    [Test]
    procedure SaveTo_ConnectionTimeout_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_ConnectionRetryCount_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_NotificationOverrides_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_BatteryTrayOverrides_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_BatteryBackgroundColor_WritesTransparent;
    [Test]
    procedure SaveTo_DeviceTypeOverride_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_ShowProfiles_WritesOnlyIfSet;
    [Test]
    procedure SaveTo_LastSeen_WritesISO8601;
    [Test]
    procedure SaveTo_LastSeen_Zero_SkipsWrite;
    [Test]
    procedure SaveTo_ClearsOldDeviceSections;
    [Test]
    procedure SaveTo_ClearsModifiedFlag;
    [Test]
    procedure SaveTo_SectionNameUsesHexAddress;

    // Round-trip tests
    [Test]
    procedure RoundTrip_AllFields_PreservesValues;
    [Test]
    procedure RoundTrip_DefaultConfig_PreservesDefaults;
    [Test]
    procedure RoundTrip_MultipleDevices_PreservesAll;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  App.DeviceConfigRepository;

{ TDeviceConfigRepositoryTests }

procedure TDeviceConfigRepositoryTests.Setup;
begin
  FRepository := CreateDeviceConfigRepository;
end;

procedure TDeviceConfigRepositoryTests.TearDown;
begin
  FRepository := nil;
end;

procedure TDeviceConfigRepositoryTests.GetConfig_UnknownDevice_ReturnsDefault;
var
  Config: TDeviceConfig;
begin
  Config := FRepository.GetConfig($AABBCCDDEEFF);

  Assert.AreEqual(UInt64($AABBCCDDEEFF), Config.Address);
  Assert.AreEqual('', Config.Name);
  Assert.AreEqual('', Config.Alias);
  Assert.IsFalse(Config.Pinned);
  Assert.IsFalse(Config.Hidden);
end;

procedure TDeviceConfigRepositoryTests.SetConfig_AndGetConfig_ReturnsValue;
var
  Config, Retrieved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  Config.Name := 'Test Device';
  Config.Alias := 'My Alias';
  Config.Pinned := True;

  FRepository.SetConfig(Config);
  Retrieved := FRepository.GetConfig($123456789ABC);

  Assert.AreEqual('Test Device', Retrieved.Name);
  Assert.AreEqual('My Alias', Retrieved.Alias);
  Assert.IsTrue(Retrieved.Pinned);
end;

procedure TDeviceConfigRepositoryTests.SetConfig_SetsModifiedFlag;
var
  Config: TDeviceConfig;
begin
  Assert.IsFalse(FRepository.IsModified);

  Config := TDeviceConfig.Default($123456789ABC);
  FRepository.SetConfig(Config);

  Assert.IsTrue(FRepository.IsModified);
end;

procedure TDeviceConfigRepositoryTests.Remove_ExistingDevice_RemovesIt;
var
  Config: TDeviceConfig;
  Addresses: TArray<UInt64>;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  FRepository.SetConfig(Config);
  FRepository.ClearModified;

  FRepository.Remove($123456789ABC);

  Addresses := FRepository.GetAllAddresses;
  Assert.AreEqual(Integer(0), Integer(Length(Addresses)));
  Assert.IsTrue(FRepository.IsModified);
end;

procedure TDeviceConfigRepositoryTests.Remove_NonExisting_NoError;
begin
  // Should not raise exception
  FRepository.Remove($AABBCCDDEEFF);
  Assert.Pass;
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_NewDevice_CreatesConfig;
var
  Config: TDeviceConfig;
  TestTime: TDateTime;
begin
  TestTime := Now;
  FRepository.RegisterDevice($AABBCCDDEEFF, 'New Device', TestTime);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('New Device', Config.Name);
  Assert.AreEqual(Double(TestTime), Double(Config.LastSeen), 0.0001);
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_ExistingDevice_UpdatesLastSeen;
var
  Config: TDeviceConfig;
  OldTime, NewTime: TDateTime;
begin
  OldTime := Now - 1;
  NewTime := Now;

  FRepository.RegisterDevice($AABBCCDDEEFF, 'Device', OldTime);
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Device', NewTime);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(Double(NewTime), Double(Config.LastSeen), 0.0001);
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_ExistingDevice_UpdatesName;
var
  Config: TDeviceConfig;
begin
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Old Name', Now);
  FRepository.RegisterDevice($AABBCCDDEEFF, 'New Name', Now);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('New Name', Config.Name);
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_GenericName_KeepsCachedName;
var
  Config: TDeviceConfig;
begin
  // Register device with good name first
  FRepository.RegisterDevice($88C9E8A166B4, 'WH-1000XM4', Now);

  // Try to update with generic Windows name (what Windows returns for offline devices)
  FRepository.RegisterDevice($88C9E8A166B4, 'Bluetooth 88:c9:e8:a1:66:b4', Now);

  // Should keep the good cached name, not overwrite with generic
  Config := FRepository.GetConfig($88C9E8A166B4);
  Assert.AreEqual('WH-1000XM4', Config.Name, 'Should preserve good cached name when Windows provides generic name');
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_GenericName_UpdatesIfNoCache;
var
  Config: TDeviceConfig;
begin
  // Register new device with generic name (no previous cache)
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Bluetooth aa:bb:cc:dd:ee:ff', Now);

  // Should accept generic name when there's no better alternative
  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('Bluetooth aa:bb:cc:dd:ee:ff', Config.Name, 'Should accept generic name for new device with no cache');
end;

procedure TDeviceConfigRepositoryTests.RegisterDevice_GoodName_OverwritesGenericCache;
var
  Config: TDeviceConfig;
begin
  // Register device with generic name first
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Bluetooth aa:bb:cc:dd:ee:ff', Now);

  // Update with good name from Windows when device comes online
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Sony WH-1000XM4', Now);

  // Should replace generic name with good name
  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('Sony WH-1000XM4', Config.Name, 'Should replace generic cached name when Windows provides good name');
end;

procedure TDeviceConfigRepositoryTests.GetAllAddresses_Empty_ReturnsEmpty;
var
  Addresses: TArray<UInt64>;
begin
  Addresses := FRepository.GetAllAddresses;
  Assert.AreEqual(Integer(0), Integer(Length(Addresses)));
end;

procedure TDeviceConfigRepositoryTests.GetAllAddresses_WithDevices_ReturnsAll;
var
  Config: TDeviceConfig;
  Addresses: TArray<UInt64>;
begin
  Config := TDeviceConfig.Default($111111111111);
  FRepository.SetConfig(Config);

  Config := TDeviceConfig.Default($222222222222);
  FRepository.SetConfig(Config);

  Config := TDeviceConfig.Default($333333333333);
  FRepository.SetConfig(Config);

  Addresses := FRepository.GetAllAddresses;
  Assert.AreEqual(Integer(3), Integer(Length(Addresses)));
end;

procedure TDeviceConfigRepositoryTests.GetAll_Empty_ReturnsEmpty;
var
  All: TArray<TDeviceConfig>;
begin
  All := FRepository.GetAll;
  Assert.AreEqual(Integer(0), Integer(Length(All)));
end;

procedure TDeviceConfigRepositoryTests.GetAll_WithDevices_ReturnsAll;
var
  Config: TDeviceConfig;
  All: TArray<TDeviceConfig>;
begin
  Config := TDeviceConfig.Default($111111111111);
  Config.Name := 'Device 1';
  FRepository.SetConfig(Config);

  Config := TDeviceConfig.Default($222222222222);
  Config.Name := 'Device 2';
  FRepository.SetConfig(Config);

  All := FRepository.GetAll;
  Assert.AreEqual(Integer(2), Integer(Length(All)));
end;

procedure TDeviceConfigRepositoryTests.IsModified_Initially_False;
begin
  Assert.IsFalse(FRepository.IsModified);
end;

procedure TDeviceConfigRepositoryTests.ClearModified_ResetsFlag;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($123456789ABC);
  FRepository.SetConfig(Config);
  Assert.IsTrue(FRepository.IsModified);

  FRepository.ClearModified;
  Assert.IsFalse(FRepository.IsModified);
end;

{ TDeviceConfigPersistenceTests }

procedure TDeviceConfigPersistenceTests.Setup;
begin
  FRepository := CreateDeviceConfigRepository;
  FIni := TMemIniFile.Create('');
end;

procedure TDeviceConfigPersistenceTests.TearDown;
begin
  FIni.Free;
  FRepository := nil;
end;

// --- LoadFrom tests ---

procedure TDeviceConfigPersistenceTests.LoadFrom_EmptyIni_LoadsNothing;
begin
  FRepository.LoadFrom(FIni);
  Assert.AreEqual(Integer(0), Integer(Length(FRepository.GetAllAddresses)));
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_BasicDevice_LoadsNameAndAlias;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.581862015DAE', 'Name', 'WH-1000XM4');
  FIni.WriteString('Device.581862015DAE', 'Alias', 'My Headphones');

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($581862015DAE);
  Assert.AreEqual('WH-1000XM4', Config.Name);
  Assert.AreEqual('My Headphones', Config.Alias);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_BooleanFields_LoadsPinnedAndHidden;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteBool('Device.AABBCCDDEEFF', 'Pinned', True);
  FIni.WriteBool('Device.AABBCCDDEEFF', 'Hidden', True);
  FIni.WriteBool('Device.AABBCCDDEEFF', 'AutoConnect', True);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.IsTrue(Config.Pinned);
  Assert.IsTrue(Config.Hidden);
  Assert.IsTrue(Config.AutoConnect);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_ConnectionSettings_LoadsTimeoutAndRetry;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'ConnectionTimeout', 5000);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'ConnectionRetryCount', 3);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(5000, Config.ConnectionTimeout);
  Assert.AreEqual(3, Config.ConnectionRetryCount);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_NotificationOverrides_LoadsAllFields;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'NotifyOnConnect', 1);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'NotifyOnDisconnect', 0);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'NotifyOnConnectFailed', 1);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'NotifyOnAutoConnect', 0);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(1, Config.Notifications.OnConnect);
  Assert.AreEqual(0, Config.Notifications.OnDisconnect);
  Assert.AreEqual(1, Config.Notifications.OnConnectFailed);
  Assert.AreEqual(0, Config.Notifications.OnAutoConnect);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_BatteryTrayOverrides_LoadsAllFields;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryTrayIcon', 1);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryIconColor', $00FF00);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryBackgroundColor', -2);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryShowNumeric', 1);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryLowThreshold', 20);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryNotifyLow', 1);
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'BatteryNotifyFull', 0);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(1, Config.BatteryTray.ShowTrayIcon);
  Assert.AreEqual($00FF00, Config.BatteryTray.IconColor);
  Assert.AreEqual(-2, Config.BatteryTray.BackgroundColor);
  Assert.AreEqual(1, Config.BatteryTray.ShowNumericValue);
  Assert.AreEqual(20, Config.BatteryTray.LowBatteryThreshold);
  Assert.AreEqual(1, Config.BatteryTray.NotifyLowBattery);
  Assert.AreEqual(0, Config.BatteryTray.NotifyFullyCharged);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_DeviceTypeOverride_LoadsValue;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'DeviceTypeOverride', 3);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(3, Config.DeviceTypeOverride);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_ShowProfiles_LoadsValue;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteInteger('Device.AABBCCDDEEFF', 'ShowProfiles', 1);

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(1, Config.ShowProfiles);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_LastSeen_ParsesISO8601;
var
  Config: TDeviceConfig;
  Expected: TDateTime;
begin
  Expected := EncodeDateTime(2025, 6, 15, 10, 30, 0, 0);
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteString('Device.AABBCCDDEEFF', 'LastSeen', DateToISO8601(Expected, False));

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  // Allow 1-second tolerance for ISO 8601 round-trip
  Assert.AreEqual(Double(Expected), Double(Config.LastSeen), 1.0 / SecsPerDay);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_LastSeen_InvalidDate_SetsZero;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  FIni.WriteString('Device.AABBCCDDEEFF', 'LastSeen', 'not-a-date');

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(Double(0), Double(Config.LastSeen), 0.0001);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_LastSeen_Empty_SetsZero;
var
  Config: TDeviceConfig;
begin
  FIni.WriteString('Device.AABBCCDDEEFF', 'Name', 'Test');
  // LastSeen key not present at all

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual(Double(0), Double(Config.LastSeen), 0.0001);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_ColonMACFormat_ParsesCorrectly;
var
  Config: TDeviceConfig;
begin
  // Section name uses colon-separated MAC format (supported for backward compatibility)
  FIni.WriteString('Device.58:18:62:01:5D:AE', 'Name', 'Colon Device');

  FRepository.LoadFrom(FIni);

  Config := FRepository.GetConfig($581862015DAE);
  Assert.AreEqual('Colon Device', Config.Name);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_NonDeviceSection_Ignored;
begin
  FIni.WriteString('General', 'WindowMode', '0');
  FIni.WriteString('Appearance', 'Theme', 'dark');

  FRepository.LoadFrom(FIni);

  Assert.AreEqual(Integer(0), Integer(Length(FRepository.GetAllAddresses)));
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_InvalidAddress_Ignored;
begin
  // "Device.ZZZZZZ" is not valid hex
  FIni.WriteString('Device.ZZZZZZZZZZZZ', 'Name', 'Bad Address');

  FRepository.LoadFrom(FIni);

  Assert.AreEqual(Integer(0), Integer(Length(FRepository.GetAllAddresses)));
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_MultipleDevices_LoadsAll;
begin
  FIni.WriteString('Device.111111111111', 'Name', 'Device 1');
  FIni.WriteString('Device.222222222222', 'Name', 'Device 2');
  FIni.WriteString('Device.333333333333', 'Name', 'Device 3');

  FRepository.LoadFrom(FIni);

  Assert.AreEqual(Integer(3), Integer(Length(FRepository.GetAllAddresses)));
  Assert.AreEqual('Device 1', FRepository.GetConfig($111111111111).Name);
  Assert.AreEqual('Device 2', FRepository.GetConfig($222222222222).Name);
  Assert.AreEqual('Device 3', FRepository.GetConfig($333333333333).Name);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_ClearsExistingDevices;
begin
  // Pre-load a device
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Old Device', Now);

  // LoadFrom should clear existing devices
  FIni.WriteString('Device.111111111111', 'Name', 'New Device');
  FRepository.LoadFrom(FIni);

  Assert.AreEqual(Integer(1), Integer(Length(FRepository.GetAllAddresses)));
  Assert.AreEqual('New Device', FRepository.GetConfig($111111111111).Name);
  // Old device should be gone
  Assert.AreEqual('', FRepository.GetConfig($AABBCCDDEEFF).Name);
end;

procedure TDeviceConfigPersistenceTests.LoadFrom_ClearsModifiedFlag;
begin
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Device', Now);
  Assert.IsTrue(FRepository.IsModified);

  FRepository.LoadFrom(FIni);

  Assert.IsFalse(FRepository.IsModified);
end;

// --- SaveTo tests ---

procedure TDeviceConfigPersistenceTests.SaveTo_BasicDevice_WritesNameAndAlias;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'WH-1000XM4';
  Config.Alias := 'My Headphones';
  FRepository.SetConfig(Config);

  FRepository.SaveTo(FIni);

  Assert.AreEqual('WH-1000XM4', FIni.ReadString('Device.AABBCCDDEEFF', 'Name', ''));
  Assert.AreEqual('My Headphones', FIni.ReadString('Device.AABBCCDDEEFF', 'Alias', ''));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_BooleanFields_WritesPinnedAndHidden;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Pinned := True;
  Config.Hidden := True;
  Config.AutoConnect := True;
  FRepository.SetConfig(Config);

  FRepository.SaveTo(FIni);

  Assert.IsTrue(FIni.ReadBool('Device.AABBCCDDEEFF', 'Pinned', False));
  Assert.IsTrue(FIni.ReadBool('Device.AABBCCDDEEFF', 'Hidden', False));
  Assert.IsTrue(FIni.ReadBool('Device.AABBCCDDEEFF', 'AutoConnect', False));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_ConnectionTimeout_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  // Default value (-1) should not be written
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(-1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'ConnectionTimeout', -1),
    'Default timeout should not be written (key should be absent)');

  // Non-default value should be written
  FIni.Free;
  FIni := TMemIniFile.Create('');
  FRepository := CreateDeviceConfigRepository;
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.ConnectionTimeout := 5000;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(5000, FIni.ReadInteger('Device.AABBCCDDEEFF', 'ConnectionTimeout', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_ConnectionRetryCount_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.ConnectionRetryCount := 3;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(3, FIni.ReadInteger('Device.AABBCCDDEEFF', 'ConnectionRetryCount', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_NotificationOverrides_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Notifications.OnConnect := 1;
  // OnDisconnect left at -1 (default, should not be written)
  Config.Notifications.OnConnectFailed := 0;
  Config.Notifications.OnAutoConnect := 1;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'NotifyOnConnect', -1));
  // Default should not be written - reading with fallback -1 should return -1
  Assert.AreEqual(-1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'NotifyOnDisconnect', -1));
  Assert.AreEqual(0, FIni.ReadInteger('Device.AABBCCDDEEFF', 'NotifyOnConnectFailed', -1));
  Assert.AreEqual(1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'NotifyOnAutoConnect', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_BatteryTrayOverrides_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.BatteryTray.ShowTrayIcon := 1;
  Config.BatteryTray.LowBatteryThreshold := 20;
  // Other battery fields left at -1 (default)
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'BatteryTrayIcon', -1));
  Assert.AreEqual(20, FIni.ReadInteger('Device.AABBCCDDEEFF', 'BatteryLowThreshold', -1));
  // Defaults should not be written
  Assert.AreEqual(-1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'BatteryIconColor', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_BatteryBackgroundColor_WritesTransparent;
var
  Config: TDeviceConfig;
begin
  // -2 means "transparent" and should be written (even though it's negative)
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.BatteryTray.BackgroundColor := -2;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(-2, FIni.ReadInteger('Device.AABBCCDDEEFF', 'BatteryBackgroundColor', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_DeviceTypeOverride_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.DeviceTypeOverride := 3;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(3, FIni.ReadInteger('Device.AABBCCDDEEFF', 'DeviceTypeOverride', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_ShowProfiles_WritesOnlyIfSet;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.ShowProfiles := 1;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual(1, FIni.ReadInteger('Device.AABBCCDDEEFF', 'ShowProfiles', -1));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_LastSeen_WritesISO8601;
var
  Config: TDeviceConfig;
  SavedStr: string;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.LastSeen := EncodeDateTime(2025, 6, 15, 10, 30, 0, 0);
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  SavedStr := FIni.ReadString('Device.AABBCCDDEEFF', 'LastSeen', '');
  Assert.IsTrue(SavedStr <> '', 'LastSeen should be written');
  Assert.IsTrue(Pos('2025', SavedStr) > 0, 'Date should contain year 2025');
end;

procedure TDeviceConfigPersistenceTests.SaveTo_LastSeen_Zero_SkipsWrite;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.LastSeen := 0;
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Assert.AreEqual('', FIni.ReadString('Device.AABBCCDDEEFF', 'LastSeen', ''),
    'Zero LastSeen should not be written');
end;

procedure TDeviceConfigPersistenceTests.SaveTo_ClearsOldDeviceSections;
begin
  // Write device A to INI directly
  FIni.WriteString('Device.111111111111', 'Name', 'Old Device');
  // Repository only has device B
  FRepository.RegisterDevice($222222222222, 'New Device', Now);

  FRepository.SaveTo(FIni);

  // Old device section should be removed
  Assert.AreEqual('', FIni.ReadString('Device.111111111111', 'Name', ''),
    'Old device section should be erased');
  Assert.AreEqual('New Device', FIni.ReadString('Device.222222222222', 'Name', ''));
end;

procedure TDeviceConfigPersistenceTests.SaveTo_ClearsModifiedFlag;
begin
  FRepository.RegisterDevice($AABBCCDDEEFF, 'Device', Now);
  Assert.IsTrue(FRepository.IsModified);

  FRepository.SaveTo(FIni);

  Assert.IsFalse(FRepository.IsModified);
end;

procedure TDeviceConfigPersistenceTests.SaveTo_SectionNameUsesHexAddress;
var
  Config: TDeviceConfig;
  Sections: TStringList;
  Found: Boolean;
  Section: string;
begin
  Config := TDeviceConfig.Default($0A0B0C0D0E0F);
  Config.Name := 'Test';
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  Sections := TStringList.Create;
  try
    FIni.ReadSections(Sections);
    Found := False;
    for Section in Sections do
      if Section = 'Device.0A0B0C0D0E0F' then
        Found := True;
    Assert.IsTrue(Found, 'Section should use 12-digit uppercase hex address');
  finally
    Sections.Free;
  end;
end;

// --- Round-trip tests ---

procedure TDeviceConfigPersistenceTests.RoundTrip_AllFields_PreservesValues;
var
  Config, Loaded: TDeviceConfig;
  LoadRepo: IDeviceConfigRepository;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Sony WH-1000XM4';
  Config.Alias := 'My Headphones';
  Config.Pinned := True;
  Config.Hidden := False;
  Config.AutoConnect := True;
  Config.ConnectionTimeout := 5000;
  Config.ConnectionRetryCount := 3;
  Config.Notifications.OnConnect := 1;
  Config.Notifications.OnDisconnect := 0;
  Config.Notifications.OnConnectFailed := 1;
  Config.Notifications.OnAutoConnect := 0;
  Config.BatteryTray.ShowTrayIcon := 1;
  Config.BatteryTray.IconColor := $00FF00;
  Config.BatteryTray.BackgroundColor := -2;
  Config.BatteryTray.ShowNumericValue := 1;
  Config.BatteryTray.LowBatteryThreshold := 20;
  Config.BatteryTray.NotifyLowBattery := 1;
  Config.BatteryTray.NotifyFullyCharged := 0;
  Config.DeviceTypeOverride := 3;
  Config.ShowProfiles := 1;
  Config.LastSeen := EncodeDateTime(2025, 6, 15, 10, 30, 0, 0);

  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  // Load into a fresh repository
  LoadRepo := CreateDeviceConfigRepository;
  LoadRepo.LoadFrom(FIni);

  Loaded := LoadRepo.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('Sony WH-1000XM4', Loaded.Name);
  Assert.AreEqual('My Headphones', Loaded.Alias);
  Assert.IsTrue(Loaded.Pinned);
  Assert.IsFalse(Loaded.Hidden);
  Assert.IsTrue(Loaded.AutoConnect);
  Assert.AreEqual(5000, Loaded.ConnectionTimeout);
  Assert.AreEqual(3, Loaded.ConnectionRetryCount);
  Assert.AreEqual(1, Loaded.Notifications.OnConnect);
  Assert.AreEqual(0, Loaded.Notifications.OnDisconnect);
  Assert.AreEqual(1, Loaded.Notifications.OnConnectFailed);
  Assert.AreEqual(0, Loaded.Notifications.OnAutoConnect);
  Assert.AreEqual(1, Loaded.BatteryTray.ShowTrayIcon);
  Assert.AreEqual($00FF00, Loaded.BatteryTray.IconColor);
  Assert.AreEqual(-2, Loaded.BatteryTray.BackgroundColor);
  Assert.AreEqual(1, Loaded.BatteryTray.ShowNumericValue);
  Assert.AreEqual(20, Loaded.BatteryTray.LowBatteryThreshold);
  Assert.AreEqual(1, Loaded.BatteryTray.NotifyLowBattery);
  Assert.AreEqual(0, Loaded.BatteryTray.NotifyFullyCharged);
  Assert.AreEqual(3, Loaded.DeviceTypeOverride);
  Assert.AreEqual(1, Loaded.ShowProfiles);
  Assert.AreEqual(Double(Config.LastSeen), Double(Loaded.LastSeen), 1.0 / SecsPerDay);
end;

procedure TDeviceConfigPersistenceTests.RoundTrip_DefaultConfig_PreservesDefaults;
var
  Config, Loaded: TDeviceConfig;
  LoadRepo: IDeviceConfigRepository;
begin
  // Save default config (all fields at defaults)
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Default Device';
  FRepository.SetConfig(Config);
  FRepository.SaveTo(FIni);

  LoadRepo := CreateDeviceConfigRepository;
  LoadRepo.LoadFrom(FIni);

  Loaded := LoadRepo.GetConfig($AABBCCDDEEFF);
  Assert.AreEqual('Default Device', Loaded.Name);
  Assert.AreEqual('', Loaded.Alias);
  Assert.IsFalse(Loaded.Pinned);
  Assert.IsFalse(Loaded.Hidden);
  Assert.IsFalse(Loaded.AutoConnect);
  Assert.AreEqual(-1, Loaded.ConnectionTimeout);
  Assert.AreEqual(-1, Loaded.ConnectionRetryCount);
  Assert.AreEqual(-1, Loaded.Notifications.OnConnect);
  Assert.AreEqual(-1, Loaded.DeviceTypeOverride);
  Assert.AreEqual(-1, Loaded.ShowProfiles);
end;

procedure TDeviceConfigPersistenceTests.RoundTrip_MultipleDevices_PreservesAll;
var
  Config: TDeviceConfig;
  LoadRepo: IDeviceConfigRepository;
  Loaded1, Loaded2: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($111111111111);
  Config.Name := 'Device 1';
  Config.Pinned := True;
  FRepository.SetConfig(Config);

  Config := TDeviceConfig.Default($222222222222);
  Config.Name := 'Device 2';
  Config.AutoConnect := True;
  FRepository.SetConfig(Config);

  FRepository.SaveTo(FIni);

  LoadRepo := CreateDeviceConfigRepository;
  LoadRepo.LoadFrom(FIni);

  Assert.AreEqual(Integer(2), Integer(Length(LoadRepo.GetAllAddresses)));

  Loaded1 := LoadRepo.GetConfig($111111111111);
  Assert.AreEqual('Device 1', Loaded1.Name);
  Assert.IsTrue(Loaded1.Pinned);

  Loaded2 := LoadRepo.GetConfig($222222222222);
  Assert.AreEqual('Device 2', Loaded2.Name);
  Assert.IsTrue(Loaded2.AutoConnect);
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceConfigRepositoryTests);
  TDUnitX.RegisterTestFixture(TDeviceConfigPersistenceTests);

end.
