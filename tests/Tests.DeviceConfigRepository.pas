{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Config Repository Unit Tests             }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.DeviceConfigRepository;

interface

uses
  DUnitX.TestFramework,
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

implementation

uses
  System.SysUtils,
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

initialization
  TDUnitX.RegisterTestFixture(TDeviceConfigRepositoryTests);

end.
