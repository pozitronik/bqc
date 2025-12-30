{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Repository Unit Tests                    }
{                                                       }
{*******************************************************}

unit Tests.DeviceRepository;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.DeviceRepository,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TBluetoothDeviceRepository.
  /// </summary>
  [TestFixture]
  TDeviceRepositoryTests = class
  private
    FRepository: IDeviceRepository;
    FListChangedCount: Integer;
    procedure HandleListChanged(Sender: TObject);
    function CreateTestDevice(AAddress: UInt64; const AName: string;
      AState: TBluetoothConnectionState = csDisconnected): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;

    // GetAll tests
    [Test]
    procedure GetAll_EmptyRepository_ReturnsEmptyArray;
    [Test]
    procedure GetAll_WithDevices_ReturnsAllDevices;

    // GetByAddress tests
    [Test]
    procedure GetByAddress_ExistingDevice_ReturnsDevice;
    [Test]
    procedure GetByAddress_NonExisting_ReturnsEmptyRecord;

    // TryGetByAddress tests
    [Test]
    procedure TryGetByAddress_ExistingDevice_ReturnsTrueAndDevice;
    [Test]
    procedure TryGetByAddress_NonExisting_ReturnsFalse;

    // Contains tests
    [Test]
    procedure Contains_ExistingDevice_ReturnsTrue;
    [Test]
    procedure Contains_NonExisting_ReturnsFalse;

    // AddOrUpdate tests
    [Test]
    procedure AddOrUpdate_NewDevice_AddsToRepository;
    [Test]
    procedure AddOrUpdate_ExistingDevice_UpdatesDevice;
    [Test]
    procedure AddOrUpdate_NewDevice_FiresListChanged;
    [Test]
    procedure AddOrUpdate_ExistingDevice_DoesNotFireListChanged;

    // UpdateConnectionState tests
    [Test]
    procedure UpdateConnectionState_ExistingDevice_UpdatesState;
    [Test]
    procedure UpdateConnectionState_NonExisting_ReturnsEmptyRecord;

    // Remove tests
    [Test]
    procedure Remove_ExistingDevice_RemovesFromRepository;
    [Test]
    procedure Remove_NonExisting_DoesNothing;
    [Test]
    procedure Remove_ExistingDevice_FiresListChanged;

    // Clear tests
    [Test]
    procedure Clear_WithDevices_RemovesAll;
    [Test]
    procedure Clear_WithDevices_FiresListChanged;
    [Test]
    procedure Clear_EmptyRepository_DoesNotFireListChanged;

    // Count tests
    [Test]
    procedure Count_EmptyRepository_ReturnsZero;
    [Test]
    procedure Count_WithDevices_ReturnsCorrectCount;
  end;

  /// <summary>
  /// Test fixture for TMockDeviceRepository.
  /// </summary>
  [TestFixture]
  TMockDeviceRepositoryTests = class
  private
    FRepository: TMockDeviceRepository;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Refresh_IncrementsCallCount;
    [Test]
    procedure AddOrUpdate_AddsDevice;
    [Test]
    procedure GetByAddress_ReturnsDevice;
  end;

  /// <summary>
  /// Test fixture for TBluetoothDeviceRepository with injected mock query.
  /// Tests the Refresh method using mock device enumeration.
  /// </summary>
  [TestFixture]
  TDeviceRepositoryRefreshTests = class
  private
    FMockQuery: TMockBluetoothDeviceQuery;
    FRepository: IDeviceRepository;
    FListChangedCount: Integer;
    procedure HandleListChanged(Sender: TObject);
    function CreateTestDevice(AAddress: UInt64; const AName: string;
      AState: TBluetoothConnectionState = csDisconnected): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Refresh_CallsEnumeratePairedDevices;
    [Test]
    procedure Refresh_PopulatesRepositoryFromQuery;
    [Test]
    procedure Refresh_ClearsExistingDevicesFirst;
    [Test]
    procedure Refresh_FiresListChanged;
    [Test]
    procedure Refresh_WithEmptyResult_ClearsRepository;
    [Test]
    procedure Refresh_MultipleDevices_AddsAll;
  end;

  /// <summary>
  /// Test fixture for TRegistryNameCache.
  /// Tests cache behavior including expiration and negative caching.
  /// </summary>
  [TestFixture]
  TRegistryNameCacheTests = class
  private
    FCache: TRegistryNameCache;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Basic Cache Operations }
    [Test]
    procedure TryGetCached_EmptyCache_ReturnsFalse;
    [Test]
    procedure CacheResult_Found_StoresNameAndFound;
    [Test]
    procedure CacheResult_NotFound_StoresNegativeResult;
    [Test]
    procedure TryGetCached_AfterCacheFound_ReturnsTrue;
    [Test]
    procedure TryGetCached_AfterCacheNotFound_ReturnsTrueWithFoundFalse;

    { Multiple Entries }
    [Test]
    procedure Cache_MultipleAddresses_StoresIndependently;
    [Test]
    procedure Cache_UpdateExisting_OverwritesPreviousValue;

    { Clear Operation }
    [Test]
    procedure Clear_RemovesAllEntries;

    { Count Property }
    [Test]
    procedure Count_EmptyCache_ReturnsZero;
    [Test]
    procedure Count_WithEntries_ReturnsCorrectCount;

    { Expiration Behavior - using short expiry }
    [Test]
    procedure TryGetCached_ExpiredEntry_ReturnsFalse;
  end;

implementation

{ TDeviceRepositoryTests }

procedure TDeviceRepositoryTests.Setup;
begin
  FRepository := CreateDeviceRepository;
  FListChangedCount := 0;
  FRepository.OnListChanged := HandleListChanged;
end;

procedure TDeviceRepositoryTests.HandleListChanged(Sender: TObject);
begin
  Inc(FListChangedCount);
end;

function TDeviceRepositoryTests.CreateTestDevice(AAddress: UInt64;
  const AName: string; AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address[0] := AAddress and $FF;
  Address[1] := (AAddress shr 8) and $FF;
  Address[2] := (AAddress shr 16) and $FF;
  Address[3] := (AAddress shr 24) and $FF;
  Address[4] := (AAddress shr 32) and $FF;
  Address[5] := (AAddress shr 40) and $FF;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AAddress,
    AName,
    btAudioOutput,
    AState,
    True,  // IsPaired
    True,  // IsAuthenticated
    0,     // ClassOfDevice
    Now,   // LastSeen
    Now    // LastUsed
  );
end;

procedure TDeviceRepositoryTests.GetAll_EmptyRepository_ReturnsEmptyArray;
begin
  Assert.AreEqual(Integer(0), Integer(Length(FRepository.GetAll)));
end;

procedure TDeviceRepositoryTests.GetAll_WithDevices_ReturnsAllDevices;
begin
  FRepository.AddOrUpdate(CreateTestDevice($111111111111, 'Device1'));
  FRepository.AddOrUpdate(CreateTestDevice($222222222222, 'Device2'));
  FRepository.AddOrUpdate(CreateTestDevice($333333333333, 'Device3'));

  Assert.AreEqual(Integer(3), Integer(Length(FRepository.GetAll)));
end;

procedure TDeviceRepositoryTests.GetByAddress_ExistingDevice_ReturnsDevice;
var
  Device: TBluetoothDeviceInfo;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  Device := FRepository.GetByAddress($AABBCCDDEEFF);

  Assert.AreEqual('TestDevice', Device.Name);
  Assert.AreEqual(UInt64($AABBCCDDEEFF), Device.AddressInt);
end;

procedure TDeviceRepositoryTests.GetByAddress_NonExisting_ReturnsEmptyRecord;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := FRepository.GetByAddress($999999999999);

  Assert.AreEqual(UInt64(0), Device.AddressInt);
end;

procedure TDeviceRepositoryTests.TryGetByAddress_ExistingDevice_ReturnsTrueAndDevice;
var
  Device: TBluetoothDeviceInfo;
  Found: Boolean;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  Found := FRepository.TryGetByAddress($AABBCCDDEEFF, Device);

  Assert.IsTrue(Found);
  Assert.AreEqual('TestDevice', Device.Name);
end;

procedure TDeviceRepositoryTests.TryGetByAddress_NonExisting_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
  Found: Boolean;
begin
  Found := FRepository.TryGetByAddress($999999999999, Device);

  Assert.IsFalse(Found);
end;

procedure TDeviceRepositoryTests.Contains_ExistingDevice_ReturnsTrue;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  Assert.IsTrue(FRepository.Contains($AABBCCDDEEFF));
end;

procedure TDeviceRepositoryTests.Contains_NonExisting_ReturnsFalse;
begin
  Assert.IsFalse(FRepository.Contains($999999999999));
end;

procedure TDeviceRepositoryTests.AddOrUpdate_NewDevice_AddsToRepository;
begin
  Assert.AreEqual(0, FRepository.Count);

  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  Assert.AreEqual(1, FRepository.Count);
  Assert.IsTrue(FRepository.Contains($AABBCCDDEEFF));
end;

procedure TDeviceRepositoryTests.AddOrUpdate_ExistingDevice_UpdatesDevice;
var
  Device: TBluetoothDeviceInfo;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'OldName', csDisconnected));
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'NewName', csConnected));

  Assert.AreEqual(1, FRepository.Count);
  Device := FRepository.GetByAddress($AABBCCDDEEFF);
  Assert.AreEqual('NewName', Device.Name);
  Assert.AreEqual(Ord(csConnected), Ord(Device.ConnectionState));
end;

procedure TDeviceRepositoryTests.AddOrUpdate_NewDevice_FiresListChanged;
begin
  FListChangedCount := 0;

  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  Assert.AreEqual(1, FListChangedCount);
end;

procedure TDeviceRepositoryTests.AddOrUpdate_ExistingDevice_DoesNotFireListChanged;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));
  FListChangedCount := 0;

  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'UpdatedDevice'));

  Assert.AreEqual(0, FListChangedCount);
end;

procedure TDeviceRepositoryTests.UpdateConnectionState_ExistingDevice_UpdatesState;
var
  Device: TBluetoothDeviceInfo;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice', csDisconnected));

  Device := FRepository.UpdateConnectionState($AABBCCDDEEFF, csConnected);

  Assert.AreEqual(Ord(csConnected), Ord(Device.ConnectionState));

  // Verify it was actually stored
  Device := FRepository.GetByAddress($AABBCCDDEEFF);
  Assert.AreEqual(Ord(csConnected), Ord(Device.ConnectionState));
end;

procedure TDeviceRepositoryTests.UpdateConnectionState_NonExisting_ReturnsEmptyRecord;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := FRepository.UpdateConnectionState($999999999999, csConnected);

  Assert.AreEqual(UInt64(0), Device.AddressInt);
end;

procedure TDeviceRepositoryTests.Remove_ExistingDevice_RemovesFromRepository;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));
  Assert.AreEqual(1, FRepository.Count);

  FRepository.Remove($AABBCCDDEEFF);

  Assert.AreEqual(0, FRepository.Count);
  Assert.IsFalse(FRepository.Contains($AABBCCDDEEFF));
end;

procedure TDeviceRepositoryTests.Remove_NonExisting_DoesNothing;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));

  FRepository.Remove($999999999999);

  Assert.AreEqual(1, FRepository.Count);
end;

procedure TDeviceRepositoryTests.Remove_ExistingDevice_FiresListChanged;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));
  FListChangedCount := 0;

  FRepository.Remove($AABBCCDDEEFF);

  Assert.AreEqual(1, FListChangedCount);
end;

procedure TDeviceRepositoryTests.Clear_WithDevices_RemovesAll;
begin
  FRepository.AddOrUpdate(CreateTestDevice($111111111111, 'Device1'));
  FRepository.AddOrUpdate(CreateTestDevice($222222222222, 'Device2'));
  Assert.AreEqual(2, FRepository.Count);

  FRepository.Clear;

  Assert.AreEqual(0, FRepository.Count);
end;

procedure TDeviceRepositoryTests.Clear_WithDevices_FiresListChanged;
begin
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));
  FListChangedCount := 0;

  FRepository.Clear;

  Assert.AreEqual(1, FListChangedCount);
end;

procedure TDeviceRepositoryTests.Clear_EmptyRepository_DoesNotFireListChanged;
begin
  FListChangedCount := 0;

  FRepository.Clear;

  Assert.AreEqual(0, FListChangedCount);
end;

procedure TDeviceRepositoryTests.Count_EmptyRepository_ReturnsZero;
begin
  Assert.AreEqual(0, FRepository.Count);
end;

procedure TDeviceRepositoryTests.Count_WithDevices_ReturnsCorrectCount;
begin
  FRepository.AddOrUpdate(CreateTestDevice($111111111111, 'Device1'));
  FRepository.AddOrUpdate(CreateTestDevice($222222222222, 'Device2'));

  Assert.AreEqual(2, FRepository.Count);
end;

{ TMockDeviceRepositoryTests }

procedure TMockDeviceRepositoryTests.Setup;
begin
  FRepository := TMockDeviceRepository.Create;
end;

procedure TMockDeviceRepositoryTests.TearDown;
begin
  FRepository.Free;
end;

procedure TMockDeviceRepositoryTests.Refresh_IncrementsCallCount;
begin
  Assert.AreEqual(0, FRepository.RefreshCallCount);
  FRepository.Refresh;
  Assert.AreEqual(1, FRepository.RefreshCallCount);
  FRepository.Refresh;
  Assert.AreEqual(2, FRepository.RefreshCallCount);
end;

procedure TMockDeviceRepositoryTests.AddOrUpdate_AddsDevice;
var
  Device: TBluetoothDeviceInfo;
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Device := TBluetoothDeviceInfo.Create(Address, $123456, 'Test', btAudioOutput,
    csDisconnected, True, True, 0, Now, Now);

  FRepository.AddOrUpdate(Device);

  Assert.AreEqual(1, FRepository.Count);
end;

procedure TMockDeviceRepositoryTests.GetByAddress_ReturnsDevice;
var
  Device, Retrieved: TBluetoothDeviceInfo;
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Device := TBluetoothDeviceInfo.Create(Address, $123456, 'TestDevice', btAudioOutput,
    csConnected, True, True, 0, Now, Now);

  FRepository.AddOrUpdate(Device);
  Retrieved := FRepository.GetByAddress($123456);

  Assert.AreEqual('TestDevice', Retrieved.Name);
end;

{ TDeviceRepositoryRefreshTests }

procedure TDeviceRepositoryRefreshTests.Setup;
begin
  FMockQuery := TMockBluetoothDeviceQuery.Create;
  // Note: TBluetoothDeviceRepository takes ownership via interface
  FRepository := TBluetoothDeviceRepository.Create(FMockQuery);
  FRepository.OnListChanged := HandleListChanged;
  FListChangedCount := 0;
end;

procedure TDeviceRepositoryRefreshTests.HandleListChanged(Sender: TObject);
begin
  Inc(FListChangedCount);
end;

function TDeviceRepositoryRefreshTests.CreateTestDevice(AAddress: UInt64;
  const AName: string; AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address[0] := AAddress and $FF;
  Address[1] := (AAddress shr 8) and $FF;
  Address[2] := (AAddress shr 16) and $FF;
  Address[3] := (AAddress shr 24) and $FF;
  Address[4] := (AAddress shr 32) and $FF;
  Address[5] := (AAddress shr 40) and $FF;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AAddress,
    AName,
    btAudioOutput,
    AState,
    True,  // IsPaired
    True,  // IsAuthenticated
    0,     // ClassOfDevice
    Now,   // LastSeen
    Now    // LastUsed
  );
end;

procedure TDeviceRepositoryRefreshTests.Refresh_CallsEnumeratePairedDevices;
begin
  Assert.AreEqual(0, FMockQuery.EnumerateCallCount);

  FRepository.Refresh;

  Assert.AreEqual(1, FMockQuery.EnumerateCallCount);
end;

procedure TDeviceRepositoryRefreshTests.Refresh_PopulatesRepositoryFromQuery;
var
  Device: TBluetoothDeviceInfo;
begin
  FMockQuery.AddDevice(CreateTestDevice($AABBCCDDEEFF, 'TestHeadphones', csConnected));

  FRepository.Refresh;

  Assert.AreEqual(1, FRepository.Count);
  Assert.IsTrue(FRepository.TryGetByAddress($AABBCCDDEEFF, Device));
  Assert.AreEqual('TestHeadphones', Device.Name);
  Assert.AreEqual(Ord(csConnected), Ord(Device.ConnectionState));
end;

procedure TDeviceRepositoryRefreshTests.Refresh_ClearsExistingDevicesFirst;
begin
  // Add device directly to repository
  FRepository.AddOrUpdate(CreateTestDevice($111111111111, 'ExistingDevice'));
  Assert.AreEqual(1, FRepository.Count);
  FListChangedCount := 0;

  // Setup mock to return different device
  FMockQuery.AddDevice(CreateTestDevice($222222222222, 'NewDevice'));

  FRepository.Refresh;

  // Old device should be gone, only new device present
  Assert.AreEqual(1, FRepository.Count);
  Assert.IsFalse(FRepository.Contains($111111111111));
  Assert.IsTrue(FRepository.Contains($222222222222));
end;

procedure TDeviceRepositoryRefreshTests.Refresh_FiresListChanged;
begin
  FMockQuery.AddDevice(CreateTestDevice($AABBCCDDEEFF, 'TestDevice'));
  FListChangedCount := 0;

  FRepository.Refresh;

  Assert.AreEqual(1, FListChangedCount);
end;

procedure TDeviceRepositoryRefreshTests.Refresh_WithEmptyResult_ClearsRepository;
begin
  // Add device directly
  FRepository.AddOrUpdate(CreateTestDevice($AABBCCDDEEFF, 'ExistingDevice'));
  Assert.AreEqual(1, FRepository.Count);

  // Mock returns empty - no devices configured

  FRepository.Refresh;

  Assert.AreEqual(0, FRepository.Count);
end;

procedure TDeviceRepositoryRefreshTests.Refresh_MultipleDevices_AddsAll;
begin
  FMockQuery.AddDevice(CreateTestDevice($111111111111, 'Device1'));
  FMockQuery.AddDevice(CreateTestDevice($222222222222, 'Device2'));
  FMockQuery.AddDevice(CreateTestDevice($333333333333, 'Device3'));

  FRepository.Refresh;

  Assert.AreEqual(3, FRepository.Count);
  Assert.IsTrue(FRepository.Contains($111111111111));
  Assert.IsTrue(FRepository.Contains($222222222222));
  Assert.IsTrue(FRepository.Contains($333333333333));
end;

{ TRegistryNameCacheTests }

procedure TRegistryNameCacheTests.Setup;
begin
  FCache := TRegistryNameCache.Create(60);  // 60 second expiry
end;

procedure TRegistryNameCacheTests.TearDown;
begin
  FCache.Free;
end;

procedure TRegistryNameCacheTests.TryGetCached_EmptyCache_ReturnsFalse;
var
  Name: string;
  Found: Boolean;
begin
  Assert.IsFalse(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
end;

procedure TRegistryNameCacheTests.CacheResult_Found_StoresNameAndFound;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($AABBCCDDEEFF, 'TestDevice', True);

  Assert.IsTrue(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
  Assert.AreEqual('TestDevice', Name);
  Assert.IsTrue(Found);
end;

procedure TRegistryNameCacheTests.CacheResult_NotFound_StoresNegativeResult;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($AABBCCDDEEFF, '', False);

  Assert.IsTrue(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
  Assert.AreEqual('', Name);
  Assert.IsFalse(Found);
end;

procedure TRegistryNameCacheTests.TryGetCached_AfterCacheFound_ReturnsTrue;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($AABBCCDDEEFF, 'CachedName', True);

  Assert.IsTrue(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
  Assert.IsTrue(Found);
  Assert.AreEqual('CachedName', Name);
end;

procedure TRegistryNameCacheTests.TryGetCached_AfterCacheNotFound_ReturnsTrueWithFoundFalse;
var
  Name: string;
  Found: Boolean;
begin
  // Cache a negative result (registry lookup failed)
  FCache.CacheResult($AABBCCDDEEFF, '', False);

  // Should return True (cache hit) but Found=False
  Assert.IsTrue(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
  Assert.IsFalse(Found, 'Found should be False for negative cached result');
end;

procedure TRegistryNameCacheTests.Cache_MultipleAddresses_StoresIndependently;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($111111111111, 'Device1', True);
  FCache.CacheResult($222222222222, 'Device2', True);
  FCache.CacheResult($333333333333, '', False);  // Negative result

  Assert.IsTrue(FCache.TryGetCached($111111111111, Name, Found));
  Assert.AreEqual('Device1', Name);
  Assert.IsTrue(Found);

  Assert.IsTrue(FCache.TryGetCached($222222222222, Name, Found));
  Assert.AreEqual('Device2', Name);
  Assert.IsTrue(Found);

  Assert.IsTrue(FCache.TryGetCached($333333333333, Name, Found));
  Assert.AreEqual('', Name);
  Assert.IsFalse(Found);
end;

procedure TRegistryNameCacheTests.Cache_UpdateExisting_OverwritesPreviousValue;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($AABBCCDDEEFF, 'OldName', True);
  FCache.CacheResult($AABBCCDDEEFF, 'NewName', True);

  Assert.IsTrue(FCache.TryGetCached($AABBCCDDEEFF, Name, Found));
  Assert.AreEqual('NewName', Name);
end;

procedure TRegistryNameCacheTests.Clear_RemovesAllEntries;
var
  Name: string;
  Found: Boolean;
begin
  FCache.CacheResult($111111111111, 'Device1', True);
  FCache.CacheResult($222222222222, 'Device2', True);

  FCache.Clear;

  Assert.IsFalse(FCache.TryGetCached($111111111111, Name, Found));
  Assert.IsFalse(FCache.TryGetCached($222222222222, Name, Found));
  Assert.AreEqual(0, FCache.Count);
end;

procedure TRegistryNameCacheTests.Count_EmptyCache_ReturnsZero;
begin
  Assert.AreEqual(0, FCache.Count);
end;

procedure TRegistryNameCacheTests.Count_WithEntries_ReturnsCorrectCount;
begin
  FCache.CacheResult($111111111111, 'Device1', True);
  FCache.CacheResult($222222222222, 'Device2', True);
  FCache.CacheResult($333333333333, '', False);

  Assert.AreEqual(3, FCache.Count);
end;

procedure TRegistryNameCacheTests.TryGetCached_ExpiredEntry_ReturnsFalse;
var
  ShortExpiryCache: TRegistryNameCache;
  Name: string;
  Found: Boolean;
begin
  // Create cache with 1-second expiry for testing
  ShortExpiryCache := TRegistryNameCache.Create(1);
  try
    ShortExpiryCache.CacheResult($AABBCCDDEEFF, 'TestDevice', True);

    // Should be valid immediately
    Assert.IsTrue(ShortExpiryCache.TryGetCached($AABBCCDDEEFF, Name, Found),
      'Cache entry should be valid immediately');

    // Wait for expiration
    Sleep(1100);  // 1.1 seconds

    // Should now be expired
    Assert.IsFalse(ShortExpiryCache.TryGetCached($AABBCCDDEEFF, Name, Found),
      'Cache entry should be expired after 1.1 seconds');
  finally
    ShortExpiryCache.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceRepositoryTests);
  TDUnitX.RegisterTestFixture(TMockDeviceRepositoryTests);
  TDUnitX.RegisterTestFixture(TDeviceRepositoryRefreshTests);
  TDUnitX.RegisterTestFixture(TRegistryNameCacheTests);

end.
