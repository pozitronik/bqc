{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Repository Unit Tests                    }
{                                                       }
{       Copyright (c) 2024                              }
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

initialization
  TDUnitX.RegisterTestFixture(TDeviceRepositoryTests);
  TDUnitX.RegisterTestFixture(TMockDeviceRepositoryTests);

end.
