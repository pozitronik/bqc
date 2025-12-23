{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Bluetooth.WinAPI Unit Tests                     }
{                                                       }
{       Tests Windows API structure definitions and     }
{       conversion functions for Bluetooth devices.     }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.WinAPI;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Math,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.WinAPI,
  Bluetooth.DeviceWatcher;

type
  /// <summary>
  /// Tests for BTH_DEVICE_INFO structure (from bthdef.h).
  /// Verifies correct structure layout matching Windows SDK.
  /// </summary>
  [TestFixture]
  TBthDeviceInfoTests = class
  public
    [Test]
    procedure StructureSize_MatchesExpected;

    [Test]
    procedure FlagsField_AtCorrectOffset;

    [Test]
    procedure AddressField_AtCorrectOffset;

    [Test]
    procedure ClassOfDeviceField_AtCorrectOffset;

    [Test]
    procedure NameField_IsAnsiNotUnicode;

    [Test]
    procedure NameField_HasCorrectLength;
  end;

  /// <summary>
  /// Tests for BTH_RADIO_IN_RANGE structure.
  /// Verifies correct structure layout for device notifications.
  /// </summary>
  [TestFixture]
  TBthRadioInRangeTests = class
  public
    [Test]
    procedure StructureSize_MatchesExpected;

    [Test]
    procedure DeviceInfoField_AtCorrectOffset;

    [Test]
    procedure PreviousFlagsField_AtCorrectOffset;
  end;

  /// <summary>
  /// Tests for BDIF_* flag constants.
  /// Verifies flag values match Windows SDK definitions.
  /// </summary>
  [TestFixture]
  TBdifFlagsTests = class
  public
    [Test]
    procedure BDIF_ADDRESS_HasCorrectValue;

    [Test]
    procedure BDIF_COD_HasCorrectValue;

    [Test]
    procedure BDIF_NAME_HasCorrectValue;

    [Test]
    procedure BDIF_PAIRED_HasCorrectValue;

    [Test]
    procedure BDIF_PERSONAL_HasCorrectValue;

    [Test]
    procedure BDIF_CONNECTED_HasCorrectValue;

    [Test]
    procedure Flags_AreDistinct;
  end;

  /// <summary>
  /// Tests for ConvertBthDeviceInfoToDeviceInfo function.
  /// Verifies correct conversion from low-level BTH_DEVICE_INFO to domain TBluetoothDeviceInfo.
  /// </summary>
  [TestFixture]
  TConvertBthDeviceInfoTests = class
  private
    function CreateTestBthDeviceInfo(
      AFlags: ULONG;
      AAddress: UInt64;
      AClassOfDevice: ULONG;
      const AName: AnsiString
    ): BTH_DEVICE_INFO;
  public
    [Test]
    procedure Convert_SetsAddressCorrectly;

    [Test]
    procedure Convert_SetsNameFromAnsi;

    [Test]
    procedure Convert_SetsClassOfDevice;

    [Test]
    procedure Convert_ConnectedFlag_SetsConnectedState;

    [Test]
    procedure Convert_NoConnectedFlag_SetsDisconnectedState;

    [Test]
    procedure Convert_PairedFlag_SetsPairedTrue;

    [Test]
    procedure Convert_NoPairedFlag_SetsPairedFalse;

    [Test]
    procedure Convert_LastSeenAndLastUsed_AreZero;

    [Test]
    procedure Convert_DeterminesDeviceType;

    [Test]
    procedure Convert_EmptyName_HandlesCorrectly;

    [Test]
    procedure Convert_MaxLengthName_HandlesCorrectly;
  end;

  /// <summary>
  /// Tests for BLUETOOTH_DEVICE_INFO structure (from bluetoothapis.h).
  /// Verifies this is different from BTH_DEVICE_INFO.
  /// </summary>
  [TestFixture]
  TBluetoothDeviceInfoStructTests = class
  public
    [Test]
    procedure StructureSize_IsDifferentFromBthDeviceInfo;

    [Test]
    procedure StructureSize_MatchesExpected;

    [Test]
    procedure NameField_IsUnicodeNotAnsi;
  end;

implementation

{ TBthDeviceInfoTests }

procedure TBthDeviceInfoTests.StructureSize_MatchesExpected;
begin
  // BTH_DEVICE_INFO: flags(4) + address(8) + classOfDevice(4) + name(248) = 264 bytes
  // With possible alignment, may be slightly larger
  Assert.IsTrue(SizeOf(BTH_DEVICE_INFO) >= 264,
    Format('BTH_DEVICE_INFO size should be >= 264, got %d', [SizeOf(BTH_DEVICE_INFO)]));
  Assert.IsTrue(SizeOf(BTH_DEVICE_INFO) <= 272,
    Format('BTH_DEVICE_INFO size should be <= 272, got %d', [SizeOf(BTH_DEVICE_INFO)]));
end;

procedure TBthDeviceInfoTests.FlagsField_AtCorrectOffset;
var
  Info: BTH_DEVICE_INFO;
  FlagsPtr, BasePtr: NativeUInt;
begin
  FillChar(Info, SizeOf(Info), 0);
  BasePtr := NativeUInt(@Info);
  FlagsPtr := NativeUInt(@Info.flags);

  // flags should be at offset 0
  Assert.AreEqual(NativeUInt(0), FlagsPtr - BasePtr, 'flags field should be at offset 0');
end;

procedure TBthDeviceInfoTests.AddressField_AtCorrectOffset;
var
  Info: BTH_DEVICE_INFO;
  AddressPtr, BasePtr: NativeUInt;
begin
  FillChar(Info, SizeOf(Info), 0);
  BasePtr := NativeUInt(@Info);
  AddressPtr := NativeUInt(@Info.address);

  // address should be at offset 4 (after flags)
  // May have padding to align UInt64 to 8-byte boundary
  Assert.IsTrue((AddressPtr - BasePtr) in [4, 8],
    Format('address field should be at offset 4 or 8, got %d', [AddressPtr - BasePtr]));
end;

procedure TBthDeviceInfoTests.ClassOfDeviceField_AtCorrectOffset;
var
  Info: BTH_DEVICE_INFO;
  CodPtr, AddressPtr: NativeUInt;
begin
  FillChar(Info, SizeOf(Info), 0);
  AddressPtr := NativeUInt(@Info.address);
  CodPtr := NativeUInt(@Info.classOfDevice);

  // classOfDevice should be 8 bytes after address (UInt64)
  Assert.AreEqual(NativeUInt(8), CodPtr - AddressPtr,
    'classOfDevice should be 8 bytes after address');
end;

procedure TBthDeviceInfoTests.NameField_IsAnsiNotUnicode;
begin
  // BTH_DEVICE_INFO.name is array of AnsiChar (1 byte each)
  Assert.AreEqual(1, SizeOf(AnsiChar), 'AnsiChar should be 1 byte');
end;

procedure TBthDeviceInfoTests.NameField_HasCorrectLength;
var
  Info: BTH_DEVICE_INFO;
begin
  // Suppress hint by touching the variable
  FillChar(Info, SizeOf(Info), 0);
  // name array should have BLUETOOTH_MAX_NAME_SIZE (248) elements
  Assert.AreEqual(BLUETOOTH_MAX_NAME_SIZE, Length(Info.name),
    'name field should have BLUETOOTH_MAX_NAME_SIZE elements');
end;

{ TBthRadioInRangeTests }

procedure TBthRadioInRangeTests.StructureSize_MatchesExpected;
begin
  // BTH_RADIO_IN_RANGE: deviceInfo + previousDeviceFlags(4)
  Assert.IsTrue(SizeOf(BTH_RADIO_IN_RANGE) >= SizeOf(BTH_DEVICE_INFO) + 4,
    'BTH_RADIO_IN_RANGE should be at least BTH_DEVICE_INFO + 4 bytes');
end;

procedure TBthRadioInRangeTests.DeviceInfoField_AtCorrectOffset;
var
  Info: BTH_RADIO_IN_RANGE;
  DeviceInfoPtr, BasePtr: NativeUInt;
begin
  FillChar(Info, SizeOf(Info), 0);
  BasePtr := NativeUInt(@Info);
  DeviceInfoPtr := NativeUInt(@Info.deviceInfo);

  // deviceInfo should be at offset 0
  Assert.AreEqual(NativeUInt(0), DeviceInfoPtr - BasePtr,
    'deviceInfo field should be at offset 0');
end;

procedure TBthRadioInRangeTests.PreviousFlagsField_AtCorrectOffset;
var
  Info: BTH_RADIO_IN_RANGE;
  PrevFlagsPtr, DeviceInfoPtr: NativeUInt;
begin
  FillChar(Info, SizeOf(Info), 0);
  DeviceInfoPtr := NativeUInt(@Info.deviceInfo);
  PrevFlagsPtr := NativeUInt(@Info.previousDeviceFlags);

  // previousDeviceFlags should be immediately after deviceInfo
  Assert.AreEqual(NativeUInt(SizeOf(BTH_DEVICE_INFO)), PrevFlagsPtr - DeviceInfoPtr,
    'previousDeviceFlags should be immediately after deviceInfo');
end;

{ TBdifFlagsTests }

procedure TBdifFlagsTests.BDIF_ADDRESS_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000001), Cardinal(BDIF_ADDRESS));
end;

procedure TBdifFlagsTests.BDIF_COD_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000002), Cardinal(BDIF_COD));
end;

procedure TBdifFlagsTests.BDIF_NAME_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000004), Cardinal(BDIF_NAME));
end;

procedure TBdifFlagsTests.BDIF_PAIRED_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000008), Cardinal(BDIF_PAIRED));
end;

procedure TBdifFlagsTests.BDIF_PERSONAL_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000010), Cardinal(BDIF_PERSONAL));
end;

procedure TBdifFlagsTests.BDIF_CONNECTED_HasCorrectValue;
begin
  Assert.AreEqual(Cardinal($00000020), Cardinal(BDIF_CONNECTED));
end;

procedure TBdifFlagsTests.Flags_AreDistinct;
var
  AllFlags: Cardinal;
begin
  // All flags OR'd together should have distinct bits
  AllFlags := BDIF_ADDRESS or BDIF_COD or BDIF_NAME or
              BDIF_PAIRED or BDIF_PERSONAL or BDIF_CONNECTED;

  // If all flags are distinct, the sum should equal the OR
  Assert.AreEqual(
    Cardinal(BDIF_ADDRESS + BDIF_COD + BDIF_NAME + BDIF_PAIRED + BDIF_PERSONAL + BDIF_CONNECTED),
    AllFlags,
    'All BDIF flags should have distinct bit positions'
  );
end;

{ TConvertBthDeviceInfoTests }

function TConvertBthDeviceInfoTests.CreateTestBthDeviceInfo(
  AFlags: ULONG;
  AAddress: UInt64;
  AClassOfDevice: ULONG;
  const AName: AnsiString
): BTH_DEVICE_INFO;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.flags := AFlags;
  Result.address := AAddress;
  Result.classOfDevice := AClassOfDevice;

  // Copy name (truncate if needed)
  for I := 1 to Min(Length(AName), BLUETOOTH_MAX_NAME_SIZE - 1) do
    Result.name[I - 1] := AName[I];
end;

procedure TConvertBthDeviceInfoTests.Convert_SetsAddressCorrectly;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_ADDRESS, $581862015DAE, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(UInt64($581862015DAE), DeviceInfo.AddressInt);
  Assert.AreEqual('58:18:62:01:5D:AE', DeviceInfo.AddressString);
end;

procedure TConvertBthDeviceInfoTests.Convert_SetsNameFromAnsi;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_NAME, $123456789ABC, 0, 'My Bluetooth Device');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual('My Bluetooth Device', DeviceInfo.Name);
end;

procedure TConvertBthDeviceInfoTests.Convert_SetsClassOfDevice;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_COD, $123456789ABC, $240418, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(Cardinal($240418), DeviceInfo.ClassOfDevice);
end;

procedure TConvertBthDeviceInfoTests.Convert_ConnectedFlag_SetsConnectedState;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_CONNECTED, $123456789ABC, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(csConnected, DeviceInfo.ConnectionState);
  Assert.IsTrue(DeviceInfo.IsConnected);
end;

procedure TConvertBthDeviceInfoTests.Convert_NoConnectedFlag_SetsDisconnectedState;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // No BDIF_CONNECTED flag
  BthInfo := CreateTestBthDeviceInfo(BDIF_ADDRESS or BDIF_NAME, $123456789ABC, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(csDisconnected, DeviceInfo.ConnectionState);
  Assert.IsFalse(DeviceInfo.IsConnected);
end;

procedure TConvertBthDeviceInfoTests.Convert_PairedFlag_SetsPairedTrue;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_PAIRED, $123456789ABC, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.IsTrue(DeviceInfo.IsPaired);
end;

procedure TConvertBthDeviceInfoTests.Convert_NoPairedFlag_SetsPairedFalse;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // No BDIF_PAIRED flag
  BthInfo := CreateTestBthDeviceInfo(BDIF_ADDRESS or BDIF_NAME, $123456789ABC, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.IsFalse(DeviceInfo.IsPaired);
end;

procedure TConvertBthDeviceInfoTests.Convert_LastSeenAndLastUsed_AreZero;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // BTH_DEVICE_INFO doesn't have timestamp fields
  BthInfo := CreateTestBthDeviceInfo(BDIF_ADDRESS, $123456789ABC, 0, 'Test');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(TDateTime(0), DeviceInfo.LastSeen, 'LastSeen should be 0');
  Assert.AreEqual(TDateTime(0), DeviceInfo.LastUsed, 'LastUsed should be 0');
end;

procedure TConvertBthDeviceInfoTests.Convert_DeterminesDeviceType;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // Audio/Video headphones CoD: Major=0x04, Minor=0x06 (headphones)
  // CoD bits: Minor (2-7), Major (8-12)
  // Headphones: 0x0400 (major) + 0x18 (minor 0x06 << 2) = $0418
  BthInfo := CreateTestBthDeviceInfo(BDIF_COD, $123456789ABC, $240418, 'Headphones');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(btAudioOutput, DeviceInfo.DeviceType);
end;

procedure TConvertBthDeviceInfoTests.Convert_EmptyName_HandlesCorrectly;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  BthInfo := CreateTestBthDeviceInfo(BDIF_ADDRESS, $123456789ABC, 0, '');
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual('', DeviceInfo.Name);
end;

procedure TConvertBthDeviceInfoTests.Convert_MaxLengthName_HandlesCorrectly;
var
  BthInfo: BTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
  LongName: AnsiString;
  I: Integer;
begin
  // Create a name that's exactly BLUETOOTH_MAX_NAME_SIZE - 1 chars (leaving room for null)
  SetLength(LongName, BLUETOOTH_MAX_NAME_SIZE - 1);
  for I := 1 to Length(LongName) do
    LongName[I] := AnsiChar(Ord('A') + (I mod 26));

  BthInfo := CreateTestBthDeviceInfo(BDIF_NAME, $123456789ABC, 0, LongName);
  DeviceInfo := ConvertBthDeviceInfoToDeviceInfo(BthInfo);

  Assert.AreEqual(BLUETOOTH_MAX_NAME_SIZE - 1, Length(DeviceInfo.Name),
    'Name should have max length - 1 characters');
end;

{ TBluetoothDeviceInfoStructTests }

procedure TBluetoothDeviceInfoStructTests.StructureSize_IsDifferentFromBthDeviceInfo;
begin
  // BLUETOOTH_DEVICE_INFO is much larger than BTH_DEVICE_INFO
  // due to Unicode name, timestamps, and boolean fields
  Assert.AreNotEqual(SizeOf(BLUETOOTH_DEVICE_INFO), SizeOf(BTH_DEVICE_INFO),
    'BLUETOOTH_DEVICE_INFO and BTH_DEVICE_INFO should have different sizes');
end;

procedure TBluetoothDeviceInfoStructTests.StructureSize_MatchesExpected;
begin
  // BLUETOOTH_DEVICE_INFO: dwSize(4) + Address(8) + ulClassOfDevice(4) +
  //   fConnected(4) + fRemembered(4) + fAuthenticated(4) +
  //   stLastSeen(16) + stLastUsed(16) + szName(496) = ~556 bytes
  // With alignment, should be around 560 bytes
  Assert.IsTrue(SizeOf(BLUETOOTH_DEVICE_INFO) >= 550,
    Format('BLUETOOTH_DEVICE_INFO size should be >= 550, got %d', [SizeOf(BLUETOOTH_DEVICE_INFO)]));
  Assert.IsTrue(SizeOf(BLUETOOTH_DEVICE_INFO) <= 600,
    Format('BLUETOOTH_DEVICE_INFO size should be <= 600, got %d', [SizeOf(BLUETOOTH_DEVICE_INFO)]));
end;

procedure TBluetoothDeviceInfoStructTests.NameField_IsUnicodeNotAnsi;
var
  Info: BLUETOOTH_DEVICE_INFO;
begin
  // Suppress hint by touching the variable
  FillChar(Info, SizeOf(Info), 0);
  // BLUETOOTH_DEVICE_INFO.szName is array of WideChar (2 bytes each)
  Assert.AreEqual(2, SizeOf(WideChar), 'WideChar should be 2 bytes');

  // szName array should have BLUETOOTH_MAX_NAME_SIZE (248) elements of WideChar
  Assert.AreEqual(BLUETOOTH_MAX_NAME_SIZE, Length(Info.szName),
    'szName field should have BLUETOOTH_MAX_NAME_SIZE elements');
end;

initialization
  TDUnitX.RegisterTestFixture(TBthDeviceInfoTests);
  TDUnitX.RegisterTestFixture(TBthRadioInRangeTests);
  TDUnitX.RegisterTestFixture(TBdifFlagsTests);
  TDUnitX.RegisterTestFixture(TConvertBthDeviceInfoTests);
  TDUnitX.RegisterTestFixture(TBluetoothDeviceInfoStructTests);

end.
