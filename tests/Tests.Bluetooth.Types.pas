{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Bluetooth.Types Unit Tests                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.Bluetooth.Types;

interface

uses
  DUnitX.TestFramework,
  Bluetooth.Types;

type
  /// <summary>
  /// Test fixture for DetermineDeviceType function.
  /// Tests Class of Device parsing and device type detection.
  /// </summary>
  [TestFixture]
  TDetermineDeviceTypeTests = class
  public
    // Computer devices (Major class 0x01)
    [Test]
    procedure Computer_Desktop_ReturnsComputer;
    [Test]
    procedure Computer_Laptop_ReturnsComputer;

    // Phone devices (Major class 0x02)
    [Test]
    procedure Phone_Smartphone_ReturnsPhone;
    [Test]
    procedure Phone_Cellular_ReturnsPhone;

    // Audio/Video devices (Major class 0x04)
    [Test]
    procedure AudioVideo_Headset_ReturnsHeadset;
    [Test]
    procedure AudioVideo_Handsfree_ReturnsHeadset;
    [Test]
    procedure AudioVideo_Microphone_ReturnsAudioInput;
    [Test]
    procedure AudioVideo_Loudspeaker_ReturnsAudioOutput;
    [Test]
    procedure AudioVideo_Headphones_ReturnsAudioOutput;
    [Test]
    procedure AudioVideo_PortableAudio_ReturnsAudioOutput;
    [Test]
    procedure AudioVideo_HiFiAudio_ReturnsAudioOutput;
    [Test]
    procedure AudioVideo_Unknown_ReturnsAudioOutput;

    // Peripheral devices (Major class 0x05)
    [Test]
    procedure Peripheral_Keyboard_ReturnsKeyboard;
    [Test]
    procedure Peripheral_Mouse_ReturnsMouse;
    [Test]
    procedure Peripheral_Combo_ReturnsKeyboard;
    [Test]
    procedure Peripheral_Gamepad_ReturnsGamepad;
    [Test]
    procedure Peripheral_GenericHID_ReturnsHID;

    // Unknown/Other devices
    [Test]
    procedure Unknown_ZeroCoD_ReturnsUnknown;
    [Test]
    procedure Unknown_OtherMajorClass_ReturnsUnknown;
  end;

  /// <summary>
  /// Test fixture for TBluetoothDeviceInfo record.
  /// Tests factory methods, property getters, and helper methods.
  /// </summary>
  [TestFixture]
  TBluetoothDeviceInfoTests = class
  private
    function CreateTestDevice(
      ADeviceType: TBluetoothDeviceType;
      AConnectionState: TBluetoothConnectionState
    ): TBluetoothDeviceInfo;
  public
    // Factory method tests
    [Test]
    procedure Create_SetsAllFields;

    // AddressString tests
    [Test]
    procedure AddressString_FormatsCorrectly;
    [Test]
    procedure AddressString_ZeroAddress_FormatsWithZeros;

    // IsConnected tests
    [Test]
    procedure IsConnected_WhenConnected_ReturnsTrue;
    [Test]
    procedure IsConnected_WhenDisconnected_ReturnsFalse;
    [Test]
    procedure IsConnected_WhenConnecting_ReturnsFalse;
    [Test]
    procedure IsConnected_WhenDisconnecting_ReturnsFalse;
    [Test]
    procedure IsConnected_WhenError_ReturnsFalse;
    [Test]
    procedure IsConnected_WhenUnknown_ReturnsFalse;

    // IsAudioDevice tests
    [Test]
    procedure IsAudioDevice_AudioOutput_ReturnsTrue;
    [Test]
    procedure IsAudioDevice_AudioInput_ReturnsTrue;
    [Test]
    procedure IsAudioDevice_Headset_ReturnsTrue;
    [Test]
    procedure IsAudioDevice_Keyboard_ReturnsFalse;
    [Test]
    procedure IsAudioDevice_Unknown_ReturnsFalse;
    [Test]
    procedure IsAudioDevice_Computer_ReturnsFalse;
    [Test]
    procedure IsAudioDevice_Phone_ReturnsFalse;
    [Test]
    procedure IsAudioDevice_Mouse_ReturnsFalse;

    // IsInputDevice tests
    [Test]
    procedure IsInputDevice_Keyboard_ReturnsTrue;
    [Test]
    procedure IsInputDevice_Mouse_ReturnsTrue;
    [Test]
    procedure IsInputDevice_Gamepad_ReturnsTrue;
    [Test]
    procedure IsInputDevice_HID_ReturnsTrue;
    [Test]
    procedure IsInputDevice_AudioOutput_ReturnsFalse;
    [Test]
    procedure IsInputDevice_Computer_ReturnsFalse;
    [Test]
    procedure IsInputDevice_Phone_ReturnsFalse;
    [Test]
    procedure IsInputDevice_Headset_ReturnsFalse;
    [Test]
    procedure IsInputDevice_Unknown_ReturnsFalse;

    // WithConnectionState tests
    [Test]
    procedure WithConnectionState_ReturnsNewRecordWithUpdatedState;
    [Test]
    procedure WithConnectionState_PreservesOtherFields;

    // WithName tests
    [Test]
    procedure WithName_ReturnsNewRecordWithUpdatedName;
    [Test]
    procedure WithName_PreservesOtherFields;
    [Test]
    procedure WithName_EmptyString_SetsEmptyName;
    [Test]
    procedure WithName_UnicodeCharacters_PreservesUnicode;
  end;

  /// <summary>
  /// Test fixture for EBluetoothException and derived exceptions.
  /// </summary>
  [TestFixture]
  TBluetoothExceptionTests = class
  public
    [Test]
    procedure EBluetoothException_StoresMessage;
    [Test]
    procedure EBluetoothException_StoresErrorCode;
    [Test]
    procedure EBluetoothException_DefaultErrorCodeIsZero;
    [Test]
    procedure EBluetoothAdapterNotFound_IsBluetoothException;
    [Test]
    procedure EBluetoothDeviceError_IsBluetoothException;
  end;

  /// <summary>
  /// Test fixture for TBatteryStatus record.
  /// Tests factory methods and property accessors.
  /// </summary>
  [TestFixture]
  TBatteryStatusTests = class
  public
    // NotSupported tests
    [Test]
    procedure NotSupported_IsNotSupported;
    [Test]
    procedure NotSupported_LevelIsNegative;
    [Test]
    procedure NotSupported_HasLevelReturnsFalse;

    // Unknown tests
    [Test]
    procedure Unknown_IsSupported;
    [Test]
    procedure Unknown_LevelIsNegative;
    [Test]
    procedure Unknown_HasLevelReturnsFalse;

    // Create tests
    [Test]
    procedure Create_ValidLevel_SetsLevel;
    [Test]
    procedure Create_ValidLevel_IsSupported;
    [Test]
    procedure Create_ValidLevel_HasLevelReturnsTrue;
    [Test]
    procedure Create_ZeroLevel_IsValid;
    [Test]
    procedure Create_HundredLevel_IsValid;
    [Test]
    procedure Create_NegativeLevel_ClampsToZero;
    [Test]
    procedure Create_OverHundred_ClampsToHundred;
  end;

  /// <summary>
  /// Test fixture for address conversion functions.
  /// </summary>
  [TestFixture]
  TAddressConversionTests = class
  public
    [Test]
    procedure UInt64ToBluetoothAddress_ZeroValue_ReturnsZeroBytes;
    [Test]
    procedure UInt64ToBluetoothAddress_SimpleValue_ReturnsCorrectBytes;
    [Test]
    procedure UInt64ToBluetoothAddress_FullValue_ReturnsCorrectBytes;
    [Test]
    procedure BluetoothAddressToUInt64_ZeroBytes_ReturnsZero;
    [Test]
    procedure BluetoothAddressToUInt64_SimpleValue_ReturnsCorrectValue;
    [Test]
    procedure BluetoothAddressToUInt64_FullValue_ReturnsCorrectValue;
    [Test]
    procedure RoundTrip_UInt64ToAddressAndBack_PreservesValue;
    [Test]
    procedure RoundTrip_AddressToUInt64AndBack_PreservesValue;

    { FormatAddressAsMAC tests }
    [Test]
    procedure FormatAddressAsMAC_ZeroValue_ReturnsZeroMAC;
    [Test]
    procedure FormatAddressAsMAC_TypicalAddress_ReturnsCorrectFormat;
    [Test]
    procedure FormatAddressAsMAC_AllOnes_ReturnsAllFF;
    [Test]
    procedure FormatAddressAsMAC_SingleByte_ReturnsCorrectFormat;
    [Test]
    procedure FormatAddressAsMAC_MatchesAddressString;
  end;

implementation

uses
  System.SysUtils;

{ TDetermineDeviceTypeTests }

// Class of Device bit layout:
// Bits 0-1: Format type (always 00)
// Bits 2-7: Minor device class
// Bits 8-12: Major device class
// Bits 13-23: Service class

const
  // Major class shifted to position
  COD_COMPUTER    = $0100;  // Major class 0x01
  COD_PHONE       = $0200;  // Major class 0x02
  COD_AUDIO_VIDEO = $0400;  // Major class 0x04
  COD_PERIPHERAL  = $0500;  // Major class 0x05

  // Minor class values (already shifted to bits 2-7)
  MINOR_AV_HEADSET        = $04;   // 0x01 << 2
  MINOR_AV_HANDSFREE      = $08;   // 0x02 << 2
  MINOR_AV_MICROPHONE     = $10;   // 0x04 << 2
  MINOR_AV_LOUDSPEAKER    = $14;   // 0x05 << 2
  MINOR_AV_HEADPHONES     = $18;   // 0x06 << 2
  MINOR_AV_PORTABLE_AUDIO = $1C;   // 0x07 << 2
  MINOR_AV_HIFI_AUDIO     = $20;   // 0x08 << 2

  MINOR_PERIPH_KEYBOARD   = $40;   // 0x10 << 2
  MINOR_PERIPH_POINTING   = $80;   // 0x20 << 2
  MINOR_PERIPH_COMBO      = $C0;   // 0x30 << 2
  MINOR_PERIPH_GAMEPAD    = $20;   // 0x08 << 2 (in lower nibble)

procedure TDetermineDeviceTypeTests.Computer_Desktop_ReturnsComputer;
begin
  Assert.AreEqual(btComputer, DetermineDeviceType(COD_COMPUTER or $04)); // Desktop
end;

procedure TDetermineDeviceTypeTests.Computer_Laptop_ReturnsComputer;
begin
  Assert.AreEqual(btComputer, DetermineDeviceType(COD_COMPUTER or $0C)); // Laptop
end;

procedure TDetermineDeviceTypeTests.Phone_Smartphone_ReturnsPhone;
begin
  Assert.AreEqual(btPhone, DetermineDeviceType(COD_PHONE or $0C)); // Smartphone
end;

procedure TDetermineDeviceTypeTests.Phone_Cellular_ReturnsPhone;
begin
  Assert.AreEqual(btPhone, DetermineDeviceType(COD_PHONE or $04)); // Cellular
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Headset_ReturnsHeadset;
begin
  Assert.AreEqual(btHeadset, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_HEADSET));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Handsfree_ReturnsHeadset;
begin
  Assert.AreEqual(btHeadset, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_HANDSFREE));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Microphone_ReturnsAudioInput;
begin
  Assert.AreEqual(btAudioInput, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_MICROPHONE));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Loudspeaker_ReturnsAudioOutput;
begin
  Assert.AreEqual(btAudioOutput, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_LOUDSPEAKER));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Headphones_ReturnsAudioOutput;
begin
  Assert.AreEqual(btAudioOutput, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_HEADPHONES));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_PortableAudio_ReturnsAudioOutput;
begin
  Assert.AreEqual(btAudioOutput, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_PORTABLE_AUDIO));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_HiFiAudio_ReturnsAudioOutput;
begin
  Assert.AreEqual(btAudioOutput, DetermineDeviceType(COD_AUDIO_VIDEO or MINOR_AV_HIFI_AUDIO));
end;

procedure TDetermineDeviceTypeTests.AudioVideo_Unknown_ReturnsAudioOutput;
begin
  // Unknown minor class in audio/video should default to AudioOutput
  Assert.AreEqual(btAudioOutput, DetermineDeviceType(COD_AUDIO_VIDEO or $FC)); // Unknown minor
end;

procedure TDetermineDeviceTypeTests.Peripheral_Keyboard_ReturnsKeyboard;
begin
  Assert.AreEqual(btKeyboard, DetermineDeviceType(COD_PERIPHERAL or MINOR_PERIPH_KEYBOARD));
end;

procedure TDetermineDeviceTypeTests.Peripheral_Mouse_ReturnsMouse;
begin
  Assert.AreEqual(btMouse, DetermineDeviceType(COD_PERIPHERAL or MINOR_PERIPH_POINTING));
end;

procedure TDetermineDeviceTypeTests.Peripheral_Combo_ReturnsKeyboard;
begin
  // Combo keyboard+mouse should return keyboard
  Assert.AreEqual(btKeyboard, DetermineDeviceType(COD_PERIPHERAL or MINOR_PERIPH_COMBO));
end;

procedure TDetermineDeviceTypeTests.Peripheral_Gamepad_ReturnsGamepad;
begin
  Assert.AreEqual(btGamepad, DetermineDeviceType(COD_PERIPHERAL or MINOR_PERIPH_GAMEPAD));
end;

procedure TDetermineDeviceTypeTests.Peripheral_GenericHID_ReturnsHID;
begin
  // No keyboard/pointing/gamepad bits set
  Assert.AreEqual(btHID, DetermineDeviceType(COD_PERIPHERAL or $04));
end;

procedure TDetermineDeviceTypeTests.Unknown_ZeroCoD_ReturnsUnknown;
begin
  Assert.AreEqual(btUnknown, DetermineDeviceType(0));
end;

procedure TDetermineDeviceTypeTests.Unknown_OtherMajorClass_ReturnsUnknown;
begin
  Assert.AreEqual(btUnknown, DetermineDeviceType($0600)); // Major class 0x06 (Imaging)
end;

{ TBluetoothDeviceInfoTests }

function TBluetoothDeviceInfoTests.CreateTestDevice(
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  Address[0] := $AE;
  Address[1] := $5D;
  Address[2] := $01;
  Address[3] := $62;
  Address[4] := $18;
  Address[5] := $58;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    $581862015DAE,
    'Test Device',
    ADeviceType,
    AConnectionState,
    True,   // IsPaired
    True,   // IsAuthenticated
    $240404, // ClassOfDevice (headphones)
    Now,
    Now - 1
  );
end;

procedure TBluetoothDeviceInfoTests.Create_SetsAllFields;
var
  Address: TBluetoothAddress;
  Device: TBluetoothDeviceInfo;
  TestLastSeen, TestLastUsed: TDateTime;
begin
  Address[0] := $01;
  Address[1] := $02;
  Address[2] := $03;
  Address[3] := $04;
  Address[4] := $05;
  Address[5] := $06;
  TestLastSeen := Now;
  TestLastUsed := Now - 0.5;

  Device := TBluetoothDeviceInfo.Create(
    Address,
    $060504030201,
    'My Device',
    btAudioOutput,
    csConnected,
    True,
    False,
    $240404,
    TestLastSeen,
    TestLastUsed
  );

  Assert.AreEqual(UInt64($060504030201), Device.AddressInt);
  Assert.AreEqual('My Device', Device.Name);
  Assert.AreEqual(btAudioOutput, Device.DeviceType);
  Assert.AreEqual(csConnected, Device.ConnectionState);
  Assert.IsTrue(Device.IsPaired);
  Assert.IsFalse(Device.IsAuthenticated);
  Assert.AreEqual(Cardinal($240404), Device.ClassOfDevice);
  Assert.AreEqual(TestLastSeen, Device.LastSeen);
  Assert.AreEqual(TestLastUsed, Device.LastUsed);
end;

procedure TBluetoothDeviceInfoTests.AddressString_FormatsCorrectly;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csConnected);
  Assert.AreEqual('58:18:62:01:5D:AE', Device.AddressString);
end;

procedure TBluetoothDeviceInfoTests.AddressString_ZeroAddress_FormatsWithZeros;
var
  Address: TBluetoothAddress;
  Device: TBluetoothDeviceInfo;
begin
  FillChar(Address, SizeOf(Address), 0);
  Device := TBluetoothDeviceInfo.Create(
    Address, 0, 'Zero', btUnknown, csDisconnected, False, False, 0, 0, 0
  );
  Assert.AreEqual('00:00:00:00:00:00', Device.AddressString);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenConnected_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csConnected);
  Assert.IsTrue(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenDisconnected_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csDisconnected);
  Assert.IsFalse(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenConnecting_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csConnecting);
  Assert.IsFalse(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenDisconnecting_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csDisconnecting);
  Assert.IsFalse(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenError_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csError);
  Assert.IsFalse(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsConnected_WhenUnknown_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csUnknown);
  Assert.IsFalse(Device.IsConnected);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_AudioOutput_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csDisconnected);
  Assert.IsTrue(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_AudioInput_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioInput, csDisconnected);
  Assert.IsTrue(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Headset_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btHeadset, csDisconnected);
  Assert.IsTrue(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Keyboard_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btKeyboard, csDisconnected);
  Assert.IsFalse(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Unknown_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btUnknown, csDisconnected);
  Assert.IsFalse(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Computer_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btComputer, csDisconnected);
  Assert.IsFalse(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Phone_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btPhone, csDisconnected);
  Assert.IsFalse(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsAudioDevice_Mouse_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btMouse, csDisconnected);
  Assert.IsFalse(Device.IsAudioDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Keyboard_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btKeyboard, csDisconnected);
  Assert.IsTrue(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Mouse_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btMouse, csDisconnected);
  Assert.IsTrue(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Gamepad_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btGamepad, csDisconnected);
  Assert.IsTrue(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_HID_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btHID, csDisconnected);
  Assert.IsTrue(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_AudioOutput_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btAudioOutput, csDisconnected);
  Assert.IsFalse(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Computer_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btComputer, csDisconnected);
  Assert.IsFalse(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Phone_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btPhone, csDisconnected);
  Assert.IsFalse(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Headset_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btHeadset, csDisconnected);
  Assert.IsFalse(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.IsInputDevice_Unknown_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice(btUnknown, csDisconnected);
  Assert.IsFalse(Device.IsInputDevice);
end;

procedure TBluetoothDeviceInfoTests.WithConnectionState_ReturnsNewRecordWithUpdatedState;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);
  Updated := Original.WithConnectionState(csConnected);

  Assert.AreEqual(csConnected, Updated.ConnectionState);
  Assert.AreEqual(csDisconnected, Original.ConnectionState); // Original unchanged
end;

procedure TBluetoothDeviceInfoTests.WithConnectionState_PreservesOtherFields;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);
  Updated := Original.WithConnectionState(csConnected);

  Assert.AreEqual(Original.AddressInt, Updated.AddressInt);
  Assert.AreEqual(Original.Name, Updated.Name);
  Assert.AreEqual(Original.DeviceType, Updated.DeviceType);
  Assert.AreEqual(Original.IsPaired, Updated.IsPaired);
  Assert.AreEqual(Original.IsAuthenticated, Updated.IsAuthenticated);
end;

procedure TBluetoothDeviceInfoTests.WithName_ReturnsNewRecordWithUpdatedName;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);
  Updated := Original.WithName('New Name');

  Assert.AreEqual('New Name', Updated.Name);
  Assert.AreEqual('Test Device', Original.Name); // Original unchanged
end;

procedure TBluetoothDeviceInfoTests.WithName_PreservesOtherFields;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csConnected);
  Updated := Original.WithName('New Name');

  Assert.AreEqual(Original.AddressInt, Updated.AddressInt);
  Assert.AreEqual(Original.ConnectionState, Updated.ConnectionState);
  Assert.AreEqual(Original.DeviceType, Updated.DeviceType);
  Assert.AreEqual(Original.IsPaired, Updated.IsPaired);
end;

procedure TBluetoothDeviceInfoTests.WithName_EmptyString_SetsEmptyName;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);
  Updated := Original.WithName('');

  Assert.AreEqual('', Updated.Name);
end;

procedure TBluetoothDeviceInfoTests.WithName_UnicodeCharacters_PreservesUnicode;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);
  Updated := Original.WithName('Test Device - russkiy');

  Assert.AreEqual('Test Device - russkiy', Updated.Name);
end;

{ TBluetoothExceptionTests }

procedure TBluetoothExceptionTests.EBluetoothException_StoresMessage;
var
  E: EBluetoothException;
begin
  E := EBluetoothException.Create('Test error message', 0);
  try
    Assert.AreEqual('Test error message', E.Message);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothException_StoresErrorCode;
var
  E: EBluetoothException;
begin
  E := EBluetoothException.Create('Error', 12345);
  try
    Assert.AreEqual(Cardinal(12345), E.ErrorCode);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothException_DefaultErrorCodeIsZero;
var
  E: EBluetoothException;
begin
  E := EBluetoothException.Create('Error');
  try
    Assert.AreEqual(Cardinal(0), E.ErrorCode);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothAdapterNotFound_IsBluetoothException;
var
  E: EBluetoothAdapterNotFound;
begin
  E := EBluetoothAdapterNotFound.Create('Adapter not found', 0);
  try
    Assert.IsTrue(E is EBluetoothException);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothDeviceError_IsBluetoothException;
var
  E: EBluetoothDeviceError;
begin
  E := EBluetoothDeviceError.Create('Device error', 0);
  try
    Assert.IsTrue(E is EBluetoothException);
  finally
    E.Free;
  end;
end;

{ TBatteryStatusTests }

procedure TBatteryStatusTests.NotSupported_IsNotSupported;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.IsFalse(Status.IsSupported);
end;

procedure TBatteryStatusTests.NotSupported_LevelIsNegative;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.IsTrue(Status.Level < 0);
end;

procedure TBatteryStatusTests.NotSupported_HasLevelReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.IsFalse(Status.HasLevel);
end;

procedure TBatteryStatusTests.Unknown_IsSupported;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.IsTrue(Status.IsSupported);
end;

procedure TBatteryStatusTests.Unknown_LevelIsNegative;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.IsTrue(Status.Level < 0);
end;

procedure TBatteryStatusTests.Unknown_HasLevelReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.IsFalse(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_ValidLevel_SetsLevel;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(75);
  Assert.AreEqual(75, Status.Level);
end;

procedure TBatteryStatusTests.Create_ValidLevel_IsSupported;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(50);
  Assert.IsTrue(Status.IsSupported);
end;

procedure TBatteryStatusTests.Create_ValidLevel_HasLevelReturnsTrue;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(50);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_ZeroLevel_IsValid;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(0);
  Assert.AreEqual(0, Status.Level);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_HundredLevel_IsValid;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(100);
  Assert.AreEqual(100, Status.Level);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_NegativeLevel_ClampsToZero;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(-10);
  Assert.AreEqual(0, Status.Level);
end;

procedure TBatteryStatusTests.Create_OverHundred_ClampsToHundred;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(150);
  Assert.AreEqual(100, Status.Level);
end;

{ TAddressConversionTests }

procedure TAddressConversionTests.UInt64ToBluetoothAddress_ZeroValue_ReturnsZeroBytes;
var
  Address: TBluetoothAddress;
begin
  Address := UInt64ToBluetoothAddress(0);

  Assert.AreEqual(Byte(0), Address[0]);
  Assert.AreEqual(Byte(0), Address[1]);
  Assert.AreEqual(Byte(0), Address[2]);
  Assert.AreEqual(Byte(0), Address[3]);
  Assert.AreEqual(Byte(0), Address[4]);
  Assert.AreEqual(Byte(0), Address[5]);
end;

procedure TAddressConversionTests.UInt64ToBluetoothAddress_SimpleValue_ReturnsCorrectBytes;
var
  Address: TBluetoothAddress;
begin
  // Value $0102030405 should result in bytes [05, 04, 03, 02, 01, 00] (little-endian)
  Address := UInt64ToBluetoothAddress($0102030405);

  Assert.AreEqual(Byte($05), Address[0]);
  Assert.AreEqual(Byte($04), Address[1]);
  Assert.AreEqual(Byte($03), Address[2]);
  Assert.AreEqual(Byte($02), Address[3]);
  Assert.AreEqual(Byte($01), Address[4]);
  Assert.AreEqual(Byte($00), Address[5]);
end;

procedure TAddressConversionTests.UInt64ToBluetoothAddress_FullValue_ReturnsCorrectBytes;
var
  Address: TBluetoothAddress;
begin
  // Typical Bluetooth address: 58:18:62:01:5D:AE stored as $581862015DAE
  Address := UInt64ToBluetoothAddress($581862015DAE);

  Assert.AreEqual(Byte($AE), Address[0]);
  Assert.AreEqual(Byte($5D), Address[1]);
  Assert.AreEqual(Byte($01), Address[2]);
  Assert.AreEqual(Byte($62), Address[3]);
  Assert.AreEqual(Byte($18), Address[4]);
  Assert.AreEqual(Byte($58), Address[5]);
end;

procedure TAddressConversionTests.BluetoothAddressToUInt64_ZeroBytes_ReturnsZero;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Assert.AreEqual(UInt64(0), BluetoothAddressToUInt64(Address));
end;

procedure TAddressConversionTests.BluetoothAddressToUInt64_SimpleValue_ReturnsCorrectValue;
var
  Address: TBluetoothAddress;
begin
  Address[0] := $05;
  Address[1] := $04;
  Address[2] := $03;
  Address[3] := $02;
  Address[4] := $01;
  Address[5] := $00;

  Assert.AreEqual(UInt64($0102030405), BluetoothAddressToUInt64(Address));
end;

procedure TAddressConversionTests.BluetoothAddressToUInt64_FullValue_ReturnsCorrectValue;
var
  Address: TBluetoothAddress;
begin
  Address[0] := $AE;
  Address[1] := $5D;
  Address[2] := $01;
  Address[3] := $62;
  Address[4] := $18;
  Address[5] := $58;

  Assert.AreEqual(UInt64($581862015DAE), BluetoothAddressToUInt64(Address));
end;

procedure TAddressConversionTests.RoundTrip_UInt64ToAddressAndBack_PreservesValue;
var
  Original: UInt64;
  Address: TBluetoothAddress;
  Converted: UInt64;
begin
  Original := $AABBCCDDEEFF;
  Address := UInt64ToBluetoothAddress(Original);
  Converted := BluetoothAddressToUInt64(Address);

  Assert.AreEqual(Original, Converted);
end;

procedure TAddressConversionTests.RoundTrip_AddressToUInt64AndBack_PreservesValue;
var
  Original, Converted: TBluetoothAddress;
  Value: UInt64;
  I: Integer;
begin
  Original[0] := $11;
  Original[1] := $22;
  Original[2] := $33;
  Original[3] := $44;
  Original[4] := $55;
  Original[5] := $66;

  Value := BluetoothAddressToUInt64(Original);
  Converted := UInt64ToBluetoothAddress(Value);

  for I := 0 to 5 do
    Assert.AreEqual(Original[I], Converted[I], Format('Byte %d mismatch', [I]));
end;

procedure TAddressConversionTests.FormatAddressAsMAC_ZeroValue_ReturnsZeroMAC;
begin
  Assert.AreEqual('00:00:00:00:00:00', FormatAddressAsMAC(0));
end;

procedure TAddressConversionTests.FormatAddressAsMAC_TypicalAddress_ReturnsCorrectFormat;
begin
  // $581862015DAE should format as 58:18:62:01:5D:AE (big-endian)
  Assert.AreEqual('58:18:62:01:5D:AE', FormatAddressAsMAC($581862015DAE));
end;

procedure TAddressConversionTests.FormatAddressAsMAC_AllOnes_ReturnsAllFF;
begin
  // All 48 bits set to 1
  Assert.AreEqual('FF:FF:FF:FF:FF:FF', FormatAddressAsMAC($FFFFFFFFFFFF));
end;

procedure TAddressConversionTests.FormatAddressAsMAC_SingleByte_ReturnsCorrectFormat;
begin
  // Only lowest byte set
  Assert.AreEqual('00:00:00:00:00:AB', FormatAddressAsMAC($AB));

  // Only highest byte set
  Assert.AreEqual('CD:00:00:00:00:00', FormatAddressAsMAC(UInt64($CD) shl 40));
end;

procedure TAddressConversionTests.FormatAddressAsMAC_MatchesAddressString;
var
  Device: TBluetoothDeviceInfo;
  AddressInt: UInt64;
begin
  // Verify FormatAddressAsMAC produces same result as TBluetoothDeviceInfo.AddressString
  AddressInt := $AABBCCDDEEFF;
  Device := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AddressInt),
    AddressInt,
    'Test Device',
    btUnknown,
    csDisconnected,
    False, False, 0, 0, 0
  );

  Assert.AreEqual(Device.AddressString, FormatAddressAsMAC(AddressInt),
    'FormatAddressAsMAC should produce same result as TBluetoothDeviceInfo.AddressString');
end;

initialization
  TDUnitX.RegisterTestFixture(TDetermineDeviceTypeTests);
  TDUnitX.RegisterTestFixture(TBluetoothDeviceInfoTests);
  TDUnitX.RegisterTestFixture(TBluetoothExceptionTests);
  TDUnitX.RegisterTestFixture(TBatteryStatusTests);
  TDUnitX.RegisterTestFixture(TAddressConversionTests);

end.
