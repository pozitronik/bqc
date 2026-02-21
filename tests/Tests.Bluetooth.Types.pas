{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Bluetooth.Types Unit Tests                      }
{                                                       }
{*******************************************************}

unit Tests.Bluetooth.Types;

interface

uses
  DUnitX.TestFramework,
  Bluetooth.Types,
  Bluetooth.Interfaces;

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

    // Edge case tests
    [Test]
    procedure Create_WithEmptyName_Succeeds;
    [Test]
    procedure Create_WithZeroAddress_Succeeds;
    [Test]
    procedure Create_WithMaxDateTime_Succeeds;
    [Test]
    procedure WithName_VeryLongString_Handles1000Chars;
    [Test]
    procedure WithName_SpecialCharacters_HandlesControlChars;
    [Test]
    procedure WithConnectionState_AllStates_ChainMultipleCalls;
    [Test]
    procedure AddressString_VerifyUppercaseFormat;
    [Test]
    procedure AddressString_VerifyColonPositions;
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

    // Edge case tests
    [Test]
    procedure EBluetoothException_EmptyMessage_Handles;
    [Test]
    procedure EBluetoothException_VeryLongMessage_Handles;
    [Test]
    procedure EBluetoothException_MaxErrorCode_Handles;
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

    // Pending tests
    [Test]
    procedure Pending_IsSupported;
    [Test]
    procedure Pending_LevelIsMinus2;
    [Test]
    procedure Pending_HasLevelReturnsFalse;
    [Test]
    procedure Pending_IsPendingReturnsTrue;
    [Test]
    procedure IsPending_NotSupported_ReturnsFalse;
    [Test]
    procedure IsPending_Unknown_ReturnsFalse;
    [Test]
    procedure IsPending_ValidLevel_ReturnsFalse;

    // Edge case tests
    [Test]
    procedure Create_BoundaryValueMinus1_ClampsCorrectly;
    [Test]
    procedure Create_BoundaryValue101_ClampsTo100;
    [Test]
    procedure Create_ExtremeValueMinus1000_Handles;
    [Test]
    procedure Create_ExtremeValue1000_Handles;
    [Test]
    procedure NotSupported_VerifyExactLevel_IsMinus1;
    [Test]
    procedure Unknown_VerifyExactLevel_IsMinus1;
    [Test]
    procedure HasLevel_BoundaryBetween0And1_ReturnsTrue;
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

    // Edge case tests
    [Test]
    procedure UInt64ToBluetoothAddress_AllOnes_MaxValue;
    [Test]
    procedure BluetoothAddressToUInt64_AllFF_Returns48BitMax;
    [Test]
    procedure FormatAddressAsMAC_LeadingZeros_PadsCorrectly;
  end;

  /// <summary>
  /// Test fixture for TConnectionResult record.
  /// Tests factory methods and result handling.
  /// </summary>
  [TestFixture]
  TConnectionResultTests = class
  public
    [Test]
    procedure Ok_ReturnsSuccessTrue;
    [Test]
    procedure Ok_ReturnsErrorCodeZero;
    [Test]
    procedure Fail_ReturnsSuccessFalse;
    [Test]
    procedure Fail_PreservesErrorCode;
    [Test]
    procedure Fail_WithZeroErrorCode_EdgeCase;
  end;

  /// <summary>
  /// Additional edge case tests for DetermineDeviceType function.
  /// </summary>
  [TestFixture]
  TDetermineDeviceTypeEdgeCaseTests = class
  public
    [Test]
    procedure DetermineDeviceType_AllBitsSet_FFFFFFFF;
    [Test]
    procedure DetermineDeviceType_MinorClassBoundary;
  end;

  /// <summary>
  /// Test fixture for TBluetoothProfile record.
  /// Tests factory method, immutable copy, display names, and state queries.
  /// </summary>
  [TestFixture]
  TBluetoothProfileTests = class
  public
    // Create tests
    [Test]
    procedure Create_SetsAllFields;
    [Test]
    procedure Create_WithUnknownType_Succeeds;

    // WithConnectionState tests
    [Test]
    procedure WithConnectionState_ReturnsCopyWithNewState;
    [Test]
    procedure WithConnectionState_PreservesOtherFields;

    // IsConnected tests
    [Test]
    procedure IsConnected_Connected_ReturnsTrue;
    [Test]
    procedure IsConnected_Available_ReturnsFalse;
    [Test]
    procedure IsConnected_Unknown_ReturnsFalse;

    // DisplayName tests - all profile types
    [Test]
    procedure DisplayName_A2DPSink_ReturnsCorrectName;
    [Test]
    procedure DisplayName_A2DPSource_ReturnsCorrectName;
    [Test]
    procedure DisplayName_HFP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_HSP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_AVRCP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_HID_ReturnsCorrectName;
    [Test]
    procedure DisplayName_SPP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_PAN_ReturnsCorrectName;
    [Test]
    procedure DisplayName_OBEX_ReturnsCorrectName;
    [Test]
    procedure DisplayName_PBAP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_MAP_ReturnsCorrectName;
    [Test]
    procedure DisplayName_Unknown_ReturnsUnknown;

    // ShortName tests - all profile types
    [Test]
    procedure ShortName_A2DPSink_ReturnsA2DP;
    [Test]
    procedure ShortName_A2DPSource_ReturnsA2DPSrc;
    [Test]
    procedure ShortName_HFP_ReturnsHFP;
    [Test]
    procedure ShortName_HSP_ReturnsHSP;
    [Test]
    procedure ShortName_AVRCP_ReturnsAVRCP;
    [Test]
    procedure ShortName_HID_ReturnsHID;
    [Test]
    procedure ShortName_SPP_ReturnsSPP;
    [Test]
    procedure ShortName_PAN_ReturnsPAN;
    [Test]
    procedure ShortName_OBEX_ReturnsOBEX;
    [Test]
    procedure ShortName_PBAP_ReturnsPBAP;
    [Test]
    procedure ShortName_MAP_ReturnsMAP;
    [Test]
    procedure ShortName_Unknown_ReturnsQuestionMark;
  end;

  /// <summary>
  /// Test fixture for TDeviceProfileInfo record.
  /// Tests profile collection queries: count, filtering, lookup.
  /// </summary>
  [TestFixture]
  TDeviceProfileInfoTests = class
  private
    function CreateProfile(AType: TBluetoothProfileType;
      AState: TProfileConnectionState): TBluetoothProfile;
  public
    // Factory tests
    [Test]
    procedure Create_SetsAllFields;
    [Test]
    procedure Empty_HasZeroProfiles;
    [Test]
    procedure Empty_SetsAddress;
    [Test]
    procedure Empty_HasZeroLastUpdated;

    // Count tests
    [Test]
    procedure Count_EmptyProfiles_ReturnsZero;
    [Test]
    procedure Count_ThreeProfiles_ReturnsThree;

    // ConnectedCount tests
    [Test]
    procedure ConnectedCount_NoneConnected_ReturnsZero;
    [Test]
    procedure ConnectedCount_AllConnected_ReturnsAll;
    [Test]
    procedure ConnectedCount_SomeConnected_ReturnsCorrect;

    // GetConnectedProfiles tests
    [Test]
    procedure GetConnectedProfiles_NoneConnected_ReturnsEmpty;
    [Test]
    procedure GetConnectedProfiles_AllConnected_ReturnsAll;
    [Test]
    procedure GetConnectedProfiles_Mixed_ReturnsOnlyConnected;

    // HasProfile tests
    [Test]
    procedure HasProfile_Exists_ReturnsTrue;
    [Test]
    procedure HasProfile_NotExists_ReturnsFalse;
    [Test]
    procedure HasProfile_EmptyProfiles_ReturnsFalse;

    // GetProfileState tests
    [Test]
    procedure GetProfileState_Exists_ReturnsState;
    [Test]
    procedure GetProfileState_NotExists_ReturnsUnknown;

    // HasProfiles tests
    [Test]
    procedure HasProfiles_WithProfiles_ReturnsTrue;
    [Test]
    procedure HasProfiles_Empty_ReturnsFalse;
  end;

  /// <summary>
  /// Test fixture for TPairingResult record.
  /// Tests all factory methods and IsSuccess logic.
  /// </summary>
  [TestFixture]
  TPairingResultTests = class
  public
    // Success
    [Test]
    procedure Success_StatusIsSuccess;
    [Test]
    procedure Success_ErrorCodeIsZero;
    [Test]
    procedure Success_IsSuccessReturnsTrue;
    [Test]
    procedure Success_HasMessage;

    // Failed
    [Test]
    procedure Failed_StatusIsFailed;
    [Test]
    procedure Failed_PreservesErrorCode;
    [Test]
    procedure Failed_PreservesMessage;
    [Test]
    procedure Failed_IsSuccessReturnsFalse;

    // Cancelled
    [Test]
    procedure Cancelled_StatusIsCancelled;
    [Test]
    procedure Cancelled_ErrorCodeIsZero;
    [Test]
    procedure Cancelled_IsSuccessReturnsFalse;

    // AlreadyPaired
    [Test]
    procedure AlreadyPaired_StatusIsAlreadyPaired;
    [Test]
    procedure AlreadyPaired_IsSuccessReturnsTrue;

    // Timeout
    [Test]
    procedure Timeout_StatusIsTimeout;
    [Test]
    procedure Timeout_IsSuccessReturnsFalse;

    // NotSupported
    [Test]
    procedure NotSupported_StatusIsNotSupported;
    [Test]
    procedure NotSupported_IsSuccessReturnsFalse;
    [Test]
    procedure NotSupported_DefaultMessage_NoPrefixedReason;
    [Test]
    procedure NotSupported_WithReason_IncludesReason;
  end;

  /// <summary>
  /// Test fixture for GuidToProfileType function.
  /// Verifies GUID-to-profile-type mapping for all known Bluetooth service UUIDs.
  /// </summary>
  [TestFixture]
  TGuidToProfileTypeTests = class
  private
    /// <summary>
    /// Creates a Bluetooth base UUID with the given short UUID.
    /// Format: 0000xxxx-0000-1000-8000-00805F9B34FB
    /// </summary>
    function MakeBluetoothGuid(AShortUUID: Word): TGUID;
  public
    // Audio profiles
    [Test]
    procedure AudioSource_110A_ReturnsSink;
    [Test]
    procedure AudioSink_110B_ReturnsSink;
    [Test]
    procedure AVRCPTarget_110C_ReturnsAVRCP;
    [Test]
    procedure AVRCPController_110E_ReturnsAVRCP;

    // Voice profiles
    [Test]
    procedure Headset_1108_ReturnsHSP;
    [Test]
    procedure HeadsetAG_1112_ReturnsHSP;
    [Test]
    procedure HandsFree_111E_ReturnsHFP;
    [Test]
    procedure HandsFreeAG_111F_ReturnsHFP;

    // HID
    [Test]
    procedure HID_1124_ReturnsHID;

    // Serial
    [Test]
    procedure SerialPort_1101_ReturnsSPP;

    // Network
    [Test]
    procedure PANU_1115_ReturnsPAN;
    [Test]
    procedure NAP_1116_ReturnsPAN;
    [Test]
    procedure GN_1117_ReturnsPAN;

    // Object transfer
    [Test]
    procedure OBEXPush_1105_ReturnsOBEX;
    [Test]
    procedure OBEXFileTransfer_1106_ReturnsOBEX;

    // Phone profiles
    [Test]
    procedure PBAP_PSE_112F_ReturnsPBAP;
    [Test]
    procedure PBAP_PCE_1130_ReturnsPBAP;
    [Test]
    procedure MAP_Server_1132_ReturnsMAP;
    [Test]
    procedure MAP_Notification_1133_ReturnsMAP;
    [Test]
    procedure MAP_Profile_1134_ReturnsMAP;

    // Unknown
    [Test]
    procedure UnknownGuid_ReturnsUnknown;
    [Test]
    procedure EmptyGuid_ReturnsUnknown;
  end;

  /// <summary>
  /// Test fixture for PsmToProfileType function.
  /// Verifies L2CAP PSM-to-profile-type mapping.
  /// </summary>
  [TestFixture]
  TPsmToProfileTypeTests = class
  public
    [Test]
    procedure SDP_0001_ReturnsUnknown;
    [Test]
    procedure RFCOMM_0003_ReturnsSPP;
    [Test]
    procedure BNEP_000F_ReturnsPAN;
    [Test]
    procedure HIDControl_0011_ReturnsHID;
    [Test]
    procedure HIDInterrupt_0013_ReturnsHID;
    [Test]
    procedure AVCTP_0017_ReturnsAVRCP;
    [Test]
    procedure AVDTP_0019_ReturnsA2DPSink;
    [Test]
    procedure AVCTPBrowsing_001B_ReturnsAVRCP;
    [Test]
    procedure ATT_001F_ReturnsUnknown;
    [Test]
    procedure UnknownPSM_ReturnsUnknown;
  end;

  /// <summary>
  /// Test fixture for GetShortUUID function.
  /// </summary>
  [TestFixture]
  TGetShortUUIDTests = class
  public
    [Test]
    procedure GetShortUUID_BluetoothBaseUUID_ReturnsShortValue;
    [Test]
    procedure GetShortUUID_ZeroGuid_ReturnsZero;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

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
  MINOR_PERIPH_GAMEPAD    = $08;   // 0x02 << 2 (Bluetooth spec: gamepad subtype = 2)

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

procedure TBluetoothDeviceInfoTests.Create_WithEmptyName_Succeeds;
var
  Address: TBluetoothAddress;
  Device: TBluetoothDeviceInfo;
begin
  FillChar(Address, SizeOf(Address), 0);
  Device := TBluetoothDeviceInfo.Create(
    Address, $123456789ABC, '', btUnknown, csDisconnected,
    False, False, 0, 0, 0
  );

  Assert.AreEqual('', Device.Name);
  Assert.AreEqual(UInt64($123456789ABC), Device.AddressInt);
end;

procedure TBluetoothDeviceInfoTests.Create_WithZeroAddress_Succeeds;
var
  Address: TBluetoothAddress;
  Device: TBluetoothDeviceInfo;
begin
  FillChar(Address, SizeOf(Address), 0);
  Device := TBluetoothDeviceInfo.Create(
    Address, 0, 'Zero Address Device', btUnknown, csDisconnected,
    False, False, 0, 0, 0
  );

  Assert.AreEqual(UInt64(0), Device.AddressInt);
  Assert.AreEqual('00:00:00:00:00:00', Device.AddressString);
end;

procedure TBluetoothDeviceInfoTests.Create_WithMaxDateTime_Succeeds;
var
  Address: TBluetoothAddress;
  Device: TBluetoothDeviceInfo;
  MaxDate: TDateTime;
begin
  FillChar(Address, SizeOf(Address), 0);
  MaxDate := MaxDateTime;

  Device := TBluetoothDeviceInfo.Create(
    Address, 0, 'Max Date Device', btUnknown, csDisconnected,
    False, False, 0, MaxDate, MaxDate
  );

  Assert.AreEqual(MaxDate, Device.LastSeen);
  Assert.AreEqual(MaxDate, Device.LastUsed);
end;

procedure TBluetoothDeviceInfoTests.WithName_VeryLongString_Handles1000Chars;
var
  Original, Updated: TBluetoothDeviceInfo;
  LongName: string;
  I: Integer;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);

  // Create a 1000 character string
  SetLength(LongName, 1000);
  for I := 1 to 1000 do
    LongName[I] := Char(Ord('A') + (I mod 26));

  Updated := Original.WithName(LongName);

  Assert.AreEqual(1000, Length(Updated.Name));
  Assert.AreEqual(LongName, Updated.Name);
end;

procedure TBluetoothDeviceInfoTests.WithName_SpecialCharacters_HandlesControlChars;
var
  Original, Updated: TBluetoothDeviceInfo;
  SpecialName: string;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);

  // Create string with control characters: NUL, TAB, CR, LF
  SpecialName := 'Test' + #0 + 'With' + #9 + 'Control' + #13#10 + 'Chars';

  Updated := Original.WithName(SpecialName);

  Assert.AreEqual(SpecialName, Updated.Name);
  Assert.IsTrue(Pos(#0, Updated.Name) > 0, 'NUL character should be preserved');
  Assert.IsTrue(Pos(#9, Updated.Name) > 0, 'TAB character should be preserved');
  Assert.IsTrue(Pos(#13, Updated.Name) > 0, 'CR character should be preserved');
  Assert.IsTrue(Pos(#10, Updated.Name) > 0, 'LF character should be preserved');
end;

procedure TBluetoothDeviceInfoTests.WithConnectionState_AllStates_ChainMultipleCalls;
var
  Original, Updated: TBluetoothDeviceInfo;
begin
  Original := CreateTestDevice(btAudioOutput, csDisconnected);

  // Chain multiple state changes
  Updated := Original
    .WithConnectionState(csConnecting)
    .WithConnectionState(csConnected)
    .WithConnectionState(csDisconnecting)
    .WithConnectionState(csDisconnected);

  Assert.AreEqual(csDisconnected, Updated.ConnectionState);
  // Original should remain unchanged
  Assert.AreEqual(csDisconnected, Original.ConnectionState);
end;

procedure TBluetoothDeviceInfoTests.AddressString_VerifyUppercaseFormat;
var
  Device: TBluetoothDeviceInfo;
  AddressStr: string;
  I: Integer;
begin
  Device := CreateTestDevice(btAudioOutput, csConnected);
  AddressStr := Device.AddressString;

  // Verify all hex characters are uppercase
  for I := 1 to Length(AddressStr) do
    if CharInSet(AddressStr[I], ['a'..'f']) then
      Assert.Fail('Address should use uppercase hex: ' + AddressStr);
end;

procedure TBluetoothDeviceInfoTests.AddressString_VerifyColonPositions;
var
  Device: TBluetoothDeviceInfo;
  AddressStr: string;
begin
  Device := CreateTestDevice(btAudioOutput, csConnected);
  AddressStr := Device.AddressString;

  // Verify format: XX:XX:XX:XX:XX:XX (17 chars total)
  Assert.AreEqual(17, Length(AddressStr), 'Address string should be 17 characters');
  Assert.AreEqual(':', AddressStr[3], 'Colon at position 3');
  Assert.AreEqual(':', AddressStr[6], 'Colon at position 6');
  Assert.AreEqual(':', AddressStr[9], 'Colon at position 9');
  Assert.AreEqual(':', AddressStr[12], 'Colon at position 12');
  Assert.AreEqual(':', AddressStr[15], 'Colon at position 15');
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

procedure TBluetoothExceptionTests.EBluetoothException_EmptyMessage_Handles;
var
  E: EBluetoothException;
begin
  E := EBluetoothException.Create('', 0);
  try
    Assert.AreEqual('', E.Message);
    Assert.AreEqual(Cardinal(0), E.ErrorCode);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothException_VeryLongMessage_Handles;
var
  E: EBluetoothException;
  LongMessage: string;
  I: Integer;
begin
  // Create a 10000 character message
  SetLength(LongMessage, 10000);
  for I := 1 to 10000 do
    LongMessage[I] := Char(Ord('A') + (I mod 26));

  E := EBluetoothException.Create(LongMessage, 42);
  try
    Assert.AreEqual(10000, Length(E.Message));
    Assert.AreEqual(LongMessage, E.Message);
    Assert.AreEqual(Cardinal(42), E.ErrorCode);
  finally
    E.Free;
  end;
end;

procedure TBluetoothExceptionTests.EBluetoothException_MaxErrorCode_Handles;
var
  E: EBluetoothException;
begin
  // Test with maximum Cardinal value ($FFFFFFFF)
  E := EBluetoothException.Create('Max error code test', $FFFFFFFF);
  try
    Assert.AreEqual(Cardinal($FFFFFFFF), E.ErrorCode);
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

procedure TBatteryStatusTests.Create_BoundaryValueMinus1_ClampsCorrectly;
var
  Status: TBatteryStatus;
begin
  // -1 should be clamped to 0
  Status := TBatteryStatus.Create(-1);
  Assert.AreEqual(0, Status.Level);
  Assert.IsTrue(Status.IsSupported);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_BoundaryValue101_ClampsTo100;
var
  Status: TBatteryStatus;
begin
  // 101 should be clamped to 100
  Status := TBatteryStatus.Create(101);
  Assert.AreEqual(100, Status.Level);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_ExtremeValueMinus1000_Handles;
var
  Status: TBatteryStatus;
begin
  // Extreme negative value should be clamped to 0
  Status := TBatteryStatus.Create(-1000);
  Assert.AreEqual(0, Status.Level);
  Assert.IsTrue(Status.IsSupported);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.Create_ExtremeValue1000_Handles;
var
  Status: TBatteryStatus;
begin
  // Extreme positive value should be clamped to 100
  Status := TBatteryStatus.Create(1000);
  Assert.AreEqual(100, Status.Level);
  Assert.IsTrue(Status.HasLevel);
end;

procedure TBatteryStatusTests.NotSupported_VerifyExactLevel_IsMinus1;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.AreEqual(-1, Status.Level);
end;

procedure TBatteryStatusTests.Unknown_VerifyExactLevel_IsMinus1;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.AreEqual(-1, Status.Level);
end;

procedure TBatteryStatusTests.HasLevel_BoundaryBetween0And1_ReturnsTrue;
var
  Status0, Status1: TBatteryStatus;
begin
  // Test boundary values 0 and 1 - both should have valid levels
  Status0 := TBatteryStatus.Create(0);
  Status1 := TBatteryStatus.Create(1);

  Assert.IsTrue(Status0.HasLevel, 'Level 0 should be valid');
  Assert.IsTrue(Status1.HasLevel, 'Level 1 should be valid');
  Assert.AreEqual(0, Status0.Level);
  Assert.AreEqual(1, Status1.Level);
end;

procedure TBatteryStatusTests.Pending_IsSupported;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Pending;
  Assert.IsTrue(Status.IsSupported);
end;

procedure TBatteryStatusTests.Pending_LevelIsMinus2;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Pending;
  Assert.AreEqual(-2, Status.Level);
end;

procedure TBatteryStatusTests.Pending_HasLevelReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Pending;
  Assert.IsFalse(Status.HasLevel);
end;

procedure TBatteryStatusTests.Pending_IsPendingReturnsTrue;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Pending;
  Assert.IsTrue(Status.IsPending);
end;

procedure TBatteryStatusTests.IsPending_NotSupported_ReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.IsFalse(Status.IsPending);
end;

procedure TBatteryStatusTests.IsPending_Unknown_ReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.IsFalse(Status.IsPending);
end;

procedure TBatteryStatusTests.IsPending_ValidLevel_ReturnsFalse;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(50);
  Assert.IsFalse(Status.IsPending);
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

procedure TAddressConversionTests.UInt64ToBluetoothAddress_AllOnes_MaxValue;
var
  Address: TBluetoothAddress;
begin
  // Test with maximum 48-bit value (all ones)
  Address := UInt64ToBluetoothAddress($FFFFFFFFFFFF);

  Assert.AreEqual(Byte($FF), Address[0]);
  Assert.AreEqual(Byte($FF), Address[1]);
  Assert.AreEqual(Byte($FF), Address[2]);
  Assert.AreEqual(Byte($FF), Address[3]);
  Assert.AreEqual(Byte($FF), Address[4]);
  Assert.AreEqual(Byte($FF), Address[5]);
end;

procedure TAddressConversionTests.BluetoothAddressToUInt64_AllFF_Returns48BitMax;
var
  Address: TBluetoothAddress;
  Value: UInt64;
begin
  // All bytes set to $FF
  FillChar(Address, SizeOf(Address), $FF);
  Value := BluetoothAddressToUInt64(Address);

  // Should return $FFFFFFFFFFFF (48 bits all set)
  Assert.AreEqual(UInt64($FFFFFFFFFFFF), Value);
end;

procedure TAddressConversionTests.FormatAddressAsMAC_LeadingZeros_PadsCorrectly;
begin
  // Test that leading zeros are properly padded
  // Address with zeros in high bytes
  Assert.AreEqual('00:01:02:03:04:05', FormatAddressAsMAC($000102030405));

  // Address with zeros in low bytes
  Assert.AreEqual('AB:00:00:00:00:00', FormatAddressAsMAC(UInt64($AB) shl 40));

  // Mixed zeros
  Assert.AreEqual('0A:00:0B:00:0C:00', FormatAddressAsMAC($0A000B000C00));
end;

{ TConnectionResultTests }

procedure TConnectionResultTests.Ok_ReturnsSuccessTrue;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Ok;
  Assert.IsTrue(Result.Success);
end;

procedure TConnectionResultTests.Ok_ReturnsErrorCodeZero;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Ok;
  Assert.AreEqual(Cardinal(0), Result.ErrorCode);
end;

procedure TConnectionResultTests.Fail_ReturnsSuccessFalse;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Fail(42);
  Assert.IsFalse(Result.Success);
end;

procedure TConnectionResultTests.Fail_PreservesErrorCode;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Fail(12345);
  Assert.AreEqual(Cardinal(12345), Result.ErrorCode);

  // Test with maximum Cardinal value
  Result := TConnectionResult.Fail($FFFFFFFF);
  Assert.AreEqual(Cardinal($FFFFFFFF), Result.ErrorCode);
end;

procedure TConnectionResultTests.Fail_WithZeroErrorCode_EdgeCase;
var
  Result: TConnectionResult;
begin
  // Edge case: Fail with zero error code - still fails but error code is 0
  Result := TConnectionResult.Fail(0);
  Assert.IsFalse(Result.Success);
  Assert.AreEqual(Cardinal(0), Result.ErrorCode);
end;

{ TBluetoothProfileTests }

procedure TBluetoothProfileTests.Create_SetsAllFields;
var
  Profile: TBluetoothProfile;
  Guid: TGUID;
begin
  Guid := StringToGUID('{0000110B-0000-1000-8000-00805F9B34FB}');
  Profile := TBluetoothProfile.Create(bptA2DPSink, Guid, pcsConnected);

  Assert.AreEqual(bptA2DPSink, Profile.ProfileType);
  Assert.IsTrue(IsEqualGUID(Guid, Profile.ServiceGuid));
  Assert.AreEqual(pcsConnected, Profile.ConnectionState);
end;

procedure TBluetoothProfileTests.Create_WithUnknownType_Succeeds;
var
  Profile: TBluetoothProfile;
begin
  Profile := TBluetoothProfile.Create(bptUnknown, TGUID.Empty, pcsUnknown);

  Assert.AreEqual(bptUnknown, Profile.ProfileType);
  Assert.AreEqual(pcsUnknown, Profile.ConnectionState);
end;

procedure TBluetoothProfileTests.WithConnectionState_ReturnsCopyWithNewState;
var
  Original, Updated: TBluetoothProfile;
begin
  Original := TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsAvailable);
  Updated := Original.WithConnectionState(pcsConnected);

  Assert.AreEqual(pcsConnected, Updated.ConnectionState);
  Assert.AreEqual(pcsAvailable, Original.ConnectionState);
end;

procedure TBluetoothProfileTests.WithConnectionState_PreservesOtherFields;
var
  Original, Updated: TBluetoothProfile;
  Guid: TGUID;
begin
  Guid := StringToGUID('{0000110B-0000-1000-8000-00805F9B34FB}');
  Original := TBluetoothProfile.Create(bptHFP, Guid, pcsAvailable);
  Updated := Original.WithConnectionState(pcsConnected);

  Assert.AreEqual(bptHFP, Updated.ProfileType);
  Assert.IsTrue(IsEqualGUID(Guid, Updated.ServiceGuid));
end;

procedure TBluetoothProfileTests.IsConnected_Connected_ReturnsTrue;
var
  Profile: TBluetoothProfile;
begin
  Profile := TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsConnected);
  Assert.IsTrue(Profile.IsConnected);
end;

procedure TBluetoothProfileTests.IsConnected_Available_ReturnsFalse;
var
  Profile: TBluetoothProfile;
begin
  Profile := TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsAvailable);
  Assert.IsFalse(Profile.IsConnected);
end;

procedure TBluetoothProfileTests.IsConnected_Unknown_ReturnsFalse;
var
  Profile: TBluetoothProfile;
begin
  Profile := TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsUnknown);
  Assert.IsFalse(Profile.IsConnected);
end;

procedure TBluetoothProfileTests.DisplayName_A2DPSink_ReturnsCorrectName;
begin
  Assert.AreEqual('A2DP (Audio Sink)',
    TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_A2DPSource_ReturnsCorrectName;
begin
  Assert.AreEqual('A2DP (Audio Source)',
    TBluetoothProfile.Create(bptA2DPSource, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_HFP_ReturnsCorrectName;
begin
  Assert.AreEqual('Hands-Free',
    TBluetoothProfile.Create(bptHFP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_HSP_ReturnsCorrectName;
begin
  Assert.AreEqual('Headset',
    TBluetoothProfile.Create(bptHSP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_AVRCP_ReturnsCorrectName;
begin
  Assert.AreEqual('Remote Control',
    TBluetoothProfile.Create(bptAVRCP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_HID_ReturnsCorrectName;
begin
  Assert.AreEqual('HID',
    TBluetoothProfile.Create(bptHID, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_SPP_ReturnsCorrectName;
begin
  Assert.AreEqual('Serial Port',
    TBluetoothProfile.Create(bptSPP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_PAN_ReturnsCorrectName;
begin
  Assert.AreEqual('Network',
    TBluetoothProfile.Create(bptPAN, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_OBEX_ReturnsCorrectName;
begin
  Assert.AreEqual('Object Exchange',
    TBluetoothProfile.Create(bptOBEX, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_PBAP_ReturnsCorrectName;
begin
  Assert.AreEqual('Phonebook',
    TBluetoothProfile.Create(bptPBAP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_MAP_ReturnsCorrectName;
begin
  Assert.AreEqual('Messages',
    TBluetoothProfile.Create(bptMAP, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.DisplayName_Unknown_ReturnsUnknown;
begin
  Assert.AreEqual('Unknown',
    TBluetoothProfile.Create(bptUnknown, TGUID.Empty, pcsUnknown).DisplayName);
end;

procedure TBluetoothProfileTests.ShortName_A2DPSink_ReturnsA2DP;
begin
  Assert.AreEqual('A2DP',
    TBluetoothProfile.Create(bptA2DPSink, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_A2DPSource_ReturnsA2DPSrc;
begin
  Assert.AreEqual('A2DP-Src',
    TBluetoothProfile.Create(bptA2DPSource, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_HFP_ReturnsHFP;
begin
  Assert.AreEqual('HFP',
    TBluetoothProfile.Create(bptHFP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_HSP_ReturnsHSP;
begin
  Assert.AreEqual('HSP',
    TBluetoothProfile.Create(bptHSP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_AVRCP_ReturnsAVRCP;
begin
  Assert.AreEqual('AVRCP',
    TBluetoothProfile.Create(bptAVRCP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_HID_ReturnsHID;
begin
  Assert.AreEqual('HID',
    TBluetoothProfile.Create(bptHID, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_SPP_ReturnsSPP;
begin
  Assert.AreEqual('SPP',
    TBluetoothProfile.Create(bptSPP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_PAN_ReturnsPAN;
begin
  Assert.AreEqual('PAN',
    TBluetoothProfile.Create(bptPAN, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_OBEX_ReturnsOBEX;
begin
  Assert.AreEqual('OBEX',
    TBluetoothProfile.Create(bptOBEX, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_PBAP_ReturnsPBAP;
begin
  Assert.AreEqual('PBAP',
    TBluetoothProfile.Create(bptPBAP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_MAP_ReturnsMAP;
begin
  Assert.AreEqual('MAP',
    TBluetoothProfile.Create(bptMAP, TGUID.Empty, pcsUnknown).ShortName);
end;

procedure TBluetoothProfileTests.ShortName_Unknown_ReturnsQuestionMark;
begin
  Assert.AreEqual('?',
    TBluetoothProfile.Create(bptUnknown, TGUID.Empty, pcsUnknown).ShortName);
end;

{ TDeviceProfileInfoTests }

function TDeviceProfileInfoTests.CreateProfile(
  AType: TBluetoothProfileType;
  AState: TProfileConnectionState): TBluetoothProfile;
begin
  Result := TBluetoothProfile.Create(AType, TGUID.Empty, AState);
end;

procedure TDeviceProfileInfoTests.Create_SetsAllFields;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
  TestDate: TDateTime;
begin
  TestDate := Now;
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsAvailable);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, TestDate);

  Assert.AreEqual(UInt64($AABBCCDDEEFF), Info.DeviceAddress);
  Assert.AreEqual(2, Info.Count);
  Assert.AreEqual(Double(TestDate), Double(Info.LastUpdated), 0.0001);
end;

procedure TDeviceProfileInfoTests.Empty_HasZeroProfiles;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($123456789ABC);
  Assert.AreEqual(0, Info.Count);
end;

procedure TDeviceProfileInfoTests.Empty_SetsAddress;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($AABBCCDDEEFF);
  Assert.AreEqual(UInt64($AABBCCDDEEFF), Info.DeviceAddress);
end;

procedure TDeviceProfileInfoTests.Empty_HasZeroLastUpdated;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($123456789ABC);
  Assert.AreEqual(Double(0), Double(Info.LastUpdated), 0.0001);
end;

procedure TDeviceProfileInfoTests.Count_EmptyProfiles_ReturnsZero;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($123456789ABC);
  Assert.AreEqual(0, Info.Count);
end;

procedure TDeviceProfileInfoTests.Count_ThreeProfiles_ReturnsThree;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 3);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsAvailable);
  Profiles[2] := CreateProfile(bptAVRCP, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(3, Info.Count);
end;

procedure TDeviceProfileInfoTests.ConnectedCount_NoneConnected_ReturnsZero;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsAvailable);
  Profiles[1] := CreateProfile(bptHFP, pcsUnknown);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(0, Info.ConnectedCount);
end;

procedure TDeviceProfileInfoTests.ConnectedCount_AllConnected_ReturnsAll;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 3);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsConnected);
  Profiles[2] := CreateProfile(bptAVRCP, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(3, Info.ConnectedCount);
end;

procedure TDeviceProfileInfoTests.ConnectedCount_SomeConnected_ReturnsCorrect;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 4);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsAvailable);
  Profiles[2] := CreateProfile(bptAVRCP, pcsConnected);
  Profiles[3] := CreateProfile(bptHSP, pcsUnknown);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(2, Info.ConnectedCount);
end;

procedure TDeviceProfileInfoTests.GetConnectedProfiles_NoneConnected_ReturnsEmpty;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
  Connected: TBluetoothProfileArray;
begin
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsAvailable);
  Profiles[1] := CreateProfile(bptHFP, pcsUnknown);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Connected := Info.GetConnectedProfiles;
  Assert.AreEqual(Integer(0), Integer(Length(Connected)));
end;

procedure TDeviceProfileInfoTests.GetConnectedProfiles_AllConnected_ReturnsAll;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
  Connected: TBluetoothProfileArray;
begin
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Connected := Info.GetConnectedProfiles;
  Assert.AreEqual(Integer(2), Integer(Length(Connected)));
end;

procedure TDeviceProfileInfoTests.GetConnectedProfiles_Mixed_ReturnsOnlyConnected;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
  Connected: TBluetoothProfileArray;
begin
  SetLength(Profiles, 3);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsAvailable);
  Profiles[1] := CreateProfile(bptHFP, pcsConnected);
  Profiles[2] := CreateProfile(bptAVRCP, pcsUnknown);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Connected := Info.GetConnectedProfiles;
  Assert.AreEqual(Integer(1), Integer(Length(Connected)));
  Assert.AreEqual(bptHFP, Connected[0].ProfileType);
end;

procedure TDeviceProfileInfoTests.HasProfile_Exists_ReturnsTrue;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsAvailable);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.IsTrue(Info.HasProfile(bptA2DPSink));
  Assert.IsTrue(Info.HasProfile(bptHFP));
end;

procedure TDeviceProfileInfoTests.HasProfile_NotExists_ReturnsFalse;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 1);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.IsFalse(Info.HasProfile(bptHFP));
  Assert.IsFalse(Info.HasProfile(bptSPP));
end;

procedure TDeviceProfileInfoTests.HasProfile_EmptyProfiles_ReturnsFalse;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($AABBCCDDEEFF);
  Assert.IsFalse(Info.HasProfile(bptA2DPSink));
end;

procedure TDeviceProfileInfoTests.GetProfileState_Exists_ReturnsState;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 2);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);
  Profiles[1] := CreateProfile(bptHFP, pcsAvailable);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(pcsConnected, Info.GetProfileState(bptA2DPSink));
  Assert.AreEqual(pcsAvailable, Info.GetProfileState(bptHFP));
end;

procedure TDeviceProfileInfoTests.GetProfileState_NotExists_ReturnsUnknown;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 1);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.AreEqual(pcsUnknown, Info.GetProfileState(bptSPP));
end;

procedure TDeviceProfileInfoTests.HasProfiles_WithProfiles_ReturnsTrue;
var
  Profiles: TBluetoothProfileArray;
  Info: TDeviceProfileInfo;
begin
  SetLength(Profiles, 1);
  Profiles[0] := CreateProfile(bptA2DPSink, pcsConnected);

  Info := TDeviceProfileInfo.Create($AABBCCDDEEFF, Profiles, Now);
  Assert.IsTrue(Info.HasProfiles);
end;

procedure TDeviceProfileInfoTests.HasProfiles_Empty_ReturnsFalse;
var
  Info: TDeviceProfileInfo;
begin
  Info := TDeviceProfileInfo.Empty($AABBCCDDEEFF);
  Assert.IsFalse(Info.HasProfiles);
end;

{ TPairingResultTests }

procedure TPairingResultTests.Success_StatusIsSuccess;
var
  R: TPairingResult;
begin
  R := TPairingResult.Success;
  Assert.AreEqual(prsSuccess, R.Status);
end;

procedure TPairingResultTests.Success_ErrorCodeIsZero;
var
  R: TPairingResult;
begin
  R := TPairingResult.Success;
  Assert.AreEqual(Cardinal(0), R.ErrorCode);
end;

procedure TPairingResultTests.Success_IsSuccessReturnsTrue;
var
  R: TPairingResult;
begin
  R := TPairingResult.Success;
  Assert.IsTrue(R.IsSuccess);
end;

procedure TPairingResultTests.Success_HasMessage;
var
  R: TPairingResult;
begin
  R := TPairingResult.Success;
  Assert.IsTrue(R.ErrorMessage <> '', 'Success should have a descriptive message');
end;

procedure TPairingResultTests.Failed_StatusIsFailed;
var
  R: TPairingResult;
begin
  R := TPairingResult.Failed(42, 'Connection refused');
  Assert.AreEqual(prsFailed, R.Status);
end;

procedure TPairingResultTests.Failed_PreservesErrorCode;
var
  R: TPairingResult;
begin
  R := TPairingResult.Failed(12345, 'Error');
  Assert.AreEqual(Cardinal(12345), R.ErrorCode);
end;

procedure TPairingResultTests.Failed_PreservesMessage;
var
  R: TPairingResult;
begin
  R := TPairingResult.Failed(0, 'Connection refused');
  Assert.AreEqual('Connection refused', R.ErrorMessage);
end;

procedure TPairingResultTests.Failed_IsSuccessReturnsFalse;
var
  R: TPairingResult;
begin
  R := TPairingResult.Failed(1, 'Error');
  Assert.IsFalse(R.IsSuccess);
end;

procedure TPairingResultTests.Cancelled_StatusIsCancelled;
var
  R: TPairingResult;
begin
  R := TPairingResult.Cancelled;
  Assert.AreEqual(prsCancelled, R.Status);
end;

procedure TPairingResultTests.Cancelled_ErrorCodeIsZero;
var
  R: TPairingResult;
begin
  R := TPairingResult.Cancelled;
  Assert.AreEqual(Cardinal(0), R.ErrorCode);
end;

procedure TPairingResultTests.Cancelled_IsSuccessReturnsFalse;
var
  R: TPairingResult;
begin
  R := TPairingResult.Cancelled;
  Assert.IsFalse(R.IsSuccess);
end;

procedure TPairingResultTests.AlreadyPaired_StatusIsAlreadyPaired;
var
  R: TPairingResult;
begin
  R := TPairingResult.AlreadyPaired;
  Assert.AreEqual(prsAlreadyPaired, R.Status);
end;

procedure TPairingResultTests.AlreadyPaired_IsSuccessReturnsTrue;
var
  R: TPairingResult;
begin
  // AlreadyPaired is considered a success (device is paired, which is the goal)
  R := TPairingResult.AlreadyPaired;
  Assert.IsTrue(R.IsSuccess);
end;

procedure TPairingResultTests.Timeout_StatusIsTimeout;
var
  R: TPairingResult;
begin
  R := TPairingResult.Timeout;
  Assert.AreEqual(prsTimeout, R.Status);
end;

procedure TPairingResultTests.Timeout_IsSuccessReturnsFalse;
var
  R: TPairingResult;
begin
  R := TPairingResult.Timeout;
  Assert.IsFalse(R.IsSuccess);
end;

procedure TPairingResultTests.NotSupported_StatusIsNotSupported;
var
  R: TPairingResult;
begin
  R := TPairingResult.NotSupported;
  Assert.AreEqual(prsNotSupported, R.Status);
end;

procedure TPairingResultTests.NotSupported_IsSuccessReturnsFalse;
var
  R: TPairingResult;
begin
  R := TPairingResult.NotSupported;
  Assert.IsFalse(R.IsSuccess);
end;

procedure TPairingResultTests.NotSupported_DefaultMessage_NoPrefixedReason;
var
  R: TPairingResult;
begin
  R := TPairingResult.NotSupported;
  Assert.AreEqual('Pairing not supported', R.ErrorMessage);
end;

procedure TPairingResultTests.NotSupported_WithReason_IncludesReason;
var
  R: TPairingResult;
begin
  R := TPairingResult.NotSupported('Windows 7 not supported');
  Assert.AreEqual('Pairing not supported: Windows 7 not supported', R.ErrorMessage);
end;

{ TGuidToProfileTypeTests }

function TGuidToProfileTypeTests.MakeBluetoothGuid(AShortUUID: Word): TGUID;
begin
  // Bluetooth base UUID: 0000xxxx-0000-1000-8000-00805F9B34FB
  Result := StringToGUID(Format('{0000%.4X-0000-1000-8000-00805F9B34FB}', [AShortUUID]));
end;

procedure TGuidToProfileTypeTests.AudioSource_110A_ReturnsSink;
begin
  Assert.AreEqual(bptA2DPSource, GuidToProfileType(MakeBluetoothGuid($110A)));
end;

procedure TGuidToProfileTypeTests.AudioSink_110B_ReturnsSink;
begin
  Assert.AreEqual(bptA2DPSink, GuidToProfileType(MakeBluetoothGuid($110B)));
end;

procedure TGuidToProfileTypeTests.AVRCPTarget_110C_ReturnsAVRCP;
begin
  Assert.AreEqual(bptAVRCP, GuidToProfileType(MakeBluetoothGuid($110C)));
end;

procedure TGuidToProfileTypeTests.AVRCPController_110E_ReturnsAVRCP;
begin
  Assert.AreEqual(bptAVRCP, GuidToProfileType(MakeBluetoothGuid($110E)));
end;

procedure TGuidToProfileTypeTests.Headset_1108_ReturnsHSP;
begin
  Assert.AreEqual(bptHSP, GuidToProfileType(MakeBluetoothGuid($1108)));
end;

procedure TGuidToProfileTypeTests.HeadsetAG_1112_ReturnsHSP;
begin
  Assert.AreEqual(bptHSP, GuidToProfileType(MakeBluetoothGuid($1112)));
end;

procedure TGuidToProfileTypeTests.HandsFree_111E_ReturnsHFP;
begin
  Assert.AreEqual(bptHFP, GuidToProfileType(MakeBluetoothGuid($111E)));
end;

procedure TGuidToProfileTypeTests.HandsFreeAG_111F_ReturnsHFP;
begin
  Assert.AreEqual(bptHFP, GuidToProfileType(MakeBluetoothGuid($111F)));
end;

procedure TGuidToProfileTypeTests.HID_1124_ReturnsHID;
begin
  Assert.AreEqual(bptHID, GuidToProfileType(MakeBluetoothGuid($1124)));
end;

procedure TGuidToProfileTypeTests.SerialPort_1101_ReturnsSPP;
begin
  Assert.AreEqual(bptSPP, GuidToProfileType(MakeBluetoothGuid($1101)));
end;

procedure TGuidToProfileTypeTests.PANU_1115_ReturnsPAN;
begin
  Assert.AreEqual(bptPAN, GuidToProfileType(MakeBluetoothGuid($1115)));
end;

procedure TGuidToProfileTypeTests.NAP_1116_ReturnsPAN;
begin
  Assert.AreEqual(bptPAN, GuidToProfileType(MakeBluetoothGuid($1116)));
end;

procedure TGuidToProfileTypeTests.GN_1117_ReturnsPAN;
begin
  Assert.AreEqual(bptPAN, GuidToProfileType(MakeBluetoothGuid($1117)));
end;

procedure TGuidToProfileTypeTests.OBEXPush_1105_ReturnsOBEX;
begin
  Assert.AreEqual(bptOBEX, GuidToProfileType(MakeBluetoothGuid($1105)));
end;

procedure TGuidToProfileTypeTests.OBEXFileTransfer_1106_ReturnsOBEX;
begin
  Assert.AreEqual(bptOBEX, GuidToProfileType(MakeBluetoothGuid($1106)));
end;

procedure TGuidToProfileTypeTests.PBAP_PSE_112F_ReturnsPBAP;
begin
  Assert.AreEqual(bptPBAP, GuidToProfileType(MakeBluetoothGuid($112F)));
end;

procedure TGuidToProfileTypeTests.PBAP_PCE_1130_ReturnsPBAP;
begin
  Assert.AreEqual(bptPBAP, GuidToProfileType(MakeBluetoothGuid($1130)));
end;

procedure TGuidToProfileTypeTests.MAP_Server_1132_ReturnsMAP;
begin
  Assert.AreEqual(bptMAP, GuidToProfileType(MakeBluetoothGuid($1132)));
end;

procedure TGuidToProfileTypeTests.MAP_Notification_1133_ReturnsMAP;
begin
  Assert.AreEqual(bptMAP, GuidToProfileType(MakeBluetoothGuid($1133)));
end;

procedure TGuidToProfileTypeTests.MAP_Profile_1134_ReturnsMAP;
begin
  Assert.AreEqual(bptMAP, GuidToProfileType(MakeBluetoothGuid($1134)));
end;

procedure TGuidToProfileTypeTests.UnknownGuid_ReturnsUnknown;
begin
  // Non-Bluetooth UUID should return unknown
  Assert.AreEqual(bptUnknown, GuidToProfileType(MakeBluetoothGuid($FFFF)));
end;

procedure TGuidToProfileTypeTests.EmptyGuid_ReturnsUnknown;
begin
  Assert.AreEqual(bptUnknown, GuidToProfileType(TGUID.Empty));
end;

{ TPsmToProfileTypeTests }

procedure TPsmToProfileTypeTests.SDP_0001_ReturnsUnknown;
begin
  Assert.AreEqual(bptUnknown, PsmToProfileType($0001));
end;

procedure TPsmToProfileTypeTests.RFCOMM_0003_ReturnsSPP;
begin
  Assert.AreEqual(bptSPP, PsmToProfileType($0003));
end;

procedure TPsmToProfileTypeTests.BNEP_000F_ReturnsPAN;
begin
  Assert.AreEqual(bptPAN, PsmToProfileType($000F));
end;

procedure TPsmToProfileTypeTests.HIDControl_0011_ReturnsHID;
begin
  Assert.AreEqual(bptHID, PsmToProfileType($0011));
end;

procedure TPsmToProfileTypeTests.HIDInterrupt_0013_ReturnsHID;
begin
  Assert.AreEqual(bptHID, PsmToProfileType($0013));
end;

procedure TPsmToProfileTypeTests.AVCTP_0017_ReturnsAVRCP;
begin
  Assert.AreEqual(bptAVRCP, PsmToProfileType($0017));
end;

procedure TPsmToProfileTypeTests.AVDTP_0019_ReturnsA2DPSink;
begin
  Assert.AreEqual(bptA2DPSink, PsmToProfileType($0019));
end;

procedure TPsmToProfileTypeTests.AVCTPBrowsing_001B_ReturnsAVRCP;
begin
  Assert.AreEqual(bptAVRCP, PsmToProfileType($001B));
end;

procedure TPsmToProfileTypeTests.ATT_001F_ReturnsUnknown;
begin
  Assert.AreEqual(bptUnknown, PsmToProfileType($001F));
end;

procedure TPsmToProfileTypeTests.UnknownPSM_ReturnsUnknown;
begin
  Assert.AreEqual(bptUnknown, PsmToProfileType($FFFF));
end;

{ TGetShortUUIDTests }

procedure TGetShortUUIDTests.GetShortUUID_BluetoothBaseUUID_ReturnsShortValue;
var
  Guid: TGUID;
begin
  // Bluetooth A2DP Sink: 0000110B-0000-1000-8000-00805F9B34FB
  Guid := StringToGUID('{0000110B-0000-1000-8000-00805F9B34FB}');
  Assert.AreEqual(Word($110B), GetShortUUID(Guid));
end;

procedure TGetShortUUIDTests.GetShortUUID_ZeroGuid_ReturnsZero;
begin
  Assert.AreEqual(Word(0), GetShortUUID(TGUID.Empty));
end;

{ TDetermineDeviceTypeEdgeCaseTests }

procedure TDetermineDeviceTypeEdgeCaseTests.DetermineDeviceType_AllBitsSet_FFFFFFFF;
var
  DeviceType: TBluetoothDeviceType;
begin
  // With all bits set ($FFFFFFFF):
  // Major class = ($FFFFFFFF shr 8) and $1F = $1F = 31 (not a recognized major class)
  // Should return btUnknown
  DeviceType := DetermineDeviceType($FFFFFFFF);
  Assert.AreEqual(btUnknown, DeviceType);
end;

procedure TDetermineDeviceTypeEdgeCaseTests.DetermineDeviceType_MinorClassBoundary;
var
  DeviceType: TBluetoothDeviceType;
begin
  // Test minor class at boundary (maximum 6-bit value = $3F = 63)
  // Major class = Audio/Video ($04), minor class at maximum value
  // Audio/Video with unknown minor should default to btAudioOutput
  DeviceType := DetermineDeviceType($0400 or $FC); // Major=$04, Minor=$3F (63)
  Assert.AreEqual(btAudioOutput, DeviceType, 'Unknown minor in Audio/Video should default to AudioOutput');

  // Test peripheral with no specific type bits set
  // Major class = Peripheral ($05), minor class = 0
  DeviceType := DetermineDeviceType($0500);
  Assert.AreEqual(btHID, DeviceType, 'Peripheral with no type bits should return HID');
end;

initialization
  TDUnitX.RegisterTestFixture(TDetermineDeviceTypeTests);
  TDUnitX.RegisterTestFixture(TDetermineDeviceTypeEdgeCaseTests);
  TDUnitX.RegisterTestFixture(TBluetoothDeviceInfoTests);
  TDUnitX.RegisterTestFixture(TBluetoothExceptionTests);
  TDUnitX.RegisterTestFixture(TBatteryStatusTests);
  TDUnitX.RegisterTestFixture(TAddressConversionTests);
  TDUnitX.RegisterTestFixture(TConnectionResultTests);
  TDUnitX.RegisterTestFixture(TBluetoothProfileTests);
  TDUnitX.RegisterTestFixture(TDeviceProfileInfoTests);
  TDUnitX.RegisterTestFixture(TPairingResultTests);
  TDUnitX.RegisterTestFixture(TGuidToProfileTypeTests);
  TDUnitX.RegisterTestFixture(TPsmToProfileTypeTests);
  TDUnitX.RegisterTestFixture(TGetShortUUIDTests);

end.
