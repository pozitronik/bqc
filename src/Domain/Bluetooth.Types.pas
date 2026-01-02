{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Domain Types and Enumerations                   }
{                                                       }
{*******************************************************}

unit Bluetooth.Types;

interface

uses
  System.SysUtils;

type
  /// <summary>
  /// Represents the type/category of a Bluetooth device.
  /// </summary>
  TBluetoothDeviceType = (
    /// <summary>Unknown or unrecognized device type.</summary>
    btUnknown,
    /// <summary>Audio output device (headphones, speakers).</summary>
    btAudioOutput,
    /// <summary>Audio input device (microphone).</summary>
    btAudioInput,
    /// <summary>Headset with microphone (audio in/out).</summary>
    btHeadset,
    /// <summary>Computer device.</summary>
    btComputer,
    /// <summary>Phone device.</summary>
    btPhone,
    /// <summary>Keyboard.</summary>
    btKeyboard,
    /// <summary>Mouse or pointing device.</summary>
    btMouse,
    /// <summary>Gamepad or joystick.</summary>
    btGamepad,
    /// <summary>Generic Human Interface Device.</summary>
    btHID
  );

const
  /// <summary>
  /// Display names for device type override selection.
  /// Index 0 is "Auto-detect" (value -1), subsequent indices map to TBluetoothDeviceType.
  /// </summary>
  DeviceTypeNames: array[0..10] of string = (
    'Auto-detect',   // Index 0 = override value -1
    'Unknown',       // Index 1 = btUnknown (0)
    'Audio Output',  // Index 2 = btAudioOutput (1)
    'Audio Input',   // Index 3 = btAudioInput (2)
    'Headset',       // Index 4 = btHeadset (3)
    'Computer',      // Index 5 = btComputer (4)
    'Phone',         // Index 6 = btPhone (5)
    'Keyboard',      // Index 7 = btKeyboard (6)
    'Mouse',         // Index 8 = btMouse (7)
    'Gamepad',       // Index 9 = btGamepad (8)
    'Input Device'   // Index 10 = btHID (9)
  );

type
  /// <summary>
  /// Represents the connection state of a Bluetooth device.
  /// </summary>
  TBluetoothConnectionState = (
    /// <summary>Device is disconnected.</summary>
    csDisconnected,
    /// <summary>Device is currently connected.</summary>
    csConnected,
    /// <summary>Connection is in progress.</summary>
    csConnecting,
    /// <summary>Disconnection is in progress.</summary>
    csDisconnecting,
    /// <summary>Connection state is unknown.</summary>
    csUnknown,
    /// <summary>An error occurred.</summary>
    csError
  );

  /// <summary>
  /// Status of a pairing operation result.
  /// </summary>
  TPairingResultStatus = (
    /// <summary>Pairing completed successfully.</summary>
    prsSuccess,
    /// <summary>User cancelled the pairing operation.</summary>
    prsCancelled,
    /// <summary>Pairing failed due to error.</summary>
    prsFailed,
    /// <summary>Device is already paired.</summary>
    prsAlreadyPaired,
    /// <summary>Pairing operation timed out.</summary>
    prsTimeout,
    /// <summary>Pairing not supported for this device/platform.</summary>
    prsNotSupported
  );

  /// <summary>
  /// Event types that can be debounced.
  /// Used by IEventDebouncer to filter duplicate events.
  /// </summary>
  TDeviceEventType = (
    /// <summary>Device connection event.</summary>
    detConnect,
    /// <summary>Device disconnection event.</summary>
    detDisconnect,
    /// <summary>Device attribute change event.</summary>
    detAttributeChange
  );

  // -------------------------------------------------------------------------
  // Bluetooth Profile Types
  // -------------------------------------------------------------------------

  /// <summary>
  /// Represents the type of a Bluetooth profile/service.
  /// </summary>
  TBluetoothProfileType = (
    /// <summary>Unknown or unrecognized profile.</summary>
    bptUnknown,
    /// <summary>Advanced Audio Distribution Profile - Sink (receive audio).</summary>
    bptA2DPSink,
    /// <summary>Advanced Audio Distribution Profile - Source (send audio).</summary>
    bptA2DPSource,
    /// <summary>Hands-Free Profile.</summary>
    bptHFP,
    /// <summary>Headset Profile.</summary>
    bptHSP,
    /// <summary>Audio/Video Remote Control Profile.</summary>
    bptAVRCP,
    /// <summary>Human Interface Device profile.</summary>
    bptHID,
    /// <summary>Serial Port Profile.</summary>
    bptSPP,
    /// <summary>Personal Area Network.</summary>
    bptPAN,
    /// <summary>Object Exchange (OBEX) profiles.</summary>
    bptOBEX,
    /// <summary>Phonebook Access Profile.</summary>
    bptPBAP,
    /// <summary>Message Access Profile.</summary>
    bptMAP
  );
  TBluetoothProfileTypes = set of TBluetoothProfileType;

  /// <summary>
  /// Connection state of a Bluetooth profile.
  /// </summary>
  TProfileConnectionState = (
    /// <summary>Profile state is unknown.</summary>
    pcsUnknown,
    /// <summary>Profile is available but not connected.</summary>
    pcsAvailable,
    /// <summary>Profile is currently connected.</summary>
    pcsConnected
  );

  /// <summary>
  /// Represents a single Bluetooth profile/service.
  /// Immutable value type for thread safety.
  /// </summary>
  TBluetoothProfile = record
  private
    FProfileType: TBluetoothProfileType;
    FServiceGuid: TGUID;
    FConnectionState: TProfileConnectionState;
  public
    /// <summary>
    /// Creates a new profile record.
    /// </summary>
    class function Create(
      AProfileType: TBluetoothProfileType;
      const AServiceGuid: TGUID;
      AConnectionState: TProfileConnectionState
    ): TBluetoothProfile; static;

    /// <summary>
    /// Returns a copy with updated connection state.
    /// </summary>
    function WithConnectionState(AState: TProfileConnectionState): TBluetoothProfile;

    /// <summary>
    /// Profile type.
    /// </summary>
    property ProfileType: TBluetoothProfileType read FProfileType;

    /// <summary>
    /// Service GUID for this profile.
    /// </summary>
    property ServiceGuid: TGUID read FServiceGuid;

    /// <summary>
    /// Current connection state of this profile.
    /// </summary>
    property ConnectionState: TProfileConnectionState read FConnectionState;

    /// <summary>
    /// Returns display name for this profile type.
    /// </summary>
    function DisplayName: string;

    /// <summary>
    /// Returns short name for this profile type.
    /// </summary>
    function ShortName: string;

    /// <summary>
    /// Whether this profile is currently connected.
    /// </summary>
    function IsConnected: Boolean;
  end;

  /// <summary>
  /// Array of Bluetooth profiles.
  /// </summary>
  TBluetoothProfileArray = TArray<TBluetoothProfile>;

  /// <summary>
  /// Contains profile information for a device.
  /// Immutable value type for thread safety.
  /// </summary>
  TDeviceProfileInfo = record
  private
    FDeviceAddress: UInt64;
    FProfiles: TBluetoothProfileArray;
    FLastUpdated: TDateTime;
  public
    /// <summary>
    /// Creates a new device profile info record.
    /// </summary>
    class function Create(
      ADeviceAddress: UInt64;
      const AProfiles: TBluetoothProfileArray;
      ALastUpdated: TDateTime
    ): TDeviceProfileInfo; static;

    /// <summary>
    /// Creates an empty profile info for a device.
    /// </summary>
    class function Empty(ADeviceAddress: UInt64): TDeviceProfileInfo; static;

    /// <summary>
    /// Device Bluetooth address.
    /// </summary>
    property DeviceAddress: UInt64 read FDeviceAddress;

    /// <summary>
    /// Array of profiles for this device.
    /// </summary>
    property Profiles: TBluetoothProfileArray read FProfiles;

    /// <summary>
    /// When profile information was last updated.
    /// </summary>
    property LastUpdated: TDateTime read FLastUpdated;

    /// <summary>
    /// Number of profiles.
    /// </summary>
    function Count: Integer;

    /// <summary>
    /// Number of connected profiles.
    /// </summary>
    function ConnectedCount: Integer;

    /// <summary>
    /// Returns array of connected profiles only.
    /// </summary>
    function GetConnectedProfiles: TBluetoothProfileArray;

    /// <summary>
    /// Checks if device has a specific profile type.
    /// </summary>
    function HasProfile(AType: TBluetoothProfileType): Boolean;

    /// <summary>
    /// Gets connection state of a specific profile type.
    /// </summary>
    function GetProfileState(AType: TBluetoothProfileType): TProfileConnectionState;

    /// <summary>
    /// Whether any profiles are available.
    /// </summary>
    function HasProfiles: Boolean;
  end;

  /// <summary>
  /// 6-byte Bluetooth MAC address.
  /// </summary>
  TBluetoothAddress = array[0..5] of Byte;

  /// <summary>
  /// Record representing a Bluetooth device with all relevant information.
  /// Immutable value type for thread safety.
  /// </summary>
  TBluetoothDeviceInfo = record
  private
    FAddress: TBluetoothAddress;
    FAddressInt: UInt64;
    FName: string;
    FDeviceType: TBluetoothDeviceType;
    FConnectionState: TBluetoothConnectionState;
    FIsPaired: Boolean;
    FIsAuthenticated: Boolean;
    FClassOfDevice: Cardinal;
    FLastSeen: TDateTime;
    FLastUsed: TDateTime;
  public
    /// <summary>
    /// Creates a new device info record.
    /// </summary>
    class function Create(
      const AAddress: TBluetoothAddress;
      AAddressInt: UInt64;
      const AName: string;
      ADeviceType: TBluetoothDeviceType;
      AConnectionState: TBluetoothConnectionState;
      AIsPaired: Boolean;
      AIsAuthenticated: Boolean;
      AClassOfDevice: Cardinal;
      ALastSeen: TDateTime;
      ALastUsed: TDateTime
    ): TBluetoothDeviceInfo; static;

    /// <summary>
    /// Returns a copy with updated connection state.
    /// </summary>
    function WithConnectionState(AState: TBluetoothConnectionState): TBluetoothDeviceInfo;

    /// <summary>
    /// Returns a copy with updated name.
    /// </summary>
    function WithName(const AName: string): TBluetoothDeviceInfo;

    /// <summary>
    /// Bluetooth MAC address as byte array.
    /// </summary>
    property Address: TBluetoothAddress read FAddress;

    /// <summary>
    /// Bluetooth MAC address as 64-bit integer.
    /// </summary>
    property AddressInt: UInt64 read FAddressInt;

    /// <summary>
    /// Formatted address string (XX:XX:XX:XX:XX:XX).
    /// </summary>
    function AddressString: string;

    /// <summary>
    /// Display name of the device.
    /// </summary>
    property Name: string read FName;

    /// <summary>
    /// Type/category of the device.
    /// </summary>
    property DeviceType: TBluetoothDeviceType read FDeviceType;

    /// <summary>
    /// Current connection state.
    /// </summary>
    property ConnectionState: TBluetoothConnectionState read FConnectionState;

    /// <summary>
    /// Whether the device is currently connected.
    /// </summary>
    function IsConnected: Boolean;

    /// <summary>
    /// Whether the device is paired (remembered).
    /// </summary>
    property IsPaired: Boolean read FIsPaired;

    /// <summary>
    /// Whether the device has been authenticated.
    /// </summary>
    property IsAuthenticated: Boolean read FIsAuthenticated;

    /// <summary>
    /// Raw Class of Device value.
    /// </summary>
    property ClassOfDevice: Cardinal read FClassOfDevice;

    /// <summary>
    /// Last time the device was seen.
    /// </summary>
    property LastSeen: TDateTime read FLastSeen;

    /// <summary>
    /// Last time the device was used.
    /// </summary>
    property LastUsed: TDateTime read FLastUsed;

    /// <summary>
    /// Whether this is an audio device (headphones, speakers, headset).
    /// </summary>
    function IsAudioDevice: Boolean;

    /// <summary>
    /// Whether this is an input device (keyboard, mouse, gamepad).
    /// </summary>
    function IsInputDevice: Boolean;
  end;

  /// <summary>
  /// Dynamic array of Bluetooth device info records.
  /// </summary>
  TBluetoothDeviceInfoArray = TArray<TBluetoothDeviceInfo>;

  /// <summary>
  /// Represents the battery status of a Bluetooth device.
  /// Immutable value type for thread safety.
  /// </summary>
  TBatteryStatus = record
  private
    FIsSupported: Boolean;
    FLevel: Integer;  // 0-100, or -1 if unknown
  public
    /// <summary>
    /// Creates a battery status indicating the feature is not supported.
    /// </summary>
    class function NotSupported: TBatteryStatus; static;

    /// <summary>
    /// Creates a battery status indicating the level is unknown.
    /// Used when the device supports battery reporting but level couldn't be read.
    /// </summary>
    class function Unknown: TBatteryStatus; static;

    /// <summary>
    /// Creates a battery status with a specific level.
    /// </summary>
    /// <param name="ALevel">Battery level 0-100.</param>
    class function Create(ALevel: Integer): TBatteryStatus; static;

    /// <summary>
    /// Creates a battery status indicating refresh is pending.
    /// Used when device just connected and battery query is in progress.
    /// </summary>
    class function Pending: TBatteryStatus; static;

    /// <summary>
    /// Whether the device supports battery level reporting.
    /// </summary>
    property IsSupported: Boolean read FIsSupported;

    /// <summary>
    /// Battery level percentage (0-100), -1 if unknown, -2 if pending refresh.
    /// Only valid when IsSupported is True.
    /// </summary>
    property Level: Integer read FLevel;

    /// <summary>
    /// Returns True if battery level is known and valid.
    /// </summary>
    function HasLevel: Boolean;

    /// <summary>
    /// Returns True if battery refresh is pending.
    /// </summary>
    function IsPending: Boolean;
  end;

  /// <summary>
  /// Result of a Bluetooth pairing operation.
  /// Encapsulates success/failure status, error codes, and messages.
  /// </summary>
  TPairingResult = record
  private
    FStatus: TPairingResultStatus;
    FErrorCode: Cardinal;
    FErrorMessage: string;
  public
    /// <summary>
    /// Creates a successful pairing result.
    /// </summary>
    class function Success: TPairingResult; static;

    /// <summary>
    /// Creates a failed pairing result with error code and message.
    /// </summary>
    class function Failed(AErrorCode: Cardinal; const AMessage: string): TPairingResult; static;

    /// <summary>
    /// Creates a cancelled pairing result.
    /// </summary>
    class function Cancelled: TPairingResult; static;

    /// <summary>
    /// Creates an already-paired result.
    /// </summary>
    class function AlreadyPaired: TPairingResult; static;

    /// <summary>
    /// Creates a timeout result.
    /// </summary>
    class function Timeout: TPairingResult; static;

    /// <summary>
    /// Creates a not-supported result.
    /// </summary>
    class function NotSupported(const AReason: string = ''): TPairingResult; static;

    /// <summary>
    /// Returns True if pairing was successful.
    /// </summary>
    function IsSuccess: Boolean;

    /// <summary>
    /// The status of the pairing operation.
    /// </summary>
    property Status: TPairingResultStatus read FStatus;

    /// <summary>
    /// Windows error code if pairing failed, 0 otherwise.
    /// </summary>
    property ErrorCode: Cardinal read FErrorCode;

    /// <summary>
    /// Human-readable error message or status description.
    /// </summary>
    property ErrorMessage: string read FErrorMessage;
  end;

  /// <summary>
  /// Exception raised for Bluetooth-related errors.
  /// </summary>
  EBluetoothException = class(Exception)
  private
    FErrorCode: Cardinal;
  public
    constructor Create(const AMessage: string; AErrorCode: Cardinal = 0); reintroduce;
    property ErrorCode: Cardinal read FErrorCode;
  end;

  /// <summary>
  /// Exception raised when no Bluetooth adapter is found.
  /// </summary>
  EBluetoothAdapterNotFound = class(EBluetoothException);

  /// <summary>
  /// Exception raised when a device operation fails.
  /// </summary>
  EBluetoothDeviceError = class(EBluetoothException);

/// <summary>
/// Determines the device type from Class of Device value.
/// </summary>
function DetermineDeviceType(AClassOfDevice: Cardinal): TBluetoothDeviceType;

/// <summary>
/// Converts a 64-bit integer address to TBluetoothAddress byte array (little-endian).
/// </summary>
function UInt64ToBluetoothAddress(AValue: UInt64): TBluetoothAddress;

/// <summary>
/// Converts a TBluetoothAddress byte array to 64-bit integer (little-endian).
/// </summary>
function BluetoothAddressToUInt64(const AAddress: TBluetoothAddress): UInt64;

/// <summary>
/// Formats a 64-bit Bluetooth address as a MAC address string.
/// Returns format: "XX:XX:XX:XX:XX:XX" (big-endian, uppercase).
/// </summary>
/// <param name="AAddressInt">The 64-bit address value.</param>
/// <returns>Formatted MAC address string.</returns>
function FormatAddressAsMAC(AAddressInt: UInt64): string;

/// <summary>
/// Maps a Bluetooth service GUID to a profile type.
/// </summary>
/// <param name="AGuid">The service GUID.</param>
/// <returns>The profile type, or bptUnknown if not recognized.</returns>
function GuidToProfileType(const AGuid: TGUID): TBluetoothProfileType;

/// <summary>
/// Maps an L2CAP PSM value to a profile type.
/// </summary>
/// <param name="APsm">The PSM value from L2CAP event.</param>
/// <returns>The profile type, or bptUnknown if not recognized.</returns>
function PsmToProfileType(APsm: Word): TBluetoothProfileType;

/// <summary>
/// Returns the short UUID (16-bit) from a full Bluetooth GUID.
/// Bluetooth base UUID: 0000xxxx-0000-1000-8000-00805F9B34FB
/// </summary>
/// <param name="AGuid">The full GUID.</param>
/// <returns>The 16-bit short UUID.</returns>
function GetShortUUID(const AGuid: TGUID): Word;

implementation

{ TBluetoothDeviceInfo }

class function TBluetoothDeviceInfo.Create(
  const AAddress: TBluetoothAddress;
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState;
  AIsPaired: Boolean;
  AIsAuthenticated: Boolean;
  AClassOfDevice: Cardinal;
  ALastSeen: TDateTime;
  ALastUsed: TDateTime
): TBluetoothDeviceInfo;
begin
  Result.FAddress := AAddress;
  Result.FAddressInt := AAddressInt;
  Result.FName := AName;
  Result.FDeviceType := ADeviceType;
  Result.FConnectionState := AConnectionState;
  Result.FIsPaired := AIsPaired;
  Result.FIsAuthenticated := AIsAuthenticated;
  Result.FClassOfDevice := AClassOfDevice;
  Result.FLastSeen := ALastSeen;
  Result.FLastUsed := ALastUsed;
end;

function TBluetoothDeviceInfo.WithConnectionState(
  AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
begin
  Result := Self;
  Result.FConnectionState := AState;
end;

function TBluetoothDeviceInfo.WithName(const AName: string): TBluetoothDeviceInfo;
begin
  Result := Self;
  Result.FName := AName;
end;

function TBluetoothDeviceInfo.AddressString: string;
begin
  Result := FormatAddressAsMAC(FAddressInt);
end;

function TBluetoothDeviceInfo.IsConnected: Boolean;
begin
  Result := FConnectionState = csConnected;
end;

function TBluetoothDeviceInfo.IsAudioDevice: Boolean;
begin
  Result := FDeviceType in [btAudioOutput, btAudioInput, btHeadset];
end;

function TBluetoothDeviceInfo.IsInputDevice: Boolean;
begin
  Result := FDeviceType in [btKeyboard, btMouse, btGamepad, btHID];
end;

{ TBluetoothProfile }

class function TBluetoothProfile.Create(
  AProfileType: TBluetoothProfileType;
  const AServiceGuid: TGUID;
  AConnectionState: TProfileConnectionState
): TBluetoothProfile;
begin
  Result.FProfileType := AProfileType;
  Result.FServiceGuid := AServiceGuid;
  Result.FConnectionState := AConnectionState;
end;

function TBluetoothProfile.WithConnectionState(
  AState: TProfileConnectionState): TBluetoothProfile;
begin
  Result := Self;
  Result.FConnectionState := AState;
end;

function TBluetoothProfile.DisplayName: string;
begin
  case FProfileType of
    bptA2DPSink:   Result := 'A2DP (Audio Sink)';
    bptA2DPSource: Result := 'A2DP (Audio Source)';
    bptHFP:        Result := 'Hands-Free';
    bptHSP:        Result := 'Headset';
    bptAVRCP:      Result := 'Remote Control';
    bptHID:        Result := 'HID';
    bptSPP:        Result := 'Serial Port';
    bptPAN:        Result := 'Network';
    bptOBEX:       Result := 'Object Exchange';
    bptPBAP:       Result := 'Phonebook';
    bptMAP:        Result := 'Messages';
  else
    Result := 'Unknown';
  end;
end;

function TBluetoothProfile.ShortName: string;
begin
  case FProfileType of
    bptA2DPSink:   Result := 'A2DP';
    bptA2DPSource: Result := 'A2DP-Src';
    bptHFP:        Result := 'HFP';
    bptHSP:        Result := 'HSP';
    bptAVRCP:      Result := 'AVRCP';
    bptHID:        Result := 'HID';
    bptSPP:        Result := 'SPP';
    bptPAN:        Result := 'PAN';
    bptOBEX:       Result := 'OBEX';
    bptPBAP:       Result := 'PBAP';
    bptMAP:        Result := 'MAP';
  else
    Result := '?';
  end;
end;

function TBluetoothProfile.IsConnected: Boolean;
begin
  Result := FConnectionState = pcsConnected;
end;

{ TDeviceProfileInfo }

class function TDeviceProfileInfo.Create(
  ADeviceAddress: UInt64;
  const AProfiles: TBluetoothProfileArray;
  ALastUpdated: TDateTime
): TDeviceProfileInfo;
begin
  Result.FDeviceAddress := ADeviceAddress;
  Result.FProfiles := AProfiles;
  Result.FLastUpdated := ALastUpdated;
end;

class function TDeviceProfileInfo.Empty(ADeviceAddress: UInt64): TDeviceProfileInfo;
begin
  Result.FDeviceAddress := ADeviceAddress;
  SetLength(Result.FProfiles, 0);
  Result.FLastUpdated := 0;
end;

function TDeviceProfileInfo.Count: Integer;
begin
  Result := Length(FProfiles);
end;

function TDeviceProfileInfo.ConnectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FProfiles) do
    if FProfiles[I].IsConnected then
      Inc(Result);
end;

function TDeviceProfileInfo.GetConnectedProfiles: TBluetoothProfileArray;
var
  I, Idx: Integer;
begin
  SetLength(Result, ConnectedCount);
  Idx := 0;
  for I := 0 to High(FProfiles) do
    if FProfiles[I].IsConnected then
    begin
      Result[Idx] := FProfiles[I];
      Inc(Idx);
    end;
end;

function TDeviceProfileInfo.HasProfile(AType: TBluetoothProfileType): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FProfiles) do
    if FProfiles[I].ProfileType = AType then
      Exit(True);
  Result := False;
end;

function TDeviceProfileInfo.GetProfileState(
  AType: TBluetoothProfileType): TProfileConnectionState;
var
  I: Integer;
begin
  for I := 0 to High(FProfiles) do
    if FProfiles[I].ProfileType = AType then
      Exit(FProfiles[I].ConnectionState);
  Result := pcsUnknown;
end;

function TDeviceProfileInfo.HasProfiles: Boolean;
begin
  Result := Length(FProfiles) > 0;
end;

{ TBatteryStatus }

class function TBatteryStatus.NotSupported: TBatteryStatus;
begin
  Result.FIsSupported := False;
  Result.FLevel := -1;
end;

class function TBatteryStatus.Unknown: TBatteryStatus;
begin
  Result.FIsSupported := True;
  Result.FLevel := -1;
end;

class function TBatteryStatus.Create(ALevel: Integer): TBatteryStatus;
begin
  Result.FIsSupported := True;
  // Clamp to valid range
  if ALevel < 0 then
    Result.FLevel := 0
  else if ALevel > 100 then
    Result.FLevel := 100
  else
    Result.FLevel := ALevel;
end;

class function TBatteryStatus.Pending: TBatteryStatus;
begin
  Result.FIsSupported := True;
  Result.FLevel := -2;  // Sentinel value for pending state
end;

function TBatteryStatus.HasLevel: Boolean;
begin
  Result := FIsSupported and (FLevel >= 0) and (FLevel <= 100);
end;

function TBatteryStatus.IsPending: Boolean;
begin
  Result := FIsSupported and (FLevel = -2);
end;

{ TPairingResult }

class function TPairingResult.Success: TPairingResult;
begin
  Result.FStatus := prsSuccess;
  Result.FErrorCode := 0;
  Result.FErrorMessage := 'Pairing completed successfully';
end;

class function TPairingResult.Failed(AErrorCode: Cardinal; const AMessage: string): TPairingResult;
begin
  Result.FStatus := prsFailed;
  Result.FErrorCode := AErrorCode;
  Result.FErrorMessage := AMessage;
end;

class function TPairingResult.Cancelled: TPairingResult;
begin
  Result.FStatus := prsCancelled;
  Result.FErrorCode := 0;
  Result.FErrorMessage := 'Pairing cancelled by user';
end;

class function TPairingResult.AlreadyPaired: TPairingResult;
begin
  Result.FStatus := prsAlreadyPaired;
  Result.FErrorCode := 0;
  Result.FErrorMessage := 'Device is already paired';
end;

class function TPairingResult.Timeout: TPairingResult;
begin
  Result.FStatus := prsTimeout;
  Result.FErrorCode := 0;
  Result.FErrorMessage := 'Pairing operation timed out';
end;

class function TPairingResult.NotSupported(const AReason: string): TPairingResult;
begin
  Result.FStatus := prsNotSupported;
  Result.FErrorCode := 0;
  if AReason <> '' then
    Result.FErrorMessage := 'Pairing not supported: ' + AReason
  else
    Result.FErrorMessage := 'Pairing not supported';
end;

function TPairingResult.IsSuccess: Boolean;
begin
  Result := (FStatus = prsSuccess) or (FStatus = prsAlreadyPaired);
end;

{ EBluetoothException }

constructor EBluetoothException.Create(const AMessage: string; AErrorCode: Cardinal);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
end;

{ Class of Device Constants }

const
  COD_MAJOR_COMPUTER    = $01;
  COD_MAJOR_PHONE       = $02;
  COD_MAJOR_AUDIO_VIDEO = $04;
  COD_MAJOR_PERIPHERAL  = $05;

  COD_MINOR_AV_HEADSET        = $01;
  COD_MINOR_AV_HANDSFREE      = $02;
  COD_MINOR_AV_MICROPHONE     = $04;
  COD_MINOR_AV_LOUDSPEAKER    = $05;
  COD_MINOR_AV_HEADPHONES     = $06;
  COD_MINOR_AV_PORTABLE_AUDIO = $07;
  COD_MINOR_AV_HIFI_AUDIO     = $08;

  COD_MINOR_PERIPH_KEYBOARD   = $10;
  COD_MINOR_PERIPH_POINTING   = $20;
  COD_MINOR_PERIPH_COMBO      = $30;
  COD_MINOR_PERIPH_GAMEPAD    = $08;

{ Helper Functions }

function DetermineDeviceType(AClassOfDevice: Cardinal): TBluetoothDeviceType;
var
  MajorClass, MinorClass: Byte;
begin
  // Extract major class (bits 8-12) and minor class (bits 2-7)
  MajorClass := (AClassOfDevice shr 8) and $1F;
  MinorClass := (AClassOfDevice shr 2) and $3F;

  case MajorClass of
    COD_MAJOR_COMPUTER:
      Result := btComputer;

    COD_MAJOR_PHONE:
      Result := btPhone;

    COD_MAJOR_AUDIO_VIDEO:
      case MinorClass of
        COD_MINOR_AV_HEADSET,
        COD_MINOR_AV_HANDSFREE:
          Result := btHeadset;
        COD_MINOR_AV_MICROPHONE:
          Result := btAudioInput;
        COD_MINOR_AV_LOUDSPEAKER,
        COD_MINOR_AV_HEADPHONES,
        COD_MINOR_AV_PORTABLE_AUDIO,
        COD_MINOR_AV_HIFI_AUDIO:
          Result := btAudioOutput;
      else
        Result := btAudioOutput; // Default for audio devices
      end;

    COD_MAJOR_PERIPHERAL:
      begin
        // Check keyboard/pointing bits (bits 6-7 of minor class)
        if (MinorClass and $30) = COD_MINOR_PERIPH_KEYBOARD then
          Result := btKeyboard
        else if (MinorClass and $30) = COD_MINOR_PERIPH_POINTING then
          Result := btMouse
        else if (MinorClass and $30) = COD_MINOR_PERIPH_COMBO then
          Result := btKeyboard // Keyboard+Mouse combo
        else if (MinorClass and $0F) = COD_MINOR_PERIPH_GAMEPAD then
          Result := btGamepad
        else
          Result := btHID;
      end;
  else
    Result := btUnknown;
  end;
end;

function UInt64ToBluetoothAddress(AValue: UInt64): TBluetoothAddress;
begin
  Result[0] := Byte(AValue);
  Result[1] := Byte(AValue shr 8);
  Result[2] := Byte(AValue shr 16);
  Result[3] := Byte(AValue shr 24);
  Result[4] := Byte(AValue shr 32);
  Result[5] := Byte(AValue shr 40);
end;

function BluetoothAddressToUInt64(const AAddress: TBluetoothAddress): UInt64;
begin
  Result := UInt64(AAddress[0]) or
            (UInt64(AAddress[1]) shl 8) or
            (UInt64(AAddress[2]) shl 16) or
            (UInt64(AAddress[3]) shl 24) or
            (UInt64(AAddress[4]) shl 32) or
            (UInt64(AAddress[5]) shl 40);
end;

function FormatAddressAsMAC(AAddressInt: UInt64): string;
begin
  // Format as big-endian MAC address (most significant byte first)
  Result := Format('%.2X:%.2X:%.2X:%.2X:%.2X:%.2X', [
    (AAddressInt shr 40) and $FF,
    (AAddressInt shr 32) and $FF,
    (AAddressInt shr 24) and $FF,
    (AAddressInt shr 16) and $FF,
    (AAddressInt shr 8) and $FF,
    AAddressInt and $FF
  ]);
end;

function GetShortUUID(const AGuid: TGUID): Word;
begin
  // Bluetooth base UUID: 0000xxxx-0000-1000-8000-00805F9B34FB
  // The short UUID is in the D1 field (first 32 bits), lower 16 bits
  Result := Word(AGuid.D1);
end;

function GuidToProfileType(const AGuid: TGUID): TBluetoothProfileType;
var
  ShortUUID: Word;
begin
  ShortUUID := GetShortUUID(AGuid);

  case ShortUUID of
    // Audio profiles
    $110A: Result := bptA2DPSource;  // AudioSource
    $110B: Result := bptA2DPSink;    // AudioSink
    $110C: Result := bptAVRCP;       // A/V Remote Control Target
    $110E: Result := bptAVRCP;       // A/V Remote Control Controller

    // Voice profiles
    $1108: Result := bptHSP;         // Headset
    $1112: Result := bptHSP;         // Headset Audio Gateway
    $111E: Result := bptHFP;         // Hands-Free
    $111F: Result := bptHFP;         // Hands-Free Audio Gateway

    // HID
    $1124: Result := bptHID;         // Human Interface Device

    // Serial
    $1101: Result := bptSPP;         // Serial Port

    // Network
    $1115: Result := bptPAN;         // PANU
    $1116: Result := bptPAN;         // NAP
    $1117: Result := bptPAN;         // GN

    // Object transfer
    $1105: Result := bptOBEX;        // OBEX Object Push
    $1106: Result := bptOBEX;        // OBEX File Transfer

    // Phone profiles
    $112F: Result := bptPBAP;        // Phonebook Access - PSE
    $1130: Result := bptPBAP;        // Phonebook Access - PCE
    $1132: Result := bptMAP;         // Message Access Server
    $1133: Result := bptMAP;         // Message Notification Server
    $1134: Result := bptMAP;         // Message Access Profile
  else
    Result := bptUnknown;
  end;
end;

function PsmToProfileType(APsm: Word): TBluetoothProfileType;
begin
  // L2CAP PSM values
  // Note: Some profiles share PSMs or use dynamic PSMs
  case APsm of
    $0001: Result := bptUnknown;     // SDP - not a user-facing profile
    $0003: Result := bptSPP;         // RFCOMM (also used by HFP signaling)
    $000F: Result := bptPAN;         // BNEP
    $0011: Result := bptHID;         // HID Control
    $0013: Result := bptHID;         // HID Interrupt
    $0017: Result := bptAVRCP;       // AVCTP (Control)
    $0019: Result := bptA2DPSink;    // AVDTP (A2DP streaming)
    $001B: Result := bptAVRCP;       // AVCTP Browsing
    $001F: Result := bptUnknown;     // ATT (BLE) - handled separately
  else
    Result := bptUnknown;
  end;
end;

end.
