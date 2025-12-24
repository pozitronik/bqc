{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Domain Types and Enumerations                   }
{                                                       }
{       Copyright (c) 2024                              }
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

end.
