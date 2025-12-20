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

    /// <summary>
    /// Gets a human-readable connection status string.
    /// </summary>
    function ConnectionStateText: string;

    /// <summary>
    /// Gets a human-readable device type string.
    /// </summary>
    function DeviceTypeText: string;
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
  Result := Format('%.2X:%.2X:%.2X:%.2X:%.2X:%.2X', [
    FAddress[5], FAddress[4], FAddress[3],
    FAddress[2], FAddress[1], FAddress[0]
  ]);
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

function TBluetoothDeviceInfo.ConnectionStateText: string;
begin
  case FConnectionState of
    csDisconnected:  Result := 'Disconnected';
    csConnected:     Result := 'Connected';
    csConnecting:    Result := 'Connecting...';
    csDisconnecting: Result := 'Disconnecting...';
    csError:         Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TBluetoothDeviceInfo.DeviceTypeText: string;
begin
  case FDeviceType of
    btAudioOutput: Result := 'Audio Output';
    btAudioInput:  Result := 'Audio Input';
    btHeadset:     Result := 'Headset';
    btComputer:    Result := 'Computer';
    btPhone:       Result := 'Phone';
    btKeyboard:    Result := 'Keyboard';
    btMouse:       Result := 'Mouse';
    btGamepad:     Result := 'Gamepad';
    btHID:         Result := 'Input Device';
  else
    Result := 'Unknown';
  end;
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

end.
