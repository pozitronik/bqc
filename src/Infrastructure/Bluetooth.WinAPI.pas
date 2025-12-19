{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Windows Bluetooth API Declarations              }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.WinAPI;

interface

uses
  Winapi.Windows;

const
  /// <summary>
  /// Bluetooth API library name.
  /// </summary>
  BLUETOOTH_API_LIB = 'bthprops.cpl';

  /// <summary>
  /// Maximum Bluetooth device name length.
  /// </summary>
  BLUETOOTH_MAX_NAME_SIZE = 248;

  /// <summary>
  /// Service state flag: Enable the service.
  /// </summary>
  BLUETOOTH_SERVICE_ENABLE  = $0001;

  /// <summary>
  /// Service state flag: Disable the service.
  /// </summary>
  BLUETOOTH_SERVICE_DISABLE = $0000;

  // -------------------------------------------------------------------------
  // Bluetooth Service GUIDs (Common Profiles)
  // -------------------------------------------------------------------------

  /// <summary>
  /// Advanced Audio Distribution Profile - Audio Sink (A2DP).
  /// Used for high-quality audio streaming to headphones/speakers.
  /// </summary>
  AudioSinkServiceClass_UUID: TGUID = '{0000110B-0000-1000-8000-00805F9B34FB}';

  /// <summary>
  /// Hands-Free Profile (HFP).
  /// Used for headset audio and call control.
  /// </summary>
  HandsfreeServiceClass_UUID: TGUID = '{0000111E-0000-1000-8000-00805F9B34FB}';

  /// <summary>
  /// Headset Profile (HSP).
  /// Basic headset audio profile.
  /// </summary>
  HeadsetServiceClass_UUID: TGUID = '{00001108-0000-1000-8000-00805F9B34FB}';

  /// <summary>
  /// Audio/Video Remote Control Profile (AVRCP).
  /// Used for media control commands.
  /// </summary>
  AVRemoteControlServiceClass_UUID: TGUID = '{0000110E-0000-1000-8000-00805F9B34FB}';

  /// <summary>
  /// Human Interface Device (HID) Profile.
  /// Used for keyboards, mice, gamepads.
  /// </summary>
  HumanInterfaceDeviceServiceClass_UUID: TGUID = '{00001124-0000-1000-8000-00805F9B34FB}';

  // -------------------------------------------------------------------------
  // Class of Device (CoD) Major Device Classes
  // -------------------------------------------------------------------------

  COD_MAJOR_COMPUTER    = $01;
  COD_MAJOR_PHONE       = $02;
  COD_MAJOR_AUDIO_VIDEO = $04;
  COD_MAJOR_PERIPHERAL  = $05;
  COD_MAJOR_IMAGING     = $06;

  // -------------------------------------------------------------------------
  // CoD Minor Device Classes for Audio/Video
  // -------------------------------------------------------------------------

  COD_MINOR_AV_HEADSET       = $01;
  COD_MINOR_AV_HANDSFREE     = $02;
  COD_MINOR_AV_MICROPHONE    = $04;
  COD_MINOR_AV_LOUDSPEAKER   = $05;
  COD_MINOR_AV_HEADPHONES    = $06;
  COD_MINOR_AV_PORTABLE_AUDIO = $07;
  COD_MINOR_AV_HIFI_AUDIO    = $08;

  // -------------------------------------------------------------------------
  // CoD Minor Device Classes for Peripheral
  // -------------------------------------------------------------------------

  COD_MINOR_PERIPH_KEYBOARD  = $10;
  COD_MINOR_PERIPH_POINTING  = $20;
  COD_MINOR_PERIPH_COMBO     = $30;
  COD_MINOR_PERIPH_GAMEPAD   = $08;

type
  /// <summary>
  /// Handle to a Bluetooth radio find operation.
  /// </summary>
  HBLUETOOTH_RADIO_FIND = THandle;

  /// <summary>
  /// Handle to a Bluetooth device find operation.
  /// </summary>
  HBLUETOOTH_DEVICE_FIND = THandle;

  /// <summary>
  /// Bluetooth address (6 bytes / 48 bits).
  /// </summary>
  BLUETOOTH_ADDRESS = record
    case Integer of
      0: (rgBytes: array[0..5] of Byte);
      1: (ullLong: UInt64);
  end;
  PBLUETOOTH_ADDRESS = ^BLUETOOTH_ADDRESS;

  /// <summary>
  /// Parameters for finding Bluetooth radios.
  /// </summary>
  BLUETOOTH_FIND_RADIO_PARAMS = record
    /// <summary>
    /// Size of this structure in bytes.
    /// Must be set before calling BluetoothFindFirstRadio.
    /// </summary>
    dwSize: DWORD;
  end;
  PBLUETOOTH_FIND_RADIO_PARAMS = ^BLUETOOTH_FIND_RADIO_PARAMS;

  /// <summary>
  /// Information about a Bluetooth radio adapter.
  /// </summary>
  BLUETOOTH_RADIO_INFO = record
    /// <summary>
    /// Size of this structure in bytes.
    /// </summary>
    dwSize: DWORD;

    /// <summary>
    /// Bluetooth address of the radio.
    /// </summary>
    address: BLUETOOTH_ADDRESS;

    /// <summary>
    /// Name of the radio.
    /// </summary>
    szName: array[0..BLUETOOTH_MAX_NAME_SIZE - 1] of WideChar;

    /// <summary>
    /// Class of Device value for the radio.
    /// </summary>
    ulClassOfDevice: ULONG;

    /// <summary>
    /// LMP subversion.
    /// </summary>
    lmpSubversion: Word;

    /// <summary>
    /// Manufacturer identifier.
    /// </summary>
    manufacturer: Word;
  end;
  PBLUETOOTH_RADIO_INFO = ^BLUETOOTH_RADIO_INFO;

  /// <summary>
  /// Information about a Bluetooth device.
  /// </summary>
  BLUETOOTH_DEVICE_INFO = record
    /// <summary>
    /// Size of this structure in bytes.
    /// Must be set before using.
    /// </summary>
    dwSize: DWORD;

    /// <summary>
    /// Bluetooth address of the device.
    /// </summary>
    Address: BLUETOOTH_ADDRESS;

    /// <summary>
    /// Class of Device value.
    /// Contains major and minor device class information.
    /// </summary>
    ulClassOfDevice: ULONG;

    /// <summary>
    /// Whether the device is currently connected.
    /// </summary>
    fConnected: BOOL;

    /// <summary>
    /// Whether the device is remembered (paired).
    /// </summary>
    fRemembered: BOOL;

    /// <summary>
    /// Whether the device has been authenticated.
    /// </summary>
    fAuthenticated: BOOL;

    /// <summary>
    /// Last time the device was seen.
    /// </summary>
    stLastSeen: SYSTEMTIME;

    /// <summary>
    /// Last time the device was used.
    /// </summary>
    stLastUsed: SYSTEMTIME;

    /// <summary>
    /// Display name of the device.
    /// </summary>
    szName: array[0..BLUETOOTH_MAX_NAME_SIZE - 1] of WideChar;
  end;
  PBLUETOOTH_DEVICE_INFO = ^BLUETOOTH_DEVICE_INFO;

  /// <summary>
  /// Parameters for searching Bluetooth devices.
  /// </summary>
  BLUETOOTH_DEVICE_SEARCH_PARAMS = record
    /// <summary>
    /// Size of this structure in bytes.
    /// </summary>
    dwSize: DWORD;

    /// <summary>
    /// Return authenticated devices.
    /// </summary>
    fReturnAuthenticated: BOOL;

    /// <summary>
    /// Return remembered (paired) devices.
    /// </summary>
    fReturnRemembered: BOOL;

    /// <summary>
    /// Return unknown devices.
    /// </summary>
    fReturnUnknown: BOOL;

    /// <summary>
    /// Return connected devices.
    /// </summary>
    fReturnConnected: BOOL;

    /// <summary>
    /// Issue a new inquiry (scan for devices).
    /// </summary>
    fIssueInquiry: BOOL;

    /// <summary>
    /// Timeout multiplier for inquiry (in 1.28 second units).
    /// </summary>
    cTimeoutMultiplier: Byte;

    /// <summary>
    /// Handle to the radio to search on.
    /// Use 0 to search on all radios.
    /// </summary>
    hRadio: THandle;
  end;
  PBLUETOOTH_DEVICE_SEARCH_PARAMS = ^BLUETOOTH_DEVICE_SEARCH_PARAMS;

// =============================================================================
// Bluetooth Radio Functions
// =============================================================================

/// <summary>
/// Finds the first Bluetooth radio adapter.
/// </summary>
/// <param name="pbtfrp">Pointer to search parameters.</param>
/// <param name="phRadio">Receives the handle to the radio.</param>
/// <returns>Handle to the find operation, or 0 on failure.</returns>
function BluetoothFindFirstRadio(
  const pbtfrp: PBLUETOOTH_FIND_RADIO_PARAMS;
  out phRadio: THandle
): HBLUETOOTH_RADIO_FIND; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Finds the next Bluetooth radio adapter.
/// </summary>
/// <param name="hFind">Handle from BluetoothFindFirstRadio.</param>
/// <param name="phRadio">Receives the handle to the next radio.</param>
/// <returns>True if another radio was found.</returns>
function BluetoothFindNextRadio(
  hFind: HBLUETOOTH_RADIO_FIND;
  out phRadio: THandle
): BOOL; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Closes a radio find handle.
/// </summary>
/// <param name="hFind">Handle to close.</param>
/// <returns>True on success.</returns>
function BluetoothFindRadioClose(
  hFind: HBLUETOOTH_RADIO_FIND
): BOOL; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Gets information about a Bluetooth radio.
/// </summary>
/// <param name="hRadio">Handle to the radio.</param>
/// <param name="pRadioInfo">Receives the radio information.</param>
/// <returns>ERROR_SUCCESS on success.</returns>
function BluetoothGetRadioInfo(
  hRadio: THandle;
  var pRadioInfo: BLUETOOTH_RADIO_INFO
): DWORD; stdcall; external BLUETOOTH_API_LIB;

// =============================================================================
// Bluetooth Device Functions
// =============================================================================

/// <summary>
/// Finds the first Bluetooth device matching the search parameters.
/// </summary>
/// <param name="pbtsp">Pointer to search parameters.</param>
/// <param name="pbtdi">Receives device information.</param>
/// <returns>Handle to the find operation, or 0 on failure.</returns>
function BluetoothFindFirstDevice(
  const pbtsp: PBLUETOOTH_DEVICE_SEARCH_PARAMS;
  var pbtdi: BLUETOOTH_DEVICE_INFO
): HBLUETOOTH_DEVICE_FIND; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Finds the next Bluetooth device.
/// </summary>
/// <param name="hFind">Handle from BluetoothFindFirstDevice.</param>
/// <param name="pbtdi">Receives device information.</param>
/// <returns>True if another device was found.</returns>
function BluetoothFindNextDevice(
  hFind: HBLUETOOTH_DEVICE_FIND;
  var pbtdi: BLUETOOTH_DEVICE_INFO
): BOOL; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Closes a device find handle.
/// </summary>
/// <param name="hFind">Handle to close.</param>
/// <returns>True on success.</returns>
function BluetoothFindDeviceClose(
  hFind: HBLUETOOTH_DEVICE_FIND
): BOOL; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Refreshes device information from the system.
/// </summary>
/// <param name="pbtdi">Device info structure with Address set.</param>
/// <returns>ERROR_SUCCESS on success.</returns>
function BluetoothGetDeviceInfo(
  hRadio: THandle;
  var pbtdi: BLUETOOTH_DEVICE_INFO
): DWORD; stdcall; external BLUETOOTH_API_LIB;

// =============================================================================
// Bluetooth Service Functions
// =============================================================================

/// <summary>
/// Enables or disables a Bluetooth service for a device.
/// This is the primary method to connect/disconnect devices.
/// </summary>
/// <param name="hRadio">Handle to the radio (can be 0).</param>
/// <param name="pbtdi">Device information.</param>
/// <param name="pGuidService">GUID of the service to enable/disable.</param>
/// <param name="dwServiceFlags">BLUETOOTH_SERVICE_ENABLE or BLUETOOTH_SERVICE_DISABLE.</param>
/// <returns>ERROR_SUCCESS on success.</returns>
function BluetoothSetServiceState(
  hRadio: THandle;
  const pbtdi: PBLUETOOTH_DEVICE_INFO;
  const pGuidService: PGUID;
  dwServiceFlags: DWORD
): DWORD; stdcall; external BLUETOOTH_API_LIB;

/// <summary>
/// Enumerates the services installed for a device.
/// </summary>
/// <param name="hRadio">Handle to the radio (can be 0).</param>
/// <param name="pbtdi">Device information.</param>
/// <param name="pcServiceInout">In: size of pGuidServices array. Out: number of services.</param>
/// <param name="pGuidServices">Array to receive service GUIDs.</param>
/// <returns>ERROR_SUCCESS on success.</returns>
function BluetoothEnumerateInstalledServices(
  hRadio: THandle;
  const pbtdi: PBLUETOOTH_DEVICE_INFO;
  var pcServiceInout: DWORD;
  pGuidServices: PGUID
): DWORD; stdcall; external BLUETOOTH_API_LIB;

// =============================================================================
// Helper Functions
// =============================================================================

/// <summary>
/// Extracts the major device class from a Class of Device value.
/// </summary>
/// <param name="AClassOfDevice">The full CoD value.</param>
/// <returns>Major device class (bits 8-12).</returns>
function GetMajorDeviceClass(AClassOfDevice: ULONG): Byte; inline;

/// <summary>
/// Extracts the minor device class from a Class of Device value.
/// </summary>
/// <param name="AClassOfDevice">The full CoD value.</param>
/// <returns>Minor device class (bits 2-7).</returns>
function GetMinorDeviceClass(AClassOfDevice: ULONG): Byte; inline;

/// <summary>
/// Formats a Bluetooth address as a string (XX:XX:XX:XX:XX:XX).
/// </summary>
/// <param name="AAddress">The Bluetooth address.</param>
/// <returns>Formatted address string.</returns>
function BluetoothAddressToString(const AAddress: BLUETOOTH_ADDRESS): string;

/// <summary>
/// Initializes a BLUETOOTH_DEVICE_SEARCH_PARAMS structure for finding paired devices.
/// </summary>
/// <param name="AParams">The structure to initialize.</param>
/// <param name="ARadioHandle">Optional radio handle (0 for all radios).</param>
procedure InitDeviceSearchParams(
  out AParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  ARadioHandle: THandle = 0
);

/// <summary>
/// Initializes a BLUETOOTH_DEVICE_INFO structure.
/// </summary>
/// <param name="ADeviceInfo">The structure to initialize.</param>
procedure InitDeviceInfo(out ADeviceInfo: BLUETOOTH_DEVICE_INFO);

implementation

uses
  System.SysUtils;

function GetMajorDeviceClass(AClassOfDevice: ULONG): Byte;
begin
  Result := (AClassOfDevice shr 8) and $1F;
end;

function GetMinorDeviceClass(AClassOfDevice: ULONG): Byte;
begin
  Result := (AClassOfDevice shr 2) and $3F;
end;

function BluetoothAddressToString(const AAddress: BLUETOOTH_ADDRESS): string;
begin
  Result := Format('%.2X:%.2X:%.2X:%.2X:%.2X:%.2X', [
    AAddress.rgBytes[5],
    AAddress.rgBytes[4],
    AAddress.rgBytes[3],
    AAddress.rgBytes[2],
    AAddress.rgBytes[1],
    AAddress.rgBytes[0]
  ]);
end;

procedure InitDeviceSearchParams(
  out AParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  ARadioHandle: THandle);
begin
  FillChar(AParams, SizeOf(AParams), 0);
  AParams.dwSize := SizeOf(BLUETOOTH_DEVICE_SEARCH_PARAMS);
  AParams.fReturnAuthenticated := True;
  AParams.fReturnRemembered := True;
  AParams.fReturnConnected := True;
  AParams.fReturnUnknown := False;
  AParams.fIssueInquiry := False;
  AParams.cTimeoutMultiplier := 0;
  AParams.hRadio := ARadioHandle;
end;

procedure InitDeviceInfo(out ADeviceInfo: BLUETOOTH_DEVICE_INFO);
begin
  FillChar(ADeviceInfo, SizeOf(ADeviceInfo), 0);
  ADeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
end;

end.
