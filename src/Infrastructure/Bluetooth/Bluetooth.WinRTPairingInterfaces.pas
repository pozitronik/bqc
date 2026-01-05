{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Bluetooth Pairing Interfaces              }
{                                                       }
{       Interface definitions for WinRT pairing APIs:   }
{       Windows.Devices.Enumeration pairing services.   }
{       Supports both simple (Windows dialogs) and      }
{       custom (event-driven) pairing ceremonies.       }
{                                                       }
{*******************************************************}

unit Bluetooth.WinRTPairingInterfaces;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  WinRT.AsyncHelpers;

type
  /// <summary>
  /// Status of a device pairing operation.
  /// Maps to Windows.Devices.Enumeration.DevicePairingResultStatus.
  /// </summary>
  TDevicePairingResultStatus = (
    /// <summary>Device successfully paired.</summary>
    dprsSuccess = 0,
    /// <summary>Device not ready to pair (may need to enter pairing mode).</summary>
    dprsNotReadyToPair = 1,
    /// <summary>Pairing was attempted but device did not pair.</summary>
    dprsNotPaired = 2,
    /// <summary>Device is already paired with this computer.</summary>
    dprsAlreadyPaired = 3,
    /// <summary>Device rejected the connection attempt.</summary>
    dprsConnectionRejected = 4,
    /// <summary>Device has too many existing connections.</summary>
    dprsTooManyConnections = 5,
    /// <summary>Hardware failure prevented pairing.</summary>
    dprsHardwareFailure = 6,
    /// <summary>Authentication timed out (user didn't respond to PIN prompt).</summary>
    dprsAuthenticationTimeout = 7,
    /// <summary>Authentication not allowed (policy restriction).</summary>
    dprsAuthenticationNotAllowed = 8,
    /// <summary>Authentication failed (wrong PIN or rejected).</summary>
    dprsAuthenticationFailure = 9,
    /// <summary>Device has no compatible Bluetooth profiles.</summary>
    dprsNoSupportedProfiles = 10,
    /// <summary>Required security level could not be met.</summary>
    dprsProtectionLevelCouldNotBeMet = 11,
    /// <summary>Access denied (permissions issue).</summary>
    dprsAccessDenied = 12,
    /// <summary>Invalid ceremony data provided.</summary>
    dprsInvalidCeremonyData = 13,
    /// <summary>User cancelled the pairing operation.</summary>
    dprsPairingCanceled = 14,
    /// <summary>Another pairing operation already in progress.</summary>
    dprsOperationAlreadyInProgress = 15,
    /// <summary>Required custom pairing handler not registered.</summary>
    dprsRequiredHandlerNotRegistered = 16,
    /// <summary>Custom pairing handler rejected the pairing.</summary>
    dprsRejectedByHandler = 17,
    /// <summary>Device already has an association with another computer.</summary>
    dprsRemoteDeviceHasAssociation = 18,
    /// <summary>Generic failure (see error message for details).</summary>
    dprsFailed = 19
  );

  /// <summary>
  /// Protection level for device pairing.
  /// Maps to Windows.Devices.Enumeration.DevicePairingProtectionLevel.
  /// </summary>
  TDevicePairingProtectionLevel = (
    /// <summary>Use system default protection level.</summary>
    dpplDefault = 0,
    /// <summary>No encryption or authentication required.</summary>
    dpplNone = 1,
    /// <summary>Encryption required.</summary>
    dpplEncryption = 2,
    /// <summary>Both encryption and authentication required.</summary>
    dpplEncryptionAndAuthentication = 3
  );

  /// <summary>
  /// Types of pairing ceremonies that can be performed.
  /// Maps to Windows.Devices.Enumeration.DevicePairingKinds (flags enum).
  /// </summary>
  TDevicePairingKinds = type Cardinal;

const
  /// <summary>No specific pairing ceremony.</summary>
  dpkNone = $0;
  /// <summary>User must confirm pairing (no PIN).</summary>
  dpkConfirmOnly = $1;
  /// <summary>Display PIN to user (device accepts PIN).</summary>
  dpkDisplayPin = $2;
  /// <summary>Prompt user to enter PIN (device generates PIN).</summary>
  dpkProvidePin = $4;
  /// <summary>Numeric comparison - user confirms PINs match on both devices.</summary>
  dpkConfirmPinMatch = $8;

type
  /// <summary>
  /// Result of a device pairing operation.
  /// Windows.Devices.Enumeration.DevicePairingResult.
  /// </summary>
  IDevicePairingResult = interface(IInspectable)
    ['{072B02BF-DD95-4025-9B37-DE51ADBA37B7}']
    function get_Status(out status: Integer): HRESULT; stdcall;
    function get_ProtectionLevelUsed(out value: Integer): HRESULT; stdcall;
  end;

  /// <summary>
  /// Result of a device unpairing operation.
  /// Windows.Devices.Enumeration.DeviceUnpairingResult.
  /// </summary>
  IDeviceUnpairingResult = interface(IInspectable)
    ['{66F44AD3-79D9-444B-92CF-A92EF72571C7}']
    function get_Status(out status: Integer): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation for DevicePairingResult.
  /// Generic IAsyncOperation<DevicePairingResult>.
  /// </summary>
  IAsyncOperationDevicePairingResult = interface(IInspectable)
    ['{8B98E195-E354-5E8E-B8CD-F3C8C2CD541C}']
    function put_Completed(handler: IInspectable): HRESULT; stdcall;
    function get_Completed(out handler: IInspectable): HRESULT; stdcall;
    function GetResults(out results: IDevicePairingResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation for DeviceUnpairingResult.
  /// Generic IAsyncOperation<DeviceUnpairingResult>.
  /// </summary>
  IAsyncOperationDeviceUnpairingResult = interface(IInspectable)
    ['{8AC4B7E1-97A3-5764-883C-A79834C29040}']
    function put_Completed(handler: IInspectable): HRESULT; stdcall;
    function get_Completed(out handler: IInspectable): HRESULT; stdcall;
    function GetResults(out results: IDeviceUnpairingResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Device pairing service (basic).
  /// Windows.Devices.Enumeration.DeviceInformationPairing.
  /// </summary>
  IDeviceInformationPairing = interface(IInspectable)
    ['{2C4769F5-F684-40D5-8469-E8DBAAB70485}']
    function get_IsPaired(out value: Boolean): HRESULT; stdcall;
    function get_CanPair(out value: Boolean): HRESULT; stdcall;
    function PairAsync(out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
    function PairWithProtectionLevelAsync(minProtectionLevel: Integer;
      out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Custom device pairing with event-driven ceremony.
  /// Windows.Devices.Enumeration.DeviceInformationCustomPairing.
  /// Allows application to handle pairing UI (PIN entry, confirmation, etc.).
  /// </summary>
  IDeviceInformationCustomPairing = interface(IInspectable)
    ['{85138C02-4EE6-4914-8370-107A39144C0E}']
    function PairAsync(pairingKindsSupported: TDevicePairingKinds;
      out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
    function PairWithProtectionLevelAsync(pairingKindsSupported: TDevicePairingKinds;
      minProtectionLevel: Integer;
      out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
    function PairWithProtectionLevelAndSettingsAsync(pairingKindsSupported: TDevicePairingKinds;
      minProtectionLevel: Integer; devicePairingSettings: IInspectable;
      out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
    function add_PairingRequested(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_PairingRequested(token: Int64): HRESULT; stdcall;
  end;

  /// <summary>
  /// Device pairing service (extended).
  /// Windows.Devices.Enumeration.DeviceInformationPairing2.
  /// Adds unpairing and custom pairing support.
  /// </summary>
  IDeviceInformationPairing2 = interface(IInspectable)
    ['{F68612FD-0AEE-4328-85CC-1C742BB1790D}']
    function get_ProtectionLevel(out value: Integer): HRESULT; stdcall;
    function get_Custom(out value: IDeviceInformationCustomPairing): HRESULT; stdcall;
    function PairWithProtectionLevelAndSettingsAsync(minProtectionLevel: Integer;
      devicePairingSettings: IInspectable;
      out operation: IAsyncOperationDevicePairingResult): HRESULT; stdcall;
    function UnpairAsync(out operation: IAsyncOperationDeviceUnpairingResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Extended device information interface with Pairing property.
  /// Windows.Devices.Enumeration.IDeviceInformation2.
  /// Provides access to device pairing capabilities.
  /// </summary>
  IDeviceInformation2 = interface(IInspectable)
    ['{F156A638-7997-48D9-A10C-269D46533F48}']
    function get_Kind(out value: Integer): HRESULT; stdcall;
    function get_Pairing(out value: IDeviceInformationPairing): HRESULT; stdcall;
  end;

  /// <summary>
  /// Event arguments for custom pairing requests.
  /// Windows.Devices.Enumeration.DevicePairingRequestedEventArgs.
  /// Provides pairing ceremony details and accept/reject methods.
  /// </summary>
  IDevicePairingRequestedEventArgs = interface(IInspectable)
    ['{F717FC56-DE6B-487F-8376-0180ACA69963}']
    function get_DeviceInformation(out value: IInspectable): HRESULT; stdcall;
    function get_PairingKind(out value: TDevicePairingKinds): HRESULT; stdcall;
    function get_Pin(out value: HSTRING): HRESULT; stdcall;
    function Accept: HRESULT; stdcall;
    function AcceptWithPin(pin: HSTRING): HRESULT; stdcall;
    function GetDeferral(out result: IInspectable): HRESULT; stdcall;
  end;

/// <summary>
/// Converts WinRT DevicePairingResultStatus to user-friendly error message.
/// Provides detailed explanations for all 20 possible status codes.
/// </summary>
function GetPairingResultMessage(AStatus: TDevicePairingResultStatus): string;

/// <summary>
/// Converts WinRT DevicePairingResultStatus integer to enum.
/// Safe conversion with range checking.
/// </summary>
function IntToPairingResultStatus(AValue: Integer): TDevicePairingResultStatus;

/// <summary>
/// Gets the pairing interface from a DeviceInformation object.
/// Pass IDeviceInformation as IInspectable. Returns nil if device doesn't support pairing or on error.
/// </summary>
function GetDevicePairingInterface(const ADeviceInfo: IInspectable): IDeviceInformationPairing;

implementation

uses
  App.Logger;

function GetPairingResultMessage(AStatus: TDevicePairingResultStatus): string;
begin
  case AStatus of
    dprsSuccess:
      Result := 'Device paired successfully';

    dprsNotReadyToPair:
      Result := 'Device not ready to pair. Make sure device is in pairing mode ' +
                '(check device manual for pairing button or procedure)';

    dprsNotPaired:
      Result := 'Pairing failed. Device did not complete pairing procedure. ' +
                'Try resetting device and attempting pairing again';

    dprsAlreadyPaired:
      Result := 'Device is already paired with this computer';

    dprsConnectionRejected:
      Result := 'Device rejected the connection. Device may require manual acceptance ' +
                'of pairing request (check device display or buttons)';

    dprsTooManyConnections:
      Result := 'Device has too many active connections. Disconnect device from other ' +
                'computers or devices, then try again';

    dprsHardwareFailure:
      Result := 'Hardware failure prevented pairing. Check that Bluetooth adapter is ' +
                'working properly and device is not defective';

    dprsAuthenticationTimeout:
      Result := 'Authentication timed out. If prompted for PIN, enter it more quickly. ' +
                'Some devices have short timeout periods (15-30 seconds)';

    dprsAuthenticationNotAllowed:
      Result := 'Authentication not allowed. Windows security policy may be preventing ' +
                'Bluetooth pairing. Check Group Policy settings or contact administrator';

    dprsAuthenticationFailure:
      Result := 'Authentication failed. If PIN was required, verify correct PIN was entered. ' +
                'Try unpairing device (if partially paired) and retry';

    dprsNoSupportedProfiles:
      Result := 'Device has no compatible Bluetooth profiles. Device may not be compatible ' +
                'with Windows or may require additional drivers';

    dprsProtectionLevelCouldNotBeMet:
      Result := 'Required security level could not be met. Device may not support ' +
                'encryption or authentication required by Windows';

    dprsAccessDenied:
      Result := 'Access denied. Check Windows permissions for Bluetooth operations. ' +
                'May require administrator privileges';

    dprsInvalidCeremonyData:
      Result := 'Invalid pairing data provided. Internal pairing protocol error. ' +
                'Try restarting Bluetooth adapter';

    dprsPairingCanceled:
      Result := 'Pairing cancelled by user';

    dprsOperationAlreadyInProgress:
      Result := 'Another pairing operation is already in progress. Wait for current ' +
                'operation to complete or restart application';

    dprsRequiredHandlerNotRegistered:
      Result := 'Required pairing handler not registered. Internal application error. ' +
                'Try restarting application';

    dprsRejectedByHandler:
      Result := 'Pairing rejected by application. Internal pairing policy blocked operation';

    dprsRemoteDeviceHasAssociation:
      Result := 'Device is already paired with another computer. Unpair device from ' +
                'other computer first (consult device manual for unpairing procedure)';

    dprsFailed:
      Result := 'Pairing failed. Check device is in pairing mode, within range (< 10 meters), ' +
                'and has sufficient battery';
  else
    Result := Format('Unknown pairing status: %d', [Ord(AStatus)]);
  end;
end;

function IntToPairingResultStatus(AValue: Integer): TDevicePairingResultStatus;
begin
  if (AValue >= Ord(Low(TDevicePairingResultStatus))) and
     (AValue <= Ord(High(TDevicePairingResultStatus))) then
    Result := TDevicePairingResultStatus(AValue)
  else
  begin
    LogWarning('IntToPairingResultStatus: Invalid status value %d, defaulting to dprsFailed', [AValue], 'WinRTPairingInterfaces');
    Result := dprsFailed;
  end;
end;

function GetDevicePairingInterface(const ADeviceInfo: IInspectable): IDeviceInformationPairing;
var
  HR: HRESULT;
  DevInfo2: IDeviceInformation2;
  PairingIntf: IDeviceInformationPairing;
begin
  Result := nil;

  if ADeviceInfo = nil then
  begin
    LogDebug('GetDevicePairingInterface: ADeviceInfo is nil', 'WinRTPairingInterfaces');
    Exit;
  end;

  // Query for IDeviceInformation2 which has Pairing property
  HR := ADeviceInfo.QueryInterface(IDeviceInformation2, DevInfo2);
  if Failed(HR) or (DevInfo2 = nil) then
  begin
    LogDebug('GetDevicePairingInterface: QueryInterface for IDeviceInformation2 failed: 0x%.8X', [HR], 'WinRTPairingInterfaces');
    Exit;
  end;

  // Get Pairing interface from DeviceInformation2
  HR := DevInfo2.get_Pairing(PairingIntf);
  if Failed(HR) or (PairingIntf = nil) then
  begin
    LogDebug('GetDevicePairingInterface: get_Pairing failed: 0x%.8X', [HR], 'WinRTPairingInterfaces');
    Exit;
  end;

  Result := PairingIntf;
end;

end.
