{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Pairing Strategy Pattern Implementation         }
{                                                       }
{       Provides extensible pairing strategies for      }
{       Windows Classic and WinRT Bluetooth APIs.       }
{       Follows same pattern as connection strategies.  }
{                                                       }
{*******************************************************}

unit Bluetooth.PairingStrategies;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  App.ConfigEnums,
  App.ConnectionConfigIntf;

type
  /// <summary>
  /// Interface for pairing strategy implementations.
  /// Allows different pairing approaches (Windows Classic, WinRT, custom).
  /// </summary>
  IPairingStrategy = interface
    ['{F1A2B3C4-5555-6666-7777-888899990000}']

    /// <summary>
    /// Attempts to pair with a device.
    /// </summary>
    /// <param name="ADevice">Device to pair with.</param>
    /// <param name="AProgressCallback">Optional callback for user feedback.</param>
    /// <returns>Pairing result.</returns>
    function Pair(
      const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback
    ): TPairingResult;

    /// <summary>
    /// Attempts to unpair a device.
    /// </summary>
    /// <param name="ADeviceAddress">Address of device to unpair.</param>
    /// <returns>Pairing result.</returns>
    function Unpair(ADeviceAddress: UInt64): TPairingResult;

    /// <summary>
    /// Checks if this strategy can handle pairing for the given platform.
    /// </summary>
    /// <param name="APlatform">Bluetooth platform.</param>
    /// <returns>True if strategy supports this platform.</returns>
    function CanHandle(APlatform: TBluetoothPlatform): Boolean;

    /// <summary>
    /// Gets strategy name for logging.
    /// </summary>
    function GetName: string;

    /// <summary>
    /// Gets priority (higher = preferred).
    /// Used by factory to select best strategy.
    /// </summary>
    function GetPriority: Integer;
  end;

  /// <summary>
  /// Factory for creating and managing pairing strategies.
  /// Selects appropriate strategy based on platform configuration.
  /// </summary>
  IPairingStrategyFactory = interface
    ['{F2A3B4C5-6666-7777-8888-999900001111}']

    /// <summary>
    /// Gets the appropriate pairing strategy for the current platform.
    /// Selects highest-priority strategy that can handle the platform.
    /// </summary>
    /// <param name="APlatform">Bluetooth platform.</param>
    /// <returns>Pairing strategy, or nil if none available.</returns>
    function GetStrategy(APlatform: TBluetoothPlatform): IPairingStrategy;

    /// <summary>
    /// Registers a custom pairing strategy.
    /// Allows extending with new strategies at runtime.
    /// </summary>
    /// <param name="AStrategy">Strategy to register.</param>
    procedure RegisterStrategy(AStrategy: IPairingStrategy);

    /// <summary>
    /// Clears all registered strategies (for testing).
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  /// Windows Classic Bluetooth pairing strategy.
  /// Uses BluetoothAuthenticateDevice API (Win7-Win11).
  /// Delegates pairing to Windows dialog, provides SSP support.
  /// </summary>
  TWindowsPairingStrategy = class(TInterfacedObject, IPairingStrategy)
  private
    /// <summary>
    /// Converts Windows error code to user-friendly message.
    /// </summary>
    function GetErrorMessage(AErrorCode: Cardinal): string;
  public
    /// <summary>
    /// Pairs device using Windows authentication dialog.
    /// </summary>
    function Pair(
      const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback
    ): TPairingResult;

    /// <summary>
    /// Unpairs device using Windows API.
    /// </summary>
    function Unpair(ADeviceAddress: UInt64): TPairingResult;

    /// <summary>
    /// Handles Classic and Auto platforms.
    /// </summary>
    function CanHandle(APlatform: TBluetoothPlatform): Boolean;

    /// <summary>
    /// Returns "Windows Classic Pairing".
    /// </summary>
    function GetName: string;

    /// <summary>
    /// Returns priority 50 (default/fallback).
    /// </summary>
    function GetPriority: Integer;
  end;

  /// <summary>
  /// WinRT Bluetooth Simple pairing strategy.
  /// Uses Windows.Devices.Enumeration DeviceInformation.Pairing.PairAsync.
  /// Delegates pairing UI to Windows system dialogs (automatic mode).
  /// Requires Windows 8+ and WinRT APIs.
  /// </summary>
  TWinRTSimplePairingStrategy = class(TInterfacedObject, IPairingStrategy)
  private
    FConnectionConfig: IConnectionConfig;

    /// <summary>
    /// Converts WinRT DevicePairingResultStatus to TPairingResult.
    /// Maps all 20 WinRT status codes to appropriate result types.
    /// </summary>
    function MapPairingResult(AStatus: Integer; const ADevice: TBluetoothDeviceInfo): TPairingResult;
  public
    /// <summary>
    /// Creates simple pairing strategy with optional config injection.
    /// </summary>
    constructor Create(AConnectionConfig: IConnectionConfig);

    /// <summary>
    /// Pairs device using Windows automatic pairing dialogs.
    /// Timeout configured via IConnectionConfig.PairingTimeout.
    /// </summary>
    function Pair(
      const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback
    ): TPairingResult;

    /// <summary>
    /// Unpairs device using WinRT UnpairAsync.
    /// </summary>
    function Unpair(ADeviceAddress: UInt64): TPairingResult;

    /// <summary>
    /// Handles WinRT and Auto platforms when PairingMode=Automatic.
    /// </summary>
    function CanHandle(APlatform: TBluetoothPlatform): Boolean;

    /// <summary>
    /// Returns "WinRT Simple Pairing (Windows Dialogs)".
    /// </summary>
    function GetName: string;

    /// <summary>
    /// Returns priority 100 (preferred over Classic when mode=Automatic).
    /// </summary>
    function GetPriority: Integer;
  end;

  /// <summary>
  /// WinRT Custom pairing strategy (future implementation).
  /// Uses DeviceInformationCustomPairing with PairingRequested event.
  /// Application handles pairing UI and ceremony.
  /// Currently returns NotSupported - stub for Phase 2.
  /// </summary>
  TWinRTCustomPairingStrategy = class(TInterfacedObject, IPairingStrategy)
  public
    function Pair(
      const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback
    ): TPairingResult;

    function Unpair(ADeviceAddress: UInt64): TPairingResult;

    function CanHandle(APlatform: TBluetoothPlatform): Boolean;

    function GetName: string;

    function GetPriority: Integer;
  end;

  /// <summary>
  /// Pairing strategy factory implementation.
  /// Manages strategy registration and selection.
  /// </summary>
  TPairingStrategyFactory = class(TInterfacedObject, IPairingStrategyFactory)
  private
    FStrategies: TList<IPairingStrategy>;
    FConnectionConfig: IConnectionConfig;

    /// <summary>
    /// Registers default strategies (Windows, WinRT Simple, WinRT Custom).
    /// </summary>
    procedure RegisterDefaultStrategies;
  public
    /// <summary>
    /// Creates factory with optional config for strategy injection.
    /// </summary>
    constructor Create(AConnectionConfig: IConnectionConfig = nil);
    destructor Destroy; override;

    /// <summary>
    /// Gets highest-priority strategy that can handle the platform.
    /// </summary>
    function GetStrategy(APlatform: TBluetoothPlatform): IPairingStrategy;

    /// <summary>
    /// Registers a custom strategy.
    /// </summary>
    procedure RegisterStrategy(AStrategy: IPairingStrategy);

    /// <summary>
    /// Clears all strategies (for testing).
    /// </summary>
    procedure Clear;
  end;

/// <summary>
/// Creates a pairing strategy factory with default strategies.
/// </summary>
/// <param name="AConnectionConfig">Optional connection config for strategy injection.</param>
function CreatePairingStrategyFactory(AConnectionConfig: IConnectionConfig = nil): IPairingStrategyFactory;

implementation

uses
  Winapi.ActiveX,
  App.Logger,
  App.Config,
  App.WinRTSupport,
  WinRT.AsyncHelpers,
  Bluetooth.WinRTPairingInterfaces,
  Bluetooth.WinRTDeviceQuery;

{ TWindowsPairingStrategy }

function TWindowsPairingStrategy.GetErrorMessage(AErrorCode: Cardinal): string;
begin
  case AErrorCode of
    ERROR_SUCCESS:
      Result := 'Success';
    ERROR_CANCELLED:
      Result := 'Pairing cancelled';
    ERROR_TIMEOUT:
      Result := 'Pairing timed out';
    ERROR_INVALID_PARAMETER:
      Result := 'Invalid parameter or device not found';
    ERROR_NOT_SUPPORTED:
      Result := 'Pairing not supported';
    ERROR_ACCESS_DENIED:
      Result := 'Access denied - check Bluetooth permissions';
    ERROR_DEVICE_NOT_FOUND:
      Result := 'Device not found or not in pairing mode. Ensure device is in pairing mode and in range';
  else
    Result := Format('Pairing failed (error code %d)', [AErrorCode]);
  end;
end;

function TWindowsPairingStrategy.Pair(
  const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback
): TPairingResult;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ErrorCode: DWORD;
begin
  LogInfo('Pair: Initiating pairing for device $%.12X, Name="%s"',
    [ADevice.AddressInt, ADevice.Name], ClassName);

  // Initialize device info structure
  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address.ullLong := ADevice.AddressInt;

  // Query current device information
  ErrorCode := BluetoothGetDeviceInfo(0, DeviceInfo);

  if ErrorCode <> ERROR_SUCCESS then
  begin
    LogError('Pair: BluetoothGetDeviceInfo failed with error %d - device may not be discoverable or in range',
      [ErrorCode], ClassName);
    Result := TPairingResult.Failed(ErrorCode, GetErrorMessage(ErrorCode));
    Exit;
  end;

  LogDebug('Pair: Device state - Connected=%d, Remembered=%d, Authenticated=%d',
    [Ord(DeviceInfo.fConnected), Ord(DeviceInfo.fRemembered), Ord(DeviceInfo.fAuthenticated)], ClassName);

  // Check if already paired (fAuthenticated = truly paired, fRemembered = just known/remembered)
  if DeviceInfo.fAuthenticated then
  begin
    LogInfo('Pair: Device is already paired', ClassName);
    Result := TPairingResult.AlreadyPaired;
    Exit;
  end;

  // Notify progress callback
  if Assigned(AProgressCallback) then
    AProgressCallback('Waiting for pairing confirmation...');

  // Initiate pairing using Secure Simple Pairing (SSP)
  // hwndParent=0: Use default parent window
  // hRadio=0: Use default radio
  // pszPasskey=nil, ulPasskeyLength=0: Use SSP (no PIN required for modern devices)
  LogDebug('Pair: Calling BluetoothAuthenticateDevice', ClassName);
  ErrorCode := BluetoothAuthenticateDevice(0, 0, @DeviceInfo, nil, 0);

  case ErrorCode of
    ERROR_SUCCESS:
      begin
        LogInfo('Pair: Pairing succeeded', ClassName);
        Result := TPairingResult.Success;
      end;

    ERROR_CANCELLED:
      begin
        LogInfo('Pair: Pairing cancelled by user', ClassName);
        Result := TPairingResult.Cancelled;
      end;

    ERROR_TIMEOUT:
      begin
        LogWarning('Pair: Pairing timed out', ClassName);
        Result := TPairingResult.Timeout;
      end;

    ERROR_DEVICE_NOT_FOUND:
      begin
        LogError('Pair: Device not found or not in pairing mode (error %d). ' +
          'Device must be in pairing mode, not just discoverable.', [ERROR_DEVICE_NOT_FOUND], ClassName);
        Result := TPairingResult.Failed(ErrorCode, GetErrorMessage(ErrorCode));
      end;

  else
    begin
      LogError('Pair: Pairing failed with error %d (%s)', [ErrorCode, GetErrorMessage(ErrorCode)], ClassName);
      Result := TPairingResult.Failed(ErrorCode, GetErrorMessage(ErrorCode));
    end;
  end;
end;

function TWindowsPairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
var
  Address: BLUETOOTH_ADDRESS;
  ErrorCode: DWORD;
begin
  LogInfo('Unpair: Removing pairing for device $%.12X', [ADeviceAddress], ClassName);

  Address.ullLong := ADeviceAddress;

  ErrorCode := BluetoothRemoveDevice(@Address);

  if ErrorCode = ERROR_SUCCESS then
  begin
    LogInfo('Unpair: Device unpaired successfully', ClassName);
    Result := TPairingResult.Success;
  end
  else
  begin
    LogError('Unpair: Failed with error %d', [ErrorCode], ClassName);
    Result := TPairingResult.Failed(ErrorCode, GetErrorMessage(ErrorCode));
  end;
end;

function TWindowsPairingStrategy.CanHandle(APlatform: TBluetoothPlatform): Boolean;
begin
  // Supports Classic Bluetooth and Auto (fallback to Classic)
  Result := APlatform in [bpClassic, bpAuto];
end;

function TWindowsPairingStrategy.GetName: string;
begin
  Result := 'Windows Classic Pairing';
end;

function TWindowsPairingStrategy.GetPriority: Integer;
begin
  // Default priority - can be overridden by higher-priority strategies
  Result := 50;
end;

{ TWinRTSimplePairingStrategy }

constructor TWinRTSimplePairingStrategy.Create(AConnectionConfig: IConnectionConfig);
begin
  inherited Create;
  FConnectionConfig := AConnectionConfig;
  LogDebug('Create: Simple pairing strategy initialized', ClassName);
end;

function TWinRTSimplePairingStrategy.MapPairingResult(AStatus: Integer;
  const ADevice: TBluetoothDeviceInfo): TPairingResult;
var
  Status: TDevicePairingResultStatus;
  Message: string;
begin
  Status := IntToPairingResultStatus(AStatus);
  Message := GetPairingResultMessage(Status);

  case Status of
    dprsSuccess:
      Result := TPairingResult.Success;

    dprsAlreadyPaired:
      Result := TPairingResult.AlreadyPaired;

    dprsPairingCanceled:
      Result := TPairingResult.Cancelled;

    dprsAuthenticationTimeout:
      Result := TPairingResult.Timeout;

    dprsNotReadyToPair,
    dprsNotPaired,
    dprsConnectionRejected,
    dprsTooManyConnections,
    dprsHardwareFailure,
    dprsAuthenticationNotAllowed,
    dprsAuthenticationFailure,
    dprsNoSupportedProfiles,
    dprsProtectionLevelCouldNotBeMet,
    dprsAccessDenied,
    dprsInvalidCeremonyData,
    dprsOperationAlreadyInProgress,
    dprsRequiredHandlerNotRegistered,
    dprsRejectedByHandler,
    dprsRemoteDeviceHasAssociation,
    dprsFailed:
      Result := TPairingResult.Failed(AStatus, Message);
  else
    Result := TPairingResult.Failed(AStatus, Format('Unknown pairing status: %d', [AStatus]));
  end;
end;

function TWinRTSimplePairingStrategy.Pair(
  const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback
): TPairingResult;
var
  HR: HRESULT;
  DeviceId: string;
  DeviceIdStr: HSTRING;
  Factory: IInspectable;
  DevInfoStatics: IDeviceInformationStatics;
  AsyncDevInfoOp: IInspectable;
  AsyncInfo: IAsyncInfo;
  DevInfo: IDeviceInformation;
  PairingIntf: IDeviceInformationPairing;
  AsyncPairOp: IAsyncOperationDevicePairingResult;
  PairResult: IDevicePairingResult;
  Status: Integer;
  TimeoutMs: Cardinal;
begin
  LogInfo('Pair: Initiating WinRT simple pairing for device $%.12X, Name="%s"',
    [ADevice.AddressInt, ADevice.Name], ClassName);

  // Check WinRT availability
  if not EnsureWinRTInitialized(ClassName) then
  begin
    LogError('Pair: WinRT not available', ClassName);
    Result := TPairingResult.NotSupported('WinRT APIs not available (requires Windows 8+)');
    Exit;
  end;

  // Get timeout from config (default 30 seconds)
  TimeoutMs := DEF_PAIRING_TIMEOUT;
  if Assigned(FConnectionConfig) then
    TimeoutMs := FConnectionConfig.PairingTimeout;

  // Build WinRT device ID from Bluetooth address
  // Format: Bluetooth#Bluetooth<addr_reversed>-<addr_formatted>
  DeviceId := Format('Bluetooth#Bluetooth%.12x-%.2x:%.2x:%.2x:%.2x:%.2x:%.2x',
    [((ADevice.AddressInt and $FF) shl 40) or
     ((ADevice.AddressInt and $FF00) shl 24) or
     ((ADevice.AddressInt and $FF0000) shl 8) or
     ((ADevice.AddressInt and $FF000000) shr 8) or
     ((ADevice.AddressInt and $FF00000000) shr 24) or
     ((ADevice.AddressInt and $FF0000000000) shr 40),
     (ADevice.AddressInt shr 40) and $FF,
     (ADevice.AddressInt shr 32) and $FF,
     (ADevice.AddressInt shr 24) and $FF,
     (ADevice.AddressInt shr 16) and $FF,
     (ADevice.AddressInt shr 8) and $FF,
     ADevice.AddressInt and $FF]);

  LogDebug('Pair: Device ID: %s', [DeviceId], ClassName);

  // Get DeviceInformation.CreateFromIdAsync
  if not GetActivationFactory('Windows.Devices.Enumeration.DeviceInformation',
      IDeviceInformationStatics, Factory, ClassName) then
  begin
    Result := TPairingResult.Failed(0, 'Failed to get DeviceInformation factory');
    Exit;
  end;

  HR := Factory.QueryInterface(IDeviceInformationStatics, DevInfoStatics);
  if Failed(HR) or (DevInfoStatics = nil) then
  begin
    LogError('Pair: QueryInterface for DevInfoStatics failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to query DeviceInformationStatics interface');
    Exit;
  end;

  // Create DeviceInformation from ID
  DeviceIdStr := CreateHString(DeviceId);
  try
    HR := DevInfoStatics.CreateFromIdAsync(DeviceIdStr, AsyncDevInfoOp);
    if Failed(HR) or (AsyncDevInfoOp = nil) then
    begin
      LogError('Pair: CreateFromIdAsync failed: 0x%.8X', [HR], ClassName);
      Result := TPairingResult.Failed(HR, 'Device not found or not in pairing mode');
      Exit;
    end;
  finally
    FreeHString(DeviceIdStr);
  end;

  // Wait for DeviceInformation
  if not Supports(AsyncDevInfoOp, IAsyncInfo, AsyncInfo) then
  begin
    LogError('Pair: Failed to get IAsyncInfo for CreateFromIdAsync', ClassName);
    Result := TPairingResult.Failed(0, 'Internal error: async operation failed');
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, 10000, ClassName) then
  begin
    LogError('Pair: CreateFromIdAsync timeout', ClassName);
    Result := TPairingResult.Timeout;
    Exit;
  end;

  // Get DeviceInformation
  HR := (AsyncDevInfoOp as IAsyncOperationDeviceInformation).GetResults(DevInfo);
  if Failed(HR) or (DevInfo = nil) then
  begin
    LogError('Pair: GetResults for DeviceInformation failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to retrieve device information');
    Exit;
  end;

  // Get Pairing interface
  PairingIntf := GetDevicePairingInterface(DevInfo);
  if PairingIntf = nil then
  begin
    LogError('Pair: Failed to get pairing interface', ClassName);
    Result := TPairingResult.Failed(0, 'Device does not support pairing');
    Exit;
  end;

  // Check if already paired
  var IsPaired: Boolean := False;
  HR := PairingIntf.get_IsPaired(IsPaired);
  if Succeeded(HR) and IsPaired then
  begin
    LogInfo('Pair: Device is already paired', ClassName);
    Result := TPairingResult.AlreadyPaired;
    Exit;
  end;

  // Check if can pair
  var CanPair: Boolean := False;
  HR := PairingIntf.get_CanPair(CanPair);
  if Failed(HR) or not CanPair then
  begin
    LogError('Pair: Device cannot be paired (CanPair=False)', ClassName);
    Result := TPairingResult.Failed(HR, 'Device not ready to pair or pairing not allowed');
    Exit;
  end;

  // Notify progress
  if Assigned(AProgressCallback) then
    AProgressCallback('Waiting for pairing confirmation...');

  // Initiate pairing (Windows will show dialog)
  LogDebug('Pair: Calling PairAsync', ClassName);
  HR := PairingIntf.PairAsync(AsyncPairOp);
  if Failed(HR) or (AsyncPairOp = nil) then
  begin
    LogError('Pair: PairAsync failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to initiate pairing');
    Exit;
  end;

  // Wait for pairing to complete
  if not Supports(AsyncPairOp, IAsyncInfo, AsyncInfo) then
  begin
    LogError('Pair: Failed to get IAsyncInfo for PairAsync', ClassName);
    Result := TPairingResult.Failed(0, 'Internal error: async pairing operation failed');
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, TimeoutMs, ClassName) then
  begin
    LogWarning('Pair: Pairing timeout after %dms', [TimeoutMs], ClassName);
    Result := TPairingResult.Timeout;
    Exit;
  end;

  // Get pairing result
  HR := AsyncPairOp.GetResults(PairResult);
  if Failed(HR) or (PairResult = nil) then
  begin
    LogError('Pair: GetResults for pairing failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to retrieve pairing result');
    Exit;
  end;

  // Get status
  HR := PairResult.get_Status(Status);
  if Failed(HR) then
  begin
    LogError('Pair: get_Status failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get pairing status');
    Exit;
  end;

  // Map result
  Result := MapPairingResult(Status, ADevice);

  if Result.IsSuccess then
    LogInfo('Pair: Pairing succeeded', ClassName)
  else
    LogError('Pair: Pairing failed - Status=%d, Message=%s',
      [Status, Result.ErrorMessage], ClassName);
end;

function TWinRTSimplePairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
var
  HR: HRESULT;
  DeviceId: string;
  DeviceIdStr: HSTRING;
  Factory: IInspectable;
  DevInfoStatics: IDeviceInformationStatics;
  AsyncDevInfoOp: IInspectable;
  AsyncInfo: IAsyncInfo;
  DevInfo: IDeviceInformation;
  PairingIntf: IDeviceInformationPairing;
  Pairing2: IDeviceInformationPairing2;
  AsyncUnpairOp: IAsyncOperationDeviceUnpairingResult;
  UnpairResult: IDeviceUnpairingResult;
  Status: Integer;
begin
  LogInfo('Unpair: Removing pairing for device $%.12X', [ADeviceAddress], ClassName);

  // Check WinRT availability
  if not EnsureWinRTInitialized(ClassName) then
  begin
    LogError('Unpair: WinRT not available', ClassName);
    Result := TPairingResult.NotSupported('WinRT APIs not available (requires Windows 8+)');
    Exit;
  end;

  // Build WinRT device ID
  DeviceId := Format('Bluetooth#Bluetooth%.12x-%.2x:%.2x:%.2x:%.2x:%.2x:%.2x',
    [((ADeviceAddress and $FF) shl 40) or
     ((ADeviceAddress and $FF00) shl 24) or
     ((ADeviceAddress and $FF0000) shl 8) or
     ((ADeviceAddress and $FF000000) shr 8) or
     ((ADeviceAddress and $FF00000000) shr 24) or
     ((ADeviceAddress and $FF0000000000) shr 40),
     (ADeviceAddress shr 40) and $FF,
     (ADeviceAddress shr 32) and $FF,
     (ADeviceAddress shr 24) and $FF,
     (ADeviceAddress shr 16) and $FF,
     (ADeviceAddress shr 8) and $FF,
     ADeviceAddress and $FF]);

  LogDebug('Unpair: Device ID: %s', [DeviceId], ClassName);

  // Get DeviceInformation
  if not GetActivationFactory('Windows.Devices.Enumeration.DeviceInformation',
      IDeviceInformationStatics, Factory, ClassName) then
  begin
    Result := TPairingResult.Failed(0, 'Failed to get DeviceInformation factory');
    Exit;
  end;

  HR := Factory.QueryInterface(IDeviceInformationStatics, DevInfoStatics);
  if Failed(HR) or (DevInfoStatics = nil) then
  begin
    LogError('Unpair: QueryInterface for DevInfoStatics failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to query DeviceInformationStatics interface');
    Exit;
  end;

  DeviceIdStr := CreateHString(DeviceId);
  try
    HR := DevInfoStatics.CreateFromIdAsync(DeviceIdStr, AsyncDevInfoOp);
    if Failed(HR) or (AsyncDevInfoOp = nil) then
    begin
      LogError('Unpair: CreateFromIdAsync failed: 0x%.8X', [HR], ClassName);
      Result := TPairingResult.Failed(HR, 'Device not found');
      Exit;
    end;
  finally
    FreeHString(DeviceIdStr);
  end;

  if not Supports(AsyncDevInfoOp, IAsyncInfo, AsyncInfo) then
  begin
    LogError('Unpair: Failed to get IAsyncInfo', ClassName);
    Result := TPairingResult.Failed(0, 'Internal error');
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, 10000, ClassName) then
  begin
    LogError('Unpair: CreateFromIdAsync timeout', ClassName);
    Result := TPairingResult.Timeout;
    Exit;
  end;

  HR := (AsyncDevInfoOp as IAsyncOperationDeviceInformation).GetResults(DevInfo);
  if Failed(HR) or (DevInfo = nil) then
  begin
    LogError('Unpair: GetResults failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get device information');
    Exit;
  end;

  // Get Pairing interface
  PairingIntf := GetDevicePairingInterface(DevInfo);
  if PairingIntf = nil then
  begin
    LogError('Unpair: Failed to get pairing interface', ClassName);
    Result := TPairingResult.Failed(0, 'Device does not support pairing operations');
    Exit;
  end;

  // Query for IDeviceInformationPairing2 (adds UnpairAsync)
  HR := PairingIntf.QueryInterface(IDeviceInformationPairing2, Pairing2);
  if Failed(HR) or (Pairing2 = nil) then
  begin
    LogError('Unpair: QueryInterface for Pairing2 failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Unpairing not supported (requires Windows 10+)');
    Exit;
  end;

  // Initiate unpairing
  LogDebug('Unpair: Calling UnpairAsync', ClassName);
  HR := Pairing2.UnpairAsync(AsyncUnpairOp);
  if Failed(HR) or (AsyncUnpairOp = nil) then
  begin
    LogError('Unpair: UnpairAsync failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to initiate unpairing');
    Exit;
  end;

  if not Supports(AsyncUnpairOp, IAsyncInfo, AsyncInfo) then
  begin
    LogError('Unpair: Failed to get IAsyncInfo for UnpairAsync', ClassName);
    Result := TPairingResult.Failed(0, 'Internal error');
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, 10000, ClassName) then
  begin
    LogWarning('Unpair: Unpairing timeout', ClassName);
    Result := TPairingResult.Timeout;
    Exit;
  end;

  HR := AsyncUnpairOp.GetResults(UnpairResult);
  if Failed(HR) or (UnpairResult = nil) then
  begin
    LogError('Unpair: GetResults failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get unpairing result');
    Exit;
  end;

  HR := UnpairResult.get_Status(Status);
  if Failed(HR) then
  begin
    LogError('Unpair: get_Status failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get unpairing status');
    Exit;
  end;

  // Status 0 = success for unpairing (DeviceUnpairingResultStatus.Unpaired)
  if Status = 0 then
  begin
    LogInfo('Unpair: Device unpaired successfully', ClassName);
    Result := TPairingResult.Success;
  end
  else
  begin
    LogError('Unpair: Failed with status %d', [Status], ClassName);
    Result := TPairingResult.Failed(Status, Format('Unpairing failed with status %d', [Status]));
  end;
end;

function TWinRTSimplePairingStrategy.CanHandle(APlatform: TBluetoothPlatform): Boolean;
begin
  // Supports WinRT and Auto platforms
  // Only handles when PairingMode is Automatic (checked by factory)
  Result := APlatform in [bpWinRT, bpAuto];
end;

function TWinRTSimplePairingStrategy.GetName: string;
begin
  Result := 'WinRT Simple Pairing (Windows Dialogs)';
end;

function TWinRTSimplePairingStrategy.GetPriority: Integer;
begin
  // Higher priority than Classic (100 vs 50)
  // Preferred when WinRT is available and mode is Automatic
  Result := 100;
end;

{ TWinRTCustomPairingStrategy }

function TWinRTCustomPairingStrategy.Pair(
  const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback
): TPairingResult;
begin
  LogInfo('Pair: WinRT custom pairing not yet implemented (Phase 2)', ClassName);
  Result := TPairingResult.NotSupported('Custom pairing with in-app UI will be implemented in Phase 2');
end;

function TWinRTCustomPairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
begin
  LogInfo('Unpair: WinRT custom pairing not yet implemented (Phase 2)', ClassName);
  Result := TPairingResult.NotSupported('Custom pairing with in-app UI will be implemented in Phase 2');
end;

function TWinRTCustomPairingStrategy.CanHandle(APlatform: TBluetoothPlatform): Boolean;
begin
  // Will support WinRT and Auto when PairingMode=Custom (Phase 2)
  Result := False;  // Not implemented yet
end;

function TWinRTCustomPairingStrategy.GetName: string;
begin
  Result := 'WinRT Custom Pairing (In-App UI) - Not Implemented';
end;

function TWinRTCustomPairingStrategy.GetPriority: Integer;
begin
  // Higher priority than Simple (110 vs 100) when Custom mode enabled
  Result := 110;
end;

{ TPairingStrategyFactory }

constructor TPairingStrategyFactory.Create(AConnectionConfig: IConnectionConfig);
begin
  inherited Create;
  FConnectionConfig := AConnectionConfig;
  FStrategies := TList<IPairingStrategy>.Create;
  RegisterDefaultStrategies;
end;

destructor TPairingStrategyFactory.Destroy;
begin
  FStrategies.Free;
  inherited Destroy;
end;

procedure TPairingStrategyFactory.RegisterDefaultStrategies;
begin
  // Register Windows Classic strategy (always available)
  RegisterStrategy(TWindowsPairingStrategy.Create);

  // Register WinRT Simple pairing strategy (requires Windows 8+)
  // Config is injected for timeout and mode configuration
  RegisterStrategy(TWinRTSimplePairingStrategy.Create(FConnectionConfig));

  // Register WinRT Custom pairing strategy (Phase 2 - not implemented yet)
  RegisterStrategy(TWinRTCustomPairingStrategy.Create);

  LogDebug('RegisterDefaultStrategies: Registered %d pairing strategies', [FStrategies.Count], ClassName);
end;

function TPairingStrategyFactory.GetStrategy(APlatform: TBluetoothPlatform): IPairingStrategy;
var
  Strategy: IPairingStrategy;
  BestStrategy: IPairingStrategy;
  BestPriority: Integer;
begin
  BestStrategy := nil;
  BestPriority := -1;

  // Find highest-priority strategy that can handle this platform
  for Strategy in FStrategies do
  begin
    if Strategy.CanHandle(APlatform) and (Strategy.GetPriority > BestPriority) then
    begin
      BestStrategy := Strategy;
      BestPriority := Strategy.GetPriority;
    end;
  end;

  if Assigned(BestStrategy) then
    LogDebug('GetStrategy: Selected "%s" (priority %d) for platform %d',
      [BestStrategy.GetName, BestPriority, Ord(APlatform)], ClassName)
  else
    LogWarning('GetStrategy: No strategy found for platform %d', [Ord(APlatform)], ClassName);

  Result := BestStrategy;
end;

procedure TPairingStrategyFactory.RegisterStrategy(AStrategy: IPairingStrategy);
begin
  if not Assigned(AStrategy) then
    Exit;

  FStrategies.Add(AStrategy);
  LogDebug('RegisterStrategy: Registered "%s" (priority %d)',
    [AStrategy.GetName, AStrategy.GetPriority], ClassName);
end;

procedure TPairingStrategyFactory.Clear;
begin
  FStrategies.Clear;
  LogDebug('Clear: All strategies cleared', ClassName);
end;

function CreatePairingStrategyFactory(AConnectionConfig: IConnectionConfig): IPairingStrategyFactory;
begin
  Result := TPairingStrategyFactory.Create(AConnectionConfig);
end;

end.
