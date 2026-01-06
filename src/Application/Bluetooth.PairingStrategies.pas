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
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Forms,
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

    /// <summary>
    /// Fallback unpair using Classic Bluetooth API.
    /// Used when WinRT IDeviceInformationPairing2 is not available (Windows 8/8.1).
    /// </summary>
    function UnpairUsingClassicAPI(ADeviceAddress: UInt64): TPairingResult;
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

{ Helper Functions }

/// <summary>
/// Calls BluetoothAuthenticateDevice with a timeout to prevent indefinite blocking.
/// Windows pairing API can hang forever if the dialog doesn't appear or user doesn't respond.
/// This wrapper runs the call in a dedicated thread and enforces a timeout.
/// </summary>
/// <param name="ADeviceInfo">Device to pair with.</param>
/// <param name="ATimeoutMs">Timeout in milliseconds.</param>
/// <returns>Windows error code (ERROR_TIMEOUT if timed out).</returns>
function CallBluetoothAuthenticateDeviceWithTimeout(
  var ADeviceInfo: BLUETOOTH_DEVICE_INFO;
  ATimeoutMs: Cardinal
): DWORD;
var
  CompletionEvent: TEvent;
  ErrorCode: DWORD;
  PairingThread: TThread;
  WaitResult: TWaitResult;
  DeviceInfoCopy: BLUETOOTH_DEVICE_INFO;
begin
  ErrorCode := ERROR_TIMEOUT;
  DeviceInfoCopy := ADeviceInfo;  // Create local copy for thread capture
  CompletionEvent := TEvent.Create(nil, True, False, '');
  try
    // Create thread to call the blocking Windows API
    PairingThread := TThread.CreateAnonymousThread(
      procedure
      begin
        ErrorCode := BluetoothAuthenticateDevice(Application.Handle, 0, @DeviceInfoCopy, nil, 0);
        CompletionEvent.SetEvent;
      end
    );
    PairingThread.FreeOnTerminate := False;
    PairingThread.Start;

    try
      // Wait for completion with timeout
      WaitResult := CompletionEvent.WaitFor(ATimeoutMs);

      if WaitResult = wrSignaled then
      begin
        // Pairing completed (success or failure)
        Result := ErrorCode;
        PairingThread.WaitFor;  // Ensure thread is done
        PairingThread.Free;
      end
      else
      begin
        // Timeout - thread is still blocked in Windows API
        // We cannot safely terminate it, so we abandon it
        // Set FreeOnTerminate so it eventually cleans itself up
        PairingThread.FreeOnTerminate := True;
        Result := ERROR_TIMEOUT;
      end;
    except
      PairingThread.Free;
      raise;
    end;
  finally
    CompletionEvent.Free;
  end;
end;

{ TWindowsPairingStrategy }

function TWindowsPairingStrategy.GetErrorMessage(AErrorCode: Cardinal): string;
const
  ERROR_NOT_AUTHENTICATED = 1244;
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
    ERROR_NOT_AUTHENTICATED:
      Result := 'Authentication failed. Check if Windows showed a pairing dialog (may be hidden) or if device requires button press';
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

  // CRITICAL ISSUE: Windows auto-connects to devices at HCI/L2CAP level when they enter pairing mode,
  // even if they're not in the Windows registry. This blocks the authentication dialog from appearing.
  // BluetoothRemoveDevice fails because the device isn't registered (fRemembered=0).
  //
  // SOLUTION: Sacrificial pairing attempt (blocks ~15 sec, fails with ERROR_NOT_AUTHENTICATED) forces
  // Windows to disconnect as a side effect. Then wait 5 seconds for device to fully reset into pairing
  // mode before the real attempt. This allows the toast to appear.
  if DeviceInfo.fConnected then
  begin
    LogWarning('Pair: Device is connected but not authenticated. Using sacrificial attempt to force disconnect...', ClassName);
    if Assigned(AProgressCallback) then
      AProgressCallback('Preparing device for pairing...');

    // Sacrificial pairing attempt - MUST complete naturally (no timeout) to trigger disconnect
    LogDebug('Pair: Sacrificial attempt - calling BluetoothAuthenticateDevice (will block ~15 sec)', ClassName);
    ErrorCode := BluetoothAuthenticateDevice(Application.Handle, 0, @DeviceInfo, nil, 0);
    LogDebug('Pair: Sacrificial attempt completed with code %d', [ErrorCode], ClassName);

    // CRITICAL: Wait 5 seconds for:
    // 1. Device to fully disconnect from Windows
    // 2. Device to reset and re-enter pairing mode properly
    // 3. Windows to clean up connection state
    // Empirical testing shows shorter delays result in Windows reconnecting too quickly.
    LogDebug('Pair: Waiting 5 seconds for device to reset into pairing mode', ClassName);
    Sleep(5000);

    // Re-query device state - should be disconnected
    InitDeviceInfo(DeviceInfo);
    DeviceInfo.Address.ullLong := ADevice.AddressInt;
    ErrorCode := BluetoothGetDeviceInfo(0, DeviceInfo);

    if ErrorCode = ERROR_SUCCESS then
    begin
      LogDebug('Pair: Device state after sacrificial + delay - Connected=%d', [Ord(DeviceInfo.fConnected)], ClassName);
      if DeviceInfo.fConnected then
        LogWarning('Pair: Device still connected after sacrificial attempt, pairing may fail', ClassName)
      else
        LogDebug('Pair: Device successfully disconnected', ClassName);
    end
    else
    begin
      LogDebug('Pair: Device not found after sacrificial attempt (expected)', ClassName);
      InitDeviceInfo(DeviceInfo);
      DeviceInfo.Address.ullLong := ADevice.AddressInt;
    end;

    LogInfo('Pair: Retrying pairing after sacrificial disconnect + delay', ClassName);
  end;

  // Notify progress callback
  if Assigned(AProgressCallback) then
    AProgressCallback('Waiting for pairing confirmation...');

  // Initiate pairing using Secure Simple Pairing (SSP)
  // hwndParent=Application.Handle: Use application window as parent (CRITICAL for dialog signaling)
  // hRadio=0: Use default radio
  // pszPasskey=nil, ulPasskeyLength=0: Use SSP (no PIN required for modern devices)
  //
  // CRITICAL: Do NOT use timeout wrapper for this call. The timeout wrapper creates a nested thread
  // that can hang when user cancels the dialog - Windows returns ERROR_CANCELLED but the wrapper's
  // WaitFor() never unblocks, leaving the main pairing thread stuck forever. Without the wrapper,
  // the API completes naturally (success, cancel, or timeout from Windows) and always returns,
  // ensuring HandlePairingResult is called and FIsPairing flag is cleared.
  //
  // IMPORTANT: For devices discovered via WM_DEVICECHANGE, there's a race condition where
  // BluetoothGetDeviceInfo succeeds but BluetoothAuthenticateDevice returns ERROR_DEVICE_NOT_FOUND.
  // Windows needs time to fully register the device in all Bluetooth subsystems.
  // Retry with delay if we get ERROR_DEVICE_NOT_FOUND on first attempt.

  LogDebug('Pair: Calling BluetoothAuthenticateDevice (no timeout, will block until user responds)', ClassName);
  ErrorCode := BluetoothAuthenticateDevice(Application.Handle, 0, @DeviceInfo, nil, 0);

  // Retry logic for ERROR_DEVICE_NOT_FOUND (race condition)
  // This can occur when device was just discovered but Windows hasn't fully registered it yet
  if ErrorCode = ERROR_DEVICE_NOT_FOUND then
  begin
    LogWarning('Pair: First attempt returned ERROR_DEVICE_NOT_FOUND (race condition), ' +
      'waiting 800ms and retrying', ClassName);
    Sleep(800);

    // Re-query device info to ensure we have latest state
    InitDeviceInfo(DeviceInfo);
    DeviceInfo.Address.ullLong := ADevice.AddressInt;
    BluetoothGetDeviceInfo(0, DeviceInfo);

    LogDebug('Pair: Retry - calling BluetoothAuthenticateDevice again', ClassName);
    ErrorCode := BluetoothAuthenticateDevice(Application.Handle, 0, @DeviceInfo, nil, 0);
  end;

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
        LogError('Pair: Device not found or not in pairing mode (error %d) after retry. ' +
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
  DeviceId: HSTRING;
  DeviceIdStr: string;
  BTDeviceFactory: IInspectable;
  DevInfoFactory: IInspectable;
  BTDeviceStatics: IBluetoothDeviceStatics;
  AsyncBTDeviceOp: IAsyncOperationBluetoothDevice;
  AsyncInfo: IAsyncInfo;
  BTDevice: IBluetoothDevice;
  DevInfoStatics: IDeviceInformationStatics;
  AsyncDevInfoOp: IInspectable;
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

  // Get BluetoothDevice from address using FromBluetoothAddressAsync
  LogDebug('Pair: Getting BluetoothDevice from address $%.12X', [ADevice.AddressInt], ClassName);

  if not GetActivationFactory('Windows.Devices.Bluetooth.BluetoothDevice',
      IBluetoothDeviceStatics, BTDeviceFactory, ClassName) then
  begin
    Result := TPairingResult.Failed(0, 'Failed to get BluetoothDevice factory');
    Exit;
  end;

  HR := BTDeviceFactory.QueryInterface(IBluetoothDeviceStatics, BTDeviceStatics);
  if Failed(HR) or (BTDeviceStatics = nil) then
  begin
    LogError('Pair: QueryInterface for BluetoothDeviceStatics failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to query BluetoothDeviceStatics interface');
    Exit;
  end;

  // Get BluetoothDevice from address
  HR := BTDeviceStatics.FromBluetoothAddressAsync(ADevice.AddressInt, AsyncBTDeviceOp);
  if Failed(HR) or (AsyncBTDeviceOp = nil) then
  begin
    LogError('Pair: FromBluetoothAddressAsync failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get Bluetooth device');
    Exit;
  end;

  // Wait for BluetoothDevice
  if not Supports(AsyncBTDeviceOp, IAsyncInfo, AsyncInfo) then
  begin
    LogError('Pair: Failed to get IAsyncInfo for FromBluetoothAddressAsync', ClassName);
    Result := TPairingResult.Failed(0, 'Internal error: async operation failed');
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, 10000, ClassName) then
  begin
    // Check if it was a timeout or an async error
    var ErrorCode: HRESULT := S_OK;
    var AsyncStatus: Integer := 0;
    AsyncInfo.get_Status(AsyncStatus);
    AsyncInfo.get_ErrorCode(ErrorCode);

    if AsyncStatus = 3 then // AsyncStatus_Error
    begin
      LogError('Pair: FromBluetoothAddressAsync failed with HRESULT 0x%.8X', [ErrorCode], ClassName);
      Result := TPairingResult.Failed(ErrorCode,
        'Device not found. Ensure device is in pairing mode and within range');
      Exit;
    end
    else
    begin
      LogError('Pair: FromBluetoothAddressAsync timeout', ClassName);
      Result := TPairingResult.Timeout;
      Exit;
    end;
  end;

  // Get BluetoothDevice result
  HR := AsyncBTDeviceOp.GetResults(BTDevice);
  if Failed(HR) or (BTDevice = nil) then
  begin
    LogError('Pair: GetResults for BluetoothDevice failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to retrieve Bluetooth device');
    Exit;
  end;

  // Verify we got the correct device by checking its Bluetooth address
  var ReturnedAddress: UInt64 := 0;
  HR := BTDevice.get_BluetoothAddress(ReturnedAddress);
  if Succeeded(HR) then
  begin
    LogDebug('Pair: BluetoothDevice address: $%.12X (requested: $%.12X)',
      [ReturnedAddress, ADevice.AddressInt], ClassName);
    if ReturnedAddress <> ADevice.AddressInt then
      LogWarning('Pair: WARNING - FromBluetoothAddressAsync returned wrong device! Requested $%.12X, got $%.12X',
        [ADevice.AddressInt, ReturnedAddress], ClassName);
  end;

  // Get device ID from BluetoothDevice
  HR := BTDevice.get_DeviceId(DeviceId);
  if Failed(HR) or (DeviceId = 0) then
  begin
    LogError('Pair: get_DeviceId failed: 0x%.8X', [HR], ClassName);
    Result := TPairingResult.Failed(HR, 'Failed to get device ID');
    Exit;
  end;

  DeviceIdStr := HStringToString(DeviceId);
  LogDebug('Pair: Device ID: %s', [DeviceIdStr], ClassName);

  try
    // Get DeviceInformation from device ID
    LogDebug('Pair: Getting DeviceInformation factory', ClassName);
    if not GetActivationFactory('Windows.Devices.Enumeration.DeviceInformation',
        IDeviceInformationStatics, DevInfoFactory, ClassName) then
    begin
      FreeHString(DeviceId);
      LogError('Pair: GetActivationFactory for DeviceInformation failed', ClassName);
      Result := TPairingResult.Failed(0, 'Failed to get DeviceInformation factory');
      Exit;
    end;
    LogDebug('Pair: Got DeviceInformation factory', ClassName);

    HR := DevInfoFactory.QueryInterface(IDeviceInformationStatics, DevInfoStatics);
    if Failed(HR) or (DevInfoStatics = nil) then
    begin
      FreeHString(DeviceId);
      LogError('Pair: QueryInterface for DevInfoStatics failed: 0x%.8X', [HR], ClassName);
      Result := TPairingResult.Failed(HR, 'Failed to query DeviceInformationStatics interface');
      Exit;
    end;
    LogDebug('Pair: Got DeviceInformationStatics interface', ClassName);

    // Create DeviceInformation from device ID
    LogDebug('Pair: Calling CreateFromIdAsync with device ID', ClassName);
    HR := DevInfoStatics.CreateFromIdAsync(DeviceId, AsyncDevInfoOp);
    FreeHString(DeviceId);

    if Failed(HR) or (AsyncDevInfoOp = nil) then
    begin
      LogError('Pair: CreateFromIdAsync failed: 0x%.8X', [HR], ClassName);
      Result := TPairingResult.Failed(HR, 'Failed to get device information');
      Exit;
    end;
    LogDebug('Pair: CreateFromIdAsync succeeded', ClassName);
  except
    on E: Exception do
    begin
      FreeHString(DeviceId);
      LogError('Pair: Exception in DeviceInformation creation: %s', [E.Message], ClassName);
      Result := TPairingResult.Failed(0, 'Exception: ' + E.Message);
      Exit;
    end;
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
    // Check if it was a timeout or an async error
    var ErrorCode: HRESULT := S_OK;
    var AsyncStatus: Integer := 0;
    AsyncInfo.get_Status(AsyncStatus);
    AsyncInfo.get_ErrorCode(ErrorCode);

    if AsyncStatus = 3 then // AsyncStatus_Error
    begin
      LogError('Pair: CreateFromIdAsync failed with HRESULT 0x%.8X', [ErrorCode], ClassName);
      Result := TPairingResult.Failed(ErrorCode,
        'Device not found. Ensure device is in pairing mode and within range');
      Exit;
    end
    else
    begin
      LogError('Pair: CreateFromIdAsync timeout', ClassName);
      Result := TPairingResult.Timeout;
      Exit;
    end;
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
    // Check if it was a timeout or an async error/cancellation
    var ErrorCode: HRESULT := S_OK;
    var AsyncStatus: Integer := 0;
    AsyncInfo.get_Status(AsyncStatus);
    AsyncInfo.get_ErrorCode(ErrorCode);

    if AsyncStatus = 2 then // AsyncStatus_Canceled
    begin
      LogWarning('Pair: Pairing was cancelled by user', ClassName);
      Result := TPairingResult.Cancelled;
      Exit;
    end
    else if AsyncStatus = 3 then // AsyncStatus_Error
    begin
      LogError('Pair: PairAsync failed with HRESULT 0x%.8X', [ErrorCode], ClassName);
      Result := TPairingResult.Failed(ErrorCode, 'Pairing failed');
      Exit;
    end
    else
    begin
      LogWarning('Pair: Pairing timeout after %dms', [TimeoutMs], ClassName);
      Result := TPairingResult.Timeout;
      Exit;
    end;
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
begin
  LogInfo('Unpair: Removing pairing for device $%.12X', [ADeviceAddress], ClassName);

  // WinRT unpair is complex and unreliable (hangs on CreateFromIdAsync on some systems).
  // Use Classic Bluetooth API which works reliably on all Windows versions (7-11).
  LogDebug('Unpair: Using Classic Bluetooth API (BluetoothRemoveDevice)', ClassName);
  Result := UnpairUsingClassicAPI(ADeviceAddress);
end;

function TWinRTSimplePairingStrategy.UnpairUsingClassicAPI(ADeviceAddress: UInt64): TPairingResult;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  DeviceName: string;
  AddressesToUnpair: TList<UInt64>;
  Address: UInt64;
  BtAddress: BLUETOOTH_ADDRESS;
  ErrorCode: DWORD;
  SuccessCount, FailureCount: Integer;
  FirstError: DWORD;
  Found: Boolean;
begin
  LogInfo('UnpairUsingClassicAPI: Unpairing device $%.12X (with TWS multi-address support)', [ADeviceAddress], ClassName);

  AddressesToUnpair := TList<UInt64>.Create;
  try
    try
      // First pass: Find the device name for the given address and collect all addresses with same name
      // This handles TWS (True Wireless Stereo) devices that have multiple BT addresses (left/right earbuds)
      Found := False;
      DeviceName := '';
      InitDeviceSearchParams(SearchParams, 0);
      InitDeviceInfo(DeviceInfo);

      FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);
      if FindHandle <> 0 then
      begin
        try
          repeat
            // Find the target device to get its name
            if DeviceInfo.Address.ullLong = ADeviceAddress then
            begin
              Found := True;
              DeviceName := string(DeviceInfo.szName);
              LogDebug('UnpairUsingClassicAPI: Target device found - Name="%s", fAuthenticated=%d, fRemembered=%d, fConnected=%d',
                [DeviceName, Ord(DeviceInfo.fAuthenticated), Ord(DeviceInfo.fRemembered), Ord(DeviceInfo.fConnected)], ClassName);
            end;

            // Collect all addresses with the same device name (for TWS devices)
            if (DeviceName <> '') and (string(DeviceInfo.szName) = DeviceName) then
            begin
              if not AddressesToUnpair.Contains(DeviceInfo.Address.ullLong) then
              begin
                AddressesToUnpair.Add(DeviceInfo.Address.ullLong);
                LogDebug('UnpairUsingClassicAPI: Found paired address $%.12X for device "%s"',
                  [DeviceInfo.Address.ullLong, DeviceName], ClassName);
              end;
            end;

            DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
          until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
        finally
          BluetoothFindDeviceClose(FindHandle);
        end;
      end;

      if not Found then
      begin
        LogWarning('UnpairUsingClassicAPI: Device $%.12X not found in pairing database, may already be unpaired', [ADeviceAddress], ClassName);
        Result := TPairingResult.Success;  // Consider already unpaired as success
        Exit;
      end;

      if AddressesToUnpair.Count = 0 then
      begin
        LogWarning('UnpairUsingClassicAPI: No addresses found to unpair for device "%s"', [DeviceName], ClassName);
        Result := TPairingResult.Success;  // Nothing to unpair
        Exit;
      end;

      // Log TWS device detection
      if AddressesToUnpair.Count > 1 then
        LogInfo('UnpairUsingClassicAPI: TWS device detected - "%s" has %d paired addresses, will unpair all',
          [DeviceName, AddressesToUnpair.Count], ClassName)
      else
        LogDebug('UnpairUsingClassicAPI: Single-address device "%s"', [DeviceName], ClassName);

      // Unpair all addresses for this device
      SuccessCount := 0;
      FailureCount := 0;
      FirstError := 0;

      for Address in AddressesToUnpair do
      begin
        BtAddress.ullLong := Address;
        ErrorCode := BluetoothRemoveDevice(@BtAddress);

        if ErrorCode = ERROR_SUCCESS then
        begin
          Inc(SuccessCount);
          LogInfo('UnpairUsingClassicAPI: Successfully unpaired address $%.12X', [Address], ClassName);
        end
        else
        begin
          Inc(FailureCount);
          if FirstError = 0 then
            FirstError := ErrorCode;
          LogError('UnpairUsingClassicAPI: Failed to unpair address $%.12X with error %d', [Address, ErrorCode], ClassName);
        end;
      end;

      // Report results
      LogInfo('UnpairUsingClassicAPI: Unpair complete for "%s" - Success=%d, Failed=%d, Total=%d',
        [DeviceName, SuccessCount, FailureCount, AddressesToUnpair.Count], ClassName);

      if FailureCount = 0 then
        Result := TPairingResult.Success
      else if SuccessCount > 0 then
        Result := TPairingResult.Failed(FirstError, Format('Partially unpaired: %d/%d addresses failed', [FailureCount, AddressesToUnpair.Count]))
      else
        Result := TPairingResult.Failed(FirstError, Format('Failed to unpair device (error %d)', [FirstError]));

    except
      on E: Exception do
      begin
        LogError('UnpairUsingClassicAPI: Exception: %s', [E.Message], ClassName);
        Result := TPairingResult.Failed(0, 'Classic unpair failed: ' + E.Message);
      end;
    end;
  finally
    AddressesToUnpair.Free;
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
