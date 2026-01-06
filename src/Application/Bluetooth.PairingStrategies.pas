{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Pairing Strategy Pattern Implementation         }
{                                                       }
{       Provides extensible pairing strategies for      }
{       Windows Bluetooth pairing operations.           }
{       Currently implements only Classic Bluetooth     }
{       pairing (Win32 API). Follows same pattern as    }
{       connection strategies.                          }
{                                                       }
{       IMPORTANT: WinRT Pairing Not Supported          }
{       See detailed explanation below.                 }
{                                                       }
{*******************************************************}

{===============================================================================
  WHY WINRT PAIRING IS NOT SUPPORTED - ARCHITECTURAL ANALYSIS
===============================================================================

  This unit previously contained WinRT-based pairing strategies using
  Windows.Devices.Bluetooth custom pairing APIs. These were removed because
  they were fundamentally non-functional due to an architectural mismatch
  between device discovery and pairing subsystems.

===============================================================================}

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
  /// Currently only implemented by TWindowsPairingStrategy (Classic Bluetooth).
  /// Designed to allow different pairing approaches, but WinRT pairing was
  /// removed due to architectural incompatibility (see unit header for details).
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
  /// Delegates pairing to Windows system dialogs, provides SSP support.
  ///
  /// This is currently the ONLY pairing strategy used by the application.
  /// Works reliably with devices discovered via Classic Bluetooth monitoring.
  /// WinRT pairing strategies were removed - see unit header for explanation.
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
  /// Pairing strategy factory implementation.
  /// Manages strategy registration and selection.
  /// </summary>
  TPairingStrategyFactory = class(TInterfacedObject, IPairingStrategyFactory)
  private
    FStrategies: TList<IPairingStrategy>;
    FConnectionConfig: IConnectionConfig;

    /// <summary>
    /// Registers default strategies.
    /// Currently only registers Windows Classic Bluetooth pairing.
    /// See unit header comment for explanation of why WinRT pairing is not supported.
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
  App.Config;

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
  RegisterStrategy(TWindowsPairingStrategy.Create);

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
