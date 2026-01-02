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
  App.ConfigEnums;

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
  /// WinRT Bluetooth pairing strategy (future enhancement).
  /// Uses Windows.Devices.Enumeration DeviceInformation.Pairing.PairAsync.
  /// Currently returns NotSupported - stub for future implementation.
  /// </summary>
  TWinRTPairingStrategy = class(TInterfacedObject, IPairingStrategy)
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

    /// <summary>
    /// Registers default strategies (Windows, WinRT).
    /// </summary>
    procedure RegisterDefaultStrategies;
  public
    constructor Create;
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
function CreatePairingStrategyFactory: IPairingStrategyFactory;

implementation

uses
  App.Logger;

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
    LogError('Pair: BluetoothGetDeviceInfo failed with error %d', [ErrorCode], ClassName);
    Result := TPairingResult.Failed(ErrorCode, GetErrorMessage(ErrorCode));
    Exit;
  end;

  // Check if already paired
  if DeviceInfo.fRemembered then
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

  else
    begin
      LogError('Pair: Pairing failed with error %d', [ErrorCode], ClassName);
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

{ TWinRTPairingStrategy }

function TWinRTPairingStrategy.Pair(
  const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback
): TPairingResult;
begin
  LogInfo('Pair: WinRT pairing not yet implemented', ClassName);
  Result := TPairingResult.NotSupported('WinRT pairing will be implemented in future version');
end;

function TWinRTPairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
begin
  LogInfo('Unpair: WinRT pairing not yet implemented', ClassName);
  Result := TPairingResult.NotSupported('WinRT pairing will be implemented in future version');
end;

function TWinRTPairingStrategy.CanHandle(APlatform: TBluetoothPlatform): Boolean;
begin
  // Will support WinRT platform when implemented
  Result := APlatform = bpWinRT;
end;

function TWinRTPairingStrategy.GetName: string;
begin
  Result := 'WinRT Pairing (Not Implemented)';
end;

function TWinRTPairingStrategy.GetPriority: Integer;
begin
  // Higher priority than Windows Classic, but not yet functional
  Result := 100;
end;

{ TPairingStrategyFactory }

constructor TPairingStrategyFactory.Create;
begin
  inherited Create;
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

  // Register WinRT strategy (stub for future)
  RegisterStrategy(TWinRTPairingStrategy.Create);

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

function CreatePairingStrategyFactory: IPairingStrategyFactory;
begin
  Result := TPairingStrategyFactory.Create;
end;

end.
