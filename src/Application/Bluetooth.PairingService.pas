{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Pairing Service Implementation                  }
{                                                       }
{       Wraps pairing strategies and provides pairing   }
{       state query capabilities. Service layer for     }
{       pairing operations.                             }
{                                                       }
{*******************************************************}

unit Bluetooth.PairingService;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.PairingStrategies,
  App.ConfigEnums,
  App.ConnectionConfigIntf;

type
  /// <summary>
  /// Pairing service implementation.
  /// Wraps pairing strategy pattern and provides pairing state queries.
  /// </summary>
  TBluetoothPairingService = class(TInterfacedObject, IBluetoothPairingService)
  private
    FPairingStrategyFactory: IPairingStrategyFactory;
    FDeviceRepository: IDeviceRepository;
    FConnectionConfig: IConnectionConfig;

  public
    /// <summary>
    /// Creates pairing service with injected dependencies.
    /// </summary>
    /// <param name="APairingStrategyFactory">Factory for pairing strategies.</param>
    /// <param name="ADeviceRepository">Repository for device query (can be nil).</param>
    /// <param name="AConnectionConfig">Connection configuration (can be nil, defaults to bpAuto).</param>
    constructor Create(
      APairingStrategyFactory: IPairingStrategyFactory;
      ADeviceRepository: IDeviceRepository;
      AConnectionConfig: IConnectionConfig
    );

    /// <summary>
    /// Pairs device using appropriate strategy for current platform.
    /// </summary>
    function PairDevice(
      const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback
    ): TPairingResult;

    /// <summary>
    /// Unpairs device using appropriate strategy.
    /// </summary>
    function UnpairDevice(ADeviceAddress: UInt64): TPairingResult;

    /// <summary>
    /// Checks if device is paired by querying Windows API directly.
    /// </summary>
    function IsDevicePaired(ADeviceAddress: UInt64): Boolean;

    /// <summary>
    /// Enumerates all paired device addresses from Windows.
    /// </summary>
    function GetPairedDeviceAddresses: TArray<UInt64>;
  end;

/// <summary>
/// Creates a pairing service instance.
/// </summary>
/// <param name="APairingStrategyFactory">Factory for pairing strategies.</param>
/// <param name="ADeviceRepository">Device repository (optional).</param>
/// <param name="AConnectionConfig">Connection configuration (optional).</param>
/// <returns>Pairing service instance.</returns>
function CreatePairingService(
  APairingStrategyFactory: IPairingStrategyFactory;
  ADeviceRepository: IDeviceRepository;
  AConnectionConfig: IConnectionConfig
): IBluetoothPairingService;

implementation

uses
  App.Logger;

{ TBluetoothPairingService }

constructor TBluetoothPairingService.Create(
  APairingStrategyFactory: IPairingStrategyFactory;
  ADeviceRepository: IDeviceRepository;
  AConnectionConfig: IConnectionConfig
);
begin
  inherited Create;

  if not Assigned(APairingStrategyFactory) then
    raise EArgumentNilException.Create('PairingStrategyFactory cannot be nil');

  FPairingStrategyFactory := APairingStrategyFactory;
  FDeviceRepository := ADeviceRepository;
  FConnectionConfig := AConnectionConfig;

  LogDebug('Create: Pairing service initialized', ClassName);
end;

function TBluetoothPairingService.PairDevice(
  const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback
): TPairingResult;
var
  Strategy: IPairingStrategy;
begin
  LogInfo('PairDevice: Initiating pairing for device $%.12X, Name="%s"',
    [ADevice.AddressInt, ADevice.Name], ClassName);

  // Get appropriate strategy
  Strategy := FPairingStrategyFactory.GetStrategy;

  if not Assigned(Strategy) then
  begin
    LogError('PairDevice: No pairing strategy available', ClassName);
    Result := TPairingResult.NotSupported('No pairing strategy available');
    Exit;
  end;

  LogDebug('PairDevice: Using strategy "%s"', [Strategy.GetName], ClassName);

  // Delegate to strategy
  Result := Strategy.Pair(ADevice, AProgressCallback);

  // Log result
  case Result.Status of
    prsSuccess:
      LogInfo('PairDevice: Pairing succeeded', ClassName);
    prsCancelled:
      LogInfo('PairDevice: Pairing cancelled', ClassName);
    prsFailed:
      LogError('PairDevice: Pairing failed - %s (code %d)',
        [Result.ErrorMessage, Result.ErrorCode], ClassName);
    prsAlreadyPaired:
      LogInfo('PairDevice: Device already paired', ClassName);
    prsTimeout:
      LogWarning('PairDevice: Pairing timed out', ClassName);
    prsNotSupported:
      LogWarning('PairDevice: Pairing not supported - %s', [Result.ErrorMessage], ClassName);
  end;
end;

function TBluetoothPairingService.UnpairDevice(ADeviceAddress: UInt64): TPairingResult;
var
  Strategy: IPairingStrategy;
begin
  LogInfo('UnpairDevice: Removing pairing for device $%.12X', [ADeviceAddress], ClassName);

  // Get appropriate strategy
  Strategy := FPairingStrategyFactory.GetStrategy;

  if not Assigned(Strategy) then
  begin
    LogError('UnpairDevice: No pairing strategy available', ClassName);
    Result := TPairingResult.NotSupported('No pairing strategy available');
    Exit;
  end;

  LogDebug('UnpairDevice: Using strategy "%s"', [Strategy.GetName], ClassName);

  // Delegate to strategy
  Result := Strategy.Unpair(ADeviceAddress);

  // Log result
  if Result.IsSuccess then
    LogInfo('UnpairDevice: Device unpaired successfully', ClassName)
  else
    LogError('UnpairDevice: Failed - %s (code %d)',
      [Result.ErrorMessage, Result.ErrorCode], ClassName);
end;

function TBluetoothPairingService.IsDevicePaired(ADeviceAddress: UInt64): Boolean;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ErrorCode: DWORD;
begin
  LogDebug('IsDevicePaired: Checking pairing status for device $%.12X', [ADeviceAddress], ClassName);

  // Initialize device info structure
  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address.ullLong := ADeviceAddress;

  // Query device information from Windows
  ErrorCode := BluetoothGetDeviceInfo(0, DeviceInfo);

  if ErrorCode = ERROR_SUCCESS then
  begin
    Result := DeviceInfo.fAuthenticated;  // Use fAuthenticated (true pairing), not fRemembered (just known)
    if Result then
      LogDebug('IsDevicePaired: Device $%.12X is paired', [ADeviceAddress], ClassName)
    else
      LogDebug('IsDevicePaired: Device $%.12X is NOT paired', [ADeviceAddress], ClassName);
  end
  else
  begin
    // Device not found or error
    LogDebug('IsDevicePaired: BluetoothGetDeviceInfo failed with error %d (device likely not paired)',
      [ErrorCode], ClassName);
    Result := False;
  end;
end;

function TBluetoothPairingService.GetPairedDeviceAddresses: TArray<UInt64>;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  AddressList: TList<UInt64>;
begin
  LogDebug('GetPairedDeviceAddresses: Enumerating paired devices', ClassName);

  AddressList := TList<UInt64>.Create;
  try
    // Initialize search parameters for paired devices only
    InitDeviceSearchParams(SearchParams, 0);

    // Initialize device info structure
    InitDeviceInfo(DeviceInfo);

    // Start device enumeration
    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);

    if FindHandle <> 0 then
    begin
      try
        repeat
          // Only include truly paired devices (fAuthenticated), not just remembered ones
          if DeviceInfo.fAuthenticated then
          begin
            AddressList.Add(DeviceInfo.Address.ullLong);
            LogDebug('GetPairedDeviceAddresses: Found paired device $%.12X',
              [DeviceInfo.Address.ullLong], ClassName);
          end;

          // Prepare for next iteration
          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end
    else
      LogDebug('GetPairedDeviceAddresses: No devices found or enumeration failed (error=%d)',
        [GetLastError], ClassName);

    Result := AddressList.ToArray;
    LogInfo('GetPairedDeviceAddresses: Found %d paired devices', [Length(Result)], ClassName);
  finally
    AddressList.Free;
  end;
end;

function CreatePairingService(
  APairingStrategyFactory: IPairingStrategyFactory;
  ADeviceRepository: IDeviceRepository;
  AConnectionConfig: IConnectionConfig
): IBluetoothPairingService;
begin
  Result := TBluetoothPairingService.Create(
    APairingStrategyFactory,
    ADeviceRepository,
    AConnectionConfig
  );
end;

end.
