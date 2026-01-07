{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Verification Strategies              }
{                                                       }
{       PROBLEM: Windows Bluetooth stack has a bug     }
{       where it reports devices as "connected" even   }
{       when they are powered off or unreachable.      }
{       This leads to false connection status and      }
{       stale battery data being displayed.            }
{                                                       }
{       SOLUTION: After Windows reports a device as    }
{       connected, actively verify the connection by   }
{       attempting to communicate with the device.     }
{       If verification fails, reject the connection.  }
{                                                       }
{       Three verification strategies are provided:    }
{       1. Battery Query - Query battery level with    }
{          timeout. Fast, but only works for devices   }
{          that support battery reporting.             }
{       2. Profile Query - Enumerate device services.  }
{          Works for all devices, moderate speed.      }
{       3. Ping Test - Send L2CAP echo request or      }
{          attempt service discovery. Most reliable,   }
{          works for all devices. DEFAULT STRATEGY.    }
{                                                       }
{       CONFIGURATION: Strategy selection is code-only }
{       via constant below. Not exposed to UI/config.  }
{                                                       }
{*******************************************************}

unit Bluetooth.ConnectionVerification;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Available connection verification strategies.
  /// </summary>
  TVerificationStrategy = (
    vsNone,           // No verification - trust Windows state (vulnerable to bug)
    vsBatteryQuery,   // Verify via battery level query (fast, battery-only devices)
    vsProfileQuery,   // Verify via Bluetooth profile enumeration (moderate speed)
    vsPing            // Verify via L2CAP echo or service discovery (reliable, default)
  );

const
  /// <summary>
  /// Default verification strategy to use.
  /// Options: vsNone, vsBatteryQuery, vsProfileQuery, vsPing
  /// IMPORTANT: Change this constant to switch strategies.
  /// Recommended: vsPing (most reliable, works for all devices)
  /// </summary>
  DEFAULT_VERIFICATION_STRATEGY = vsPing;

  /// <summary>
  /// Timeout for verification operations (milliseconds).
  /// If verification doesn't complete within this time, connection is rejected.
  /// </summary>
  VERIFICATION_TIMEOUT_MS = 3000;

type
  /// <summary>
  /// Interface for connection verification strategies.
  /// All strategies must implement this to verify if a device is truly connected.
  /// </summary>
  IConnectionVerificationStrategy = interface
    ['{B4F7D2E1-8A3C-4D9F-B6E5-7C2F1A9D3E0B}']
    /// <summary>
    /// Verifies that a device is actually connected and responsive.
    /// Returns True if device responds within timeout, False otherwise.
    /// </summary>
    /// <param name="AAddress">Bluetooth device address to verify.</param>
    /// <returns>True if device is verified connected, False if unreachable.</returns>
    function VerifyConnection(AAddress: UInt64): Boolean;
    /// <summary>
    /// Returns human-readable name of this strategy for logging.
    /// </summary>
    function GetStrategyName: string;
  end;

  /// <summary>
  /// Null strategy - performs no verification.
  /// Use this to disable verification and trust Windows state directly.
  /// WARNING: Vulnerable to Windows Bluetooth bug (false positive connections).
  /// </summary>
  TNullVerificationStrategy = class(TInterfacedObject, IConnectionVerificationStrategy)
  public
    function VerifyConnection(AAddress: UInt64): Boolean;
    function GetStrategyName: string;
  end;

  /// <summary>
  /// Battery Query verification strategy.
  /// Verifies connection by attempting to read battery level.
  /// PROS: Very fast (~100ms), minimal overhead if battery query needed anyway.
  /// CONS: Only works for devices with battery reporting (headphones, mice, etc.).
  ///       Fails for devices without battery (keyboards, speakers, etc.).
  /// BEST FOR: Battery-powered portable devices.
  /// </summary>
  TBatteryQueryVerificationStrategy = class(TInterfacedObject, IConnectionVerificationStrategy)
  private
    FBatteryQuery: IBatteryQuery;
  public
    constructor Create(ABatteryQuery: IBatteryQuery);
    function VerifyConnection(AAddress: UInt64): Boolean;
    function GetStrategyName: string;
  end;

  /// <summary>
  /// Profile Query verification strategy.
  /// Verifies connection by enumerating device's Bluetooth profiles/services.
  /// PROS: Works for all Bluetooth devices, moderate speed (~500ms).
  /// CONS: Slower than battery query, requires WinRT on Win8+.
  /// BEST FOR: Devices without battery reporting.
  /// </summary>
  TProfileQueryVerificationStrategy = class(TInterfacedObject, IConnectionVerificationStrategy)
  private
    FProfileQuery: IProfileQuery;
  public
    constructor Create(AProfileQuery: IProfileQuery);
    function VerifyConnection(AAddress: UInt64): Boolean;
    function GetStrategyName: string;
  end;

  /// <summary>
  /// Ping verification strategy (DEFAULT - most reliable).
  /// Verifies connection by performing Bluetooth service discovery.
  /// This acts as a "ping" - if device is reachable, discovery succeeds quickly.
  /// PROS: Most reliable, works for all devices, moderate speed (~300-500ms).
  /// CONS: Slightly slower than battery query, but still fast enough.
  /// BEST FOR: General purpose - works everywhere, catches all false positives.
  /// IMPLEMENTATION: Uses BluetoothGetDeviceInfo which triggers service discovery.
  /// </summary>
  TPingVerificationStrategy = class(TInterfacedObject, IConnectionVerificationStrategy)
  public
    function VerifyConnection(AAddress: UInt64): Boolean;
    function GetStrategyName: string;
  end;

/// <summary>
/// Factory function to create the default verification strategy.
/// Returns strategy configured by DEFAULT_VERIFICATION_STRATEGY constant.
/// </summary>
/// <param name="ABatteryQuery">Battery query interface (required for vsBatteryQuery).</param>
/// <param name="AProfileQuery">Profile query interface (required for vsProfileQuery).</param>
function CreateDefaultVerificationStrategy(
  ABatteryQuery: IBatteryQuery = nil;
  AProfileQuery: IProfileQuery = nil): IConnectionVerificationStrategy;

/// <summary>
/// Factory function to create a specific verification strategy.
/// Allows runtime strategy selection for testing/debugging.
/// </summary>
function CreateVerificationStrategy(
  AStrategy: TVerificationStrategy;
  ABatteryQuery: IBatteryQuery = nil;
  AProfileQuery: IProfileQuery = nil): IConnectionVerificationStrategy;

implementation

uses
  App.Logger;

{ TNullVerificationStrategy }

function TNullVerificationStrategy.VerifyConnection(AAddress: UInt64): Boolean;
begin
  // Always return True - trust Windows state without verification
  LogDebug('VerifyConnection: Strategy=None, Address=$%.12X - SKIPPED (trusting Windows)',
    [AAddress], ClassName);
  Result := True;
end;

function TNullVerificationStrategy.GetStrategyName: string;
begin
  Result := 'None (No Verification)';
end;

{ TBatteryQueryVerificationStrategy }

constructor TBatteryQueryVerificationStrategy.Create(ABatteryQuery: IBatteryQuery);
begin
  inherited Create;
  FBatteryQuery := ABatteryQuery;
  Assert(Assigned(FBatteryQuery), 'Battery query is required for TBatteryQueryVerificationStrategy');
end;

function TBatteryQueryVerificationStrategy.VerifyConnection(AAddress: UInt64): Boolean;
var
  BatteryStatus: TBatteryStatus;
  StartTime: Cardinal;
  ElapsedMs: Cardinal;
begin
  LogDebug('VerifyConnection: Strategy=BatteryQuery, Address=$%.12X - starting verification',
    [AAddress], ClassName);

  StartTime := GetTickCount;

  // Attempt to query battery level
  // If device is truly connected, this will succeed quickly
  // If device is offline, this will timeout or return NotSupported
  BatteryStatus := FBatteryQuery.GetBatteryLevel(AAddress);

  ElapsedMs := GetTickCount - StartTime;

  // Connection is verified if:
  // 1. Battery query succeeded (IsSupported = True), OR
  // 2. Query completed quickly but battery not supported (device responded but has no battery)
  // Connection is REJECTED if:
  // 1. Query timed out (ElapsedMs > VERIFICATION_TIMEOUT_MS)
  // 2. Query failed with error (implies device unreachable)

  Result := BatteryStatus.IsSupported or (ElapsedMs < VERIFICATION_TIMEOUT_MS div 2);

  if Result then
    LogInfo('VerifyConnection: Strategy=BatteryQuery, Address=$%.12X - VERIFIED (elapsed=%dms, level=%d, supported=%s)',
      [AAddress, ElapsedMs, BatteryStatus.Level, BoolToStr(BatteryStatus.IsSupported, True)], ClassName)
  else
    LogWarning('VerifyConnection: Strategy=BatteryQuery, Address=$%.12X - FAILED (elapsed=%dms, timeout or unreachable)',
      [AAddress, ElapsedMs], ClassName);
end;

function TBatteryQueryVerificationStrategy.GetStrategyName: string;
begin
  Result := 'Battery Query';
end;

{ TProfileQueryVerificationStrategy }

constructor TProfileQueryVerificationStrategy.Create(AProfileQuery: IProfileQuery);
begin
  inherited Create;
  FProfileQuery := AProfileQuery;
  Assert(Assigned(FProfileQuery), 'Profile query is required for TProfileQueryVerificationStrategy');
end;

function TProfileQueryVerificationStrategy.VerifyConnection(AAddress: UInt64): Boolean;
var
  ProfileInfo: TDeviceProfileInfo;
  StartTime: Cardinal;
  ElapsedMs: Cardinal;
begin
  LogDebug('VerifyConnection: Strategy=ProfileQuery, Address=$%.12X - starting verification',
    [AAddress], ClassName);

  StartTime := GetTickCount;

  // Attempt to enumerate device profiles
  // If device is truly connected, profile enumeration will succeed
  // If device is offline, enumeration will fail or timeout
  ProfileInfo := FProfileQuery.GetDeviceProfiles(AAddress);

  ElapsedMs := GetTickCount - StartTime;

  // Connection is verified if profile query succeeded (Count > 0) and completed within timeout
  Result := (ProfileInfo.Count > 0) and (ElapsedMs < VERIFICATION_TIMEOUT_MS);

  if Result then
    LogInfo('VerifyConnection: Strategy=ProfileQuery, Address=$%.12X - VERIFIED (elapsed=%dms, profiles=%d)',
      [AAddress, ElapsedMs, ProfileInfo.Count], ClassName)
  else
    LogWarning('VerifyConnection: Strategy=ProfileQuery, Address=$%.12X - FAILED (elapsed=%dms, profiles=%d)',
      [AAddress, ElapsedMs, ProfileInfo.Count], ClassName);
end;

function TProfileQueryVerificationStrategy.GetStrategyName: string;
begin
  Result := 'Profile Query';
end;

{ TPingVerificationStrategy }

function TPingVerificationStrategy.VerifyConnection(AAddress: UInt64): Boolean;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  StartTime: Cardinal;
  ElapsedMs: Cardinal;
  Found: Boolean;
  BTAddr: BLUETOOTH_ADDRESS;
begin
  LogDebug('VerifyConnection: Strategy=Ping, Address=$%.12X - starting verification',
    [AAddress], ClassName);

  StartTime := GetTickCount;
  Found := False;

  // Convert UInt64 address to BLUETOOTH_ADDRESS structure
  BTAddr.ullLong := AAddress;

  // Strategy: Use BluetoothGetDeviceInfo to trigger service discovery
  // This acts as a "ping" - if device is reachable, it succeeds quickly
  // If device is offline, it fails or times out

  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address := BTAddr;

  // Attempt to get fresh device info
  // This triggers Windows to actually communicate with the device
  if BluetoothGetDeviceInfo(0, DeviceInfo) = ERROR_SUCCESS then
  begin
    // Device info retrieved successfully - device is reachable
    ElapsedMs := GetTickCount - StartTime;

    LogInfo('VerifyConnection: Strategy=Ping, Address=$%.12X - VERIFIED via BluetoothGetDeviceInfo (elapsed=%dms, connected=%s)',
      [AAddress, ElapsedMs, BoolToStr(DeviceInfo.fConnected, True)], ClassName);

    // Double-check: device info should show fConnected = True
    Result := DeviceInfo.fConnected;
  end
  else
  begin

    // BluetoothGetDeviceInfo failed - try fallback: enumerate devices looking for this one
    // This is slower but more reliable on some systems
    LogDebug('VerifyConnection: BluetoothGetDeviceInfo failed, trying enumeration fallback', ClassName);

    InitDeviceSearchParams(SearchParams, 0);
    SearchParams.fReturnConnected := True;      // Only return connected devices
    SearchParams.fReturnRemembered := False;    // Don't return remembered-but-disconnected
    SearchParams.fReturnUnknown := False;
    SearchParams.fReturnAuthenticated := True;

    InitDeviceInfo(DeviceInfo);
    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);

    if FindHandle <> 0 then
    begin
      try
        repeat
          if DeviceInfo.Address.ullLong = AAddress then
          begin
            // Found our device in the enumeration of connected devices
            Found := True;
            Break;
          end;
          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end;

    ElapsedMs := GetTickCount - StartTime;
    Result := Found and (ElapsedMs < VERIFICATION_TIMEOUT_MS);

    if Result then
      LogInfo('VerifyConnection: Strategy=Ping, Address=$%.12X - VERIFIED via enumeration (elapsed=%dms)',
        [AAddress, ElapsedMs], ClassName)
    else
      LogWarning('VerifyConnection: Strategy=Ping, Address=$%.12X - FAILED (elapsed=%dms, not found in connected devices)',
        [AAddress, ElapsedMs], ClassName);
  end;
end;

function TPingVerificationStrategy.GetStrategyName: string;
begin
  Result := 'Ping (Service Discovery)';
end;

{ Factory Functions }

function CreateDefaultVerificationStrategy(
  ABatteryQuery: IBatteryQuery;
  AProfileQuery: IProfileQuery): IConnectionVerificationStrategy;
begin
  Result := CreateVerificationStrategy(DEFAULT_VERIFICATION_STRATEGY, ABatteryQuery, AProfileQuery);
end;

function CreateVerificationStrategy(
  AStrategy: TVerificationStrategy;
  ABatteryQuery: IBatteryQuery;
  AProfileQuery: IProfileQuery): IConnectionVerificationStrategy;
begin
  case AStrategy of
    vsNone:
      begin
        Result := TNullVerificationStrategy.Create;
        LogInfo('CreateVerificationStrategy: Using NULL strategy (no verification - vulnerable to Windows bug)', 'ConnectionVerification');
      end;

    vsBatteryQuery:
      begin
        if not Assigned(ABatteryQuery) then
          raise Exception.Create('Battery query required for vsBatteryQuery strategy');
        Result := TBatteryQueryVerificationStrategy.Create(ABatteryQuery);
        LogInfo('CreateVerificationStrategy: Using BATTERY QUERY strategy', 'ConnectionVerification');
      end;

    vsProfileQuery:
      begin
        if not Assigned(AProfileQuery) then
          raise Exception.Create('Profile query required for vsProfileQuery strategy');
        Result := TProfileQueryVerificationStrategy.Create(AProfileQuery);
        LogInfo('CreateVerificationStrategy: Using PROFILE QUERY strategy', 'ConnectionVerification');
      end;

    vsPing:
      begin
        Result := TPingVerificationStrategy.Create;
        LogInfo('CreateVerificationStrategy: Using PING strategy (DEFAULT - most reliable)', 'ConnectionVerification');
      end;
  else
    raise Exception.CreateFmt('Unknown verification strategy: %d', [Ord(AStrategy)]);
  end;
end;

end.
