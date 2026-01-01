{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Domain Interfaces (Abstractions)                }
{                                                       }
{*******************************************************}

unit Bluetooth.Interfaces;

interface

uses
  System.SysUtils,
  Bluetooth.Types;

type
  /// <summary>
  /// Event handler for device connection state changes.
  /// </summary>
  TDeviceStateChangedEvent = procedure(
    Sender: TObject;
    const ADevice: TBluetoothDeviceInfo
  ) of object;

  /// <summary>
  /// Event handler for device list changes.
  /// </summary>
  TDeviceListChangedEvent = procedure(Sender: TObject) of object;

  /// <summary>
  /// Event handler for discovered unpaired devices.
  /// Fired when an unpaired device comes into Bluetooth range.
  /// </summary>
  TDeviceDiscoveredEvent = procedure(
    Sender: TObject;
    const ADevice: TBluetoothDeviceInfo
  ) of object;

  /// <summary>
  /// Event handler for device out of range.
  /// Fired when a device leaves Bluetooth range.
  /// </summary>
  TDeviceOutOfRangeEvent = procedure(
    Sender: TObject;
    const ADeviceAddress: UInt64
  ) of object;

  /// <summary>
  /// Event handler for errors.
  /// </summary>
  TBluetoothErrorEvent = procedure(
    Sender: TObject;
    const AMessage: string;
    AErrorCode: Cardinal
  ) of object;

  /// <summary>
  /// Interface for querying Bluetooth adapter availability.
  /// Single Responsibility: Only checks adapter presence.
  /// </summary>
  IBluetoothAdapterQuery = interface
    ['{A1B2C3D4-1111-2222-3333-444455556666}']

    /// <summary>
    /// Checks if a Bluetooth adapter is available.
    /// </summary>
    /// <returns>True if at least one adapter is present.</returns>
    function IsAdapterAvailable: Boolean;

    /// <summary>
    /// Gets the name of the primary Bluetooth adapter.
    /// </summary>
    /// <returns>Adapter name or empty string if not available.</returns>
    function GetAdapterName: string;
  end;

  /// <summary>
  /// Interface for enumerating Bluetooth devices.
  /// Single Responsibility: Only enumerates devices.
  /// </summary>
  IBluetoothDeviceEnumerator = interface
    ['{A1B2C3D4-2222-3333-4444-555566667777}']

    /// <summary>
    /// Gets all paired (remembered) Bluetooth devices.
    /// </summary>
    /// <returns>Array of device information records.</returns>
    function GetPairedDevices: TBluetoothDeviceInfoArray;

    /// <summary>
    /// Refreshes the connection status of a specific device.
    /// </summary>
    /// <param name="ADevice">The device to refresh.</param>
    /// <returns>Updated device information.</returns>
    function RefreshDeviceStatus(const ADevice: TBluetoothDeviceInfo): TBluetoothDeviceInfo;
  end;

  /// <summary>
  /// Interface for connecting/disconnecting Bluetooth devices.
  /// Single Responsibility: Only handles connection operations.
  /// </summary>
  IBluetoothConnectionManager = interface
    ['{A1B2C3D4-3333-4444-5555-666677778888}']

    /// <summary>
    /// Connects to a Bluetooth device.
    /// </summary>
    /// <param name="ADevice">The device to connect to.</param>
    /// <returns>True if connection was successful.</returns>
    function Connect(const ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Disconnects from a Bluetooth device.
    /// </summary>
    /// <param name="ADevice">The device to disconnect from.</param>
    /// <returns>True if disconnection was successful.</returns>
    function Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Toggles the connection state of a device.
    /// </summary>
    /// <param name="ADevice">The device to toggle.</param>
    /// <returns>True if operation was successful.</returns>
    function ToggleConnection(const ADevice: TBluetoothDeviceInfo): Boolean;
  end;

  /// <summary>
  /// Facade interface combining all Bluetooth operations.
  /// Note: TBluetoothService also implements IBluetoothDeviceEnumerator and
  /// IBluetoothConnectionManager for clients that need only specific functionality.
  /// </summary>
  IBluetoothService = interface
    ['{A1B2C3D4-4444-5555-6666-777788889999}']

    /// <summary>
    /// Checks if a Bluetooth adapter is available.
    /// </summary>
    function IsAdapterAvailable: Boolean;

    /// <summary>
    /// Gets all paired Bluetooth devices.
    /// </summary>
    function GetPairedDevices: TBluetoothDeviceInfoArray;

    /// <summary>
    /// Refreshes the status of all paired devices.
    /// </summary>
    function RefreshAllDevices: TBluetoothDeviceInfoArray;

    /// <summary>
    /// Connects to a device.
    /// </summary>
    function Connect(const ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Disconnects from a device.
    /// </summary>
    function Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Toggles device connection.
    /// </summary>
    function ToggleConnection(const ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Gets/sets the handler for device state changes.
    /// </summary>
    function GetOnDeviceStateChanged: TDeviceStateChangedEvent;
    procedure SetOnDeviceStateChanged(AValue: TDeviceStateChangedEvent);
    property OnDeviceStateChanged: TDeviceStateChangedEvent
      read GetOnDeviceStateChanged write SetOnDeviceStateChanged;

    /// <summary>
    /// Gets/sets the handler for device list changes.
    /// </summary>
    function GetOnDeviceListChanged: TDeviceListChangedEvent;
    procedure SetOnDeviceListChanged(AValue: TDeviceListChangedEvent);
    property OnDeviceListChanged: TDeviceListChangedEvent
      read GetOnDeviceListChanged write SetOnDeviceListChanged;

    /// <summary>
    /// Gets/sets the handler for discovered unpaired devices.
    /// </summary>
    function GetOnDeviceDiscovered: TDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TDeviceDiscoveredEvent);
    property OnDeviceDiscovered: TDeviceDiscoveredEvent
      read GetOnDeviceDiscovered write SetOnDeviceDiscovered;

    /// <summary>
    /// Gets/sets the handler for device out of range.
    /// </summary>
    function GetOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TDeviceOutOfRangeEvent);
    property OnDeviceOutOfRange: TDeviceOutOfRangeEvent
      read GetOnDeviceOutOfRange write SetOnDeviceOutOfRange;

    /// <summary>
    /// Gets/sets the handler for errors.
    /// </summary>
    function GetOnError: TBluetoothErrorEvent;
    procedure SetOnError(AValue: TBluetoothErrorEvent);
    property OnError: TBluetoothErrorEvent
      read GetOnError write SetOnError;

    /// <summary>
    /// Triggers an active device discovery scan.
    /// Restarts the device watcher to refresh discovered devices.
    /// </summary>
    procedure TriggerDiscoveryScan;

    /// <summary>
    /// Performs an active Bluetooth inquiry to scan for nearby unpaired devices.
    /// Fires OnDeviceDiscovered for each device found.
    /// This is a blocking operation that can take several seconds.
    /// </summary>
    procedure ScanForNearbyDevices;
  end;

  /// <summary>
  /// Strategy interface for device-specific connection logic.
  /// Implements Strategy Pattern for different device types.
  /// </summary>
  IConnectionStrategy = interface
    ['{A1B2C3D4-5555-6666-7777-88889999AAAA}']

    /// <summary>
    /// Checks if this strategy can handle the given device type.
    /// </summary>
    /// <param name="ADeviceType">The device type to check.</param>
    /// <returns>True if this strategy supports the device type.</returns>
    function CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;

    /// <summary>
    /// Gets the service GUIDs to use for connection.
    /// </summary>
    /// <returns>Array of service GUIDs.</returns>
    function GetServiceGuids: TArray<TGUID>;

    /// <summary>
    /// Gets the priority of this strategy (higher = preferred).
    /// </summary>
    /// <returns>Priority value.</returns>
    function GetPriority: Integer;
  end;

  /// <summary>
  /// Event handler for device monitor state changes.
  /// Reports device address and new connection state.
  /// </summary>
  TMonitorDeviceStateEvent = procedure(
    Sender: TObject;
    const ADeviceAddress: UInt64;
    ANewState: TBluetoothConnectionState
  ) of object;

  /// <summary>
  /// Event handler for device monitor errors.
  /// </summary>
  TMonitorErrorEvent = procedure(
    Sender: TObject;
    const AMessage: string;
    AErrorCode: Cardinal
  ) of object;

  /// <summary>
  /// Event handler for monitor discovering unpaired devices.
  /// Fired when the monitor detects an unpaired device in Bluetooth range.
  /// </summary>
  TMonitorDeviceDiscoveredEvent = procedure(
    Sender: TObject;
    const ADevice: TBluetoothDeviceInfo
  ) of object;

  /// <summary>
  /// Event handler for device out of range.
  /// Fired when the monitor detects that a device has left Bluetooth range.
  /// </summary>
  TMonitorDeviceOutOfRangeEvent = procedure(
    Sender: TObject;
    const ADeviceAddress: UInt64
  ) of object;

  /// <summary>
  /// Interface for monitoring device connection state changes.
  /// Abstracts different monitoring mechanisms (watcher, polling).
  /// </summary>
  IDeviceMonitor = interface
    ['{C3D4E5F6-7777-8888-9999-AAAA0000CCCC}']

    /// <summary>
    /// Starts monitoring for device state changes.
    /// </summary>
    /// <returns>True if monitoring started successfully.</returns>
    function Start: Boolean;

    /// <summary>
    /// Stops monitoring.
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Checks if the monitor is currently running.
    /// </summary>
    function IsRunning: Boolean;

    /// <summary>
    /// Gets/sets the handler for discovered devices.
    /// </summary>
    function GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);

    /// <summary>
    /// Gets/sets the handler for device state changes.
    /// </summary>
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    property OnDeviceStateChanged: TMonitorDeviceStateEvent
      read GetOnDeviceStateChanged write SetOnDeviceStateChanged;

    /// <summary>
    /// Gets/sets the handler for device out of range.
    /// </summary>
    function GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);
    property OnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent
      read GetOnDeviceOutOfRange write SetOnDeviceOutOfRange;

    /// <summary>
    /// Gets/sets the handler for errors.
    /// </summary>
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);
    property OnError: TMonitorErrorEvent
      read GetOnError write SetOnError;
  end;

  /// <summary>
  /// Factory interface for creating device monitors.
  /// Enables dependency injection and different monitoring strategies.
  /// </summary>
  IDeviceMonitorFactory = interface
    ['{D4E5F6A7-8888-9999-AAAA-BBBB0000DDDD}']

    /// <summary>
    /// Creates a device monitor based on configuration.
    /// </summary>
    /// <returns>Configured device monitor instance.</returns>
    function CreateMonitor: IDeviceMonitor;
  end;

  /// <summary>
  /// Factory interface for creating and managing connection strategies.
  /// Enables dependency injection and testing with mock strategies.
  /// </summary>
  IConnectionStrategyFactory = interface
    ['{B2C3D4E5-6666-7777-8888-99990000BBBB}']

    /// <summary>
    /// Gets the appropriate strategy for a device type.
    /// Returns the highest priority strategy that can handle the device type.
    /// </summary>
    /// <param name="ADeviceType">The device type.</param>
    /// <returns>Best matching strategy, or nil if none found.</returns>
    function GetStrategy(ADeviceType: TBluetoothDeviceType): IConnectionStrategy;

    /// <summary>
    /// Registers a custom strategy.
    /// </summary>
    /// <param name="AStrategy">The strategy to register.</param>
    procedure RegisterStrategy(AStrategy: IConnectionStrategy);

    /// <summary>
    /// Gets all registered strategies.
    /// </summary>
    /// <returns>Array of all strategies.</returns>
    function GetAllStrategies: TArray<IConnectionStrategy>;

    /// <summary>
    /// Clears all registered strategies.
    /// Useful for testing.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  /// Low-level interface for querying paired Bluetooth devices from Windows API.
  /// Used by IDeviceRepository to enumerate devices.
  /// Separated for testability - allows mocking Windows API calls.
  /// </summary>
  IBluetoothDeviceQuery = interface
    ['{B2C3D4E5-AAAA-BBBB-CCCC-DDDD11112222}']

    /// <summary>
    /// Enumerates all paired Bluetooth devices from Windows.
    /// </summary>
    /// <returns>Array of device information records.</returns>
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;
  end;

  /// <summary>
  /// Repository interface for device storage and retrieval.
  /// Centralizes device cache management and enumeration.
  /// </summary>
  IDeviceRepository = interface
    ['{E5F6A7B8-9999-AAAA-BBBB-CCCC0000EEEE}']

    /// <summary>
    /// Gets all cached devices.
    /// </summary>
    function GetAll: TBluetoothDeviceInfoArray;

    /// <summary>
    /// Gets a device by its address.
    /// Returns empty record if not found.
    /// </summary>
    function GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;

    /// <summary>
    /// Tries to get a device by its address.
    /// </summary>
    /// <returns>True if device was found.</returns>
    function TryGetByAddress(AAddress: UInt64; out ADevice: TBluetoothDeviceInfo): Boolean;

    /// <summary>
    /// Checks if a device with the given address exists in the repository.
    /// </summary>
    function Contains(AAddress: UInt64): Boolean;

    /// <summary>
    /// Adds or updates a device in the repository.
    /// </summary>
    procedure AddOrUpdate(const ADevice: TBluetoothDeviceInfo);

    /// <summary>
    /// Updates the connection state of a device.
    /// </summary>
    /// <returns>The updated device, or empty record if not found.</returns>
    function UpdateConnectionState(AAddress: UInt64;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;

    /// <summary>
    /// Removes a device from the repository.
    /// </summary>
    procedure Remove(AAddress: UInt64);

    /// <summary>
    /// Clears all devices from the repository.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Refreshes all devices from Windows Bluetooth API.
    /// Clears cache and re-enumerates paired devices.
    /// </summary>
    procedure Refresh;

    /// <summary>
    /// Gets the number of devices in the repository.
    /// </summary>
    function GetCount: Integer;
    property Count: Integer read GetCount;

    /// <summary>
    /// Event fired when the device list changes (add/remove).
    /// </summary>
    function GetOnListChanged: TDeviceListChangedEvent;
    procedure SetOnListChanged(AValue: TDeviceListChangedEvent);
    property OnListChanged: TDeviceListChangedEvent
      read GetOnListChanged write SetOnListChanged;
  end;

  /// <summary>
  /// Result of a connection execution attempt.
  /// </summary>
  TConnectionResult = record
    Success: Boolean;
    ErrorCode: Cardinal;
    class function Ok: TConnectionResult; static;
    class function Fail(AErrorCode: Cardinal): TConnectionResult; static;
  end;

  /// <summary>
  /// Interface for executing Bluetooth connection operations.
  /// Single Responsibility: Only handles low-level connection execution.
  /// Separates Windows API calls from business logic.
  /// </summary>
  IConnectionExecutor = interface
    ['{F6A7B8C9-AAAA-BBBB-CCCC-DDDD0000FFFF}']

    /// <summary>
    /// Executes a connection or disconnection operation.
    /// </summary>
    /// <param name="ADevice">The device to connect/disconnect.</param>
    /// <param name="AServiceGuids">Service GUIDs to use for the operation.</param>
    /// <param name="AEnable">True to connect, False to disconnect.</param>
    /// <param name="ARetryCount">Number of retry attempts on failure.</param>
    /// <returns>Connection result with success status and error code.</returns>
    function Execute(
      const ADevice: TBluetoothDeviceInfo;
      const AServiceGuids: TArray<TGUID>;
      AEnable: Boolean;
      ARetryCount: Integer
    ): TConnectionResult;
  end;

  /// <summary>
  /// Interface for debouncing Bluetooth device events.
  /// Prevents duplicate event processing when multiple Windows Bluetooth
  /// events fire for the same device state change.
  /// </summary>
  IEventDebouncer = interface
    ['{A7B8C9D0-EEEE-FFFF-0000-111122223333}']

    /// <summary>
    /// Checks if an event should be processed or filtered as a duplicate.
    /// </summary>
    /// <param name="AAddress">Device Bluetooth address.</param>
    /// <param name="AEventType">Type of event.</param>
    /// <param name="AConnectionState">Current connection state.</param>
    /// <returns>True if event should be processed, False if duplicate.</returns>
    function ShouldProcess(
      AAddress: UInt64;
      AEventType: TDeviceEventType;
      AConnectionState: TBluetoothConnectionState
    ): Boolean;

    /// <summary>
    /// Clears all recorded events, allowing immediate processing of next event.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Gets the debounce interval in milliseconds.
    /// </summary>
    function GetDebounceMs: Integer;

    /// <summary>
    /// Sets the debounce interval in milliseconds.
    /// </summary>
    procedure SetDebounceMs(AValue: Integer);

    /// <summary>
    /// Debounce interval in milliseconds.
    /// </summary>
    property DebounceMs: Integer read GetDebounceMs write SetDebounceMs;
  end;

  /// <summary>
  /// Strategy interface for battery level query implementations.
  /// Implements Strategy Pattern for different query mechanisms.
  /// Examples: BLE GATT, SetupAPI device properties, HID reports.
  /// </summary>
  IBatteryQueryStrategy = interface
    ['{E1F2A3B4-CCCC-DDDD-EEEE-FFFF66667777}']

    /// <summary>
    /// Attempts to query battery level using this strategy.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    /// <param name="ATimeoutMs">Timeout in milliseconds.</param>
    /// <param name="AStatus">Output battery status if successful.</param>
    /// <returns>True if query succeeded, False to try next strategy.</returns>
    function TryQuery(ADeviceAddress: UInt64; ATimeoutMs: Cardinal;
      out AStatus: TBatteryStatus): Boolean;

    /// <summary>
    /// Gets the priority of this strategy (higher = tried first).
    /// </summary>
    function GetPriority: Integer;

    /// <summary>
    /// Gets a human-readable name for this strategy.
    /// </summary>
    function GetName: string;
  end;

  /// <summary>
  /// Interface for querying Bluetooth device battery levels.
  /// Single Responsibility: Only handles battery level queries.
  /// Uses BLE GATT Battery Service (UUID 0x180F) when available.
  /// </summary>
  IBatteryQuery = interface
    ['{C9D0E1F2-AAAA-BBBB-CCCC-DDDD44445555}']

    /// <summary>
    /// Gets the battery level for a Bluetooth device.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address of the device.</param>
    /// <returns>
    /// Battery status indicating support and level.
    /// Returns NotSupported if device doesn't have Battery Service.
    /// Returns Unknown if device supports battery but read failed.
    /// Returns level (0-100) on success.
    /// </returns>
    /// <remarks>
    /// This operation requires the device to be connected.
    /// Only BLE devices with GATT Battery Service are supported.
    /// The operation may take several seconds due to GATT communication.
    /// </remarks>
    function GetBatteryLevel(ADeviceAddress: UInt64): TBatteryStatus;

    /// <summary>
    /// Gets the battery level with a custom timeout.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address of the device.</param>
    /// <param name="ATimeoutMs">Timeout in milliseconds for the GATT operation.</param>
    /// <returns>Battery status.</returns>
    function GetBatteryLevelWithTimeout(ADeviceAddress: UInt64;
      ATimeoutMs: Cardinal): TBatteryStatus;
  end;

  /// <summary>
  /// Event handler for battery level query completion.
  /// </summary>
  TBatteryQueryCompletedEvent = procedure(
    Sender: TObject;
    ADeviceAddress: UInt64;
    const AStatus: TBatteryStatus
  ) of object;

  /// <summary>
  /// Interface for caching and managing battery level information.
  /// Single Responsibility: Caches battery levels and provides async refresh.
  /// </summary>
  IBatteryCache = interface
    ['{D0E1F2A3-BBBB-CCCC-DDDD-EEEE55556666}']

    /// <summary>
    /// Gets the cached battery status for a device.
    /// Returns TBatteryStatus.NotSupported if not in cache.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    /// <returns>Cached battery status.</returns>
    function GetBatteryStatus(ADeviceAddress: UInt64): TBatteryStatus;

    /// <summary>
    /// Sets the battery status in the cache.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    /// <param name="AStatus">Battery status to cache.</param>
    procedure SetBatteryStatus(ADeviceAddress: UInt64; const AStatus: TBatteryStatus);

    /// <summary>
    /// Checks if a device has a cached battery status.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    /// <returns>True if status is cached.</returns>
    function HasCachedStatus(ADeviceAddress: UInt64): Boolean;

    /// <summary>
    /// Requests an async battery level refresh for a device.
    /// Results are reported via OnQueryCompleted event.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    procedure RequestRefresh(ADeviceAddress: UInt64);

    /// <summary>
    /// Requests async battery level refresh for all connected devices.
    /// Results are reported via OnQueryCompleted event for each device.
    /// </summary>
    /// <param name="ADeviceAddresses">Array of 64-bit Bluetooth addresses.</param>
    procedure RequestRefreshAll(const ADeviceAddresses: TArray<UInt64>);

    /// <summary>
    /// Clears all cached battery statuses.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Removes a single device from the cache.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    procedure Remove(ADeviceAddress: UInt64);

    /// <summary>
    /// Gets/sets the event handler for query completion.
    /// </summary>
    function GetOnQueryCompleted: TBatteryQueryCompletedEvent;
    procedure SetOnQueryCompleted(AValue: TBatteryQueryCompletedEvent);
    property OnQueryCompleted: TBatteryQueryCompletedEvent
      read GetOnQueryCompleted write SetOnQueryCompleted;
  end;

  /// <summary>
  /// Interface for querying Bluetooth device profiles (enabled services).
  /// Single Responsibility: Only handles profile enumeration.
  /// Separates Windows API calls from business logic for testability.
  /// </summary>
  IProfileQuery = interface
    ['{B8C9D0E1-FFFF-0000-1111-222233334444}']

    /// <summary>
    /// Gets profile information for a device.
    /// Queries enabled services and converts to profile types.
    /// </summary>
    /// <param name="ADeviceAddress">Device Bluetooth address.</param>
    /// <returns>Device profile information record.</returns>
    function GetDeviceProfiles(ADeviceAddress: UInt64): TDeviceProfileInfo;

    /// <summary>
    /// Clears any cached profile information.
    /// </summary>
    procedure ClearCache;
  end;

  /// <summary>
  /// Callback type for battery query execution results.
  /// </summary>
  TBatteryQueryCallback = reference to procedure(AAddress: UInt64; const AStatus: TBatteryStatus);

  /// <summary>
  /// Interface for executing battery queries asynchronously or synchronously.
  /// Separates execution concerns from cache storage (SRP).
  /// </summary>
  IBatteryQueryExecutor = interface
    ['{F1A2B3C4-DDDD-EEEE-FFFF-000011112222}']

    /// <summary>
    /// Executes a battery query for a device.
    /// The callback is invoked when the query completes.
    /// </summary>
    /// <param name="ADeviceAddress">The 64-bit Bluetooth address.</param>
    /// <param name="ACallback">Callback invoked with the result.</param>
    procedure Execute(ADeviceAddress: UInt64; ACallback: TBatteryQueryCallback);

    /// <summary>
    /// Initiates shutdown, preventing new queries and waiting for pending ones.
    /// </summary>
    procedure Shutdown;

    /// <summary>
    /// Returns true if the executor has been shut down.
    /// </summary>
    function IsShutdown: Boolean;
  end;

implementation

{ TConnectionResult }

class function TConnectionResult.Ok: TConnectionResult;
begin
  Result.Success := True;
  Result.ErrorCode := 0;
end;

class function TConnectionResult.Fail(AErrorCode: Cardinal): TConnectionResult;
begin
  Result.Success := False;
  Result.ErrorCode := AErrorCode;
end;

end.
