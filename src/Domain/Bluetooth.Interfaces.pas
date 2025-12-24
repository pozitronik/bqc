{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Domain Interfaces (Abstractions)                }
{                                                       }
{       Copyright (c) 2024                              }
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
    /// Gets/sets the handler for errors.
    /// </summary>
    function GetOnError: TBluetoothErrorEvent;
    procedure SetOnError(AValue: TBluetoothErrorEvent);
    property OnError: TBluetoothErrorEvent
      read GetOnError write SetOnError;
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
    /// Gets/sets the handler for device state changes.
    /// </summary>
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    property OnDeviceStateChanged: TMonitorDeviceStateEvent
      read GetOnDeviceStateChanged write SetOnDeviceStateChanged;

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
