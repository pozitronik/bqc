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
  /// Follows Interface Segregation Principle by extending smaller interfaces.
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

implementation

end.
