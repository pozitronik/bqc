{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Bluetooth Device Connection Watcher             }
{                                                       }
{       Monitors Bluetooth device connection state      }
{       changes using WM_DEVICECHANGE notifications.    }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceWatcher;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.WinAPI,
  Bluetooth.DeviceConverter,
  App.Logger;

type
  /// <summary>
  /// Interface for querying device connection state.
  /// Extracted for testability - allows mocking device state in tests.
  /// </summary>
  IDeviceStateQuery = interface
    ['{7F8B2A3C-9D4E-4F6B-A1C5-8E3D2F7A6B9C}']
    /// <summary>
    /// Queries the actual connection state of a device.
    /// </summary>
    /// <param name="AAddress">Bluetooth device address.</param>
    /// <returns>
    /// csConnected if device is connected,
    /// csDisconnected if device is disconnected,
    /// csUnknown if query failed or state cannot be determined.
    /// </returns>
    function QueryConnectionState(AAddress: UInt64): TBluetoothConnectionState;
  end;

  /// <summary>
  /// Handler procedure for processing Bluetooth custom events.
  /// </summary>
  TBluetoothEventHandler = procedure(AEventData: Pointer) of object;

  /// <summary>
  /// Event type for device connection state changes.
  /// </summary>
  TDeviceConnectionEvent = procedure(
    Sender: TObject;
    const ADeviceAddress: UInt64;
    AConnected: Boolean
  ) of object;

  /// <summary>
  /// Event type for device attribute changes (including connection state).
  /// Uses domain type TBluetoothDeviceInfo instead of Windows API types.
  /// </summary>
  TDeviceAttributeChangedEvent = procedure(
    Sender: TObject;
    const ADeviceInfo: TBluetoothDeviceInfo
  ) of object;

  /// <summary>
  /// Event type for device discovery (radio in range).
  /// Uses domain type TBluetoothDeviceInfo instead of Windows API types.
  /// </summary>
  TDeviceDiscoveredEvent = procedure(
    Sender: TObject;
    const ADeviceInfo: TBluetoothDeviceInfo
  ) of object;

  /// <summary>
  /// Event type for device out of range.
  /// </summary>
  TDeviceOutOfRangeEvent = procedure(
    Sender: TObject;
    const ADeviceAddress: UInt64
  ) of object;

  /// <summary>
  /// Event type for watcher errors.
  /// </summary>
  TDeviceWatcherErrorEvent = procedure(
    Sender: TObject;
    const AMessage: string;
    AErrorCode: DWORD
  ) of object;

  /// <summary>
  /// Watches for Bluetooth device connection state changes using
  /// WM_DEVICECHANGE notifications via RegisterDeviceNotification.
  /// </summary>
  TBluetoothDeviceWatcher = class
  private
    FWindowHandle: HWND;
    FRadioHandle: THandle;
    FNotifyHandle: THandle;  // HDEVNOTIFY - handle from RegisterDeviceNotification
    FIsRunning: Boolean;
    FStateQuery: IDeviceStateQuery;  // Injected dependency for testing

    // GUID-to-handler registry for extensible event processing (OCP)
    FEventHandlers: TDictionary<TGUID, TBluetoothEventHandler>;

    FOnDeviceConnected: TDeviceConnectionEvent;
    FOnDeviceDisconnected: TDeviceConnectionEvent;
    FOnDeviceAttributeChanged: TDeviceAttributeChangedEvent;
    FOnDeviceDiscovered: TDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    FOnError: TDeviceWatcherErrorEvent;

    procedure RegisterEventHandlers;
    procedure WndProc(var Msg: TMessage);
    procedure HandleDeviceChange(wParam: WPARAM; lParam: LPARAM);

    function OpenBluetoothRadio: Boolean;
    procedure CloseBluetoothRadio;
    function RegisterForNotifications: Boolean;
    procedure UnregisterNotifications;

    procedure DoDeviceConnected(const ADeviceAddress: UInt64);
    procedure DoDeviceDisconnected(const ADeviceAddress: UInt64);
    procedure DoDeviceAttributeChanged(const ADeviceInfo: TBluetoothDeviceInfo);
    procedure DoDeviceDiscovered(const ADeviceInfo: TBluetoothDeviceInfo);
    procedure DoDeviceOutOfRange(const ADeviceAddress: UInt64);
    procedure DoError(const AMessage: string; AErrorCode: DWORD);

  protected
    /// <summary>
    /// Protected for testing - processes custom Bluetooth events by dispatching to appropriate handler.
    /// </summary>
    procedure ProcessCustomEvent(ABroadcast: Pointer);

    /// <summary>
    /// Protected for testing - processes HCI (Host Controller Interface) events.
    /// </summary>
    procedure ProcessHciEvent(AEventInfo: Pointer);

    /// <summary>
    /// Protected for testing - processes L2CAP (Logical Link Control and Adaptation Protocol) events.
    /// </summary>
    procedure ProcessL2CapEvent(AEventInfo: Pointer);

    /// <summary>
    /// Protected for testing - processes radio in range (discovery) events.
    /// </summary>
    procedure ProcessRadioInRange(AEventInfo: Pointer);

    /// <summary>
    /// Protected for testing - processes radio out of range events.
    /// </summary>
    procedure ProcessRadioOutOfRange(AAddress: Pointer);

    /// <summary>
    /// Protected for testing - queries actual device connection state via Windows API.
    /// Returns csConnected, csDisconnected, or csUnknown (if query fails).
    /// </summary>
    function QueryDeviceConnectionState(AAddress: UInt64): TBluetoothConnectionState;

  public
    /// <summary>
    /// Creates device watcher with optional state query dependency injection.
    /// </summary>
    /// <param name="AStateQuery">
    /// Device state query implementation. If nil, uses default Windows API implementation.
    /// </param>
    constructor Create(AStateQuery: IDeviceStateQuery = nil);
    destructor Destroy; override;

    /// <summary>
    /// Starts watching for device connection state changes.
    /// </summary>
    /// <returns>True if watcher started successfully.</returns>
    function Start: Boolean;

    /// <summary>
    /// Stops watching for device connection state changes.
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Whether the watcher is currently running.
    /// </summary>
    property IsRunning: Boolean read FIsRunning;

    /// <summary>
    /// Fired when a device connects at ACL level.
    /// </summary>
    property OnDeviceConnected: TDeviceConnectionEvent
      read FOnDeviceConnected write FOnDeviceConnected;

    /// <summary>
    /// Fired when a device disconnects at ACL level.
    /// </summary>
    property OnDeviceDisconnected: TDeviceConnectionEvent
      read FOnDeviceDisconnected write FOnDeviceDisconnected;

    /// <summary>
    /// Fired when device attributes change (name, class, connection state, etc.).
    /// </summary>
    property OnDeviceAttributeChanged: TDeviceAttributeChangedEvent
      read FOnDeviceAttributeChanged write FOnDeviceAttributeChanged;

    /// <summary>
    /// Fired when a new device is discovered (comes into range).
    /// </summary>
    property OnDeviceDiscovered: TDeviceDiscoveredEvent
      read FOnDeviceDiscovered write FOnDeviceDiscovered;

    /// <summary>
    /// Fired when a device goes out of range.
    /// </summary>
    property OnDeviceOutOfRange: TDeviceOutOfRangeEvent
      read FOnDeviceOutOfRange write FOnDeviceOutOfRange;

    /// <summary>
    /// Fired when an error occurs in the watcher.
    /// </summary>
    property OnError: TDeviceWatcherErrorEvent
      read FOnError write FOnError;
  end;

implementation

uses
  Vcl.Forms;

type
  /// <summary>
  /// Default implementation of IDeviceStateQuery using Windows Bluetooth API.
  /// </summary>
  TDefaultDeviceStateQuery = class(TInterfacedObject, IDeviceStateQuery)
  public
    function QueryConnectionState(AAddress: UInt64): TBluetoothConnectionState;
  end;

{ TDefaultDeviceStateQuery }

function TDefaultDeviceStateQuery.QueryConnectionState(
  AAddress: UInt64): TBluetoothConnectionState;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ErrorCode: DWORD;
begin
  // Query actual device state via Windows API
  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address.ullLong := AAddress;

  ErrorCode := BluetoothGetDeviceInfo(0, DeviceInfo);
  if ErrorCode = ERROR_SUCCESS then
  begin
    if DeviceInfo.fConnected then
      Result := csConnected
    else
      Result := csDisconnected;
  end
  else
  begin
    LogDebug('QueryConnectionState: BluetoothGetDeviceInfo failed, error=%d',
      [ErrorCode], 'TDefaultDeviceStateQuery');
    Result := csUnknown;
  end;
end;

{ TBluetoothDeviceWatcher }

constructor TBluetoothDeviceWatcher.Create(AStateQuery: IDeviceStateQuery);
begin
  inherited Create;
  FWindowHandle := 0;
  FRadioHandle := 0;
  FNotifyHandle := 0;
  FIsRunning := False;

  // Use injected state query or create default
  if Assigned(AStateQuery) then
    FStateQuery := AStateQuery
  else
    FStateQuery := TDefaultDeviceStateQuery.Create;

  // Initialize event handlers registry
  FEventHandlers := TDictionary<TGUID, TBluetoothEventHandler>.Create;
  RegisterEventHandlers;
end;

destructor TBluetoothDeviceWatcher.Destroy;
begin
  Stop;
  FEventHandlers.Free;
  inherited Destroy;
end;

procedure TBluetoothDeviceWatcher.RegisterEventHandlers;
begin
  // Register known Bluetooth event GUIDs with their handlers (OCP)
  // To add a new event type, simply add a new registration here
  FEventHandlers.Add(GUID_BLUETOOTH_HCI_EVENT, ProcessHciEvent);
  FEventHandlers.Add(GUID_BLUETOOTH_L2CAP_EVENT, ProcessL2CapEvent);
  FEventHandlers.Add(GUID_BLUETOOTH_RADIO_IN_RANGE, ProcessRadioInRange);
  FEventHandlers.Add(GUID_BLUETOOTH_RADIO_OUT_OF_RANGE, ProcessRadioOutOfRange);
  LogDebug('Registered %d event handlers', [FEventHandlers.Count], ClassName);
end;

function TBluetoothDeviceWatcher.Start: Boolean;
begin
  LogDebug('Start called', ClassName);
  Result := False;

  if FIsRunning then
  begin
    LogDebug('Already running', ClassName);
    Result := True;
    Exit;
  end;

  // Create message-only window for receiving notifications
  FWindowHandle := AllocateHWnd(WndProc);
  if FWindowHandle = 0 then
  begin
    LogWarning('Failed to create notification window: %d', [GetLastError], ClassName);
    DoError('Failed to create notification window', GetLastError);
    Exit;
  end;
  LogDebug('Window handle created: %d', [FWindowHandle], ClassName);

  // Open Bluetooth radio handle
  if not OpenBluetoothRadio then
  begin
    LogWarning('Failed to open Bluetooth radio', ClassName);
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;
  LogDebug('Radio handle opened: %d', [FRadioHandle], ClassName);

  // Register for device notifications
  if not RegisterForNotifications then
  begin
    LogWarning('Failed to register for notifications', ClassName);
    CloseBluetoothRadio;
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;
  LogDebug('Notification handle: %d', [FNotifyHandle], ClassName);

  FIsRunning := True;
  LogInfo('Started successfully', ClassName);
  Result := True;
end;

procedure TBluetoothDeviceWatcher.Stop;
begin
  if not FIsRunning then
    Exit;

  FIsRunning := False;

  UnregisterNotifications;
  CloseBluetoothRadio;

  if FWindowHandle <> 0 then
  begin
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;
end;

function TBluetoothDeviceWatcher.OpenBluetoothRadio: Boolean;
var
  FindParams: BLUETOOTH_FIND_RADIO_PARAMS;
  FindHandle: HBLUETOOTH_RADIO_FIND;
begin
  Result := False;

  FindParams.dwSize := SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  FindHandle := BluetoothFindFirstRadio(@FindParams, FRadioHandle);

  if FindHandle = 0 then
  begin
    DoError('No Bluetooth radio found', GetLastError);
    Exit;
  end;

  BluetoothFindRadioClose(FindHandle);
  Result := True;
end;

procedure TBluetoothDeviceWatcher.CloseBluetoothRadio;
begin
  if FRadioHandle <> 0 then
  begin
    CloseHandle(FRadioHandle);
    FRadioHandle := 0;
  end;
end;

function TBluetoothDeviceWatcher.RegisterForNotifications: Boolean;
var
  BroadcastHandle: DEV_BROADCAST_HANDLE;
begin
  Result := False;

  if FRadioHandle = 0 then
  begin
    DoError('No radio handle available for notification registration', 0);
    Exit;
  end;

  FillChar(BroadcastHandle, SizeOf(BroadcastHandle), 0);
  BroadcastHandle.dbch_size := SizeOf(DEV_BROADCAST_HANDLE);
  BroadcastHandle.dbch_devicetype := DBT_DEVTYP_HANDLE;
  BroadcastHandle.dbch_handle := FRadioHandle;

  FNotifyHandle := RegisterDeviceNotificationW(
    FWindowHandle,
    @BroadcastHandle,
    DEVICE_NOTIFY_WINDOW_HANDLE
  );

  if FNotifyHandle = 0 then
  begin
    DoError('Failed to register for device notifications', GetLastError);
    Exit;
  end;

  Result := True;
end;

procedure TBluetoothDeviceWatcher.UnregisterNotifications;
begin
  if FNotifyHandle <> 0 then
  begin
    UnregisterDeviceNotification(FNotifyHandle);
    FNotifyHandle := 0;
  end;
end;

procedure TBluetoothDeviceWatcher.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DEVICECHANGE then
  begin
    LogDebug('WM_DEVICECHANGE received, wParam=$%.4X', [Msg.WParam], ClassName);
    HandleDeviceChange(Msg.WParam, Msg.LParam);
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TBluetoothDeviceWatcher.HandleDeviceChange(wParam: WPARAM; lParam: LPARAM);
var
  Header: PDEV_BROADCAST_HDR;
  BroadcastHandle: PDEV_BROADCAST_HANDLE;
begin
  if lParam = 0 then
  begin
    LogDebug('HandleDeviceChange: lParam is 0, ignoring', ClassName);
    Exit;
  end;

  // Check for custom event (DBT_CUSTOMEVENT)
  if wParam <> DBT_CUSTOMEVENT then
  begin
    LogDebug('HandleDeviceChange: Not DBT_CUSTOMEVENT ($%.4X), ignoring', [wParam], ClassName);
    Exit;
  end;

  Header := PDEV_BROADCAST_HDR(lParam);

  // Verify it's a handle-based notification
  if Header^.dbch_devicetype <> DBT_DEVTYP_HANDLE then
  begin
    LogDebug('HandleDeviceChange: Not DBT_DEVTYP_HANDLE (%d), ignoring', [Header^.dbch_devicetype], ClassName);
    Exit;
  end;

  LogDebug('HandleDeviceChange: Processing DBT_CUSTOMEVENT with DBT_DEVTYP_HANDLE', ClassName);
  BroadcastHandle := PDEV_BROADCAST_HANDLE(lParam);
  ProcessCustomEvent(BroadcastHandle);
end;

procedure TBluetoothDeviceWatcher.ProcessCustomEvent(ABroadcast: Pointer);
var
  Broadcast: PDEV_BROADCAST_HANDLE;
  Handler: TBluetoothEventHandler;
  Found: Boolean;
begin
  Broadcast := PDEV_BROADCAST_HANDLE(ABroadcast);

  // BUG FIX: Synchronize dictionary access (TDictionary is not thread-safe)
  // WndProc can be called from any thread per Windows documentation
  System.TMonitor.Enter(FEventHandlers);
  try
    // Look up handler in registry (OCP - extensible without modification)
    Found := FEventHandlers.TryGetValue(Broadcast^.dbch_eventguid, Handler);
  finally
    System.TMonitor.Exit(FEventHandlers);
  end;

  // Call handler outside lock to avoid deadlocks
  if Found then
  begin
    LogDebug('ProcessCustomEvent: Found handler for GUID', ClassName);
    Handler(@Broadcast^.dbch_data[0]);
  end
  else
  begin
    LogDebug('ProcessCustomEvent: Unknown GUID {%.8X-%.4X-%.4X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X}', [
      Broadcast^.dbch_eventguid.D1,
      Broadcast^.dbch_eventguid.D2,
      Broadcast^.dbch_eventguid.D3,
      Broadcast^.dbch_eventguid.D4[0],
      Broadcast^.dbch_eventguid.D4[1],
      Broadcast^.dbch_eventguid.D4[2],
      Broadcast^.dbch_eventguid.D4[3],
      Broadcast^.dbch_eventguid.D4[4],
      Broadcast^.dbch_eventguid.D4[5],
      Broadcast^.dbch_eventguid.D4[6],
      Broadcast^.dbch_eventguid.D4[7]
    ], ClassName);
  end;
end;

function TBluetoothDeviceWatcher.QueryDeviceConnectionState(
  AAddress: UInt64): TBluetoothConnectionState;
begin
  // Delegate to injected state query (allows mocking in tests)
  Result := FStateQuery.QueryConnectionState(AAddress);
end;

procedure TBluetoothDeviceWatcher.ProcessHciEvent(AEventInfo: Pointer);
var
  EventInfo: PBTH_HCI_EVENT_INFO;
  DeviceAddress: UInt64;
  ActualState: TBluetoothConnectionState;
begin
  // BUG FIX: Validate pointer before dereferencing
  if AEventInfo = nil then
  begin
    DoError('ProcessHciEvent: Nil event data received', 0);
    Exit;
  end;

  EventInfo := PBTH_HCI_EVENT_INFO(AEventInfo);
  // bthAddress is UInt64 (BTH_ADDR), not BLUETOOTH_ADDRESS record
  DeviceAddress := EventInfo^.bthAddress;

  LogDebug('ProcessHciEvent: Address=$%.12X, ConnectionType=%d, Connected=%d', [
    DeviceAddress,
    EventInfo^.connectionType,
    EventInfo^.connected
  ], ClassName);

  // SCO events (voice/HFP profile) don't represent full device connect/disconnect.
  // Audio devices switch between A2DP (ACL) and HFP (SCO) during calls.
  // Verify actual state before firing events for SCO to avoid:
  // - False disconnects when call ends but A2DP is still active
  // - Battery status reset when HFP connects on already-connected device
  if EventInfo^.connectionType = HCI_CONNECTION_TYPE_SCO then
  begin
    ActualState := QueryDeviceConnectionState(DeviceAddress);
    LogDebug('ProcessHciEvent: SCO event, verified state=%d', [Ord(ActualState)], ClassName);

    if EventInfo^.connected <> 0 then
    begin
      // SCO connect - only fire if device state is KNOWN and was disconnected
      // BUG FIX: Check for csDisconnected explicitly, ignore csUnknown (query failure)
      if ActualState = csDisconnected then
      begin
        LogDebug('ProcessHciEvent: SCO connect on new device, firing OnDeviceConnected', ClassName);
        DoDeviceConnected(DeviceAddress);
      end
      else if ActualState = csConnected then
        LogDebug('ProcessHciEvent: Device already connected, ignoring SCO connect (profile switch)', ClassName)
      else
        LogDebug('ProcessHciEvent: Device state unknown, ignoring SCO connect (cannot determine if new connection)', ClassName);
    end
    else
    begin
      // SCO disconnect - only fire if device state is KNOWN and actually disconnected
      // BUG FIX: Check for csDisconnected explicitly, ignore csUnknown (query failure)
      if ActualState = csDisconnected then
      begin
        LogDebug('ProcessHciEvent: SCO disconnect, device not connected, firing OnDeviceDisconnected', ClassName);
        DoDeviceDisconnected(DeviceAddress);
      end
      else if ActualState = csConnected then
        LogDebug('ProcessHciEvent: Device still connected (A2DP active), ignoring SCO disconnect', ClassName)
      else
        LogDebug('ProcessHciEvent: Device state unknown, ignoring SCO disconnect (cannot determine if fully disconnected)', ClassName);
    end;
    Exit;
  end;

  // ACL and other connection types - handle normally
  if EventInfo^.connected <> 0 then
  begin
    LogDebug('ProcessHciEvent: Firing OnDeviceConnected', ClassName);
    DoDeviceConnected(DeviceAddress);
  end
  else
  begin
    LogDebug('ProcessHciEvent: Firing OnDeviceDisconnected', ClassName);
    DoDeviceDisconnected(DeviceAddress);
  end;
end;

procedure TBluetoothDeviceWatcher.ProcessL2CapEvent(AEventInfo: Pointer);
var
  EventInfo: PBTH_L2CAP_EVENT_INFO;
  ProfileType: TBluetoothProfileType;
  ProfileName: string;
  Profile: TBluetoothProfile;
begin
  // BUG FIX: Validate pointer before dereferencing
  if AEventInfo = nil then
  begin
    DoError('ProcessL2CapEvent: Nil event data received', 0);
    Exit;
  end;

  EventInfo := PBTH_L2CAP_EVENT_INFO(AEventInfo);

  // Map PSM to profile type for better logging
  ProfileType := PsmToProfileType(EventInfo^.psm);
  if ProfileType <> bptUnknown then
  begin
    // Create temporary profile to get display name
    Profile := TBluetoothProfile.Create(ProfileType, TGUID.Empty, pcsUnknown);
    ProfileName := Profile.ShortName;
  end
  else
    ProfileName := Format('PSM=$%.4X', [EventInfo^.psm]);

  LogDebug('ProcessL2CapEvent: Address=$%.12X, Profile=%s, Connected=%d, Initiated=%d', [
    EventInfo^.bthAddress,
    ProfileName,
    EventInfo^.connected,
    EventInfo^.initiated
  ], ClassName);

  // L2CAP events indicate profile-level connections.
  // This information can be used to track which profiles are active.
  // For now, we log this for research purposes.
  // TODO: Emit OnProfileConnected/OnProfileDisconnected events for profile tracking.
end;

procedure TBluetoothDeviceWatcher.ProcessRadioInRange(AEventInfo: Pointer);
var
  EventInfo: PBTH_RADIO_IN_RANGE;
  DeviceInfo: TBluetoothDeviceInfo;
  IsConnected, IsPaired: Boolean;
begin
  // BUG FIX: Validate pointer before dereferencing
  if AEventInfo = nil then
  begin
    DoError('ProcessRadioInRange: Nil event data received', 0);
    Exit;
  end;

  EventInfo := PBTH_RADIO_IN_RANGE(AEventInfo);

  // BTH_DEVICE_INFO uses flags field, not bool fields like BLUETOOTH_DEVICE_INFO
  IsConnected := (EventInfo^.deviceInfo.flags and BDIF_CONNECTED) <> 0;
  IsPaired := (EventInfo^.deviceInfo.flags and BDIF_PAIRED) <> 0;

  LogDebug('ProcessRadioInRange: Address=$%.12X, Name="%s", Flags=$%.8X (Connected=%d, Paired=%d), PrevFlags=$%.8X', [
    EventInfo^.deviceInfo.address,
    string(AnsiString(EventInfo^.deviceInfo.name)),
    EventInfo^.deviceInfo.flags,
    Ord(IsConnected),
    Ord(IsPaired),
    EventInfo^.previousDeviceFlags
  ], ClassName);

  // Convert low-level BTH_DEVICE_INFO to domain type at the boundary
  DeviceInfo := ConvertBthDeviceInfo(EventInfo^.deviceInfo);

  // RadioInRange provides device info including connection state
  DoDeviceAttributeChanged(DeviceInfo);

  // Also fire discovered event for newly discovered devices (not paired)
  if not IsPaired then
  begin
    LogDebug('ProcessRadioInRange: Device not paired, firing OnDeviceDiscovered', ClassName);
    DoDeviceDiscovered(DeviceInfo);
  end;
end;

procedure TBluetoothDeviceWatcher.ProcessRadioOutOfRange(AAddress: Pointer);
var
  Address: PBLUETOOTH_ADDRESS;
begin
  // BUG FIX: Validate pointer before dereferencing
  if AAddress = nil then
  begin
    DoError('ProcessRadioOutOfRange: Nil address data received', 0);
    Exit;
  end;

  Address := PBLUETOOTH_ADDRESS(AAddress);
  LogDebug('ProcessRadioOutOfRange: Address=$%.12X', [Address^.ullLong], ClassName);
  DoDeviceOutOfRange(Address^.ullLong);
end;

procedure TBluetoothDeviceWatcher.DoDeviceConnected(const ADeviceAddress: UInt64);
begin
  LogDebug('DoDeviceConnected: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceConnected), True)
  ], ClassName);
  if Assigned(FOnDeviceConnected) then
    FOnDeviceConnected(Self, ADeviceAddress, True);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDisconnected(const ADeviceAddress: UInt64);
begin
  LogDebug('DoDeviceDisconnected: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceDisconnected), True)
  ], ClassName);
  if Assigned(FOnDeviceDisconnected) then
    FOnDeviceDisconnected(Self, ADeviceAddress, False);
end;

procedure TBluetoothDeviceWatcher.DoDeviceAttributeChanged(
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  LogDebug('DoDeviceAttributeChanged: Address=$%.12X, Handler assigned=%s', [
    ADeviceInfo.AddressInt,
    BoolToStr(Assigned(FOnDeviceAttributeChanged), True)
  ], ClassName);
  if Assigned(FOnDeviceAttributeChanged) then
    FOnDeviceAttributeChanged(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDiscovered(
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  LogDebug('DoDeviceDiscovered: Address=$%.12X, Handler assigned=%s', [
    ADeviceInfo.AddressInt,
    BoolToStr(Assigned(FOnDeviceDiscovered), True)
  ], ClassName);
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceOutOfRange(const ADeviceAddress: UInt64);
begin
  LogDebug('DoDeviceOutOfRange: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceOutOfRange), True)
  ], ClassName);
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TBluetoothDeviceWatcher.DoError(const AMessage: string; AErrorCode: DWORD);
begin
  LogError('DoError: %s (code=%d)', [AMessage, AErrorCode], ClassName);
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

end.
