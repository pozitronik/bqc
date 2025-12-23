{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Bluetooth Device Connection Watcher             }
{                                                       }
{       Monitors Bluetooth device connection state      }
{       changes using WM_DEVICECHANGE notifications.    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceWatcher;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Bluetooth.Types,
  App.Logger;

type
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

    FOnDeviceConnected: TDeviceConnectionEvent;
    FOnDeviceDisconnected: TDeviceConnectionEvent;
    FOnDeviceAttributeChanged: TDeviceAttributeChangedEvent;
    FOnDeviceDiscovered: TDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    FOnError: TDeviceWatcherErrorEvent;

    procedure WndProc(var Msg: TMessage);
    procedure HandleDeviceChange(wParam: WPARAM; lParam: LPARAM);
    procedure ProcessCustomEvent(ABroadcast: Pointer);
    procedure ProcessHciEvent(AEventInfo: Pointer);
    procedure ProcessL2CapEvent(AEventInfo: Pointer);
    procedure ProcessRadioInRange(AEventInfo: Pointer);
    procedure ProcessRadioOutOfRange(AAddress: Pointer);

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

  public
    constructor Create;
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
  Vcl.Forms,
  Bluetooth.WinAPI;

{ Helper Functions }

/// <summary>
/// Converts a Windows API BLUETOOTH_DEVICE_INFO to domain TBluetoothDeviceInfo.
/// This conversion happens at the infrastructure boundary to keep domain
/// types isolated from Windows API specifics.
/// </summary>
function ConvertToDeviceInfo(const AWinAPIInfo: BLUETOOTH_DEVICE_INFO): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
  ConnectionState: TBluetoothConnectionState;
  LastSeen, LastUsed: TDateTime;
begin
  Address := UInt64ToBluetoothAddress(AWinAPIInfo.Address.ullLong);

  if AWinAPIInfo.fConnected then
    ConnectionState := csConnected
  else
    ConnectionState := csDisconnected;

  // Convert SYSTEMTIME to TDateTime (0 means unknown)
  try
    if (AWinAPIInfo.stLastSeen.wYear > 0) then
      LastSeen := SystemTimeToDateTime(AWinAPIInfo.stLastSeen)
    else
      LastSeen := 0;
  except
    LastSeen := 0;
  end;

  try
    if (AWinAPIInfo.stLastUsed.wYear > 0) then
      LastUsed := SystemTimeToDateTime(AWinAPIInfo.stLastUsed)
    else
      LastUsed := 0;
  except
    LastUsed := 0;
  end;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AWinAPIInfo.Address.ullLong,
    string(AWinAPIInfo.szName),
    DetermineDeviceType(AWinAPIInfo.ulClassOfDevice),
    ConnectionState,
    AWinAPIInfo.fRemembered,
    AWinAPIInfo.fAuthenticated,
    AWinAPIInfo.ulClassOfDevice,
    LastSeen,
    LastUsed
  );
end;

{ TBluetoothDeviceWatcher }

constructor TBluetoothDeviceWatcher.Create;
begin
  inherited Create;
  FWindowHandle := 0;
  FRadioHandle := 0;
  FNotifyHandle := 0;
  FIsRunning := False;
end;

destructor TBluetoothDeviceWatcher.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TBluetoothDeviceWatcher.Start: Boolean;
begin
  Log('Start called', ClassName);
  Result := False;

  if FIsRunning then
  begin
    Log('Already running', ClassName);
    Result := True;
    Exit;
  end;

  // Create message-only window for receiving notifications
  FWindowHandle := AllocateHWnd(WndProc);
  if FWindowHandle = 0 then
  begin
    Log('Failed to create notification window: %d', [GetLastError], ClassName);
    DoError('Failed to create notification window', GetLastError);
    Exit;
  end;
  Log('Window handle created: %d', [FWindowHandle], ClassName);

  // Open Bluetooth radio handle
  if not OpenBluetoothRadio then
  begin
    Log('Failed to open Bluetooth radio', ClassName);
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;
  Log('Radio handle opened: %d', [FRadioHandle], ClassName);

  // Register for device notifications
  if not RegisterForNotifications then
  begin
    Log('Failed to register for notifications', ClassName);
    CloseBluetoothRadio;
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;
  Log('Notification handle: %d', [FNotifyHandle], ClassName);

  FIsRunning := True;
  Log('Started successfully', ClassName);
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
    Log('WM_DEVICECHANGE received, wParam=$%.4X', [Msg.WParam], ClassName);
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
    Log('HandleDeviceChange: lParam is 0, ignoring', ClassName);
    Exit;
  end;

  // Check for custom event (DBT_CUSTOMEVENT)
  if wParam <> DBT_CUSTOMEVENT then
  begin
    Log('HandleDeviceChange: Not DBT_CUSTOMEVENT ($%.4X), ignoring', [wParam], ClassName);
    Exit;
  end;

  Header := PDEV_BROADCAST_HDR(lParam);

  // Verify it's a handle-based notification
  if Header^.dbch_devicetype <> DBT_DEVTYP_HANDLE then
  begin
    Log('HandleDeviceChange: Not DBT_DEVTYP_HANDLE (%d), ignoring', [Header^.dbch_devicetype], ClassName);
    Exit;
  end;

  Log('HandleDeviceChange: Processing DBT_CUSTOMEVENT with DBT_DEVTYP_HANDLE', ClassName);
  BroadcastHandle := PDEV_BROADCAST_HANDLE(lParam);
  ProcessCustomEvent(BroadcastHandle);
end;

procedure TBluetoothDeviceWatcher.ProcessCustomEvent(ABroadcast: Pointer);
var
  Broadcast: PDEV_BROADCAST_HANDLE;
begin
  Broadcast := PDEV_BROADCAST_HANDLE(ABroadcast);
  // Check which Bluetooth event type this is
  if IsEqualGUID(Broadcast^.dbch_eventguid, GUID_BLUETOOTH_HCI_EVENT) then
  begin
    Log('ProcessCustomEvent: GUID_BLUETOOTH_HCI_EVENT', ClassName);
    // ACL-level connect/disconnect
    ProcessHciEvent(@Broadcast^.dbch_data[0]);
  end
  else if IsEqualGUID(Broadcast^.dbch_eventguid, GUID_BLUETOOTH_L2CAP_EVENT) then
  begin
    Log('ProcessCustomEvent: GUID_BLUETOOTH_L2CAP_EVENT', ClassName);
    // L2CAP channel establish/terminate
    ProcessL2CapEvent(@Broadcast^.dbch_data[0]);
  end
  else if IsEqualGUID(Broadcast^.dbch_eventguid, GUID_BLUETOOTH_RADIO_IN_RANGE) then
  begin
    Log('ProcessCustomEvent: GUID_BLUETOOTH_RADIO_IN_RANGE', ClassName);
    // Device attributes changed or device discovered
    ProcessRadioInRange(@Broadcast^.dbch_data[0]);
  end
  else if IsEqualGUID(Broadcast^.dbch_eventguid, GUID_BLUETOOTH_RADIO_OUT_OF_RANGE) then
  begin
    Log('ProcessCustomEvent: GUID_BLUETOOTH_RADIO_OUT_OF_RANGE', ClassName);
    // Device went out of range
    ProcessRadioOutOfRange(@Broadcast^.dbch_data[0]);
  end
  else
  begin
    Log('ProcessCustomEvent: Unknown GUID {%.8X-%.4X-%.4X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X}', [
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

procedure TBluetoothDeviceWatcher.ProcessHciEvent(AEventInfo: Pointer);
var
  EventInfo: PBTH_HCI_EVENT_INFO;
  DeviceAddress: UInt64;
begin
  EventInfo := PBTH_HCI_EVENT_INFO(AEventInfo);
  // bthAddress is UInt64 (BTH_ADDR), not BLUETOOTH_ADDRESS record
  DeviceAddress := EventInfo^.bthAddress;

  Log('ProcessHciEvent: Address=$%.12X, ConnectionType=%d, Connected=%d', [
    DeviceAddress,
    EventInfo^.connectionType,
    EventInfo^.connected
  ], ClassName);

  if EventInfo^.connected <> 0 then
  begin
    Log('ProcessHciEvent: Firing OnDeviceConnected', ClassName);
    DoDeviceConnected(DeviceAddress);
  end
  else
  begin
    Log('ProcessHciEvent: Firing OnDeviceDisconnected', ClassName);
    DoDeviceDisconnected(DeviceAddress);
  end;
end;

procedure TBluetoothDeviceWatcher.ProcessL2CapEvent(AEventInfo: Pointer);
var
  EventInfo: PBTH_L2CAP_EVENT_INFO;
begin
  EventInfo := PBTH_L2CAP_EVENT_INFO(AEventInfo);
  Log('ProcessL2CapEvent: Address=$%.12X, PSM=%d, Connected=%d, Initiated=%d', [
    EventInfo^.bthAddress,
    EventInfo^.psm,
    EventInfo^.connected,
    EventInfo^.initiated
  ], ClassName);
  // L2CAP events are lower-level and less useful for UI updates.
  // For now, we don't process these - HCI events and RadioInRange are more relevant.
  // This could be extended in the future if needed.
end;

procedure TBluetoothDeviceWatcher.ProcessRadioInRange(AEventInfo: Pointer);
var
  EventInfo: PBTH_RADIO_IN_RANGE;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  EventInfo := PBTH_RADIO_IN_RANGE(AEventInfo);
  Log('ProcessRadioInRange: Address=$%.12X, Name="%s", Connected=%d, Remembered=%d, PrevFlags=$%.8X', [
    EventInfo^.deviceInfo.Address.ullLong,
    string(EventInfo^.deviceInfo.szName),
    Ord(EventInfo^.deviceInfo.fConnected),
    Ord(EventInfo^.deviceInfo.fRemembered),
    EventInfo^.previousDeviceFlags
  ], ClassName);

  // Convert Windows API type to domain type at the boundary
  DeviceInfo := ConvertToDeviceInfo(EventInfo^.deviceInfo);

  // RadioInRange provides full device info including connection state
  DoDeviceAttributeChanged(DeviceInfo);

  // Also fire discovered event for newly discovered devices
  if not EventInfo^.deviceInfo.fRemembered then
  begin
    Log('ProcessRadioInRange: Device not remembered, firing OnDeviceDiscovered', ClassName);
    DoDeviceDiscovered(DeviceInfo);
  end;
end;

procedure TBluetoothDeviceWatcher.ProcessRadioOutOfRange(AAddress: Pointer);
var
  Address: PBLUETOOTH_ADDRESS;
begin
  Address := PBLUETOOTH_ADDRESS(AAddress);
  Log('ProcessRadioOutOfRange: Address=$%.12X', [Address^.ullLong], ClassName);
  DoDeviceOutOfRange(Address^.ullLong);
end;

procedure TBluetoothDeviceWatcher.DoDeviceConnected(const ADeviceAddress: UInt64);
begin
  Log('DoDeviceConnected: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceConnected), True)
  ], ClassName);
  if Assigned(FOnDeviceConnected) then
    FOnDeviceConnected(Self, ADeviceAddress, True);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDisconnected(const ADeviceAddress: UInt64);
begin
  Log('DoDeviceDisconnected: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceDisconnected), True)
  ], ClassName);
  if Assigned(FOnDeviceDisconnected) then
    FOnDeviceDisconnected(Self, ADeviceAddress, False);
end;

procedure TBluetoothDeviceWatcher.DoDeviceAttributeChanged(
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  Log('DoDeviceAttributeChanged: Address=$%.12X, Handler assigned=%s', [
    ADeviceInfo.AddressInt,
    BoolToStr(Assigned(FOnDeviceAttributeChanged), True)
  ], ClassName);
  if Assigned(FOnDeviceAttributeChanged) then
    FOnDeviceAttributeChanged(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDiscovered(
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  Log('DoDeviceDiscovered: Address=$%.12X, Handler assigned=%s', [
    ADeviceInfo.AddressInt,
    BoolToStr(Assigned(FOnDeviceDiscovered), True)
  ], ClassName);
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceOutOfRange(const ADeviceAddress: UInt64);
begin
  Log('DoDeviceOutOfRange: Address=$%.12X, Handler assigned=%s', [
    ADeviceAddress,
    BoolToStr(Assigned(FOnDeviceOutOfRange), True)
  ], ClassName);
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TBluetoothDeviceWatcher.DoError(const AMessage: string; AErrorCode: DWORD);
begin
  Log('DoError: %s (code=%d)', [AMessage, AErrorCode], ClassName);
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

end.
