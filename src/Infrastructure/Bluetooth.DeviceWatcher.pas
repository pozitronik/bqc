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
  Bluetooth.WinAPI;

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
  /// </summary>
  TDeviceAttributeChangedEvent = procedure(
    Sender: TObject;
    const ADeviceInfo: BLUETOOTH_DEVICE_INFO
  ) of object;

  /// <summary>
  /// Event type for device discovery (radio in range).
  /// </summary>
  TDeviceDiscoveredEvent = procedure(
    Sender: TObject;
    const ADeviceInfo: BLUETOOTH_DEVICE_INFO
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
    FNotifyHandle: HDEVNOTIFY;
    FIsRunning: Boolean;

    FOnDeviceConnected: TDeviceConnectionEvent;
    FOnDeviceDisconnected: TDeviceConnectionEvent;
    FOnDeviceAttributeChanged: TDeviceAttributeChangedEvent;
    FOnDeviceDiscovered: TDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TDeviceOutOfRangeEvent;
    FOnError: TDeviceWatcherErrorEvent;

    procedure WndProc(var Msg: TMessage);
    procedure HandleDeviceChange(wParam: WPARAM; lParam: LPARAM);
    procedure ProcessCustomEvent(const ABroadcast: PDEV_BROADCAST_HANDLE);
    procedure ProcessHciEvent(const AEventInfo: PBTH_HCI_EVENT_INFO);
    procedure ProcessL2CapEvent(const AEventInfo: PBTH_L2CAP_EVENT_INFO);
    procedure ProcessRadioInRange(const AEventInfo: PBTH_RADIO_IN_RANGE);
    procedure ProcessRadioOutOfRange(const AAddress: PBLUETOOTH_ADDRESS);

    function OpenBluetoothRadio: Boolean;
    procedure CloseBluetoothRadio;
    function RegisterForNotifications: Boolean;
    procedure UnregisterNotifications;

    procedure DoDeviceConnected(const ADeviceAddress: UInt64);
    procedure DoDeviceDisconnected(const ADeviceAddress: UInt64);
    procedure DoDeviceAttributeChanged(const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
    procedure DoDeviceDiscovered(const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
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
  Vcl.Forms;

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
  Result := False;

  if FIsRunning then
  begin
    Result := True;
    Exit;
  end;

  // Create message-only window for receiving notifications
  FWindowHandle := AllocateHWnd(WndProc);
  if FWindowHandle = 0 then
  begin
    DoError('Failed to create notification window', GetLastError);
    Exit;
  end;

  // Open Bluetooth radio handle
  if not OpenBluetoothRadio then
  begin
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;

  // Register for device notifications
  if not RegisterForNotifications then
  begin
    CloseBluetoothRadio;
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
    Exit;
  end;

  FIsRunning := True;
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
    HandleDeviceChange(Msg.WParam, Msg.LParam)
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TBluetoothDeviceWatcher.HandleDeviceChange(wParam: WPARAM; lParam: LPARAM);
var
  Header: PDEV_BROADCAST_HDR;
  BroadcastHandle: PDEV_BROADCAST_HANDLE;
begin
  if lParam = 0 then
    Exit;

  // Check for custom event (DBT_CUSTOMEVENT)
  if wParam <> DBT_CUSTOMEVENT then
    Exit;

  Header := PDEV_BROADCAST_HDR(lParam);

  // Verify it's a handle-based notification
  if Header^.dbch_devicetype <> DBT_DEVTYP_HANDLE then
    Exit;

  BroadcastHandle := PDEV_BROADCAST_HANDLE(lParam);
  ProcessCustomEvent(BroadcastHandle);
end;

procedure TBluetoothDeviceWatcher.ProcessCustomEvent(const ABroadcast: PDEV_BROADCAST_HANDLE);
begin
  // Check which Bluetooth event type this is
  if IsEqualGUID(ABroadcast^.dbch_eventguid, GUID_BLUETOOTH_HCI_EVENT) then
  begin
    // ACL-level connect/disconnect
    ProcessHciEvent(PBTH_HCI_EVENT_INFO(@ABroadcast^.dbch_data[0]));
  end
  else if IsEqualGUID(ABroadcast^.dbch_eventguid, GUID_BLUETOOTH_L2CAP_EVENT) then
  begin
    // L2CAP channel establish/terminate
    ProcessL2CapEvent(PBTH_L2CAP_EVENT_INFO(@ABroadcast^.dbch_data[0]));
  end
  else if IsEqualGUID(ABroadcast^.dbch_eventguid, GUID_BLUETOOTH_RADIO_IN_RANGE) then
  begin
    // Device attributes changed or device discovered
    ProcessRadioInRange(PBTH_RADIO_IN_RANGE(@ABroadcast^.dbch_data[0]));
  end
  else if IsEqualGUID(ABroadcast^.dbch_eventguid, GUID_BLUETOOTH_RADIO_OUT_OF_RANGE) then
  begin
    // Device went out of range
    ProcessRadioOutOfRange(PBLUETOOTH_ADDRESS(@ABroadcast^.dbch_data[0]));
  end;
end;

procedure TBluetoothDeviceWatcher.ProcessHciEvent(const AEventInfo: PBTH_HCI_EVENT_INFO);
var
  DeviceAddress: UInt64;
begin
  // bthAddress is UInt64 (BTH_ADDR), not BLUETOOTH_ADDRESS record
  DeviceAddress := AEventInfo^.bthAddress;

  if AEventInfo^.connected <> 0 then
    DoDeviceConnected(DeviceAddress)
  else
    DoDeviceDisconnected(DeviceAddress);
end;

procedure TBluetoothDeviceWatcher.ProcessL2CapEvent(const AEventInfo: PBTH_L2CAP_EVENT_INFO);
begin
  // L2CAP events are lower-level and less useful for UI updates.
  // For now, we don't process these - HCI events and RadioInRange are more relevant.
  // This could be extended in the future if needed.
end;

procedure TBluetoothDeviceWatcher.ProcessRadioInRange(const AEventInfo: PBTH_RADIO_IN_RANGE);
begin
  // RadioInRange provides full device info including connection state
  DoDeviceAttributeChanged(AEventInfo^.deviceInfo);

  // Also fire discovered event for newly discovered devices
  if not AEventInfo^.deviceInfo.fRemembered then
    DoDeviceDiscovered(AEventInfo^.deviceInfo);
end;

procedure TBluetoothDeviceWatcher.ProcessRadioOutOfRange(const AAddress: PBLUETOOTH_ADDRESS);
begin
  DoDeviceOutOfRange(AAddress^.ullLong);
end;

procedure TBluetoothDeviceWatcher.DoDeviceConnected(const ADeviceAddress: UInt64);
begin
  if Assigned(FOnDeviceConnected) then
    FOnDeviceConnected(Self, ADeviceAddress, True);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDisconnected(const ADeviceAddress: UInt64);
begin
  if Assigned(FOnDeviceDisconnected) then
    FOnDeviceDisconnected(Self, ADeviceAddress, False);
end;

procedure TBluetoothDeviceWatcher.DoDeviceAttributeChanged(
  const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
begin
  if Assigned(FOnDeviceAttributeChanged) then
    FOnDeviceAttributeChanged(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceDiscovered(
  const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
begin
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADeviceInfo);
end;

procedure TBluetoothDeviceWatcher.DoDeviceOutOfRange(const ADeviceAddress: UInt64);
begin
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TBluetoothDeviceWatcher.DoError(const AMessage: string; AErrorCode: DWORD);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

end.
