{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Bluetooth Service Implementation           }
{                                                       }
{       Implements IBluetoothService facade.            }
{       Follows Single Responsibility by delegating     }
{       to specialized components.                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.Service;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.ConnectionStrategies,
  Bluetooth.DeviceWatcher,
  App.Logger;

type
  /// <summary>
  /// Main Bluetooth service implementation.
  /// Facade pattern: Provides unified interface to Bluetooth subsystem.
  /// </summary>
  TBluetoothService = class(TInterfacedObject, IBluetoothService)
  private
    FOnDeviceStateChanged: TDeviceStateChangedEvent;
    FOnDeviceListChanged: TDeviceListChangedEvent;
    FOnError: TBluetoothErrorEvent;
    FRadioHandle: THandle;
    FDeviceCache: TDictionary<UInt64, TBluetoothDeviceInfo>;
    FDeviceWatcher: TBluetoothDeviceWatcher;
    FDeviceWatcherStarted: Boolean;

    { Polling fallback - used when device watcher fails }
    // TODO: Add configuration option to enable/disable polling fallback
    // TODO: Add configuration option for polling interval
    FPollingEnabled: Boolean;
    FPollingHandle: HWND;
    FPollingTimerID: UINT_PTR;

    procedure CloseRadio;
    function ConvertToDeviceInfo(const AWinDeviceInfo: BLUETOOTH_DEVICE_INFO): TBluetoothDeviceInfo;
    function ConvertSystemTimeToDateTime(const ASysTime: TSystemTime): TDateTime;
    procedure DoDeviceStateChanged(const ADevice: TBluetoothDeviceInfo);
    procedure DoDeviceListChanged;
    procedure DoError(const AMessage: string; AErrorCode: Cardinal);

    function ConnectWithStrategy(
      const ADevice: TBluetoothDeviceInfo;
      AEnable: Boolean
    ): Boolean;

    { Device watcher event handlers }
    procedure HandleWatcherDeviceConnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherDeviceDisconnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherDeviceAttributeChanged(Sender: TObject;
      const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
    procedure HandleWatcherError(Sender: TObject;
      const AMessage: string; AErrorCode: DWORD);

    function RefreshDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    procedure StartDeviceWatcher;
    procedure StopDeviceWatcher;

    { Polling fallback methods }
    procedure StartPollingFallback;
    procedure StopPollingFallback;
    procedure PollingWndProc(var Msg: TMessage);
    procedure CheckDeviceStatesPolling;

  protected
    { IBluetoothService }
    function IsAdapterAvailable: Boolean;
    function GetPairedDevices: TBluetoothDeviceInfoArray;
    function RefreshAllDevices: TBluetoothDeviceInfoArray;
    function Connect(const ADevice: TBluetoothDeviceInfo): Boolean;
    function Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;
    function ToggleConnection(const ADevice: TBluetoothDeviceInfo): Boolean;

    function GetOnDeviceStateChanged: TDeviceStateChangedEvent;
    procedure SetOnDeviceStateChanged(AValue: TDeviceStateChangedEvent);
    function GetOnDeviceListChanged: TDeviceListChangedEvent;
    procedure SetOnDeviceListChanged(AValue: TDeviceListChangedEvent);
    function GetOnError: TBluetoothErrorEvent;
    procedure SetOnError(AValue: TBluetoothErrorEvent);

  public
    constructor Create;
    destructor Destroy; override;
  end;

/// <summary>
/// Creates the default Bluetooth service instance.
/// Factory function for dependency injection.
/// </summary>
function CreateBluetoothService: IBluetoothService;

implementation

uses
  System.DateUtils,
  Vcl.Forms;

const
  POLLING_TIMER_ID = 1;
  // TODO: Make polling interval configurable
  POLLING_INTERVAL_MS = 2000;  // Check device states every 2 seconds

function CreateBluetoothService: IBluetoothService;
begin
  Result := TBluetoothService.Create;
end;

{ TBluetoothService }

constructor TBluetoothService.Create;
begin
  inherited Create;
  FRadioHandle := 0;
  FDeviceCache := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  FDeviceWatcher := nil;
  FDeviceWatcherStarted := False;
  FPollingEnabled := False;
  FPollingHandle := 0;
  FPollingTimerID := 0;

  // Start device watcher for real-time connection monitoring
  StartDeviceWatcher;
end;

destructor TBluetoothService.Destroy;
begin
  StopPollingFallback;
  StopDeviceWatcher;
  CloseRadio;
  FDeviceCache.Free;
  inherited Destroy;
end;

procedure TBluetoothService.CloseRadio;
begin
  if FRadioHandle <> 0 then
  begin
    CloseHandle(FRadioHandle);
    FRadioHandle := 0;
  end;
end;

function TBluetoothService.IsAdapterAvailable: Boolean;
var
  FindParams: BLUETOOTH_FIND_RADIO_PARAMS;
  FindHandle: HBLUETOOTH_RADIO_FIND;
  RadioHandle: THandle;
begin
  FindParams.dwSize := SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  FindHandle := BluetoothFindFirstRadio(@FindParams, RadioHandle);

  Result := FindHandle <> 0;

  if Result then
  begin
    CloseHandle(RadioHandle);
    BluetoothFindRadioClose(FindHandle);
  end;
end;

function TBluetoothService.GetPairedDevices: TBluetoothDeviceInfoArray;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  DeviceList: TList<TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
begin
  DeviceList := TList<TBluetoothDeviceInfo>.Create;
  try
    // Initialize search parameters for paired devices
    InitDeviceSearchParams(SearchParams, 0);

    // Initialize device info structure
    InitDeviceInfo(DeviceInfo);

    // Find first device
    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);

    if FindHandle <> 0 then
    begin
      try
        repeat
          Device := ConvertToDeviceInfo(DeviceInfo);
          DeviceList.Add(Device);

          // Update cache
          FDeviceCache.AddOrSetValue(Device.AddressInt, Device);

          // Reset structure size for next iteration
          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end;

    Result := DeviceList.ToArray;

  finally
    DeviceList.Free;
  end;
end;

function TBluetoothService.RefreshAllDevices: TBluetoothDeviceInfoArray;
begin
  // Clear cache and re-enumerate
  FDeviceCache.Clear;
  Result := GetPairedDevices;
end;

function TBluetoothService.ConvertToDeviceInfo(
  const AWinDeviceInfo: BLUETOOTH_DEVICE_INFO): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
  ConnectionState: TBluetoothConnectionState;
begin
  // Copy address bytes
  Move(AWinDeviceInfo.Address.rgBytes[0], Address[0], 6);

  // Determine connection state
  if AWinDeviceInfo.fConnected then
    ConnectionState := csConnected
  else
    ConnectionState := csDisconnected;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AWinDeviceInfo.Address.ullLong,
    string(AWinDeviceInfo.szName),
    DetermineDeviceType(AWinDeviceInfo.ulClassOfDevice),
    ConnectionState,
    AWinDeviceInfo.fRemembered,
    AWinDeviceInfo.fAuthenticated,
    AWinDeviceInfo.ulClassOfDevice,
    ConvertSystemTimeToDateTime(AWinDeviceInfo.stLastSeen),
    ConvertSystemTimeToDateTime(AWinDeviceInfo.stLastUsed)
  );
end;

function TBluetoothService.ConvertSystemTimeToDateTime(
  const ASysTime: TSystemTime): TDateTime;
begin
  try
    if (ASysTime.wYear >= 1900) and (ASysTime.wYear <= 2100) then
      Result := EncodeDateTime(
        ASysTime.wYear,
        ASysTime.wMonth,
        ASysTime.wDay,
        ASysTime.wHour,
        ASysTime.wMinute,
        ASysTime.wSecond,
        ASysTime.wMilliseconds
      )
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function TBluetoothService.Connect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := ConnectWithStrategy(ADevice, True);
end;

function TBluetoothService.Disconnect(const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := ConnectWithStrategy(ADevice, False);
end;

function TBluetoothService.ToggleConnection(
  const ADevice: TBluetoothDeviceInfo): Boolean;
begin
  if ADevice.IsConnected then
    Result := Disconnect(ADevice)
  else
    Result := Connect(ADevice);
end;

function TBluetoothService.ConnectWithStrategy(
  const ADevice: TBluetoothDeviceInfo;
  AEnable: Boolean
): Boolean;
var
  Strategy: IConnectionStrategy;
  ServiceGuids: TArray<TGUID>;
  ServiceGuid: TGUID;
  WinDeviceInfo: BLUETOOTH_DEVICE_INFO;
  ServiceFlag: DWORD;
  ErrorCode: DWORD;
  UpdatedDevice: TBluetoothDeviceInfo;
  AnySuccess: Boolean;
begin
  Result := False;
  AnySuccess := False;

  // Get appropriate strategy for this device type
  Strategy := TConnectionStrategyFactory.GetStrategy(ADevice.DeviceType);
  if Strategy = nil then
  begin
    DoError('No connection strategy found for device type', 0);
    Exit;
  end;

  // Get service GUIDs from strategy
  ServiceGuids := Strategy.GetServiceGuids;
  if Length(ServiceGuids) = 0 then
  begin
    DoError('No service GUIDs defined for device', 0);
    Exit;
  end;

  // Prepare device info structure
  InitDeviceInfo(WinDeviceInfo);
  WinDeviceInfo.Address.ullLong := ADevice.AddressInt;
  Move(ADevice.Address[0], WinDeviceInfo.Address.rgBytes[0], 6);

  // Notify state change (connecting/disconnecting)
  if AEnable then
    UpdatedDevice := ADevice.WithConnectionState(csConnecting)
  else
    UpdatedDevice := ADevice.WithConnectionState(csDisconnecting);
  DoDeviceStateChanged(UpdatedDevice);

  // Set service flag
  if AEnable then
    ServiceFlag := BLUETOOTH_SERVICE_ENABLE
  else
    ServiceFlag := BLUETOOTH_SERVICE_DISABLE;

  // Try each service GUID
  ErrorCode := ERROR_SUCCESS;
  for ServiceGuid in ServiceGuids do
  begin
    ErrorCode := BluetoothSetServiceState(
      0,  // Use first available radio
      @WinDeviceInfo,
      @ServiceGuid,
      ServiceFlag
    );

    if ErrorCode = ERROR_SUCCESS then
      AnySuccess := True;
  end;

  Result := AnySuccess;

  // Update final state
  if Result then
  begin
    if AEnable then
      UpdatedDevice := ADevice.WithConnectionState(csConnected)
    else
      UpdatedDevice := ADevice.WithConnectionState(csDisconnected);
  end
  else
  begin
    UpdatedDevice := ADevice.WithConnectionState(csError);
    DoError('Failed to change connection state', ErrorCode);
  end;

  // Update cache and notify
  FDeviceCache.AddOrSetValue(UpdatedDevice.AddressInt, UpdatedDevice);
  DoDeviceStateChanged(UpdatedDevice);
end;

procedure TBluetoothService.DoDeviceStateChanged(
  const ADevice: TBluetoothDeviceInfo);
begin
  Log('[Service] DoDeviceStateChanged: Address=$%.12X, Name="%s", ConnectionState=%d, Handler assigned=%s', [
    ADevice.AddressInt,
    ADevice.Name,
    Ord(ADevice.ConnectionState),
    BoolToStr(Assigned(FOnDeviceStateChanged), True)
  ]);
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, ADevice);
end;

procedure TBluetoothService.DoDeviceListChanged;
begin
  if Assigned(FOnDeviceListChanged) then
    FOnDeviceListChanged(Self);
end;

procedure TBluetoothService.DoError(const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

function TBluetoothService.GetOnDeviceStateChanged: TDeviceStateChangedEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TBluetoothService.SetOnDeviceStateChanged(
  AValue: TDeviceStateChangedEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TBluetoothService.GetOnDeviceListChanged: TDeviceListChangedEvent;
begin
  Result := FOnDeviceListChanged;
end;

procedure TBluetoothService.SetOnDeviceListChanged(
  AValue: TDeviceListChangedEvent);
begin
  FOnDeviceListChanged := AValue;
end;

function TBluetoothService.GetOnError: TBluetoothErrorEvent;
begin
  Result := FOnError;
end;

procedure TBluetoothService.SetOnError(AValue: TBluetoothErrorEvent);
begin
  FOnError := AValue;
end;

{ Device Watcher Methods }

procedure TBluetoothService.StartDeviceWatcher;
begin
  if FDeviceWatcherStarted then
    Exit;

  FDeviceWatcher := TBluetoothDeviceWatcher.Create;
  FDeviceWatcher.OnDeviceConnected := HandleWatcherDeviceConnected;
  FDeviceWatcher.OnDeviceDisconnected := HandleWatcherDeviceDisconnected;
  FDeviceWatcher.OnDeviceAttributeChanged := HandleWatcherDeviceAttributeChanged;
  FDeviceWatcher.OnError := HandleWatcherError;

  if FDeviceWatcher.Start then
    FDeviceWatcherStarted := True
  else
  begin
    // Watcher failed to start - error was already reported via OnError
    // TODO: Add configuration option to fall back to polling here
    FDeviceWatcher.Free;
    FDeviceWatcher := nil;
  end;
end;

procedure TBluetoothService.StopDeviceWatcher;
begin
  if FDeviceWatcher <> nil then
  begin
    FDeviceWatcher.Stop;
    FDeviceWatcher.Free;
    FDeviceWatcher := nil;
  end;
  FDeviceWatcherStarted := False;
end;

function TBluetoothService.RefreshDeviceByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ErrorCode: DWORD;
begin
  // Initialize structure with address to query
  InitDeviceInfo(DeviceInfo);
  DeviceInfo.Address.ullLong := AAddress;

  // Query current device info from Windows
  ErrorCode := BluetoothGetDeviceInfo(0, DeviceInfo);

  if ErrorCode = ERROR_SUCCESS then
    Result := ConvertToDeviceInfo(DeviceInfo)
  else
  begin
    // Device not found - return empty record with just the address
    FillChar(Result, SizeOf(Result), 0);
  end;
end;

procedure TBluetoothService.HandleWatcherDeviceConnected(Sender: TObject;
  const ADeviceAddress: UInt64; AConnected: Boolean);
var
  Device: TBluetoothDeviceInfo;
  UpdatedDevice: TBluetoothDeviceInfo;
begin
  Log('[Service] HandleWatcherDeviceConnected: Address=$%.12X', [ADeviceAddress]);

  // Refresh device info from Windows to get current state
  Device := RefreshDeviceByAddress(ADeviceAddress);
  Log('[Service] HandleWatcherDeviceConnected: RefreshDeviceByAddress returned AddressInt=$%.12X, Name="%s"', [
    Device.AddressInt, Device.Name
  ]);

  if Device.AddressInt <> 0 then
  begin
    // Update cache
    if FDeviceCache.ContainsKey(ADeviceAddress) then
    begin
      Log('[Service] HandleWatcherDeviceConnected: Found in FDeviceCache, updating to csConnected');
      UpdatedDevice := FDeviceCache[ADeviceAddress].WithConnectionState(csConnected);
      FDeviceCache[ADeviceAddress] := UpdatedDevice;
      DoDeviceStateChanged(UpdatedDevice);
    end
    else if Device.IsPaired then
    begin
      // New paired device we haven't seen before
      Log('[Service] HandleWatcherDeviceConnected: New paired device, adding to cache');
      FDeviceCache.AddOrSetValue(ADeviceAddress, Device);
      DoDeviceStateChanged(Device);
    end;
  end
  else
    Log('[Service] HandleWatcherDeviceConnected: RefreshDeviceByAddress returned invalid device');
end;

procedure TBluetoothService.HandleWatcherDeviceDisconnected(Sender: TObject;
  const ADeviceAddress: UInt64; AConnected: Boolean);
var
  UpdatedDevice: TBluetoothDeviceInfo;
  Device: TBluetoothDeviceInfo;
begin
  Log('[Service] HandleWatcherDeviceDisconnected: Address=$%.12X', [ADeviceAddress]);

  // Update cache with disconnected state
  if FDeviceCache.ContainsKey(ADeviceAddress) then
  begin
    Log('[Service] HandleWatcherDeviceDisconnected: Found in FDeviceCache, updating to csDisconnected');
    UpdatedDevice := FDeviceCache[ADeviceAddress].WithConnectionState(csDisconnected);
    FDeviceCache[ADeviceAddress] := UpdatedDevice;
    DoDeviceStateChanged(UpdatedDevice);
  end
  else
  begin
    Log('[Service] HandleWatcherDeviceDisconnected: Not in cache, trying RefreshDeviceByAddress');
    // Device not in cache - try to refresh from Windows
    Device := RefreshDeviceByAddress(ADeviceAddress);
    if (Device.AddressInt <> 0) and (Device.Name <> '') and Device.IsPaired then
    begin
      Log('[Service] HandleWatcherDeviceDisconnected: Got paired device from Windows, Name="%s"', [Device.Name]);
      UpdatedDevice := Device.WithConnectionState(csDisconnected);
      FDeviceCache.AddOrSetValue(ADeviceAddress, UpdatedDevice);
      DoDeviceStateChanged(UpdatedDevice);
    end
    else
      Log('[Service] HandleWatcherDeviceDisconnected: Device not found, has no name, or not paired');
  end;
end;

procedure TBluetoothService.HandleWatcherDeviceAttributeChanged(Sender: TObject;
  const ADeviceInfo: BLUETOOTH_DEVICE_INFO);
var
  Device: TBluetoothDeviceInfo;
  CachedDevice: TBluetoothDeviceInfo;
  Address: UInt64;
  EventName: string;
begin
  Address := ADeviceInfo.Address.ullLong;

  Log('[Service] HandleWatcherDeviceAttributeChanged: Address=$%.12X, Name="%s", Connected=%d, Remembered=%d', [
    Address,
    string(ADeviceInfo.szName),
    Ord(ADeviceInfo.fConnected),
    Ord(ADeviceInfo.fRemembered)
  ]);

  // Skip events with zero address (invalid)
  if Address = 0 then
  begin
    Log('[Service] HandleWatcherDeviceAttributeChanged: Zero address, skipping');
    Exit;
  end;

  // Get name from event (may be empty)
  EventName := Trim(string(ADeviceInfo.szName));

  // Convert event data to our device info
  Device := ConvertToDeviceInfo(ADeviceInfo);

  // If event has empty name, the structure data is likely corrupted due to alignment issues.
  // In this case, preserve both the cached name AND connection state.
  if EventName = '' then
  begin
    Log('[Service] HandleWatcherDeviceAttributeChanged: Event has empty name (structure likely misaligned), checking cache');
    if FDeviceCache.TryGetValue(Address, CachedDevice) then
    begin
      // Use cached device entirely since event data is unreliable
      if CachedDevice.Name <> '' then
      begin
        Log('[Service] HandleWatcherDeviceAttributeChanged: Using cached device Name="%s", ConnectionState=%d', [
          CachedDevice.Name, Ord(CachedDevice.ConnectionState)
        ]);
        // Keep the cached device as-is, don't update from unreliable event data
        Exit;
      end;
    end;
  end;

  // Skip if we still have no valid name (likely spurious event)
  if Device.Name = '' then
  begin
    Log('[Service] HandleWatcherDeviceAttributeChanged: No valid name after cache check, skipping');
    Exit;
  end;

  Log('[Service] HandleWatcherDeviceAttributeChanged: Final device Name="%s", ConnectionState=%d', [
    Device.Name, Ord(Device.ConnectionState)
  ]);

  // Update cache only for paired devices
  if ADeviceInfo.fRemembered then
  begin
    FDeviceCache.AddOrSetValue(Address, Device);
    DoDeviceStateChanged(Device);
  end;
end;

procedure TBluetoothService.HandleWatcherError(Sender: TObject;
  const AMessage: string; AErrorCode: DWORD);
begin
  DoError(AMessage, AErrorCode);
end;

{ Polling Fallback Methods }

procedure TBluetoothService.StartPollingFallback;
begin
  if FPollingEnabled then
    Exit;

  // Create message-only window for timer
  FPollingHandle := AllocateHWnd(PollingWndProc);
  if FPollingHandle = 0 then
  begin
    DoError('Failed to create polling window', GetLastError);
    Exit;
  end;

  // Start polling timer
  FPollingTimerID := SetTimer(FPollingHandle, POLLING_TIMER_ID, POLLING_INTERVAL_MS, nil);
  if FPollingTimerID = 0 then
  begin
    DeallocateHWnd(FPollingHandle);
    FPollingHandle := 0;
    DoError('Failed to start polling timer', GetLastError);
    Exit;
  end;

  FPollingEnabled := True;
end;

procedure TBluetoothService.StopPollingFallback;
begin
  if not FPollingEnabled then
    Exit;

  if FPollingTimerID <> 0 then
  begin
    KillTimer(FPollingHandle, POLLING_TIMER_ID);
    FPollingTimerID := 0;
  end;

  if FPollingHandle <> 0 then
  begin
    DeallocateHWnd(FPollingHandle);
    FPollingHandle := 0;
  end;

  FPollingEnabled := False;
end;

procedure TBluetoothService.PollingWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_TIMER) and (UINT_PTR(Msg.WParam) = POLLING_TIMER_ID) then
    CheckDeviceStatesPolling
  else
    Msg.Result := DefWindowProc(FPollingHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TBluetoothService.CheckDeviceStatesPolling;
var
  Addresses: TArray<UInt64>;
  Address: UInt64;
  CurrentDevice: TBluetoothDeviceInfo;
  CachedDevice: TBluetoothDeviceInfo;
  WasConnected, IsNowConnected: Boolean;
begin
  // Get snapshot of addresses to check (avoid modifying collection during iteration)
  Addresses := FDeviceCache.Keys.ToArray;

  // Check each cached device for state changes
  for Address in Addresses do
  begin
    if not FDeviceCache.TryGetValue(Address, CachedDevice) then
      Continue;

    // Query current state from Windows
    CurrentDevice := RefreshDeviceByAddress(Address);

    if CurrentDevice.AddressInt = 0 then
      Continue;  // Device not found

    // Check if connection state changed
    WasConnected := CachedDevice.IsConnected;
    IsNowConnected := CurrentDevice.IsConnected;

    if WasConnected <> IsNowConnected then
    begin
      // Update cache
      FDeviceCache[Address] := CurrentDevice;

      // Notify listeners
      DoDeviceStateChanged(CurrentDevice);
    end;
  end;
end;

end.
