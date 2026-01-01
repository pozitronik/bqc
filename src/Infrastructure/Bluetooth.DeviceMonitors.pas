{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Monitor Implementations                  }
{                                                       }
{       Implements IDeviceMonitor for various           }
{       monitoring strategies (polling, watcher).       }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceMonitors;

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
  Bluetooth.DeviceWatcher,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf;

type
  /// <summary>
  /// Polling-based device monitor implementation.
  /// Uses a timer to periodically check device connection states.
  /// </summary>
  TPollingMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    FOnError: TMonitorErrorEvent;
    FPollingInterval: Integer;
    FRunning: Boolean;
    FPollingHandle: HWND;
    FPollingTimerID: UINT_PTR;
    FDeviceStates: TDictionary<UInt64, TBluetoothConnectionState>;

    procedure PollingWndProc(var Msg: TMessage);
    procedure CheckDeviceStates;
    function QueryDeviceConnectionState(AAddress: UInt64): TBluetoothConnectionState;
    function EnumeratePairedDeviceAddresses: TArray<UInt64>;

    procedure DoDeviceStateChanged(AAddress: UInt64; ANewState: TBluetoothConnectionState);
    procedure DoError(const AMessage: string; AErrorCode: Cardinal);

  protected
    { IDeviceMonitor }
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);
    function GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
    function GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);

  public
    constructor Create(APollingInterval: Integer);
    destructor Destroy; override;

    property PollingInterval: Integer read FPollingInterval;
  end;

  /// <summary>
  /// Device watcher-based monitor implementation.
  /// Uses WM_DEVICECHANGE notifications for real-time state changes.
  /// Wraps TBluetoothDeviceWatcher with IDeviceMonitor interface.
  /// </summary>
  TDeviceWatcherMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    FOnError: TMonitorErrorEvent;
    FWatcher: TBluetoothDeviceWatcher;
    FRunning: Boolean;

    { Event handlers for TBluetoothDeviceWatcher }
    procedure HandleWatcherDeviceConnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherDeviceDisconnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherDeviceDiscovered(Sender: TObject;
      const ADevice: TBluetoothDeviceInfo);
    procedure HandleWatcherDeviceOutOfRange(Sender: TObject;
      const ADeviceAddress: UInt64);
    procedure HandleWatcherError(Sender: TObject;
      const AMessage: string; AErrorCode: DWORD);

    procedure DoDeviceStateChanged(AAddress: UInt64; ANewState: TBluetoothConnectionState);
    procedure DoDeviceDiscovered(const ADevice: TBluetoothDeviceInfo);
    procedure DoDeviceOutOfRange(const ADeviceAddress: UInt64);
    procedure DoError(const AMessage: string; AErrorCode: Cardinal);

  protected
    { IDeviceMonitor }
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);
    function GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
    function GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Fallback monitor that tries primary monitor first, then falls back to secondary.
  /// Composite pattern: combines two monitoring strategies.
  /// Monitors are injected for testability.
  /// </summary>
  TFallbackMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    FOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    FOnError: TMonitorErrorEvent;
    FPrimaryMonitor: IDeviceMonitor;
    FSecondaryMonitor: IDeviceMonitor;
    FActiveMonitor: IDeviceMonitor;

    procedure HandlePrimaryError(Sender: TObject;
      const AMessage: string; AErrorCode: Cardinal);
    procedure HandleDeviceStateChanged(Sender: TObject;
      const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
    procedure HandleDeviceDiscovered(Sender: TObject;
      const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceOutOfRange(Sender: TObject;
      const ADeviceAddress: UInt64);
    procedure HandleSecondaryError(Sender: TObject;
      const AMessage: string; AErrorCode: Cardinal);

    procedure SwitchToSecondary;
    procedure WireUpMonitorEvents;

  protected
    { IDeviceMonitor }
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);
    function GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
    procedure SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
    function GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
    procedure SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);

  public
    /// <summary>
    /// Creates a fallback monitor with injected primary and secondary monitors.
    /// </summary>
    /// <param name="APrimaryMonitor">The primary monitor to try first.</param>
    /// <param name="ASecondaryMonitor">The fallback monitor if primary fails.</param>
    constructor Create(APrimaryMonitor, ASecondaryMonitor: IDeviceMonitor);
    destructor Destroy; override;
  end;

  /// <summary>
  /// Factory for creating device monitors based on configuration.
  /// </summary>
  TDeviceMonitorFactory = class(TInterfacedObject, IDeviceMonitorFactory)
  private
    FPollingConfig: IPollingConfig;
  public
    constructor Create(APollingConfig: IPollingConfig);

    { IDeviceMonitorFactory }
    function CreateMonitor: IDeviceMonitor;
  end;

const
  POLLING_MONITOR_TIMER_ID = 2;
  DEFAULT_POLLING_INTERVAL = 2000;
  MIN_POLLING_INTERVAL = 500;
  MAX_POLLING_INTERVAL = 30000;

implementation

uses
  Vcl.Forms,
  App.Logger;

{ TPollingMonitor }

constructor TPollingMonitor.Create(APollingInterval: Integer);
begin
  inherited Create;
  FRunning := False;
  FPollingHandle := 0;
  FPollingTimerID := 0;
  FDeviceStates := TDictionary<UInt64, TBluetoothConnectionState>.Create;

  // Validate and store polling interval
  FPollingInterval := APollingInterval;
  if FPollingInterval < MIN_POLLING_INTERVAL then
    FPollingInterval := MIN_POLLING_INTERVAL;
  if FPollingInterval > MAX_POLLING_INTERVAL then
    FPollingInterval := MAX_POLLING_INTERVAL;
end;

destructor TPollingMonitor.Destroy;
begin
  Stop;
  FDeviceStates.Free;
  inherited Destroy;
end;

function TPollingMonitor.Start: Boolean;
begin
  Result := False;

  if FRunning then
    Exit(True);

  // Create message-only window for timer
  FPollingHandle := AllocateHWnd(PollingWndProc);
  if FPollingHandle = 0 then
  begin
    DoError('Failed to create polling window', GetLastError);
    Exit;
  end;

  // Start polling timer
  FPollingTimerID := SetTimer(FPollingHandle, POLLING_MONITOR_TIMER_ID, FPollingInterval, nil);
  if FPollingTimerID = 0 then
  begin
    DeallocateHWnd(FPollingHandle);
    FPollingHandle := 0;
    DoError('Failed to start polling timer', GetLastError);
    Exit;
  end;

  // Initialize device states on first run
  FDeviceStates.Clear;
  CheckDeviceStates;

  FRunning := True;
  LogDebug('Started with interval %d ms', [FPollingInterval], ClassName);
  Result := True;
end;

procedure TPollingMonitor.Stop;
begin
  if not FRunning then
    Exit;

  if FPollingTimerID <> 0 then
  begin
    KillTimer(FPollingHandle, POLLING_MONITOR_TIMER_ID);
    FPollingTimerID := 0;
  end;

  if FPollingHandle <> 0 then
  begin
    DeallocateHWnd(FPollingHandle);
    FPollingHandle := 0;
  end;

  FRunning := False;
  LogDebug('Stopped', ClassName);
end;

function TPollingMonitor.IsRunning: Boolean;
begin
  Result := FRunning;
end;

procedure TPollingMonitor.PollingWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_TIMER) and (UINT_PTR(Msg.WParam) = POLLING_MONITOR_TIMER_ID) then
    CheckDeviceStates
  else
    Msg.Result := DefWindowProc(FPollingHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TPollingMonitor.CheckDeviceStates;
var
  Addresses: TArray<UInt64>;
  Address: UInt64;
  CurrentState, PreviousState: TBluetoothConnectionState;
  IsFirstCheck: Boolean;
begin
  // Get all paired device addresses
  Addresses := EnumeratePairedDeviceAddresses;

  for Address in Addresses do
  begin
    // Query current connection state
    CurrentState := QueryDeviceConnectionState(Address);

    // Check if we have a previous state
    IsFirstCheck := not FDeviceStates.TryGetValue(Address, PreviousState);

    // Update stored state
    FDeviceStates.AddOrSetValue(Address, CurrentState);

    // Fire event if state changed (but not on first check)
    if (not IsFirstCheck) and (CurrentState <> PreviousState) then
    begin
      LogDebug('Device $%.12X state changed: %d -> %d', [
        Address, Ord(PreviousState), Ord(CurrentState)
      ], ClassName);
      DoDeviceStateChanged(Address, CurrentState);
    end;
  end;
end;

function TPollingMonitor.QueryDeviceConnectionState(AAddress: UInt64): TBluetoothConnectionState;
var
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  ErrorCode: DWORD;
begin
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
    Result := csUnknown;
end;

function TPollingMonitor.EnumeratePairedDeviceAddresses: TArray<UInt64>;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  AddressList: TList<UInt64>;
begin
  AddressList := TList<UInt64>.Create;
  try
    InitDeviceSearchParams(SearchParams, 0);
    InitDeviceInfo(DeviceInfo);

    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);

    if FindHandle <> 0 then
    begin
      try
        repeat
          AddressList.Add(DeviceInfo.Address.ullLong);
          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end;

    Result := AddressList.ToArray;
  finally
    AddressList.Free;
  end;
end;

procedure TPollingMonitor.DoDeviceStateChanged(AAddress: UInt64;
  ANewState: TBluetoothConnectionState);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, AAddress, ANewState);
end;

procedure TPollingMonitor.DoError(const AMessage: string; AErrorCode: Cardinal);
begin
  LogDebug('Error: %s (code %d)', [AMessage, AErrorCode], ClassName);
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

function TPollingMonitor.GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
begin
  Result := FOnDeviceDiscovered;
end;

function TPollingMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TPollingMonitor.SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
begin
  FOnDeviceDiscovered := AValue;
end;

function TPollingMonitor.GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
begin
  Result := FOnDeviceOutOfRange;
end;

procedure TPollingMonitor.SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);
begin
  FOnDeviceOutOfRange := AValue;
end;

procedure TPollingMonitor.SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TPollingMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TPollingMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

{ TDeviceWatcherMonitor }

constructor TDeviceWatcherMonitor.Create;
begin
  inherited Create;
  FWatcher := nil;
  FRunning := False;
end;

destructor TDeviceWatcherMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TDeviceWatcherMonitor.Start: Boolean;
begin
  Result := False;

  if FRunning then
    Exit(True);

  FWatcher := TBluetoothDeviceWatcher.Create;
  FWatcher.OnDeviceConnected := HandleWatcherDeviceConnected;
  FWatcher.OnDeviceDisconnected := HandleWatcherDeviceDisconnected;
  FWatcher.OnDeviceDiscovered := HandleWatcherDeviceDiscovered;
  FWatcher.OnDeviceOutOfRange := HandleWatcherDeviceOutOfRange;
  FWatcher.OnError := HandleWatcherError;

  if FWatcher.Start then
  begin
    FRunning := True;
    LogDebug('Started successfully', ClassName);
    Result := True;
  end
  else
  begin
    LogDebug('Failed to start', ClassName);
    FWatcher.Free;
    FWatcher := nil;
  end;
end;

procedure TDeviceWatcherMonitor.Stop;
begin
  if not FRunning then
    Exit;

  if FWatcher <> nil then
  begin
    FWatcher.Stop;
    FWatcher.Free;
    FWatcher := nil;
  end;

  FRunning := False;
  LogDebug('Stopped', ClassName);
end;

function TDeviceWatcherMonitor.IsRunning: Boolean;
begin
  Result := FRunning;
end;

procedure TDeviceWatcherMonitor.HandleWatcherDeviceConnected(Sender: TObject;
  const ADeviceAddress: UInt64; AConnected: Boolean);
begin
  DoDeviceStateChanged(ADeviceAddress, csConnected);
end;

procedure TDeviceWatcherMonitor.HandleWatcherDeviceDisconnected(Sender: TObject;
  const ADeviceAddress: UInt64; AConnected: Boolean);
begin
  DoDeviceStateChanged(ADeviceAddress, csDisconnected);
end;

procedure TDeviceWatcherMonitor.HandleWatcherDeviceDiscovered(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  DoDeviceDiscovered(ADevice);
end;

procedure TDeviceWatcherMonitor.HandleWatcherDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
begin
  DoDeviceOutOfRange(ADeviceAddress);
end;

procedure TDeviceWatcherMonitor.HandleWatcherError(Sender: TObject;
  const AMessage: string; AErrorCode: DWORD);
begin
  DoError(AMessage, AErrorCode);
end;

procedure TDeviceWatcherMonitor.DoDeviceStateChanged(AAddress: UInt64;
  ANewState: TBluetoothConnectionState);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, AAddress, ANewState);
end;

procedure TDeviceWatcherMonitor.DoDeviceDiscovered(const ADevice: TBluetoothDeviceInfo);
begin
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADevice);
end;

procedure TDeviceWatcherMonitor.DoDeviceOutOfRange(const ADeviceAddress: UInt64);
begin
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TDeviceWatcherMonitor.DoError(const AMessage: string; AErrorCode: Cardinal);
begin
  LogDebug('Error: %s (code %d)', [AMessage, AErrorCode], ClassName);
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

function TDeviceWatcherMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TDeviceWatcherMonitor.SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TDeviceWatcherMonitor.GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
begin
  Result := FOnDeviceDiscovered;
end;

procedure TDeviceWatcherMonitor.SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
begin
  FOnDeviceDiscovered := AValue;
end;

function TDeviceWatcherMonitor.GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
begin
  Result := FOnDeviceOutOfRange;
end;

procedure TDeviceWatcherMonitor.SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);
begin
  FOnDeviceOutOfRange := AValue;
end;

function TDeviceWatcherMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TDeviceWatcherMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

{ TFallbackMonitor }

constructor TFallbackMonitor.Create(APrimaryMonitor, ASecondaryMonitor: IDeviceMonitor);
begin
  inherited Create;
  FPrimaryMonitor := APrimaryMonitor;
  FSecondaryMonitor := ASecondaryMonitor;
  FActiveMonitor := nil;
end;

destructor TFallbackMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TFallbackMonitor.WireUpMonitorEvents;
begin
  // Wire up primary monitor events
  if FPrimaryMonitor <> nil then
  begin
    FPrimaryMonitor.SetOnDeviceStateChanged(HandleDeviceStateChanged);
    FPrimaryMonitor.SetOnDeviceDiscovered(HandleDeviceDiscovered);
    FPrimaryMonitor.SetOnDeviceOutOfRange(HandleDeviceOutOfRange);
    FPrimaryMonitor.SetOnError(HandlePrimaryError);
  end;

  // Wire up secondary monitor events
  if FSecondaryMonitor <> nil then
  begin
    FSecondaryMonitor.SetOnDeviceStateChanged(HandleDeviceStateChanged);
    FSecondaryMonitor.SetOnDeviceDiscovered(HandleDeviceDiscovered);
    FSecondaryMonitor.SetOnDeviceOutOfRange(HandleDeviceOutOfRange);
    FSecondaryMonitor.SetOnError(HandleSecondaryError);
  end;
end;

function TFallbackMonitor.Start: Boolean;
begin
  if IsRunning then
    Exit(True);

  // Wire up event handlers
  WireUpMonitorEvents;

  // Try to start primary monitor
  if (FPrimaryMonitor <> nil) and FPrimaryMonitor.Start then
  begin
    FActiveMonitor := FPrimaryMonitor;
    LogDebug('Started with primary monitor', ClassName);
    Result := True;
  end
  else
  begin
    // Primary failed or not available, switch to secondary
    LogDebug('Primary monitor failed, switching to secondary', ClassName);
    SwitchToSecondary;
    Result := IsRunning;
  end;
end;

procedure TFallbackMonitor.Stop;
begin
  if FPrimaryMonitor <> nil then
  begin
    FPrimaryMonitor.Stop;
    FPrimaryMonitor := nil;
  end;

  if FSecondaryMonitor <> nil then
  begin
    FSecondaryMonitor.Stop;
    FSecondaryMonitor := nil;
  end;

  FActiveMonitor := nil;
  LogDebug('Stopped', ClassName);
end;

function TFallbackMonitor.IsRunning: Boolean;
begin
  Result := (FActiveMonitor <> nil) and FActiveMonitor.IsRunning;
end;

procedure TFallbackMonitor.HandlePrimaryError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  LogDebug('Primary monitor error: %s, switching to secondary', [AMessage], ClassName);
  SwitchToSecondary;

  // Forward the error to external handler
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

procedure TFallbackMonitor.HandleDeviceStateChanged(Sender: TObject;
  const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, ADeviceAddress, ANewState);
end;

procedure TFallbackMonitor.HandleDeviceDiscovered(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  if Assigned(FOnDeviceDiscovered) then
    FOnDeviceDiscovered(Self, ADevice);
end;

procedure TFallbackMonitor.HandleDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
begin
  if Assigned(FOnDeviceOutOfRange) then
    FOnDeviceOutOfRange(Self, ADeviceAddress);
end;

procedure TFallbackMonitor.HandleSecondaryError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  // Secondary errors are just forwarded, no further fallback
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

procedure TFallbackMonitor.SwitchToSecondary;
begin
  // Stop primary if running
  if (FPrimaryMonitor <> nil) and FPrimaryMonitor.IsRunning then
    FPrimaryMonitor.Stop;

  // Start secondary monitor if available
  if FSecondaryMonitor <> nil then
  begin
    if FSecondaryMonitor.Start then
    begin
      FActiveMonitor := FSecondaryMonitor;
      LogDebug('Switched to secondary monitor', ClassName);
    end
    else
    begin
      LogDebug('Failed to start secondary monitor', ClassName);
      FActiveMonitor := nil;
    end;
  end
  else
  begin
    LogDebug('No secondary monitor available', ClassName);
    FActiveMonitor := nil;
  end;
end;

function TFallbackMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TFallbackMonitor.SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TFallbackMonitor.GetOnDeviceDiscovered: TMonitorDeviceDiscoveredEvent;
begin
  Result := FOnDeviceDiscovered;
end;

procedure TFallbackMonitor.SetOnDeviceDiscovered(AValue: TMonitorDeviceDiscoveredEvent);
begin
  FOnDeviceDiscovered := AValue;
end;

function TFallbackMonitor.GetOnDeviceOutOfRange: TMonitorDeviceOutOfRangeEvent;
begin
  Result := FOnDeviceOutOfRange;
end;

procedure TFallbackMonitor.SetOnDeviceOutOfRange(AValue: TMonitorDeviceOutOfRangeEvent);
begin
  FOnDeviceOutOfRange := AValue;
end;

function TFallbackMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TFallbackMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

{ TDeviceMonitorFactory }

constructor TDeviceMonitorFactory.Create(APollingConfig: IPollingConfig);
begin
  inherited Create;
  FPollingConfig := APollingConfig;
end;

function TDeviceMonitorFactory.CreateMonitor: IDeviceMonitor;
var
  PrimaryMonitor, SecondaryMonitor: IDeviceMonitor;
begin
  case FPollingConfig.PollingMode of
    pmDisabled:
      begin
        // Use device watcher only, no fallback
        LogDebug('Creating DeviceWatcherMonitor (polling disabled)', ClassName);
        Result := TDeviceWatcherMonitor.Create;
      end;
    pmFallback:
      begin
        // Use fallback monitor (watcher + polling fallback)
        LogDebug('Creating FallbackMonitor (watcher with polling fallback)', ClassName);
        PrimaryMonitor := TDeviceWatcherMonitor.Create;
        SecondaryMonitor := TPollingMonitor.Create(FPollingConfig.PollingInterval);
        Result := TFallbackMonitor.Create(PrimaryMonitor, SecondaryMonitor);
      end;
    pmPrimary:
      begin
        // Use polling as primary method
        LogDebug('Creating PollingMonitor (polling as primary, interval=%d)', [
          FPollingConfig.PollingInterval
        ], ClassName);
        Result := TPollingMonitor.Create(FPollingConfig.PollingInterval);
      end;
  else
    // Default to fallback mode
    PrimaryMonitor := TDeviceWatcherMonitor.Create;
    SecondaryMonitor := TPollingMonitor.Create(DEFAULT_POLLING_INTERVAL);
    Result := TFallbackMonitor.Create(PrimaryMonitor, SecondaryMonitor);
  end;
end;

end.
