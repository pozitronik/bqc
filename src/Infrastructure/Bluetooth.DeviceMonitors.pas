{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Monitor Implementations                  }
{                                                       }
{       Implements IDeviceMonitor for various           }
{       monitoring strategies (polling, watcher).       }
{                                                       }
{       Copyright (c) 2024                              }
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
  App.ConfigInterfaces;

type
  /// <summary>
  /// Polling-based device monitor implementation.
  /// Uses a timer to periodically check device connection states.
  /// </summary>
  TPollingMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
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
    FOnError: TMonitorErrorEvent;
    FWatcher: TBluetoothDeviceWatcher;
    FRunning: Boolean;

    { Event handlers for TBluetoothDeviceWatcher }
    procedure HandleWatcherDeviceConnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherDeviceDisconnected(Sender: TObject;
      const ADeviceAddress: UInt64; AConnected: Boolean);
    procedure HandleWatcherError(Sender: TObject;
      const AMessage: string; AErrorCode: DWORD);

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

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Fallback monitor that tries device watcher first, then falls back to polling.
  /// Composite pattern: combines two monitoring strategies.
  /// </summary>
  TFallbackMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnError: TMonitorErrorEvent;
    FPrimaryMonitor: IDeviceMonitor;
    FSecondaryMonitor: IDeviceMonitor;
    FActiveMonitor: IDeviceMonitor;
    FPollingInterval: Integer;

    procedure HandlePrimaryError(Sender: TObject;
      const AMessage: string; AErrorCode: Cardinal);
    procedure HandleDeviceStateChanged(Sender: TObject;
      const ADeviceAddress: UInt64; ANewState: TBluetoothConnectionState);
    procedure HandleError(Sender: TObject;
      const AMessage: string; AErrorCode: Cardinal);

    procedure SwitchToSecondary;

  protected
    { IDeviceMonitor }
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);

  public
    constructor Create(APollingInterval: Integer);
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

function TPollingMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
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

function TDeviceWatcherMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TDeviceWatcherMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

{ TFallbackMonitor }

constructor TFallbackMonitor.Create(APollingInterval: Integer);
begin
  inherited Create;
  FPollingInterval := APollingInterval;
  FPrimaryMonitor := nil;
  FSecondaryMonitor := nil;
  FActiveMonitor := nil;
end;

destructor TFallbackMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TFallbackMonitor.Start: Boolean;
begin
  if IsRunning then
    Exit(True);

  // Create primary monitor (device watcher)
  FPrimaryMonitor := TDeviceWatcherMonitor.Create;
  FPrimaryMonitor.OnDeviceStateChanged := HandleDeviceStateChanged;
  FPrimaryMonitor.OnError := HandlePrimaryError;

  // Try to start primary monitor
  if FPrimaryMonitor.Start then
  begin
    FActiveMonitor := FPrimaryMonitor;
    LogDebug('Started with device watcher (primary)', ClassName);
    Result := True;
  end
  else
  begin
    // Primary failed, switch to secondary
    LogDebug('Device watcher failed, switching to polling', ClassName);
    FPrimaryMonitor := nil;
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

procedure TFallbackMonitor.HandleError(Sender: TObject;
  const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

procedure TFallbackMonitor.SwitchToSecondary;
begin
  // Stop primary if running
  if (FPrimaryMonitor <> nil) and FPrimaryMonitor.IsRunning then
    FPrimaryMonitor.Stop;

  // Create and start secondary monitor (polling)
  if FSecondaryMonitor = nil then
  begin
    FSecondaryMonitor := TPollingMonitor.Create(FPollingInterval);
    FSecondaryMonitor.OnDeviceStateChanged := HandleDeviceStateChanged;
    FSecondaryMonitor.OnError := HandleError;
  end;

  if FSecondaryMonitor.Start then
  begin
    FActiveMonitor := FSecondaryMonitor;
    LogDebug('Switched to polling monitor (interval=%d ms)', [FPollingInterval], ClassName);
  end
  else
  begin
    LogDebug('Failed to start polling monitor', ClassName);
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
        Result := TFallbackMonitor.Create(FPollingConfig.PollingInterval);
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
    Result := TFallbackMonitor.Create(DEFAULT_POLLING_INTERVAL);
  end;
end;

end.
