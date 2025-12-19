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
  System.Generics.Collections,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.ConnectionStrategies;

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
  System.DateUtils;

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
end;

destructor TBluetoothService.Destroy;
begin
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
    DoDeviceListChanged;

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

end.
