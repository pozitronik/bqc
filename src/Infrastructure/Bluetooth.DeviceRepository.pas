{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Repository Implementation                }
{                                                       }
{       Centralizes device cache and enumeration.       }
{       Uses injected IBluetoothDeviceQuery for         }
{       Windows API abstraction (testability).          }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceRepository;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI,
  Bluetooth.DeviceConverter;

type
  /// <summary>
  /// Windows API implementation of IBluetoothDeviceQuery.
  /// Wraps BluetoothFindFirstDevice/Next/Close calls.
  /// </summary>
  TWindowsBluetoothDeviceQuery = class(TInterfacedObject, IBluetoothDeviceQuery)
  public
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;
  end;

  /// <summary>
  /// Repository for Bluetooth device storage and retrieval.
  /// Uses injected IBluetoothDeviceQuery for enumeration (testable).
  /// </summary>
  TBluetoothDeviceRepository = class(TInterfacedObject, IDeviceRepository)
  private
    FDevices: TDictionary<UInt64, TBluetoothDeviceInfo>;
    FDeviceQuery: IBluetoothDeviceQuery;
    FOnListChanged: TDeviceListChangedEvent;

    procedure DoListChanged;

  protected
    { IDeviceRepository }
    function GetAll: TBluetoothDeviceInfoArray;
    function GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    function TryGetByAddress(AAddress: UInt64; out ADevice: TBluetoothDeviceInfo): Boolean;
    function Contains(AAddress: UInt64): Boolean;
    procedure AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
    function UpdateConnectionState(AAddress: UInt64;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
    procedure Remove(AAddress: UInt64);
    procedure Clear;
    procedure Refresh;
    function GetCount: Integer;
    function GetOnListChanged: TDeviceListChangedEvent;
    procedure SetOnListChanged(AValue: TDeviceListChangedEvent);

  public
    constructor Create(ADeviceQuery: IBluetoothDeviceQuery);
    destructor Destroy; override;
  end;

/// <summary>
/// Creates a device repository with Windows API query implementation.
/// </summary>
function CreateDeviceRepository: IDeviceRepository;

/// <summary>
/// Creates the default Windows API device query.
/// </summary>
function CreateBluetoothDeviceQuery: IBluetoothDeviceQuery;

implementation

uses
  App.Logger;

function CreateBluetoothDeviceQuery: IBluetoothDeviceQuery;
begin
  Result := TWindowsBluetoothDeviceQuery.Create;
end;

function CreateDeviceRepository: IDeviceRepository;
begin
  Result := TBluetoothDeviceRepository.Create(CreateBluetoothDeviceQuery);
end;

{ TWindowsBluetoothDeviceQuery }

function TWindowsBluetoothDeviceQuery.EnumeratePairedDevices: TBluetoothDeviceInfoArray;
var
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  Device: TBluetoothDeviceInfo;
  DeviceList: TList<TBluetoothDeviceInfo>;
  LastError: DWORD;
begin
  LogDebug('EnumeratePairedDevices: Starting Windows API enumeration', ClassName);
  DeviceList := TList<TBluetoothDeviceInfo>.Create;
  try
    InitDeviceSearchParams(SearchParams, 0);
    InitDeviceInfo(DeviceInfo);

    FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);
    LastError := GetLastError;
    LogDebug('EnumeratePairedDevices: BluetoothFindFirstDevice returned handle=$%X, LastError=%d',
      [FindHandle, LastError], ClassName);

    if FindHandle <> 0 then
    begin
      try
        repeat
          Device := ConvertBluetoothDeviceInfo(DeviceInfo);
          LogDebug('EnumeratePairedDevices: Found device Address=$%.12X, Name="%s", Connected=%s, Remembered=%s, Authenticated=%s',
            [Device.AddressInt, Device.Name, BoolToStr(DeviceInfo.fConnected, True),
             BoolToStr(DeviceInfo.fRemembered, True), BoolToStr(DeviceInfo.fAuthenticated, True)], ClassName);
          DeviceList.Add(Device);
          DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
        until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
      finally
        BluetoothFindDeviceClose(FindHandle);
      end;
    end
    else
      LogWarning('EnumeratePairedDevices: BluetoothFindFirstDevice failed, no devices found', ClassName);

    Result := DeviceList.ToArray;
    LogDebug('EnumeratePairedDevices: Complete, found %d devices', [Length(Result)], ClassName);
  finally
    DeviceList.Free;
  end;
end;

{ TBluetoothDeviceRepository }

constructor TBluetoothDeviceRepository.Create(ADeviceQuery: IBluetoothDeviceQuery);
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  FDeviceQuery := ADeviceQuery;
end;

destructor TBluetoothDeviceRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

function TBluetoothDeviceRepository.GetAll: TBluetoothDeviceInfoArray;
begin
  Result := FDevices.Values.ToArray;
end;

function TBluetoothDeviceRepository.GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    FillChar(Result, SizeOf(Result), 0);
end;

function TBluetoothDeviceRepository.TryGetByAddress(AAddress: UInt64;
  out ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := FDevices.TryGetValue(AAddress, ADevice);
end;

function TBluetoothDeviceRepository.Contains(AAddress: UInt64): Boolean;
begin
  Result := FDevices.ContainsKey(AAddress);
end;

procedure TBluetoothDeviceRepository.AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
var
  IsNew: Boolean;
begin
  IsNew := not FDevices.ContainsKey(ADevice.AddressInt);
  FDevices.AddOrSetValue(ADevice.AddressInt, ADevice);

  if IsNew then
    DoListChanged;
end;

function TBluetoothDeviceRepository.UpdateConnectionState(AAddress: UInt64;
  AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Device: TBluetoothDeviceInfo;
begin
  if FDevices.TryGetValue(AAddress, Device) then
  begin
    Result := Device.WithConnectionState(AState);
    FDevices[AAddress] := Result;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TBluetoothDeviceRepository.Remove(AAddress: UInt64);
var
  Existed: Boolean;
begin
  Existed := FDevices.ContainsKey(AAddress);
  FDevices.Remove(AAddress);

  if Existed then
    DoListChanged;
end;

procedure TBluetoothDeviceRepository.Clear;
var
  HadDevices: Boolean;
begin
  HadDevices := FDevices.Count > 0;
  FDevices.Clear;

  if HadDevices then
    DoListChanged;
end;

procedure TBluetoothDeviceRepository.Refresh;
var
  Devices: TBluetoothDeviceInfoArray;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('Refresh: Enumerating paired devices', ClassName);
  FDevices.Clear;

  // Use injected query for enumeration (testable)
  Devices := FDeviceQuery.EnumeratePairedDevices;
  for Device in Devices do
    FDevices.AddOrSetValue(Device.AddressInt, Device);

  LogDebug('Refresh: Found %d paired devices', [FDevices.Count], ClassName);
  DoListChanged;
end;

function TBluetoothDeviceRepository.GetCount: Integer;
begin
  Result := FDevices.Count;
end;

procedure TBluetoothDeviceRepository.DoListChanged;
begin
  if Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

function TBluetoothDeviceRepository.GetOnListChanged: TDeviceListChangedEvent;
begin
  Result := FOnListChanged;
end;

procedure TBluetoothDeviceRepository.SetOnListChanged(AValue: TDeviceListChangedEvent);
begin
  FOnListChanged := AValue;
end;

end.
