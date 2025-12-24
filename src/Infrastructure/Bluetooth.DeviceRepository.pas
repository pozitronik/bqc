{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Repository Implementation                }
{                                                       }
{       Centralizes device cache and enumeration.       }
{                                                       }
{       Copyright (c) 2024                              }
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
  /// Repository for Bluetooth device storage and retrieval.
  /// Wraps internal cache and Windows API enumeration.
  /// </summary>
  TBluetoothDeviceRepository = class(TInterfacedObject, IDeviceRepository)
  private
    FDevices: TDictionary<UInt64, TBluetoothDeviceInfo>;
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
    constructor Create;
    destructor Destroy; override;
  end;

/// <summary>
/// Creates a device repository instance.
/// </summary>
function CreateDeviceRepository: IDeviceRepository;

implementation

uses
  App.Logger;

function CreateDeviceRepository: IDeviceRepository;
begin
  Result := TBluetoothDeviceRepository.Create;
end;

{ TBluetoothDeviceRepository }

constructor TBluetoothDeviceRepository.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
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
  SearchParams: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceInfo: BLUETOOTH_DEVICE_INFO;
  FindHandle: HBLUETOOTH_DEVICE_FIND;
  Device: TBluetoothDeviceInfo;
begin
  LogDebug('Refresh: Enumerating paired devices', ClassName);
  FDevices.Clear;

  InitDeviceSearchParams(SearchParams, 0);
  InitDeviceInfo(DeviceInfo);

  FindHandle := BluetoothFindFirstDevice(@SearchParams, DeviceInfo);

  if FindHandle <> 0 then
  begin
    try
      repeat
        Device := ConvertBluetoothDeviceInfo(DeviceInfo);
        FDevices.AddOrSetValue(Device.AddressInt, Device);
        DeviceInfo.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
      until not BluetoothFindNextDevice(FindHandle, DeviceInfo);
    finally
      BluetoothFindDeviceClose(FindHandle);
    end;
  end;

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
