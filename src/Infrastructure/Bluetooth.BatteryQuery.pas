{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Level Query via WinRT GATT              }
{                                                       }
{       Uses Windows.Devices.Bluetooth.                 }
{       GenericAttributeProfile API to read battery     }
{       level from BLE devices.                         }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.BatteryQuery;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces;

type
  /// <summary>
  /// Implementation of IBatteryQuery using WinRT GATT APIs.
  /// Queries the standard Bluetooth Low Energy Battery Service (0x180F).
  /// </summary>
  TBatteryQuery = class(TInterfacedObject, IBatteryQuery)
  private const
    DEFAULT_TIMEOUT_MS = 5000;
  public
    /// <summary>
    /// Gets the battery level for a Bluetooth device.
    /// Uses default timeout of 5 seconds.
    /// </summary>
    function GetBatteryLevel(ADeviceAddress: UInt64): TBatteryStatus;

    /// <summary>
    /// Gets the battery level with a custom timeout.
    /// </summary>
    function GetBatteryLevelWithTimeout(ADeviceAddress: UInt64;
      ATimeoutMs: Cardinal): TBatteryStatus;
  end;

  /// <summary>
  /// Implementation of IBatteryCache for caching battery levels.
  /// Thread-safe cache with async refresh support.
  /// </summary>
  TBatteryCache = class(TInterfacedObject, IBatteryCache)
  private
    FCache: TDictionary<UInt64, TBatteryStatus>;
    FBatteryQuery: IBatteryQuery;
    FOnQueryCompleted: TBatteryQueryCompletedEvent;
    FLock: TCriticalSection;
    FShuttingDown: Integer;   // Atomic flag: 0 = running, 1 = shutting down
    FPendingQueries: Integer; // Atomic counter of in-flight background queries
  public
    constructor Create(ABatteryQuery: IBatteryQuery);
    destructor Destroy; override;

    function GetBatteryStatus(ADeviceAddress: UInt64): TBatteryStatus;
    procedure SetBatteryStatus(ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
    function HasCachedStatus(ADeviceAddress: UInt64): Boolean;
    procedure RequestRefresh(ADeviceAddress: UInt64);
    procedure RequestRefreshAll(const ADeviceAddresses: TArray<UInt64>);
    procedure Clear;
    procedure Remove(ADeviceAddress: UInt64);

    function GetOnQueryCompleted: TBatteryQueryCompletedEvent;
    procedure SetOnQueryCompleted(AValue: TBatteryQueryCompletedEvent);
    property OnQueryCompleted: TBatteryQueryCompletedEvent
      read GetOnQueryCompleted write SetOnQueryCompleted;
  end;

/// <summary>
/// Creates a battery query instance.
/// Factory function for dependency injection.
/// </summary>
function CreateBatteryQuery: IBatteryQuery;

/// <summary>
/// Creates a battery cache instance.
/// Factory function for dependency injection.
/// </summary>
function CreateBatteryCache(ABatteryQuery: IBatteryQuery): IBatteryCache;

/// <summary>
/// Queries battery level for a Bluetooth device.
/// Standalone function for simple use cases.
/// </summary>
/// <param name="ADeviceAddress">64-bit Bluetooth address.</param>
/// <param name="ATimeoutMs">Timeout in milliseconds.</param>
/// <returns>Battery status.</returns>
function QueryBluetoothBattery(ADeviceAddress: UInt64;
  ATimeoutMs: Cardinal = 5000): TBatteryStatus;

implementation

uses
  Winapi.ActiveX,
  App.Logger;

const
  // WinRT initialization
  RO_INIT_MULTITHREADED = 1;

  // Async operation status
  AsyncStatus_Started   = 0;
  AsyncStatus_Completed = 1;
  AsyncStatus_Canceled  = 2;
  AsyncStatus_Error     = 3;

  // GATT communication status
  GattCommunicationStatus_Success      = 0;
  GattCommunicationStatus_Unreachable  = 1;
  GattCommunicationStatus_ProtocolError = 2;
  GattCommunicationStatus_AccessDenied = 3;

  // BluetoothCacheMode
  BluetoothCacheMode_Cached      = 0;
  BluetoothCacheMode_Uncached    = 1;

  // Runtime class names
  RuntimeClass_BluetoothLEDevice: string = 'Windows.Devices.Bluetooth.BluetoothLEDevice';

  // Standard Bluetooth GATT UUIDs
  // Battery Service: 0x180F
  BATTERY_SERVICE_UUID: TGUID = '{0000180F-0000-1000-8000-00805F9B34FB}';
  // Battery Level Characteristic: 0x2A19
  BATTERY_LEVEL_UUID: TGUID = '{00002A19-0000-1000-8000-00805F9B34FB}';

  // Log source
  LOG_SOURCE = 'BatteryQuery';

  // SetupAPI constants
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_DEVICEINTERFACE = $00000010;

  // Device property types
  DEVPROP_TYPE_BYTE = $00000003;

  // Bluetooth device interface GUIDs
  GUID_BTHPORT_DEVICE_INTERFACE: TGUID = '{0850302A-B344-4FDA-9BE9-90576B8D46F0}';
  // Bluetooth device class GUID
  GUID_DEVCLASS_BLUETOOTH: TGUID = '{E0CBF06C-CD8B-4647-BB8A-263B43F0F974}';

type
  HSTRING = type THandle;

  // SetupAPI types
  HDEVINFO = THandle;
  DEVPROPTYPE = Cardinal;

  DEVPROPKEY = record
    fmtid: TGUID;
    pid: Cardinal;
  end;

  SP_DEVINFO_DATA = record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD;
    Reserved: ULONG_PTR;
  end;
  PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;

  SP_DEVICE_INTERFACE_DATA = record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  PSP_DEVICE_INTERFACE_DATA = ^SP_DEVICE_INTERFACE_DATA;

  SP_DEVICE_INTERFACE_DETAIL_DATA_W = record
    cbSize: DWORD;
    DevicePath: array[0..0] of WideChar;
  end;
  PSP_DEVICE_INTERFACE_DETAIL_DATA_W = ^SP_DEVICE_INTERFACE_DETAIL_DATA_W;

const
  // DEVPKEY_Bluetooth_BatteryPercent = {104EA319-6EE2-4701-BD47-8DDBF425BBE5}, 2
  DEVPKEY_Bluetooth_BatteryPercent: DEVPROPKEY = (
    fmtid: '{104EA319-6EE2-4701-BD47-8DDBF425BBE5}';
    pid: 2
  );

  // Alternative: PKEY_Devices_BatteryPercent = {49CD1F76-5626-4B17-A4E8-18B4AA1A2213}, 2
  PKEY_Devices_BatteryPercent: DEVPROPKEY = (
    fmtid: '{49CD1F76-5626-4B17-A4E8-18B4AA1A2213}';
    pid: 2
  );

  // Another alternative: DEVPKEY_Bluetooth_Battery = {2BD67D8B-8BEB-48D5-87E0-6CDA3428040A}, 4
  DEVPKEY_Bluetooth_Battery: DEVPROPKEY = (
    fmtid: '{2BD67D8B-8BEB-48D5-87E0-6CDA3428040A}';
    pid: 4
  );

// SetupAPI function imports
function SetupDiGetClassDevsW(ClassGuid: PGUID; Enumerator: PWideChar;
  hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
  external 'setupapi.dll' name 'SetupDiGetClassDevsW';

function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSP_DEVINFO_DATA; InterfaceClassGuid: PGUID;
  MemberIndex: DWORD; var DeviceInterfaceData: SP_DEVICE_INTERFACE_DATA): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiEnumDeviceInterfaces';

function SetupDiGetDeviceInterfaceDetailW(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA;
  DeviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA_W;
  DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
  DeviceInfoData: PSP_DEVINFO_DATA): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiGetDeviceInterfaceDetailW';

function SetupDiGetDevicePropertyW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSP_DEVINFO_DATA; const PropertyKey: DEVPROPKEY;
  var PropertyType: DEVPROPTYPE; PropertyBuffer: PByte;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD; Flags: DWORD): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiGetDevicePropertyW';

function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiDestroyDeviceInfoList';

function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD;
  var DeviceInfoData: SP_DEVINFO_DATA): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiEnumDeviceInfo';

function SetupDiGetDeviceInstanceIdW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSP_DEVINFO_DATA; DeviceInstanceId: PWideChar;
  DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  external 'setupapi.dll' name 'SetupDiGetDeviceInstanceIdW';

type
  // Forward declarations
  IBluetoothLEDevice = interface;
  IGattDeviceService = interface;
  IGattCharacteristic = interface;

  /// <summary>
  /// Base WinRT interface.
  /// </summary>
  IInspectable = interface(IUnknown)
    ['{AF86E2E0-B12D-4C6A-9C5A-D7AA65101E90}']
    function GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT; stdcall;
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    function GetTrustLevel(out trustLevel: Integer): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation info interface.
  /// </summary>
  IAsyncInfo = interface(IInspectable)
    ['{00000036-0000-0000-C000-000000000046}']
    function get_Id(out id: Cardinal): HRESULT; stdcall;
    function get_Status(out status: Integer): HRESULT; stdcall;
    function get_ErrorCode(out errorCode: HRESULT): HRESULT; stdcall;
    function Cancel: HRESULT; stdcall;
    function Close: HRESULT; stdcall;
  end;

  /// <summary>
  /// Generic async operation completed handler (not used but required for interface).
  /// </summary>
  IAsyncOperationCompletedHandler = interface(IUnknown)
  end;

  /// <summary>
  /// Async operation for BluetoothLEDevice.
  /// </summary>
  IAsyncOperationBluetoothLEDevice = interface(IInspectable)
    ['{375F9D67-74A2-5F91-A11D-169093718D41}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IBluetoothLEDevice): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT device services result.
  /// </summary>
  IGattDeviceServicesResult = interface(IInspectable)
    ['{171DD3EE-016D-419D-838A-576CF475A3D8}']
    function get_Status(out value: Integer): HRESULT; stdcall;
    function get_ProtocolError(out value: Byte): HRESULT; stdcall;
    function get_Services(out value: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation for GATT services result.
  /// </summary>
  IAsyncOperationGattDeviceServicesResult = interface(IInspectable)
    ['{E7E0F56E-B6A3-5B3F-9C41-2A5E2C960A07}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IGattDeviceServicesResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT characteristics result.
  /// </summary>
  IGattCharacteristicsResult = interface(IInspectable)
    ['{1194945C-B257-4F3E-9DB7-F68BC9A9AEF2}']
    function get_Status(out value: Integer): HRESULT; stdcall;
    function get_ProtocolError(out value: Byte): HRESULT; stdcall;
    function get_Characteristics(out value: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation for GATT characteristics result.
  /// </summary>
  IAsyncOperationGattCharacteristicsResult = interface(IInspectable)
    ['{C72ADDD2-E4E2-5902-BB09-195A6A082B96}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IGattCharacteristicsResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Windows.Storage.Streams.IBuffer for reading data.
  /// </summary>
  IBuffer = interface(IInspectable)
    ['{905A0FE0-BC53-11DF-8C49-001E4FC686DA}']
    function get_Capacity(out value: Cardinal): HRESULT; stdcall;
    function get_Length(out value: Cardinal): HRESULT; stdcall;
    function put_Length(value: Cardinal): HRESULT; stdcall;
  end;

  /// <summary>
  /// IBufferByteAccess for direct buffer access.
  /// </summary>
  IBufferByteAccess = interface(IUnknown)
    ['{905A0FEF-BC53-11DF-8C49-001E4FC686DA}']
    function Buffer(out value: PByte): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT read result.
  /// </summary>
  IGattReadResult = interface(IInspectable)
    ['{63A66F08-1ABA-441E-92F3-3B0B0FD4C2FA}']
    function get_Status(out value: Integer): HRESULT; stdcall;
    function get_Value(out value: IBuffer): HRESULT; stdcall;
  end;

  /// <summary>
  /// Async operation for GATT read result.
  /// </summary>
  IAsyncOperationGattReadResult = interface(IInspectable)
    ['{D40432A8-1E14-51D0-B49B-AE2CE1AA05E5}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IGattReadResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// Vector view for GATT services.
  /// </summary>
  IVectorViewGattDeviceService = interface(IInspectable)
    ['{D3EC7DEA-5E3C-5B29-9F83-70A1E11CE932}']
    function GetAt(index: Cardinal; out item: IGattDeviceService): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
    function IndexOf(item: IGattDeviceService; out index: Cardinal; out found: Boolean): HRESULT; stdcall;
    function GetMany(startIndex: Cardinal; capacity: Cardinal; out items: IGattDeviceService;
      out actual: Cardinal): HRESULT; stdcall;
  end;

  /// <summary>
  /// Vector view for GATT characteristics.
  /// </summary>
  IVectorViewGattCharacteristic = interface(IInspectable)
    ['{E3C56728-7F2D-5A0D-AD38-030D39C60F9F}']
    function GetAt(index: Cardinal; out item: IGattCharacteristic): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
    function IndexOf(item: IGattCharacteristic; out index: Cardinal; out found: Boolean): HRESULT; stdcall;
    function GetMany(startIndex: Cardinal; capacity: Cardinal; out items: IGattCharacteristic;
      out actual: Cardinal): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT device service interface.
  /// </summary>
  IGattDeviceService = interface(IInspectable)
    ['{AC7B7C05-B33C-47CF-990F-6B8F5577DF71}']
    function GetCharacteristics(characteristicUuid: TGUID;
      out value: IInspectable): HRESULT; stdcall;
    function GetIncludedServices(serviceUuid: TGUID;
      out value: IInspectable): HRESULT; stdcall;
    function get_DeviceId(out value: HSTRING): HRESULT; stdcall;
    function get_Uuid(out value: TGUID): HRESULT; stdcall;
    function get_AttributeHandle(out value: Word): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT device service 3 (for GetCharacteristicsForUuidAsync).
  /// </summary>
  IGattDeviceService3 = interface(IInspectable)
    ['{B293A950-0C53-437C-A9B3-5C3210C6E569}']
    function get_DeviceAccessInformation(out value: IInspectable): HRESULT; stdcall;
    function get_Session(out value: IInspectable): HRESULT; stdcall;
    function get_SharingMode(out value: Integer): HRESULT; stdcall;
    function RequestAccessAsync(out operation: IInspectable): HRESULT; stdcall;
    function OpenAsync(sharingMode: Integer; out operation: IInspectable): HRESULT; stdcall;
    function GetCharacteristicsAsync(out operation: IAsyncOperationGattCharacteristicsResult): HRESULT; stdcall;
    function GetCharacteristicsForUuidAsync(characteristicUuid: TGUID;
      out operation: IAsyncOperationGattCharacteristicsResult): HRESULT; stdcall;
    function GetCharacteristicsWithCacheModeAsync(cacheMode: Integer;
      out operation: IAsyncOperationGattCharacteristicsResult): HRESULT; stdcall;
    function GetCharacteristicsForUuidWithCacheModeAsync(characteristicUuid: TGUID;
      cacheMode: Integer; out operation: IAsyncOperationGattCharacteristicsResult): HRESULT; stdcall;
    function GetIncludedServicesAsync(out operation: IInspectable): HRESULT; stdcall;
    function GetIncludedServicesForUuidAsync(serviceUuid: TGUID;
      out operation: IInspectable): HRESULT; stdcall;
    function GetIncludedServicesWithCacheModeAsync(cacheMode: Integer;
      out operation: IInspectable): HRESULT; stdcall;
    function GetIncludedServicesForUuidWithCacheModeAsync(serviceUuid: TGUID;
      cacheMode: Integer; out operation: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// GATT characteristic interface.
  /// </summary>
  IGattCharacteristic = interface(IInspectable)
    ['{59CB50C1-5934-4F68-A198-EB864FA44E6B}']
    function GetDescriptors(descriptorUuid: TGUID; out value: IInspectable): HRESULT; stdcall;
    function get_CharacteristicProperties(out value: Cardinal): HRESULT; stdcall;
    function get_ProtectionLevel(out value: Integer): HRESULT; stdcall;
    function put_ProtectionLevel(value: Integer): HRESULT; stdcall;
    function get_UserDescription(out value: HSTRING): HRESULT; stdcall;
    function get_Uuid(out value: TGUID): HRESULT; stdcall;
    function get_AttributeHandle(out value: Word): HRESULT; stdcall;
    function get_PresentationFormats(out value: IInspectable): HRESULT; stdcall;
    function ReadValueAsync(out asyncOp: IAsyncOperationGattReadResult): HRESULT; stdcall;
    function ReadValueWithCacheModeAsync(cacheMode: Integer;
      out asyncOp: IAsyncOperationGattReadResult): HRESULT; stdcall;
    function WriteValueAsync(value: IBuffer; out asyncOp: IInspectable): HRESULT; stdcall;
    function WriteValueWithOptionAsync(value: IBuffer; writeOption: Integer;
      out asyncOp: IInspectable): HRESULT; stdcall;
    function ReadClientCharacteristicConfigurationDescriptorAsync(
      out asyncOp: IInspectable): HRESULT; stdcall;
    function WriteClientCharacteristicConfigurationDescriptorAsync(
      clientCharacteristicConfigurationDescriptorValue: Integer;
      out asyncOp: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothLEDevice interface.
  /// </summary>
  IBluetoothLEDevice = interface(IInspectable)
    ['{B5EE2F7B-4AD8-4642-AC48-80A0B500E887}']
    function get_DeviceId(out value: HSTRING): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_GattServices(out value: IInspectable): HRESULT; stdcall;
    function get_ConnectionStatus(out value: Integer): HRESULT; stdcall;
    function get_BluetoothAddress(out value: UInt64): HRESULT; stdcall;
    function GetGattService(serviceUuid: TGUID; out service: IGattDeviceService): HRESULT; stdcall;
    function add_NameChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_NameChanged(token: Int64): HRESULT; stdcall;
    function add_GattServicesChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_GattServicesChanged(token: Int64): HRESULT; stdcall;
    function add_ConnectionStatusChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_ConnectionStatusChanged(token: Int64): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothLEDevice3 interface for GetGattServicesForUuidAsync.
  /// </summary>
  IBluetoothLEDevice3 = interface(IInspectable)
    ['{AEE9E493-44AC-40DC-AF33-B2C13C01CA46}']
    function get_DeviceAccessInformation(out value: IInspectable): HRESULT; stdcall;
    function RequestAccessAsync(out operation: IInspectable): HRESULT; stdcall;
    function GetGattServicesAsync(out operation: IAsyncOperationGattDeviceServicesResult): HRESULT; stdcall;
    function GetGattServicesWithCacheModeAsync(cacheMode: Integer;
      out operation: IAsyncOperationGattDeviceServicesResult): HRESULT; stdcall;
    function GetGattServicesForUuidAsync(serviceUuid: TGUID;
      out operation: IAsyncOperationGattDeviceServicesResult): HRESULT; stdcall;
    function GetGattServicesForUuidWithCacheModeAsync(serviceUuid: TGUID;
      cacheMode: Integer; out operation: IAsyncOperationGattDeviceServicesResult): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothLEDevice statics for FromBluetoothAddressAsync.
  /// </summary>
  IBluetoothLEDeviceStatics = interface(IInspectable)
    ['{C8CF1A19-F0B6-4BF0-8689-41303DE2D9F4}']
    function FromIdAsync(deviceId: HSTRING; out operation: IAsyncOperationBluetoothLEDevice): HRESULT; stdcall;
    function FromBluetoothAddressAsync(bluetoothAddress: UInt64;
      out operation: IAsyncOperationBluetoothLEDevice): HRESULT; stdcall;
    function GetDeviceSelector(out selector: HSTRING): HRESULT; stdcall;
  end;

// External WinRT functions
function WindowsCreateString(sourceString: PWideChar; length: Cardinal;
  out str: HSTRING): HRESULT; stdcall; external 'combase.dll';

function WindowsDeleteString(str: HSTRING): HRESULT; stdcall;
  external 'combase.dll';

function RoInitialize(initType: Cardinal): HRESULT; stdcall;
  external 'combase.dll';

function RoGetActivationFactory(activatableClassId: HSTRING; const iid: TGUID;
  out factory: IInspectable): HRESULT; stdcall; external 'combase.dll';

/// <summary>
/// Creates an HSTRING from a Delphi string.
/// </summary>
function CreateHString(const AStr: string): HSTRING;
begin
  Result := 0;
  if AStr <> '' then
    WindowsCreateString(PWideChar(AStr), Length(AStr), Result);
end;

/// <summary>
/// Frees an HSTRING.
/// </summary>
procedure FreeHString(AStr: HSTRING);
begin
  if AStr <> 0 then
    WindowsDeleteString(AStr);
end;

/// <summary>
/// Waits for an async operation to complete with polling.
/// </summary>
function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo;
  ATimeoutMs: Cardinal): Boolean;
var
  Status: Integer;
  StartTime: Cardinal;
begin
  Result := False;
  if AAsyncInfo = nil then
  begin
    LogDebug('WaitForAsyncOperation: AAsyncInfo is nil', LOG_SOURCE);
    Exit;
  end;

  StartTime := GetTickCount;
  repeat
    if Failed(AAsyncInfo.get_Status(Status)) then
    begin
      LogDebug('WaitForAsyncOperation: get_Status failed', LOG_SOURCE);
      Exit;
    end;

    if Status <> AsyncStatus_Started then
    begin
      Result := (Status = AsyncStatus_Completed);
      if not Result then
        LogDebug('WaitForAsyncOperation: Async status=%d (not completed)', [Status], LOG_SOURCE);
      Exit;
    end;

    Sleep(10);
  until (GetTickCount - StartTime) > ATimeoutMs;

  LogDebug('WaitForAsyncOperation: Timeout after %dms', [ATimeoutMs], LOG_SOURCE);
end;

/// <summary>
/// Gets BluetoothLEDevice from Bluetooth address.
/// </summary>
function GetBluetoothLEDevice(AAddress: UInt64; ATimeoutMs: Cardinal;
  out ADevice: IBluetoothLEDevice): Boolean;
var
  HR: HRESULT;
  ClassName: HSTRING;
  Factory: IInspectable;
  Statics: IBluetoothLEDeviceStatics;
  AsyncOp: IAsyncOperationBluetoothLEDevice;
  AsyncInfo: IAsyncInfo;
begin
  Result := False;
  ADevice := nil;

  HR := RoInitialize(RO_INIT_MULTITHREADED);
  if Failed(HR) and (HR <> RPC_E_CHANGED_MODE) then
  begin
    LogDebug('RoInitialize failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  ClassName := CreateHString(RuntimeClass_BluetoothLEDevice);
  if ClassName = 0 then
  begin
    LogDebug('Failed to create HSTRING for class name', LOG_SOURCE);
    Exit;
  end;

  try
    HR := RoGetActivationFactory(ClassName, IBluetoothLEDeviceStatics, Factory);
    if Failed(HR) or (Factory = nil) then
    begin
      LogDebug('RoGetActivationFactory failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    HR := Factory.QueryInterface(IBluetoothLEDeviceStatics, Statics);
    if Failed(HR) or (Statics = nil) then
    begin
      LogDebug('QueryInterface for statics failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    HR := Statics.FromBluetoothAddressAsync(AAddress, AsyncOp);
    if Failed(HR) or (AsyncOp = nil) then
    begin
      LogDebug('FromBluetoothAddressAsync failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
    begin
      LogDebug('Failed to get IAsyncInfo', LOG_SOURCE);
      Exit;
    end;

    if not WaitForAsyncOperation(AsyncInfo, ATimeoutMs) then
    begin
      LogDebug('Async operation timed out or failed', LOG_SOURCE);
      Exit;
    end;

    HR := AsyncOp.GetResults(ADevice);
    if Failed(HR) or (ADevice = nil) then
    begin
      LogDebug('GetResults failed: 0x%.8X (Device=%p) - Device may be Classic Bluetooth, not BLE', [HR, Pointer(ADevice)], LOG_SOURCE);
      Exit;
    end;

    Result := True;
    LogDebug('Successfully got BluetoothLEDevice for address $%.12X', [AAddress], LOG_SOURCE);
  finally
    FreeHString(ClassName);
  end;
end;

/// <summary>
/// Gets the Battery Service from a BluetoothLEDevice.
/// </summary>
function GetBatteryService(const ADevice: IBluetoothLEDevice; ATimeoutMs: Cardinal;
  out AService: IGattDeviceService): Boolean;
var
  HR: HRESULT;
  Device3: IBluetoothLEDevice3;
  AsyncOp: IAsyncOperationGattDeviceServicesResult;
  AsyncInfo: IAsyncInfo;
  ServicesResult: IGattDeviceServicesResult;
  Status: Integer;
  Services: IInspectable;
  ServicesView: IVectorViewGattDeviceService;
  Count: Cardinal;
begin
  Result := False;
  AService := nil;

  // Get IBluetoothLEDevice3 for GetGattServicesForUuidAsync
  HR := ADevice.QueryInterface(IBluetoothLEDevice3, Device3);
  if Failed(HR) or (Device3 = nil) then
  begin
    LogDebug('QueryInterface for IBluetoothLEDevice3 failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  // Query for Battery Service specifically
  HR := Device3.GetGattServicesForUuidAsync(BATTERY_SERVICE_UUID, AsyncOp);
  if Failed(HR) or (AsyncOp = nil) then
  begin
    LogDebug('GetGattServicesForUuidAsync failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
  begin
    LogDebug('Failed to get IAsyncInfo for services', LOG_SOURCE);
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, ATimeoutMs) then
  begin
    LogDebug('Services async operation timed out', LOG_SOURCE);
    Exit;
  end;

  HR := AsyncOp.GetResults(ServicesResult);
  if Failed(HR) or (ServicesResult = nil) then
  begin
    LogDebug('GetResults for services failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := ServicesResult.get_Status(Status);
  if Failed(HR) or (Status <> GattCommunicationStatus_Success) then
  begin
    LogDebug('GATT services status: %d', [Status], LOG_SOURCE);
    Exit;
  end;

  HR := ServicesResult.get_Services(Services);
  if Failed(HR) or (Services = nil) then
  begin
    LogDebug('get_Services failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := Services.QueryInterface(IVectorViewGattDeviceService, ServicesView);
  if Failed(HR) or (ServicesView = nil) then
  begin
    LogDebug('QueryInterface for services view failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := ServicesView.get_Size(Count);
  if Failed(HR) or (Count = 0) then
  begin
    LogDebug('No Battery Service found (count=%d)', [Count], LOG_SOURCE);
    Exit;
  end;

  HR := ServicesView.GetAt(0, AService);
  if Failed(HR) or (AService = nil) then
  begin
    LogDebug('GetAt for service failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  Result := True;
  LogDebug('Found Battery Service', LOG_SOURCE);
end;

/// <summary>
/// Gets the Battery Level characteristic from the Battery Service.
/// </summary>
function GetBatteryLevelCharacteristic(const AService: IGattDeviceService;
  ATimeoutMs: Cardinal; out ACharacteristic: IGattCharacteristic): Boolean;
var
  HR: HRESULT;
  Service3: IGattDeviceService3;
  AsyncOp: IAsyncOperationGattCharacteristicsResult;
  AsyncInfo: IAsyncInfo;
  CharsResult: IGattCharacteristicsResult;
  Status: Integer;
  Chars: IInspectable;
  CharsView: IVectorViewGattCharacteristic;
  Count: Cardinal;
begin
  Result := False;
  ACharacteristic := nil;

  HR := AService.QueryInterface(IGattDeviceService3, Service3);
  if Failed(HR) or (Service3 = nil) then
  begin
    LogDebug('QueryInterface for IGattDeviceService3 failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := Service3.GetCharacteristicsForUuidAsync(BATTERY_LEVEL_UUID, AsyncOp);
  if Failed(HR) or (AsyncOp = nil) then
  begin
    LogDebug('GetCharacteristicsForUuidAsync failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
  begin
    LogDebug('Failed to get IAsyncInfo for characteristics', LOG_SOURCE);
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, ATimeoutMs) then
  begin
    LogDebug('Characteristics async operation timed out', LOG_SOURCE);
    Exit;
  end;

  HR := AsyncOp.GetResults(CharsResult);
  if Failed(HR) or (CharsResult = nil) then
  begin
    LogDebug('GetResults for characteristics failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := CharsResult.get_Status(Status);
  if Failed(HR) or (Status <> GattCommunicationStatus_Success) then
  begin
    LogDebug('GATT characteristics status: %d', [Status], LOG_SOURCE);
    Exit;
  end;

  HR := CharsResult.get_Characteristics(Chars);
  if Failed(HR) or (Chars = nil) then
  begin
    LogDebug('get_Characteristics failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := Chars.QueryInterface(IVectorViewGattCharacteristic, CharsView);
  if Failed(HR) or (CharsView = nil) then
  begin
    LogDebug('QueryInterface for characteristics view failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := CharsView.get_Size(Count);
  if Failed(HR) or (Count = 0) then
  begin
    LogDebug('No Battery Level characteristic found (count=%d)', [Count], LOG_SOURCE);
    Exit;
  end;

  HR := CharsView.GetAt(0, ACharacteristic);
  if Failed(HR) or (ACharacteristic = nil) then
  begin
    LogDebug('GetAt for characteristic failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  Result := True;
  LogDebug('Found Battery Level characteristic', LOG_SOURCE);
end;

/// <summary>
/// Reads the battery level value from the characteristic.
/// </summary>
function ReadBatteryLevel(const ACharacteristic: IGattCharacteristic;
  ATimeoutMs: Cardinal; out ALevel: Integer): Boolean;
var
  HR: HRESULT;
  AsyncOp: IAsyncOperationGattReadResult;
  AsyncInfo: IAsyncInfo;
  ReadResult: IGattReadResult;
  Status: Integer;
  Buffer: IBuffer;
  BufferAccess: IBufferByteAccess;
  DataPtr: PByte;
  Length: Cardinal;
begin
  Result := False;
  ALevel := -1;

  HR := ACharacteristic.ReadValueAsync(AsyncOp);
  if Failed(HR) or (AsyncOp = nil) then
  begin
    LogDebug('ReadValueAsync failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
  begin
    LogDebug('Failed to get IAsyncInfo for read', LOG_SOURCE);
    Exit;
  end;

  if not WaitForAsyncOperation(AsyncInfo, ATimeoutMs) then
  begin
    LogDebug('Read async operation timed out', LOG_SOURCE);
    Exit;
  end;

  HR := AsyncOp.GetResults(ReadResult);
  if Failed(HR) or (ReadResult = nil) then
  begin
    LogDebug('GetResults for read failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := ReadResult.get_Status(Status);
  if Failed(HR) or (Status <> GattCommunicationStatus_Success) then
  begin
    LogDebug('GATT read status: %d', [Status], LOG_SOURCE);
    Exit;
  end;

  HR := ReadResult.get_Value(Buffer);
  if Failed(HR) or (Buffer = nil) then
  begin
    LogDebug('get_Value failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := Buffer.get_Length(Length);
  if Failed(HR) or (Length = 0) then
  begin
    LogDebug('Buffer length is 0', LOG_SOURCE);
    Exit;
  end;

  HR := Buffer.QueryInterface(IBufferByteAccess, BufferAccess);
  if Failed(HR) or (BufferAccess = nil) then
  begin
    LogDebug('QueryInterface for IBufferByteAccess failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  HR := BufferAccess.Buffer(DataPtr);
  if Failed(HR) or (DataPtr = nil) then
  begin
    LogDebug('Buffer access failed: 0x%.8X', [HR], LOG_SOURCE);
    Exit;
  end;

  // Battery level is a single byte (0-100)
  ALevel := DataPtr^;
  Result := True;
  LogDebug('Battery level: %d%%', [ALevel], LOG_SOURCE);
end;

/// <summary>
/// Extracts Bluetooth address from device path string.
/// Device path format: \\?\BTHENUM#Dev_XXXXXXXXXXXX#...
/// or similar patterns containing the MAC address.
/// </summary>
function ExtractAddressFromDevicePath(const ADevicePath: string): UInt64;
var
  UpperPath: string;
  StartPos, I: Integer;
  AddrStr: string;
  AddrValue: UInt64;
begin
  Result := 0;
  UpperPath := UpperCase(ADevicePath);

  // Look for address pattern after "DEV_" or "_"
  StartPos := Pos('DEV_', UpperPath);
  if StartPos > 0 then
    StartPos := StartPos + 4
  else
  begin
    // Try to find a 12-char hex sequence
    StartPos := Pos('_', UpperPath);
    if StartPos > 0 then
      Inc(StartPos);
  end;

  if StartPos <= 0 then
    Exit;

  // Extract 12 hex characters
  AddrStr := '';
  for I := StartPos to Length(UpperPath) do
  begin
    if CharInSet(UpperPath[I], ['0'..'9', 'A'..'F']) then
    begin
      AddrStr := AddrStr + UpperPath[I];
      if Length(AddrStr) = 12 then
        Break;
    end
    else if Length(AddrStr) > 0 then
      Break; // Stop at first non-hex after we started collecting
  end;

  if Length(AddrStr) = 12 then
  begin
    if TryStrToUInt64('$' + AddrStr, AddrValue) then
      Result := AddrValue;
  end;
end;

/// <summary>
/// Queries battery level using SetupAPI device properties.
/// This works for Classic Bluetooth devices (headphones, etc.).
/// Enumerates Bluetooth device class to find the device by address.
/// Checks all matching devices (main and child nodes) for battery property.
/// </summary>
function QueryBatteryViaDeviceProperty(ADeviceAddress: UInt64): TBatteryStatus;
var
  DevInfo: HDEVINFO;
  DevInfoData: SP_DEVINFO_DATA;
  Index: DWORD;
  InstanceId: array[0..511] of WideChar;
  InstanceIdStr: string;
  PropertyType: DEVPROPTYPE;
  BatteryLevel: Byte;
  Found: Boolean;
  AddressStr: string;
  MatchCount: Integer;
begin
  Result := TBatteryStatus.NotSupported;
  AddressStr := Format('%.12X', [ADeviceAddress]);
  LogDebug('QueryBatteryViaDeviceProperty: Looking for $%s', [AddressStr], LOG_SOURCE);

  // Get ALL present devices (not just Bluetooth class) to find HFP/audio devices
  DevInfo := SetupDiGetClassDevsW(nil, nil, 0, DIGCF_PRESENT or DIGCF_ALLCLASSES);
  if DevInfo = INVALID_HANDLE_VALUE then
  begin
    LogDebug('SetupDiGetClassDevsW failed: %d', [GetLastError], LOG_SOURCE);
    Exit;
  end;

  try
    Index := 0;
    Found := False;
    MatchCount := 0;

    while True do
    begin
      FillChar(DevInfoData, SizeOf(DevInfoData), 0);
      DevInfoData.cbSize := SizeOf(SP_DEVINFO_DATA);

      if not SetupDiEnumDeviceInfo(DevInfo, Index, DevInfoData) then
      begin
        if GetLastError = ERROR_NO_MORE_ITEMS then
          Break;
        Inc(Index);
        Continue;
      end;

      // Get device instance ID
      FillChar(InstanceId, SizeOf(InstanceId), 0);
      if SetupDiGetDeviceInstanceIdW(DevInfo, @DevInfoData, @InstanceId[0],
        Length(InstanceId), nil) then
      begin
        InstanceIdStr := InstanceId;

        // Check if this device's instance ID contains our address (case-insensitive)
        if Pos(AddressStr, UpperCase(InstanceIdStr)) > 0 then
        begin
          Inc(MatchCount);

          // Try multiple battery property keys
          PropertyType := 0;
          BatteryLevel := 0;

          // Try DEVPKEY_Bluetooth_BatteryPercent
          if SetupDiGetDevicePropertyW(DevInfo, @DevInfoData,
            DEVPKEY_Bluetooth_BatteryPercent, PropertyType,
            @BatteryLevel, SizeOf(BatteryLevel), nil, 0) and
            (PropertyType = DEVPROP_TYPE_BYTE) then
          begin
            LogInfo('Battery (BT_BatteryPercent): %d%% from %s', [BatteryLevel, InstanceIdStr], LOG_SOURCE);
            Result := TBatteryStatus.Create(BatteryLevel);
            Found := True;
            Break;
          end;

          // Try PKEY_Devices_BatteryPercent
          PropertyType := 0;
          BatteryLevel := 0;
          if SetupDiGetDevicePropertyW(DevInfo, @DevInfoData,
            PKEY_Devices_BatteryPercent, PropertyType,
            @BatteryLevel, SizeOf(BatteryLevel), nil, 0) and
            (PropertyType = DEVPROP_TYPE_BYTE) then
          begin
            LogInfo('Battery (Devices_BatteryPercent): %d%% from %s', [BatteryLevel, InstanceIdStr], LOG_SOURCE);
            Result := TBatteryStatus.Create(BatteryLevel);
            Found := True;
            Break;
          end;

          // Try DEVPKEY_Bluetooth_Battery
          PropertyType := 0;
          BatteryLevel := 0;
          if SetupDiGetDevicePropertyW(DevInfo, @DevInfoData,
            DEVPKEY_Bluetooth_Battery, PropertyType,
            @BatteryLevel, SizeOf(BatteryLevel), nil, 0) and
            (PropertyType = DEVPROP_TYPE_BYTE) then
          begin
            LogInfo('Battery (BT_Battery): %d%% from %s', [BatteryLevel, InstanceIdStr], LOG_SOURCE);
            Result := TBatteryStatus.Create(BatteryLevel);
            Found := True;
            Break;
          end;
        end;
      end;

      Inc(Index);
    end;

    if not Found then
      LogDebug('No battery property for $%s (%d devices checked, %d matched)', [AddressStr, Index, MatchCount], LOG_SOURCE);
  finally
    SetupDiDestroyDeviceInfoList(DevInfo);
  end;
end;

{ Public Functions }

function QueryBluetoothBatteryViaBLE(ADeviceAddress: UInt64;
  ATimeoutMs: Cardinal): TBatteryStatus;
var
  Device: IBluetoothLEDevice;
  Service: IGattDeviceService;
  Characteristic: IGattCharacteristic;
  Level: Integer;
begin
  LogDebug('Trying BLE GATT query for $%.12X', [ADeviceAddress], LOG_SOURCE);

  // Step 1: Get the BLE device
  if not GetBluetoothLEDevice(ADeviceAddress, ATimeoutMs, Device) then
  begin
    LogDebug('Device not found or not BLE', LOG_SOURCE);
    Result := TBatteryStatus.NotSupported;
    Exit;
  end;

  // Step 2: Get the Battery Service
  if not GetBatteryService(Device, ATimeoutMs, Service) then
  begin
    LogDebug('Battery Service not available', LOG_SOURCE);
    Result := TBatteryStatus.NotSupported;
    Exit;
  end;

  // Step 3: Get the Battery Level characteristic
  if not GetBatteryLevelCharacteristic(Service, ATimeoutMs, Characteristic) then
  begin
    LogDebug('Battery Level characteristic not available', LOG_SOURCE);
    Result := TBatteryStatus.Unknown;
    Exit;
  end;

  // Step 4: Read the battery level
  if not ReadBatteryLevel(Characteristic, ATimeoutMs, Level) then
  begin
    LogDebug('Failed to read battery level', LOG_SOURCE);
    Result := TBatteryStatus.Unknown;
    Exit;
  end;

  Result := TBatteryStatus.Create(Level);
  LogInfo('Battery level via BLE for $%.12X: %d%%', [ADeviceAddress, Level], LOG_SOURCE);
end;

function QueryBluetoothBattery(ADeviceAddress: UInt64;
  ATimeoutMs: Cardinal): TBatteryStatus;
begin
  LogDebug('Querying battery for device $%.12X', [ADeviceAddress], LOG_SOURCE);

  // First try: BLE GATT Battery Service (for BLE devices)
  Result := QueryBluetoothBatteryViaBLE(ADeviceAddress, ATimeoutMs);

  // If BLE failed (device is not BLE or no battery service), try SetupAPI
  // This works for Classic Bluetooth devices like headphones
  if not Result.HasLevel then
  begin
    LogDebug('BLE query returned no level, trying SetupAPI device property', LOG_SOURCE);
    Result := QueryBatteryViaDeviceProperty(ADeviceAddress);
  end;

  if Result.HasLevel then
    LogInfo('Final battery level for $%.12X: %d%%', [ADeviceAddress, Result.Level], LOG_SOURCE)
  else
    LogDebug('No battery level available for $%.12X', [ADeviceAddress], LOG_SOURCE);
end;

function CreateBatteryQuery: IBatteryQuery;
begin
  Result := TBatteryQuery.Create;
end;

{ TBatteryQuery }

function TBatteryQuery.GetBatteryLevel(ADeviceAddress: UInt64): TBatteryStatus;
begin
  Result := GetBatteryLevelWithTimeout(ADeviceAddress, DEFAULT_TIMEOUT_MS);
end;

function TBatteryQuery.GetBatteryLevelWithTimeout(ADeviceAddress: UInt64;
  ATimeoutMs: Cardinal): TBatteryStatus;
begin
  Result := QueryBluetoothBattery(ADeviceAddress, ATimeoutMs);
end;

{ TBatteryCache }

constructor TBatteryCache.Create(ABatteryQuery: IBatteryQuery);
begin
  inherited Create;
  FBatteryQuery := ABatteryQuery;
  FCache := TDictionary<UInt64, TBatteryStatus>.Create;
  FLock := TCriticalSection.Create;
  FOnQueryCompleted := nil;
  FShuttingDown := 0;
  FPendingQueries := 0;
end;

destructor TBatteryCache.Destroy;
const
  MAX_WAIT_MS = 5000;
  POLL_INTERVAL_MS = 50;
var
  WaitedMs: Integer;
begin
  // Signal shutdown to background threads
  TInterlocked.Exchange(FShuttingDown, 1);
  FOnQueryCompleted := nil;
  FBatteryQuery := nil;

  // Wait for pending queries to complete (with timeout)
  WaitedMs := 0;
  while (TInterlocked.Read(FPendingQueries) > 0) and (WaitedMs < MAX_WAIT_MS) do
  begin
    Sleep(POLL_INTERVAL_MS);
    Inc(WaitedMs, POLL_INTERVAL_MS);
  end;

  if TInterlocked.Read(FPendingQueries) > 0 then
    LogWarning('BatteryCache destroyed with %d pending queries', [FPendingQueries], LOG_SOURCE);

  FLock.Free;
  FCache.Free;
  inherited;
end;

function TBatteryCache.GetBatteryStatus(ADeviceAddress: UInt64): TBatteryStatus;
begin
  FLock.Enter;
  try
    if not FCache.TryGetValue(ADeviceAddress, Result) then
      Result := TBatteryStatus.NotSupported;
  finally
    FLock.Leave;
  end;
end;

procedure TBatteryCache.SetBatteryStatus(ADeviceAddress: UInt64;
  const AStatus: TBatteryStatus);
begin
  FLock.Enter;
  try
    FCache.AddOrSetValue(ADeviceAddress, AStatus);
  finally
    FLock.Leave;
  end;
end;

function TBatteryCache.HasCachedStatus(ADeviceAddress: UInt64): Boolean;
begin
  FLock.Enter;
  try
    Result := FCache.ContainsKey(ADeviceAddress);
  finally
    FLock.Leave;
  end;
end;

procedure TBatteryCache.RequestRefresh(ADeviceAddress: UInt64);
var
  LAddress: UInt64;
  LQuery: IBatteryQuery;
  LOnCompleted: TBatteryQueryCompletedEvent;
  LSelf: TBatteryCache;
begin
  // Check if shutting down before starting new query
  if TInterlocked.Read(FShuttingDown) <> 0 then
    Exit;

  LAddress := ADeviceAddress;
  LQuery := FBatteryQuery;
  LOnCompleted := FOnQueryCompleted;
  LSelf := Self;

  // Track pending query
  TInterlocked.Increment(FPendingQueries);

  TThread.CreateAnonymousThread(
    procedure
    var
      Status: TBatteryStatus;
    begin
      try
        try
          Status := LQuery.GetBatteryLevel(LAddress);
        except
          on E: Exception do
          begin
            LogError('RequestRefresh: Exception querying $%.12X: %s', [LAddress, E.Message], LOG_SOURCE);
            Status := TBatteryStatus.Unknown;
          end;
        end;

        // Check if shutting down before accessing cache
        if TInterlocked.Read(LSelf.FShuttingDown) = 0 then
        begin
          // Update cache thread-safely
          LSelf.FLock.Enter;
          try
            LSelf.FCache.AddOrSetValue(LAddress, Status);
          finally
            LSelf.FLock.Leave;
          end;

          // Notify on main thread (only if not shutting down)
          if Assigned(LOnCompleted) and (TInterlocked.Read(LSelf.FShuttingDown) = 0) then
          begin
            TThread.Queue(nil,
              procedure
              begin
                // Double-check callback and shutdown state
                if Assigned(LOnCompleted) and (TInterlocked.Read(LSelf.FShuttingDown) = 0) then
                  LOnCompleted(LSelf, LAddress, Status);
              end
            );
          end;
        end;
      finally
        // Always decrement pending counter
        TInterlocked.Decrement(LSelf.FPendingQueries);
      end;
    end
  ).Start;
end;

procedure TBatteryCache.RequestRefreshAll(const ADeviceAddresses: TArray<UInt64>);
var
  I: Integer;
begin
  for I := 0 to High(ADeviceAddresses) do
    RequestRefresh(ADeviceAddresses[I]);
end;

procedure TBatteryCache.Clear;
begin
  FLock.Enter;
  try
    FCache.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TBatteryCache.Remove(ADeviceAddress: UInt64);
begin
  FLock.Enter;
  try
    FCache.Remove(ADeviceAddress);
  finally
    FLock.Leave;
  end;
end;

function TBatteryCache.GetOnQueryCompleted: TBatteryQueryCompletedEvent;
begin
  Result := FOnQueryCompleted;
end;

procedure TBatteryCache.SetOnQueryCompleted(AValue: TBatteryQueryCompletedEvent);
begin
  FOnQueryCompleted := AValue;
end;

function CreateBatteryCache(ABatteryQuery: IBatteryQuery): IBatteryCache;
begin
  Result := TBatteryCache.Create(ABatteryQuery);
end;

end.
