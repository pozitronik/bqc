{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Device Query Implementation               }
{                                                       }
{       Uses Windows.Devices.Bluetooth and              }
{       Windows.Devices.Enumeration APIs for device     }
{       enumeration. Supports both Classic and BLE.     }
{                                                       }
{*******************************************************}

unit Bluetooth.WinRTDeviceQuery;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces;

type
  /// <summary>
  /// WinRT implementation of IBluetoothDeviceQuery.
  /// Uses Windows.Devices.Bluetooth APIs for enumeration.
  /// Supports both Classic Bluetooth and Bluetooth Low Energy devices.
  /// </summary>
  TWinRTBluetoothDeviceQuery = class(TInterfacedObject, IBluetoothDeviceQuery)
  private const
    LOG_SOURCE = 'WinRTDeviceQuery';
    DEFAULT_TIMEOUT_MS = 10000;
  private
    function EnumerateClassicDevices: TBluetoothDeviceInfoArray;
    function EnumerateBLEDevices: TBluetoothDeviceInfoArray;
    function MergeDeviceArrays(const AClassic, ABLE: TBluetoothDeviceInfoArray): TBluetoothDeviceInfoArray;
  public
    function EnumeratePairedDevices: TBluetoothDeviceInfoArray;
  end;

/// <summary>
/// Creates a WinRT-based Bluetooth device query.
/// </summary>
function CreateWinRTBluetoothDeviceQuery: IBluetoothDeviceQuery;

implementation

uses
  Winapi.ActiveX,
  App.Logger,
  App.WinRTSupport,
  WinRT.AsyncHelpers;

const
  // BluetoothConnectionStatus
  BluetoothConnectionStatus_Disconnected = 0;
  BluetoothConnectionStatus_Connected    = 1;

  // Runtime class names
  RuntimeClass_BluetoothDevice: string = 'Windows.Devices.Bluetooth.BluetoothDevice';
  RuntimeClass_BluetoothLEDevice: string = 'Windows.Devices.Bluetooth.BluetoothLEDevice';
  RuntimeClass_DeviceInformation: string = 'Windows.Devices.Enumeration.DeviceInformation';

type

  /// <summary>
  /// Generic async operation completed handler.
  /// </summary>
  IAsyncOperationCompletedHandler = interface(IUnknown)
  end;

  // Forward declarations
  IBluetoothDevice = interface;
  IBluetoothLEDevice = interface;
  IDeviceInformation = interface;
  IDeviceInformationCollection = interface;

  /// <summary>
  /// Async operation for BluetoothDevice.
  /// </summary>
  IAsyncOperationBluetoothDevice = interface(IInspectable)
    ['{B58D8D19-44BD-5AC0-A0D6-1B50800F5191}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IBluetoothDevice): HRESULT; stdcall;
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
  /// Async operation for DeviceInformationCollection.
  /// </summary>
  IAsyncOperationDeviceInformationCollection = interface(IInspectable)
    ['{45180254-082E-5274-B2E7-AC0517F44D07}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IDeviceInformationCollection): HRESULT; stdcall;
  end;

  /// <summary>
  /// IVectorView for DeviceInformation.
  /// </summary>
  IVectorViewDeviceInformation = interface(IInspectable)
    ['{E170688F-3495-5BF6-AAB5-9CAC17E0F10F}']
    function GetAt(index: Cardinal; out item: IDeviceInformation): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
    function IndexOf(item: IDeviceInformation; out index: Cardinal; out found: Boolean): HRESULT; stdcall;
    function GetMany(startIndex: Cardinal; capacity: Cardinal; out items: IDeviceInformation;
      out actual: Cardinal): HRESULT; stdcall;
  end;

  /// <summary>
  /// DeviceInformationCollection (implements IVectorView).
  /// </summary>
  IDeviceInformationCollection = interface(IVectorViewDeviceInformation)
    ['{E170688F-3495-5BF6-AAB5-9CAC17E0F10F}']
  end;

  /// <summary>
  /// DeviceInformation interface.
  /// </summary>
  IDeviceInformation = interface(IInspectable)
    ['{ABA0FB95-4398-489D-8E44-E6130927011F}']
    function get_Id(out value: HSTRING): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_IsEnabled(out value: Boolean): HRESULT; stdcall;
    function get_IsDefault(out value: Boolean): HRESULT; stdcall;
    function get_EnclosureLocation(out value: IInspectable): HRESULT; stdcall;
    function get_Properties(out value: IInspectable): HRESULT; stdcall;
    function Update(updateInfo: IInspectable): HRESULT; stdcall;
    function GetThumbnailAsync(out asyncOp: IInspectable): HRESULT; stdcall;
    function GetGlyphThumbnailAsync(out asyncOp: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// DeviceInformation statics for FindAllAsync.
  /// </summary>
  IDeviceInformationStatics = interface(IInspectable)
    ['{C17F100E-3A46-4A78-8013-769DC9B97390}']
    function CreateFromIdAsync(deviceId: HSTRING;
      out asyncOp: IInspectable): HRESULT; stdcall;
    function CreateFromIdAsyncAdditionalProperties(deviceId: HSTRING;
      additionalProperties: IInspectable; out asyncOp: IInspectable): HRESULT; stdcall;
    function FindAllAsync(out asyncOp: IAsyncOperationDeviceInformationCollection): HRESULT; stdcall;
    function FindAllAsyncDeviceClass(deviceClass: Integer;
      out asyncOp: IAsyncOperationDeviceInformationCollection): HRESULT; stdcall;
    function FindAllAsyncAqsFilter(aqsFilter: HSTRING;
      out asyncOp: IAsyncOperationDeviceInformationCollection): HRESULT; stdcall;
    function FindAllAsyncAqsFilterAndAdditionalProperties(aqsFilter: HSTRING;
      additionalProperties: IInspectable;
      out asyncOp: IAsyncOperationDeviceInformationCollection): HRESULT; stdcall;
    function CreateWatcher(out watcher: IInspectable): HRESULT; stdcall;
    function CreateWatcherDeviceClass(deviceClass: Integer;
      out watcher: IInspectable): HRESULT; stdcall;
    function CreateWatcherAqsFilter(aqsFilter: HSTRING;
      out watcher: IInspectable): HRESULT; stdcall;
    function CreateWatcherAqsFilterAndAdditionalProperties(aqsFilter: HSTRING;
      additionalProperties: IInspectable;
      out watcher: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// Classic Bluetooth device interface.
  /// </summary>
  IBluetoothDevice = interface(IInspectable)
    ['{2335B156-90D2-4A04-AEF5-0E20B9E6B707}']
    function get_DeviceId(out value: HSTRING): HRESULT; stdcall;
    function get_HostName(out value: IInspectable): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_ClassOfDevice(out value: IInspectable): HRESULT; stdcall;
    function get_SdpRecords(out value: IInspectable): HRESULT; stdcall;
    function get_RfcommServices(out value: IInspectable): HRESULT; stdcall;
    function get_ConnectionStatus(out value: Integer): HRESULT; stdcall;
    function get_BluetoothAddress(out value: UInt64): HRESULT; stdcall;
  end;

  /// <summary>
  /// IBluetoothClassOfDevice for accessing CoD values.
  /// </summary>
  IBluetoothClassOfDevice = interface(IInspectable)
    ['{D640227E-D7D7-4661-9454-65039CA17A2B}']
    function get_RawValue(out value: Cardinal): HRESULT; stdcall;
    function get_MajorClass(out value: Integer): HRESULT; stdcall;
    function get_MinorClass(out value: Integer): HRESULT; stdcall;
    function get_ServiceCapabilities(out value: Integer): HRESULT; stdcall;
  end;

  /// <summary>
  /// IBluetoothDevice3 for GetDeviceSelectorFromPairingState.
  /// </summary>
  IBluetoothDevice3 = interface(IInspectable)
    ['{57FFF78B-651A-4454-B90F-EB21EF0B0D71}']
    function get_DeviceAccessInformation(out value: IInspectable): HRESULT; stdcall;
    function RequestAccessAsync(out operation: IInspectable): HRESULT; stdcall;
    function GetRfcommServicesAsync(out operation: IInspectable): HRESULT; stdcall;
    function GetRfcommServicesWithCacheModeAsync(cacheMode: Integer;
      out operation: IInspectable): HRESULT; stdcall;
    function GetRfcommServicesForIdAsync(serviceId: IInspectable;
      out operation: IInspectable): HRESULT; stdcall;
    function GetRfcommServicesForIdWithCacheModeAsync(serviceId: IInspectable;
      cacheMode: Integer; out operation: IInspectable): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothDevice statics for GetDeviceSelector and FromIdAsync.
  /// </summary>
  IBluetoothDeviceStatics = interface(IInspectable)
    ['{0991DF51-57DB-4725-BBD7-84F64327EC2C}']
    function FromIdAsync(deviceId: HSTRING;
      out operation: IAsyncOperationBluetoothDevice): HRESULT; stdcall;
    function FromHostNameAsync(hostName: IInspectable;
      out operation: IAsyncOperationBluetoothDevice): HRESULT; stdcall;
    function FromBluetoothAddressAsync(address: UInt64;
      out operation: IAsyncOperationBluetoothDevice): HRESULT; stdcall;
    function GetDeviceSelector(out selector: HSTRING): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothDevice statics2 for GetDeviceSelectorFromPairingState.
  /// </summary>
  IBluetoothDeviceStatics2 = interface(IInspectable)
    ['{C29E8E2F-4E14-4477-AA1B-B8B47E5B7ECE}']
    function GetDeviceSelectorFromPairingState(pairingState: Boolean;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromConnectionStatus(connectionStatus: Integer;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromDeviceName(deviceName: HSTRING;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromBluetoothAddress(bluetoothAddress: UInt64;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromClassOfDevice(classOfDevice: IInspectable;
      out selector: HSTRING): HRESULT; stdcall;
  end;

  /// <summary>
  /// BLE device interface.
  /// </summary>
  IBluetoothLEDevice = interface(IInspectable)
    ['{B5EE2F7B-4AD8-4642-AC48-80A0B500E887}']
    function get_DeviceId(out value: HSTRING): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_GattServices(out value: IInspectable): HRESULT; stdcall;
    function get_ConnectionStatus(out value: Integer): HRESULT; stdcall;
    function get_BluetoothAddress(out value: UInt64): HRESULT; stdcall;
    function GetGattService(serviceUuid: TGUID;
      out service: IInspectable): HRESULT; stdcall;
    function add_NameChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_NameChanged(token: Int64): HRESULT; stdcall;
    function add_GattServicesChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_GattServicesChanged(token: Int64): HRESULT; stdcall;
    function add_ConnectionStatusChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_ConnectionStatusChanged(token: Int64): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothLEDevice statics for GetDeviceSelector and FromIdAsync.
  /// </summary>
  IBluetoothLEDeviceStatics = interface(IInspectable)
    ['{C8CF1A19-F0B6-4BF0-8689-41303DE2D9F4}']
    function FromIdAsync(deviceId: HSTRING;
      out operation: IAsyncOperationBluetoothLEDevice): HRESULT; stdcall;
    function FromBluetoothAddressAsync(bluetoothAddress: UInt64;
      out operation: IAsyncOperationBluetoothLEDevice): HRESULT; stdcall;
    function GetDeviceSelector(out selector: HSTRING): HRESULT; stdcall;
  end;

  /// <summary>
  /// BluetoothLEDevice statics2 for GetDeviceSelectorFromPairingState.
  /// </summary>
  IBluetoothLEDeviceStatics2 = interface(IInspectable)
    ['{5F2B145F-0348-4418-2B20-AB6BBD5B03E6}']
    function GetDeviceSelectorFromPairingState(paired: Boolean;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromConnectionStatus(connectionStatus: Integer;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromDeviceName(deviceName: HSTRING;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromBluetoothAddress(bluetoothAddress: UInt64;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromBluetoothAddressWithBluetoothAddressType(
      bluetoothAddress: UInt64; bluetoothAddressType: Integer;
      out selector: HSTRING): HRESULT; stdcall;
    function GetDeviceSelectorFromAppearance(appearance: IInspectable;
      out selector: HSTRING): HRESULT; stdcall;
    function FromBluetoothAddressWithBluetoothAddressTypeAsync(
      bluetoothAddress: UInt64; bluetoothAddressType: Integer;
      out operation: IAsyncOperationBluetoothLEDevice): HRESULT; stdcall;
  end;

{ TWinRTBluetoothDeviceQuery }

function TWinRTBluetoothDeviceQuery.EnumeratePairedDevices: TBluetoothDeviceInfoArray;
var
  ClassicDevices, BLEDevices: TBluetoothDeviceInfoArray;
  ThreadID: Cardinal;
begin
  ThreadID := GetCurrentThreadId;
  LogDebug('EnumeratePairedDevices: Entry [ThreadID=%d]', [ThreadID], LOG_SOURCE);

  // Check if WinRT is available (Windows 8+)
  if not IsWinRTAvailable then
  begin
    LogDebug('EnumeratePairedDevices: WinRT not available (Windows 7?), returning empty', LOG_SOURCE);
    SetLength(Result, 0);
    Exit;
  end;

  LogDebug('EnumeratePairedDevices: Starting WinRT enumeration [ThreadID=%d]', [ThreadID], LOG_SOURCE);

  // Initialize WinRT (if not already)
  if not EnsureWinRTInitialized(LOG_SOURCE) then
  begin
    LogDebug('EnumeratePairedDevices: WinRT initialization failed', LOG_SOURCE);
    SetLength(Result, 0);
    Exit;
  end;

  // Enumerate both Classic and BLE devices
  LogDebug('EnumeratePairedDevices: About to call EnumerateClassicDevices', LOG_SOURCE);
  try
    ClassicDevices := EnumerateClassicDevices;
    LogDebug('EnumeratePairedDevices: EnumerateClassicDevices returned, found %d Classic devices', [Length(ClassicDevices)], LOG_SOURCE);
  except
    on E: Exception do
    begin
      LogDebug('EnumeratePairedDevices: EnumerateClassicDevices raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
      SetLength(ClassicDevices, 0);
    end;
  end;

  LogDebug('EnumeratePairedDevices: About to call EnumerateBLEDevices', LOG_SOURCE);
  try
    BLEDevices := EnumerateBLEDevices;
    LogDebug('EnumeratePairedDevices: EnumerateBLEDevices returned, found %d BLE devices', [Length(BLEDevices)], LOG_SOURCE);
  except
    on E: Exception do
    begin
      LogDebug('EnumeratePairedDevices: EnumerateBLEDevices raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
      SetLength(BLEDevices, 0);
    end;
  end;

  // Merge results, removing duplicates
  Result := MergeDeviceArrays(ClassicDevices, BLEDevices);
  LogDebug('EnumeratePairedDevices: Total %d unique devices after merge', [Length(Result)], LOG_SOURCE);
end;

function TWinRTBluetoothDeviceQuery.EnumerateClassicDevices: TBluetoothDeviceInfoArray;
var
  HR: HRESULT;
  ClassName, SelectorStr: HSTRING;
  Factory: IInspectable;
  Statics: IBluetoothDeviceStatics;
  Statics2: IBluetoothDeviceStatics2;
  DevInfoStatics: IDeviceInformationStatics;
  AsyncOp: IAsyncOperationDeviceInformationCollection;
  AsyncInfo: IAsyncInfo;
  Collection: IDeviceInformationCollection;
  Count, I: Cardinal;
  DevInfo: IDeviceInformation;
  DeviceId, DeviceName: HSTRING;
  DeviceIdStr: string;
  AsyncDevOp: IAsyncOperationBluetoothDevice;
  BTDevice: IBluetoothDevice;
  Address: UInt64;
  ConnStatus: Integer;
  DeviceList: TList<TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
  DeviceNameStr: string;
  ClassOfDeviceIntf: IInspectable;
  ClassOfDevice: IBluetoothClassOfDevice;
  CoD: Cardinal;
  ThreadID: Cardinal;
begin
  ThreadID := GetCurrentThreadId;
  LogDebug('EnumerateClassicDevices: Entry [ThreadID=%d]', [ThreadID], LOG_SOURCE);

  SetLength(Result, 0);
  LogDebug('EnumerateClassicDevices: About to create DeviceList [ThreadID=%d]', [ThreadID], LOG_SOURCE);
  DeviceList := TList<TBluetoothDeviceInfo>.Create;
  LogDebug('EnumerateClassicDevices: DeviceList created successfully [ThreadID=%d]', [ThreadID], LOG_SOURCE);
  try
    // Get BluetoothDevice.GetDeviceSelectorFromPairingState(true)
    if not GetActivationFactory(RuntimeClass_BluetoothDevice, IBluetoothDeviceStatics, Factory, LOG_SOURCE) then
      Exit;

    HR := Factory.QueryInterface(IBluetoothDeviceStatics2, Statics2);
    if Failed(HR) or (Statics2 = nil) then
    begin
      LogDebug('EnumerateClassicDevices: QueryInterface for Statics2 failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    HR := Factory.QueryInterface(IBluetoothDeviceStatics, Statics);
    if Failed(HR) or (Statics = nil) then
    begin
      LogDebug('EnumerateClassicDevices: QueryInterface for Statics failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    // Get selector for paired devices
    HR := Statics2.GetDeviceSelectorFromPairingState(True, SelectorStr);
    if Failed(HR) or (SelectorStr = 0) then
    begin
      LogDebug('EnumerateClassicDevices: GetDeviceSelectorFromPairingState failed: 0x%.8X', [HR], LOG_SOURCE);
      Exit;
    end;

    try
      LogDebug('EnumerateClassicDevices: Got selector: %s', [HStringToString(SelectorStr)], LOG_SOURCE);

      // Get DeviceInformation.FindAllAsync(selector)
      if not GetActivationFactory(RuntimeClass_DeviceInformation, IDeviceInformationStatics, Factory, LOG_SOURCE) then
        Exit;

      HR := Factory.QueryInterface(IDeviceInformationStatics, DevInfoStatics);
      if Failed(HR) or (DevInfoStatics = nil) then
      begin
        LogDebug('EnumerateClassicDevices: QueryInterface for DevInfoStatics failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      HR := DevInfoStatics.FindAllAsyncAqsFilter(SelectorStr, AsyncOp);
      if Failed(HR) or (AsyncOp = nil) then
      begin
        LogDebug('EnumerateClassicDevices: FindAllAsyncAqsFilter failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
      begin
        LogDebug('EnumerateClassicDevices: Failed to get IAsyncInfo', LOG_SOURCE);
        Exit;
      end;

      if not WaitForAsyncOperation(AsyncInfo, DEFAULT_TIMEOUT_MS, LOG_SOURCE) then
      begin
        LogDebug('EnumerateClassicDevices: FindAllAsync timeout', LOG_SOURCE);
        Exit;
      end;

      HR := AsyncOp.GetResults(Collection);
      if Failed(HR) or (Collection = nil) then
      begin
        LogDebug('EnumerateClassicDevices: GetResults failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      // Iterate through devices
      HR := Collection.get_Size(Count);
      if Failed(HR) then
      begin
        LogDebug('EnumerateClassicDevices: get_Size failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      LogDebug('EnumerateClassicDevices: Found %d device info entries', [Count], LOG_SOURCE);
      LogDebug('EnumerateClassicDevices: About to enter device loop (Count=%d)', [Count], LOG_SOURCE);

      I := 0;
      while I < Count do
      begin
        HR := Collection.GetAt(I, DevInfo);
        if Failed(HR) or (DevInfo = nil) then
          Continue;

        // Get device ID
        HR := DevInfo.get_Id(DeviceId);
        if Failed(HR) or (DeviceId = 0) then
          Continue;

        DeviceIdStr := HStringToString(DeviceId);
        FreeHString(DeviceId);

        // Get BluetoothDevice.FromIdAsync
        ClassName := CreateHString(DeviceIdStr);
        HR := Statics.FromIdAsync(ClassName, AsyncDevOp);
        FreeHString(ClassName);

        if Failed(HR) or (AsyncDevOp = nil) then
        begin
          LogDebug('EnumerateClassicDevices: FromIdAsync failed for %s: 0x%.8X', [DeviceIdStr, HR], LOG_SOURCE);
          Continue;
        end;

        if not Supports(AsyncDevOp, IAsyncInfo, AsyncInfo) then
          Continue;

        if not WaitForAsyncOperation(AsyncInfo, DEFAULT_TIMEOUT_MS, LOG_SOURCE) then
          Continue;

          HR := AsyncDevOp.GetResults(BTDevice);
          if Failed(HR) or (BTDevice = nil) then
          begin
            LogDebug('EnumerateClassicDevices: GetResults for BTDevice failed: 0x%.8X', [HR], LOG_SOURCE);
            Continue;
          end;

          // Extract device info
          BTDevice.get_BluetoothAddress(Address);

          DeviceName := 0;
          BTDevice.get_Name(DeviceName);
          DeviceNameStr := HStringToString(DeviceName);
          FreeHString(DeviceName);

          BTDevice.get_ConnectionStatus(ConnStatus);

          // Get Class of Device
          CoD := 0;
          HR := BTDevice.get_ClassOfDevice(ClassOfDeviceIntf);
          if Succeeded(HR) and (ClassOfDeviceIntf <> nil) then
          begin
            if Supports(ClassOfDeviceIntf, IBluetoothClassOfDevice, ClassOfDevice) then
              ClassOfDevice.get_RawValue(CoD);
          end;

          // Create TBluetoothDeviceInfo
          Device := TBluetoothDeviceInfo.Create(
            UInt64ToBluetoothAddress(Address),
            Address,
            DeviceNameStr,
            DetermineDeviceType(CoD),
            TBluetoothConnectionState(Ord(ConnStatus = BluetoothConnectionStatus_Connected)),
            True,  // IsPaired
            True,  // IsAuthenticated
            CoD,
            Now,
            0
          );

          LogDebug('EnumerateClassicDevices: Device Address=$%.12X, Name="%s", CoD=$%.8X, Connected=%s',
            [Address, DeviceNameStr, CoD, BoolToStr(ConnStatus = BluetoothConnectionStatus_Connected, True)], LOG_SOURCE);

          DeviceList.Add(Device);

        Inc(I);
      end;

      LogDebug('EnumerateClassicDevices: Exited device loop, DeviceList.Count=%d', [DeviceList.Count], LOG_SOURCE);
    finally
      LogDebug('EnumerateClassicDevices: Entered inner finally block, about to FreeHString(SelectorStr)', LOG_SOURCE);
      try
        FreeHString(SelectorStr);
        LogDebug('EnumerateClassicDevices: FreeHString(SelectorStr) completed', LOG_SOURCE);
      except
        on E: Exception do
          LogDebug('EnumerateClassicDevices: FreeHString raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
      end;
      LogDebug('EnumerateClassicDevices: Exiting inner finally block', LOG_SOURCE);
    end;

    LogDebug('EnumerateClassicDevices: About to call DeviceList.ToArray (Count=%d)', [DeviceList.Count], LOG_SOURCE);
    try
      Result := DeviceList.ToArray;
      LogDebug('EnumerateClassicDevices: DeviceList.ToArray completed, Result length=%d', [Length(Result)], LOG_SOURCE);
    except
      on E: Exception do
      begin
        LogDebug('EnumerateClassicDevices: DeviceList.ToArray raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
        SetLength(Result, 0);
      end;
    end;
  finally
    LogDebug('EnumerateClassicDevices: Entered outer finally block, about to Free DeviceList', LOG_SOURCE);
    try
      DeviceList.Free;
      LogDebug('EnumerateClassicDevices: DeviceList.Free completed', LOG_SOURCE);
    except
      on E: Exception do
        LogDebug('EnumerateClassicDevices: DeviceList.Free raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
    end;
    LogDebug('EnumerateClassicDevices: Exiting outer finally block', LOG_SOURCE);
  end;
  LogDebug('EnumerateClassicDevices: Function exit, returning %d devices', [Length(Result)], LOG_SOURCE);
end;

function TWinRTBluetoothDeviceQuery.EnumerateBLEDevices: TBluetoothDeviceInfoArray;
var
  HR: HRESULT;
  ClassName, SelectorStr: HSTRING;
  ThreadID: Cardinal;
  Factory: IInspectable;
  Statics: IBluetoothLEDeviceStatics;
  Statics2: IBluetoothLEDeviceStatics2;
  DevInfoStatics: IDeviceInformationStatics;
  AsyncOp: IAsyncOperationDeviceInformationCollection;
  AsyncInfo: IAsyncInfo;
  Collection: IDeviceInformationCollection;
  Count, I: Cardinal;
  DevInfo: IDeviceInformation;
  DeviceId, DeviceName: HSTRING;
  DeviceIdStr: string;
  AsyncDevOp: IAsyncOperationBluetoothLEDevice;
  BLEDevice: IBluetoothLEDevice;
  Address: UInt64;
  ConnStatus: Integer;
  DeviceList: TList<TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
  DeviceNameStr: string;
begin
  ThreadID := GetCurrentThreadId;
  LogDebug('EnumerateBLEDevices: Entry [ThreadID=%d]', [ThreadID], LOG_SOURCE);

  SetLength(Result, 0);
  LogDebug('EnumerateBLEDevices: About to create DeviceList [ThreadID=%d]', [ThreadID], LOG_SOURCE);
  DeviceList := TList<TBluetoothDeviceInfo>.Create;
  LogDebug('EnumerateBLEDevices: DeviceList created successfully [ThreadID=%d]', [ThreadID], LOG_SOURCE);
  try
    // Get BluetoothLEDevice.GetDeviceSelectorFromPairingState(true)
    ClassName := CreateHString(RuntimeClass_BluetoothLEDevice);
    if ClassName = 0 then
    begin
      LogDebug('EnumerateBLEDevices: Failed to create HSTRING for BluetoothLEDevice', LOG_SOURCE);
      Exit;
    end;

    try
      HR := RoGetActivationFactory(ClassName, IBluetoothLEDeviceStatics, Factory);
      if Failed(HR) or (Factory = nil) then
      begin
        LogDebug('EnumerateBLEDevices: RoGetActivationFactory failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      HR := Factory.QueryInterface(IBluetoothLEDeviceStatics2, Statics2);
      if Failed(HR) or (Statics2 = nil) then
      begin
        LogDebug('EnumerateBLEDevices: QueryInterface for Statics2 failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      HR := Factory.QueryInterface(IBluetoothLEDeviceStatics, Statics);
      if Failed(HR) or (Statics = nil) then
      begin
        LogDebug('EnumerateBLEDevices: QueryInterface for Statics failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      // Get selector for paired devices
      HR := Statics2.GetDeviceSelectorFromPairingState(True, SelectorStr);
      if Failed(HR) or (SelectorStr = 0) then
      begin
        LogDebug('EnumerateBLEDevices: GetDeviceSelectorFromPairingState failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;
    finally
      FreeHString(ClassName);
    end;

    try
      LogDebug('EnumerateBLEDevices: Got selector: %s', [HStringToString(SelectorStr)], LOG_SOURCE);

      // Get DeviceInformation.FindAllAsync(selector)
      ClassName := CreateHString(RuntimeClass_DeviceInformation);
      if ClassName = 0 then
      begin
        LogDebug('EnumerateBLEDevices: Failed to create HSTRING for DeviceInformation', LOG_SOURCE);
        Exit;
      end;

      try
        HR := RoGetActivationFactory(ClassName, IDeviceInformationStatics, Factory);
        if Failed(HR) or (Factory = nil) then
        begin
          LogDebug('EnumerateBLEDevices: RoGetActivationFactory for DeviceInformation failed: 0x%.8X', [HR], LOG_SOURCE);
          Exit;
        end;

        HR := Factory.QueryInterface(IDeviceInformationStatics, DevInfoStatics);
        if Failed(HR) or (DevInfoStatics = nil) then
        begin
          LogDebug('EnumerateBLEDevices: QueryInterface for DevInfoStatics failed: 0x%.8X', [HR], LOG_SOURCE);
          Exit;
        end;

        HR := DevInfoStatics.FindAllAsyncAqsFilter(SelectorStr, AsyncOp);
        if Failed(HR) or (AsyncOp = nil) then
        begin
          LogDebug('EnumerateBLEDevices: FindAllAsyncAqsFilter failed: 0x%.8X', [HR], LOG_SOURCE);
          Exit;
        end;

        if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
        begin
          LogDebug('EnumerateBLEDevices: Failed to get IAsyncInfo', LOG_SOURCE);
          Exit;
        end;

        if not WaitForAsyncOperation(AsyncInfo, DEFAULT_TIMEOUT_MS, LOG_SOURCE) then
        begin
          LogDebug('EnumerateBLEDevices: FindAllAsync timeout', LOG_SOURCE);
          Exit;
        end;

        HR := AsyncOp.GetResults(Collection);
        if Failed(HR) or (Collection = nil) then
        begin
          LogDebug('EnumerateBLEDevices: GetResults failed: 0x%.8X', [HR], LOG_SOURCE);
          Exit;
        end;
      finally
        FreeHString(ClassName);
      end;

      // Iterate through devices
      HR := Collection.get_Size(Count);
      if Failed(HR) then
      begin
        LogDebug('EnumerateBLEDevices: get_Size failed: 0x%.8X', [HR], LOG_SOURCE);
        Exit;
      end;

      LogDebug('EnumerateBLEDevices: Found %d device info entries', [Count], LOG_SOURCE);
      LogDebug('EnumerateBLEDevices: About to enter device loop (Count=%d)', [Count], LOG_SOURCE);

      I := 0;
      while I < Count do
      begin
        HR := Collection.GetAt(I, DevInfo);
        if Failed(HR) or (DevInfo = nil) then
          Continue;

        // Get device ID
        HR := DevInfo.get_Id(DeviceId);
        if Failed(HR) or (DeviceId = 0) then
          Continue;

        DeviceIdStr := HStringToString(DeviceId);
        FreeHString(DeviceId);

        // Get BluetoothLEDevice.FromIdAsync
        ClassName := CreateHString(DeviceIdStr);
        try
          HR := Statics.FromIdAsync(ClassName, AsyncDevOp);
          if Failed(HR) or (AsyncDevOp = nil) then
          begin
            LogDebug('EnumerateBLEDevices: FromIdAsync failed for %s: 0x%.8X', [DeviceIdStr, HR], LOG_SOURCE);
            Continue;
          end;

          if not Supports(AsyncDevOp, IAsyncInfo, AsyncInfo) then
            Continue;

          if not WaitForAsyncOperation(AsyncInfo, DEFAULT_TIMEOUT_MS, LOG_SOURCE) then
            Continue;

          HR := AsyncDevOp.GetResults(BLEDevice);
          if Failed(HR) or (BLEDevice = nil) then
          begin
            LogDebug('EnumerateBLEDevices: GetResults for BLEDevice failed: 0x%.8X', [HR], LOG_SOURCE);
            Continue;
          end;

          // Extract device info
          BLEDevice.get_BluetoothAddress(Address);

          DeviceName := 0;
          BLEDevice.get_Name(DeviceName);
          DeviceNameStr := HStringToString(DeviceName);
          FreeHString(DeviceName);

          BLEDevice.get_ConnectionStatus(ConnStatus);

          // Create TBluetoothDeviceInfo (BLE devices have CoD=0)
          Device := TBluetoothDeviceInfo.Create(
            UInt64ToBluetoothAddress(Address),
            Address,
            DeviceNameStr,
            btUnknown,  // BLE devices don't have CoD, will be determined by usage
            TBluetoothConnectionState(Ord(ConnStatus = BluetoothConnectionStatus_Connected)),
            True,  // IsPaired
            True,  // IsAuthenticated
            0,     // CoD = 0 for BLE
            Now,
            0
          );

          LogDebug('EnumerateBLEDevices: Device Address=$%.12X, Name="%s", CoD=0 (BLE), Connected=%s',
            [Address, DeviceNameStr, BoolToStr(ConnStatus = BluetoothConnectionStatus_Connected, True)], LOG_SOURCE);

          DeviceList.Add(Device);
        finally
          FreeHString(ClassName);
        end;

        Inc(I);
      end;

      LogDebug('EnumerateBLEDevices: Exited device loop, DeviceList.Count=%d', [DeviceList.Count], LOG_SOURCE);
    finally
      LogDebug('EnumerateBLEDevices: Entered inner finally block, about to FreeHString(SelectorStr)', LOG_SOURCE);
      try
        FreeHString(SelectorStr);
        LogDebug('EnumerateBLEDevices: FreeHString(SelectorStr) completed', LOG_SOURCE);
      except
        on E: Exception do
          LogDebug('EnumerateBLEDevices: FreeHString raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
      end;
      LogDebug('EnumerateBLEDevices: Exiting inner finally block', LOG_SOURCE);
    end;

    LogDebug('EnumerateBLEDevices: About to call DeviceList.ToArray (Count=%d)', [DeviceList.Count], LOG_SOURCE);
    try
      Result := DeviceList.ToArray;
      LogDebug('EnumerateBLEDevices: DeviceList.ToArray completed, Result length=%d', [Length(Result)], LOG_SOURCE);
    except
      on E: Exception do
      begin
        LogDebug('EnumerateBLEDevices: DeviceList.ToArray raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
        SetLength(Result, 0);
      end;
    end;
  finally
    LogDebug('EnumerateBLEDevices: Entered outer finally block, about to Free DeviceList', LOG_SOURCE);
    try
      DeviceList.Free;
      LogDebug('EnumerateBLEDevices: DeviceList.Free completed', LOG_SOURCE);
    except
      on E: Exception do
        LogDebug('EnumerateBLEDevices: DeviceList.Free raised exception: %s - %s', [E.ClassName, E.Message], LOG_SOURCE);
    end;
    LogDebug('EnumerateBLEDevices: Exiting outer finally block', LOG_SOURCE);
  end;
  LogDebug('EnumerateBLEDevices: Function exit, returning %d devices', [Length(Result)], LOG_SOURCE);
end;

function TWinRTBluetoothDeviceQuery.MergeDeviceArrays(
  const AClassic, ABLE: TBluetoothDeviceInfoArray): TBluetoothDeviceInfoArray;
var
  AddressSet: TDictionary<UInt64, TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
  Existing: TBluetoothDeviceInfo;
begin
  AddressSet := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  try
    // Add Classic devices first
    for Device in AClassic do
      AddressSet.AddOrSetValue(Device.AddressInt, Device);

    // Add BLE devices, preferring Classic if duplicate (Classic has more info like CoD)
    for Device in ABLE do
    begin
      if not AddressSet.TryGetValue(Device.AddressInt, Existing) then
        AddressSet.Add(Device.AddressInt, Device)
      else
      begin
        // If Classic device has empty name but BLE has name, use BLE name
        if (Existing.Name = '') and (Device.Name <> '') then
        begin
          LogDebug('MergeDeviceArrays: Using BLE name "%s" for address $%.12X (Classic had empty name)',
            [Device.Name, Device.AddressInt], LOG_SOURCE);
          AddressSet[Device.AddressInt] := Existing.WithName(Device.Name);
        end;
      end;
    end;

    Result := AddressSet.Values.ToArray;
  finally
    AddressSet.Free;
  end;
end;

{ Factory Function }

function CreateWinRTBluetoothDeviceQuery: IBluetoothDeviceQuery;
begin
  Result := TWinRTBluetoothDeviceQuery.Create;
end;

end.
