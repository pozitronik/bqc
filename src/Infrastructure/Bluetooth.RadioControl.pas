{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Bluetooth Radio Control via WinRT               }
{                                                       }
{       Uses Windows.Devices.Radios API for proper      }
{       software radio control without admin rights.    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.RadioControl;

interface

uses
  Winapi.Windows;

type
  /// <summary>
  /// Result of a radio control operation.
  /// </summary>
  TRadioControlResult = (
    rcSuccess,
    rcDeviceNotFound,
    rcAccessDenied,
    rcError
  );

  /// <summary>
  /// Extended result with error code for debugging.
  /// </summary>
  TRadioControlResultEx = record
    Result: TRadioControlResult;
    ErrorCode: DWORD;
  end;

/// <summary>
/// Enables or disables the Bluetooth radio adapter using WinRT API.
/// Does not require administrator privileges.
/// </summary>
/// <param name="AEnable">True to enable, False to disable.</param>
/// <returns>Result indicating success or type of failure.</returns>
function SetBluetoothRadioState(AEnable: Boolean): TRadioControlResult;

/// <summary>
/// Enables or disables the Bluetooth radio adapter with extended result.
/// Uses WinRT API. Does not require administrator privileges.
/// </summary>
/// <param name="AEnable">True to enable, False to disable.</param>
/// <returns>Extended result with error code.</returns>
function SetBluetoothRadioStateEx(AEnable: Boolean): TRadioControlResultEx;

/// <summary>
/// Gets the current enabled state of the Bluetooth radio.
/// </summary>
/// <param name="AEnabled">Output: True if enabled, False if disabled.</param>
/// <returns>True if state was retrieved successfully.</returns>
function GetBluetoothRadioState(out AEnabled: Boolean): Boolean;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX;

const
  // WinRT initialization types
  RO_INIT_SINGLETHREADED = 0;
  RO_INIT_MULTITHREADED  = 1;

  // RadioState enum values
  RadioState_Unknown  = 0;
  RadioState_On       = 1;
  RadioState_Off      = 2;
  RadioState_Disabled = 3;

  // RadioKind enum values
  RadioKind_Other           = 0;
  RadioKind_WiFi            = 1;
  RadioKind_MobileBroadband = 2;
  RadioKind_Bluetooth       = 3;
  RadioKind_FM              = 4;

  // RadioAccessStatus enum values
  RadioAccessStatus_Unspecified   = 0;
  RadioAccessStatus_Allowed       = 1;
  RadioAccessStatus_DeniedByUser  = 2;
  RadioAccessStatus_DeniedBySystem = 3;

  // AsyncStatus enum values
  AsyncStatus_Started   = 0;
  AsyncStatus_Completed = 1;
  AsyncStatus_Canceled  = 2;
  AsyncStatus_Error     = 3;

  // Runtime class name
  RuntimeClass_Radio: string = 'Windows.Devices.Radios.Radio';

type
  // WinRT string handle
  HSTRING = type THandle;
  PHSTRING = ^HSTRING;

  // Forward declarations
  IInspectable = interface;
  IRadio = interface;
  IRadioStatics = interface;
  IAsyncInfo = interface;

  // IInspectable - base interface for WinRT objects
  IInspectable = interface(IUnknown)
    ['{AF86E2E0-B12D-4C6A-9C5A-D7AA65101E90}']
    function GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT; stdcall;
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    function GetTrustLevel(out trustLevel: Integer): HRESULT; stdcall;
  end;

  // IAsyncInfo - base interface for async operations
  IAsyncInfo = interface(IInspectable)
    ['{00000036-0000-0000-C000-000000000046}']
    function get_Id(out id: Cardinal): HRESULT; stdcall;
    function get_Status(out status: Integer): HRESULT; stdcall;
    function get_ErrorCode(out errorCode: HRESULT): HRESULT; stdcall;
    function Cancel: HRESULT; stdcall;
    function Close: HRESULT; stdcall;
  end;

  // Generic async operation completed handler (placeholder for vtable)
  IAsyncOperationCompletedHandler = interface(IUnknown)
  end;

  // IAsyncOperation<IVectorView<Radio>>
  IAsyncOperationRadioVector = interface(IInspectable)
    ['{EAC62C40-8DBC-5854-8BA0-B7B9940E7389}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IInspectable): HRESULT; stdcall;
  end;

  // IAsyncOperation<RadioAccessStatus>
  IAsyncOperationRadioAccessStatus = interface(IInspectable)
    ['{21FB30EF-072F-502C-9898-D0C3B2CD9AC5}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: Integer): HRESULT; stdcall;
  end;

  // IVectorView<Radio>
  IVectorViewRadio = interface(IInspectable)
    ['{65066C36-090B-5466-B8E5-E7565DC34175}']
    function GetAt(index: Cardinal; out item: IRadio): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
    function IndexOf(item: IRadio; out index: Cardinal; out found: Boolean): HRESULT; stdcall;
    function GetMany(startIndex: Cardinal; capacity: Cardinal; out items: IRadio;
      out actual: Cardinal): HRESULT; stdcall;
  end;

  // IRadio interface
  IRadio = interface(IInspectable)
    ['{252118DF-B33E-416A-875F-1CF38AE2D83E}']
    function SetStateAsync(value: Integer; out operation: IAsyncOperationRadioAccessStatus): HRESULT; stdcall;
    function add_StateChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_StateChanged(token: Int64): HRESULT; stdcall;
    function get_State(out value: Integer): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_Kind(out value: Integer): HRESULT; stdcall;
  end;

  // IRadioStatics - factory interface
  IRadioStatics = interface(IInspectable)
    ['{5FB6A12E-67CB-46AE-AAE9-65919F86EFF4}']
    function GetRadiosAsync(out operation: IAsyncOperationRadioVector): HRESULT; stdcall;
    function GetDeviceSelector(out selector: HSTRING): HRESULT; stdcall;
    function FromIdAsync(deviceId: HSTRING; out operation: IInspectable): HRESULT; stdcall;
    function RequestAccessAsync(out operation: IAsyncOperationRadioAccessStatus): HRESULT; stdcall;
  end;

// WinRT String functions
function WindowsCreateString(sourceString: PWideChar; length: Cardinal;
  out str: HSTRING): HRESULT; stdcall;
  external 'combase.dll' name 'WindowsCreateString';

function WindowsDeleteString(str: HSTRING): HRESULT; stdcall;
  external 'combase.dll' name 'WindowsDeleteString';

function WindowsGetStringRawBuffer(str: HSTRING; out length: Cardinal): PWideChar; stdcall;
  external 'combase.dll' name 'WindowsGetStringRawBuffer';

// WinRT initialization
function RoInitialize(initType: Cardinal): HRESULT; stdcall;
  external 'combase.dll' name 'RoInitialize';

procedure RoUninitialize; stdcall;
  external 'combase.dll' name 'RoUninitialize';

// WinRT activation
function RoGetActivationFactory(activatableClassId: HSTRING; const iid: TGUID;
  out factory: IInspectable): HRESULT; stdcall;
  external 'combase.dll' name 'RoGetActivationFactory';

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
/// Converts an HSTRING to a Delphi string.
/// </summary>
function HStringToString(AStr: HSTRING): string;
var
  Len: Cardinal;
  P: PWideChar;
begin
  if AStr = 0 then
    Exit('');
  P := WindowsGetStringRawBuffer(AStr, Len);
  if (P <> nil) and (Len > 0) then
    SetString(Result, P, Len)
  else
    Result := '';
end;

/// <summary>
/// Waits for an async operation to complete.
/// </summary>
function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo; ATimeoutMs: Cardinal = 10000): Boolean;
var
  Status: Integer;
  StartTime: Cardinal;
begin
  Result := False;
  if AAsyncInfo = nil then
    Exit;

  StartTime := GetTickCount;
  repeat
    if Failed(AAsyncInfo.get_Status(Status)) then
      Exit;

    if Status <> AsyncStatus_Started then
    begin
      Result := (Status = AsyncStatus_Completed);
      Exit;
    end;

    Sleep(10);
  until (GetTickCount - StartTime) > ATimeoutMs;
end;

/// <summary>
/// Gets the Bluetooth radio using WinRT API.
/// </summary>
function GetBluetoothRadio(out ARadio: IRadio): Boolean;
var
  HR: HRESULT;
  ClassName: HSTRING;
  Factory: IInspectable;
  RadioStatics: IRadioStatics;
  AsyncOp: IAsyncOperationRadioVector;
  AsyncInfo: IAsyncInfo;
  RadioVector: IInspectable;
  VectorView: IVectorViewRadio;
  Count, I: Cardinal;
  Radio: IRadio;
  RadioKind: Integer;
begin
  Result := False;
  ARadio := nil;

  // Initialize WinRT
  HR := RoInitialize(RO_INIT_MULTITHREADED);
  if Failed(HR) and (HR <> RPC_E_CHANGED_MODE) then
    Exit;

  try
    // Create class name HSTRING
    ClassName := CreateHString(RuntimeClass_Radio);
    if ClassName = 0 then
      Exit;

    try
      // Get activation factory
      HR := RoGetActivationFactory(ClassName, IRadioStatics, Factory);
      if Failed(HR) or (Factory = nil) then
        Exit;

      // Query for IRadioStatics
      HR := Factory.QueryInterface(IRadioStatics, RadioStatics);
      if Failed(HR) or (RadioStatics = nil) then
        Exit;

      // Get radios async
      HR := RadioStatics.GetRadiosAsync(AsyncOp);
      if Failed(HR) or (AsyncOp = nil) then
        Exit;

      // Wait for completion
      if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
        Exit;

      if not WaitForAsyncOperation(AsyncInfo) then
        Exit;

      // Get results
      HR := AsyncOp.GetResults(RadioVector);
      if Failed(HR) or (RadioVector = nil) then
        Exit;

      // Query for IVectorView<Radio>
      HR := RadioVector.QueryInterface(IVectorViewRadio, VectorView);
      if Failed(HR) or (VectorView = nil) then
        Exit;

      // Get count
      HR := VectorView.get_Size(Count);
      if Failed(HR) or (Count = 0) then
        Exit;

      // Find Bluetooth radio
      for I := 0 to Count - 1 do
      begin
        HR := VectorView.GetAt(I, Radio);
        if Failed(HR) or (Radio = nil) then
          Continue;

        HR := Radio.get_Kind(RadioKind);
        if Succeeded(HR) and (RadioKind = RadioKind_Bluetooth) then
        begin
          ARadio := Radio;
          Result := True;
          Exit;
        end;
      end;

    finally
      FreeHString(ClassName);
    end;
  finally
    // Note: Don't uninitialize here as caller may need the radio reference
  end;
end;

function SetBluetoothRadioStateEx(AEnable: Boolean): TRadioControlResultEx;
var
  Radio: IRadio;
  AsyncOp: IAsyncOperationRadioAccessStatus;
  AsyncInfo: IAsyncInfo;
  AccessStatus: Integer;
  DesiredState: Integer;
  HR: HRESULT;
begin
  Result.Result := rcDeviceNotFound;
  Result.ErrorCode := ERROR_SUCCESS;

  // Get Bluetooth radio
  if not GetBluetoothRadio(Radio) then
  begin
    Result.Result := rcDeviceNotFound;
    Exit;
  end;

  try
    // Determine desired state
    if AEnable then
      DesiredState := RadioState_On
    else
      DesiredState := RadioState_Off;

    // Set state async
    HR := Radio.SetStateAsync(DesiredState, AsyncOp);
    if Failed(HR) or (AsyncOp = nil) then
    begin
      Result.Result := rcError;
      Result.ErrorCode := HR;
      Exit;
    end;

    // Wait for completion
    if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
    begin
      Result.Result := rcError;
      Exit;
    end;

    if not WaitForAsyncOperation(AsyncInfo) then
    begin
      Result.Result := rcError;
      Result.ErrorCode := ERROR_TIMEOUT;
      Exit;
    end;

    // Get result
    HR := AsyncOp.GetResults(AccessStatus);
    if Failed(HR) then
    begin
      Result.Result := rcError;
      Result.ErrorCode := HR;
      Exit;
    end;

    // Interpret access status
    case AccessStatus of
      RadioAccessStatus_Allowed:
        Result.Result := rcSuccess;
      RadioAccessStatus_DeniedByUser,
      RadioAccessStatus_DeniedBySystem:
        begin
          Result.Result := rcAccessDenied;
          Result.ErrorCode := AccessStatus;
        end;
    else
      Result.Result := rcError;
      Result.ErrorCode := AccessStatus;
    end;

  finally
    RoUninitialize;
  end;
end;

function SetBluetoothRadioState(AEnable: Boolean): TRadioControlResult;
begin
  Result := SetBluetoothRadioStateEx(AEnable).Result;
end;

function GetBluetoothRadioState(out AEnabled: Boolean): Boolean;
var
  Radio: IRadio;
  RadioState: Integer;
  HR: HRESULT;
begin
  Result := False;
  AEnabled := False;

  // Get Bluetooth radio
  if not GetBluetoothRadio(Radio) then
    Exit;

  try
    // Get current state
    HR := Radio.get_State(RadioState);
    if Failed(HR) then
      Exit;

    // Check if radio is on
    AEnabled := (RadioState = RadioState_On);
    Result := True;
  finally
    RoUninitialize;
  end;
end;

end.
