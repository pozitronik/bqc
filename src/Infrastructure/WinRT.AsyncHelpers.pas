{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Async Operation Helpers                   }
{                                                       }
{       Provides reusable patterns for WinRT async      }
{       operations, reducing boilerplate code.          }
{                                                       }
{       Design Principles:                              }
{       - DRY: Single implementation of async patterns  }
{       - SRP: Only handles WinRT async mechanics       }
{       - OCP: New result types via generic interfaces  }
{                                                       }
{*******************************************************}

unit WinRT.AsyncHelpers;

interface

uses
  Winapi.Windows,
  System.SysUtils;

type
  /// <summary>
  /// Result of a WinRT async operation.
  /// Encapsulates success/failure state with optional error details.
  /// </summary>
  TWinRTAsyncResult = record
    Success: Boolean;
    ErrorCode: HRESULT;
    ErrorMessage: string;
    class function Ok: TWinRTAsyncResult; static;
    class function Fail(AErrorCode: HRESULT; const AMessage: string = ''): TWinRTAsyncResult; static;
  end;

  /// <summary>
  /// GATT communication status constants.
  /// Mirrors Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus.
  /// </summary>
  TGattCommunicationStatus = (
    gcsSuccess = 0,
    gcsUnreachable = 1,
    gcsProtocolError = 2,
    gcsAccessDenied = 3
  );

  HSTRING = type THandle;

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

const
  // Async operation status values
  AsyncStatus_Started   = 0;
  AsyncStatus_Completed = 1;
  AsyncStatus_Canceled  = 2;
  AsyncStatus_Error     = 3;

  // WinRT initialization mode
  RO_INIT_MULTITHREADED = 1;

/// <summary>
/// Initializes WinRT runtime if not already initialized.
/// Thread-safe and idempotent - can be called multiple times.
/// </summary>
/// <returns>True if initialization succeeded or was already done.</returns>
function EnsureWinRTInitialized: Boolean;

/// <summary>
/// Waits for a WinRT async operation to complete with timeout.
/// Uses polling with configurable interval to avoid blocking.
/// </summary>
/// <param name="AAsyncInfo">The async operation info interface.</param>
/// <param name="ATimeoutMs">Maximum time to wait in milliseconds.</param>
/// <param name="APollIntervalMs">Polling interval in milliseconds (default 10).</param>
/// <returns>True if operation completed successfully within timeout.</returns>
function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo;
  ATimeoutMs: Cardinal; APollIntervalMs: Cardinal = 10): Boolean;

/// <summary>
/// Executes a WinRT async operation and waits for completion.
/// Combines async info extraction and waiting into a single call.
/// </summary>
/// <param name="AAsyncOp">The async operation (must support IAsyncInfo).</param>
/// <param name="ATimeoutMs">Maximum time to wait in milliseconds.</param>
/// <returns>Result indicating success or failure with error details.</returns>
function ExecuteAsyncWithTimeout(const AAsyncOp: IUnknown;
  ATimeoutMs: Cardinal): TWinRTAsyncResult;

/// <summary>
/// Gets the size of a WinRT vector view.
/// Works with any IVectorView implementation.
/// </summary>
/// <param name="AVectorView">The vector view interface.</param>
/// <param name="ASize">Output size value.</param>
/// <returns>True if size was retrieved successfully.</returns>
function GetVectorSize(const AVectorView: IInspectable; out ASize: Cardinal): Boolean;

/// <summary>
/// Creates an HSTRING from a Delphi string.
/// </summary>
function CreateHString(const AStr: string): HSTRING;

/// <summary>
/// Frees an HSTRING.
/// </summary>
procedure FreeHString(AStr: HSTRING);

/// <summary>
/// Gets the WinRT activation factory for a runtime class.
/// </summary>
/// <param name="AClassName">The fully qualified runtime class name.</param>
/// <param name="AIID">The interface ID of the factory.</param>
/// <param name="AFactory">Output factory interface.</param>
/// <returns>True if factory was obtained successfully.</returns>
function GetActivationFactory(const AClassName: string; const AIID: TGUID;
  out AFactory: IInspectable): Boolean;

implementation

uses
  App.Logger;

const
  LOG_SOURCE = 'WinRT.AsyncHelpers';

// External WinRT functions
function WindowsCreateString(sourceString: PWideChar; length: Cardinal;
  out str: HSTRING): HRESULT; stdcall; external 'combase.dll';

function WindowsDeleteString(str: HSTRING): HRESULT; stdcall;
  external 'combase.dll';

function RoInitialize(initType: Cardinal): HRESULT; stdcall;
  external 'combase.dll';

function RoGetActivationFactory(activatableClassId: HSTRING; const iid: TGUID;
  out factory: IInspectable): HRESULT; stdcall; external 'combase.dll';

{ TWinRTAsyncResult }

class function TWinRTAsyncResult.Ok: TWinRTAsyncResult;
begin
  Result.Success := True;
  Result.ErrorCode := S_OK;
  Result.ErrorMessage := '';
end;

class function TWinRTAsyncResult.Fail(AErrorCode: HRESULT;
  const AMessage: string): TWinRTAsyncResult;
begin
  Result.Success := False;
  Result.ErrorCode := AErrorCode;
  if AMessage <> '' then
    Result.ErrorMessage := AMessage
  else
    Result.ErrorMessage := Format('HRESULT: 0x%.8X', [AErrorCode]);
end;

{ Public Functions }

function EnsureWinRTInitialized: Boolean;
var
  HR: HRESULT;
begin
  HR := RoInitialize(RO_INIT_MULTITHREADED);
  // S_OK = success, RPC_E_CHANGED_MODE = already initialized (different mode)
  // Both are acceptable outcomes
  Result := Succeeded(HR) or (HR = RPC_E_CHANGED_MODE);
  if not Result then
    LogDebug('RoInitialize failed: 0x%.8X', [HR], LOG_SOURCE);
end;

function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo;
  ATimeoutMs: Cardinal; APollIntervalMs: Cardinal): Boolean;
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

    Sleep(APollIntervalMs);
  until (GetTickCount - StartTime) > ATimeoutMs;

  LogDebug('WaitForAsyncOperation: Timeout after %dms', [ATimeoutMs], LOG_SOURCE);
end;

function ExecuteAsyncWithTimeout(const AAsyncOp: IUnknown;
  ATimeoutMs: Cardinal): TWinRTAsyncResult;
var
  AsyncInfo: IAsyncInfo;
begin
  if AAsyncOp = nil then
  begin
    Result := TWinRTAsyncResult.Fail(E_POINTER, 'Async operation is nil');
    Exit;
  end;

  if not Supports(AAsyncOp, IAsyncInfo, AsyncInfo) then
  begin
    Result := TWinRTAsyncResult.Fail(E_NOINTERFACE, 'Failed to get IAsyncInfo');
    Exit;
  end;

  if WaitForAsyncOperation(AsyncInfo, ATimeoutMs) then
    Result := TWinRTAsyncResult.Ok
  else
    Result := TWinRTAsyncResult.Fail(HRESULT($80004005), 'Async operation timeout or failure');
end;

function GetVectorSize(const AVectorView: IInspectable; out ASize: Cardinal): Boolean;
type
  // Generic vector view interface with get_Size method
  // The get_Size method is at the same vtable offset for all IVectorView<T>
  IVectorViewBase = interface(IInspectable)
    ['{00000000-0000-0000-0000-000000000000}']
    function GetAt(index: Cardinal; out item: IInspectable): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
  end;
var
  VectorBase: IVectorViewBase;
begin
  Result := False;
  ASize := 0;

  if AVectorView = nil then
    Exit;

  // This works because get_Size is at the same vtable position in all IVectorView<T>
  // We use a type cast to access the method
  try
    VectorBase := IVectorViewBase(AVectorView);
    Result := Succeeded(VectorBase.get_Size(ASize));
  except
    Result := False;
  end;
end;

function CreateHString(const AStr: string): HSTRING;
begin
  Result := 0;
  if AStr <> '' then
    WindowsCreateString(PWideChar(AStr), Length(AStr), Result);
end;

procedure FreeHString(AStr: HSTRING);
begin
  if AStr <> 0 then
    WindowsDeleteString(AStr);
end;

function GetActivationFactory(const AClassName: string; const AIID: TGUID;
  out AFactory: IInspectable): Boolean;
var
  ClassName: HSTRING;
  HR: HRESULT;
begin
  Result := False;
  AFactory := nil;

  if not EnsureWinRTInitialized then
    Exit;

  ClassName := CreateHString(AClassName);
  if ClassName = 0 then
  begin
    LogDebug('Failed to create HSTRING for class name: %s', [AClassName], LOG_SOURCE);
    Exit;
  end;

  try
    HR := RoGetActivationFactory(ClassName, AIID, AFactory);
    Result := Succeeded(HR) and (AFactory <> nil);
    if not Result then
      LogDebug('RoGetActivationFactory failed for %s: 0x%.8X', [AClassName, HR], LOG_SOURCE);
  finally
    FreeHString(ClassName);
  end;
end;

end.
