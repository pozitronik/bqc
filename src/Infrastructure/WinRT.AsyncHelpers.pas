{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Async Operation Helpers                   }
{                                                       }
{       Reusable helpers for Windows Runtime async      }
{       operations, HSTRING handling, and activation    }
{       factory patterns.                               }
{                                                       }
{       Consolidates duplicated WinRT patterns from     }
{       BatteryQuery, RadioControl, and                 }
{       WinRTDeviceQuery units.                         }
{                                                       }
{*******************************************************}

unit WinRT.AsyncHelpers;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX;

type
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

  /// <summary>
  /// Result of a WinRT operation.
  /// Provides success/failure status with error information.
  /// </summary>
  TWinRTAsyncResult = record
    Success: Boolean;
    ErrorCode: HRESULT;
    ErrorMessage: string;

    class function MakeSuccess: TWinRTAsyncResult; static;
    class function MakeError(AErrorCode: HRESULT; const AMessage: string): TWinRTAsyncResult; static;
  end;

const
  // WinRT initialization mode
  RO_INIT_MULTITHREADED = 1;

  // Async operation status codes
  AsyncStatus_Started   = 0;
  AsyncStatus_Completed = 1;
  AsyncStatus_Canceled  = 2;
  AsyncStatus_Error     = 3;

  // Common HRESULT codes
  RPC_E_CHANGED_MODE = HRESULT($80010106);  // RoInitialize called with different mode

/// <summary>
/// External WinRT functions.
/// Use delayed loading for Windows 7 compatibility.
/// </summary>
{$WARN SYMBOL_PLATFORM OFF}
function WindowsCreateString(sourceString: PWideChar; length: Cardinal;
  out str: HSTRING): HRESULT; stdcall; external 'combase.dll' delayed;

function WindowsDeleteString(str: HSTRING): HRESULT; stdcall;
  external 'combase.dll' delayed;

function WindowsGetStringRawBuffer(str: HSTRING;
  length: PCardinal): PWideChar; stdcall; external 'combase.dll' delayed;

function RoInitialize(initType: Cardinal): HRESULT; stdcall;
  external 'combase.dll' delayed;

function RoGetActivationFactory(activatableClassId: HSTRING; const iid: TGUID;
  out factory: IInspectable): HRESULT; stdcall; external 'combase.dll' delayed;
{$WARN SYMBOL_PLATFORM ON}

/// <summary>
/// Creates an HSTRING from a Delphi string.
/// Returns 0 for empty strings.
/// </summary>
function CreateHString(const AStr: string): HSTRING;

/// <summary>
/// Frees an HSTRING created by CreateHString.
/// Safe to call with 0 (no-op).
/// </summary>
procedure FreeHString(AStr: HSTRING);

/// <summary>
/// Converts HSTRING to Delphi string.
/// Returns empty string for 0 handle.
/// </summary>
function HStringToString(AStr: HSTRING): string;

/// <summary>
/// Ensures WinRT is initialized.
/// Idempotent - safe to call multiple times.
/// Returns True if WinRT is available and initialized.
/// Logs error if initialization fails.
/// </summary>
function EnsureWinRTInitialized(const ALogSource: string = ''): Boolean;

/// <summary>
/// Waits for an async operation to complete with polling.
/// Returns True if operation completed successfully.
/// Returns False on timeout, cancellation, or error.
/// Logs debug messages for failures (if ALogSource provided).
/// </summary>
function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo;
  ATimeoutMs: Cardinal = 10000; const ALogSource: string = ''): Boolean;

/// <summary>
/// Gets a WinRT activation factory for the specified runtime class.
/// Handles HSTRING creation and cleanup automatically.
/// Returns True if factory was successfully obtained.
/// </summary>
function GetActivationFactory(const ARuntimeClassName: string;
  const AIID: TGUID; out AFactory: IInspectable;
  const ALogSource: string = ''): Boolean;

implementation

uses
  System.SysUtils,
  App.WinRTSupport,
  App.Logger;

{ TWinRTAsyncResult }

class function TWinRTAsyncResult.MakeSuccess: TWinRTAsyncResult;
begin
  Result.Success := True;
  Result.ErrorCode := S_OK;
  Result.ErrorMessage := '';
end;

class function TWinRTAsyncResult.MakeError(AErrorCode: HRESULT;
  const AMessage: string): TWinRTAsyncResult;
begin
  Result.Success := False;
  Result.ErrorCode := AErrorCode;
  Result.ErrorMessage := AMessage;
end;

{ HSTRING Helpers }

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

function HStringToString(AStr: HSTRING): string;
var
  Ptr: PWideChar;
  Len: Cardinal;
begin
  if AStr = 0 then
    Exit('');

  Ptr := WindowsGetStringRawBuffer(AStr, @Len);
  if (Ptr <> nil) and (Len > 0) then
    SetString(Result, Ptr, Len)
  else
    Result := '';
end;

{ WinRT Initialization and Async Helpers }

function EnsureWinRTInitialized(const ALogSource: string): Boolean;
var
  HR: HRESULT;
begin
  // Check if WinRT is available (Windows 8+)
  if not IsWinRTAvailable then
  begin
    if ALogSource <> '' then
      LogDebug('WinRT not available on this system', ALogSource);
    Exit(False);
  end;

  // RoInitialize is reference-counted and idempotent
  HR := RoInitialize(RO_INIT_MULTITHREADED);

  // S_OK (0) = success
  // S_FALSE (1) = already initialized (also success)
  // RPC_E_CHANGED_MODE = already initialized with different mode (also acceptable)
  Result := Succeeded(HR) or (HR = 1) or (HR = RPC_E_CHANGED_MODE);

  if not Result and (ALogSource <> '') then
    LogDebug('RoInitialize failed: 0x%.8X', [HR], ALogSource);
end;

function WaitForAsyncOperation(const AAsyncInfo: IAsyncInfo;
  ATimeoutMs: Cardinal; const ALogSource: string): Boolean;
var
  Status: Integer;
  StartTime: Cardinal;
begin
  Result := False;

  if AAsyncInfo = nil then
  begin
    if ALogSource <> '' then
      LogDebug('WaitForAsyncOperation: AAsyncInfo is nil', ALogSource);
    Exit;
  end;

  StartTime := GetTickCount;
  repeat
    if Failed(AAsyncInfo.get_Status(Status)) then
    begin
      if ALogSource <> '' then
        LogDebug('WaitForAsyncOperation: get_Status failed', ALogSource);
      Exit;
    end;

    if Status <> AsyncStatus_Started then
    begin
      Result := (Status = AsyncStatus_Completed);
      if not Result and (ALogSource <> '') then
        LogDebug('WaitForAsyncOperation: Async status=%d (not completed)', [Status], ALogSource);
      Exit;
    end;

    Sleep(10);
  until (GetTickCount - StartTime) > ATimeoutMs;

  if ALogSource <> '' then
    LogDebug('WaitForAsyncOperation: Timeout after %dms', [ATimeoutMs], ALogSource);
end;

function GetActivationFactory(const ARuntimeClassName: string;
  const AIID: TGUID; out AFactory: IInspectable;
  const ALogSource: string): Boolean;
var
  HR: HRESULT;
  ClassName: HSTRING;
begin
  Result := False;
  AFactory := nil;

  ClassName := CreateHString(ARuntimeClassName);
  if ClassName = 0 then
  begin
    if ALogSource <> '' then
      LogDebug('GetActivationFactory: Failed to create HSTRING for %s', [ARuntimeClassName], ALogSource);
    Exit;
  end;

  try
    HR := RoGetActivationFactory(ClassName, AIID, AFactory);
    Result := Succeeded(HR) and (AFactory <> nil);

    if not Result and (ALogSource <> '') then
      LogDebug('GetActivationFactory: RoGetActivationFactory failed for %s: 0x%.8X',
        [ARuntimeClassName, HR], ALogSource);
  finally
    FreeHString(ClassName);
  end;
end;

end.
