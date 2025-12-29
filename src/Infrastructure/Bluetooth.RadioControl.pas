{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Bluetooth Radio Control via WinRT               }
{                                                       }
{       Uses Windows.Devices.Radios API for proper      }
{       software radio control without admin rights.    }
{                                                       }
{*******************************************************}

unit Bluetooth.RadioControl;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes;

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
  /// Event type for radio state changes.
  /// </summary>
  TRadioStateChangedEvent = procedure(Sender: TObject; AEnabled: Boolean) of object;

  /// <summary>
  /// Interface for managing Bluetooth radio state.
  /// Abstracts WinRT radio control API for testability.
  /// </summary>
  IRadioStateManager = interface
    ['{B7E3C4D5-2222-3333-4444-555566667777}']

    /// <summary>
    /// Gets the current enabled state of the Bluetooth radio.
    /// </summary>
    /// <param name="AEnabled">Output: True if radio is on, False if off.</param>
    /// <returns>True if state was successfully retrieved, False otherwise.</returns>
    function GetState(out AEnabled: Boolean): Boolean;

    /// <summary>
    /// Sets the Bluetooth radio state.
    /// </summary>
    /// <param name="AEnable">True to enable, False to disable.</param>
    /// <returns>Result of the operation.</returns>
    function SetState(AEnable: Boolean): TRadioControlResult;

    /// <summary>
    /// Sets the Bluetooth radio state with extended result.
    /// </summary>
    /// <param name="AEnable">True to enable, False to disable.</param>
    /// <returns>Extended result with error code.</returns>
    function SetStateEx(AEnable: Boolean): TRadioControlResultEx;

    /// <summary>
    /// Starts watching for radio state changes.
    /// </summary>
    procedure StartWatching;

    /// <summary>
    /// Stops watching for radio state changes.
    /// </summary>
    procedure StopWatching;

    /// <summary>
    /// Gets the OnStateChanged event handler.
    /// </summary>
    function GetOnStateChanged: TRadioStateChangedEvent;

    /// <summary>
    /// Sets the OnStateChanged event handler.
    /// </summary>
    procedure SetOnStateChanged(AValue: TRadioStateChangedEvent);

    /// <summary>
    /// Event fired when Bluetooth radio state changes.
    /// </summary>
    property OnStateChanged: TRadioStateChangedEvent read GetOnStateChanged write SetOnStateChanged;
  end;

  /// <summary>
  /// Watches for Bluetooth radio state changes using polling.
  /// </summary>
  TBluetoothRadioWatcher = class
  private
    FOnStateChanged: TRadioStateChangedEvent;
    FHandle: HWND;
    FTimerID: UINT_PTR;
    FLastState: Boolean;
    FLastStateKnown: Boolean;
    procedure WndProc(var Msg: TMessage);
    procedure CheckRadioState;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Starts watching for Bluetooth radio state changes.
    /// </summary>
    procedure Start;

    /// <summary>
    /// Stops watching for state changes.
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Event fired when Bluetooth radio state changes.
    /// </summary>
    property OnStateChanged: TRadioStateChangedEvent read FOnStateChanged write FOnStateChanged;
  end;

  /// <summary>
  /// Implementation of IRadioStateManager.
  /// Wraps WinRT radio control functions and provides state change watching.
  /// </summary>
  TRadioStateManager = class(TInterfacedObject, IRadioStateManager)
  private
    FWatcher: TBluetoothRadioWatcher;
    FOnStateChanged: TRadioStateChangedEvent;
    procedure HandleWatcherStateChanged(Sender: TObject; AEnabled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    { IRadioStateManager }
    function GetState(out AEnabled: Boolean): Boolean;
    function SetState(AEnable: Boolean): TRadioControlResult;
    function SetStateEx(AEnable: Boolean): TRadioControlResultEx;
    procedure StartWatching;
    procedure StopWatching;
    function GetOnStateChanged: TRadioStateChangedEvent;
    procedure SetOnStateChanged(AValue: TRadioStateChangedEvent);
  end;

/// <summary>
/// Enables or disables the Bluetooth radio adapter using WinRT API.
/// </summary>
function SetBluetoothRadioState(AEnable: Boolean): TRadioControlResult;

/// <summary>
/// Enables or disables the Bluetooth radio adapter with extended result.
/// </summary>
function SetBluetoothRadioStateEx(AEnable: Boolean): TRadioControlResultEx;

/// <summary>
/// Gets the current enabled state of the Bluetooth radio.
/// </summary>
function GetBluetoothRadioState(out AEnabled: Boolean): Boolean;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX,
  App.WinRTSupport;

const
  TIMER_ID_RADIO_POLL = 1;
  RADIO_POLL_INTERVAL_MS = 500;
  RO_INIT_MULTITHREADED = 1;

  RadioState_Unknown  = 0;
  RadioState_On       = 1;
  RadioState_Off      = 2;
  RadioState_Disabled = 3;

  RadioKind_Bluetooth = 3;

  RadioAccessStatus_Allowed       = 1;
  RadioAccessStatus_DeniedByUser  = 2;
  RadioAccessStatus_DeniedBySystem = 3;

  AsyncStatus_Started   = 0;
  AsyncStatus_Completed = 1;

  RuntimeClass_Radio: string = 'Windows.Devices.Radios.Radio';

type
  HSTRING = type THandle;

  IInspectable = interface(IUnknown)
    ['{AF86E2E0-B12D-4C6A-9C5A-D7AA65101E90}']
    function GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT; stdcall;
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    function GetTrustLevel(out trustLevel: Integer): HRESULT; stdcall;
  end;

  IAsyncInfo = interface(IInspectable)
    ['{00000036-0000-0000-C000-000000000046}']
    function get_Id(out id: Cardinal): HRESULT; stdcall;
    function get_Status(out status: Integer): HRESULT; stdcall;
    function get_ErrorCode(out errorCode: HRESULT): HRESULT; stdcall;
    function Cancel: HRESULT; stdcall;
    function Close: HRESULT; stdcall;
  end;

  IAsyncOperationCompletedHandler = interface(IUnknown)
  end;

  IAsyncOperationRadioVector = interface(IInspectable)
    ['{EAC62C40-8DBC-5854-8BA0-B7B9940E7389}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: IInspectable): HRESULT; stdcall;
  end;

  IAsyncOperationRadioAccessStatus = interface(IInspectable)
    ['{21FB30EF-072F-502C-9898-D0C3B2CD9AC5}']
    function put_Completed(handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function get_Completed(out handler: IAsyncOperationCompletedHandler): HRESULT; stdcall;
    function GetResults(out results: Integer): HRESULT; stdcall;
  end;

  IRadio = interface;

  IVectorViewRadio = interface(IInspectable)
    ['{65066C36-090B-5466-B8E5-E7565DC34175}']
    function GetAt(index: Cardinal; out item: IRadio): HRESULT; stdcall;
    function get_Size(out size: Cardinal): HRESULT; stdcall;
    function IndexOf(item: IRadio; out index: Cardinal; out found: Boolean): HRESULT; stdcall;
    function GetMany(startIndex: Cardinal; capacity: Cardinal; out items: IRadio;
      out actual: Cardinal): HRESULT; stdcall;
  end;

  IRadio = interface(IInspectable)
    ['{252118DF-B33E-416A-875F-1CF38AE2D83E}']
    function SetStateAsync(value: Integer; out operation: IAsyncOperationRadioAccessStatus): HRESULT; stdcall;
    function add_StateChanged(handler: IInspectable; out token: Int64): HRESULT; stdcall;
    function remove_StateChanged(token: Int64): HRESULT; stdcall;
    function get_State(out value: Integer): HRESULT; stdcall;
    function get_Name(out value: HSTRING): HRESULT; stdcall;
    function get_Kind(out value: Integer): HRESULT; stdcall;
  end;

  IRadioStatics = interface(IInspectable)
    ['{5FB6A12E-67CB-46AE-AAE9-65919F86EFF4}']
    function GetRadiosAsync(out operation: IAsyncOperationRadioVector): HRESULT; stdcall;
    function GetDeviceSelector(out selector: HSTRING): HRESULT; stdcall;
    function FromIdAsync(deviceId: HSTRING; out operation: IInspectable): HRESULT; stdcall;
    function RequestAccessAsync(out operation: IAsyncOperationRadioAccessStatus): HRESULT; stdcall;
  end;

// External WinRT functions - use delayed loading for Windows 7 compatibility
function WindowsCreateString(sourceString: PWideChar; length: Cardinal;
  out str: HSTRING): HRESULT; stdcall; external 'combase.dll' delayed;

function WindowsDeleteString(str: HSTRING): HRESULT; stdcall;
  external 'combase.dll' delayed;

function WindowsGetStringRawBuffer(str: HSTRING; out length: Cardinal): PWideChar; stdcall;
  external 'combase.dll' delayed;

function RoInitialize(initType: Cardinal): HRESULT; stdcall;
  external 'combase.dll' delayed;

/// <summary>
/// RoUninitialize is declared but intentionally NOT called in this unit.
///
/// IMPORTANT: We do NOT call RoUninitialize for the following reasons:
///
/// 1. WinRT COM objects (IRadio, IVectorViewRadio, etc.) may still be held
///    by the Bluetooth subsystem after our code returns. Calling RoUninitialize
///    would invalidate these interface pointers, causing access violations
///    when they are later released or used by the system.
///
/// 2. For desktop applications, the Windows Runtime is designed to be
///    initialized once and remain active for the process lifetime.
///    The runtime cleans up automatically when the process exits.
///
/// 3. Multiple calls to RoInitialize are reference-counted, but calling
///    RoUninitialize while interfaces are still alive is undefined behavior.
///
/// This is the recommended pattern for WinRT usage in desktop applications.
/// See: https://docs.microsoft.com/en-us/windows/win32/api/roapi/nf-roapi-roinitialize
/// </summary>
procedure RoUninitialize; stdcall;
  external 'combase.dll' delayed;

function RoGetActivationFactory(activatableClassId: HSTRING; const iid: TGUID;
  out factory: IInspectable): HRESULT; stdcall; external 'combase.dll' delayed;

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

  // Check if WinRT is available (Windows 8+)
  if not IsWinRTAvailable then
    Exit;

  HR := RoInitialize(RO_INIT_MULTITHREADED);
  if Failed(HR) and (HR <> RPC_E_CHANGED_MODE) then
    Exit;

  try
    ClassName := CreateHString(RuntimeClass_Radio);
    if ClassName = 0 then
      Exit;

    try
      HR := RoGetActivationFactory(ClassName, IRadioStatics, Factory);
      if Failed(HR) or (Factory = nil) then
        Exit;

      HR := Factory.QueryInterface(IRadioStatics, RadioStatics);
      if Failed(HR) or (RadioStatics = nil) then
        Exit;

      HR := RadioStatics.GetRadiosAsync(AsyncOp);
      if Failed(HR) or (AsyncOp = nil) then
        Exit;

      if not Supports(AsyncOp, IAsyncInfo, AsyncInfo) then
        Exit;

      if not WaitForAsyncOperation(AsyncInfo) then
        Exit;

      HR := AsyncOp.GetResults(RadioVector);
      if Failed(HR) or (RadioVector = nil) then
        Exit;

      HR := RadioVector.QueryInterface(IVectorViewRadio, VectorView);
      if Failed(HR) or (VectorView = nil) then
        Exit;

      HR := VectorView.get_Size(Count);
      if Failed(HR) or (Count = 0) then
        Exit;

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
    // See RoUninitialize declaration for why we don't call it here
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

  if not GetBluetoothRadio(Radio) then
  begin
    Result.Result := rcDeviceNotFound;
    Exit;
  end;

  if AEnable then
    DesiredState := RadioState_On
  else
    DesiredState := RadioState_Off;

  HR := Radio.SetStateAsync(DesiredState, AsyncOp);
  if Failed(HR) or (AsyncOp = nil) then
  begin
    Result.Result := rcError;
    Result.ErrorCode := HR;
    Exit;
  end;

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

  HR := AsyncOp.GetResults(AccessStatus);
  if Failed(HR) then
  begin
    Result.Result := rcError;
    Result.ErrorCode := HR;
    Exit;
  end;

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
  // See RoUninitialize declaration for why we don't call it here
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

  if not GetBluetoothRadio(Radio) then
    Exit;

  HR := Radio.get_State(RadioState);
  if Failed(HR) then
    Exit;

  AEnabled := (RadioState = RadioState_On);
  Result := True;
  // See RoUninitialize declaration for why we don't call it here
end;

{ TBluetoothRadioWatcher }

constructor TBluetoothRadioWatcher.Create;
begin
  inherited Create;
  FHandle := AllocateHWnd(WndProc);
  FTimerID := 0;
  FLastState := False;
  FLastStateKnown := False;
end;

destructor TBluetoothRadioWatcher.Destroy;
begin
  Stop;

  if FHandle <> 0 then
  begin
    DeallocateHWnd(FHandle);
    FHandle := 0;
  end;

  inherited Destroy;
end;

procedure TBluetoothRadioWatcher.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_TIMER) and (UINT_PTR(Msg.WParam) = TIMER_ID_RADIO_POLL) then
    CheckRadioState
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TBluetoothRadioWatcher.CheckRadioState;
var
  CurrentState: Boolean;
begin
  if GetBluetoothRadioState(CurrentState) then
  begin
    if FLastStateKnown then
    begin
      // Only fire event if state actually changed
      if CurrentState <> FLastState then
      begin
        FLastState := CurrentState;
        if Assigned(FOnStateChanged) then
          FOnStateChanged(Self, CurrentState);
      end;
    end
    else
    begin
      // First time - just record the state, don't fire event
      FLastState := CurrentState;
      FLastStateKnown := True;
    end;
  end;
end;

procedure TBluetoothRadioWatcher.Start;
begin
  if FTimerID <> 0 then
    Exit;

  // Get initial state
  if GetBluetoothRadioState(FLastState) then
    FLastStateKnown := True
  else
    FLastStateKnown := False;

  // Start polling timer
  FTimerID := SetTimer(FHandle, TIMER_ID_RADIO_POLL, RADIO_POLL_INTERVAL_MS, nil);
end;

procedure TBluetoothRadioWatcher.Stop;
begin
  if FTimerID <> 0 then
  begin
    KillTimer(FHandle, TIMER_ID_RADIO_POLL);
    FTimerID := 0;
  end;
  FLastStateKnown := False;
end;

{ TRadioStateManager }

constructor TRadioStateManager.Create;
begin
  inherited Create;
  FWatcher := TBluetoothRadioWatcher.Create;
  FWatcher.OnStateChanged := HandleWatcherStateChanged;
end;

destructor TRadioStateManager.Destroy;
begin
  FWatcher.Free;
  inherited Destroy;
end;

function TRadioStateManager.GetState(out AEnabled: Boolean): Boolean;
begin
  Result := GetBluetoothRadioState(AEnabled);
end;

function TRadioStateManager.SetState(AEnable: Boolean): TRadioControlResult;
begin
  Result := SetBluetoothRadioState(AEnable);
end;

function TRadioStateManager.SetStateEx(AEnable: Boolean): TRadioControlResultEx;
begin
  Result := SetBluetoothRadioStateEx(AEnable);
end;

procedure TRadioStateManager.StartWatching;
begin
  FWatcher.Start;
end;

procedure TRadioStateManager.StopWatching;
begin
  FWatcher.Stop;
end;

function TRadioStateManager.GetOnStateChanged: TRadioStateChangedEvent;
begin
  Result := FOnStateChanged;
end;

procedure TRadioStateManager.SetOnStateChanged(AValue: TRadioStateChangedEvent);
begin
  FOnStateChanged := AValue;
end;

procedure TRadioStateManager.HandleWatcherStateChanged(Sender: TObject; AEnabled: Boolean);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, AEnabled);
end;

end.
