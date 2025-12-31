unit Tests.WinRTAsyncHelpers;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Winapi.ActiveX,
  WinRT.AsyncHelpers;

type
  [TestFixture]
  TWinRTAsyncResultTests = class
  public
    [Test]
    procedure MakeSuccess_CreatesSuccessResult;
    [Test]
    procedure MakeError_CreatesErrorResult;
  end;

  [TestFixture]
  THStringHelpersTests = class
  public
    [Test]
    procedure CreateHString_EmptyString_ReturnsZero;
    [Test]
    procedure CreateHString_NonEmptyString_ReturnsNonZero;
    [Test]
    procedure FreeHString_Zero_DoesNotCrash;
    [Test]
    procedure FreeHString_ValidHandle_Succeeds;
    [Test]
    procedure HStringToString_Zero_ReturnsEmpty;
    [Test]
    procedure HStringToString_ValidHandle_ReturnsCorrectString;
    [Test]
    procedure HStringToString_UnicodeString_HandlesCorrectly;
    [Test]
    procedure CreateAndFreeHString_RoundTrip_Succeeds;
  end;

  [TestFixture]
  TWinRTInitializationTests = class
  public
    [Test]
    procedure EnsureWinRTInitialized_WithoutLogSource_Succeeds;
    [Test]
    procedure EnsureWinRTInitialized_WithLogSource_Succeeds;
    [Test]
    procedure EnsureWinRTInitialized_MultipleCalls_AreIdempotent;
  end;

  /// <summary>
  /// Mock async operation for testing WaitForAsyncOperation.
  /// </summary>
  TMockAsyncInfo = class(TInterfacedObject, IAsyncInfo)
  private
    FStatus: Integer;
    FErrorCode: HRESULT;
    FStatusCallCount: Integer;
  public
    constructor Create(AStatus: Integer; AErrorCode: HRESULT = S_OK);

    // IInspectable
    function GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT; stdcall;
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    function GetTrustLevel(out trustLevel: Integer): HRESULT; stdcall;

    // IAsyncInfo
    function get_Id(out id: Cardinal): HRESULT; stdcall;
    function get_Status(out status: Integer): HRESULT; stdcall;
    function get_ErrorCode(out errorCode: HRESULT): HRESULT; stdcall;
    function Cancel: HRESULT; stdcall;
    function Close: HRESULT; stdcall;

    property CurrentStatus: Integer read FStatus write FStatus;
    property StatusCallCount: Integer read FStatusCallCount;
  end;

  [TestFixture]
  TWaitForAsyncOperationTests = class
  public
    [Test]
    procedure WaitForAsyncOperation_NilAsyncInfo_ReturnsFalse;
    [Test]
    procedure WaitForAsyncOperation_CompletedStatus_ReturnsTrue;
    [Test]
    procedure WaitForAsyncOperation_CanceledStatus_ReturnsFalse;
    [Test]
    procedure WaitForAsyncOperation_ErrorStatus_ReturnsFalse;
    [Test]
    procedure WaitForAsyncOperation_WithLogSource_Succeeds;
    [Test]
    procedure WaitForAsyncOperation_Timeout_ReturnsFalse;
  end;

  [TestFixture]
  TGetActivationFactoryTests = class
  public
    [Test]
    procedure GetActivationFactory_EmptyClassName_ReturnsFalse;
    [Test]
    procedure GetActivationFactory_InvalidClassName_ReturnsFalse;
    [Test]
    procedure GetActivationFactory_WithLogSource_Succeeds;
  end;

implementation

uses
  System.SysUtils,
  App.WinRTSupport;

{ TWinRTAsyncResultTests }

procedure TWinRTAsyncResultTests.MakeSuccess_CreatesSuccessResult;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.MakeSuccess;

  Assert.IsTrue(Result.Success);
  Assert.AreEqual(HRESULT(S_OK), Result.ErrorCode);
  Assert.AreEqual('', Result.ErrorMessage);
end;

procedure TWinRTAsyncResultTests.MakeError_CreatesErrorResult;
var
  Result: TWinRTAsyncResult;
const
  TestError = HRESULT($80004005);  // E_FAIL
  TestMessage = 'Test error message';
begin
  Result := TWinRTAsyncResult.MakeError(TestError, TestMessage);

  Assert.IsFalse(Result.Success);
  Assert.AreEqual(TestError, Result.ErrorCode);
  Assert.AreEqual(TestMessage, Result.ErrorMessage);
end;

{ THStringHelpersTests }

procedure THStringHelpersTests.CreateHString_EmptyString_ReturnsZero;
var
  Handle: HSTRING;
begin
  Handle := CreateHString('');
  Assert.AreEqual(HSTRING(0), Handle);
end;

procedure THStringHelpersTests.CreateHString_NonEmptyString_ReturnsNonZero;
var
  Handle: HSTRING;
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Handle := CreateHString('Test');
  try
    Assert.AreNotEqual(HSTRING(0), Handle);
  finally
    FreeHString(Handle);
  end;
end;

procedure THStringHelpersTests.FreeHString_Zero_DoesNotCrash;
begin
  // Should not crash or raise exception
  FreeHString(0);
  Assert.Pass;
end;

procedure THStringHelpersTests.FreeHString_ValidHandle_Succeeds;
var
  Handle: HSTRING;
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Handle := CreateHString('Test');
  Assert.AreNotEqual(HSTRING(0), Handle);

  // Should not crash
  FreeHString(Handle);
  Assert.Pass;
end;

procedure THStringHelpersTests.HStringToString_Zero_ReturnsEmpty;
var
  Str: string;
begin
  Str := HStringToString(0);
  Assert.AreEqual('', Str);
end;

procedure THStringHelpersTests.HStringToString_ValidHandle_ReturnsCorrectString;
var
  Handle: HSTRING;
  Str: string;
const
  TestStr = 'Hello, WinRT!';
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Handle := CreateHString(TestStr);
  try
    Str := HStringToString(Handle);
    Assert.AreEqual(TestStr, Str);
  finally
    FreeHString(Handle);
  end;
end;

procedure THStringHelpersTests.HStringToString_UnicodeString_HandlesCorrectly;
var
  Handle: HSTRING;
  Str: string;
const
  TestStr = 'Привет, мир! 你好世界';  // Russian and Chinese
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Handle := CreateHString(TestStr);
  try
    Str := HStringToString(Handle);
    Assert.AreEqual(TestStr, Str);
  finally
    FreeHString(Handle);
  end;
end;

procedure THStringHelpersTests.CreateAndFreeHString_RoundTrip_Succeeds;
var
  Handle: HSTRING;
  Original, Result: string;
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Original := 'Windows.Devices.Bluetooth.BluetoothDevice';
  Handle := CreateHString(Original);
  try
    Assert.AreNotEqual(HSTRING(0), Handle);
    Result := HStringToString(Handle);
    Assert.AreEqual(Original, Result);
  finally
    FreeHString(Handle);
  end;
end;

{ TWinRTInitializationTests }

procedure TWinRTInitializationTests.EnsureWinRTInitialized_WithoutLogSource_Succeeds;
var
  Result: Boolean;
begin
  Result := EnsureWinRTInitialized;

  if IsWinRTAvailable then
    Assert.IsTrue(Result, 'WinRT should initialize on Windows 8+')
  else
    Assert.IsFalse(Result, 'WinRT should not initialize on Windows 7');
end;

procedure TWinRTInitializationTests.EnsureWinRTInitialized_WithLogSource_Succeeds;
var
  Result: Boolean;
begin
  Result := EnsureWinRTInitialized('TestSource');

  if IsWinRTAvailable then
    Assert.IsTrue(Result)
  else
    Assert.IsFalse(Result);
end;

procedure TWinRTInitializationTests.EnsureWinRTInitialized_MultipleCalls_AreIdempotent;
var
  Result1, Result2, Result3: Boolean;
begin
  Result1 := EnsureWinRTInitialized;
  Result2 := EnsureWinRTInitialized;
  Result3 := EnsureWinRTInitialized;

  Assert.AreEqual(Result1, Result2, 'First and second calls should return same result');
  Assert.AreEqual(Result2, Result3, 'Second and third calls should return same result');
end;

{ TMockAsyncInfo }

constructor TMockAsyncInfo.Create(AStatus: Integer; AErrorCode: HRESULT);
begin
  inherited Create;
  FStatus := AStatus;
  FErrorCode := AErrorCode;
  FStatusCallCount := 0;
end;

function TMockAsyncInfo.GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TMockAsyncInfo.GetRuntimeClassName(out className: HSTRING): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TMockAsyncInfo.GetTrustLevel(out trustLevel: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TMockAsyncInfo.get_Id(out id: Cardinal): HRESULT;
begin
  id := 1;
  Result := S_OK;
end;

function TMockAsyncInfo.get_Status(out status: Integer): HRESULT;
begin
  Inc(FStatusCallCount);
  status := FStatus;
  Result := S_OK;
end;

function TMockAsyncInfo.get_ErrorCode(out errorCode: HRESULT): HRESULT;
begin
  errorCode := FErrorCode;
  Result := S_OK;
end;

function TMockAsyncInfo.Cancel: HRESULT;
begin
  Result := S_OK;
end;

function TMockAsyncInfo.Close: HRESULT;
begin
  Result := S_OK;
end;

{ TWaitForAsyncOperationTests }

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_NilAsyncInfo_ReturnsFalse;
var
  Result: Boolean;
begin
  Result := WaitForAsyncOperation(nil, 1000);
  Assert.IsFalse(Result);
end;

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_CompletedStatus_ReturnsTrue;
var
  Mock: TMockAsyncInfo;
  Result: Boolean;
begin
  Mock := TMockAsyncInfo.Create(AsyncStatus_Completed);
  try
    Result := WaitForAsyncOperation(Mock, 1000);
    Assert.IsTrue(Result);
    Assert.AreEqual(1, Mock.StatusCallCount, 'Should call get_Status once for completed status');
  finally
    Mock.Free;
  end;
end;

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_CanceledStatus_ReturnsFalse;
var
  Mock: TMockAsyncInfo;
  Result: Boolean;
begin
  Mock := TMockAsyncInfo.Create(AsyncStatus_Canceled);
  try
    Result := WaitForAsyncOperation(Mock, 1000);
    Assert.IsFalse(Result);
  finally
    Mock.Free;
  end;
end;

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_ErrorStatus_ReturnsFalse;
var
  Mock: TMockAsyncInfo;
  Result: Boolean;
begin
  Mock := TMockAsyncInfo.Create(AsyncStatus_Error);
  try
    Result := WaitForAsyncOperation(Mock, 1000);
    Assert.IsFalse(Result);
  finally
    Mock.Free;
  end;
end;

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_WithLogSource_Succeeds;
var
  Mock: TMockAsyncInfo;
  Result: Boolean;
begin
  Mock := TMockAsyncInfo.Create(AsyncStatus_Completed);
  try
    Result := WaitForAsyncOperation(Mock, 1000, 'TestSource');
    Assert.IsTrue(Result);
  finally
    Mock.Free;
  end;
end;

procedure TWaitForAsyncOperationTests.WaitForAsyncOperation_Timeout_ReturnsFalse;
var
  Mock: TMockAsyncInfo;
  Result: Boolean;
  StartTime, ElapsedMs: Cardinal;
const
  TimeoutMs = 100;
begin
  Mock := TMockAsyncInfo.Create(AsyncStatus_Started);  // Never completes
  try
    StartTime := GetTickCount;
    Result := WaitForAsyncOperation(Mock, TimeoutMs);
    ElapsedMs := GetTickCount - StartTime;

    Assert.IsFalse(Result, 'Should return False on timeout');
    Assert.IsTrue(ElapsedMs >= TimeoutMs,
      Format('Should wait at least %dms, waited %dms', [TimeoutMs, ElapsedMs]));
    Assert.IsTrue(Mock.StatusCallCount > 1, 'Should poll status multiple times');
  finally
    Mock.Free;
  end;
end;

{ TGetActivationFactoryTests }

procedure TGetActivationFactoryTests.GetActivationFactory_EmptyClassName_ReturnsFalse;
var
  Factory: IInspectable;
  Result: Boolean;
const
  DummyGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
begin
  Result := GetActivationFactory('', DummyGUID, Factory);
  Assert.IsFalse(Result);
  Assert.IsNull(Factory);
end;

procedure TGetActivationFactoryTests.GetActivationFactory_InvalidClassName_ReturnsFalse;
var
  Factory: IInspectable;
  Result: Boolean;
const
  DummyGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  Result := GetActivationFactory('Invalid.Class.Name.That.Does.Not.Exist',
    DummyGUID, Factory);
  Assert.IsFalse(Result);
  Assert.IsNull(Factory);
end;

procedure TGetActivationFactoryTests.GetActivationFactory_WithLogSource_Succeeds;
var
  Factory: IInspectable;
  Result: Boolean;
const
  DummyGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
begin
  if not IsWinRTAvailable then
    Exit;  // Skip test on Windows 7

  // Invalid class, but we're testing that logging doesn't crash
  Result := GetActivationFactory('Invalid.Class', DummyGUID, Factory, 'TestSource');
  Assert.IsFalse(Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TWinRTAsyncResultTests);
  TDUnitX.RegisterTestFixture(THStringHelpersTests);
  TDUnitX.RegisterTestFixture(TWinRTInitializationTests);
  TDUnitX.RegisterTestFixture(TWaitForAsyncOperationTests);
  TDUnitX.RegisterTestFixture(TGetActivationFactoryTests);

end.
