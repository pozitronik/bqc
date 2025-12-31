{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       WinRT Async Helpers Unit Tests                  }
{                                                       }
{       Tests for WinRT async operation helpers.        }
{                                                       }
{*******************************************************}

unit Tests.WinRTAsyncHelpers;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  WinRT.AsyncHelpers;

type
  [TestFixture]
  TWinRTAsyncResultTests = class
  public
    [Test]
    procedure Ok_SetsSuccessTrue;

    [Test]
    procedure Ok_SetsErrorCodeToSOK;

    [Test]
    procedure Fail_SetsSuccessFalse;

    [Test]
    procedure Fail_SetsErrorCode;

    [Test]
    procedure Fail_WithMessage_SetsErrorMessage;

    [Test]
    procedure Fail_WithoutMessage_GeneratesDefaultMessage;
  end;

  [TestFixture]
  TWinRTHStringTests = class
  public
    [Test]
    procedure CreateHString_EmptyString_ReturnsZero;

    [Test]
    procedure CreateHString_NonEmptyString_ReturnsNonZero;

    [Test]
    procedure FreeHString_ZeroHandle_DoesNotRaise;
  end;

  [TestFixture]
  TWinRTInitTests = class
  public
    [Test]
    procedure EnsureWinRTInitialized_Succeeds;

    [Test]
    procedure EnsureWinRTInitialized_CanBeCalledMultipleTimes;
  end;

  [TestFixture]
  TExecuteAsyncWithTimeoutTests = class
  public
    [Test]
    procedure ExecuteAsyncWithTimeout_NilOp_ReturnsFail;
  end;

implementation

{ TWinRTAsyncResultTests }

procedure TWinRTAsyncResultTests.Ok_SetsSuccessTrue;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Ok;

  Assert.IsTrue(Result.Success, 'Success should be True');
end;

procedure TWinRTAsyncResultTests.Ok_SetsErrorCodeToSOK;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Ok;

  Assert.AreEqual(S_OK, Result.ErrorCode, 'ErrorCode should be S_OK');
end;

procedure TWinRTAsyncResultTests.Fail_SetsSuccessFalse;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Fail(E_FAIL);

  Assert.IsFalse(Result.Success, 'Success should be False');
end;

procedure TWinRTAsyncResultTests.Fail_SetsErrorCode;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Fail(E_NOTIMPL);

  Assert.AreEqual(E_NOTIMPL, Result.ErrorCode, 'ErrorCode should match');
end;

procedure TWinRTAsyncResultTests.Fail_WithMessage_SetsErrorMessage;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Fail(E_FAIL, 'Custom error message');

  Assert.AreEqual('Custom error message', Result.ErrorMessage, 'ErrorMessage should match');
end;

procedure TWinRTAsyncResultTests.Fail_WithoutMessage_GeneratesDefaultMessage;
var
  Result: TWinRTAsyncResult;
begin
  Result := TWinRTAsyncResult.Fail(E_FAIL);

  Assert.Contains(Result.ErrorMessage, '0x', 'ErrorMessage should contain hex code');
end;

{ TWinRTHStringTests }

procedure TWinRTHStringTests.CreateHString_EmptyString_ReturnsZero;
var
  Str: HSTRING;
begin
  Str := CreateHString('');

  Assert.AreEqual(HSTRING(0), Str, 'Empty string should return zero handle');
end;

procedure TWinRTHStringTests.CreateHString_NonEmptyString_ReturnsNonZero;
var
  Str: HSTRING;
begin
  Str := CreateHString('Test');
  try
    Assert.AreNotEqual(HSTRING(0), Str, 'Non-empty string should return non-zero handle');
  finally
    FreeHString(Str);
  end;
end;

procedure TWinRTHStringTests.FreeHString_ZeroHandle_DoesNotRaise;
begin
  // Should not raise any exception
  FreeHString(0);

  Assert.Pass('FreeHString(0) completed without error');
end;

{ TWinRTInitTests }

procedure TWinRTInitTests.EnsureWinRTInitialized_Succeeds;
var
  Result: Boolean;
begin
  Result := EnsureWinRTInitialized;

  Assert.IsTrue(Result, 'WinRT initialization should succeed');
end;

procedure TWinRTInitTests.EnsureWinRTInitialized_CanBeCalledMultipleTimes;
var
  Result1, Result2: Boolean;
begin
  Result1 := EnsureWinRTInitialized;
  Result2 := EnsureWinRTInitialized;

  Assert.IsTrue(Result1, 'First initialization should succeed');
  Assert.IsTrue(Result2, 'Second initialization should also succeed');
end;

{ TExecuteAsyncWithTimeoutTests }

procedure TExecuteAsyncWithTimeoutTests.ExecuteAsyncWithTimeout_NilOp_ReturnsFail;
var
  Result: TWinRTAsyncResult;
begin
  Result := ExecuteAsyncWithTimeout(nil, 1000);

  Assert.IsFalse(Result.Success, 'Nil operation should return failure');
  Assert.AreEqual(E_POINTER, Result.ErrorCode, 'ErrorCode should be E_POINTER');
end;

initialization
  TDUnitX.RegisterTestFixture(TWinRTAsyncResultTests);
  TDUnitX.RegisterTestFixture(TWinRTHStringTests);
  TDUnitX.RegisterTestFixture(TWinRTInitTests);
  TDUnitX.RegisterTestFixture(TExecuteAsyncWithTimeoutTests);

end.
