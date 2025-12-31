{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Async Executor Unit Tests                       }
{                                                       }
{       Tests for IAsyncExecutor implementations.       }
{                                                       }
{*******************************************************}

unit Tests.AsyncExecutor;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  App.AsyncExecutor;

type
  [TestFixture]
  TSynchronousExecutorTests = class
  private
    FExecutor: TSynchronousExecutor;
    FWorkExecuted: Boolean;
    FCallbackExecuted: Boolean;
    FErrorHandled: Boolean;
    FLastError: Exception;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Execute_RunsWorkImmediately;

    [Test]
    procedure Execute_IncrementsExecuteCount;

    [Test]
    procedure Execute_NilWork_DoesNotRaise;

    [Test]
    procedure ExecuteWithCallback_RunsWorkAndCallback;

    [Test]
    procedure ExecuteWithCallback_NilWork_RunsCallbackOnly;

    [Test]
    procedure ExecuteWithCallback_NilCallback_RunsWorkOnly;

    [Test]
    procedure ExecuteWithErrorHandler_OnSuccess_RunsCallback;

    [Test]
    procedure ExecuteWithErrorHandler_OnError_RunsErrorHandler;

    [Test]
    procedure ExecuteWithErrorHandler_SetsLastException;

    [Test]
    procedure ExecuteCount_TracksAllCalls;
  end;

  [TestFixture]
  TThreadPoolExecutorTests = class
  private
    FExecutor: IAsyncExecutor;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Execute_NilWork_DoesNotRaise;

    [Test]
    procedure ExecuteWithCallback_NilWork_DoesNotRaise;
  end;

  [TestFixture]
  TFactoryFunctionTests = class
  public
    [Test]
    procedure CreateAsyncExecutor_ReturnsThreadPoolExecutor;

    [Test]
    procedure CreateSynchronousExecutor_ReturnsSynchronousExecutor;
  end;

implementation

{ TSynchronousExecutorTests }

procedure TSynchronousExecutorTests.Setup;
begin
  FExecutor := TSynchronousExecutor.Create;
  FWorkExecuted := False;
  FCallbackExecuted := False;
  FErrorHandled := False;
  FLastError := nil;
end;

procedure TSynchronousExecutorTests.TearDown;
begin
  FExecutor.Free;
end;

procedure TSynchronousExecutorTests.Execute_RunsWorkImmediately;
begin
  FExecutor.Execute(
    procedure
    begin
      FWorkExecuted := True;
    end
  );

  Assert.IsTrue(FWorkExecuted, 'Work should be executed immediately');
end;

procedure TSynchronousExecutorTests.Execute_IncrementsExecuteCount;
begin
  Assert.AreEqual(0, FExecutor.ExecuteCount, 'Initial count should be 0');

  FExecutor.Execute(procedure begin end);

  Assert.AreEqual(1, FExecutor.ExecuteCount, 'Count should be 1 after Execute');
end;

procedure TSynchronousExecutorTests.Execute_NilWork_DoesNotRaise;
begin
  // Should not raise any exception
  FExecutor.Execute(nil);

  Assert.AreEqual(1, FExecutor.ExecuteCount, 'Count should increment even for nil');
end;

procedure TSynchronousExecutorTests.ExecuteWithCallback_RunsWorkAndCallback;
begin
  FExecutor.ExecuteWithCallback(
    procedure
    begin
      FWorkExecuted := True;
    end,
    procedure
    begin
      FCallbackExecuted := True;
    end
  );

  Assert.IsTrue(FWorkExecuted, 'Work should be executed');
  Assert.IsTrue(FCallbackExecuted, 'Callback should be executed');
end;

procedure TSynchronousExecutorTests.ExecuteWithCallback_NilWork_RunsCallbackOnly;
begin
  FExecutor.ExecuteWithCallback(
    nil,
    procedure
    begin
      FCallbackExecuted := True;
    end
  );

  Assert.IsFalse(FWorkExecuted, 'Work should not be executed (was nil)');
  Assert.IsTrue(FCallbackExecuted, 'Callback should be executed');
end;

procedure TSynchronousExecutorTests.ExecuteWithCallback_NilCallback_RunsWorkOnly;
begin
  FExecutor.ExecuteWithCallback(
    procedure
    begin
      FWorkExecuted := True;
    end,
    nil
  );

  Assert.IsTrue(FWorkExecuted, 'Work should be executed');
  // No exception should occur for nil callback
  Assert.Pass('Nil callback handled correctly');
end;

procedure TSynchronousExecutorTests.ExecuteWithErrorHandler_OnSuccess_RunsCallback;
begin
  FExecutor.ExecuteWithErrorHandler(
    procedure
    begin
      FWorkExecuted := True;
    end,
    procedure
    begin
      FCallbackExecuted := True;
    end,
    procedure(const E: Exception)
    begin
      FErrorHandled := True;
    end
  );

  Assert.IsTrue(FWorkExecuted, 'Work should be executed');
  Assert.IsTrue(FCallbackExecuted, 'Callback should be executed on success');
  Assert.IsFalse(FErrorHandled, 'Error handler should not be called on success');
end;

procedure TSynchronousExecutorTests.ExecuteWithErrorHandler_OnError_RunsErrorHandler;
begin
  FExecutor.ExecuteWithErrorHandler(
    procedure
    begin
      raise Exception.Create('Test error');
    end,
    procedure
    begin
      FCallbackExecuted := True;
    end,
    procedure(const E: Exception)
    begin
      FErrorHandled := True;
      FLastError := E;
    end
  );

  Assert.IsFalse(FCallbackExecuted, 'Callback should not be called on error');
  Assert.IsTrue(FErrorHandled, 'Error handler should be called');
  Assert.IsNotNull(FLastError, 'Error should be captured');
  Assert.AreEqual('Test error', FLastError.Message, 'Error message should match');
end;

procedure TSynchronousExecutorTests.ExecuteWithErrorHandler_SetsLastException;
begin
  FExecutor.ExecuteWithErrorHandler(
    procedure
    begin
      raise Exception.Create('Test exception');
    end,
    nil,
    nil
  );

  Assert.IsNotNull(FExecutor.LastException, 'LastException should be set');
  Assert.AreEqual('Test exception', FExecutor.LastException.Message);
end;

procedure TSynchronousExecutorTests.ExecuteCount_TracksAllCalls;
begin
  FExecutor.Execute(procedure begin end);
  FExecutor.ExecuteWithCallback(procedure begin end, nil);
  FExecutor.ExecuteWithErrorHandler(procedure begin end, nil, nil);

  Assert.AreEqual(3, FExecutor.ExecuteCount, 'All three calls should be tracked');
end;

{ TThreadPoolExecutorTests }

procedure TThreadPoolExecutorTests.Setup;
begin
  FExecutor := CreateAsyncExecutor;
end;

procedure TThreadPoolExecutorTests.Execute_NilWork_DoesNotRaise;
begin
  // Should not raise any exception
  FExecutor.Execute(nil);

  Assert.Pass('Nil work handled correctly');
end;

procedure TThreadPoolExecutorTests.ExecuteWithCallback_NilWork_DoesNotRaise;
begin
  // Should not raise any exception
  FExecutor.ExecuteWithCallback(nil, nil);

  Assert.Pass('Nil work and callback handled correctly');
end;

{ TFactoryFunctionTests }

procedure TFactoryFunctionTests.CreateAsyncExecutor_ReturnsThreadPoolExecutor;
var
  Executor: IAsyncExecutor;
begin
  Executor := CreateAsyncExecutor;

  Assert.IsNotNull(Executor, 'Executor should not be nil');
  // Verify it's the production implementation by checking type
  Assert.IsTrue(Executor is TThreadPoolExecutor, 'Should return TThreadPoolExecutor');
end;

procedure TFactoryFunctionTests.CreateSynchronousExecutor_ReturnsSynchronousExecutor;
var
  Executor: IAsyncExecutor;
begin
  Executor := CreateSynchronousExecutor;

  Assert.IsNotNull(Executor, 'Executor should not be nil');
  // Verify it's the test implementation by checking type
  Assert.IsTrue(Executor is TSynchronousExecutor, 'Should return TSynchronousExecutor');
end;

initialization
  TDUnitX.RegisterTestFixture(TSynchronousExecutorTests);
  TDUnitX.RegisterTestFixture(TThreadPoolExecutorTests);
  TDUnitX.RegisterTestFixture(TFactoryFunctionTests);

end.
