{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Async Executor Tests                            }
{                                                       }
{*******************************************************}

unit Tests.AsyncExecutor;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  Vcl.Forms,
  App.AsyncExecutor,
  Tests.Mocks;

type
  /// <summary>
  /// Exception class that tracks when its destructor is called.
  /// Used to detect use-after-free bugs in async error handling.
  /// </summary>
  TTrackableException = class(Exception)
  private
    class var FInstanceCount: Integer;
    class var FDestroyCount: Integer;
    class var FLastDestroyedMessage: string;
  public
    constructor Create(const Msg: string);
    destructor Destroy; override;
    class procedure ResetTracking;
    /// <summary>Number of TTrackableException instances created.</summary>
    class function GetInstanceCount: Integer;
    /// <summary>Number of TTrackableException instances destroyed.</summary>
    class function GetDestroyCount: Integer;
    /// <summary>Message of the last destroyed exception.</summary>
    class function GetLastDestroyedMessage: string;
  end;

type
  /// <summary>
  /// Tests for TThreadAsyncExecutor - production implementation.
  /// Note: These tests verify basic functionality but can't fully test
  /// async behavior without timing dependencies.
  /// </summary>
  [TestFixture]
  TThreadAsyncExecutorTests = class
  private
    FExecutor: IAsyncExecutor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Create_ReturnsNonNilInstance;

    [Test]
    procedure RunAsync_ExecutesProcedure;

    [Test]
    procedure RunDelayed_ExecutesProcedureAfterDelay;
  end;

  /// <summary>
  /// Tests for TSynchronousExecutor - synchronous test implementation.
  /// </summary>
  [TestFixture]
  TSynchronousExecutorTests = class
  private
    FExecutor: TSynchronousExecutor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure RunAsync_ExecutesImmediately;

    [Test]
    procedure RunAsync_IncrementsCount;

    [Test]
    procedure RunAsync_HandlesNilProc;

    [Test]
    procedure RunDelayed_ExecutesImmediately;

    [Test]
    procedure RunDelayed_IgnoresDelay;

    [Test]
    procedure RunDelayed_IncrementsCount;

    [Test]
    procedure RunDelayed_HandlesNilProc;

    [Test]
    procedure RunAsyncWithErrorHandler_ExecutesSuccessfully;

    [Test]
    procedure RunAsyncWithErrorHandler_CatchesException;

    [Test]
    procedure RunAsyncWithErrorHandler_CallsErrorHandler;

    [Test]
    procedure RunAsyncWithErrorHandler_StoresLastException;

    [Test]
    procedure RunAsyncWithErrorHandler_IncrementsCount;

    [Test]
    procedure RunAsyncWithErrorHandler_HandlesNilWork;

    [Test]
    procedure RunAsyncWithErrorHandler_HandlesNilErrorHandler;

    [Test]
    procedure ExecutionCounts_StartAtZero;

    [Test]
    procedure ExecutionCounts_TrackMultipleCalls;
  end;

  /// <summary>
  /// Tests for TMockAsyncExecutor - test implementation.
  /// </summary>
  [TestFixture]
  TMockAsyncExecutorTests = class
  private
    FExecutor: TMockAsyncExecutor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Default behavior tests }
    [Test]
    procedure Create_DefaultsToNonSynchronous;

    { Synchronous mode tests }
    [Test]
    procedure RunAsync_Synchronous_ExecutesImmediately;

    [Test]
    procedure RunDelayed_Synchronous_ExecutesImmediately;

    [Test]
    procedure RunDelayed_Synchronous_RecordsDelay;

    { Non-synchronous mode tests }
    [Test]
    procedure RunAsync_NonSynchronous_DefersExecution;

    [Test]
    procedure RunDelayed_NonSynchronous_DefersExecution;

    [Test]
    procedure ExecutePending_ExecutesAllQueuedProcs;

    [Test]
    procedure ExecutePending_ClearsPendingQueue;

    [Test]
    procedure ClearPending_DiscardsQueuedProcs;

    { Call count tracking tests }
    [Test]
    procedure RunAsync_IncrementsCallCount;

    [Test]
    procedure RunDelayed_IncrementsCallCount;

    [Test]
    procedure PendingCount_ReflectsQueuedProcs;
  end;

  /// <summary>
  /// Tests for exception lifetime in TThreadAsyncExecutor.RunAsyncWithErrorHandler.
  /// These tests verify correct exception object handling across thread boundaries.
  /// </summary>
  [TestFixture]
  TAsyncExecutorExceptionLifetimeTests = class
  private
    FExecutor: IAsyncExecutor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>
    /// Verifies that the exception object is NOT destroyed before the error handler is called.
    /// This test FAILS if there is a use-after-free bug where the exception is captured
    /// by reference in TThread.Queue but destroyed when the except block exits.
    /// CORRECT behavior: Exception should be alive when handler runs (DestroyCount = 0).
    /// BUGGY behavior: Exception is destroyed before handler runs (DestroyCount > 0).
    /// </summary>
    [Test]
    procedure RunAsyncWithErrorHandler_ExceptionNotDestroyedBeforeHandler;

    /// <summary>
    /// Verifies that exception message is accessible in error handler.
    /// If exception was freed, accessing Message may return garbage or crash.
    /// </summary>
    [Test]
    procedure RunAsyncWithErrorHandler_ExceptionMessageAccessibleInHandler;
  end;

implementation

{ TThreadAsyncExecutorTests }

procedure TThreadAsyncExecutorTests.Setup;
begin
  FExecutor := CreateAsyncExecutor;
end;

procedure TThreadAsyncExecutorTests.TearDown;
begin
  FExecutor := nil;
  // Give async operations time to complete
  Sleep(50);
end;

procedure TThreadAsyncExecutorTests.Create_ReturnsNonNilInstance;
begin
  Assert.IsNotNull(FExecutor);
end;

procedure TThreadAsyncExecutorTests.RunAsync_ExecutesProcedure;
var
  Executed: Boolean;
begin
  Executed := False;

  FExecutor.RunAsync(
    procedure
    begin
      Executed := True;
    end
  );

  // Wait for async completion
  Sleep(100);

  Assert.IsTrue(Executed, 'Procedure should have been executed');
end;

procedure TThreadAsyncExecutorTests.RunDelayed_ExecutesProcedureAfterDelay;
var
  Executed: Boolean;
begin
  Executed := False;

  FExecutor.RunDelayed(
    procedure
    begin
      Executed := True;
    end,
    50  // 50ms delay
  );

  // Should not be executed immediately
  Assert.IsFalse(Executed, 'Procedure should not execute immediately');

  // Wait for delay + execution
  Sleep(150);

  Assert.IsTrue(Executed, 'Procedure should have been executed after delay');
end;

{ TSynchronousExecutorTests }

procedure TSynchronousExecutorTests.Setup;
begin
  FExecutor := TSynchronousExecutor.Create;
end;

procedure TSynchronousExecutorTests.TearDown;
begin
  FExecutor.Free;
end;

procedure TSynchronousExecutorTests.RunAsync_ExecutesImmediately;
var
  Executed: Boolean;
begin
  Executed := False;

  FExecutor.RunAsync(
    procedure
    begin
      Executed := True;
    end
  );

  Assert.IsTrue(Executed, 'Procedure should execute immediately');
end;

procedure TSynchronousExecutorTests.RunAsync_IncrementsCount;
begin
  Assert.AreEqual(0, FExecutor.RunAsyncCount, 'Initial count should be 0');

  FExecutor.RunAsync(procedure begin end);
  Assert.AreEqual(1, FExecutor.RunAsyncCount, 'Count should be 1 after first call');

  FExecutor.RunAsync(procedure begin end);
  Assert.AreEqual(2, FExecutor.RunAsyncCount, 'Count should be 2 after second call');
end;

procedure TSynchronousExecutorTests.RunAsync_HandlesNilProc;
begin
  // Should not crash
  FExecutor.RunAsync(nil);
  Assert.AreEqual(1, FExecutor.RunAsyncCount, 'Count should increment even with nil proc');
end;

procedure TSynchronousExecutorTests.RunDelayed_ExecutesImmediately;
var
  Executed: Boolean;
begin
  Executed := False;

  FExecutor.RunDelayed(
    procedure
    begin
      Executed := True;
    end,
    1000  // Delay should be ignored
  );

  Assert.IsTrue(Executed, 'Procedure should execute immediately despite delay');
end;

procedure TSynchronousExecutorTests.RunDelayed_IgnoresDelay;
var
  StartTime, EndTime: TDateTime;
begin
  StartTime := Now;

  FExecutor.RunDelayed(
    procedure
    begin
    end,
    1000  // 1 second delay (ignored)
  );

  EndTime := Now;

  // Execution should be instant (< 100ms)
  Assert.IsTrue(MilliSecondsBetween(EndTime, StartTime) < 100,
    'Delay should be ignored in synchronous executor');
end;

procedure TSynchronousExecutorTests.RunDelayed_IncrementsCount;
begin
  Assert.AreEqual(0, FExecutor.RunDelayedCount, 'Initial count should be 0');

  FExecutor.RunDelayed(procedure begin end, 100);
  Assert.AreEqual(1, FExecutor.RunDelayedCount);

  FExecutor.RunDelayed(procedure begin end, 200);
  Assert.AreEqual(2, FExecutor.RunDelayedCount);
end;

procedure TSynchronousExecutorTests.RunDelayed_HandlesNilProc;
begin
  // Should not crash
  FExecutor.RunDelayed(nil, 100);
  Assert.AreEqual(1, FExecutor.RunDelayedCount, 'Count should increment even with nil proc');
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_ExecutesSuccessfully;
var
  Executed: Boolean;
begin
  Executed := False;

  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      Executed := True;
    end,
    procedure(const E: Exception)
    begin
      Assert.Fail('Error handler should not be called on success');
    end
  );

  Assert.IsTrue(Executed, 'Work should execute successfully');
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_CatchesException;
begin
  // Should not crash - exception should be caught
  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise Exception.Create('Test exception');
    end,
    procedure(const E: Exception)
    begin
      // Exception caught
    end
  );

  Assert.Pass('Exception was caught and handled');
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_CallsErrorHandler;
var
  ErrorHandled: Boolean;
  ErrorMessage: string;
begin
  ErrorHandled := False;
  ErrorMessage := '';

  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise Exception.Create('Expected error');
    end,
    procedure(const E: Exception)
    begin
      ErrorHandled := True;
      ErrorMessage := E.Message;
    end
  );

  Assert.IsTrue(ErrorHandled, 'Error handler should be called');
  Assert.AreEqual('Expected error', ErrorMessage, 'Error message should match');
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_StoresLastException;
begin
  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise Exception.Create('Stored exception');
    end,
    procedure(const E: Exception)
    begin
    end
  );

  Assert.IsNotNull(FExecutor.LastException, 'Last exception should be stored');
  Assert.AreEqual('Stored exception', FExecutor.LastException.Message);
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_IncrementsCount;
begin
  Assert.AreEqual(0, FExecutor.RunAsyncWithErrorHandlerCount, 'Initial count should be 0');

  FExecutor.RunAsyncWithErrorHandler(
    procedure begin end,
    procedure(const E: Exception) begin end
  );
  Assert.AreEqual(1, FExecutor.RunAsyncWithErrorHandlerCount);

  FExecutor.RunAsyncWithErrorHandler(
    procedure begin raise Exception.Create('Test'); end,
    procedure(const E: Exception) begin end
  );
  Assert.AreEqual(2, FExecutor.RunAsyncWithErrorHandlerCount);
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_HandlesNilWork;
begin
  // Should not crash
  FExecutor.RunAsyncWithErrorHandler(
    nil,
    procedure(const E: Exception) begin end
  );
  Assert.AreEqual(1, FExecutor.RunAsyncWithErrorHandlerCount, 'Count should increment');
end;

procedure TSynchronousExecutorTests.RunAsyncWithErrorHandler_HandlesNilErrorHandler;
begin
  // Should not crash even without error handler
  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise Exception.Create('No handler');
    end,
    nil
  );
  Assert.Pass('Exception was caught even without error handler');
end;

procedure TSynchronousExecutorTests.ExecutionCounts_StartAtZero;
begin
  Assert.AreEqual(0, FExecutor.RunAsyncCount);
  Assert.AreEqual(0, FExecutor.RunDelayedCount);
  Assert.AreEqual(0, FExecutor.RunAsyncWithErrorHandlerCount);
end;

procedure TSynchronousExecutorTests.ExecutionCounts_TrackMultipleCalls;
begin
  FExecutor.RunAsync(procedure begin end);
  FExecutor.RunAsync(procedure begin end);
  FExecutor.RunDelayed(procedure begin end, 100);
  FExecutor.RunAsyncWithErrorHandler(procedure begin end, nil);

  Assert.AreEqual(2, FExecutor.RunAsyncCount);
  Assert.AreEqual(1, FExecutor.RunDelayedCount);
  Assert.AreEqual(1, FExecutor.RunAsyncWithErrorHandlerCount);
end;

{ TMockAsyncExecutorTests }

procedure TMockAsyncExecutorTests.Setup;
begin
  FExecutor := TMockAsyncExecutor.Create;
end;

procedure TMockAsyncExecutorTests.TearDown;
begin
  FExecutor.Free;
end;

procedure TMockAsyncExecutorTests.Create_DefaultsToNonSynchronous;
begin
  Assert.IsFalse(FExecutor.Synchronous, 'Should default to non-synchronous mode');
end;

procedure TMockAsyncExecutorTests.RunAsync_Synchronous_ExecutesImmediately;
var
  Executed: Boolean;
begin
  FExecutor.Synchronous := True;
  Executed := False;

  FExecutor.RunAsync(
    procedure
    begin
      Executed := True;
    end
  );

  Assert.IsTrue(Executed, 'Procedure should execute immediately in synchronous mode');
end;

procedure TMockAsyncExecutorTests.RunDelayed_Synchronous_ExecutesImmediately;
var
  Executed: Boolean;
begin
  FExecutor.Synchronous := True;
  Executed := False;

  FExecutor.RunDelayed(
    procedure
    begin
      Executed := True;
    end,
    1000  // Delay is ignored in sync mode
  );

  Assert.IsTrue(Executed, 'Procedure should execute immediately in synchronous mode');
end;

procedure TMockAsyncExecutorTests.RunDelayed_Synchronous_RecordsDelay;
begin
  FExecutor.Synchronous := True;

  FExecutor.RunDelayed(
    procedure
    begin
    end,
    500
  );

  Assert.AreEqual(500, FExecutor.LastDelayMs, 'Should record the delay value');
end;

procedure TMockAsyncExecutorTests.RunAsync_NonSynchronous_DefersExecution;
var
  Executed: Boolean;
begin
  FExecutor.Synchronous := False;
  Executed := False;

  FExecutor.RunAsync(
    procedure
    begin
      Executed := True;
    end
  );

  Assert.IsFalse(Executed, 'Procedure should be deferred in non-synchronous mode');
end;

procedure TMockAsyncExecutorTests.RunDelayed_NonSynchronous_DefersExecution;
var
  Executed: Boolean;
begin
  FExecutor.Synchronous := False;
  Executed := False;

  FExecutor.RunDelayed(
    procedure
    begin
      Executed := True;
    end,
    100
  );

  Assert.IsFalse(Executed, 'Procedure should be deferred in non-synchronous mode');
end;

procedure TMockAsyncExecutorTests.ExecutePending_ExecutesAllQueuedProcs;
var
  Counter: Integer;
begin
  FExecutor.Synchronous := False;
  Counter := 0;

  FExecutor.RunAsync(procedure begin Inc(Counter); end);
  FExecutor.RunAsync(procedure begin Inc(Counter); end);
  FExecutor.RunAsync(procedure begin Inc(Counter); end);

  Assert.AreEqual(0, Counter, 'Counter should be 0 before ExecutePending');

  FExecutor.ExecutePending;

  Assert.AreEqual(3, Counter, 'All 3 procedures should have executed');
end;

procedure TMockAsyncExecutorTests.ExecutePending_ClearsPendingQueue;
begin
  FExecutor.Synchronous := False;

  FExecutor.RunAsync(procedure begin end);
  FExecutor.RunAsync(procedure begin end);

  Assert.AreEqual(2, FExecutor.PendingCount, 'Should have 2 pending procs');

  FExecutor.ExecutePending;

  Assert.AreEqual(0, FExecutor.PendingCount, 'Queue should be cleared after ExecutePending');
end;

procedure TMockAsyncExecutorTests.ClearPending_DiscardsQueuedProcs;
var
  Executed: Boolean;
begin
  FExecutor.Synchronous := False;
  Executed := False;

  FExecutor.RunAsync(
    procedure
    begin
      Executed := True;
    end
  );

  FExecutor.ClearPending;
  FExecutor.ExecutePending;  // Nothing should execute

  Assert.IsFalse(Executed, 'Cleared procedure should not execute');
end;

procedure TMockAsyncExecutorTests.RunAsync_IncrementsCallCount;
begin
  Assert.AreEqual(0, FExecutor.RunAsyncCallCount, 'Initial call count should be 0');

  FExecutor.RunAsync(procedure begin end);
  Assert.AreEqual(1, FExecutor.RunAsyncCallCount);

  FExecutor.RunAsync(procedure begin end);
  Assert.AreEqual(2, FExecutor.RunAsyncCallCount);
end;

procedure TMockAsyncExecutorTests.RunDelayed_IncrementsCallCount;
begin
  Assert.AreEqual(0, FExecutor.RunDelayedCallCount, 'Initial call count should be 0');

  FExecutor.RunDelayed(procedure begin end, 100);
  Assert.AreEqual(1, FExecutor.RunDelayedCallCount);

  FExecutor.RunDelayed(procedure begin end, 200);
  Assert.AreEqual(2, FExecutor.RunDelayedCallCount);
end;

procedure TMockAsyncExecutorTests.PendingCount_ReflectsQueuedProcs;
begin
  FExecutor.Synchronous := False;

  Assert.AreEqual(0, FExecutor.PendingCount);

  FExecutor.RunAsync(procedure begin end);
  Assert.AreEqual(1, FExecutor.PendingCount);

  FExecutor.RunDelayed(procedure begin end, 100);
  Assert.AreEqual(2, FExecutor.PendingCount);

  FExecutor.ClearPending;
  Assert.AreEqual(0, FExecutor.PendingCount);
end;

{ TTrackableException }

constructor TTrackableException.Create(const Msg: string);
begin
  inherited Create(Msg);
  Inc(FInstanceCount);
end;

destructor TTrackableException.Destroy;
begin
  Inc(FDestroyCount);
  FLastDestroyedMessage := Message;
  inherited Destroy;
end;

class procedure TTrackableException.ResetTracking;
begin
  FInstanceCount := 0;
  FDestroyCount := 0;
  FLastDestroyedMessage := '';
end;

class function TTrackableException.GetInstanceCount: Integer;
begin
  Result := TTrackableException.FInstanceCount;
end;

class function TTrackableException.GetDestroyCount: Integer;
begin
  Result := TTrackableException.FDestroyCount;
end;

class function TTrackableException.GetLastDestroyedMessage: string;
begin
  Result := TTrackableException.FLastDestroyedMessage;
end;

{ TAsyncExecutorExceptionLifetimeTests }

procedure TAsyncExecutorExceptionLifetimeTests.Setup;
begin
  TTrackableException.ResetTracking;
  FExecutor := TThreadAsyncExecutor.Create;
end;

procedure TAsyncExecutorExceptionLifetimeTests.TearDown;
begin
  FExecutor := nil;
  // Allow any pending async operations to complete
  Sleep(100);
  Application.ProcessMessages;
end;

procedure TAsyncExecutorExceptionLifetimeTests.RunAsyncWithErrorHandler_ExceptionNotDestroyedBeforeHandler;
var
  HandlerCalled: Boolean;
  DestroyCountWhenHandlerCalled: Integer;
  WaitIterations: Integer;
const
  MAX_WAIT_ITERATIONS = 200;  // 200 * 10ms = 2 seconds max wait
begin
  // Arrange
  HandlerCalled := False;
  DestroyCountWhenHandlerCalled := -1;

  // Act: Run async work that raises a trackable exception
  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise TTrackableException.Create('Test exception for lifetime tracking');
    end,
    procedure(const E: Exception)
    begin
      // Capture destroy count at the moment handler is called
      DestroyCountWhenHandlerCalled := TTrackableException.GetDestroyCount;
      HandlerCalled := True;
    end
  );

  // Wait for handler to be called via TThread.Queue
  // In console applications, CheckSynchronize processes queued calls
  // ProcessMessages handles VCL message queue
  WaitIterations := 0;
  while (not HandlerCalled) and (WaitIterations < MAX_WAIT_ITERATIONS) do
  begin
    Sleep(10);
    CheckSynchronize(0);  // Process TThread.Queue calls
    Application.ProcessMessages;
    Inc(WaitIterations);
  end;

  // Assert: Handler must have been called
  Assert.IsTrue(HandlerCalled,
    'Error handler was not called within timeout. Test infrastructure issue.');

  // Assert: Exception should NOT be destroyed before handler is called
  // CORRECT behavior: DestroyCount = 0 when handler runs
  // BUGGY behavior: DestroyCount > 0 (exception freed before handler)
  Assert.AreEqual(0, DestroyCountWhenHandlerCalled,
    'BUG DETECTED: Exception was destroyed BEFORE error handler was called. ' +
    'This is a use-after-free vulnerability. DestroyCount=' +
    IntToStr(DestroyCountWhenHandlerCalled));
end;

procedure TAsyncExecutorExceptionLifetimeTests.RunAsyncWithErrorHandler_ExceptionMessageAccessibleInHandler;
var
  HandlerCalled: Boolean;
  ReceivedMessage: string;
  MessageAccessFailed: Boolean;
  WaitIterations: Integer;
const
  EXPECTED_MESSAGE = 'Unique test message 12345';
  MAX_WAIT_ITERATIONS = 200;
begin
  // Arrange
  HandlerCalled := False;
  ReceivedMessage := '';
  MessageAccessFailed := False;

  // Act: Run async work that raises exception with specific message
  FExecutor.RunAsyncWithErrorHandler(
    procedure
    begin
      raise TTrackableException.Create(EXPECTED_MESSAGE);
    end,
    procedure(const E: Exception)
    begin
      HandlerCalled := True;
      try
        ReceivedMessage := E.Message;
      except
        // If exception object is freed, accessing Message may crash
        MessageAccessFailed := True;
        ReceivedMessage := '<ACCESS FAILED>';
      end;
    end
  );

  // Wait for handler
  WaitIterations := 0;
  while (not HandlerCalled) and (WaitIterations < MAX_WAIT_ITERATIONS) do
  begin
    Sleep(10);
    CheckSynchronize(0);  // Process TThread.Queue calls
    Application.ProcessMessages;
    Inc(WaitIterations);
  end;

  // Assert
  Assert.IsTrue(HandlerCalled,
    'Error handler was not called within timeout.');

  Assert.IsFalse(MessageAccessFailed,
    'BUG DETECTED: Accessing exception message caused an error. ' +
    'Exception object was likely freed before handler ran.');

  Assert.AreEqual(EXPECTED_MESSAGE, ReceivedMessage,
    'BUG DETECTED: Exception message does not match. ' +
    'Expected: "' + EXPECTED_MESSAGE + '", Got: "' + ReceivedMessage + '". ' +
    'Exception object may have been corrupted or freed.');
end;

initialization
  TDUnitX.RegisterTestFixture(TThreadAsyncExecutorTests);
  TDUnitX.RegisterTestFixture(TSynchronousExecutorTests);
  TDUnitX.RegisterTestFixture(TMockAsyncExecutorTests);
  TDUnitX.RegisterTestFixture(TAsyncExecutorExceptionLifetimeTests);

end.
