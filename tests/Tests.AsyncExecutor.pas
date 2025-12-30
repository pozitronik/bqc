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
  App.AsyncExecutor,
  Tests.Mocks;

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

initialization
  TDUnitX.RegisterTestFixture(TThreadAsyncExecutorTests);
  TDUnitX.RegisterTestFixture(TMockAsyncExecutorTests);

end.
