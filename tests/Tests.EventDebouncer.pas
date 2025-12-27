{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Event Debouncer Unit Tests                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.EventDebouncer;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.SyncObjs,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.EventDebouncer;

type
  /// <summary>
  /// Test fixture for TDeviceEventDebouncer class.
  /// Tests event debouncing logic, thread safety, and edge cases.
  /// </summary>
  [TestFixture]
  TDeviceEventDebouncerTests = class
  private
    FDebouncer: IEventDebouncer;
    const
      TEST_ADDRESS_1 = UInt64($001122334455);
      TEST_ADDRESS_2 = UInt64($665544332211);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor tests }
    [Test]
    procedure Create_DefaultDebounceMs_Is500;

    [Test]
    procedure Create_CustomDebounceMs_SetsValue;

    [Test]
    procedure Create_ZeroDebounceMs_AllowsImmediateReprocess;

    [Test]
    procedure Create_NegativeDebounceMs_BehavesAsZero;

    { ShouldProcess - basic behavior }
    [Test]
    procedure ShouldProcess_FirstEvent_ReturnsTrue;

    [Test]
    procedure ShouldProcess_SameEventWithinDebounce_ReturnsFalse;

    [Test]
    procedure ShouldProcess_DifferentAddress_ReturnsTrue;

    [Test]
    procedure ShouldProcess_DifferentEventType_ReturnsTrue;

    [Test]
    procedure ShouldProcess_DifferentConnectionState_ReturnsTrue;

    { Event type combinations }
    [Test]
    procedure ShouldProcess_ConnectThenDisconnect_BothAllowed;

    [Test]
    procedure ShouldProcess_DisconnectThenConnect_BothAllowed;

    [Test]
    procedure ShouldProcess_AttrChangeThenConnect_BothAllowed;

    { Connection state combinations }
    [Test]
    procedure ShouldProcess_ConnectedThenConnecting_BothAllowed;

    [Test]
    procedure ShouldProcess_ErrorStateEvent_ProcessedNormally;

    [Test]
    procedure ShouldProcess_UnknownStateEvent_ProcessedNormally;

    { Clear tests }
    [Test]
    procedure Clear_AllowsImmediateReprocessing;

    [Test]
    procedure Clear_EmptyDebouncer_DoesNotRaise;

    [Test]
    procedure Clear_AfterMultipleEvents_ClearsAll;

    { DebounceMs property tests }
    [Test]
    procedure DebounceMs_GetReturnsSetValue;

    [Test]
    procedure DebounceMs_ChangeAffectsFutureEvents;

    { Edge cases }
    [Test]
    procedure ShouldProcess_ZeroAddress_HandledCorrectly;

    [Test]
    procedure ShouldProcess_MaxAddress_HandledCorrectly;

    [Test]
    procedure ShouldProcess_RapidSequence_OnlyFirstProcessed;

    [Test]
    procedure ShouldProcess_ManyDifferentDevices_AllProcessed;

    { Interface compliance }
    [Test]
    procedure Interface_GetDebounceMs_ReturnsCorrectValue;

    [Test]
    procedure Interface_SetDebounceMs_UpdatesValue;

    { Thread safety tests }
    [Test]
    procedure ThreadSafety_ConcurrentShouldProcess_NoRaceCondition;

    [Test]
    procedure ThreadSafety_ClearDuringShouldProcess_NoCrash;

    [Test]
    procedure ThreadSafety_SetDebounceMsDuringShouldProcess_Safe;

    { Additional edge case tests }
    [Test]
    procedure ShouldProcess_ZeroDebounceMs_AlwaysAllows;

    [Test]
    procedure ShouldProcess_NegativeDebounceMs_TreatedAsZero;

    [Test]
    procedure ShouldProcess_MaxUInt64Address_Handles;

    [Test]
    procedure ShouldProcess_SameAddressDifferentStates_TrackedSeparately;

    [Test]
    procedure ShouldProcess_RapidFire_DebounceWorks;

    [Test]
    procedure Clear_ThenShouldProcess_AllowsImmediately;

    [Test]
    procedure ShouldProcess_ManyUniqueAddresses_MemoryHandled;

    [Test]
    procedure SetDebounceMs_Boundary_VeryLargeValue;
  end;

  /// <summary>
  /// Test fixture for debouncer timing behavior.
  /// These tests verify timing-related behavior.
  /// </summary>
  [TestFixture]
  TDebouncerTimingTests = class
  private
    FDebouncer: TDeviceEventDebouncer;
    const
      TEST_ADDRESS = UInt64($AABBCCDDEEFF);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure ShouldProcess_WithVeryShortDebounce_AllowsQuickReprocess;
  end;

implementation

{ TDeviceEventDebouncerTests }

procedure TDeviceEventDebouncerTests.Setup;
begin
  // Create with short debounce for faster tests
  FDebouncer := TDeviceEventDebouncer.Create(100);
end;

procedure TDeviceEventDebouncerTests.TearDown;
begin
  FDebouncer := nil;
end;

procedure TDeviceEventDebouncerTests.Create_DefaultDebounceMs_Is500;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create;
  try
    Assert.AreEqual(500, Debouncer.DebounceMs);
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.Create_CustomDebounceMs_SetsValue;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(1000);
  try
    Assert.AreEqual(1000, Debouncer.DebounceMs);
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.Create_ZeroDebounceMs_AllowsImmediateReprocess;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(0);
  try
    // First call
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
    // Second call immediately - should be allowed with 0 debounce
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.Create_NegativeDebounceMs_BehavesAsZero;
var
  Debouncer: TDeviceEventDebouncer;
begin
  // Negative debounce should be clamped to 0 (no debounce)
  Debouncer := TDeviceEventDebouncer.Create(-100);
  try
    // Verify negative value was clamped to 0
    Assert.AreEqual(0, Debouncer.DebounceMs, 'Negative value should be clamped to 0');

    // With 0 debounce, all events should be processed
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
      'With 0 debounce, duplicate events should be allowed');
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_FirstEvent_ReturnsTrue;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
    'First event for a device should always be processed');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_SameEventWithinDebounce_ReturnsFalse;
begin
  // First event - should be processed
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Immediately same event - should be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
    'Duplicate event within debounce window should be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_DifferentAddress_ReturnsTrue;
begin
  // First device connects
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Different device connects - should be allowed
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_2, detConnect, csConnected),
    'Different device address should not be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_DifferentEventType_ReturnsTrue;
begin
  // Connect event
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Disconnect event for same device - should be allowed (different event type)
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detDisconnect, csConnected),
    'Different event type for same device should not be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_DifferentConnectionState_ReturnsTrue;
begin
  // Connected state
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Connecting state for same device/event - should be allowed (different state)
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnecting),
    'Different connection state should not be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ConnectThenDisconnect_BothAllowed;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detDisconnect, csDisconnected));
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_DisconnectThenConnect_BothAllowed;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detDisconnect, csDisconnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_AttrChangeThenConnect_BothAllowed;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detAttributeChange, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ConnectedThenConnecting_BothAllowed;
begin
  // Same event type but different states
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnecting));
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ErrorStateEvent_ProcessedNormally;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csError),
    'Error state events should be processed normally');

  // Duplicate should be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csError));
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_UnknownStateEvent_ProcessedNormally;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csUnknown),
    'Unknown state events should be processed normally');

  // Duplicate should be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csUnknown));
end;

procedure TDeviceEventDebouncerTests.Clear_AllowsImmediateReprocessing;
begin
  // Process an event
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Duplicate would be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

  // Clear the debouncer
  FDebouncer.Clear;

  // Same event should now be allowed
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
    'After Clear, event should be processed again');
end;

procedure TDeviceEventDebouncerTests.Clear_EmptyDebouncer_DoesNotRaise;
begin
  // Clear on empty debouncer should not raise
  FDebouncer.Clear;
  Assert.Pass;
end;

procedure TDeviceEventDebouncerTests.Clear_AfterMultipleEvents_ClearsAll;
begin
  // Process multiple events from different devices
  FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected);
  FDebouncer.ShouldProcess(TEST_ADDRESS_2, detDisconnect, csDisconnected);

  // Clear all
  FDebouncer.Clear;

  // Both should be allowed again
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_2, detDisconnect, csDisconnected));
end;

procedure TDeviceEventDebouncerTests.DebounceMs_GetReturnsSetValue;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(750);
  try
    Assert.AreEqual(750, Debouncer.DebounceMs);
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.DebounceMs_ChangeAffectsFutureEvents;
var
  Debouncer: TDeviceEventDebouncer;
begin
  // Create with large debounce
  Debouncer := TDeviceEventDebouncer.Create(10000);
  try
    // First event
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

    // Duplicate filtered with large debounce
    Assert.IsFalse(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));

    // Change to 0 ms debounce
    Debouncer.DebounceMs := 0;

    // Now same event should be allowed (since 0 means no debounce)
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ZeroAddress_HandledCorrectly;
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(0, detConnect, csConnected),
    'Zero address should be handled');

  Assert.IsFalse(FDebouncer.ShouldProcess(0, detConnect, csConnected),
    'Duplicate zero address event should be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_MaxAddress_HandledCorrectly;
const
  MAX_BT_ADDRESS = UInt64($FFFFFFFFFFFF); // 48 bits all 1s
begin
  Assert.IsTrue(FDebouncer.ShouldProcess(MAX_BT_ADDRESS, detConnect, csConnected),
    'Max address should be handled');

  Assert.IsFalse(FDebouncer.ShouldProcess(MAX_BT_ADDRESS, detConnect, csConnected),
    'Duplicate max address event should be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_RapidSequence_OnlyFirstProcessed;
var
  I: Integer;
  ProcessedCount: Integer;
begin
  ProcessedCount := 0;

  // Rapidly fire 10 identical events
  for I := 1 to 10 do
  begin
    if FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected) then
      Inc(ProcessedCount);
  end;

  Assert.AreEqual(1, ProcessedCount, 'Only first event in rapid sequence should be processed');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ManyDifferentDevices_AllProcessed;
var
  I: Integer;
  ProcessedCount: Integer;
begin
  ProcessedCount := 0;

  // Fire events for 100 different devices
  for I := 1 to 100 do
  begin
    if FDebouncer.ShouldProcess(UInt64(I), detConnect, csConnected) then
      Inc(ProcessedCount);
  end;

  Assert.AreEqual(100, ProcessedCount, 'Events from different devices should all be processed');
end;

procedure TDeviceEventDebouncerTests.Interface_GetDebounceMs_ReturnsCorrectValue;
begin
  Assert.AreEqual(100, FDebouncer.GetDebounceMs);
end;

procedure TDeviceEventDebouncerTests.Interface_SetDebounceMs_UpdatesValue;
begin
  FDebouncer.SetDebounceMs(250);
  Assert.AreEqual(250, FDebouncer.GetDebounceMs);
end;

procedure TDeviceEventDebouncerTests.ThreadSafety_ConcurrentShouldProcess_NoRaceCondition;
const
  THREAD_COUNT = 5;
  ITERATIONS_PER_THREAD = 100;
var
  Threads: array[0..THREAD_COUNT - 1] of TThread;
  ExceptionCount: Integer;
  ExceptionLock: TCriticalSection;
  AllDone: TEvent;
  ThreadsFinished: Integer;
  I: Integer;
  Debouncer: TDeviceEventDebouncer;
begin
  // Create a dedicated debouncer for this test to avoid interface reference issues
  Debouncer := TDeviceEventDebouncer.Create(50);
  ExceptionLock := TCriticalSection.Create;
  AllDone := TEvent.Create(nil, True, False, '');
  try
    ExceptionCount := 0;
    ThreadsFinished := 0;

    // Create 5 threads, each calling ShouldProcess 100 times with different addresses
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I] := TThread.CreateAnonymousThread(
        procedure
        var
          J: Integer;
          ThreadIndex: Integer;
          BaseAddress: UInt64;
        begin
          // Capture the thread index via closure
          ThreadIndex := TThread.CurrentThread.ThreadID mod THREAD_COUNT;
          BaseAddress := UInt64(ThreadIndex) * ITERATIONS_PER_THREAD;
          try
            for J := 0 to ITERATIONS_PER_THREAD - 1 do
            begin
              // Each thread uses its own range of addresses
              Debouncer.ShouldProcess(BaseAddress + UInt64(J), detConnect, csConnected);
              // Also test with same address to stress debounce logic
              Debouncer.ShouldProcess(BaseAddress, detConnect, csConnected);
            end;
          except
            on E: Exception do
            begin
              ExceptionLock.Enter;
              try
                Inc(ExceptionCount);
              finally
                ExceptionLock.Leave;
              end;
            end;
          end;

          ExceptionLock.Enter;
          try
            Inc(ThreadsFinished);
            if ThreadsFinished = THREAD_COUNT then
              AllDone.SetEvent;
          finally
            ExceptionLock.Leave;
          end;
        end
      );
      Threads[I].FreeOnTerminate := False;
    end;

    // Start all threads
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;

    // Wait for all threads to complete (max 10 seconds to avoid infinite wait)
    Assert.AreEqual(wrSignaled, AllDone.WaitFor(10000), 'Threads did not complete in time - possible deadlock');

    // Wait for threads to fully terminate and free them
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;

    Assert.AreEqual(0, ExceptionCount, 'Race condition detected - exceptions occurred during concurrent access');
  finally
    Debouncer.Free;
    ExceptionLock.Free;
    AllDone.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ThreadSafety_ClearDuringShouldProcess_NoCrash;
const
  ITERATIONS = 200;
var
  ProcessThread, ClearThread: TThread;
  ExceptionCount: Integer;
  ExceptionLock: TCriticalSection;
  StopFlag: Boolean;
  AllDone: TEvent;
  ThreadsFinished: Integer;
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(50);
  ExceptionLock := TCriticalSection.Create;
  AllDone := TEvent.Create(nil, True, False, '');
  try
    ExceptionCount := 0;
    StopFlag := False;
    ThreadsFinished := 0;

    // Thread that calls ShouldProcess in a loop
    ProcessThread := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        try
          for J := 0 to ITERATIONS - 1 do
          begin
            if StopFlag then
              Break;
            Debouncer.ShouldProcess(UInt64(J mod 10), detConnect, csConnected);
            Debouncer.ShouldProcess(UInt64(J mod 10), detDisconnect, csDisconnected);
          end;
        except
          on E: Exception do
          begin
            ExceptionLock.Enter;
            try
              Inc(ExceptionCount);
            finally
              ExceptionLock.Leave;
            end;
          end;
        end;

        ExceptionLock.Enter;
        try
          Inc(ThreadsFinished);
          if ThreadsFinished = 2 then
            AllDone.SetEvent;
        finally
          ExceptionLock.Leave;
        end;
      end
    );
    ProcessThread.FreeOnTerminate := False;

    // Thread that calls Clear repeatedly
    ClearThread := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        try
          for J := 0 to ITERATIONS - 1 do
          begin
            if StopFlag then
              Break;
            Debouncer.Clear;
          end;
        except
          on E: Exception do
          begin
            ExceptionLock.Enter;
            try
              Inc(ExceptionCount);
            finally
              ExceptionLock.Leave;
            end;
          end;
        end;

        ExceptionLock.Enter;
        try
          Inc(ThreadsFinished);
          if ThreadsFinished = 2 then
            AllDone.SetEvent;
        finally
          ExceptionLock.Leave;
        end;
      end
    );
    ClearThread.FreeOnTerminate := False;

    // Start both threads
    ProcessThread.Start;
    ClearThread.Start;

    // Wait for both threads to complete (max 10 seconds)
    if AllDone.WaitFor(10000) <> wrSignaled then
    begin
      StopFlag := True;
      Assert.Fail('Threads did not complete in time - possible deadlock');
    end;

    ProcessThread.WaitFor;
    ClearThread.WaitFor;
    ProcessThread.Free;
    ClearThread.Free;

    Assert.AreEqual(0, ExceptionCount, 'Exceptions occurred during concurrent Clear and ShouldProcess');
  finally
    Debouncer.Free;
    ExceptionLock.Free;
    AllDone.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ThreadSafety_SetDebounceMsDuringShouldProcess_Safe;
const
  ITERATIONS = 200;
var
  ProcessThread, SetterThread: TThread;
  ExceptionCount: Integer;
  ExceptionLock: TCriticalSection;
  StopFlag: Boolean;
  AllDone: TEvent;
  ThreadsFinished: Integer;
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(50);
  ExceptionLock := TCriticalSection.Create;
  AllDone := TEvent.Create(nil, True, False, '');
  try
    ExceptionCount := 0;
    StopFlag := False;
    ThreadsFinished := 0;

    // Thread that calls ShouldProcess in a loop
    ProcessThread := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        try
          for J := 0 to ITERATIONS - 1 do
          begin
            if StopFlag then
              Break;
            Debouncer.ShouldProcess(UInt64(J mod 10), detConnect, csConnected);
          end;
        except
          on E: Exception do
          begin
            ExceptionLock.Enter;
            try
              Inc(ExceptionCount);
            finally
              ExceptionLock.Leave;
            end;
          end;
        end;

        ExceptionLock.Enter;
        try
          Inc(ThreadsFinished);
          if ThreadsFinished = 2 then
            AllDone.SetEvent;
        finally
          ExceptionLock.Leave;
        end;
      end
    );
    ProcessThread.FreeOnTerminate := False;

    // Thread that changes DebounceMs repeatedly
    SetterThread := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        try
          for J := 0 to ITERATIONS - 1 do
          begin
            if StopFlag then
              Break;
            Debouncer.DebounceMs := (J mod 100) + 1;
          end;
        except
          on E: Exception do
          begin
            ExceptionLock.Enter;
            try
              Inc(ExceptionCount);
            finally
              ExceptionLock.Leave;
            end;
          end;
        end;

        ExceptionLock.Enter;
        try
          Inc(ThreadsFinished);
          if ThreadsFinished = 2 then
            AllDone.SetEvent;
        finally
          ExceptionLock.Leave;
        end;
      end
    );
    SetterThread.FreeOnTerminate := False;

    // Start both threads
    ProcessThread.Start;
    SetterThread.Start;

    // Wait for both threads to complete (max 10 seconds)
    if AllDone.WaitFor(10000) <> wrSignaled then
    begin
      StopFlag := True;
      Assert.Fail('Threads did not complete in time - possible deadlock');
    end;

    ProcessThread.WaitFor;
    SetterThread.WaitFor;
    ProcessThread.Free;
    SetterThread.Free;

    Assert.AreEqual(0, ExceptionCount, 'Exceptions occurred during concurrent DebounceMs change and ShouldProcess');
  finally
    Debouncer.Free;
    ExceptionLock.Free;
    AllDone.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ZeroDebounceMs_AlwaysAllows;
var
  Debouncer: TDeviceEventDebouncer;
  I: Integer;
  AllPassed: Boolean;
begin
  Debouncer := TDeviceEventDebouncer.Create(0);
  try
    AllPassed := True;
    // With DebounceMs = 0, every call should return True
    for I := 1 to 10 do
    begin
      if not Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected) then
      begin
        AllPassed := False;
        Break;
      end;
    end;

    Assert.IsTrue(AllPassed, 'With DebounceMs = 0, all events should pass through');
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_NegativeDebounceMs_TreatedAsZero;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(-500);
  try
    // Negative value should be clamped to 0
    Assert.AreEqual(0, Debouncer.DebounceMs, 'Negative debounce should be clamped to 0');

    // With effective 0 debounce, all events should pass
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
      'With clamped 0 debounce, duplicate should be allowed');
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_MaxUInt64Address_Handles;
const
  MAX_UINT64: UInt64 = $FFFFFFFFFFFFFFFF;
begin
  // Test with maximum possible UInt64 address
  Assert.IsTrue(FDebouncer.ShouldProcess(MAX_UINT64, detConnect, csConnected),
    'Max UInt64 address should be handled');

  Assert.IsFalse(FDebouncer.ShouldProcess(MAX_UINT64, detConnect, csConnected),
    'Duplicate max UInt64 address event should be filtered');

  // Different event type should still be allowed
  Assert.IsTrue(FDebouncer.ShouldProcess(MAX_UINT64, detDisconnect, csDisconnected),
    'Different event type for max address should be allowed');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_SameAddressDifferentStates_TrackedSeparately;
begin
  // Same address, same event type, different states should be tracked separately
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnecting));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csDisconnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csDisconnecting));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csUnknown));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csError));

  // Now duplicates should be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
    'Duplicate connected state should be filtered');
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnecting),
    'Duplicate connecting state should be filtered');
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csDisconnected),
    'Duplicate disconnected state should be filtered');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_RapidFire_DebounceWorks;
var
  I: Integer;
  ProcessedCount: Integer;
begin
  ProcessedCount := 0;

  // Rapidly fire 100 identical events
  for I := 1 to 100 do
  begin
    if FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected) then
      Inc(ProcessedCount);
  end;

  Assert.AreEqual(1, ProcessedCount, 'Only the first of 100 rapid-fire events should be processed');
end;

procedure TDeviceEventDebouncerTests.Clear_ThenShouldProcess_AllowsImmediately;
begin
  // First process some events
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_2, detDisconnect, csDisconnected));

  // Verify duplicates are blocked
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS_2, detDisconnect, csDisconnected));

  // Clear the debouncer
  FDebouncer.Clear;

  // After Clear, all addresses should be allowed again immediately
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
    'After Clear, address 1 should be allowed immediately');
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS_2, detDisconnect, csDisconnected),
    'After Clear, address 2 should be allowed immediately');
end;

procedure TDeviceEventDebouncerTests.ShouldProcess_ManyUniqueAddresses_MemoryHandled;
const
  ADDRESS_COUNT = 10000;
var
  I: Integer;
  Debouncer: TDeviceEventDebouncer;
  ProcessedCount: Integer;
begin
  Debouncer := TDeviceEventDebouncer.Create(100);
  try
    ProcessedCount := 0;

    // Add 10000 unique addresses
    for I := 1 to ADDRESS_COUNT do
    begin
      if Debouncer.ShouldProcess(UInt64(I), detConnect, csConnected) then
        Inc(ProcessedCount);
    end;

    // All unique addresses should have been processed
    Assert.AreEqual(ADDRESS_COUNT, ProcessedCount,
      Format('All %d unique addresses should be processed', [ADDRESS_COUNT]));

    // Verify the debouncer is still functional after handling many addresses
    Assert.IsFalse(Debouncer.ShouldProcess(1, detConnect, csConnected),
      'Debouncer should still filter duplicates after handling many addresses');

    // Clear should work without issues
    Debouncer.Clear;

    // After clear, should allow again
    Assert.IsTrue(Debouncer.ShouldProcess(1, detConnect, csConnected),
      'After clear, should allow previously blocked address');
  finally
    Debouncer.Free;
  end;
end;

procedure TDeviceEventDebouncerTests.SetDebounceMs_Boundary_VeryLargeValue;
var
  Debouncer: TDeviceEventDebouncer;
begin
  Debouncer := TDeviceEventDebouncer.Create(100);
  try
    // Set to MaxInt
    Debouncer.DebounceMs := MaxInt;
    Assert.AreEqual(MaxInt, Debouncer.DebounceMs, 'Should accept MaxInt value');

    // First event should still be processed
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
      'First event should be processed even with MaxInt debounce');

    // Duplicate should be filtered (since debounce is very large)
    Assert.IsFalse(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
      'Duplicate should be filtered with MaxInt debounce');

    // Different address should still work
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_2, detConnect, csConnected),
      'Different address should be allowed');

    // Verify Clear still works with large debounce value
    Debouncer.Clear;
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected),
      'After Clear, should allow event even with MaxInt debounce');
  finally
    Debouncer.Free;
  end;
end;

{ TDebouncerTimingTests }

procedure TDebouncerTimingTests.Setup;
begin
  // Use very short debounce for timing tests
  FDebouncer := TDeviceEventDebouncer.Create(10);
end;

procedure TDebouncerTimingTests.TearDown;
begin
  FDebouncer.Free;
end;

procedure TDebouncerTimingTests.ShouldProcess_WithVeryShortDebounce_AllowsQuickReprocess;
begin
  // First event
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS, detConnect, csConnected));

  // Immediate duplicate - should be filtered
  Assert.IsFalse(FDebouncer.ShouldProcess(TEST_ADDRESS, detConnect, csConnected));

  // Wait slightly longer than debounce period
  Sleep(15);

  // Should now be allowed
  Assert.IsTrue(FDebouncer.ShouldProcess(TEST_ADDRESS, detConnect, csConnected),
    'After debounce period, event should be processed again');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceEventDebouncerTests);
  TDUnitX.RegisterTestFixture(TDebouncerTimingTests);

end.
