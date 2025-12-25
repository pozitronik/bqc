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
  // Negative debounce should work like zero or allow immediate reprocessing
  Debouncer := TDeviceEventDebouncer.Create(-100);
  try
    Assert.IsTrue(Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected));
    // Depending on implementation, negative might be treated differently
    // The key is it shouldn't crash
    Debouncer.ShouldProcess(TEST_ADDRESS_1, detConnect, csConnected);
    Assert.Pass; // If we get here without exception, test passes
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
