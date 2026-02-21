{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Device Watcher Tests                            }
{                                                       }
{       GOAL: Reveal bugs in event processing logic.   }
{                                                       }
{       CRITICAL TESTABILITY ISSUES:                    }
{       1. ProcessHciEvent, ProcessL2CapEvent private   }
{       2. No dependency injection for Windows APIs     }
{       3. Tight coupling to Windows message pump       }
{                                                       }
{       APPROACH: Use test-friendly subclass to expose  }
{       private methods for testing. After bugs proven, }
{       refactor to use interface injection.            }
{                                                       }
{*******************************************************}

unit Tests.DeviceWatcher;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.DeviceWatcher,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Mock implementation of IDeviceStateQuery for testing.
  /// Allows tests to control what connection state is returned.
  /// </summary>
  TMockDeviceStateQuery = class(TInterfacedObject, IDeviceStateQuery)
  private
    FConnectionState: TBluetoothConnectionState;
    FQueryCount: Integer;
  public
    constructor Create(AInitialState: TBluetoothConnectionState);
    function QueryConnectionState(AAddress: UInt64): TBluetoothConnectionState;

    /// <summary>
    /// Sets the state that will be returned by next QueryConnectionState call.
    /// </summary>
    procedure SetState(AState: TBluetoothConnectionState);

    /// <summary>
    /// Returns the number of times QueryConnectionState was called.
    /// </summary>
    function GetQueryCount: Integer;
  end;

  /// <summary>
  /// Test-friendly subclass that exposes protected methods for testing.
  /// </summary>
  TTestableDeviceWatcher = class(TBluetoothDeviceWatcher)
  public
    procedure TestProcessHciEvent(AEventInfo: Pointer);
    procedure TestProcessL2CapEvent(AEventInfo: Pointer);
    procedure TestProcessRadioInRange(AEventInfo: Pointer);
    procedure TestProcessRadioOutOfRange(AAddress: Pointer);
  end;

  /// <summary>
  /// Test helper to capture events for verification.
  /// </summary>
  TDeviceWatcherEventCapture = class
  public
    ConnectedEventFired: Boolean;
    DisconnectedEventFired: Boolean;
    AttributeChangedEventFired: Boolean;
    DiscoveredEventFired: Boolean;
    OutOfRangeEventFired: Boolean;
    ErrorEventFired: Boolean;

    ConnectedAddress: UInt64;
    DisconnectedAddress: UInt64;
    ErrorMessage: string;
    ErrorCode: DWORD;

    procedure Reset;
    procedure OnDeviceConnected(Sender: TObject; const AAddress: UInt64; AConnected: Boolean);
    procedure OnDeviceDisconnected(Sender: TObject; const AAddress: UInt64; AConnected: Boolean);
    procedure OnDeviceAttributeChanged(Sender: TObject; const ADeviceInfo: TBluetoothDeviceInfo);
    procedure OnDeviceDiscovered(Sender: TObject; const ADeviceInfo: TBluetoothDeviceInfo);
    procedure OnDeviceOutOfRange(Sender: TObject; const ADeviceAddress: UInt64);
    procedure OnWatcherError(Sender: TObject; const AMessage: string; AErrorCode: DWORD);
  end;

  /// <summary>
  /// Test fixture for basic lifecycle and initialization.
  /// </summary>
  [TestFixture]
  TDeviceWatcherLifecycleTests = class
  private
    FWatcher: TBluetoothDeviceWatcher;
    FEventCapture: TDeviceWatcherEventCapture;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Create_StartsInStoppedState;

    [Test]
    procedure Create_EventPropertiesAcceptHandlers;
  end;

  /// <summary>
  /// Runtime tests for SCO Event Handling Bug.
  /// CRITICAL BUG: csUnknown treated as "not connected" - fires incorrect events.
  /// </summary>
  [TestFixture]
  TScoEventHandlingRuntimeTests = class
  private
    FWatcher: TTestableDeviceWatcher;
    FMockStateQuery: TMockDeviceStateQuery;
    FEventCapture: TDeviceWatcherEventCapture;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure ProcessHciEvent_ScoConnect_QueryReturnsUnknown_ProveBug1;

    [Test]
    procedure ProcessHciEvent_ScoDisconnect_QueryReturnsUnknown_ProveBug1;

    [Test]
    procedure ProcessHciEvent_AclConnect_FiresEvent;

    [Test]
    procedure ProcessHciEvent_AclDisconnect_FiresEvent;
  end;

implementation

{ TMockDeviceStateQuery }

constructor TMockDeviceStateQuery.Create(AInitialState: TBluetoothConnectionState);
begin
  inherited Create;
  FConnectionState := AInitialState;
  FQueryCount := 0;
end;

function TMockDeviceStateQuery.QueryConnectionState(AAddress: UInt64): TBluetoothConnectionState;
begin
  Inc(FQueryCount);
  Result := FConnectionState;
end;

procedure TMockDeviceStateQuery.SetState(AState: TBluetoothConnectionState);
begin
  FConnectionState := AState;
end;

function TMockDeviceStateQuery.GetQueryCount: Integer;
begin
  Result := FQueryCount;
end;

{ TTestableDeviceWatcher }

procedure TTestableDeviceWatcher.TestProcessHciEvent(AEventInfo: Pointer);
begin
  // Call protected method from base class
  ProcessHciEvent(AEventInfo);
end;

procedure TTestableDeviceWatcher.TestProcessL2CapEvent(AEventInfo: Pointer);
begin
  ProcessL2CapEvent(AEventInfo);
end;

procedure TTestableDeviceWatcher.TestProcessRadioInRange(AEventInfo: Pointer);
begin
  ProcessRadioInRange(AEventInfo);
end;

procedure TTestableDeviceWatcher.TestProcessRadioOutOfRange(AAddress: Pointer);
begin
  ProcessRadioOutOfRange(AAddress);
end;

{ TDeviceWatcherEventCapture }

procedure TDeviceWatcherEventCapture.Reset;
begin
  ConnectedEventFired := False;
  DisconnectedEventFired := False;
  AttributeChangedEventFired := False;
  DiscoveredEventFired := False;
  OutOfRangeEventFired := False;
  ErrorEventFired := False;
  ConnectedAddress := 0;
  DisconnectedAddress := 0;
  ErrorMessage := '';
  ErrorCode := 0;
end;

procedure TDeviceWatcherEventCapture.OnDeviceConnected(Sender: TObject;
  const AAddress: UInt64; AConnected: Boolean);
begin
  ConnectedEventFired := True;
  ConnectedAddress := AAddress;
end;

procedure TDeviceWatcherEventCapture.OnDeviceDisconnected(Sender: TObject;
  const AAddress: UInt64; AConnected: Boolean);
begin
  DisconnectedEventFired := True;
  DisconnectedAddress := AAddress;
end;

procedure TDeviceWatcherEventCapture.OnDeviceAttributeChanged(Sender: TObject;
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  AttributeChangedEventFired := True;
end;

procedure TDeviceWatcherEventCapture.OnDeviceDiscovered(Sender: TObject;
  const ADeviceInfo: TBluetoothDeviceInfo);
begin
  DiscoveredEventFired := True;
end;

procedure TDeviceWatcherEventCapture.OnDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
begin
  OutOfRangeEventFired := True;
end;

procedure TDeviceWatcherEventCapture.OnWatcherError(Sender: TObject;
  const AMessage: string; AErrorCode: DWORD);
begin
  ErrorEventFired := True;
  ErrorMessage := AMessage;
  ErrorCode := AErrorCode;
end;

{ TDeviceWatcherLifecycleTests }

procedure TDeviceWatcherLifecycleTests.Setup;
begin
  FWatcher := TBluetoothDeviceWatcher.Create;
  FEventCapture := TDeviceWatcherEventCapture.Create;
end;

procedure TDeviceWatcherLifecycleTests.TearDown;
begin
  FWatcher.Free;
  FEventCapture.Free;
end;

procedure TDeviceWatcherLifecycleTests.Create_StartsInStoppedState;
begin
  Assert.IsFalse(FWatcher.IsRunning, 'Watcher should not be running after creation');
end;

procedure TDeviceWatcherLifecycleTests.Create_EventPropertiesAcceptHandlers;
begin
  // Verify that event properties can be assigned without errors
  FWatcher.OnDeviceConnected := FEventCapture.OnDeviceConnected;
  FWatcher.OnDeviceDisconnected := FEventCapture.OnDeviceDisconnected;
  FWatcher.OnDeviceAttributeChanged := FEventCapture.OnDeviceAttributeChanged;
  FWatcher.OnDeviceDiscovered := FEventCapture.OnDeviceDiscovered;
  FWatcher.OnDeviceOutOfRange := FEventCapture.OnDeviceOutOfRange;
  FWatcher.OnError := FEventCapture.OnWatcherError;

  Assert.Pass('All event properties accept handlers correctly');
end;

{ TScoEventHandlingRuntimeTests }

procedure TScoEventHandlingRuntimeTests.Setup;
begin
  // Create mock that returns csUnknown (simulating query failure)
  FMockStateQuery := TMockDeviceStateQuery.Create(csUnknown);

  // Create watcher with injected mock
  FWatcher := TTestableDeviceWatcher.Create(FMockStateQuery);

  // Wire up event capture
  FEventCapture := TDeviceWatcherEventCapture.Create;
  FWatcher.OnDeviceConnected := FEventCapture.OnDeviceConnected;
  FWatcher.OnDeviceDisconnected := FEventCapture.OnDeviceDisconnected;
end;

procedure TScoEventHandlingRuntimeTests.TearDown;
begin
  FWatcher.Free;
  FMockStateQuery := nil;  // Interface counting
  FEventCapture.Free;
end;

procedure TScoEventHandlingRuntimeTests.ProcessHciEvent_ScoConnect_QueryReturnsUnknown_ProveBug1;
var
  EventInfo: BTH_HCI_EVENT_INFO;
  TestAddress: UInt64;
begin
  // *** THIS TEST PROVES BUG #1 EXISTS ***
  //
  // SCENARIO: SCO connect event when QueryDeviceConnectionState returns csUnknown
  // BUGGY CODE (line 478):
  //   if ActualState <> csConnected then DoDeviceConnected(DeviceAddress);
  // Since csUnknown <> csConnected, event fires even though state is unknown!
  //
  // EXPECTED BEHAVIOR: Should NOT fire event when state is csUnknown
  // ACTUAL BEHAVIOR: WILL fire event (BUG!)

  TestAddress := $112233445566;

  FillChar(EventInfo, SizeOf(EventInfo), 0);
  EventInfo.bthAddress := TestAddress;
  EventInfo.connectionType := HCI_CONNECTION_TYPE_SCO;
  EventInfo.connected := 1;  // SCO connect event

  // Process event - will query mock which returns csUnknown
  FWatcher.TestProcessHciEvent(@EventInfo);

  // *** BUG PROOF: This assertion will FAIL, proving bug exists ***
  // The event should NOT fire when state is unknown, but it does!
  Assert.IsFalse(FEventCapture.ConnectedEventFired,
    'BUG: OnDeviceConnected fired when state query returned csUnknown! ' +
    'Expected: No event. Actual: Event fired. This is incorrect behavior.');
end;

procedure TScoEventHandlingRuntimeTests.ProcessHciEvent_ScoDisconnect_QueryReturnsUnknown_ProveBug1;
var
  EventInfo: BTH_HCI_EVENT_INFO;
  TestAddress: UInt64;
begin
  // *** THIS TEST PROVES BUG #1 EXISTS (disconnect variant) ***

  TestAddress := $112233445566;

  FillChar(EventInfo, SizeOf(EventInfo), 0);
  EventInfo.bthAddress := TestAddress;
  EventInfo.connectionType := HCI_CONNECTION_TYPE_SCO;
  EventInfo.connected := 0;  // SCO disconnect event

  FWatcher.TestProcessHciEvent(@EventInfo);

  // *** BUG PROOF: This assertion will FAIL, proving bug exists ***
  Assert.IsFalse(FEventCapture.DisconnectedEventFired,
    'BUG: OnDeviceDisconnected fired when state query returned csUnknown! ' +
    'Expected: No event. Actual: Event fired. This is incorrect behavior.');
end;

procedure TScoEventHandlingRuntimeTests.ProcessHciEvent_AclConnect_FiresEvent;
var
  EventInfo: BTH_HCI_EVENT_INFO;
  TestAddress: UInt64;
begin
  // ACL events should fire regardless (no SCO filtering logic)
  TestAddress := $112233445566;

  FillChar(EventInfo, SizeOf(EventInfo), 0);
  EventInfo.bthAddress := TestAddress;
  EventInfo.connectionType := HCI_CONNECTION_TYPE_ACL;
  EventInfo.connected := 1;

  FWatcher.TestProcessHciEvent(@EventInfo);

  Assert.IsTrue(FEventCapture.ConnectedEventFired, 'ACL connect should fire event');
end;

procedure TScoEventHandlingRuntimeTests.ProcessHciEvent_AclDisconnect_FiresEvent;
var
  EventInfo: BTH_HCI_EVENT_INFO;
  TestAddress: UInt64;
begin
  TestAddress := $112233445566;

  FillChar(EventInfo, SizeOf(EventInfo), 0);
  EventInfo.bthAddress := TestAddress;
  EventInfo.connectionType := HCI_CONNECTION_TYPE_ACL;
  EventInfo.connected := 0;

  FWatcher.TestProcessHciEvent(@EventInfo);

  Assert.IsTrue(FEventCapture.DisconnectedEventFired, 'ACL disconnect should fire event');
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceWatcherLifecycleTests);
  TDUnitX.RegisterTestFixture(TScoEventHandlingRuntimeTests);

end.
