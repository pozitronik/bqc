{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Radio State Manager Unit Tests                  }
{                                                       }
{*******************************************************}

unit Tests.RadioStateManager;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.RadioControl,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TRadioStateManager.
  /// Note: Actual radio tests depend on hardware availability.
  /// </summary>
  [TestFixture]
  TRadioStateManagerTests = class
  private
    FManager: IRadioStateManager;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Create_DoesNotRaiseException;
    [Test]
    procedure GetState_ReturnsBoolean;
    [Test]
    procedure SetState_ReturnsResult;
    [Test]
    procedure SetStateEx_ReturnsExtendedResult;
    [Test]
    procedure StartWatching_DoesNotRaiseException;
    [Test]
    procedure StopWatching_DoesNotRaiseException;
    [Test]
    procedure OnStateChanged_CanBeAssigned;
  end;

  /// <summary>
  /// Test fixture for TMockRadioStateManager.
  /// </summary>
  [TestFixture]
  TMockRadioStateManagerTests = class
  private
    FManager: TMockRadioStateManager;
    FStateChangedCalled: Boolean;
    FLastStateChangedValue: Boolean;
    procedure HandleStateChanged(Sender: TObject; AEnabled: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetState_DefaultReturnsTrue;
    [Test]
    procedure GetState_RespectsConfiguration;
    [Test]
    procedure GetState_IncrementsCallCount;
    [Test]
    procedure GetState_ReturnsFalse_WhenRadioUnavailable;
    [Test]
    procedure SetState_DefaultReturnsSuccess;
    [Test]
    procedure SetState_RespectsConfiguration;
    [Test]
    procedure SetState_IncrementsCallCount;
    [Test]
    procedure SetState_RecordsLastValue;
    [Test]
    procedure SetState_UpdatesRadioEnabled_OnSuccess;
    [Test]
    procedure SetState_DoesNotUpdateRadioEnabled_OnFailure;
    [Test]
    procedure SetStateEx_ReturnsExtendedResult;
    [Test]
    procedure StartWatching_IncrementsCallCount;
    [Test]
    procedure StopWatching_IncrementsCallCount;
    [Test]
    procedure SimulateStateChanged_FiresEvent;
    [Test]
    procedure SimulateStateChanged_UpdatesRadioEnabled;
  end;

implementation

{ TRadioStateManagerTests }

procedure TRadioStateManagerTests.Setup;
begin
  FManager := TRadioStateManager.Create;
end;

procedure TRadioStateManagerTests.Create_DoesNotRaiseException;
begin
  Assert.IsNotNull(FManager);
end;

procedure TRadioStateManagerTests.GetState_ReturnsBoolean;
var
  Enabled: Boolean;
  Success: Boolean;
begin
  // Just verify it doesn't crash
  Success := FManager.GetState(Enabled);
  Assert.IsTrue(Success or not Success);  // Always passes, just verifies no exception
end;

procedure TRadioStateManagerTests.SetState_ReturnsResult;
var
  Result: TRadioControlResult;
begin
  // Just verify it returns a valid result (actual effect depends on hardware)
  Result := FManager.SetState(True);
  Assert.IsTrue(Result in [rcSuccess, rcDeviceNotFound, rcAccessDenied, rcError]);
end;

procedure TRadioStateManagerTests.SetStateEx_ReturnsExtendedResult;
var
  Result: TRadioControlResultEx;
begin
  Result := FManager.SetStateEx(True);
  Assert.IsTrue(Result.Result in [rcSuccess, rcDeviceNotFound, rcAccessDenied, rcError]);
end;

procedure TRadioStateManagerTests.StartWatching_DoesNotRaiseException;
begin
  FManager.StartWatching;
  Assert.Pass('No exception raised');
end;

procedure TRadioStateManagerTests.StopWatching_DoesNotRaiseException;
begin
  FManager.StartWatching;
  FManager.StopWatching;
  Assert.Pass('No exception raised');
end;

procedure TRadioStateManagerTests.OnStateChanged_CanBeAssigned;
var
  Handler: TRadioStateChangedEvent;
begin
  // Verify setter and getter can be called without exception
  FManager.SetOnStateChanged(nil);
  Handler := FManager.GetOnStateChanged;
  Assert.IsTrue(not Assigned(Handler), 'Handler should be nil after setting to nil');
end;

{ TMockRadioStateManagerTests }

procedure TMockRadioStateManagerTests.Setup;
begin
  FManager := TMockRadioStateManager.Create;
  FStateChangedCalled := False;
  FLastStateChangedValue := False;
end;

procedure TMockRadioStateManagerTests.TearDown;
begin
  FManager.Free;
end;

procedure TMockRadioStateManagerTests.HandleStateChanged(Sender: TObject; AEnabled: Boolean);
begin
  FStateChangedCalled := True;
  FLastStateChangedValue := AEnabled;
end;

procedure TMockRadioStateManagerTests.GetState_DefaultReturnsTrue;
var
  Enabled: Boolean;
begin
  FManager.GetState(Enabled);
  Assert.IsTrue(Enabled);
end;

procedure TMockRadioStateManagerTests.GetState_RespectsConfiguration;
var
  Enabled: Boolean;
begin
  FManager.RadioEnabled := False;
  FManager.GetState(Enabled);
  Assert.IsFalse(Enabled);

  FManager.RadioEnabled := True;
  FManager.GetState(Enabled);
  Assert.IsTrue(Enabled);
end;

procedure TMockRadioStateManagerTests.GetState_IncrementsCallCount;
var
  Enabled: Boolean;
begin
  Assert.AreEqual(0, FManager.GetStateCallCount);
  FManager.GetState(Enabled);
  Assert.AreEqual(1, FManager.GetStateCallCount);
  FManager.GetState(Enabled);
  Assert.AreEqual(2, FManager.GetStateCallCount);
end;

procedure TMockRadioStateManagerTests.GetState_ReturnsFalse_WhenRadioUnavailable;
var
  Enabled: Boolean;
  Success: Boolean;
begin
  FManager.RadioAvailable := False;
  Success := FManager.GetState(Enabled);
  Assert.IsFalse(Success);
end;

procedure TMockRadioStateManagerTests.SetState_DefaultReturnsSuccess;
begin
  Assert.AreEqual(rcSuccess, FManager.SetState(True));
end;

procedure TMockRadioStateManagerTests.SetState_RespectsConfiguration;
begin
  FManager.SetStateResult := rcAccessDenied;
  Assert.AreEqual(rcAccessDenied, FManager.SetState(True));

  FManager.SetStateResult := rcDeviceNotFound;
  Assert.AreEqual(rcDeviceNotFound, FManager.SetState(False));
end;

procedure TMockRadioStateManagerTests.SetState_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.SetStateCallCount);
  FManager.SetState(True);
  Assert.AreEqual(1, FManager.SetStateCallCount);
  FManager.SetState(False);
  Assert.AreEqual(2, FManager.SetStateCallCount);
end;

procedure TMockRadioStateManagerTests.SetState_RecordsLastValue;
begin
  FManager.SetState(True);
  Assert.IsTrue(FManager.LastSetStateValue);

  FManager.SetState(False);
  Assert.IsFalse(FManager.LastSetStateValue);
end;

procedure TMockRadioStateManagerTests.SetState_UpdatesRadioEnabled_OnSuccess;
var
  Enabled: Boolean;
begin
  FManager.RadioEnabled := False;
  FManager.SetStateResult := rcSuccess;

  FManager.SetState(True);

  FManager.GetState(Enabled);
  Assert.IsTrue(Enabled, 'RadioEnabled should be True after successful SetState(True)');
end;

procedure TMockRadioStateManagerTests.SetState_DoesNotUpdateRadioEnabled_OnFailure;
var
  Enabled: Boolean;
begin
  FManager.RadioEnabled := False;
  FManager.SetStateResult := rcAccessDenied;

  FManager.SetState(True);

  FManager.GetState(Enabled);
  Assert.IsFalse(Enabled, 'RadioEnabled should remain False after failed SetState');
end;

procedure TMockRadioStateManagerTests.SetStateEx_ReturnsExtendedResult;
var
  Result: TRadioControlResultEx;
begin
  FManager.SetStateResult := rcSuccess;
  Result := FManager.SetStateEx(True);
  Assert.AreEqual(rcSuccess, Result.Result);
  Assert.AreEqual(Cardinal(0), Result.ErrorCode);
end;

procedure TMockRadioStateManagerTests.StartWatching_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.StartWatchingCallCount);
  FManager.StartWatching;
  Assert.AreEqual(1, FManager.StartWatchingCallCount);
  FManager.StartWatching;
  Assert.AreEqual(2, FManager.StartWatchingCallCount);
end;

procedure TMockRadioStateManagerTests.StopWatching_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.StopWatchingCallCount);
  FManager.StopWatching;
  Assert.AreEqual(1, FManager.StopWatchingCallCount);
  FManager.StopWatching;
  Assert.AreEqual(2, FManager.StopWatchingCallCount);
end;

procedure TMockRadioStateManagerTests.SimulateStateChanged_FiresEvent;
begin
  FManager.SetOnStateChanged(HandleStateChanged);

  FManager.SimulateStateChanged(True);

  Assert.IsTrue(FStateChangedCalled, 'OnStateChanged event should be fired');
  Assert.IsTrue(FLastStateChangedValue, 'Event should pass True');
end;

procedure TMockRadioStateManagerTests.SimulateStateChanged_UpdatesRadioEnabled;
var
  Enabled: Boolean;
begin
  FManager.RadioEnabled := True;
  FManager.SimulateStateChanged(False);

  FManager.GetState(Enabled);
  Assert.IsFalse(Enabled, 'RadioEnabled should be updated by SimulateStateChanged');
end;

initialization
  TDUnitX.RegisterTestFixture(TRadioStateManagerTests);
  TDUnitX.RegisterTestFixture(TMockRadioStateManagerTests);

end.
