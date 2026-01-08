{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Connection Verification Strategy Tests          }
{                                                       }
{       GOAL: Reveal bugs in verification logic.       }
{                                                       }
{       KEY FINDINGS:                                   }
{       1. GetTickCount overflow bug (untestable)       }
{       2. TProfileQuery rejects devices with 0 profiles}
{       3. Timing logic cannot be tested deterministically}
{                                                       }
{*******************************************************}

unit Tests.ConnectionVerification;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionVerification,
  Tests.Mocks.Bluetooth;

type
  /// <summary>
  /// Tests for TNullVerificationStrategy.
  /// </summary>
  [TestFixture]
  TNullVerificationStrategyTests = class
  public
    [Test]
    procedure VerifyConnection_AlwaysReturnsTrue;

    [Test]
    procedure GetStrategyName_ReturnsExpectedName;
  end;

  /// <summary>
  /// Tests for TBatteryQueryVerificationStrategy.
  /// LIMITATION: Cannot test timing logic without refactoring.
  /// </summary>
  [TestFixture]
  TBatteryQueryVerificationStrategyTests = class
  private
    FStrategy: IConnectionVerificationStrategy;
    FMockBatteryQuery: TMockBatteryQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure VerifyConnection_BatterySupported_ReturnsTrue;

    [Test]
    procedure VerifyConnection_BatteryNotSupported_ReturnsTrueBecauseFast;

    [Test]
    procedure GetStrategyName_ReturnsExpectedName;
  end;

  /// <summary>
  /// Tests for TProfileQueryVerificationStrategy.
  /// CRITICAL: Reveals bug - devices with zero profiles are rejected!
  /// </summary>
  [TestFixture]
  TProfileQueryVerificationStrategyTests = class
  private
    FStrategy: IConnectionVerificationStrategy;
    FMockProfileQuery: TMockProfileQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure VerifyConnection_WithProfiles_ReturnsTrue;

    [Test]
    procedure VerifyConnection_ZeroProfiles_ReturnsTrue;

    [Test]
    procedure GetStrategyName_ReturnsExpectedName;
  end;

  /// <summary>
  /// Tests for factory functions.
  /// </summary>
  [TestFixture]
  TConnectionVerificationFactoryTests = class
  public
    [Test]
    procedure CreateDefaultVerificationStrategy_ReturnsPingStrategy;

    [Test]
    procedure CreateVerificationStrategy_Ping_NoDependenciesNeeded;
  end;

implementation

uses
  Winapi.Windows;  // For Sleep

{ TNullVerificationStrategyTests }

procedure TNullVerificationStrategyTests.VerifyConnection_AlwaysReturnsTrue;
var
  Strategy: IConnectionVerificationStrategy;
begin
  Strategy := TNullVerificationStrategy.Create;

  // Should always return True regardless of address
  Assert.IsTrue(Strategy.VerifyConnection(0), 'Should return True for address 0');
  Assert.IsTrue(Strategy.VerifyConnection($112233445566), 'Should return True for valid address');
  Assert.IsTrue(Strategy.VerifyConnection(High(UInt64)), 'Should return True for max address');
end;

procedure TNullVerificationStrategyTests.GetStrategyName_ReturnsExpectedName;
var
  Strategy: IConnectionVerificationStrategy;
begin
  Strategy := TNullVerificationStrategy.Create;
  Assert.AreEqual('None (No Verification)', Strategy.GetStrategyName);
end;

{ TBatteryQueryVerificationStrategyTests }

procedure TBatteryQueryVerificationStrategyTests.Setup;
begin
  FMockBatteryQuery := TMockBatteryQuery.Create;
  FStrategy := TBatteryQueryVerificationStrategy.Create(FMockBatteryQuery);
end;

procedure TBatteryQueryVerificationStrategyTests.TearDown;
begin
  FStrategy := nil;
  FMockBatteryQuery := nil;  // Interface reference counting handles cleanup
end;

procedure TBatteryQueryVerificationStrategyTests.VerifyConnection_BatterySupported_ReturnsTrue;
var
  TestAddress: UInt64;
begin
  TestAddress := $112233445566;

  // Configure mock: Device has battery at 75%
  FMockBatteryQuery.BatteryStatus := TBatteryStatus.Create(75);

  // Verification should succeed because battery is supported
  Assert.IsTrue(FStrategy.VerifyConnection(TestAddress),
    'Should return True when battery is supported');

  // Verify mock was called
  Assert.AreEqual(1, FMockBatteryQuery.GetBatteryLevelCallCount,
    'GetBatteryLevel should be called once');
  Assert.AreEqual(TestAddress, FMockBatteryQuery.LastQueryAddress,
    'Should query correct device address');
end;

procedure TBatteryQueryVerificationStrategyTests.VerifyConnection_BatteryNotSupported_ReturnsTrueBecauseFast;
var
  TestAddress: UInt64;
begin
  TestAddress := $112233445566;

  // Configure mock: Device does not support battery reporting
  FMockBatteryQuery.BatteryStatus := TBatteryStatus.NotSupported;

  // CRITICAL DESIGN ISSUE REVEALED:
  // Line 222 of ConnectionVerification.pas:
  //   Result := BatteryStatus.IsSupported or (ElapsedMs < VERIFICATION_TIMEOUT_MS div 2);
  //
  // Because TMockBatteryQuery returns instantly (<1500ms), this returns TRUE
  // even though battery is NOT supported!
  //
  // The logic assumes: "fast response with NotSupported = device is online but has no battery"
  // But this could also be: "cached NotSupported response for offline device"
  //
  // TESTABILITY PROBLEM: Cannot test timing without Sleep (unreliable) or refactoring
  // to inject clock/timer.

  Assert.IsTrue(FStrategy.VerifyConnection(TestAddress),
    'Currently returns True - design assumes fast NotSupported = device is online');

  // UNTESTABLE SCENARIO:
  // - Device offline, query takes 1400ms (slow but < 1500ms threshold)
  // - Returns NotSupported
  // - Verification returns TRUE (false positive!)
  //
  // This bug is UNTESTABLE without refactoring GetTickCount to injectable clock.
end;

procedure TBatteryQueryVerificationStrategyTests.GetStrategyName_ReturnsExpectedName;
begin
  Assert.AreEqual('Battery Query', FStrategy.GetStrategyName);
end;

{ TProfileQueryVerificationStrategyTests }

procedure TProfileQueryVerificationStrategyTests.Setup;
begin
  FMockProfileQuery := TMockProfileQuery.Create;
  FStrategy := TProfileQueryVerificationStrategy.Create(FMockProfileQuery);
end;

procedure TProfileQueryVerificationStrategyTests.TearDown;
begin
  FStrategy := nil;
  FMockProfileQuery := nil;  // Interface reference counting handles cleanup
end;

procedure TProfileQueryVerificationStrategyTests.VerifyConnection_WithProfiles_ReturnsTrue;
var
  TestAddress: UInt64;
  ProfileInfo: TDeviceProfileInfo;
  DummyGuid: TGUID;
  Profiles: TBluetoothProfileArray;
begin
  TestAddress := $112233445566;

  // Configure mock: Device has 2 profiles
  ZeroMemory(@DummyGuid, SizeOf(TGUID));
  SetLength(Profiles, 2);
  Profiles[0] := TBluetoothProfile.Create(bptA2DPSink, DummyGuid, pcsConnected);
  Profiles[1] := TBluetoothProfile.Create(bptAVRCP, DummyGuid, pcsConnected);

  ProfileInfo := TDeviceProfileInfo.Create(TestAddress, Profiles, Now);

  FMockProfileQuery.SetProfileInfo(TestAddress, ProfileInfo);

  // Verification should succeed
  Assert.IsTrue(FStrategy.VerifyConnection(TestAddress),
    'Should return True when device has profiles');
end;

procedure TProfileQueryVerificationStrategyTests.VerifyConnection_ZeroProfiles_ReturnsTrue;
var
  TestAddress: UInt64;
  ProfileInfo: TDeviceProfileInfo;
begin
  TestAddress := $112233445566;

  // Configure mock: Device has ZERO profiles (but query succeeded quickly)
  ProfileInfo := TDeviceProfileInfo.Empty(TestAddress);

  FMockProfileQuery.SetProfileInfo(TestAddress, ProfileInfo);

  // *** BUG FIX VALIDATED ***
  // PREVIOUS BUG (line 265 - FIXED):
  //   Result := (ProfileInfo.Count > 0) and (ElapsedMs < VERIFICATION_TIMEOUT_MS);
  //   This REJECTED devices with zero profiles!
  //
  // CURRENT LOGIC (line 267 - CORRECT):
  //   Result := (ElapsedMs < VERIFICATION_TIMEOUT_MS);
  //   Accepts devices if query completed within timeout, regardless of profile count
  //
  // REAL-WORLD SCENARIO:
  // - Some Bluetooth devices don't expose profiles via enumeration API
  // - Devices may hide profiles due to permission issues
  // - Profile query succeeds quickly but returns Count=0
  // - Device is ONLINE and reachable → should be accepted
  //
  // FIXED BEHAVIOR: Returns True (device correctly verified as online)

  Assert.IsTrue(FStrategy.VerifyConnection(TestAddress),
    'Zero-profile devices are now correctly accepted when query completes within timeout');
end;

procedure TProfileQueryVerificationStrategyTests.GetStrategyName_ReturnsExpectedName;
begin
  Assert.AreEqual('Profile Query', FStrategy.GetStrategyName);
end;

{ TConnectionVerificationFactoryTests }

procedure TConnectionVerificationFactoryTests.CreateDefaultVerificationStrategy_ReturnsPingStrategy;
var
  Strategy: IConnectionVerificationStrategy;
begin
  Strategy := CreateDefaultVerificationStrategy;

  Assert.IsNotNull(Strategy, 'Should create strategy');
  Assert.AreEqual('Ping (Service Discovery)', Strategy.GetStrategyName,
    'Default strategy should be Ping (vsPing constant)');
end;

procedure TConnectionVerificationFactoryTests.CreateVerificationStrategy_Ping_NoDependenciesNeeded;
var
  Strategy: IConnectionVerificationStrategy;
begin
  // Ping strategy doesn't need battery or profile query dependencies
  Strategy := CreateVerificationStrategy(vsPing, nil, nil);

  Assert.IsNotNull(Strategy, 'Should create ping strategy without dependencies');
  Assert.AreEqual('Ping (Service Discovery)', Strategy.GetStrategyName);
end;

initialization
  TDUnitX.RegisterTestFixture(TNullVerificationStrategyTests);
  TDUnitX.RegisterTestFixture(TBatteryQueryVerificationStrategyTests);
  TDUnitX.RegisterTestFixture(TProfileQueryVerificationStrategyTests);
  TDUnitX.RegisterTestFixture(TConnectionVerificationFactoryTests);

end.
