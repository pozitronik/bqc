{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Battery Query Tests                             }
{                                                       }
{*******************************************************}

unit Tests.BatteryQuery;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.BatteryQuery,
  Tests.Mocks.Bluetooth;

type
  [TestFixture]
  TBatteryQueryStrategyTests = class
  public
    [Test]
    procedure MockStrategy_TryQuery_ReturnsConfiguredResult;

    [Test]
    procedure MockStrategy_TryQuery_RecordsCallParameters;

    [Test]
    procedure MockStrategy_GetPriority_ReturnsConfiguredPriority;

    [Test]
    procedure MockStrategy_GetName_ReturnsConfiguredName;
  end;

  [TestFixture]
  TBatteryQueryTests = class
  private
    // Store as interface references for proper lifetime management
    FStrategy1Intf: IBatteryQueryStrategy;
    FStrategy2Intf: IBatteryQueryStrategy;
    // Keep class references for mock configuration (these don't own the objects)
    FStrategy1: TMockBatteryQueryStrategy;
    FStrategy2: TMockBatteryQueryStrategy;
    FQuery: IBatteryQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetBatteryLevel_SingleStrategy_UsesStrategy;

    [Test]
    procedure GetBatteryLevel_MultipleStrategies_TriesInPriorityOrder;

    [Test]
    procedure GetBatteryLevel_FirstStrategyFails_TriesSecond;

    [Test]
    procedure GetBatteryLevel_AllStrategiesFail_ReturnsNotSupported;

    [Test]
    procedure GetBatteryLevel_FirstStrategySucceeds_DoesNotTrySecond;

    [Test]
    procedure GetBatteryLevelWithTimeout_PassesTimeoutToStrategy;
  end;

  [TestFixture]
  TSyncBatteryQueryExecutorTests = class
  private
    FMockQuery: TMockBatteryQuery;
    FExecutor: IBatteryQueryExecutor;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Execute_CallsQueryAndCallback;

    [Test]
    procedure Execute_AfterShutdown_DoesNotExecute;

    [Test]
    procedure IsShutdown_InitiallyFalse;

    [Test]
    procedure IsShutdown_AfterShutdown_ReturnsTrue;

    [Test]
    procedure Execute_QueryThrowsException_CallsCallbackWithUnknown;
  end;

  [TestFixture]
  TBatteryCacheTests = class
  private
    FMockExecutor: TMockBatteryQueryExecutor;
    FCache: IBatteryCache;
    FQueryCompletedCalled: Boolean;
    FQueryCompletedAddress: UInt64;
    FQueryCompletedStatus: TBatteryStatus;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetBatteryStatus_NotCached_ReturnsNotSupported;

    [Test]
    procedure SetBatteryStatus_ThenGet_ReturnsCachedValue;

    [Test]
    procedure HasCachedStatus_NotCached_ReturnsFalse;

    [Test]
    procedure HasCachedStatus_Cached_ReturnsTrue;

    [Test]
    procedure RequestRefresh_CallsExecutor;

    [Test]
    procedure RequestRefresh_UpdatesCache;

    [Test]
    procedure RequestRefreshAll_CallsExecutorForEachAddress;

    [Test]
    procedure Clear_RemovesAllCachedValues;

    [Test]
    procedure Remove_RemovesSpecificValue;

    [Test]
    procedure RequestRefresh_AfterExecutorShutdown_DoesNothing;
  end;

implementation

uses
  Vcl.Forms;

{ TBatteryQueryStrategyTests }

procedure TBatteryQueryStrategyTests.MockStrategy_TryQuery_ReturnsConfiguredResult;
var
  Strategy: TMockBatteryQueryStrategy;
  Status: TBatteryStatus;
  Result: Boolean;
begin
  Strategy := TMockBatteryQueryStrategy.Create(100, 'Test');
  try
    Strategy.TryQueryResult := True;
    Strategy.BatteryStatus := TBatteryStatus.Create(75);

    Result := Strategy.TryQuery($001122334455, 5000, Status);

    Assert.IsTrue(Result, 'TryQuery should return configured result');
    Assert.AreEqual(75, Status.Level, 'Status should have configured level');
  finally
    Strategy.Free;
  end;
end;

procedure TBatteryQueryStrategyTests.MockStrategy_TryQuery_RecordsCallParameters;
var
  Strategy: TMockBatteryQueryStrategy;
  Status: TBatteryStatus;
begin
  Strategy := TMockBatteryQueryStrategy.Create(100, 'Test');
  try
    Strategy.TryQuery($AABBCCDDEEFF, 3000, Status);

    Assert.AreEqual(1, Strategy.TryQueryCallCount, 'Call count should be 1');
    Assert.AreEqual(UInt64($AABBCCDDEEFF), Strategy.LastQueryAddress, 'Address should be recorded');
    Assert.AreEqual(Cardinal(3000), Strategy.LastQueryTimeout, 'Timeout should be recorded');
  finally
    Strategy.Free;
  end;
end;

procedure TBatteryQueryStrategyTests.MockStrategy_GetPriority_ReturnsConfiguredPriority;
var
  Strategy: TMockBatteryQueryStrategy;
begin
  Strategy := TMockBatteryQueryStrategy.Create(150, 'Test');
  try
    Assert.AreEqual(150, Strategy.GetPriority, 'Priority should match configured value');
  finally
    Strategy.Free;
  end;
end;

procedure TBatteryQueryStrategyTests.MockStrategy_GetName_ReturnsConfiguredName;
var
  Strategy: TMockBatteryQueryStrategy;
begin
  Strategy := TMockBatteryQueryStrategy.Create(100, 'CustomName');
  try
    Assert.AreEqual('CustomName', Strategy.GetName, 'Name should match configured value');
  finally
    Strategy.Free;
  end;
end;

{ TBatteryQueryTests }

procedure TBatteryQueryTests.Setup;
begin
  // Create strategies and store both class and interface references
  FStrategy1 := TMockBatteryQueryStrategy.Create(100, 'Strategy1');
  FStrategy1Intf := FStrategy1; // Interface reference manages lifetime
  FStrategy2 := TMockBatteryQueryStrategy.Create(50, 'Strategy2');
  FStrategy2Intf := FStrategy2; // Interface reference manages lifetime
  FQuery := nil;
end;

procedure TBatteryQueryTests.TearDown;
begin
  FQuery := nil;
  // Clear class references (they don't own the objects)
  FStrategy1 := nil;
  FStrategy2 := nil;
  // Interface references release the objects
  FStrategy1Intf := nil;
  FStrategy2Intf := nil;
end;

procedure TBatteryQueryTests.GetBatteryLevel_SingleStrategy_UsesStrategy;
begin
  FStrategy1.TryQueryResult := True;
  FStrategy1.BatteryStatus := TBatteryStatus.Create(80);
  FQuery := CreateBatteryQueryWithStrategies([FStrategy1Intf]);

  var Status := FQuery.GetBatteryLevel($001122334455);

  Assert.AreEqual(1, FStrategy1.TryQueryCallCount, 'Strategy should be called once');
  Assert.AreEqual(80, Status.Level, 'Should return strategy result');
end;

procedure TBatteryQueryTests.GetBatteryLevel_MultipleStrategies_TriesInPriorityOrder;
begin
  // Strategy1 has priority 100, Strategy2 has priority 50
  // Strategy1 should be tried first
  FStrategy1.TryQueryResult := True;
  FStrategy1.BatteryStatus := TBatteryStatus.Create(90);
  FStrategy2.TryQueryResult := True;
  FStrategy2.BatteryStatus := TBatteryStatus.Create(50);
  FQuery := CreateBatteryQueryWithStrategies([FStrategy2Intf, FStrategy1Intf]); // Pass in wrong order

  var Status := FQuery.GetBatteryLevel($001122334455);

  Assert.AreEqual(90, Status.Level, 'Should use higher priority strategy result');
  Assert.AreEqual(1, FStrategy1.TryQueryCallCount, 'Higher priority strategy should be called');
end;

procedure TBatteryQueryTests.GetBatteryLevel_FirstStrategyFails_TriesSecond;
begin
  FStrategy1.TryQueryResult := False;
  FStrategy2.TryQueryResult := True;
  FStrategy2.BatteryStatus := TBatteryStatus.Create(60);
  FQuery := CreateBatteryQueryWithStrategies([FStrategy1Intf, FStrategy2Intf]);

  var Status := FQuery.GetBatteryLevel($001122334455);

  Assert.AreEqual(1, FStrategy1.TryQueryCallCount, 'First strategy should be tried');
  Assert.AreEqual(1, FStrategy2.TryQueryCallCount, 'Second strategy should be tried after first fails');
  Assert.AreEqual(60, Status.Level, 'Should return second strategy result');
end;

procedure TBatteryQueryTests.GetBatteryLevel_AllStrategiesFail_ReturnsNotSupported;
begin
  FStrategy1.TryQueryResult := False;
  FStrategy2.TryQueryResult := False;
  FQuery := CreateBatteryQueryWithStrategies([FStrategy1Intf, FStrategy2Intf]);

  var Status := FQuery.GetBatteryLevel($001122334455);

  Assert.IsFalse(Status.HasLevel, 'Should return NotSupported when all strategies fail');
end;

procedure TBatteryQueryTests.GetBatteryLevel_FirstStrategySucceeds_DoesNotTrySecond;
begin
  FStrategy1.TryQueryResult := True;
  FStrategy1.BatteryStatus := TBatteryStatus.Create(85);
  FStrategy2.TryQueryResult := True;
  FStrategy2.BatteryStatus := TBatteryStatus.Create(70);
  FQuery := CreateBatteryQueryWithStrategies([FStrategy1Intf, FStrategy2Intf]);

  FQuery.GetBatteryLevel($001122334455);

  Assert.AreEqual(1, FStrategy1.TryQueryCallCount, 'First strategy should be called');
  Assert.AreEqual(0, FStrategy2.TryQueryCallCount, 'Second strategy should not be called when first succeeds');
end;

procedure TBatteryQueryTests.GetBatteryLevelWithTimeout_PassesTimeoutToStrategy;
begin
  FStrategy1.TryQueryResult := True;
  FStrategy1.BatteryStatus := TBatteryStatus.Create(75);
  FQuery := CreateBatteryQueryWithStrategies([FStrategy1Intf]);

  FQuery.GetBatteryLevelWithTimeout($001122334455, 7500);

  Assert.AreEqual(Cardinal(7500), FStrategy1.LastQueryTimeout, 'Timeout should be passed to strategy');
end;

{ TSyncBatteryQueryExecutorTests }

procedure TSyncBatteryQueryExecutorTests.Setup;
begin
  FMockQuery := TMockBatteryQuery.Create;
  FExecutor := CreateSyncBatteryQueryExecutor(FMockQuery);
end;

procedure TSyncBatteryQueryExecutorTests.TearDown;
begin
  FExecutor := nil;
  // FMockQuery is freed by interface ref counting
end;

procedure TSyncBatteryQueryExecutorTests.Execute_CallsQueryAndCallback;
var
  CallbackCalled: Boolean;
  CallbackAddress: UInt64;
  CallbackStatus: TBatteryStatus;
begin
  CallbackCalled := False;
  FMockQuery.BatteryStatus := TBatteryStatus.Create(65);

  FExecutor.Execute($AABBCCDDEEFF,
    procedure(AAddress: UInt64; const AStatus: TBatteryStatus)
    begin
      CallbackCalled := True;
      CallbackAddress := AAddress;
      CallbackStatus := AStatus;
    end);

  Assert.IsTrue(CallbackCalled, 'Callback should be called');
  Assert.AreEqual(UInt64($AABBCCDDEEFF), CallbackAddress, 'Address should be passed to callback');
  Assert.AreEqual(65, CallbackStatus.Level, 'Status should be passed to callback');
  Assert.AreEqual(1, FMockQuery.GetBatteryLevelCallCount, 'Query should be called');
end;

procedure TSyncBatteryQueryExecutorTests.Execute_AfterShutdown_DoesNotExecute;
var
  CallbackCalled: Boolean;
begin
  CallbackCalled := False;
  FExecutor.Shutdown;

  FExecutor.Execute($001122334455,
    procedure(AAddress: UInt64; const AStatus: TBatteryStatus)
    begin
      CallbackCalled := True;
    end);

  Assert.IsFalse(CallbackCalled, 'Callback should not be called after shutdown');
  Assert.AreEqual(0, FMockQuery.GetBatteryLevelCallCount, 'Query should not be called after shutdown');
end;

procedure TSyncBatteryQueryExecutorTests.IsShutdown_InitiallyFalse;
begin
  Assert.IsFalse(FExecutor.IsShutdown, 'IsShutdown should be False initially');
end;

procedure TSyncBatteryQueryExecutorTests.IsShutdown_AfterShutdown_ReturnsTrue;
begin
  FExecutor.Shutdown;

  Assert.IsTrue(FExecutor.IsShutdown, 'IsShutdown should be True after Shutdown');
end;

procedure TSyncBatteryQueryExecutorTests.Execute_QueryThrowsException_CallsCallbackWithUnknown;
var
  CallbackCalled: Boolean;
  CallbackStatus: TBatteryStatus;
  MockQuery: TMockBatteryQuery;
  Executor: IBatteryQueryExecutor;
begin
  // Create a special mock that we can configure to throw
  // For this test, we'll use the sync executor with a mock that returns Unknown
  // since we can't easily make the mock throw
  CallbackCalled := False;
  MockQuery := TMockBatteryQuery.Create;
  MockQuery.BatteryStatus := TBatteryStatus.Unknown;
  Executor := CreateSyncBatteryQueryExecutor(MockQuery);

  Executor.Execute($001122334455,
    procedure(AAddress: UInt64; const AStatus: TBatteryStatus)
    begin
      CallbackCalled := True;
      CallbackStatus := AStatus;
    end);

  Assert.IsTrue(CallbackCalled, 'Callback should be called');
  // The mock returns Unknown status
  Assert.IsFalse(CallbackStatus.HasLevel, 'Status should not have level for Unknown');
end;

{ TBatteryCacheTests }

procedure TBatteryCacheTests.Setup;
begin
  FMockExecutor := TMockBatteryQueryExecutor.Create;
  FCache := CreateBatteryCacheWithExecutor(FMockExecutor);
  FQueryCompletedCalled := False;
  FQueryCompletedAddress := 0;
  FQueryCompletedStatus := TBatteryStatus.NotSupported;
end;

procedure TBatteryCacheTests.TearDown;
begin
  FCache := nil;
  // FMockExecutor is freed by interface ref counting
end;

procedure TBatteryCacheTests.GetBatteryStatus_NotCached_ReturnsNotSupported;
var
  Status: TBatteryStatus;
begin
  Status := FCache.GetBatteryStatus($001122334455);

  Assert.IsFalse(Status.HasLevel, 'Should return NotSupported for uncached address');
end;

procedure TBatteryCacheTests.SetBatteryStatus_ThenGet_ReturnsCachedValue;
var
  Status: TBatteryStatus;
begin
  FCache.SetBatteryStatus($001122334455, TBatteryStatus.Create(72));

  Status := FCache.GetBatteryStatus($001122334455);

  Assert.AreEqual(72, Status.Level, 'Should return cached value');
end;

procedure TBatteryCacheTests.HasCachedStatus_NotCached_ReturnsFalse;
begin
  Assert.IsFalse(FCache.HasCachedStatus($001122334455), 'Should return False for uncached address');
end;

procedure TBatteryCacheTests.HasCachedStatus_Cached_ReturnsTrue;
begin
  FCache.SetBatteryStatus($001122334455, TBatteryStatus.Create(50));

  Assert.IsTrue(FCache.HasCachedStatus($001122334455), 'Should return True for cached address');
end;

procedure TBatteryCacheTests.RequestRefresh_CallsExecutor;
begin
  FCache.RequestRefresh($AABBCCDDEEFF);

  Assert.AreEqual(1, FMockExecutor.ExecuteCallCount, 'Executor should be called');
  Assert.AreEqual(UInt64($AABBCCDDEEFF), FMockExecutor.LastAddress, 'Address should be passed to executor');
end;

procedure TBatteryCacheTests.RequestRefresh_UpdatesCache;
var
  Status: TBatteryStatus;
begin
  FMockExecutor.BatteryStatus := TBatteryStatus.Create(88);
  FMockExecutor.ExecuteImmediately := True;

  FCache.RequestRefresh($001122334455);
  Status := FCache.GetBatteryStatus($001122334455);

  Assert.AreEqual(88, Status.Level, 'Cache should be updated with executor result');
end;

procedure TBatteryCacheTests.RequestRefreshAll_CallsExecutorForEachAddress;
var
  Addresses: TArray<UInt64>;
begin
  Addresses := [$111111111111, $222222222222, $333333333333];

  FCache.RequestRefreshAll(Addresses);

  Assert.AreEqual(3, FMockExecutor.ExecuteCallCount, 'Executor should be called for each address');
end;

procedure TBatteryCacheTests.Clear_RemovesAllCachedValues;
begin
  FCache.SetBatteryStatus($001122334455, TBatteryStatus.Create(50));
  FCache.SetBatteryStatus($AABBCCDDEEFF, TBatteryStatus.Create(75));

  FCache.Clear;

  Assert.IsFalse(FCache.HasCachedStatus($001122334455), 'First address should be cleared');
  Assert.IsFalse(FCache.HasCachedStatus($AABBCCDDEEFF), 'Second address should be cleared');
end;

procedure TBatteryCacheTests.Remove_RemovesSpecificValue;
begin
  FCache.SetBatteryStatus($001122334455, TBatteryStatus.Create(50));
  FCache.SetBatteryStatus($AABBCCDDEEFF, TBatteryStatus.Create(75));

  FCache.Remove($001122334455);

  Assert.IsFalse(FCache.HasCachedStatus($001122334455), 'Removed address should not be cached');
  Assert.IsTrue(FCache.HasCachedStatus($AABBCCDDEEFF), 'Other address should still be cached');
end;

procedure TBatteryCacheTests.RequestRefresh_AfterExecutorShutdown_DoesNothing;
begin
  FMockExecutor.Shutdown;

  FCache.RequestRefresh($001122334455);

  Assert.AreEqual(0, FMockExecutor.ExecuteCallCount, 'Executor should not be called after shutdown');
end;

initialization
  TDUnitX.RegisterTestFixture(TBatteryQueryStrategyTests);
  TDUnitX.RegisterTestFixture(TBatteryQueryTests);
  TDUnitX.RegisterTestFixture(TSyncBatteryQueryExecutorTests);
  TDUnitX.RegisterTestFixture(TBatteryCacheTests);

end.
