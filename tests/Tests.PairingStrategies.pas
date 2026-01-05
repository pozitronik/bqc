{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Pairing Strategy Unit Tests                     }
{                                                       }
{*******************************************************}

unit Tests.PairingStrategies;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.PairingStrategies,
  App.ConfigEnums;

type
  /// <summary>
  /// Test fixture for TPairingResult record.
  /// </summary>
  [TestFixture]
  TPairingResultTests = class
  public
    // Factory method tests
    [Test]
    procedure Success_ReturnsSuccessStatus;
    [Test]
    procedure Success_HasNoError;
    [Test]
    procedure Success_IsSuccess;

    [Test]
    procedure Failed_ReturnsFailedStatus;
    [Test]
    procedure Failed_StoresErrorCode;
    [Test]
    procedure Failed_StoresErrorMessage;
    [Test]
    procedure Failed_IsNotSuccess;

    [Test]
    procedure Cancelled_ReturnsCancelledStatus;
    [Test]
    procedure Cancelled_IsNotSuccess;

    [Test]
    procedure AlreadyPaired_ReturnsAlreadyPairedStatus;
    [Test]
    procedure AlreadyPaired_IsSuccess;

    [Test]
    procedure Timeout_ReturnsTimeoutStatus;
    [Test]
    procedure Timeout_IsNotSuccess;

    [Test]
    procedure NotSupported_ReturnsNotSupportedStatus;
    [Test]
    procedure NotSupported_StoresReason;
    [Test]
    procedure NotSupported_IsNotSuccess;
  end;

  /// <summary>
  /// Test fixture for TWindowsPairingStrategy.
  /// </summary>
  [TestFixture]
  TWindowsPairingStrategyTests = class
  private
    FStrategy: IPairingStrategy;
  public
    [Setup]
    procedure Setup;

    // CanHandle tests
    [Test]
    procedure CanHandle_Classic_ReturnsTrue;
    [Test]
    procedure CanHandle_Auto_ReturnsTrue;
    [Test]
    procedure CanHandle_WinRT_ReturnsFalse;

    // GetPriority tests
    [Test]
    procedure GetPriority_Returns50;

    // GetName tests
    [Test]
    procedure GetName_ReturnsWindowsClassicPairing;
  end;

  /// <summary>
  /// Test fixture for TWinRTPairingStrategy.
  /// </summary>
  [TestFixture]
  TWinRTPairingStrategyTests = class
  private
    FStrategy: IPairingStrategy;
  public
    [Setup]
    procedure Setup;

    // CanHandle tests
    [Test]
    procedure CanHandle_WinRT_ReturnsTrue;
    [Test]
    procedure CanHandle_Classic_ReturnsFalse;
    [Test]
    procedure CanHandle_Auto_ReturnsTrue;

    // GetPriority tests
    [Test]
    procedure GetPriority_Returns100;

    // GetName tests
    [Test]
    procedure GetName_ReturnsWinRTSimplePairing;
  end;

  /// <summary>
  /// Test fixture for TPairingStrategyFactory.
  /// </summary>
  [TestFixture]
  TPairingStrategyFactoryTests = class
  private
    FFactory: IPairingStrategyFactory;
  public
    [Setup]
    procedure Setup;

    // Default strategies tests
    [Test]
    procedure Create_RegistersDefaultStrategies;

    // GetStrategy tests
    [Test]
    procedure GetStrategy_Classic_ReturnsWindowsStrategy;
    [Test]
    procedure GetStrategy_Auto_ReturnsWinRTStrategy;
    [Test]
    procedure GetStrategy_WinRT_ReturnsWinRTStrategy;

    // Priority selection tests
    [Test]
    procedure GetStrategy_SelectsHighestPriority;

    // RegisterStrategy tests
    [Test]
    procedure RegisterStrategy_AddsToList;

    // Clear tests
    [Test]
    procedure Clear_RemovesAllStrategies;
    [Test]
    procedure Clear_GetStrategyReturnsNil;
  end;

implementation

uses
  System.DateUtils,
  Winapi.Windows;

type
  /// <summary>
  /// Custom pairing strategy for testing priority selection.
  /// </summary>
  TCustomPairingStrategy = class(TInterfacedObject, IPairingStrategy)
  private
    FPriority: Integer;
  public
    constructor Create(APriority: Integer = 50);
    function Pair(const ADevice: TBluetoothDeviceInfo;
                  AProgressCallback: TPairingProgressCallback): TPairingResult;
    function Unpair(ADeviceAddress: UInt64): TPairingResult;
    function CanHandle(APlatform: TBluetoothPlatform): Boolean;
    function GetName: string;
    function GetPriority: Integer;
  end;

{ TPairingResultTests }

procedure TPairingResultTests.Success_ReturnsSuccessStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Success;
  Assert.AreEqual(Ord(prsSuccess), Ord(Result.Status));
end;

procedure TPairingResultTests.Success_HasNoError;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Success;
  Assert.AreEqual(Cardinal(0), Result.ErrorCode);
  Assert.AreEqual('Pairing completed successfully', Result.ErrorMessage);
end;

procedure TPairingResultTests.Success_IsSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Success;
  Assert.IsTrue(Result.IsSuccess);
end;

procedure TPairingResultTests.Failed_ReturnsFailedStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Failed(123, 'Test error');
  Assert.AreEqual(Ord(prsFailed), Ord(Result.Status));
end;

procedure TPairingResultTests.Failed_StoresErrorCode;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Failed(123, 'Test error');
  Assert.AreEqual(Cardinal(123), Result.ErrorCode);
end;

procedure TPairingResultTests.Failed_StoresErrorMessage;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Failed(123, 'Test error');
  Assert.AreEqual('Test error', Result.ErrorMessage);
end;

procedure TPairingResultTests.Failed_IsNotSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Failed(123, 'Test error');
  Assert.IsFalse(Result.IsSuccess);
end;

procedure TPairingResultTests.Cancelled_ReturnsCancelledStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Cancelled;
  Assert.AreEqual(Ord(prsCancelled), Ord(Result.Status));
end;

procedure TPairingResultTests.Cancelled_IsNotSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Cancelled;
  Assert.IsFalse(Result.IsSuccess);
end;

procedure TPairingResultTests.AlreadyPaired_ReturnsAlreadyPairedStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.AlreadyPaired;
  Assert.AreEqual(Ord(prsAlreadyPaired), Ord(Result.Status));
end;

procedure TPairingResultTests.AlreadyPaired_IsSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.AlreadyPaired;
  Assert.IsTrue(Result.IsSuccess);
end;

procedure TPairingResultTests.Timeout_ReturnsTimeoutStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Timeout;
  Assert.AreEqual(Ord(prsTimeout), Ord(Result.Status));
end;

procedure TPairingResultTests.Timeout_IsNotSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.Timeout;
  Assert.IsFalse(Result.IsSuccess);
end;

procedure TPairingResultTests.NotSupported_ReturnsNotSupportedStatus;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.NotSupported('Test reason');
  Assert.AreEqual(Ord(prsNotSupported), Ord(Result.Status));
end;

procedure TPairingResultTests.NotSupported_StoresReason;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.NotSupported('Test reason');
  Assert.AreEqual('Pairing not supported: Test reason', Result.ErrorMessage);
end;

procedure TPairingResultTests.NotSupported_IsNotSuccess;
var
  Result: TPairingResult;
begin
  Result := TPairingResult.NotSupported('Test reason');
  Assert.IsFalse(Result.IsSuccess);
end;

{ TWindowsPairingStrategyTests }

procedure TWindowsPairingStrategyTests.Setup;
begin
  FStrategy := TWindowsPairingStrategy.Create;
end;

procedure TWindowsPairingStrategyTests.CanHandle_Classic_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(bpClassic));
end;

procedure TWindowsPairingStrategyTests.CanHandle_Auto_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(bpAuto));
end;

procedure TWindowsPairingStrategyTests.CanHandle_WinRT_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(bpWinRT));
end;

procedure TWindowsPairingStrategyTests.GetPriority_Returns50;
begin
  Assert.AreEqual(50, FStrategy.GetPriority);
end;

procedure TWindowsPairingStrategyTests.GetName_ReturnsWindowsClassicPairing;
begin
  Assert.AreEqual('Windows Classic Pairing', FStrategy.GetName);
end;

{ TWinRTPairingStrategyTests }

procedure TWinRTPairingStrategyTests.Setup;
begin
  FStrategy := TWinRTSimplePairingStrategy.Create(nil);  // Pass nil config for basic tests
end;

procedure TWinRTPairingStrategyTests.CanHandle_WinRT_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(bpWinRT));
end;

procedure TWinRTPairingStrategyTests.CanHandle_Classic_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(bpClassic));
end;

procedure TWinRTPairingStrategyTests.CanHandle_Auto_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(bpAuto));
end;

procedure TWinRTPairingStrategyTests.GetPriority_Returns100;
begin
  Assert.AreEqual(100, FStrategy.GetPriority);
end;

procedure TWinRTPairingStrategyTests.GetName_ReturnsWinRTSimplePairing;
begin
  Assert.AreEqual('WinRT Simple Pairing (Windows Dialogs)', FStrategy.GetName);
end;

{ TPairingStrategyFactoryTests }

procedure TPairingStrategyFactoryTests.Setup;
begin
  FFactory := CreatePairingStrategyFactory;
end;

procedure TPairingStrategyFactoryTests.Create_RegistersDefaultStrategies;
var
  WindowsStrategy: IPairingStrategy;
  WinRTStrategy: IPairingStrategy;
begin
  // Factory should register at least 2 default strategies (Windows and WinRT)
  WindowsStrategy := FFactory.GetStrategy(bpClassic);
  WinRTStrategy := FFactory.GetStrategy(bpWinRT);

  Assert.IsNotNull(WindowsStrategy);
  Assert.IsNotNull(WinRTStrategy);
end;

procedure TPairingStrategyFactoryTests.GetStrategy_Classic_ReturnsWindowsStrategy;
var
  Strategy: IPairingStrategy;
begin
  Strategy := FFactory.GetStrategy(bpClassic);

  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy.CanHandle(bpClassic));
  Assert.AreEqual(50, Strategy.GetPriority);
  Assert.AreEqual('Windows Classic Pairing', Strategy.GetName);
end;

procedure TPairingStrategyFactoryTests.GetStrategy_Auto_ReturnsWinRTStrategy;
var
  Strategy: IPairingStrategy;
begin
  Strategy := FFactory.GetStrategy(bpAuto);

  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy.CanHandle(bpAuto));
  Assert.AreEqual(100, Strategy.GetPriority);
  Assert.AreEqual('WinRT Simple Pairing (Windows Dialogs)', Strategy.GetName);
end;

procedure TPairingStrategyFactoryTests.GetStrategy_WinRT_ReturnsWinRTStrategy;
var
  Strategy: IPairingStrategy;
begin
  Strategy := FFactory.GetStrategy(bpWinRT);

  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy.CanHandle(bpWinRT));
  Assert.AreEqual(100, Strategy.GetPriority);
  Assert.AreEqual('WinRT Simple Pairing (Windows Dialogs)', Strategy.GetName);
end;

procedure TPairingStrategyFactoryTests.GetStrategy_SelectsHighestPriority;
var
  Factory: IPairingStrategyFactory;
  LowPriorityStrategy: TCustomPairingStrategy;
  HighPriorityStrategy: TCustomPairingStrategy;
  SelectedStrategy: IPairingStrategy;
begin
  // Create factory without default strategies
  Factory := TPairingStrategyFactory.Create;
  Factory.Clear;

  // Register two strategies that can handle bpClassic with different priorities
  LowPriorityStrategy := TCustomPairingStrategy.Create(25);
  HighPriorityStrategy := TCustomPairingStrategy.Create(75);

  Factory.RegisterStrategy(LowPriorityStrategy);
  Factory.RegisterStrategy(HighPriorityStrategy);

  SelectedStrategy := Factory.GetStrategy(bpClassic);

  Assert.IsNotNull(SelectedStrategy);
  Assert.AreEqual(75, SelectedStrategy.GetPriority);
end;

procedure TPairingStrategyFactoryTests.RegisterStrategy_AddsToList;
var
  Factory: IPairingStrategyFactory;
  Strategy: TCustomPairingStrategy;
  SelectedStrategy: IPairingStrategy;
begin
  Factory := TPairingStrategyFactory.Create;
  Factory.Clear;

  // Initially should return nil (no strategies)
  SelectedStrategy := Factory.GetStrategy(bpClassic);
  Assert.IsNull(SelectedStrategy);

  // Register a custom strategy
  Strategy := TCustomPairingStrategy.Create;
  Factory.RegisterStrategy(Strategy);

  // Now should return the registered strategy
  SelectedStrategy := Factory.GetStrategy(bpClassic);
  Assert.IsNotNull(SelectedStrategy);
end;

procedure TPairingStrategyFactoryTests.Clear_RemovesAllStrategies;
var
  Strategy: IPairingStrategy;
begin
  // Factory starts with default strategies
  Strategy := FFactory.GetStrategy(bpClassic);
  Assert.IsNotNull(Strategy);

  // Clear should remove all
  FFactory.Clear;

  Strategy := FFactory.GetStrategy(bpClassic);
  Assert.IsNull(Strategy);
end;

procedure TPairingStrategyFactoryTests.Clear_GetStrategyReturnsNil;
begin
  FFactory.Clear;

  Assert.IsNull(FFactory.GetStrategy(bpClassic));
  Assert.IsNull(FFactory.GetStrategy(bpAuto));
  Assert.IsNull(FFactory.GetStrategy(bpWinRT));
end;

{ TCustomPairingStrategy }

constructor TCustomPairingStrategy.Create(APriority: Integer);
begin
  inherited Create;
  FPriority := APriority;
end;

function TCustomPairingStrategy.Pair(const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback): TPairingResult;
begin
  Result := TPairingResult.NotSupported('Test strategy');
end;

function TCustomPairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
begin
  Result := TPairingResult.NotSupported('Test strategy');
end;

function TCustomPairingStrategy.CanHandle(APlatform: TBluetoothPlatform): Boolean;
begin
  // Handle all platforms for testing
  Result := True;
end;

function TCustomPairingStrategy.GetName: string;
begin
  Result := 'Custom Test Strategy';
end;

function TCustomPairingStrategy.GetPriority: Integer;
begin
  Result := FPriority;
end;

initialization
  TDUnitX.RegisterTestFixture(TPairingResultTests);
  TDUnitX.RegisterTestFixture(TWindowsPairingStrategyTests);
  TDUnitX.RegisterTestFixture(TWinRTPairingStrategyTests);
  TDUnitX.RegisterTestFixture(TPairingStrategyFactoryTests);

end.
