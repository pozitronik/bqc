{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Pairing Service Tests                           }
{                                                       }
{       Tests for TBluetoothPairingService covering:    }
{       - Constructor validation                        }
{       - Strategy delegation for Pair/Unpair           }
{       - Result status handling                        }
{       - No-strategy fallback behavior                 }
{                                                       }
{       NOTE: IsDevicePaired and GetPairedDeviceAddresses}
{       directly call Windows APIs and are not unit     }
{       testable without OS-level mocking.              }
{                                                       }
{*******************************************************}

unit Tests.PairingService;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.PairingStrategies,
  Bluetooth.PairingService,
  App.ConfigEnums,
  App.ConnectionConfigIntf;

type
  /// <summary>
  /// Mock pairing strategy for testing service delegation.
  /// Returns configurable results for Pair and Unpair operations.
  /// </summary>
  TMockPairingStrategy = class(TInterfacedObject, IPairingStrategy)
  private
    FPairResult: TPairingResult;
    FUnpairResult: TPairingResult;
    FPairCallCount: Integer;
    FUnpairCallCount: Integer;
    FLastPairDevice: TBluetoothDeviceInfo;
    FLastUnpairAddress: UInt64;
    FLastProgressCallback: TPairingProgressCallback;
  public
    constructor Create;

    // IPairingStrategy
    function Pair(const ADevice: TBluetoothDeviceInfo;
      AProgressCallback: TPairingProgressCallback): TPairingResult;
    function Unpair(ADeviceAddress: UInt64): TPairingResult;
    function CanHandle: Boolean;
    function GetName: string;
    function GetPriority: Integer;

    // Test configuration
    property PairResult: TPairingResult read FPairResult write FPairResult;
    property UnpairResult: TPairingResult read FUnpairResult write FUnpairResult;

    // Test verification
    property PairCallCount: Integer read FPairCallCount;
    property UnpairCallCount: Integer read FUnpairCallCount;
    property LastPairDevice: TBluetoothDeviceInfo read FLastPairDevice;
    property LastUnpairAddress: UInt64 read FLastUnpairAddress;
    property LastProgressCallback: TPairingProgressCallback read FLastProgressCallback;
  end;

  /// <summary>
  /// Mock pairing strategy factory for testing.
  /// Returns configurable strategy or nil.
  /// </summary>
  TMockPairingStrategyFactory = class(TInterfacedObject, IPairingStrategyFactory)
  private
    FStrategy: IPairingStrategy;
    FGetStrategyCallCount: Integer;
  public
    constructor Create;

    // IPairingStrategyFactory
    function GetStrategy: IPairingStrategy;
    procedure RegisterStrategy(AStrategy: IPairingStrategy);
    procedure Clear;

    // Test configuration
    property Strategy: IPairingStrategy read FStrategy write FStrategy;

    // Test verification
    property GetStrategyCallCount: Integer read FGetStrategyCallCount;
  end;

  /// <summary>
  /// Tests for TBluetoothPairingService focusing on
  /// constructor validation and strategy delegation.
  /// </summary>
  [TestFixture]
  TBluetoothPairingServiceTests = class
  private
    FService: IBluetoothPairingService;
    FFactory: TMockPairingStrategyFactory;
    FStrategy: TMockPairingStrategy;

    function CreateTestDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Constructor tests
    [Test]
    procedure Create_WithNilFactory_RaisesException;
    [Test]
    procedure Create_WithValidFactory_Succeeds;
    [Test]
    procedure Create_AcceptsNilRepositoryAndConfig;

    // PairDevice - delegation tests
    [Test]
    procedure PairDevice_CallsFactoryGetStrategy;
    [Test]
    procedure PairDevice_DelegatesToStrategy;
    [Test]
    procedure PairDevice_PassesDeviceToStrategy;
    [Test]
    procedure PairDevice_PassesCallbackToStrategy;
    [Test]
    procedure PairDevice_ReturnsStrategyResult;

    // PairDevice - no strategy tests
    [Test]
    procedure PairDevice_WhenNoStrategy_ReturnsNotSupported;
    [Test]
    procedure PairDevice_WhenNoStrategy_DoesNotThrow;

    // PairDevice - result status tests
    [Test]
    procedure PairDevice_Success_ReturnsSuccess;
    [Test]
    procedure PairDevice_Failed_ReturnsFailed;
    [Test]
    procedure PairDevice_Cancelled_ReturnsCancelled;
    [Test]
    procedure PairDevice_AlreadyPaired_ReturnsAlreadyPaired;
    [Test]
    procedure PairDevice_Timeout_ReturnsTimeout;

    // UnpairDevice - delegation tests
    [Test]
    procedure UnpairDevice_CallsFactoryGetStrategy;
    [Test]
    procedure UnpairDevice_DelegatesToStrategy;
    [Test]
    procedure UnpairDevice_PassesAddressToStrategy;
    [Test]
    procedure UnpairDevice_ReturnsStrategyResult;

    // UnpairDevice - no strategy tests
    [Test]
    procedure UnpairDevice_WhenNoStrategy_ReturnsNotSupported;
    [Test]
    procedure UnpairDevice_WhenNoStrategy_DoesNotThrow;

    // UnpairDevice - result status tests
    [Test]
    procedure UnpairDevice_Success_ReturnsSuccess;
    [Test]
    procedure UnpairDevice_Failed_ReturnsFailed;
  end;

implementation

{ TMockPairingStrategy }

constructor TMockPairingStrategy.Create;
begin
  inherited Create;
  FPairResult := TPairingResult.Success;
  FUnpairResult := TPairingResult.Success;
  FPairCallCount := 0;
  FUnpairCallCount := 0;
  FLastUnpairAddress := 0;
end;

function TMockPairingStrategy.Pair(const ADevice: TBluetoothDeviceInfo;
  AProgressCallback: TPairingProgressCallback): TPairingResult;
begin
  Inc(FPairCallCount);
  FLastPairDevice := ADevice;
  FLastProgressCallback := AProgressCallback;
  Result := FPairResult;
end;

function TMockPairingStrategy.Unpair(ADeviceAddress: UInt64): TPairingResult;
begin
  Inc(FUnpairCallCount);
  FLastUnpairAddress := ADeviceAddress;
  Result := FUnpairResult;
end;

function TMockPairingStrategy.CanHandle: Boolean;
begin
  Result := True;
end;

function TMockPairingStrategy.GetName: string;
begin
  Result := 'Mock Pairing Strategy';
end;

function TMockPairingStrategy.GetPriority: Integer;
begin
  Result := 100;
end;

{ TMockPairingStrategyFactory }

constructor TMockPairingStrategyFactory.Create;
begin
  inherited Create;
  FStrategy := nil;
  FGetStrategyCallCount := 0;
end;

function TMockPairingStrategyFactory.GetStrategy: IPairingStrategy;
begin
  Inc(FGetStrategyCallCount);
  Result := FStrategy;
end;

procedure TMockPairingStrategyFactory.RegisterStrategy(AStrategy: IPairingStrategy);
begin
  FStrategy := AStrategy;
end;

procedure TMockPairingStrategyFactory.Clear;
begin
  FStrategy := nil;
end;

{ TBluetoothPairingServiceTests }

procedure TBluetoothPairingServiceTests.Setup;
begin
  FFactory := TMockPairingStrategyFactory.Create;
  FStrategy := TMockPairingStrategy.Create;
  FFactory.Strategy := FStrategy;

  FService := TBluetoothPairingService.Create(
    FFactory as IPairingStrategyFactory,
    nil,  // DeviceRepository - not used
    nil   // ConnectionConfig - not used
  );
end;

procedure TBluetoothPairingServiceTests.TearDown;
begin
  FService := nil;
  FStrategy := nil;
  FFactory := nil;
end;

function TBluetoothPairingServiceTests.CreateTestDevice(AAddress: UInt64;
  const AName: string): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddress),
    AAddress,
    AName,
    btAudioOutput,
    csDisconnected,
    False,  // IsPaired
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

{ Constructor tests }

procedure TBluetoothPairingServiceTests.Create_WithNilFactory_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      TBluetoothPairingService.Create(nil, nil, nil);
    end,
    EArgumentNilException,
    'Should raise EArgumentNilException for nil factory'
  );
end;

procedure TBluetoothPairingServiceTests.Create_WithValidFactory_Succeeds;
var
  Service: IBluetoothPairingService;
begin
  Service := TBluetoothPairingService.Create(
    FFactory as IPairingStrategyFactory,
    nil,
    nil
  );

  Assert.IsNotNull(Service, 'Service should be created successfully');
end;

procedure TBluetoothPairingServiceTests.Create_AcceptsNilRepositoryAndConfig;
var
  Service: IBluetoothPairingService;
begin
  // Should not raise - nil repository and config are acceptable
  Service := TBluetoothPairingService.Create(
    FFactory as IPairingStrategyFactory,
    nil,
    nil
  );

  Assert.IsNotNull(Service, 'Service should accept nil repository and config');
end;

{ PairDevice - delegation tests }

procedure TBluetoothPairingServiceTests.PairDevice_CallsFactoryGetStrategy;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001, 'Test Device');

  FService.PairDevice(Device, nil);

  Assert.AreEqual(1, FFactory.GetStrategyCallCount,
    'Should call GetStrategy on factory');
end;

procedure TBluetoothPairingServiceTests.PairDevice_DelegatesToStrategy;
var
  Device: TBluetoothDeviceInfo;
begin
  Device := CreateTestDevice($001, 'Test Device');

  FService.PairDevice(Device, nil);

  Assert.AreEqual(1, FStrategy.PairCallCount,
    'Should delegate to strategy Pair method');
end;

procedure TBluetoothPairingServiceTests.PairDevice_PassesDeviceToStrategy;
var
  Device: TBluetoothDeviceInfo;
  ExpectedAddress: UInt64;
begin
  ExpectedAddress := $AABBCCDD;
  Device := CreateTestDevice(ExpectedAddress, 'My Headphones');

  FService.PairDevice(Device, nil);

  Assert.AreEqual(ExpectedAddress, FStrategy.LastPairDevice.AddressInt,
    'Should pass correct device address to strategy');
  Assert.AreEqual('My Headphones', FStrategy.LastPairDevice.Name,
    'Should pass correct device name to strategy');
end;

procedure TBluetoothPairingServiceTests.PairDevice_PassesCallbackToStrategy;
var
  Device: TBluetoothDeviceInfo;
  CallbackCalled: Boolean;
  ProgressCallback: TPairingProgressCallback;
begin
  Device := CreateTestDevice($001, 'Test');
  CallbackCalled := False;

  ProgressCallback := procedure(const AProgress: string)
    begin
      CallbackCalled := True;
    end;

  FService.PairDevice(Device, ProgressCallback);

  // Verify callback was passed by checking it's assigned
  Assert.IsTrue(Assigned(FStrategy.LastProgressCallback),
    'Should pass callback to strategy');
end;

procedure TBluetoothPairingServiceTests.PairDevice_ReturnsStrategyResult;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.AlreadyPaired;

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsAlreadyPaired), Ord(Result.Status),
    'Should return result from strategy');
end;

{ PairDevice - no strategy tests }

procedure TBluetoothPairingServiceTests.PairDevice_WhenNoStrategy_ReturnsNotSupported;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FFactory.Strategy := nil;  // No strategy available

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsNotSupported), Ord(Result.Status),
    'Should return NotSupported when no strategy');
end;

procedure TBluetoothPairingServiceTests.PairDevice_WhenNoStrategy_DoesNotThrow;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FFactory.Strategy := nil;

  // Should not throw - just return NotSupported
  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsNotSupported), Ord(Result.Status),
    'Should return NotSupported without throwing');
end;

{ PairDevice - result status tests }

procedure TBluetoothPairingServiceTests.PairDevice_Success_ReturnsSuccess;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.Success;

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsSuccess), Ord(Result.Status));
  Assert.IsTrue(Result.IsSuccess);
end;

procedure TBluetoothPairingServiceTests.PairDevice_Failed_ReturnsFailed;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.Failed(123, 'Auth failed');

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsFailed), Ord(Result.Status));
  Assert.AreEqual(Cardinal(123), Result.ErrorCode);
  Assert.AreEqual('Auth failed', Result.ErrorMessage);
end;

procedure TBluetoothPairingServiceTests.PairDevice_Cancelled_ReturnsCancelled;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.Cancelled;

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsCancelled), Ord(Result.Status));
end;

procedure TBluetoothPairingServiceTests.PairDevice_AlreadyPaired_ReturnsAlreadyPaired;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.AlreadyPaired;

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsAlreadyPaired), Ord(Result.Status));
  Assert.IsTrue(Result.IsSuccess, 'AlreadyPaired should be considered success');
end;

procedure TBluetoothPairingServiceTests.PairDevice_Timeout_ReturnsTimeout;
var
  Device: TBluetoothDeviceInfo;
  Result: TPairingResult;
begin
  Device := CreateTestDevice($001, 'Test');
  FStrategy.PairResult := TPairingResult.Timeout;

  Result := FService.PairDevice(Device, nil);

  Assert.AreEqual(Ord(prsTimeout), Ord(Result.Status));
end;

{ UnpairDevice - delegation tests }

procedure TBluetoothPairingServiceTests.UnpairDevice_CallsFactoryGetStrategy;
begin
  FService.UnpairDevice($001);

  Assert.AreEqual(1, FFactory.GetStrategyCallCount,
    'Should call GetStrategy on factory');
end;

procedure TBluetoothPairingServiceTests.UnpairDevice_DelegatesToStrategy;
begin
  FService.UnpairDevice($001);

  Assert.AreEqual(1, FStrategy.UnpairCallCount,
    'Should delegate to strategy Unpair method');
end;

procedure TBluetoothPairingServiceTests.UnpairDevice_PassesAddressToStrategy;
var
  ExpectedAddress: UInt64;
begin
  ExpectedAddress := $AABBCCDDEEFF;

  FService.UnpairDevice(ExpectedAddress);

  Assert.AreEqual(ExpectedAddress, FStrategy.LastUnpairAddress,
    'Should pass correct address to strategy');
end;

procedure TBluetoothPairingServiceTests.UnpairDevice_ReturnsStrategyResult;
var
  Result: TPairingResult;
begin
  FStrategy.UnpairResult := TPairingResult.Failed(456, 'Device busy');

  Result := FService.UnpairDevice($001);

  Assert.AreEqual(Ord(prsFailed), Ord(Result.Status),
    'Should return result from strategy');
  Assert.AreEqual(Cardinal(456), Result.ErrorCode);
end;

{ UnpairDevice - no strategy tests }

procedure TBluetoothPairingServiceTests.UnpairDevice_WhenNoStrategy_ReturnsNotSupported;
var
  Result: TPairingResult;
begin
  FFactory.Strategy := nil;

  Result := FService.UnpairDevice($001);

  Assert.AreEqual(Ord(prsNotSupported), Ord(Result.Status),
    'Should return NotSupported when no strategy');
end;

procedure TBluetoothPairingServiceTests.UnpairDevice_WhenNoStrategy_DoesNotThrow;
var
  Result: TPairingResult;
begin
  FFactory.Strategy := nil;

  // Should not throw - just return NotSupported
  Result := FService.UnpairDevice($001);

  Assert.AreEqual(Ord(prsNotSupported), Ord(Result.Status),
    'Should return NotSupported without throwing');
end;

{ UnpairDevice - result status tests }

procedure TBluetoothPairingServiceTests.UnpairDevice_Success_ReturnsSuccess;
var
  Result: TPairingResult;
begin
  FStrategy.UnpairResult := TPairingResult.Success;

  Result := FService.UnpairDevice($001);

  Assert.AreEqual(Ord(prsSuccess), Ord(Result.Status));
  Assert.IsTrue(Result.IsSuccess);
end;

procedure TBluetoothPairingServiceTests.UnpairDevice_Failed_ReturnsFailed;
var
  Result: TPairingResult;
begin
  FStrategy.UnpairResult := TPairingResult.Failed(789, 'Access denied');

  Result := FService.UnpairDevice($001);

  Assert.AreEqual(Ord(prsFailed), Ord(Result.Status));
  Assert.AreEqual(Cardinal(789), Result.ErrorCode);
  Assert.AreEqual('Access denied', Result.ErrorMessage);
end;

initialization
  TDUnitX.RegisterTestFixture(TBluetoothPairingServiceTests);

end.
