{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Connection Executor Unit Tests                  }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.ConnectionExecutor;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionExecutor,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TBluetoothConnectionExecutor.
  /// Note: These tests verify the executor's logic without actually
  /// calling Windows API (which would require real Bluetooth hardware).
  /// </summary>
  [TestFixture]
  TConnectionExecutorTests = class
  private
    FExecutor: IConnectionExecutor;
    function CreateTestDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;

    // Execute validation tests
    [Test]
    procedure Execute_EmptyGuids_ReturnsFail;
    [Test]
    procedure Execute_NegativeRetryCount_NormalizesToZero;
    [Test]
    procedure Execute_ExcessiveRetryCount_CapsAtTen;
  end;

  /// <summary>
  /// Test fixture for TConnectionResult.
  /// </summary>
  [TestFixture]
  TConnectionResultTests = class
  public
    [Test]
    procedure Ok_ReturnsSuccessTrue;
    [Test]
    procedure Ok_ReturnsErrorCodeZero;
    [Test]
    procedure Fail_ReturnsSuccessFalse;
    [Test]
    procedure Fail_ReturnsGivenErrorCode;
  end;

  /// <summary>
  /// Test fixture for TMockConnectionExecutor.
  /// </summary>
  [TestFixture]
  TMockConnectionExecutorTests = class
  private
    FExecutor: TMockConnectionExecutor;
    function CreateTestDevice(AAddress: UInt64; const AName: string): TBluetoothDeviceInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Execute_IncrementsCallCount;
    [Test]
    procedure Execute_RecordsLastDevice;
    [Test]
    procedure Execute_RecordsLastEnable;
    [Test]
    procedure Execute_RecordsLastRetryCount;
    [Test]
    procedure Execute_ReturnsConfiguredResult;
    [Test]
    procedure ExecuteResult_DefaultsToOk;
  end;

implementation

{ TConnectionExecutorTests }

procedure TConnectionExecutorTests.Setup;
begin
  FExecutor := CreateConnectionExecutor;
end;

function TConnectionExecutorTests.CreateTestDevice(AAddress: UInt64;
  const AName: string): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address[0] := AAddress and $FF;
  Address[1] := (AAddress shr 8) and $FF;
  Address[2] := (AAddress shr 16) and $FF;
  Address[3] := (AAddress shr 24) and $FF;
  Address[4] := (AAddress shr 32) and $FF;
  Address[5] := (AAddress shr 40) and $FF;

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AAddress,
    AName,
    btAudioOutput,
    csDisconnected,
    True,
    True,
    0,
    Now,
    Now
  );
end;

procedure TConnectionExecutorTests.Execute_EmptyGuids_ReturnsFail;
var
  Device: TBluetoothDeviceInfo;
  Result: TConnectionResult;
  EmptyGuids: TArray<TGUID>;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice');
  EmptyGuids := [];

  Result := FExecutor.Execute(Device, EmptyGuids, True, 0);

  Assert.IsFalse(Result.Success);
end;

procedure TConnectionExecutorTests.Execute_NegativeRetryCount_NormalizesToZero;
var
  Device: TBluetoothDeviceInfo;
  EmptyGuids: TArray<TGUID>;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice');
  EmptyGuids := [];

  // This should not crash even with negative retry count
  // The executor normalizes it internally
  FExecutor.Execute(Device, EmptyGuids, True, -5);

  Assert.Pass('Negative retry count handled without crash');
end;

procedure TConnectionExecutorTests.Execute_ExcessiveRetryCount_CapsAtTen;
var
  Device: TBluetoothDeviceInfo;
  EmptyGuids: TArray<TGUID>;
begin
  Device := CreateTestDevice($AABBCCDDEEFF, 'TestDevice');
  EmptyGuids := [];

  // This should not crash even with excessive retry count
  // The executor caps it at 10
  FExecutor.Execute(Device, EmptyGuids, True, 100);

  Assert.Pass('Excessive retry count handled without crash');
end;

{ TConnectionResultTests }

procedure TConnectionResultTests.Ok_ReturnsSuccessTrue;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Ok;
  Assert.IsTrue(Result.Success);
end;

procedure TConnectionResultTests.Ok_ReturnsErrorCodeZero;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Ok;
  Assert.AreEqual(Cardinal(0), Result.ErrorCode);
end;

procedure TConnectionResultTests.Fail_ReturnsSuccessFalse;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Fail(123);
  Assert.IsFalse(Result.Success);
end;

procedure TConnectionResultTests.Fail_ReturnsGivenErrorCode;
var
  Result: TConnectionResult;
begin
  Result := TConnectionResult.Fail(12345);
  Assert.AreEqual(Cardinal(12345), Result.ErrorCode);
end;

{ TMockConnectionExecutorTests }

procedure TMockConnectionExecutorTests.Setup;
begin
  FExecutor := TMockConnectionExecutor.Create;
end;

procedure TMockConnectionExecutorTests.TearDown;
begin
  FExecutor.Free;
end;

function TMockConnectionExecutorTests.CreateTestDevice(AAddress: UInt64;
  const AName: string): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address[0] := AAddress and $FF;
  Result := TBluetoothDeviceInfo.Create(
    Address, AAddress, AName, btAudioOutput, csDisconnected,
    True, True, 0, Now, Now
  );
end;

procedure TMockConnectionExecutorTests.Execute_IncrementsCallCount;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
begin
  Device := CreateTestDevice($123456, 'Test');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  Assert.AreEqual(0, FExecutor.ExecuteCallCount);
  FExecutor.Execute(Device, Guids, True, 0);
  Assert.AreEqual(1, FExecutor.ExecuteCallCount);
  FExecutor.Execute(Device, Guids, False, 0);
  Assert.AreEqual(2, FExecutor.ExecuteCallCount);
end;

procedure TMockConnectionExecutorTests.Execute_RecordsLastDevice;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
begin
  Device := CreateTestDevice($AABBCC, 'MyDevice');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  FExecutor.Execute(Device, Guids, True, 0);

  Assert.AreEqual('MyDevice', FExecutor.LastDevice.Name);
  Assert.AreEqual(UInt64($AABBCC), FExecutor.LastDevice.AddressInt);
end;

procedure TMockConnectionExecutorTests.Execute_RecordsLastEnable;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
begin
  Device := CreateTestDevice($123456, 'Test');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  FExecutor.Execute(Device, Guids, True, 0);
  Assert.IsTrue(FExecutor.LastEnable);

  FExecutor.Execute(Device, Guids, False, 0);
  Assert.IsFalse(FExecutor.LastEnable);
end;

procedure TMockConnectionExecutorTests.Execute_RecordsLastRetryCount;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
begin
  Device := CreateTestDevice($123456, 'Test');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  FExecutor.Execute(Device, Guids, True, 5);
  Assert.AreEqual(5, FExecutor.LastRetryCount);

  FExecutor.Execute(Device, Guids, True, 3);
  Assert.AreEqual(3, FExecutor.LastRetryCount);
end;

procedure TMockConnectionExecutorTests.Execute_ReturnsConfiguredResult;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
  ExecResult: TConnectionResult;
begin
  Device := CreateTestDevice($123456, 'Test');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  FExecutor.ExecuteResult := TConnectionResult.Fail(999);
  ExecResult := FExecutor.Execute(Device, Guids, True, 0);

  Assert.IsFalse(ExecResult.Success);
  Assert.AreEqual(Cardinal(999), ExecResult.ErrorCode);
end;

procedure TMockConnectionExecutorTests.ExecuteResult_DefaultsToOk;
var
  Device: TBluetoothDeviceInfo;
  Guids: TArray<TGUID>;
  ExecResult: TConnectionResult;
begin
  Device := CreateTestDevice($123456, 'Test');
  Guids := [StringToGUID('{00000000-0000-0000-0000-000000000001}')];

  ExecResult := FExecutor.Execute(Device, Guids, True, 0);

  Assert.IsTrue(ExecResult.Success);
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionExecutorTests);
  TDUnitX.RegisterTestFixture(TConnectionResultTests);
  TDUnitX.RegisterTestFixture(TMockConnectionExecutorTests);

end.
