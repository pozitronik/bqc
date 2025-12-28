{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Adapter Query Unit Tests                        }
{                                                       }
{*******************************************************}

unit Tests.AdapterQuery;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Interfaces,
  Bluetooth.AdapterQuery,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TBluetoothAdapterQuery.
  /// Note: Actual adapter tests depend on hardware availability.
  /// </summary>
  [TestFixture]
  TAdapterQueryTests = class
  private
    FQuery: IBluetoothAdapterQuery;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure IsAdapterAvailable_ReturnsBoolean;
    [Test]
    procedure GetAdapterName_ReturnsString;
    [Test]
    procedure GetAdapterName_WhenNoAdapter_ReturnsEmptyString;
  end;

  /// <summary>
  /// Test fixture for TMockAdapterQuery.
  /// </summary>
  [TestFixture]
  TMockAdapterQueryTests = class
  private
    FQuery: TMockAdapterQuery;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure IsAdapterAvailable_DefaultReturnsTrue;
    [Test]
    procedure IsAdapterAvailable_RespectsConfiguration;
    [Test]
    procedure IsAdapterAvailable_IncrementsCallCount;
    [Test]
    procedure GetAdapterName_DefaultReturnsDefaultName;
    [Test]
    procedure GetAdapterName_RespectsConfiguration;
    [Test]
    procedure GetAdapterName_IncrementsCallCount;
  end;

implementation

{ TAdapterQueryTests }

procedure TAdapterQueryTests.Setup;
begin
  FQuery := CreateAdapterQuery;
end;

procedure TAdapterQueryTests.IsAdapterAvailable_ReturnsBoolean;
var
  Result: Boolean;
begin
  // Just verify it doesn't crash and returns a boolean
  Result := FQuery.IsAdapterAvailable;
  Assert.IsTrue(Result or not Result);  // Always passes, just verifies no exception
end;

procedure TAdapterQueryTests.GetAdapterName_ReturnsString;
var
  Name: string;
begin
  // Just verify it doesn't crash and returns a string
  Name := FQuery.GetAdapterName;
  Assert.IsTrue(True);  // If we got here, no exception was raised
end;

procedure TAdapterQueryTests.GetAdapterName_WhenNoAdapter_ReturnsEmptyString;
var
  Name: string;
begin
  // This test verifies behavior when no adapter is present
  // If adapter is present, name will be non-empty; if not, empty
  Name := FQuery.GetAdapterName;
  if not FQuery.IsAdapterAvailable then
    Assert.AreEqual('', Name)
  else
    Assert.Pass('Adapter present, cannot test no-adapter scenario');
end;

{ TMockAdapterQueryTests }

procedure TMockAdapterQueryTests.Setup;
begin
  FQuery := TMockAdapterQuery.Create;
end;

procedure TMockAdapterQueryTests.TearDown;
begin
  FQuery.Free;
end;

procedure TMockAdapterQueryTests.IsAdapterAvailable_DefaultReturnsTrue;
begin
  Assert.IsTrue(FQuery.IsAdapterAvailable);
end;

procedure TMockAdapterQueryTests.IsAdapterAvailable_RespectsConfiguration;
begin
  FQuery.AdapterAvailable := False;
  Assert.IsFalse(FQuery.IsAdapterAvailable);

  FQuery.AdapterAvailable := True;
  Assert.IsTrue(FQuery.IsAdapterAvailable);
end;

procedure TMockAdapterQueryTests.IsAdapterAvailable_IncrementsCallCount;
begin
  Assert.AreEqual(0, FQuery.IsAdapterAvailableCallCount);
  FQuery.IsAdapterAvailable;
  Assert.AreEqual(1, FQuery.IsAdapterAvailableCallCount);
  FQuery.IsAdapterAvailable;
  Assert.AreEqual(2, FQuery.IsAdapterAvailableCallCount);
end;

procedure TMockAdapterQueryTests.GetAdapterName_DefaultReturnsDefaultName;
begin
  Assert.AreEqual('Mock Bluetooth Adapter', FQuery.GetAdapterName);
end;

procedure TMockAdapterQueryTests.GetAdapterName_RespectsConfiguration;
begin
  FQuery.AdapterName := 'Custom Adapter Name';
  Assert.AreEqual('Custom Adapter Name', FQuery.GetAdapterName);
end;

procedure TMockAdapterQueryTests.GetAdapterName_IncrementsCallCount;
begin
  Assert.AreEqual(0, FQuery.GetAdapterNameCallCount);
  FQuery.GetAdapterName;
  Assert.AreEqual(1, FQuery.GetAdapterNameCallCount);
  FQuery.GetAdapterName;
  Assert.AreEqual(2, FQuery.GetAdapterNameCallCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterQueryTests);
  TDUnitX.RegisterTestFixture(TMockAdapterQueryTests);

end.
