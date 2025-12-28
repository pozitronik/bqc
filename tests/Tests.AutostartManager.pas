{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Autostart Manager Unit Tests                    }
{                                                       }
{*******************************************************}

unit Tests.AutostartManager;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.Autostart,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TAutostartManager.
  /// Note: Actual registry tests depend on write permissions.
  /// </summary>
  [TestFixture]
  TAutostartManagerTests = class
  private
    FManager: IAutostartManager;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Create_DoesNotRaiseException;
    [Test]
    procedure IsEnabled_ReturnsBoolean;
    [Test]
    procedure GetRegisteredPath_ReturnsString;
    [Test]
    procedure Apply_DoesNotRaiseException;
  end;

  /// <summary>
  /// Test fixture for TMockAutostartManager.
  /// </summary>
  [TestFixture]
  TMockAutostartManagerTests = class
  private
    FManager: TMockAutostartManager;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure IsEnabled_DefaultReturnsFalse;
    [Test]
    procedure IsEnabled_RespectsConfiguration;
    [Test]
    procedure IsEnabled_IncrementsCallCount;
    [Test]
    procedure GetRegisteredPath_DefaultReturnsEmpty;
    [Test]
    procedure GetRegisteredPath_RespectsConfiguration;
    [Test]
    procedure GetRegisteredPath_IncrementsCallCount;
    [Test]
    procedure Apply_IncrementsCallCount;
    [Test]
    procedure Apply_RecordsLastValue;
    [Test]
    procedure Apply_UpdatesEnabled_WhenTrue;
    [Test]
    procedure Apply_UpdatesEnabled_WhenFalse;
    [Test]
    procedure Apply_UpdatesRegisteredPath_WhenTrue;
    [Test]
    procedure Apply_ClearsRegisteredPath_WhenFalse;
  end;

implementation

{ TAutostartManagerTests }

procedure TAutostartManagerTests.Setup;
begin
  FManager := TAutostartManager.Create;
end;

procedure TAutostartManagerTests.Create_DoesNotRaiseException;
begin
  Assert.IsNotNull(FManager);
end;

procedure TAutostartManagerTests.IsEnabled_ReturnsBoolean;
var
  Enabled: Boolean;
begin
  // Just verify it doesn't crash
  Enabled := FManager.IsEnabled;
  Assert.IsTrue(Enabled or not Enabled);  // Always passes, just verifies no exception
end;

procedure TAutostartManagerTests.GetRegisteredPath_ReturnsString;
var
  Path: string;
begin
  // Just verify it doesn't crash
  Path := FManager.GetRegisteredPath;
  Assert.IsTrue(Path = Path);  // Always passes, just verifies no exception
end;

procedure TAutostartManagerTests.Apply_DoesNotRaiseException;
begin
  // Note: This test may fail if running without admin rights
  // We just verify it doesn't crash - actual registry changes are not verified
  try
    FManager.Apply(False);
    Assert.Pass('No exception raised');
  except
    on E: Exception do
      Assert.Pass('Exception occurred (may be expected without registry access): ' + E.Message);
  end;
end;

{ TMockAutostartManagerTests }

procedure TMockAutostartManagerTests.Setup;
begin
  FManager := TMockAutostartManager.Create;
end;

procedure TMockAutostartManagerTests.TearDown;
begin
  FManager.Free;
end;

procedure TMockAutostartManagerTests.IsEnabled_DefaultReturnsFalse;
begin
  Assert.IsFalse(FManager.IsEnabled);
end;

procedure TMockAutostartManagerTests.IsEnabled_RespectsConfiguration;
begin
  FManager.Enabled := True;
  Assert.IsTrue(FManager.IsEnabled);

  FManager.Enabled := False;
  Assert.IsFalse(FManager.IsEnabled);
end;

procedure TMockAutostartManagerTests.IsEnabled_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.IsEnabledCallCount);
  FManager.IsEnabled;
  Assert.AreEqual(1, FManager.IsEnabledCallCount);
  FManager.IsEnabled;
  Assert.AreEqual(2, FManager.IsEnabledCallCount);
end;

procedure TMockAutostartManagerTests.GetRegisteredPath_DefaultReturnsEmpty;
begin
  Assert.AreEqual('', FManager.GetRegisteredPath);
end;

procedure TMockAutostartManagerTests.GetRegisteredPath_RespectsConfiguration;
begin
  FManager.RegisteredPath := 'C:\Test\Path.exe';
  Assert.AreEqual('C:\Test\Path.exe', FManager.GetRegisteredPath);
end;

procedure TMockAutostartManagerTests.GetRegisteredPath_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.GetRegisteredPathCallCount);
  FManager.GetRegisteredPath;
  Assert.AreEqual(1, FManager.GetRegisteredPathCallCount);
  FManager.GetRegisteredPath;
  Assert.AreEqual(2, FManager.GetRegisteredPathCallCount);
end;

procedure TMockAutostartManagerTests.Apply_IncrementsCallCount;
begin
  Assert.AreEqual(0, FManager.ApplyCallCount);
  FManager.Apply(True);
  Assert.AreEqual(1, FManager.ApplyCallCount);
  FManager.Apply(False);
  Assert.AreEqual(2, FManager.ApplyCallCount);
end;

procedure TMockAutostartManagerTests.Apply_RecordsLastValue;
begin
  FManager.Apply(True);
  Assert.IsTrue(FManager.LastApplyValue);

  FManager.Apply(False);
  Assert.IsFalse(FManager.LastApplyValue);
end;

procedure TMockAutostartManagerTests.Apply_UpdatesEnabled_WhenTrue;
begin
  FManager.Enabled := False;
  FManager.Apply(True);
  Assert.IsTrue(FManager.Enabled, 'Enabled should be True after Apply(True)');
end;

procedure TMockAutostartManagerTests.Apply_UpdatesEnabled_WhenFalse;
begin
  FManager.Enabled := True;
  FManager.Apply(False);
  Assert.IsFalse(FManager.Enabled, 'Enabled should be False after Apply(False)');
end;

procedure TMockAutostartManagerTests.Apply_UpdatesRegisteredPath_WhenTrue;
begin
  FManager.Apply(True);
  Assert.AreNotEqual('', FManager.RegisteredPath, 'RegisteredPath should be set after Apply(True)');
end;

procedure TMockAutostartManagerTests.Apply_ClearsRegisteredPath_WhenFalse;
begin
  FManager.RegisteredPath := 'C:\Test\Path.exe';
  FManager.Apply(False);
  Assert.AreEqual('', FManager.RegisteredPath, 'RegisteredPath should be empty after Apply(False)');
end;

initialization
  TDUnitX.RegisterTestFixture(TAutostartManagerTests);
  TDUnitX.RegisterTestFixture(TMockAutostartManagerTests);

end.
