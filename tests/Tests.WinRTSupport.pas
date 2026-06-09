{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       WinRT Support Detection Tests                   }
{                                                       }
{*******************************************************}

unit Tests.WinRTSupport;

interface

uses
  DUnitX.TestFramework,
  App.WinRTSupport;

type
  /// <summary>
  /// Test fixture for TWinRTSupport availability detection.
  /// Detection itself depends on the OS, so tests assert the contract
  /// (caching, stability, shorthand consistency) rather than absolute
  /// values - except IsAvailable, which must be True because the
  /// application and its test suite target Windows 10+ only.
  /// </summary>
  [TestFixture]
  TWinRTSupportTests = class
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure IsAvailable_OnWindows10Plus_ReturnsTrue;
    [Test]
    procedure IsAvailable_RepeatedCalls_ReturnsSameValue;
    [Test]
    procedure IsAvailable_AfterResetCache_ReturnsSameValue;
    [Test]
    procedure IsDarkModeSupported_RepeatedCalls_ReturnsSameValue;
    [Test]
    procedure ShorthandFunctions_MatchClassMethods;
  end;

implementation

{ TWinRTSupportTests }

procedure TWinRTSupportTests.Setup;
begin
  // Other fixtures may have triggered detection already; start clean
  TWinRTSupport.ResetCache;
end;

procedure TWinRTSupportTests.IsAvailable_OnWindows10Plus_ReturnsTrue;
begin
  Assert.IsTrue(TWinRTSupport.IsAvailable,
    'WinRT must be detected on Windows 10+ (combase.dll/RoInitialize present); ' +
    'False means the detection logic is broken');
end;

procedure TWinRTSupportTests.IsAvailable_RepeatedCalls_ReturnsSameValue;
var
  First: Boolean;
begin
  First := TWinRTSupport.IsAvailable;
  Assert.AreEqual(First, TWinRTSupport.IsAvailable,
    'Cached result must not change between calls');
end;

procedure TWinRTSupportTests.IsAvailable_AfterResetCache_ReturnsSameValue;
var
  First: Boolean;
begin
  First := TWinRTSupport.IsAvailable;
  TWinRTSupport.ResetCache;
  Assert.AreEqual(First, TWinRTSupport.IsAvailable,
    'Re-running detection on the same OS must yield the same result');
end;

procedure TWinRTSupportTests.IsDarkModeSupported_RepeatedCalls_ReturnsSameValue;
var
  First: Boolean;
begin
  First := TWinRTSupport.IsDarkModeSupported;
  Assert.AreEqual(First, TWinRTSupport.IsDarkModeSupported,
    'OS version query must be deterministic within a process');
end;

procedure TWinRTSupportTests.ShorthandFunctions_MatchClassMethods;
begin
  Assert.AreEqual(TWinRTSupport.IsAvailable, IsWinRTAvailable,
    'IsWinRTAvailable shorthand must delegate to TWinRTSupport.IsAvailable');
  Assert.AreEqual(TWinRTSupport.IsDarkModeSupported,
    App.WinRTSupport.IsDarkModeSupported,
    'IsDarkModeSupported shorthand must delegate to the class method');
end;

initialization
  TDUnitX.RegisterTestFixture(TWinRTSupportTests);

end.
