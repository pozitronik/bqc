{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       System Theme Detector Tests                     }
{                                                       }
{*******************************************************}

unit Tests.SystemThemeDetector;

interface

uses
  DUnitX.TestFramework,
  App.SystemThemeDetector;

type
  /// <summary>
  /// Test fixture for TSystemThemeDetector class.
  /// Tests Windows theme detection functionality.
  /// </summary>
  [TestFixture]
  TSystemThemeDetectorTests = class
  public
    { GetTaskbarTheme Tests }
    [Test]
    procedure GetTaskbarTheme_ReturnsValidValue;

    { IsDarkMode Tests }
    [Test]
    procedure IsDarkMode_ReturnsBooleanConsistentWithGetTaskbarTheme;

    { Consistency Tests }
    [Test]
    procedure MultipleCalls_ReturnConsistentResults;
  end;

implementation

{ TSystemThemeDetectorTests }

procedure TSystemThemeDetectorTests.GetTaskbarTheme_ReturnsValidValue;
var
  Theme: TSystemThemeMode;
begin
  // Should return either stmLight or stmDark (not crash)
  Theme := TSystemThemeDetector.GetTaskbarTheme;
  Assert.IsTrue((Theme = stmLight) or (Theme = stmDark),
    'GetTaskbarTheme should return stmLight or stmDark');
end;

procedure TSystemThemeDetectorTests.IsDarkMode_ReturnsBooleanConsistentWithGetTaskbarTheme;
var
  Theme: TSystemThemeMode;
  IsDark: Boolean;
begin
  Theme := TSystemThemeDetector.GetTaskbarTheme;
  IsDark := TSystemThemeDetector.IsDarkMode;

  // IsDarkMode should return True if and only if Theme is stmDark
  if Theme = stmDark then
    Assert.IsTrue(IsDark, 'IsDarkMode should return True when theme is stmDark')
  else
    Assert.IsFalse(IsDark, 'IsDarkMode should return False when theme is stmLight');
end;

procedure TSystemThemeDetectorTests.MultipleCalls_ReturnConsistentResults;
var
  Theme1, Theme2: TSystemThemeMode;
  IsDark1, IsDark2: Boolean;
begin
  // Multiple calls should return consistent results
  Theme1 := TSystemThemeDetector.GetTaskbarTheme;
  IsDark1 := TSystemThemeDetector.IsDarkMode;
  Theme2 := TSystemThemeDetector.GetTaskbarTheme;
  IsDark2 := TSystemThemeDetector.IsDarkMode;

  Assert.AreEqual(Integer(Theme1), Integer(Theme2),
    'Multiple GetTaskbarTheme calls should return same result');
  Assert.AreEqual(IsDark1, IsDark2,
    'Multiple IsDarkMode calls should return same result');
end;

initialization
  TDUnitX.RegisterTestFixture(TSystemThemeDetectorTests);

end.
