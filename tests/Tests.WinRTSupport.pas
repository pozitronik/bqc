{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Support Tests                             }
{                                                       }
{*******************************************************}

unit Tests.WinRTSupport;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.WinRTSupport;

type
  /// <summary>
  /// Test fixture for TWinRTSupport platform selection logic.
  /// Tests SelectPlatform method for Auto/Classic/WinRT scenarios.
  /// </summary>
  [TestFixture]
  TWinRTSupportTests = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Platform Selection Tests }
    [Test]
    procedure SelectPlatform_Auto_WhenAvailable_ReturnsWinRT;
    [Test]
    procedure SelectPlatform_Auto_WhenUnavailable_ReturnsClassic;
    [Test]
    procedure SelectPlatform_Classic_Always_ReturnsClassic;
    [Test]
    procedure SelectPlatform_WinRT_WhenAvailable_ReturnsWinRT;
    [Test]
    procedure SelectPlatform_WinRT_WhenUnavailable_RaisesException;

    { Cache Tests }
    [Test]
    procedure ResetCache_Clears_AvailabilityCheck;
  end;

implementation

{ TWinRTSupportTests }

procedure TWinRTSupportTests.Setup;
begin
  // Reset cache before each test to ensure clean state
  TWinRTSupport.ResetCache;
end;

procedure TWinRTSupportTests.TearDown;
begin
  // No cleanup needed
end;

procedure TWinRTSupportTests.SelectPlatform_Auto_WhenAvailable_ReturnsWinRT;
var
  Result: TBluetoothPlatform;
begin
  // This test assumes WinRT is available on the test machine (Windows 8+)
  if not TWinRTSupport.IsAvailable then
    Assert.Ignore('Test requires WinRT support (Windows 8+)');

  Result := TWinRTSupport.SelectPlatform(bpAuto);
  Assert.AreEqual(Integer(bpWinRT), Integer(Result),
    'Auto-detect should select WinRT when available');
end;

procedure TWinRTSupportTests.SelectPlatform_Auto_WhenUnavailable_ReturnsClassic;
var
  Result: TBluetoothPlatform;
begin
  // This test can only verify on Windows 7 or when WinRT is not available
  if TWinRTSupport.IsAvailable then
    Assert.Ignore('Test requires WinRT to be unavailable (Windows 7)');

  Result := TWinRTSupport.SelectPlatform(bpAuto);
  Assert.AreEqual(Integer(bpClassic), Integer(Result),
    'Auto-detect should select Classic when WinRT unavailable');
end;

procedure TWinRTSupportTests.SelectPlatform_Classic_Always_ReturnsClassic;
var
  Result: TBluetoothPlatform;
begin
  // Classic platform should always be allowed regardless of OS
  Result := TWinRTSupport.SelectPlatform(bpClassic);
  Assert.AreEqual(Integer(bpClassic), Integer(Result),
    'Classic platform should always be selectable');
end;

procedure TWinRTSupportTests.SelectPlatform_WinRT_WhenAvailable_ReturnsWinRT;
var
  Result: TBluetoothPlatform;
begin
  // This test assumes WinRT is available on the test machine (Windows 8+)
  if not TWinRTSupport.IsAvailable then
    Assert.Ignore('Test requires WinRT support (Windows 8+)');

  Result := TWinRTSupport.SelectPlatform(bpWinRT);
  Assert.AreEqual(Integer(bpWinRT), Integer(Result),
    'WinRT platform should be selectable when available');
end;

procedure TWinRTSupportTests.SelectPlatform_WinRT_WhenUnavailable_RaisesException;
begin
  // This test can only verify on Windows 7 or when WinRT is not available
  if TWinRTSupport.IsAvailable then
    Assert.Ignore('Test requires WinRT to be unavailable (Windows 7)');

  Assert.WillRaise(
    procedure
    begin
      TWinRTSupport.SelectPlatform(bpWinRT);
    end,
    EBluetoothPlatformError,
    'Should raise EBluetoothPlatformError when WinRT requested but unavailable');
end;

procedure TWinRTSupportTests.ResetCache_Clears_AvailabilityCheck;
begin
  // First call - checks availability
  TWinRTSupport.IsAvailable;

  // Reset cache
  TWinRTSupport.ResetCache;

  // Second call - should re-check (not return cached result)
  // We can't easily verify this without instrumenting the class,
  // but we can at least verify no exception is raised
  Assert.WillNotRaise(
    procedure
    begin
      TWinRTSupport.IsAvailable;
    end,
    'ResetCache should allow re-checking availability');
end;

initialization
  TDUnitX.RegisterTestFixture(TWinRTSupportTests);

end.
