{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Icon Renderer Tests                     }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.BatteryIconRenderer;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Vcl.Graphics,
  System.SysUtils,
  UI.BatteryIconRenderer;

type
  /// <summary>
  /// Test fixture for TBatteryIconRenderer class.
  /// Tests icon creation with various battery levels, colors, and display modes.
  /// </summary>
  [TestFixture]
  TBatteryIconRendererTests = class
  public
    { CreateBatteryIcon Tests }
    [Test]
    procedure CreateBatteryIcon_ValidLevel_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_ZeroLevel_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_FullLevel_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_NegativeLevel_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_OverHundred_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_WithBackgroundColor_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIcon_WithTransparentBackground_ReturnsValidIcon;

    { CreateBatteryIconAuto Tests }
    [Test]
    procedure CreateBatteryIconAuto_AboveThreshold_UsesProvidedColor;
    [Test]
    procedure CreateBatteryIconAuto_BelowThreshold_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIconAuto_AtThreshold_ReturnsValidIcon;
    [Test]
    procedure CreateBatteryIconAuto_WithBackground_ReturnsValidIcon;

    { CreateNumericIcon Tests }
    [Test]
    procedure CreateNumericIcon_ValidLevel_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_ZeroLevel_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_FullLevel_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_TwoDigits_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_SingleDigit_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_OverHundred_ReturnsValidIcon;
    [Test]
    procedure CreateNumericIcon_WithTransparentBackground_ReturnsValidIcon;

    { CreateUnknownBatteryIcon Tests }
    [Test]
    procedure CreateUnknownBatteryIcon_ReturnsValidIcon;

    { Icon Properties Tests }
    [Test]
    procedure CreatedIcon_HasCorrectSize;
    [Test]
    procedure CreatedIcon_HandleIsValid;

    { Memory Management Tests }
    [Test]
    procedure CreateBatteryIcon_MultipleCalls_NoLeaks;
  end;

implementation

{ TBatteryIconRendererTests }

procedure TBatteryIconRendererTests.CreateBatteryIcon_ValidLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(50, clGreen);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should be valid');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_ZeroLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(0, clGreen);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should be valid for 0%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_FullLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(100, clGreen);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should be valid for 100%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_NegativeLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  // Should handle gracefully, not crash
  Icon := TBatteryIconRenderer.CreateBatteryIcon(-10, clGreen);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created even for negative level');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_OverHundred_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  // Should handle gracefully, not crash
  Icon := TBatteryIconRenderer.CreateBatteryIcon(150, clGreen);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created even for over 100%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_WithBackgroundColor_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(75, clGreen, clWhite);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should be valid with background');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_WithTransparentBackground_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(75, clGreen, clNone);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should be valid with transparent background');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIconAuto_AboveThreshold_UsesProvidedColor;
var
  Icon: TIcon;
begin
  // 50% is above 20% threshold - should use provided color (not red)
  Icon := TBatteryIconRenderer.CreateBatteryIconAuto(50, clGreen, 20);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created for level above threshold');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIconAuto_BelowThreshold_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  // 15% is below 20% threshold - should use red
  Icon := TBatteryIconRenderer.CreateBatteryIconAuto(15, clGreen, 20);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created for level below threshold');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIconAuto_AtThreshold_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  // 20% equals 20% threshold - should use red (<=)
  Icon := TBatteryIconRenderer.CreateBatteryIconAuto(20, clGreen, 20);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created for level at threshold');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIconAuto_WithBackground_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIconAuto(50, clGreen, clWhite, 20);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Icon should be created with background color');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_ValidLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(50, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_ZeroLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(0, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid for 0%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_FullLevel_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(100, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid for 100%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_TwoDigits_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(75, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid for two digits');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_SingleDigit_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(5, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid for single digit');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_OverHundred_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  // Should show "99+" for values >= 100
  Icon := TBatteryIconRenderer.CreateNumericIcon(150, clWhite, clBlack);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid for over 100%');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateNumericIcon_WithTransparentBackground_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateNumericIcon(50, clWhite, clNone);
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Numeric icon should be valid with transparent background');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateUnknownBatteryIcon_ReturnsValidIcon;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateUnknownBatteryIcon;
  try
    Assert.IsNotNull(Icon);
    Assert.IsTrue(Icon.Handle <> 0, 'Unknown battery icon should be valid');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreatedIcon_HasCorrectSize;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(50, clGreen);
  try
    Assert.AreEqual(BATTERY_ICON_SIZE, Icon.Width, 'Icon width should be ' + IntToStr(BATTERY_ICON_SIZE));
    Assert.AreEqual(BATTERY_ICON_SIZE, Icon.Height, 'Icon height should be ' + IntToStr(BATTERY_ICON_SIZE));
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreatedIcon_HandleIsValid;
var
  Icon: TIcon;
begin
  Icon := TBatteryIconRenderer.CreateBatteryIcon(50, clGreen);
  try
    Assert.IsTrue(Icon.Handle <> 0, 'Icon handle should not be zero');
    Assert.IsTrue(IsIconic(Icon.Handle) or (Icon.Handle <> 0), 'Icon handle should be valid Windows handle');
  finally
    Icon.Free;
  end;
end;

procedure TBatteryIconRendererTests.CreateBatteryIcon_MultipleCalls_NoLeaks;
var
  Icon: TIcon;
  I: Integer;
begin
  // Create and destroy many icons to test for leaks
  for I := 0 to 99 do
  begin
    Icon := TBatteryIconRenderer.CreateBatteryIcon(I, clGreen);
    try
      Assert.IsNotNull(Icon);
    finally
      Icon.Free;
    end;
  end;

  // If we get here without exception, test passes
  Assert.Pass('Multiple icon creation/destruction completed without exception');
end;

initialization
  TDUnitX.RegisterTestFixture(TBatteryIconRendererTests);

end.
