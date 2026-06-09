{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       DPI Scaling Tests                               }
{                                                       }
{       Verifies that TListDataSource and               }
{       TCustomScrollbar scale correctly for DPI.       }
{                                                       }
{*******************************************************}

unit Tests.DpiScaling;

interface

uses
  DUnitX.TestFramework,
  App.DpiScaling,
  UI.ListDataSource,
  UI.CustomScrollbar,
  Tests.Mocks.Config;

type
  /// <summary>
  /// Tests that TListDataSource scales item heights for different DPI values.
  /// Config values are treated as 96-DPI baseline; heights scale proportionally.
  /// </summary>
  [TestFixture]
  TDataSourceDpiTests = class
  private
    FDataSource: TListDataSource;
    FLayoutConfig: TMockLayoutConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure UpdateConfigs_At96DPI_ReturnsRawItemHeight;

    [Test]
    procedure UpdateConfigs_At192DPI_DoublesItemHeight;

    [Test]
    procedure UpdateConfigs_At144DPI_ScalesItemHeightBy150Percent;

    [Test]
    procedure UpdateConfigs_At96DPI_ActionButtonHeightIsUnscaled;

    [Test]
    procedure UpdateConfigs_At192DPI_ActionButtonHeightDoubles;

    [Test]
    procedure UpdateConfigs_PpiChangeRecalculatesHeights;
  end;

  /// <summary>
  /// Tests that TCustomScrollbar scales geometry for different DPI values.
  /// </summary>
  [TestFixture]
  TScrollbarDpiTests = class
  private
    FScrollbar: TCustomScrollbar;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure DefaultPPI_Is96;

    [Test]
    procedure UpdateDPI_At192_DoublesScrollbarWidth;

    [Test]
    procedure UpdateDPI_At96_ScrollbarWidthIs12;

    [Test]
    procedure MouseWheel_At96DPI_ScrollsBy60Pixels;

    [Test]
    procedure MouseWheel_At240DPI_ScalesScrollSpeed;

    [Test]
    procedure MouseWheel_NoScrollableContent_ReturnsFalse;

    [Test]
    procedure MouseWheel_WithScrollableContent_ReturnsTrue;
  end;

  /// <summary>
  /// Tests the pure DPI conversion helpers (App.DpiScaling) used to persist window
  /// geometry in a DPI-neutral form. The round-trip must never grow a value across
  /// cross-DPI moves -- regression guard for the window-size runaway-growth bug.
  /// </summary>
  [TestFixture]
  TDpiScalingHelperTests = class
  public
    [Test]
    procedure RoundTrip_PreservesLogicalValue_AcrossDpiValues;
    [Test]
    procedure CrossDpiMove_DoesNotCompound_OverManyIterations;
    [Test]
    procedure LogicalToPhysical_At96_IsIdentity;
    [Test]
    procedure LogicalToPhysical_At192_Doubles;
    [Test]
    procedure NonPositivePPI_TreatedAs96;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  App.DeviceDisplayTypes,
  Bluetooth.Types;

const
  DEFAULT_DPI = 96;

{ TDataSourceDpiTests }

procedure TDataSourceDpiTests.Setup;
begin
  FDataSource := TListDataSource.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FLayoutConfig.ItemHeight := 70;
end;

procedure TDataSourceDpiTests.TearDown;
begin
  FDataSource.Free;
  // FLayoutConfig freed by reference counting
end;

procedure TDataSourceDpiTests.UpdateConfigs_At96DPI_ReturnsRawItemHeight;
var
  Items: TDeviceDisplayItemArray;
begin
  // Arrange: single paired device item
  SetLength(Items, 1);
  Items[0].Source := dsPaired;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  // Act
  FDataSource.UpdateConfigs(FLayoutConfig, nil, 96);

  // Assert: at 96 DPI, item height equals raw config value
  Assert.AreEqual(70, FDataSource.GetItemHeight(0),
    'At 96 DPI, item height should equal config value (70)');
end;

procedure TDataSourceDpiTests.UpdateConfigs_At192DPI_DoublesItemHeight;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 1);
  Items[0].Source := dsPaired;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  FDataSource.UpdateConfigs(FLayoutConfig, nil, 192);

  Assert.AreEqual(140, FDataSource.GetItemHeight(0),
    'At 192 DPI (200%), item height should double to 140');
end;

procedure TDataSourceDpiTests.UpdateConfigs_At144DPI_ScalesItemHeightBy150Percent;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 1);
  Items[0].Source := dsPaired;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  FDataSource.UpdateConfigs(FLayoutConfig, nil, 144);

  // MulDiv(70, 144, 96) = 105
  Assert.AreEqual(105, FDataSource.GetItemHeight(0),
    'At 144 DPI (150%), item height should be 105');
end;

procedure TDataSourceDpiTests.UpdateConfigs_At96DPI_ActionButtonHeightIsUnscaled;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 1);
  Items[0].Source := dsAction;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  FDataSource.UpdateConfigs(FLayoutConfig, nil, 96);

  // ACTION_BUTTON_HEIGHT=28 + ACTION_BUTTON_PADDING=8 * 2 = 44
  Assert.AreEqual(44, FDataSource.GetItemHeight(0),
    'At 96 DPI, action button height should be 44 (28 + 8*2)');
end;

procedure TDataSourceDpiTests.UpdateConfigs_At192DPI_ActionButtonHeightDoubles;
var
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Items, 1);
  Items[0].Source := dsAction;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  FDataSource.UpdateConfigs(FLayoutConfig, nil, 192);

  // MulDiv(28, 192, 96) + MulDiv(8, 192, 96)*2 = 56 + 16*2 = 88
  Assert.AreEqual(88, FDataSource.GetItemHeight(0),
    'At 192 DPI, action button height should double to 88');
end;

procedure TDataSourceDpiTests.UpdateConfigs_PpiChangeRecalculatesHeights;
var
  Items: TDeviceDisplayItemArray;
  HeightAt96, HeightAt192: Integer;
begin
  SetLength(Items, 1);
  Items[0].Source := dsPaired;
  Items[0].Device := Default(TBluetoothDeviceInfo);
  FDataSource.SetDisplayItems(Items);

  // First at 96 DPI
  FDataSource.UpdateConfigs(FLayoutConfig, nil, 96);
  HeightAt96 := FDataSource.GetItemHeight(0);

  // Then change to 192 DPI
  FDataSource.UpdateConfigs(FLayoutConfig, nil, 192);
  HeightAt192 := FDataSource.GetItemHeight(0);

  Assert.AreEqual(70, HeightAt96, 'Height at 96 DPI');
  Assert.AreEqual(140, HeightAt192, 'Height at 192 DPI');
  Assert.AreNotEqual(HeightAt96, HeightAt192,
    'Heights at different DPI should differ');
end;

{ TScrollbarDpiTests }

procedure TScrollbarDpiTests.Setup;
begin
  FScrollbar := TCustomScrollbar.Create;
end;

procedure TScrollbarDpiTests.TearDown;
begin
  FScrollbar.Free;
end;

procedure TScrollbarDpiTests.DefaultPPI_Is96;
begin
  // Default scrollbar at 96 DPI - verify geometry matches base constants
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(100);

  // Scrollbar should be at right edge, width = 12 (SCROLLBAR_WIDTH at 96 DPI)
  // We can't directly test GetScrollbarRect (private), but we can verify
  // HandleMouseDown detects clicks in the scrollbar area
  Assert.IsTrue(FScrollbar.HandleMouseDown(295, 200),
    'Click at x=295 should hit scrollbar (width=12, control width=300)');
end;

procedure TScrollbarDpiTests.UpdateDPI_At192_DoublesScrollbarWidth;
begin
  FScrollbar.UpdateDPI(192);
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(100);

  // At 192 DPI, scrollbar width = 24 (12 * 2)
  // Click at x=280 should be inside scrollbar (300-24=276 to 300)
  Assert.IsTrue(FScrollbar.HandleMouseDown(280, 200),
    'At 192 DPI, click at x=280 should hit scrollbar (width=24)');

  // Click at x=270 should be outside scrollbar
  FScrollbar.HandleMouseUp(280, 200);
  Assert.IsFalse(FScrollbar.HandleMouseDown(270, 200),
    'At 192 DPI, click at x=270 should miss scrollbar (starts at 276)');
end;

procedure TScrollbarDpiTests.UpdateDPI_At96_ScrollbarWidthIs12;
begin
  FScrollbar.UpdateDPI(96);
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(100);

  // At 96 DPI, scrollbar starts at 300-12=288
  Assert.IsTrue(FScrollbar.HandleMouseDown(290, 200),
    'At 96 DPI, click at x=290 should hit scrollbar');

  FScrollbar.HandleMouseUp(290, 200);
  Assert.IsFalse(FScrollbar.HandleMouseDown(285, 200),
    'At 96 DPI, click at x=285 should miss scrollbar (starts at 288)');
end;

procedure TScrollbarDpiTests.MouseWheel_At96DPI_ScrollsBy60Pixels;
begin
  FScrollbar.UpdateDPI(96);
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(500);
  FScrollbar.ScrollTo(250);

  // WHEEL_DELTA=120, scroll amount = MulDiv(120, 96, 96) div 2 = 60
  FScrollbar.HandleMouseWheel(120);

  // Scrolled up by 60: 250 - 60 = 190
  Assert.AreEqual(190, FScrollbar.ScrollPos,
    'At 96 DPI, wheel delta 120 should scroll 60px (250 -> 190)');
end;

procedure TScrollbarDpiTests.MouseWheel_At240DPI_ScalesScrollSpeed;
begin
  FScrollbar.UpdateDPI(240);
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(500);
  FScrollbar.ScrollTo(250);

  // WHEEL_DELTA=120, scroll amount = MulDiv(120, 240, 96) div 2 = 300 div 2 = 150
  FScrollbar.HandleMouseWheel(120);

  // Scrolled up by 150: 250 - 150 = 100
  Assert.AreEqual(100, FScrollbar.ScrollPos,
    'At 240 DPI, wheel delta 120 should scroll 150px (250 -> 100)');
end;

procedure TScrollbarDpiTests.MouseWheel_NoScrollableContent_ReturnsFalse;
begin
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(0);

  Assert.IsFalse(FScrollbar.HandleMouseWheel(120),
    'Wheel event should not be consumed when no scrollable content');
end;

procedure TScrollbarDpiTests.MouseWheel_WithScrollableContent_ReturnsTrue;
begin
  FScrollbar.UpdateClientSize(300, 400);
  FScrollbar.UpdateScrollRange(100);

  Assert.IsTrue(FScrollbar.HandleMouseWheel(120),
    'Wheel event should be consumed when scrollable content exists');
end;

{ TDpiScalingHelperTests }

procedure TDpiScalingHelperTests.RoundTrip_PreservesLogicalValue_AcrossDpiValues;
const
  PPIs: array[0..4] of Integer = (96, 120, 144, 168, 192);
  Logicals: array[0..5] of Integer = (280, 300, 320, 400, 800, 1200);
var
  P, L, RoundTripped: Integer;
begin
  for P in PPIs do
    for L in Logicals do
    begin
      RoundTripped := PhysicalToLogical(LogicalToPhysical(L, P), P);
      // Allow +/-1px rounding drift, but never growth.
      Assert.IsTrue(Abs(RoundTripped - L) <= 1,
        Format('Round-trip of logical %d at %d DPI returned %d (drift > 1px)', [L, P, RoundTripped]));
    end;
end;

procedure TDpiScalingHelperTests.CrossDpiMove_DoesNotCompound_OverManyIterations;
var
  Logical, Physical, I: Integer;
begin
  // Reproduce the exact bug loop: restore logical->physical on a 96-DPI startup monitor,
  // VCL scales to 150% on move, FormDestroy normalizes back to logical. The persisted
  // LOGICAL value must stay constant (no per-run multiplication).
  Logical := 320;
  for I := 1 to 8 do
  begin
    Physical := LogicalToPhysical(Logical, 96);        // restored at 96 DPI
    Physical := (Physical * 144 + 48) div 96;          // VCL ScaleForPPI -> 150%
    Logical := PhysicalToLogical(Physical, 144);       // FormDestroy normalizes back
    Assert.IsTrue(Abs(Logical - 320) <= 1,
      Format('Iteration %d: logical drifted to %d (runaway not prevented)', [I, Logical]));
  end;
end;

procedure TDpiScalingHelperTests.LogicalToPhysical_At96_IsIdentity;
begin
  Assert.AreEqual(320, LogicalToPhysical(320, 96));
  Assert.AreEqual(320, PhysicalToLogical(320, 96));
end;

procedure TDpiScalingHelperTests.LogicalToPhysical_At192_Doubles;
begin
  Assert.AreEqual(640, LogicalToPhysical(320, 192));
  Assert.AreEqual(320, PhysicalToLogical(640, 192));
end;

procedure TDpiScalingHelperTests.NonPositivePPI_TreatedAs96;
begin
  // Guard against div-by-zero / garbage CurrentPPI at startup.
  Assert.AreEqual(320, LogicalToPhysical(320, 0));
  Assert.AreEqual(320, PhysicalToLogical(320, 0));
  Assert.AreEqual(320, LogicalToPhysical(320, -100));
end;

initialization
  // This unit previously had NO registration, so its fixtures never ran. Register all of
  // them (existing + new) so the DPI tests are actually executed by the DUnitX runner.
  TDUnitX.RegisterTestFixture(TDataSourceDpiTests);
  TDUnitX.RegisterTestFixture(TScrollbarDpiTests);
  TDUnitX.RegisterTestFixture(TDpiScalingHelperTests);

end.
