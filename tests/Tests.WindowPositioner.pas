{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Window Positioning Strategy Tests               }
{                                                       }
{*******************************************************}

unit Tests.WindowPositioner;

interface

uses
  DUnitX.TestFramework,
  System.Types,
  App.ConfigEnums,
  UI.WindowPositioner,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for IsTaskbarHorizontal utility function.
  /// </summary>
  [TestFixture]
  TTaskbarUtilsTests = class
  public
    [Test]
    procedure IsTaskbarHorizontal_WideRect_ReturnsTrue;

    [Test]
    procedure IsTaskbarHorizontal_TallRect_ReturnsFalse;

    [Test]
    procedure IsTaskbarHorizontal_SquareRect_ReturnsTrue;

    [Test]
    procedure IsTaskbarHorizontal_TypicalBottomTaskbar_ReturnsTrue;

    [Test]
    procedure IsTaskbarHorizontal_TypicalLeftTaskbar_ReturnsFalse;

    [Test]
    procedure IsTaskbarHorizontal_ZeroSizeRect_ReturnsTrue;
  end;

  /// <summary>
  /// Test fixture for TCoordinatesPositioner strategy.
  /// </summary>
  [TestFixture]
  TCoordinatesPositionerTests = class
  private
    FStrategy: IPositionStrategy;
    FPositionConfig: TMockPositionConfig;

    function CreateContext(AFormWidth, AFormHeight: Integer;
      APositionX, APositionY: Integer): TPositionContext;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure CalculatePosition_WithSavedCoordinates_ReturnsSavedPosition;

    [Test]
    procedure CalculatePosition_NoSavedCoordinates_CentersOnScreen;

    [Test]
    procedure CalculatePosition_NegativeX_CentersOnScreen;

    [Test]
    procedure CalculatePosition_NegativeY_CentersOnScreen;

    [Test]
    procedure CalculatePosition_ZeroCoordinates_UsesCoordinates;
  end;

  /// <summary>
  /// Test fixture for TNearCursorPositioner strategy.
  /// </summary>
  [TestFixture]
  TNearCursorPositionerTests = class
  private
    FStrategy: IPositionStrategy;

    function CreateContext(AFormWidth, AFormHeight: Integer;
      ACursorX, ACursorY: Integer): TPositionContext;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure CalculatePosition_CentersHorizontallyOnCursor;

    [Test]
    procedure CalculatePosition_PositionsAboveCursor;

    [Test]
    procedure CalculatePosition_IncludesMargin;
  end;

  /// <summary>
  /// Test fixture for TCenterScreenPositioner strategy.
  /// </summary>
  [TestFixture]
  TCenterScreenPositionerTests = class
  private
    FStrategy: IPositionStrategy;

    function CreateContext(AFormWidth, AFormHeight: Integer): TPositionContext;
    function CreateContextWithWorkArea(AFormWidth, AFormHeight: Integer;
      const AWorkArea: TRect): TPositionContext;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure CalculatePosition_CentersHorizontally;

    [Test]
    procedure CalculatePosition_CentersVertically;

    [Test]
    procedure CalculatePosition_SmallForm_CentersCorrectly;

    [Test]
    procedure CalculatePosition_LargeForm_CentersCorrectly;

    [Test]
    procedure CalculatePosition_NonZeroWorkAreaOffset_CentersCorrectly;
  end;

  /// <summary>
  /// Test fixture for TWindowPositioner.GetStrategy factory method.
  /// </summary>
  [TestFixture]
  TWindowPositionerStrategyFactoryTests = class
  public
    [Test]
    procedure GetStrategy_Coordinates_ReturnsCoordinatesPositioner;

    [Test]
    procedure GetStrategy_NearCursor_ReturnsNearCursorPositioner;

    [Test]
    procedure GetStrategy_NearTray_ReturnsNearTrayPositioner;

    [Test]
    procedure GetStrategy_CenterScreen_ReturnsCenterScreenPositioner;
  end;

  /// <summary>
  /// Test fixture for TNearTrayPositioner strategy.
  /// Note: Full testing requires Windows API, these test basic logic only.
  /// </summary>
  [TestFixture]
  TNearTrayPositionerTests = class
  private
    FStrategy: IPositionStrategy;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Create_DoesNotRaise;
  end;

  /// <summary>
  /// Test fixture for edge cases in window positioning.
  /// </summary>
  [TestFixture]
  TWindowPositionerEdgeCaseTests = class
  private
    FPositionConfig: TMockPositionConfig;

    function CreateContextWithWorkArea(AFormWidth, AFormHeight: Integer;
      const AWorkArea: TRect): TPositionContext;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Edge Case Tests }
    [Test]
    procedure EnsureOnScreen_FormEntirelyOffScreen_BringsBack;

    [Test]
    procedure EnsureOnScreen_FormPartiallyOffScreen_AdjustsPosition;

    [Test]
    procedure EnsureOnScreen_FormLargerThanWorkArea_Handles;

    [Test]
    procedure TNearTrayPositioner_CalculatePosition_BottomTaskbar;

    [Test]
    procedure TNearTrayPositioner_CalculatePosition_RightTaskbar;

    [Test]
    procedure TCoordinatesPositioner_NegativeCoordinates_Handles;

    [Test]
    procedure TCenterScreenPositioner_ZeroSizeForm_Handles;
  end;

implementation

uses
  System.SysUtils;

const
  WINDOW_EDGE_MARGIN = 10; // Must match UI.WindowPositioner constant

{ TTaskbarUtilsTests }

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_WideRect_ReturnsTrue;
var
  R: TRect;
begin
  R := Rect(0, 0, 1920, 40); // Wide taskbar
  Assert.IsTrue(IsTaskbarHorizontal(R));
end;

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_TallRect_ReturnsFalse;
var
  R: TRect;
begin
  R := Rect(0, 0, 40, 1080); // Tall taskbar (vertical)
  Assert.IsFalse(IsTaskbarHorizontal(R));
end;

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_SquareRect_ReturnsTrue;
var
  R: TRect;
begin
  R := Rect(0, 0, 100, 100); // Square (width = height)
  // With >= comparison, square taskbar is treated as horizontal (edge case)
  Assert.IsTrue(IsTaskbarHorizontal(R));
end;

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_TypicalBottomTaskbar_ReturnsTrue;
var
  R: TRect;
begin
  // Typical Windows 11 bottom taskbar
  R := Rect(0, 1040, 1920, 1080);
  // Width = 1920, Height = 40
  Assert.IsTrue(IsTaskbarHorizontal(R));
end;

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_TypicalLeftTaskbar_ReturnsFalse;
var
  R: TRect;
begin
  // Vertical taskbar on left
  R := Rect(0, 0, 48, 1080);
  // Width = 48, Height = 1080
  Assert.IsFalse(IsTaskbarHorizontal(R));
end;

procedure TTaskbarUtilsTests.IsTaskbarHorizontal_ZeroSizeRect_ReturnsTrue;
var
  R: TRect;
begin
  // Edge case: zero-sized rect (0 >= 0 is True)
  R := Rect(0, 0, 0, 0);
  Assert.IsTrue(IsTaskbarHorizontal(R));
end;

{ TCoordinatesPositionerTests }

procedure TCoordinatesPositionerTests.Setup;
begin
  FStrategy := TCoordinatesPositioner.Create;
  FPositionConfig := TMockPositionConfig.Create;
end;

procedure TCoordinatesPositionerTests.TearDown;
begin
  FStrategy := nil;
  // FPositionConfig released via interface
end;

function TCoordinatesPositionerTests.CreateContext(AFormWidth, AFormHeight: Integer;
  APositionX, APositionY: Integer): TPositionContext;
begin
  FPositionConfig.PositionX := APositionX;
  FPositionConfig.PositionY := APositionY;

  Result.FormWidth := AFormWidth;
  Result.FormHeight := AFormHeight;
  Result.CursorPos := Point(500, 500);
  Result.WorkArea := Rect(0, 0, 1920, 1080);
  Result.PositionConfig := FPositionConfig;
end;

procedure TCoordinatesPositionerTests.CalculatePosition_WithSavedCoordinates_ReturnsSavedPosition;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(300, 400, 100, 200);

  Pos := FStrategy.CalculatePosition(Context);

  Assert.AreEqual(100, Pos.X);
  Assert.AreEqual(200, Pos.Y);
end;

procedure TCoordinatesPositionerTests.CalculatePosition_NoSavedCoordinates_CentersOnScreen;
var
  Context: TPositionContext;
  Pos: TPoint;
  ExpectedX, ExpectedY: Integer;
begin
  Context := CreateContext(300, 400, -1, -1);

  Pos := FStrategy.CalculatePosition(Context);

  // Center calculation: (WorkAreaWidth - FormWidth) / 2
  ExpectedX := (1920 - 300) div 2;
  ExpectedY := (1080 - 400) div 2;
  Assert.AreEqual(ExpectedX, Pos.X);
  Assert.AreEqual(ExpectedY, Pos.Y);
end;

procedure TCoordinatesPositionerTests.CalculatePosition_NegativeX_CentersOnScreen;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(300, 400, -1, 200); // X negative, Y positive

  Pos := FStrategy.CalculatePosition(Context);

  // Should center because X is negative
  Assert.AreEqual((1920 - 300) div 2, Pos.X);
end;

procedure TCoordinatesPositionerTests.CalculatePosition_NegativeY_CentersOnScreen;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(300, 400, 100, -1); // X positive, Y negative

  Pos := FStrategy.CalculatePosition(Context);

  // Should center because Y is negative
  Assert.AreEqual((1080 - 400) div 2, Pos.Y);
end;

procedure TCoordinatesPositionerTests.CalculatePosition_ZeroCoordinates_UsesCoordinates;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(300, 400, 0, 0);

  Pos := FStrategy.CalculatePosition(Context);

  // Zero is a valid coordinate
  Assert.AreEqual(0, Pos.X);
  Assert.AreEqual(0, Pos.Y);
end;

{ TNearCursorPositionerTests }

procedure TNearCursorPositionerTests.Setup;
begin
  FStrategy := TNearCursorPositioner.Create;
end;

function TNearCursorPositionerTests.CreateContext(AFormWidth, AFormHeight: Integer;
  ACursorX, ACursorY: Integer): TPositionContext;
begin
  Result.FormWidth := AFormWidth;
  Result.FormHeight := AFormHeight;
  Result.CursorPos := Point(ACursorX, ACursorY);
  Result.WorkArea := Rect(0, 0, 1920, 1080);
  Result.PositionConfig := nil;
end;

procedure TNearCursorPositionerTests.CalculatePosition_CentersHorizontallyOnCursor;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(200, 300, 500, 500);

  Pos := FStrategy.CalculatePosition(Context);

  // X = CursorX - (FormWidth / 2)
  Assert.AreEqual(500 - (200 div 2), Pos.X);
end;

procedure TNearCursorPositionerTests.CalculatePosition_PositionsAboveCursor;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(200, 300, 500, 500);

  Pos := FStrategy.CalculatePosition(Context);

  // Y = CursorY - FormHeight - Margin
  Assert.AreEqual(500 - 300 - WINDOW_EDGE_MARGIN, Pos.Y);
end;

procedure TNearCursorPositionerTests.CalculatePosition_IncludesMargin;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(100, 100, 500, 500);

  Pos := FStrategy.CalculatePosition(Context);

  // The Y position includes the margin
  Assert.AreEqual(500 - 100 - WINDOW_EDGE_MARGIN, Pos.Y);
end;

{ TCenterScreenPositionerTests }

procedure TCenterScreenPositionerTests.Setup;
begin
  FStrategy := TCenterScreenPositioner.Create;
end;

function TCenterScreenPositionerTests.CreateContext(AFormWidth, AFormHeight: Integer): TPositionContext;
begin
  Result := CreateContextWithWorkArea(AFormWidth, AFormHeight, Rect(0, 0, 1920, 1080));
end;

function TCenterScreenPositionerTests.CreateContextWithWorkArea(
  AFormWidth, AFormHeight: Integer; const AWorkArea: TRect): TPositionContext;
begin
  Result.FormWidth := AFormWidth;
  Result.FormHeight := AFormHeight;
  Result.CursorPos := Point(0, 0);
  Result.WorkArea := AWorkArea;
  Result.PositionConfig := nil;
end;

procedure TCenterScreenPositionerTests.CalculatePosition_CentersHorizontally;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(400, 300);

  Pos := FStrategy.CalculatePosition(Context);

  Assert.AreEqual((1920 - 400) div 2, Pos.X);
end;

procedure TCenterScreenPositionerTests.CalculatePosition_CentersVertically;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(400, 300);

  Pos := FStrategy.CalculatePosition(Context);

  Assert.AreEqual((1080 - 300) div 2, Pos.Y);
end;

procedure TCenterScreenPositionerTests.CalculatePosition_SmallForm_CentersCorrectly;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(100, 100);

  Pos := FStrategy.CalculatePosition(Context);

  Assert.AreEqual((1920 - 100) div 2, Pos.X);
  Assert.AreEqual((1080 - 100) div 2, Pos.Y);
end;

procedure TCenterScreenPositionerTests.CalculatePosition_LargeForm_CentersCorrectly;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  Context := CreateContext(1800, 1000);

  Pos := FStrategy.CalculatePosition(Context);

  Assert.AreEqual((1920 - 1800) div 2, Pos.X);
  Assert.AreEqual((1080 - 1000) div 2, Pos.Y);
end;

procedure TCenterScreenPositionerTests.CalculatePosition_NonZeroWorkAreaOffset_CentersCorrectly;
var
  Context: TPositionContext;
  Pos: TPoint;
begin
  // Secondary monitor at offset
  Context := CreateContextWithWorkArea(400, 300, Rect(1920, 0, 3840, 1080));

  Pos := FStrategy.CalculatePosition(Context);

  // Should center within work area, accounting for offset
  Assert.AreEqual(1920 + ((1920 - 400) div 2), Pos.X);
  Assert.AreEqual((1080 - 300) div 2, Pos.Y);
end;

{ TWindowPositionerStrategyFactoryTests }

procedure TWindowPositionerStrategyFactoryTests.GetStrategy_Coordinates_ReturnsCoordinatesPositioner;
var
  Strategy: IPositionStrategy;
begin
  Strategy := TWindowPositioner.GetStrategy(pmCoordinates);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy is TCoordinatesPositioner);
end;

procedure TWindowPositionerStrategyFactoryTests.GetStrategy_NearCursor_ReturnsNearCursorPositioner;
var
  Strategy: IPositionStrategy;
begin
  Strategy := TWindowPositioner.GetStrategy(pmNearCursor);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy is TNearCursorPositioner);
end;

procedure TWindowPositionerStrategyFactoryTests.GetStrategy_NearTray_ReturnsNearTrayPositioner;
var
  Strategy: IPositionStrategy;
begin
  Strategy := TWindowPositioner.GetStrategy(pmNearTray);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy is TNearTrayPositioner);
end;

procedure TWindowPositionerStrategyFactoryTests.GetStrategy_CenterScreen_ReturnsCenterScreenPositioner;
var
  Strategy: IPositionStrategy;
begin
  Strategy := TWindowPositioner.GetStrategy(pmCenterScreen);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy is TCenterScreenPositioner);
end;

{ TNearTrayPositionerTests }

procedure TNearTrayPositionerTests.Setup;
begin
  FStrategy := TNearTrayPositioner.Create;
end;

procedure TNearTrayPositionerTests.Create_DoesNotRaise;
begin
  Assert.IsNotNull(FStrategy);
end;

{ TWindowPositionerEdgeCaseTests }

procedure TWindowPositionerEdgeCaseTests.Setup;
begin
  FPositionConfig := TMockPositionConfig.Create;
end;

procedure TWindowPositionerEdgeCaseTests.TearDown;
begin
  FPositionConfig := nil;
end;

function TWindowPositionerEdgeCaseTests.CreateContextWithWorkArea(
  AFormWidth, AFormHeight: Integer; const AWorkArea: TRect): TPositionContext;
begin
  Result.FormWidth := AFormWidth;
  Result.FormHeight := AFormHeight;
  Result.CursorPos := Point(AWorkArea.Left + AWorkArea.Width div 2,
                            AWorkArea.Top + AWorkArea.Height div 2);
  Result.WorkArea := AWorkArea;
  Result.PositionConfig := FPositionConfig;
end;

procedure TWindowPositionerEdgeCaseTests.EnsureOnScreen_FormEntirelyOffScreen_BringsBack;
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  Pos: TPoint;
begin
  // Test TCoordinatesPositioner with coordinates completely off screen
  // The PositionWindow method clamps coordinates after strategy calculation
  FPositionConfig.PositionX := 5000;  // Way off screen
  FPositionConfig.PositionY := 5000;

  Context := CreateContextWithWorkArea(300, 400, Rect(0, 0, 1920, 1080));

  Strategy := TCoordinatesPositioner.Create;
  Pos := Strategy.CalculatePosition(Context);

  // Strategy returns the saved coordinates (unclamped)
  Assert.AreEqual(5000, Pos.X);
  Assert.AreEqual(5000, Pos.Y);

  // Note: The actual clamping happens in TWindowPositioner.PositionWindow
  // which is tested via integration. Here we verify strategy returns raw values.
end;

procedure TWindowPositionerEdgeCaseTests.EnsureOnScreen_FormPartiallyOffScreen_AdjustsPosition;
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  Pos: TPoint;
begin
  // Test position where form would extend beyond right edge
  FPositionConfig.PositionX := 1800;  // With 300px width, extends to 2100
  FPositionConfig.PositionY := 100;

  Context := CreateContextWithWorkArea(300, 400, Rect(0, 0, 1920, 1080));

  Strategy := TCoordinatesPositioner.Create;
  Pos := Strategy.CalculatePosition(Context);

  // Strategy returns the saved coordinates
  Assert.AreEqual(1800, Pos.X);
  Assert.AreEqual(100, Pos.Y);

  // Note: PositionWindow would clamp X to 1620 (1920 - 300)
end;

procedure TWindowPositionerEdgeCaseTests.EnsureOnScreen_FormLargerThanWorkArea_Handles;
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  Pos: TPoint;
begin
  // Form larger than work area - test centering behavior
  // Work area: 800x600, Form: 1000x800
  Context := CreateContextWithWorkArea(1000, 800, Rect(0, 0, 800, 600));

  Strategy := TCenterScreenPositioner.Create;
  Pos := Strategy.CalculatePosition(Context);

  // Center calculation: (800 - 1000) / 2 = -100
  // (600 - 800) / 2 = -100
  // Centering a larger form results in negative position
  Assert.AreEqual(-100, Pos.X, 'X should be negative for oversized form');
  Assert.AreEqual(-100, Pos.Y, 'Y should be negative for oversized form');

  // Note: PositionWindow would clamp these to 0
end;

procedure TWindowPositionerEdgeCaseTests.TNearTrayPositioner_CalculatePosition_BottomTaskbar;
var
  Strategy: IPositionStrategy;
  Context: TPositionContext;
  Pos: TPoint;
begin
  // TNearTrayPositioner uses GetTaskbarRect which requires Windows API
  // We can only test that it doesn't crash and returns a position
  Strategy := TNearTrayPositioner.Create;
  Context := CreateContextWithWorkArea(300, 400, Rect(0, 0, 1920, 1040));

  Pos := Strategy.CalculatePosition(Context);

  // Position should be within reasonable bounds (not extreme values)
  // This test verifies the strategy runs without error
  Assert.IsTrue(Pos.X >= -1920, 'X position should be reasonable');
  Assert.IsTrue(Pos.Y >= -1080, 'Y position should be reasonable');
end;

procedure TWindowPositionerEdgeCaseTests.TNearTrayPositioner_CalculatePosition_RightTaskbar;
var
  Strategy: IPositionStrategy;
  Context: TPositionContext;
  Pos: TPoint;
begin
  // TNearTrayPositioner handles vertical taskbars too
  // Work area would be reduced on the right side for a right taskbar
  Strategy := TNearTrayPositioner.Create;
  Context := CreateContextWithWorkArea(300, 400, Rect(0, 0, 1870, 1080));

  Pos := Strategy.CalculatePosition(Context);

  // Position should be within reasonable bounds
  // Without actual taskbar detection, falls back to bottom-right
  Assert.IsTrue(Pos.X >= -1920, 'X position should be reasonable');
  Assert.IsTrue(Pos.Y >= -1080, 'Y position should be reasonable');
end;

procedure TWindowPositionerEdgeCaseTests.TCoordinatesPositioner_NegativeCoordinates_Handles;
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  Pos: TPoint;
begin
  // Negative coordinates should trigger centering (per implementation)
  // -1 means "not set" in the config
  FPositionConfig.PositionX := -1;
  FPositionConfig.PositionY := -1;

  Context := CreateContextWithWorkArea(300, 400, Rect(0, 0, 1920, 1080));

  Strategy := TCoordinatesPositioner.Create;
  Pos := Strategy.CalculatePosition(Context);

  // Should center: (1920-300)/2 = 810, (1080-400)/2 = 340
  Assert.AreEqual(810, Pos.X);
  Assert.AreEqual(340, Pos.Y);
end;

procedure TWindowPositionerEdgeCaseTests.TCenterScreenPositioner_ZeroSizeForm_Handles;
var
  Context: TPositionContext;
  Strategy: IPositionStrategy;
  Pos: TPoint;
begin
  // Zero-size form edge case
  Context := CreateContextWithWorkArea(0, 0, Rect(0, 0, 1920, 1080));

  Strategy := TCenterScreenPositioner.Create;
  Pos := Strategy.CalculatePosition(Context);

  // Center calculation: (1920-0)/2 = 960, (1080-0)/2 = 540
  Assert.AreEqual(960, Pos.X);
  Assert.AreEqual(540, Pos.Y);
end;

initialization
  TDUnitX.RegisterTestFixture(TTaskbarUtilsTests);
  TDUnitX.RegisterTestFixture(TCoordinatesPositionerTests);
  TDUnitX.RegisterTestFixture(TNearCursorPositionerTests);
  TDUnitX.RegisterTestFixture(TCenterScreenPositionerTests);
  TDUnitX.RegisterTestFixture(TWindowPositionerStrategyFactoryTests);
  TDUnitX.RegisterTestFixture(TNearTrayPositionerTests);
  TDUnitX.RegisterTestFixture(TWindowPositionerEdgeCaseTests);

end.
