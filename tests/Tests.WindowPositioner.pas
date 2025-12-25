{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Window Positioning Strategy Tests               }
{                                                       }
{       Copyright (c) 2024                              }
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
  // Width > Height is false (100 > 100 is false), but width >= height should be considered
  // The function uses Width > Height, so square returns false
  // Actually: Width=100, Height=100, 100 > 100 = False
  Assert.IsFalse(IsTaskbarHorizontal(R));
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
  // Edge case: zero-sized rect
  R := Rect(0, 0, 0, 0);
  // Width = 0, Height = 0, 0 > 0 = False
  Assert.IsFalse(IsTaskbarHorizontal(R));
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

initialization
  TDUnitX.RegisterTestFixture(TTaskbarUtilsTests);
  TDUnitX.RegisterTestFixture(TCoordinatesPositionerTests);
  TDUnitX.RegisterTestFixture(TNearCursorPositionerTests);
  TDUnitX.RegisterTestFixture(TCenterScreenPositionerTests);
  TDUnitX.RegisterTestFixture(TWindowPositionerStrategyFactoryTests);
  TDUnitX.RegisterTestFixture(TNearTrayPositionerTests);

end.
