{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TListGeometry Unit Tests                        }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.ListGeometry;

interface

uses
  DUnitX.TestFramework,
  System.Types,
  UI.ListGeometry;

type
  /// <summary>
  /// Test fixture for TListGeometry class.
  /// Tests list item positioning, scroll calculations, and hit testing.
  /// </summary>
  [TestFixture]
  TListGeometryTests = class
  private
    const
      // Standard test values matching typical UI configuration
      ITEM_HEIGHT = 70;
      ITEM_MARGIN = 4;
      CLIENT_WIDTH = 300;
      CLIENT_HEIGHT = 400;
  public
    { GetItemRect Tests }
    [Test]
    procedure GetItemRect_FirstItem_ReturnsCorrectBounds;
    [Test]
    procedure GetItemRect_SecondItem_ReturnsCorrectBounds;
    [Test]
    procedure GetItemRect_WithScroll_AdjustsTop;
    [Test]
    procedure GetItemRect_RespectsMargins;

    { ItemAtPos Tests }
    [Test]
    procedure ItemAtPos_EmptyList_ReturnsNegativeOne;
    [Test]
    procedure ItemAtPos_ClickOnFirstItem_ReturnsZero;
    [Test]
    procedure ItemAtPos_ClickOnSecondItem_ReturnsOne;
    [Test]
    procedure ItemAtPos_ClickBetweenItems_ReturnsNegativeOne;
    [Test]
    procedure ItemAtPos_ClickOutsideMargins_ReturnsNegativeOne;
    [Test]
    procedure ItemAtPos_ClickBelowItems_ReturnsNegativeOne;
    [Test]
    procedure ItemAtPos_WithScroll_FindsCorrectItem;

    { CalculateTotalHeight Tests }
    [Test]
    procedure CalculateTotalHeight_NoItems_ReturnsMargin;
    [Test]
    procedure CalculateTotalHeight_OneItem_ReturnsCorrectHeight;
    [Test]
    procedure CalculateTotalHeight_MultipleItems_ReturnsCorrectHeight;

    { CalculateMaxScroll Tests }
    [Test]
    procedure CalculateMaxScroll_ContentFits_ReturnsZero;
    [Test]
    procedure CalculateMaxScroll_ContentExceeds_ReturnsPositive;
    [Test]
    procedure CalculateMaxScroll_EmptyList_ReturnsZero;

    { ClampScrollPos Tests }
    [Test]
    procedure ClampScrollPos_NegativeValue_ReturnsZero;
    [Test]
    procedure ClampScrollPos_ExceedsMax_ReturnsMax;
    [Test]
    procedure ClampScrollPos_ValidValue_ReturnsSame;

    { ScrollPosToMakeVisible Tests }
    [Test]
    procedure ScrollPosToMakeVisible_ItemAlreadyVisible_NoChange;
    [Test]
    procedure ScrollPosToMakeVisible_ItemAbove_ScrollsUp;
    [Test]
    procedure ScrollPosToMakeVisible_ItemBelow_ScrollsDown;

    { IsItemVisible Tests }
    [Test]
    procedure IsItemVisible_FullyVisible_ReturnsTrue;
    [Test]
    procedure IsItemVisible_PartiallyAbove_ReturnsTrue;
    [Test]
    procedure IsItemVisible_PartiallyBelow_ReturnsTrue;
    [Test]
    procedure IsItemVisible_CompletelyAbove_ReturnsFalse;
    [Test]
    procedure IsItemVisible_CompletelyBelow_ReturnsFalse;

    { Edge Case Tests - Negative Indices }
    [Test]
    procedure GetItemRect_NegativeIndex_ReturnsNegativeTop;
    [Test]
    procedure GetItemTop_NegativeIndex_ReturnsNegativeValue;

    { Edge Case Tests - Zero Dimensions }
    [Test]
    procedure GetItemRect_ZeroItemHeight_ReturnsZeroHeightRect;
    [Test]
    procedure GetItemRect_ZeroMargin_NoSpacing;
    [Test]
    procedure CalculateTotalHeight_ZeroItemHeight_ReturnsMarginOnly;
    [Test]
    procedure CalculateMaxScroll_ZeroClientHeight_ReturnsFullHeight;
    [Test]
    procedure ItemAtPos_ZeroClientWidth_NoHits;

    { Edge Case Tests - Boundary Clicks }
    [Test]
    procedure ItemAtPos_ClickExactlyOnTopEdge_ReturnsItem;
    [Test]
    procedure ItemAtPos_ClickExactlyOnBottomEdge_ReturnsNegativeOne;
    [Test]
    procedure ItemAtPos_ClickExactlyOnLeftEdge_ReturnsItem;
    [Test]
    procedure ItemAtPos_ClickExactlyOnRightEdge_ReturnsNegativeOne;

    { Edge Case Tests - Large Values }
    [Test]
    procedure GetItemRect_LargeIndex_CalculatesCorrectly;
    [Test]
    procedure ClampScrollPos_ZeroMaxScroll_AlwaysReturnsZero;
    [Test]
    procedure CalculateTotalHeight_NegativeItemCount_ReturnsMargin;

    { Edge Case Tests - Single Item }
    [Test]
    procedure ScrollPosToMakeVisible_SingleItemList_Works;
    [Test]
    procedure ItemAtPos_SingleItem_ClickAbove_ReturnsNegativeOne;
  end;

implementation

{ TListGeometryTests - GetItemRect }

procedure TListGeometryTests.GetItemRect_FirstItem_ReturnsCorrectBounds;
var
  R: TRect;
begin
  R := TListGeometry.GetItemRect(0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);

  Assert.AreEqual(ITEM_MARGIN, R.Left);
  Assert.AreEqual(CLIENT_WIDTH - ITEM_MARGIN, R.Right);
  Assert.AreEqual(ITEM_MARGIN, R.Top);
  Assert.AreEqual(ITEM_MARGIN + ITEM_HEIGHT, R.Bottom);
end;

procedure TListGeometryTests.GetItemRect_SecondItem_ReturnsCorrectBounds;
var
  R: TRect;
  ExpectedTop: Integer;
begin
  R := TListGeometry.GetItemRect(1, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);

  ExpectedTop := ITEM_MARGIN + (ITEM_HEIGHT + ITEM_MARGIN);
  Assert.AreEqual(ExpectedTop, R.Top);
  Assert.AreEqual(ExpectedTop + ITEM_HEIGHT, R.Bottom);
end;

procedure TListGeometryTests.GetItemRect_WithScroll_AdjustsTop;
var
  R: TRect;
  ScrollPos: Integer;
begin
  ScrollPos := 50;
  R := TListGeometry.GetItemRect(0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, ScrollPos);

  Assert.AreEqual(ITEM_MARGIN - ScrollPos, R.Top);
end;

procedure TListGeometryTests.GetItemRect_RespectsMargins;
var
  R: TRect;
  CustomMargin: Integer;
begin
  CustomMargin := 10;
  R := TListGeometry.GetItemRect(0, ITEM_HEIGHT, CustomMargin, CLIENT_WIDTH, 0);

  Assert.AreEqual(CustomMargin, R.Left);
  Assert.AreEqual(CLIENT_WIDTH - CustomMargin, R.Right);
  Assert.AreEqual(CustomMargin, R.Top);
end;

{ TListGeometryTests - ItemAtPos }

procedure TListGeometryTests.ItemAtPos_EmptyList_ReturnsNegativeOne;
var
  Index: Integer;
begin
  Index := TListGeometry.ItemAtPos(100, 50, 0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index);
end;

procedure TListGeometryTests.ItemAtPos_ClickOnFirstItem_ReturnsZero;
var
  Index: Integer;
  ClickY: Integer;
begin
  // Click in the middle of the first item
  ClickY := ITEM_MARGIN + (ITEM_HEIGHT div 2);
  Index := TListGeometry.ItemAtPos(100, ClickY, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(0, Index);
end;

procedure TListGeometryTests.ItemAtPos_ClickOnSecondItem_ReturnsOne;
var
  Index: Integer;
  ClickY: Integer;
begin
  // Click in the middle of the second item
  ClickY := ITEM_MARGIN + ITEM_HEIGHT + ITEM_MARGIN + (ITEM_HEIGHT div 2);
  Index := TListGeometry.ItemAtPos(100, ClickY, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(1, Index);
end;

procedure TListGeometryTests.ItemAtPos_ClickBetweenItems_ReturnsNegativeOne;
var
  Index: Integer;
  ClickY: Integer;
begin
  // Click in the margin between first and second item
  ClickY := ITEM_MARGIN + ITEM_HEIGHT + (ITEM_MARGIN div 2);
  Index := TListGeometry.ItemAtPos(100, ClickY, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index);
end;

procedure TListGeometryTests.ItemAtPos_ClickOutsideMargins_ReturnsNegativeOne;
var
  Index: Integer;
begin
  // Click in the left margin (X = 0, which is < ITEM_MARGIN)
  Index := TListGeometry.ItemAtPos(0, ITEM_MARGIN + 10, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index);
end;

procedure TListGeometryTests.ItemAtPos_ClickBelowItems_ReturnsNegativeOne;
var
  Index: Integer;
  ClickY: Integer;
begin
  // Click below all items (only 2 items)
  ClickY := 500;
  Index := TListGeometry.ItemAtPos(100, ClickY, 2, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index);
end;

procedure TListGeometryTests.ItemAtPos_WithScroll_FindsCorrectItem;
var
  Index: Integer;
  ScrollPos, ClickY: Integer;
begin
  // Scroll down 100 pixels, click where second item should now be
  ScrollPos := 100;
  ClickY := ITEM_MARGIN + ITEM_HEIGHT + ITEM_MARGIN + (ITEM_HEIGHT div 2) - ScrollPos;
  Index := TListGeometry.ItemAtPos(100, ClickY, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, ScrollPos);
  Assert.AreEqual(1, Index);
end;

{ TListGeometryTests - CalculateTotalHeight }

procedure TListGeometryTests.CalculateTotalHeight_NoItems_ReturnsMargin;
var
  Height: Integer;
begin
  Height := TListGeometry.CalculateTotalHeight(0, ITEM_HEIGHT, ITEM_MARGIN);
  Assert.AreEqual(ITEM_MARGIN, Height);
end;

procedure TListGeometryTests.CalculateTotalHeight_OneItem_ReturnsCorrectHeight;
var
  Height, Expected: Integer;
begin
  // margin + item + margin
  Expected := ITEM_MARGIN + ITEM_HEIGHT + ITEM_MARGIN;
  Height := TListGeometry.CalculateTotalHeight(1, ITEM_HEIGHT, ITEM_MARGIN);
  Assert.AreEqual(Expected, Height);
end;

procedure TListGeometryTests.CalculateTotalHeight_MultipleItems_ReturnsCorrectHeight;
var
  Height, Expected: Integer;
begin
  // 3 items: margin + (item+margin)*3 = margin + 3*item + 3*margin = 4*margin + 3*item
  // Actually: margin + item + margin + item + margin + item + margin
  // = ITEM_COUNT * (ITEM_HEIGHT + ITEM_MARGIN) + ITEM_MARGIN
  Expected := 3 * (ITEM_HEIGHT + ITEM_MARGIN) + ITEM_MARGIN;
  Height := TListGeometry.CalculateTotalHeight(3, ITEM_HEIGHT, ITEM_MARGIN);
  Assert.AreEqual(Expected, Height);
end;

{ TListGeometryTests - CalculateMaxScroll }

procedure TListGeometryTests.CalculateMaxScroll_ContentFits_ReturnsZero;
var
  MaxScroll: Integer;
begin
  // 2 items, tall client area that fits everything
  MaxScroll := TListGeometry.CalculateMaxScroll(2, ITEM_HEIGHT, ITEM_MARGIN, 1000);
  Assert.AreEqual(0, MaxScroll);
end;

procedure TListGeometryTests.CalculateMaxScroll_ContentExceeds_ReturnsPositive;
var
  MaxScroll, TotalHeight: Integer;
begin
  // 10 items with a smaller client height
  TotalHeight := TListGeometry.CalculateTotalHeight(10, ITEM_HEIGHT, ITEM_MARGIN);
  MaxScroll := TListGeometry.CalculateMaxScroll(10, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);

  Assert.AreEqual(TotalHeight - CLIENT_HEIGHT, MaxScroll);
  Assert.IsTrue(MaxScroll > 0, 'MaxScroll should be positive');
end;

procedure TListGeometryTests.CalculateMaxScroll_EmptyList_ReturnsZero;
var
  MaxScroll: Integer;
begin
  MaxScroll := TListGeometry.CalculateMaxScroll(0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);
  Assert.AreEqual(0, MaxScroll);
end;

{ TListGeometryTests - ClampScrollPos }

procedure TListGeometryTests.ClampScrollPos_NegativeValue_ReturnsZero;
var
  Result: Integer;
begin
  Result := TListGeometry.ClampScrollPos(-50, 100);
  Assert.AreEqual(0, Result);
end;

procedure TListGeometryTests.ClampScrollPos_ExceedsMax_ReturnsMax;
var
  Result: Integer;
begin
  Result := TListGeometry.ClampScrollPos(150, 100);
  Assert.AreEqual(100, Result);
end;

procedure TListGeometryTests.ClampScrollPos_ValidValue_ReturnsSame;
var
  Result: Integer;
begin
  Result := TListGeometry.ClampScrollPos(50, 100);
  Assert.AreEqual(50, Result);
end;

{ TListGeometryTests - ScrollPosToMakeVisible }

procedure TListGeometryTests.ScrollPosToMakeVisible_ItemAlreadyVisible_NoChange;
var
  NewScroll: Integer;
begin
  // First item is visible with scroll at 0
  NewScroll := TListGeometry.ScrollPosToMakeVisible(0, 0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);
  Assert.AreEqual(0, NewScroll);
end;

procedure TListGeometryTests.ScrollPosToMakeVisible_ItemAbove_ScrollsUp;
var
  NewScroll, CurrentScroll: Integer;
begin
  // Scrolled down, first item is above viewport
  CurrentScroll := 200;
  NewScroll := TListGeometry.ScrollPosToMakeVisible(0, CurrentScroll, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);

  Assert.IsTrue(NewScroll < CurrentScroll, 'Should scroll up (decrease scroll)');
  Assert.AreEqual(0, NewScroll, 'Should scroll to show first item');
end;

procedure TListGeometryTests.ScrollPosToMakeVisible_ItemBelow_ScrollsDown;
var
  NewScroll, ItemBottom: Integer;
begin
  // Item 8 is below the viewport when scroll is at 0
  ItemBottom := TListGeometry.GetItemTop(8, ITEM_HEIGHT, ITEM_MARGIN) + ITEM_HEIGHT;
  NewScroll := TListGeometry.ScrollPosToMakeVisible(8, 0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);

  Assert.IsTrue(NewScroll > 0, 'Should scroll down (increase scroll)');
  // Item bottom should now be at or near client height
  Assert.AreEqual(ItemBottom - CLIENT_HEIGHT + ITEM_MARGIN, NewScroll);
end;

{ TListGeometryTests - IsItemVisible }

procedure TListGeometryTests.IsItemVisible_FullyVisible_ReturnsTrue;
var
  R: TRect;
begin
  R := Rect(10, 50, 100, 120);
  Assert.IsTrue(TListGeometry.IsItemVisible(R, CLIENT_HEIGHT));
end;

procedure TListGeometryTests.IsItemVisible_PartiallyAbove_ReturnsTrue;
var
  R: TRect;
begin
  R := Rect(10, -30, 100, 20);  // Top is above, but bottom is visible
  Assert.IsTrue(TListGeometry.IsItemVisible(R, CLIENT_HEIGHT));
end;

procedure TListGeometryTests.IsItemVisible_PartiallyBelow_ReturnsTrue;
var
  R: TRect;
begin
  R := Rect(10, CLIENT_HEIGHT - 30, 100, CLIENT_HEIGHT + 20);
  Assert.IsTrue(TListGeometry.IsItemVisible(R, CLIENT_HEIGHT));
end;

procedure TListGeometryTests.IsItemVisible_CompletelyAbove_ReturnsFalse;
var
  R: TRect;
begin
  R := Rect(10, -100, 100, -10);  // Completely above viewport
  Assert.IsFalse(TListGeometry.IsItemVisible(R, CLIENT_HEIGHT));
end;

procedure TListGeometryTests.IsItemVisible_CompletelyBelow_ReturnsFalse;
var
  R: TRect;
begin
  R := Rect(10, CLIENT_HEIGHT + 10, 100, CLIENT_HEIGHT + 100);
  Assert.IsFalse(TListGeometry.IsItemVisible(R, CLIENT_HEIGHT));
end;

{ TListGeometryTests - Edge Cases: Negative Indices }

procedure TListGeometryTests.GetItemRect_NegativeIndex_ReturnsNegativeTop;
var
  R: TRect;
begin
  // Negative index should calculate negative top position
  R := TListGeometry.GetItemRect(-1, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);

  // Top = ITEM_MARGIN + (-1) * (ITEM_HEIGHT + ITEM_MARGIN) = ITEM_MARGIN - ITEM_HEIGHT - ITEM_MARGIN = -ITEM_HEIGHT
  Assert.AreEqual(-ITEM_HEIGHT, R.Top);
  Assert.AreEqual(0, R.Bottom); // -ITEM_HEIGHT + ITEM_HEIGHT = 0
end;

procedure TListGeometryTests.GetItemTop_NegativeIndex_ReturnsNegativeValue;
var
  Top: Integer;
begin
  Top := TListGeometry.GetItemTop(-1, ITEM_HEIGHT, ITEM_MARGIN);
  Assert.IsTrue(Top < 0, 'Negative index should produce negative top');
end;

{ TListGeometryTests - Edge Cases: Zero Dimensions }

procedure TListGeometryTests.GetItemRect_ZeroItemHeight_ReturnsZeroHeightRect;
var
  R: TRect;
begin
  R := TListGeometry.GetItemRect(0, 0, ITEM_MARGIN, CLIENT_WIDTH, 0);

  Assert.AreEqual(R.Top, R.Bottom, 'Zero height item should have equal top and bottom');
  Assert.AreEqual(0, R.Height);
end;

procedure TListGeometryTests.GetItemRect_ZeroMargin_NoSpacing;
var
  R0, R1: TRect;
begin
  R0 := TListGeometry.GetItemRect(0, ITEM_HEIGHT, 0, CLIENT_WIDTH, 0);
  R1 := TListGeometry.GetItemRect(1, ITEM_HEIGHT, 0, CLIENT_WIDTH, 0);

  // With zero margin, items should be adjacent
  Assert.AreEqual(0, R0.Left);
  Assert.AreEqual(0, R0.Top);
  Assert.AreEqual(R0.Bottom, R1.Top, 'Items should be adjacent with zero margin');
end;

procedure TListGeometryTests.CalculateTotalHeight_ZeroItemHeight_ReturnsMarginOnly;
var
  Height: Integer;
begin
  // 3 items with zero height: just margins
  Height := TListGeometry.CalculateTotalHeight(3, 0, ITEM_MARGIN);
  // Formula: 3 * (0 + ITEM_MARGIN) + ITEM_MARGIN = 4 * ITEM_MARGIN
  Assert.AreEqual(4 * ITEM_MARGIN, Height);
end;

procedure TListGeometryTests.CalculateMaxScroll_ZeroClientHeight_ReturnsFullHeight;
var
  MaxScroll, TotalHeight: Integer;
begin
  TotalHeight := TListGeometry.CalculateTotalHeight(3, ITEM_HEIGHT, ITEM_MARGIN);
  MaxScroll := TListGeometry.CalculateMaxScroll(3, ITEM_HEIGHT, ITEM_MARGIN, 0);

  // With zero client height, max scroll should be total height
  Assert.AreEqual(TotalHeight, MaxScroll);
end;

procedure TListGeometryTests.ItemAtPos_ZeroClientWidth_NoHits;
var
  Index: Integer;
begin
  // With zero client width, items have negative width (Left > Right)
  Index := TListGeometry.ItemAtPos(0, ITEM_MARGIN + 10, 5, ITEM_HEIGHT, ITEM_MARGIN, 0, 0);
  Assert.AreEqual(-1, Index, 'Should not hit any item with zero client width');
end;

{ TListGeometryTests - Edge Cases: Boundary Clicks }

procedure TListGeometryTests.ItemAtPos_ClickExactlyOnTopEdge_ReturnsItem;
var
  Index: Integer;
  TopEdge: Integer;
begin
  TopEdge := ITEM_MARGIN; // Top edge of first item
  Index := TListGeometry.ItemAtPos(CLIENT_WIDTH div 2, TopEdge, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(0, Index, 'Click on top edge should hit the item');
end;

procedure TListGeometryTests.ItemAtPos_ClickExactlyOnBottomEdge_ReturnsNegativeOne;
var
  Index: Integer;
  BottomEdge: Integer;
begin
  BottomEdge := ITEM_MARGIN + ITEM_HEIGHT; // Bottom edge of first item (exclusive)
  Index := TListGeometry.ItemAtPos(CLIENT_WIDTH div 2, BottomEdge, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index, 'Click on bottom edge should not hit (exclusive bound)');
end;

procedure TListGeometryTests.ItemAtPos_ClickExactlyOnLeftEdge_ReturnsItem;
var
  Index: Integer;
begin
  // Left edge is ITEM_MARGIN (inclusive)
  Index := TListGeometry.ItemAtPos(ITEM_MARGIN, ITEM_MARGIN + 10, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(0, Index, 'Click on left edge should hit the item');
end;

procedure TListGeometryTests.ItemAtPos_ClickExactlyOnRightEdge_ReturnsNegativeOne;
var
  Index: Integer;
  RightEdge: Integer;
begin
  RightEdge := CLIENT_WIDTH - ITEM_MARGIN; // Right edge (exclusive)
  Index := TListGeometry.ItemAtPos(RightEdge, ITEM_MARGIN + 10, 5, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index, 'Click on right edge should not hit (exclusive bound)');
end;

{ TListGeometryTests - Edge Cases: Large Values }

procedure TListGeometryTests.GetItemRect_LargeIndex_CalculatesCorrectly;
var
  R: TRect;
  LargeIndex: Integer;
  ExpectedTop: Integer;
begin
  LargeIndex := 1000;
  R := TListGeometry.GetItemRect(LargeIndex, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);

  ExpectedTop := ITEM_MARGIN + LargeIndex * (ITEM_HEIGHT + ITEM_MARGIN);
  Assert.AreEqual(ExpectedTop, R.Top);
  Assert.AreEqual(ExpectedTop + ITEM_HEIGHT, R.Bottom);
end;

procedure TListGeometryTests.ClampScrollPos_ZeroMaxScroll_AlwaysReturnsZero;
begin
  Assert.AreEqual(0, TListGeometry.ClampScrollPos(-100, 0));
  Assert.AreEqual(0, TListGeometry.ClampScrollPos(0, 0));
  Assert.AreEqual(0, TListGeometry.ClampScrollPos(100, 0));
end;

procedure TListGeometryTests.CalculateTotalHeight_NegativeItemCount_ReturnsMargin;
var
  Height: Integer;
begin
  // Negative item count should be treated like zero items
  Height := TListGeometry.CalculateTotalHeight(-5, ITEM_HEIGHT, ITEM_MARGIN);
  Assert.AreEqual(ITEM_MARGIN, Height);
end;

{ TListGeometryTests - Edge Cases: Single Item }

procedure TListGeometryTests.ScrollPosToMakeVisible_SingleItemList_Works;
var
  NewScroll: Integer;
begin
  // Single item that fits in viewport
  NewScroll := TListGeometry.ScrollPosToMakeVisible(0, 0, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_HEIGHT);
  Assert.AreEqual(0, NewScroll, 'Single visible item should not need scrolling');
end;

procedure TListGeometryTests.ItemAtPos_SingleItem_ClickAbove_ReturnsNegativeOne;
var
  Index: Integer;
begin
  // Click above the single item (in top margin area, but above item start)
  Index := TListGeometry.ItemAtPos(CLIENT_WIDTH div 2, ITEM_MARGIN - 1, 1, ITEM_HEIGHT, ITEM_MARGIN, CLIENT_WIDTH, 0);
  Assert.AreEqual(-1, Index, 'Click above single item should return -1');
end;

initialization
  TDUnitX.RegisterTestFixture(TListGeometryTests);

end.
