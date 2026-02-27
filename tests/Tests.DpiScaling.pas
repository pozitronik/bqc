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
  end;

implementation

uses
  Winapi.Windows,
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

end.
