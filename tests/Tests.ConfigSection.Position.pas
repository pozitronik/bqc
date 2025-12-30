{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Position Config Section Tests                   }
{                                                       }
{*******************************************************}

unit Tests.ConfigSection.Position;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConfigSection.Position,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test fixture for TPositionConfigSection class.
  /// Tests default values, getters, setters, and modification notifications.
  /// </summary>
  [TestFixture]
  TPositionConfigSectionTests = class
  private
    FSection: TPositionConfigSection;
    FModifiedCalled: Boolean;

    procedure HandleModified;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure Create_InitializesWithDefaults;
    [Test]
    procedure Create_WithNilNotifier_CreatesInstance;

    { Default Values Tests }
    [Test]
    procedure Default_PositionMode_IsCoordinates;
    [Test]
    procedure Default_PositionX_IsNegativeOne;
    [Test]
    procedure Default_PositionY_IsNegativeOne;
    [Test]
    procedure Default_PositionW_IsNegativeOne;
    [Test]
    procedure Default_PositionH_IsNegativeOne;

    { SetDefaults Tests }
    [Test]
    procedure SetDefaults_RestoresAllDefaults;

    { PositionMode Tests }
    [Test]
    procedure SetPositionMode_UpdatesValue;
    [Test]
    procedure SetPositionMode_SameValue_NoModification;
    [Test]
    procedure SetPositionMode_DifferentValue_NotifiesModified;

    { PositionX Tests }
    [Test]
    procedure SetPositionX_UpdatesValue;
    [Test]
    procedure SetPositionX_SameValue_NoModification;
    [Test]
    procedure SetPositionX_DifferentValue_NotifiesModified;

    { PositionY Tests }
    [Test]
    procedure SetPositionY_UpdatesValue;
    [Test]
    procedure SetPositionY_SameValue_NoModification;
    [Test]
    procedure SetPositionY_DifferentValue_NotifiesModified;

    { PositionW Tests }
    [Test]
    procedure SetPositionW_UpdatesValue;
    [Test]
    procedure SetPositionW_SameValue_NoModification;
    [Test]
    procedure SetPositionW_DifferentValue_NotifiesModified;

    { PositionH Tests }
    [Test]
    procedure SetPositionH_UpdatesValue;
    [Test]
    procedure SetPositionH_SameValue_NoModification;
    [Test]
    procedure SetPositionH_DifferentValue_NotifiesModified;

    { Interface Tests }
    [Test]
    procedure ImplementsIPositionConfig;
  end;

implementation

uses
  App.SettingsRepository;

{ TPositionConfigSectionTests }

procedure TPositionConfigSectionTests.Setup;
begin
  FModifiedCalled := False;
  FSection := TPositionConfigSection.Create(HandleModified);
end;

procedure TPositionConfigSectionTests.TearDown;
begin
  FSection.Free;
end;

procedure TPositionConfigSectionTests.HandleModified;
begin
  FModifiedCalled := True;
end;

procedure TPositionConfigSectionTests.Create_InitializesWithDefaults;
begin
  Assert.AreEqual(Integer(DEF_POSITION_MODE), Integer(FSection.PositionMode));
  Assert.AreEqual(DEF_POSITION_X, FSection.PositionX);
  Assert.AreEqual(DEF_POSITION_Y, FSection.PositionY);
  Assert.AreEqual(DEF_POSITION_W, FSection.PositionW);
  Assert.AreEqual(DEF_POSITION_H, FSection.PositionH);
end;

procedure TPositionConfigSectionTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TPositionConfigSection;
begin
  Section := TPositionConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

procedure TPositionConfigSectionTests.Default_PositionMode_IsCoordinates;
begin
  Assert.AreEqual(Integer(pmCoordinates), Integer(FSection.PositionMode));
end;

procedure TPositionConfigSectionTests.Default_PositionX_IsNegativeOne;
begin
  Assert.AreEqual(-1, FSection.PositionX);
end;

procedure TPositionConfigSectionTests.Default_PositionY_IsNegativeOne;
begin
  Assert.AreEqual(-1, FSection.PositionY);
end;

procedure TPositionConfigSectionTests.Default_PositionW_IsNegativeOne;
begin
  Assert.AreEqual(-1, FSection.PositionW);
end;

procedure TPositionConfigSectionTests.Default_PositionH_IsNegativeOne;
begin
  Assert.AreEqual(-1, FSection.PositionH);
end;

procedure TPositionConfigSectionTests.SetDefaults_RestoresAllDefaults;
begin
  // Change all values
  FSection.PositionMode := pmNearCursor;
  FSection.PositionX := 100;
  FSection.PositionY := 200;
  FSection.PositionW := 400;
  FSection.PositionH := 500;

  // Reset to defaults
  FSection.SetDefaults;

  // Verify all defaults are restored
  Assert.AreEqual(Integer(DEF_POSITION_MODE), Integer(FSection.PositionMode));
  Assert.AreEqual(DEF_POSITION_X, FSection.PositionX);
  Assert.AreEqual(DEF_POSITION_Y, FSection.PositionY);
  Assert.AreEqual(DEF_POSITION_W, FSection.PositionW);
  Assert.AreEqual(DEF_POSITION_H, FSection.PositionH);
end;

procedure TPositionConfigSectionTests.SetPositionMode_UpdatesValue;
begin
  FSection.PositionMode := pmNearCursor;
  Assert.AreEqual(Integer(pmNearCursor), Integer(FSection.PositionMode));
end;

procedure TPositionConfigSectionTests.SetPositionMode_SameValue_NoModification;
begin
  FSection.PositionMode := DEF_POSITION_MODE;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPositionConfigSectionTests.SetPositionMode_DifferentValue_NotifiesModified;
begin
  FSection.PositionMode := pmNearCursor;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPositionConfigSectionTests.SetPositionX_UpdatesValue;
begin
  FSection.PositionX := 100;
  Assert.AreEqual(100, FSection.PositionX);
end;

procedure TPositionConfigSectionTests.SetPositionX_SameValue_NoModification;
begin
  FSection.PositionX := DEF_POSITION_X;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPositionConfigSectionTests.SetPositionX_DifferentValue_NotifiesModified;
begin
  FSection.PositionX := 100;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPositionConfigSectionTests.SetPositionY_UpdatesValue;
begin
  FSection.PositionY := 200;
  Assert.AreEqual(200, FSection.PositionY);
end;

procedure TPositionConfigSectionTests.SetPositionY_SameValue_NoModification;
begin
  FSection.PositionY := DEF_POSITION_Y;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPositionConfigSectionTests.SetPositionY_DifferentValue_NotifiesModified;
begin
  FSection.PositionY := 200;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPositionConfigSectionTests.SetPositionW_UpdatesValue;
begin
  FSection.PositionW := 400;
  Assert.AreEqual(400, FSection.PositionW);
end;

procedure TPositionConfigSectionTests.SetPositionW_SameValue_NoModification;
begin
  FSection.PositionW := DEF_POSITION_W;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPositionConfigSectionTests.SetPositionW_DifferentValue_NotifiesModified;
begin
  FSection.PositionW := 400;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPositionConfigSectionTests.SetPositionH_UpdatesValue;
begin
  FSection.PositionH := 500;
  Assert.AreEqual(500, FSection.PositionH);
end;

procedure TPositionConfigSectionTests.SetPositionH_SameValue_NoModification;
begin
  FSection.PositionH := DEF_POSITION_H;
  Assert.IsFalse(FModifiedCalled, 'Should not notify when value unchanged');
end;

procedure TPositionConfigSectionTests.SetPositionH_DifferentValue_NotifiesModified;
begin
  FSection.PositionH := 500;
  Assert.IsTrue(FModifiedCalled, 'Should notify when value changed');
end;

procedure TPositionConfigSectionTests.ImplementsIPositionConfig;
begin
  Assert.IsTrue(TPositionConfigSection.GetInterfaceEntry(IPositionConfig) <> nil,
    'TPositionConfigSection should implement IPositionConfig');
end;

initialization
  TDUnitX.RegisterTestFixture(TPositionConfigSectionTests);

end.
