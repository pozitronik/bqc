{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Config Section Base Class Tests                 }
{                                                       }
{*******************************************************}

unit Tests.ConfigSectionTypes;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Test descendant that exposes protected methods for testing.
  /// </summary>
  TTestableConfigSection = class(TConfigSectionBase)
  public
    FTestBoolean: Boolean;
    FTestInteger: Integer;
    FTestString: string;

    procedure PublicNotifyModified;
    procedure PublicSetFieldBoolean(var AField: Boolean; AValue: Boolean);
    procedure PublicSetFieldInteger(var AField: Integer; AValue: Integer);
    procedure PublicSetFieldString(var AField: string; const AValue: string);
  end;

  /// <summary>
  /// Test fixture for TConfigSectionBase class.
  /// Verifies base class functionality used by all config sections.
  /// </summary>
  [TestFixture]
  TConfigSectionBaseTests = class
  private
    FSection: TTestableConfigSection;
    FModifiedCallCount: Integer;

    procedure HandleModified;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure Create_WithNotifier_StoresCallback;
    [Test]
    procedure Create_WithNilNotifier_CreatesInstance;

    { NotifyModified Tests }
    [Test]
    procedure NotifyModified_WithCallback_CallsCallback;
    [Test]
    procedure NotifyModified_WithNilCallback_DoesNotCrash;
    [Test]
    procedure NotifyModified_MultipleCalls_CallsCallbackEachTime;

    { SetFieldBoolean Tests }
    [Test]
    procedure SetFieldBoolean_DifferentValue_UpdatesField;
    [Test]
    procedure SetFieldBoolean_DifferentValue_NotifiesModified;
    [Test]
    procedure SetFieldBoolean_SameValue_DoesNotUpdateField;
    [Test]
    procedure SetFieldBoolean_SameValue_DoesNotNotify;
    [Test]
    procedure SetFieldBoolean_TrueToFalse_UpdatesAndNotifies;
    [Test]
    procedure SetFieldBoolean_FalseToTrue_UpdatesAndNotifies;

    { SetFieldInteger Tests }
    [Test]
    procedure SetFieldInteger_DifferentValue_UpdatesField;
    [Test]
    procedure SetFieldInteger_DifferentValue_NotifiesModified;
    [Test]
    procedure SetFieldInteger_SameValue_DoesNotUpdateField;
    [Test]
    procedure SetFieldInteger_SameValue_DoesNotNotify;
    [Test]
    procedure SetFieldInteger_ZeroToPositive_UpdatesAndNotifies;
    [Test]
    procedure SetFieldInteger_PositiveToNegative_UpdatesAndNotifies;

    { SetFieldString Tests }
    [Test]
    procedure SetFieldString_DifferentValue_UpdatesField;
    [Test]
    procedure SetFieldString_DifferentValue_NotifiesModified;
    [Test]
    procedure SetFieldString_SameValue_DoesNotUpdateField;
    [Test]
    procedure SetFieldString_SameValue_DoesNotNotify;
    [Test]
    procedure SetFieldString_EmptyToNonEmpty_UpdatesAndNotifies;
    [Test]
    procedure SetFieldString_NonEmptyToEmpty_UpdatesAndNotifies;
    [Test]
    procedure SetFieldString_CaseSensitive_DifferentCaseNotifies;
  end;

implementation

{ TTestableConfigSection }

procedure TTestableConfigSection.PublicNotifyModified;
begin
  NotifyModified;
end;

procedure TTestableConfigSection.PublicSetFieldBoolean(var AField: Boolean; AValue: Boolean);
begin
  SetFieldBoolean(AField, AValue);
end;

procedure TTestableConfigSection.PublicSetFieldInteger(var AField: Integer; AValue: Integer);
begin
  SetFieldInteger(AField, AValue);
end;

procedure TTestableConfigSection.PublicSetFieldString(var AField: string; const AValue: string);
begin
  SetFieldString(AField, AValue);
end;

{ TConfigSectionBaseTests }

procedure TConfigSectionBaseTests.Setup;
begin
  FModifiedCallCount := 0;
  FSection := TTestableConfigSection.Create(HandleModified);
end;

procedure TConfigSectionBaseTests.TearDown;
begin
  FSection.Free;
end;

procedure TConfigSectionBaseTests.HandleModified;
begin
  Inc(FModifiedCallCount);
end;

{ Constructor Tests }

procedure TConfigSectionBaseTests.Create_WithNotifier_StoresCallback;
begin
  // Verify callback is stored by triggering notification
  FSection.PublicNotifyModified;
  Assert.AreEqual(1, FModifiedCallCount, 'Callback should be called');
end;

procedure TConfigSectionBaseTests.Create_WithNilNotifier_CreatesInstance;
var
  Section: TTestableConfigSection;
begin
  Section := TTestableConfigSection.Create(nil);
  try
    Assert.IsNotNull(Section);
  finally
    Section.Free;
  end;
end;

{ NotifyModified Tests }

procedure TConfigSectionBaseTests.NotifyModified_WithCallback_CallsCallback;
begin
  FSection.PublicNotifyModified;
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.NotifyModified_WithNilCallback_DoesNotCrash;
var
  Section: TTestableConfigSection;
begin
  Section := TTestableConfigSection.Create(nil);
  try
    // Should not raise exception
    Section.PublicNotifyModified;
    Assert.Pass('No exception raised with nil callback');
  finally
    Section.Free;
  end;
end;

procedure TConfigSectionBaseTests.NotifyModified_MultipleCalls_CallsCallbackEachTime;
begin
  FSection.PublicNotifyModified;
  FSection.PublicNotifyModified;
  FSection.PublicNotifyModified;
  Assert.AreEqual(3, FModifiedCallCount);
end;

{ SetFieldBoolean Tests }

procedure TConfigSectionBaseTests.SetFieldBoolean_DifferentValue_UpdatesField;
begin
  FSection.FTestBoolean := False;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, True);
  Assert.IsTrue(FSection.FTestBoolean);
end;

procedure TConfigSectionBaseTests.SetFieldBoolean_DifferentValue_NotifiesModified;
begin
  FSection.FTestBoolean := False;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, True);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldBoolean_SameValue_DoesNotUpdateField;
begin
  FSection.FTestBoolean := True;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, True);
  Assert.IsTrue(FSection.FTestBoolean);
end;

procedure TConfigSectionBaseTests.SetFieldBoolean_SameValue_DoesNotNotify;
begin
  FSection.FTestBoolean := True;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, True);
  Assert.AreEqual(0, FModifiedCallCount, 'Should not notify when value unchanged');
end;

procedure TConfigSectionBaseTests.SetFieldBoolean_TrueToFalse_UpdatesAndNotifies;
begin
  FSection.FTestBoolean := True;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, False);
  Assert.IsFalse(FSection.FTestBoolean);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldBoolean_FalseToTrue_UpdatesAndNotifies;
begin
  FSection.FTestBoolean := False;
  FSection.PublicSetFieldBoolean(FSection.FTestBoolean, True);
  Assert.IsTrue(FSection.FTestBoolean);
  Assert.AreEqual(1, FModifiedCallCount);
end;

{ SetFieldInteger Tests }

procedure TConfigSectionBaseTests.SetFieldInteger_DifferentValue_UpdatesField;
begin
  FSection.FTestInteger := 0;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, 42);
  Assert.AreEqual(42, FSection.FTestInteger);
end;

procedure TConfigSectionBaseTests.SetFieldInteger_DifferentValue_NotifiesModified;
begin
  FSection.FTestInteger := 0;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, 42);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldInteger_SameValue_DoesNotUpdateField;
begin
  FSection.FTestInteger := 42;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, 42);
  Assert.AreEqual(42, FSection.FTestInteger);
end;

procedure TConfigSectionBaseTests.SetFieldInteger_SameValue_DoesNotNotify;
begin
  FSection.FTestInteger := 42;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, 42);
  Assert.AreEqual(0, FModifiedCallCount, 'Should not notify when value unchanged');
end;

procedure TConfigSectionBaseTests.SetFieldInteger_ZeroToPositive_UpdatesAndNotifies;
begin
  FSection.FTestInteger := 0;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, 100);
  Assert.AreEqual(100, FSection.FTestInteger);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldInteger_PositiveToNegative_UpdatesAndNotifies;
begin
  FSection.FTestInteger := 100;
  FSection.PublicSetFieldInteger(FSection.FTestInteger, -50);
  Assert.AreEqual(-50, FSection.FTestInteger);
  Assert.AreEqual(1, FModifiedCallCount);
end;

{ SetFieldString Tests }

procedure TConfigSectionBaseTests.SetFieldString_DifferentValue_UpdatesField;
begin
  FSection.FTestString := 'old';
  FSection.PublicSetFieldString(FSection.FTestString, 'new');
  Assert.AreEqual('new', FSection.FTestString);
end;

procedure TConfigSectionBaseTests.SetFieldString_DifferentValue_NotifiesModified;
begin
  FSection.FTestString := 'old';
  FSection.PublicSetFieldString(FSection.FTestString, 'new');
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldString_SameValue_DoesNotUpdateField;
begin
  FSection.FTestString := 'same';
  FSection.PublicSetFieldString(FSection.FTestString, 'same');
  Assert.AreEqual('same', FSection.FTestString);
end;

procedure TConfigSectionBaseTests.SetFieldString_SameValue_DoesNotNotify;
begin
  FSection.FTestString := 'same';
  FSection.PublicSetFieldString(FSection.FTestString, 'same');
  Assert.AreEqual(0, FModifiedCallCount, 'Should not notify when value unchanged');
end;

procedure TConfigSectionBaseTests.SetFieldString_EmptyToNonEmpty_UpdatesAndNotifies;
begin
  FSection.FTestString := '';
  FSection.PublicSetFieldString(FSection.FTestString, 'value');
  Assert.AreEqual('value', FSection.FTestString);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldString_NonEmptyToEmpty_UpdatesAndNotifies;
begin
  FSection.FTestString := 'value';
  FSection.PublicSetFieldString(FSection.FTestString, '');
  Assert.AreEqual('', FSection.FTestString);
  Assert.AreEqual(1, FModifiedCallCount);
end;

procedure TConfigSectionBaseTests.SetFieldString_CaseSensitive_DifferentCaseNotifies;
begin
  FSection.FTestString := 'Value';
  FSection.PublicSetFieldString(FSection.FTestString, 'value');
  Assert.AreEqual('value', FSection.FTestString);
  Assert.AreEqual(1, FModifiedCallCount, 'String comparison should be case-sensitive');
end;

initialization
  TDUnitX.RegisterTestFixture(TConfigSectionBaseTests);

end.
