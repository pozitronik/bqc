{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       THotkeyManager Tests                            }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.HotkeyManager;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  UI.HotkeyManager;

type
  /// <summary>
  /// Test fixture for THotkeyManager class.
  /// Tests ParseHotkeyString, BuildHotkeyString, and round-trip conversions.
  /// </summary>
  [TestFixture]
  TTestHotkeyManager = class
  private
    FHotkeyManager: THotkeyManager;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ParseHotkeyString Tests - Valid Combinations }
    [Test]
    procedure ParseHotkeyString_CtrlAltA_ValidCombination;

    [Test]
    procedure ParseHotkeyString_CtrlShiftF1_FunctionKey;

    [Test]
    procedure ParseHotkeyString_AltSpace_SpecialKey;

    [Test]
    procedure ParseHotkeyString_CtrlEnter_SpecialKey;

    [Test]
    procedure ParseHotkeyString_CtrlEscape_SpecialKey;

    { ParseHotkeyString Tests - Edge Cases }
    [Test]
    procedure ParseHotkeyString_EmptyString_ReturnsDefaults;

    [Test]
    procedure ParseHotkeyString_InvalidModifier_Handles;

    [Test]
    procedure ParseHotkeyString_CaseInsensitive_LowerCase;

    [Test]
    procedure ParseHotkeyString_CaseInsensitive_UpperCase;

    [Test]
    procedure ParseHotkeyString_DuplicateModifiers_CtrlCtrlA;

    [Test]
    procedure ParseHotkeyString_NoModifier_SingleKey;

    { ParseHotkeyString Tests - Function Keys }
    [Test]
    procedure ParseHotkeyString_AllFunctionKeys_F1ToF12;

    { ParseHotkeyString Tests - Formatting Edge Cases }
    [Test]
    procedure ParseHotkeyString_MultiplePlusSigns_Handles;

    [Test]
    procedure ParseHotkeyString_WhitespaceAround_Trims;

    { BuildHotkeyString Tests }
    [Test]
    procedure BuildHotkeyString_ValidCombination_ReturnsFormatted;

    [Test]
    procedure BuildHotkeyString_AllModifiers_CorrectOrder;

    [Test]
    procedure BuildHotkeyString_NoModifiers_JustKey;

    [Test]
    procedure BuildHotkeyString_FunctionKey_F5;

    { Round-trip Tests }
    [Test]
    procedure RoundTrip_ParseThenBuild_MatchesOriginal;

    [Test]
    procedure RoundTrip_CommonHotkeys_AllValid;

    { Edge Cases }
    [Test]
    procedure ParseHotkeyString_OnlyModifiers_NoKey;

    [Test]
    procedure ParseHotkeyString_UnknownKey_Handles;

    [Test]
    procedure ParseHotkeyString_NumericKey_Handles;

    { Additional Special Keys Tests }
    [Test]
    procedure ParseHotkeyString_Tab_SpecialKey;

    [Test]
    procedure ParseHotkeyString_Backspace_SpecialKey;

    [Test]
    procedure ParseHotkeyString_Delete_SpecialKey;

    [Test]
    procedure ParseHotkeyString_Insert_SpecialKey;

    [Test]
    procedure ParseHotkeyString_Home_SpecialKey;

    [Test]
    procedure ParseHotkeyString_End_SpecialKey;

    [Test]
    procedure ParseHotkeyString_PageUp_SpecialKey;

    [Test]
    procedure ParseHotkeyString_PageDown_SpecialKey;

    [Test]
    procedure ParseHotkeyString_ArrowKeys_SpecialKeys;

    { Windows Key Tests }
    [Test]
    procedure ParseHotkeyString_WinModifier_Supported;

    [Test]
    procedure ParseHotkeyString_WindowsModifier_Alias;

    { Control Modifier Alias Tests }
    [Test]
    procedure ParseHotkeyString_ControlModifier_Alias;

    { BuildHotkeyString Special Keys Tests }
    [Test]
    procedure BuildHotkeyString_Space_ReturnsSpace;

    [Test]
    procedure BuildHotkeyString_Enter_ReturnsEnter;

    [Test]
    procedure BuildHotkeyString_Escape_ReturnsEscape;

    [Test]
    procedure BuildHotkeyString_ArrowKeys_ReturnsCorrectNames;

    [Test]
    procedure BuildHotkeyString_NavigationKeys_ReturnsCorrectNames;

    { BuildHotkeyString Modifier-Only Tests }
    [Test]
    procedure BuildHotkeyString_ModifierKeyOnly_ReturnsEmpty;
  end;

implementation

{ TTestHotkeyManager }

procedure TTestHotkeyManager.Setup;
begin
  FHotkeyManager := THotkeyManager.Create;
end;

procedure TTestHotkeyManager.TearDown;
begin
  FHotkeyManager.Free;
  FHotkeyManager := nil;
end;

{ ParseHotkeyString Tests - Valid Combinations }

procedure TTestHotkeyManager.ParseHotkeyString_CtrlAltA_ValidCombination;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Alt+A', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for valid combination');
  Assert.AreEqual(Cardinal(MOD_CONTROL or MOD_ALT), Modifiers, 'Modifiers should include Ctrl and Alt');
  Assert.AreEqual(Cardinal(Ord('A')), VirtualKey, 'VirtualKey should be A');
end;

procedure TTestHotkeyManager.ParseHotkeyString_CtrlShiftF1_FunctionKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Shift+F1', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for function key');
  Assert.AreEqual(Cardinal(MOD_CONTROL or MOD_SHIFT), Modifiers, 'Modifiers should include Ctrl and Shift');
  Assert.AreEqual(Cardinal(VK_F1), VirtualKey, 'VirtualKey should be VK_F1');
end;

procedure TTestHotkeyManager.ParseHotkeyString_AltSpace_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Alt+Space', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Space');
  Assert.AreEqual(Cardinal(MOD_ALT), Modifiers, 'Modifiers should include Alt');
  Assert.AreEqual(Cardinal(VK_SPACE), VirtualKey, 'VirtualKey should be VK_SPACE');
end;

procedure TTestHotkeyManager.ParseHotkeyString_CtrlEnter_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Enter', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Enter');
  Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl');
  Assert.AreEqual(Cardinal(VK_RETURN), VirtualKey, 'VirtualKey should be VK_RETURN');
end;

procedure TTestHotkeyManager.ParseHotkeyString_CtrlEscape_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Escape', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Escape');
  Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl');
  Assert.AreEqual(Cardinal(VK_ESCAPE), VirtualKey, 'VirtualKey should be VK_ESCAPE');
end;

{ ParseHotkeyString Tests - Edge Cases }

procedure TTestHotkeyManager.ParseHotkeyString_EmptyString_ReturnsDefaults;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('', Modifiers, VirtualKey);

  Assert.IsFalse(Result, 'ParseHotkeyString should return False for empty string');
  Assert.AreEqual(Cardinal(0), Modifiers, 'Modifiers should be 0 for empty string');
  Assert.AreEqual(Cardinal(0), VirtualKey, 'VirtualKey should be 0 for empty string');
end;

procedure TTestHotkeyManager.ParseHotkeyString_InvalidModifier_Handles;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Invalid+A', Modifiers, VirtualKey);

  Assert.IsFalse(Result, 'ParseHotkeyString should return False for invalid modifier');
end;

procedure TTestHotkeyManager.ParseHotkeyString_CaseInsensitive_LowerCase;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('ctrl+alt+b', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should handle lowercase');
  Assert.AreEqual(Cardinal(MOD_CONTROL or MOD_ALT), Modifiers, 'Modifiers should be parsed correctly');
  Assert.AreEqual(Cardinal(Ord('B')), VirtualKey, 'VirtualKey should be B (uppercase)');
end;

procedure TTestHotkeyManager.ParseHotkeyString_CaseInsensitive_UpperCase;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('CTRL+ALT+B', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should handle uppercase');
  Assert.AreEqual(Cardinal(MOD_CONTROL or MOD_ALT), Modifiers, 'Modifiers should be parsed correctly');
  Assert.AreEqual(Cardinal(Ord('B')), VirtualKey, 'VirtualKey should be B');
end;

procedure TTestHotkeyManager.ParseHotkeyString_DuplicateModifiers_CtrlCtrlA;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  // Duplicate modifiers should still work - the OR operation handles it
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Ctrl+A', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should handle duplicate modifiers');
  Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl (only once in result)');
  Assert.AreEqual(Cardinal(Ord('A')), VirtualKey, 'VirtualKey should be A');
end;

procedure TTestHotkeyManager.ParseHotkeyString_NoModifier_SingleKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('A', Modifiers, VirtualKey);

  // Global hotkeys require at least one modifier
  Assert.IsFalse(Result, 'ParseHotkeyString should return False for key without modifiers');
end;

{ ParseHotkeyString Tests - Function Keys }

procedure TTestHotkeyManager.ParseHotkeyString_AllFunctionKeys_F1ToF12;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    Result := THotkeyManager.ParseHotkeyString('Ctrl+F' + IntToStr(I), Modifiers, VirtualKey);

    Assert.IsTrue(Result, 'ParseHotkeyString should return True for F' + IntToStr(I));
    Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl for F' + IntToStr(I));
    Assert.AreEqual(Cardinal(VK_F1 + I - 1), VirtualKey, 'VirtualKey should be VK_F' + IntToStr(I));
  end;
end;

{ ParseHotkeyString Tests - Formatting Edge Cases }

procedure TTestHotkeyManager.ParseHotkeyString_MultiplePlusSigns_Handles;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  // Multiple plus signs would create empty parts
  Result := THotkeyManager.ParseHotkeyString('Ctrl++A', Modifiers, VirtualKey);

  // The behavior depends on implementation - empty part is not a valid modifier
  Assert.IsFalse(Result, 'ParseHotkeyString should return False for malformed input');
end;

procedure TTestHotkeyManager.ParseHotkeyString_WhitespaceAround_Trims;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString(' Ctrl + Alt + A ', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should trim whitespace');
  Assert.AreEqual(Cardinal(MOD_CONTROL or MOD_ALT), Modifiers, 'Modifiers should be parsed correctly');
  Assert.AreEqual(Cardinal(Ord('A')), VirtualKey, 'VirtualKey should be A');
end;

{ BuildHotkeyString Tests }

procedure TTestHotkeyManager.BuildHotkeyString_ValidCombination_ReturnsFormatted;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(Ord('B'), [ssCtrl, ssAlt]);

  Assert.AreEqual('Ctrl+Alt+B', Result, 'BuildHotkeyString should return formatted string');
end;

procedure TTestHotkeyManager.BuildHotkeyString_AllModifiers_CorrectOrder;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(Ord('X'), [ssCtrl, ssAlt, ssShift]);

  // Order should be Ctrl+Alt+Shift based on the implementation
  Assert.AreEqual('Ctrl+Alt+Shift+X', Result, 'BuildHotkeyString should have correct modifier order');
end;

procedure TTestHotkeyManager.BuildHotkeyString_NoModifiers_JustKey;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(Ord('A'), []);

  // No modifiers means invalid hotkey
  Assert.AreEqual('', Result, 'BuildHotkeyString should return empty for no modifiers');
end;

procedure TTestHotkeyManager.BuildHotkeyString_FunctionKey_F5;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_F5, [ssCtrl]);

  Assert.AreEqual('Ctrl+F5', Result, 'BuildHotkeyString should format function keys correctly');
end;

{ Round-trip Tests }

procedure TTestHotkeyManager.RoundTrip_ParseThenBuild_MatchesOriginal;
var
  Modifiers, VirtualKey: Cardinal;
  Shift: TShiftState;
  ParseResult: Boolean;
  BuiltString: string;
begin
  // Parse the original string
  ParseResult := THotkeyManager.ParseHotkeyString('Ctrl+Alt+B', Modifiers, VirtualKey);
  Assert.IsTrue(ParseResult, 'ParseHotkeyString should succeed');

  // Convert modifiers to TShiftState
  Shift := [];
  if (Modifiers and MOD_CONTROL) <> 0 then
    Include(Shift, ssCtrl);
  if (Modifiers and MOD_ALT) <> 0 then
    Include(Shift, ssAlt);
  if (Modifiers and MOD_SHIFT) <> 0 then
    Include(Shift, ssShift);

  // Build back to string
  BuiltString := THotkeyManager.BuildHotkeyString(Word(VirtualKey), Shift);

  Assert.AreEqual('Ctrl+Alt+B', BuiltString, 'Round-trip should produce original string');
end;

procedure TTestHotkeyManager.RoundTrip_CommonHotkeys_AllValid;
var
  TestCases: array[0..4] of string;
  TestCase: string;
  Modifiers, VirtualKey: Cardinal;
  Shift: TShiftState;
  ParseResult: Boolean;
  BuiltString: string;
begin
  TestCases[0] := 'Ctrl+B';
  TestCases[1] := 'Alt+F4';
  TestCases[2] := 'Ctrl+Shift+N';
  TestCases[3] := 'Ctrl+Space';
  TestCases[4] := 'Alt+Enter';

  for TestCase in TestCases do
  begin
    ParseResult := THotkeyManager.ParseHotkeyString(TestCase, Modifiers, VirtualKey);
    Assert.IsTrue(ParseResult, 'ParseHotkeyString should succeed for: ' + TestCase);

    Shift := [];
    if (Modifiers and MOD_CONTROL) <> 0 then
      Include(Shift, ssCtrl);
    if (Modifiers and MOD_ALT) <> 0 then
      Include(Shift, ssAlt);
    if (Modifiers and MOD_SHIFT) <> 0 then
      Include(Shift, ssShift);

    BuiltString := THotkeyManager.BuildHotkeyString(Word(VirtualKey), Shift);
    Assert.AreEqual(TestCase, BuiltString, 'Round-trip should match for: ' + TestCase);
  end;
end;

{ Edge Cases }

procedure TTestHotkeyManager.ParseHotkeyString_OnlyModifiers_NoKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Alt+', Modifiers, VirtualKey);

  // Empty key part after splitting should fail
  Assert.IsFalse(Result, 'ParseHotkeyString should return False for only modifiers');
end;

procedure TTestHotkeyManager.ParseHotkeyString_UnknownKey_Handles;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Unknown', Modifiers, VirtualKey);

  Assert.IsFalse(Result, 'ParseHotkeyString should return False for unknown key');
end;

procedure TTestHotkeyManager.ParseHotkeyString_NumericKey_Handles;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+1', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should handle numeric keys');
  Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl');
  Assert.AreEqual(Cardinal(Ord('1')), VirtualKey, 'VirtualKey should be 1');
end;

{ Additional Special Keys Tests }

procedure TTestHotkeyManager.ParseHotkeyString_Tab_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Tab', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Tab');
  Assert.AreEqual(Cardinal(VK_TAB), VirtualKey, 'VirtualKey should be VK_TAB');
end;

procedure TTestHotkeyManager.ParseHotkeyString_Backspace_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Backspace', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Backspace');
  Assert.AreEqual(Cardinal(VK_BACK), VirtualKey, 'VirtualKey should be VK_BACK');
end;

procedure TTestHotkeyManager.ParseHotkeyString_Delete_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Delete', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Delete');
  Assert.AreEqual(Cardinal(VK_DELETE), VirtualKey, 'VirtualKey should be VK_DELETE');
end;

procedure TTestHotkeyManager.ParseHotkeyString_Insert_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Insert', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Insert');
  Assert.AreEqual(Cardinal(VK_INSERT), VirtualKey, 'VirtualKey should be VK_INSERT');
end;

procedure TTestHotkeyManager.ParseHotkeyString_Home_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Home', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Home');
  Assert.AreEqual(Cardinal(VK_HOME), VirtualKey, 'VirtualKey should be VK_HOME');
end;

procedure TTestHotkeyManager.ParseHotkeyString_End_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+End', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for End');
  Assert.AreEqual(Cardinal(VK_END), VirtualKey, 'VirtualKey should be VK_END');
end;

procedure TTestHotkeyManager.ParseHotkeyString_PageUp_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+PageUp', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for PageUp');
  Assert.AreEqual(Cardinal(VK_PRIOR), VirtualKey, 'VirtualKey should be VK_PRIOR');
end;

procedure TTestHotkeyManager.ParseHotkeyString_PageDown_SpecialKey;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Ctrl+PageDown', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should return True for PageDown');
  Assert.AreEqual(Cardinal(VK_NEXT), VirtualKey, 'VirtualKey should be VK_NEXT');
end;

procedure TTestHotkeyManager.ParseHotkeyString_ArrowKeys_SpecialKeys;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  // Up
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Up', Modifiers, VirtualKey);
  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Up');
  Assert.AreEqual(Cardinal(VK_UP), VirtualKey, 'VirtualKey should be VK_UP');

  // Down
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Down', Modifiers, VirtualKey);
  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Down');
  Assert.AreEqual(Cardinal(VK_DOWN), VirtualKey, 'VirtualKey should be VK_DOWN');

  // Left
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Left', Modifiers, VirtualKey);
  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Left');
  Assert.AreEqual(Cardinal(VK_LEFT), VirtualKey, 'VirtualKey should be VK_LEFT');

  // Right
  Result := THotkeyManager.ParseHotkeyString('Ctrl+Right', Modifiers, VirtualKey);
  Assert.IsTrue(Result, 'ParseHotkeyString should return True for Right');
  Assert.AreEqual(Cardinal(VK_RIGHT), VirtualKey, 'VirtualKey should be VK_RIGHT');
end;

{ Windows Key Tests }

procedure TTestHotkeyManager.ParseHotkeyString_WinModifier_Supported;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Win+A', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should support Win modifier');
  Assert.AreEqual(Cardinal(MOD_WIN), Modifiers, 'Modifiers should include Win');
  Assert.AreEqual(Cardinal(Ord('A')), VirtualKey, 'VirtualKey should be A');
end;

procedure TTestHotkeyManager.ParseHotkeyString_WindowsModifier_Alias;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Windows+B', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should support Windows modifier alias');
  Assert.AreEqual(Cardinal(MOD_WIN), Modifiers, 'Modifiers should include Win');
  Assert.AreEqual(Cardinal(Ord('B')), VirtualKey, 'VirtualKey should be B');
end;

{ Control Modifier Alias Tests }

procedure TTestHotkeyManager.ParseHotkeyString_ControlModifier_Alias;
var
  Modifiers, VirtualKey: Cardinal;
  Result: Boolean;
begin
  Result := THotkeyManager.ParseHotkeyString('Control+A', Modifiers, VirtualKey);

  Assert.IsTrue(Result, 'ParseHotkeyString should support Control modifier alias');
  Assert.AreEqual(Cardinal(MOD_CONTROL), Modifiers, 'Modifiers should include Ctrl');
  Assert.AreEqual(Cardinal(Ord('A')), VirtualKey, 'VirtualKey should be A');
end;

{ BuildHotkeyString Special Keys Tests }

procedure TTestHotkeyManager.BuildHotkeyString_Space_ReturnsSpace;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_SPACE, [ssCtrl]);

  Assert.AreEqual('Ctrl+Space', Result, 'BuildHotkeyString should return Space for VK_SPACE');
end;

procedure TTestHotkeyManager.BuildHotkeyString_Enter_ReturnsEnter;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_RETURN, [ssCtrl]);

  Assert.AreEqual('Ctrl+Enter', Result, 'BuildHotkeyString should return Enter for VK_RETURN');
end;

procedure TTestHotkeyManager.BuildHotkeyString_Escape_ReturnsEscape;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_ESCAPE, [ssAlt]);

  Assert.AreEqual('Alt+Escape', Result, 'BuildHotkeyString should return Escape for VK_ESCAPE');
end;

procedure TTestHotkeyManager.BuildHotkeyString_ArrowKeys_ReturnsCorrectNames;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_UP, [ssCtrl]);
  Assert.AreEqual('Ctrl+Up', Result, 'BuildHotkeyString should return Up for VK_UP');

  Result := THotkeyManager.BuildHotkeyString(VK_DOWN, [ssCtrl]);
  Assert.AreEqual('Ctrl+Down', Result, 'BuildHotkeyString should return Down for VK_DOWN');

  Result := THotkeyManager.BuildHotkeyString(VK_LEFT, [ssCtrl]);
  Assert.AreEqual('Ctrl+Left', Result, 'BuildHotkeyString should return Left for VK_LEFT');

  Result := THotkeyManager.BuildHotkeyString(VK_RIGHT, [ssCtrl]);
  Assert.AreEqual('Ctrl+Right', Result, 'BuildHotkeyString should return Right for VK_RIGHT');
end;

procedure TTestHotkeyManager.BuildHotkeyString_NavigationKeys_ReturnsCorrectNames;
var
  Result: string;
begin
  Result := THotkeyManager.BuildHotkeyString(VK_HOME, [ssCtrl]);
  Assert.AreEqual('Ctrl+Home', Result, 'BuildHotkeyString should return Home for VK_HOME');

  Result := THotkeyManager.BuildHotkeyString(VK_END, [ssCtrl]);
  Assert.AreEqual('Ctrl+End', Result, 'BuildHotkeyString should return End for VK_END');

  Result := THotkeyManager.BuildHotkeyString(VK_PRIOR, [ssCtrl]);
  Assert.AreEqual('Ctrl+PageUp', Result, 'BuildHotkeyString should return PageUp for VK_PRIOR');

  Result := THotkeyManager.BuildHotkeyString(VK_NEXT, [ssCtrl]);
  Assert.AreEqual('Ctrl+PageDown', Result, 'BuildHotkeyString should return PageDown for VK_NEXT');

  Result := THotkeyManager.BuildHotkeyString(VK_INSERT, [ssCtrl]);
  Assert.AreEqual('Ctrl+Insert', Result, 'BuildHotkeyString should return Insert for VK_INSERT');

  Result := THotkeyManager.BuildHotkeyString(VK_DELETE, [ssCtrl]);
  Assert.AreEqual('Ctrl+Delete', Result, 'BuildHotkeyString should return Delete for VK_DELETE');
end;

{ BuildHotkeyString Modifier-Only Tests }

procedure TTestHotkeyManager.BuildHotkeyString_ModifierKeyOnly_ReturnsEmpty;
var
  Result: string;
begin
  // Pressing just Ctrl (VK_CONTROL) should not produce a valid hotkey
  Result := THotkeyManager.BuildHotkeyString(VK_CONTROL, [ssCtrl]);
  Assert.AreEqual('', Result, 'BuildHotkeyString should return empty for modifier key only');

  // Same for Alt
  Result := THotkeyManager.BuildHotkeyString(VK_MENU, [ssAlt]);
  Assert.AreEqual('', Result, 'BuildHotkeyString should return empty for Alt key only');

  // Same for Shift
  Result := THotkeyManager.BuildHotkeyString(VK_SHIFT, [ssShift]);
  Assert.AreEqual('', Result, 'BuildHotkeyString should return empty for Shift key only');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHotkeyManager);

end.
