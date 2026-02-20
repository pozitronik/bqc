{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TSystemIconCache Win7 Icon Constants Tests      }
{                                                       }
{       Verifies Win7 system icon definitions use       }
{       correct DLL and indices after c639c11 bug fix.  }
{                                                       }
{*******************************************************}

unit Tests.DeviceListTypes;

interface

uses
  DUnitX.TestFramework,
  UI.DeviceListTypes,
  Bluetooth.Types;

type
  /// <summary>
  /// Test fixture for Win7 system icon constant values.
  /// Verifies that icon definitions use correct DLL files and indices
  /// after fixing BUG #4 introduced in commit c639c11.
  /// </summary>
  [TestFixture]
  TSystemIconConstantsTests = class
  public
    { Win7 Icon DLL Tests - BUG #4 Regression Prevention }
    [Test]
    procedure Win7Icons_UseDdoresDll_NotImageresDll;

    [Test]
    procedure HeadphoneIcon_HasCorrectDllAndIndex;

    [Test]
    procedure KeyboardIcon_HasCorrectDllAndIndex;

    [Test]
    procedure MouseIcon_HasCorrectDllAndIndex;

    [Test]
    procedure PhoneIcon_HasCorrectDllAndIndex;

    [Test]
    procedure ComputerIcon_HasCorrectDllAndIndex;

    [Test]
    procedure GamepadIcon_HasCorrectDllAndIndex;

    [Test]
    procedure GamepadIcon_IsNotMissing;
  end;

implementation

{ TSystemIconConstantsTests }

procedure TSystemIconConstantsTests.Win7Icons_UseDdoresDll_NotImageresDll;
begin
  // BUG #4: During c639c11 extraction, Win7 icons were incorrectly changed
  // from ddores.dll to imageres.dll, breaking Windows 7 icon display.
  // ddores.dll (Device Center resources) contains proper device-specific icons.

  Assert.AreEqual('ddores.dll', SYSICON_HEADPHONE_FILE,
    'Headphone should use ddores.dll (Device Center), not imageres.dll');
  Assert.AreEqual('ddores.dll', SYSICON_KEYBOARD_FILE,
    'Keyboard should use ddores.dll (Device Center), not imageres.dll');
  Assert.AreEqual('ddores.dll', SYSICON_MOUSE_FILE,
    'Mouse should use ddores.dll (Device Center), not imageres.dll');
  Assert.AreEqual('ddores.dll', SYSICON_PHONE_FILE,
    'Phone should use ddores.dll (Device Center), not imageres.dll');
  Assert.AreEqual('ddores.dll', SYSICON_COMPUTER_FILE,
    'Computer should use ddores.dll (Device Center), not imageres.dll');
  Assert.AreEqual('ddores.dll', SYSICON_GAMEPAD_FILE,
    'Gamepad should use ddores.dll (Device Center), not imageres.dll');
end;

procedure TSystemIconConstantsTests.HeadphoneIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_HEADPHONE_FILE);
  Assert.AreEqual(2, SYSICON_HEADPHONE_INDEX,
    'Headphone icon index should be 2 in ddores.dll');
end;

procedure TSystemIconConstantsTests.KeyboardIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_KEYBOARD_FILE);
  Assert.AreEqual(26, SYSICON_KEYBOARD_INDEX,
    'Keyboard icon index should be 26 in ddores.dll');
end;

procedure TSystemIconConstantsTests.MouseIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_MOUSE_FILE);
  Assert.AreEqual(27, SYSICON_MOUSE_INDEX,
    'Mouse icon index should be 27 in ddores.dll');
end;

procedure TSystemIconConstantsTests.PhoneIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_PHONE_FILE);
  Assert.AreEqual(11, SYSICON_PHONE_INDEX,
    'Phone icon index should be 11 in ddores.dll');
end;

procedure TSystemIconConstantsTests.ComputerIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_COMPUTER_FILE);
  Assert.AreEqual(12, SYSICON_COMPUTER_INDEX,
    'Computer icon index should be 12 in ddores.dll');
end;

procedure TSystemIconConstantsTests.GamepadIcon_HasCorrectDllAndIndex;
begin
  // Correct values from before c639c11 extraction
  Assert.AreEqual('ddores.dll', SYSICON_GAMEPAD_FILE);
  Assert.AreEqual(25, SYSICON_GAMEPAD_INDEX,
    'Gamepad icon index should be 25 in ddores.dll');
end;

procedure TSystemIconConstantsTests.GamepadIcon_IsNotMissing;
begin
  // BUG #4: Gamepad icon constants were completely removed during c639c11 extraction
  // This test ensures they remain defined and have correct values

  // If constants are missing, this will fail to compile
  // Verify they're defined with correct values
  Assert.IsNotEmpty(SYSICON_GAMEPAD_FILE,
    'SYSICON_GAMEPAD_FILE must be defined');
  Assert.AreNotEqual(0, SYSICON_GAMEPAD_INDEX,
    'SYSICON_GAMEPAD_INDEX must be defined with non-zero value');

  // Verify actual values match expected
  Assert.AreEqual('ddores.dll', SYSICON_GAMEPAD_FILE);
  Assert.AreEqual(25, SYSICON_GAMEPAD_INDEX);
end;

end.
