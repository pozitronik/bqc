{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       TDeviceItemRenderer Icon Character Tests        }
{                                                       }
{       Verifies Segoe MDL2 Assets icon character       }
{       codes are correct after c639c11 bug fix.        }
{                                                       }
{*******************************************************}

unit Tests.DeviceItemRenderer;

interface

uses
  DUnitX.TestFramework,
  UI.DeviceItemRenderer;

type
  /// <summary>
  /// Test fixture for Segoe MDL2 Assets icon character constants.
  /// Verifies that icon character codes use correct Unicode values
  /// after fixing BUG #6 introduced in commit c639c11.
  /// </summary>
  [TestFixture]
  TDeviceIconCharacterTests = class
  public
    { Segoe MDL2 Icon Character Tests - BUG #6 Regression Prevention }
    [Test]
    procedure PhoneIcon_HasCorrectCharacterCode;

    [Test]
    procedure KeyboardIcon_HasCorrectCharacterCode;

    [Test]
    procedure PinIcon_HasImprovedCharacterCode;

    [Test]
    procedure AllIconConstants_AreDefined;
  end;

implementation

{ TDeviceIconCharacterTests }

procedure TDeviceIconCharacterTests.PhoneIcon_HasCorrectCharacterCode;
begin
  // BUG #6: During c639c11 extraction, ICON_PHONE was incorrectly changed
  // from #$E8EA to #$E717, resulting in wrong visual icon on Windows 10+.
  // The original #$E8EA (0xE8EA) displays the correct phone icon.

  Assert.AreEqual(#$E8EA, ICON_PHONE,
    'ICON_PHONE should be #$E8EA (original Segoe MDL2 character), not #$E717');
end;

procedure TDeviceIconCharacterTests.KeyboardIcon_HasCorrectCharacterCode;
begin
  // BUG #6: During c639c11 extraction, ICON_KEYBOARD was incorrectly changed
  // from #$E765 to #$E92E, resulting in wrong visual icon on Windows 10+.
  // The original #$E765 (0xE765) displays the correct keyboard icon.

  Assert.AreEqual(#$E765, ICON_KEYBOARD,
    'ICON_KEYBOARD should be #$E765 (original Segoe MDL2 character), not #$E92E');
end;

procedure TDeviceIconCharacterTests.PinIcon_HasImprovedCharacterCode;
begin
  // BUG #5: PIN icon was changed from #$E718 to #$E840 during c639c11.
  // User confirmed #$E840 is an improvement (better visual appearance).
  // This test documents the intentional change and prevents accidental reversion.

  Assert.AreEqual(#$E840, ICON_PIN,
    'ICON_PIN should remain #$E840 (improved icon, confirmed by user)');
end;

procedure TDeviceIconCharacterTests.AllIconConstants_AreDefined;
begin
  // Verify all icon constants are defined (compilation test)
  Assert.IsTrue(ICON_BLUETOOTH <> #$0000, 'ICON_BLUETOOTH must be defined');
  Assert.IsTrue(ICON_HEADPHONE <> #$0000, 'ICON_HEADPHONE must be defined');
  Assert.IsTrue(ICON_MICROPHONE <> #$0000, 'ICON_MICROPHONE must be defined');
  Assert.IsTrue(ICON_COMPUTER <> #$0000, 'ICON_COMPUTER must be defined');
  Assert.IsTrue(ICON_PHONE <> #$0000, 'ICON_PHONE must be defined');
  Assert.IsTrue(ICON_KEYBOARD <> #$0000, 'ICON_KEYBOARD must be defined');
  Assert.IsTrue(ICON_MOUSE <> #$0000, 'ICON_MOUSE must be defined');
  Assert.IsTrue(ICON_GAMEPAD <> #$0000, 'ICON_GAMEPAD must be defined');
  Assert.IsTrue(ICON_PIN <> #$0000, 'ICON_PIN must be defined');
  Assert.IsTrue(ICON_GENERIC <> #$0000, 'ICON_GENERIC must be defined');
end;

end.
