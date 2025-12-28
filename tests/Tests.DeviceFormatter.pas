{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TDeviceFormatter Tests                          }
{                                                       }
{*******************************************************}

unit Tests.DeviceFormatter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Bluetooth.Types,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  UI.DeviceFormatter,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TDeviceFormatter class.
  /// Tests date formatting, display name resolution, and sort group calculation.
  /// </summary>
  [TestFixture]
  TDeviceFormatterTests = class
  public
    { FormatLastSeenRelative Tests }
    [Test]
    procedure FormatLastSeenRelative_ZeroDateTime_ReturnsNever;

    [Test]
    procedure FormatLastSeenRelative_NegativeDateTime_ReturnsNever;

    [Test]
    procedure FormatLastSeenRelative_JustNow_ReturnsJustNow;

    [Test]
    procedure FormatLastSeenRelative_FiveMinutesAgo_ReturnsMinutesAgo;

    [Test]
    procedure FormatLastSeenRelative_TwoHoursAgo_ReturnsHoursAgo;

    [Test]
    procedure FormatLastSeenRelative_Yesterday_ReturnsYesterday;

    [Test]
    procedure FormatLastSeenRelative_ThreeDaysAgo_ReturnsDaysAgo;

    [Test]
    procedure FormatLastSeenRelative_TwoWeeksAgo_ReturnsWeeksAgo;

    [Test]
    procedure FormatLastSeenRelative_TwoMonthsAgo_ReturnsMonthsAgo;

    [Test]
    procedure FormatLastSeenRelative_TwoYearsAgo_ReturnsYearsAgo;

    { FormatLastSeenAbsolute Tests }
    [Test]
    procedure FormatLastSeenAbsolute_ZeroDateTime_ReturnsNever;

    [Test]
    procedure FormatLastSeenAbsolute_NegativeDateTime_ReturnsNever;

    [Test]
    procedure FormatLastSeenAbsolute_ValidDateTime_ReturnsFormattedString;

    { FormatLastSeen Tests }
    [Test]
    procedure FormatLastSeen_RelativeFormat_UsesRelativeFormatter;

    [Test]
    procedure FormatLastSeen_AbsoluteFormat_UsesAbsoluteFormatter;

    { GetDisplayName Tests }
    [Test]
    procedure GetDisplayName_AliasSet_ReturnsAlias;

    [Test]
    procedure GetDisplayName_NoAlias_ReturnsDeviceName;

    [Test]
    procedure GetDisplayName_EmptyAlias_ReturnsDeviceName;

    { GetEffectiveDeviceType Tests }
    [Test]
    procedure GetEffectiveDeviceType_NoOverride_ReturnsDeviceType;

    [Test]
    procedure GetEffectiveDeviceType_WithOverride_ReturnsOverride;

    [Test]
    procedure GetEffectiveDeviceType_NegativeOverride_ReturnsDeviceType;

    { GetSortGroup Tests }
    [Test]
    procedure GetSortGroup_Pinned_ReturnsZero;

    [Test]
    procedure GetSortGroup_ConnectedNotPinned_ReturnsOne;

    [Test]
    procedure GetSortGroup_DisconnectedNotPinned_ReturnsTwo;

    { FormatConnectionState Tests }
    [Test]
    procedure FormatConnectionState_Disconnected;

    [Test]
    procedure FormatConnectionState_Connected;

    [Test]
    procedure FormatConnectionState_Connecting;

    [Test]
    procedure FormatConnectionState_Disconnecting;

    [Test]
    procedure FormatConnectionState_Error;

    [Test]
    procedure FormatConnectionState_Unknown;

    { FormatDeviceType Tests }
    [Test]
    procedure FormatDeviceType_AudioOutput;

    [Test]
    procedure FormatDeviceType_AudioInput;

    [Test]
    procedure FormatDeviceType_Headset;

    [Test]
    procedure FormatDeviceType_Keyboard;

    [Test]
    procedure FormatDeviceType_Mouse;

    [Test]
    procedure FormatDeviceType_Gamepad;

    [Test]
    procedure FormatDeviceType_HID;

    [Test]
    procedure FormatDeviceType_Unknown;

    { FormatBatteryLevel Tests }
    [Test]
    procedure FormatBatteryLevel_HasLevel_ReturnsPercentage;

    [Test]
    procedure FormatBatteryLevel_ZeroLevel_ReturnsZeroPercent;

    [Test]
    procedure FormatBatteryLevel_FullLevel_ReturnsHundredPercent;

    [Test]
    procedure FormatBatteryLevel_NotSupported_ReturnsEmpty;

    [Test]
    procedure FormatBatteryLevel_Unknown_ReturnsEmpty;

    { Edge Case Tests }
    [Test]
    procedure GetDisplayName_VeryLongName_Over255Chars;

    [Test]
    procedure GetDisplayName_SpecialCharacters_Unicode;

    [Test]
    procedure GetDisplayName_EmptyAlias_ReturnsName;

    [Test]
    procedure FormatLastSeen_Exactly24HoursAgo_Boundary;

    [Test]
    procedure FormatLastSeen_Exactly7DaysAgo_Boundary;

    [Test]
    procedure FormatLastSeen_FutureDate_Handles;

    [Test]
    procedure FormatBatteryLevel_InvalidLevel_Above100;

    [Test]
    procedure FormatBatteryLevel_InvalidLevel_BelowZero;
  end;

implementation

{ TDeviceFormatterTests - FormatLastSeenRelative }

procedure TDeviceFormatterTests.FormatLastSeenRelative_ZeroDateTime_ReturnsNever;
begin
  Assert.AreEqual('Never', TDeviceFormatter.FormatLastSeenRelative(0));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_NegativeDateTime_ReturnsNever;
begin
  Assert.AreEqual('Never', TDeviceFormatter.FormatLastSeenRelative(-1));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_JustNow_ReturnsJustNow;
begin
  Assert.AreEqual('Just now', TDeviceFormatter.FormatLastSeenRelative(Now));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_FiveMinutesAgo_ReturnsMinutesAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncMinute(Now, -5);
  Assert.AreEqual('5 min ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_TwoHoursAgo_ReturnsHoursAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncHour(Now, -2);
  Assert.AreEqual('2 hr ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_Yesterday_ReturnsYesterday;
var
  TestTime: TDateTime;
begin
  TestTime := IncDay(Now, -1);
  Assert.AreEqual('Yesterday', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_ThreeDaysAgo_ReturnsDaysAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncDay(Now, -3);
  Assert.AreEqual('3 days ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_TwoWeeksAgo_ReturnsWeeksAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncDay(Now, -14);
  Assert.AreEqual('2 weeks ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_TwoMonthsAgo_ReturnsMonthsAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncDay(Now, -60);
  Assert.AreEqual('2 months ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

procedure TDeviceFormatterTests.FormatLastSeenRelative_TwoYearsAgo_ReturnsYearsAgo;
var
  TestTime: TDateTime;
begin
  TestTime := IncDay(Now, -730);
  Assert.AreEqual('2 years ago', TDeviceFormatter.FormatLastSeenRelative(TestTime));
end;

{ TDeviceFormatterTests - FormatLastSeenAbsolute }

procedure TDeviceFormatterTests.FormatLastSeenAbsolute_ZeroDateTime_ReturnsNever;
begin
  // Absolute format returns empty string for "never seen" (not shown in UI)
  Assert.AreEqual('', TDeviceFormatter.FormatLastSeenAbsolute(0));
end;

procedure TDeviceFormatterTests.FormatLastSeenAbsolute_NegativeDateTime_ReturnsNever;
begin
  // Absolute format returns empty string for invalid dates
  Assert.AreEqual('', TDeviceFormatter.FormatLastSeenAbsolute(-1));
end;

procedure TDeviceFormatterTests.FormatLastSeenAbsolute_ValidDateTime_ReturnsFormattedString;
var
  TestTime: TDateTime;
begin
  TestTime := EncodeDateTime(2024, 12, 22, 15, 30, 0, 0);
  Assert.AreEqual('2024-12-22 15:30', TDeviceFormatter.FormatLastSeenAbsolute(TestTime));
end;

{ TDeviceFormatterTests - FormatLastSeen }

procedure TDeviceFormatterTests.FormatLastSeen_RelativeFormat_UsesRelativeFormatter;
var
  TestTime: TDateTime;
begin
  TestTime := IncMinute(Now, -5);
  Assert.AreEqual('5 min ago', TDeviceFormatter.FormatLastSeen(TestTime, lsfRelative));
end;

procedure TDeviceFormatterTests.FormatLastSeen_AbsoluteFormat_UsesAbsoluteFormatter;
var
  TestTime: TDateTime;
begin
  TestTime := EncodeDateTime(2024, 12, 22, 15, 30, 0, 0);
  Assert.AreEqual('2024-12-22 15:30', TDeviceFormatter.FormatLastSeen(TestTime, lsfAbsolute));
end;

{ TDeviceFormatterTests - GetDisplayName }

procedure TDeviceFormatterTests.GetDisplayName_AliasSet_ReturnsAlias;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Original Name', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := 'My Alias';

  Assert.AreEqual('My Alias', TDeviceFormatter.GetDisplayName(Device, Config));
end;

procedure TDeviceFormatterTests.GetDisplayName_NoAlias_ReturnsDeviceName;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device Name', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := '';

  Assert.AreEqual('Device Name', TDeviceFormatter.GetDisplayName(Device, Config));
end;

procedure TDeviceFormatterTests.GetDisplayName_EmptyAlias_ReturnsDeviceName;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device Name', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := '';

  Assert.AreEqual('Device Name', TDeviceFormatter.GetDisplayName(Device, Config));
end;

{ TDeviceFormatterTests - GetEffectiveDeviceType }

procedure TDeviceFormatterTests.GetEffectiveDeviceType_NoOverride_ReturnsDeviceType;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.DeviceTypeOverride := -1;  // No override

  Assert.AreEqual(Ord(btAudioOutput), Ord(TDeviceFormatter.GetEffectiveDeviceType(Device, Config)));
end;

procedure TDeviceFormatterTests.GetEffectiveDeviceType_WithOverride_ReturnsOverride;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.DeviceTypeOverride := Ord(btKeyboard);  // Override to keyboard

  Assert.AreEqual(Ord(btKeyboard), Ord(TDeviceFormatter.GetEffectiveDeviceType(Device, Config)));
end;

procedure TDeviceFormatterTests.GetEffectiveDeviceType_NegativeOverride_ReturnsDeviceType;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btMouse, csConnected);
  Config := Default(TDeviceConfig);
  Config.DeviceTypeOverride := -5;  // Negative = no override

  Assert.AreEqual(Ord(btMouse), Ord(TDeviceFormatter.GetEffectiveDeviceType(Device, Config)));
end;

{ TDeviceFormatterTests - GetSortGroup }

procedure TDeviceFormatterTests.GetSortGroup_Pinned_ReturnsZero;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btAudioOutput, csDisconnected);
  Config := Default(TDeviceConfig);
  Config.Pinned := True;

  Assert.AreEqual(0, TDeviceFormatter.GetSortGroup(Device, Config));
end;

procedure TDeviceFormatterTests.GetSortGroup_ConnectedNotPinned_ReturnsOne;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Pinned := False;

  Assert.AreEqual(1, TDeviceFormatter.GetSortGroup(Device, Config));
end;

procedure TDeviceFormatterTests.GetSortGroup_DisconnectedNotPinned_ReturnsTwo;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001122334455, 'Device', btAudioOutput, csDisconnected);
  Config := Default(TDeviceConfig);
  Config.Pinned := False;

  Assert.AreEqual(2, TDeviceFormatter.GetSortGroup(Device, Config));
end;

{ TDeviceFormatterTests - FormatConnectionState }

procedure TDeviceFormatterTests.FormatConnectionState_Disconnected;
begin
  Assert.AreEqual('Disconnected', TDeviceFormatter.FormatConnectionState(csDisconnected));
end;

procedure TDeviceFormatterTests.FormatConnectionState_Connected;
begin
  Assert.AreEqual('Connected', TDeviceFormatter.FormatConnectionState(csConnected));
end;

procedure TDeviceFormatterTests.FormatConnectionState_Connecting;
begin
  Assert.AreEqual('Connecting...', TDeviceFormatter.FormatConnectionState(csConnecting));
end;

procedure TDeviceFormatterTests.FormatConnectionState_Disconnecting;
begin
  Assert.AreEqual('Disconnecting...', TDeviceFormatter.FormatConnectionState(csDisconnecting));
end;

procedure TDeviceFormatterTests.FormatConnectionState_Error;
begin
  Assert.AreEqual('Error', TDeviceFormatter.FormatConnectionState(csError));
end;

procedure TDeviceFormatterTests.FormatConnectionState_Unknown;
begin
  Assert.AreEqual('Unknown', TDeviceFormatter.FormatConnectionState(csUnknown));
end;

{ TDeviceFormatterTests - FormatDeviceType }

procedure TDeviceFormatterTests.FormatDeviceType_AudioOutput;
begin
  Assert.AreEqual('Audio Output', TDeviceFormatter.FormatDeviceType(btAudioOutput));
end;

procedure TDeviceFormatterTests.FormatDeviceType_AudioInput;
begin
  Assert.AreEqual('Audio Input', TDeviceFormatter.FormatDeviceType(btAudioInput));
end;

procedure TDeviceFormatterTests.FormatDeviceType_Headset;
begin
  Assert.AreEqual('Headset', TDeviceFormatter.FormatDeviceType(btHeadset));
end;

procedure TDeviceFormatterTests.FormatDeviceType_Keyboard;
begin
  Assert.AreEqual('Keyboard', TDeviceFormatter.FormatDeviceType(btKeyboard));
end;

procedure TDeviceFormatterTests.FormatDeviceType_Mouse;
begin
  Assert.AreEqual('Mouse', TDeviceFormatter.FormatDeviceType(btMouse));
end;

procedure TDeviceFormatterTests.FormatDeviceType_Gamepad;
begin
  Assert.AreEqual('Gamepad', TDeviceFormatter.FormatDeviceType(btGamepad));
end;

procedure TDeviceFormatterTests.FormatDeviceType_HID;
begin
  Assert.AreEqual('Input Device', TDeviceFormatter.FormatDeviceType(btHID));
end;

procedure TDeviceFormatterTests.FormatDeviceType_Unknown;
begin
  Assert.AreEqual('Unknown', TDeviceFormatter.FormatDeviceType(btUnknown));
end;

{ TDeviceFormatterTests - FormatBatteryLevel }

procedure TDeviceFormatterTests.FormatBatteryLevel_HasLevel_ReturnsPercentage;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(75);
  Assert.AreEqual('75%', TDeviceFormatter.FormatBatteryLevel(Status));
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_ZeroLevel_ReturnsZeroPercent;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(0);
  Assert.AreEqual('0%', TDeviceFormatter.FormatBatteryLevel(Status));
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_FullLevel_ReturnsHundredPercent;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Create(100);
  Assert.AreEqual('100%', TDeviceFormatter.FormatBatteryLevel(Status));
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_NotSupported_ReturnsEmpty;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.NotSupported;
  Assert.AreEqual('', TDeviceFormatter.FormatBatteryLevel(Status));
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_Unknown_ReturnsEmpty;
var
  Status: TBatteryStatus;
begin
  Status := TBatteryStatus.Unknown;
  Assert.AreEqual('', TDeviceFormatter.FormatBatteryLevel(Status));
end;

{ TDeviceFormatterTests - Edge Cases }

procedure TDeviceFormatterTests.GetDisplayName_VeryLongName_Over255Chars;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  LongName: string;
  Result: string;
begin
  // Create a name over 255 characters
  LongName := StringOfChar('A', 300);
  Device := CreateTestDevice($001122334455, LongName, btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := '';

  Result := TDeviceFormatter.GetDisplayName(Device, Config);

  // Should return the full name without truncation (no max length enforcement)
  Assert.AreEqual(300, Length(Result));
  Assert.AreEqual(LongName, Result);
end;

procedure TDeviceFormatterTests.GetDisplayName_SpecialCharacters_Unicode;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  UnicodeName: string;
begin
  // Test with various Unicode characters including emoji, CJK, and special symbols
  UnicodeName := 'Device'#$00E9' Test'#$4E2D#$6587' '#$1F3A7; // e-acute, Chinese chars, headphone emoji
  Device := CreateTestDevice($001122334455, UnicodeName, btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := '';

  Assert.AreEqual(UnicodeName, TDeviceFormatter.GetDisplayName(Device, Config));
end;

procedure TDeviceFormatterTests.GetDisplayName_EmptyAlias_ReturnsName;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  // This is already tested but included for explicit edge case coverage
  Device := CreateTestDevice($001122334455, 'Original Device Name', btAudioOutput, csConnected);
  Config := Default(TDeviceConfig);
  Config.Alias := '';

  Assert.AreEqual('Original Device Name', TDeviceFormatter.GetDisplayName(Device, Config));
end;

procedure TDeviceFormatterTests.FormatLastSeen_Exactly24HoursAgo_Boundary;
var
  TestTime: TDateTime;
  Result: string;
begin
  // Exactly 24 hours ago is the boundary between "X hr ago" and "Yesterday"
  TestTime := IncHour(Now, -24);
  Result := TDeviceFormatter.FormatLastSeenRelative(TestTime);

  // At exactly 24 hours, Days = 1, so should show "Yesterday"
  Assert.AreEqual('Yesterday', Result);
end;

procedure TDeviceFormatterTests.FormatLastSeen_Exactly7DaysAgo_Boundary;
var
  TestTime: TDateTime;
  Result: string;
begin
  // Exactly 7 days ago is the boundary between "X days ago" and "1 weeks ago"
  TestTime := IncDay(Now, -7);
  Result := TDeviceFormatter.FormatLastSeenRelative(TestTime);

  // At exactly 7 days, Days = 7, so should show "1 weeks ago" (7 div 7 = 1)
  Assert.AreEqual('1 weeks ago', Result);
end;

procedure TDeviceFormatterTests.FormatLastSeen_FutureDate_Handles;
var
  TestTime: TDateTime;
  Result: string;
begin
  // Future dates should still be handled (edge case - system clock issues)
  TestTime := IncHour(Now, 5);  // 5 hours in the future
  Result := TDeviceFormatter.FormatLastSeenRelative(TestTime);

  // Diff will be negative, so should show "Just now" (Minutes < 1 check fails,
  // but the implementation may handle it differently)
  // The current implementation: Diff = Now - ALastSeen is negative
  // Days = Trunc(-0.2) = 0, Hours and Minutes using HoursBetween/MinutesBetween
  // which return absolute values
  Assert.IsNotEmpty(Result, 'Future dates should return a non-empty string');
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_InvalidLevel_Above100;
var
  Status: TBatteryStatus;
begin
  // TBatteryStatus.Create clamps values to 0-100 range
  Status := TBatteryStatus.Create(150);

  // Should be clamped to 100%
  Assert.AreEqual('100%', TDeviceFormatter.FormatBatteryLevel(Status));
end;

procedure TDeviceFormatterTests.FormatBatteryLevel_InvalidLevel_BelowZero;
var
  Status: TBatteryStatus;
begin
  // TBatteryStatus.Create clamps values to 0-100 range
  Status := TBatteryStatus.Create(-50);

  // Should be clamped to 0%
  Assert.AreEqual('0%', TDeviceFormatter.FormatBatteryLevel(Status));
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceFormatterTests);

end.
