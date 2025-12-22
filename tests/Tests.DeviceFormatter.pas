{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TDeviceFormatter Tests                          }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.DeviceFormatter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Bluetooth.Types,
  App.ConfigInterfaces,
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
  Assert.AreEqual('Never', TDeviceFormatter.FormatLastSeenAbsolute(0));
end;

procedure TDeviceFormatterTests.FormatLastSeenAbsolute_NegativeDateTime_ReturnsNever;
begin
  Assert.AreEqual('Never', TDeviceFormatter.FormatLastSeenAbsolute(-1));
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

initialization
  TDUnitX.RegisterTestFixture(TDeviceFormatterTests);

end.
