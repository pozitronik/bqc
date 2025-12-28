{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Bluetooth.DeviceConverter Unit Tests            }
{                                                       }
{       Tests device conversion functions that          }
{       transform Windows API structures to domain      }
{       TBluetoothDeviceInfo records.                   }
{                                                       }
{*******************************************************}

unit Tests.DeviceConverter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  System.Math,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.WinAPI,
  Bluetooth.DeviceConverter;

type
  /// <summary>
  /// Tests for SafeSystemTimeToDateTime function.
  /// </summary>
  [TestFixture]
  TSafeSystemTimeToDateTimeTests = class
  public
    [Test]
    procedure ValidDate_ReturnsCorrectDateTime;

    [Test]
    procedure ZeroYear_ReturnsZero;

    [Test]
    procedure YearBefore1900_ReturnsZero;

    [Test]
    procedure YearAfter2100_ReturnsZero;

    [Test]
    procedure InvalidMonth_ReturnsZero;

    [Test]
    procedure InvalidDay_ReturnsZero;

    [Test]
    procedure MinValidYear_ReturnsDateTime;

    [Test]
    procedure MaxValidYear_ReturnsDateTime;

    [Test]
    procedure PreservesTimeComponents;
  end;

  /// <summary>
  /// Tests for ConvertBluetoothDeviceInfo function.
  /// Converts BLUETOOTH_DEVICE_INFO (from BluetoothFindFirstDevice etc.)
  /// </summary>
  [TestFixture]
  TConvertBluetoothDeviceInfoTests = class
  private
    function CreateTestDeviceInfo(
      AAddress: UInt64;
      const AName: string;
      AClassOfDevice: ULONG;
      AConnected, ARemembered, AAuthenticated: Boolean
    ): BLUETOOTH_DEVICE_INFO;
  public
    [Test]
    procedure Convert_SetsAddressCorrectly;

    [Test]
    procedure Convert_SetsNameCorrectly;

    [Test]
    procedure Convert_SetsClassOfDevice;

    [Test]
    procedure Convert_Connected_SetsConnectedState;

    [Test]
    procedure Convert_NotConnected_SetsDisconnectedState;

    [Test]
    procedure Convert_Remembered_SetsPairedTrue;

    [Test]
    procedure Convert_NotRemembered_SetsPairedFalse;

    [Test]
    procedure Convert_Authenticated_SetsAuthenticatedTrue;

    [Test]
    procedure Convert_DeterminesDeviceType_Audio;

    [Test]
    procedure Convert_DeterminesDeviceType_Keyboard;

    [Test]
    procedure Convert_ValidLastSeen_SetsDateTime;

    [Test]
    procedure Convert_InvalidLastSeen_SetsZero;

    [Test]
    procedure Convert_ValidLastUsed_SetsDateTime;

    [Test]
    procedure Convert_EmptyName_HandlesCorrectly;
  end;

implementation

{ TSafeSystemTimeToDateTimeTests }

procedure TSafeSystemTimeToDateTimeTests.ValidDate_ReturnsCorrectDateTime;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2024;
  SysTime.wMonth := 6;
  SysTime.wDay := 15;
  SysTime.wHour := 10;
  SysTime.wMinute := 30;
  SysTime.wSecond := 45;
  SysTime.wMilliseconds := 500;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(2024, YearOf(Result));
  Assert.AreEqual(6, MonthOf(Result));
  Assert.AreEqual(15, DayOf(Result));
end;

procedure TSafeSystemTimeToDateTimeTests.ZeroYear_ReturnsZero;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 0;
  SysTime.wMonth := 6;
  SysTime.wDay := 15;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(Double(0), Double(Result), 0.0001);
end;

procedure TSafeSystemTimeToDateTimeTests.YearBefore1900_ReturnsZero;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 1899;
  SysTime.wMonth := 6;
  SysTime.wDay := 15;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(Double(0), Double(Result), 0.0001);
end;

procedure TSafeSystemTimeToDateTimeTests.YearAfter2100_ReturnsZero;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2101;
  SysTime.wMonth := 6;
  SysTime.wDay := 15;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(Double(0), Double(Result), 0.0001);
end;

procedure TSafeSystemTimeToDateTimeTests.InvalidMonth_ReturnsZero;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2024;
  SysTime.wMonth := 13;  // Invalid month
  SysTime.wDay := 15;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(Double(0), Double(Result), 0.0001);
end;

procedure TSafeSystemTimeToDateTimeTests.InvalidDay_ReturnsZero;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2024;
  SysTime.wMonth := 6;
  SysTime.wDay := 32;  // Invalid day

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(Double(0), Double(Result), 0.0001);
end;

procedure TSafeSystemTimeToDateTimeTests.MinValidYear_ReturnsDateTime;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 1900;
  SysTime.wMonth := 1;
  SysTime.wDay := 1;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(1900, YearOf(Result));
end;

procedure TSafeSystemTimeToDateTimeTests.MaxValidYear_ReturnsDateTime;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2100;
  SysTime.wMonth := 12;
  SysTime.wDay := 31;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(2100, YearOf(Result));
end;

procedure TSafeSystemTimeToDateTimeTests.PreservesTimeComponents;
var
  SysTime: TSystemTime;
  Result: TDateTime;
begin
  FillChar(SysTime, SizeOf(SysTime), 0);
  SysTime.wYear := 2024;
  SysTime.wMonth := 6;
  SysTime.wDay := 15;
  SysTime.wHour := 14;
  SysTime.wMinute := 35;
  SysTime.wSecond := 22;
  SysTime.wMilliseconds := 123;

  Result := SafeSystemTimeToDateTime(SysTime);

  Assert.AreEqual(14, HourOf(Result));
  Assert.AreEqual(35, MinuteOf(Result));
  Assert.AreEqual(22, SecondOf(Result));
  Assert.AreEqual(123, MilliSecondOf(Result));
end;

{ TConvertBluetoothDeviceInfoTests }

function TConvertBluetoothDeviceInfoTests.CreateTestDeviceInfo(
  AAddress: UInt64;
  const AName: string;
  AClassOfDevice: ULONG;
  AConnected, ARemembered, AAuthenticated: Boolean
): BLUETOOTH_DEVICE_INFO;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.dwSize := SizeOf(BLUETOOTH_DEVICE_INFO);
  Result.Address.ullLong := AAddress;
  Result.ulClassOfDevice := AClassOfDevice;
  Result.fConnected := AConnected;
  Result.fRemembered := ARemembered;
  Result.fAuthenticated := AAuthenticated;

  // Copy name (max 248 chars)
  for I := 1 to Min(Length(AName), BLUETOOTH_MAX_NAME_SIZE) do
    Result.szName[I - 1] := AName[I];
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_SetsAddressCorrectly;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo($112233445566, 'Test', 0, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(UInt64($112233445566), DeviceInfo.AddressInt);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_SetsNameCorrectly;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'My Headphones', 0, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual('My Headphones', DeviceInfo.Name);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_SetsClassOfDevice;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', $240404, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Cardinal($240404), DeviceInfo.ClassOfDevice);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_Connected_SetsConnectedState;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, True, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Integer(csConnected), Integer(DeviceInfo.ConnectionState));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_NotConnected_SetsDisconnectedState;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Integer(csDisconnected), Integer(DeviceInfo.ConnectionState));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_Remembered_SetsPairedTrue;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, True, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.IsTrue(DeviceInfo.IsPaired);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_NotRemembered_SetsPairedFalse;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.IsFalse(DeviceInfo.IsPaired);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_Authenticated_SetsAuthenticatedTrue;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, True);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.IsTrue(DeviceInfo.IsAuthenticated);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_DeterminesDeviceType_Audio;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // Audio class: Major class 4 (Audio/Video) at bits 8-12, Minor class 6 (Headphones) at bits 2-7
  // CoD = ($04 shl 8) or ($06 shl 2) = $0400 or $18 = $0418
  WinInfo := CreateTestDeviceInfo(0, 'Headphones', $0418, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Integer(btAudioOutput), Integer(DeviceInfo.DeviceType));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_DeterminesDeviceType_Keyboard;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  // Peripheral class: Major class 5 at bits 8-12, Keyboard ($10) at bits 6-7 of minor
  // CoD = ($05 shl 8) or ($10 shl 2) = $0500 or $40 = $0540
  WinInfo := CreateTestDeviceInfo(0, 'Keyboard', $0540, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Integer(btKeyboard), Integer(DeviceInfo.DeviceType));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_ValidLastSeen_SetsDateTime;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, False);
  WinInfo.stLastSeen.wYear := 2024;
  WinInfo.stLastSeen.wMonth := 6;
  WinInfo.stLastSeen.wDay := 15;
  WinInfo.stLastSeen.wHour := 10;

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.IsTrue(DeviceInfo.LastSeen > 0, 'LastSeen should be set');
  Assert.AreEqual(2024, YearOf(DeviceInfo.LastSeen));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_InvalidLastSeen_SetsZero;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, False);
  // stLastSeen is already zeroed from FillChar

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual(Double(0), Double(DeviceInfo.LastSeen), 0.0001);
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_ValidLastUsed_SetsDateTime;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, 'Test', 0, False, False, False);
  WinInfo.stLastUsed.wYear := 2024;
  WinInfo.stLastUsed.wMonth := 5;
  WinInfo.stLastUsed.wDay := 20;

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.IsTrue(DeviceInfo.LastUsed > 0, 'LastUsed should be set');
  Assert.AreEqual(2024, YearOf(DeviceInfo.LastUsed));
end;

procedure TConvertBluetoothDeviceInfoTests.Convert_EmptyName_HandlesCorrectly;
var
  WinInfo: BLUETOOTH_DEVICE_INFO;
  DeviceInfo: TBluetoothDeviceInfo;
begin
  WinInfo := CreateTestDeviceInfo(0, '', 0, False, False, False);

  DeviceInfo := ConvertBluetoothDeviceInfo(WinInfo);

  Assert.AreEqual('', DeviceInfo.Name);
end;

initialization
  TDUnitX.RegisterTestFixture(TSafeSystemTimeToDateTimeTests);
  TDUnitX.RegisterTestFixture(TConvertBluetoothDeviceInfoTests);

end.
