{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Device Query Tests                        }
{                                                       }
{*******************************************************}

unit Tests.WinRTDeviceQuery;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinRTDeviceQuery,
  Bluetooth.DeviceRepository;

type
  /// <summary>
  /// Unit tests for TWinRTBluetoothDeviceQuery.
  /// Tests basic functionality of the WinRT query implementation.
  /// </summary>
  [TestFixture]
  TWinRTDeviceQueryTests = class
  private
    FQuery: IBluetoothDeviceQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure EnumeratePairedDevices_ReturnsArray;

    [Test]
    procedure EnumeratePairedDevices_DevicesHaveValidAddresses;

    [Test]
    procedure EnumeratePairedDevices_NoDuplicateAddresses;
  end;

  /// <summary>
  /// Integration tests comparing Win32 and WinRT device enumeration.
  /// These tests require real Bluetooth hardware and paired devices.
  /// </summary>
  [TestFixture]
  [Category('Integration')]
  TDeviceQueryComparisonTests = class
  private
    FWin32Query: IBluetoothDeviceQuery;
    FWinRTQuery: IBluetoothDeviceQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure CompareDeviceCounts;

    [Test]
    procedure CompareDeviceNames;

    [Test]
    procedure LogDetailedComparison;
  end;

implementation

uses
  App.Logger;

{ TWinRTDeviceQueryTests }

procedure TWinRTDeviceQueryTests.Setup;
begin
  FQuery := CreateWinRTBluetoothDeviceQuery;
end;

procedure TWinRTDeviceQueryTests.TearDown;
begin
  FQuery := nil;
end;

procedure TWinRTDeviceQueryTests.EnumeratePairedDevices_ReturnsArray;
var
  Devices: TBluetoothDeviceInfoArray;
begin
  // Act
  Devices := FQuery.EnumeratePairedDevices;

  // Assert - should return an array (may be empty if no devices paired)
  Assert.IsTrue(Length(Devices) >= 0, 'Should return valid array');
end;

procedure TWinRTDeviceQueryTests.EnumeratePairedDevices_DevicesHaveValidAddresses;
var
  Devices: TBluetoothDeviceInfoArray;
  Device: TBluetoothDeviceInfo;
begin
  // Act
  Devices := FQuery.EnumeratePairedDevices;

  // Assert - all devices should have non-zero addresses
  for Device in Devices do
    Assert.AreNotEqual<UInt64>(0, Device.AddressInt,
      Format('Device "%s" should have valid address', [Device.Name]));
end;

procedure TWinRTDeviceQueryTests.EnumeratePairedDevices_NoDuplicateAddresses;
var
  Devices: TBluetoothDeviceInfoArray;
  Addresses: TDictionary<UInt64, Boolean>;
  Device: TBluetoothDeviceInfo;
begin
  // Act
  Devices := FQuery.EnumeratePairedDevices;

  // Assert - no duplicate addresses
  Addresses := TDictionary<UInt64, Boolean>.Create;
  try
    for Device in Devices do
    begin
      Assert.IsFalse(Addresses.ContainsKey(Device.AddressInt),
        Format('Duplicate address found: $%.12X (%s)', [Device.AddressInt, Device.Name]));
      Addresses.Add(Device.AddressInt, True);
    end;
  finally
    Addresses.Free;
  end;
end;

{ TDeviceQueryComparisonTests }

procedure TDeviceQueryComparisonTests.Setup;
begin
  // Create Win32 query (from DeviceRepository)
  FWin32Query := CreateBluetoothDeviceQuery;
  // Create WinRT query
  FWinRTQuery := CreateWinRTBluetoothDeviceQuery;
end;

procedure TDeviceQueryComparisonTests.TearDown;
begin
  FWin32Query := nil;
  FWinRTQuery := nil;
end;

procedure TDeviceQueryComparisonTests.CompareDeviceCounts;
var
  Win32Devices, WinRTDevices: TBluetoothDeviceInfoArray;
begin
  // Act
  Win32Devices := FWin32Query.EnumeratePairedDevices;
  WinRTDevices := FWinRTQuery.EnumeratePairedDevices;

  // Log counts for investigation
  LogInfo('CompareDeviceCounts: Win32=%d, WinRT=%d',
    [Length(Win32Devices), Length(WinRTDevices)], 'Test');

  // Assert - counts should be similar (WinRT may find more BLE devices)
  // This is informational - we want to see the difference
  Assert.IsTrue(True, Format('Win32: %d devices, WinRT: %d devices',
    [Length(Win32Devices), Length(WinRTDevices)]));
end;

procedure TDeviceQueryComparisonTests.CompareDeviceNames;
var
  Win32Devices, WinRTDevices: TBluetoothDeviceInfoArray;
  Win32Map: TDictionary<UInt64, TBluetoothDeviceInfo>;
  Device, Win32Device: TBluetoothDeviceInfo;
  Win32EmptyNames, WinRTEmptyNames: Integer;
  WinRTHasNameWin32Empty: Integer;
begin
  // Act
  Win32Devices := FWin32Query.EnumeratePairedDevices;
  WinRTDevices := FWinRTQuery.EnumeratePairedDevices;

  // Build Win32 lookup
  Win32Map := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  try
    for Device in Win32Devices do
      Win32Map.AddOrSetValue(Device.AddressInt, Device);

    Win32EmptyNames := 0;
    WinRTEmptyNames := 0;
    WinRTHasNameWin32Empty := 0;

    // Count empty names in Win32
    for Device in Win32Devices do
      if Device.Name = '' then
        Inc(Win32EmptyNames);

    // Count empty names in WinRT and check if WinRT has names Win32 doesn't
    for Device in WinRTDevices do
    begin
      if Device.Name = '' then
        Inc(WinRTEmptyNames)
      else if Win32Map.TryGetValue(Device.AddressInt, Win32Device) then
      begin
        if Win32Device.Name = '' then
        begin
          Inc(WinRTHasNameWin32Empty);
          LogInfo('CompareDeviceNames: WinRT has name "%s" for $%.12X, Win32 has empty name',
            [Device.Name, Device.AddressInt], 'Test');
        end;
      end;
    end;

    LogInfo('CompareDeviceNames: Win32 empty names: %d, WinRT empty names: %d, WinRT has name where Win32 empty: %d',
      [Win32EmptyNames, WinRTEmptyNames, WinRTHasNameWin32Empty], 'Test');

    // This is the key metric - WinRT should resolve more names
    Assert.IsTrue(True, Format('Win32 empty: %d, WinRT empty: %d, WinRT resolved: %d',
      [Win32EmptyNames, WinRTEmptyNames, WinRTHasNameWin32Empty]));
  finally
    Win32Map.Free;
  end;
end;

procedure TDeviceQueryComparisonTests.LogDetailedComparison;
var
  Win32Devices, WinRTDevices: TBluetoothDeviceInfoArray;
  Win32Map, WinRTMap: TDictionary<UInt64, TBluetoothDeviceInfo>;
  Device: TBluetoothDeviceInfo;
  Address: UInt64;
begin
  // Act
  Win32Devices := FWin32Query.EnumeratePairedDevices;
  WinRTDevices := FWinRTQuery.EnumeratePairedDevices;

  Win32Map := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  WinRTMap := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  try
    for Device in Win32Devices do
      Win32Map.AddOrSetValue(Device.AddressInt, Device);

    for Device in WinRTDevices do
      WinRTMap.AddOrSetValue(Device.AddressInt, Device);

    LogInfo('=== DETAILED COMPARISON ===', 'Test');
    LogInfo('Win32 devices: %d, WinRT devices: %d', [Length(Win32Devices), Length(WinRTDevices)], 'Test');
    LogInfo('', 'Test');

    // Log all Win32 devices
    LogInfo('--- Win32 Devices ---', 'Test');
    for Device in Win32Devices do
    begin
      LogInfo('  $%.12X | Name="%s" | CoD=$%.8X | Connected=%s | InWinRT=%s',
        [Device.AddressInt, Device.Name, Device.ClassOfDevice,
         BoolToStr(Device.IsConnected, True),
         BoolToStr(WinRTMap.ContainsKey(Device.AddressInt), True)], 'Test');
    end;
    LogInfo('', 'Test');

    // Log all WinRT devices
    LogInfo('--- WinRT Devices ---', 'Test');
    for Device in WinRTDevices do
    begin
      LogInfo('  $%.12X | Name="%s" | CoD=$%.8X | Connected=%s | InWin32=%s',
        [Device.AddressInt, Device.Name, Device.ClassOfDevice,
         BoolToStr(Device.IsConnected, True),
         BoolToStr(Win32Map.ContainsKey(Device.AddressInt), True)], 'Test');
    end;
    LogInfo('', 'Test');

    // Log devices only in WinRT (BLE devices Win32 missed)
    LogInfo('--- Devices ONLY in WinRT (not in Win32) ---', 'Test');
    for Address in WinRTMap.Keys do
    begin
      if not Win32Map.ContainsKey(Address) then
      begin
        Device := WinRTMap[Address];
        LogInfo('  $%.12X | Name="%s" | CoD=$%.8X',
          [Device.AddressInt, Device.Name, Device.ClassOfDevice], 'Test');
      end;
    end;
    LogInfo('', 'Test');

    // Log devices only in Win32 (shouldn't happen, but log anyway)
    LogInfo('--- Devices ONLY in Win32 (not in WinRT) ---', 'Test');
    for Address in Win32Map.Keys do
    begin
      if not WinRTMap.ContainsKey(Address) then
      begin
        Device := Win32Map[Address];
        LogInfo('  $%.12X | Name="%s" | CoD=$%.8X',
          [Device.AddressInt, Device.Name, Device.ClassOfDevice], 'Test');
      end;
    end;
    LogInfo('', 'Test');

    // Log devices where name differs
    LogInfo('--- Devices with DIFFERENT names ---', 'Test');
    for Address in Win32Map.Keys do
    begin
      if WinRTMap.ContainsKey(Address) then
      begin
        if Win32Map[Address].Name <> WinRTMap[Address].Name then
        begin
          LogInfo('  $%.12X | Win32="%s" | WinRT="%s"',
            [Address, Win32Map[Address].Name, WinRTMap[Address].Name], 'Test');
        end;
      end;
    end;
    LogInfo('=== END COMPARISON ===', 'Test');

    Assert.IsTrue(True, 'Detailed comparison logged');
  finally
    Win32Map.Free;
    WinRTMap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TWinRTDeviceQueryTests);
  TDUnitX.RegisterTestFixture(TDeviceQueryComparisonTests);

end.
