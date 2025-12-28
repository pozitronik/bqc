{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Conversion Utilities                     }
{                                                       }
{       Converts Windows API Bluetooth structures       }
{       to domain TBluetoothDeviceInfo records.         }
{                                                       }
{*******************************************************}

unit Bluetooth.DeviceConverter;

interface

uses
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.WinAPI;

/// <summary>
/// Converts a Windows API BLUETOOTH_DEVICE_INFO to domain TBluetoothDeviceInfo.
/// Used for device information from BluetoothFindFirstDevice, BluetoothGetDeviceInfo, etc.
/// </summary>
function ConvertBluetoothDeviceInfo(const AWinAPIInfo: BLUETOOTH_DEVICE_INFO): TBluetoothDeviceInfo;

/// <summary>
/// Converts a BTH_DEVICE_INFO (from WM_DEVICECHANGE notifications) to domain TBluetoothDeviceInfo.
/// Note: BTH_DEVICE_INFO uses ANSI names and has different flags than BLUETOOTH_DEVICE_INFO.
/// </summary>
function ConvertBthDeviceInfo(const ABthInfo: BTH_DEVICE_INFO): TBluetoothDeviceInfo;

/// <summary>
/// Safely converts Windows SYSTEMTIME to TDateTime.
/// Returns 0 if the date is invalid or conversion fails.
/// </summary>
function SafeSystemTimeToDateTime(const ASysTime: TSystemTime): TDateTime;

implementation

uses
  System.SysUtils,
  System.DateUtils;

function SafeSystemTimeToDateTime(const ASysTime: TSystemTime): TDateTime;
begin
  Result := 0;
  try
    // Validate year range to avoid conversion errors
    if (ASysTime.wYear >= 1900) and (ASysTime.wYear <= 2100) and
       (ASysTime.wMonth >= 1) and (ASysTime.wMonth <= 12) and
       (ASysTime.wDay >= 1) and (ASysTime.wDay <= 31) then
    begin
      Result := EncodeDateTime(
        ASysTime.wYear,
        ASysTime.wMonth,
        ASysTime.wDay,
        ASysTime.wHour,
        ASysTime.wMinute,
        ASysTime.wSecond,
        ASysTime.wMilliseconds
      );
    end;
  except
    Result := 0;
  end;
end;

function ConvertBluetoothDeviceInfo(const AWinAPIInfo: BLUETOOTH_DEVICE_INFO): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
  ConnectionState: TBluetoothConnectionState;
  LastSeen, LastUsed: TDateTime;
begin
  Address := UInt64ToBluetoothAddress(AWinAPIInfo.Address.ullLong);

  if AWinAPIInfo.fConnected then
    ConnectionState := csConnected
  else
    ConnectionState := csDisconnected;

  LastSeen := SafeSystemTimeToDateTime(AWinAPIInfo.stLastSeen);
  LastUsed := SafeSystemTimeToDateTime(AWinAPIInfo.stLastUsed);

  Result := TBluetoothDeviceInfo.Create(
    Address,
    AWinAPIInfo.Address.ullLong,
    string(AWinAPIInfo.szName),
    DetermineDeviceType(AWinAPIInfo.ulClassOfDevice),
    ConnectionState,
    AWinAPIInfo.fRemembered,
    AWinAPIInfo.fAuthenticated,
    AWinAPIInfo.ulClassOfDevice,
    LastSeen,
    LastUsed
  );
end;

function ConvertBthDeviceInfo(const ABthInfo: BTH_DEVICE_INFO): TBluetoothDeviceInfo;
var
  Address: TBluetoothAddress;
  ConnectionState: TBluetoothConnectionState;
  DeviceName: string;
begin
  Address := UInt64ToBluetoothAddress(ABthInfo.address);

  // Check BDIF_CONNECTED flag to determine connection state
  if (ABthInfo.flags and BDIF_CONNECTED) <> 0 then
    ConnectionState := csConnected
  else
    ConnectionState := csDisconnected;

  // Convert ANSI name to string (BTH_DEVICE_INFO uses ANSI, not Unicode!)
  DeviceName := string(AnsiString(ABthInfo.name));

  Result := TBluetoothDeviceInfo.Create(
    Address,
    ABthInfo.address,
    DeviceName,
    DetermineDeviceType(ABthInfo.classOfDevice),
    ConnectionState,
    (ABthInfo.flags and BDIF_PAIRED) <> 0,     // fRemembered ~ BDIF_PAIRED
    (ABthInfo.flags and BDIF_PAIRED) <> 0,     // fAuthenticated ~ BDIF_PAIRED
    ABthInfo.classOfDevice,
    0,  // LastSeen not available in BTH_DEVICE_INFO
    0   // LastUsed not available in BTH_DEVICE_INFO
  );
end;

end.
