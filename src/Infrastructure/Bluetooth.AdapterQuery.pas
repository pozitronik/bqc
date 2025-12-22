{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Adapter Query Implementation                    }
{                                                       }
{       Queries Bluetooth adapter availability          }
{       via Windows API.                                }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.AdapterQuery;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Queries Bluetooth adapter availability via Windows API.
  /// Single Responsibility: Only checks adapter presence and info.
  /// </summary>
  TBluetoothAdapterQuery = class(TInterfacedObject, IBluetoothAdapterQuery)
  protected
    { IBluetoothAdapterQuery }
    function IsAdapterAvailable: Boolean;
    function GetAdapterName: string;
  end;

/// <summary>
/// Creates an adapter query instance.
/// </summary>
function CreateAdapterQuery: IBluetoothAdapterQuery;

implementation

uses
  App.Logger;

function CreateAdapterQuery: IBluetoothAdapterQuery;
begin
  Result := TBluetoothAdapterQuery.Create;
end;

{ TBluetoothAdapterQuery }

function TBluetoothAdapterQuery.IsAdapterAvailable: Boolean;
var
  FindParams: BLUETOOTH_FIND_RADIO_PARAMS;
  FindHandle: HBLUETOOTH_RADIO_FIND;
  RadioHandle: THandle;
begin
  FindParams.dwSize := SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  FindHandle := BluetoothFindFirstRadio(@FindParams, RadioHandle);

  Result := FindHandle <> 0;

  if Result then
  begin
    CloseHandle(RadioHandle);
    BluetoothFindRadioClose(FindHandle);
  end;
end;

function TBluetoothAdapterQuery.GetAdapterName: string;
var
  FindParams: BLUETOOTH_FIND_RADIO_PARAMS;
  FindHandle: HBLUETOOTH_RADIO_FIND;
  RadioHandle: THandle;
  RadioInfo: BLUETOOTH_RADIO_INFO;
begin
  Result := '';

  FindParams.dwSize := SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  FindHandle := BluetoothFindFirstRadio(@FindParams, RadioHandle);

  if FindHandle <> 0 then
  begin
    try
      RadioInfo.dwSize := SizeOf(BLUETOOTH_RADIO_INFO);
      if BluetoothGetRadioInfo(RadioHandle, RadioInfo) = ERROR_SUCCESS then
        Result := string(RadioInfo.szName);
    finally
      CloseHandle(RadioHandle);
      BluetoothFindRadioClose(FindHandle);
    end;
  end;
end;

end.
