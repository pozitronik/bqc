{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Types                            }
{                                                       }
{       Application layer types for device display.     }
{       Consumed by presenters and views.               }
{                                                       }
{*******************************************************}

unit App.DeviceDisplayTypes;

interface

uses
  Bluetooth.Types;

type
  /// <summary>
  /// Source of a device in the display list.
  /// Used to distinguish between paired devices and discovered (unpaired) devices.
  /// </summary>
  TDeviceSource = (
    dsPaired,      // Device is paired and from the main device list
    dsDiscovered   // Device is discovered (unpaired) and from the discovery cache
  );

  /// <summary>
  /// Pre-processed display item for device list rendering.
  /// Contains all data needed for display without further config lookups.
  /// Created by presenter, consumed by view (Information Expert pattern).
  /// </summary>
  TDeviceDisplayItem = record
    /// <summary>Original device data from Bluetooth service.</summary>
    Device: TBluetoothDeviceInfo;

    /// <summary>Source of device (paired or discovered).</summary>
    Source: TDeviceSource;

    /// <summary>Display name (alias if set, otherwise device name).</summary>
    DisplayName: string;

    /// <summary>Whether device is pinned to top of list.</summary>
    IsPinned: Boolean;

    /// <summary>Effective device type (override or auto-detected).</summary>
    EffectiveDeviceType: TBluetoothDeviceType;

    /// <summary>Pre-formatted last seen text based on appearance config.</summary>
    LastSeenText: string;

    /// <summary>Raw last seen value for sorting.</summary>
    LastSeen: TDateTime;

    /// <summary>Sort group: 0=Pinned, 1=Connected (not pinned), 2=Disconnected, 3=Discovered.</summary>
    SortGroup: Integer;

    /// <summary>Battery status for devices supporting Battery Service.</summary>
    BatteryStatus: TBatteryStatus;

    /// <summary>Pre-formatted battery text (e.g., "85%").</summary>
    BatteryText: string;

    /// <summary>Array of Bluetooth profiles available for this device.</summary>
    Profiles: TBluetoothProfileArray;

    /// <summary>Creates a display item from device and config data.</summary>
    class function Create(const ADevice: TBluetoothDeviceInfo;
      ASource: TDeviceSource;
      const ADisplayName: string; AIsPinned: Boolean;
      AEffectiveDeviceType: TBluetoothDeviceType;
      const ALastSeenText: string; ALastSeen: TDateTime;
      ASortGroup: Integer; const ABatteryStatus: TBatteryStatus;
      const ABatteryText: string;
      const AProfiles: TBluetoothProfileArray): TDeviceDisplayItem; static;
  end;

  TDeviceDisplayItemArray = TArray<TDeviceDisplayItem>;

implementation

{ TDeviceDisplayItem }

class function TDeviceDisplayItem.Create(const ADevice: TBluetoothDeviceInfo;
  ASource: TDeviceSource;
  const ADisplayName: string; AIsPinned: Boolean;
  AEffectiveDeviceType: TBluetoothDeviceType;
  const ALastSeenText: string; ALastSeen: TDateTime;
  ASortGroup: Integer; const ABatteryStatus: TBatteryStatus;
  const ABatteryText: string;
  const AProfiles: TBluetoothProfileArray): TDeviceDisplayItem;
begin
  Result.Device := ADevice;
  Result.Source := ASource;
  Result.DisplayName := ADisplayName;
  Result.IsPinned := AIsPinned;
  Result.EffectiveDeviceType := AEffectiveDeviceType;
  Result.LastSeenText := ALastSeenText;
  Result.LastSeen := ALastSeen;
  Result.SortGroup := ASortGroup;
  Result.BatteryStatus := ABatteryStatus;
  Result.BatteryText := ABatteryText;
  Result.Profiles := AProfiles;
end;

end.
