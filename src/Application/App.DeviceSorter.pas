{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Item Sorting                     }
{                                                       }
{       Provides sorting logic for device display.      }
{       Extracted from TDeviceListBox to follow SRP.    }
{                                                       }
{*******************************************************}

unit App.DeviceSorter;

interface

uses
  App.DeviceDisplayTypes;

type
  /// <summary>
  /// Static class providing sorting functionality for device display items.
  /// Sort order: Pinned -> Connected -> Disconnected -> Discovered.
  /// Within groups: LastSeen (most recent first) -> DisplayName -> Address.
  /// Special: Discovered devices (group 3) sort named before unnamed.
  /// </summary>
  TDeviceSorter = class
  private
    /// <summary>
    /// Checks if a device has a real name (not empty and not generic fallback pattern).
    /// Generic patterns include "Device XX:XX:XX:XX:XX:XX" format.
    /// </summary>
    /// <param name="AName">The device name to check.</param>
    /// <returns>True if name is real, False if empty or generic fallback.</returns>
    class function HasRealDeviceName(const AName: string): Boolean; static;
  public
    /// <summary>
    /// Sorts an array of device display items in place.
    /// Sort order:
    ///   1. By SortGroup (0=Pinned, 1=Connected, 2=Disconnected, 3=Discovered)
    ///   2. For Discovered (group 3): Named devices before unnamed devices
    ///   3. By LastSeen (most recent first)
    ///   4. By DisplayName (case-insensitive alphabetical)
    ///   5. By Address (ascending)
    /// </summary>
    /// <param name="AItems">Array of display items to sort in place.</param>
    class procedure Sort(var AItems: TDeviceDisplayItemArray); static;

    /// <summary>
    /// Compares two display items according to the sorting rules.
    /// </summary>
    /// <param name="ALeft">First item to compare.</param>
    /// <param name="ARight">Second item to compare.</param>
    /// <returns>
    ///   Negative if ALeft should come before ARight,
    ///   Positive if ALeft should come after ARight,
    ///   Zero if they are equal in sort order.
    /// </returns>
    class function Compare(const ALeft, ARight: TDeviceDisplayItem): Integer; static;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

{ TDeviceSorter }

class function TDeviceSorter.HasRealDeviceName(const AName: string): Boolean;
begin
  // Device has real name if not empty AND not generic "Device XX:XX:XX..." fallback pattern.
  // This pattern is used by TDeviceFormatter.GetDisplayName as a last resort
  // when no other name is available (not alias, not API name, not cached name).
  Result := (AName <> '') and not AName.StartsWith('Device ');
end;

class function TDeviceSorter.Compare(const ALeft, ARight: TDeviceDisplayItem): Integer;
var
  LeftHasName, RightHasName: Boolean;
begin
  // Compare by sort group first (Pinned=0 < Connected=1 < Disconnected=2 < Discovered=3)
  Result := ALeft.SortGroup - ARight.SortGroup;
  if Result <> 0 then
    Exit;

  // Special handling for discovered devices (group 3):
  // Sort named devices before unnamed devices to reduce visual noise.
  if (ALeft.SortGroup = 3) and (ARight.SortGroup = 3) then
  begin
    LeftHasName := HasRealDeviceName(ALeft.DisplayName);
    RightHasName := HasRealDeviceName(ARight.DisplayName);

    // Named devices come before unnamed devices
    if LeftHasName and not RightHasName then
      Exit(-1)  // Left (named) comes first
    else if RightHasName and not LeftHasName then
      Exit(1);  // Right (named) comes first

    // Both named or both unnamed: fall through to standard sorting
  end;

  // Within same group (and same named/unnamed sub-group for discovered),
  // sort by LastSeen (most recent first)
  if ALeft.LastSeen > ARight.LastSeen then
    Result := -1
  else if ALeft.LastSeen < ARight.LastSeen then
    Result := 1
  else
  begin
    // Same LastSeen, sort by display name (case-insensitive)
    Result := CompareText(ALeft.DisplayName, ARight.DisplayName);

    // If names are also equal, sort by MAC address for stable ordering
    if Result = 0 then
    begin
      if ALeft.Device.AddressInt < ARight.Device.AddressInt then
        Result := -1
      else if ALeft.Device.AddressInt > ARight.Device.AddressInt then
        Result := 1;
    end;
  end;
end;

class procedure TDeviceSorter.Sort(var AItems: TDeviceDisplayItemArray);
var
  Comparer: IComparer<TDeviceDisplayItem>;
begin
  if Length(AItems) <= 1 then
    Exit;

  Comparer := TComparer<TDeviceDisplayItem>.Construct(
    function(const Left, Right: TDeviceDisplayItem): Integer
    begin
      Result := TDeviceSorter.Compare(Left, Right);
    end
  );

  TArray.Sort<TDeviceDisplayItem>(AItems, Comparer);
end;

end.
