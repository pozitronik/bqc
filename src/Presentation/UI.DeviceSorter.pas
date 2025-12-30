{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Display Item Sorting                     }
{                                                       }
{       Provides sorting logic for device display.      }
{       Extracted from TDeviceListBox to follow SRP.    }
{                                                       }
{*******************************************************}

unit UI.DeviceSorter;

interface

uses
  App.DeviceDisplayTypes;

type
  /// <summary>
  /// Static class providing sorting functionality for device display items.
  /// Sort order: Pinned -> Connected -> Disconnected.
  /// Within groups: LastSeen (most recent first) -> DisplayName -> Address.
  /// </summary>
  TDeviceSorter = class
  public
    /// <summary>
    /// Sorts an array of device display items in place.
    /// Sort order:
    ///   1. By SortGroup (0=Pinned, 1=Connected, 2=Disconnected)
    ///   2. By LastSeen (most recent first)
    ///   3. By DisplayName (case-insensitive alphabetical)
    ///   4. By Address (ascending)
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

class function TDeviceSorter.Compare(const ALeft, ARight: TDeviceDisplayItem): Integer;
begin
  // Compare by sort group first (Pinned=0 < Connected=1 < Disconnected=2)
  Result := ALeft.SortGroup - ARight.SortGroup;
  if Result <> 0 then
    Exit;

  // Within same group, sort by LastSeen (most recent first)
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
