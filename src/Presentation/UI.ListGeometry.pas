{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       List Geometry Calculator                        }
{                                                       }
{       Pure calculation logic for list layout,         }
{       item positioning, and scroll management.        }
{       Extracted for testability.                      }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.ListGeometry;

interface

uses
  System.Types,
  System.Math;

type
  /// <summary>
  /// Handles geometry calculations for a virtual list control.
  /// Stateless methods for calculating item positions and scroll ranges.
  /// </summary>
  TListGeometry = class
  public
    /// <summary>
    /// Calculates the bounding rectangle for an item at the given index.
    /// </summary>
    /// <param name="AIndex">Zero-based item index.</param>
    /// <param name="AItemHeight">Height of each item in pixels.</param>
    /// <param name="AItemMargin">Margin between items in pixels.</param>
    /// <param name="AClientWidth">Width of the client area.</param>
    /// <param name="AScrollPos">Current scroll position.</param>
    /// <returns>Rectangle defining the item's bounds.</returns>
    class function GetItemRect(AIndex: Integer; AItemHeight, AItemMargin,
      AClientWidth, AScrollPos: Integer): TRect;

    /// <summary>
    /// Finds the item index at the given coordinates.
    /// </summary>
    /// <param name="X">X coordinate.</param>
    /// <param name="Y">Y coordinate.</param>
    /// <param name="AItemCount">Total number of items.</param>
    /// <param name="AItemHeight">Height of each item.</param>
    /// <param name="AItemMargin">Margin between items.</param>
    /// <param name="AClientWidth">Width of the client area.</param>
    /// <param name="AScrollPos">Current scroll position.</param>
    /// <returns>Item index or -1 if no item at position.</returns>
    class function ItemAtPos(X, Y: Integer; AItemCount, AItemHeight,
      AItemMargin, AClientWidth, AScrollPos: Integer): Integer;

    /// <summary>
    /// Calculates the maximum scroll position.
    /// </summary>
    /// <param name="AItemCount">Total number of items.</param>
    /// <param name="AItemHeight">Height of each item.</param>
    /// <param name="AItemMargin">Margin between items.</param>
    /// <param name="AClientHeight">Height of visible area.</param>
    /// <returns>Maximum scroll position (0 if no scrolling needed).</returns>
    class function CalculateMaxScroll(AItemCount, AItemHeight, AItemMargin,
      AClientHeight: Integer): Integer;

    /// <summary>
    /// Calculates the total content height.
    /// </summary>
    /// <param name="AItemCount">Total number of items.</param>
    /// <param name="AItemHeight">Height of each item.</param>
    /// <param name="AItemMargin">Margin between items.</param>
    /// <returns>Total height of all items including margins.</returns>
    class function CalculateTotalHeight(AItemCount, AItemHeight,
      AItemMargin: Integer): Integer;

    /// <summary>
    /// Clamps a scroll position to valid range.
    /// </summary>
    /// <param name="AScrollPos">Desired scroll position.</param>
    /// <param name="AMaxScroll">Maximum allowed scroll position.</param>
    /// <returns>Clamped scroll position.</returns>
    class function ClampScrollPos(AScrollPos, AMaxScroll: Integer): Integer;

    /// <summary>
    /// Calculates the scroll position needed to make an item visible.
    /// </summary>
    /// <param name="AIndex">Index of item to make visible.</param>
    /// <param name="ACurrentScrollPos">Current scroll position.</param>
    /// <param name="AItemHeight">Height of each item.</param>
    /// <param name="AItemMargin">Margin between items.</param>
    /// <param name="AClientHeight">Height of visible area.</param>
    /// <returns>New scroll position.</returns>
    class function ScrollPosToMakeVisible(AIndex, ACurrentScrollPos, AItemHeight,
      AItemMargin, AClientHeight: Integer): Integer;

    /// <summary>
    /// Calculates the top position of an item (before scroll offset).
    /// </summary>
    class function GetItemTop(AIndex, AItemHeight, AItemMargin: Integer): Integer;

    /// <summary>
    /// Determines if an item rectangle is visible within the client area.
    /// </summary>
    class function IsItemVisible(const AItemRect: TRect; AClientHeight: Integer): Boolean;
  end;

implementation

{ TListGeometry }

class function TListGeometry.GetItemRect(AIndex: Integer; AItemHeight, AItemMargin,
  AClientWidth, AScrollPos: Integer): TRect;
begin
  Result.Left := AItemMargin;
  Result.Right := AClientWidth - AItemMargin;
  Result.Top := AItemMargin + AIndex * (AItemHeight + AItemMargin) - AScrollPos;
  Result.Bottom := Result.Top + AItemHeight;
end;

class function TListGeometry.ItemAtPos(X, Y: Integer; AItemCount, AItemHeight,
  AItemMargin, AClientWidth, AScrollPos: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to AItemCount - 1 do
  begin
    R := GetItemRect(I, AItemHeight, AItemMargin, AClientWidth, AScrollPos);
    if (X >= R.Left) and (X < R.Right) and (Y >= R.Top) and (Y < R.Bottom) then
      Exit(I);
  end;
  Result := -1;
end;

class function TListGeometry.CalculateTotalHeight(AItemCount, AItemHeight,
  AItemMargin: Integer): Integer;
begin
  if AItemCount <= 0 then
    Result := AItemMargin
  else
    Result := AItemCount * (AItemHeight + AItemMargin) + AItemMargin;
end;

class function TListGeometry.CalculateMaxScroll(AItemCount, AItemHeight, AItemMargin,
  AClientHeight: Integer): Integer;
var
  TotalHeight: Integer;
begin
  TotalHeight := CalculateTotalHeight(AItemCount, AItemHeight, AItemMargin);
  Result := Max(0, TotalHeight - AClientHeight);
end;

class function TListGeometry.ClampScrollPos(AScrollPos, AMaxScroll: Integer): Integer;
begin
  Result := EnsureRange(AScrollPos, 0, AMaxScroll);
end;

class function TListGeometry.GetItemTop(AIndex, AItemHeight, AItemMargin: Integer): Integer;
begin
  Result := AItemMargin + AIndex * (AItemHeight + AItemMargin);
end;

class function TListGeometry.ScrollPosToMakeVisible(AIndex, ACurrentScrollPos, AItemHeight,
  AItemMargin, AClientHeight: Integer): Integer;
var
  ItemTop, ItemBottom: Integer;
begin
  ItemTop := GetItemTop(AIndex, AItemHeight, AItemMargin);
  ItemBottom := ItemTop + AItemHeight;

  if ItemTop < ACurrentScrollPos then
    // Item is above visible area, scroll up
    Result := ItemTop - AItemMargin
  else if ItemBottom > ACurrentScrollPos + AClientHeight then
    // Item is below visible area, scroll down
    Result := ItemBottom - AClientHeight + AItemMargin
  else
    // Item is already visible
    Result := ACurrentScrollPos;
end;

class function TListGeometry.IsItemVisible(const AItemRect: TRect;
  AClientHeight: Integer): Boolean;
begin
  Result := (AItemRect.Bottom > 0) and (AItemRect.Top < AClientHeight);
end;

end.
