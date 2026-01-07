{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       List Data Source                                }
{                                                       }
{       EXTRACTED FROM: UI.DeviceList (god class)       }
{       Manages display items, index map, heights,      }
{       selection/hover state.                          }
{                                                       }
{*******************************************************}

unit UI.ListDataSource;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Bluetooth.Types,
  App.DeviceDisplayTypes,
  App.LayoutConfigIntf,
  App.ProfileConfigIntf;

type
  /// <summary>
  /// Data source for device list, extracted from TDeviceListBox god class.
  /// Manages display items array, index map, item heights, selection/hover state.
  /// </summary>
  TListDataSource = class
  private
    FDisplayItems: TDeviceDisplayItemArray;
    FDisplayItemIndexMap: TDictionary<UInt64, Integer>;  // Address -> Index for O(1) lookup
    FItemHeights: TArray<Integer>;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;

    // Dependencies (for height calculation)
    FLayoutConfig: ILayoutConfig;
    FProfileConfig: IProfileConfig;
    FBaseItemHeight: Integer;  // Cached base height

    // Events
    FOnItemsChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;

    procedure RebuildIndexMap;
    procedure RecalculateItemHeights;
    function CalculateItemHeight(AIndex: Integer): Integer;
    function GetProfileSectionHeight(AProfileCount: Integer): Integer;
    function GetProfileLineHeight: Integer;
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetHoverIndex(AValue: Integer);
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Updates configuration dependencies and recalculates item heights.
    /// Call when layout or profile config changes.
    /// </summary>
    procedure UpdateConfigs(ALayoutConfig: ILayoutConfig; AProfileConfig: IProfileConfig);

    /// <summary>
    /// Clears all items and resets selection/hover state.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Sets display items, preserving selection/hover by device address.
    /// Rebuilds index map and recalculates heights.
    /// </summary>
    procedure SetDisplayItems(const AItems: TDeviceDisplayItemArray);

    /// <summary>
    /// Updates single item by device address (O(1) lookup).
    /// </summary>
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);

    /// <summary>
    /// Finds item index by device address. Returns -1 if not found.
    /// </summary>
    function FindItemByAddress(AAddress: UInt64): Integer;

    /// <summary>
    /// Gets display item by index. Returns default if out of bounds.
    /// </summary>
    function GetItem(AIndex: Integer): TDeviceDisplayItem;

    /// <summary>
    /// Gets device by index. Returns default if out of bounds.
    /// </summary>
    function GetDevice(AIndex: Integer): TBluetoothDeviceInfo;

    /// <summary>
    /// Gets item height by index. Returns 0 if out of bounds.
    /// </summary>
    function GetItemHeight(AIndex: Integer): Integer;

    /// <summary>
    /// Gets selected display item. Returns default if no selection.
    /// </summary>
    function GetSelectedDisplayItem: TDeviceDisplayItem;

    /// <summary>
    /// Gets selected device. Returns default if no selection.
    /// </summary>
    function GetSelectedDevice: TBluetoothDeviceInfo;

    property Items: TDeviceDisplayItemArray read FDisplayItems;
    property ItemHeights: TArray<Integer> read FItemHeights;
    property ItemCount: Integer read GetItemCount;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property HoverIndex: Integer read FHoverIndex write SetHoverIndex;

    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

implementation

const
  DEFAULT_ITEM_HEIGHT = 60;
  DEFAULT_PROFILE_FONT_SIZE = 7;
  PROFILE_LINE_HEIGHT_FACTOR = 2;  // LineHeight = FontSize * Factor
  ACTION_BUTTON_HEIGHT = 28;
  ACTION_BUTTON_PADDING = 8;

{ TListDataSource }

constructor TListDataSource.Create;
begin
  inherited Create;
  FDisplayItems := nil;
  FDisplayItemIndexMap := TDictionary<UInt64, Integer>.Create;
  FItemHeights := nil;
  FSelectedIndex := -1;
  FHoverIndex := -1;
  FLayoutConfig := nil;
  FProfileConfig := nil;
  FBaseItemHeight := DEFAULT_ITEM_HEIGHT;
end;

destructor TListDataSource.Destroy;
begin
  FDisplayItemIndexMap.Free;
  inherited Destroy;
end;

procedure TListDataSource.UpdateConfigs(ALayoutConfig: ILayoutConfig; AProfileConfig: IProfileConfig);
begin
  FLayoutConfig := ALayoutConfig;
  FProfileConfig := AProfileConfig;

  // Cache base height
  if Assigned(FLayoutConfig) then
    FBaseItemHeight := FLayoutConfig.ItemHeight
  else
    FBaseItemHeight := DEFAULT_ITEM_HEIGHT;

  // Recalculate heights with new config
  RecalculateItemHeights;

  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TListDataSource.Clear;
begin
  FDisplayItems := nil;
  FItemHeights := nil;
  FDisplayItemIndexMap.Clear;
  FSelectedIndex := -1;
  FHoverIndex := -1;

  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TListDataSource.RebuildIndexMap;
var
  I: Integer;
begin
  FDisplayItemIndexMap.Clear;
  for I := 0 to High(FDisplayItems) do
    FDisplayItemIndexMap.Add(FDisplayItems[I].Device.AddressInt, I);
end;

function TListDataSource.GetProfileLineHeight: Integer;
var
  FontSize: Integer;
begin
  if Assigned(FProfileConfig) then
    FontSize := FProfileConfig.ProfileFontSize
  else
    FontSize := DEFAULT_PROFILE_FONT_SIZE;
  Result := FontSize * PROFILE_LINE_HEIGHT_FACTOR;
end;

function TListDataSource.GetProfileSectionHeight(AProfileCount: Integer): Integer;
var
  LineHeight: Integer;
begin
  if AProfileCount <= 1 then
    Result := 0
  else
  begin
    LineHeight := GetProfileLineHeight;
    Result := AProfileCount * LineHeight + LineHeight div 2;
  end;
end;

function TListDataSource.CalculateItemHeight(AIndex: Integer): Integer;
var
  BaseHeight: Integer;
  ProfileCount: Integer;
begin
  // Action items (scan button, etc.) have smaller height
  if (AIndex >= 0) and (AIndex < Length(FDisplayItems)) and
     (FDisplayItems[AIndex].Source = dsAction) then
  begin
    Result := ACTION_BUTTON_HEIGHT + (ACTION_BUTTON_PADDING * 2);
    Exit;
  end;

  BaseHeight := FBaseItemHeight;

  // Add profile section height if profiles are shown
  if Assigned(FProfileConfig) and FProfileConfig.ShowProfiles and
     (AIndex >= 0) and (AIndex < Length(FDisplayItems)) then
  begin
    ProfileCount := Length(FDisplayItems[AIndex].Profiles);
    Result := BaseHeight + GetProfileSectionHeight(ProfileCount);
  end
  else
    Result := BaseHeight;
end;

procedure TListDataSource.RecalculateItemHeights;
var
  I: Integer;
begin
  SetLength(FItemHeights, Length(FDisplayItems));
  for I := 0 to High(FDisplayItems) do
    FItemHeights[I] := CalculateItemHeight(I);
end;

procedure TListDataSource.SetDisplayItems(const AItems: TDeviceDisplayItemArray);
var
  OldHoverAddress: UInt64;
  OldSelectedAddress: UInt64;
  NewIndex: Integer;
begin
  // Save addresses of currently hovered/selected items to restore them after rebuild
  if (FHoverIndex >= 0) and (FHoverIndex < Length(FDisplayItems)) then
    OldHoverAddress := FDisplayItems[FHoverIndex].Device.AddressInt
  else
    OldHoverAddress := 0;

  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    OldSelectedAddress := FDisplayItems[FSelectedIndex].Device.AddressInt
  else
    OldSelectedAddress := 0;

  FDisplayItems := AItems;

  // Rebuild index map for O(1) lookup by device address
  RebuildIndexMap;

  // Recalculate item heights
  RecalculateItemHeights;

  // Restore hover state by looking up the device in the new list
  if OldHoverAddress <> 0 then
  begin
    if FDisplayItemIndexMap.TryGetValue(OldHoverAddress, NewIndex) then
      FHoverIndex := NewIndex
    else
      FHoverIndex := -1;
  end
  else
    FHoverIndex := -1;

  // Restore selection state by looking up the device in the new list
  if OldSelectedAddress <> 0 then
  begin
    if FDisplayItemIndexMap.TryGetValue(OldSelectedAddress, NewIndex) then
      FSelectedIndex := NewIndex
    else
      FSelectedIndex := -1;
  end
  else if FSelectedIndex >= Length(FDisplayItems) then
    FSelectedIndex := -1;

  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TListDataSource.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
var
  Index: Integer;
begin
  // O(1) lookup using index map instead of O(n) linear search
  if FDisplayItemIndexMap.TryGetValue(AItem.Device.AddressInt, Index) then
  begin
    FDisplayItems[Index] := AItem;
    // Recalculate height for this item in case profiles changed
    if Index < Length(FItemHeights) then
      FItemHeights[Index] := CalculateItemHeight(Index);

    if Assigned(FOnItemsChanged) then
      FOnItemsChanged(Self);
  end;
end;

function TListDataSource.FindItemByAddress(AAddress: UInt64): Integer;
begin
  if not FDisplayItemIndexMap.TryGetValue(AAddress, Result) then
    Result := -1;
end;

function TListDataSource.GetItem(AIndex: Integer): TDeviceDisplayItem;
begin
  if (AIndex >= 0) and (AIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[AIndex]
  else
    Result := Default(TDeviceDisplayItem);
end;

function TListDataSource.GetDevice(AIndex: Integer): TBluetoothDeviceInfo;
begin
  if (AIndex >= 0) and (AIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[AIndex].Device
  else
    Result := Default(TBluetoothDeviceInfo);
end;

function TListDataSource.GetItemHeight(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FItemHeights)) then
    Result := FItemHeights[AIndex]
  else
    Result := 0;
end;

function TListDataSource.GetItemCount: Integer;
begin
  Result := Length(FDisplayItems);
end;

function TListDataSource.GetSelectedDisplayItem: TDeviceDisplayItem;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[FSelectedIndex]
  else
    Result := Default(TDeviceDisplayItem);
end;

function TListDataSource.GetSelectedDevice: TBluetoothDeviceInfo;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FDisplayItems)) then
    Result := FDisplayItems[FSelectedIndex].Device
  else
    Result := Default(TBluetoothDeviceInfo);
end;

procedure TListDataSource.SetSelectedIndex(AValue: Integer);
var
  Count: Integer;
begin
  Count := GetItemCount;
  if AValue < -1 then AValue := -1;
  if AValue >= Count then AValue := Count - 1;

  if FSelectedIndex <> AValue then
  begin
    FSelectedIndex := AValue;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

procedure TListDataSource.SetHoverIndex(AValue: Integer);
begin
  if FHoverIndex <> AValue then
  begin
    FHoverIndex := AValue;
    // No event needed - hover is purely visual
  end;
end;

end.
