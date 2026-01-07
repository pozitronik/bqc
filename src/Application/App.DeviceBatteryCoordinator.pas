{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Battery Coordinator                      }
{                                                       }
{       EXTRACTED FROM: App.MainPresenter (god class)   }
{       Handles battery monitoring and updates.         }
{                                                       }
{*******************************************************}

unit App.DeviceBatteryCoordinator;

interface

uses
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.MainViewInterfaces,
  App.DeviceDisplayTypes,
  App.DeviceDisplayItemBuilder,
  App.AppearanceConfigIntf,
  App.AsyncExecutor;

type
  /// <summary>
  /// Function type for finding device by address.
  /// Returns empty record (AddressInt=0) if not found.
  /// </summary>
  TFindDeviceFunc = reference to function(AAddress: UInt64): TBluetoothDeviceInfo;

  /// <summary>
  /// Function type for getting connected device addresses.
  /// Returns array of addresses for all currently connected devices.
  /// </summary>
  TGetConnectedAddressesFunc = reference to function: TArray<UInt64>;

  /// <summary>
  /// Coordinator for battery monitoring logic.
  /// Extracted from TMainPresenter to separate battery concerns.
  /// Manages battery cache, handles battery query events, refreshes battery for connected devices.
  /// </summary>
  TDeviceBatteryCoordinator = class
  private
    FBatteryCache: IBatteryCache;
    FDisplayItemBuilder: IDeviceDisplayItemBuilder;
    FDeviceListView: IDeviceListView;
    FAsyncExecutor: IAsyncExecutor;
    FFindDeviceFunc: TFindDeviceFunc;
    FGetConnectedAddressesFunc: TGetConnectedAddressesFunc;
    FIsShuttingDown: Boolean;

    /// <summary>
    /// Handles battery query completion event from battery cache.
    /// Updates single display item with new battery status using O(1) lookup.
    /// </summary>
    procedure HandleBatteryQueryCompleted(Sender: TObject; ADeviceAddress: UInt64;
      const AStatus: TBatteryStatus);

  public
    /// <summary>
    /// Creates battery coordinator with dependencies.
    /// </summary>
    constructor Create(
      ABatteryCache: IBatteryCache;
      ADisplayItemBuilder: IDeviceDisplayItemBuilder;
      ADeviceListView: IDeviceListView;
      AAsyncExecutor: IAsyncExecutor;
      AFindDeviceFunc: TFindDeviceFunc;
      AGetConnectedAddressesFunc: TGetConnectedAddressesFunc
    );

    /// <summary>
    /// Initializes battery cache event handler.
    /// Call after construction to wire up events.
    /// </summary>
    procedure Initialize;

    /// <summary>
    /// Refreshes battery for all currently connected devices.
    /// Called when view is shown or devices are loaded.
    /// </summary>
    procedure RefreshBatteryForConnectedDevices;

    /// <summary>
    /// Schedules delayed battery refresh for a device.
    /// Used after device connection to wait for Windows battery drivers to update.
    /// </summary>
    procedure ScheduleDelayedBatteryRefresh(AAddress: UInt64; ADelayMs: Integer);

    /// <summary>
    /// Marks coordinator as shutting down to prevent async callback execution.
    /// </summary>
    procedure Shutdown;

    /// <summary>
    /// Gets the battery cache instance.
    /// </summary>
    property BatteryCache: IBatteryCache read FBatteryCache;
  end;

implementation

uses
  App.Logger;

{ TDeviceBatteryCoordinator }

constructor TDeviceBatteryCoordinator.Create(
  ABatteryCache: IBatteryCache;
  ADisplayItemBuilder: IDeviceDisplayItemBuilder;
  ADeviceListView: IDeviceListView;
  AAsyncExecutor: IAsyncExecutor;
  AFindDeviceFunc: TFindDeviceFunc;
  AGetConnectedAddressesFunc: TGetConnectedAddressesFunc
);
begin
  inherited Create;
  FBatteryCache := ABatteryCache;
  FDisplayItemBuilder := ADisplayItemBuilder;
  FDeviceListView := ADeviceListView;
  FAsyncExecutor := AAsyncExecutor;
  FFindDeviceFunc := AFindDeviceFunc;
  FGetConnectedAddressesFunc := AGetConnectedAddressesFunc;
  FIsShuttingDown := False;
end;

procedure TDeviceBatteryCoordinator.Initialize;
begin
  FBatteryCache.OnQueryCompleted := HandleBatteryQueryCompleted;
end;

procedure TDeviceBatteryCoordinator.Shutdown;
begin
  FIsShuttingDown := True;
end;

procedure TDeviceBatteryCoordinator.HandleBatteryQueryCompleted(Sender: TObject;
  ADeviceAddress: UInt64; const AStatus: TBatteryStatus);
var
  Device: TBluetoothDeviceInfo;
  DisplayItem: TDeviceDisplayItem;
begin
  // Skip if coordinator is shutting down
  if FIsShuttingDown then
    Exit;

  LogDebug('HandleBatteryQueryCompleted: Address=$%.12X, Level=%d', [ADeviceAddress, AStatus.Level], ClassName);

  // Find the device using injected function - O(1) lookup
  Device := FFindDeviceFunc(ADeviceAddress);
  if Device.AddressInt = 0 then
  begin
    LogDebug('HandleBatteryQueryCompleted: Device not found, skipping update', ClassName);
    Exit;
  end;

  // Build single display item and update UI - O(1) update instead of O(n) full rebuild
  // Battery level doesn't affect sort order, so single-item update is safe
  DisplayItem := FDisplayItemBuilder.BuildDisplayItem(Device);
  FDeviceListView.UpdateDisplayItem(DisplayItem);
  LogDebug('HandleBatteryQueryCompleted: Updated single display item', ClassName);
end;

procedure TDeviceBatteryCoordinator.RefreshBatteryForConnectedDevices;
var
  ConnectedAddresses: TArray<UInt64>;
begin
  // Get connected addresses using injected function
  ConnectedAddresses := FGetConnectedAddressesFunc();

  if Length(ConnectedAddresses) > 0 then
  begin
    LogDebug('RefreshBatteryForConnectedDevices: Refreshing %d devices', [Length(ConnectedAddresses)], ClassName);
    FBatteryCache.RequestRefreshAll(ConnectedAddresses);
  end;
end;

procedure TDeviceBatteryCoordinator.ScheduleDelayedBatteryRefresh(AAddress: UInt64; ADelayMs: Integer);
var
  LAddress: UInt64;
  LBatteryCache: IBatteryCache;
  LSelf: TDeviceBatteryCoordinator;
begin
  LAddress := AAddress;
  LBatteryCache := FBatteryCache;
  LSelf := Self;

  FAsyncExecutor.RunDelayed(
    procedure
    begin
      // Check shutdown flag before executing async callback
      if not LSelf.FIsShuttingDown then
      begin
        // LBatteryCache is always valid (real or null object)
        LBatteryCache.RequestRefresh(LAddress);
      end;
    end,
    ADelayMs
  );
end;

end.
