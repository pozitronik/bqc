{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Discovery Coordinator                    }
{                                                       }
{       EXTRACTED FROM: App.MainPresenter (god class)   }
{       Handles device discovery and scanning.          }
{                                                       }
{*******************************************************}

unit App.DeviceDiscoveryCoordinator;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  App.MainViewInterfaces,
  App.AsyncExecutor;

type
  /// <summary>
  /// Procedure type for refreshing display items in UI.
  /// </summary>
  TRefreshDisplayItemsProc = reference to procedure;

  /// <summary>
  /// Procedure type for clearing device status message.
  /// </summary>
  TClearDeviceStatusProc = reference to procedure(ADeviceAddress: UInt64);

  /// <summary>
  /// Procedure type for queueing work with shutdown safety check.
  /// </summary>
  TQueueIfNotShutdownProc = reference to procedure(AProc: TProc);

  /// <summary>
  /// Coordinator for device discovery logic.
  /// Extracted from TMainPresenter to separate discovery concerns.
  /// Manages unpaired device cache, handles discovery events, controls scanning.
  /// </summary>
  TDeviceDiscoveryCoordinator = class
  private
    FBluetoothService: IBluetoothService;
    FStatusView: IStatusView;
    FAsyncExecutor: IAsyncExecutor;
    FRefreshDisplayItems: TRefreshDisplayItemsProc;
    FClearDeviceStatus: TClearDeviceStatusProc;
    FQueueIfNotShutdown: TQueueIfNotShutdownProc;

    FUnpairedDevicesInRange: TList<TBluetoothDeviceInfo>;
    FUnpairedDeviceIndexMap: TDictionary<UInt64, Integer>;
    FIsScanning: Boolean;

    /// <summary>
    /// Handles device discovered event from Bluetooth service.
    /// Adds/updates unpaired devices in cache and refreshes UI.
    /// </summary>
    procedure HandleDeviceDiscovered(Sender: TObject; const ADevice: TBluetoothDeviceInfo);

    /// <summary>
    /// Handles device out of range event from Bluetooth service.
    /// Removes device from unpaired cache and refreshes UI.
    /// </summary>
    procedure HandleDeviceOutOfRange(Sender: TObject; const ADeviceAddress: UInt64);

  public
    /// <summary>
    /// Creates discovery coordinator with dependencies.
    /// </summary>
    constructor Create(
      ABluetoothService: IBluetoothService;
      AStatusView: IStatusView;
      AAsyncExecutor: IAsyncExecutor;
      ARefreshDisplayItems: TRefreshDisplayItemsProc;
      AClearDeviceStatus: TClearDeviceStatusProc;
      AQueueIfNotShutdown: TQueueIfNotShutdownProc
    );

    /// <summary>
    /// Destructor - frees owned resources.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    /// Initializes event handlers for Bluetooth service.
    /// Call after construction to wire up discovery events.
    /// </summary>
    procedure Initialize;

    /// <summary>
    /// Starts asynchronous Bluetooth device scan.
    /// Prevents concurrent scans, updates UI, runs scan in background thread.
    /// </summary>
    procedure StartScan;

    /// <summary>
    /// Clears unpaired devices cache.
    /// Called on refresh to reset volatile discovery data.
    /// </summary>
    procedure ClearUnpairedDevices;

    /// <summary>
    /// Finds unpaired device by address.
    /// Returns empty device record (AddressInt=0) if not found.
    /// </summary>
    function FindUnpairedDevice(AAddress: UInt64): TBluetoothDeviceInfo;

    /// <summary>
    /// Gets read-only access to unpaired devices list.
    /// Used by presenter to build display items.
    /// </summary>
    property UnpairedDevices: TList<TBluetoothDeviceInfo> read FUnpairedDevicesInRange;

    /// <summary>
    /// Gets current scanning state.
    /// Used by presenter to show scan button text and progress.
    /// </summary>
    property IsScanning: Boolean read FIsScanning;
  end;

implementation

uses
  App.Logger;

{ TDeviceDiscoveryCoordinator }

constructor TDeviceDiscoveryCoordinator.Create(
  ABluetoothService: IBluetoothService;
  AStatusView: IStatusView;
  AAsyncExecutor: IAsyncExecutor;
  ARefreshDisplayItems: TRefreshDisplayItemsProc;
  AClearDeviceStatus: TClearDeviceStatusProc;
  AQueueIfNotShutdown: TQueueIfNotShutdownProc
);
begin
  inherited Create;
  FBluetoothService := ABluetoothService;
  FStatusView := AStatusView;
  FAsyncExecutor := AAsyncExecutor;
  FRefreshDisplayItems := ARefreshDisplayItems;
  FClearDeviceStatus := AClearDeviceStatus;
  FQueueIfNotShutdown := AQueueIfNotShutdown;

  FUnpairedDevicesInRange := TList<TBluetoothDeviceInfo>.Create;
  FUnpairedDeviceIndexMap := TDictionary<UInt64, Integer>.Create;
  FIsScanning := False;
end;

destructor TDeviceDiscoveryCoordinator.Destroy;
begin
  FUnpairedDevicesInRange.Free;
  FUnpairedDeviceIndexMap.Free;
  inherited;
end;

procedure TDeviceDiscoveryCoordinator.Initialize;
begin
  FBluetoothService.OnDeviceDiscovered := HandleDeviceDiscovered;
  FBluetoothService.OnDeviceOutOfRange := HandleDeviceOutOfRange;
end;

procedure TDeviceDiscoveryCoordinator.HandleDeviceDiscovered(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  Index: Integer;
begin
  FQueueIfNotShutdown(
    procedure
    begin
      // Only track unpaired devices
      if ADevice.IsPaired then
        Exit;

      LogDebug('HandleDeviceDiscovered: Unpaired device $%.12X, Name="%s"', [
        ADevice.AddressInt, ADevice.Name
      ], ClassName);

      // Add or update in unpaired devices cache
      if FUnpairedDeviceIndexMap.TryGetValue(ADevice.AddressInt, Index) then
      begin
        // Device already in cache - update info
        FUnpairedDevicesInRange[Index] := ADevice;
        LogDebug('HandleDeviceDiscovered: Updated unpaired device at index %d', [Index], ClassName);
      end
      else
      begin
        // New unpaired device - append and add to index
        Index := FUnpairedDevicesInRange.Count;
        FUnpairedDevicesInRange.Add(ADevice);
        FUnpairedDeviceIndexMap.Add(ADevice.AddressInt, Index);
        LogDebug('HandleDeviceDiscovered: Added new unpaired device at index %d', [Index], ClassName);
      end;

      // Refresh display to show updated unpaired devices list
      FRefreshDisplayItems();
    end
  );
end;

procedure TDeviceDiscoveryCoordinator.HandleDeviceOutOfRange(Sender: TObject;
  const ADeviceAddress: UInt64);
var
  Index: Integer;
begin
  FQueueIfNotShutdown(
    procedure
    var
      I: Integer;
    begin
      LogDebug('HandleDeviceOutOfRange: Device $%.12X left range', [ADeviceAddress], ClassName);

      // Clear any status message for this device since it's going away
      FClearDeviceStatus(ADeviceAddress);

      // Remove from unpaired devices cache if present
      if FUnpairedDeviceIndexMap.TryGetValue(ADeviceAddress, Index) then
      begin
        FUnpairedDevicesInRange.Delete(Index);
        FUnpairedDeviceIndexMap.Remove(ADeviceAddress);

        // Only update indices >= removed position: O(n-k) instead of O(2n)
        for I := Index to FUnpairedDevicesInRange.Count - 1 do
          FUnpairedDeviceIndexMap.AddOrSetValue(FUnpairedDevicesInRange[I].AddressInt, I);

        LogDebug('HandleDeviceOutOfRange: Removed unpaired device from cache', ClassName);

        // Refresh display to remove device from UI
        FRefreshDisplayItems();
      end;
    end
  );
end;

procedure TDeviceDiscoveryCoordinator.StartScan;
begin
  // Prevent concurrent scans
  if FIsScanning then
  begin
    LogDebug('StartScan: Scan already in progress, ignoring', ClassName);
    Exit;
  end;

  LogInfo('StartScan: Starting async device scan', ClassName);
  FIsScanning := True;
  FStatusView.SetScanning(True);
  FRefreshDisplayItems();  // Update UI immediately to show progress line
  FStatusView.ShowStatus('Scanning for devices...');

  // Run scan in background thread
  FAsyncExecutor.RunAsync(
    procedure
    begin
      try
        // Background work: perform Bluetooth inquiry
        FBluetoothService.ScanForNearbyDevices;
      finally
        // Queue completion on main thread
        FQueueIfNotShutdown(
          procedure
          begin
            FIsScanning := False;
            FStatusView.SetScanning(False);
            FRefreshDisplayItems();  // Update UI to hide progress line
            FStatusView.ShowStatus('Scan complete');
            LogInfo('StartScan: Scan complete', ClassName);
          end
        );
      end;
    end
  );
end;

procedure TDeviceDiscoveryCoordinator.ClearUnpairedDevices;
begin
  LogInfo('ClearUnpairedDevices: Clearing unpaired devices cache', ClassName);
  FUnpairedDevicesInRange.Clear;
  FUnpairedDeviceIndexMap.Clear;
end;

function TDeviceDiscoveryCoordinator.FindUnpairedDevice(AAddress: UInt64): TBluetoothDeviceInfo;
var
  Index: Integer;
begin
  if FUnpairedDeviceIndexMap.TryGetValue(AAddress, Index) then
    Result := FUnpairedDevicesInRange[Index]
  else
    Result := Default(TBluetoothDeviceInfo);  // Empty record with AddressInt=0
end;

end.
