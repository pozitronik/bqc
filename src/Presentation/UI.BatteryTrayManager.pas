{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Battery Tray Icon Manager                       }
{                                                       }
{*******************************************************}

/// <summary>
/// Manages per-device battery level tray icons.
/// Creates and updates system tray icons for each connected device with battery info.
/// </summary>
unit UI.BatteryTrayManager;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Menus,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  App.BatteryTrayConfigIntf,
  App.SystemThemeDetector,
  UI.BatteryIconRenderer;

const
  /// <summary>
  /// Callback message for all battery tray icons.
  /// Icons are distinguished by uID field passed in wParam.
  /// Using WM_USER + 201 to avoid conflict with:
  ///   - WM_HOTKEY_DETECTED (WM_USER + 100)
  ///   - WM_TRAYICON (WM_USER + 101)
  ///   - WM_FOREGROUND_LOST (WM_USER + 200)
  /// </summary>
  WM_BATTERYTRAY_CALLBACK = WM_USER + 201;

  /// <summary>
  /// Shell icon version constants for modern behavior.
  /// </summary>
  NIM_SETVERSION = $00000004;
  NOTIFYICON_VERSION_4 = 4;
  NIF_SHOWTIP = $00000080;

type
  /// <summary>
  /// Notification state for a device to prevent repeated notifications.
  /// </summary>
  TDeviceBatteryState = record
    Address: UInt64;
    LastLevel: Integer;
    LowBatteryNotified: Boolean;
    FullyChargedNotified: Boolean;
  end;

  /// <summary>
  /// Cached icon parameters to avoid redundant GDI recreation.
  /// Stores all visual parameters that affect icon appearance.
  /// </summary>
  TIconCacheEntry = record
    Level: Integer;
    Color: TColor;
    BackgroundColor: TColor;
    OutlineColor: TColor;
    Threshold: Integer;
    ShowNumeric: Boolean;
    IsPending: Boolean;
    Name: string;
  end;

  /// <summary>
  /// Callback for device tray icon click events.
  /// </summary>
  TDeviceTrayClickEvent = procedure(Sender: TObject; AAddress: UInt64) of object;

  /// <summary>
  /// Callback for battery notification events.
  /// </summary>
  TBatteryNotificationEvent = procedure(Sender: TObject; AAddress: UInt64;
    const ADeviceName: string; ALevel: Integer; AIsLowBattery: Boolean) of object;

  /// <summary>
  /// Manages multiple tray icons for device battery levels.
  /// Each connected device with battery info gets its own tray icon.
  /// </summary>
  TBatteryTrayManager = class
  private
    FOwnerHandle: HWND;
    FDeviceIcons: TDictionary<UInt64, TNotifyIconData>;
    FBatteryStates: TDictionary<UInt64, TDeviceBatteryState>;
    FIconCache: TDictionary<UInt64, TIconCacheEntry>;
    FConfig: IBatteryTrayConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FNextIconId: Cardinal;
    FOnDeviceClick: TDeviceTrayClickEvent;
    FOnBatteryNotification: TBatteryNotificationEvent;
    FEnabled: Boolean;
    FInUpdate: Boolean;  // Re-entrancy guard: Shell_NotifyIcon can pump Windows messages

    function GetNextIconId: Cardinal;
    function CreateNotifyIconData(AAddress: UInt64; const AName: string;
      ALevel: Integer; AColor: TColor; ABackgroundColor: TColor;
      AShowNumeric: Boolean): TNotifyIconData;
    function IconCacheMatches(const ACached, ANew: TIconCacheEntry): Boolean;
    function GetEffectiveColor(AAddress: UInt64): TColor;
    function GetEffectiveBackgroundColor(AAddress: UInt64): TColor;
    function GetEffectiveOutlineColor: TColor;
    function GetEffectiveThreshold(AAddress: UInt64): Integer;
    function ShouldShowTrayIcon(AAddress: UInt64): Boolean;
    function ShouldShowNumericValue(AAddress: UInt64): Boolean;
    function ShouldNotifyLowBattery(AAddress: UInt64): Boolean;
    function ShouldNotifyFullyCharged(AAddress: UInt64): Boolean;
    procedure UpdateBatteryState(AAddress: UInt64; ALevel: Integer;
      const ADeviceName: string);
    procedure CheckBatteryNotifications(AAddress: UInt64; ALevel: Integer;
      const ADeviceName: string);
  public
    constructor Create(AOwnerHandle: HWND; AConfig: IBatteryTrayConfig;
      ADeviceConfigProvider: IDeviceConfigProvider);
    destructor Destroy; override;

    /// <summary>
    /// Adds or updates a device battery tray icon.
    /// </summary>
    procedure UpdateDevice(AAddress: UInt64; const AName: string;
      ALevel: Integer; AIsConnected: Boolean);

    /// <summary>
    /// Shows a pending/loading tray icon for a device.
    /// Used when device is connected but battery level is being refreshed.
    /// </summary>
    procedure UpdateDevicePending(AAddress: UInt64; const AName: string);

    /// <summary>
    /// Removes a device's tray icon.
    /// </summary>
    procedure RemoveDevice(AAddress: UInt64);

    /// <summary>
    /// Removes all device tray icons.
    /// </summary>
    procedure ClearAll;

    /// <summary>
    /// Refreshes all device icons (e.g., after config change).
    /// </summary>
    procedure RefreshAll;

    /// <summary>
    /// Enables or disables the battery tray icons.
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;

    /// <summary>
    /// Called when a device tray icon is clicked.
    /// </summary>
    property OnDeviceClick: TDeviceTrayClickEvent read FOnDeviceClick write FOnDeviceClick;

    /// <summary>
    /// Called when a battery notification should be shown.
    /// </summary>
    property OnBatteryNotification: TBatteryNotificationEvent
      read FOnBatteryNotification write FOnBatteryNotification;
  end;

const
  // Hysteresis values to prevent notification spam
  LOW_BATTERY_RESET_MARGIN = 5;  // Reset low battery flag when level rises above threshold + 5%
  FULLY_CHARGED_RESET_LEVEL = 95; // Reset fully charged flag when level drops below 95%

implementation

uses
  App.Logger;

{ TBatteryTrayManager }

constructor TBatteryTrayManager.Create(AOwnerHandle: HWND;
  AConfig: IBatteryTrayConfig; ADeviceConfigProvider: IDeviceConfigProvider);
begin
  inherited Create;
  FOwnerHandle := AOwnerHandle;
  FConfig := AConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FDeviceIcons := TDictionary<UInt64, TNotifyIconData>.Create;
  FBatteryStates := TDictionary<UInt64, TDeviceBatteryState>.Create;
  FIconCache := TDictionary<UInt64, TIconCacheEntry>.Create;
  FNextIconId := 1000; // Start from 1000 to avoid conflicts with main tray icon
  FEnabled := True;
  FInUpdate := False;
end;

destructor TBatteryTrayManager.Destroy;
begin
  ClearAll;
  FIconCache.Free;
  FBatteryStates.Free;
  FDeviceIcons.Free;
  inherited Destroy;
end;

function TBatteryTrayManager.GetNextIconId: Cardinal;
begin
  Result := FNextIconId;
  Inc(FNextIconId);
end;

function TBatteryTrayManager.IconCacheMatches(const ACached, ANew: TIconCacheEntry): Boolean;
begin
  Result := (ACached.Level = ANew.Level) and
            (ACached.Color = ANew.Color) and
            (ACached.BackgroundColor = ANew.BackgroundColor) and
            (ACached.OutlineColor = ANew.OutlineColor) and
            (ACached.Threshold = ANew.Threshold) and
            (ACached.ShowNumeric = ANew.ShowNumeric) and
            (ACached.IsPending = ANew.IsPending) and
            (ACached.Name = ANew.Name);
end;

function TBatteryTrayManager.GetEffectiveColor(AAddress: UInt64): TColor;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    if DeviceConfig.BatteryTray.IconColor >= 0 then
      Exit(TColor(DeviceConfig.BatteryTray.IconColor));
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultIconColor
  else
    Result := clGreen;
end;

function TBatteryTrayManager.GetEffectiveBackgroundColor(AAddress: UInt64): TColor;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    // -1 = use global default, -2 = transparent, >= 0 = custom color
    if DeviceConfig.BatteryTray.BackgroundColor = -2 then
      Exit(TColor($1FFFFFFF))  // Transparent
    else if DeviceConfig.BatteryTray.BackgroundColor >= 0 then
      Exit(TColor(DeviceConfig.BatteryTray.BackgroundColor));
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultBackgroundColor
  else
    Result := TColor($1FFFFFFF); // Transparent by default
end;

function TBatteryTrayManager.GetEffectiveOutlineColor: TColor;
var
  Mode: TOutlineColorMode;
begin
  if Assigned(FConfig) then
  begin
    Mode := FConfig.DefaultOutlineColorMode;
    case Mode of
      ocmAuto:
        begin
          // Auto-detect from Windows dark/light mode
          if TSystemThemeDetector.IsDarkMode then
            Result := BATTERY_OUTLINE_COLOR_LIGHT  // White on dark taskbar
          else
            Result := BATTERY_OUTLINE_COLOR_DARK;  // Black on light taskbar
        end;
      ocmLight:
        Result := BATTERY_OUTLINE_COLOR_LIGHT;
      ocmDark:
        Result := BATTERY_OUTLINE_COLOR_DARK;
      ocmCustom:
        Result := FConfig.DefaultCustomOutlineColor;
    else
      Result := BATTERY_OUTLINE_COLOR_DARK;
    end;
  end
  else
    Result := BATTERY_OUTLINE_COLOR_DARK;
end;

function TBatteryTrayManager.GetEffectiveThreshold(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    if DeviceConfig.BatteryTray.LowBatteryThreshold >= 0 then
      Exit(DeviceConfig.BatteryTray.LowBatteryThreshold);
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultLowBatteryThreshold
  else
    Result := 20;
end;

function TBatteryTrayManager.ShouldShowTrayIcon(AAddress: UInt64): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  // Check if battery tray icons are globally enabled
  if Assigned(FConfig) and not FConfig.ShowBatteryTrayIcons then
    Exit(False);

  // Check per-device override
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    case DeviceConfig.BatteryTray.ShowTrayIcon of
      0: Exit(False);  // Explicitly disabled
      1: Exit(True);   // Explicitly enabled
      // -1: Use global default (already checked above)
    end;
  end;

  Result := True;
end;

function TBatteryTrayManager.ShouldShowNumericValue(AAddress: UInt64): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    case DeviceConfig.BatteryTray.ShowNumericValue of
      0: Exit(False);
      1: Exit(True);
    end;
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultShowNumericValue
  else
    Result := False;
end;

function TBatteryTrayManager.ShouldNotifyLowBattery(AAddress: UInt64): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    case DeviceConfig.BatteryTray.NotifyLowBattery of
      0: Exit(False);
      1: Exit(True);
    end;
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultNotifyLowBattery
  else
    Result := True;
end;

function TBatteryTrayManager.ShouldNotifyFullyCharged(AAddress: UInt64): Boolean;
var
  DeviceConfig: TDeviceConfig;
begin
  if Assigned(FDeviceConfigProvider) then
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(AAddress);
    case DeviceConfig.BatteryTray.NotifyFullyCharged of
      0: Exit(False);
      1: Exit(True);
    end;
  end;

  if Assigned(FConfig) then
    Result := FConfig.DefaultNotifyFullyCharged
  else
    Result := False;
end;

function TBatteryTrayManager.CreateNotifyIconData(AAddress: UInt64;
  const AName: string; ALevel: Integer; AColor: TColor; ABackgroundColor: TColor;
  AShowNumeric: Boolean): TNotifyIconData;
var
  Icon: TIcon;
  Threshold: Integer;
  OutlineColor: TColor;
  Tooltip: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(TNotifyIconData);
  Result.Wnd := FOwnerHandle;
  Result.uID := GetNextIconId;
  Result.uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE or NIF_SHOWTIP;
  Result.uCallbackMessage := WM_BATTERYTRAY_CALLBACK; // Same message for all icons, uID distinguishes them

  // Create battery icon (numeric or graphical)
  Threshold := GetEffectiveThreshold(AAddress);
  OutlineColor := GetEffectiveOutlineColor;
  if AShowNumeric then
    Icon := TBatteryIconRenderer.CreateNumericIcon(ALevel, AColor, ABackgroundColor)
  else
    Icon := TBatteryIconRenderer.CreateBatteryIconAuto(ALevel, AColor, ABackgroundColor, Threshold, OutlineColor);
  try
    Result.hIcon := CopyIcon(Icon.Handle);
  finally
    Icon.Free;
  end;

  // Set tooltip
  Tooltip := Format('%s: %d%%', [AName, ALevel]);
  StrLCopy(Result.szTip, PChar(Tooltip), Length(Result.szTip) - 1);
end;

procedure TBatteryTrayManager.UpdateBatteryState(AAddress: UInt64;
  ALevel: Integer; const ADeviceName: string);
var
  State: TDeviceBatteryState;
  Threshold: Integer;
begin
  Threshold := GetEffectiveThreshold(AAddress);

  if FBatteryStates.TryGetValue(AAddress, State) then
  begin
    // Reset low battery notification if level rises above threshold + margin
    if State.LowBatteryNotified and (ALevel > Threshold + LOW_BATTERY_RESET_MARGIN) then
      State.LowBatteryNotified := False;

    // Reset fully charged notification if level drops below reset level
    if State.FullyChargedNotified and (ALevel < FULLY_CHARGED_RESET_LEVEL) then
      State.FullyChargedNotified := False;

    State.LastLevel := ALevel;
    FBatteryStates[AAddress] := State;
  end
  else
  begin
    // New device - initialize state
    State.Address := AAddress;
    State.LastLevel := ALevel;
    State.LowBatteryNotified := False;
    State.FullyChargedNotified := False;
    FBatteryStates.Add(AAddress, State);
  end;
end;

procedure TBatteryTrayManager.CheckBatteryNotifications(AAddress: UInt64;
  ALevel: Integer; const ADeviceName: string);
var
  State: TDeviceBatteryState;
  Threshold: Integer;
begin
  if not FBatteryStates.TryGetValue(AAddress, State) then
    Exit;

  Threshold := GetEffectiveThreshold(AAddress);

  // Check for low battery
  if (ALevel <= Threshold) and not State.LowBatteryNotified then
  begin
    if ShouldNotifyLowBattery(AAddress) then
    begin
      State.LowBatteryNotified := True;
      FBatteryStates[AAddress] := State;

      if Assigned(FOnBatteryNotification) then
        FOnBatteryNotification(Self, AAddress, ADeviceName, ALevel, True);

      LogInfo('Low battery notification for %s: %d%%', [ADeviceName, ALevel], ClassName);
    end;
  end;

  // Check for fully charged
  if (ALevel = 100) and not State.FullyChargedNotified then
  begin
    if ShouldNotifyFullyCharged(AAddress) then
    begin
      State.FullyChargedNotified := True;
      FBatteryStates[AAddress] := State;

      if Assigned(FOnBatteryNotification) then
        FOnBatteryNotification(Self, AAddress, ADeviceName, ALevel, False);

      LogInfo('Fully charged notification for %s', [ADeviceName], ClassName);
    end;
  end;
end;

procedure TBatteryTrayManager.UpdateDevice(AAddress: UInt64;
  const AName: string; ALevel: Integer; AIsConnected: Boolean);
var
  IconData: TNotifyIconData;
  ExistingIcon: TNotifyIconData;
  HasExistingIcon: Boolean;
  Color: TColor;
  BackgroundColor: TColor;
  OutlineColor: TColor;
  Threshold: Integer;
  ShowNumeric: Boolean;
  Icon: TIcon;
  Tooltip: string;
  OldIconHandle: HICON;
  NewCacheEntry, CachedEntry: TIconCacheEntry;
begin
  if not FEnabled then
    Exit;

  // Re-entrancy guard: Shell_NotifyIcon can pump Windows messages, potentially
  // triggering another RefreshDisplayItems while we're in the middle of an update.
  // Skip nested updates to prevent dictionary corruption.
  if FInUpdate then
  begin
    LogWarning('UpdateDevice: Re-entrancy detected for %s (Address=$%.12X), skipping',
      [AName, AAddress], ClassName);
    Exit;
  end;

  // Remove icon if device is not connected or battery level is invalid
  if not AIsConnected or (ALevel < 0) then
  begin
    RemoveDevice(AAddress);
    Exit;
  end;

  // Check if we should show tray icon for this device
  if not ShouldShowTrayIcon(AAddress) then
  begin
    RemoveDevice(AAddress);
    Exit;
  end;

  // Update battery state and check for notifications
  UpdateBatteryState(AAddress, ALevel, AName);
  CheckBatteryNotifications(AAddress, ALevel, AName);

  Color := GetEffectiveColor(AAddress);
  BackgroundColor := GetEffectiveBackgroundColor(AAddress);
  OutlineColor := GetEffectiveOutlineColor;
  Threshold := GetEffectiveThreshold(AAddress);
  ShowNumeric := ShouldShowNumericValue(AAddress);

  // Build cache entry for current parameters
  NewCacheEntry.Level := ALevel;
  NewCacheEntry.Color := Color;
  NewCacheEntry.BackgroundColor := BackgroundColor;
  NewCacheEntry.OutlineColor := OutlineColor;
  NewCacheEntry.Threshold := Threshold;
  NewCacheEntry.ShowNumeric := ShowNumeric;
  NewCacheEntry.IsPending := False;
  NewCacheEntry.Name := AName;

  // Single dictionary lookup for existing icon (avoid redundant ContainsKey + TryGetValue)
  HasExistingIcon := FDeviceIcons.TryGetValue(AAddress, ExistingIcon);

  // Check if we already have an icon with identical parameters - skip GDI recreation
  if HasExistingIcon and
     FIconCache.TryGetValue(AAddress, CachedEntry) and
     IconCacheMatches(CachedEntry, NewCacheEntry) then
  begin
    Exit;
  end;

  FInUpdate := True;
  try
    if HasExistingIcon then
    begin
      // Update existing icon
      if ShowNumeric then
        Icon := TBatteryIconRenderer.CreateNumericIcon(ALevel, Color, BackgroundColor)
      else
        Icon := TBatteryIconRenderer.CreateBatteryIconAuto(ALevel, Color, BackgroundColor, Threshold, OutlineColor);
      try
        OldIconHandle := ExistingIcon.hIcon;
        ExistingIcon.hIcon := CopyIcon(Icon.Handle);
        Tooltip := Format('%s: %d%%', [AName, ALevel]);
        StrLCopy(ExistingIcon.szTip, PChar(Tooltip), Length(ExistingIcon.szTip) - 1);

        if Shell_NotifyIcon(NIM_MODIFY, @ExistingIcon) then
        begin
          // Success - destroy old icon handle and update dictionary
          if OldIconHandle <> 0 then
            DestroyIcon(OldIconHandle);
          FDeviceIcons[AAddress] := ExistingIcon;
          FIconCache[AAddress] := NewCacheEntry;
        end
        else
        begin
          // Failed - destroy new icon handle, keep old one
          if ExistingIcon.hIcon <> 0 then
            DestroyIcon(ExistingIcon.hIcon);
          ExistingIcon.hIcon := OldIconHandle; // Restore old handle
          LogWarning('Failed to update battery tray icon for %s', [AName], ClassName);
        end;
      finally
        Icon.Free;
      end;
    end
    else
    begin
      // Create new icon
      IconData := CreateNotifyIconData(AAddress, AName, ALevel, Color, BackgroundColor, ShowNumeric);
      if Shell_NotifyIcon(NIM_ADD, @IconData) then
      begin
        // Set version for modern behavior (prevents issues on hover)
        // Note: uTimeout and uVersion share the same union offset
        IconData.uTimeout := NOTIFYICON_VERSION_4;
        Shell_NotifyIcon(NIM_SETVERSION, @IconData);

        FDeviceIcons.Add(AAddress, IconData);
        FIconCache[AAddress] := NewCacheEntry;
        LogDebug('Battery tray icon added for %s', [AName], ClassName);
      end
      else
      begin
        // Failed to add icon, cleanup
        if IconData.hIcon <> 0 then
          DestroyIcon(IconData.hIcon);
        LogWarning('Failed to add battery tray icon for %s', [AName], ClassName);
      end;
    end;
  finally
    FInUpdate := False;
  end;
end;

procedure TBatteryTrayManager.UpdateDevicePending(AAddress: UInt64; const AName: string);
var
  IconData: TNotifyIconData;
  ExistingIcon: TNotifyIconData;
  Icon: TIcon;
  Tooltip: string;
  OldIconHandle: HICON;
  NewCacheEntry, CachedEntry: TIconCacheEntry;
begin
  if not FEnabled then
    Exit;

  // Re-entrancy guard: Shell_NotifyIcon can pump Windows messages, potentially
  // triggering another RefreshDisplayItems while we're in the middle of an update.
  // Skip nested updates to prevent dictionary corruption.
  if FInUpdate then
  begin
    LogWarning('UpdateDevicePending: Re-entrancy detected for %s (Address=$%.12X), skipping',
      [AName, AAddress], ClassName);
    Exit;
  end;

  // Check if we should show tray icon for this device
  if not ShouldShowTrayIcon(AAddress) then
  begin
    RemoveDevice(AAddress);
    Exit;
  end;

  // Build cache entry for pending icon
  NewCacheEntry.Level := -1;
  NewCacheEntry.Color := 0;
  NewCacheEntry.BackgroundColor := 0;
  NewCacheEntry.OutlineColor := 0;
  NewCacheEntry.Threshold := 0;
  NewCacheEntry.ShowNumeric := False;
  NewCacheEntry.IsPending := True;
  NewCacheEntry.Name := AName;

  // Check if we already have a pending icon with same name - skip GDI recreation
  if FDeviceIcons.ContainsKey(AAddress) and
     FIconCache.TryGetValue(AAddress, CachedEntry) and
     IconCacheMatches(CachedEntry, NewCacheEntry) then
  begin
    Exit;
  end;

  Tooltip := Format('%s: ...', [AName]);

  FInUpdate := True;
  try
    if FDeviceIcons.TryGetValue(AAddress, ExistingIcon) then
    begin
      // Update existing icon to pending
      Icon := TBatteryIconRenderer.CreatePendingBatteryIcon;
      try
        OldIconHandle := ExistingIcon.hIcon;
        ExistingIcon.hIcon := CopyIcon(Icon.Handle);
        StrLCopy(ExistingIcon.szTip, PChar(Tooltip), Length(ExistingIcon.szTip) - 1);

        if Shell_NotifyIcon(NIM_MODIFY, @ExistingIcon) then
        begin
          if OldIconHandle <> 0 then
            DestroyIcon(OldIconHandle);
          FDeviceIcons[AAddress] := ExistingIcon;
          FIconCache[AAddress] := NewCacheEntry;
        end
        else
        begin
          if ExistingIcon.hIcon <> 0 then
            DestroyIcon(ExistingIcon.hIcon);
          ExistingIcon.hIcon := OldIconHandle;
          LogWarning('Failed to update battery tray icon to pending for %s', [AName], ClassName);
        end;
      finally
        Icon.Free;
      end;
    end
    else
    begin
      // Create new pending icon
      FillChar(IconData, SizeOf(IconData), 0);
      IconData.cbSize := SizeOf(TNotifyIconData);
      IconData.Wnd := FOwnerHandle;
      IconData.uID := GetNextIconId;
      IconData.uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE or NIF_SHOWTIP;
      IconData.uCallbackMessage := WM_BATTERYTRAY_CALLBACK;

      Icon := TBatteryIconRenderer.CreatePendingBatteryIcon;
      try
        IconData.hIcon := CopyIcon(Icon.Handle);
      finally
        Icon.Free;
      end;

      StrLCopy(IconData.szTip, PChar(Tooltip), Length(IconData.szTip) - 1);

      if Shell_NotifyIcon(NIM_ADD, @IconData) then
      begin
        IconData.uTimeout := NOTIFYICON_VERSION_4;
        Shell_NotifyIcon(NIM_SETVERSION, @IconData);
        FDeviceIcons.Add(AAddress, IconData);
        FIconCache[AAddress] := NewCacheEntry;
        LogDebug('Pending battery tray icon added for %s', [AName], ClassName);
      end
      else
      begin
        if IconData.hIcon <> 0 then
          DestroyIcon(IconData.hIcon);
        LogWarning('Failed to add pending battery tray icon for %s', [AName], ClassName);
      end;
    end;
  finally
    FInUpdate := False;
  end;
end;

procedure TBatteryTrayManager.RemoveDevice(AAddress: UInt64);
var
  IconData: TNotifyIconData;
begin
  if FDeviceIcons.TryGetValue(AAddress, IconData) then
  begin
    Shell_NotifyIcon(NIM_DELETE, @IconData);
    if IconData.hIcon <> 0 then
      DestroyIcon(IconData.hIcon);
    FDeviceIcons.Remove(AAddress);
    LogDebug('Battery tray icon removed for device $%.12X', [AAddress], ClassName);
  end;

  // Also remove battery state and icon cache
  FBatteryStates.Remove(AAddress);
  FIconCache.Remove(AAddress);
end;

procedure TBatteryTrayManager.ClearAll;
var
  Pair: TPair<UInt64, TNotifyIconData>;
  IconData: TNotifyIconData;
begin
  for Pair in FDeviceIcons do
  begin
    IconData := Pair.Value;
    Shell_NotifyIcon(NIM_DELETE, @IconData);
    if IconData.hIcon <> 0 then
      DestroyIcon(IconData.hIcon);
  end;
  FDeviceIcons.Clear;
  FBatteryStates.Clear;
  FIconCache.Clear;
  LogDebug('All battery tray icons cleared', ClassName);
end;

procedure TBatteryTrayManager.RefreshAll;
var
  Addresses: TArray<UInt64>;
  Address: UInt64;
begin
  // Store addresses to avoid modifying dictionary during iteration
  Addresses := FDeviceIcons.Keys.ToArray;

  // Remove all icons first
  for Address in Addresses do
  begin
    RemoveDevice(Address);
  end;

  // Icons will be re-added on next UpdateDevice call from presenter
  LogDebug('Battery tray icons refresh requested', ClassName);
end;

end.
