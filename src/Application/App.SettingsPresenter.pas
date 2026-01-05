{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Settings Presenter (MVP Pattern)                }
{                                                       }
{*******************************************************}

unit App.SettingsPresenter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.LogConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.NotificationConfigIntf,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf;

type
  //----------------------------------------------------------------------------
  // View Settings Records - Data transfer objects for view communication
  //----------------------------------------------------------------------------

  /// <summary>
  /// General window and behavior settings.
  /// </summary>
  TGeneralViewSettings = record
    WindowMode: TWindowMode;
    OnTop: Boolean;
    MinimizeToTray: Boolean;
    CloseToTray: Boolean;
    StartMinimized: Boolean;
    HideOnFocusLoss: Boolean;
    Autostart: Boolean;
    PositionMode: TPositionMode;
  end;

  /// <summary>
  /// Hotkey configuration settings.
  /// </summary>
  THotkeyViewSettings = record
    Hotkey: string;
    UseLowLevelHook: Boolean;
    CastPanelHotkey: string;
    BluetoothPanelHotkey: string;
  end;

  /// <summary>
  /// Visual appearance settings.
  /// </summary>
  TAppearanceViewSettings = record
    Theme: string;
    VsfDir: string;
    ShowAddresses: Boolean;
    ShowDeviceIcons: Boolean;
    ShowLastSeen: Boolean;
    LastSeenRelative: Boolean;
    ShowBatteryLevel: Boolean;
    ConnectedColor: TColor;
  end;

  /// <summary>
  /// Layout dimension and font size settings.
  /// </summary>
  TLayoutViewSettings = record
    ItemHeight: Integer;
    ItemPadding: Integer;
    ItemMargin: Integer;
    IconSize: Integer;
    CornerRadius: Integer;
    BorderWidth: Integer;
    BorderColor: TColor;
    DeviceNameFontSize: Integer;
    StatusFontSize: Integer;
    AddressFontSize: Integer;
    IconFontSize: Integer;
    ShowUnpairedDevices: Boolean;
    ShowUnidentifiedDevices: Boolean;
    ScrollbarWidth: Integer;
    ScrollbarOpacity: Integer;
  end;

  /// <summary>
  /// Connection and polling settings.
  /// </summary>
  TConnectionViewSettings = record
    Timeout: Integer;
    RetryCount: Integer;
    EnumerationMode: TEnumerationMode;
    BluetoothPlatform: TBluetoothPlatform;
    PollingMode: TPollingMode;
    PollingInterval: Integer;
    NotifyOnConnect: Boolean;
    NotifyOnDisconnect: Boolean;
    NotifyOnConnectFailed: Boolean;
    NotifyOnAutoConnect: Boolean;
  end;

  /// <summary>
  /// Logging configuration settings.
  /// </summary>
  TLoggingViewSettings = record
    Enabled: Boolean;
    Filename: string;
    Append: Boolean;
    LevelIndex: Integer;  // Index in log level ComboBox
  end;

  /// <summary>
  /// Per-device configuration settings.
  /// </summary>
  TDeviceViewSettings = record
    Alias: string;
    DeviceTypeIndex: Integer;
    Pinned: Boolean;
    Hidden: Boolean;
    AutoConnect: Boolean;
    Timeout: Integer;
    RetryCount: Integer;
    NotifyConnectIndex: Integer;
    NotifyDisconnectIndex: Integer;
    NotifyFailedIndex: Integer;
    NotifyAutoIndex: Integer;
    // Battery tray per-device overrides
    BatteryTrayIconIndex: Integer;   // 0=Default, 1=No, 2=Yes
    BatteryIconColor: Integer;       // -1=Default, or TColor value
    BatteryBackgroundColor: Integer; // -1=Default, or TColor value
    BatteryShowNumericIndex: Integer; // 0=Default, 1=No, 2=Yes
    BatteryThreshold: Integer;       // -1=Default, or 0-100
    BatteryNotifyLowIndex: Integer;  // 0=Default, 1=No, 2=Yes
    BatteryNotifyFullIndex: Integer; // 0=Default, 1=No, 2=Yes
    // Profile display per-device override
    ShowProfilesIndex: Integer;      // 0=Default, 1=No, 2=Yes
  end;

  /// <summary>
  /// Battery tray global settings.
  /// </summary>
  TBatteryTrayViewSettings = record
    ShowBatteryTrayIcons: Boolean;
    DefaultIconColor: TColor;
    DefaultBackgroundColor: TColor;
    DefaultShowNumericValue: Boolean;
    DefaultLowBatteryThreshold: Integer;
    DefaultNotifyLowBattery: Boolean;
    DefaultNotifyFullyCharged: Boolean;
    DefaultOutlineColorModeIndex: Integer;  // 0=Auto, 1=Light, 2=Dark, 3=Custom
    DefaultCustomOutlineColor: TColor;
  end;

  /// <summary>
  /// Profile display settings.
  /// </summary>
  TProfileViewSettings = record
    ShowProfiles: Boolean;
    ProfileFontSize: Integer;
  end;

  //----------------------------------------------------------------------------
  // Focused Settings View Interfaces (ISP-compliant)
  //----------------------------------------------------------------------------

  /// <summary>
  /// Dialog control operations for settings view.
  /// </summary>
  ISettingsDialogView = interface
    ['{D4E5F6A7-B8C9-0123-DEF0-456789012345}']
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);
    procedure PopulateThemeList(const ACurrentTheme: string);
  end;

  /// <summary>
  /// General settings view operations.
  /// </summary>
  IGeneralSettingsView = interface
    ['{E5F6A7B8-C9D0-1234-EF01-567890123456}']
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);
  end;

  /// <summary>
  /// Hotkey settings view operations.
  /// </summary>
  IHotkeySettingsView = interface
    ['{F6A7B8C9-D0E1-2345-F012-678901234567}']
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);
  end;

  /// <summary>
  /// Appearance settings view operations.
  /// </summary>
  IAppearanceSettingsView = interface
    ['{A7B8C9D0-E1F2-3456-0123-789012345678}']
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
  end;

  /// <summary>
  /// Layout settings view operations.
  /// </summary>
  ILayoutSettingsView = interface
    ['{B8C9D0E1-F2A3-4567-1234-890123456789}']
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);
  end;

  /// <summary>
  /// Connection settings view operations.
  /// </summary>
  IConnectionSettingsView = interface
    ['{C9D0E1F2-A3B4-5678-2345-901234567890}']
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);
  end;

  /// <summary>
  /// Logging settings view operations.
  /// </summary>
  ILoggingSettingsView = interface
    ['{D0E1F2A3-B4C5-6789-3456-012345678901}']
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);
  end;

  /// <summary>
  /// Device settings view operations.
  /// </summary>
  IDeviceSettingsView = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-345678901234}']
    procedure PopulateDeviceList(const AItems: TArray<string>);
    function GetSelectedDeviceIndex: Integer;
    procedure SetSelectedDeviceIndex(AIndex: Integer);
    function GetDeviceSettings: TDeviceViewSettings;
    procedure SetDeviceSettings(const ASettings: TDeviceViewSettings);
    procedure ClearDeviceSettings;
  end;

  /// <summary>
  /// Battery tray settings view operations.
  /// </summary>
  IBatteryTraySettingsView = interface
    ['{E1F2A3B4-C5D6-7890-ABCD-EF0123456789}']
    function GetBatteryTraySettings: TBatteryTrayViewSettings;
    procedure SetBatteryTraySettings(const ASettings: TBatteryTrayViewSettings);
  end;

  /// <summary>
  /// Profile display settings view operations.
  /// </summary>
  IProfileSettingsView = interface
    ['{F2A3B4C5-D6E7-8901-BCDE-F01234567890}']
    function GetProfileSettings: TProfileViewSettings;
    procedure SetProfileSettings(const ASettings: TProfileViewSettings);
  end;

  //----------------------------------------------------------------------------
  // TDeviceSettingsPresenter - Device settings presenter (SRP-compliant)
  //----------------------------------------------------------------------------

  /// <summary>
  /// Focused presenter for per-device configuration management.
  /// Extracted from TSettingsPresenter to follow Single Responsibility Principle.
  /// </summary>
  TDeviceSettingsPresenter = class
  private
    FView: IDeviceSettingsView;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FDeviceAddresses: TList<UInt64>;
    FSelectedDeviceIndex: Integer;
  public
    constructor Create(
      AView: IDeviceSettingsView;
      ADeviceConfigProvider: IDeviceConfigProvider
    );
    destructor Destroy; override;

    /// <summary>
    /// Loads the device list from configuration.
    /// </summary>
    procedure LoadDeviceList;

    /// <summary>
    /// Loads settings for a specific device by index.
    /// </summary>
    procedure LoadDeviceSettings(AIndex: Integer);

    /// <summary>
    /// Saves settings for a specific device by index.
    /// </summary>
    procedure SaveDeviceSettings(AIndex: Integer);

    /// <summary>
    /// Saves the currently selected device settings.
    /// </summary>
    procedure SaveCurrentDevice;

    /// <summary>
    /// Called when user selects a device in the list.
    /// </summary>
    procedure OnDeviceSelected(AIndex: Integer);

    /// <summary>
    /// Called when user wants to forget/remove a device.
    /// </summary>
    procedure OnForgetDeviceClicked(AIndex: Integer);

    /// <summary>
    /// Called when user requests to refresh the device list.
    /// </summary>
    procedure OnRefreshDevicesClicked;
  end;

  //----------------------------------------------------------------------------
  // TSettingsPresenter - General settings dialog presenter
  //----------------------------------------------------------------------------

  /// <summary>
  /// Presenter for the Settings dialog.
  /// Handles loading and saving general configuration.
  /// Device settings are delegated to TDeviceSettingsPresenter (SRP).
  /// </summary>
  TSettingsPresenter = class
  private
    // View interfaces (ISP-compliant)
    FDialogView: ISettingsDialogView;
    FGeneralSettingsView: IGeneralSettingsView;
    FHotkeySettingsView: IHotkeySettingsView;
    FAppearanceSettingsView: IAppearanceSettingsView;
    FLayoutSettingsView: ILayoutSettingsView;
    FConnectionSettingsView: IConnectionSettingsView;
    FLoggingSettingsView: ILoggingSettingsView;
    FBatteryTraySettingsView: IBatteryTraySettingsView;
    FProfileSettingsView: IProfileSettingsView;

    FDevicePresenter: TDeviceSettingsPresenter;
    FAppConfig: IAppConfig;
    FModified: Boolean;
    FOnSettingsApplied: TNotifyEvent;

    // Config interface fields (extracted once in constructor, eliminates duplicate extraction)
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FPositionConfig: IPositionConfig;
    FHotkeyConfig: IHotkeyConfig;
    FAppearanceConfig: IAppearanceConfig;
    FLayoutConfig: ILayoutConfig;
    FConnectionConfig: IConnectionConfig;
    FPollingConfig: IPollingConfig;
    FNotificationConfig: INotificationConfig;
    FLogConfig: ILogConfig;
    FBatteryTrayConfig: IBatteryTrayConfig;
    FProfileConfig: IProfileConfig;

    function ValidateHotkeys(const AHotkey: THotkeyViewSettings): Boolean;
  public
    constructor Create(
      ADialogView: ISettingsDialogView;
      AGeneralSettingsView: IGeneralSettingsView;
      AHotkeySettingsView: IHotkeySettingsView;
      AAppearanceSettingsView: IAppearanceSettingsView;
      ALayoutSettingsView: ILayoutSettingsView;
      AConnectionSettingsView: IConnectionSettingsView;
      ALoggingSettingsView: ILoggingSettingsView;
      ABatteryTraySettingsView: IBatteryTraySettingsView;
      AProfileSettingsView: IProfileSettingsView;
      ADeviceSettingsView: IDeviceSettingsView;
      AAppConfig: IAppConfig;
      ADeviceConfigProvider: IDeviceConfigProvider;
      ABatteryTrayConfig: IBatteryTrayConfig;
      AProfileConfig: IProfileConfig
    );
    destructor Destroy; override;

    procedure LoadSettings;
    function SaveSettings: Boolean;

    procedure OnOKClicked;
    procedure OnCancelClicked;
    procedure OnApplyClicked;

    procedure OnResetSizeClicked;
    procedure OnResetPositionClicked;
    procedure OnDeviceSelected(AIndex: Integer);
    procedure OnForgetDeviceClicked(AIndex: Integer);
    procedure OnRefreshDevicesClicked;
    procedure OnResetDefaultsClicked;
    procedure OnResetLayoutClicked;

    procedure MarkModified;
    property IsModified: Boolean read FModified;
    property OnSettingsApplied: TNotifyEvent read FOnSettingsApplied write FOnSettingsApplied;
  end;

implementation

uses
  App.Logger,
  App.Config,  // For DEF_* constants
  App.DeviceConfigTypes,
  Bluetooth.Types;

const
  /// <summary>
  /// UI index offset for ComboBox items that include "Default" or "Use Global" at index 0.
  /// Config stores -1 for "use default", 0 for first real option, etc.
  /// UI ComboBox has "Default" at index 0, first real option at index 1, etc.
  /// Conversion: UIIndex = ConfigValue + UI_DEFAULT_ITEM_OFFSET
  ///             ConfigValue = UIIndex - UI_DEFAULT_ITEM_OFFSET
  /// </summary>
  UI_DEFAULT_ITEM_OFFSET = 1;

{ TDeviceSettingsPresenter }

constructor TDeviceSettingsPresenter.Create(
  AView: IDeviceSettingsView;
  ADeviceConfigProvider: IDeviceConfigProvider
);
begin
  inherited Create;
  FView := AView;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FDeviceAddresses := TList<UInt64>.Create;
  FSelectedDeviceIndex := -1;
  LogDebug('Created', ClassName);
end;

destructor TDeviceSettingsPresenter.Destroy;
begin
  FDeviceAddresses.Free;
  LogDebug('Destroyed', ClassName);
  inherited;
end;

procedure TDeviceSettingsPresenter.LoadDeviceList;
var
  Addresses: TArray<UInt64>;
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  DisplayName: string;
  Items: TArray<string>;
  I: Integer;
begin
  FDeviceAddresses.Clear;
  SetLength(Items, 0);

  Addresses := FDeviceConfigProvider.GetConfiguredDeviceAddresses;
  SetLength(Items, Length(Addresses));

  I := 0;
  for Address in Addresses do
  begin
    DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(Address);
    if DeviceConfig.Name <> '' then
      DisplayName := Format('%s (%s)', [DeviceConfig.Name, FormatAddressAsMAC(Address)])
    else
      DisplayName := FormatAddressAsMAC(Address);

    Items[I] := DisplayName;
    FDeviceAddresses.Add(Address);
    Inc(I);
  end;

  FView.PopulateDeviceList(Items);

  if Length(Items) > 0 then
  begin
    FView.SetSelectedDeviceIndex(0);
    LoadDeviceSettings(0);
  end
  else
  begin
    FSelectedDeviceIndex := -1;
    FView.ClearDeviceSettings;
  end;
end;

procedure TDeviceSettingsPresenter.LoadDeviceSettings(AIndex: Integer);
var
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  // Save previous device settings if any
  if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
    SaveDeviceSettings(FSelectedDeviceIndex);

  FSelectedDeviceIndex := AIndex;

  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(Address);

  Settings.Alias := DeviceConfig.Alias;
  Settings.DeviceTypeIndex := DeviceConfig.DeviceTypeOverride + UI_DEFAULT_ITEM_OFFSET;
  Settings.Pinned := DeviceConfig.Pinned;
  Settings.Hidden := DeviceConfig.Hidden;
  Settings.AutoConnect := DeviceConfig.AutoConnect;
  Settings.Timeout := DeviceConfig.ConnectionTimeout;
  Settings.RetryCount := DeviceConfig.ConnectionRetryCount;
  Settings.NotifyConnectIndex := DeviceConfig.Notifications.OnConnect + UI_DEFAULT_ITEM_OFFSET;
  Settings.NotifyDisconnectIndex := DeviceConfig.Notifications.OnDisconnect + UI_DEFAULT_ITEM_OFFSET;
  Settings.NotifyFailedIndex := DeviceConfig.Notifications.OnConnectFailed + UI_DEFAULT_ITEM_OFFSET;
  Settings.NotifyAutoIndex := DeviceConfig.Notifications.OnAutoConnect + UI_DEFAULT_ITEM_OFFSET;
  // Battery tray per-device settings
  Settings.BatteryTrayIconIndex := DeviceConfig.BatteryTray.ShowTrayIcon + UI_DEFAULT_ITEM_OFFSET;
  Settings.BatteryIconColor := DeviceConfig.BatteryTray.IconColor;
  Settings.BatteryBackgroundColor := DeviceConfig.BatteryTray.BackgroundColor;
  Settings.BatteryShowNumericIndex := DeviceConfig.BatteryTray.ShowNumericValue + UI_DEFAULT_ITEM_OFFSET;
  Settings.BatteryThreshold := DeviceConfig.BatteryTray.LowBatteryThreshold;
  Settings.BatteryNotifyLowIndex := DeviceConfig.BatteryTray.NotifyLowBattery + UI_DEFAULT_ITEM_OFFSET;
  Settings.BatteryNotifyFullIndex := DeviceConfig.BatteryTray.NotifyFullyCharged + UI_DEFAULT_ITEM_OFFSET;
  // Profile display per-device setting
  Settings.ShowProfilesIndex := DeviceConfig.ShowProfiles + UI_DEFAULT_ITEM_OFFSET;

  FView.SetDeviceSettings(Settings);
end;

procedure TDeviceSettingsPresenter.SaveDeviceSettings(AIndex: Integer);
var
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then
    Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := FDeviceConfigProvider.GetDeviceConfig(Address);
  Settings := FView.GetDeviceSettings;

  DeviceConfig.Alias := Settings.Alias;
  DeviceConfig.DeviceTypeOverride := Settings.DeviceTypeIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.Pinned := Settings.Pinned;
  DeviceConfig.Hidden := Settings.Hidden;
  DeviceConfig.AutoConnect := Settings.AutoConnect;
  DeviceConfig.ConnectionTimeout := Settings.Timeout;
  DeviceConfig.ConnectionRetryCount := Settings.RetryCount;
  DeviceConfig.Notifications.OnConnect := Settings.NotifyConnectIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.Notifications.OnDisconnect := Settings.NotifyDisconnectIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.Notifications.OnConnectFailed := Settings.NotifyFailedIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.Notifications.OnAutoConnect := Settings.NotifyAutoIndex - UI_DEFAULT_ITEM_OFFSET;
  // Battery tray per-device settings
  DeviceConfig.BatteryTray.ShowTrayIcon := Settings.BatteryTrayIconIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.BatteryTray.IconColor := Settings.BatteryIconColor;
  DeviceConfig.BatteryTray.BackgroundColor := Settings.BatteryBackgroundColor;
  DeviceConfig.BatteryTray.ShowNumericValue := Settings.BatteryShowNumericIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.BatteryTray.LowBatteryThreshold := Settings.BatteryThreshold;
  DeviceConfig.BatteryTray.NotifyLowBattery := Settings.BatteryNotifyLowIndex - UI_DEFAULT_ITEM_OFFSET;
  DeviceConfig.BatteryTray.NotifyFullyCharged := Settings.BatteryNotifyFullIndex - UI_DEFAULT_ITEM_OFFSET;
  // Profile display per-device setting
  DeviceConfig.ShowProfiles := Settings.ShowProfilesIndex - UI_DEFAULT_ITEM_OFFSET;

  FDeviceConfigProvider.SetDeviceConfig(DeviceConfig);
end;

procedure TDeviceSettingsPresenter.SaveCurrentDevice;
begin
  if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
    SaveDeviceSettings(FSelectedDeviceIndex);
end;

procedure TDeviceSettingsPresenter.OnDeviceSelected(AIndex: Integer);
begin
  LogDebug('OnDeviceSelected: Index=%d', [AIndex], ClassName);
  LoadDeviceSettings(AIndex);
end;

procedure TDeviceSettingsPresenter.OnForgetDeviceClicked(AIndex: Integer);
var
  Address: UInt64;
begin
  LogInfo('OnForgetDeviceClicked: Index=%d', [AIndex], ClassName);
  if (AIndex >= 0) and (AIndex < FDeviceAddresses.Count) then
  begin
    Address := FDeviceAddresses[AIndex];
    FDeviceConfigProvider.RemoveDeviceConfig(Address);
    FSelectedDeviceIndex := -1;
    LoadDeviceList;
  end;
end;

procedure TDeviceSettingsPresenter.OnRefreshDevicesClicked;
begin
  LogInfo('OnRefreshDevicesClicked', ClassName);
  LoadDeviceList;
end;

{ TSettingsPresenter }

constructor TSettingsPresenter.Create(
  ADialogView: ISettingsDialogView;
  AGeneralSettingsView: IGeneralSettingsView;
  AHotkeySettingsView: IHotkeySettingsView;
  AAppearanceSettingsView: IAppearanceSettingsView;
  ALayoutSettingsView: ILayoutSettingsView;
  AConnectionSettingsView: IConnectionSettingsView;
  ALoggingSettingsView: ILoggingSettingsView;
  ABatteryTraySettingsView: IBatteryTraySettingsView;
  AProfileSettingsView: IProfileSettingsView;
  ADeviceSettingsView: IDeviceSettingsView;
  AAppConfig: IAppConfig;
  ADeviceConfigProvider: IDeviceConfigProvider;
  ABatteryTrayConfig: IBatteryTrayConfig;
  AProfileConfig: IProfileConfig
);
begin
  inherited Create;

  // Store view interfaces
  FDialogView := ADialogView;
  FGeneralSettingsView := AGeneralSettingsView;
  FHotkeySettingsView := AHotkeySettingsView;
  FAppearanceSettingsView := AAppearanceSettingsView;
  FLayoutSettingsView := ALayoutSettingsView;
  FConnectionSettingsView := AConnectionSettingsView;
  FLoggingSettingsView := ALoggingSettingsView;
  FBatteryTraySettingsView := ABatteryTraySettingsView;
  FProfileSettingsView := AProfileSettingsView;

  FAppConfig := AAppConfig;
  FModified := False;

  // Extract config interfaces once (eliminates duplicate extraction in LoadSettings/SaveSettings)
  FGeneralConfig := AAppConfig.AsGeneralConfig;
  FWindowConfig := AAppConfig.AsWindowConfig;
  FPositionConfig := AAppConfig.AsPositionConfig;
  FHotkeyConfig := AAppConfig.AsHotkeyConfig;
  FAppearanceConfig := AAppConfig.AsAppearanceConfig;
  FLayoutConfig := AAppConfig.AsLayoutConfig;
  FConnectionConfig := AAppConfig.AsConnectionConfig;
  FPollingConfig := AAppConfig.AsPollingConfig;
  FNotificationConfig := AAppConfig.AsNotificationConfig;
  FLogConfig := AAppConfig.AsLogConfig;
  FBatteryTrayConfig := ABatteryTrayConfig;
  FProfileConfig := AProfileConfig;

  // Create device settings presenter with delegation
  FDevicePresenter := TDeviceSettingsPresenter.Create(ADeviceSettingsView, ADeviceConfigProvider);

  LogDebug('Created', ClassName);
end;

destructor TSettingsPresenter.Destroy;
begin
  FDevicePresenter.Free;
  LogDebug('Destroyed', ClassName);
  inherited;
end;

procedure TSettingsPresenter.LoadSettings;
var
  General: TGeneralViewSettings;
  Hotkey: THotkeyViewSettings;
  Appearance: TAppearanceViewSettings;
  Layout: TLayoutViewSettings;
  Connection: TConnectionViewSettings;
  Logging: TLoggingViewSettings;
begin
  LogDebug('LoadSettings', ClassName);

  // General settings (uses stored config interfaces)
  General.WindowMode := FGeneralConfig.WindowMode;
  General.OnTop := FGeneralConfig.OnTop;
  General.MinimizeToTray := FWindowConfig.MinimizeToTray;
  General.CloseToTray := FWindowConfig.CloseToTray;
  General.StartMinimized := FWindowConfig.StartMinimized;
  General.HideOnFocusLoss := FWindowConfig.MenuHideOnFocusLoss;
  General.Autostart := FGeneralConfig.Autostart;
  General.PositionMode := FPositionConfig.PositionMode;
  FGeneralSettingsView.SetGeneralSettings(General);

  // Hotkey settings
  Hotkey.Hotkey := FHotkeyConfig.Hotkey;
  Hotkey.UseLowLevelHook := FHotkeyConfig.UseLowLevelHook;
  Hotkey.CastPanelHotkey := FHotkeyConfig.CastPanelHotkey;
  Hotkey.BluetoothPanelHotkey := FHotkeyConfig.BluetoothPanelHotkey;
  FHotkeySettingsView.SetHotkeySettings(Hotkey);

  // Theme list (view gets available styles from TThemeManager)
  FDialogView.PopulateThemeList(FAppearanceConfig.Theme);

  // Appearance settings
  Appearance.Theme := FAppearanceConfig.Theme;
  Appearance.VsfDir := FAppearanceConfig.VsfDir;
  Appearance.ShowAddresses := FAppearanceConfig.ShowAddresses;
  Appearance.ShowDeviceIcons := FAppearanceConfig.ShowDeviceIcons;
  Appearance.ShowLastSeen := FAppearanceConfig.ShowLastSeen;
  Appearance.LastSeenRelative := FAppearanceConfig.LastSeenFormat = lsfRelative;
  Appearance.ShowBatteryLevel := FAppearanceConfig.ShowBatteryLevel;
  Appearance.ConnectedColor := TColor(FAppearanceConfig.ConnectedColor);
  FAppearanceSettingsView.SetAppearanceSettings(Appearance);

  // Layout settings
  Layout.ItemHeight := FLayoutConfig.ItemHeight;
  Layout.ItemPadding := FLayoutConfig.ItemPadding;
  Layout.ItemMargin := FLayoutConfig.ItemMargin;
  Layout.IconSize := FLayoutConfig.IconSize;
  Layout.CornerRadius := FLayoutConfig.CornerRadius;
  Layout.BorderWidth := FLayoutConfig.ItemBorderWidth;
  Layout.BorderColor := TColor(FLayoutConfig.ItemBorderColor);
  Layout.DeviceNameFontSize := FLayoutConfig.DeviceNameFontSize;
  Layout.StatusFontSize := FLayoutConfig.StatusFontSize;
  Layout.AddressFontSize := FLayoutConfig.AddressFontSize;
  Layout.IconFontSize := FLayoutConfig.IconFontSize;
  Layout.ShowUnpairedDevices := FLayoutConfig.ShowUnpairedDevices;
  Layout.ShowUnidentifiedDevices := FLayoutConfig.ShowUnidentifiedDevices;
  Layout.ScrollbarWidth := FLayoutConfig.ScrollbarWidth;
  Layout.ScrollbarOpacity := FLayoutConfig.ScrollbarOpacity;
  FLayoutSettingsView.SetLayoutSettings(Layout);

  // Connection settings
  Connection.Timeout := FConnectionConfig.ConnectionTimeout;
  Connection.RetryCount := FConnectionConfig.ConnectionRetryCount;
  Connection.EnumerationMode := FConnectionConfig.EnumerationMode;
  Connection.BluetoothPlatform := FConnectionConfig.BluetoothPlatform;
  Connection.PollingMode := FPollingConfig.PollingMode;
  Connection.PollingInterval := FPollingConfig.PollingInterval;
  Connection.NotifyOnConnect := FNotificationConfig.NotifyOnConnect = nmBalloon;
  Connection.NotifyOnDisconnect := FNotificationConfig.NotifyOnDisconnect = nmBalloon;
  Connection.NotifyOnConnectFailed := FNotificationConfig.NotifyOnConnectFailed = nmBalloon;
  Connection.NotifyOnAutoConnect := FNotificationConfig.NotifyOnAutoConnect = nmBalloon;
  FConnectionSettingsView.SetConnectionSettings(Connection);

  // Logging settings
  Logging.Enabled := FLogConfig.LogEnabled;
  Logging.Filename := FLogConfig.LogFilename;
  Logging.Append := FLogConfig.LogAppend;
  Logging.LevelIndex := Ord(FLogConfig.LogLevel);
  FLoggingSettingsView.SetLoggingSettings(Logging);

  // Battery tray settings
  if Assigned(FBatteryTrayConfig) then
  begin
    var BatteryTray: TBatteryTrayViewSettings;
    BatteryTray.ShowBatteryTrayIcons := FBatteryTrayConfig.ShowBatteryTrayIcons;
    BatteryTray.DefaultIconColor := FBatteryTrayConfig.DefaultIconColor;
    BatteryTray.DefaultBackgroundColor := FBatteryTrayConfig.DefaultBackgroundColor;
    BatteryTray.DefaultShowNumericValue := FBatteryTrayConfig.DefaultShowNumericValue;
    BatteryTray.DefaultLowBatteryThreshold := FBatteryTrayConfig.DefaultLowBatteryThreshold;
    BatteryTray.DefaultNotifyLowBattery := FBatteryTrayConfig.DefaultNotifyLowBattery;
    BatteryTray.DefaultNotifyFullyCharged := FBatteryTrayConfig.DefaultNotifyFullyCharged;
    BatteryTray.DefaultOutlineColorModeIndex := Ord(FBatteryTrayConfig.DefaultOutlineColorMode);
    BatteryTray.DefaultCustomOutlineColor := FBatteryTrayConfig.DefaultCustomOutlineColor;
    FBatteryTraySettingsView.SetBatteryTraySettings(BatteryTray);
  end;

  // Profile settings
  if Assigned(FProfileConfig) then
  begin
    var Profile: TProfileViewSettings;
    Profile.ShowProfiles := FProfileConfig.ShowProfiles;
    Profile.ProfileFontSize := FProfileConfig.ProfileFontSize;
    FProfileSettingsView.SetProfileSettings(Profile);
  end;

  // Device list (delegated to device presenter)
  FDevicePresenter.LoadDeviceList;

  FModified := False;
  LogDebug('LoadSettings: Complete', ClassName);
end;

function TSettingsPresenter.SaveSettings: Boolean;
var
  General: TGeneralViewSettings;
  Hotkey: THotkeyViewSettings;
  Appearance: TAppearanceViewSettings;
  Layout: TLayoutViewSettings;
  Connection: TConnectionViewSettings;
  Logging: TLoggingViewSettings;
  BatteryTray: TBatteryTrayViewSettings;
begin
  LogDebug('SaveSettings', ClassName);
  Result := True;
  try
    // Validate hotkeys first (before saving anything)
    Hotkey := FHotkeySettingsView.GetHotkeySettings;
    if not ValidateHotkeys(Hotkey) then
    begin
      Result := False;
      Exit;
    end;

    // Save current device settings (delegated to device presenter)
    FDevicePresenter.SaveCurrentDevice;

    // General settings (uses stored config interfaces)
    General := FGeneralSettingsView.GetGeneralSettings;
    FGeneralConfig.WindowMode := General.WindowMode;
    FGeneralConfig.OnTop := General.OnTop;
    FWindowConfig.MinimizeToTray := General.MinimizeToTray;
    FWindowConfig.CloseToTray := General.CloseToTray;
    FWindowConfig.StartMinimized := General.StartMinimized;
    FWindowConfig.MenuHideOnFocusLoss := General.HideOnFocusLoss;
    FGeneralConfig.Autostart := General.Autostart;
    FPositionConfig.PositionMode := General.PositionMode;

    // Hotkey settings (Hotkey already read above for validation)
    FHotkeyConfig.Hotkey := Hotkey.Hotkey;
    FHotkeyConfig.UseLowLevelHook := Hotkey.UseLowLevelHook;
    FHotkeyConfig.CastPanelHotkey := Hotkey.CastPanelHotkey;
    FHotkeyConfig.BluetoothPanelHotkey := Hotkey.BluetoothPanelHotkey;

    // Appearance settings
    Appearance := FAppearanceSettingsView.GetAppearanceSettings;
    FAppearanceConfig.Theme := Appearance.Theme;
    FAppearanceConfig.VsfDir := Appearance.VsfDir;
    FAppearanceConfig.ShowAddresses := Appearance.ShowAddresses;
    FAppearanceConfig.ShowDeviceIcons := Appearance.ShowDeviceIcons;
    FAppearanceConfig.ShowLastSeen := Appearance.ShowLastSeen;
    if Appearance.LastSeenRelative then
      FAppearanceConfig.LastSeenFormat := lsfRelative
    else
      FAppearanceConfig.LastSeenFormat := lsfAbsolute;
    FAppearanceConfig.ShowBatteryLevel := Appearance.ShowBatteryLevel;
    FAppearanceConfig.ConnectedColor := Integer(Appearance.ConnectedColor);

    // Layout settings
    Layout := FLayoutSettingsView.GetLayoutSettings;
    FLayoutConfig.ItemHeight := Layout.ItemHeight;
    FLayoutConfig.ItemPadding := Layout.ItemPadding;
    FLayoutConfig.ItemMargin := Layout.ItemMargin;
    FLayoutConfig.IconSize := Layout.IconSize;
    FLayoutConfig.CornerRadius := Layout.CornerRadius;
    FLayoutConfig.ItemBorderWidth := Layout.BorderWidth;
    FLayoutConfig.ItemBorderColor := Integer(Layout.BorderColor);
    FLayoutConfig.DeviceNameFontSize := Layout.DeviceNameFontSize;
    FLayoutConfig.StatusFontSize := Layout.StatusFontSize;
    FLayoutConfig.AddressFontSize := Layout.AddressFontSize;
    FLayoutConfig.IconFontSize := Layout.IconFontSize;
    FLayoutConfig.ShowUnpairedDevices := Layout.ShowUnpairedDevices;
    FLayoutConfig.ShowUnidentifiedDevices := Layout.ShowUnidentifiedDevices;
    FLayoutConfig.ScrollbarWidth := Layout.ScrollbarWidth;
    FLayoutConfig.ScrollbarOpacity := Layout.ScrollbarOpacity;

    // Connection settings
    Connection := FConnectionSettingsView.GetConnectionSettings;
    FConnectionConfig.ConnectionTimeout := Connection.Timeout;
    FConnectionConfig.ConnectionRetryCount := Connection.RetryCount;
    FConnectionConfig.EnumerationMode := Connection.EnumerationMode;
    FConnectionConfig.BluetoothPlatform := Connection.BluetoothPlatform;
    FPollingConfig.PollingMode := Connection.PollingMode;
    FPollingConfig.PollingInterval := Connection.PollingInterval;
    if Connection.NotifyOnConnect then
      FNotificationConfig.NotifyOnConnect := nmBalloon
    else
      FNotificationConfig.NotifyOnConnect := nmNone;
    if Connection.NotifyOnDisconnect then
      FNotificationConfig.NotifyOnDisconnect := nmBalloon
    else
      FNotificationConfig.NotifyOnDisconnect := nmNone;
    if Connection.NotifyOnConnectFailed then
      FNotificationConfig.NotifyOnConnectFailed := nmBalloon
    else
      FNotificationConfig.NotifyOnConnectFailed := nmNone;
    if Connection.NotifyOnAutoConnect then
      FNotificationConfig.NotifyOnAutoConnect := nmBalloon
    else
      FNotificationConfig.NotifyOnAutoConnect := nmNone;

    // Logging settings
    Logging := FLoggingSettingsView.GetLoggingSettings;
    FLogConfig.LogEnabled := Logging.Enabled;
    FLogConfig.LogFilename := Logging.Filename;
    FLogConfig.LogAppend := Logging.Append;
    FLogConfig.LogLevel := TLogLevel(Logging.LevelIndex);

    // Battery tray settings
    if Assigned(FBatteryTrayConfig) then
    begin
      BatteryTray := FBatteryTraySettingsView.GetBatteryTraySettings;
      FBatteryTrayConfig.ShowBatteryTrayIcons := BatteryTray.ShowBatteryTrayIcons;
      FBatteryTrayConfig.DefaultIconColor := BatteryTray.DefaultIconColor;
      FBatteryTrayConfig.DefaultBackgroundColor := BatteryTray.DefaultBackgroundColor;
      FBatteryTrayConfig.DefaultShowNumericValue := BatteryTray.DefaultShowNumericValue;
      FBatteryTrayConfig.DefaultLowBatteryThreshold := BatteryTray.DefaultLowBatteryThreshold;
      FBatteryTrayConfig.DefaultNotifyLowBattery := BatteryTray.DefaultNotifyLowBattery;
      FBatteryTrayConfig.DefaultNotifyFullyCharged := BatteryTray.DefaultNotifyFullyCharged;
      FBatteryTrayConfig.DefaultOutlineColorMode := TOutlineColorMode(BatteryTray.DefaultOutlineColorModeIndex);
      FBatteryTrayConfig.DefaultCustomOutlineColor := BatteryTray.DefaultCustomOutlineColor;
    end;

    // Profile settings
    if Assigned(FProfileConfig) then
    begin
      var Profile: TProfileViewSettings;
      Profile := FProfileSettingsView.GetProfileSettings;
      FProfileConfig.ShowProfiles := Profile.ShowProfiles;
      FProfileConfig.ProfileFontSize := Profile.ProfileFontSize;
    end;

    // Save configuration to file
    FAppConfig.Save;
    FModified := False;
    LogDebug('SaveSettings: Success', ClassName);

    // Notify that settings were applied
    if Assigned(FOnSettingsApplied) then
      FOnSettingsApplied(Self);
  except
    on E: Exception do
    begin
      LogError('SaveSettings: Error - %s', [E.Message], ClassName);
      FDialogView.ShowError('Failed to save settings: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TSettingsPresenter.OnOKClicked;
begin
  LogDebug('OnOKClicked', ClassName);
  if SaveSettings then
    FDialogView.CloseWithOK;
end;

procedure TSettingsPresenter.OnCancelClicked;
begin
  LogDebug('OnCancelClicked', ClassName);
  FDialogView.CloseWithCancel;
end;

procedure TSettingsPresenter.OnApplyClicked;
begin
  LogDebug('OnApplyClicked', ClassName);
  if SaveSettings then
    FDialogView.SetApplyEnabled(False);
end;

procedure TSettingsPresenter.OnResetSizeClicked;
begin
  LogDebug('OnResetSizeClicked', ClassName);
  FPositionConfig.PositionW := -1;
  FPositionConfig.PositionH := -1;
  FDialogView.ShowInfo('Window size will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnResetPositionClicked;
begin
  LogDebug('OnResetPositionClicked', ClassName);
  FPositionConfig.PositionX := -1;
  FPositionConfig.PositionY := -1;
  FDialogView.ShowInfo('Window position will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnDeviceSelected(AIndex: Integer);
begin
  FDevicePresenter.OnDeviceSelected(AIndex);
end;

procedure TSettingsPresenter.OnForgetDeviceClicked(AIndex: Integer);
begin
  FDevicePresenter.OnForgetDeviceClicked(AIndex);
end;

procedure TSettingsPresenter.OnRefreshDevicesClicked;
begin
  FDevicePresenter.OnRefreshDevicesClicked;
end;

procedure TSettingsPresenter.OnResetDefaultsClicked;
begin
  LogInfo('OnResetDefaultsClicked', ClassName);
  try
    // Delete config file
    if FileExists(FAppConfig.ConfigPath) then
      System.SysUtils.DeleteFile(FAppConfig.ConfigPath);

    // Reload defaults
    FAppConfig.Load;
    LoadSettings;

    FDialogView.ShowInfo('Settings have been reset to defaults.');
  except
    on E: Exception do
      FDialogView.ShowError('Failed to reset settings: ' + E.Message);
  end;
end;

procedure TSettingsPresenter.OnResetLayoutClicked;
var
  Layout: TLayoutViewSettings;
  Appearance: TAppearanceViewSettings;
begin
  LogDebug('OnResetLayoutClicked', ClassName);

  // Reset layout settings to defaults
  Layout.ItemHeight := DEF_ITEM_HEIGHT;
  Layout.ItemPadding := DEF_ITEM_PADDING;
  Layout.ItemMargin := DEF_ITEM_MARGIN;
  Layout.IconSize := DEF_ICON_SIZE;
  Layout.CornerRadius := DEF_CORNER_RADIUS;
  Layout.BorderWidth := DEF_ITEM_BORDER_WIDTH;
  Layout.BorderColor := TColor(DEF_ITEM_BORDER_COLOR);
  Layout.DeviceNameFontSize := DEF_DEVICE_NAME_FONT_SIZE;
  Layout.StatusFontSize := DEF_STATUS_FONT_SIZE;
  Layout.AddressFontSize := DEF_ADDRESS_FONT_SIZE;
  Layout.IconFontSize := DEF_ICON_FONT_SIZE;
  Layout.ShowUnpairedDevices := False;  // Default: don't show unpaired devices
  Layout.ShowUnidentifiedDevices := True;  // Default: show all devices (non-breaking)
  Layout.ScrollbarWidth := DEF_SCROLLBAR_WIDTH;
  Layout.ScrollbarOpacity := DEF_SCROLLBAR_OPACITY;
  FLayoutSettingsView.SetLayoutSettings(Layout);

  // Reset connected color (in appearance settings)
  Appearance := FAppearanceSettingsView.GetAppearanceSettings;
  Appearance.ConnectedColor := TColor(DEF_CONNECTED_COLOR);
  FAppearanceSettingsView.SetAppearanceSettings(Appearance);

  MarkModified;
end;

procedure TSettingsPresenter.MarkModified;
begin
  FModified := True;
  FDialogView.SetApplyEnabled(True);
end;

function TSettingsPresenter.ValidateHotkeys(const AHotkey: THotkeyViewSettings): Boolean;
var
  Hotkeys: TArray<string>;
  Names: TArray<string>;
  I, J: Integer;
begin
  Result := True;

  // Collect all non-empty hotkeys with their names
  SetLength(Hotkeys, 0);
  SetLength(Names, 0);

  if AHotkey.Hotkey <> '' then
  begin
    SetLength(Hotkeys, Length(Hotkeys) + 1);
    SetLength(Names, Length(Names) + 1);
    Hotkeys[High(Hotkeys)] := UpperCase(AHotkey.Hotkey);
    Names[High(Names)] := 'Main hotkey';
  end;

  if AHotkey.CastPanelHotkey <> '' then
  begin
    SetLength(Hotkeys, Length(Hotkeys) + 1);
    SetLength(Names, Length(Names) + 1);
    Hotkeys[High(Hotkeys)] := UpperCase(AHotkey.CastPanelHotkey);
    Names[High(Names)] := 'Cast panel hotkey';
  end;

  if AHotkey.BluetoothPanelHotkey <> '' then
  begin
    SetLength(Hotkeys, Length(Hotkeys) + 1);
    SetLength(Names, Length(Names) + 1);
    Hotkeys[High(Hotkeys)] := UpperCase(AHotkey.BluetoothPanelHotkey);
    Names[High(Names)] := 'Bluetooth panel hotkey';
  end;

  // Check for duplicates
  for I := 0 to High(Hotkeys) do
  begin
    for J := I + 1 to High(Hotkeys) do
    begin
      if Hotkeys[I] = Hotkeys[J] then
      begin
        FDialogView.ShowError(Format('Hotkey collision: %s and %s use the same hotkey "%s"',
          [Names[I], Names[J], Hotkeys[I]]));
        Result := False;
        Exit;
      end;
    end;
  end;
end;

end.
