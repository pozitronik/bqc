{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Settings Presenter (MVP Pattern)                }
{                                                       }
{       Copyright (c) 2024                              }
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
  App.ConfigInterfaces;

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
  end;

  /// <summary>
  /// Connection and polling settings.
  /// </summary>
  TConnectionViewSettings = record
    Timeout: Integer;
    RetryCount: Integer;
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

  //----------------------------------------------------------------------------
  // ISettingsView - Combined interface for backward compatibility
  //----------------------------------------------------------------------------

  /// <summary>
  /// Combined settings view interface (implemented by SettingsForm).
  /// Includes all settings categories for components that need full access.
  /// For focused access, use the individual interfaces above.
  /// </summary>
  ISettingsView = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
    // Dialog control (from ISettingsDialogView)
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);
    procedure PopulateThemeList(const ACurrentTheme: string);

    // General settings (from IGeneralSettingsView)
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);

    // Hotkey settings (from IHotkeySettingsView)
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);

    // Appearance settings (from IAppearanceSettingsView)
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);

    // Layout settings (from ILayoutSettingsView)
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);

    // Connection settings (from IConnectionSettingsView)
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);

    // Logging settings (from ILoggingSettingsView)
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);
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
    FView: ISettingsView;
    FDevicePresenter: TDeviceSettingsPresenter;
    FAppConfig: IAppConfig;
    FModified: Boolean;
    FOnSettingsApplied: TNotifyEvent;
  public
    constructor Create(
      AView: ISettingsView;
      ADeviceSettingsView: IDeviceSettingsView;
      AAppConfig: IAppConfig;
      ADeviceConfigProvider: IDeviceConfigProvider
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
  Log('Created', ClassName);
end;

destructor TDeviceSettingsPresenter.Destroy;
begin
  FDeviceAddresses.Free;
  Log('Destroyed', ClassName);
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

  FDeviceConfigProvider.SetDeviceConfig(DeviceConfig);
end;

procedure TDeviceSettingsPresenter.SaveCurrentDevice;
begin
  if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
    SaveDeviceSettings(FSelectedDeviceIndex);
end;

procedure TDeviceSettingsPresenter.OnDeviceSelected(AIndex: Integer);
begin
  Log('OnDeviceSelected: Index=%d', [AIndex], ClassName);
  LoadDeviceSettings(AIndex);
end;

procedure TDeviceSettingsPresenter.OnForgetDeviceClicked(AIndex: Integer);
var
  Address: UInt64;
begin
  Log('OnForgetDeviceClicked: Index=%d', [AIndex], ClassName);
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
  Log('OnRefreshDevicesClicked', ClassName);
  LoadDeviceList;
end;

{ TSettingsPresenter }

constructor TSettingsPresenter.Create(
  AView: ISettingsView;
  ADeviceSettingsView: IDeviceSettingsView;
  AAppConfig: IAppConfig;
  ADeviceConfigProvider: IDeviceConfigProvider
);
begin
  inherited Create;
  FView := AView;
  FAppConfig := AAppConfig;
  FModified := False;

  // Create device settings presenter with delegation
  FDevicePresenter := TDeviceSettingsPresenter.Create(ADeviceSettingsView, ADeviceConfigProvider);

  Log('Created', ClassName);
end;

destructor TSettingsPresenter.Destroy;
begin
  FDevicePresenter.Free;
  Log('Destroyed', ClassName);
  inherited;
end;

procedure TSettingsPresenter.LoadSettings;
var
  AppCfg: IAppConfig;
  GeneralCfg: IGeneralConfig;
  WindowCfg: IWindowConfig;
  PositionCfg: IPositionConfig;
  HotkeyCfg: IHotkeyConfig;
  AppearanceCfg: IAppearanceConfig;
  LayoutCfg: ILayoutConfig;
  ConnectionCfg: IConnectionConfig;
  PollingCfg: IPollingConfig;
  NotificationCfg: INotificationConfig;
  LogCfg: ILogConfig;
  General: TGeneralViewSettings;
  Hotkey: THotkeyViewSettings;
  Appearance: TAppearanceViewSettings;
  Layout: TLayoutViewSettings;
  Connection: TConnectionViewSettings;
  Logging: TLoggingViewSettings;
begin
  Log('LoadSettings', ClassName);

  // Get interfaces from AppConfig
  AppCfg := FAppConfig;
  GeneralCfg := AppCfg.AsGeneralConfig;
  WindowCfg := AppCfg.AsWindowConfig;
  PositionCfg := AppCfg.AsPositionConfig;
  HotkeyCfg := AppCfg.AsHotkeyConfig;
  AppearanceCfg := AppCfg.AsAppearanceConfig;
  LayoutCfg := AppCfg.AsLayoutConfig;
  ConnectionCfg := AppCfg.AsConnectionConfig;
  PollingCfg := AppCfg.AsPollingConfig;
  NotificationCfg := AppCfg.AsNotificationConfig;
  LogCfg := AppCfg.AsLogConfig;

  // General settings
  General.WindowMode := GeneralCfg.WindowMode;
  General.OnTop := GeneralCfg.OnTop;
  General.MinimizeToTray := WindowCfg.MinimizeToTray;
  General.CloseToTray := WindowCfg.CloseToTray;
  General.HideOnFocusLoss := WindowCfg.MenuHideOnFocusLoss;
  General.Autostart := GeneralCfg.Autostart;
  General.PositionMode := PositionCfg.PositionMode;
  FView.SetGeneralSettings(General);

  // Hotkey settings
  Hotkey.Hotkey := HotkeyCfg.Hotkey;
  Hotkey.UseLowLevelHook := HotkeyCfg.UseLowLevelHook;
  FView.SetHotkeySettings(Hotkey);

  // Theme list (view gets available styles from TThemeManager)
  FView.PopulateThemeList(AppearanceCfg.Theme);

  // Appearance settings
  Appearance.Theme := AppearanceCfg.Theme;
  Appearance.VsfDir := AppearanceCfg.VsfDir;
  Appearance.ShowAddresses := AppearanceCfg.ShowAddresses;
  Appearance.ShowDeviceIcons := AppearanceCfg.ShowDeviceIcons;
  Appearance.ShowLastSeen := AppearanceCfg.ShowLastSeen;
  Appearance.LastSeenRelative := AppearanceCfg.LastSeenFormat = lsfRelative;
  Appearance.ConnectedColor := TColor(AppearanceCfg.ConnectedColor);
  FView.SetAppearanceSettings(Appearance);

  // Layout settings
  Layout.ItemHeight := LayoutCfg.ItemHeight;
  Layout.ItemPadding := LayoutCfg.ItemPadding;
  Layout.ItemMargin := LayoutCfg.ItemMargin;
  Layout.IconSize := LayoutCfg.IconSize;
  Layout.CornerRadius := LayoutCfg.CornerRadius;
  Layout.BorderWidth := LayoutCfg.ItemBorderWidth;
  Layout.BorderColor := TColor(LayoutCfg.ItemBorderColor);
  Layout.DeviceNameFontSize := LayoutCfg.DeviceNameFontSize;
  Layout.StatusFontSize := LayoutCfg.StatusFontSize;
  Layout.AddressFontSize := LayoutCfg.AddressFontSize;
  Layout.IconFontSize := LayoutCfg.IconFontSize;
  FView.SetLayoutSettings(Layout);

  // Connection settings
  Connection.Timeout := ConnectionCfg.ConnectionTimeout;
  Connection.RetryCount := ConnectionCfg.ConnectionRetryCount;
  Connection.PollingMode := PollingCfg.PollingMode;
  Connection.PollingInterval := PollingCfg.PollingInterval;
  Connection.NotifyOnConnect := NotificationCfg.NotifyOnConnect = nmBalloon;
  Connection.NotifyOnDisconnect := NotificationCfg.NotifyOnDisconnect = nmBalloon;
  Connection.NotifyOnConnectFailed := NotificationCfg.NotifyOnConnectFailed = nmBalloon;
  Connection.NotifyOnAutoConnect := NotificationCfg.NotifyOnAutoConnect = nmBalloon;
  FView.SetConnectionSettings(Connection);

  // Logging settings
  Logging.Enabled := LogCfg.LogEnabled;
  Logging.Filename := LogCfg.LogFilename;
  Logging.Append := LogCfg.LogAppend;
  FView.SetLoggingSettings(Logging);

  // Device list (delegated to device presenter)
  FDevicePresenter.LoadDeviceList;

  FModified := False;
  Log('LoadSettings: Complete', ClassName);
end;

function TSettingsPresenter.SaveSettings: Boolean;
var
  AppCfg: IAppConfig;
  GeneralCfg: IGeneralConfig;
  WindowCfg: IWindowConfig;
  PositionCfg: IPositionConfig;
  HotkeyCfg: IHotkeyConfig;
  AppearanceCfg: IAppearanceConfig;
  LayoutCfg: ILayoutConfig;
  ConnectionCfg: IConnectionConfig;
  PollingCfg: IPollingConfig;
  NotificationCfg: INotificationConfig;
  LogCfg: ILogConfig;
  General: TGeneralViewSettings;
  Hotkey: THotkeyViewSettings;
  Appearance: TAppearanceViewSettings;
  Layout: TLayoutViewSettings;
  Connection: TConnectionViewSettings;
  Logging: TLoggingViewSettings;
begin
  Log('SaveSettings', ClassName);
  Result := True;
  try
    // Save current device settings (delegated to device presenter)
    FDevicePresenter.SaveCurrentDevice;

    // Get interfaces from AppConfig
    AppCfg := FAppConfig;
    GeneralCfg := AppCfg.AsGeneralConfig;
    WindowCfg := AppCfg.AsWindowConfig;
    PositionCfg := AppCfg.AsPositionConfig;
    HotkeyCfg := AppCfg.AsHotkeyConfig;
    AppearanceCfg := AppCfg.AsAppearanceConfig;
    LayoutCfg := AppCfg.AsLayoutConfig;
    ConnectionCfg := AppCfg.AsConnectionConfig;
    PollingCfg := AppCfg.AsPollingConfig;
    NotificationCfg := AppCfg.AsNotificationConfig;
    LogCfg := AppCfg.AsLogConfig;

    // General settings
    General := FView.GetGeneralSettings;
    GeneralCfg.WindowMode := General.WindowMode;
    GeneralCfg.OnTop := General.OnTop;
    WindowCfg.MinimizeToTray := General.MinimizeToTray;
    WindowCfg.CloseToTray := General.CloseToTray;
    WindowCfg.MenuHideOnFocusLoss := General.HideOnFocusLoss;
    GeneralCfg.Autostart := General.Autostart;
    PositionCfg.PositionMode := General.PositionMode;

    // Hotkey settings
    Hotkey := FView.GetHotkeySettings;
    HotkeyCfg.Hotkey := Hotkey.Hotkey;
    HotkeyCfg.UseLowLevelHook := Hotkey.UseLowLevelHook;

    // Appearance settings
    Appearance := FView.GetAppearanceSettings;
    AppearanceCfg.Theme := Appearance.Theme;
    AppearanceCfg.VsfDir := Appearance.VsfDir;
    AppearanceCfg.ShowAddresses := Appearance.ShowAddresses;
    AppearanceCfg.ShowDeviceIcons := Appearance.ShowDeviceIcons;
    AppearanceCfg.ShowLastSeen := Appearance.ShowLastSeen;
    if Appearance.LastSeenRelative then
      AppearanceCfg.LastSeenFormat := lsfRelative
    else
      AppearanceCfg.LastSeenFormat := lsfAbsolute;
    AppearanceCfg.ConnectedColor := Integer(Appearance.ConnectedColor);

    // Layout settings
    Layout := FView.GetLayoutSettings;
    LayoutCfg.ItemHeight := Layout.ItemHeight;
    LayoutCfg.ItemPadding := Layout.ItemPadding;
    LayoutCfg.ItemMargin := Layout.ItemMargin;
    LayoutCfg.IconSize := Layout.IconSize;
    LayoutCfg.CornerRadius := Layout.CornerRadius;
    LayoutCfg.ItemBorderWidth := Layout.BorderWidth;
    LayoutCfg.ItemBorderColor := Integer(Layout.BorderColor);
    LayoutCfg.DeviceNameFontSize := Layout.DeviceNameFontSize;
    LayoutCfg.StatusFontSize := Layout.StatusFontSize;
    LayoutCfg.AddressFontSize := Layout.AddressFontSize;
    LayoutCfg.IconFontSize := Layout.IconFontSize;

    // Connection settings
    Connection := FView.GetConnectionSettings;
    ConnectionCfg.ConnectionTimeout := Connection.Timeout;
    ConnectionCfg.ConnectionRetryCount := Connection.RetryCount;
    PollingCfg.PollingMode := Connection.PollingMode;
    PollingCfg.PollingInterval := Connection.PollingInterval;
    if Connection.NotifyOnConnect then
      NotificationCfg.NotifyOnConnect := nmBalloon
    else
      NotificationCfg.NotifyOnConnect := nmNone;
    if Connection.NotifyOnDisconnect then
      NotificationCfg.NotifyOnDisconnect := nmBalloon
    else
      NotificationCfg.NotifyOnDisconnect := nmNone;
    if Connection.NotifyOnConnectFailed then
      NotificationCfg.NotifyOnConnectFailed := nmBalloon
    else
      NotificationCfg.NotifyOnConnectFailed := nmNone;
    if Connection.NotifyOnAutoConnect then
      NotificationCfg.NotifyOnAutoConnect := nmBalloon
    else
      NotificationCfg.NotifyOnAutoConnect := nmNone;

    // Logging settings
    Logging := FView.GetLoggingSettings;
    LogCfg.LogEnabled := Logging.Enabled;
    LogCfg.LogFilename := Logging.Filename;
    LogCfg.LogAppend := Logging.Append;

    // Save configuration to file
    FAppConfig.Save;
    FModified := False;
    Log('SaveSettings: Success', ClassName);

    // Notify that settings were applied
    if Assigned(FOnSettingsApplied) then
      FOnSettingsApplied(Self);
  except
    on E: Exception do
    begin
      Log('SaveSettings: Error - %s', [E.Message], ClassName);
      FView.ShowError('Failed to save settings: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TSettingsPresenter.OnOKClicked;
begin
  Log('OnOKClicked', ClassName);
  if SaveSettings then
    FView.CloseWithOK;
end;

procedure TSettingsPresenter.OnCancelClicked;
begin
  Log('OnCancelClicked', ClassName);
  FView.CloseWithCancel;
end;

procedure TSettingsPresenter.OnApplyClicked;
begin
  Log('OnApplyClicked', ClassName);
  if SaveSettings then
    FView.SetApplyEnabled(False);
end;

procedure TSettingsPresenter.OnResetSizeClicked;
var
  PositionCfg: IPositionConfig;
begin
  Log('OnResetSizeClicked', ClassName);
  PositionCfg := FAppConfig.AsPositionConfig;
  PositionCfg.PositionW := -1;
  PositionCfg.PositionH := -1;
  FView.ShowInfo('Window size will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnResetPositionClicked;
var
  PositionCfg: IPositionConfig;
begin
  Log('OnResetPositionClicked', ClassName);
  PositionCfg := FAppConfig.AsPositionConfig;
  PositionCfg.PositionX := -1;
  PositionCfg.PositionY := -1;
  FView.ShowInfo('Window position will be reset to default on next application start.');
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
  Log('OnResetDefaultsClicked', ClassName);
  try
    // Delete config file
    if FileExists(FAppConfig.ConfigPath) then
      System.SysUtils.DeleteFile(FAppConfig.ConfigPath);

    // Reload defaults
    FAppConfig.Load;
    LoadSettings;

    FView.ShowInfo('Settings have been reset to defaults.');
  except
    on E: Exception do
      FView.ShowError('Failed to reset settings: ' + E.Message);
  end;
end;

procedure TSettingsPresenter.OnResetLayoutClicked;
var
  Layout: TLayoutViewSettings;
  Appearance: TAppearanceViewSettings;
begin
  Log('OnResetLayoutClicked', ClassName);

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
  FView.SetLayoutSettings(Layout);

  // Reset connected color (in appearance settings)
  Appearance := FView.GetAppearanceSettings;
  Appearance.ConnectedColor := TColor(DEF_CONNECTED_COLOR);
  FView.SetAppearanceSettings(Appearance);

  MarkModified;
end;

procedure TSettingsPresenter.MarkModified;
begin
  FModified := True;
  FView.SetApplyEnabled(True);
end;

end.
