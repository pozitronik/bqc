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
  // ISettingsView - Interface for the settings view
  //----------------------------------------------------------------------------

  /// <summary>
  /// Interface for the settings view (implemented by SettingsForm).
  /// Provides data access through grouped settings records.
  /// </summary>
  ISettingsView = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
    // Dialog control
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);

    // General settings
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);

    // Hotkey settings
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);

    // Appearance settings
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);

    // Layout settings
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);

    // Connection settings
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);

    // Logging settings
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);

    // Theme list management
    procedure PopulateThemeList(const ACurrentTheme: string);

    // Device list management
    procedure PopulateDeviceList(const AItems: TArray<string>);
    function GetSelectedDeviceIndex: Integer;
    procedure SetSelectedDeviceIndex(AIndex: Integer);
    function GetDeviceSettings: TDeviceViewSettings;
    procedure SetDeviceSettings(const ASettings: TDeviceViewSettings);
    procedure ClearDeviceSettings;
  end;

  //----------------------------------------------------------------------------
  // TSettingsPresenter - Settings dialog presenter
  //----------------------------------------------------------------------------

  /// <summary>
  /// Presenter for the Settings dialog.
  /// Handles loading and saving configuration through the ISettingsView interface.
  /// </summary>
  TSettingsPresenter = class
  private
    FView: ISettingsView;
    FModified: Boolean;
    FDeviceAddresses: TList<UInt64>;
    FSelectedDeviceIndex: Integer;
    FOnSettingsApplied: TNotifyEvent;

    procedure LoadDeviceList;
    procedure LoadDeviceSettings(AIndex: Integer);
    procedure SaveDeviceSettings(AIndex: Integer);

  public
    constructor Create(AView: ISettingsView);
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
  App.Config,
  App.Bootstrap,
  Bluetooth.Types;

{ TSettingsPresenter }

constructor TSettingsPresenter.Create(AView: ISettingsView);
begin
  inherited Create;
  FView := AView;
  FModified := False;
  FDeviceAddresses := TList<UInt64>.Create;
  FSelectedDeviceIndex := -1;
  Log('Created', ClassName);
end;

destructor TSettingsPresenter.Destroy;
begin
  FDeviceAddresses.Free;
  Log('Destroyed', ClassName);
  inherited;
end;

procedure TSettingsPresenter.LoadDeviceList;
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

  Addresses := Bootstrap.DeviceConfigProvider.GetConfiguredDeviceAddresses;
  SetLength(Items, Length(Addresses));

  I := 0;
  for Address in Addresses do
  begin
    DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);
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

procedure TSettingsPresenter.LoadDeviceSettings(AIndex: Integer);
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
  DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);

  Settings.Alias := DeviceConfig.Alias;
  Settings.DeviceTypeIndex := DeviceConfig.DeviceTypeOverride + 1;
  Settings.Pinned := DeviceConfig.Pinned;
  Settings.Hidden := DeviceConfig.Hidden;
  Settings.AutoConnect := DeviceConfig.AutoConnect;
  Settings.Timeout := DeviceConfig.ConnectionTimeout;
  Settings.RetryCount := DeviceConfig.ConnectionRetryCount;
  Settings.NotifyConnectIndex := DeviceConfig.Notifications.OnConnect + 1;
  Settings.NotifyDisconnectIndex := DeviceConfig.Notifications.OnDisconnect + 1;
  Settings.NotifyFailedIndex := DeviceConfig.Notifications.OnConnectFailed + 1;
  Settings.NotifyAutoIndex := DeviceConfig.Notifications.OnAutoConnect + 1;

  FView.SetDeviceSettings(Settings);
end;

procedure TSettingsPresenter.SaveDeviceSettings(AIndex: Integer);
var
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then
    Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);
  Settings := FView.GetDeviceSettings;

  DeviceConfig.Alias := Settings.Alias;
  DeviceConfig.DeviceTypeOverride := Settings.DeviceTypeIndex - 1;
  DeviceConfig.Pinned := Settings.Pinned;
  DeviceConfig.Hidden := Settings.Hidden;
  DeviceConfig.AutoConnect := Settings.AutoConnect;
  DeviceConfig.ConnectionTimeout := Settings.Timeout;
  DeviceConfig.ConnectionRetryCount := Settings.RetryCount;
  DeviceConfig.Notifications.OnConnect := Settings.NotifyConnectIndex - 1;
  DeviceConfig.Notifications.OnDisconnect := Settings.NotifyDisconnectIndex - 1;
  DeviceConfig.Notifications.OnConnectFailed := Settings.NotifyFailedIndex - 1;
  DeviceConfig.Notifications.OnAutoConnect := Settings.NotifyAutoIndex - 1;

  Bootstrap.DeviceConfigProvider.SetDeviceConfig(DeviceConfig);
end;

procedure TSettingsPresenter.LoadSettings;
var
  Cfg: TAppConfig;
  General: TGeneralViewSettings;
  Hotkey: THotkeyViewSettings;
  Appearance: TAppearanceViewSettings;
  Layout: TLayoutViewSettings;
  Connection: TConnectionViewSettings;
  Logging: TLoggingViewSettings;
begin
  Log('LoadSettings', ClassName);

  Cfg := TAppConfig(Bootstrap.AppConfig);

  // General settings
  General.WindowMode := Cfg.WindowMode;
  General.OnTop := Cfg.OnTop;
  General.MinimizeToTray := Cfg.MinimizeToTray;
  General.CloseToTray := Cfg.CloseToTray;
  General.HideOnFocusLoss := Cfg.MenuHideOnFocusLoss;
  General.Autostart := Cfg.Autostart;
  General.PositionMode := Cfg.PositionMode;
  FView.SetGeneralSettings(General);

  // Hotkey settings
  Hotkey.Hotkey := Cfg.Hotkey;
  Hotkey.UseLowLevelHook := Cfg.UseLowLevelHook;
  FView.SetHotkeySettings(Hotkey);

  // Theme list (view gets available styles from TThemeManager)
  FView.PopulateThemeList(Cfg.Theme);

  // Appearance settings
  Appearance.Theme := Cfg.Theme;
  Appearance.VsfDir := Cfg.VsfDir;
  Appearance.ShowAddresses := Cfg.ShowAddresses;
  Appearance.ShowDeviceIcons := Cfg.ShowDeviceIcons;
  Appearance.ShowLastSeen := Cfg.ShowLastSeen;
  Appearance.LastSeenRelative := Cfg.LastSeenFormat = lsfRelative;
  Appearance.ConnectedColor := TColor(Cfg.ConnectedColor);
  FView.SetAppearanceSettings(Appearance);

  // Layout settings
  Layout.ItemHeight := Cfg.ItemHeight;
  Layout.ItemPadding := Cfg.ItemPadding;
  Layout.ItemMargin := Cfg.ItemMargin;
  Layout.IconSize := Cfg.IconSize;
  Layout.CornerRadius := Cfg.CornerRadius;
  Layout.BorderWidth := Cfg.ItemBorderWidth;
  Layout.BorderColor := TColor(Cfg.ItemBorderColor);
  Layout.DeviceNameFontSize := Cfg.DeviceNameFontSize;
  Layout.StatusFontSize := Cfg.StatusFontSize;
  Layout.AddressFontSize := Cfg.AddressFontSize;
  Layout.IconFontSize := Cfg.IconFontSize;
  FView.SetLayoutSettings(Layout);

  // Connection settings
  Connection.Timeout := Cfg.ConnectionTimeout;
  Connection.RetryCount := Cfg.ConnectionRetryCount;
  Connection.PollingMode := Cfg.PollingMode;
  Connection.PollingInterval := Cfg.PollingInterval;
  Connection.NotifyOnConnect := Cfg.NotifyOnConnect = nmBalloon;
  Connection.NotifyOnDisconnect := Cfg.NotifyOnDisconnect = nmBalloon;
  Connection.NotifyOnConnectFailed := Cfg.NotifyOnConnectFailed = nmBalloon;
  Connection.NotifyOnAutoConnect := Cfg.NotifyOnAutoConnect = nmBalloon;
  FView.SetConnectionSettings(Connection);

  // Logging settings
  Logging.Enabled := Cfg.LogEnabled;
  Logging.Filename := Cfg.LogFilename;
  Logging.Append := Cfg.LogAppend;
  FView.SetLoggingSettings(Logging);

  // Device list
  LoadDeviceList;

  FModified := False;
  Log('LoadSettings: Complete', ClassName);
end;

function TSettingsPresenter.SaveSettings: Boolean;
var
  Cfg: TAppConfig;
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
    // Save current device settings if any selected
    if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
      SaveDeviceSettings(FSelectedDeviceIndex);

    Cfg := TAppConfig(Bootstrap.AppConfig);

    // General settings
    General := FView.GetGeneralSettings;
    Cfg.WindowMode := General.WindowMode;
    Cfg.OnTop := General.OnTop;
    Cfg.MinimizeToTray := General.MinimizeToTray;
    Cfg.CloseToTray := General.CloseToTray;
    Cfg.MenuHideOnFocusLoss := General.HideOnFocusLoss;
    Cfg.Autostart := General.Autostart;
    Cfg.PositionMode := General.PositionMode;

    // Hotkey settings
    Hotkey := FView.GetHotkeySettings;
    Cfg.Hotkey := Hotkey.Hotkey;
    Cfg.UseLowLevelHook := Hotkey.UseLowLevelHook;

    // Appearance settings
    Appearance := FView.GetAppearanceSettings;
    Cfg.Theme := Appearance.Theme;
    Cfg.VsfDir := Appearance.VsfDir;
    Cfg.ShowAddresses := Appearance.ShowAddresses;
    Cfg.ShowDeviceIcons := Appearance.ShowDeviceIcons;
    Cfg.ShowLastSeen := Appearance.ShowLastSeen;
    if Appearance.LastSeenRelative then
      Cfg.LastSeenFormat := lsfRelative
    else
      Cfg.LastSeenFormat := lsfAbsolute;
    Cfg.ConnectedColor := Integer(Appearance.ConnectedColor);

    // Layout settings
    Layout := FView.GetLayoutSettings;
    Cfg.ItemHeight := Layout.ItemHeight;
    Cfg.ItemPadding := Layout.ItemPadding;
    Cfg.ItemMargin := Layout.ItemMargin;
    Cfg.IconSize := Layout.IconSize;
    Cfg.CornerRadius := Layout.CornerRadius;
    Cfg.ItemBorderWidth := Layout.BorderWidth;
    Cfg.ItemBorderColor := Integer(Layout.BorderColor);
    Cfg.DeviceNameFontSize := Layout.DeviceNameFontSize;
    Cfg.StatusFontSize := Layout.StatusFontSize;
    Cfg.AddressFontSize := Layout.AddressFontSize;
    Cfg.IconFontSize := Layout.IconFontSize;

    // Connection settings
    Connection := FView.GetConnectionSettings;
    Cfg.ConnectionTimeout := Connection.Timeout;
    Cfg.ConnectionRetryCount := Connection.RetryCount;
    Cfg.PollingMode := Connection.PollingMode;
    Cfg.PollingInterval := Connection.PollingInterval;
    if Connection.NotifyOnConnect then
      Cfg.NotifyOnConnect := nmBalloon
    else
      Cfg.NotifyOnConnect := nmNone;
    if Connection.NotifyOnDisconnect then
      Cfg.NotifyOnDisconnect := nmBalloon
    else
      Cfg.NotifyOnDisconnect := nmNone;
    if Connection.NotifyOnConnectFailed then
      Cfg.NotifyOnConnectFailed := nmBalloon
    else
      Cfg.NotifyOnConnectFailed := nmNone;
    if Connection.NotifyOnAutoConnect then
      Cfg.NotifyOnAutoConnect := nmBalloon
    else
      Cfg.NotifyOnAutoConnect := nmNone;

    // Logging settings
    Logging := FView.GetLoggingSettings;
    Cfg.LogEnabled := Logging.Enabled;
    Cfg.LogFilename := Logging.Filename;
    Cfg.LogAppend := Logging.Append;

    // Save configuration to file
    Bootstrap.AppConfig.Save;
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
begin
  Log('OnResetSizeClicked', ClassName);
  TAppConfig(Bootstrap.AppConfig).PositionW := -1;
  TAppConfig(Bootstrap.AppConfig).PositionH := -1;
  FView.ShowInfo('Window size will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnResetPositionClicked;
begin
  Log('OnResetPositionClicked', ClassName);
  TAppConfig(Bootstrap.AppConfig).PositionX := -1;
  TAppConfig(Bootstrap.AppConfig).PositionY := -1;
  FView.ShowInfo('Window position will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnDeviceSelected(AIndex: Integer);
begin
  Log('OnDeviceSelected: Index=%d', [AIndex], ClassName);
  LoadDeviceSettings(AIndex);
end;

procedure TSettingsPresenter.OnForgetDeviceClicked(AIndex: Integer);
var
  Address: UInt64;
begin
  Log('OnForgetDeviceClicked: Index=%d', [AIndex], ClassName);
  if (AIndex >= 0) and (AIndex < FDeviceAddresses.Count) then
  begin
    Address := FDeviceAddresses[AIndex];
    Bootstrap.DeviceConfigProvider.RemoveDeviceConfig(Address);
    FSelectedDeviceIndex := -1;
    LoadDeviceList;
  end;
end;

procedure TSettingsPresenter.OnRefreshDevicesClicked;
begin
  Log('OnRefreshDevicesClicked', ClassName);
  LoadDeviceList;
end;

procedure TSettingsPresenter.OnResetDefaultsClicked;
begin
  Log('OnResetDefaultsClicked', ClassName);
  try
    // Delete config file
    if FileExists(Bootstrap.AppConfig.ConfigPath) then
      System.SysUtils.DeleteFile(Bootstrap.AppConfig.ConfigPath);

    // Reload defaults
    Bootstrap.AppConfig.Load;
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
