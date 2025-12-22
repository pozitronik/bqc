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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Interface for the settings view (implemented by SettingsForm).
  /// </summary>
  ISettingsView = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);
  end;

  /// <summary>
  /// Presenter for the Settings dialog.
  /// Handles loading and saving configuration.
  /// </summary>
  TSettingsPresenter = class
  private
    FView: ISettingsView;
    FModified: Boolean;
    FDeviceAddresses: TList<UInt64>;
    FSelectedDeviceIndex: Integer;
    FOnSettingsApplied: TNotifyEvent;

    // Control references (set during LoadSettings)
    // Tab: General
    FComboWindowMode: TComboBox;
    FCheckOnTop: TCheckBox;
    FCheckMinimizeToTray: TCheckBox;
    FCheckCloseToTray: TCheckBox;
    FCheckHideOnFocusLoss: TCheckBox;
    FCheckAutostart: TCheckBox;

    // Tab: Hotkey & Position
    FEditHotkey: TEdit;
    FCheckUseLowLevelHook: TCheckBox;
    FComboPositionMode: TComboBox;

    // Tab: Appearance
    FComboTheme: TComboBox;
    FEditVsfDir: TEdit;
    FCheckShowAddresses: TCheckBox;

    // Tab: Connection
    FEditTimeout: TEdit;
    FEditRetryCount: TEdit;
    FUpDownTimeout: TUpDown;
    FUpDownRetryCount: TUpDown;
    FCheckNotifyOnConnect: TCheckBox;
    FCheckNotifyOnDisconnect: TCheckBox;
    FCheckNotifyOnConnectFailed: TCheckBox;
    FCheckNotifyOnAutoConnect: TCheckBox;
    FComboPollingMode: TComboBox;
    FEditPollingInterval: TEdit;
    FUpDownPollingInterval: TUpDown;

    // Tab: Devices
    FListDevices: TListBox;
    FLabelDeviceAddressValue: TLabel;
    FLabelDeviceLastSeenValue: TLabel;
    FEditDeviceAlias: TEdit;
    FCheckDevicePinned: TCheckBox;
    FCheckDeviceHidden: TCheckBox;
    FCheckDeviceAutoConnect: TCheckBox;
    FComboDeviceType: TComboBox;
    FEditDeviceTimeout: TEdit;
    FEditDeviceRetryCount: TEdit;
    FUpDownDeviceTimeout: TUpDown;
    FUpDownDeviceRetryCount: TUpDown;
    FComboDeviceNotifyConnect: TComboBox;
    FComboDeviceNotifyDisconnect: TComboBox;
    FComboDeviceNotifyFailed: TComboBox;
    FComboDeviceNotifyAuto: TComboBox;

    // Tab: Advanced
    FCheckLogEnabled: TCheckBox;
    FEditLogFilename: TEdit;
    FCheckLogAppend: TCheckBox;

    // Tab: Appearance (new)
    FCheckShowDeviceIcons: TCheckBox;
    FCheckShowLastSeen: TCheckBox;
    FRadioLastSeenRelative: TRadioButton;
    FRadioLastSeenAbsolute: TRadioButton;
    FShapeConnectedColor: TShape;
    FEditItemHeight: TEdit;
    FEditItemPadding: TEdit;
    FEditItemMargin: TEdit;
    FEditIconSize: TEdit;
    FEditCornerRadius: TEdit;
    FEditDeviceNameSize: TEdit;
    FEditStatusSize: TEdit;
    FEditAddressSize: TEdit;
    FEditIconFontSize: TEdit;
    // TUpDown controls for layout (need to set Position directly)
    FUpDownItemHeight: TUpDown;
    FUpDownItemPadding: TUpDown;
    FUpDownItemMargin: TUpDown;
    FUpDownIconSize: TUpDown;
    FUpDownCornerRadius: TUpDown;
    FUpDownDeviceNameSize: TUpDown;
    FUpDownStatusSize: TUpDown;
    FUpDownAddressSize: TUpDown;
    FUpDownIconFontSize: TUpDown;
    FUpDownBorderWidth: TUpDown;
    FShapeBorderColor: TShape;

    procedure InitControlReferences;
    procedure InitUpDownLimits;
    procedure LoadThemeList;
    procedure LoadDeviceList;
    procedure LoadDeviceSettings(AIndex: Integer);
    procedure SaveDeviceSettings(AIndex: Integer);
    function FormatAddress(AAddress: UInt64): string;

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
  Winapi.Windows,
  System.IOUtils,
  System.DateUtils,
  Vcl.Forms,
  App.Logger,
  App.Config,
  App.Bootstrap,
  UI.Theme,
  Bluetooth.Types;

{ TSettingsPresenter }

constructor TSettingsPresenter.Create(AView: ISettingsView);
begin
  inherited Create;
  FView := AView;
  FModified := False;
  FDeviceAddresses := TList<UInt64>.Create;
  FSelectedDeviceIndex := -1;
  Log('[SettingsPresenter] Created');
end;

destructor TSettingsPresenter.Destroy;
begin
  FDeviceAddresses.Free;
  Log('[SettingsPresenter] Destroyed');
  inherited;
end;

procedure TSettingsPresenter.InitControlReferences;
var
  Form: TForm;
  ViewObj: TObject;
  I: Integer;
begin
  // Get the form from the interface (cast to TObject works in Delphi XE2+)
  ViewObj := FView as TObject;
  if not (ViewObj is TForm) then
  begin
    Log('[SettingsPresenter] InitControlReferences: Failed to get form reference');
    Exit;
  end;
  Form := TForm(ViewObj);

  // Tab: General
  FComboWindowMode := Form.FindComponent('ComboWindowMode') as TComboBox;
  FCheckOnTop := Form.FindComponent('CheckOnTop') as TCheckBox;
  FCheckMinimizeToTray := Form.FindComponent('CheckMinimizeToTray') as TCheckBox;
  FCheckCloseToTray := Form.FindComponent('CheckCloseToTray') as TCheckBox;
  FCheckHideOnFocusLoss := Form.FindComponent('CheckHideOnFocusLoss') as TCheckBox;
  FCheckAutostart := Form.FindComponent('CheckAutostart') as TCheckBox;

  // Tab: Hotkey & Position
  FEditHotkey := Form.FindComponent('EditHotkey') as TEdit;
  FCheckUseLowLevelHook := Form.FindComponent('CheckUseLowLevelHook') as TCheckBox;
  FComboPositionMode := Form.FindComponent('ComboPositionMode') as TComboBox;

  // Tab: Appearance
  FComboTheme := Form.FindComponent('ComboTheme') as TComboBox;
  FEditVsfDir := Form.FindComponent('EditVsfDir') as TEdit;
  FCheckShowAddresses := Form.FindComponent('CheckShowAddresses') as TCheckBox;

  // Tab: Connection
  FEditTimeout := Form.FindComponent('EditTimeout') as TEdit;
  FEditRetryCount := Form.FindComponent('EditRetryCount') as TEdit;
  FUpDownTimeout := Form.FindComponent('UpDownTimeout') as TUpDown;
  FUpDownRetryCount := Form.FindComponent('UpDownRetryCount') as TUpDown;
  FCheckNotifyOnConnect := Form.FindComponent('CheckNotifyOnConnect') as TCheckBox;
  FCheckNotifyOnDisconnect := Form.FindComponent('CheckNotifyOnDisconnect') as TCheckBox;
  FCheckNotifyOnConnectFailed := Form.FindComponent('CheckNotifyOnConnectFailed') as TCheckBox;
  FCheckNotifyOnAutoConnect := Form.FindComponent('CheckNotifyOnAutoConnect') as TCheckBox;
  FComboPollingMode := Form.FindComponent('ComboPollingMode') as TComboBox;
  FEditPollingInterval := Form.FindComponent('EditPollingInterval') as TEdit;
  FUpDownPollingInterval := Form.FindComponent('UpDownPollingInterval') as TUpDown;

  // Tab: Devices
  FListDevices := Form.FindComponent('ListDevices') as TListBox;
  FLabelDeviceAddressValue := Form.FindComponent('LabelDeviceAddressValue') as TLabel;
  FLabelDeviceLastSeenValue := Form.FindComponent('LabelDeviceLastSeenValue') as TLabel;
  FEditDeviceAlias := Form.FindComponent('EditDeviceAlias') as TEdit;
  FCheckDevicePinned := Form.FindComponent('CheckDevicePinned') as TCheckBox;
  FCheckDeviceHidden := Form.FindComponent('CheckDeviceHidden') as TCheckBox;
  FCheckDeviceAutoConnect := Form.FindComponent('CheckDeviceAutoConnect') as TCheckBox;
  FComboDeviceType := Form.FindComponent('ComboDeviceType') as TComboBox;
  FEditDeviceTimeout := Form.FindComponent('EditDeviceTimeout') as TEdit;
  FEditDeviceRetryCount := Form.FindComponent('EditDeviceRetryCount') as TEdit;
  FUpDownDeviceTimeout := Form.FindComponent('UpDownDeviceTimeout') as TUpDown;
  FUpDownDeviceRetryCount := Form.FindComponent('UpDownDeviceRetryCount') as TUpDown;
  FComboDeviceNotifyConnect := Form.FindComponent('ComboDeviceNotifyConnect') as TComboBox;
  FComboDeviceNotifyDisconnect := Form.FindComponent('ComboDeviceNotifyDisconnect') as TComboBox;
  FComboDeviceNotifyFailed := Form.FindComponent('ComboDeviceNotifyFailed') as TComboBox;
  FComboDeviceNotifyAuto := Form.FindComponent('ComboDeviceNotifyAuto') as TComboBox;

  // Tab: Advanced
  FCheckLogEnabled := Form.FindComponent('CheckLogEnabled') as TCheckBox;
  FEditLogFilename := Form.FindComponent('EditLogFilename') as TEdit;
  FCheckLogAppend := Form.FindComponent('CheckLogAppend') as TCheckBox;

  // Tab: Appearance (new)
  FCheckShowDeviceIcons := Form.FindComponent('CheckShowDeviceIcons') as TCheckBox;
  FCheckShowLastSeen := Form.FindComponent('CheckShowLastSeen') as TCheckBox;
  FRadioLastSeenRelative := Form.FindComponent('RadioLastSeenRelative') as TRadioButton;
  FRadioLastSeenAbsolute := Form.FindComponent('RadioLastSeenAbsolute') as TRadioButton;
  FShapeConnectedColor := Form.FindComponent('ShapeConnectedColor') as TShape;
  FEditItemHeight := Form.FindComponent('EditItemHeight') as TEdit;
  FEditItemPadding := Form.FindComponent('EditItemPadding') as TEdit;
  FEditItemMargin := Form.FindComponent('EditItemMargin') as TEdit;
  FEditIconSize := Form.FindComponent('EditIconSize') as TEdit;
  FEditCornerRadius := Form.FindComponent('EditCornerRadius') as TEdit;
  FEditDeviceNameSize := Form.FindComponent('EditDeviceNameSize') as TEdit;
  FEditStatusSize := Form.FindComponent('EditStatusSize') as TEdit;
  FEditAddressSize := Form.FindComponent('EditAddressSize') as TEdit;
  FEditIconFontSize := Form.FindComponent('EditIconFontSize') as TEdit;
  // TUpDown controls
  FUpDownItemHeight := Form.FindComponent('UpDownItemHeight') as TUpDown;
  FUpDownItemPadding := Form.FindComponent('UpDownItemPadding') as TUpDown;
  FUpDownItemMargin := Form.FindComponent('UpDownItemMargin') as TUpDown;
  FUpDownIconSize := Form.FindComponent('UpDownIconSize') as TUpDown;
  FUpDownCornerRadius := Form.FindComponent('UpDownCornerRadius') as TUpDown;
  FUpDownDeviceNameSize := Form.FindComponent('UpDownDeviceNameSize') as TUpDown;
  FUpDownStatusSize := Form.FindComponent('UpDownStatusSize') as TUpDown;
  FUpDownAddressSize := Form.FindComponent('UpDownAddressSize') as TUpDown;
  FUpDownIconFontSize := Form.FindComponent('UpDownIconFontSize') as TUpDown;
  FUpDownBorderWidth := Form.FindComponent('UpDownBorderWidth') as TUpDown;
  FShapeBorderColor := Form.FindComponent('ShapeBorderColor') as TShape;

  // Populate device type combo from DeviceTypeNames array
  if FComboDeviceType <> nil then
  begin
    FComboDeviceType.Items.Clear;
    for I := Low(DeviceTypeNames) to High(DeviceTypeNames) do
      FComboDeviceType.Items.Add(DeviceTypeNames[I]);
    FComboDeviceType.Style := csDropDownList;
  end;

  Log('[SettingsPresenter] InitControlReferences: Complete');
end;

procedure TSettingsPresenter.InitUpDownLimits;
begin
  // Layout limits
  if FUpDownItemHeight <> nil then
  begin
    FUpDownItemHeight.Min := MIN_ITEM_HEIGHT;
    FUpDownItemHeight.Max := MAX_ITEM_HEIGHT;
  end;
  if FUpDownItemPadding <> nil then
  begin
    FUpDownItemPadding.Min := MIN_ITEM_PADDING;
    FUpDownItemPadding.Max := MAX_ITEM_PADDING;
  end;
  if FUpDownItemMargin <> nil then
  begin
    FUpDownItemMargin.Min := MIN_ITEM_MARGIN;
    FUpDownItemMargin.Max := MAX_ITEM_MARGIN;
  end;
  if FUpDownIconSize <> nil then
  begin
    FUpDownIconSize.Min := MIN_ICON_SIZE;
    FUpDownIconSize.Max := MAX_ICON_SIZE;
  end;
  if FUpDownCornerRadius <> nil then
  begin
    FUpDownCornerRadius.Min := MIN_CORNER_RADIUS;
    FUpDownCornerRadius.Max := MAX_CORNER_RADIUS;
  end;
  if FUpDownBorderWidth <> nil then
  begin
    FUpDownBorderWidth.Min := MIN_ITEM_BORDER_WIDTH;
    FUpDownBorderWidth.Max := MAX_ITEM_BORDER_WIDTH;
  end;

  // Font size limits
  if FUpDownDeviceNameSize <> nil then
  begin
    FUpDownDeviceNameSize.Min := MIN_DEVICE_NAME_FONT_SIZE;
    FUpDownDeviceNameSize.Max := MAX_DEVICE_NAME_FONT_SIZE;
  end;
  if FUpDownStatusSize <> nil then
  begin
    FUpDownStatusSize.Min := MIN_STATUS_FONT_SIZE;
    FUpDownStatusSize.Max := MAX_STATUS_FONT_SIZE;
  end;
  if FUpDownAddressSize <> nil then
  begin
    FUpDownAddressSize.Min := MIN_ADDRESS_FONT_SIZE;
    FUpDownAddressSize.Max := MAX_ADDRESS_FONT_SIZE;
  end;
  if FUpDownIconFontSize <> nil then
  begin
    FUpDownIconFontSize.Min := MIN_ICON_FONT_SIZE;
    FUpDownIconFontSize.Max := MAX_ICON_FONT_SIZE;
  end;

  // Connection limits
  if FUpDownTimeout <> nil then
  begin
    FUpDownTimeout.Min := MIN_CONNECTION_TIMEOUT;
    FUpDownTimeout.Max := MAX_CONNECTION_TIMEOUT;
  end;
  if FUpDownRetryCount <> nil then
  begin
    FUpDownRetryCount.Min := MIN_CONNECTION_RETRY_COUNT;
    FUpDownRetryCount.Max := MAX_CONNECTION_RETRY_COUNT;
  end;
  if FUpDownPollingInterval <> nil then
  begin
    FUpDownPollingInterval.Min := MIN_POLLING_INTERVAL;
    FUpDownPollingInterval.Max := MAX_POLLING_INTERVAL;
  end;

  // Per-device connection limits (allow -1 for "use global")
  if FUpDownDeviceTimeout <> nil then
  begin
    FUpDownDeviceTimeout.Min := -1;
    FUpDownDeviceTimeout.Max := MAX_CONNECTION_TIMEOUT;
  end;
  if FUpDownDeviceRetryCount <> nil then
  begin
    FUpDownDeviceRetryCount.Min := -1;
    FUpDownDeviceRetryCount.Max := MAX_CONNECTION_RETRY_COUNT;
  end;

  Log('[SettingsPresenter] InitUpDownLimits: Complete');
end;

procedure TSettingsPresenter.LoadThemeList;
var
  Styles: TArray<string>;
  StyleName: string;
begin
  if FComboTheme = nil then Exit;

  FComboTheme.Items.Clear;

  // Add all available styles (embedded + loaded from VsfDir)
  Styles := Theme.GetAvailableStyles;
  for StyleName in Styles do
    FComboTheme.Items.Add(StyleName);

  // Select current theme
  if (TAppConfig(Bootstrap.AppConfig).Theme <> '') and (FComboTheme.Items.IndexOf(TAppConfig(Bootstrap.AppConfig).Theme) >= 0) then
    FComboTheme.ItemIndex := FComboTheme.Items.IndexOf(TAppConfig(Bootstrap.AppConfig).Theme)
  else if FComboTheme.Items.Count > 0 then
    FComboTheme.ItemIndex := 0; // First available
end;

procedure TSettingsPresenter.LoadDeviceList;
var
  Addresses: TArray<UInt64>;
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  DisplayName: string;
begin
  if FListDevices = nil then Exit;

  FListDevices.Items.Clear;
  FDeviceAddresses.Clear;

  Addresses := Bootstrap.DeviceConfigProvider.GetConfiguredDeviceAddresses;
  for Address in Addresses do
  begin
    DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);
    // Show device name with address: "DeviceName (XX:XX:XX:XX:XX:XX)"
    if DeviceConfig.Name <> '' then
      DisplayName := Format('%s (%s)', [DeviceConfig.Name, FormatAddress(Address)])
    else
      // Edge case: device not yet discovered, show address only
      DisplayName := FormatAddress(Address);

    FListDevices.Items.Add(DisplayName);
    FDeviceAddresses.Add(Address);
  end;

  if FListDevices.Items.Count > 0 then
  begin
    FListDevices.ItemIndex := 0;
    LoadDeviceSettings(0);
  end
  else
  begin
    FSelectedDeviceIndex := -1;
    // Clear device settings panel
    if FLabelDeviceAddressValue <> nil then
      FLabelDeviceAddressValue.Caption := '';
    if FEditDeviceAlias <> nil then
      FEditDeviceAlias.Text := '';
  end;
end;

function TSettingsPresenter.FormatAddress(AAddress: UInt64): string;
begin
  Result := FormatAddressAsMAC(AAddress);
end;

procedure TSettingsPresenter.LoadDeviceSettings(AIndex: Integer);
var
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
begin
  // Save previous device settings if any
  if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
    SaveDeviceSettings(FSelectedDeviceIndex);

  FSelectedDeviceIndex := AIndex;

  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);

  if FLabelDeviceAddressValue <> nil then
    FLabelDeviceAddressValue.Caption := FormatAddress(Address);
  // Display LastSeen timestamp
  if FLabelDeviceLastSeenValue <> nil then
  begin
    if DeviceConfig.LastSeen > 0 then
      FLabelDeviceLastSeenValue.Caption := DateTimeToStr(DeviceConfig.LastSeen)
    else
      FLabelDeviceLastSeenValue.Caption := 'Never';
  end;
  if FEditDeviceAlias <> nil then
    FEditDeviceAlias.Text := DeviceConfig.Alias;
  if FCheckDevicePinned <> nil then
    FCheckDevicePinned.Checked := DeviceConfig.Pinned;
  if FCheckDeviceHidden <> nil then
    FCheckDeviceHidden.Checked := DeviceConfig.Hidden;
  if FCheckDeviceAutoConnect <> nil then
    FCheckDeviceAutoConnect.Checked := DeviceConfig.AutoConnect;
  // DeviceType combo: -1=Auto(0), 0=Unknown(1), 1=AudioOutput(2), etc.
  if FComboDeviceType <> nil then
    FComboDeviceType.ItemIndex := DeviceConfig.DeviceTypeOverride + 1;
  // Use TUpDown.Position for spin controls (with Edit.Text fallback)
  if FUpDownDeviceTimeout <> nil then
    FUpDownDeviceTimeout.Position := DeviceConfig.ConnectionTimeout
  else if FEditDeviceTimeout <> nil then
    FEditDeviceTimeout.Text := IntToStr(DeviceConfig.ConnectionTimeout);
  if FUpDownDeviceRetryCount <> nil then
    FUpDownDeviceRetryCount.Position := DeviceConfig.ConnectionRetryCount
  else if FEditDeviceRetryCount <> nil then
    FEditDeviceRetryCount.Text := IntToStr(DeviceConfig.ConnectionRetryCount);

  // Notification combos: -1=Default(0), 0=None(1), 1=Balloon(2)
  if FComboDeviceNotifyConnect <> nil then
    FComboDeviceNotifyConnect.ItemIndex := DeviceConfig.Notifications.OnConnect + 1;
  if FComboDeviceNotifyDisconnect <> nil then
    FComboDeviceNotifyDisconnect.ItemIndex := DeviceConfig.Notifications.OnDisconnect + 1;
  if FComboDeviceNotifyFailed <> nil then
    FComboDeviceNotifyFailed.ItemIndex := DeviceConfig.Notifications.OnConnectFailed + 1;
  if FComboDeviceNotifyAuto <> nil then
    FComboDeviceNotifyAuto.ItemIndex := DeviceConfig.Notifications.OnAutoConnect + 1;
end;

procedure TSettingsPresenter.SaveDeviceSettings(AIndex: Integer);
var
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
begin
  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then
    Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := Bootstrap.DeviceConfigProvider.GetDeviceConfig(Address);

  if FEditDeviceAlias <> nil then
    DeviceConfig.Alias := FEditDeviceAlias.Text;
  if FCheckDevicePinned <> nil then
    DeviceConfig.Pinned := FCheckDevicePinned.Checked;
  if FCheckDeviceHidden <> nil then
    DeviceConfig.Hidden := FCheckDeviceHidden.Checked;
  if FCheckDeviceAutoConnect <> nil then
    DeviceConfig.AutoConnect := FCheckDeviceAutoConnect.Checked;
  // DeviceType combo: Index 0=Auto(-1), 1=Unknown(0), 2=AudioOutput(1), etc.
  if FComboDeviceType <> nil then
    DeviceConfig.DeviceTypeOverride := FComboDeviceType.ItemIndex - 1;
  // Use TUpDown.Position for spin controls (with Edit.Text fallback)
  if FUpDownDeviceTimeout <> nil then
    DeviceConfig.ConnectionTimeout := FUpDownDeviceTimeout.Position
  else if FEditDeviceTimeout <> nil then
    DeviceConfig.ConnectionTimeout := StrToIntDef(FEditDeviceTimeout.Text, -1);

  if FUpDownDeviceRetryCount <> nil then
    DeviceConfig.ConnectionRetryCount := FUpDownDeviceRetryCount.Position
  else if FEditDeviceRetryCount <> nil then
    DeviceConfig.ConnectionRetryCount := StrToIntDef(FEditDeviceRetryCount.Text, -1);

  // Notification combos: Index 0=Default(-1), 1=None(0), 2=Balloon(1)
  if FComboDeviceNotifyConnect <> nil then
    DeviceConfig.Notifications.OnConnect := FComboDeviceNotifyConnect.ItemIndex - 1;
  if FComboDeviceNotifyDisconnect <> nil then
    DeviceConfig.Notifications.OnDisconnect := FComboDeviceNotifyDisconnect.ItemIndex - 1;
  if FComboDeviceNotifyFailed <> nil then
    DeviceConfig.Notifications.OnConnectFailed := FComboDeviceNotifyFailed.ItemIndex - 1;
  if FComboDeviceNotifyAuto <> nil then
    DeviceConfig.Notifications.OnAutoConnect := FComboDeviceNotifyAuto.ItemIndex - 1;

  Bootstrap.DeviceConfigProvider.SetDeviceConfig(DeviceConfig);
end;

procedure TSettingsPresenter.LoadSettings;
begin
  Log('[SettingsPresenter] LoadSettings');

  InitControlReferences;
  InitUpDownLimits;

  // Tab: General
  // ComboBox: 0 = Window mode, 1 = Menu mode
  if FComboWindowMode <> nil then
  begin
    if TAppConfig(Bootstrap.AppConfig).WindowMode = wmWindow then
      FComboWindowMode.ItemIndex := 0
    else
      FComboWindowMode.ItemIndex := 1;
  end;
  if FCheckOnTop <> nil then
    FCheckOnTop.Checked := TAppConfig(Bootstrap.AppConfig).OnTop;
  if FCheckMinimizeToTray <> nil then
    FCheckMinimizeToTray.Checked := TAppConfig(Bootstrap.AppConfig).MinimizeToTray;
  if FCheckCloseToTray <> nil then
    FCheckCloseToTray.Checked := TAppConfig(Bootstrap.AppConfig).CloseToTray;
  if FCheckHideOnFocusLoss <> nil then
    FCheckHideOnFocusLoss.Checked := TAppConfig(Bootstrap.AppConfig).MenuHideOnFocusLoss;
  if FCheckAutostart <> nil then
    FCheckAutostart.Checked := TAppConfig(Bootstrap.AppConfig).Autostart;

  // Tab: Hotkey & Position
  if FEditHotkey <> nil then
    FEditHotkey.Text := TAppConfig(Bootstrap.AppConfig).Hotkey;
  if FCheckUseLowLevelHook <> nil then
    FCheckUseLowLevelHook.Checked := TAppConfig(Bootstrap.AppConfig).UseLowLevelHook;
  if FComboPositionMode <> nil then
    FComboPositionMode.ItemIndex := Ord(TAppConfig(Bootstrap.AppConfig).PositionMode);

  // Tab: Appearance
  if FEditVsfDir <> nil then
    FEditVsfDir.Text := TAppConfig(Bootstrap.AppConfig).VsfDir;
  LoadThemeList;
  if FCheckShowAddresses <> nil then
    FCheckShowAddresses.Checked := TAppConfig(Bootstrap.AppConfig).ShowAddresses;

  // Tab: Connection - use TUpDown.Position for spin controls
  if FUpDownTimeout <> nil then
    FUpDownTimeout.Position := TAppConfig(Bootstrap.AppConfig).ConnectionTimeout;
  if FUpDownRetryCount <> nil then
    FUpDownRetryCount.Position := TAppConfig(Bootstrap.AppConfig).ConnectionRetryCount;
  if FCheckNotifyOnConnect <> nil then
    FCheckNotifyOnConnect.Checked := TAppConfig(Bootstrap.AppConfig).NotifyOnConnect = nmBalloon;
  if FCheckNotifyOnDisconnect <> nil then
    FCheckNotifyOnDisconnect.Checked := TAppConfig(Bootstrap.AppConfig).NotifyOnDisconnect = nmBalloon;
  if FCheckNotifyOnConnectFailed <> nil then
    FCheckNotifyOnConnectFailed.Checked := TAppConfig(Bootstrap.AppConfig).NotifyOnConnectFailed = nmBalloon;
  if FCheckNotifyOnAutoConnect <> nil then
    FCheckNotifyOnAutoConnect.Checked := TAppConfig(Bootstrap.AppConfig).NotifyOnAutoConnect = nmBalloon;
  if FComboPollingMode <> nil then
    FComboPollingMode.ItemIndex := Ord(TAppConfig(Bootstrap.AppConfig).PollingMode);
  if FUpDownPollingInterval <> nil then
    FUpDownPollingInterval.Position := TAppConfig(Bootstrap.AppConfig).PollingInterval;

  // Tab: Devices
  LoadDeviceList;

  // Tab: Advanced
  if FCheckLogEnabled <> nil then
    FCheckLogEnabled.Checked := TAppConfig(Bootstrap.AppConfig).LogEnabled;
  if FEditLogFilename <> nil then
    FEditLogFilename.Text := TAppConfig(Bootstrap.AppConfig).LogFilename;
  if FCheckLogAppend <> nil then
    FCheckLogAppend.Checked := TAppConfig(Bootstrap.AppConfig).LogAppend;

  // Tab: Appearance (new)
  if FCheckShowDeviceIcons <> nil then
    FCheckShowDeviceIcons.Checked := TAppConfig(Bootstrap.AppConfig).ShowDeviceIcons;
  if FCheckShowLastSeen <> nil then
    FCheckShowLastSeen.Checked := TAppConfig(Bootstrap.AppConfig).ShowLastSeen;
  if FRadioLastSeenRelative <> nil then
    FRadioLastSeenRelative.Checked := TAppConfig(Bootstrap.AppConfig).LastSeenFormat = lsfRelative;
  if FRadioLastSeenAbsolute <> nil then
    FRadioLastSeenAbsolute.Checked := TAppConfig(Bootstrap.AppConfig).LastSeenFormat = lsfAbsolute;
  if FShapeConnectedColor <> nil then
    FShapeConnectedColor.Brush.Color := TColor(TAppConfig(Bootstrap.AppConfig).ConnectedColor);
  // Set TUpDown.Position directly - this automatically updates the associated Edit.Text
  if FUpDownItemHeight <> nil then
    FUpDownItemHeight.Position := TAppConfig(Bootstrap.AppConfig).ItemHeight;
  if FUpDownItemPadding <> nil then
    FUpDownItemPadding.Position := TAppConfig(Bootstrap.AppConfig).ItemPadding;
  if FUpDownItemMargin <> nil then
    FUpDownItemMargin.Position := TAppConfig(Bootstrap.AppConfig).ItemMargin;
  if FUpDownIconSize <> nil then
    FUpDownIconSize.Position := TAppConfig(Bootstrap.AppConfig).IconSize;
  if FUpDownCornerRadius <> nil then
    FUpDownCornerRadius.Position := TAppConfig(Bootstrap.AppConfig).CornerRadius;
  if FUpDownDeviceNameSize <> nil then
    FUpDownDeviceNameSize.Position := TAppConfig(Bootstrap.AppConfig).DeviceNameFontSize;
  if FUpDownStatusSize <> nil then
    FUpDownStatusSize.Position := TAppConfig(Bootstrap.AppConfig).StatusFontSize;
  if FUpDownAddressSize <> nil then
    FUpDownAddressSize.Position := TAppConfig(Bootstrap.AppConfig).AddressFontSize;
  if FUpDownIconFontSize <> nil then
    FUpDownIconFontSize.Position := TAppConfig(Bootstrap.AppConfig).IconFontSize;
  if FUpDownBorderWidth <> nil then
    FUpDownBorderWidth.Position := TAppConfig(Bootstrap.AppConfig).ItemBorderWidth;
  if FShapeBorderColor <> nil then
    FShapeBorderColor.Brush.Color := TColor(TAppConfig(Bootstrap.AppConfig).ItemBorderColor);

  FModified := False;
  Log('[SettingsPresenter] LoadSettings: Complete');
end;

function TSettingsPresenter.SaveSettings: Boolean;
begin
  Log('[SettingsPresenter] SaveSettings');
  Result := True;
  try
    // Save current device settings if any selected
    if (FSelectedDeviceIndex >= 0) and (FSelectedDeviceIndex < FDeviceAddresses.Count) then
      SaveDeviceSettings(FSelectedDeviceIndex);

    // Tab: General
    // ComboBox: 0 = Window mode, 1 = Menu mode
    if FComboWindowMode <> nil then
    begin
      if FComboWindowMode.ItemIndex = 0 then
        TAppConfig(Bootstrap.AppConfig).WindowMode := wmWindow
      else
        TAppConfig(Bootstrap.AppConfig).WindowMode := wmMenu;
    end;
    if FCheckOnTop <> nil then
      TAppConfig(Bootstrap.AppConfig).OnTop := FCheckOnTop.Checked;
    if FCheckMinimizeToTray <> nil then
      TAppConfig(Bootstrap.AppConfig).MinimizeToTray := FCheckMinimizeToTray.Checked;
    if FCheckCloseToTray <> nil then
      TAppConfig(Bootstrap.AppConfig).CloseToTray := FCheckCloseToTray.Checked;
    if FCheckHideOnFocusLoss <> nil then
      TAppConfig(Bootstrap.AppConfig).MenuHideOnFocusLoss := FCheckHideOnFocusLoss.Checked;
    if FCheckAutostart <> nil then
      TAppConfig(Bootstrap.AppConfig).Autostart := FCheckAutostart.Checked;

    // Tab: Hotkey & Position
    if FEditHotkey <> nil then
      TAppConfig(Bootstrap.AppConfig).Hotkey := FEditHotkey.Text;
    if FCheckUseLowLevelHook <> nil then
      TAppConfig(Bootstrap.AppConfig).UseLowLevelHook := FCheckUseLowLevelHook.Checked;
    if FComboPositionMode <> nil then
      TAppConfig(Bootstrap.AppConfig).PositionMode := TPositionMode(FComboPositionMode.ItemIndex);

    // Tab: Appearance
    if FComboTheme <> nil then
      TAppConfig(Bootstrap.AppConfig).Theme := FComboTheme.Text;
    if FEditVsfDir <> nil then
      TAppConfig(Bootstrap.AppConfig).VsfDir := FEditVsfDir.Text;
    if FCheckShowAddresses <> nil then
      TAppConfig(Bootstrap.AppConfig).ShowAddresses := FCheckShowAddresses.Checked;

    // Tab: Connection
    if FEditTimeout <> nil then
      TAppConfig(Bootstrap.AppConfig).ConnectionTimeout := StrToIntDef(FEditTimeout.Text, 10000);
    if FEditRetryCount <> nil then
      TAppConfig(Bootstrap.AppConfig).ConnectionRetryCount := StrToIntDef(FEditRetryCount.Text, 2);
    if FCheckNotifyOnConnect <> nil then
    begin
      if FCheckNotifyOnConnect.Checked then
        TAppConfig(Bootstrap.AppConfig).NotifyOnConnect := nmBalloon
      else
        TAppConfig(Bootstrap.AppConfig).NotifyOnConnect := nmNone;
    end;
    if FCheckNotifyOnDisconnect <> nil then
    begin
      if FCheckNotifyOnDisconnect.Checked then
        TAppConfig(Bootstrap.AppConfig).NotifyOnDisconnect := nmBalloon
      else
        TAppConfig(Bootstrap.AppConfig).NotifyOnDisconnect := nmNone;
    end;
    if FCheckNotifyOnConnectFailed <> nil then
    begin
      if FCheckNotifyOnConnectFailed.Checked then
        TAppConfig(Bootstrap.AppConfig).NotifyOnConnectFailed := nmBalloon
      else
        TAppConfig(Bootstrap.AppConfig).NotifyOnConnectFailed := nmNone;
    end;
    if FCheckNotifyOnAutoConnect <> nil then
    begin
      if FCheckNotifyOnAutoConnect.Checked then
        TAppConfig(Bootstrap.AppConfig).NotifyOnAutoConnect := nmBalloon
      else
        TAppConfig(Bootstrap.AppConfig).NotifyOnAutoConnect := nmNone;
    end;
    if FComboPollingMode <> nil then
      TAppConfig(Bootstrap.AppConfig).PollingMode := TPollingMode(FComboPollingMode.ItemIndex);
    if FEditPollingInterval <> nil then
      TAppConfig(Bootstrap.AppConfig).PollingInterval := StrToIntDef(FEditPollingInterval.Text, 2000);

    // Tab: Advanced
    if FCheckLogEnabled <> nil then
      TAppConfig(Bootstrap.AppConfig).LogEnabled := FCheckLogEnabled.Checked;
    if FEditLogFilename <> nil then
      TAppConfig(Bootstrap.AppConfig).LogFilename := FEditLogFilename.Text;
    if FCheckLogAppend <> nil then
      TAppConfig(Bootstrap.AppConfig).LogAppend := FCheckLogAppend.Checked;

    // Tab: Appearance (new)
    if FCheckShowDeviceIcons <> nil then
      TAppConfig(Bootstrap.AppConfig).ShowDeviceIcons := FCheckShowDeviceIcons.Checked;
    if FCheckShowLastSeen <> nil then
      TAppConfig(Bootstrap.AppConfig).ShowLastSeen := FCheckShowLastSeen.Checked;
    if FRadioLastSeenRelative <> nil then
    begin
      if FRadioLastSeenRelative.Checked then
        TAppConfig(Bootstrap.AppConfig).LastSeenFormat := lsfRelative
      else
        TAppConfig(Bootstrap.AppConfig).LastSeenFormat := lsfAbsolute;
    end;
    if FShapeConnectedColor <> nil then
      TAppConfig(Bootstrap.AppConfig).ConnectedColor := Integer(FShapeConnectedColor.Brush.Color);
    if FEditItemHeight <> nil then
      TAppConfig(Bootstrap.AppConfig).ItemHeight := StrToIntDef(FEditItemHeight.Text, 70);
    if FEditItemPadding <> nil then
      TAppConfig(Bootstrap.AppConfig).ItemPadding := StrToIntDef(FEditItemPadding.Text, 12);
    if FEditItemMargin <> nil then
      TAppConfig(Bootstrap.AppConfig).ItemMargin := StrToIntDef(FEditItemMargin.Text, 4);
    if FEditIconSize <> nil then
      TAppConfig(Bootstrap.AppConfig).IconSize := StrToIntDef(FEditIconSize.Text, 32);
    if FEditCornerRadius <> nil then
      TAppConfig(Bootstrap.AppConfig).CornerRadius := StrToIntDef(FEditCornerRadius.Text, 8);
    if FEditDeviceNameSize <> nil then
      TAppConfig(Bootstrap.AppConfig).DeviceNameFontSize := StrToIntDef(FEditDeviceNameSize.Text, 11);
    if FEditStatusSize <> nil then
      TAppConfig(Bootstrap.AppConfig).StatusFontSize := StrToIntDef(FEditStatusSize.Text, 9);
    if FEditAddressSize <> nil then
      TAppConfig(Bootstrap.AppConfig).AddressFontSize := StrToIntDef(FEditAddressSize.Text, 8);
    if FEditIconFontSize <> nil then
      TAppConfig(Bootstrap.AppConfig).IconFontSize := StrToIntDef(FEditIconFontSize.Text, 16);
    if FUpDownBorderWidth <> nil then
      TAppConfig(Bootstrap.AppConfig).ItemBorderWidth := FUpDownBorderWidth.Position;
    if FShapeBorderColor <> nil then
      TAppConfig(Bootstrap.AppConfig).ItemBorderColor := Integer(FShapeBorderColor.Brush.Color);

    // Save configuration to file
    Bootstrap.AppConfig.Save;
    FModified := False;
    Log('[SettingsPresenter] SaveSettings: Success');

    // Notify that settings were applied
    if Assigned(FOnSettingsApplied) then
      FOnSettingsApplied(Self);
  except
    on E: Exception do
    begin
      Log('[SettingsPresenter] SaveSettings: Error - %s', [E.Message]);
      FView.ShowError('Failed to save settings: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TSettingsPresenter.OnOKClicked;
begin
  Log('[SettingsPresenter] OnOKClicked');
  if SaveSettings then
    FView.CloseWithOK;
end;

procedure TSettingsPresenter.OnCancelClicked;
begin
  Log('[SettingsPresenter] OnCancelClicked');
  FView.CloseWithCancel;
end;

procedure TSettingsPresenter.OnApplyClicked;
begin
  Log('[SettingsPresenter] OnApplyClicked');
  if SaveSettings then
    FView.SetApplyEnabled(False);
end;

procedure TSettingsPresenter.OnResetSizeClicked;
begin
  Log('[SettingsPresenter] OnResetSizeClicked');
  TAppConfig(Bootstrap.AppConfig).PositionW := -1;
  TAppConfig(Bootstrap.AppConfig).PositionH := -1;
  FView.ShowInfo('Window size will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnResetPositionClicked;
begin
  Log('[SettingsPresenter] OnResetPositionClicked');
  TAppConfig(Bootstrap.AppConfig).PositionX := -1;
  TAppConfig(Bootstrap.AppConfig).PositionY := -1;
  FView.ShowInfo('Window position will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnDeviceSelected(AIndex: Integer);
begin
  Log('[SettingsPresenter] OnDeviceSelected: Index=%d', [AIndex]);
  LoadDeviceSettings(AIndex);
end;

procedure TSettingsPresenter.OnForgetDeviceClicked(AIndex: Integer);
var
  Address: UInt64;
begin
  Log('[SettingsPresenter] OnForgetDeviceClicked: Index=%d', [AIndex]);
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
  Log('[SettingsPresenter] OnRefreshDevicesClicked');
  LoadDeviceList;
end;

procedure TSettingsPresenter.OnResetDefaultsClicked;
begin
  Log('[SettingsPresenter] OnResetDefaultsClicked');
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
begin
  Log('[SettingsPresenter] OnResetLayoutClicked');
  // Reset layout settings to defaults using TUpDown.Position
  if FUpDownItemHeight <> nil then
    FUpDownItemHeight.Position := DEF_ITEM_HEIGHT;
  if FUpDownItemPadding <> nil then
    FUpDownItemPadding.Position := DEF_ITEM_PADDING;
  if FUpDownItemMargin <> nil then
    FUpDownItemMargin.Position := DEF_ITEM_MARGIN;
  if FUpDownIconSize <> nil then
    FUpDownIconSize.Position := DEF_ICON_SIZE;
  if FUpDownCornerRadius <> nil then
    FUpDownCornerRadius.Position := DEF_CORNER_RADIUS;
  if FUpDownDeviceNameSize <> nil then
    FUpDownDeviceNameSize.Position := DEF_DEVICE_NAME_FONT_SIZE;
  if FUpDownStatusSize <> nil then
    FUpDownStatusSize.Position := DEF_STATUS_FONT_SIZE;
  if FUpDownAddressSize <> nil then
    FUpDownAddressSize.Position := DEF_ADDRESS_FONT_SIZE;
  if FUpDownIconFontSize <> nil then
    FUpDownIconFontSize.Position := DEF_ICON_FONT_SIZE;
  if FUpDownBorderWidth <> nil then
    FUpDownBorderWidth.Position := DEF_ITEM_BORDER_WIDTH;
  if FShapeBorderColor <> nil then
    FShapeBorderColor.Brush.Color := TColor(DEF_ITEM_BORDER_COLOR);
  if FShapeConnectedColor <> nil then
    FShapeConnectedColor.Brush.Color := TColor(DEF_CONNECTED_COLOR);
  MarkModified;
end;

procedure TSettingsPresenter.MarkModified;
begin
  FModified := True;
  FView.SetApplyEnabled(True);
end;

end.
