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
  System.Generics.Collections,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  App.Config;

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
    FCheckShowAddresses: TCheckBox;

    // Tab: Connection
    FEditTimeout: TEdit;
    FEditRetryCount: TEdit;
    FCheckNotifyOnConnect: TCheckBox;
    FCheckNotifyOnDisconnect: TCheckBox;
    FCheckNotifyOnConnectFailed: TCheckBox;
    FCheckNotifyOnAutoConnect: TCheckBox;
    FComboPollingMode: TComboBox;
    FEditPollingInterval: TEdit;

    // Tab: Devices
    FListDevices: TListBox;
    FLabelDeviceAddressValue: TLabel;
    FEditDeviceAlias: TEdit;
    FCheckDevicePinned: TCheckBox;
    FCheckDeviceHidden: TCheckBox;
    FCheckDeviceAutoConnect: TCheckBox;
    FEditDeviceTimeout: TEdit;
    FEditDeviceRetryCount: TEdit;
    FComboDeviceNotifyConnect: TComboBox;
    FComboDeviceNotifyDisconnect: TComboBox;
    FComboDeviceNotifyFailed: TComboBox;
    FComboDeviceNotifyAuto: TComboBox;

    // Tab: Advanced
    FCheckLogEnabled: TCheckBox;
    FEditLogFilename: TEdit;
    FCheckLogAppend: TCheckBox;

    procedure InitControlReferences;
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

    procedure MarkModified;
    property IsModified: Boolean read FModified;
    property OnSettingsApplied: TNotifyEvent read FOnSettingsApplied write FOnSettingsApplied;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  Vcl.Forms,
  App.Logger,
  UI.Theme;

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
  FCheckShowAddresses := Form.FindComponent('CheckShowAddresses') as TCheckBox;

  // Tab: Connection
  FEditTimeout := Form.FindComponent('EditTimeout') as TEdit;
  FEditRetryCount := Form.FindComponent('EditRetryCount') as TEdit;
  FCheckNotifyOnConnect := Form.FindComponent('CheckNotifyOnConnect') as TCheckBox;
  FCheckNotifyOnDisconnect := Form.FindComponent('CheckNotifyOnDisconnect') as TCheckBox;
  FCheckNotifyOnConnectFailed := Form.FindComponent('CheckNotifyOnConnectFailed') as TCheckBox;
  FCheckNotifyOnAutoConnect := Form.FindComponent('CheckNotifyOnAutoConnect') as TCheckBox;
  FComboPollingMode := Form.FindComponent('ComboPollingMode') as TComboBox;
  FEditPollingInterval := Form.FindComponent('EditPollingInterval') as TEdit;

  // Tab: Devices
  FListDevices := Form.FindComponent('ListDevices') as TListBox;
  FLabelDeviceAddressValue := Form.FindComponent('LabelDeviceAddressValue') as TLabel;
  FEditDeviceAlias := Form.FindComponent('EditDeviceAlias') as TEdit;
  FCheckDevicePinned := Form.FindComponent('CheckDevicePinned') as TCheckBox;
  FCheckDeviceHidden := Form.FindComponent('CheckDeviceHidden') as TCheckBox;
  FCheckDeviceAutoConnect := Form.FindComponent('CheckDeviceAutoConnect') as TCheckBox;
  FEditDeviceTimeout := Form.FindComponent('EditDeviceTimeout') as TEdit;
  FEditDeviceRetryCount := Form.FindComponent('EditDeviceRetryCount') as TEdit;
  FComboDeviceNotifyConnect := Form.FindComponent('ComboDeviceNotifyConnect') as TComboBox;
  FComboDeviceNotifyDisconnect := Form.FindComponent('ComboDeviceNotifyDisconnect') as TComboBox;
  FComboDeviceNotifyFailed := Form.FindComponent('ComboDeviceNotifyFailed') as TComboBox;
  FComboDeviceNotifyAuto := Form.FindComponent('ComboDeviceNotifyAuto') as TComboBox;

  // Tab: Advanced
  FCheckLogEnabled := Form.FindComponent('CheckLogEnabled') as TCheckBox;
  FEditLogFilename := Form.FindComponent('EditLogFilename') as TEdit;
  FCheckLogAppend := Form.FindComponent('CheckLogAppend') as TCheckBox;

  Log('[SettingsPresenter] InitControlReferences: Complete');
end;

procedure TSettingsPresenter.LoadThemeList;
var
  VsfDir, VsfPath: string;
  Files: TArray<string>;
  FileName: string;
begin
  if FComboTheme = nil then Exit;

  FComboTheme.Items.Clear;

  // Add built-in themes
  FComboTheme.Items.Add('System');
  FComboTheme.Items.Add('Light');
  FComboTheme.Items.Add('Dark');

  // Add loaded VCL styles
  VsfDir := Config.VsfDir;
  if ExtractFilePath(VsfDir) = '' then
    VsfPath := ExtractFilePath(ParamStr(0)) + VsfDir
  else
    VsfPath := VsfDir;

  if TDirectory.Exists(VsfPath) then
  begin
    Files := TDirectory.GetFiles(VsfPath, '*.vsf');
    for FileName in Files do
      FComboTheme.Items.Add(TPath.GetFileNameWithoutExtension(FileName));
  end;

  // Select current theme
  if FComboTheme.Items.IndexOf(Config.Theme) >= 0 then
    FComboTheme.ItemIndex := FComboTheme.Items.IndexOf(Config.Theme)
  else
    FComboTheme.ItemIndex := 0; // System
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

  Addresses := Config.GetConfiguredDeviceAddresses;
  for Address in Addresses do
  begin
    DeviceConfig := Config.GetDeviceConfig(Address);
    if DeviceConfig.Alias <> '' then
      DisplayName := DeviceConfig.Alias
    else
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
  Result := Format('%.2X:%.2X:%.2X:%.2X:%.2X:%.2X', [
    (AAddress shr 40) and $FF,
    (AAddress shr 32) and $FF,
    (AAddress shr 24) and $FF,
    (AAddress shr 16) and $FF,
    (AAddress shr 8) and $FF,
    AAddress and $FF
  ]);
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
  DeviceConfig := Config.GetDeviceConfig(Address);

  if FLabelDeviceAddressValue <> nil then
    FLabelDeviceAddressValue.Caption := FormatAddress(Address);
  if FEditDeviceAlias <> nil then
    FEditDeviceAlias.Text := DeviceConfig.Alias;
  if FCheckDevicePinned <> nil then
    FCheckDevicePinned.Checked := DeviceConfig.Pinned;
  if FCheckDeviceHidden <> nil then
    FCheckDeviceHidden.Checked := DeviceConfig.Hidden;
  if FCheckDeviceAutoConnect <> nil then
    FCheckDeviceAutoConnect.Checked := DeviceConfig.AutoConnect;
  if FEditDeviceTimeout <> nil then
    FEditDeviceTimeout.Text := IntToStr(DeviceConfig.ConnectionTimeout);
  if FEditDeviceRetryCount <> nil then
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
  if (AIndex < 0) or (AIndex >= FDeviceAddresses.Count) then Exit;

  Address := FDeviceAddresses[AIndex];
  DeviceConfig := Config.GetDeviceConfig(Address);

  if FEditDeviceAlias <> nil then
    DeviceConfig.Alias := FEditDeviceAlias.Text;
  if FCheckDevicePinned <> nil then
    DeviceConfig.Pinned := FCheckDevicePinned.Checked;
  if FCheckDeviceHidden <> nil then
    DeviceConfig.Hidden := FCheckDeviceHidden.Checked;
  if FCheckDeviceAutoConnect <> nil then
    DeviceConfig.AutoConnect := FCheckDeviceAutoConnect.Checked;
  if FEditDeviceTimeout <> nil then
    DeviceConfig.ConnectionTimeout := StrToIntDef(FEditDeviceTimeout.Text, -1);
  if FEditDeviceRetryCount <> nil then
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

  Config.SetDeviceConfig(DeviceConfig);
end;

procedure TSettingsPresenter.LoadSettings;
begin
  Log('[SettingsPresenter] LoadSettings');

  InitControlReferences;

  // Tab: General
  // ComboBox: 0 = Window mode, 1 = Menu mode
  if FComboWindowMode <> nil then
  begin
    if Config.WindowMode = wmWindow then
      FComboWindowMode.ItemIndex := 0
    else
      FComboWindowMode.ItemIndex := 1;
  end;
  if FCheckOnTop <> nil then
    FCheckOnTop.Checked := Config.OnTop;
  if FCheckMinimizeToTray <> nil then
    FCheckMinimizeToTray.Checked := Config.MinimizeToTray;
  if FCheckCloseToTray <> nil then
    FCheckCloseToTray.Checked := Config.CloseToTray;
  if FCheckHideOnFocusLoss <> nil then
    FCheckHideOnFocusLoss.Checked := Config.MenuHideOnFocusLoss;
  if FCheckAutostart <> nil then
    FCheckAutostart.Checked := Config.Autostart;

  // Tab: Hotkey & Position
  if FEditHotkey <> nil then
    FEditHotkey.Text := Config.Hotkey;
  if FCheckUseLowLevelHook <> nil then
    FCheckUseLowLevelHook.Checked := Config.UseLowLevelHook;
  if FComboPositionMode <> nil then
    FComboPositionMode.ItemIndex := Ord(Config.PositionMode);

  // Tab: Appearance
  LoadThemeList;
  if FCheckShowAddresses <> nil then
    FCheckShowAddresses.Checked := Config.ShowAddresses;

  // Tab: Connection
  if FEditTimeout <> nil then
    FEditTimeout.Text := IntToStr(Config.ConnectionTimeout);
  if FEditRetryCount <> nil then
    FEditRetryCount.Text := IntToStr(Config.ConnectionRetryCount);
  if FCheckNotifyOnConnect <> nil then
    FCheckNotifyOnConnect.Checked := Config.NotifyOnConnect = nmBalloon;
  if FCheckNotifyOnDisconnect <> nil then
    FCheckNotifyOnDisconnect.Checked := Config.NotifyOnDisconnect = nmBalloon;
  if FCheckNotifyOnConnectFailed <> nil then
    FCheckNotifyOnConnectFailed.Checked := Config.NotifyOnConnectFailed = nmBalloon;
  if FCheckNotifyOnAutoConnect <> nil then
    FCheckNotifyOnAutoConnect.Checked := Config.NotifyOnAutoConnect = nmBalloon;
  if FComboPollingMode <> nil then
    FComboPollingMode.ItemIndex := Ord(Config.PollingMode);
  if FEditPollingInterval <> nil then
    FEditPollingInterval.Text := IntToStr(Config.PollingInterval);

  // Tab: Devices
  LoadDeviceList;

  // Tab: Advanced
  if FCheckLogEnabled <> nil then
    FCheckLogEnabled.Checked := Config.LogEnabled;
  if FEditLogFilename <> nil then
    FEditLogFilename.Text := Config.LogFilename;
  if FCheckLogAppend <> nil then
    FCheckLogAppend.Checked := Config.LogAppend;

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
        Config.WindowMode := wmWindow
      else
        Config.WindowMode := wmMenu;
    end;
    if FCheckOnTop <> nil then
      Config.OnTop := FCheckOnTop.Checked;
    if FCheckMinimizeToTray <> nil then
      Config.MinimizeToTray := FCheckMinimizeToTray.Checked;
    if FCheckCloseToTray <> nil then
      Config.CloseToTray := FCheckCloseToTray.Checked;
    if FCheckHideOnFocusLoss <> nil then
      Config.MenuHideOnFocusLoss := FCheckHideOnFocusLoss.Checked;
    if FCheckAutostart <> nil then
      Config.Autostart := FCheckAutostart.Checked;

    // Tab: Hotkey & Position
    if FEditHotkey <> nil then
      Config.Hotkey := FEditHotkey.Text;
    if FCheckUseLowLevelHook <> nil then
      Config.UseLowLevelHook := FCheckUseLowLevelHook.Checked;
    if FComboPositionMode <> nil then
      Config.PositionMode := TPositionMode(FComboPositionMode.ItemIndex);

    // Tab: Appearance
    if FComboTheme <> nil then
      Config.Theme := FComboTheme.Text;
    if FCheckShowAddresses <> nil then
      Config.ShowAddresses := FCheckShowAddresses.Checked;

    // Tab: Connection
    if FEditTimeout <> nil then
      Config.ConnectionTimeout := StrToIntDef(FEditTimeout.Text, 10000);
    if FEditRetryCount <> nil then
      Config.ConnectionRetryCount := StrToIntDef(FEditRetryCount.Text, 2);
    if FCheckNotifyOnConnect <> nil then
    begin
      if FCheckNotifyOnConnect.Checked then
        Config.NotifyOnConnect := nmBalloon
      else
        Config.NotifyOnConnect := nmNone;
    end;
    if FCheckNotifyOnDisconnect <> nil then
    begin
      if FCheckNotifyOnDisconnect.Checked then
        Config.NotifyOnDisconnect := nmBalloon
      else
        Config.NotifyOnDisconnect := nmNone;
    end;
    if FCheckNotifyOnConnectFailed <> nil then
    begin
      if FCheckNotifyOnConnectFailed.Checked then
        Config.NotifyOnConnectFailed := nmBalloon
      else
        Config.NotifyOnConnectFailed := nmNone;
    end;
    if FCheckNotifyOnAutoConnect <> nil then
    begin
      if FCheckNotifyOnAutoConnect.Checked then
        Config.NotifyOnAutoConnect := nmBalloon
      else
        Config.NotifyOnAutoConnect := nmNone;
    end;
    if FComboPollingMode <> nil then
      Config.PollingMode := TPollingMode(FComboPollingMode.ItemIndex);
    if FEditPollingInterval <> nil then
      Config.PollingInterval := StrToIntDef(FEditPollingInterval.Text, 2000);

    // Tab: Advanced
    if FCheckLogEnabled <> nil then
      Config.LogEnabled := FCheckLogEnabled.Checked;
    if FEditLogFilename <> nil then
      Config.LogFilename := FEditLogFilename.Text;
    if FCheckLogAppend <> nil then
      Config.LogAppend := FCheckLogAppend.Checked;

    // Save configuration to file
    Config.Save;
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
    FView.ShowInfo('Settings saved successfully.');
end;

procedure TSettingsPresenter.OnResetSizeClicked;
begin
  Log('[SettingsPresenter] OnResetSizeClicked');
  Config.PositionW := -1;
  Config.PositionH := -1;
  FView.ShowInfo('Window size will be reset to default on next application start.');
end;

procedure TSettingsPresenter.OnResetPositionClicked;
begin
  Log('[SettingsPresenter] OnResetPositionClicked');
  Config.PositionX := -1;
  Config.PositionY := -1;
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
    Config.RemoveDeviceConfig(Address);
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
    if FileExists(Config.ConfigPath) then
      System.SysUtils.DeleteFile(Config.ConfigPath);

    // Reload defaults
    Config.Load;
    LoadSettings;

    FView.ShowInfo('Settings have been reset to defaults.');
  except
    on E: Exception do
      FView.ShowError('Failed to reset settings: ' + E.Message);
  end;
end;

procedure TSettingsPresenter.MarkModified;
begin
  FModified := True;
end;

end.
