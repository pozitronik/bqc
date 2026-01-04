unit SettingsForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.LogConfigIntf,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf,
  App.SettingsPresenter,
  App.WinRTSupport,
  UI.Theme,
  HotkeyPickerForm;

type
  /// <summary>
  /// Settings dialog form.
  /// Implements focused settings interfaces for ISP compliance.
  /// </summary>
  TFormSettings = class(TForm,
    ISettingsDialogView,
    IGeneralSettingsView,
    IHotkeySettingsView,
    IAppearanceSettingsView,
    ILayoutSettingsView,
    IConnectionSettingsView,
    ILoggingSettingsView,
    IDeviceSettingsView,
    IBatteryTraySettingsView,
    IProfileSettingsView)
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonApply: TButton;
    PageControl: TPageControl;
    SaveDialogLog: TSaveDialog;

    { Tab: Window }
    TabGeneral: TTabSheet;
    GroupWindowMode: TGroupBox;
    LabelWindowMode: TLabel;
    ComboWindowMode: TComboBox;
    GroupWindowOptions: TGroupBox;
    CheckMinimizeToTray: TCheckBox;
    CheckCloseToTray: TCheckBox;
    CheckStartMinimized: TCheckBox;
    GroupMenuOptions: TGroupBox;
    CheckHideOnFocusLoss: TCheckBox;
    GroupPosition: TGroupBox;
    LabelPositionMode: TLabel;
    ComboPositionMode: TComboBox;
    ButtonResetSize: TButton;
    ButtonResetPosition: TButton;
    CheckOnTop: TCheckBox;
    CheckAutostart: TCheckBox;

    { Tab: Appearance }
    TabAppearance: TTabSheet;
    GroupDisplayOptions: TGroupBox;
    LabelConnectedColor: TLabel;
    ShapeConnectedColor: TShape;
    CheckShowDeviceIcons: TCheckBox;
    CheckShowLastSeen: TCheckBox;
    RadioLastSeenRelative: TRadioButton;
    RadioLastSeenAbsolute: TRadioButton;
    CheckShowAddresses: TCheckBox;
    CheckShowBatteryLevel: TCheckBox;
    GroupLayout: TGroupBox;
    LabelItemHeight: TLabel;
    LabelItemPadding: TLabel;
    LabelItemMargin: TLabel;
    LabelIconSize: TLabel;
    LabelCornerRadius: TLabel;
    LabelBorderWidth: TLabel;
    LabelBorderColor: TLabel;
    LabelPx1: TLabel;
    LabelPx2: TLabel;
    LabelPx3: TLabel;
    LabelPx4: TLabel;
    LabelPx5: TLabel;
    LabelPx6: TLabel;
    EditItemHeight: TEdit;
    UpDownItemHeight: TUpDown;
    EditItemPadding: TEdit;
    UpDownItemPadding: TUpDown;
    EditItemMargin: TEdit;
    UpDownItemMargin: TUpDown;
    EditIconSize: TEdit;
    UpDownIconSize: TUpDown;
    EditCornerRadius: TEdit;
    UpDownCornerRadius: TUpDown;
    EditBorderWidth: TEdit;
    UpDownBorderWidth: TUpDown;
    ShapeBorderColor: TShape;
    ButtonResetLayout: TButton;
    GroupFontSizes: TGroupBox;
    LabelDeviceNameSize: TLabel;
    LabelStatusSize: TLabel;
    LabelAddressSize: TLabel;
    LabelIconFontSize: TLabel;
    EditDeviceNameSize: TEdit;
    UpDownDeviceNameSize: TUpDown;
    EditStatusSize: TEdit;
    UpDownStatusSize: TUpDown;
    EditAddressSize: TEdit;
    UpDownAddressSize: TUpDown;
    EditIconFontSize: TEdit;
    UpDownIconFontSize: TUpDown;
    GroupProfiles: TGroupBox;
    LabelProfileFontSize: TLabel;
    LabelProfileFontSizePt: TLabel;
    CheckShowProfiles: TCheckBox;
    EditProfileFontSize: TEdit;
    UpDownProfileFontSize: TUpDown;

    { Tab: Keys }
    TabKeys: TTabSheet;
    GroupHotkey: TGroupBox;
    LabelHotkey: TLabel;
    EditHotkey: TEdit;
    ButtonRecordHotkey: TButton;
    ButtonClearHotkey: TButton;
    CheckUseLowLevelHook: TCheckBox;
    GroupSystemPanels: TGroupBox;
    LabelCastPanelHotkey: TLabel;
    LabelBluetoothPanelHotkey: TLabel;
    EditCastPanelHotkey: TEdit;
    ButtonRecordCastHotkey: TButton;
    ButtonClearCastHotkey: TButton;
    EditBluetoothPanelHotkey: TEdit;
    ButtonRecordBluetoothHotkey: TButton;
    ButtonClearBluetoothHotkey: TButton;

    { Tab: Themes }
    TabThemes: TTabSheet;
    GroupTheme: TGroupBox;
    LabelTheme: TLabel;
    LabelVsfDir: TLabel;
    ComboTheme: TComboBox;
    EditVsfDir: TEdit;
    ButtonBrowseVsfDir: TButton;

    { Tab: Connection }
    TabConnection: TTabSheet;
    GroupConnectionDefaults: TGroupBox;
    LabelTimeout: TLabel;
    LabelTimeoutMs: TLabel;
    LabelRetryCount: TLabel;
    EditTimeout: TEdit;
    UpDownTimeout: TUpDown;
    EditRetryCount: TEdit;
    UpDownRetryCount: TUpDown;
    GroupPolling: TGroupBox;
    LabelPollingMode: TLabel;
    LabelPollingInterval: TLabel;
    LabelPollingIntervalMs: TLabel;
    LabelPollingModeHint: TLabel;
    ComboPollingMode: TComboBox;
    EditPollingInterval: TEdit;
    UpDownPollingInterval: TUpDown;
    GroupNotifications: TGroupBox;
    CheckNotifyOnConnect: TCheckBox;
    CheckNotifyOnDisconnect: TCheckBox;
    CheckNotifyOnConnectFailed: TCheckBox;
    CheckNotifyOnAutoConnect: TCheckBox;

    { Tab: Devices }
    TabDevices: TTabSheet;
    PanelDeviceList: TPanel;
    ListDevices: TListBox;
    PanelDeviceButtons: TPanel;
    ButtonForgetDevice: TButton;
    ButtonRefreshDevices: TButton;
    PanelDeviceSettings: TPanel;
    GroupDeviceInfo: TGroupBox;
    GroupDeviceConnection: TGroupBox;
    LabelDeviceTimeout: TLabel;
    LabelDeviceTimeoutMs: TLabel;
    LabelDeviceRetry: TLabel;
    EditDeviceTimeout: TEdit;
    UpDownDeviceTimeout: TUpDown;
    EditDeviceRetryCount: TEdit;
    UpDownDeviceRetryCount: TUpDown;
    GroupDeviceNotifications: TGroupBox;
    LabelDeviceNotifyConnect: TLabel;
    LabelDeviceNotifyDisconnect: TLabel;
    LabelDeviceNotifyFailed: TLabel;
    LabelDeviceNotifyAuto: TLabel;
    ComboDeviceNotifyConnect: TComboBox;
    ComboDeviceNotifyDisconnect: TComboBox;
    ComboDeviceNotifyFailed: TComboBox;
    ComboDeviceNotifyAuto: TComboBox;
    GroupDeviceBatteryTray: TGroupBox;
    LabelDeviceBatteryTrayIcon: TLabel;
    LabelDeviceBatteryColor: TLabel;
    LabelDeviceBatteryBackground: TLabel;
    LabelDeviceBatteryThreshold: TLabel;
    LabelDeviceBatteryNumeric: TLabel;
    LabelDeviceBatteryNotifyLow: TLabel;
    LabelDeviceBatteryNotifyFull: TLabel;
    ComboDeviceBatteryTrayIcon: TComboBox;
    ComboDeviceBatteryColorMode: TComboBox;
    ComboDeviceBatteryBackgroundMode: TComboBox;
    ComboDeviceBatteryNumeric: TComboBox;
    ComboDeviceBatteryNotifyLow: TComboBox;
    ComboDeviceBatteryNotifyFull: TComboBox;
    EditDeviceBatteryThreshold: TEdit;
    UpDownDeviceBatteryThreshold: TUpDown;
    ShapeDeviceBatteryColor: TShape;
    ShapeDeviceBatteryBackground: TShape;

    { Tab: Battery Tray }
    TabBatteryTray: TTabSheet;
    GroupBatteryTrayGlobal: TGroupBox;
    CheckShowBatteryTrayIcons: TCheckBox;
    LabelDefaultBatteryColor: TLabel;
    ShapeDefaultBatteryColor: TShape;
    LabelDefaultBackgroundColor: TLabel;
    ShapeDefaultBackgroundColor: TShape;
    CheckTransparentBackground: TCheckBox;
    LabelDefaultBatteryThreshold: TLabel;
    EditDefaultBatteryThreshold: TEdit;
    UpDownDefaultBatteryThreshold: TUpDown;
    LabelDefaultBatteryThresholdPct: TLabel;
    CheckShowNumericValue: TCheckBox;
    CheckAutoColorOnLow: TCheckBox;
    GroupBatteryNotifications: TGroupBox;
    CheckDefaultNotifyLowBattery: TCheckBox;
    CheckDefaultNotifyFullyCharged: TCheckBox;

    { Tab: Diagnostics }
    TabAdvanced: TTabSheet;
    GroupLogging: TGroupBox;
    LabelLogFilename: TLabel;
    LabelLogLevel: TLabel;
    CheckLogEnabled: TCheckBox;
    CheckLogAppend: TCheckBox;
    EditLogFilename: TEdit;
    ComboLogLevel: TComboBox;
    ButtonBrowseLogFile: TButton;
    ButtonOpenLogFile: TButton;
    GroupActions: TGroupBox;
    ButtonOpenConfig: TButton;
    ButtonResetDefaults: TButton;

    { Common dialogs }
    ColorDialogConnected: TColorDialog;
    GroupDeviceGeneral: TGroupBox;
    LabelDeviceAlias: TLabel;
    LabelDeviceType: TLabel;
    EditDeviceAlias: TEdit;
    ComboDeviceType: TComboBox;
    CheckDevicePinned: TCheckBox;
    CheckDeviceHidden: TCheckBox;
    CheckDeviceAutoConnect: TCheckBox;
    LabelDeviceShowProfiles: TLabel;
    ComboDeviceShowProfiles: TComboBox;
    LabelEnumerationMode: TLabel;
    LabelEnumerationModeHint: TLabel;
    ComboEnumerationMode: TComboBox;
    LabelBluetoothPlatform: TLabel;
    LabelBluetoothPlatformHint: TLabel;
    ComboBluetoothPlatform: TComboBox;
    LabelOutlineColorMode: TLabel;
    ComboOutlineColorMode: TComboBox;
    LabelCustomOutlineColor: TLabel;
    ShapeCustomOutlineColor: TShape;

    { Form events }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    { Button events }
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);

    { General tab events }
    procedure ComboWindowModeChange(Sender: TObject);

    { Hotkey tab events }
    procedure ButtonRecordHotkeyClick(Sender: TObject);
    procedure ButtonClearHotkeyClick(Sender: TObject);
    procedure ButtonResetSizeClick(Sender: TObject);
    procedure ButtonResetPositionClick(Sender: TObject);
    procedure ButtonRecordCastHotkeyClick(Sender: TObject);
    procedure ButtonClearCastHotkeyClick(Sender: TObject);
    procedure ButtonRecordBluetoothHotkeyClick(Sender: TObject);
    procedure ButtonClearBluetoothHotkeyClick(Sender: TObject);

    { Devices tab events }
    procedure ListDevicesClick(Sender: TObject);
    procedure ButtonForgetDeviceClick(Sender: TObject);
    procedure ButtonRefreshDevicesClick(Sender: TObject);

    { Theme events }
    procedure ButtonBrowseVsfDirClick(Sender: TObject);

    { Advanced tab events }
    procedure ButtonBrowseLogFileClick(Sender: TObject);
    procedure ButtonOpenConfigClick(Sender: TObject);
    procedure ButtonOpenLogFileClick(Sender: TObject);
    procedure ButtonResetDefaultsClick(Sender: TObject);

    { Appearance tab events }
    procedure ButtonResetLayoutClick(Sender: TObject);
    procedure HandleShapeColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HandleTransparentCheckboxClick(Sender: TObject);

    { Device battery tray events }
    procedure ComboDeviceBatteryColorModeChange(Sender: TObject);
    procedure ComboDeviceBatteryBackgroundModeChange(Sender: TObject);

    { Battery tray tab events }
    procedure ComboOutlineColorModeChange(Sender: TObject);

  private
    FPresenter: TSettingsPresenter;

    { Injected dependencies }
    FAppConfig: IAppConfig;
    FLogConfig: ILogConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FBatteryTrayConfig: IBatteryTrayConfig;
    FProfileConfig: IProfileConfig;
    FThemeManager: IThemeManager;

    { ISettingsDialogView implementation }
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);

    { IGeneralSettingsView implementation }
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);

    { IHotkeySettingsView implementation }
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);

    { IAppearanceSettingsView implementation }
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
    procedure PopulateThemeList(const ACurrentTheme: string);

    { ILayoutSettingsView implementation }
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);

    { IConnectionSettingsView implementation }
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);

    { ILoggingSettingsView implementation }
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);

    { IDeviceSettingsView implementation }
    procedure PopulateDeviceList(const AItems: TArray<string>);
    function GetSelectedDeviceIndex: Integer;
    procedure SetSelectedDeviceIndex(AIndex: Integer);
    function GetDeviceSettings: TDeviceViewSettings;
    procedure SetDeviceSettings(const ASettings: TDeviceViewSettings);
    procedure ClearDeviceSettings;

    { IBatteryTraySettingsView implementation }
    function GetBatteryTraySettings: TBatteryTrayViewSettings;
    procedure SetBatteryTraySettings(const ASettings: TBatteryTrayViewSettings);

    { IProfileSettingsView implementation }
    function GetProfileSettings: TProfileViewSettings;
    procedure SetProfileSettings(const ASettings: TProfileViewSettings);

    { UI helpers }
    procedure InitUpDownLimits;
    procedure InitDeviceTypeCombo;
    procedure UpdateWindowModeControls;
    procedure HandleSettingChanged(Sender: TObject);
    procedure ConnectChangeHandlers;
    procedure ConfigureWinRTDependentControls;

  public
    { Public declarations }
    procedure Setup(
      AAppConfig: IAppConfig;
      ALogConfig: ILogConfig;
      ADeviceConfigProvider: IDeviceConfigProvider;
      ABatteryTrayConfig: IBatteryTrayConfig;
      AProfileConfig: IProfileConfig;
      AThemeManager: IThemeManager
    );
    function GetOnSettingsApplied: TNotifyEvent;
    procedure SetOnSettingsApplied(AValue: TNotifyEvent);
    property OnSettingsApplied: TNotifyEvent read GetOnSettingsApplied write SetOnSettingsApplied;
  end;

var
  FormSettings: TFormSettings;

implementation

uses
  ShellAPI,
  System.Generics.Collections,
  Vcl.FileCtrl,
  App.Logger,
  App.Config,
  Bluetooth.Types,
  UI.HotkeyManager;

{$R *.dfm}

{ TFormSettings }

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  LogDebug('FormCreate', ClassName);
  // Dependencies must be injected via Setup() before showing dialog
  // Setup() is called by the parent form (MainForm) after Create
end;

procedure TFormSettings.Setup(
  AAppConfig: IAppConfig;
  ALogConfig: ILogConfig;
  ADeviceConfigProvider: IDeviceConfigProvider;
  ABatteryTrayConfig: IBatteryTrayConfig;
  AProfileConfig: IProfileConfig;
  AThemeManager: IThemeManager
);
begin
  FAppConfig := AAppConfig;
  FLogConfig := ALogConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FBatteryTrayConfig := ABatteryTrayConfig;
  FProfileConfig := AProfileConfig;
  FThemeManager := AThemeManager;

  // Create presenter with injected dependencies
  FPresenter := TSettingsPresenter.Create(
    Self as ISettingsDialogView,
    Self as IGeneralSettingsView,
    Self as IHotkeySettingsView,
    Self as IAppearanceSettingsView,
    Self as ILayoutSettingsView,
    Self as IConnectionSettingsView,
    Self as ILoggingSettingsView,
    Self as IBatteryTraySettingsView,
    Self as IProfileSettingsView,
    Self as IDeviceSettingsView,
    FAppConfig,
    FDeviceConfigProvider,
    FBatteryTrayConfig,
    FProfileConfig
  );
end;

procedure TFormSettings.FormDestroy(Sender: TObject);
begin
  LogDebug('FormDestroy', ClassName);
  FPresenter.Free;
end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
  LogDebug('FormShow', ClassName);
  InitUpDownLimits;
  InitDeviceTypeCombo;
  ConfigureWinRTDependentControls;
  FPresenter.LoadSettings;
  UpdateWindowModeControls;
  ConnectChangeHandlers;
  ButtonApply.Enabled := False;
end;

procedure TFormSettings.ButtonOKClick(Sender: TObject);
begin
  FPresenter.OnOKClicked;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
  FPresenter.OnCancelClicked;
end;

procedure TFormSettings.ButtonApplyClick(Sender: TObject);
begin
  FPresenter.OnApplyClicked;
end;

{ ISettingsDialogView implementation }

procedure TFormSettings.CloseWithOK;
begin
  ModalResult := mrOk;
end;

procedure TFormSettings.CloseWithCancel;
begin
  ModalResult := mrCancel;
end;

procedure TFormSettings.ShowError(const AMessage: string);
begin
  MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure TFormSettings.ShowInfo(const AMessage: string);
begin
  MessageDlg(AMessage, mtInformation, [mbOK], 0);
end;

procedure TFormSettings.SetApplyEnabled(AEnabled: Boolean);
begin
  ButtonApply.Enabled := AEnabled;
end;

{ IGeneralSettingsView implementation }

function TFormSettings.GetGeneralSettings: TGeneralViewSettings;
begin
  Result.WindowMode := TWindowMode(ComboWindowMode.ItemIndex);
  Result.OnTop := CheckOnTop.Checked;
  Result.MinimizeToTray := CheckMinimizeToTray.Checked;
  Result.CloseToTray := CheckCloseToTray.Checked;
  Result.StartMinimized := CheckStartMinimized.Checked;
  Result.HideOnFocusLoss := CheckHideOnFocusLoss.Checked;
  Result.Autostart := CheckAutostart.Checked;
  Result.PositionMode := TPositionMode(ComboPositionMode.ItemIndex);
end;

procedure TFormSettings.SetGeneralSettings(const ASettings: TGeneralViewSettings);
begin
  ComboWindowMode.ItemIndex := Ord(ASettings.WindowMode);
  CheckOnTop.Checked := ASettings.OnTop;
  CheckMinimizeToTray.Checked := ASettings.MinimizeToTray;
  CheckCloseToTray.Checked := ASettings.CloseToTray;
  CheckStartMinimized.Checked := ASettings.StartMinimized;
  CheckHideOnFocusLoss.Checked := ASettings.HideOnFocusLoss;
  CheckAutostart.Checked := ASettings.Autostart;
  ComboPositionMode.ItemIndex := Ord(ASettings.PositionMode);
  UpdateWindowModeControls;
end;

function TFormSettings.GetHotkeySettings: THotkeyViewSettings;
begin
  Result.Hotkey := EditHotkey.Text;
  Result.UseLowLevelHook := CheckUseLowLevelHook.Checked;
  Result.CastPanelHotkey := EditCastPanelHotkey.Text;
  Result.BluetoothPanelHotkey := EditBluetoothPanelHotkey.Text;
end;

procedure TFormSettings.SetHotkeySettings(const ASettings: THotkeyViewSettings);
begin
  EditHotkey.Text := ASettings.Hotkey;
  CheckUseLowLevelHook.Checked := ASettings.UseLowLevelHook;
  EditCastPanelHotkey.Text := ASettings.CastPanelHotkey;
  EditBluetoothPanelHotkey.Text := ASettings.BluetoothPanelHotkey;
end;

function TFormSettings.GetAppearanceSettings: TAppearanceViewSettings;
begin
  // Extract actual style name from display name (which may include filename)
  Result.Theme := FThemeManager.GetStyleNameFromDisplay(ComboTheme.Text);
  Result.VsfDir := EditVsfDir.Text;
  Result.ShowAddresses := CheckShowAddresses.Checked;
  Result.ShowDeviceIcons := CheckShowDeviceIcons.Checked;
  Result.ShowLastSeen := CheckShowLastSeen.Checked;
  Result.LastSeenRelative := RadioLastSeenRelative.Checked;
  Result.ShowBatteryLevel := CheckShowBatteryLevel.Checked;
  Result.ConnectedColor := ShapeConnectedColor.Brush.Color;
end;

procedure TFormSettings.SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
begin
  // Theme is set via PopulateThemeList
  EditVsfDir.Text := ASettings.VsfDir;
  CheckShowAddresses.Checked := ASettings.ShowAddresses;
  CheckShowDeviceIcons.Checked := ASettings.ShowDeviceIcons;
  CheckShowLastSeen.Checked := ASettings.ShowLastSeen;
  RadioLastSeenRelative.Checked := ASettings.LastSeenRelative;
  RadioLastSeenAbsolute.Checked := not ASettings.LastSeenRelative;
  CheckShowBatteryLevel.Checked := ASettings.ShowBatteryLevel;
  ShapeConnectedColor.Brush.Color := ASettings.ConnectedColor;
end;

function TFormSettings.GetLayoutSettings: TLayoutViewSettings;
begin
  Result.ItemHeight := UpDownItemHeight.Position;
  Result.ItemPadding := UpDownItemPadding.Position;
  Result.ItemMargin := UpDownItemMargin.Position;
  Result.IconSize := UpDownIconSize.Position;
  Result.CornerRadius := UpDownCornerRadius.Position;
  Result.BorderWidth := UpDownBorderWidth.Position;
  Result.BorderColor := ShapeBorderColor.Brush.Color;
  Result.DeviceNameFontSize := UpDownDeviceNameSize.Position;
  Result.StatusFontSize := UpDownStatusSize.Position;
  Result.AddressFontSize := UpDownAddressSize.Position;
  Result.IconFontSize := UpDownIconFontSize.Position;
end;

procedure TFormSettings.SetLayoutSettings(const ASettings: TLayoutViewSettings);
begin
  UpDownItemHeight.Position := ASettings.ItemHeight;
  UpDownItemPadding.Position := ASettings.ItemPadding;
  UpDownItemMargin.Position := ASettings.ItemMargin;
  UpDownIconSize.Position := ASettings.IconSize;
  UpDownCornerRadius.Position := ASettings.CornerRadius;
  UpDownBorderWidth.Position := ASettings.BorderWidth;
  ShapeBorderColor.Brush.Color := ASettings.BorderColor;
  UpDownDeviceNameSize.Position := ASettings.DeviceNameFontSize;
  UpDownStatusSize.Position := ASettings.StatusFontSize;
  UpDownAddressSize.Position := ASettings.AddressFontSize;
  UpDownIconFontSize.Position := ASettings.IconFontSize;
end;

function TFormSettings.GetConnectionSettings: TConnectionViewSettings;
begin
  Result.Timeout := UpDownTimeout.Position;
  Result.RetryCount := UpDownRetryCount.Position;
  Result.EnumerationMode := TEnumerationMode(ComboEnumerationMode.ItemIndex);
  Result.BluetoothPlatform := TBluetoothPlatform(ComboBluetoothPlatform.ItemIndex);
  Result.PollingMode := TPollingMode(ComboPollingMode.ItemIndex);
  Result.PollingInterval := UpDownPollingInterval.Position;
  Result.NotifyOnConnect := CheckNotifyOnConnect.Checked;
  Result.NotifyOnDisconnect := CheckNotifyOnDisconnect.Checked;
  Result.NotifyOnConnectFailed := CheckNotifyOnConnectFailed.Checked;
  Result.NotifyOnAutoConnect := CheckNotifyOnAutoConnect.Checked;
end;

procedure TFormSettings.SetConnectionSettings(const ASettings: TConnectionViewSettings);
begin
  UpDownTimeout.Position := ASettings.Timeout;
  UpDownRetryCount.Position := ASettings.RetryCount;
  ComboEnumerationMode.ItemIndex := Ord(ASettings.EnumerationMode);
  ComboBluetoothPlatform.ItemIndex := Ord(ASettings.BluetoothPlatform);
  ComboPollingMode.ItemIndex := Ord(ASettings.PollingMode);
  UpDownPollingInterval.Position := ASettings.PollingInterval;
  CheckNotifyOnConnect.Checked := ASettings.NotifyOnConnect;
  CheckNotifyOnDisconnect.Checked := ASettings.NotifyOnDisconnect;
  CheckNotifyOnConnectFailed.Checked := ASettings.NotifyOnConnectFailed;
  CheckNotifyOnAutoConnect.Checked := ASettings.NotifyOnAutoConnect;
end;

function TFormSettings.GetLoggingSettings: TLoggingViewSettings;
begin
  Result.Enabled := CheckLogEnabled.Checked;
  Result.Filename := EditLogFilename.Text;
  Result.Append := CheckLogAppend.Checked;
  Result.LevelIndex := ComboLogLevel.ItemIndex;
end;

procedure TFormSettings.SetLoggingSettings(const ASettings: TLoggingViewSettings);
begin
  CheckLogEnabled.Checked := ASettings.Enabled;
  EditLogFilename.Text := ASettings.Filename;
  CheckLogAppend.Checked := ASettings.Append;
  ComboLogLevel.ItemIndex := ASettings.LevelIndex;
end;

{ IAppearanceSettingsView - Theme management }

procedure TFormSettings.PopulateThemeList(const ACurrentTheme: string);
var
  Styles: TArray<string>;
  StyleName, DisplayName, CurrentDisplayName: string;
  Idx: Integer;
begin
  // Get available styles from theme manager (injected dependency)
  Styles := FThemeManager.GetAvailableStyles;
  TArray.Sort<string>(Styles);

  ComboTheme.Items.Clear;
  CurrentDisplayName := '';
  for StyleName in Styles do
  begin
    DisplayName := FThemeManager.GetStyleDisplayName(StyleName);
    ComboTheme.Items.Add(DisplayName);
    if SameText(StyleName, ACurrentTheme) then
      CurrentDisplayName := DisplayName;
  end;

  // Select current theme by display name
  if CurrentDisplayName <> '' then
    Idx := ComboTheme.Items.IndexOf(CurrentDisplayName)
  else
    Idx := -1;

  if Idx >= 0 then
    ComboTheme.ItemIndex := Idx
  else if ComboTheme.Items.Count > 0 then
    ComboTheme.ItemIndex := 0;
end;

{ IDeviceSettingsView implementation }

procedure TFormSettings.PopulateDeviceList(const AItems: TArray<string>);
var
  Item: string;
begin
  ListDevices.Items.Clear;
  for Item in AItems do
    ListDevices.Items.Add(Item);
end;

function TFormSettings.GetSelectedDeviceIndex: Integer;
begin
  Result := ListDevices.ItemIndex;
end;

procedure TFormSettings.SetSelectedDeviceIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < ListDevices.Items.Count) then
    ListDevices.ItemIndex := AIndex
  else
    ListDevices.ItemIndex := -1;
end;

function TFormSettings.GetDeviceSettings: TDeviceViewSettings;
begin
  Result.Alias := EditDeviceAlias.Text;
  Result.DeviceTypeIndex := ComboDeviceType.ItemIndex;
  Result.Pinned := CheckDevicePinned.Checked;
  Result.Hidden := CheckDeviceHidden.Checked;
  Result.AutoConnect := CheckDeviceAutoConnect.Checked;
  Result.Timeout := UpDownDeviceTimeout.Position;
  Result.RetryCount := UpDownDeviceRetryCount.Position;
  Result.NotifyConnectIndex := ComboDeviceNotifyConnect.ItemIndex;
  Result.NotifyDisconnectIndex := ComboDeviceNotifyDisconnect.ItemIndex;
  Result.NotifyFailedIndex := ComboDeviceNotifyFailed.ItemIndex;
  Result.NotifyAutoIndex := ComboDeviceNotifyAuto.ItemIndex;
  // Battery tray per-device settings
  Result.BatteryTrayIconIndex := ComboDeviceBatteryTrayIcon.ItemIndex;
  // Icon color: -1 = Default, otherwise use shape color
  if ComboDeviceBatteryColorMode.ItemIndex = 0 then  // Default
    Result.BatteryIconColor := -1
  else
    Result.BatteryIconColor := Integer(ShapeDeviceBatteryColor.Brush.Color);
  // Background: -1 = Default, -2 = Transparent, otherwise use shape color
  case ComboDeviceBatteryBackgroundMode.ItemIndex of
    0: Result.BatteryBackgroundColor := -1;  // Default
    1: Result.BatteryBackgroundColor := Integer(ShapeDeviceBatteryBackground.Brush.Color);  // Custom
    2: Result.BatteryBackgroundColor := -2;  // Transparent
  else
    Result.BatteryBackgroundColor := -1;
  end;
  Result.BatteryShowNumericIndex := ComboDeviceBatteryNumeric.ItemIndex;
  Result.BatteryThreshold := UpDownDeviceBatteryThreshold.Position;
  Result.BatteryNotifyLowIndex := ComboDeviceBatteryNotifyLow.ItemIndex;
  Result.BatteryNotifyFullIndex := ComboDeviceBatteryNotifyFull.ItemIndex;
  // Profile display per-device setting
  Result.ShowProfilesIndex := ComboDeviceShowProfiles.ItemIndex;
end;

procedure TFormSettings.SetDeviceSettings(const ASettings: TDeviceViewSettings);
begin
  EditDeviceAlias.Text := ASettings.Alias;
  ComboDeviceType.ItemIndex := ASettings.DeviceTypeIndex;
  CheckDevicePinned.Checked := ASettings.Pinned;
  CheckDeviceHidden.Checked := ASettings.Hidden;
  CheckDeviceAutoConnect.Checked := ASettings.AutoConnect;
  UpDownDeviceTimeout.Position := ASettings.Timeout;
  UpDownDeviceRetryCount.Position := ASettings.RetryCount;
  ComboDeviceNotifyConnect.ItemIndex := ASettings.NotifyConnectIndex;
  ComboDeviceNotifyDisconnect.ItemIndex := ASettings.NotifyDisconnectIndex;
  ComboDeviceNotifyFailed.ItemIndex := ASettings.NotifyFailedIndex;
  ComboDeviceNotifyAuto.ItemIndex := ASettings.NotifyAutoIndex;
  // Battery tray per-device settings
  ComboDeviceBatteryTrayIcon.ItemIndex := ASettings.BatteryTrayIconIndex;
  // Icon color: -1 = Default, else Custom with color
  if ASettings.BatteryIconColor < 0 then
  begin
    ComboDeviceBatteryColorMode.ItemIndex := 0;  // Default
    ShapeDeviceBatteryColor.Brush.Color := clGreen;  // Show default color
    ShapeDeviceBatteryColor.Visible := False;
  end
  else
  begin
    ComboDeviceBatteryColorMode.ItemIndex := 1;  // Custom
    ShapeDeviceBatteryColor.Brush.Color := TColor(ASettings.BatteryIconColor);
    ShapeDeviceBatteryColor.Visible := True;
  end;
  // Background: -1 = Default, -2 = Transparent, else Custom with color
  if ASettings.BatteryBackgroundColor = -2 then
  begin
    ComboDeviceBatteryBackgroundMode.ItemIndex := 2;  // Transparent
    ShapeDeviceBatteryBackground.Visible := False;
  end
  else if ASettings.BatteryBackgroundColor < 0 then
  begin
    ComboDeviceBatteryBackgroundMode.ItemIndex := 0;  // Default
    ShapeDeviceBatteryBackground.Visible := False;
  end
  else
  begin
    ComboDeviceBatteryBackgroundMode.ItemIndex := 1;  // Custom
    ShapeDeviceBatteryBackground.Brush.Color := TColor(ASettings.BatteryBackgroundColor);
    ShapeDeviceBatteryBackground.Visible := True;
  end;
  ComboDeviceBatteryNumeric.ItemIndex := ASettings.BatteryShowNumericIndex;
  UpDownDeviceBatteryThreshold.Position := ASettings.BatteryThreshold;
  ComboDeviceBatteryNotifyLow.ItemIndex := ASettings.BatteryNotifyLowIndex;
  ComboDeviceBatteryNotifyFull.ItemIndex := ASettings.BatteryNotifyFullIndex;
  // Profile display per-device setting
  ComboDeviceShowProfiles.ItemIndex := ASettings.ShowProfilesIndex;
end;

procedure TFormSettings.ClearDeviceSettings;
begin
  EditDeviceAlias.Text := '';
  ComboDeviceType.ItemIndex := 0;
  CheckDevicePinned.Checked := False;
  CheckDeviceHidden.Checked := False;
  CheckDeviceAutoConnect.Checked := False;
  UpDownDeviceTimeout.Position := -1;
  UpDownDeviceRetryCount.Position := -1;
  ComboDeviceNotifyConnect.ItemIndex := 0;
  ComboDeviceNotifyDisconnect.ItemIndex := 0;
  ComboDeviceNotifyFailed.ItemIndex := 0;
  ComboDeviceNotifyAuto.ItemIndex := 0;
  // Battery tray per-device settings
  ComboDeviceBatteryTrayIcon.ItemIndex := 0;
  ComboDeviceBatteryColorMode.ItemIndex := 0;  // Default
  ShapeDeviceBatteryColor.Brush.Color := clGreen;
  ShapeDeviceBatteryColor.Visible := False;
  ComboDeviceBatteryBackgroundMode.ItemIndex := 0;  // Default
  ShapeDeviceBatteryBackground.Brush.Color := clWhite;
  ShapeDeviceBatteryBackground.Visible := False;
  ComboDeviceBatteryNumeric.ItemIndex := 0;
  UpDownDeviceBatteryThreshold.Position := -1;
  ComboDeviceBatteryNotifyLow.ItemIndex := 0;
  ComboDeviceBatteryNotifyFull.ItemIndex := 0;
  // Profile display per-device setting
  ComboDeviceShowProfiles.ItemIndex := 0;  // Default
end;

{ IBatteryTraySettingsView implementation }

function TFormSettings.GetBatteryTraySettings: TBatteryTrayViewSettings;
begin
  Result.ShowBatteryTrayIcons := CheckShowBatteryTrayIcons.Checked;
  Result.DefaultIconColor := ShapeDefaultBatteryColor.Brush.Color;
  // Background color: transparent if checkbox is checked, otherwise use shape color
  if CheckTransparentBackground.Checked then
    Result.DefaultBackgroundColor := TColor($1FFFFFFF)  // Transparent
  else
    Result.DefaultBackgroundColor := ShapeDefaultBackgroundColor.Brush.Color;
  Result.DefaultShowNumericValue := CheckShowNumericValue.Checked;
  Result.DefaultLowBatteryThreshold := UpDownDefaultBatteryThreshold.Position;
  Result.DefaultNotifyLowBattery := CheckDefaultNotifyLowBattery.Checked;
  Result.DefaultNotifyFullyCharged := CheckDefaultNotifyFullyCharged.Checked;
  // Outline color mode
  if Assigned(ComboOutlineColorMode) then
    Result.DefaultOutlineColorModeIndex := ComboOutlineColorMode.ItemIndex
  else
    Result.DefaultOutlineColorModeIndex := 0; // Auto
  if Assigned(ShapeCustomOutlineColor) then
    Result.DefaultCustomOutlineColor := ShapeCustomOutlineColor.Brush.Color
  else
    Result.DefaultCustomOutlineColor := clBlack;
end;

procedure TFormSettings.SetBatteryTraySettings(const ASettings: TBatteryTrayViewSettings);
var
  IsTransparent: Boolean;
  IsCustomOutline: Boolean;
begin
  CheckShowBatteryTrayIcons.Checked := ASettings.ShowBatteryTrayIcons;
  ShapeDefaultBatteryColor.Brush.Color := ASettings.DefaultIconColor;
  // Background color: check if transparent
  IsTransparent := (ASettings.DefaultBackgroundColor = clNone) or
                   (ASettings.DefaultBackgroundColor > $1000000);
  CheckTransparentBackground.Checked := IsTransparent;
  if IsTransparent then
    ShapeDefaultBackgroundColor.Brush.Color := clWhite  // Show white when transparent
  else
    ShapeDefaultBackgroundColor.Brush.Color := ASettings.DefaultBackgroundColor;
  ShapeDefaultBackgroundColor.Enabled := not IsTransparent;
  CheckShowNumericValue.Checked := ASettings.DefaultShowNumericValue;
  UpDownDefaultBatteryThreshold.Position := ASettings.DefaultLowBatteryThreshold;
  CheckDefaultNotifyLowBattery.Checked := ASettings.DefaultNotifyLowBattery;
  CheckDefaultNotifyFullyCharged.Checked := ASettings.DefaultNotifyFullyCharged;
  // Outline color mode
  if Assigned(ComboOutlineColorMode) then
    ComboOutlineColorMode.ItemIndex := ASettings.DefaultOutlineColorModeIndex;
  if Assigned(ShapeCustomOutlineColor) then
    ShapeCustomOutlineColor.Brush.Color := ASettings.DefaultCustomOutlineColor;
  // Show/hide custom outline color picker based on mode
  IsCustomOutline := ASettings.DefaultOutlineColorModeIndex = 3;  // 3 = Custom
  if Assigned(LabelCustomOutlineColor) then
    LabelCustomOutlineColor.Visible := IsCustomOutline;
  if Assigned(ShapeCustomOutlineColor) then
    ShapeCustomOutlineColor.Visible := IsCustomOutline;
end;

{ IProfileSettingsView implementation }

function TFormSettings.GetProfileSettings: TProfileViewSettings;
begin
  Result.ShowProfiles := CheckShowProfiles.Checked;
  Result.ProfileFontSize := UpDownProfileFontSize.Position;
end;

procedure TFormSettings.SetProfileSettings(const ASettings: TProfileViewSettings);
begin
  CheckShowProfiles.Checked := ASettings.ShowProfiles;
  UpDownProfileFontSize.Position := ASettings.ProfileFontSize;
end;

{ UI helpers - Initialization }

procedure TFormSettings.InitUpDownLimits;
begin
  // Layout limits
  UpDownItemHeight.Min := MIN_ITEM_HEIGHT;
  UpDownItemHeight.Max := MAX_ITEM_HEIGHT;
  UpDownItemPadding.Min := MIN_ITEM_PADDING;
  UpDownItemPadding.Max := MAX_ITEM_PADDING;
  UpDownItemMargin.Min := MIN_ITEM_MARGIN;
  UpDownItemMargin.Max := MAX_ITEM_MARGIN;
  UpDownIconSize.Min := MIN_ICON_SIZE;
  UpDownIconSize.Max := MAX_ICON_SIZE;
  UpDownCornerRadius.Min := MIN_CORNER_RADIUS;
  UpDownCornerRadius.Max := MAX_CORNER_RADIUS;
  UpDownBorderWidth.Min := MIN_ITEM_BORDER_WIDTH;
  UpDownBorderWidth.Max := MAX_ITEM_BORDER_WIDTH;

  // Font size limits
  UpDownDeviceNameSize.Min := MIN_DEVICE_NAME_FONT_SIZE;
  UpDownDeviceNameSize.Max := MAX_DEVICE_NAME_FONT_SIZE;
  UpDownStatusSize.Min := MIN_STATUS_FONT_SIZE;
  UpDownStatusSize.Max := MAX_STATUS_FONT_SIZE;
  UpDownAddressSize.Min := MIN_ADDRESS_FONT_SIZE;
  UpDownAddressSize.Max := MAX_ADDRESS_FONT_SIZE;
  UpDownIconFontSize.Min := MIN_ICON_FONT_SIZE;
  UpDownIconFontSize.Max := MAX_ICON_FONT_SIZE;

  // Connection limits
  UpDownTimeout.Min := MIN_CONNECTION_TIMEOUT;
  UpDownTimeout.Max := MAX_CONNECTION_TIMEOUT;
  UpDownRetryCount.Min := MIN_CONNECTION_RETRY_COUNT;
  UpDownRetryCount.Max := MAX_CONNECTION_RETRY_COUNT;
  UpDownPollingInterval.Min := MIN_POLLING_INTERVAL;
  UpDownPollingInterval.Max := MAX_POLLING_INTERVAL;

  // Per-device connection limits (allow -1 for "use global")
  UpDownDeviceTimeout.Min := -1;
  UpDownDeviceTimeout.Max := MAX_CONNECTION_TIMEOUT;
  UpDownDeviceRetryCount.Min := -1;
  UpDownDeviceRetryCount.Max := MAX_CONNECTION_RETRY_COUNT;
end;

procedure TFormSettings.InitDeviceTypeCombo;
var
  I: Integer;
begin
  ComboDeviceType.Items.Clear;
  for I := Low(DeviceTypeNames) to High(DeviceTypeNames) do
    ComboDeviceType.Items.Add(DeviceTypeNames[I]);
  ComboDeviceType.Style := csDropDownList;
end;

function TFormSettings.GetOnSettingsApplied: TNotifyEvent;
begin
  Result := FPresenter.OnSettingsApplied;
end;

procedure TFormSettings.SetOnSettingsApplied(AValue: TNotifyEvent);
begin
  FPresenter.OnSettingsApplied := AValue;
end;

{ General tab events }

procedure TFormSettings.ComboWindowModeChange(Sender: TObject);
begin
  UpdateWindowModeControls;
end;

procedure TFormSettings.UpdateWindowModeControls;
var
  IsMenuMode: Boolean;
begin
  // ComboBox matches TWindowMode: 0 = Menu, 1 = Window
  IsMenuMode := ComboWindowMode.ItemIndex = 0;

  // Apply bold style to the active mode's group caption
  if IsMenuMode then
  begin
    GroupMenuOptions.Font.Style := [fsBold];
    GroupWindowOptions.Font.Style := [];
  end
  else
  begin
    GroupWindowOptions.Font.Style := [fsBold];
    GroupMenuOptions.Font.Style := [];
  end;
end;

procedure TFormSettings.HandleSettingChanged(Sender: TObject);
begin
  FPresenter.MarkModified;
  // Special handling for window mode combo - also update UI
  if Sender = ComboWindowMode then
    UpdateWindowModeControls;
end;

procedure TFormSettings.ConnectChangeHandlers;
begin
  // Tab: General
  ComboWindowMode.OnChange := HandleSettingChanged;
  CheckOnTop.OnClick := HandleSettingChanged;
  CheckMinimizeToTray.OnClick := HandleSettingChanged;
  CheckCloseToTray.OnClick := HandleSettingChanged;
  CheckStartMinimized.OnClick := HandleSettingChanged;
  CheckHideOnFocusLoss.OnClick := HandleSettingChanged;
  CheckAutostart.OnClick := HandleSettingChanged;
  ComboPositionMode.OnChange := HandleSettingChanged;

  // Tab: Hotkey & Visuals
  EditHotkey.OnChange := HandleSettingChanged;
  CheckUseLowLevelHook.OnClick := HandleSettingChanged;
  EditCastPanelHotkey.OnChange := HandleSettingChanged;
  EditBluetoothPanelHotkey.OnChange := HandleSettingChanged;
  ComboTheme.OnChange := HandleSettingChanged;
  EditVsfDir.OnChange := HandleSettingChanged;
  CheckShowAddresses.OnClick := HandleSettingChanged;

  // Tab: Connection
  EditTimeout.OnChange := HandleSettingChanged;
  EditRetryCount.OnChange := HandleSettingChanged;
  ComboEnumerationMode.OnChange := HandleSettingChanged;
  ComboBluetoothPlatform.OnChange := HandleSettingChanged;
  ComboPollingMode.OnChange := HandleSettingChanged;
  EditPollingInterval.OnChange := HandleSettingChanged;

  // Tab: Notifications
  CheckNotifyOnConnect.OnClick := HandleSettingChanged;
  CheckNotifyOnDisconnect.OnClick := HandleSettingChanged;
  CheckNotifyOnConnectFailed.OnClick := HandleSettingChanged;
  CheckNotifyOnAutoConnect.OnClick := HandleSettingChanged;

  // Tab: Devices
  EditDeviceAlias.OnChange := HandleSettingChanged;
  ComboDeviceType.OnChange := HandleSettingChanged;
  CheckDevicePinned.OnClick := HandleSettingChanged;
  CheckDeviceHidden.OnClick := HandleSettingChanged;
  CheckDeviceAutoConnect.OnClick := HandleSettingChanged;
  EditDeviceTimeout.OnChange := HandleSettingChanged;
  EditDeviceRetryCount.OnChange := HandleSettingChanged;
  ComboDeviceNotifyConnect.OnChange := HandleSettingChanged;
  ComboDeviceNotifyDisconnect.OnChange := HandleSettingChanged;
  ComboDeviceNotifyFailed.OnChange := HandleSettingChanged;
  ComboDeviceNotifyAuto.OnChange := HandleSettingChanged;
  // Device battery tray settings
  ComboDeviceBatteryTrayIcon.OnChange := HandleSettingChanged;
  ShapeDeviceBatteryColor.OnMouseDown := HandleShapeColorMouseDown;
  ShapeDeviceBatteryBackground.OnMouseDown := HandleShapeColorMouseDown;
  ComboDeviceBatteryNumeric.OnChange := HandleSettingChanged;
  EditDeviceBatteryThreshold.OnChange := HandleSettingChanged;
  ComboDeviceBatteryNotifyLow.OnChange := HandleSettingChanged;
  ComboDeviceBatteryNotifyFull.OnChange := HandleSettingChanged;
  // Device profile display setting
  ComboDeviceShowProfiles.OnChange := HandleSettingChanged;

  // Tab: Battery Tray (global)
  CheckShowBatteryTrayIcons.OnClick := HandleSettingChanged;
  ShapeDefaultBatteryColor.OnMouseDown := HandleShapeColorMouseDown;
  ShapeDefaultBackgroundColor.OnMouseDown := HandleShapeColorMouseDown;
  CheckTransparentBackground.OnClick := HandleTransparentCheckboxClick;
  EditDefaultBatteryThreshold.OnChange := HandleSettingChanged;
  CheckShowNumericValue.OnClick := HandleSettingChanged;
  CheckAutoColorOnLow.OnClick := HandleSettingChanged;
  CheckDefaultNotifyLowBattery.OnClick := HandleSettingChanged;
  CheckDefaultNotifyFullyCharged.OnClick := HandleSettingChanged;
  // Outline color mode
  if Assigned(ComboOutlineColorMode) then
    ComboOutlineColorMode.OnChange := ComboOutlineColorModeChange;
  if Assigned(ShapeCustomOutlineColor) then
    ShapeCustomOutlineColor.OnMouseDown := HandleShapeColorMouseDown;

  // Tab: Advanced
  CheckLogEnabled.OnClick := HandleSettingChanged;
  EditLogFilename.OnChange := HandleSettingChanged;
  CheckLogAppend.OnClick := HandleSettingChanged;
  ComboLogLevel.OnChange := HandleSettingChanged;

  // Tab: Appearance
  CheckShowDeviceIcons.OnClick := HandleSettingChanged;
  CheckShowLastSeen.OnClick := HandleSettingChanged;
  RadioLastSeenRelative.OnClick := HandleSettingChanged;
  RadioLastSeenAbsolute.OnClick := HandleSettingChanged;
  CheckShowBatteryLevel.OnClick := HandleSettingChanged;
  ShapeConnectedColor.OnMouseDown := HandleShapeColorMouseDown;
  EditItemHeight.OnChange := HandleSettingChanged;
  EditItemPadding.OnChange := HandleSettingChanged;
  EditItemMargin.OnChange := HandleSettingChanged;
  EditIconSize.OnChange := HandleSettingChanged;
  EditCornerRadius.OnChange := HandleSettingChanged;
  EditBorderWidth.OnChange := HandleSettingChanged;
  ShapeBorderColor.OnMouseDown := HandleShapeColorMouseDown;
  EditDeviceNameSize.OnChange := HandleSettingChanged;
  EditStatusSize.OnChange := HandleSettingChanged;
  EditAddressSize.OnChange := HandleSettingChanged;
  EditIconFontSize.OnChange := HandleSettingChanged;

  // Profile settings
  CheckShowProfiles.OnClick := HandleSettingChanged;
  EditProfileFontSize.OnChange := HandleSettingChanged;
end;

procedure TFormSettings.ConfigureWinRTDependentControls;
const
  WINRT_UNAVAILABLE_SUFFIX = ' (Windows 8+)';
var
  I: Integer;
  ItemText: string;
begin
  // Configure controls that depend on WinRT availability
  if not IsWinRTAvailable then
  begin
    LogInfo('WinRT not available - disabling WinRT-dependent options', ClassName);

    // Enumeration mode: mark WinRT options as unavailable
    // TEnumerationMode: emClassic=0, emWinRT=1, emHybrid=2
    // WinRT and Hybrid modes require WinRT
    for I := 0 to ComboEnumerationMode.Items.Count - 1 do
    begin
      ItemText := ComboEnumerationMode.Items[I];
      // Mark WinRT-dependent options (indices 1 and 2)
      if (I >= 1) and (Pos(WINRT_UNAVAILABLE_SUFFIX, ItemText) = 0) then
        ComboEnumerationMode.Items[I] := ItemText + WINRT_UNAVAILABLE_SUFFIX;
    end;
    // Force Classic mode if WinRT mode was selected
    if ComboEnumerationMode.ItemIndex >= 1 then
      ComboEnumerationMode.ItemIndex := 0;

    // Outline color mode: Auto detection requires Windows 10 dark mode detection
    // TOutlineColorMode: ocmAuto=0, ocmLight=1, ocmDark=2, ocmCustom=3
    // Auto mode falls back safely but won't auto-detect dark mode
    if Assigned(ComboOutlineColorMode) and (ComboOutlineColorMode.Items.Count > 0) then
    begin
      ItemText := ComboOutlineColorMode.Items[0];
      if Pos(WINRT_UNAVAILABLE_SUFFIX, ItemText) = 0 then
        ComboOutlineColorMode.Items[0] := ItemText + WINRT_UNAVAILABLE_SUFFIX;
    end;
  end;

  // Dark mode detection requires Windows 10 version 1809+
  if not IsDarkModeSupported then
  begin
    LogDebug('Dark mode not supported - Auto outline color will use fallback', ClassName);
    // Note: Auto mode will still work, just won't detect dark mode
  end;
end;

{ Hotkey tab events }

procedure TFormSettings.ButtonRecordHotkeyClick(Sender: TObject);
var
  NewHotkey: string;
begin
  NewHotkey := TFormHotkeyPicker.PickHotkey(EditHotkey.Text);
  if NewHotkey <> '' then
  begin
    EditHotkey.Text := NewHotkey;
    FPresenter.MarkModified;
  end;
end;

procedure TFormSettings.ButtonClearHotkeyClick(Sender: TObject);
begin
  EditHotkey.Text := '';
  FPresenter.MarkModified;
end;

procedure TFormSettings.ButtonResetSizeClick(Sender: TObject);
begin
  FPresenter.OnResetSizeClicked;
end;

procedure TFormSettings.ButtonResetPositionClick(Sender: TObject);
begin
  FPresenter.OnResetPositionClicked;
end;

procedure TFormSettings.ButtonRecordCastHotkeyClick(Sender: TObject);
var
  NewHotkey: string;
begin
  NewHotkey := TFormHotkeyPicker.PickHotkey(EditCastPanelHotkey.Text);
  if NewHotkey <> '' then
  begin
    EditCastPanelHotkey.Text := NewHotkey;
    FPresenter.MarkModified;
  end;
end;

procedure TFormSettings.ButtonClearCastHotkeyClick(Sender: TObject);
begin
  EditCastPanelHotkey.Text := '';
  FPresenter.MarkModified;
end;

procedure TFormSettings.ButtonRecordBluetoothHotkeyClick(Sender: TObject);
var
  NewHotkey: string;
begin
  NewHotkey := TFormHotkeyPicker.PickHotkey(EditBluetoothPanelHotkey.Text);
  if NewHotkey <> '' then
  begin
    EditBluetoothPanelHotkey.Text := NewHotkey;
    FPresenter.MarkModified;
  end;
end;

procedure TFormSettings.ButtonClearBluetoothHotkeyClick(Sender: TObject);
begin
  EditBluetoothPanelHotkey.Text := '';
  FPresenter.MarkModified;
end;

{ Devices tab events }

procedure TFormSettings.ListDevicesClick(Sender: TObject);
begin
  FPresenter.OnDeviceSelected(ListDevices.ItemIndex);
end;

procedure TFormSettings.ButtonForgetDeviceClick(Sender: TObject);
begin
  if ListDevices.ItemIndex >= 0 then
  begin
    if MessageDlg('Are you sure you want to forget this device?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FPresenter.OnForgetDeviceClicked(ListDevices.ItemIndex);
    end;
  end;
end;

procedure TFormSettings.ButtonRefreshDevicesClick(Sender: TObject);
begin
  FPresenter.OnRefreshDevicesClicked;
end;

{ Theme events }

procedure TFormSettings.ButtonBrowseVsfDirClick(Sender: TObject);
var
  Dialog: TFileOpenDialog;
  InitialDir: string;
  ExePath: string;
begin
  Dialog := TFileOpenDialog.Create(Self);
  try
    Dialog.Title := 'Select Styles Directory';
    Dialog.Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];

    // Resolve current path (may be relative)
    ExePath := ExtractFilePath(ParamStr(0));
    InitialDir := EditVsfDir.Text;
    if InitialDir = '' then
      InitialDir := ExePath
    else if not TPath.IsPathRooted(InitialDir) then
      InitialDir := TPath.Combine(ExePath, InitialDir);

    if System.SysUtils.DirectoryExists(InitialDir) then
      Dialog.DefaultFolder := InitialDir;

    if Dialog.Execute then
      EditVsfDir.Text := Dialog.FileName;
  finally
    Dialog.Free;
  end;
end;

{ Advanced tab events }

procedure TFormSettings.ButtonBrowseLogFileClick(Sender: TObject);
begin
  SaveDialogLog.FileName := EditLogFilename.Text;
  if SaveDialogLog.Execute then
    EditLogFilename.Text := SaveDialogLog.FileName;
end;

procedure TFormSettings.ButtonOpenConfigClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(FAppConfig.ConfigPath), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormSettings.ButtonOpenLogFileClick(Sender: TObject);
var
  LogPath: string;
begin
  LogPath := FLogConfig.LogFilename;
  if ExtractFilePath(LogPath) = '' then
    LogPath := ExtractFilePath(ParamStr(0)) + LogPath;

  if FileExists(LogPath) then
    ShellExecute(0, 'open', PChar(LogPath), nil, nil, SW_SHOWNORMAL)
  else
    ShowError('Log file does not exist: ' + LogPath);
end;

procedure TFormSettings.ButtonResetDefaultsClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to reset all settings to defaults?' + sLineBreak +
    'This will delete your configuration file.',
    mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    FPresenter.OnResetDefaultsClicked;
  end;
end;

{ Appearance tab events }

procedure TFormSettings.HandleShapeColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Shape: TShape;
begin
  if Button <> mbLeft then
    Exit;
  if not (Sender is TShape) then
    Exit;
  Shape := TShape(Sender);

  // Special handling for background color shape: uncheck transparent when clicked
  if Shape = ShapeDefaultBackgroundColor then
  begin
    if CheckTransparentBackground.Checked then
    begin
      CheckTransparentBackground.Checked := False;
      Shape.Enabled := True;
    end;
  end;

  ColorDialogConnected.Color := Shape.Brush.Color;
  if ColorDialogConnected.Execute then
  begin
    Shape.Brush.Color := ColorDialogConnected.Color;
    FPresenter.MarkModified;
  end;
end;

procedure TFormSettings.HandleTransparentCheckboxClick(Sender: TObject);
begin
  // Enable/disable the background color shape based on checkbox state
  ShapeDefaultBackgroundColor.Enabled := not CheckTransparentBackground.Checked;
  if CheckTransparentBackground.Checked then
    ShapeDefaultBackgroundColor.Brush.Color := clWhite;
  FPresenter.MarkModified;
end;

procedure TFormSettings.ButtonResetLayoutClick(Sender: TObject);
begin
  FPresenter.OnResetLayoutClicked;
end;

procedure TFormSettings.ComboDeviceBatteryColorModeChange(Sender: TObject);
begin
  // Show color picker only when "Custom" is selected
  ShapeDeviceBatteryColor.Visible := (ComboDeviceBatteryColorMode.ItemIndex = 1);
  if ShapeDeviceBatteryColor.Visible and (ShapeDeviceBatteryColor.Brush.Color = clGreen) then
  begin
    // Open color dialog immediately when switching to Custom
    ColorDialogConnected.Color := clGreen;
    if ColorDialogConnected.Execute then
      ShapeDeviceBatteryColor.Brush.Color := ColorDialogConnected.Color;
  end;
  FPresenter.MarkModified;
end;

procedure TFormSettings.ComboDeviceBatteryBackgroundModeChange(Sender: TObject);
begin
  // Show color picker only when "Custom" is selected (index 1)
  ShapeDeviceBatteryBackground.Visible := (ComboDeviceBatteryBackgroundMode.ItemIndex = 1);
  if ShapeDeviceBatteryBackground.Visible and (ShapeDeviceBatteryBackground.Brush.Color = clWhite) then
  begin
    // Open color dialog immediately when switching to Custom
    ColorDialogConnected.Color := clWhite;
    if ColorDialogConnected.Execute then
      ShapeDeviceBatteryBackground.Brush.Color := ColorDialogConnected.Color;
  end;
  FPresenter.MarkModified;
end;

procedure TFormSettings.ComboOutlineColorModeChange(Sender: TObject);
var
  IsCustom: Boolean;
begin
  // Show custom color picker only when "Custom" is selected (index 3)
  IsCustom := ComboOutlineColorMode.ItemIndex = 3;
  if Assigned(LabelCustomOutlineColor) then
    LabelCustomOutlineColor.Visible := IsCustom;
  if Assigned(ShapeCustomOutlineColor) then
  begin
    ShapeCustomOutlineColor.Visible := IsCustom;
    // Open color dialog immediately when switching to Custom
    if IsCustom and (ShapeCustomOutlineColor.Brush.Color = clBlack) then
    begin
      ColorDialogConnected.Color := clBlack;
      if ColorDialogConnected.Execute then
        ShapeCustomOutlineColor.Brush.Color := ColorDialogConnected.Color;
    end;
  end;
  FPresenter.MarkModified;
end;

end.
