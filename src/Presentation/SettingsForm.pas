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
  App.ConfigInterfaces,
  App.SettingsPresenter,
  UI.Theme;

type
  /// <summary>
  /// Settings dialog form.
  /// Implements focused settings interfaces for ISP compliance.
  /// Also implements combined ISettingsView for backward compatibility.
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
    ISettingsView)
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

    { Tab: Keys }
    TabKeys: TTabSheet;
    GroupHotkey: TGroupBox;
    LabelHotkey: TLabel;
    EditHotkey: TEdit;
    ButtonRecordHotkey: TButton;
    ButtonClearHotkey: TButton;
    CheckUseLowLevelHook: TCheckBox;

    { Tab: Themes }
    TabThemes: TTabSheet;
    GroupTheme: TGroupBox;
    LabelTheme: TLabel;
    LabelVsfDir: TLabel;
    ComboTheme: TComboBox;
    CheckPreviewTheme: TCheckBox;
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
    LabelDeviceAlias: TLabel;
    EditDeviceAlias: TEdit;
    LabelDeviceType: TLabel;
    ComboDeviceType: TComboBox;
    CheckDevicePinned: TCheckBox;
    CheckDeviceHidden: TCheckBox;
    CheckDeviceAutoConnect: TCheckBox;
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

  private
    FPresenter: TSettingsPresenter;
    FRecordingHotkey: Boolean;
    FOriginalTheme: string;

    { Injected dependencies }
    FAppConfig: IAppConfig;
    FLogConfig: ILogConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
    FThemeManager: IThemeManager;

    { ISettingsView implementation - Dialog control }
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);

    { ISettingsView implementation - Settings access }
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);

    { ISettingsView implementation - Theme management }
    procedure PopulateThemeList(const ACurrentTheme: string);

    { ISettingsView implementation - Device management }
    procedure PopulateDeviceList(const AItems: TArray<string>);
    function GetSelectedDeviceIndex: Integer;
    procedure SetSelectedDeviceIndex(AIndex: Integer);
    function GetDeviceSettings: TDeviceViewSettings;
    procedure SetDeviceSettings(const ASettings: TDeviceViewSettings);
    procedure ClearDeviceSettings;

    { UI helpers }
    procedure InitUpDownLimits;
    procedure InitDeviceTypeCombo;
    procedure UpdateWindowModeControls;
    procedure HandleSettingChanged(Sender: TObject);
    procedure HandleThemeComboChange(Sender: TObject);
    procedure ConnectChangeHandlers;

  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  public
    { Public declarations }
    procedure Setup(
      AAppConfig: IAppConfig;
      ALogConfig: ILogConfig;
      ADeviceConfigProvider: IDeviceConfigProvider;
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
  FRecordingHotkey := False;
  KeyPreview := True;
end;

procedure TFormSettings.Setup(
  AAppConfig: IAppConfig;
  ALogConfig: ILogConfig;
  ADeviceConfigProvider: IDeviceConfigProvider;
  AThemeManager: IThemeManager
);
begin
  FAppConfig := AAppConfig;
  FLogConfig := ALogConfig;
  FDeviceConfigProvider := ADeviceConfigProvider;
  FThemeManager := AThemeManager;

  // Create presenter with injected dependencies
  FPresenter := TSettingsPresenter.Create(
    Self as ISettingsView,
    Self as IDeviceSettingsView,
    FAppConfig,
    FDeviceConfigProvider
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
  FPresenter.LoadSettings;
  UpdateWindowModeControls;
  ConnectChangeHandlers;
  ButtonApply.Enabled := False;
  // Store original theme for possible revert on cancel
  FOriginalTheme := FThemeManager.CurrentStyleName;
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

{ ISettingsView implementation }

procedure TFormSettings.CloseWithOK;
begin
  ModalResult := mrOk;
end;

procedure TFormSettings.CloseWithCancel;
begin
  // Restore original theme if preview was used
  if FThemeManager.CurrentStyleName <> FOriginalTheme then
    FThemeManager.SetStyle(FOriginalTheme);
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

{ ISettingsView implementation - Settings access }

function TFormSettings.GetGeneralSettings: TGeneralViewSettings;
begin
  Result.WindowMode := TWindowMode(ComboWindowMode.ItemIndex);
  Result.OnTop := CheckOnTop.Checked;
  Result.MinimizeToTray := CheckMinimizeToTray.Checked;
  Result.CloseToTray := CheckCloseToTray.Checked;
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
  CheckHideOnFocusLoss.Checked := ASettings.HideOnFocusLoss;
  CheckAutostart.Checked := ASettings.Autostart;
  ComboPositionMode.ItemIndex := Ord(ASettings.PositionMode);
  UpdateWindowModeControls;
end;

function TFormSettings.GetHotkeySettings: THotkeyViewSettings;
begin
  Result.Hotkey := EditHotkey.Text;
  Result.UseLowLevelHook := CheckUseLowLevelHook.Checked;
end;

procedure TFormSettings.SetHotkeySettings(const ASettings: THotkeyViewSettings);
begin
  EditHotkey.Text := ASettings.Hotkey;
  CheckUseLowLevelHook.Checked := ASettings.UseLowLevelHook;
end;

function TFormSettings.GetAppearanceSettings: TAppearanceViewSettings;
begin
  Result.Theme := ComboTheme.Text;
  Result.VsfDir := EditVsfDir.Text;
  Result.ShowAddresses := CheckShowAddresses.Checked;
  Result.ShowDeviceIcons := CheckShowDeviceIcons.Checked;
  Result.ShowLastSeen := CheckShowLastSeen.Checked;
  Result.LastSeenRelative := RadioLastSeenRelative.Checked;
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

{ ISettingsView implementation - Theme management }

procedure TFormSettings.PopulateThemeList(const ACurrentTheme: string);
var
  Styles: TArray<string>;
  StyleName: string;
  Idx: Integer;
begin
  // Get available styles from theme manager (injected dependency)
  Styles := FThemeManager.GetAvailableStyles;
  TArray.Sort<string>(Styles);

  ComboTheme.Items.Clear;
  for StyleName in Styles do
    ComboTheme.Items.Add(StyleName);

  // Select current theme
  Idx := ComboTheme.Items.IndexOf(ACurrentTheme);
  if Idx >= 0 then
    ComboTheme.ItemIndex := Idx
  else if ComboTheme.Items.Count > 0 then
    ComboTheme.ItemIndex := 0;
end;

{ ISettingsView implementation - Device management }

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
  ComboDeviceType.Items.Add('Auto-detect');
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
  IsWindowMode: Boolean;
begin
  // 0 = Window mode, 1 = Menu mode
  IsWindowMode := ComboWindowMode.ItemIndex = 0;

  // Apply bold style to the active mode's group caption
  if IsWindowMode then
  begin
    GroupWindowOptions.Font.Style := [fsBold];
    GroupMenuOptions.Font.Style := [];
  end
  else
  begin
    GroupWindowOptions.Font.Style := [];
    GroupMenuOptions.Font.Style := [fsBold];
  end;
end;

procedure TFormSettings.HandleSettingChanged(Sender: TObject);
begin
  FPresenter.MarkModified;
  // Special handling for window mode combo - also update UI
  if Sender = ComboWindowMode then
    UpdateWindowModeControls;
end;

procedure TFormSettings.HandleThemeComboChange(Sender: TObject);
begin
  FPresenter.MarkModified;
  // Apply theme immediately if preview is enabled
  if CheckPreviewTheme.Checked and (ComboTheme.Text <> '') then
    FThemeManager.SetStyle(ComboTheme.Text);
end;

procedure TFormSettings.ConnectChangeHandlers;
begin
  // Tab: General
  ComboWindowMode.OnChange := HandleSettingChanged;
  CheckOnTop.OnClick := HandleSettingChanged;
  CheckMinimizeToTray.OnClick := HandleSettingChanged;
  CheckCloseToTray.OnClick := HandleSettingChanged;
  CheckHideOnFocusLoss.OnClick := HandleSettingChanged;
  CheckAutostart.OnClick := HandleSettingChanged;
  ComboPositionMode.OnChange := HandleSettingChanged;

  // Tab: Hotkey & Visuals
  EditHotkey.OnChange := HandleSettingChanged;
  CheckUseLowLevelHook.OnClick := HandleSettingChanged;
  ComboTheme.OnChange := HandleThemeComboChange;
  EditVsfDir.OnChange := HandleSettingChanged;
  CheckShowAddresses.OnClick := HandleSettingChanged;

  // Tab: Connection
  EditTimeout.OnChange := HandleSettingChanged;
  EditRetryCount.OnChange := HandleSettingChanged;
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
end;

{ Hotkey tab events }

procedure TFormSettings.ButtonRecordHotkeyClick(Sender: TObject);
begin
  FRecordingHotkey := True;
  EditHotkey.Text := 'Press hotkey combination...';
  EditHotkey.SetFocus;
  ButtonRecordHotkey.Enabled := False;
end;

procedure TFormSettings.KeyDown(var Key: Word; Shift: TShiftState);
var
  HotkeyStr: string;
begin
  inherited;

  if FRecordingHotkey then
  begin
    // Handle Escape to cancel recording
    if Key = VK_ESCAPE then
    begin
      FRecordingHotkey := False;
      ButtonRecordHotkey.Enabled := True;
      FPresenter.LoadSettings; // Restore original value
      Key := 0;
      Exit;
    end;

    // Use THotkeyManager to build the hotkey string
    HotkeyStr := THotkeyManager.BuildHotkeyString(Key, Shift);

    if HotkeyStr <> '' then
    begin
      EditHotkey.Text := HotkeyStr;
      FRecordingHotkey := False;
      ButtonRecordHotkey.Enabled := True;
      Key := 0; // Consume the key
    end;
  end;
end;

procedure TFormSettings.ButtonClearHotkeyClick(Sender: TObject);
begin
  EditHotkey.Text := '';
  FRecordingHotkey := False;
  ButtonRecordHotkey.Enabled := True;
end;

procedure TFormSettings.ButtonResetSizeClick(Sender: TObject);
begin
  FPresenter.OnResetSizeClicked;
end;

procedure TFormSettings.ButtonResetPositionClick(Sender: TObject);
begin
  FPresenter.OnResetPositionClicked;
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
  ColorDialogConnected.Color := Shape.Brush.Color;
  if ColorDialogConnected.Execute then
  begin
    Shape.Brush.Color := ColorDialogConnected.Color;
    FPresenter.MarkModified;
  end;
end;

procedure TFormSettings.ButtonResetLayoutClick(Sender: TObject);
begin
  FPresenter.OnResetLayoutClicked;
end;

end.
