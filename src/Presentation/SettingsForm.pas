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
  App.SettingsPresenter;

type
  /// <summary>
  /// Settings dialog form.
  /// Implements ISettingsView for MVP pattern.
  /// </summary>
  TFormSettings = class(TForm, ISettingsView)
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonApply: TButton;
    PageControl: TPageControl;
    SaveDialogLog: TSaveDialog;

    { Tab: General }
    TabGeneral: TTabSheet;
    GroupWindowMode: TGroupBox;
    LabelWindowMode: TLabel;
    ComboWindowMode: TComboBox;
    TabHotkeyVisuals: TTabSheet;
    GroupHotkey: TGroupBox;
    LabelHotkey: TLabel;
    EditHotkey: TEdit;
    ButtonRecordHotkey: TButton;
    ButtonClearHotkey: TButton;
    CheckUseLowLevelHook: TCheckBox;

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
    ComboPollingMode: TComboBox;
    EditPollingInterval: TEdit;
    UpDownPollingInterval: TUpDown;

    { Tab: Devices }
    TabDevices: TTabSheet;
    PanelDeviceList: TPanel;
    ListDevices: TListBox;
    PanelDeviceButtons: TPanel;
    PanelDeviceSettings: TPanel;
    GroupDeviceInfo: TGroupBox;
    LabelDeviceAlias: TLabel;
    EditDeviceAlias: TEdit;

    { Tab: Advanced }
    TabAdvanced: TTabSheet;
    GroupLogging: TGroupBox;
    LabelLogFilename: TLabel;
    CheckLogEnabled: TCheckBox;
    EditLogFilename: TEdit;
    ButtonBrowseLogFile: TButton;
    CheckLogAppend: TCheckBox;
    GroupActions: TGroupBox;
    ButtonOpenConfig: TButton;
    ButtonResetDefaults: TButton;
    CheckAutostart: TCheckBox;
    GroupPosition: TGroupBox;
    LabelPositionMode: TLabel;
    ComboPositionMode: TComboBox;
    ButtonResetSize: TButton;
    ButtonResetPosition: TButton;
    GroupWindowOptions: TGroupBox;
    CheckMinimizeToTray: TCheckBox;
    CheckCloseToTray: TCheckBox;
    GroupMenuOptions: TGroupBox;
    CheckHideOnFocusLoss: TCheckBox;
    CheckOnTop: TCheckBox;
    GroupTheme: TGroupBox;
    LabelTheme: TLabel;
    LabelVsfDir: TLabel;
    ComboTheme: TComboBox;
    EditVsfDir: TEdit;
    ButtonBrowseVsfDir: TButton;
    GroupNotifications: TGroupBox;
    CheckNotifyOnConnect: TCheckBox;
    CheckNotifyOnDisconnect: TCheckBox;
    CheckNotifyOnConnectFailed: TCheckBox;
    CheckNotifyOnAutoConnect: TCheckBox;
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
    ButtonOpenLogFile: TButton;
    ButtonForgetDevice: TButton;
    ButtonRefreshDevices: TButton;
    CheckShowAddresses: TCheckBox;

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

  private
    FPresenter: TSettingsPresenter;
    FRecordingHotkey: Boolean;

    { ISettingsView implementation }
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);

    { UI helpers }
    procedure UpdateWindowModeControls;
    procedure HandleSettingChanged(Sender: TObject);
    procedure ConnectChangeHandlers;

  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  public
    { Public declarations }
    function GetOnSettingsApplied: TNotifyEvent;
    procedure SetOnSettingsApplied(AValue: TNotifyEvent);
    property OnSettingsApplied: TNotifyEvent read GetOnSettingsApplied write SetOnSettingsApplied;
  end;

var
  FormSettings: TFormSettings;

implementation

uses
  ShellAPI,
  Vcl.FileCtrl,
  App.Logger,
  App.Config;

{$R *.dfm}

{ TFormSettings }

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  Log('[SettingsForm] FormCreate');
  FPresenter := TSettingsPresenter.Create(Self);
  FRecordingHotkey := False;
  KeyPreview := True;
end;

procedure TFormSettings.FormDestroy(Sender: TObject);
begin
  Log('[SettingsForm] FormDestroy');
  FPresenter.Free;
end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Log('[SettingsForm] FormShow');
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

{ ISettingsView implementation }

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
  ComboTheme.OnChange := HandleSettingChanged;
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
  CheckDevicePinned.OnClick := HandleSettingChanged;
  CheckDeviceHidden.OnClick := HandleSettingChanged;
  CheckDeviceAutoConnect.OnClick := HandleSettingChanged;
  EditDeviceTimeout.OnChange := HandleSettingChanged;
  EditDeviceRetryCount.OnChange := HandleSettingChanged;
  ComboDeviceNotifyConnect.OnChange := HandleSettingChanged;
  ComboDeviceNotifyDisconnect.OnChange := HandleSettingChanged;
  ComboDeviceNotifyFailed.OnChange := HandleSettingChanged;
  ComboDeviceNotifyAuto.OnChange := HandleSettingChanged;
  // Note: ComboDeviceType change handler is set dynamically via presenter if control exists

  // Tab: Advanced
  CheckLogEnabled.OnClick := HandleSettingChanged;
  EditLogFilename.OnChange := HandleSettingChanged;
  CheckLogAppend.OnClick := HandleSettingChanged;
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
    // Build hotkey string from key and modifiers
    HotkeyStr := '';

    if ssCtrl in Shift then
      HotkeyStr := HotkeyStr + 'Ctrl+';
    if ssAlt in Shift then
      HotkeyStr := HotkeyStr + 'Alt+';
    if ssShift in Shift then
      HotkeyStr := HotkeyStr + 'Shift+';

    // Check for Win key - need to check GetAsyncKeyState
    if (GetAsyncKeyState(VK_LWIN) < 0) or (GetAsyncKeyState(VK_RWIN) < 0) then
      HotkeyStr := HotkeyStr + 'Win+';

    // Only accept if there's at least one modifier and a non-modifier key
    if (HotkeyStr <> '') and not (Key in [VK_CONTROL, VK_MENU, VK_SHIFT, VK_LWIN, VK_RWIN]) then
    begin
      // Convert key code to string
      case Key of
        VK_F1..VK_F12:
          HotkeyStr := HotkeyStr + 'F' + IntToStr(Key - VK_F1 + 1);
        VK_SPACE:
          HotkeyStr := HotkeyStr + 'Space';
        VK_RETURN:
          HotkeyStr := HotkeyStr + 'Enter';
        VK_ESCAPE:
          HotkeyStr := HotkeyStr + 'Escape';
        VK_TAB:
          HotkeyStr := HotkeyStr + 'Tab';
        VK_BACK:
          HotkeyStr := HotkeyStr + 'Backspace';
        VK_DELETE:
          HotkeyStr := HotkeyStr + 'Delete';
        VK_INSERT:
          HotkeyStr := HotkeyStr + 'Insert';
        VK_HOME:
          HotkeyStr := HotkeyStr + 'Home';
        VK_END:
          HotkeyStr := HotkeyStr + 'End';
        VK_PRIOR:
          HotkeyStr := HotkeyStr + 'PageUp';
        VK_NEXT:
          HotkeyStr := HotkeyStr + 'PageDown';
        VK_UP:
          HotkeyStr := HotkeyStr + 'Up';
        VK_DOWN:
          HotkeyStr := HotkeyStr + 'Down';
        VK_LEFT:
          HotkeyStr := HotkeyStr + 'Left';
        VK_RIGHT:
          HotkeyStr := HotkeyStr + 'Right';
        Ord('0')..Ord('9'):
          HotkeyStr := HotkeyStr + Chr(Key);
        Ord('A')..Ord('Z'):
          HotkeyStr := HotkeyStr + Chr(Key);
      else
        // Unknown key, cancel recording
        HotkeyStr := '';
      end;

      if HotkeyStr <> '' then
      begin
        EditHotkey.Text := HotkeyStr;
        FRecordingHotkey := False;
        ButtonRecordHotkey.Enabled := True;
        Key := 0; // Consume the key
      end;
    end
    else if Key = VK_ESCAPE then
    begin
      // Cancel recording
      FRecordingHotkey := False;
      ButtonRecordHotkey.Enabled := True;
      FPresenter.LoadSettings; // Restore original value
      Key := 0;
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
  ShellExecute(0, 'open', PChar(Config.ConfigPath), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormSettings.ButtonOpenLogFileClick(Sender: TObject);
var
  LogPath: string;
begin
  LogPath := Config.LogFilename;
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

end.
