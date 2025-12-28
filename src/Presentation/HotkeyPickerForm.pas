unit HotkeyPickerForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  /// <summary>
  /// Modal dialog for hotkey selection.
  /// Combines keyboard recording with manual selection via checkboxes and dropdown.
  /// </summary>
  TFormHotkeyPicker = class(TForm)
    PanelMain: TPanel;
    GroupModifiers: TGroupBox;
    CheckCtrl: TCheckBox;
    CheckAlt: TCheckBox;
    CheckShift: TCheckBox;
    CheckWin: TCheckBox;
    GroupKey: TGroupBox;
    ComboKey: TComboBox;
    LabelKey: TLabel;
    PanelResult: TPanel;
    LabelResultCaption: TLabel;
    LabelResult: TLabel;
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonClear: TButton;
    LabelHint: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckModifierClick(Sender: TObject);
    procedure ComboKeyChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);

  private
    FOriginalHotkey: string;
    FRecordingHook: HHOOK;

    procedure PopulateKeyList;
    procedure UpdateResultLabel;
    function BuildHotkeyFromControls: string;
    procedure ParseHotkeyToControls(const AHotkey: string);
    procedure InstallRecordingHook;
    procedure UninstallRecordingHook;
    procedure ProcessRecordedKey(AVirtualKey: Cardinal; AModifiers: Cardinal);

  protected
    procedure WMHotkeyRecorded(var Msg: TMessage); message WM_USER + 201;

  public
    /// <summary>
    /// Shows the hotkey picker dialog.
    /// </summary>
    /// <param name="ACurrentHotkey">Current hotkey string to pre-select.</param>
    /// <returns>Selected hotkey string, or empty if cancelled.</returns>
    class function PickHotkey(const ACurrentHotkey: string): string;
  end;

implementation

{$R *.dfm}

uses
  App.Logger;

const
  WM_HOTKEY_PICKER_RECORDED = WM_USER + 201;
  WH_KEYBOARD_LL = 13;
  LLKHF_UP = $80;

type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

  TKeyInfo = record
    Name: string;
    VK: Cardinal;
  end;

const
  /// <summary>List of supported keys for the dropdown.</summary>
  SupportedKeys: array[0..47] of TKeyInfo = (
    (Name: 'A'; VK: Ord('A')),
    (Name: 'B'; VK: Ord('B')),
    (Name: 'C'; VK: Ord('C')),
    (Name: 'D'; VK: Ord('D')),
    (Name: 'E'; VK: Ord('E')),
    (Name: 'F'; VK: Ord('F')),
    (Name: 'G'; VK: Ord('G')),
    (Name: 'H'; VK: Ord('H')),
    (Name: 'I'; VK: Ord('I')),
    (Name: 'J'; VK: Ord('J')),
    (Name: 'K'; VK: Ord('K')),
    (Name: 'L'; VK: Ord('L')),
    (Name: 'M'; VK: Ord('M')),
    (Name: 'N'; VK: Ord('N')),
    (Name: 'O'; VK: Ord('O')),
    (Name: 'P'; VK: Ord('P')),
    (Name: 'Q'; VK: Ord('Q')),
    (Name: 'R'; VK: Ord('R')),
    (Name: 'S'; VK: Ord('S')),
    (Name: 'T'; VK: Ord('T')),
    (Name: 'U'; VK: Ord('U')),
    (Name: 'V'; VK: Ord('V')),
    (Name: 'W'; VK: Ord('W')),
    (Name: 'X'; VK: Ord('X')),
    (Name: 'Y'; VK: Ord('Y')),
    (Name: 'Z'; VK: Ord('Z')),
    (Name: '0'; VK: Ord('0')),
    (Name: '1'; VK: Ord('1')),
    (Name: '2'; VK: Ord('2')),
    (Name: '3'; VK: Ord('3')),
    (Name: '4'; VK: Ord('4')),
    (Name: '5'; VK: Ord('5')),
    (Name: '6'; VK: Ord('6')),
    (Name: '7'; VK: Ord('7')),
    (Name: '8'; VK: Ord('8')),
    (Name: '9'; VK: Ord('9')),
    (Name: 'F1'; VK: VK_F1),
    (Name: 'F2'; VK: VK_F2),
    (Name: 'F3'; VK: VK_F3),
    (Name: 'F4'; VK: VK_F4),
    (Name: 'F5'; VK: VK_F5),
    (Name: 'F6'; VK: VK_F6),
    (Name: 'F7'; VK: VK_F7),
    (Name: 'F8'; VK: VK_F8),
    (Name: 'F9'; VK: VK_F9),
    (Name: 'F10'; VK: VK_F10),
    (Name: 'F11'; VK: VK_F11),
    (Name: 'F12'; VK: VK_F12)
  );

  /// <summary>Additional special keys.</summary>
  SpecialKeys: array[0..14] of TKeyInfo = (
    (Name: 'Space'; VK: VK_SPACE),
    (Name: 'Enter'; VK: VK_RETURN),
    (Name: 'Tab'; VK: VK_TAB),
    (Name: 'Escape'; VK: VK_ESCAPE),
    (Name: 'Backspace'; VK: VK_BACK),
    (Name: 'Delete'; VK: VK_DELETE),
    (Name: 'Insert'; VK: VK_INSERT),
    (Name: 'Home'; VK: VK_HOME),
    (Name: 'End'; VK: VK_END),
    (Name: 'PageUp'; VK: VK_PRIOR),
    (Name: 'PageDown'; VK: VK_NEXT),
    (Name: 'Up'; VK: VK_UP),
    (Name: 'Down'; VK: VK_DOWN),
    (Name: 'Left'; VK: VK_LEFT),
    (Name: 'Right'; VK: VK_RIGHT)
  );

var
  GPickerHook: HHOOK = 0;
  GPickerFormHandle: HWND = 0;
  GPickerModifiers: Cardinal = 0;

function PickerKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KbdStruct: PKBDLLHOOKSTRUCT;
  VK: Cardinal;
  IsKeyDown: Boolean;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(GPickerHook, nCode, wParam, lParam);
    Exit;
  end;

  KbdStruct := PKBDLLHOOKSTRUCT(lParam);
  VK := KbdStruct^.vkCode;
  IsKeyDown := (KbdStruct^.flags and LLKHF_UP) = 0;

  // Track modifier key states
  case VK of
    VK_LWIN, VK_RWIN:
      if IsKeyDown then
        GPickerModifiers := GPickerModifiers or MOD_WIN
      else
        GPickerModifiers := GPickerModifiers and not MOD_WIN;
    VK_LCONTROL, VK_RCONTROL:
      if IsKeyDown then
        GPickerModifiers := GPickerModifiers or MOD_CONTROL
      else
        GPickerModifiers := GPickerModifiers and not MOD_CONTROL;
    VK_LMENU, VK_RMENU:
      if IsKeyDown then
        GPickerModifiers := GPickerModifiers or MOD_ALT
      else
        GPickerModifiers := GPickerModifiers and not MOD_ALT;
    VK_LSHIFT, VK_RSHIFT:
      if IsKeyDown then
        GPickerModifiers := GPickerModifiers or MOD_SHIFT
      else
        GPickerModifiers := GPickerModifiers and not MOD_SHIFT;
  else
    // Non-modifier key pressed with at least one modifier
    if IsKeyDown and (GPickerModifiers <> 0) then
    begin
      // Post message to form: WParam = VK, LParam = Modifiers
      PostMessage(GPickerFormHandle, WM_HOTKEY_PICKER_RECORDED, VK, GPickerModifiers);
      // Block the key
      Result := 1;
      Exit;
    end;
  end;

  // Block all keys while picker is active to prevent system actions
  if IsKeyDown then
    Result := 1
  else
    Result := CallNextHookEx(GPickerHook, nCode, wParam, lParam);
end;

{ TFormHotkeyPicker }

class function TFormHotkeyPicker.PickHotkey(const ACurrentHotkey: string): string;
var
  Form: TFormHotkeyPicker;
begin
  Result := '';
  Form := TFormHotkeyPicker.Create(nil);
  try
    Form.FOriginalHotkey := ACurrentHotkey;
    Form.ParseHotkeyToControls(ACurrentHotkey);
    if Form.ShowModal = mrOk then
      Result := Form.BuildHotkeyFromControls;
  finally
    Form.Free;
  end;
end;

procedure TFormHotkeyPicker.FormCreate(Sender: TObject);
begin
  FRecordingHook := 0;
  KeyPreview := True;
  PopulateKeyList;
end;

procedure TFormHotkeyPicker.FormDestroy(Sender: TObject);
begin
  UninstallRecordingHook;
end;

procedure TFormHotkeyPicker.FormShow(Sender: TObject);
begin
  UpdateResultLabel;
  InstallRecordingHook;
end;

procedure TFormHotkeyPicker.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Handle Escape to cancel
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end
  // Handle Enter to confirm
  else if Key = VK_RETURN then
  begin
    if BuildHotkeyFromControls <> '' then
      ModalResult := mrOk
    else
      ModalResult := mrCancel;
    Key := 0;
  end;
end;

procedure TFormHotkeyPicker.PopulateKeyList;
var
  I: Integer;
begin
  ComboKey.Items.Clear;
  ComboKey.Items.Add(''); // Empty option

  // Add letter and number keys
  for I := Low(SupportedKeys) to High(SupportedKeys) do
    ComboKey.Items.AddObject(SupportedKeys[I].Name, TObject(SupportedKeys[I].VK));

  // Add special keys
  for I := Low(SpecialKeys) to High(SpecialKeys) do
    ComboKey.Items.AddObject(SpecialKeys[I].Name, TObject(SpecialKeys[I].VK));

  ComboKey.ItemIndex := 0;
end;

procedure TFormHotkeyPicker.UpdateResultLabel;
var
  Hotkey: string;
begin
  Hotkey := BuildHotkeyFromControls;
  if Hotkey = '' then
    LabelResult.Caption := '(none)'
  else
    LabelResult.Caption := Hotkey;

  // Enable OK button only if valid hotkey is selected
  ButtonOK.Enabled := Hotkey <> '';
end;

function TFormHotkeyPicker.BuildHotkeyFromControls: string;
begin
  Result := '';

  // Need at least one modifier
  if not (CheckCtrl.Checked or CheckAlt.Checked or CheckShift.Checked or CheckWin.Checked) then
    Exit;

  // Need a key selected
  if (ComboKey.ItemIndex <= 0) or (ComboKey.Text = '') then
    Exit;

  // Build the hotkey string
  if CheckCtrl.Checked then
    Result := Result + 'Ctrl+';
  if CheckAlt.Checked then
    Result := Result + 'Alt+';
  if CheckShift.Checked then
    Result := Result + 'Shift+';
  if CheckWin.Checked then
    Result := Result + 'Win+';

  Result := Result + ComboKey.Text;
end;

procedure TFormHotkeyPicker.ParseHotkeyToControls(const AHotkey: string);
var
  Parts: TArray<string>;
  Part, KeyPart: string;
  I: Integer;
begin
  // Reset all controls
  CheckCtrl.Checked := False;
  CheckAlt.Checked := False;
  CheckShift.Checked := False;
  CheckWin.Checked := False;
  ComboKey.ItemIndex := 0;

  if Trim(AHotkey) = '' then
    Exit;

  // Split by '+' and process
  Parts := AHotkey.Split(['+']);
  if Length(Parts) = 0 then
    Exit;

  // Last part is the key
  KeyPart := Trim(Parts[High(Parts)]);

  // Process modifiers
  for I := 0 to High(Parts) - 1 do
  begin
    Part := Trim(Parts[I]).ToUpper;
    if (Part = 'CTRL') or (Part = 'CONTROL') then
      CheckCtrl.Checked := True
    else if Part = 'ALT' then
      CheckAlt.Checked := True
    else if Part = 'SHIFT' then
      CheckShift.Checked := True
    else if (Part = 'WIN') or (Part = 'WINDOWS') then
      CheckWin.Checked := True;
  end;

  // Find the key in the combo box (case-insensitive)
  for I := 0 to ComboKey.Items.Count - 1 do
  begin
    if SameText(ComboKey.Items[I], KeyPart) then
    begin
      ComboKey.ItemIndex := I;
      Break;
    end;
  end;
end;

procedure TFormHotkeyPicker.InstallRecordingHook;
begin
  if GPickerHook = 0 then
  begin
    GPickerFormHandle := Handle;
    GPickerModifiers := 0;
    GPickerHook := SetWindowsHookEx(WH_KEYBOARD_LL, @PickerKeyboardProc, HInstance, 0);
    if GPickerHook <> 0 then
      LogDebug('Hotkey picker hook installed', ClassName)
    else
      LogWarning('Failed to install hotkey picker hook', ClassName);
  end;
end;

procedure TFormHotkeyPicker.UninstallRecordingHook;
begin
  if GPickerHook <> 0 then
  begin
    UnhookWindowsHookEx(GPickerHook);
    GPickerHook := 0;
    GPickerFormHandle := 0;
    GPickerModifiers := 0;
    LogDebug('Hotkey picker hook uninstalled', ClassName);
  end;
end;

procedure TFormHotkeyPicker.ProcessRecordedKey(AVirtualKey: Cardinal; AModifiers: Cardinal);
var
  KeyName: string;
  I: Integer;
begin
  // Update modifier checkboxes
  CheckCtrl.Checked := (AModifiers and MOD_CONTROL) <> 0;
  CheckAlt.Checked := (AModifiers and MOD_ALT) <> 0;
  CheckShift.Checked := (AModifiers and MOD_SHIFT) <> 0;
  CheckWin.Checked := (AModifiers and MOD_WIN) <> 0;

  // Find key name from virtual key code
  KeyName := '';

  // Check supported keys
  for I := Low(SupportedKeys) to High(SupportedKeys) do
  begin
    if SupportedKeys[I].VK = AVirtualKey then
    begin
      KeyName := SupportedKeys[I].Name;
      Break;
    end;
  end;

  // Check special keys if not found
  if KeyName = '' then
  begin
    for I := Low(SpecialKeys) to High(SpecialKeys) do
    begin
      if SpecialKeys[I].VK = AVirtualKey then
      begin
        KeyName := SpecialKeys[I].Name;
        Break;
      end;
    end;
  end;

  // Select key in combo box
  if KeyName <> '' then
  begin
    for I := 0 to ComboKey.Items.Count - 1 do
    begin
      if SameText(ComboKey.Items[I], KeyName) then
      begin
        ComboKey.ItemIndex := I;
        Break;
      end;
    end;
  end;

  UpdateResultLabel;
end;

procedure TFormHotkeyPicker.WMHotkeyRecorded(var Msg: TMessage);
begin
  ProcessRecordedKey(Msg.WParam, Msg.LParam);
end;

procedure TFormHotkeyPicker.CheckModifierClick(Sender: TObject);
begin
  UpdateResultLabel;
end;

procedure TFormHotkeyPicker.ComboKeyChange(Sender: TObject);
begin
  UpdateResultLabel;
end;

procedure TFormHotkeyPicker.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormHotkeyPicker.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormHotkeyPicker.ButtonClearClick(Sender: TObject);
begin
  CheckCtrl.Checked := False;
  CheckAlt.Checked := False;
  CheckShift.Checked := False;
  CheckWin.Checked := False;
  ComboKey.ItemIndex := 0;
  UpdateResultLabel;
end;

end.
