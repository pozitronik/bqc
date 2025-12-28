object FormHotkeyPicker: TFormHotkeyPicker
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Hotkey'
  ClientHeight = 260
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 219
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object LabelHint: TLabel
      Left = 12
      Top = 8
      Width = 296
      Height = 30
      AutoSize = False
      Caption = 'Press a hotkey combination, or select modifiers and key below:'
      WordWrap = True
    end
    object GroupModifiers: TGroupBox
      Left = 12
      Top = 42
      Width = 145
      Height = 90
      Caption = 'Modifiers'
      TabOrder = 0
      object CheckCtrl: TCheckBox
        Left = 12
        Top = 20
        Width = 60
        Height = 17
        Caption = 'Ctrl'
        TabOrder = 0
        OnClick = CheckModifierClick
      end
      object CheckAlt: TCheckBox
        Left = 76
        Top = 20
        Width = 60
        Height = 17
        Caption = 'Alt'
        TabOrder = 1
        OnClick = CheckModifierClick
      end
      object CheckShift: TCheckBox
        Left = 12
        Top = 44
        Width = 60
        Height = 17
        Caption = 'Shift'
        TabOrder = 2
        OnClick = CheckModifierClick
      end
      object CheckWin: TCheckBox
        Left = 76
        Top = 44
        Width = 60
        Height = 17
        Caption = 'Win'
        TabOrder = 3
        OnClick = CheckModifierClick
      end
    end
    object GroupKey: TGroupBox
      Left = 163
      Top = 42
      Width = 145
      Height = 90
      Caption = 'Key'
      TabOrder = 1
      object LabelKey: TLabel
        Left = 12
        Top = 20
        Width = 20
        Height = 15
        Caption = 'Key:'
      end
      object ComboKey: TComboBox
        Left = 12
        Top = 40
        Width = 121
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = ComboKeyChange
      end
    end
    object PanelResult: TPanel
      Left = 12
      Top = 140
      Width = 296
      Height = 65
      BevelKind = bkFlat
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 2
      object LabelResultCaption: TLabel
        Left = 8
        Top = 8
        Width = 81
        Height = 15
        Caption = 'Current hotkey:'
      end
      object LabelResult: TLabel
        Left = 8
        Top = 30
        Width = 280
        Height = 25
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        Caption = '(none)'
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 219
    Width = 320
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOK: TButton
      Left = 156
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 237
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
    object ButtonClear: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 2
      OnClick = ButtonClearClick
    end
  end
end
