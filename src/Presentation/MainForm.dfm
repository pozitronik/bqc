object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Bluetooth Quick Connect'
  ClientHeight = 500
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object HeaderPanel: TPanel
    Left = 0
    Top = 0
    Width = 380
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object TitleLabel: TLabel
      Left = 16
      Top = 16
      Width = 94
      Height = 28
      Caption = 'Bluetooth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BluetoothToggle: TToggleSwitch
      Left = 310
      Top = 16
      Width = 50
      Height = 20
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = HandleBluetoothToggle
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 470
    Width = 380
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object StatusLabel: TLabel
      Left = 16
      Top = 8
      Width = 32
      Height = 15
      Caption = 'Ready'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object SettingsLink: TLabel
    Left = 16
    Top = 448
    Width = 129
    Height = 15
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'More Bluetooth settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = HandleSettingsClick
  end
end
