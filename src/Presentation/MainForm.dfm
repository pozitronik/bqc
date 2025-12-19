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
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Right = 10
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 374
    object TitleLabel: TLabel
      Left = 10
      Top = 0
      Width = 113
      Height = 33
      Align = alLeft
      Caption = 'Bluetooth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 0
      ExplicitHeight = 41
    end
    object BluetoothToggle: TToggleSwitch
      Left = 297
      Top = 0
      Width = 73
      Height = 33
      Align = alRight
      Color = clBtnFace
      StyleName = 'Windows'
      TabOrder = 0
      OnClick = HandleBluetoothToggle
      ExplicitLeft = 291
      ExplicitHeight = 20
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 470
    Width = 380
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Right = 10
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 453
    ExplicitWidth = 374
    object StatusLabel: TLabel
      Left = 338
      Top = 0
      Width = 32
      Height = 30
      Align = alRight
      Caption = 'Ready'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 16
      ExplicitTop = 8
      ExplicitHeight = 15
    end
    object SettingsLink: TLabel
      Left = 10
      Top = 0
      Width = 127
      Height = 30
      Cursor = crHandPoint
      Align = alLeft
      Caption = 'More Bluetooth settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsUnderline]
      ParentFont = False
      Layout = tlCenter
      OnClick = HandleSettingsClick
      ExplicitLeft = 16
      ExplicitTop = 15
      ExplicitHeight = 15
    end
  end
  object DevicesPanel: TPanel
    Left = 0
    Top = 33
    Width = 380
    Height = 437
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 160
    ExplicitTop = 144
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
end
