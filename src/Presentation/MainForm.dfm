object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Bluetooth Quick Connect'
  ClientHeight = 361
  ClientWidth = 284
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object HeaderPanel: TPanel
    Left = 0
    Top = 0
    Width = 284
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Right = 10
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 380
    object TitleLabel: TLabel
      Left = 10
      Top = 0
      Width = 98
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
      OnClick = TitleLabelClick
      ExplicitHeight = 30
    end
    object BluetoothToggle: TToggleSwitch
      Left = 201
      Top = 0
      Width = 73
      Height = 33
      Align = alRight
      Color = clBtnFace
      StyleName = 'Windows'
      TabOrder = 0
      OnClick = HandleBluetoothToggle
      ExplicitLeft = 297
      ExplicitHeight = 20
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 331
    Width = 284
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Right = 10
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 470
    ExplicitWidth = 380
    object StatusLabel: TLabel
      Left = 242
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
      OnClick = StatusLabelClick
      ExplicitLeft = 338
      ExplicitHeight = 15
    end
    object SettingsLink: TLabel
      Left = 10
      Top = 0
      Width = 148
      Height = 30
      Cursor = crHandPoint
      Align = alLeft
      Caption = 'Windows Bluetooth settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsUnderline]
      ParentFont = False
      Layout = tlCenter
      OnClick = HandleSettingsClick
      ExplicitHeight = 15
    end
  end
  object DevicesPanel: TPanel
    Left = 0
    Top = 33
    Width = 284
    Height = 298
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 380
    ExplicitHeight = 437
  end
end
