object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 400
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 359
    Width = 500
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonOK: TButton
      Left = 232
      Top = 8
      Width = 80
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 318
      Top = 8
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
    object ButtonApply: TButton
      Left = 404
      Top = 8
      Width = 80
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = ButtonApplyClick
    end
  end
  object PanelContent: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 359
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object LabelPlaceholder: TLabel
      Left = 0
      Top = 0
      Width = 500
      Height = 359
      Align = alClient
      Alignment = taCenter
      Caption = 'Settings content will be added here'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
  end
end
