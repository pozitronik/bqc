object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Bluetooth Quick Connect'
  ClientHeight = 400
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 16
      Width = 116
      Height = 21
      Caption = 'Paired Devices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnRefresh: TButton
      Left = 408
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
  end
  object lvDevices: TListView
    Left = 0
    Top = 50
    Width = 500
    Height = 325
    Align = alClient
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvDevicesDblClick
    OnSelectItem = lvDevicesSelectItem
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 375
    Width = 500
    Height = 25
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 2
    object lblStatus: TLabel
      Left = 8
      Top = 5
      Width = 40
      Height = 15
      Caption = 'Ready.'
    end
  end
end
