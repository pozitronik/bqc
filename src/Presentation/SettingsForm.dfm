object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 450
  ClientWidth = 550
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
    Top = 409
    Width = 550
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonOK: TButton
      Left = 282
      Top = 8
      Width = 80
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 368
      Top = 8
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
    object ButtonApply: TButton
      Left = 454
      Top = 8
      Width = 80
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = ButtonApplyClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 550
    Height = 409
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 1
    object TabGeneral: TTabSheet
      Caption = 'General'
      object GroupWindowMode: TGroupBox
        Left = 12
        Top = 12
        Width = 250
        Height = 105
        Caption = 'Window Mode'
        TabOrder = 0
        object RadioWindowMode: TRadioButton
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'Window mode (normal window)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = RadioWindowModeClick
        end
        object RadioMenuMode: TRadioButton
          Left = 16
          Top = 52
          Width = 200
          Height = 17
          Caption = 'Menu mode (popup)'
          TabOrder = 1
          OnClick = RadioWindowModeClick
        end
        object CheckOnTop: TCheckBox
          Left = 16
          Top = 76
          Width = 200
          Height = 17
          Caption = 'Always on top'
          TabOrder = 2
        end
      end
      object GroupWindowOptions: TGroupBox
        Left = 12
        Top = 123
        Width = 250
        Height = 85
        Caption = 'Window Mode Options'
        TabOrder = 1
        object CheckMinimizeToTray: TCheckBox
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'Minimize to tray'
          TabOrder = 0
        end
        object CheckCloseToTray: TCheckBox
          Left = 16
          Top = 52
          Width = 200
          Height = 17
          Caption = 'Close to tray'
          TabOrder = 1
        end
      end
      object GroupMenuOptions: TGroupBox
        Left = 12
        Top = 214
        Width = 250
        Height = 60
        Caption = 'Menu Mode Options'
        TabOrder = 2
        object CheckHideOnFocusLoss: TCheckBox
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'Hide on focus loss'
          TabOrder = 0
        end
      end
      object GroupStartup: TGroupBox
        Left = 274
        Top = 12
        Width = 250
        Height = 60
        Caption = 'Startup'
        TabOrder = 3
        object CheckAutostart: TCheckBox
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'Start with Windows'
          TabOrder = 0
        end
      end
    end
    object TabHotkey: TTabSheet
      Caption = 'Hotkey && Position'
      ImageIndex = 1
      object GroupHotkey: TGroupBox
        Left = 12
        Top = 12
        Width = 510
        Height = 120
        Caption = 'Global Hotkey'
        TabOrder = 0
        object LabelHotkey: TLabel
          Left = 16
          Top = 32
          Width = 41
          Height = 15
          Caption = 'Hotkey:'
        end
        object LabelHotkeyHint: TLabel
          Left = 16
          Top = 92
          Width = 272
          Height = 15
          Caption = 'Low-level hook required to override system hotkeys'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object EditHotkey: TEdit
          Left = 80
          Top = 28
          Width = 200
          Height = 23
          ReadOnly = True
          TabOrder = 0
        end
        object ButtonRecordHotkey: TButton
          Left = 286
          Top = 27
          Width = 80
          Height = 25
          Caption = 'Record...'
          TabOrder = 1
          OnClick = ButtonRecordHotkeyClick
        end
        object ButtonClearHotkey: TButton
          Left = 372
          Top = 27
          Width = 80
          Height = 25
          Caption = 'Clear'
          TabOrder = 2
          OnClick = ButtonClearHotkeyClick
        end
        object CheckUseLowLevelHook: TCheckBox
          Left = 16
          Top = 64
          Width = 200
          Height = 17
          Caption = 'Use low-level keyboard hook'
          TabOrder = 3
        end
      end
      object GroupPosition: TGroupBox
        Left = 12
        Top = 144
        Width = 510
        Height = 100
        Caption = 'Window Position'
        TabOrder = 1
        object LabelPositionMode: TLabel
          Left = 16
          Top = 32
          Width = 80
          Height = 15
          Caption = 'Position mode:'
        end
        object ComboPositionMode: TComboBox
          Left = 110
          Top = 28
          Width = 200
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Use saved coordinates'
            'Near system tray'
            'Near mouse cursor'
            'Center of screen')
        end
        object ButtonResetSize: TButton
          Left = 16
          Top = 64
          Width = 150
          Height = 25
          Caption = 'Reset size to default'
          TabOrder = 1
          OnClick = ButtonResetSizeClick
        end
        object ButtonResetPosition: TButton
          Left = 172
          Top = 64
          Width = 150
          Height = 25
          Caption = 'Reset position to default'
          TabOrder = 2
          OnClick = ButtonResetPositionClick
        end
      end
    end
    object TabAppearance: TTabSheet
      Caption = 'Appearance'
      ImageIndex = 2
      object GroupTheme: TGroupBox
        Left = 12
        Top = 12
        Width = 510
        Height = 100
        Caption = 'Theme'
        TabOrder = 0
        object LabelTheme: TLabel
          Left = 16
          Top = 32
          Width = 40
          Height = 15
          Caption = 'Theme:'
        end
        object ComboTheme: TComboBox
          Left = 80
          Top = 28
          Width = 200
          Height = 23
          Style = csDropDownList
          TabOrder = 0
        end
        object CheckShowAddresses: TCheckBox
          Left = 16
          Top = 64
          Width = 250
          Height = 17
          Caption = 'Show device MAC addresses'
          TabOrder = 1
        end
      end
    end
    object TabConnection: TTabSheet
      Caption = 'Connection'
      ImageIndex = 3
      object GroupConnectionDefaults: TGroupBox
        Left = 12
        Top = 12
        Width = 250
        Height = 100
        Caption = 'Default Connection Settings'
        TabOrder = 0
        object LabelTimeout: TLabel
          Left = 16
          Top = 32
          Width = 48
          Height = 15
          Caption = 'Timeout:'
        end
        object LabelTimeoutMs: TLabel
          Left = 200
          Top = 32
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object LabelRetryCount: TLabel
          Left = 16
          Top = 64
          Width = 80
          Height = 15
          Caption = 'Retry attempts:'
        end
        object EditTimeout: TEdit
          Left = 110
          Top = 28
          Width = 75
          Height = 23
          TabOrder = 0
          Text = '0'
        end
        object UpDownTimeout: TUpDown
          Left = 185
          Top = 28
          Width = 16
          Height = 23
          Associate = EditTimeout
          Max = 60000
          Increment = 1000
          TabOrder = 1
        end
        object EditRetryCount: TEdit
          Left = 110
          Top = 60
          Width = 50
          Height = 23
          TabOrder = 2
          Text = '0'
        end
        object UpDownRetryCount: TUpDown
          Left = 160
          Top = 60
          Width = 16
          Height = 23
          Associate = EditRetryCount
          Max = 10
          TabOrder = 3
        end
      end
      object GroupNotifications: TGroupBox
        Left = 274
        Top = 12
        Width = 250
        Height = 140
        Caption = 'Notifications'
        TabOrder = 1
        object CheckNotifyOnConnect: TCheckBox
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'On connect'
          TabOrder = 0
        end
        object CheckNotifyOnDisconnect: TCheckBox
          Left = 16
          Top = 52
          Width = 200
          Height = 17
          Caption = 'On disconnect'
          TabOrder = 1
        end
        object CheckNotifyOnConnectFailed: TCheckBox
          Left = 16
          Top = 76
          Width = 200
          Height = 17
          Caption = 'On connection failed'
          TabOrder = 2
        end
        object CheckNotifyOnAutoConnect: TCheckBox
          Left = 16
          Top = 100
          Width = 200
          Height = 17
          Caption = 'On auto-connect'
          TabOrder = 3
        end
      end
      object GroupPolling: TGroupBox
        Left = 12
        Top = 124
        Width = 250
        Height = 120
        Caption = 'Device Monitoring'
        TabOrder = 2
        object LabelPollingMode: TLabel
          Left = 16
          Top = 32
          Width = 74
          Height = 15
          Caption = 'Polling mode:'
        end
        object LabelPollingInterval: TLabel
          Left = 16
          Top = 64
          Width = 42
          Height = 15
          Caption = 'Interval:'
        end
        object LabelPollingIntervalMs: TLabel
          Left = 200
          Top = 64
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object ComboPollingMode: TComboBox
          Left = 110
          Top = 28
          Width = 130
          Height = 23
          Hint = 'Requires app restart'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Items.Strings = (
            'Disabled'
            'Fallback'
            'Primary')
        end
        object EditPollingInterval: TEdit
          Left = 110
          Top = 60
          Width = 75
          Height = 23
          TabOrder = 1
          Text = '0'
        end
        object UpDownPollingInterval: TUpDown
          Left = 185
          Top = 60
          Width = 16
          Height = 23
          Associate = EditPollingInterval
          Max = 10000
          Increment = 500
          TabOrder = 2
        end
      end
    end
    object TabDevices: TTabSheet
      Caption = 'Devices'
      ImageIndex = 4
      object PanelDeviceList: TPanel
        Left = 0
        Top = 0
        Width = 200
        Height = 379
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ListDevices: TListBox
          Left = 0
          Top = 0
          Width = 200
          Height = 347
          Align = alClient
          ItemHeight = 15
          TabOrder = 0
          OnClick = ListDevicesClick
        end
        object PanelDeviceButtons: TPanel
          Left = 0
          Top = 347
          Width = 200
          Height = 32
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonForgetDevice: TButton
            Left = 4
            Top = 4
            Width = 90
            Height = 25
            Caption = 'Forget'
            TabOrder = 0
            OnClick = ButtonForgetDeviceClick
          end
          object ButtonRefreshDevices: TButton
            Left = 100
            Top = 4
            Width = 90
            Height = 25
            Caption = 'Refresh'
            TabOrder = 1
            OnClick = ButtonRefreshDevicesClick
          end
        end
      end
      object PanelDeviceSettings: TPanel
        Left = 200
        Top = 0
        Width = 342
        Height = 379
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object GroupDeviceInfo: TGroupBox
          Left = 12
          Top = 8
          Width = 318
          Height = 100
          Caption = 'Device Settings'
          TabOrder = 0
          object LabelDeviceAlias: TLabel
            Left = 16
            Top = 32
            Width = 28
            Height = 15
            Caption = 'Alias:'
          end
          object LabelDeviceAddress: TLabel
            Left = 16
            Top = 64
            Width = 45
            Height = 15
            Caption = 'Address:'
          end
          object LabelDeviceAddressValue: TLabel
            Left = 80
            Top = 64
            Width = 87
            Height = 15
            Caption = '00:00:00:00:00:00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object EditDeviceAlias: TEdit
            Left = 80
            Top = 28
            Width = 220
            Height = 23
            TabOrder = 0
          end
        end
        object GroupDeviceOptions: TGroupBox
          Left = 12
          Top = 114
          Width = 318
          Height = 90
          Caption = 'Options'
          TabOrder = 1
          object CheckDevicePinned: TCheckBox
            Left = 16
            Top = 28
            Width = 150
            Height = 17
            Caption = 'Pin to top of list'
            TabOrder = 0
          end
          object CheckDeviceHidden: TCheckBox
            Left = 16
            Top = 52
            Width = 150
            Height = 17
            Caption = 'Hide from list'
            TabOrder = 1
          end
          object CheckDeviceAutoConnect: TCheckBox
            Left = 170
            Top = 28
            Width = 140
            Height = 17
            Caption = 'Auto-connect on start'
            TabOrder = 2
          end
        end
        object GroupDeviceConnection: TGroupBox
          Left = 12
          Top = 210
          Width = 318
          Height = 80
          Caption = 'Connection (use -1 for default)'
          TabOrder = 2
          object LabelDeviceTimeout: TLabel
            Left = 16
            Top = 28
            Width = 48
            Height = 15
            Caption = 'Timeout:'
          end
          object LabelDeviceRetry: TLabel
            Left = 16
            Top = 52
            Width = 30
            Height = 15
            Caption = 'Retry:'
          end
          object EditDeviceTimeout: TEdit
            Left = 80
            Top = 24
            Width = 75
            Height = 23
            TabOrder = 0
          end
          object EditDeviceRetryCount: TEdit
            Left = 80
            Top = 48
            Width = 50
            Height = 23
            TabOrder = 1
          end
        end
        object GroupDeviceNotifications: TGroupBox
          Left = 12
          Top = 296
          Width = 318
          Height = 75
          Caption = 'Notification Overrides'
          TabOrder = 3
          object LabelDeviceNotifyConnect: TLabel
            Left = 16
            Top = 24
            Width = 48
            Height = 15
            Caption = 'Connect:'
          end
          object LabelDeviceNotifyDisconnect: TLabel
            Left = 16
            Top = 48
            Width = 62
            Height = 15
            Caption = 'Disconnect:'
          end
          object LabelDeviceNotifyFailed: TLabel
            Left = 170
            Top = 24
            Width = 34
            Height = 15
            Caption = 'Failed:'
          end
          object LabelDeviceNotifyAuto: TLabel
            Left = 170
            Top = 48
            Width = 29
            Height = 15
            Caption = 'Auto:'
          end
          object ComboDeviceNotifyConnect: TComboBox
            Left = 85
            Top = 20
            Width = 75
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            Items.Strings = (
              'Default'
              'None'
              'Balloon')
          end
          object ComboDeviceNotifyDisconnect: TComboBox
            Left = 85
            Top = 44
            Width = 75
            Height = 23
            Style = csDropDownList
            TabOrder = 1
            Items.Strings = (
              'Default'
              'None'
              'Balloon')
          end
          object ComboDeviceNotifyFailed: TComboBox
            Left = 220
            Top = 20
            Width = 75
            Height = 23
            Style = csDropDownList
            TabOrder = 2
            Items.Strings = (
              'Default'
              'None'
              'Balloon')
          end
          object ComboDeviceNotifyAuto: TComboBox
            Left = 220
            Top = 44
            Width = 75
            Height = 23
            Style = csDropDownList
            TabOrder = 3
            Items.Strings = (
              'Default'
              'None'
              'Balloon')
          end
        end
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 5
      object GroupLogging: TGroupBox
        Left = 12
        Top = 12
        Width = 510
        Height = 130
        Caption = 'Logging'
        TabOrder = 0
        object LabelLogFilename: TLabel
          Left = 16
          Top = 60
          Width = 42
          Height = 15
          Caption = 'Log file:'
        end
        object CheckLogEnabled: TCheckBox
          Left = 16
          Top = 28
          Width = 200
          Height = 17
          Caption = 'Enable logging'
          TabOrder = 0
        end
        object EditLogFilename: TEdit
          Left = 80
          Top = 56
          Width = 340
          Height = 23
          TabOrder = 1
        end
        object ButtonBrowseLogFile: TButton
          Left = 426
          Top = 55
          Width = 75
          Height = 25
          Caption = 'Browse...'
          TabOrder = 2
          OnClick = ButtonBrowseLogFileClick
        end
        object CheckLogAppend: TCheckBox
          Left = 16
          Top = 92
          Width = 200
          Height = 17
          Caption = 'Append to existing log'
          TabOrder = 3
        end
      end
      object GroupActions: TGroupBox
        Left = 12
        Top = 154
        Width = 510
        Height = 100
        Caption = 'Actions'
        TabOrder = 1
        object ButtonOpenConfig: TButton
          Left = 16
          Top = 28
          Width = 150
          Height = 25
          Caption = 'Open config file'
          TabOrder = 0
          OnClick = ButtonOpenConfigClick
        end
        object ButtonOpenLogFile: TButton
          Left = 172
          Top = 28
          Width = 150
          Height = 25
          Caption = 'Open log file'
          TabOrder = 1
          OnClick = ButtonOpenLogFileClick
        end
        object ButtonResetDefaults: TButton
          Left = 16
          Top = 60
          Width = 150
          Height = 25
          Caption = 'Reset to defaults'
          TabOrder = 2
          OnClick = ButtonResetDefaultsClick
        end
      end
    end
  end
  object SaveDialogLog: TSaveDialog
    DefaultExt = 'log'
    Filter = 'Log files (*.log)|*.log|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Select Log File'
    Left = 480
    Top = 368
  end
end
