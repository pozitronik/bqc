object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 410
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 369
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
    Height = 369
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 1
    object TabGeneral: TTabSheet
      Caption = 'Window'
      object GroupWindowMode: TGroupBox
        Left = 0
        Top = 0
        Width = 534
        Height = 165
        Align = alTop
        Caption = 'How to display?'
        TabOrder = 0
        object LabelWindowMode: TLabel
          Left = 12
          Top = 24
          Width = 34
          Height = 15
          Caption = 'Mode:'
        end
        object ComboWindowMode: TComboBox
          Left = 74
          Top = 24
          Width = 245
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          OnChange = ComboWindowModeChange
          Items.Strings = (
            'Window (normal window)'
            'Menu (popup)')
        end
        object GroupWindowOptions: TGroupBox
          Left = 12
          Top = 57
          Width = 518
          Height = 45
          Caption = 'When it'#39's window:'
          TabOrder = 1
          object CheckMinimizeToTray: TCheckBox
            Left = 12
            Top = 17
            Width = 120
            Height = 17
            Caption = 'Minimize to tray'
            TabOrder = 0
          end
          object CheckCloseToTray: TCheckBox
            Left = 144
            Top = 17
            Width = 120
            Height = 17
            Caption = 'Close to tray'
            TabOrder = 1
          end
        end
        object GroupMenuOptions: TGroupBox
          Left = 12
          Top = 106
          Width = 518
          Height = 45
          Caption = 'When it'#39's menu:'
          TabOrder = 2
          object CheckHideOnFocusLoss: TCheckBox
            Left = 12
            Top = 17
            Width = 125
            Height = 17
            Caption = 'Hide on focus loss'
            TabOrder = 0
          end
        end
      end
      object GroupPosition: TGroupBox
        Left = 0
        Top = 165
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Positioning:'
        TabOrder = 1
        object LabelPositionMode: TLabel
          Left = 12
          Top = 25
          Width = 46
          Height = 15
          Caption = 'Position:'
        end
        object ComboPositionMode: TComboBox
          Left = 74
          Top = 24
          Width = 250
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
          Left = 12
          Top = 56
          Width = 150
          Height = 25
          Caption = 'Reset size to default'
          TabOrder = 1
          OnClick = ButtonResetSizeClick
        end
        object ButtonResetPosition: TButton
          Left = 174
          Top = 56
          Width = 150
          Height = 25
          Caption = 'Reset position to default'
          TabOrder = 2
          OnClick = ButtonResetPositionClick
        end
        object CheckOnTop: TCheckBox
          Left = 336
          Top = 24
          Width = 105
          Height = 17
          Caption = 'Always on top'
          TabOrder = 3
        end
      end
      object CheckAutostart: TCheckBox
        Left = 0
        Top = 273
        Width = 136
        Height = 21
        Caption = 'Start with Windows'
        TabOrder = 2
      end
      object CheckShowAddresses: TCheckBox
        Left = 156
        Top = 275
        Width = 181
        Height = 17
        Caption = 'Show device MAC addresses'
        TabOrder = 3
      end
    end
    object TabAppearance: TTabSheet
      Caption = 'Device List'
      ImageIndex = 1
      object GroupDisplayOptions: TGroupBox
        Left = 0
        Top = 0
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Display Options'
        TabOrder = 0
        object LabelConnectedColor: TLabel
          Left = 270
          Top = 24
          Width = 91
          Height = 15
          Caption = 'Connected color:'
        end
        object ShapeConnectedColor: TShape
          Left = 358
          Top = 22
          Width = 50
          Height = 21
          Cursor = crHandPoint
          OnMouseDown = ShapeConnectedColorMouseDown
        end
        object CheckShowDeviceIcons: TCheckBox
          Left = 12
          Top = 24
          Width = 150
          Height = 17
          Caption = 'Show device icons'
          TabOrder = 0
        end
        object CheckShowLastSeen: TCheckBox
          Left = 12
          Top = 47
          Width = 150
          Height = 17
          Caption = 'Show last seen time'
          TabOrder = 1
        end
        object RadioLastSeenRelative: TRadioButton
          Left = 32
          Top = 70
          Width = 110
          Height = 17
          Caption = 'Relative format'
          TabOrder = 2
        end
        object RadioLastSeenAbsolute: TRadioButton
          Left = 150
          Top = 70
          Width = 110
          Height = 17
          Caption = 'Absolute format'
          TabOrder = 3
        end
      end
      object GroupLayout: TGroupBox
        Left = 0
        Top = 95
        Width = 534
        Height = 120
        Align = alTop
        Caption = 'Layout'
        TabOrder = 1
        object LabelItemHeight: TLabel
          Left = 12
          Top = 24
          Width = 64
          Height = 15
          Caption = 'Item height:'
        end
        object LabelItemPadding: TLabel
          Left = 12
          Top = 53
          Width = 74
          Height = 15
          Caption = 'Item padding:'
        end
        object LabelItemMargin: TLabel
          Left = 12
          Top = 82
          Width = 68
          Height = 15
          Caption = 'Item margin:'
        end
        object LabelIconSize: TLabel
          Left = 190
          Top = 24
          Width = 48
          Height = 15
          Caption = 'Icon size:'
        end
        object LabelCornerRadius: TLabel
          Left = 190
          Top = 53
          Width = 74
          Height = 15
          Caption = 'Corner radius:'
        end
        object LabelPx1: TLabel
          Left = 163
          Top = 24
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelPx2: TLabel
          Left = 163
          Top = 53
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelPx3: TLabel
          Left = 163
          Top = 82
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelPx4: TLabel
          Left = 341
          Top = 24
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelPx5: TLabel
          Left = 341
          Top = 53
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object EditItemHeight: TEdit
          Left = 100
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 0
          Text = '70'
        end
        object UpDownItemHeight: TUpDown
          Left = 145
          Top = 21
          Width = 16
          Height = 23
          Associate = EditItemHeight
          Min = 30
          Max = 200
          Position = 70
          TabOrder = 1
        end
        object EditItemPadding: TEdit
          Left = 100
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 2
          Text = '12'
        end
        object UpDownItemPadding: TUpDown
          Left = 145
          Top = 50
          Width = 16
          Height = 23
          Associate = EditItemPadding
          Max = 50
          Position = 12
          TabOrder = 3
        end
        object EditItemMargin: TEdit
          Left = 100
          Top = 79
          Width = 45
          Height = 23
          TabOrder = 4
          Text = '4'
        end
        object UpDownItemMargin: TUpDown
          Left = 145
          Top = 79
          Width = 16
          Height = 23
          Associate = EditItemMargin
          Max = 30
          Position = 4
          TabOrder = 5
        end
        object EditIconSize: TEdit
          Left = 278
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 6
          Text = '32'
        end
        object UpDownIconSize: TUpDown
          Left = 323
          Top = 21
          Width = 16
          Height = 23
          Associate = EditIconSize
          Min = 16
          Max = 64
          Position = 32
          TabOrder = 7
        end
        object EditCornerRadius: TEdit
          Left = 278
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 8
          Text = '8'
        end
        object UpDownCornerRadius: TUpDown
          Left = 323
          Top = 50
          Width = 16
          Height = 23
          Associate = EditCornerRadius
          Max = 30
          Position = 8
          TabOrder = 9
        end
        object ButtonResetLayout: TButton
          Left = 368
          Top = 82
          Width = 150
          Height = 25
          Caption = 'Reset layout to defaults'
          TabOrder = 10
          OnClick = ButtonResetLayoutClick
        end
      end
      object GroupFontSizes: TGroupBox
        Left = 0
        Top = 215
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Font Sizes (pt)'
        TabOrder = 2
        object LabelDeviceNameSize: TLabel
          Left = 12
          Top = 24
          Width = 71
          Height = 15
          Caption = 'Device name:'
        end
        object LabelStatusSize: TLabel
          Left = 12
          Top = 53
          Width = 57
          Height = 15
          Caption = 'Status text:'
        end
        object LabelAddressSize: TLabel
          Left = 190
          Top = 24
          Width = 67
          Height = 15
          Caption = 'Address text:'
        end
        object LabelIconFontSize: TLabel
          Left = 190
          Top = 53
          Width = 51
          Height = 15
          Caption = 'Icon font:'
        end
        object EditDeviceNameSize: TEdit
          Left = 100
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 0
          Text = '11'
        end
        object UpDownDeviceNameSize: TUpDown
          Left = 145
          Top = 21
          Width = 16
          Height = 23
          Associate = EditDeviceNameSize
          Min = 6
          Max = 24
          Position = 11
          TabOrder = 1
        end
        object EditStatusSize: TEdit
          Left = 100
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 2
          Text = '9'
        end
        object UpDownStatusSize: TUpDown
          Left = 145
          Top = 50
          Width = 16
          Height = 23
          Associate = EditStatusSize
          Min = 6
          Max = 24
          Position = 9
          TabOrder = 3
        end
        object EditAddressSize: TEdit
          Left = 278
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 4
          Text = '8'
        end
        object UpDownAddressSize: TUpDown
          Left = 323
          Top = 21
          Width = 16
          Height = 23
          Associate = EditAddressSize
          Min = 6
          Max = 24
          Position = 8
          TabOrder = 5
        end
        object EditIconFontSize: TEdit
          Left = 278
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 6
          Text = '16'
        end
        object UpDownIconFontSize: TUpDown
          Left = 323
          Top = 50
          Width = 16
          Height = 23
          Associate = EditIconFontSize
          Min = 8
          Max = 32
          Position = 16
          TabOrder = 7
        end
      end
    end
    object TabKeys: TTabSheet
      Caption = 'Keys'
      ImageIndex = 2
      object GroupHotkey: TGroupBox
        Left = 0
        Top = 0
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Global Hotkey'
        TabOrder = 0
        object LabelHotkey: TLabel
          Left = 12
          Top = 24
          Width = 41
          Height = 15
          Caption = 'Hotkey:'
        end
        object EditHotkey: TEdit
          Left = 74
          Top = 24
          Width = 250
          Height = 23
          ReadOnly = True
          TabOrder = 0
        end
        object ButtonRecordHotkey: TButton
          Left = 335
          Top = 24
          Width = 80
          Height = 23
          Caption = 'Record...'
          TabOrder = 1
          OnClick = ButtonRecordHotkeyClick
        end
        object ButtonClearHotkey: TButton
          Left = 427
          Top = 24
          Width = 80
          Height = 23
          Caption = 'Clear'
          TabOrder = 2
          OnClick = ButtonClearHotkeyClick
        end
        object CheckUseLowLevelHook: TCheckBox
          Left = 12
          Top = 58
          Width = 381
          Height = 17
          Caption = 'Use low-level keyboard hook (require to override system hotkeys)'
          TabOrder = 3
        end
      end
    end
    object TabThemes: TTabSheet
      Caption = 'Themes'
      ImageIndex = 3
      object GroupTheme: TGroupBox
        Left = 0
        Top = 0
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Theme'
        TabOrder = 0
        object LabelTheme: TLabel
          Left = 12
          Top = 24
          Width = 40
          Height = 15
          Caption = 'Theme:'
        end
        object LabelVsfDir: TLabel
          Left = 12
          Top = 59
          Width = 62
          Height = 15
          Caption = 'Themes dir:'
        end
        object ComboTheme: TComboBox
          Left = 74
          Top = 24
          Width = 250
          Height = 23
          Style = csDropDownList
          TabOrder = 0
        end
        object EditVsfDir: TEdit
          Left = 94
          Top = 56
          Width = 346
          Height = 23
          TabOrder = 1
        end
        object ButtonBrowseVsfDir: TButton
          Left = 446
          Top = 56
          Width = 80
          Height = 23
          Caption = 'Browse...'
          TabOrder = 2
          OnClick = ButtonBrowseVsfDirClick
        end
      end
    end
    object TabConnection: TTabSheet
      Caption = 'Connection'
      ImageIndex = 4
      object GroupConnectionDefaults: TGroupBox
        Left = 0
        Top = 0
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Default Connection Settings'
        TabOrder = 0
        object LabelTimeout: TLabel
          Left = 12
          Top = 24
          Width = 48
          Height = 15
          Caption = 'Timeout:'
        end
        object LabelTimeoutMs: TLabel
          Left = 206
          Top = 24
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object LabelRetryCount: TLabel
          Left = 12
          Top = 58
          Width = 80
          Height = 15
          Caption = 'Retry attempts:'
        end
        object EditTimeout: TEdit
          Left = 110
          Top = 24
          Width = 75
          Height = 23
          TabOrder = 0
          Text = '0'
        end
        object UpDownTimeout: TUpDown
          Left = 185
          Top = 24
          Width = 16
          Height = 23
          Associate = EditTimeout
          Max = 60000
          Increment = 1000
          TabOrder = 1
        end
        object EditRetryCount: TEdit
          Left = 110
          Top = 58
          Width = 75
          Height = 23
          TabOrder = 2
          Text = '0'
        end
        object UpDownRetryCount: TUpDown
          Left = 185
          Top = 58
          Width = 16
          Height = 23
          Associate = EditRetryCount
          Max = 10
          TabOrder = 3
        end
      end
      object GroupPolling: TGroupBox
        Left = 0
        Top = 95
        Width = 534
        Height = 95
        Align = alTop
        Caption = 'Device Monitoring'
        TabOrder = 1
        object LabelPollingMode: TLabel
          Left = 12
          Top = 24
          Width = 74
          Height = 15
          Caption = 'Polling mode:'
        end
        object LabelPollingInterval: TLabel
          Left = 12
          Top = 57
          Width = 42
          Height = 15
          Caption = 'Interval:'
        end
        object LabelPollingIntervalMs: TLabel
          Left = 206
          Top = 57
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object ComboPollingMode: TComboBox
          Left = 110
          Top = 24
          Width = 125
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
          Top = 57
          Width = 75
          Height = 23
          TabOrder = 1
          Text = '0'
        end
        object UpDownPollingInterval: TUpDown
          Left = 185
          Top = 57
          Width = 16
          Height = 23
          Associate = EditPollingInterval
          Max = 10000
          Increment = 500
          TabOrder = 2
        end
      end
      object GroupNotifications: TGroupBox
        Left = 0
        Top = 190
        Width = 534
        Height = 123
        Align = alTop
        Caption = 'Notifications'
        TabOrder = 2
        object CheckNotifyOnConnect: TCheckBox
          Left = 12
          Top = 24
          Width = 200
          Height = 17
          Caption = 'On connect'
          TabOrder = 0
        end
        object CheckNotifyOnDisconnect: TCheckBox
          Left = 12
          Top = 49
          Width = 200
          Height = 17
          Caption = 'On disconnect'
          TabOrder = 1
        end
        object CheckNotifyOnConnectFailed: TCheckBox
          Left = 12
          Top = 72
          Width = 200
          Height = 17
          Caption = 'On connection failed'
          TabOrder = 2
        end
        object CheckNotifyOnAutoConnect: TCheckBox
          Left = 12
          Top = 96
          Width = 200
          Height = 17
          Caption = 'On auto-connect'
          TabOrder = 3
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
        Height = 326
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ListDevices: TListBox
          Left = 0
          Top = 0
          Width = 200
          Height = 294
          Align = alClient
          ItemHeight = 15
          TabOrder = 0
          OnClick = ListDevicesClick
        end
        object PanelDeviceButtons: TPanel
          Left = 0
          Top = 294
          Width = 200
          Height = 32
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonForgetDevice: TButton
            Left = 0
            Top = 4
            Width = 96
            Height = 23
            Caption = 'Forget'
            TabOrder = 0
            OnClick = ButtonForgetDeviceClick
          end
          object ButtonRefreshDevices: TButton
            Left = 104
            Top = 4
            Width = 96
            Height = 23
            Caption = 'Refresh'
            TabOrder = 1
            OnClick = ButtonRefreshDevicesClick
          end
        end
      end
      object PanelDeviceSettings: TPanel
        Left = 200
        Top = 0
        Width = 334
        Height = 326
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object GroupDeviceInfo: TGroupBox
          Left = 0
          Top = 0
          Width = 334
          Height = 334
          Align = alTop
          Caption = 'Device Settings'
          TabOrder = 0
          object LabelDeviceAlias: TLabel
            Left = 12
            Top = 24
            Width = 28
            Height = 15
            Caption = 'Alias:'
          end
          object LabelDeviceType: TLabel
            Left = 12
            Top = 58
            Width = 28
            Height = 15
            Caption = 'Type:'
          end
          object EditDeviceAlias: TEdit
            Left = 74
            Top = 26
            Width = 250
            Height = 23
            TabOrder = 0
          end
          object CheckDeviceHidden: TCheckBox
            Left = 160
            Top = 86
            Width = 140
            Height = 17
            Caption = 'Hide from list'
            TabOrder = 2
          end
          object CheckDevicePinned: TCheckBox
            Left = 12
            Top = 86
            Width = 140
            Height = 17
            Caption = 'Pin to top of list'
            TabOrder = 1
          end
          object CheckDeviceAutoConnect: TCheckBox
            Left = 12
            Top = 109
            Width = 140
            Height = 17
            Caption = 'Auto-connect on start'
            TabOrder = 3
          end
          object GroupDeviceConnection: TGroupBox
            Left = 12
            Top = 133
            Width = 308
            Height = 95
            Caption = 'Connection (overrides global parameters)'
            TabOrder = 4
            object LabelDeviceTimeout: TLabel
              Left = 12
              Top = 25
              Width = 48
              Height = 15
              Caption = 'Timeout:'
            end
            object LabelDeviceTimeoutMs: TLabel
              Left = 206
              Top = 25
              Width = 16
              Height = 15
              Caption = 'ms'
            end
            object LabelDeviceRetry: TLabel
              Left = 12
              Top = 59
              Width = 80
              Height = 15
              Caption = 'Retry attempts:'
            end
            object EditDeviceTimeout: TEdit
              Left = 110
              Top = 25
              Width = 75
              Height = 23
              TabOrder = 0
              Text = '-1'
            end
            object UpDownDeviceTimeout: TUpDown
              Left = 185
              Top = 25
              Width = 16
              Height = 23
              Associate = EditDeviceTimeout
              Min = -1
              Max = 60000
              Increment = 1000
              Position = -1
              TabOrder = 1
            end
            object EditDeviceRetryCount: TEdit
              Left = 110
              Top = 59
              Width = 75
              Height = 23
              TabOrder = 2
              Text = '-1'
            end
            object UpDownDeviceRetryCount: TUpDown
              Left = 185
              Top = 59
              Width = 16
              Height = 23
              Associate = EditDeviceRetryCount
              Min = -1
              Max = 10
              Position = -1
              TabOrder = 3
            end
          end
          object GroupDeviceNotifications: TGroupBox
            Left = 12
            Top = 231
            Width = 308
            Height = 93
            Caption = 'Notifications (overrides global parameters)'
            TabOrder = 5
            object LabelDeviceNotifyConnect: TLabel
              Left = 12
              Top = 24
              Width = 48
              Height = 15
              Caption = 'Connect:'
            end
            object LabelDeviceNotifyDisconnect: TLabel
              Left = 12
              Top = 60
              Width = 62
              Height = 15
              Caption = 'Disconnect:'
            end
            object LabelDeviceNotifyFailed: TLabel
              Left = 153
              Top = 24
              Width = 34
              Height = 15
              Caption = 'Failed:'
            end
            object LabelDeviceNotifyAuto: TLabel
              Left = 153
              Top = 60
              Width = 77
              Height = 15
              Caption = 'Auto-connect:'
            end
            object ComboDeviceNotifyConnect: TComboBox
              Left = 80
              Top = 24
              Width = 60
              Height = 23
              Style = csDropDownList
              TabOrder = 0
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyDisconnect: TComboBox
              Left = 80
              Top = 60
              Width = 60
              Height = 23
              Style = csDropDownList
              TabOrder = 1
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyFailed: TComboBox
              Left = 239
              Top = 24
              Width = 60
              Height = 23
              Style = csDropDownList
              TabOrder = 2
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyAuto: TComboBox
              Left = 239
              Top = 60
              Width = 60
              Height = 23
              Style = csDropDownList
              TabOrder = 3
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
          end
          object ComboDeviceType: TComboBox
            Left = 74
            Top = 55
            Width = 246
            Height = 23
            Style = csDropDownList
            TabOrder = 6
          end
        end
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Diagnostics'
      ImageIndex = 6
      object GroupLogging: TGroupBox
        Left = 0
        Top = 0
        Width = 540
        Height = 95
        Caption = 'Logging'
        TabOrder = 0
        object LabelLogFilename: TLabel
          Left = 12
          Top = 57
          Width = 42
          Height = 15
          Caption = 'Log file:'
        end
        object CheckLogEnabled: TCheckBox
          Left = 12
          Top = 24
          Width = 112
          Height = 17
          Caption = 'Enable logging'
          TabOrder = 0
        end
        object CheckLogAppend: TCheckBox
          Left = 149
          Top = 24
          Width = 200
          Height = 17
          Caption = 'Append to existing log'
          TabOrder = 1
        end
        object EditLogFilename: TEdit
          Left = 70
          Top = 57
          Width = 288
          Height = 23
          TabOrder = 2
        end
        object ButtonBrowseLogFile: TButton
          Left = 364
          Top = 57
          Width = 80
          Height = 23
          Caption = 'Browse...'
          TabOrder = 3
          OnClick = ButtonBrowseLogFileClick
        end
        object ButtonOpenLogFile: TButton
          Left = 450
          Top = 57
          Width = 80
          Height = 23
          Caption = 'Open log file'
          TabOrder = 4
          OnClick = ButtonOpenLogFileClick
        end
      end
      object GroupActions: TGroupBox
        Left = 0
        Top = 101
        Width = 540
        Height = 59
        Caption = 'Actions'
        TabOrder = 1
        object ButtonOpenConfig: TButton
          Left = 12
          Top = 24
          Width = 150
          Height = 23
          Caption = 'Open config file'
          TabOrder = 0
          OnClick = ButtonOpenConfigClick
        end
        object ButtonResetDefaults: TButton
          Left = 168
          Top = 24
          Width = 150
          Height = 23
          Caption = 'Reset to defaults'
          TabOrder = 1
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
    Left = 229
    Top = 338
  end
  object ColorDialogConnected: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 309
    Top = 338
  end
end
