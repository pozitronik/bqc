object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 588
  ClientWidth = 544
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
    Top = 547
    Width = 544
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 530
    ExplicitWidth = 538
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
    Width = 544
    Height = 547
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 538
    ExplicitHeight = 530
    object TabGeneral: TTabSheet
      Caption = 'Window'
      object GroupWindowMode: TGroupBox
        Left = 0
        Top = 0
        Width = 536
        Height = 165
        Align = alTop
        Caption = 'How to display?'
        TabOrder = 0
        ExplicitWidth = 530
        object LabelWindowMode: TLabel
          Left = 12
          Top = 27
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
            'Menu (popup)'
            'Window (normal window)')
        end
        object GroupWindowOptions: TGroupBox
          Left = 2
          Top = 53
          Width = 532
          Height = 55
          Align = alBottom
          Caption = 'When it'#39's window:'
          TabOrder = 1
          ExplicitWidth = 526
          object CheckMinimizeToTray: TCheckBox
            Left = 12
            Top = 25
            Width = 120
            Height = 17
            Caption = 'Minimize to tray'
            TabOrder = 0
          end
          object CheckCloseToTray: TCheckBox
            Left = 276
            Top = 25
            Width = 120
            Height = 17
            Caption = 'Close to tray'
            TabOrder = 2
          end
          object CheckStartMinimized: TCheckBox
            Left = 148
            Top = 25
            Width = 120
            Height = 17
            Caption = 'Start minimized'
            TabOrder = 1
          end
        end
        object GroupMenuOptions: TGroupBox
          Left = 2
          Top = 108
          Width = 532
          Height = 55
          Align = alBottom
          Caption = 'When it'#39's menu:'
          TabOrder = 2
          ExplicitWidth = 526
          object CheckHideOnFocusLoss: TCheckBox
            Left = 12
            Top = 25
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
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Positioning:'
        TabOrder = 1
        ExplicitWidth = 530
        object LabelPositionMode: TLabel
          Left = 12
          Top = 28
          Width = 46
          Height = 15
          Caption = 'Position:'
        end
        object ComboPositionMode: TComboBox
          Left = 74
          Top = 25
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
          Top = 28
          Width = 105
          Height = 17
          Caption = 'Always on top'
          TabOrder = 3
        end
      end
      object CheckAutostart: TCheckBox
        Left = 12
        Top = 275
        Width = 136
        Height = 21
        Caption = 'Start with Windows'
        TabOrder = 2
      end
    end
    object TabAppearance: TTabSheet
      Caption = 'Device List'
      ImageIndex = 1
      object GroupDisplayOptions: TGroupBox
        Left = 0
        Top = 0
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Display Options'
        TabOrder = 0
        object LabelConnectedColor: TLabel
          Left = 274
          Top = 24
          Width = 125
          Height = 15
          Caption = 'Connected status color:'
        end
        object ShapeConnectedColor: TShape
          Left = 405
          Top = 22
          Width = 50
          Height = 21
          Cursor = crHandPoint
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
        object CheckShowAddresses: TCheckBox
          Left = 274
          Top = 49
          Width = 181
          Height = 17
          Caption = 'Show device MAC addresses'
          TabOrder = 4
        end
        object CheckShowBatteryLevel: TCheckBox
          Left = 274
          Top = 72
          Width = 181
          Height = 17
          Caption = 'Show battery level'
          TabOrder = 5
        end
      end
      object GroupLayout: TGroupBox
        Left = 0
        Top = 95
        Width = 536
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
          Left = 364
          Top = 23
          Width = 59
          Height = 15
          Caption = 'Icon width:'
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
          Left = 506
          Top = 24
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelPx5: TLabel
          Left = 333
          Top = 53
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelBorderWidth: TLabel
          Left = 190
          Top = 24
          Width = 71
          Height = 15
          Caption = 'Border width:'
        end
        object LabelPx6: TLabel
          Left = 333
          Top = 24
          Width = 12
          Height = 15
          Caption = 'px'
        end
        object LabelBorderColor: TLabel
          Left = 190
          Top = 82
          Width = 68
          Height = 15
          Caption = 'Border color:'
        end
        object ShapeBorderColor: TShape
          Left = 272
          Top = 79
          Width = 61
          Height = 23
          Brush.Color = clGray
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
        object EditBorderWidth: TEdit
          Left = 272
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 2
          Text = '0'
        end
        object UpDownBorderWidth: TUpDown
          Left = 317
          Top = 21
          Width = 16
          Height = 23
          Associate = EditBorderWidth
          Max = 20
          TabOrder = 3
        end
        object EditIconSize: TEdit
          Left = 443
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 4
          Text = '32'
        end
        object UpDownIconSize: TUpDown
          Left = 488
          Top = 21
          Width = 16
          Height = 23
          Associate = EditIconSize
          Min = 16
          Max = 64
          Position = 32
          TabOrder = 5
        end
        object EditItemPadding: TEdit
          Left = 100
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 6
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
          TabOrder = 7
        end
        object EditCornerRadius: TEdit
          Left = 272
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 8
          Text = '8'
        end
        object UpDownCornerRadius: TUpDown
          Left = 317
          Top = 50
          Width = 16
          Height = 23
          Associate = EditCornerRadius
          Max = 30
          Position = 8
          TabOrder = 9
        end
        object EditItemMargin: TEdit
          Left = 100
          Top = 79
          Width = 45
          Height = 23
          TabOrder = 10
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
          TabOrder = 11
        end
        object ButtonResetLayout: TButton
          Left = 364
          Top = 79
          Width = 154
          Height = 23
          Caption = 'Reset layout to defaults'
          TabOrder = 12
          OnClick = ButtonResetLayoutClick
        end
      end
      object GroupDiscovery: TGroupBox
        Left = 0
        Top = 215
        Width = 536
        Height = 80
        Align = alTop
        Caption = 'New devices && discovery'
        TabOrder = 2
        object CheckShowUnpairedDevices: TCheckBox
          Left = 12
          Top = 24
          Width = 500
          Height = 17
          Caption = 'Show unpaired devices in range (requires scan)'
          TabOrder = 0
        end
        object CheckShowUnidentifiedDevices: TCheckBox
          Left = 12
          Top = 47
          Width = 500
          Height = 17
          Caption = 'Show unidentified devices in scan results'
          TabOrder = 1
        end
      end
      object GroupProfiles: TGroupBox
        Left = 0
        Top = 270
        Width = 536
        Height = 55
        Align = alTop
        Caption = 'Profiles'
        TabOrder = 3
        object LabelProfileFontSize: TLabel
          Left = 274
          Top = 24
          Width = 49
          Height = 15
          Caption = 'Font size:'
        end
        object LabelProfileFontSizePt: TLabel
          Left = 396
          Top = 24
          Width = 11
          Height = 15
          Caption = 'pt'
        end
        object CheckShowProfiles: TCheckBox
          Left = 12
          Top = 24
          Width = 250
          Height = 17
          Caption = 'Show connected device profiles'
          TabOrder = 0
        end
        object EditProfileFontSize: TEdit
          Left = 335
          Top = 21
          Width = 40
          Height = 23
          TabOrder = 1
          Text = '7'
        end
        object UpDownProfileFontSize: TUpDown
          Left = 375
          Top = 21
          Width = 17
          Height = 23
          Associate = EditProfileFontSize
          Min = 6
          Max = 14
          Position = 7
          TabOrder = 2
        end
      end
      object GroupFontSizes: TGroupBox
        Left = 0
        Top = 325
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Font Sizes (pt)'
        TabOrder = 4
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
        object EditAddressSize: TEdit
          Left = 278
          Top = 21
          Width = 45
          Height = 23
          TabOrder = 2
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
          TabOrder = 3
        end
        object EditStatusSize: TEdit
          Left = 100
          Top = 50
          Width = 45
          Height = 23
          TabOrder = 4
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
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Global Hotkey'
        TabOrder = 0
        object LabelHotkey: TLabel
          Left = 12
          Top = 27
          Width = 41
          Height = 15
          Caption = 'Hotkey:'
        end
        object EditHotkey: TEdit
          Left = 117
          Top = 24
          Width = 207
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
      object GroupSystemPanels: TGroupBox
        Left = 0
        Top = 95
        Width = 536
        Height = 96
        Align = alTop
        Caption = 'Windows System Panels Hotkeys'
        TabOrder = 1
        object LabelCastPanelHotkey: TLabel
          Left = 12
          Top = 27
          Width = 58
          Height = 15
          Caption = 'Cast panel:'
        end
        object LabelBluetoothPanelHotkey: TLabel
          Left = 12
          Top = 59
          Width = 87
          Height = 15
          Caption = 'Bluetooth panel:'
        end
        object EditCastPanelHotkey: TEdit
          Left = 117
          Top = 24
          Width = 207
          Height = 23
          ReadOnly = True
          TabOrder = 0
        end
        object ButtonRecordCastHotkey: TButton
          Left = 335
          Top = 24
          Width = 80
          Height = 23
          Caption = 'Record...'
          TabOrder = 1
          OnClick = ButtonRecordCastHotkeyClick
        end
        object ButtonClearCastHotkey: TButton
          Left = 427
          Top = 24
          Width = 80
          Height = 23
          Caption = 'Clear'
          TabOrder = 2
          OnClick = ButtonClearCastHotkeyClick
        end
        object EditBluetoothPanelHotkey: TEdit
          Left = 117
          Top = 56
          Width = 207
          Height = 23
          ReadOnly = True
          TabOrder = 3
        end
        object ButtonRecordBluetoothHotkey: TButton
          Left = 335
          Top = 56
          Width = 80
          Height = 23
          Caption = 'Record...'
          TabOrder = 4
          OnClick = ButtonRecordBluetoothHotkeyClick
        end
        object ButtonClearBluetoothHotkey: TButton
          Left = 427
          Top = 56
          Width = 80
          Height = 23
          Caption = 'Clear'
          TabOrder = 5
          OnClick = ButtonClearBluetoothHotkeyClick
        end
      end
    end
    object TabThemes: TTabSheet
      Caption = 'Themes'
      ImageIndex = 3
      object GroupTheme: TGroupBox
        Left = 0
        Top = 0
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Theme'
        TabOrder = 0
        object LabelTheme: TLabel
          Left = 12
          Top = 27
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
          Left = 94
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
        Width = 536
        Height = 95
        Align = alTop
        Caption = 'Default Connection Settings'
        TabOrder = 0
        object LabelTimeout: TLabel
          Left = 12
          Top = 27
          Width = 48
          Height = 15
          Caption = 'Timeout:'
        end
        object LabelTimeoutMs: TLabel
          Left = 225
          Top = 27
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object LabelRetryCount: TLabel
          Left = 12
          Top = 61
          Width = 80
          Height = 15
          Caption = 'Retry attempts:'
        end
        object EditTimeout: TEdit
          Left = 129
          Top = 24
          Width = 75
          Height = 23
          TabOrder = 0
          Text = '0'
        end
        object UpDownTimeout: TUpDown
          Left = 204
          Top = 24
          Width = 16
          Height = 23
          Associate = EditTimeout
          Max = 60000
          Increment = 1000
          TabOrder = 1
        end
        object EditRetryCount: TEdit
          Left = 129
          Top = 58
          Width = 75
          Height = 23
          TabOrder = 2
          Text = '0'
        end
        object UpDownRetryCount: TUpDown
          Left = 204
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
        Width = 536
        Height = 160
        Align = alTop
        Caption = 'Device Monitoring'
        TabOrder = 1
        object LabelPollingMode: TLabel
          Left = 12
          Top = 27
          Width = 74
          Height = 15
          Caption = 'Polling mode:'
        end
        object LabelPollingInterval: TLabel
          Left = 12
          Top = 60
          Width = 42
          Height = 15
          Caption = 'Interval:'
        end
        object LabelPollingIntervalMs: TLabel
          Left = 225
          Top = 60
          Width = 16
          Height = 15
          Caption = 'ms'
        end
        object LabelPollingModeHint: TLabel
          Left = 269
          Top = 27
          Width = 130
          Height = 15
          Caption = 'Requires program restart'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnShadow
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object LabelEnumerationMode: TLabel
          Left = 12
          Top = 95
          Width = 105
          Height = 15
          Caption = 'Enumeration mode:'
        end
        object LabelEnumerationModeHint: TLabel
          Left = 269
          Top = 95
          Width = 130
          Height = 15
          Caption = 'Requires program restart'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnShadow
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object LabelBluetoothPlatform: TLabel
          Left = 12
          Top = 127
          Width = 104
          Height = 15
          Caption = 'Bluetooth platform:'
        end
        object LabelBluetoothPlatformHint: TLabel
          Left = 269
          Top = 127
          Width = 130
          Height = 15
          Caption = 'Requires program restart'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnShadow
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object ComboPollingMode: TComboBox
          Left = 129
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
          Left = 129
          Top = 57
          Width = 75
          Height = 23
          TabOrder = 1
          Text = '0'
        end
        object UpDownPollingInterval: TUpDown
          Left = 204
          Top = 57
          Width = 16
          Height = 23
          Associate = EditPollingInterval
          Max = 10000
          Increment = 500
          TabOrder = 2
        end
        object ComboEnumerationMode: TComboBox
          Left = 129
          Top = 92
          Width = 125
          Height = 23
          Hint = 'Requires app restart'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Items.Strings = (
            'Win32 API only'
            'WinRT API only'
            'Combined')
        end
        object ComboBluetoothPlatform: TComboBox
          Left = 129
          Top = 124
          Width = 125
          Height = 23
          Hint = 'Requires app restart'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Items.Strings = (
            'Auto-detect'
            'Classic (Win32)'
            'WinRT')
        end
      end
      object GroupNotifications: TGroupBox
        Left = 0
        Top = 390
        Width = 536
        Height = 123
        Align = alTop
        Caption = 'Notifications'
        TabOrder = 3
        ExplicitTop = 224
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
    object TabBatteryTray: TTabSheet
      Caption = 'Battery Tray'
      ImageIndex = 6
      object GroupBatteryTrayGlobal: TGroupBox
        Left = 0
        Top = 0
        Width = 536
        Height = 195
        Align = alTop
        Caption = 'Battery Tray Icons'
        TabOrder = 0
        object LabelDefaultBatteryColor: TLabel
          Left = 12
          Top = 54
          Width = 56
          Height = 15
          Caption = 'Icon color:'
        end
        object ShapeDefaultBatteryColor: TShape
          Left = 120
          Top = 53
          Width = 70
          Height = 20
          Cursor = crHandPoint
          OnMouseDown = HandleShapeColorMouseDown
        end
        object LabelDefaultBackgroundColor: TLabel
          Left = 210
          Top = 54
          Width = 97
          Height = 15
          Caption = 'Background color:'
        end
        object ShapeDefaultBackgroundColor: TShape
          Left = 320
          Top = 52
          Width = 70
          Height = 20
          Cursor = crHandPoint
          OnMouseDown = HandleShapeColorMouseDown
        end
        object LabelDefaultBatteryThreshold: TLabel
          Left = 12
          Top = 113
          Width = 78
          Height = 15
          Caption = 'Low threshold:'
        end
        object LabelDefaultBatteryThresholdPct: TLabel
          Left = 183
          Top = 113
          Width = 10
          Height = 15
          Caption = '%'
        end
        object LabelOutlineColorMode: TLabel
          Left = 12
          Top = 82
          Width = 66
          Height = 15
          Caption = 'Icon outline:'
        end
        object LabelCustomOutlineColor: TLabel
          Left = 320
          Top = 82
          Width = 75
          Height = 15
          Caption = 'Custom color:'
          Visible = False
        end
        object ShapeCustomOutlineColor: TShape
          Left = 400
          Top = 81
          Width = 70
          Height = 20
          Cursor = crHandPoint
          Brush.Color = clBlack
          Visible = False
          OnMouseDown = HandleShapeColorMouseDown
        end
        object CheckTransparentBackground: TCheckBox
          Left = 400
          Top = 53
          Width = 100
          Height = 17
          Caption = 'Transparent'
          TabOrder = 1
        end
        object CheckShowBatteryTrayIcons: TCheckBox
          Left = 12
          Top = 29
          Width = 250
          Height = 17
          Caption = 'Show battery level icons in system tray'
          TabOrder = 0
        end
        object EditDefaultBatteryThreshold: TEdit
          Left = 120
          Top = 110
          Width = 40
          Height = 23
          NumbersOnly = True
          TabOrder = 3
          Text = '20'
        end
        object UpDownDefaultBatteryThreshold: TUpDown
          Left = 160
          Top = 110
          Width = 17
          Height = 23
          Associate = EditDefaultBatteryThreshold
          Position = 20
          TabOrder = 4
        end
        object CheckShowNumericValue: TCheckBox
          Left = 12
          Top = 143
          Width = 250
          Height = 17
          Caption = 'Show numeric percentage instead of bar'
          TabOrder = 5
        end
        object CheckAutoColorOnLow: TCheckBox
          Left = 12
          Top = 166
          Width = 300
          Height = 17
          Caption = 'Automatically use red color for low battery'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object ComboOutlineColorMode: TComboBox
          Left = 120
          Top = 81
          Width = 187
          Height = 23
          Style = csDropDownList
          TabOrder = 2
          Items.Strings = (
            'Auto (follow system)'
            'Light (white)'
            'Dark (black)'
            'Custom')
        end
      end
      object GroupBatteryNotifications: TGroupBox
        Left = 0
        Top = 195
        Width = 536
        Height = 80
        Align = alTop
        Caption = 'Default Notifications'
        TabOrder = 1
        object CheckDefaultNotifyLowBattery: TCheckBox
          Left = 12
          Top = 24
          Width = 250
          Height = 17
          Caption = 'Notify when battery is low'
          TabOrder = 0
        end
        object CheckDefaultNotifyFullyCharged: TCheckBox
          Left = 12
          Top = 49
          Width = 250
          Height = 17
          Caption = 'Notify when fully charged'
          TabOrder = 1
        end
      end
    end
    object TabDevices: TTabSheet
      Caption = 'Devices'
      ImageIndex = 5
      object PanelDeviceList: TPanel
        Left = 0
        Top = 0
        Width = 200
        Height = 517
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ListDevices: TListBox
          Left = 0
          Top = 0
          Width = 200
          Height = 485
          Align = alClient
          ItemHeight = 15
          TabOrder = 0
          OnClick = ListDevicesClick
        end
        object PanelDeviceButtons: TPanel
          Left = 0
          Top = 485
          Width = 200
          Height = 32
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonForgetDevice: TButton
            Left = 0
            Top = 4
            Width = 95
            Height = 23
            Caption = 'Forget'
            TabOrder = 0
            OnClick = ButtonForgetDeviceClick
          end
          object ButtonRefreshDevices: TButton
            Left = 104
            Top = 4
            Width = 95
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
        Width = 336
        Height = 517
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object GroupDeviceInfo: TGroupBox
          Left = 0
          Top = 0
          Width = 336
          Height = 517
          Align = alClient
          Caption = 'Device Settings'
          TabOrder = 0
          object GroupDeviceConnection: TGroupBox
            Left = 2
            Top = 151
            Width = 332
            Height = 95
            Align = alTop
            Caption = 'Connection (overrides global parameters)'
            TabOrder = 1
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
              Top = 57
              Width = 80
              Height = 15
              Caption = 'Retry attempts:'
            end
            object EditDeviceTimeout: TEdit
              Left = 110
              Top = 22
              Width = 75
              Height = 23
              TabOrder = 0
              Text = '-1'
            end
            object UpDownDeviceTimeout: TUpDown
              Left = 185
              Top = 22
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
              Top = 54
              Width = 75
              Height = 23
              TabOrder = 2
              Text = '-1'
            end
            object UpDownDeviceRetryCount: TUpDown
              Left = 185
              Top = 54
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
            Left = 2
            Top = 246
            Width = 332
            Height = 95
            Align = alTop
            Caption = 'Notifications (overrides global parameters)'
            TabOrder = 2
            object LabelDeviceNotifyConnect: TLabel
              Left = 12
              Top = 27
              Width = 48
              Height = 15
              Caption = 'Connect:'
            end
            object LabelDeviceNotifyDisconnect: TLabel
              Left = 12
              Top = 58
              Width = 62
              Height = 15
              Caption = 'Disconnect:'
            end
            object LabelDeviceNotifyFailed: TLabel
              Left = 170
              Top = 27
              Width = 34
              Height = 15
              Caption = 'Failed:'
            end
            object LabelDeviceNotifyAuto: TLabel
              Left = 170
              Top = 59
              Width = 77
              Height = 15
              Caption = 'Auto-connect:'
            end
            object ComboDeviceNotifyConnect: TComboBox
              Left = 80
              Top = 24
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 0
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyFailed: TComboBox
              Left = 256
              Top = 24
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 1
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyDisconnect: TComboBox
              Left = 80
              Top = 56
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 2
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
            object ComboDeviceNotifyAuto: TComboBox
              Left = 256
              Top = 56
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 3
              Items.Strings = (
                'Default'
                'None'
                'Balloon')
            end
          end
          object GroupDeviceBatteryTray: TGroupBox
            Left = 2
            Top = 341
            Width = 332
            Height = 175
            Align = alTop
            Caption = 'Battery Tray Icon (overrides global parameters)'
            TabOrder = 3
            object LabelDeviceBatteryTrayIcon: TLabel
              Left = 12
              Top = 24
              Width = 58
              Height = 15
              Caption = 'Show icon:'
            end
            object LabelDeviceBatteryColor: TLabel
              Left = 12
              Top = 52
              Width = 56
              Height = 15
              Caption = 'Icon color:'
            end
            object LabelDeviceBatteryBackground: TLabel
              Left = 12
              Top = 80
              Width = 67
              Height = 15
              Caption = 'Background:'
            end
            object LabelDeviceBatteryThreshold: TLabel
              Left = 12
              Top = 108
              Width = 78
              Height = 15
              Caption = 'Low threshold:'
            end
            object LabelDeviceBatteryNumeric: TLabel
              Left = 176
              Top = 108
              Width = 49
              Height = 15
              Caption = 'Numeric:'
            end
            object LabelDeviceBatteryNotifyLow: TLabel
              Left = 12
              Top = 136
              Width = 58
              Height = 15
              Caption = 'Notify low:'
            end
            object LabelDeviceBatteryNotifyFull: TLabel
              Left = 176
              Top = 136
              Width = 56
              Height = 15
              Caption = 'Notify full:'
            end
            object ShapeDeviceBatteryColor: TShape
              Left = 256
              Top = 52
              Width = 70
              Height = 20
              Cursor = crHandPoint
              OnMouseDown = HandleShapeColorMouseDown
            end
            object ShapeDeviceBatteryBackground: TShape
              Left = 256
              Top = 80
              Width = 70
              Height = 20
              Cursor = crHandPoint
              OnMouseDown = HandleShapeColorMouseDown
            end
            object ComboDeviceBatteryTrayIcon: TComboBox
              Left = 97
              Top = 21
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 0
              Items.Strings = (
                'Default'
                'No'
                'Yes')
            end
            object ComboDeviceBatteryColorMode: TComboBox
              Left = 97
              Top = 49
              Width = 150
              Height = 23
              Style = csDropDownList
              TabOrder = 1
              OnChange = ComboDeviceBatteryColorModeChange
              Items.Strings = (
                'Default'
                'Custom')
            end
            object ComboDeviceBatteryBackgroundMode: TComboBox
              Left = 97
              Top = 77
              Width = 150
              Height = 23
              Style = csDropDownList
              TabOrder = 2
              OnChange = ComboDeviceBatteryBackgroundModeChange
              Items.Strings = (
                'Default'
                'Custom'
                'Transparent')
            end
            object EditDeviceBatteryThreshold: TEdit
              Left = 97
              Top = 105
              Width = 45
              Height = 23
              NumbersOnly = True
              TabOrder = 3
              Text = '-1'
            end
            object UpDownDeviceBatteryThreshold: TUpDown
              Left = 142
              Top = 105
              Width = 17
              Height = 23
              Associate = EditDeviceBatteryThreshold
              Min = -1
              Position = -1
              TabOrder = 4
            end
            object ComboDeviceBatteryNumeric: TComboBox
              Left = 256
              Top = 105
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 5
              Items.Strings = (
                'Default'
                'No'
                'Yes')
            end
            object ComboDeviceBatteryNotifyLow: TComboBox
              Left = 97
              Top = 134
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 6
              Items.Strings = (
                'Default'
                'No'
                'Yes')
            end
            object ComboDeviceBatteryNotifyFull: TComboBox
              Left = 256
              Top = 134
              Width = 70
              Height = 23
              Style = csDropDownList
              TabOrder = 7
              Items.Strings = (
                'Default'
                'No'
                'Yes')
            end
          end
          object GroupDeviceGeneral: TGroupBox
            Left = 2
            Top = 17
            Width = 332
            Height = 134
            Align = alTop
            Caption = 'General'
            TabOrder = 0
            object LabelDeviceAlias: TLabel
              Left = 12
              Top = 25
              Width = 28
              Height = 15
              Caption = 'Alias:'
            end
            object LabelDeviceType: TLabel
              Left = 12
              Top = 55
              Width = 28
              Height = 15
              Caption = 'Type:'
            end
            object LabelDeviceShowProfiles: TLabel
              Left = 166
              Top = 104
              Width = 74
              Height = 15
              Caption = 'Show profiles:'
            end
            object EditDeviceAlias: TEdit
              Left = 76
              Top = 22
              Width = 250
              Height = 23
              TabOrder = 0
            end
            object ComboDeviceType: TComboBox
              Left = 76
              Top = 52
              Width = 250
              Height = 23
              Style = csDropDownList
              TabOrder = 1
            end
            object CheckDevicePinned: TCheckBox
              Left = 12
              Top = 80
              Width = 140
              Height = 17
              Caption = 'Pin to top of list'
              TabOrder = 2
            end
            object CheckDeviceHidden: TCheckBox
              Left = 135
              Top = 80
              Width = 140
              Height = 17
              Caption = 'Hide from list'
              TabOrder = 3
            end
            object CheckDeviceAutoConnect: TCheckBox
              Left = 12
              Top = 103
              Width = 140
              Height = 17
              Caption = 'Auto-connect on start'
              TabOrder = 4
            end
            object ComboDeviceShowProfiles: TComboBox
              Left = 247
              Top = 101
              Width = 79
              Height = 23
              Style = csDropDownList
              TabOrder = 5
              Items.Strings = (
                'Default'
                'No'
                'Yes')
            end
          end
        end
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Diagnostics'
      ImageIndex = 7
      object GroupLogging: TGroupBox
        Left = 0
        Top = 0
        Width = 536
        Height = 125
        Align = alTop
        Caption = 'Logging'
        TabOrder = 0
        object LabelLogFilename: TLabel
          Left = 12
          Top = 90
          Width = 42
          Height = 15
          Caption = 'Log file:'
        end
        object LabelLogLevel: TLabel
          Left = 12
          Top = 57
          Width = 30
          Height = 15
          Caption = 'Level:'
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
        object ComboLogLevel: TComboBox
          Left = 70
          Top = 54
          Width = 120
          Height = 23
          Style = csDropDownList
          TabOrder = 2
          Items.Strings = (
            'Debug'
            'Info'
            'Warning'
            'Error')
        end
        object EditLogFilename: TEdit
          Left = 70
          Top = 87
          Width = 288
          Height = 23
          TabOrder = 3
        end
        object ButtonBrowseLogFile: TButton
          Left = 364
          Top = 87
          Width = 80
          Height = 23
          Caption = 'Browse...'
          TabOrder = 4
          OnClick = ButtonBrowseLogFileClick
        end
        object ButtonOpenLogFile: TButton
          Left = 450
          Top = 87
          Width = 80
          Height = 23
          Caption = 'Open log file'
          TabOrder = 5
          OnClick = ButtonOpenLogFileClick
        end
      end
      object GroupActions: TGroupBox
        Left = 0
        Top = 125
        Width = 536
        Height = 59
        Align = alTop
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
