{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations - Configuration            }
{                                                       }
{       DESIGN NOTES:                                   }
{       - Trivial getters/setters CANNOT be removed    }
{         (Delphi interface constraint: property       }
{         accessors in interfaces MUST exist as        }
{         methods in implementing classes)             }
{       - Direct field access properties intentional   }
{         (test setup pattern: direct access for       }
{         Arrange phase, interface methods for Act)    }
{       - Explicit initialization preferred over       }
{         implicit (documents test assumptions)        }
{                                                       }
{*******************************************************}

unit Tests.Mocks.Config;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Vcl.Graphics,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConnectionConfigIntf,
  App.LogConfigIntf,
  App.AppearanceConfigIntf,
  App.LayoutConfigIntf,
  App.NotificationConfigIntf,
  App.DeviceConfigTypes,
  App.BatteryTrayConfigIntf,
  App.ProfileConfigIntf;

type
  /// <summary>
  /// Mock implementation of ILayoutConfig for testing UI components.
  /// </summary>
  TMockLayoutConfig = class(TInterfacedObject, ILayoutConfig)
  private
    FItemHeight: Integer;
    FItemPadding: Integer;
    FItemMargin: Integer;
    FIconSize: Integer;
    FCornerRadius: Integer;
    FDeviceNameFontSize: Integer;
    FStatusFontSize: Integer;
    FAddressFontSize: Integer;
    FIconFontSize: Integer;
    FItemBorderWidth: Integer;
    FItemBorderColor: Integer;
    FShowUnpairedDevices: Boolean;
    FShowUnidentifiedDevices: Boolean;
    FScrollbarWidth: Integer;
  public
    constructor Create;

    // ILayoutConfig - getters
    // NOTE: These trivial getters are REQUIRED by Delphi.
    // Interface property accessors MUST be implemented as methods.
    // Cannot use direct field access (read FItemHeight) when interface declares (read GetItemHeight).
    function GetItemHeight: Integer;
    function GetItemPadding: Integer;
    function GetItemMargin: Integer;
    function GetIconSize: Integer;
    function GetCornerRadius: Integer;
    function GetDeviceNameFontSize: Integer;
    function GetStatusFontSize: Integer;
    function GetAddressFontSize: Integer;
    function GetIconFontSize: Integer;
    function GetItemBorderWidth: Integer;
    function GetItemBorderColor: Integer;
    function GetShowUnpairedDevices: Boolean;
    function GetShowUnidentifiedDevices: Boolean;
    function GetScrollbarWidth: Integer;

    // ILayoutConfig - setters
    // NOTE: Same constraint as getters - interface requires these methods.
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemMargin(AValue: Integer);
    procedure SetIconSize(AValue: Integer);
    procedure SetCornerRadius(AValue: Integer);
    procedure SetDeviceNameFontSize(AValue: Integer);
    procedure SetStatusFontSize(AValue: Integer);
    procedure SetAddressFontSize(AValue: Integer);
    procedure SetIconFontSize(AValue: Integer);
    procedure SetItemBorderWidth(AValue: Integer);
    procedure SetItemBorderColor(AValue: Integer);
    procedure SetShowUnpairedDevices(AValue: Boolean);
    procedure SetShowUnidentifiedDevices(AValue: Boolean);
    procedure SetScrollbarWidth(AValue: Integer);

    // Test setup properties
    // NOTE: Direct field access for test convenience (Arrange-Act-Assert pattern).
    // Tests use these for setup (MockLayout.ItemHeight := 80), interface methods for behavior verification.
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemPadding: Integer read FItemPadding write FItemPadding;
    property ItemMargin: Integer read FItemMargin write FItemMargin;
    property IconSize: Integer read FIconSize write FIconSize;
    property CornerRadius: Integer read FCornerRadius write FCornerRadius;
    property DeviceNameFontSize: Integer read FDeviceNameFontSize write FDeviceNameFontSize;
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;
    property AddressFontSize: Integer read FAddressFontSize write FAddressFontSize;
    property IconFontSize: Integer read FIconFontSize write FIconFontSize;
    property ItemBorderWidth: Integer read FItemBorderWidth write FItemBorderWidth;
    property ItemBorderColor: Integer read FItemBorderColor write FItemBorderColor;
    property ShowUnpairedDevices: Boolean read FShowUnpairedDevices write FShowUnpairedDevices;
    property ShowUnidentifiedDevices: Boolean read FShowUnidentifiedDevices write FShowUnidentifiedDevices;
    property ScrollbarWidth: Integer read FScrollbarWidth write FScrollbarWidth;
  end;

  /// <summary>
  /// Mock implementation of IAppearanceConfig for testing UI components.
  /// </summary>
  TMockAppearanceConfig = class(TInterfacedObject, IAppearanceConfig)
  private
    FShowAddresses: Boolean;
    FTheme: string;
    FVsfDir: string;
    FShowLastSeen: Boolean;
    FLastSeenFormat: TLastSeenFormat;
    FShowDeviceIcons: Boolean;
    FConnectedColor: Integer;
    FShowBatteryLevel: Boolean;
    FListBackgroundSource: TListBackgroundSource;
    FListBackgroundCustomColor: Integer;
    FMainColorSource: TMainColorSource;
    FMainCustomColor: Integer;
    FSecondaryColorSource: TSecondaryColorSource;
    FSecondaryCustomColor: Integer;
  public
    constructor Create;

    // IAppearanceConfig - getters
    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;
    function GetShowBatteryLevel: Boolean;
    function GetListBackgroundSource: TListBackgroundSource;
    function GetListBackgroundCustomColor: Integer;
    function GetMainColorSource: TMainColorSource;
    function GetMainCustomColor: Integer;
    function GetSecondaryColorSource: TSecondaryColorSource;
    function GetSecondaryCustomColor: Integer;

    // IAppearanceConfig - setters
    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);
    procedure SetShowBatteryLevel(AValue: Boolean);
    procedure SetListBackgroundSource(AValue: TListBackgroundSource);
    procedure SetListBackgroundCustomColor(AValue: Integer);
    procedure SetMainColorSource(AValue: TMainColorSource);
    procedure SetMainCustomColor(AValue: Integer);
    procedure SetSecondaryColorSource(AValue: TSecondaryColorSource);
    procedure SetSecondaryCustomColor(AValue: Integer);

    // Test setup properties
    property ShowAddresses: Boolean read FShowAddresses write FShowAddresses;
    property Theme: string read FTheme write FTheme;
    property VsfDir: string read FVsfDir write FVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write FShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write FLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write FShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write FConnectedColor;
    property ShowBatteryLevel: Boolean read FShowBatteryLevel write FShowBatteryLevel;
    property ListBackgroundSource: TListBackgroundSource read FListBackgroundSource write FListBackgroundSource;
    property ListBackgroundCustomColor: Integer read FListBackgroundCustomColor write FListBackgroundCustomColor;
    property MainColorSource: TMainColorSource read FMainColorSource write FMainColorSource;
    property MainCustomColor: Integer read FMainCustomColor write FMainCustomColor;
    property SecondaryColorSource: TSecondaryColorSource read FSecondaryColorSource write FSecondaryColorSource;
    property SecondaryCustomColor: Integer read FSecondaryCustomColor write FSecondaryCustomColor;
  end;

  /// <summary>
  /// Mock implementation of IDeviceConfigProvider for testing.
  /// Implements IDeviceConfigQuery, IDeviceConfigMutation, and combined IDeviceConfigProvider.
  /// </summary>
  TMockDeviceConfigProvider = class(TInterfacedObject,
    IDeviceConfigQuery, IDeviceConfigMutation, IDeviceConfigProvider)
  private
    FDeviceConfigs: TDictionary<UInt64, TDeviceConfig>;
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceConfigQuery
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
    function GetConfiguredDeviceAddresses: TArray<UInt64>;
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;

    // IDeviceConfigMutation
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);
    procedure RemoveDeviceConfig(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);

    // Test helpers
    procedure AddDeviceConfig(AAddress: UInt64; const AConfig: TDeviceConfig);
    procedure Clear;
    property DeviceConfigs: TDictionary<UInt64, TDeviceConfig> read FDeviceConfigs;
  end;

  /// <summary>
  /// Mock implementation of IBatteryTrayConfig for testing battery tray features.
  /// </summary>
  TMockBatteryTrayConfig = class(TInterfacedObject, IBatteryTrayConfig)
  private
    FShowBatteryTrayIcons: Boolean;
    FDefaultIconColor: TColor;
    FDefaultBackgroundColor: TColor;
    FDefaultShowNumericValue: Boolean;
    FDefaultLowBatteryThreshold: Integer;
    FDefaultNotifyLowBattery: Boolean;
    FDefaultNotifyFullyCharged: Boolean;
    FDefaultOutlineColorMode: TOutlineColorMode;
    FDefaultCustomOutlineColor: TColor;
  public
    constructor Create;

    // IBatteryTrayConfig - getters
    function GetShowBatteryTrayIcons: Boolean;
    function GetDefaultIconColor: TColor;
    function GetDefaultBackgroundColor: TColor;
    function GetDefaultShowNumericValue: Boolean;
    function GetDefaultLowBatteryThreshold: Integer;
    function GetDefaultNotifyLowBattery: Boolean;
    function GetDefaultNotifyFullyCharged: Boolean;
    function GetDefaultOutlineColorMode: TOutlineColorMode;
    function GetDefaultCustomOutlineColor: TColor;

    // IBatteryTrayConfig - setters
    procedure SetShowBatteryTrayIcons(AValue: Boolean);
    procedure SetDefaultIconColor(AValue: TColor);
    procedure SetDefaultBackgroundColor(AValue: TColor);
    procedure SetDefaultShowNumericValue(AValue: Boolean);
    procedure SetDefaultLowBatteryThreshold(AValue: Integer);
    procedure SetDefaultNotifyLowBattery(AValue: Boolean);
    procedure SetDefaultNotifyFullyCharged(AValue: Boolean);
    procedure SetDefaultOutlineColorMode(AValue: TOutlineColorMode);
    procedure SetDefaultCustomOutlineColor(AValue: TColor);

    // Direct access properties for testing
    property ShowBatteryTrayIcons: Boolean read FShowBatteryTrayIcons write FShowBatteryTrayIcons;
    property DefaultIconColor: TColor read FDefaultIconColor write FDefaultIconColor;
    property DefaultBackgroundColor: TColor read FDefaultBackgroundColor write FDefaultBackgroundColor;
    property DefaultShowNumericValue: Boolean read FDefaultShowNumericValue write FDefaultShowNumericValue;
    property DefaultLowBatteryThreshold: Integer read FDefaultLowBatteryThreshold write FDefaultLowBatteryThreshold;
    property DefaultNotifyLowBattery: Boolean read FDefaultNotifyLowBattery write FDefaultNotifyLowBattery;
    property DefaultNotifyFullyCharged: Boolean read FDefaultNotifyFullyCharged write FDefaultNotifyFullyCharged;
    property DefaultOutlineColorMode: TOutlineColorMode read FDefaultOutlineColorMode write FDefaultOutlineColorMode;
    property DefaultCustomOutlineColor: TColor read FDefaultCustomOutlineColor write FDefaultCustomOutlineColor;
  end;

  /// <summary>
  /// Mock implementation of IPollingConfig for testing services.
  /// </summary>
  TMockPollingConfig = class(TInterfacedObject, IPollingConfig)
  private
    FPollingMode: TPollingMode;
    FPollingInterval: Integer;
    FEventDebounceMs: Integer;
  public
    constructor Create;

    // IPollingConfig - getters
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

    // IPollingConfig - setters
    procedure SetPollingMode(AValue: TPollingMode);
    procedure SetPollingInterval(AValue: Integer);
    procedure SetEventDebounceMs(AValue: Integer);

    // Test setup properties
    property PollingMode: TPollingMode read FPollingMode write FPollingMode;
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
    property EventDebounceMs: Integer read FEventDebounceMs write FEventDebounceMs;
  end;

  /// <summary>
  /// Mock implementation of IConnectionConfig for testing services.
  /// </summary>
  TMockConnectionConfig = class(TInterfacedObject, IConnectionConfig)
  private
    FConnectionTimeout: Integer;
    FConnectionRetryCount: Integer;
    FEnumerationMode: TEnumerationMode;
    FBluetoothPlatform: TBluetoothPlatform;
    FAutoScanOnStartup: Boolean;
    FPairingStateSyncInterval: Integer;
    FPairingTimeout: Integer;
    FPairingMode: TPairingMode;
  public
    constructor Create;

    // IConnectionConfig - getters
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;
    function GetEnumerationMode: TEnumerationMode;
    function GetBluetoothPlatform: TBluetoothPlatform;
    function GetAutoScanOnStartup: Boolean;
    function GetPairingStateSyncInterval: Integer;
    function GetPairingTimeout: Integer;
    function GetPairingMode: TPairingMode;

    // IConnectionConfig - setters
    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetConnectionRetryCount(AValue: Integer);
    procedure SetEnumerationMode(AValue: TEnumerationMode);
    procedure SetBluetoothPlatform(AValue: TBluetoothPlatform);
    procedure SetAutoScanOnStartup(AValue: Boolean);
    procedure SetPairingStateSyncInterval(AValue: Integer);
    procedure SetPairingTimeout(AValue: Integer);
    procedure SetPairingMode(AValue: TPairingMode);

    // Test setup properties
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write FConnectionRetryCount;
    property EnumerationMode: TEnumerationMode read FEnumerationMode write FEnumerationMode;
    property BluetoothPlatform: TBluetoothPlatform read FBluetoothPlatform write FBluetoothPlatform;
    property AutoScanOnStartup: Boolean read FAutoScanOnStartup write FAutoScanOnStartup;
    property PairingStateSyncInterval: Integer read FPairingStateSyncInterval write FPairingStateSyncInterval;
    property PairingTimeout: Integer read FPairingTimeout write FPairingTimeout;
    property PairingMode: TPairingMode read FPairingMode write FPairingMode;
  end;

  /// <summary>
  /// Mock implementation of IGeneralConfig for testing.
  /// </summary>
  TMockGeneralConfig = class(TInterfacedObject, IGeneralConfig)
  private
    FWindowMode: TWindowMode;
    FOnTop: Boolean;
    FAutostart: Boolean;
  public
    constructor Create;

    // IGeneralConfig
    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;
    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);

    property WindowMode: TWindowMode read FWindowMode write FWindowMode;
    property OnTop: Boolean read FOnTop write FOnTop;
    property Autostart: Boolean read FAutostart write FAutostart;
  end;

  /// <summary>
  /// Mock implementation of IWindowConfig for testing.
  /// </summary>
  TMockWindowConfig = class(TInterfacedObject, IWindowConfig)
  private
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;
    FStartMinimized: Boolean;
    FMenuHideOnFocusLoss: Boolean;
  public
    constructor Create;

    // IWindowConfig
    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetStartMinimized: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;
    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetStartMinimized(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);

    property MinimizeToTray: Boolean read FMinimizeToTray write FMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write FCloseToTray;
    property StartMinimized: Boolean read FStartMinimized write FStartMinimized;
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write FMenuHideOnFocusLoss;
  end;

  /// <summary>
  /// Mock implementation of IPositionConfig for testing.
  /// </summary>
  TMockPositionConfig = class(TInterfacedObject, IPositionConfig)
  private
    FPositionMode: TPositionMode;
    FPositionX: Integer;
    FPositionY: Integer;
    FPositionW: Integer;
    FPositionH: Integer;
  public
    constructor Create;

    // IPositionConfig
    function GetPositionMode: TPositionMode;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetPositionW: Integer;
    function GetPositionH: Integer;
    procedure SetPositionMode(AValue: TPositionMode);
    procedure SetPositionX(AValue: Integer);
    procedure SetPositionY(AValue: Integer);
    procedure SetPositionW(AValue: Integer);
    procedure SetPositionH(AValue: Integer);

    property PositionMode: TPositionMode read FPositionMode write FPositionMode;
    property PositionX: Integer read FPositionX write FPositionX;
    property PositionY: Integer read FPositionY write FPositionY;
    property PositionW: Integer read FPositionW write FPositionW;
    property PositionH: Integer read FPositionH write FPositionH;
  end;

  /// <summary>
  /// Mock implementation of IHotkeyConfig for testing.
  /// </summary>
  TMockHotkeyConfig = class(TInterfacedObject, IHotkeyConfig)
  private
    FHotkey: string;
    FUseLowLevelHook: Boolean;
    FCastPanelHotkey: string;
    FBluetoothPanelHotkey: string;
  public
    constructor Create;

    // IHotkeyConfig
    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;
    function GetCastPanelHotkey: string;
    function GetBluetoothPanelHotkey: string;
    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);
    procedure SetCastPanelHotkey(const AValue: string);
    procedure SetBluetoothPanelHotkey(const AValue: string);

    property Hotkey: string read FHotkey write FHotkey;
    property UseLowLevelHook: Boolean read FUseLowLevelHook write FUseLowLevelHook;
    property CastPanelHotkey: string read FCastPanelHotkey write FCastPanelHotkey;
    property BluetoothPanelHotkey: string read FBluetoothPanelHotkey write FBluetoothPanelHotkey;
  end;

  /// <summary>
  /// Mock implementation of INotificationConfig for testing.
  /// </summary>
  TMockNotificationConfig = class(TInterfacedObject, INotificationConfig)
  private
    FNotifyOnConnect: TNotificationMode;
    FNotifyOnDisconnect: TNotificationMode;
    FNotifyOnConnectFailed: TNotificationMode;
    FNotifyOnAutoConnect: TNotificationMode;
  public
    constructor Create;

    // INotificationConfig
    function GetNotifyOnConnect: TNotificationMode;
    function GetNotifyOnDisconnect: TNotificationMode;
    function GetNotifyOnConnectFailed: TNotificationMode;
    function GetNotifyOnAutoConnect: TNotificationMode;
    procedure SetNotifyOnConnect(AValue: TNotificationMode);
    procedure SetNotifyOnDisconnect(AValue: TNotificationMode);
    procedure SetNotifyOnConnectFailed(AValue: TNotificationMode);
    procedure SetNotifyOnAutoConnect(AValue: TNotificationMode);

    property NotifyOnConnect: TNotificationMode read FNotifyOnConnect write FNotifyOnConnect;
    property NotifyOnDisconnect: TNotificationMode read FNotifyOnDisconnect write FNotifyOnDisconnect;
    property NotifyOnConnectFailed: TNotificationMode read FNotifyOnConnectFailed write FNotifyOnConnectFailed;
    property NotifyOnAutoConnect: TNotificationMode read FNotifyOnAutoConnect write FNotifyOnAutoConnect;
  end;

  /// <summary>
  /// Mock implementation of IProfileConfig for testing profile display features.
  /// </summary>
  TMockProfileConfig = class(TInterfacedObject, IProfileConfig)
  private
    FShowProfiles: Boolean;
    FProfileFontSize: Integer;
  public
    constructor Create;

    // IProfileConfig - getters
    function GetShowProfiles: Boolean;
    function GetProfileFontSize: Integer;

    // IProfileConfig - setters
    procedure SetShowProfiles(AValue: Boolean);
    procedure SetProfileFontSize(AValue: Integer);

    // Test setup properties
    property ShowProfiles: Boolean read FShowProfiles write FShowProfiles;
    property ProfileFontSize: Integer read FProfileFontSize write FProfileFontSize;
  end;

  /// <summary>
  /// Mock implementation of ILogConfig for testing.
  /// </summary>
  TMockLogConfig = class(TInterfacedObject, ILogConfig)
  private
    FLogEnabled: Boolean;
    FLogFilename: string;
    FLogAppend: Boolean;
    FLogLevel: TLogLevel;
  public
    constructor Create;

    // ILogConfig
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetLogLevel: TLogLevel;
    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetLogLevel(AValue: TLogLevel);

    property LogEnabled: Boolean read FLogEnabled write FLogEnabled;
    property LogFilename: string read FLogFilename write FLogFilename;
    property LogAppend: Boolean read FLogAppend write FLogAppend;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

  /// <summary>
  /// Mock implementation of IAppConfig for testing.
  /// Provides sub-interface mocks for testability.
  /// </summary>
  TMockAppConfig = class(TInterfacedObject, IAppConfig)
  private
    FModified: Boolean;
    FSaveIfModifiedCount: Integer;
    FClearModifiedCount: Integer;
    FSaveCount: Integer;
    FLoadCount: Integer;

    // Sub-interface mocks (created in constructor)
    FGeneralConfig: IGeneralConfig;
    FWindowConfig: IWindowConfig;
    FPositionConfig: IPositionConfig;
    FHotkeyConfig: IHotkeyConfig;
    FPollingConfig: IPollingConfig;
    FLogConfig: ILogConfig;
    FAppearanceConfig: IAppearanceConfig;
    FLayoutConfig: ILayoutConfig;
    FConnectionConfig: IConnectionConfig;
    FNotificationConfig: INotificationConfig;
    FBatteryTrayConfig: IBatteryTrayConfig;
    FProfileConfig: IProfileConfig;
    FDeviceConfigProvider: IDeviceConfigProvider;
  public
    constructor Create;

    // IAppConfig
    function GetConfigPath: string;
    function GetModified: Boolean;
    procedure Load;
    procedure Save;
    procedure SaveIfModified;
    procedure ClearModified;
    function AsGeneralConfig: IGeneralConfig;
    function AsWindowConfig: IWindowConfig;
    function AsPositionConfig: IPositionConfig;
    function AsHotkeyConfig: IHotkeyConfig;
    function AsPollingConfig: IPollingConfig;
    function AsLogConfig: ILogConfig;
    function AsAppearanceConfig: IAppearanceConfig;
    function AsLayoutConfig: ILayoutConfig;
    function AsConnectionConfig: IConnectionConfig;
    function AsNotificationConfig: INotificationConfig;
    function AsBatteryTrayConfig: IBatteryTrayConfig;
    function AsProfileConfig: IProfileConfig;
    function AsDeviceConfigProvider: IDeviceConfigProvider;

    // Test verification
    property Modified: Boolean read FModified write FModified;
    property SaveIfModifiedCount: Integer read FSaveIfModifiedCount;
    property ClearModifiedCount: Integer read FClearModifiedCount;
    property SaveCount: Integer read FSaveCount;
    property LoadCount: Integer read FLoadCount;

    // Access to sub-config mocks for test setup
    property GeneralConfig: IGeneralConfig read FGeneralConfig;
    property WindowConfig: IWindowConfig read FWindowConfig;
    property PositionConfig: IPositionConfig read FPositionConfig;
    property HotkeyConfig: IHotkeyConfig read FHotkeyConfig;
    property PollingConfig: IPollingConfig read FPollingConfig;
    property LogConfig: ILogConfig read FLogConfig;
    property AppearanceConfig: IAppearanceConfig read FAppearanceConfig;
    property LayoutConfig: ILayoutConfig read FLayoutConfig;
    property ConnectionConfig: IConnectionConfig read FConnectionConfig;
    property NotificationConfig: INotificationConfig read FNotificationConfig;
    property BatteryTrayConfig: IBatteryTrayConfig read FBatteryTrayConfig;
    property ProfileConfig: IProfileConfig read FProfileConfig;
  end;

implementation

{ TMockLayoutConfig }

constructor TMockLayoutConfig.Create;
begin
  inherited Create;
  // Set reasonable defaults
  // NOTE: Explicit initialization is intentional (not boilerplate).
  // Documents test assumptions and ensures predictable initial state.
  // While Delphi zero-initializes integers, explicit values serve as documentation.
  FItemHeight := 70;
  FItemPadding := 12;
  FItemMargin := 4;
  FIconSize := 32;
  FCornerRadius := 8;
  FDeviceNameFontSize := 11;
  FStatusFontSize := 9;
  FAddressFontSize := 8;
  FIconFontSize := 16;
  FItemBorderWidth := 0;
  FItemBorderColor := $00000000;
  FShowUnpairedDevices := False;
  FShowUnidentifiedDevices := True;  // Show all by default (non-breaking)
  FScrollbarWidth := 10;
end;

function TMockLayoutConfig.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TMockLayoutConfig.GetItemPadding: Integer;
begin
  Result := FItemPadding;
end;

function TMockLayoutConfig.GetItemMargin: Integer;
begin
  Result := FItemMargin;
end;

function TMockLayoutConfig.GetIconSize: Integer;
begin
  Result := FIconSize;
end;

function TMockLayoutConfig.GetCornerRadius: Integer;
begin
  Result := FCornerRadius;
end;

function TMockLayoutConfig.GetDeviceNameFontSize: Integer;
begin
  Result := FDeviceNameFontSize;
end;

function TMockLayoutConfig.GetStatusFontSize: Integer;
begin
  Result := FStatusFontSize;
end;

function TMockLayoutConfig.GetAddressFontSize: Integer;
begin
  Result := FAddressFontSize;
end;

function TMockLayoutConfig.GetIconFontSize: Integer;
begin
  Result := FIconFontSize;
end;

function TMockLayoutConfig.GetItemBorderWidth: Integer;
begin
  Result := FItemBorderWidth;
end;

function TMockLayoutConfig.GetItemBorderColor: Integer;
begin
  Result := FItemBorderColor;
end;

procedure TMockLayoutConfig.SetItemHeight(AValue: Integer);
begin
  FItemHeight := AValue;
end;

procedure TMockLayoutConfig.SetItemPadding(AValue: Integer);
begin
  FItemPadding := AValue;
end;

procedure TMockLayoutConfig.SetItemMargin(AValue: Integer);
begin
  FItemMargin := AValue;
end;

procedure TMockLayoutConfig.SetIconSize(AValue: Integer);
begin
  FIconSize := AValue;
end;

procedure TMockLayoutConfig.SetCornerRadius(AValue: Integer);
begin
  FCornerRadius := AValue;
end;

procedure TMockLayoutConfig.SetDeviceNameFontSize(AValue: Integer);
begin
  FDeviceNameFontSize := AValue;
end;

procedure TMockLayoutConfig.SetStatusFontSize(AValue: Integer);
begin
  FStatusFontSize := AValue;
end;

procedure TMockLayoutConfig.SetAddressFontSize(AValue: Integer);
begin
  FAddressFontSize := AValue;
end;

procedure TMockLayoutConfig.SetIconFontSize(AValue: Integer);
begin
  FIconFontSize := AValue;
end;

procedure TMockLayoutConfig.SetItemBorderWidth(AValue: Integer);
begin
  FItemBorderWidth := AValue;
end;

procedure TMockLayoutConfig.SetItemBorderColor(AValue: Integer);
begin
  FItemBorderColor := AValue;
end;

function TMockLayoutConfig.GetShowUnpairedDevices: Boolean;
begin
  Result := FShowUnpairedDevices;
end;

procedure TMockLayoutConfig.SetShowUnpairedDevices(AValue: Boolean);
begin
  FShowUnpairedDevices := AValue;
end;

function TMockLayoutConfig.GetShowUnidentifiedDevices: Boolean;
begin
  Result := FShowUnidentifiedDevices;
end;

procedure TMockLayoutConfig.SetShowUnidentifiedDevices(AValue: Boolean);
begin
  FShowUnidentifiedDevices := AValue;
end;

function TMockLayoutConfig.GetScrollbarWidth: Integer;
begin
  Result := FScrollbarWidth;
end;

procedure TMockLayoutConfig.SetScrollbarWidth(AValue: Integer);
begin
  FScrollbarWidth := AValue;
end;

{ TMockAppearanceConfig }

constructor TMockAppearanceConfig.Create;
begin
  inherited Create;
  FShowAddresses := False;
  FTheme := 'Windows';
  FVsfDir := 'themes';
  FShowLastSeen := False;
  FLastSeenFormat := lsfRelative;
  FShowDeviceIcons := True;
  FConnectedColor := $0000AA00;  // Green
  FShowBatteryLevel := False;  // Default to False to avoid async battery cache operations in tests
  FListBackgroundSource := lbsThemeWindow;
  FListBackgroundCustomColor := clWindow;
  FMainColorSource := mcsThemeText;
  FMainCustomColor := clWindowText;
  FSecondaryColorSource := scsThemeGrayText;
  FSecondaryCustomColor := clGrayText;
end;

function TMockAppearanceConfig.GetShowAddresses: Boolean;
begin
  Result := FShowAddresses;
end;

function TMockAppearanceConfig.GetTheme: string;
begin
  Result := FTheme;
end;

function TMockAppearanceConfig.GetVsfDir: string;
begin
  Result := FVsfDir;
end;

function TMockAppearanceConfig.GetShowLastSeen: Boolean;
begin
  Result := FShowLastSeen;
end;

function TMockAppearanceConfig.GetLastSeenFormat: TLastSeenFormat;
begin
  Result := FLastSeenFormat;
end;

function TMockAppearanceConfig.GetShowDeviceIcons: Boolean;
begin
  Result := FShowDeviceIcons;
end;

function TMockAppearanceConfig.GetConnectedColor: Integer;
begin
  Result := FConnectedColor;
end;

function TMockAppearanceConfig.GetShowBatteryLevel: Boolean;
begin
  Result := FShowBatteryLevel;
end;

procedure TMockAppearanceConfig.SetShowAddresses(AValue: Boolean);
begin
  FShowAddresses := AValue;
end;

procedure TMockAppearanceConfig.SetTheme(const AValue: string);
begin
  FTheme := AValue;
end;

procedure TMockAppearanceConfig.SetVsfDir(const AValue: string);
begin
  FVsfDir := AValue;
end;

procedure TMockAppearanceConfig.SetShowLastSeen(AValue: Boolean);
begin
  FShowLastSeen := AValue;
end;

procedure TMockAppearanceConfig.SetLastSeenFormat(AValue: TLastSeenFormat);
begin
  FLastSeenFormat := AValue;
end;

procedure TMockAppearanceConfig.SetShowDeviceIcons(AValue: Boolean);
begin
  FShowDeviceIcons := AValue;
end;

procedure TMockAppearanceConfig.SetConnectedColor(AValue: Integer);
begin
  FConnectedColor := AValue;
end;

procedure TMockAppearanceConfig.SetShowBatteryLevel(AValue: Boolean);
begin
  FShowBatteryLevel := AValue;
end;

function TMockAppearanceConfig.GetListBackgroundSource: TListBackgroundSource;
begin
  Result := FListBackgroundSource;
end;

procedure TMockAppearanceConfig.SetListBackgroundSource(AValue: TListBackgroundSource);
begin
  FListBackgroundSource := AValue;
end;

function TMockAppearanceConfig.GetListBackgroundCustomColor: Integer;
begin
  Result := FListBackgroundCustomColor;
end;

procedure TMockAppearanceConfig.SetListBackgroundCustomColor(AValue: Integer);
begin
  FListBackgroundCustomColor := AValue;
end;

function TMockAppearanceConfig.GetMainColorSource: TMainColorSource;
begin
  Result := FMainColorSource;
end;

procedure TMockAppearanceConfig.SetMainColorSource(AValue: TMainColorSource);
begin
  FMainColorSource := AValue;
end;

function TMockAppearanceConfig.GetMainCustomColor: Integer;
begin
  Result := FMainCustomColor;
end;

procedure TMockAppearanceConfig.SetMainCustomColor(AValue: Integer);
begin
  FMainCustomColor := AValue;
end;

function TMockAppearanceConfig.GetSecondaryColorSource: TSecondaryColorSource;
begin
  Result := FSecondaryColorSource;
end;

procedure TMockAppearanceConfig.SetSecondaryColorSource(AValue: TSecondaryColorSource);
begin
  FSecondaryColorSource := AValue;
end;

function TMockAppearanceConfig.GetSecondaryCustomColor: Integer;
begin
  Result := FSecondaryCustomColor;
end;

procedure TMockAppearanceConfig.SetSecondaryCustomColor(AValue: Integer);
begin
  FSecondaryCustomColor := AValue;
end;

{ TMockDeviceConfigProvider }

constructor TMockDeviceConfigProvider.Create;
begin
  inherited Create;
  FDeviceConfigs := TDictionary<UInt64, TDeviceConfig>.Create;
end;

destructor TMockDeviceConfigProvider.Destroy;
begin
  FDeviceConfigs.Free;
  inherited Destroy;
end;

function TMockDeviceConfigProvider.GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
begin
  if not FDeviceConfigs.TryGetValue(AAddress, Result) then
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TMockDeviceConfigProvider.SetDeviceConfig(const AConfig: TDeviceConfig);
begin
  FDeviceConfigs.AddOrSetValue(AConfig.Address, AConfig);
end;

procedure TMockDeviceConfigProvider.RemoveDeviceConfig(AAddress: UInt64);
begin
  FDeviceConfigs.Remove(AAddress);
end;

procedure TMockDeviceConfigProvider.RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
var
  Config: TDeviceConfig;
begin
  if FDeviceConfigs.TryGetValue(AAddress, Config) then
  begin
    Config.Name := AName;
    Config.LastSeen := ALastSeen;
    FDeviceConfigs[AAddress] := Config;
  end
  else
  begin
    Config := TDeviceConfig.Default(AAddress);
    Config.Name := AName;
    Config.LastSeen := ALastSeen;
    FDeviceConfigs.Add(AAddress, Config);
  end;
end;

function TMockDeviceConfigProvider.GetConfiguredDeviceAddresses: TArray<UInt64>;
begin
  Result := FDeviceConfigs.Keys.ToArray;
end;

function TMockDeviceConfigProvider.GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
var
  Config: TDeviceConfig;
  NotifyValue: Integer;
begin
  Result := nmNone;  // Default

  if FDeviceConfigs.TryGetValue(AAddress, Config) then
  begin
    case AEvent of
      neConnect: NotifyValue := Config.Notifications.OnConnect;
      neDisconnect: NotifyValue := Config.Notifications.OnDisconnect;
      neConnectFailed: NotifyValue := Config.Notifications.OnConnectFailed;
      neAutoConnect: NotifyValue := Config.Notifications.OnAutoConnect;
    else
      NotifyValue := -1;
    end;

    if NotifyValue >= 0 then
      Result := TNotificationMode(NotifyValue);
  end;
end;

function TMockDeviceConfigProvider.GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
var
  Config: TDeviceConfig;
begin
  if FDeviceConfigs.TryGetValue(AAddress, Config) and (Config.ConnectionTimeout >= 0) then
    Result := Config.ConnectionTimeout
  else
    Result := 10000;  // Default timeout
end;

function TMockDeviceConfigProvider.GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
var
  Config: TDeviceConfig;
begin
  if FDeviceConfigs.TryGetValue(AAddress, Config) and (Config.ConnectionRetryCount >= 0) then
    Result := Config.ConnectionRetryCount
  else
    Result := 2;  // Default retry count
end;

procedure TMockDeviceConfigProvider.AddDeviceConfig(AAddress: UInt64; const AConfig: TDeviceConfig);
begin
  FDeviceConfigs.AddOrSetValue(AAddress, AConfig);
end;

procedure TMockDeviceConfigProvider.Clear;
begin
  FDeviceConfigs.Clear;
end;

{ TMockPollingConfig }

constructor TMockPollingConfig.Create;
begin
  inherited Create;
  FPollingMode := pmFallback;
  FPollingInterval := 2000;
  FEventDebounceMs := 500;
end;

function TMockPollingConfig.GetPollingMode: TPollingMode;
begin
  Result := FPollingMode;
end;

function TMockPollingConfig.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

function TMockPollingConfig.GetEventDebounceMs: Integer;
begin
  Result := FEventDebounceMs;
end;

procedure TMockPollingConfig.SetPollingMode(AValue: TPollingMode);
begin
  FPollingMode := AValue;
end;

procedure TMockPollingConfig.SetPollingInterval(AValue: Integer);
begin
  FPollingInterval := AValue;
end;

procedure TMockPollingConfig.SetEventDebounceMs(AValue: Integer);
begin
  FEventDebounceMs := AValue;
end;

{ TMockConnectionConfig }

constructor TMockConnectionConfig.Create;
begin
  inherited Create;
  FConnectionTimeout := 10000;
  FConnectionRetryCount := 2;
  FEnumerationMode := emComposite;
  FBluetoothPlatform := bpAuto;
  FAutoScanOnStartup := False;
  FPairingStateSyncInterval := 30000;
  FPairingTimeout := 30000;
  FPairingMode := pmAutomatic;
end;

function TMockConnectionConfig.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TMockConnectionConfig.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

function TMockConnectionConfig.GetEnumerationMode: TEnumerationMode;
begin
  Result := FEnumerationMode;
end;

procedure TMockConnectionConfig.SetConnectionTimeout(AValue: Integer);
begin
  FConnectionTimeout := AValue;
end;

procedure TMockConnectionConfig.SetConnectionRetryCount(AValue: Integer);
begin
  FConnectionRetryCount := AValue;
end;

procedure TMockConnectionConfig.SetEnumerationMode(AValue: TEnumerationMode);
begin
  FEnumerationMode := AValue;
end;

function TMockConnectionConfig.GetBluetoothPlatform: TBluetoothPlatform;
begin
  Result := FBluetoothPlatform;
end;

procedure TMockConnectionConfig.SetBluetoothPlatform(AValue: TBluetoothPlatform);
begin
  FBluetoothPlatform := AValue;
end;

function TMockConnectionConfig.GetAutoScanOnStartup: Boolean;
begin
  Result := FAutoScanOnStartup;
end;

procedure TMockConnectionConfig.SetAutoScanOnStartup(AValue: Boolean);
begin
  FAutoScanOnStartup := AValue;
end;

function TMockConnectionConfig.GetPairingStateSyncInterval: Integer;
begin
  Result := FPairingStateSyncInterval;
end;

procedure TMockConnectionConfig.SetPairingStateSyncInterval(AValue: Integer);
begin
  FPairingStateSyncInterval := AValue;
end;

function TMockConnectionConfig.GetPairingTimeout: Integer;
begin
  Result := FPairingTimeout;
end;

function TMockConnectionConfig.GetPairingMode: TPairingMode;
begin
  Result := FPairingMode;
end;

procedure TMockConnectionConfig.SetPairingTimeout(AValue: Integer);
begin
  FPairingTimeout := AValue;
end;

procedure TMockConnectionConfig.SetPairingMode(AValue: TPairingMode);
begin
  FPairingMode := AValue;
end;

{ TMockGeneralConfig }

constructor TMockGeneralConfig.Create;
begin
  inherited Create;
  FWindowMode := wmWindow;
  FOnTop := False;
  FAutostart := False;
end;

function TMockGeneralConfig.GetWindowMode: TWindowMode;
begin
  Result := FWindowMode;
end;

function TMockGeneralConfig.GetOnTop: Boolean;
begin
  Result := FOnTop;
end;

function TMockGeneralConfig.GetAutostart: Boolean;
begin
  Result := FAutostart;
end;

procedure TMockGeneralConfig.SetWindowMode(AValue: TWindowMode);
begin
  FWindowMode := AValue;
end;

procedure TMockGeneralConfig.SetOnTop(AValue: Boolean);
begin
  FOnTop := AValue;
end;

procedure TMockGeneralConfig.SetAutostart(AValue: Boolean);
begin
  FAutostart := AValue;
end;

{ TMockWindowConfig }

constructor TMockWindowConfig.Create;
begin
  inherited Create;
  FMinimizeToTray := True;
  FCloseToTray := True;
  FStartMinimized := True;
  FMenuHideOnFocusLoss := True;
end;

function TMockWindowConfig.GetMinimizeToTray: Boolean;
begin
  Result := FMinimizeToTray;
end;

function TMockWindowConfig.GetCloseToTray: Boolean;
begin
  Result := FCloseToTray;
end;

function TMockWindowConfig.GetStartMinimized: Boolean;
begin
  Result := FStartMinimized;
end;

function TMockWindowConfig.GetMenuHideOnFocusLoss: Boolean;
begin
  Result := FMenuHideOnFocusLoss;
end;

procedure TMockWindowConfig.SetMinimizeToTray(AValue: Boolean);
begin
  FMinimizeToTray := AValue;
end;

procedure TMockWindowConfig.SetCloseToTray(AValue: Boolean);
begin
  FCloseToTray := AValue;
end;

procedure TMockWindowConfig.SetStartMinimized(AValue: Boolean);
begin
  FStartMinimized := AValue;
end;

procedure TMockWindowConfig.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  FMenuHideOnFocusLoss := AValue;
end;

{ TMockPositionConfig }

constructor TMockPositionConfig.Create;
begin
  inherited Create;
  FPositionMode := pmNearTray;
  FPositionX := 0;
  FPositionY := 0;
  FPositionW := 350;
  FPositionH := 400;
end;

function TMockPositionConfig.GetPositionMode: TPositionMode;
begin
  Result := FPositionMode;
end;

function TMockPositionConfig.GetPositionX: Integer;
begin
  Result := FPositionX;
end;

function TMockPositionConfig.GetPositionY: Integer;
begin
  Result := FPositionY;
end;

function TMockPositionConfig.GetPositionW: Integer;
begin
  Result := FPositionW;
end;

function TMockPositionConfig.GetPositionH: Integer;
begin
  Result := FPositionH;
end;

procedure TMockPositionConfig.SetPositionMode(AValue: TPositionMode);
begin
  FPositionMode := AValue;
end;

procedure TMockPositionConfig.SetPositionX(AValue: Integer);
begin
  FPositionX := AValue;
end;

procedure TMockPositionConfig.SetPositionY(AValue: Integer);
begin
  FPositionY := AValue;
end;

procedure TMockPositionConfig.SetPositionW(AValue: Integer);
begin
  FPositionW := AValue;
end;

procedure TMockPositionConfig.SetPositionH(AValue: Integer);
begin
  FPositionH := AValue;
end;

{ TMockHotkeyConfig }

constructor TMockHotkeyConfig.Create;
begin
  inherited Create;
  FHotkey := '';
  FUseLowLevelHook := False;
  FCastPanelHotkey := '';
  FBluetoothPanelHotkey := '';
end;

function TMockHotkeyConfig.GetHotkey: string;
begin
  Result := FHotkey;
end;

function TMockHotkeyConfig.GetUseLowLevelHook: Boolean;
begin
  Result := FUseLowLevelHook;
end;

function TMockHotkeyConfig.GetCastPanelHotkey: string;
begin
  Result := FCastPanelHotkey;
end;

function TMockHotkeyConfig.GetBluetoothPanelHotkey: string;
begin
  Result := FBluetoothPanelHotkey;
end;

procedure TMockHotkeyConfig.SetHotkey(const AValue: string);
begin
  FHotkey := AValue;
end;

procedure TMockHotkeyConfig.SetUseLowLevelHook(AValue: Boolean);
begin
  FUseLowLevelHook := AValue;
end;

procedure TMockHotkeyConfig.SetCastPanelHotkey(const AValue: string);
begin
  FCastPanelHotkey := AValue;
end;

procedure TMockHotkeyConfig.SetBluetoothPanelHotkey(const AValue: string);
begin
  FBluetoothPanelHotkey := AValue;
end;

{ TMockNotificationConfig }

constructor TMockNotificationConfig.Create;
begin
  inherited Create;
  FNotifyOnConnect := nmNone;
  FNotifyOnDisconnect := nmNone;
  FNotifyOnConnectFailed := nmNone;
  FNotifyOnAutoConnect := nmNone;
end;

function TMockNotificationConfig.GetNotifyOnConnect: TNotificationMode;
begin
  Result := FNotifyOnConnect;
end;

function TMockNotificationConfig.GetNotifyOnDisconnect: TNotificationMode;
begin
  Result := FNotifyOnDisconnect;
end;

function TMockNotificationConfig.GetNotifyOnConnectFailed: TNotificationMode;
begin
  Result := FNotifyOnConnectFailed;
end;

function TMockNotificationConfig.GetNotifyOnAutoConnect: TNotificationMode;
begin
  Result := FNotifyOnAutoConnect;
end;

procedure TMockNotificationConfig.SetNotifyOnConnect(AValue: TNotificationMode);
begin
  FNotifyOnConnect := AValue;
end;

procedure TMockNotificationConfig.SetNotifyOnDisconnect(AValue: TNotificationMode);
begin
  FNotifyOnDisconnect := AValue;
end;

procedure TMockNotificationConfig.SetNotifyOnConnectFailed(AValue: TNotificationMode);
begin
  FNotifyOnConnectFailed := AValue;
end;

procedure TMockNotificationConfig.SetNotifyOnAutoConnect(AValue: TNotificationMode);
begin
  FNotifyOnAutoConnect := AValue;
end;

{ TMockProfileConfig }

constructor TMockProfileConfig.Create;
begin
  inherited Create;
  FShowProfiles := False;  // Default to False for tests
  FProfileFontSize := 7;
end;

function TMockProfileConfig.GetShowProfiles: Boolean;
begin
  Result := FShowProfiles;
end;

function TMockProfileConfig.GetProfileFontSize: Integer;
begin
  Result := FProfileFontSize;
end;

procedure TMockProfileConfig.SetShowProfiles(AValue: Boolean);
begin
  FShowProfiles := AValue;
end;

procedure TMockProfileConfig.SetProfileFontSize(AValue: Integer);
begin
  FProfileFontSize := AValue;
end;

{ TMockLogConfig }

constructor TMockLogConfig.Create;
begin
  inherited Create;
  FLogEnabled := False;
  FLogFilename := 'bqc.log';
  FLogAppend := True;
  FLogLevel := llInfo;
end;

function TMockLogConfig.GetLogEnabled: Boolean;
begin
  Result := FLogEnabled;
end;

function TMockLogConfig.GetLogFilename: string;
begin
  Result := FLogFilename;
end;

function TMockLogConfig.GetLogAppend: Boolean;
begin
  Result := FLogAppend;
end;

function TMockLogConfig.GetLogLevel: TLogLevel;
begin
  Result := FLogLevel;
end;

procedure TMockLogConfig.SetLogEnabled(AValue: Boolean);
begin
  FLogEnabled := AValue;
end;

procedure TMockLogConfig.SetLogFilename(const AValue: string);
begin
  FLogFilename := AValue;
end;

procedure TMockLogConfig.SetLogAppend(AValue: Boolean);
begin
  FLogAppend := AValue;
end;

procedure TMockLogConfig.SetLogLevel(AValue: TLogLevel);
begin
  FLogLevel := AValue;
end;

{ TMockBatteryTrayConfig }

constructor TMockBatteryTrayConfig.Create;
begin
  inherited Create;
  FShowBatteryTrayIcons := True;
  FDefaultIconColor := clGreen;
  FDefaultBackgroundColor := clNone;  // Transparent
  FDefaultShowNumericValue := False;
  FDefaultLowBatteryThreshold := 20;
  FDefaultNotifyLowBattery := True;
  FDefaultNotifyFullyCharged := False;
  FDefaultOutlineColorMode := ocmAuto;
  FDefaultCustomOutlineColor := clBlack;
end;

function TMockBatteryTrayConfig.GetShowBatteryTrayIcons: Boolean;
begin
  Result := FShowBatteryTrayIcons;
end;

function TMockBatteryTrayConfig.GetDefaultIconColor: TColor;
begin
  Result := FDefaultIconColor;
end;

function TMockBatteryTrayConfig.GetDefaultBackgroundColor: TColor;
begin
  Result := FDefaultBackgroundColor;
end;

function TMockBatteryTrayConfig.GetDefaultShowNumericValue: Boolean;
begin
  Result := FDefaultShowNumericValue;
end;

function TMockBatteryTrayConfig.GetDefaultLowBatteryThreshold: Integer;
begin
  Result := FDefaultLowBatteryThreshold;
end;

function TMockBatteryTrayConfig.GetDefaultNotifyLowBattery: Boolean;
begin
  Result := FDefaultNotifyLowBattery;
end;

function TMockBatteryTrayConfig.GetDefaultNotifyFullyCharged: Boolean;
begin
  Result := FDefaultNotifyFullyCharged;
end;

procedure TMockBatteryTrayConfig.SetShowBatteryTrayIcons(AValue: Boolean);
begin
  FShowBatteryTrayIcons := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultIconColor(AValue: TColor);
begin
  FDefaultIconColor := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultBackgroundColor(AValue: TColor);
begin
  FDefaultBackgroundColor := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultShowNumericValue(AValue: Boolean);
begin
  FDefaultShowNumericValue := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultLowBatteryThreshold(AValue: Integer);
begin
  FDefaultLowBatteryThreshold := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultNotifyLowBattery(AValue: Boolean);
begin
  FDefaultNotifyLowBattery := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultNotifyFullyCharged(AValue: Boolean);
begin
  FDefaultNotifyFullyCharged := AValue;
end;

function TMockBatteryTrayConfig.GetDefaultOutlineColorMode: TOutlineColorMode;
begin
  Result := FDefaultOutlineColorMode;
end;

function TMockBatteryTrayConfig.GetDefaultCustomOutlineColor: TColor;
begin
  Result := FDefaultCustomOutlineColor;
end;

procedure TMockBatteryTrayConfig.SetDefaultOutlineColorMode(AValue: TOutlineColorMode);
begin
  FDefaultOutlineColorMode := AValue;
end;

procedure TMockBatteryTrayConfig.SetDefaultCustomOutlineColor(AValue: TColor);
begin
  FDefaultCustomOutlineColor := AValue;
end;

{ TMockAppConfig }

constructor TMockAppConfig.Create;
begin
  inherited Create;
  FModified := False;
  FSaveIfModifiedCount := 0;
  FClearModifiedCount := 0;
  FSaveCount := 0;
  FLoadCount := 0;

  // Create sub-interface mocks
  FGeneralConfig := TMockGeneralConfig.Create;
  FWindowConfig := TMockWindowConfig.Create;
  FPositionConfig := TMockPositionConfig.Create;
  FHotkeyConfig := TMockHotkeyConfig.Create;
  FPollingConfig := TMockPollingConfig.Create;
  FLogConfig := TMockLogConfig.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FLayoutConfig := TMockLayoutConfig.Create;
  FConnectionConfig := TMockConnectionConfig.Create;
  FNotificationConfig := TMockNotificationConfig.Create;
  FBatteryTrayConfig := TMockBatteryTrayConfig.Create;
  FProfileConfig := TMockProfileConfig.Create;
  FDeviceConfigProvider := TMockDeviceConfigProvider.Create;
end;

function TMockAppConfig.GetConfigPath: string;
begin
  Result := 'mock_config.ini';
end;

function TMockAppConfig.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TMockAppConfig.Load;
begin
  Inc(FLoadCount);
end;

procedure TMockAppConfig.Save;
begin
  Inc(FSaveCount);
  FModified := False;
end;

procedure TMockAppConfig.SaveIfModified;
begin
  Inc(FSaveIfModifiedCount);
  if FModified then
    FModified := False;
end;

procedure TMockAppConfig.ClearModified;
begin
  FModified := False;
  Inc(FClearModifiedCount);
end;

function TMockAppConfig.AsGeneralConfig: IGeneralConfig;
begin
  Result := FGeneralConfig;
end;

function TMockAppConfig.AsWindowConfig: IWindowConfig;
begin
  Result := FWindowConfig;
end;

function TMockAppConfig.AsPositionConfig: IPositionConfig;
begin
  Result := FPositionConfig;
end;

function TMockAppConfig.AsHotkeyConfig: IHotkeyConfig;
begin
  Result := FHotkeyConfig;
end;

function TMockAppConfig.AsPollingConfig: IPollingConfig;
begin
  Result := FPollingConfig;
end;

function TMockAppConfig.AsLogConfig: ILogConfig;
begin
  Result := FLogConfig;
end;

function TMockAppConfig.AsAppearanceConfig: IAppearanceConfig;
begin
  Result := FAppearanceConfig;
end;

function TMockAppConfig.AsLayoutConfig: ILayoutConfig;
begin
  Result := FLayoutConfig;
end;

function TMockAppConfig.AsConnectionConfig: IConnectionConfig;
begin
  Result := FConnectionConfig;
end;

function TMockAppConfig.AsNotificationConfig: INotificationConfig;
begin
  Result := FNotificationConfig;
end;

function TMockAppConfig.AsBatteryTrayConfig: IBatteryTrayConfig;
begin
  Result := FBatteryTrayConfig;
end;

function TMockAppConfig.AsProfileConfig: IProfileConfig;
begin
  Result := FProfileConfig;
end;

function TMockAppConfig.AsDeviceConfigProvider: IDeviceConfigProvider;
begin
  Result := FDeviceConfigProvider;
end;

end.
