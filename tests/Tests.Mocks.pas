{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations for Unit Testing           }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.Mocks;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  App.ConfigInterfaces,
  Bluetooth.Types,
  Bluetooth.Interfaces;

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
  public
    constructor Create;

    // ILayoutConfig
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

    // Test setup properties
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
  public
    constructor Create;

    // IAppearanceConfig
    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;

    // Test setup properties
    property ShowAddresses: Boolean read FShowAddresses write FShowAddresses;
    property Theme: string read FTheme write FTheme;
    property VsfDir: string read FVsfDir write FVsfDir;
    property ShowLastSeen: Boolean read FShowLastSeen write FShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read FLastSeenFormat write FLastSeenFormat;
    property ShowDeviceIcons: Boolean read FShowDeviceIcons write FShowDeviceIcons;
    property ConnectedColor: Integer read FConnectedColor write FConnectedColor;
  end;

  /// <summary>
  /// Mock implementation of IDeviceConfigProvider for testing.
  /// </summary>
  TMockDeviceConfigProvider = class(TInterfacedObject, IDeviceConfigProvider)
  private
    FDeviceConfigs: TDictionary<UInt64, TDeviceConfig>;
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceConfigProvider
    function GetDeviceConfig(AAddress: UInt64): TDeviceConfig;
    procedure SetDeviceConfig(const AConfig: TDeviceConfig);
    procedure RemoveDeviceConfig(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
    function GetConfiguredDeviceAddresses: TArray<UInt64>;
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;

    // Test helpers
    procedure AddDeviceConfig(AAddress: UInt64; const AConfig: TDeviceConfig);
    procedure Clear;
    property DeviceConfigs: TDictionary<UInt64, TDeviceConfig> read FDeviceConfigs;
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

    // IPollingConfig
    function GetPollingMode: TPollingMode;
    function GetPollingInterval: Integer;
    function GetEventDebounceMs: Integer;

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
  public
    constructor Create;

    // IConnectionConfig
    function GetConnectionTimeout: Integer;
    function GetConnectionRetryCount: Integer;

    // Test setup properties
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
    property ConnectionRetryCount: Integer read FConnectionRetryCount write FConnectionRetryCount;
  end;

  /// <summary>
  /// Mock implementation of IConnectionStrategy for testing.
  /// Allows configuring behavior for specific device types.
  /// </summary>
  TMockConnectionStrategy = class(TInterfacedObject, IConnectionStrategy)
  private
    FPriority: Integer;
    FSupportedTypes: TArray<TBluetoothDeviceType>;
    FServiceGuids: TArray<TGUID>;
    FCanHandleResult: Boolean;
  public
    constructor Create; overload;
    constructor Create(APriority: Integer; ASupportedTypes: TArray<TBluetoothDeviceType>); overload;

    // IConnectionStrategy
    function CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
    function GetServiceGuids: TArray<TGUID>;
    function GetPriority: Integer;

    // Test setup
    property Priority: Integer read FPriority write FPriority;
    property SupportedTypes: TArray<TBluetoothDeviceType> read FSupportedTypes write FSupportedTypes;
    property ServiceGuids: TArray<TGUID> read FServiceGuids write FServiceGuids;
    property CanHandleResult: Boolean read FCanHandleResult write FCanHandleResult;
  end;

  /// <summary>
  /// Mock implementation of IConnectionStrategyFactory for testing.
  /// Allows injecting custom strategies for testing connection behavior.
  /// </summary>
  TMockConnectionStrategyFactory = class(TInterfacedObject, IConnectionStrategyFactory)
  private
    FStrategies: TList<IConnectionStrategy>;
    FGetStrategyCallCount: Integer;
    FLastRequestedDeviceType: TBluetoothDeviceType;
  public
    constructor Create;
    destructor Destroy; override;

    // IConnectionStrategyFactory
    function GetStrategy(ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
    procedure RegisterStrategy(AStrategy: IConnectionStrategy);
    function GetAllStrategies: TArray<IConnectionStrategy>;
    procedure Clear;

    // Test helpers
    property GetStrategyCallCount: Integer read FGetStrategyCallCount;
    property LastRequestedDeviceType: TBluetoothDeviceType read FLastRequestedDeviceType;
    property Strategies: TList<IConnectionStrategy> read FStrategies;
  end;

implementation

{ TMockLayoutConfig }

constructor TMockLayoutConfig.Create;
begin
  inherited Create;
  // Set reasonable defaults
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

{ TMockConnectionConfig }

constructor TMockConnectionConfig.Create;
begin
  inherited Create;
  FConnectionTimeout := 10000;
  FConnectionRetryCount := 2;
end;

function TMockConnectionConfig.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TMockConnectionConfig.GetConnectionRetryCount: Integer;
begin
  Result := FConnectionRetryCount;
end;

{ TMockConnectionStrategy }

constructor TMockConnectionStrategy.Create;
begin
  inherited Create;
  FPriority := 0;
  FSupportedTypes := [];
  FServiceGuids := [];
  FCanHandleResult := True;
end;

constructor TMockConnectionStrategy.Create(APriority: Integer;
  ASupportedTypes: TArray<TBluetoothDeviceType>);
begin
  inherited Create;
  FPriority := APriority;
  FSupportedTypes := ASupportedTypes;
  FServiceGuids := [];
  FCanHandleResult := True;
end;

function TMockConnectionStrategy.CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
var
  DevType: TBluetoothDeviceType;
begin
  // If FCanHandleResult is False, always return False
  if not FCanHandleResult then
    Exit(False);

  // If no specific types configured, use FCanHandleResult
  if Length(FSupportedTypes) = 0 then
    Exit(FCanHandleResult);

  // Check if device type is in supported list
  for DevType in FSupportedTypes do
    if DevType = ADeviceType then
      Exit(True);

  Result := False;
end;

function TMockConnectionStrategy.GetServiceGuids: TArray<TGUID>;
begin
  Result := FServiceGuids;
end;

function TMockConnectionStrategy.GetPriority: Integer;
begin
  Result := FPriority;
end;

{ TMockConnectionStrategyFactory }

constructor TMockConnectionStrategyFactory.Create;
begin
  inherited Create;
  FStrategies := TList<IConnectionStrategy>.Create;
  FGetStrategyCallCount := 0;
  FLastRequestedDeviceType := btUnknown;
end;

destructor TMockConnectionStrategyFactory.Destroy;
begin
  FStrategies.Free;
  inherited Destroy;
end;

function TMockConnectionStrategyFactory.GetStrategy(
  ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
var
  Strategy: IConnectionStrategy;
  BestStrategy: IConnectionStrategy;
  BestPriority: Integer;
begin
  Inc(FGetStrategyCallCount);
  FLastRequestedDeviceType := ADeviceType;

  BestStrategy := nil;
  BestPriority := -1;

  for Strategy in FStrategies do
  begin
    if Strategy.CanHandle(ADeviceType) and (Strategy.GetPriority > BestPriority) then
    begin
      BestStrategy := Strategy;
      BestPriority := Strategy.GetPriority;
    end;
  end;

  Result := BestStrategy;
end;

procedure TMockConnectionStrategyFactory.RegisterStrategy(
  AStrategy: IConnectionStrategy);
begin
  FStrategies.Add(AStrategy);
end;

function TMockConnectionStrategyFactory.GetAllStrategies: TArray<IConnectionStrategy>;
begin
  Result := FStrategies.ToArray;
end;

procedure TMockConnectionStrategyFactory.Clear;
begin
  FStrategies.Clear;
  FGetStrategyCallCount := 0;
end;

end.
