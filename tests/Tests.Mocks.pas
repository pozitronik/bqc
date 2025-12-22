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
  /// Mock implementation of IDeviceMonitor for testing.
  /// </summary>
  TMockDeviceMonitor = class(TInterfacedObject, IDeviceMonitor)
  private
    FRunning: Boolean;
    FOnDeviceStateChanged: TMonitorDeviceStateEvent;
    FOnError: TMonitorErrorEvent;
    FStartResult: Boolean;
    FStartCallCount: Integer;
    FStopCallCount: Integer;
    function GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
    procedure SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
    function GetOnError: TMonitorErrorEvent;
    procedure SetOnError(AValue: TMonitorErrorEvent);
  public
    constructor Create;

    // IDeviceMonitor
    function Start: Boolean;
    procedure Stop;
    function IsRunning: Boolean;

    // Test helpers
    procedure SimulateDeviceStateChanged(AAddress: UInt64; AState: TBluetoothConnectionState);
    procedure SimulateError(const AMessage: string; AErrorCode: Cardinal);

    property StartResult: Boolean read FStartResult write FStartResult;
    property StartCallCount: Integer read FStartCallCount;
    property StopCallCount: Integer read FStopCallCount;
    property OnDeviceStateChanged: TMonitorDeviceStateEvent read GetOnDeviceStateChanged write SetOnDeviceStateChanged;
    property OnError: TMonitorErrorEvent read GetOnError write SetOnError;
  end;

  /// <summary>
  /// Mock implementation of IDeviceRepository for testing.
  /// </summary>
  TMockDeviceRepository = class(TInterfacedObject, IDeviceRepository)
  private
    FDevices: TDictionary<UInt64, TBluetoothDeviceInfo>;
    FOnListChanged: TDeviceListChangedEvent;
    FRefreshCallCount: Integer;
    function GetOnListChanged: TDeviceListChangedEvent;
    procedure SetOnListChanged(AValue: TDeviceListChangedEvent);
  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceRepository
    function GetAll: TBluetoothDeviceInfoArray;
    function GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
    function TryGetByAddress(AAddress: UInt64; out ADevice: TBluetoothDeviceInfo): Boolean;
    function Contains(AAddress: UInt64): Boolean;
    procedure AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
    function UpdateConnectionState(AAddress: UInt64;
      AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
    procedure Remove(AAddress: UInt64);
    procedure Clear;
    procedure Refresh;
    function GetCount: Integer;

    property RefreshCallCount: Integer read FRefreshCallCount;
    property Devices: TDictionary<UInt64, TBluetoothDeviceInfo> read FDevices;
    property Count: Integer read GetCount;
    property OnListChanged: TDeviceListChangedEvent read GetOnListChanged write SetOnListChanged;
  end;

  /// <summary>
  /// Mock implementation of IConnectionExecutor for testing.
  /// </summary>
  TMockConnectionExecutor = class(TInterfacedObject, IConnectionExecutor)
  private
    FExecuteResult: TConnectionResult;
    FExecuteCallCount: Integer;
    FLastDevice: TBluetoothDeviceInfo;
    FLastEnable: Boolean;
    FLastRetryCount: Integer;
  public
    constructor Create;

    // IConnectionExecutor
    function Execute(
      const ADevice: TBluetoothDeviceInfo;
      const AServiceGuids: TArray<TGUID>;
      AEnable: Boolean;
      ARetryCount: Integer
    ): TConnectionResult;

    property ExecuteResult: TConnectionResult read FExecuteResult write FExecuteResult;
    property ExecuteCallCount: Integer read FExecuteCallCount;
    property LastDevice: TBluetoothDeviceInfo read FLastDevice;
    property LastEnable: Boolean read FLastEnable;
    property LastRetryCount: Integer read FLastRetryCount;
  end;

  /// <summary>
  /// Mock implementation of IBluetoothAdapterQuery for testing.
  /// </summary>
  TMockAdapterQuery = class(TInterfacedObject, IBluetoothAdapterQuery)
  private
    FAdapterAvailable: Boolean;
    FAdapterName: string;
    FIsAdapterAvailableCallCount: Integer;
    FGetAdapterNameCallCount: Integer;
  public
    constructor Create;

    // IBluetoothAdapterQuery
    function IsAdapterAvailable: Boolean;
    function GetAdapterName: string;

    property AdapterAvailable: Boolean read FAdapterAvailable write FAdapterAvailable;
    property AdapterName: string read FAdapterName write FAdapterName;
    property IsAdapterAvailableCallCount: Integer read FIsAdapterAvailableCallCount;
    property GetAdapterNameCallCount: Integer read FGetAdapterNameCallCount;
  end;

/// <summary>
/// Creates a TBluetoothDeviceInfo with minimal parameters for testing.
/// </summary>
function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;

type
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

{ TMockDeviceMonitor }

constructor TMockDeviceMonitor.Create;
begin
  inherited Create;
  FRunning := False;
  FStartResult := True;
  FStartCallCount := 0;
  FStopCallCount := 0;
end;

function TMockDeviceMonitor.Start: Boolean;
begin
  Inc(FStartCallCount);
  if FStartResult then
    FRunning := True;
  Result := FStartResult;
end;

procedure TMockDeviceMonitor.Stop;
begin
  Inc(FStopCallCount);
  FRunning := False;
end;

function TMockDeviceMonitor.IsRunning: Boolean;
begin
  Result := FRunning;
end;

function TMockDeviceMonitor.GetOnDeviceStateChanged: TMonitorDeviceStateEvent;
begin
  Result := FOnDeviceStateChanged;
end;

procedure TMockDeviceMonitor.SetOnDeviceStateChanged(AValue: TMonitorDeviceStateEvent);
begin
  FOnDeviceStateChanged := AValue;
end;

function TMockDeviceMonitor.GetOnError: TMonitorErrorEvent;
begin
  Result := FOnError;
end;

procedure TMockDeviceMonitor.SetOnError(AValue: TMonitorErrorEvent);
begin
  FOnError := AValue;
end;

procedure TMockDeviceMonitor.SimulateDeviceStateChanged(AAddress: UInt64;
  AState: TBluetoothConnectionState);
begin
  if Assigned(FOnDeviceStateChanged) then
    FOnDeviceStateChanged(Self, AAddress, AState);
end;

procedure TMockDeviceMonitor.SimulateError(const AMessage: string; AErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage, AErrorCode);
end;

{ TMockDeviceRepository }

constructor TMockDeviceRepository.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TBluetoothDeviceInfo>.Create;
  FRefreshCallCount := 0;
end;

destructor TMockDeviceRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

function TMockDeviceRepository.GetAll: TBluetoothDeviceInfoArray;
begin
  Result := FDevices.Values.ToArray;
end;

function TMockDeviceRepository.GetByAddress(AAddress: UInt64): TBluetoothDeviceInfo;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    FillChar(Result, SizeOf(Result), 0);
end;

function TMockDeviceRepository.TryGetByAddress(AAddress: UInt64;
  out ADevice: TBluetoothDeviceInfo): Boolean;
begin
  Result := FDevices.TryGetValue(AAddress, ADevice);
end;

function TMockDeviceRepository.Contains(AAddress: UInt64): Boolean;
begin
  Result := FDevices.ContainsKey(AAddress);
end;

procedure TMockDeviceRepository.AddOrUpdate(const ADevice: TBluetoothDeviceInfo);
var
  IsNew: Boolean;
begin
  IsNew := not FDevices.ContainsKey(ADevice.AddressInt);
  FDevices.AddOrSetValue(ADevice.AddressInt, ADevice);
  if IsNew and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

function TMockDeviceRepository.UpdateConnectionState(AAddress: UInt64;
  AState: TBluetoothConnectionState): TBluetoothDeviceInfo;
var
  Device: TBluetoothDeviceInfo;
begin
  if FDevices.TryGetValue(AAddress, Device) then
  begin
    Result := Device.WithConnectionState(AState);
    FDevices[AAddress] := Result;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TMockDeviceRepository.Remove(AAddress: UInt64);
var
  Existed: Boolean;
begin
  Existed := FDevices.ContainsKey(AAddress);
  FDevices.Remove(AAddress);
  if Existed and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

procedure TMockDeviceRepository.Clear;
var
  HadDevices: Boolean;
begin
  HadDevices := FDevices.Count > 0;
  FDevices.Clear;
  if HadDevices and Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

procedure TMockDeviceRepository.Refresh;
begin
  Inc(FRefreshCallCount);
  // Mock refresh does nothing - test code adds devices directly
end;

function TMockDeviceRepository.GetCount: Integer;
begin
  Result := FDevices.Count;
end;

function TMockDeviceRepository.GetOnListChanged: TDeviceListChangedEvent;
begin
  Result := FOnListChanged;
end;

procedure TMockDeviceRepository.SetOnListChanged(AValue: TDeviceListChangedEvent);
begin
  FOnListChanged := AValue;
end;

{ TMockConnectionExecutor }

constructor TMockConnectionExecutor.Create;
begin
  inherited Create;
  FExecuteResult := TConnectionResult.Ok;
  FExecuteCallCount := 0;
  FLastEnable := False;
  FLastRetryCount := 0;
end;

function TMockConnectionExecutor.Execute(
  const ADevice: TBluetoothDeviceInfo;
  const AServiceGuids: TArray<TGUID>;
  AEnable: Boolean;
  ARetryCount: Integer
): TConnectionResult;
begin
  Inc(FExecuteCallCount);
  FLastDevice := ADevice;
  FLastEnable := AEnable;
  FLastRetryCount := ARetryCount;
  Result := FExecuteResult;
end;

{ TMockAdapterQuery }

constructor TMockAdapterQuery.Create;
begin
  inherited Create;
  FAdapterAvailable := True;
  FAdapterName := 'Mock Bluetooth Adapter';
  FIsAdapterAvailableCallCount := 0;
  FGetAdapterNameCallCount := 0;
end;

function TMockAdapterQuery.IsAdapterAvailable: Boolean;
begin
  Inc(FIsAdapterAvailableCallCount);
  Result := FAdapterAvailable;
end;

function TMockAdapterQuery.GetAdapterName: string;
begin
  Inc(FGetAdapterNameCallCount);
  Result := FAdapterName;
end;

{ Test Helper Functions }

function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;
begin
  Result := TBluetoothDeviceInfo.Create(
    UInt64ToBluetoothAddress(AAddressInt),
    AAddressInt,
    AName,
    ADeviceType,
    AConnectionState,
    True,   // IsPaired
    False,  // IsAuthenticated
    0,      // ClassOfDevice
    0,      // LastSeen
    0       // LastUsed
  );
end;

end.
