{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Strategy Implementations             }
{                                                       }
{       Implements Strategy Pattern for device-specific }
{       connection logic.                               }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.ConnectionStrategies;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Base class for connection strategies.
  /// Template Method Pattern: Provides common structure.
  /// </summary>
  TBaseConnectionStrategy = class(TInterfacedObject, IConnectionStrategy)
  protected
    FPriority: Integer;
    FSupportedTypes: TArray<TBluetoothDeviceType>;
    FServiceGuids: TArray<TGUID>;
  public
    function CanHandle(ADeviceType: TBluetoothDeviceType): Boolean; virtual;
    function GetServiceGuids: TArray<TGUID>; virtual;
    function GetPriority: Integer; virtual;
  end;

  /// <summary>
  /// Strategy for audio devices (headphones, speakers, headsets).
  /// Uses A2DP and HFP profiles.
  /// </summary>
  TAudioConnectionStrategy = class(TBaseConnectionStrategy)
  public
    constructor Create;
  end;

  /// <summary>
  /// Strategy for HID devices (keyboards, mice, gamepads).
  /// Uses HID profile.
  /// </summary>
  THIDConnectionStrategy = class(TBaseConnectionStrategy)
  public
    constructor Create;
  end;

  /// <summary>
  /// Fallback strategy for unknown devices.
  /// Attempts common profiles.
  /// </summary>
  TGenericConnectionStrategy = class(TBaseConnectionStrategy)
  public
    constructor Create;
    function CanHandle(ADeviceType: TBluetoothDeviceType): Boolean; override;
  end;

  /// <summary>
  /// Factory for creating and managing connection strategies.
  /// Implements Factory Pattern.
  /// </summary>
  TConnectionStrategyFactory = class
  private
    class var FStrategies: TList<IConnectionStrategy>;
    class procedure EnsureInitialized;
  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>
    /// Gets the appropriate strategy for a device type.
    /// </summary>
    /// <param name="ADeviceType">The device type.</param>
    /// <returns>Best matching strategy.</returns>
    class function GetStrategy(ADeviceType: TBluetoothDeviceType): IConnectionStrategy;

    /// <summary>
    /// Registers a custom strategy.
    /// </summary>
    /// <param name="AStrategy">The strategy to register.</param>
    class procedure RegisterStrategy(AStrategy: IConnectionStrategy);
  end;

implementation

{ TBaseConnectionStrategy }

function TBaseConnectionStrategy.CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
var
  DevType: TBluetoothDeviceType;
begin
  for DevType in FSupportedTypes do
    if DevType = ADeviceType then
      Exit(True);
  Result := False;
end;

function TBaseConnectionStrategy.GetServiceGuids: TArray<TGUID>;
begin
  Result := FServiceGuids;
end;

function TBaseConnectionStrategy.GetPriority: Integer;
begin
  Result := FPriority;
end;

{ TAudioConnectionStrategy }

constructor TAudioConnectionStrategy.Create;
begin
  inherited Create;
  FPriority := 100;
  FSupportedTypes := [btAudioOutput, btAudioInput, btHeadset];
  // A2DP Audio Sink, Handsfree, Headset profiles
  FServiceGuids := [
    AudioSinkServiceClass_UUID,
    HandsfreeServiceClass_UUID,
    HeadsetServiceClass_UUID,
    AVRemoteControlServiceClass_UUID
  ];
end;

{ THIDConnectionStrategy }

constructor THIDConnectionStrategy.Create;
begin
  inherited Create;
  FPriority := 100;
  FSupportedTypes := [btKeyboard, btMouse, btGamepad, btHID];
  // HID profile
  FServiceGuids := [
    HumanInterfaceDeviceServiceClass_UUID
  ];
end;

{ TGenericConnectionStrategy }

constructor TGenericConnectionStrategy.Create;
begin
  inherited Create;
  FPriority := 0; // Lowest priority - fallback
  FSupportedTypes := []; // Will handle any via CanHandle override
  // Try common profiles
  FServiceGuids := [
    AudioSinkServiceClass_UUID,
    HandsfreeServiceClass_UUID,
    HumanInterfaceDeviceServiceClass_UUID
  ];
end;

function TGenericConnectionStrategy.CanHandle(ADeviceType: TBluetoothDeviceType): Boolean;
begin
  // Generic strategy handles everything as a fallback
  Result := True;
end;

{ TConnectionStrategyFactory }

class constructor TConnectionStrategyFactory.Create;
begin
  FStrategies := nil;
end;

class destructor TConnectionStrategyFactory.Destroy;
begin
  FreeAndNil(FStrategies);
end;

class procedure TConnectionStrategyFactory.EnsureInitialized;
begin
  if FStrategies = nil then
  begin
    FStrategies := TList<IConnectionStrategy>.Create;
    // Register default strategies
    FStrategies.Add(TAudioConnectionStrategy.Create);
    FStrategies.Add(THIDConnectionStrategy.Create);
    FStrategies.Add(TGenericConnectionStrategy.Create);
  end;
end;

class function TConnectionStrategyFactory.GetStrategy(
  ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
var
  Strategy: IConnectionStrategy;
  BestStrategy: IConnectionStrategy;
  BestPriority: Integer;
begin
  EnsureInitialized;

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

class procedure TConnectionStrategyFactory.RegisterStrategy(
  AStrategy: IConnectionStrategy);
begin
  EnsureInitialized;
  FStrategies.Add(AStrategy);
end;

end.
