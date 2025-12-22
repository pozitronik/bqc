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
  /// Implements Factory Pattern with instance-based design for DI.
  /// </summary>
  TConnectionStrategyFactory = class(TInterfacedObject, IConnectionStrategyFactory)
  private
    FStrategies: TList<IConnectionStrategy>;
    procedure RegisterDefaultStrategies;
  public
    constructor Create; overload;
    constructor Create(ARegisterDefaults: Boolean); overload;
    destructor Destroy; override;

    // IConnectionStrategyFactory
    function GetStrategy(ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
    procedure RegisterStrategy(AStrategy: IConnectionStrategy);
    function GetAllStrategies: TArray<IConnectionStrategy>;
    procedure Clear;
  end;

  /// <summary>
  /// Creates a default connection strategy factory with standard strategies.
  /// </summary>
function CreateConnectionStrategyFactory: IConnectionStrategyFactory;

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

constructor TConnectionStrategyFactory.Create;
begin
  Create(True);
end;

constructor TConnectionStrategyFactory.Create(ARegisterDefaults: Boolean);
begin
  inherited Create;
  FStrategies := TList<IConnectionStrategy>.Create;
  if ARegisterDefaults then
    RegisterDefaultStrategies;
end;

destructor TConnectionStrategyFactory.Destroy;
begin
  FStrategies.Free;
  inherited Destroy;
end;

procedure TConnectionStrategyFactory.RegisterDefaultStrategies;
begin
  FStrategies.Add(TAudioConnectionStrategy.Create);
  FStrategies.Add(THIDConnectionStrategy.Create);
  FStrategies.Add(TGenericConnectionStrategy.Create);
end;

function TConnectionStrategyFactory.GetStrategy(
  ADeviceType: TBluetoothDeviceType): IConnectionStrategy;
var
  Strategy: IConnectionStrategy;
  BestStrategy: IConnectionStrategy;
  BestPriority: Integer;
begin
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

procedure TConnectionStrategyFactory.RegisterStrategy(
  AStrategy: IConnectionStrategy);
begin
  FStrategies.Add(AStrategy);
end;

function TConnectionStrategyFactory.GetAllStrategies: TArray<IConnectionStrategy>;
begin
  Result := FStrategies.ToArray;
end;

procedure TConnectionStrategyFactory.Clear;
begin
  FStrategies.Clear;
end;

function CreateConnectionStrategyFactory: IConnectionStrategyFactory;
begin
  Result := TConnectionStrategyFactory.Create;
end;

end.
