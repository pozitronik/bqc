{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Configuration Repository                 }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Handles per-device configuration storage and retrieval.
/// Separates device configuration from main application settings.
/// </summary>
unit App.DeviceConfigRepository;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.DateUtils,
  System.Generics.Collections,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Repository for per-device configuration.
  /// Manages device-specific settings with INI persistence.
  /// </summary>
  TDeviceConfigRepository = class(TInterfacedObject, IDeviceConfigRepository)
  private
    FDevices: TDictionary<UInt64, TDeviceConfig>;
    FModified: Boolean;
    FGlobalConfig: TObject;  // Reference to TAppConfig for effective values

    procedure SetGlobalConfig(AValue: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    // IDeviceConfigRepository
    function GetConfig(AAddress: UInt64): TDeviceConfig;
    procedure SetConfig(const AConfig: TDeviceConfig);
    procedure Remove(AAddress: UInt64);
    procedure RegisterDevice(AAddress: UInt64; const AName: string; ALastSeen: TDateTime);
    function GetAllAddresses: TArray<UInt64>;
    function GetAll: TArray<TDeviceConfig>;
    procedure LoadFrom(AIni: TObject);
    procedure SaveTo(AIni: TObject);
    function IsModified: Boolean;
    procedure ClearModified;

    // Effective value resolution (uses global defaults from TAppConfig)
    function GetEffectiveNotification(AAddress: UInt64; AEvent: TNotificationEvent): TNotificationMode;
    function GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
    function GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;

    property GlobalConfig: TObject read FGlobalConfig write SetGlobalConfig;
  end;

const
  // Device section prefix
  SEC_DEVICE_PREFIX = 'Device.';

  // Per-device INI keys
  KEY_NAME = 'Name';
  KEY_ALIAS = 'Alias';
  KEY_PINNED = 'Pinned';
  KEY_HIDDEN = 'Hidden';
  KEY_AUTO_CONNECT = 'AutoConnect';
  KEY_CONNECTION_TIMEOUT = 'ConnectionTimeout';
  KEY_CONNECTION_RETRY_COUNT = 'ConnectionRetryCount';
  KEY_NOTIFY_ON_CONNECT = 'NotifyOnConnect';
  KEY_NOTIFY_ON_DISCONNECT = 'NotifyOnDisconnect';
  KEY_NOTIFY_ON_CONNECT_FAILED = 'NotifyOnConnectFailed';
  KEY_NOTIFY_ON_AUTO_CONNECT = 'NotifyOnAutoConnect';
  KEY_DEVICE_TYPE_OVERRIDE = 'DeviceTypeOverride';
  KEY_LAST_SEEN = 'LastSeen';

/// <summary>
/// Creates a device configuration repository.
/// </summary>
function CreateDeviceConfigRepository: IDeviceConfigRepository;

implementation

uses
  App.Logger,
  App.Config;

function CreateDeviceConfigRepository: IDeviceConfigRepository;
begin
  Result := TDeviceConfigRepository.Create;
end;

{ TDeviceConfigRepository }

constructor TDeviceConfigRepository.Create;
begin
  inherited Create;
  FDevices := TDictionary<UInt64, TDeviceConfig>.Create;
  FModified := False;
  FGlobalConfig := nil;
end;

destructor TDeviceConfigRepository.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

procedure TDeviceConfigRepository.SetGlobalConfig(AValue: TObject);
begin
  FGlobalConfig := AValue;
end;

function TDeviceConfigRepository.GetConfig(AAddress: UInt64): TDeviceConfig;
begin
  if not FDevices.TryGetValue(AAddress, Result) then
    Result := TDeviceConfig.Default(AAddress);
end;

procedure TDeviceConfigRepository.SetConfig(const AConfig: TDeviceConfig);
begin
  FDevices.AddOrSetValue(AConfig.Address, AConfig);
  FModified := True;
end;

procedure TDeviceConfigRepository.Remove(AAddress: UInt64);
begin
  if FDevices.ContainsKey(AAddress) then
  begin
    FDevices.Remove(AAddress);
    FModified := True;
  end;
end;

procedure TDeviceConfigRepository.RegisterDevice(AAddress: UInt64;
  const AName: string; ALastSeen: TDateTime);
var
  DeviceConfig: TDeviceConfig;
  IsNew: Boolean;
begin
  IsNew := not FDevices.TryGetValue(AAddress, DeviceConfig);

  if IsNew then
  begin
    // Create new device config with defaults
    DeviceConfig := TDeviceConfig.Default(AAddress);
    DeviceConfig.Name := AName;
    DeviceConfig.LastSeen := ALastSeen;
    FDevices.Add(AAddress, DeviceConfig);
    FModified := True;
    Log('RegisterDevice: New device registered: %s ($%.12X)', [AName, AAddress], ClassName);
  end
  else
  begin
    // Update existing device: always update LastSeen, update Name if changed
    if (DeviceConfig.Name <> AName) or (DeviceConfig.LastSeen < ALastSeen) then
    begin
      if (AName <> '') and (DeviceConfig.Name <> AName) then
        DeviceConfig.Name := AName;
      DeviceConfig.LastSeen := ALastSeen;
      FDevices[AAddress] := DeviceConfig;
      FModified := True;
    end;
  end;
end;

function TDeviceConfigRepository.GetAllAddresses: TArray<UInt64>;
begin
  Result := FDevices.Keys.ToArray;
end;

function TDeviceConfigRepository.GetAll: TArray<TDeviceConfig>;
begin
  Result := FDevices.Values.ToArray;
end;

function TDeviceConfigRepository.IsModified: Boolean;
begin
  Result := FModified;
end;

procedure TDeviceConfigRepository.ClearModified;
begin
  FModified := False;
end;

procedure TDeviceConfigRepository.LoadFrom(AIni: TObject);
var
  Ini: TMemIniFile;
  Sections: TStringList;
  Section: string;
  AddressStr: string;
  Address: UInt64;
  DeviceConfig: TDeviceConfig;
  LastSeenStr: string;
begin
  Ini := TMemIniFile(AIni);
  FDevices.Clear;
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
      begin
        AddressStr := Section.Substring(Length(SEC_DEVICE_PREFIX));
        // Support both formats: 581862015DAE and 58:18:62:01:5D:AE
        AddressStr := StringReplace(AddressStr, ':', '', [rfReplaceAll]);
        if TryStrToUInt64('$' + AddressStr, Address) then
        begin
          DeviceConfig := TDeviceConfig.Default(Address);
          DeviceConfig.Name := Ini.ReadString(Section, KEY_NAME, '');
          DeviceConfig.Alias := Ini.ReadString(Section, KEY_ALIAS, '');
          DeviceConfig.Pinned := Ini.ReadBool(Section, KEY_PINNED, False);
          DeviceConfig.Hidden := Ini.ReadBool(Section, KEY_HIDDEN, False);
          DeviceConfig.AutoConnect := Ini.ReadBool(Section, KEY_AUTO_CONNECT, False);
          DeviceConfig.ConnectionTimeout := Ini.ReadInteger(Section, KEY_CONNECTION_TIMEOUT, -1);
          DeviceConfig.ConnectionRetryCount := Ini.ReadInteger(Section, KEY_CONNECTION_RETRY_COUNT, -1);
          // Per-device notification overrides (-1 = use global)
          DeviceConfig.Notifications.OnConnect := Ini.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT, -1);
          DeviceConfig.Notifications.OnDisconnect := Ini.ReadInteger(Section, KEY_NOTIFY_ON_DISCONNECT, -1);
          DeviceConfig.Notifications.OnConnectFailed := Ini.ReadInteger(Section, KEY_NOTIFY_ON_CONNECT_FAILED, -1);
          DeviceConfig.Notifications.OnAutoConnect := Ini.ReadInteger(Section, KEY_NOTIFY_ON_AUTO_CONNECT, -1);
          DeviceConfig.DeviceTypeOverride := Ini.ReadInteger(Section, KEY_DEVICE_TYPE_OVERRIDE, -1);
          // Parse LastSeen as ISO 8601 datetime string
          LastSeenStr := Ini.ReadString(Section, KEY_LAST_SEEN, '');
          if LastSeenStr <> '' then
          begin
            try
              DeviceConfig.LastSeen := ISO8601ToDate(LastSeenStr, False);
            except
              DeviceConfig.LastSeen := 0;
            end;
          end;
          FDevices.Add(Address, DeviceConfig);
        end;
      end;
    end;
  finally
    Sections.Free;
  end;
  FModified := False;
end;

procedure TDeviceConfigRepository.SaveTo(AIni: TObject);
var
  Ini: TMemIniFile;
  Sections: TStringList;
  Section: string;
  Pair: TPair<UInt64, TDeviceConfig>;
  SectionName: string;
begin
  Ini := TMemIniFile(AIni);

  // First, remove all existing device sections
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
    begin
      if Section.StartsWith(SEC_DEVICE_PREFIX) then
        Ini.EraseSection(Section);
    end;
  finally
    Sections.Free;
  end;

  // Write current device configurations
  for Pair in FDevices do
  begin
    SectionName := SEC_DEVICE_PREFIX + IntToHex(Pair.Key, 12);
    // Always save Name (original device name from Windows)
    Ini.WriteString(SectionName, KEY_NAME, Pair.Value.Name);
    Ini.WriteString(SectionName, KEY_ALIAS, Pair.Value.Alias);
    Ini.WriteBool(SectionName, KEY_PINNED, Pair.Value.Pinned);
    Ini.WriteBool(SectionName, KEY_HIDDEN, Pair.Value.Hidden);
    Ini.WriteBool(SectionName, KEY_AUTO_CONNECT, Pair.Value.AutoConnect);
    // Only save connection settings if they override defaults
    if Pair.Value.ConnectionTimeout >= 0 then
      Ini.WriteInteger(SectionName, KEY_CONNECTION_TIMEOUT, Pair.Value.ConnectionTimeout);
    if Pair.Value.ConnectionRetryCount >= 0 then
      Ini.WriteInteger(SectionName, KEY_CONNECTION_RETRY_COUNT, Pair.Value.ConnectionRetryCount);
    // Only save notification settings if they override globals
    if Pair.Value.Notifications.OnConnect >= 0 then
      Ini.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT, Pair.Value.Notifications.OnConnect);
    if Pair.Value.Notifications.OnDisconnect >= 0 then
      Ini.WriteInteger(SectionName, KEY_NOTIFY_ON_DISCONNECT, Pair.Value.Notifications.OnDisconnect);
    if Pair.Value.Notifications.OnConnectFailed >= 0 then
      Ini.WriteInteger(SectionName, KEY_NOTIFY_ON_CONNECT_FAILED, Pair.Value.Notifications.OnConnectFailed);
    if Pair.Value.Notifications.OnAutoConnect >= 0 then
      Ini.WriteInteger(SectionName, KEY_NOTIFY_ON_AUTO_CONNECT, Pair.Value.Notifications.OnAutoConnect);
    // Only save DeviceTypeOverride if it's set (not auto-detect)
    if Pair.Value.DeviceTypeOverride >= 0 then
      Ini.WriteInteger(SectionName, KEY_DEVICE_TYPE_OVERRIDE, Pair.Value.DeviceTypeOverride);
    // Save LastSeen as ISO 8601 datetime string
    if Pair.Value.LastSeen > 0 then
      Ini.WriteString(SectionName, KEY_LAST_SEEN, DateToISO8601(Pair.Value.LastSeen, False));
  end;
  FModified := False;
end;

function TDeviceConfigRepository.GetEffectiveNotification(AAddress: UInt64;
  AEvent: TNotificationEvent): TNotificationMode;
var
  DeviceConfig: TDeviceConfig;
  DeviceValue: Integer;
  Cfg: TAppConfig;
begin
  // Get per-device config if exists
  DeviceConfig := GetConfig(AAddress);

  // Get the per-device override value for this event
  case AEvent of
    neConnect:
      DeviceValue := DeviceConfig.Notifications.OnConnect;
    neDisconnect:
      DeviceValue := DeviceConfig.Notifications.OnDisconnect;
    neConnectFailed:
      DeviceValue := DeviceConfig.Notifications.OnConnectFailed;
    neAutoConnect:
      DeviceValue := DeviceConfig.Notifications.OnAutoConnect;
  else
    DeviceValue := -1;
  end;

  // If per-device value is set (>= 0), use it; otherwise use global
  if DeviceValue >= 0 then
    Result := TNotificationMode(DeviceValue)
  else if Assigned(FGlobalConfig) then
  begin
    Cfg := TAppConfig(FGlobalConfig);
    case AEvent of
      neConnect:
        Result := Cfg.NotifyOnConnect;
      neDisconnect:
        Result := Cfg.NotifyOnDisconnect;
      neConnectFailed:
        Result := Cfg.NotifyOnConnectFailed;
      neAutoConnect:
        Result := Cfg.NotifyOnAutoConnect;
    else
      Result := nmNone;
    end;
  end
  else
    Result := nmNone;
end;

function TDeviceConfigRepository.GetEffectiveConnectionTimeout(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetConfig(AAddress);
  if DeviceConfig.ConnectionTimeout >= 0 then
    Result := DeviceConfig.ConnectionTimeout
  else if Assigned(FGlobalConfig) then
    Result := TAppConfig(FGlobalConfig).ConnectionTimeout
  else
    Result := 10000;  // Default
end;

function TDeviceConfigRepository.GetEffectiveConnectionRetryCount(AAddress: UInt64): Integer;
var
  DeviceConfig: TDeviceConfig;
begin
  DeviceConfig := GetConfig(AAddress);
  if DeviceConfig.ConnectionRetryCount >= 0 then
    Result := DeviceConfig.ConnectionRetryCount
  else if Assigned(FGlobalConfig) then
    Result := TAppConfig(FGlobalConfig).ConnectionRetryCount
  else
    Result := 2;  // Default
end;

end.
