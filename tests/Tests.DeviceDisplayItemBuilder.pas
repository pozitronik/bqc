{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TDeviceDisplayItemBuilder Tests                 }
{                                                       }
{*******************************************************}

unit Tests.DeviceDisplayItemBuilder;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.BatteryQuery,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.DeviceConfigTypes,
  App.DeviceDisplayTypes,
  App.DeviceDisplayItemBuilder,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TDeviceDisplayItemBuilder class.
  /// Tests building and filtering of display items.
  /// </summary>
  [TestFixture]
  TDeviceDisplayItemBuilderTests = class
  private
    FConfigProvider: TMockDeviceConfigProvider;
    FAppearanceConfig: TMockAppearanceConfig;
    FProfileConfig: TMockProfileConfig;
    FProfileQuery: TMockProfileQuery;
    FBuilder: TDeviceDisplayItemBuilder;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { IsVisible Tests }
    [Test]
    procedure IsVisible_EmptyAPIName_WithConfigName_ReturnsTrue;

    [Test]
    procedure IsVisible_EmptyAPIName_WithAlias_ReturnsTrue;

    [Test]
    procedure IsVisible_EmptyAPIName_EmptyConfigName_EmptyAlias_ReturnsTrue_WithMACFallback;

    [Test]
    procedure IsVisible_HiddenDevice_ReturnsFalse;

    [Test]
    procedure IsVisible_VisibleDevice_ReturnsTrue;

    { BuildDisplayItem Tests }
    [Test]
    procedure BuildDisplayItem_UsesAlias_WhenSet;

    [Test]
    procedure BuildDisplayItem_UsesDeviceName_WhenNoAlias;

    [Test]
    procedure BuildDisplayItem_SetsIsPinned_FromConfig;

    [Test]
    procedure BuildDisplayItem_SetsEffectiveDeviceType_WithOverride;

    [Test]
    procedure BuildDisplayItem_SetsEffectiveDeviceType_WithoutOverride;

    [Test]
    procedure BuildDisplayItem_SetsLastSeenText_Relative;

    [Test]
    procedure BuildDisplayItem_SetsLastSeenText_Absolute;

    [Test]
    procedure BuildDisplayItem_CalculatesSortGroup_Pinned;

    [Test]
    procedure BuildDisplayItem_CalculatesSortGroup_Connected;

    [Test]
    procedure BuildDisplayItem_CalculatesSortGroup_Disconnected;

    { BuildDisplayItems Tests }
    [Test]
    procedure BuildDisplayItems_EmptyInput_ReturnsEmptyArray;

    [Test]
    procedure BuildDisplayItems_FiltersHiddenDevices;

    [Test]
    procedure BuildDisplayItems_FiltersEmptyNames;

    [Test]
    procedure BuildDisplayItems_SortsResult;

    [Test]
    procedure BuildDisplayItems_ProcessesAllVisibleDevices;
  end;

implementation

{ TDeviceDisplayItemBuilderTests }

procedure TDeviceDisplayItemBuilderTests.Setup;
begin
  FConfigProvider := TMockDeviceConfigProvider.Create;
  FAppearanceConfig := TMockAppearanceConfig.Create;
  FProfileConfig := TMockProfileConfig.Create;
  FProfileQuery := TMockProfileQuery.Create;
  FBuilder := TDeviceDisplayItemBuilder.Create(
    FConfigProvider, FAppearanceConfig, FProfileConfig, FProfileQuery);
  // Set null battery cache to satisfy null object pattern requirement
  FBuilder.SetBatteryCache(CreateNullBatteryCache);
end;

procedure TDeviceDisplayItemBuilderTests.TearDown;
begin
  FBuilder.Free;
  // Interfaces are reference-counted, no need to free
end;

{ IsVisible Tests }

procedure TDeviceDisplayItemBuilderTests.IsVisible_EmptyAPIName_WithConfigName_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  // Device has empty name from Windows API, but config has cached name from INI
  Device := CreateTestDevice($001, '', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  Config.Alias := '';
  Config.Name := 'Cached Name';
  FConfigProvider.AddDeviceConfig($001, Config);

  Assert.IsTrue(FBuilder.IsVisible(Device));
end;

procedure TDeviceDisplayItemBuilderTests.IsVisible_EmptyAPIName_WithAlias_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  // Device has empty name from Windows API, but has user-defined alias
  Device := CreateTestDevice($001, '', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  Config.Alias := 'My Device Alias';
  Config.Name := '';
  FConfigProvider.AddDeviceConfig($001, Config);

  Assert.IsTrue(FBuilder.IsVisible(Device));
end;

procedure TDeviceDisplayItemBuilderTests.IsVisible_EmptyAPIName_EmptyConfigName_EmptyAlias_ReturnsTrue_WithMACFallback;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  // Device has no name from API, no cached name, no alias - uses MAC address fallback
  Device := CreateTestDevice($001122334455, '', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  Config.Alias := '';
  Config.Name := '';
  FConfigProvider.AddDeviceConfig($001122334455, Config);

  // Should return True because MAC address fallback provides a name
  Assert.IsTrue(FBuilder.IsVisible(Device));
end;

procedure TDeviceDisplayItemBuilderTests.IsVisible_HiddenDevice_ReturnsFalse;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001, 'Hidden Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := True;
  FConfigProvider.AddDeviceConfig($001, Config);

  Assert.IsFalse(FBuilder.IsVisible(Device));
end;

procedure TDeviceDisplayItemBuilderTests.IsVisible_VisibleDevice_ReturnsTrue;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
begin
  Device := CreateTestDevice($001, 'Visible Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  FConfigProvider.AddDeviceConfig($001, Config);

  Assert.IsTrue(FBuilder.IsVisible(Device));
end;

{ BuildDisplayItem Tests }

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_UsesAlias_WhenSet;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Original Name', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Alias := 'My Alias';
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual('My Alias', Item.DisplayName);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_UsesDeviceName_WhenNoAlias;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Original Name', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Alias := '';
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual('Original Name', Item.DisplayName);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_SetsIsPinned_FromConfig;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Pinned := True;
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.IsTrue(Item.IsPinned);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_SetsEffectiveDeviceType_WithOverride;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.DeviceTypeOverride := Ord(btKeyboard);
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual(Ord(btKeyboard), Ord(Item.EffectiveDeviceType));
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_SetsEffectiveDeviceType_WithoutOverride;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btMouse, csConnected);

  Config := Default(TDeviceConfig);
  Config.DeviceTypeOverride := -1;  // No override
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual(Ord(btMouse), Ord(Item.EffectiveDeviceType));
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_SetsLastSeenText_Relative;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.LastSeen := IncMinute(Now, -5);
  FConfigProvider.AddDeviceConfig($001, Config);
  FAppearanceConfig.LastSeenFormat := lsfRelative;

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual('5 min ago', Item.LastSeenText);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_SetsLastSeenText_Absolute;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.LastSeen := EncodeDateTime(2024, 12, 22, 15, 30, 0, 0);
  FConfigProvider.AddDeviceConfig($001, Config);
  FAppearanceConfig.LastSeenFormat := lsfAbsolute;

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual('2024-12-22 15:30', Item.LastSeenText);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_CalculatesSortGroup_Pinned;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csDisconnected);

  Config := Default(TDeviceConfig);
  Config.Pinned := True;
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual(0, Item.SortGroup);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_CalculatesSortGroup_Connected;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Pinned := False;
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual(1, Item.SortGroup);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItem_CalculatesSortGroup_Disconnected;
var
  Device: TBluetoothDeviceInfo;
  Config: TDeviceConfig;
  Item: TDeviceDisplayItem;
begin
  Device := CreateTestDevice($001, 'Device', btAudioOutput, csDisconnected);

  Config := Default(TDeviceConfig);
  Config.Pinned := False;
  FConfigProvider.AddDeviceConfig($001, Config);

  Item := FBuilder.BuildDisplayItem(Device);

  Assert.AreEqual(2, Item.SortGroup);
end;

{ BuildDisplayItems Tests }

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItems_EmptyInput_ReturnsEmptyArray;
var
  Devices: TBluetoothDeviceInfoArray;
  Items: TDeviceDisplayItemArray;
begin
  SetLength(Devices, 0);
  Items := FBuilder.BuildDisplayItems(Devices);
  Assert.AreEqual<Integer>(0, Length(Items));
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItems_FiltersHiddenDevices;
var
  Devices: TBluetoothDeviceInfoArray;
  Items: TDeviceDisplayItemArray;
  VisibleConfig, HiddenConfig: TDeviceConfig;
begin
  SetLength(Devices, 2);
  Devices[0] := CreateTestDevice($001, 'Visible', btAudioOutput, csConnected);
  Devices[1] := CreateTestDevice($002, 'Hidden', btAudioOutput, csConnected);

  VisibleConfig := Default(TDeviceConfig);
  VisibleConfig.Hidden := False;
  FConfigProvider.AddDeviceConfig($001, VisibleConfig);

  HiddenConfig := Default(TDeviceConfig);
  HiddenConfig.Hidden := True;
  FConfigProvider.AddDeviceConfig($002,HiddenConfig);

  Items := FBuilder.BuildDisplayItems(Devices);

  Assert.AreEqual<Integer>(1, Length(Items));
  Assert.AreEqual('Visible', Items[0].DisplayName);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItems_FiltersEmptyNames;
var
  Devices: TBluetoothDeviceInfoArray;
  Items: TDeviceDisplayItemArray;
  Config, ConfigWithName: TDeviceConfig;
begin
  // Test that devices with empty API name but cached config name are NOT filtered
  SetLength(Devices, 2);
  Devices[0] := CreateTestDevice($001, 'Valid Name', btAudioOutput, csConnected);
  Devices[1] := CreateTestDevice($002, '', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  Config.Name := '';  // No cached name
  FConfigProvider.AddDeviceConfig($001, Config);

  ConfigWithName := Default(TDeviceConfig);
  ConfigWithName.Hidden := False;
  ConfigWithName.Name := 'Cached Device Name';  // Has cached name from INI
  FConfigProvider.AddDeviceConfig($002, ConfigWithName);

  Items := FBuilder.BuildDisplayItems(Devices);

  // Both devices should be visible - one with API name, one with cached name
  Assert.AreEqual<Integer>(2, Length(Items));
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItems_SortsResult;
var
  Devices: TBluetoothDeviceInfoArray;
  Items: TDeviceDisplayItemArray;
  Config: TDeviceConfig;
begin
  SetLength(Devices, 3);
  Devices[0] := CreateTestDevice($001, 'Disconnected', btAudioOutput, csDisconnected);
  Devices[1] := CreateTestDevice($002, 'Pinned', btAudioOutput, csDisconnected);
  Devices[2] := CreateTestDevice($003, 'Connected', btAudioOutput, csConnected);

  Config := Default(TDeviceConfig);
  Config.Hidden := False;
  Config.Pinned := False;
  FConfigProvider.AddDeviceConfig($001, Config);
  FConfigProvider.AddDeviceConfig($003,Config);

  Config.Pinned := True;
  FConfigProvider.AddDeviceConfig($002,Config);

  Items := FBuilder.BuildDisplayItems(Devices);

  Assert.AreEqual<Integer>(3, Length(Items));
  // Pinned first, then connected, then disconnected
  Assert.AreEqual('Pinned', Items[0].DisplayName);
  Assert.AreEqual('Connected', Items[1].DisplayName);
  Assert.AreEqual('Disconnected', Items[2].DisplayName);
end;

procedure TDeviceDisplayItemBuilderTests.BuildDisplayItems_ProcessesAllVisibleDevices;
var
  Devices: TBluetoothDeviceInfoArray;
  Items: TDeviceDisplayItemArray;
  Config: TDeviceConfig;
  I: Integer;
begin
  SetLength(Devices, 5);
  for I := 0 to 4 do
  begin
    Devices[I] := CreateTestDevice(I + 1, 'Device ' + IntToStr(I + 1), btAudioOutput, csConnected);
    Config := Default(TDeviceConfig);
    Config.Hidden := False;
    FConfigProvider.AddDeviceConfig(I + 1, Config);
  end;

  Items := FBuilder.BuildDisplayItems(Devices);

  Assert.AreEqual<Integer>(5, Length(Items));
end;

initialization
  TDUnitX.RegisterTestFixture(TDeviceDisplayItemBuilderTests);

end.
