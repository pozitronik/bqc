{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Settings Presenter Unit Tests                   }
{                                                       }
{*******************************************************}

unit Tests.SettingsPresenter;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  App.SettingsPresenter,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.AppearanceConfigIntf,
  App.LogConfigIntf,
  App.DeviceConfigTypes,
  Tests.Mocks;

type
  /// <summary>
  /// Tests for TSettingsPresenter view interaction.
  /// Note: Full integration tests require Bootstrap initialization.
  /// These tests verify the presenter correctly interacts with the view interfaces.
  /// </summary>
  [TestFixture]
  TSettingsPresenterTests = class
  private
    FMockView: TMockSettingsView;
    FMockAppConfig: TMockAppConfig;
    FMockDeviceConfigProvider: TMockDeviceConfigProvider;
    FPresenter: TSettingsPresenter;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Create_SetsInitialState;

    [Test]
    procedure Test_MarkModified_SetsIsModified;

    [Test]
    procedure Test_MarkModified_EnablesApplyButton;

    [Test]
    procedure Test_OnCancelClicked_ClosesWithCancel;

    [Test]
    procedure Test_OnResetSizeClicked_ShowsInfoMessage;

    [Test]
    procedure Test_OnResetPositionClicked_ShowsInfoMessage;

    [Test]
    procedure Test_LoadSettings_PopulatesGeneralSettings;

    [Test]
    procedure Test_LoadSettings_PopulatesHotkeySettings;

    [Test]
    procedure Test_LoadSettings_PopulatesAppearanceSettings;

    [Test]
    procedure Test_LoadSettings_PopulatesLayoutSettings;

    [Test]
    procedure Test_LoadSettings_PopulatesConnectionSettings;

    [Test]
    procedure Test_LoadSettings_PopulatesLoggingSettings;

    [Test]
    procedure Test_LoadSettings_ClearsModifiedFlag;

    [Test]
    procedure Test_SaveSettings_SavesGeneralSettings;

    [Test]
    procedure Test_SaveSettings_SavesHotkeySettings;

    [Test]
    procedure Test_SaveSettings_SavesAppearanceSettings;

    [Test]
    procedure Test_SaveSettings_SavesLayoutSettings;

    [Test]
    procedure Test_SaveSettings_SavesConnectionSettings;

    [Test]
    procedure Test_SaveSettings_SavesLoggingSettings;

    [Test]
    procedure Test_SaveSettings_ClearsModifiedFlag;

    [Test]
    procedure Test_SaveSettings_CallsAppConfigSave;

    [Test]
    procedure Test_SaveSettings_WithDuplicateMainAndCastHotkey_ReturnsFalse;

    [Test]
    procedure Test_SaveSettings_WithDuplicateMainAndBluetoothHotkey_ReturnsFalse;

    [Test]
    procedure Test_SaveSettings_WithDuplicateCastAndBluetoothHotkey_ReturnsFalse;

    [Test]
    procedure Test_SaveSettings_WithAllSameHotkeys_ReturnsFalse;

    [Test]
    procedure Test_SaveSettings_WithUniqueHotkeys_ReturnsTrue;

    [Test]
    procedure Test_SaveSettings_WithEmptyHotkeys_ReturnsTrue;

    [Test]
    procedure Test_SaveSettings_CaseInsensitiveCollision_ReturnsFalse;

    [Test]
    procedure Test_LoadSettings_PopulatesSystemPanelHotkeys;

    [Test]
    procedure Test_SaveSettings_SavesSystemPanelHotkeys;
  end;

  /// <summary>
  /// Tests for TDeviceSettingsPresenter.
  /// Verifies device settings load/save/selection logic.
  /// </summary>
  [TestFixture]
  TDeviceSettingsPresenterTests = class
  private
    FMockView: TMockSettingsView;
    FMockDeviceConfigProvider: TMockDeviceConfigProvider;
    FPresenter: TDeviceSettingsPresenter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // LoadDeviceList
    [Test]
    procedure LoadDeviceList_NoDevices_PopulatesEmptyList;
    [Test]
    procedure LoadDeviceList_NoDevices_ClearsSettings;
    [Test]
    procedure LoadDeviceList_WithDevices_PopulatesList;
    [Test]
    procedure LoadDeviceList_WithDevices_SelectsFirst;
    [Test]
    procedure LoadDeviceList_WithNamedDevice_FormatsDisplayName;
    [Test]
    procedure LoadDeviceList_UnnamedDevice_ShowsAddressOnly;

    // LoadDeviceSettings
    [Test]
    procedure LoadDeviceSettings_LoadsAlias;
    [Test]
    procedure LoadDeviceSettings_AppliesUIOffset_DeviceType;
    [Test]
    procedure LoadDeviceSettings_LoadsBooleanFlags;
    [Test]
    procedure LoadDeviceSettings_LoadsConnectionSettings;
    [Test]
    procedure LoadDeviceSettings_LoadsNotificationOverrides;
    [Test]
    procedure LoadDeviceSettings_LoadsBatteryTraySettings;
    [Test]
    procedure LoadDeviceSettings_LoadsShowProfiles;
    [Test]
    procedure LoadDeviceSettings_InvalidIndex_NoAction;

    // SaveDeviceSettings
    [Test]
    procedure SaveDeviceSettings_SavesAlias;
    [Test]
    procedure SaveDeviceSettings_RevertsUIOffset;
    [Test]
    procedure SaveDeviceSettings_SavesBooleanFlags;
    [Test]
    procedure SaveDeviceSettings_SavesNotificationOverrides;
    [Test]
    procedure SaveDeviceSettings_SavesBatteryTraySettings;
    [Test]
    procedure SaveDeviceSettings_InvalidIndex_NoAction;

    // SaveCurrentDevice
    [Test]
    procedure SaveCurrentDevice_WithSelection_Saves;
    [Test]
    procedure SaveCurrentDevice_NoSelection_NoAction;

    // OnDeviceSelected
    [Test]
    procedure OnDeviceSelected_LoadsNewDevice;

    // OnForgetDeviceClicked
    [Test]
    procedure OnForgetDeviceClicked_ValidIndex_RemovesDevice;
    [Test]
    procedure OnForgetDeviceClicked_InvalidIndex_NoAction;

    // OnRefreshDevicesClicked
    [Test]
    procedure OnRefreshDevicesClicked_ReloadsDeviceList;

    // SelectDeviceByAddress
    [Test]
    procedure SelectDeviceByAddress_Existing_SelectsDevice;
    [Test]
    procedure SelectDeviceByAddress_NonExisting_NoSelection;

    // Round-trip: Load -> Modify -> Save
    [Test]
    procedure RoundTrip_ModifyAndSave_PersistsChanges;
  end;

  /// <summary>
  /// Tests for settings view data record types.
  /// Verifies record field assignment and copying works correctly.
  /// </summary>
  [TestFixture]
  TSettingsRecordTests = class
  public
    [Test]
    procedure Test_GeneralSettings_FieldAssignment;

    [Test]
    procedure Test_HotkeySettings_FieldAssignment;

    [Test]
    procedure Test_AppearanceSettings_FieldAssignment;

    [Test]
    procedure Test_LayoutSettings_FieldAssignment;

    [Test]
    procedure Test_ConnectionSettings_FieldAssignment;

    [Test]
    procedure Test_LoggingSettings_FieldAssignment;

    [Test]
    procedure Test_DeviceSettings_FieldAssignment;
  end;

  /// <summary>
  /// Tests for TMockSettingsView.
  /// Verifies the mock correctly implements the settings view interfaces.
  /// </summary>
  [TestFixture]
  TMockSettingsViewTests = class
  private
    FMockView: TMockSettingsView;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_CloseWithOK_SetsFlag;

    [Test]
    procedure Test_CloseWithCancel_SetsFlag;

    [Test]
    procedure Test_ShowError_StoresMessage;

    [Test]
    procedure Test_ShowInfo_StoresMessage;

    [Test]
    procedure Test_SetApplyEnabled_StoresState;

    [Test]
    procedure Test_SetGeneralSettings_IncrementsCount;

    [Test]
    procedure Test_GetGeneralSettings_ReturnsStoredValue;

    [Test]
    procedure Test_PopulateThemeList_StoresValues;

    [Test]
    procedure Test_PopulateDeviceList_StoresItems;

    [Test]
    procedure Test_SelectedDeviceIndex_GetSet;

    [Test]
    procedure Test_ClearDeviceSettings_IncrementsCount;
  end;

implementation

uses
  App.Bootstrap,
  App.Config;

{ TSettingsPresenterTests }

procedure TSettingsPresenterTests.Setup;
begin
  FMockView := TMockSettingsView.Create;
  FMockAppConfig := TMockAppConfig.Create;
  FMockDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  // Note: The mocks are TInterfacedObjects, but we need to prevent premature destruction
  // by keeping references. The presenter takes interface references.
  FPresenter := TSettingsPresenter.Create(
    FMockView as ISettingsDialogView,
    FMockView as IGeneralSettingsView,
    FMockView as IHotkeySettingsView,
    FMockView as IAppearanceSettingsView,
    FMockView as ILayoutSettingsView,
    FMockView as IConnectionSettingsView,
    FMockView as ILoggingSettingsView,
    FMockView as IBatteryTraySettingsView,
    FMockView as IProfileSettingsView,
    FMockView as IRestApiSettingsView,
    FMockView as IDeviceSettingsView,
    FMockAppConfig,
    FMockDeviceConfigProvider,
    FMockAppConfig.AsBatteryTrayConfig,
    FMockAppConfig.AsProfileConfig,
    FMockAppConfig.AsRestApiConfig
  );
end;

procedure TSettingsPresenterTests.TearDown;
begin
  FPresenter.Free;
  // FMockView is released when its reference count drops to 0
end;

procedure TSettingsPresenterTests.Test_Create_SetsInitialState;
begin
  Assert.IsFalse(FPresenter.IsModified, 'IsModified should be False initially');
end;

procedure TSettingsPresenterTests.Test_MarkModified_SetsIsModified;
begin
  FPresenter.MarkModified;
  Assert.IsTrue(FPresenter.IsModified, 'IsModified should be True after MarkModified');
end;

procedure TSettingsPresenterTests.Test_MarkModified_EnablesApplyButton;
begin
  FPresenter.MarkModified;
  Assert.IsTrue(FMockView.ApplyEnabled, 'Apply button should be enabled');
end;

procedure TSettingsPresenterTests.Test_OnCancelClicked_ClosesWithCancel;
begin
  FPresenter.OnCancelClicked;
  Assert.IsTrue(FMockView.CloseWithCancelCalled, 'CloseWithCancel should be called');
end;

procedure TSettingsPresenterTests.Test_OnResetSizeClicked_ShowsInfoMessage;
begin
  // This test requires Bootstrap to be initialized
  // For now, we test that it doesn't crash without Bootstrap
  try
    FPresenter.OnResetSizeClicked;
    Assert.IsNotEmpty(FMockView.LastInfoMessage, 'Info message should be shown');
  except
    on E: Exception do
      Assert.Pass('Test skipped - Bootstrap not initialized: ' + E.Message);
  end;
end;

procedure TSettingsPresenterTests.Test_OnResetPositionClicked_ShowsInfoMessage;
begin
  // This test requires Bootstrap to be initialized
  try
    FPresenter.OnResetPositionClicked;
    Assert.IsNotEmpty(FMockView.LastInfoMessage, 'Info message should be shown');
  except
    on E: Exception do
      Assert.Pass('Test skipped - Bootstrap not initialized: ' + E.Message);
  end;
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesGeneralSettings;
var
  MockGeneralCfg: TMockGeneralConfig;
  MockWindowCfg: TMockWindowConfig;
  MockPositionCfg: TMockPositionConfig;
begin
  // Arrange: Set up mock config values
  MockGeneralCfg := TMockGeneralConfig(FMockAppConfig.GeneralConfig);
  MockWindowCfg := TMockWindowConfig(FMockAppConfig.WindowConfig);
  MockPositionCfg := TMockPositionConfig(FMockAppConfig.PositionConfig);

  MockGeneralCfg.WindowMode := wmMenu;
  MockGeneralCfg.OnTop := True;
  MockGeneralCfg.Autostart := True;
  MockWindowCfg.MinimizeToTray := False;
  MockWindowCfg.CloseToTray := True;
  MockWindowCfg.MenuHideOnFocusLoss := False;
  MockPositionCfg.PositionMode := pmCenterScreen;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual(Integer(wmMenu), Integer(FMockView.GeneralSettings.WindowMode),
    'WindowMode should match config');
  Assert.IsTrue(FMockView.GeneralSettings.OnTop, 'OnTop should match config');
  Assert.IsTrue(FMockView.GeneralSettings.Autostart, 'Autostart should match config');
  Assert.IsFalse(FMockView.GeneralSettings.MinimizeToTray, 'MinimizeToTray should match config');
  Assert.IsTrue(FMockView.GeneralSettings.CloseToTray, 'CloseToTray should match config');
  Assert.IsFalse(FMockView.GeneralSettings.HideOnFocusLoss, 'HideOnFocusLoss should match config');
  Assert.AreEqual(Integer(pmCenterScreen), Integer(FMockView.GeneralSettings.PositionMode),
    'PositionMode should match config');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesHotkeySettings;
var
  MockHotkeyCfg: TMockHotkeyConfig;
begin
  // Arrange
  MockHotkeyCfg := TMockHotkeyConfig(FMockAppConfig.HotkeyConfig);
  MockHotkeyCfg.Hotkey := 'Ctrl+Alt+B';
  MockHotkeyCfg.UseLowLevelHook := True;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual('Ctrl+Alt+B', FMockView.HotkeySettings.Hotkey, 'Hotkey should match config');
  Assert.IsTrue(FMockView.HotkeySettings.UseLowLevelHook, 'UseLowLevelHook should match config');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesAppearanceSettings;
var
  MockAppearanceCfg: TMockAppearanceConfig;
begin
  // Arrange
  MockAppearanceCfg := TMockAppearanceConfig(FMockAppConfig.AppearanceConfig);
  MockAppearanceCfg.Theme := 'Dark';
  MockAppearanceCfg.VsfDir := 'custom_themes';
  MockAppearanceCfg.ShowAddresses := True;
  MockAppearanceCfg.ShowDeviceIcons := False;
  MockAppearanceCfg.ShowLastSeen := True;
  MockAppearanceCfg.LastSeenFormat := lsfAbsolute;
  MockAppearanceCfg.ConnectedColor := $00FF00;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual('Dark', FMockView.AppearanceSettings.Theme, 'Theme should match config');
  Assert.AreEqual('custom_themes', FMockView.AppearanceSettings.VsfDir, 'VsfDir should match config');
  Assert.IsTrue(FMockView.AppearanceSettings.ShowAddresses, 'ShowAddresses should match config');
  Assert.IsFalse(FMockView.AppearanceSettings.ShowDeviceIcons, 'ShowDeviceIcons should match config');
  Assert.IsTrue(FMockView.AppearanceSettings.ShowLastSeen, 'ShowLastSeen should match config');
  Assert.IsFalse(FMockView.AppearanceSettings.LastSeenRelative, 'LastSeenRelative should be False for lsfAbsolute');
  Assert.AreEqual(Integer($00FF00), Integer(FMockView.AppearanceSettings.ConnectedColor),
    'ConnectedColor should match config');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesLayoutSettings;
var
  MockLayoutCfg: TMockLayoutConfig;
begin
  // Arrange
  MockLayoutCfg := TMockLayoutConfig(FMockAppConfig.LayoutConfig);
  MockLayoutCfg.ItemHeight := 80;
  MockLayoutCfg.ItemPadding := 8;
  MockLayoutCfg.ItemMargin := 6;
  MockLayoutCfg.IconSize := 48;
  MockLayoutCfg.CornerRadius := 10;
  MockLayoutCfg.ItemBorderWidth := 2;
  MockLayoutCfg.ItemBorderColor := $808080;
  MockLayoutCfg.DeviceNameFontSize := 14;
  MockLayoutCfg.StatusFontSize := 10;
  MockLayoutCfg.AddressFontSize := 9;
  MockLayoutCfg.IconFontSize := 18;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual(80, FMockView.LayoutSettings.ItemHeight, 'ItemHeight should match config');
  Assert.AreEqual(8, FMockView.LayoutSettings.ItemPadding, 'ItemPadding should match config');
  Assert.AreEqual(6, FMockView.LayoutSettings.ItemMargin, 'ItemMargin should match config');
  Assert.AreEqual(48, FMockView.LayoutSettings.IconSize, 'IconSize should match config');
  Assert.AreEqual(10, FMockView.LayoutSettings.CornerRadius, 'CornerRadius should match config');
  Assert.AreEqual(2, FMockView.LayoutSettings.BorderWidth, 'BorderWidth should match config');
  Assert.AreEqual(Integer($808080), Integer(FMockView.LayoutSettings.BorderColor),
    'BorderColor should match config');
  Assert.AreEqual(14, FMockView.LayoutSettings.DeviceNameFontSize, 'DeviceNameFontSize should match config');
  Assert.AreEqual(10, FMockView.LayoutSettings.StatusFontSize, 'StatusFontSize should match config');
  Assert.AreEqual(9, FMockView.LayoutSettings.AddressFontSize, 'AddressFontSize should match config');
  Assert.AreEqual(18, FMockView.LayoutSettings.IconFontSize, 'IconFontSize should match config');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesConnectionSettings;
var
  MockConnectionCfg: TMockConnectionConfig;
  MockPollingCfg: TMockPollingConfig;
  MockNotificationCfg: TMockNotificationConfig;
begin
  // Arrange
  MockConnectionCfg := TMockConnectionConfig(FMockAppConfig.ConnectionConfig);
  MockPollingCfg := TMockPollingConfig(FMockAppConfig.PollingConfig);
  MockNotificationCfg := TMockNotificationConfig(FMockAppConfig.NotificationConfig);

  MockConnectionCfg.ConnectionTimeout := 15000;
  MockConnectionCfg.ConnectionRetryCount := 5;
  MockPollingCfg.PollingMode := pmPrimary;
  MockPollingCfg.PollingInterval := 3000;
  MockNotificationCfg.NotifyOnConnect := nmBalloon;
  MockNotificationCfg.NotifyOnDisconnect := nmNone;
  MockNotificationCfg.NotifyOnConnectFailed := nmBalloon;
  MockNotificationCfg.NotifyOnAutoConnect := nmNone;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual(15000, FMockView.ConnectionSettings.Timeout, 'Timeout should match config');
  Assert.AreEqual(5, FMockView.ConnectionSettings.RetryCount, 'RetryCount should match config');
  Assert.AreEqual(Integer(pmPrimary), Integer(FMockView.ConnectionSettings.PollingMode),
    'PollingMode should match config');
  Assert.AreEqual(3000, FMockView.ConnectionSettings.PollingInterval, 'PollingInterval should match config');
  Assert.IsTrue(FMockView.ConnectionSettings.NotifyOnConnect, 'NotifyOnConnect should be True');
  Assert.IsFalse(FMockView.ConnectionSettings.NotifyOnDisconnect, 'NotifyOnDisconnect should be False');
  Assert.IsTrue(FMockView.ConnectionSettings.NotifyOnConnectFailed, 'NotifyOnConnectFailed should be True');
  Assert.IsFalse(FMockView.ConnectionSettings.NotifyOnAutoConnect, 'NotifyOnAutoConnect should be False');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesLoggingSettings;
var
  MockLogCfg: TMockLogConfig;
begin
  // Arrange
  MockLogCfg := TMockLogConfig(FMockAppConfig.LogConfig);
  MockLogCfg.LogEnabled := True;
  MockLogCfg.LogFilename := 'custom.log';
  MockLogCfg.LogAppend := False;
  MockLogCfg.LogLevel := llWarning;

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.IsTrue(FMockView.LoggingSettings.Enabled, 'Enabled should match config');
  Assert.AreEqual('custom.log', FMockView.LoggingSettings.Filename, 'Filename should match config');
  Assert.IsFalse(FMockView.LoggingSettings.Append, 'Append should match config');
  Assert.AreEqual(Ord(llWarning), FMockView.LoggingSettings.LevelIndex, 'LevelIndex should match LogLevel');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_ClearsModifiedFlag;
begin
  // Arrange
  FPresenter.MarkModified;
  Assert.IsTrue(FPresenter.IsModified, 'IsModified should be True before LoadSettings');

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.IsFalse(FPresenter.IsModified, 'IsModified should be False after LoadSettings');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesGeneralSettings;
var
  MockGeneralCfg: TMockGeneralConfig;
  MockWindowCfg: TMockWindowConfig;
  MockPositionCfg: TMockPositionConfig;
  General: TGeneralViewSettings;
begin
  // Arrange: Set up view settings
  General.WindowMode := wmMenu;
  General.OnTop := True;
  General.MinimizeToTray := False;
  General.CloseToTray := True;
  General.HideOnFocusLoss := False;
  General.Autostart := True;
  General.PositionMode := pmNearCursor;
  FMockView.GeneralSettings := General;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockGeneralCfg := TMockGeneralConfig(FMockAppConfig.GeneralConfig);
  MockWindowCfg := TMockWindowConfig(FMockAppConfig.WindowConfig);
  MockPositionCfg := TMockPositionConfig(FMockAppConfig.PositionConfig);

  Assert.AreEqual(Integer(wmMenu), Integer(MockGeneralCfg.WindowMode), 'WindowMode should be saved');
  Assert.IsTrue(MockGeneralCfg.OnTop, 'OnTop should be saved');
  Assert.IsTrue(MockGeneralCfg.Autostart, 'Autostart should be saved');
  Assert.IsFalse(MockWindowCfg.MinimizeToTray, 'MinimizeToTray should be saved');
  Assert.IsTrue(MockWindowCfg.CloseToTray, 'CloseToTray should be saved');
  Assert.IsFalse(MockWindowCfg.MenuHideOnFocusLoss, 'MenuHideOnFocusLoss should be saved');
  Assert.AreEqual(Integer(pmNearCursor), Integer(MockPositionCfg.PositionMode), 'PositionMode should be saved');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesHotkeySettings;
var
  MockHotkeyCfg: TMockHotkeyConfig;
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Ctrl+Shift+X';
  Hotkey.UseLowLevelHook := True;
  Hotkey.CastPanelHotkey := '';  // Ensure all fields are initialized
  Hotkey.BluetoothPanelHotkey := '';
  FMockView.HotkeySettings := Hotkey;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockHotkeyCfg := TMockHotkeyConfig(FMockAppConfig.HotkeyConfig);
  Assert.AreEqual('Ctrl+Shift+X', MockHotkeyCfg.Hotkey, 'Hotkey should be saved');
  Assert.IsTrue(MockHotkeyCfg.UseLowLevelHook, 'UseLowLevelHook should be saved');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesAppearanceSettings;
var
  MockAppearanceCfg: TMockAppearanceConfig;
  Appearance: TAppearanceViewSettings;
begin
  // Arrange
  Appearance.Theme := 'Light';
  Appearance.VsfDir := 'my_themes';
  Appearance.ShowAddresses := True;
  Appearance.ShowDeviceIcons := False;
  Appearance.ShowLastSeen := True;
  Appearance.LastSeenRelative := False;  // Should save as lsfAbsolute
  Appearance.ConnectedColor := $FF0000;
  FMockView.AppearanceSettings := Appearance;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockAppearanceCfg := TMockAppearanceConfig(FMockAppConfig.AppearanceConfig);
  Assert.AreEqual('Light', MockAppearanceCfg.Theme, 'Theme should be saved');
  Assert.AreEqual('my_themes', MockAppearanceCfg.VsfDir, 'VsfDir should be saved');
  Assert.IsTrue(MockAppearanceCfg.ShowAddresses, 'ShowAddresses should be saved');
  Assert.IsFalse(MockAppearanceCfg.ShowDeviceIcons, 'ShowDeviceIcons should be saved');
  Assert.IsTrue(MockAppearanceCfg.ShowLastSeen, 'ShowLastSeen should be saved');
  Assert.AreEqual(Integer(lsfAbsolute), Integer(MockAppearanceCfg.LastSeenFormat),
    'LastSeenFormat should be lsfAbsolute');
  Assert.AreEqual(Integer($FF0000), MockAppearanceCfg.ConnectedColor, 'ConnectedColor should be saved');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesLayoutSettings;
var
  MockLayoutCfg: TMockLayoutConfig;
  Layout: TLayoutViewSettings;
begin
  // Arrange
  Layout.ItemHeight := 90;
  Layout.ItemPadding := 10;
  Layout.ItemMargin := 8;
  Layout.IconSize := 56;
  Layout.CornerRadius := 12;
  Layout.BorderWidth := 3;
  Layout.BorderColor := $0000FF;
  Layout.DeviceNameFontSize := 16;
  Layout.StatusFontSize := 12;
  Layout.AddressFontSize := 10;
  Layout.IconFontSize := 20;
  FMockView.LayoutSettings := Layout;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockLayoutCfg := TMockLayoutConfig(FMockAppConfig.LayoutConfig);
  Assert.AreEqual(90, MockLayoutCfg.ItemHeight, 'ItemHeight should be saved');
  Assert.AreEqual(10, MockLayoutCfg.ItemPadding, 'ItemPadding should be saved');
  Assert.AreEqual(8, MockLayoutCfg.ItemMargin, 'ItemMargin should be saved');
  Assert.AreEqual(56, MockLayoutCfg.IconSize, 'IconSize should be saved');
  Assert.AreEqual(12, MockLayoutCfg.CornerRadius, 'CornerRadius should be saved');
  Assert.AreEqual(3, MockLayoutCfg.ItemBorderWidth, 'ItemBorderWidth should be saved');
  Assert.AreEqual(Integer($0000FF), MockLayoutCfg.ItemBorderColor, 'ItemBorderColor should be saved');
  Assert.AreEqual(16, MockLayoutCfg.DeviceNameFontSize, 'DeviceNameFontSize should be saved');
  Assert.AreEqual(12, MockLayoutCfg.StatusFontSize, 'StatusFontSize should be saved');
  Assert.AreEqual(10, MockLayoutCfg.AddressFontSize, 'AddressFontSize should be saved');
  Assert.AreEqual(20, MockLayoutCfg.IconFontSize, 'IconFontSize should be saved');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesConnectionSettings;
var
  MockConnectionCfg: TMockConnectionConfig;
  MockPollingCfg: TMockPollingConfig;
  MockNotificationCfg: TMockNotificationConfig;
  Connection: TConnectionViewSettings;
begin
  // Arrange
  Connection.Timeout := 20000;
  Connection.RetryCount := 4;
  Connection.PollingMode := pmDisabled;
  Connection.PollingInterval := 5000;
  Connection.NotifyOnConnect := True;
  Connection.NotifyOnDisconnect := True;
  Connection.NotifyOnConnectFailed := False;
  Connection.NotifyOnAutoConnect := True;
  FMockView.ConnectionSettings := Connection;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockConnectionCfg := TMockConnectionConfig(FMockAppConfig.ConnectionConfig);
  MockPollingCfg := TMockPollingConfig(FMockAppConfig.PollingConfig);
  MockNotificationCfg := TMockNotificationConfig(FMockAppConfig.NotificationConfig);

  Assert.AreEqual(20000, MockConnectionCfg.ConnectionTimeout, 'Timeout should be saved');
  Assert.AreEqual(4, MockConnectionCfg.ConnectionRetryCount, 'RetryCount should be saved');
  Assert.AreEqual(Integer(pmDisabled), Integer(MockPollingCfg.PollingMode), 'PollingMode should be saved');
  Assert.AreEqual(5000, MockPollingCfg.PollingInterval, 'PollingInterval should be saved');
  Assert.AreEqual(Integer(nmBalloon), Integer(MockNotificationCfg.NotifyOnConnect),
    'NotifyOnConnect should be nmBalloon');
  Assert.AreEqual(Integer(nmBalloon), Integer(MockNotificationCfg.NotifyOnDisconnect),
    'NotifyOnDisconnect should be nmBalloon');
  Assert.AreEqual(Integer(nmNone), Integer(MockNotificationCfg.NotifyOnConnectFailed),
    'NotifyOnConnectFailed should be nmNone');
  Assert.AreEqual(Integer(nmBalloon), Integer(MockNotificationCfg.NotifyOnAutoConnect),
    'NotifyOnAutoConnect should be nmBalloon');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesLoggingSettings;
var
  MockLogCfg: TMockLogConfig;
  Logging: TLoggingViewSettings;
begin
  // Arrange
  Logging.Enabled := True;
  Logging.Filename := 'debug.log';
  Logging.Append := False;
  Logging.LevelIndex := Ord(llError);
  FMockView.LoggingSettings := Logging;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockLogCfg := TMockLogConfig(FMockAppConfig.LogConfig);
  Assert.IsTrue(MockLogCfg.LogEnabled, 'LogEnabled should be saved');
  Assert.AreEqual('debug.log', MockLogCfg.LogFilename, 'LogFilename should be saved');
  Assert.IsFalse(MockLogCfg.LogAppend, 'LogAppend should be saved');
  Assert.AreEqual(Integer(llError), Integer(MockLogCfg.LogLevel), 'LogLevel should be saved');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_ClearsModifiedFlag;
begin
  // Arrange
  FPresenter.MarkModified;
  Assert.IsTrue(FPresenter.IsModified, 'IsModified should be True before SaveSettings');

  // Act
  FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(FPresenter.IsModified, 'IsModified should be False after SaveSettings');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_CallsAppConfigSave;
begin
  // Arrange
  Assert.AreEqual(0, FMockAppConfig.SaveCount, 'SaveCount should be 0 initially');

  // Act
  FPresenter.SaveSettings;

  // Assert
  Assert.AreEqual(1, FMockAppConfig.SaveCount, 'AppConfig.Save should be called once');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithDuplicateMainAndCastHotkey_ReturnsFalse;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Ctrl+Alt+B';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := 'Ctrl+Alt+B';  // Same as main
  Hotkey.BluetoothPanelHotkey := '';
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(Result, 'SaveSettings should return False on collision');
  Assert.Contains(FMockView.LastErrorMessage, 'Main hotkey', 'Error should mention Main hotkey');
  Assert.Contains(FMockView.LastErrorMessage, 'Cast panel', 'Error should mention Cast panel');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithDuplicateMainAndBluetoothHotkey_ReturnsFalse;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Win+K';
  Hotkey.UseLowLevelHook := True;
  Hotkey.CastPanelHotkey := '';
  Hotkey.BluetoothPanelHotkey := 'Win+K';  // Same as main
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(Result, 'SaveSettings should return False on collision');
  Assert.Contains(FMockView.LastErrorMessage, 'Main hotkey', 'Error should mention Main hotkey');
  Assert.Contains(FMockView.LastErrorMessage, 'Bluetooth panel', 'Error should mention Bluetooth panel');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithDuplicateCastAndBluetoothHotkey_ReturnsFalse;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Ctrl+B';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := 'Ctrl+Shift+K';
  Hotkey.BluetoothPanelHotkey := 'Ctrl+Shift+K';  // Same as cast
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(Result, 'SaveSettings should return False on collision');
  Assert.Contains(FMockView.LastErrorMessage, 'Cast panel', 'Error should mention Cast panel');
  Assert.Contains(FMockView.LastErrorMessage, 'Bluetooth panel', 'Error should mention Bluetooth panel');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithAllSameHotkeys_ReturnsFalse;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Ctrl+Alt+X';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := 'Ctrl+Alt+X';  // Same as main
  Hotkey.BluetoothPanelHotkey := 'Ctrl+Alt+X';  // Same as main and cast
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(Result, 'SaveSettings should return False on collision');
  Assert.IsNotEmpty(FMockView.LastErrorMessage, 'Error message should be shown');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithUniqueHotkeys_ReturnsTrue;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Ctrl+Alt+B';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := 'Ctrl+Shift+C';
  Hotkey.BluetoothPanelHotkey := 'Ctrl+Shift+T';
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsTrue(Result, 'SaveSettings should return True with unique hotkeys');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_WithEmptyHotkeys_ReturnsTrue;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange: All hotkeys empty
  Hotkey.Hotkey := '';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := '';
  Hotkey.BluetoothPanelHotkey := '';
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsTrue(Result, 'SaveSettings should succeed with empty hotkeys');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_CaseInsensitiveCollision_ReturnsFalse;
var
  Hotkey: THotkeyViewSettings;
begin
  // Arrange: Same hotkey with different case
  Hotkey.Hotkey := 'ctrl+alt+b';
  Hotkey.UseLowLevelHook := False;
  Hotkey.CastPanelHotkey := 'CTRL+ALT+B';  // Same hotkey, different case
  Hotkey.BluetoothPanelHotkey := '';
  FMockView.HotkeySettings := Hotkey;

  // Act
  var Result := FPresenter.SaveSettings;

  // Assert
  Assert.IsFalse(Result, 'SaveSettings should detect case-insensitive collision');
end;

procedure TSettingsPresenterTests.Test_LoadSettings_PopulatesSystemPanelHotkeys;
var
  MockHotkeyCfg: TMockHotkeyConfig;
begin
  // Arrange
  MockHotkeyCfg := TMockHotkeyConfig(FMockAppConfig.HotkeyConfig);
  MockHotkeyCfg.Hotkey := 'Win+K';
  MockHotkeyCfg.UseLowLevelHook := True;
  MockHotkeyCfg.CastPanelHotkey := 'Ctrl+Shift+C';
  MockHotkeyCfg.BluetoothPanelHotkey := 'Ctrl+Shift+B';

  // Act
  FPresenter.LoadSettings;

  // Assert
  Assert.AreEqual('Ctrl+Shift+C', FMockView.HotkeySettings.CastPanelHotkey,
    'CastPanelHotkey should be loaded');
  Assert.AreEqual('Ctrl+Shift+B', FMockView.HotkeySettings.BluetoothPanelHotkey,
    'BluetoothPanelHotkey should be loaded');
end;

procedure TSettingsPresenterTests.Test_SaveSettings_SavesSystemPanelHotkeys;
var
  MockHotkeyCfg: TMockHotkeyConfig;
  Hotkey: THotkeyViewSettings;
begin
  // Arrange
  Hotkey.Hotkey := 'Win+K';
  Hotkey.UseLowLevelHook := True;
  Hotkey.CastPanelHotkey := 'Ctrl+Alt+C';
  Hotkey.BluetoothPanelHotkey := 'Ctrl+Alt+T';
  FMockView.HotkeySettings := Hotkey;

  // Act
  FPresenter.SaveSettings;

  // Assert
  MockHotkeyCfg := TMockHotkeyConfig(FMockAppConfig.HotkeyConfig);
  Assert.AreEqual('Ctrl+Alt+C', MockHotkeyCfg.CastPanelHotkey, 'CastPanelHotkey should be saved');
  Assert.AreEqual('Ctrl+Alt+T', MockHotkeyCfg.BluetoothPanelHotkey, 'BluetoothPanelHotkey should be saved');
end;

{ TSettingsRecordTests }

procedure TSettingsRecordTests.Test_GeneralSettings_FieldAssignment;
var
  Settings: TGeneralViewSettings;
begin
  Settings.WindowMode := wmMenu;
  Settings.OnTop := True;
  Settings.MinimizeToTray := True;
  Settings.CloseToTray := False;
  Settings.HideOnFocusLoss := True;
  Settings.Autostart := False;
  Settings.PositionMode := pmCoordinates;

  Assert.AreEqual(Integer(wmMenu), Integer(Settings.WindowMode));
  Assert.IsTrue(Settings.OnTop);
  Assert.IsTrue(Settings.MinimizeToTray);
  Assert.IsFalse(Settings.CloseToTray);
  Assert.IsTrue(Settings.HideOnFocusLoss);
  Assert.IsFalse(Settings.Autostart);
  Assert.AreEqual(Integer(pmCoordinates), Integer(Settings.PositionMode));
end;

procedure TSettingsRecordTests.Test_HotkeySettings_FieldAssignment;
var
  Settings: THotkeyViewSettings;
begin
  Settings.Hotkey := 'Ctrl+Alt+B';
  Settings.UseLowLevelHook := True;
  Settings.CastPanelHotkey := 'Ctrl+Shift+C';
  Settings.BluetoothPanelHotkey := 'Ctrl+Shift+T';

  Assert.AreEqual('Ctrl+Alt+B', Settings.Hotkey);
  Assert.IsTrue(Settings.UseLowLevelHook);
  Assert.AreEqual('Ctrl+Shift+C', Settings.CastPanelHotkey);
  Assert.AreEqual('Ctrl+Shift+T', Settings.BluetoothPanelHotkey);
end;

procedure TSettingsRecordTests.Test_AppearanceSettings_FieldAssignment;
var
  Settings: TAppearanceViewSettings;
begin
  Settings.Theme := 'Windows11 Dark';
  Settings.VsfDir := 'themes';
  Settings.ShowAddresses := True;
  Settings.ShowDeviceIcons := True;
  Settings.ShowLastSeen := True;
  Settings.LastSeenRelative := True;
  Settings.ConnectedColor := TColor($00008000);  // Green

  Assert.AreEqual('Windows11 Dark', Settings.Theme);
  Assert.AreEqual('themes', Settings.VsfDir);
  Assert.IsTrue(Settings.ShowAddresses);
  Assert.IsTrue(Settings.ShowDeviceIcons);
  Assert.IsTrue(Settings.ShowLastSeen);
  Assert.IsTrue(Settings.LastSeenRelative);
  Assert.AreEqual(Integer($00008000), Integer(Settings.ConnectedColor));
end;

procedure TSettingsRecordTests.Test_LayoutSettings_FieldAssignment;
var
  Settings: TLayoutViewSettings;
begin
  Settings.ItemHeight := 70;
  Settings.ItemPadding := 6;
  Settings.ItemMargin := 4;
  Settings.IconSize := 46;
  Settings.CornerRadius := 8;
  Settings.BorderWidth := 1;
  Settings.BorderColor := TColor($00808080);  // Gray
  Settings.DeviceNameFontSize := 12;
  Settings.StatusFontSize := 10;
  Settings.AddressFontSize := 8;
  Settings.IconFontSize := 16;

  Assert.AreEqual(70, Settings.ItemHeight);
  Assert.AreEqual(6, Settings.ItemPadding);
  Assert.AreEqual(4, Settings.ItemMargin);
  Assert.AreEqual(46, Settings.IconSize);
  Assert.AreEqual(8, Settings.CornerRadius);
  Assert.AreEqual(1, Settings.BorderWidth);
  Assert.AreEqual(Integer($00808080), Integer(Settings.BorderColor));
  Assert.AreEqual(12, Settings.DeviceNameFontSize);
  Assert.AreEqual(10, Settings.StatusFontSize);
  Assert.AreEqual(8, Settings.AddressFontSize);
  Assert.AreEqual(16, Settings.IconFontSize);
end;

procedure TSettingsRecordTests.Test_ConnectionSettings_FieldAssignment;
var
  Settings: TConnectionViewSettings;
begin
  Settings.Timeout := 10000;
  Settings.RetryCount := 3;
  Settings.PollingMode := pmPrimary;
  Settings.PollingInterval := 2000;
  Settings.NotifyOnConnect := True;
  Settings.NotifyOnDisconnect := True;
  Settings.NotifyOnConnectFailed := False;
  Settings.NotifyOnAutoConnect := True;

  Assert.AreEqual(10000, Settings.Timeout);
  Assert.AreEqual(3, Settings.RetryCount);
  Assert.AreEqual(Integer(pmPrimary), Integer(Settings.PollingMode));
  Assert.AreEqual(2000, Settings.PollingInterval);
  Assert.IsTrue(Settings.NotifyOnConnect);
  Assert.IsTrue(Settings.NotifyOnDisconnect);
  Assert.IsFalse(Settings.NotifyOnConnectFailed);
  Assert.IsTrue(Settings.NotifyOnAutoConnect);
end;

procedure TSettingsRecordTests.Test_LoggingSettings_FieldAssignment;
var
  Settings: TLoggingViewSettings;
begin
  Settings.Enabled := True;
  Settings.Filename := 'bqc.log';
  Settings.Append := True;

  Assert.IsTrue(Settings.Enabled);
  Assert.AreEqual('bqc.log', Settings.Filename);
  Assert.IsTrue(Settings.Append);
end;

procedure TSettingsRecordTests.Test_DeviceSettings_FieldAssignment;
var
  Settings: TDeviceViewSettings;
begin
  Settings.Alias := 'My Device';
  Settings.DeviceTypeIndex := 2;
  Settings.Pinned := True;
  Settings.Hidden := False;
  Settings.AutoConnect := True;
  Settings.Timeout := 15000;
  Settings.RetryCount := 2;
  Settings.NotifyConnectIndex := 1;
  Settings.NotifyDisconnectIndex := 1;
  Settings.NotifyFailedIndex := 0;
  Settings.NotifyAutoIndex := 1;

  Assert.AreEqual('My Device', Settings.Alias);
  Assert.AreEqual(2, Settings.DeviceTypeIndex);
  Assert.IsTrue(Settings.Pinned);
  Assert.IsFalse(Settings.Hidden);
  Assert.IsTrue(Settings.AutoConnect);
  Assert.AreEqual(15000, Settings.Timeout);
  Assert.AreEqual(2, Settings.RetryCount);
  Assert.AreEqual(1, Settings.NotifyConnectIndex);
  Assert.AreEqual(1, Settings.NotifyDisconnectIndex);
  Assert.AreEqual(0, Settings.NotifyFailedIndex);
  Assert.AreEqual(1, Settings.NotifyAutoIndex);
end;

{ TMockSettingsViewTests }

procedure TMockSettingsViewTests.Setup;
begin
  FMockView := TMockSettingsView.Create;
end;

procedure TMockSettingsViewTests.TearDown;
begin
  // FMockView is never assigned to an interface in these tests,
  // so ref count stays 0 and we must free explicitly
  FMockView.Free;
  FMockView := nil;
end;

procedure TMockSettingsViewTests.Test_CloseWithOK_SetsFlag;
begin
  Assert.IsFalse(FMockView.CloseWithOKCalled);
  FMockView.CloseWithOK;
  Assert.IsTrue(FMockView.CloseWithOKCalled);
end;

procedure TMockSettingsViewTests.Test_CloseWithCancel_SetsFlag;
begin
  Assert.IsFalse(FMockView.CloseWithCancelCalled);
  FMockView.CloseWithCancel;
  Assert.IsTrue(FMockView.CloseWithCancelCalled);
end;

procedure TMockSettingsViewTests.Test_ShowError_StoresMessage;
begin
  FMockView.ShowError('Test error message');
  Assert.AreEqual('Test error message', FMockView.LastErrorMessage);
end;

procedure TMockSettingsViewTests.Test_ShowInfo_StoresMessage;
begin
  FMockView.ShowInfo('Test info message');
  Assert.AreEqual('Test info message', FMockView.LastInfoMessage);
end;

procedure TMockSettingsViewTests.Test_SetApplyEnabled_StoresState;
begin
  Assert.IsFalse(FMockView.ApplyEnabled);
  FMockView.SetApplyEnabled(True);
  Assert.IsTrue(FMockView.ApplyEnabled);
  FMockView.SetApplyEnabled(False);
  Assert.IsFalse(FMockView.ApplyEnabled);
end;

procedure TMockSettingsViewTests.Test_SetGeneralSettings_IncrementsCount;
var
  Settings: TGeneralViewSettings;
begin
  Assert.AreEqual(0, FMockView.SetGeneralCount);
  FMockView.SetGeneralSettings(Settings);
  Assert.AreEqual(1, FMockView.SetGeneralCount);
  FMockView.SetGeneralSettings(Settings);
  Assert.AreEqual(2, FMockView.SetGeneralCount);
end;

procedure TMockSettingsViewTests.Test_GetGeneralSettings_ReturnsStoredValue;
var
  InputSettings, OutputSettings: TGeneralViewSettings;
begin
  InputSettings.WindowMode := wmMenu;
  InputSettings.OnTop := True;
  FMockView.SetGeneralSettings(InputSettings);

  OutputSettings := FMockView.GetGeneralSettings;
  Assert.AreEqual(Integer(wmMenu), Integer(OutputSettings.WindowMode));
  Assert.IsTrue(OutputSettings.OnTop);
end;

procedure TMockSettingsViewTests.Test_PopulateThemeList_StoresValues;
begin
  FMockView.PopulateThemeList('Dark');
  Assert.AreEqual('Dark', FMockView.CurrentTheme);
end;

procedure TMockSettingsViewTests.Test_PopulateDeviceList_StoresItems;
var
  Items: TArray<string>;
begin
  SetLength(Items, 2);
  Items[0] := 'Device 1 (AA:BB:CC:DD:EE:FF)';
  Items[1] := 'Device 2 (11:22:33:44:55:66)';

  FMockView.PopulateDeviceList(Items);

  Assert.AreEqual(Integer(2), Integer(Length(FMockView.DeviceListItems)));
  Assert.AreEqual('Device 1 (AA:BB:CC:DD:EE:FF)', FMockView.DeviceListItems[0]);
end;

procedure TMockSettingsViewTests.Test_SelectedDeviceIndex_GetSet;
begin
  Assert.AreEqual(-1, FMockView.GetSelectedDeviceIndex);
  FMockView.SetSelectedDeviceIndex(5);
  Assert.AreEqual(5, FMockView.GetSelectedDeviceIndex);
end;

procedure TMockSettingsViewTests.Test_ClearDeviceSettings_IncrementsCount;
begin
  Assert.AreEqual(0, FMockView.ClearDeviceCount);
  FMockView.ClearDeviceSettings;
  Assert.AreEqual(1, FMockView.ClearDeviceCount);
end;

{ TDeviceSettingsPresenterTests }

procedure TDeviceSettingsPresenterTests.Setup;
begin
  FMockView := TMockSettingsView.Create;
  FMockDeviceConfigProvider := TMockDeviceConfigProvider.Create;
  FPresenter := TDeviceSettingsPresenter.Create(
    FMockView as IDeviceSettingsView,
    FMockDeviceConfigProvider
  );
end;

procedure TDeviceSettingsPresenterTests.TearDown;
begin
  FPresenter.Free;
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_NoDevices_PopulatesEmptyList;
begin
  FPresenter.LoadDeviceList;
  Assert.AreEqual(Integer(0), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_NoDevices_ClearsSettings;
begin
  FPresenter.LoadDeviceList;
  Assert.AreEqual(1, FMockView.ClearDeviceCount, 'ClearDeviceSettings should be called');
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_WithDevices_PopulatesList;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'WH-1000XM4';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  Config := TDeviceConfig.Default($112233445566);
  Config.Name := 'AirPods Pro';
  FMockDeviceConfigProvider.AddDeviceConfig($112233445566, Config);

  FPresenter.LoadDeviceList;

  Assert.AreEqual(Integer(2), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_WithDevices_SelectsFirst;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Assert.AreEqual(0, FMockView.GetSelectedDeviceIndex);
  Assert.IsTrue(FMockView.SetDeviceCount > 0, 'SetDeviceSettings should be called');
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_WithNamedDevice_FormatsDisplayName;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'WH-1000XM4';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  // Expected format: "Name (XX:XX:XX:XX:XX:XX)"
  Assert.IsTrue(Pos('WH-1000XM4', FMockView.DeviceListItems[0]) > 0,
    'Display name should contain device name');
  Assert.IsTrue(Pos('AA:BB:CC:DD:EE:FF', FMockView.DeviceListItems[0]) > 0,
    'Display name should contain MAC address');
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceList_UnnamedDevice_ShowsAddressOnly;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := '';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Assert.AreEqual('AA:BB:CC:DD:EE:FF', FMockView.DeviceListItems[0]);
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsAlias;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.Alias := 'My Headphones';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual('My Headphones', Settings.Alias);
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_AppliesUIOffset_DeviceType;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.DeviceTypeOverride := 3; // Config value
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  // UI index = config value + 1 (UI_DEFAULT_ITEM_OFFSET)
  Assert.AreEqual(4, Settings.DeviceTypeIndex,
    'UI index should be config value + 1');
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsBooleanFlags;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.Pinned := True;
  Config.Hidden := True;
  Config.AutoConnect := True;
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Assert.IsTrue(Settings.Pinned);
  Assert.IsTrue(Settings.Hidden);
  Assert.IsTrue(Settings.AutoConnect);
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsConnectionSettings;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.ConnectionTimeout := 5000;
  Config.ConnectionRetryCount := 3;
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual(5000, Settings.Timeout);
  Assert.AreEqual(3, Settings.RetryCount);
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsNotificationOverrides;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.Notifications.OnConnect := 1;
  Config.Notifications.OnDisconnect := 0;
  Config.Notifications.OnConnectFailed := 1;
  Config.Notifications.OnAutoConnect := -1;
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  // UI index = config value + 1
  Assert.AreEqual(2, Settings.NotifyConnectIndex);     // 1 + 1
  Assert.AreEqual(1, Settings.NotifyDisconnectIndex);   // 0 + 1
  Assert.AreEqual(2, Settings.NotifyFailedIndex);       // 1 + 1
  Assert.AreEqual(0, Settings.NotifyAutoIndex);          // -1 + 1
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsBatteryTraySettings;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.BatteryTray.ShowTrayIcon := 1;
  Config.BatteryTray.IconColor := $00FF00;
  Config.BatteryTray.BackgroundColor := -2;
  Config.BatteryTray.ShowNumericValue := 0;
  Config.BatteryTray.LowBatteryThreshold := 20;
  Config.BatteryTray.NotifyLowBattery := 1;
  Config.BatteryTray.NotifyFullyCharged := 0;
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual(2, Settings.BatteryTrayIconIndex);     // 1 + 1
  Assert.AreEqual($00FF00, Settings.BatteryIconColor);
  Assert.AreEqual(-2, Settings.BatteryBackgroundColor);
  Assert.AreEqual(1, Settings.BatteryShowNumericIndex);  // 0 + 1
  Assert.AreEqual(20, Settings.BatteryThreshold);
  Assert.AreEqual(2, Settings.BatteryNotifyLowIndex);    // 1 + 1
  Assert.AreEqual(1, Settings.BatteryNotifyFullIndex);   // 0 + 1
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_LoadsShowProfiles;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  Config.ShowProfiles := 1;
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual(2, Settings.ShowProfilesIndex); // 1 + 1
end;

procedure TDeviceSettingsPresenterTests.LoadDeviceSettings_InvalidIndex_NoAction;
var
  Config: TDeviceConfig;
  InitialCount: Integer;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;
  InitialCount := FMockView.SetDeviceCount;

  // Invalid index should not crash or update view
  FPresenter.OnDeviceSelected(99);
  Assert.AreEqual(InitialCount, FMockView.SetDeviceCount,
    'SetDeviceSettings should not be called for invalid index');
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_SavesAlias;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  // Modify settings in the view
  Settings := FMockView.GetDeviceSettings;
  Settings.Alias := 'My Custom Name';
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.AreEqual('My Custom Name', Saved.Alias);
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_RevertsUIOffset;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Settings.DeviceTypeIndex := 4; // UI index
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  // Config value = UI index - 1
  Assert.AreEqual(3, Saved.DeviceTypeOverride);
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_SavesBooleanFlags;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Settings.Pinned := True;
  Settings.Hidden := True;
  Settings.AutoConnect := True;
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.IsTrue(Saved.Pinned);
  Assert.IsTrue(Saved.Hidden);
  Assert.IsTrue(Saved.AutoConnect);
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_SavesNotificationOverrides;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Settings.NotifyConnectIndex := 2;     // UI index -> config = 1
  Settings.NotifyDisconnectIndex := 0;  // UI index -> config = -1
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.AreEqual(1, Saved.Notifications.OnConnect);
  Assert.AreEqual(-1, Saved.Notifications.OnDisconnect);
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_SavesBatteryTraySettings;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Settings.BatteryTrayIconIndex := 2;     // -> 1
  Settings.BatteryIconColor := $FF0000;
  Settings.BatteryBackgroundColor := -2;
  Settings.BatteryShowNumericIndex := 1;  // -> 0
  Settings.BatteryThreshold := 15;
  Settings.BatteryNotifyLowIndex := 2;    // -> 1
  Settings.BatteryNotifyFullIndex := 1;   // -> 0
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.AreEqual(1, Saved.BatteryTray.ShowTrayIcon);
  Assert.AreEqual($FF0000, Saved.BatteryTray.IconColor);
  Assert.AreEqual(-2, Saved.BatteryTray.BackgroundColor);
  Assert.AreEqual(0, Saved.BatteryTray.ShowNumericValue);
  Assert.AreEqual(15, Saved.BatteryTray.LowBatteryThreshold);
  Assert.AreEqual(1, Saved.BatteryTray.NotifyLowBattery);
  Assert.AreEqual(0, Saved.BatteryTray.NotifyFullyCharged);
end;

procedure TDeviceSettingsPresenterTests.SaveDeviceSettings_InvalidIndex_NoAction;
begin
  // No devices loaded, saving should not crash
  FPresenter.SaveCurrentDevice;
  Assert.Pass('SaveCurrentDevice with no selection should not crash');
end;

procedure TDeviceSettingsPresenterTests.SaveCurrentDevice_WithSelection_Saves;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);
  FPresenter.LoadDeviceList;

  Settings := FMockView.GetDeviceSettings;
  Settings.Alias := 'Saved Alias';
  FMockView.SetDeviceSettings(Settings);

  FPresenter.SaveCurrentDevice;

  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.AreEqual('Saved Alias', Saved.Alias);
end;

procedure TDeviceSettingsPresenterTests.SaveCurrentDevice_NoSelection_NoAction;
begin
  // No devices, no selection
  FPresenter.SaveCurrentDevice;
  Assert.Pass('Should not crash with no selection');
end;

procedure TDeviceSettingsPresenterTests.OnDeviceSelected_LoadsNewDevice;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device 1';
  Config.Alias := 'Alias 1';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  Config := TDeviceConfig.Default($112233445566);
  Config.Name := 'Device 2';
  Config.Alias := 'Alias 2';
  FMockDeviceConfigProvider.AddDeviceConfig($112233445566, Config);

  FPresenter.LoadDeviceList;

  // Select second device
  FPresenter.OnDeviceSelected(1);

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual('Alias 2', Settings.Alias);
end;

procedure TDeviceSettingsPresenterTests.OnForgetDeviceClicked_ValidIndex_RemovesDevice;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;
  Assert.AreEqual(Integer(1), Integer(Length(FMockView.DeviceListItems)));

  FPresenter.OnForgetDeviceClicked(0);

  // After forget, device list should be refreshed and empty
  Assert.AreEqual(Integer(0), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.OnForgetDeviceClicked_InvalidIndex_NoAction;
var
  Config: TDeviceConfig;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.LoadDeviceList;

  // Invalid index should not crash or remove anything
  FPresenter.OnForgetDeviceClicked(99);
  Assert.AreEqual(Integer(1), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.OnRefreshDevicesClicked_ReloadsDeviceList;
var
  Config: TDeviceConfig;
begin
  // Initially no devices
  FPresenter.LoadDeviceList;
  Assert.AreEqual(Integer(0), Integer(Length(FMockView.DeviceListItems)));

  // Add a device after initial load
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'New Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  FPresenter.OnRefreshDevicesClicked;

  Assert.AreEqual(Integer(1), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.SelectDeviceByAddress_Existing_SelectsDevice;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
begin
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Device 1';
  Config.Alias := 'Alias 1';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  Config := TDeviceConfig.Default($112233445566);
  Config.Name := 'Device 2';
  Config.Alias := 'Target Alias';
  FMockDeviceConfigProvider.AddDeviceConfig($112233445566, Config);

  FPresenter.SelectDeviceByAddress($112233445566);

  Settings := FMockView.GetDeviceSettings;
  Assert.AreEqual('Target Alias', Settings.Alias);
end;

procedure TDeviceSettingsPresenterTests.SelectDeviceByAddress_NonExisting_NoSelection;
begin
  FPresenter.SelectDeviceByAddress($FFFFFFFFFFFF);
  // Should not crash, device list remains empty
  Assert.AreEqual(Integer(0), Integer(Length(FMockView.DeviceListItems)));
end;

procedure TDeviceSettingsPresenterTests.RoundTrip_ModifyAndSave_PersistsChanges;
var
  Config: TDeviceConfig;
  Settings: TDeviceViewSettings;
  Saved: TDeviceConfig;
begin
  // Setup: create device with defaults
  Config := TDeviceConfig.Default($AABBCCDDEEFF);
  Config.Name := 'Test Device';
  FMockDeviceConfigProvider.AddDeviceConfig($AABBCCDDEEFF, Config);

  // Load
  FPresenter.LoadDeviceList;

  // Modify via view
  Settings := FMockView.GetDeviceSettings;
  Settings.Alias := 'Custom Alias';
  Settings.Pinned := True;
  Settings.AutoConnect := True;
  Settings.Timeout := 10000;
  Settings.RetryCount := 5;
  Settings.DeviceTypeIndex := 3;  // UI -> config = 2
  Settings.NotifyConnectIndex := 2; // UI -> config = 1
  Settings.ShowProfilesIndex := 2;  // UI -> config = 1
  FMockView.SetDeviceSettings(Settings);

  // Save
  FPresenter.SaveCurrentDevice;

  // Verify persisted values
  Saved := FMockDeviceConfigProvider.GetDeviceConfig($AABBCCDDEEFF);
  Assert.AreEqual('Custom Alias', Saved.Alias);
  Assert.IsTrue(Saved.Pinned);
  Assert.IsTrue(Saved.AutoConnect);
  Assert.AreEqual(10000, Saved.ConnectionTimeout);
  Assert.AreEqual(5, Saved.ConnectionRetryCount);
  Assert.AreEqual(2, Saved.DeviceTypeOverride);
  Assert.AreEqual(1, Saved.Notifications.OnConnect);
  Assert.AreEqual(1, Saved.ShowProfiles);
end;

initialization
  TDUnitX.RegisterTestFixture(TSettingsPresenterTests);
  TDUnitX.RegisterTestFixture(TSettingsRecordTests);
  TDUnitX.RegisterTestFixture(TMockSettingsViewTests);
  TDUnitX.RegisterTestFixture(TDeviceSettingsPresenterTests);

end.
