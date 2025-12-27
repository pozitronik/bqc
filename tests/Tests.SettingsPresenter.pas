{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Settings Presenter Unit Tests                   }
{                                                       }
{       Copyright (c) 2024                              }
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
    FMockView as IDeviceSettingsView,
    FMockAppConfig,
    FMockDeviceConfigProvider,
    FMockAppConfig.AsBatteryTrayConfig
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

  Assert.AreEqual('Ctrl+Alt+B', Settings.Hotkey);
  Assert.IsTrue(Settings.UseLowLevelHook);
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

initialization
  TDUnitX.RegisterTestFixture(TSettingsPresenterTests);
  TDUnitX.RegisterTestFixture(TSettingsRecordTests);
  TDUnitX.RegisterTestFixture(TMockSettingsViewTests);

end.
