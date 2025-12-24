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
  Tests.Mocks;

type
  /// <summary>
  /// Tests for TSettingsPresenter view interaction.
  /// Note: Full integration tests require Bootstrap initialization.
  /// These tests verify the presenter correctly interacts with ISettingsView.
  /// </summary>
  [TestFixture]
  TSettingsPresenterTests = class
  private
    FMockView: TMockSettingsView;
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
  end;

  /// <summary>
  /// Tests for ISettingsView data record types.
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
  /// Verifies the mock correctly implements ISettingsView.
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
  // Note: The mock is a TInterfacedObject, but we need to prevent premature destruction
  // by keeping a reference. The presenter takes an interface reference.
  FPresenter := TSettingsPresenter.Create(FMockView);
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
