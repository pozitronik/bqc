{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Settings Repository Unit Tests                  }
{                                                       }
{*******************************************************}

unit Tests.SettingsRepository;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.IniFiles,
  App.ConfigEnums,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Test fixture for TIniSettingsRepository.
  /// Tests INI file persistence for application settings.
  /// </summary>
  [TestFixture]
  TIniSettingsRepositoryTests = class
  private
    FTempDir: string;
    FConfigPath: string;
    FDeviceRepository: IDeviceConfigRepository;  // For TAppConfig.SetRepositories
    FDevicePersistence: IDeviceConfigPersistence; // For TIniSettingsRepository.Create (ISP)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Constructor tests
    [Test]
    procedure Create_SetsConfigPath;
    [Test]
    procedure GetConfigPath_ReturnsPath;

    // Load tests - file doesn't exist
    [Test]
    procedure LoadSettings_NoFile_CreatesDefault;
    [Test]
    procedure LoadSettings_NoFile_SetsDefaultValues;

    // Load tests - file exists
    [Test]
    procedure LoadSettings_ExistingFile_LoadsValues;
    [Test]
    procedure LoadSettings_ValidatesRanges;

    // Save tests
    [Test]
    procedure SaveSettings_CreatesFile;
    [Test]
    procedure SaveSettings_WritesGeneralSection;
    [Test]
    procedure SaveSettings_WritesWindowSection;
    [Test]
    procedure SaveSettings_WritesPollingSection;
    [Test]
    procedure SaveSettings_WritesLogSection;
    [Test]
    procedure SaveSettings_WritesAppearanceSection;
    [Test]
    procedure SaveSettings_WritesLayoutSection;
    [Test]
    procedure SaveSettings_WritesDeviceSection;

    // Round-trip tests
    [Test]
    procedure SaveAndLoad_PreservesWindowMode;
    [Test]
    procedure SaveAndLoad_PreservesOnTop;
    [Test]
    procedure SaveAndLoad_PreservesHotkey;
    [Test]
    procedure SaveAndLoad_PreservesPollingSettings;
    [Test]
    procedure SaveAndLoad_PreservesLogSettings;
    [Test]
    procedure SaveAndLoad_PreservesAppearanceSettings;
    [Test]
    procedure SaveAndLoad_PreservesLayoutSettings;
    [Test]
    procedure SaveAndLoad_PreservesConnectionSettings;
    [Test]
    procedure SaveAndLoad_PreservesNotificationSettings;
  end;

implementation

uses
  App.Config,
  App.SettingsRepository,
  App.DeviceConfigRepository;

{ TIniSettingsRepositoryTests }

procedure TIniSettingsRepositoryTests.Setup;
begin
  // Create temp directory for test files
  FTempDir := TPath.Combine(TPath.GetTempPath, 'BQCTests_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  FConfigPath := TPath.Combine(FTempDir, 'test.ini');

  // Create device repository (implements both interfaces)
  FDeviceRepository := CreateDeviceConfigRepository;
  // Query persistence interface for TIniSettingsRepository (ISP-compliant)
  FDevicePersistence := FDeviceRepository as IDeviceConfigPersistence;
end;

procedure TIniSettingsRepositoryTests.TearDown;
begin
  FDevicePersistence := nil;
  FDeviceRepository := nil;

  // Clean up temp directory
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TIniSettingsRepositoryTests.Create_SetsConfigPath;
var
  Repo: ISettingsRepository;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  Assert.AreEqual(FConfigPath, Repo.ConfigPath);
end;

procedure TIniSettingsRepositoryTests.GetConfigPath_ReturnsPath;
var
  Repo: ISettingsRepository;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  Assert.AreEqual(FConfigPath, Repo.GetConfigPath);
end;

procedure TIniSettingsRepositoryTests.LoadSettings_NoFile_CreatesDefault;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;  // Hold interface reference to prevent premature destruction
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // File should be created
    Assert.IsTrue(TFile.Exists(FConfigPath));
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_NoFile_SetsDefaultValues;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Verify default values
    Assert.AreEqual(Ord(wmWindow), Ord(ConfigObj.AsGeneralConfig.WindowMode));
    Assert.IsFalse(ConfigObj.AsGeneralConfig.OnTop);
    Assert.IsFalse(ConfigObj.AsGeneralConfig.Autostart);
    Assert.AreEqual('Win+K', ConfigObj.AsHotkeyConfig.Hotkey);
    Assert.AreEqual(2000, ConfigObj.AsPollingConfig.PollingInterval);
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_ExistingFile_LoadsValues;
var
  Ini: TMemIniFile;
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  // Create INI file with custom values
  Ini := TMemIniFile.Create(FConfigPath);
  try
    Ini.WriteInteger('General', 'Window', Ord(wmMenu));
    Ini.WriteBool('General', 'OnTop', True);
    Ini.WriteString('Hotkey', 'GlobalHotkey', 'Ctrl+Alt+B');
    Ini.WriteInteger('Polling', 'Interval', 5000);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    Assert.AreEqual(Ord(wmMenu), Ord(ConfigObj.AsGeneralConfig.WindowMode));
    Assert.IsTrue(ConfigObj.AsGeneralConfig.OnTop);
    Assert.AreEqual('Ctrl+Alt+B', ConfigObj.AsHotkeyConfig.Hotkey);
    Assert.AreEqual(5000, ConfigObj.AsPollingConfig.PollingInterval);
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_ValidatesRanges;
var
  Ini: TMemIniFile;
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  // Create INI file with out-of-range values
  Ini := TMemIniFile.Create(FConfigPath);
  try
    Ini.WriteInteger('Polling', 'Interval', 999999);  // Max is 10000
    Ini.WriteInteger('Layout', 'ItemHeight', 5);       // Min is 30
    Ini.WriteInteger('Layout', 'IconSize', 1000);      // Max is 64
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Values should be clamped to valid ranges
    Assert.AreEqual(10000, ConfigObj.AsPollingConfig.PollingInterval);
    Assert.AreEqual(30, ConfigObj.AsLayoutConfig.ItemHeight);
    Assert.AreEqual(64, ConfigObj.AsLayoutConfig.IconSize);
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_CreatesFile;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.SaveSettings(Config);

    Assert.IsTrue(TFile.Exists(FConfigPath));
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesGeneralSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsGeneralConfig.WindowMode := wmMenu;
    ConfigObj.AsGeneralConfig.OnTop := True;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.AreEqual(Ord(wmMenu), Ini.ReadInteger('General', 'Window', -1));
      Assert.IsTrue(Ini.ReadBool('General', 'OnTop', False));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesWindowSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsWindowConfig.MinimizeToTray := False;
    ConfigObj.AsWindowConfig.CloseToTray := False;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.IsFalse(Ini.ReadBool('Window', 'MinimizeToTray', True));
      Assert.IsFalse(Ini.ReadBool('Window', 'CloseToTray', True));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesPollingSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsPollingConfig.PollingMode := pmPrimary;
    ConfigObj.AsPollingConfig.PollingInterval := 3000;
    ConfigObj.AsPollingConfig.EventDebounceMs := 750;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.AreEqual(Ord(pmPrimary), Ini.ReadInteger('Polling', 'Mode', -1));
      Assert.AreEqual(3000, Ini.ReadInteger('Polling', 'Interval', 0));
      Assert.AreEqual(750, Ini.ReadInteger('Polling', 'EventDebounceMs', 0));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesLogSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsLogConfig.LogEnabled := True;
    ConfigObj.AsLogConfig.LogFilename := 'custom.log';
    ConfigObj.AsLogConfig.LogAppend := True;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.IsTrue(Ini.ReadBool('Log', 'Enabled', False));
      Assert.AreEqual('custom.log', Ini.ReadString('Log', 'Filename', ''));
      Assert.IsTrue(Ini.ReadBool('Log', 'Append', False));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesAppearanceSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsAppearanceConfig.ShowAddresses := True;
    ConfigObj.AsAppearanceConfig.Theme := 'Dark';
    ConfigObj.AsAppearanceConfig.ShowLastSeen := True;
    ConfigObj.AsAppearanceConfig.ShowDeviceIcons := False;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.IsTrue(Ini.ReadBool('Appearance', 'ShowAddresses', False));
      Assert.AreEqual('Dark', Ini.ReadString('Appearance', 'Theme', ''));
      Assert.IsTrue(Ini.ReadBool('Appearance', 'ShowLastSeen', False));
      Assert.IsFalse(Ini.ReadBool('Appearance', 'ShowDeviceIcons', True));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesLayoutSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsLayoutConfig.ItemHeight := 80;
    ConfigObj.AsLayoutConfig.IconSize := 48;
    ConfigObj.AsLayoutConfig.CornerRadius := 10;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.AreEqual(80, Ini.ReadInteger('Layout', 'ItemHeight', 0));
      Assert.AreEqual(48, Ini.ReadInteger('Layout', 'IconSize', 0));
      Assert.AreEqual(10, Ini.ReadInteger('Layout', 'CornerRadius', 0));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesDeviceSection;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.AsConnectionConfig.ConnectionTimeout := 15000;
    ConfigObj.AsConnectionConfig.ConnectionRetryCount := 5;
    ConfigObj.AsNotificationConfig.NotifyOnConnect := nmNone;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.AreEqual(15000, Ini.ReadInteger('Device', 'ConnectionTimeout', 0));
      Assert.AreEqual(5, Ini.ReadInteger('Device', 'ConnectionRetryCount', 0));
      Assert.AreEqual(Ord(nmNone), Ini.ReadInteger('Device', 'NotifyOnConnect', -1));
    finally
      Ini.Free;
    end;
  finally
    Config := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesWindowMode;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsGeneralConfig.WindowMode := wmMenu;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(wmMenu), Ord(ConfigObj2.AsGeneralConfig.WindowMode));
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesOnTop;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsGeneralConfig.OnTop := True;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.AsGeneralConfig.OnTop);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesHotkey;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsHotkeyConfig.Hotkey := 'Ctrl+Shift+B';
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual('Ctrl+Shift+B', ConfigObj2.AsHotkeyConfig.Hotkey);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesPollingSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsPollingConfig.PollingMode := pmPrimary;
    ConfigObj1.AsPollingConfig.PollingInterval := 4000;
    ConfigObj1.AsPollingConfig.EventDebounceMs := 1000;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(pmPrimary), Ord(ConfigObj2.AsPollingConfig.PollingMode));
    Assert.AreEqual(4000, ConfigObj2.AsPollingConfig.PollingInterval);
    Assert.AreEqual(1000, ConfigObj2.AsPollingConfig.EventDebounceMs);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesLogSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsLogConfig.LogEnabled := True;
    ConfigObj1.AsLogConfig.LogFilename := 'mylog.txt';
    ConfigObj1.AsLogConfig.LogAppend := True;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.AsLogConfig.LogEnabled);
    Assert.AreEqual('mylog.txt', ConfigObj2.AsLogConfig.LogFilename);
    Assert.IsTrue(ConfigObj2.AsLogConfig.LogAppend);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesAppearanceSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsAppearanceConfig.ShowAddresses := True;
    ConfigObj1.AsAppearanceConfig.Theme := 'CustomTheme';
    ConfigObj1.AsAppearanceConfig.ShowLastSeen := True;
    ConfigObj1.AsAppearanceConfig.LastSeenFormat := lsfAbsolute;
    ConfigObj1.AsAppearanceConfig.ShowDeviceIcons := False;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.AsAppearanceConfig.ShowAddresses);
    Assert.AreEqual('CustomTheme', ConfigObj2.AsAppearanceConfig.Theme);
    Assert.IsTrue(ConfigObj2.AsAppearanceConfig.ShowLastSeen);
    Assert.AreEqual(Ord(lsfAbsolute), Ord(ConfigObj2.AsAppearanceConfig.LastSeenFormat));
    Assert.IsFalse(ConfigObj2.AsAppearanceConfig.ShowDeviceIcons);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesLayoutSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsLayoutConfig.ItemHeight := 90;
    ConfigObj1.AsLayoutConfig.ItemPadding := 10;
    ConfigObj1.AsLayoutConfig.ItemMargin := 8;
    ConfigObj1.AsLayoutConfig.IconSize := 50;
    ConfigObj1.AsLayoutConfig.CornerRadius := 12;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(90, ConfigObj2.AsLayoutConfig.ItemHeight);
    Assert.AreEqual(10, ConfigObj2.AsLayoutConfig.ItemPadding);
    Assert.AreEqual(8, ConfigObj2.AsLayoutConfig.ItemMargin);
    Assert.AreEqual(50, ConfigObj2.AsLayoutConfig.IconSize);
    Assert.AreEqual(12, ConfigObj2.AsLayoutConfig.CornerRadius);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesConnectionSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsConnectionConfig.ConnectionTimeout := 20000;
    ConfigObj1.AsConnectionConfig.ConnectionRetryCount := 4;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(20000, ConfigObj2.AsConnectionConfig.ConnectionTimeout);
    Assert.AreEqual(4, ConfigObj2.AsConnectionConfig.ConnectionRetryCount);
  finally
    Config2 := nil;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesNotificationSettings;
var
  Repo: ISettingsRepository;
  ConfigObj1, ConfigObj2: TAppConfig;
  Config1, Config2: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDevicePersistence);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.AsNotificationConfig.NotifyOnConnect := nmNone;
    ConfigObj1.AsNotificationConfig.NotifyOnDisconnect := nmBalloon;
    ConfigObj1.AsNotificationConfig.NotifyOnConnectFailed := nmNone;
    ConfigObj1.AsNotificationConfig.NotifyOnAutoConnect := nmBalloon;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(nmNone), Ord(ConfigObj2.AsNotificationConfig.NotifyOnConnect));
    Assert.AreEqual(Ord(nmBalloon), Ord(ConfigObj2.AsNotificationConfig.NotifyOnDisconnect));
    Assert.AreEqual(Ord(nmNone), Ord(ConfigObj2.AsNotificationConfig.NotifyOnConnectFailed));
    Assert.AreEqual(Ord(nmBalloon), Ord(ConfigObj2.AsNotificationConfig.NotifyOnAutoConnect));
  finally
    Config2 := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIniSettingsRepositoryTests);

end.
