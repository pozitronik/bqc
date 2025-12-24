{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Settings Repository Unit Tests                  }
{                                                       }
{       Copyright (c) 2024                              }
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
    FDeviceRepository: IDeviceConfigRepository;
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

  // Create device repository for settings repository
  FDeviceRepository := CreateDeviceConfigRepository;
end;

procedure TIniSettingsRepositoryTests.TearDown;
begin
  FDeviceRepository := nil;

  // Clean up temp directory
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TIniSettingsRepositoryTests.Create_SetsConfigPath;
var
  Repo: ISettingsRepository;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Assert.AreEqual(FConfigPath, Repo.ConfigPath);
end;

procedure TIniSettingsRepositoryTests.GetConfigPath_ReturnsPath;
var
  Repo: ISettingsRepository;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Assert.AreEqual(FConfigPath, Repo.GetConfigPath);
end;

procedure TIniSettingsRepositoryTests.LoadSettings_NoFile_CreatesDefault;
var
  Repo: ISettingsRepository;
  ConfigObj: TAppConfig;
  Config: IAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Verify default values
    Assert.AreEqual(Ord(wmWindow), Ord(ConfigObj.WindowMode));
    Assert.IsFalse(ConfigObj.OnTop);
    Assert.IsFalse(ConfigObj.Autostart);
    Assert.AreEqual('Win+K', ConfigObj.Hotkey);
    Assert.AreEqual(2000, ConfigObj.PollingInterval);
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

  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    Assert.AreEqual(Ord(wmMenu), Ord(ConfigObj.WindowMode));
    Assert.IsTrue(ConfigObj.OnTop);
    Assert.AreEqual('Ctrl+Alt+B', ConfigObj.Hotkey);
    Assert.AreEqual(5000, ConfigObj.PollingInterval);
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

  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Values should be clamped to valid ranges
    Assert.AreEqual(10000, ConfigObj.PollingInterval);
    Assert.AreEqual(30, ConfigObj.ItemHeight);
    Assert.AreEqual(64, ConfigObj.IconSize);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.WindowMode := wmMenu;
    ConfigObj.OnTop := True;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.MinimizeToTray := False;
    ConfigObj.CloseToTray := False;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.PollingMode := pmPrimary;
    ConfigObj.PollingInterval := 3000;
    ConfigObj.EventDebounceMs := 750;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.LogEnabled := True;
    ConfigObj.LogFilename := 'custom.log';
    ConfigObj.LogAppend := True;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.ShowAddresses := True;
    ConfigObj.Theme := 'Dark';
    ConfigObj.ShowLastSeen := True;
    ConfigObj.ShowDeviceIcons := False;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.ItemHeight := 80;
    ConfigObj.IconSize := 48;
    ConfigObj.CornerRadius := 10;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  ConfigObj := TAppConfig.Create;
  Config := ConfigObj;
  try
    ConfigObj.SetRepositories(Repo, FDeviceRepository);
    ConfigObj.ConnectionTimeout := 15000;
    ConfigObj.ConnectionRetryCount := 5;
    ConfigObj.NotifyOnConnect := nmNone;
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.WindowMode := wmMenu;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(wmMenu), Ord(ConfigObj2.WindowMode));
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.OnTop := True;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.OnTop);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.Hotkey := 'Ctrl+Shift+B';
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual('Ctrl+Shift+B', ConfigObj2.Hotkey);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.PollingMode := pmPrimary;
    ConfigObj1.PollingInterval := 4000;
    ConfigObj1.EventDebounceMs := 1000;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(pmPrimary), Ord(ConfigObj2.PollingMode));
    Assert.AreEqual(4000, ConfigObj2.PollingInterval);
    Assert.AreEqual(1000, ConfigObj2.EventDebounceMs);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.LogEnabled := True;
    ConfigObj1.LogFilename := 'mylog.txt';
    ConfigObj1.LogAppend := True;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.LogEnabled);
    Assert.AreEqual('mylog.txt', ConfigObj2.LogFilename);
    Assert.IsTrue(ConfigObj2.LogAppend);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.ShowAddresses := True;
    ConfigObj1.Theme := 'CustomTheme';
    ConfigObj1.ShowLastSeen := True;
    ConfigObj1.LastSeenFormat := lsfAbsolute;
    ConfigObj1.ShowDeviceIcons := False;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(ConfigObj2.ShowAddresses);
    Assert.AreEqual('CustomTheme', ConfigObj2.Theme);
    Assert.IsTrue(ConfigObj2.ShowLastSeen);
    Assert.AreEqual(Ord(lsfAbsolute), Ord(ConfigObj2.LastSeenFormat));
    Assert.IsFalse(ConfigObj2.ShowDeviceIcons);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.ItemHeight := 90;
    ConfigObj1.ItemPadding := 10;
    ConfigObj1.ItemMargin := 8;
    ConfigObj1.IconSize := 50;
    ConfigObj1.CornerRadius := 12;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(90, ConfigObj2.ItemHeight);
    Assert.AreEqual(10, ConfigObj2.ItemPadding);
    Assert.AreEqual(8, ConfigObj2.ItemMargin);
    Assert.AreEqual(50, ConfigObj2.IconSize);
    Assert.AreEqual(12, ConfigObj2.CornerRadius);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.ConnectionTimeout := 20000;
    ConfigObj1.ConnectionRetryCount := 4;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(20000, ConfigObj2.ConnectionTimeout);
    Assert.AreEqual(4, ConfigObj2.ConnectionRetryCount);
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
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  ConfigObj1 := TAppConfig.Create;
  Config1 := ConfigObj1;
  try
    ConfigObj1.SetRepositories(Repo, FDeviceRepository);
    ConfigObj1.NotifyOnConnect := nmNone;
    ConfigObj1.NotifyOnDisconnect := nmBalloon;
    ConfigObj1.NotifyOnConnectFailed := nmNone;
    ConfigObj1.NotifyOnAutoConnect := nmBalloon;
    Repo.SaveSettings(Config1);
  finally
    Config1 := nil;
  end;

  ConfigObj2 := TAppConfig.Create;
  Config2 := ConfigObj2;
  try
    ConfigObj2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(nmNone), Ord(ConfigObj2.NotifyOnConnect));
    Assert.AreEqual(Ord(nmBalloon), Ord(ConfigObj2.NotifyOnDisconnect));
    Assert.AreEqual(Ord(nmNone), Ord(ConfigObj2.NotifyOnConnectFailed));
    Assert.AreEqual(Ord(nmBalloon), Ord(ConfigObj2.NotifyOnAutoConnect));
  finally
    Config2 := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIniSettingsRepositoryTests);

end.
