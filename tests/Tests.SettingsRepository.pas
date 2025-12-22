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
  Config: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // File should be created
    Assert.IsTrue(TFile.Exists(FConfigPath));
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_NoFile_SetsDefaultValues;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Verify default values
    Assert.AreEqual(Ord(wmWindow), Ord(Config.WindowMode));
    Assert.IsFalse(Config.OnTop);
    Assert.IsFalse(Config.Autostart);
    Assert.AreEqual('Win+K', Config.Hotkey);
    Assert.AreEqual(2000, Config.PollingInterval);
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_ExistingFile_LoadsValues;
var
  Ini: TMemIniFile;
  Repo: ISettingsRepository;
  Config: TAppConfig;
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
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    Assert.AreEqual(Ord(wmMenu), Ord(Config.WindowMode));
    Assert.IsTrue(Config.OnTop);
    Assert.AreEqual('Ctrl+Alt+B', Config.Hotkey);
    Assert.AreEqual(5000, Config.PollingInterval);
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.LoadSettings_ValidatesRanges;
var
  Ini: TMemIniFile;
  Repo: ISettingsRepository;
  Config: TAppConfig;
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
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config);

    // Values should be clamped to valid ranges
    Assert.AreEqual(10000, Config.PollingInterval);
    Assert.AreEqual(30, Config.ItemHeight);
    Assert.AreEqual(64, Config.IconSize);
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_CreatesFile;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Repo.SaveSettings(Config);

    Assert.IsTrue(TFile.Exists(FConfigPath));
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesGeneralSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.WindowMode := wmMenu;
    Config.OnTop := True;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.AreEqual(Ord(wmMenu), Ini.ReadInteger('General', 'Window', -1));
      Assert.IsTrue(Ini.ReadBool('General', 'OnTop', False));
    finally
      Ini.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesWindowSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.MinimizeToTray := False;
    Config.CloseToTray := False;
    Repo.SaveSettings(Config);

    Ini := TMemIniFile.Create(FConfigPath);
    try
      Assert.IsFalse(Ini.ReadBool('Window', 'MinimizeToTray', True));
      Assert.IsFalse(Ini.ReadBool('Window', 'CloseToTray', True));
    finally
      Ini.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesPollingSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.PollingMode := pmPrimary;
    Config.PollingInterval := 3000;
    Config.EventDebounceMs := 750;
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
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesLogSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.LogEnabled := True;
    Config.LogFilename := 'custom.log';
    Config.LogAppend := True;
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
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesAppearanceSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.ShowAddresses := True;
    Config.Theme := 'Dark';
    Config.ShowLastSeen := True;
    Config.ShowDeviceIcons := False;
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
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesLayoutSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.ItemHeight := 80;
    Config.IconSize := 48;
    Config.CornerRadius := 10;
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
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveSettings_WritesDeviceSection;
var
  Repo: ISettingsRepository;
  Config: TAppConfig;
  Ini: TMemIniFile;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);
  Config := TAppConfig.Create;
  try
    Config.SetRepositories(Repo, FDeviceRepository);
    Config.ConnectionTimeout := 15000;
    Config.ConnectionRetryCount := 5;
    Config.NotifyOnConnect := nmNone;
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
    Config.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesWindowMode;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.WindowMode := wmMenu;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(wmMenu), Ord(Config2.WindowMode));
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesOnTop;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.OnTop := True;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(Config2.OnTop);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesHotkey;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.Hotkey := 'Ctrl+Shift+B';
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual('Ctrl+Shift+B', Config2.Hotkey);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesPollingSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.PollingMode := pmPrimary;
    Config1.PollingInterval := 4000;
    Config1.EventDebounceMs := 1000;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(pmPrimary), Ord(Config2.PollingMode));
    Assert.AreEqual(4000, Config2.PollingInterval);
    Assert.AreEqual(1000, Config2.EventDebounceMs);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesLogSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.LogEnabled := True;
    Config1.LogFilename := 'mylog.txt';
    Config1.LogAppend := True;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(Config2.LogEnabled);
    Assert.AreEqual('mylog.txt', Config2.LogFilename);
    Assert.IsTrue(Config2.LogAppend);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesAppearanceSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.ShowAddresses := True;
    Config1.Theme := 'CustomTheme';
    Config1.ShowLastSeen := True;
    Config1.LastSeenFormat := lsfAbsolute;
    Config1.ShowDeviceIcons := False;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.IsTrue(Config2.ShowAddresses);
    Assert.AreEqual('CustomTheme', Config2.Theme);
    Assert.IsTrue(Config2.ShowLastSeen);
    Assert.AreEqual(Ord(lsfAbsolute), Ord(Config2.LastSeenFormat));
    Assert.IsFalse(Config2.ShowDeviceIcons);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesLayoutSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.ItemHeight := 90;
    Config1.ItemPadding := 10;
    Config1.ItemMargin := 8;
    Config1.IconSize := 50;
    Config1.CornerRadius := 12;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(90, Config2.ItemHeight);
    Assert.AreEqual(10, Config2.ItemPadding);
    Assert.AreEqual(8, Config2.ItemMargin);
    Assert.AreEqual(50, Config2.IconSize);
    Assert.AreEqual(12, Config2.CornerRadius);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesConnectionSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.ConnectionTimeout := 20000;
    Config1.ConnectionRetryCount := 4;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(20000, Config2.ConnectionTimeout);
    Assert.AreEqual(4, Config2.ConnectionRetryCount);
  finally
    Config2.Free;
  end;
end;

procedure TIniSettingsRepositoryTests.SaveAndLoad_PreservesNotificationSettings;
var
  Repo: ISettingsRepository;
  Config1, Config2: TAppConfig;
begin
  Repo := TIniSettingsRepository.Create(FConfigPath, FDeviceRepository);

  Config1 := TAppConfig.Create;
  try
    Config1.SetRepositories(Repo, FDeviceRepository);
    Config1.NotifyOnConnect := nmNone;
    Config1.NotifyOnDisconnect := nmBalloon;
    Config1.NotifyOnConnectFailed := nmNone;
    Config1.NotifyOnAutoConnect := nmBalloon;
    Repo.SaveSettings(Config1);
  finally
    Config1.Free;
  end;

  Config2 := TAppConfig.Create;
  try
    Config2.SetRepositories(Repo, FDeviceRepository);
    Repo.LoadSettings(Config2);
    Assert.AreEqual(Ord(nmNone), Ord(Config2.NotifyOnConnect));
    Assert.AreEqual(Ord(nmBalloon), Ord(Config2.NotifyOnDisconnect));
    Assert.AreEqual(Ord(nmNone), Ord(Config2.NotifyOnConnectFailed));
    Assert.AreEqual(Ord(nmBalloon), Ord(Config2.NotifyOnAutoConnect));
  finally
    Config2.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIniSettingsRepositoryTests);

end.
