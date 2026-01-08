program BQCTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  FastMM5 in 'FastMM5\FastMM5.pas',
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  Tests.Bluetooth.Types in 'Tests.Bluetooth.Types.pas',
  Tests.App.Config in 'Tests.App.Config.pas',
  Tests.ConnectionStrategies in 'Tests.ConnectionStrategies.pas',
  Tests.PairingStrategies in 'Tests.PairingStrategies.pas',
  Tests.DeviceMonitors in 'Tests.DeviceMonitors.pas',
  Tests.DeviceRepository in 'Tests.DeviceRepository.pas',
  Tests.ConnectionExecutor in 'Tests.ConnectionExecutor.pas',
  Tests.AdapterQuery in 'Tests.AdapterQuery.pas',
  Tests.DeviceConfigRepository in 'Tests.DeviceConfigRepository.pas',
  Tests.SettingsRepository in 'Tests.SettingsRepository.pas',
  Tests.DeviceFormatter in 'Tests.DeviceFormatter.pas',
  Tests.DeviceSorter in 'Tests.DeviceSorter.pas',
  Tests.DeviceDisplayItemBuilder in 'Tests.DeviceDisplayItemBuilder.pas',
  Tests.ListGeometry in 'Tests.ListGeometry.pas',
  Tests.BluetoothService in 'Tests.BluetoothService.pas',
  Tests.Logger in 'Tests.Logger.pas',
  Tests.SettingsPresenter in 'Tests.SettingsPresenter.pas',
  Tests.MainPresenter in 'Tests.MainPresenter.pas',
  Tests.WinAPI in 'Tests.WinAPI.pas',
  Tests.DeviceConverter in 'Tests.DeviceConverter.pas',
  Tests.RadioStateManager in 'Tests.RadioStateManager.pas',
  Tests.AutostartManager in 'Tests.AutostartManager.pas',
  Tests.EventDebouncer in 'Tests.EventDebouncer.pas',
  Tests.DeviceConfigProvider in 'Tests.DeviceConfigProvider.pas',
  Tests.WindowPositioner in 'Tests.WindowPositioner.pas',
  Tests.BatteryIconRenderer in 'Tests.BatteryIconRenderer.pas',
  Tests.BatteryTrayManager in 'Tests.BatteryTrayManager.pas',
  Tests.ConfigSection.BatteryTray in 'Tests.ConfigSection.BatteryTray.pas',
  Tests.ConfigSection.General in 'Tests.ConfigSection.General.pas',
  Tests.ConfigSection.Window in 'Tests.ConfigSection.Window.pas',
  Tests.ConfigSection.Position in 'Tests.ConfigSection.Position.pas',
  Tests.ConfigSection.Hotkey in 'Tests.ConfigSection.Hotkey.pas',
  Tests.ConfigSection.Polling in 'Tests.ConfigSection.Polling.pas',
  Tests.ConfigSection.Log in 'Tests.ConfigSection.Log.pas',
  Tests.ConfigSection.Appearance in 'Tests.ConfigSection.Appearance.pas',
  Tests.ConfigSection.Layout in 'Tests.ConfigSection.Layout.pas',
  Tests.ConfigSection.Connection in 'Tests.ConfigSection.Connection.pas',
  Tests.ConfigSection.Notification in 'Tests.ConfigSection.Notification.pas',
  Tests.ConfigSection.Profile in 'Tests.ConfigSection.Profile.pas',
  Tests.ConfigSectionTypes in 'Tests.ConfigSectionTypes.pas',
  Tests.SystemThemeDetector in 'Tests.SystemThemeDetector.pas',
  Tests.HotkeyManager in 'Tests.HotkeyManager.pas',
  Tests.BatteryQuery in 'Tests.BatteryQuery.pas',
  Tests.Mocks in 'Tests.Mocks.pas',
  Tests.Mocks.Config in 'Tests.Mocks.Config.pas',
  Tests.Mocks.View in 'Tests.Mocks.View.pas',
  Tests.Mocks.Bluetooth in 'Tests.Mocks.Bluetooth.pas',
  Tests.Mocks.Infrastructure in 'Tests.Mocks.Infrastructure.pas',
  Tests.AsyncExecutor in 'Tests.AsyncExecutor.pas',
  Tests.WinRTDeviceQuery in 'Tests.WinRTDeviceQuery.pas',
  Tests.WinRTAsyncHelpers in 'Tests.WinRTAsyncHelpers.pas',
  Tests.ProfileQuery in 'Tests.ProfileQuery.pas',
  Tests.DeviceList in 'Tests.DeviceList.pas',
  Tests.DeviceListTypes in 'Tests.DeviceListTypes.pas',
  Tests.DeviceItemRenderer in 'Tests.DeviceItemRenderer.pas',
  Bluetooth.Types in '..\src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in '..\src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in '..\src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.PairingStrategies in '..\src\Application\Bluetooth.PairingStrategies.pas',
  Bluetooth.PairingService in '..\src\Application\Bluetooth.PairingService.pas',
  Bluetooth.EventDebouncer in '..\src\Application\Bluetooth.EventDebouncer.pas',
  Bluetooth.Service in '..\src\Application\Bluetooth.Service.pas',
  Bluetooth.WinAPI in '..\src\Infrastructure\Bluetooth.WinAPI.pas',
  Bluetooth.DeviceMonitors in '..\src\Infrastructure\Bluetooth.DeviceMonitors.pas',
  Bluetooth.WinRTDeviceQuery in '..\src\Infrastructure\Bluetooth.WinRTDeviceQuery.pas',
  Bluetooth.DeviceRepository in '..\src\Infrastructure\Bluetooth.DeviceRepository.pas',
  Bluetooth.ConnectionExecutor in '..\src\Infrastructure\Bluetooth.ConnectionExecutor.pas',
  Bluetooth.AdapterQuery in '..\src\Infrastructure\Bluetooth.AdapterQuery.pas',
  Bluetooth.DeviceConverter in '..\src\Infrastructure\Bluetooth.DeviceConverter.pas',
  Bluetooth.DeviceWatcher in '..\src\Infrastructure\Bluetooth.DeviceWatcher.pas',
  Bluetooth.RadioControl in '..\src\Infrastructure\Bluetooth.RadioControl.pas',
  Bluetooth.BatteryQuery in '..\src\Infrastructure\Bluetooth.BatteryQuery.pas',
  Bluetooth.ProfileQuery in '..\src\Infrastructure\Bluetooth.ProfileQuery.pas',
  App.ConfigEnums in '..\src\Infrastructure\App.ConfigEnums.pas',
  App.DeviceConfigTypes in '..\src\Infrastructure\App.DeviceConfigTypes.pas',
  App.LayoutConfigIntf in '..\src\Infrastructure\App.LayoutConfigIntf.pas',
  App.AppearanceConfigIntf in '..\src\Infrastructure\App.AppearanceConfigIntf.pas',
  App.ConnectionConfigIntf in '..\src\Infrastructure\App.ConnectionConfigIntf.pas',
  App.NotificationConfigIntf in '..\src\Infrastructure\App.NotificationConfigIntf.pas',
  App.LogConfigIntf in '..\src\Infrastructure\App.LogConfigIntf.pas',
  App.ConfigInterfaces in '..\src\Infrastructure\App.ConfigInterfaces.pas',
  App.ConfigSectionTypes in '..\src\Infrastructure\App.ConfigSectionTypes.pas',
  App.ConfigSection.General in '..\src\Infrastructure\App.ConfigSection.General.pas',
  App.ConfigSection.Window in '..\src\Infrastructure\App.ConfigSection.Window.pas',
  App.ConfigSection.Position in '..\src\Infrastructure\App.ConfigSection.Position.pas',
  App.ConfigSection.Hotkey in '..\src\Infrastructure\App.ConfigSection.Hotkey.pas',
  App.ConfigSection.Polling in '..\src\Infrastructure\App.ConfigSection.Polling.pas',
  App.ConfigSection.Log in '..\src\Infrastructure\App.ConfigSection.Log.pas',
  App.ConfigSection.Appearance in '..\src\Infrastructure\App.ConfigSection.Appearance.pas',
  App.ConfigSection.Layout in '..\src\Infrastructure\App.ConfigSection.Layout.pas',
  App.ConfigSection.Connection in '..\src\Infrastructure\App.ConfigSection.Connection.pas',
  App.ConfigSection.Notification in '..\src\Infrastructure\App.ConfigSection.Notification.pas',
  App.Config in '..\src\Infrastructure\App.Config.pas',
  App.SettingsRepository in '..\src\Infrastructure\App.SettingsRepository.pas',
  App.DeviceConfigRepository in '..\src\Infrastructure\App.DeviceConfigRepository.pas',
  App.DeviceConfigProvider in '..\src\Infrastructure\App.DeviceConfigProvider.pas',
  App.Bootstrap in '..\src\Infrastructure\App.Bootstrap.pas',
  App.Logger in '..\src\Infrastructure\App.Logger.pas',
  App.AsyncExecutor in '..\src\Infrastructure\App.AsyncExecutor.pas',
  App.SystemClock in '..\src\Infrastructure\App.SystemClock.pas',
  App.SettingsPresenter in '..\src\Application\App.SettingsPresenter.pas',
  App.MainPresenter in '..\src\Application\App.MainPresenter.pas',
  App.MainViewInterfaces in '..\src\Application\App.MainViewInterfaces.pas',
  App.DeviceDisplayTypes in '..\src\Application\App.DeviceDisplayTypes.pas',
  App.Autostart in '..\src\Infrastructure\App.Autostart.pas',
  UI.DeviceList in '..\src\Presentation\UI.DeviceList.pas',
  UI.ListGeometry in '..\src\Presentation\UI.ListGeometry.pas',
  App.DeviceFormatter in '..\src\Application\App.DeviceFormatter.pas',
  App.DeviceSorter in '..\src\Application\App.DeviceSorter.pas',
  App.DeviceDisplayItemBuilder in '..\src\Application\App.DeviceDisplayItemBuilder.pas',
  UI.Theme in '..\src\Presentation\UI.Theme.pas',
  UI.WindowPositioner in '..\src\Presentation\UI.WindowPositioner.pas',
  UI.BatteryIconRenderer in '..\src\Presentation\UI.BatteryIconRenderer.pas',
  UI.BatteryTrayManager in '..\src\Presentation\UI.BatteryTrayManager.pas',
  UI.HotkeyManager in '..\src\Presentation\UI.HotkeyManager.pas',
  App.BatteryTrayConfigIntf in '..\src\Infrastructure\App.BatteryTrayConfigIntf.pas',
  App.ConfigSection.BatteryTray in '..\src\Infrastructure\App.ConfigSection.BatteryTray.pas',
  App.SystemThemeDetector in '..\src\Infrastructure\App.SystemThemeDetector.pas',
  App.WinRTSupport in '..\src\Infrastructure\App.WinRTSupport.pas',
  WinRT.AsyncHelpers in '..\src\Infrastructure\WinRT.AsyncHelpers.pas',
  App.ProfileConfigIntf in '..\src\Infrastructure\App.ProfileConfigIntf.pas',
  App.ConfigSection.Profile in '..\src\Infrastructure\App.ConfigSection.Profile.pas';


{$IFNDEF TESTINSIGHT}
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
{$ENDIF}

begin
  ReportMemoryLeaksOnShutdown := True;

{$IFDEF TESTINSIGHT}
  try
    TestInsight.DUnitX.RunRegisteredTests;
  finally
    // Shutdown Bootstrap singleton before leak detection runs
    ShutdownBootstrap;
  end;
{$ELSE}
  try
    // Check command line options
    TDUnitX.CheckCommandLine;

    // Create the test runner
    Runner := TDUnitX.CreateRunner;

    // Add loggers (False = not quiet mode, shows full output)
    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    // Generate NUnit XML output
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);

    Runner.FailsOnNoAsserts := False;

    // Run tests
    Results := Runner.Execute;

    // Wait for input if not running in CI
    if TDUnitX.Options.ExitBehavior <> TDUnitXExitBehavior.Continue then
    begin
      Write('Press Enter to exit...');
      ReadLn;
    end;

    // Return exit code based on test results
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;

  // Shutdown Bootstrap singleton before leak detection runs
  ShutdownBootstrap;
{$ENDIF}
end.
