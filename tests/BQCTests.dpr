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
  Tests.Mocks in 'Tests.Mocks.pas',
  Bluetooth.Types in '..\src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in '..\src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in '..\src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.Service in '..\src\Application\Bluetooth.Service.pas',
  Bluetooth.WinAPI in '..\src\Infrastructure\Bluetooth.WinAPI.pas',
  Bluetooth.DeviceMonitors in '..\src\Infrastructure\Bluetooth.DeviceMonitors.pas',
  Bluetooth.DeviceRepository in '..\src\Infrastructure\Bluetooth.DeviceRepository.pas',
  Bluetooth.ConnectionExecutor in '..\src\Infrastructure\Bluetooth.ConnectionExecutor.pas',
  Bluetooth.AdapterQuery in '..\src\Infrastructure\Bluetooth.AdapterQuery.pas',
  Bluetooth.DeviceWatcher in '..\src\Infrastructure\Bluetooth.DeviceWatcher.pas',
  App.ConfigInterfaces in '..\src\Infrastructure\App.ConfigInterfaces.pas',
  App.Config in '..\src\Infrastructure\App.Config.pas',
  App.SettingsRepository in '..\src\Infrastructure\App.SettingsRepository.pas',
  App.DeviceConfigRepository in '..\src\Infrastructure\App.DeviceConfigRepository.pas',
  App.Bootstrap in '..\src\Infrastructure\App.Bootstrap.pas',
  App.Logger in '..\src\Infrastructure\App.Logger.pas',
  App.SettingsPresenter in '..\src\Application\App.SettingsPresenter.pas',
  App.Autostart in '..\src\Infrastructure\App.Autostart.pas',
  UI.DeviceList in '..\src\Presentation\UI.DeviceList.pas',
  UI.ListGeometry in '..\src\Presentation\UI.ListGeometry.pas',
  UI.DeviceFormatter in '..\src\Presentation\UI.DeviceFormatter.pas',
  UI.DeviceSorter in '..\src\Presentation\UI.DeviceSorter.pas',
  UI.DeviceDisplayItemBuilder in '..\src\Presentation\UI.DeviceDisplayItemBuilder.pas';


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
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options
    TDUnitX.CheckCommandLine;

    // Create the test runner
    Runner := TDUnitX.CreateRunner;

    // Add loggers
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
{$ENDIF}
end.
