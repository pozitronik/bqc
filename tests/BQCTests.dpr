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
  Tests.Mocks in 'Tests.Mocks.pas',
  Bluetooth.Types in '..\src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in '..\src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in '..\src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.WinAPI in '..\src\Infrastructure\Bluetooth.WinAPI.pas',
  App.ConfigInterfaces in '..\src\Infrastructure\App.ConfigInterfaces.pas',
  App.Config in '..\src\Infrastructure\App.Config.pas',
  App.Bootstrap in '..\src\Infrastructure\App.Bootstrap.pas',
  App.Logger in '..\src\Infrastructure\App.Logger.pas',
  App.Autostart in '..\src\Infrastructure\App.Autostart.pas';


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
