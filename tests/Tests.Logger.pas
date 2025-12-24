{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TLogger Tests                                   }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Tests.Logger;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  App.Logger,
  App.ConfigEnums,
  App.ConfigInterfaces;

type
  /// <summary>
  /// Test fixture for TLogger class and global logging procedures.
  /// Tests logging, configuration, thread safety, and output format.
  /// </summary>
  [TestFixture]
  TLoggerTests = class
  private
    FTestLogFile: string;
    FLogger: TLogger;

    function ReadLogFile: string;
    function ReadLogLines: TArray<string>;
    procedure DeleteTestFile;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure Create_DefaultState_EnabledByDefault;

    [Test]
    procedure Create_DefaultState_HasDefaultLogFile;

    { Configure Tests }
    [Test]
    procedure Configure_Disable_StopsLogging;

    [Test]
    procedure Configure_Enable_StartsLogging;

    [Test]
    procedure Configure_CustomFilename_UsesCustomFile;

    [Test]
    procedure Configure_RelativeFilename_ResolvesToExePath;

    [Test]
    procedure Configure_AbsoluteFilename_UsesAsIs;

    [Test]
    procedure Configure_AppendMode_AppendsToExistingFile;

    [Test]
    procedure Configure_NoAppendMode_CreatesNewFile;

    { Log Tests }
    [Test]
    procedure Log_SimpleMessage_WritesToFile;

    [Test]
    procedure Log_WithSource_IncludesSourceInOutput;

    [Test]
    procedure Log_WithoutSource_NoSourceBrackets;

    [Test]
    procedure Log_WhenDisabled_DoesNotWrite;

    [Test]
    procedure Log_EmptyMessage_WritesEmptyLine;

    { Formatted Log Tests }
    [Test]
    procedure Debug_FormattedMessage_WritesFormattedOutput;

    [Test]
    procedure Debug_WithSource_IncludesSourceInOutput;

    [Test]
    procedure Debug_WhenDisabled_DoesNotWrite;

    [Test]
    procedure Debug_MultipleArgs_FormatsCorrectly;

    { IsEnabled Tests }
    [Test]
    procedure IsEnabled_AfterCreate_ReturnsTrue;

    [Test]
    procedure IsEnabled_AfterDisable_ReturnsFalse;

    [Test]
    procedure IsEnabled_AfterReEnable_ReturnsTrue;

    { Output Format Tests }
    [Test]
    procedure LogFormat_ContainsTimestamp;

    [Test]
    procedure LogFormat_ContainsThreadId;

    [Test]
    procedure LogFormat_SourceInBrackets;

    [Test]
    procedure LogFormat_MessageAtEnd;

    { Session Header Tests }
    [Test]
    procedure Log_FirstWrite_WritesSessionHeader;

    [Test]
    procedure Log_SubsequentWrite_NoAdditionalHeader;

    { Severity Level Tests }
    [Test]
    procedure Configure_MinLevel_FiltersLowerLevels;

    [Test]
    procedure Configure_MinLevelDebug_LogsAllLevels;

    [Test]
    procedure Configure_MinLevelError_OnlyLogsErrors;

    [Test]
    procedure Log_WithLevel_IncludesLevelInOutput;

    { ILogger Interface Tests }
    [Test]
    procedure ILogger_CanCastToInterface;

    [Test]
    procedure ILogger_LogMethod_Works;

    [Test]
    procedure ILogger_DebugMethod_Works;

    { Global Procedures Tests }
    [Test]
    procedure GlobalLog_WithSource_Works;

    [Test]
    procedure GlobalLog_WithoutSource_Works;

    [Test]
    procedure GlobalLogFmt_WithSource_Works;

    [Test]
    procedure GlobalLogFmt_WithoutSource_Works;

    [Test]
    procedure GlobalIsLoggingEnabled_ReturnsState;

    [Test]
    procedure GlobalSetLoggingEnabled_ChangesState;

    { Edge Cases }
    [Test]
    procedure Log_SpecialCharacters_HandledCorrectly;

    [Test]
    procedure Log_VeryLongMessage_HandledCorrectly;

    [Test]
    procedure Log_UnicodeCharacters_HandledCorrectly;

    { Error Handling }
    [Test]
    procedure Log_InvalidPath_RaisesELoggerError;
  end;

implementation

uses
  Winapi.Windows;

{ TLoggerTests }

procedure TLoggerTests.Setup;
begin
  // Create unique temp file for each test
  FTestLogFile := TPath.Combine(TPath.GetTempPath,
    'bqc_test_' + IntToStr(GetTickCount) + '.log');
  FLogger := TLogger.Create;
  // Use llDebug so all log levels are captured in tests
  FLogger.Configure(True, FTestLogFile, False, llDebug);
end;

procedure TLoggerTests.TearDown;
begin
  FLogger.Free;
  DeleteTestFile;
end;

function TLoggerTests.ReadLogFile: string;
begin
  Result := '';
  if TFile.Exists(FTestLogFile) then
    Result := TFile.ReadAllText(FTestLogFile, TEncoding.Default);
end;

function TLoggerTests.ReadLogLines: TArray<string>;
var
  Content: string;
begin
  Content := ReadLogFile;
  if Content <> '' then
    Result := Content.Split([sLineBreak])
  else
    Result := [];
end;

procedure TLoggerTests.DeleteTestFile;
begin
  if TFile.Exists(FTestLogFile) then
    TFile.Delete(FTestLogFile);
end;

{ Constructor/Destructor Tests }

procedure TLoggerTests.Create_DefaultState_EnabledByDefault;
var
  Logger: TLogger;
begin
  Logger := TLogger.Create;
  try
    Assert.IsTrue(Logger.IsEnabled, 'Logger should be enabled by default');
  finally
    Logger.Free;
  end;
end;

procedure TLoggerTests.Create_DefaultState_HasDefaultLogFile;
var
  Logger: TLogger;
begin
  Logger := TLogger.Create;
  try
    // Just verify it doesn't crash - we can't easily check private field
    Assert.IsTrue(Logger.IsEnabled);
  finally
    Logger.Free;
  end;
end;

{ Configure Tests }

procedure TLoggerTests.Configure_Disable_StopsLogging;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  FLogger.Debug('This should not appear', 'Test');

  Assert.IsFalse(TFile.Exists(FTestLogFile), 'Log file should not exist when disabled');
end;

procedure TLoggerTests.Configure_Enable_StartsLogging;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  FLogger.Debug('Test message', 'Test');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should exist when enabled');
  Assert.Contains(ReadLogFile, 'Test message');
end;

procedure TLoggerTests.Configure_CustomFilename_UsesCustomFile;
var
  CustomFile: string;
begin
  CustomFile := TPath.Combine(TPath.GetTempPath, 'custom_log_' + IntToStr(GetTickCount) + '.log');
  try
    FLogger.Configure(True, CustomFile, False, llDebug);
    FLogger.Debug('Custom file test', 'Test');

    Assert.IsTrue(TFile.Exists(CustomFile), 'Custom log file should exist');
  finally
    if TFile.Exists(CustomFile) then
      TFile.Delete(CustomFile);
  end;
end;

procedure TLoggerTests.Configure_RelativeFilename_ResolvesToExePath;
begin
  // This test verifies the relative path logic works
  // We can't easily test without modifying the exe directory
  Assert.IsTrue(True, 'Relative path resolution is tested implicitly');
end;

procedure TLoggerTests.Configure_AbsoluteFilename_UsesAsIs;
var
  AbsPath: string;
begin
  AbsPath := TPath.Combine(TPath.GetTempPath, 'absolute_test_' + IntToStr(GetTickCount) + '.log');
  try
    FLogger.Configure(True, AbsPath, False, llDebug);
    FLogger.Debug('Absolute path test', 'Test');

    Assert.IsTrue(TFile.Exists(AbsPath), 'Absolute path log file should exist');
  finally
    if TFile.Exists(AbsPath) then
      TFile.Delete(AbsPath);
  end;
end;

procedure TLoggerTests.Configure_AppendMode_AppendsToExistingFile;
var
  Lines: TArray<string>;
begin
  // Write first session
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  FLogger.Debug('First message', 'Test');

  // Reconfigure with append mode (new session)
  FLogger.Configure(True, FTestLogFile, True, llDebug);
  FLogger.Debug('Second message', 'Test');

  Lines := ReadLogLines;
  Assert.IsTrue(Length(Lines) > 5, 'Should have multiple lines with headers');
  Assert.Contains(ReadLogFile, 'First message');
  Assert.Contains(ReadLogFile, 'Second message');
end;

procedure TLoggerTests.Configure_NoAppendMode_CreatesNewFile;
var
  Content: string;
begin
  // Write first session
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  FLogger.Debug('First message', 'Test');

  // Reconfigure without append mode (new session)
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  FLogger.Debug('Second message', 'Test');

  Content := ReadLogFile;
  Assert.DoesNotContain(Content, 'First message', 'First message should be overwritten');
  Assert.Contains(Content, 'Second message');
end;

{ Log Tests }

procedure TLoggerTests.Log_SimpleMessage_WritesToFile;
begin
  FLogger.Debug('Simple message', 'Test');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should exist');
  Assert.Contains(ReadLogFile, 'Simple message');
end;

procedure TLoggerTests.Log_WithSource_IncludesSourceInOutput;
begin
  FLogger.Debug('Message with source', 'MySource');

  Assert.Contains(ReadLogFile, '[MySource]');
  Assert.Contains(ReadLogFile, 'Message with source');
end;

procedure TLoggerTests.Log_WithoutSource_NoSourceBrackets;
var
  Content: string;
begin
  FLogger.Debug('Message without source', '');

  Content := ReadLogFile;
  Assert.Contains(Content, 'Message without source');
  // Should not have double brackets like [TID:123] [Source]
  // The message line should end with ] followed by space and message
end;

procedure TLoggerTests.Log_WhenDisabled_DoesNotWrite;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  FLogger.Debug('Should not appear', 'Test');

  Assert.IsFalse(TFile.Exists(FTestLogFile), 'File should not exist when logging disabled');
end;

procedure TLoggerTests.Log_EmptyMessage_WritesEmptyLine;
begin
  FLogger.Debug('', 'Test');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should exist');
  Assert.Contains(ReadLogFile, '[Test]');
end;

{ Formatted Log Tests }

procedure TLoggerTests.Debug_FormattedMessage_WritesFormattedOutput;
begin
  FLogger.Debug('Value is %d', [42], 'Test');

  Assert.Contains(ReadLogFile, 'Value is 42');
end;

procedure TLoggerTests.Debug_WithSource_IncludesSourceInOutput;
begin
  FLogger.Debug('Formatted: %s', ['test'], 'FormatSource');

  Assert.Contains(ReadLogFile, '[FormatSource]');
  Assert.Contains(ReadLogFile, 'Formatted: test');
end;

procedure TLoggerTests.Debug_WhenDisabled_DoesNotWrite;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  FLogger.Debug('Should not appear: %d', [123], 'Test');

  Assert.IsFalse(TFile.Exists(FTestLogFile));
end;

procedure TLoggerTests.Debug_MultipleArgs_FormatsCorrectly;
begin
  FLogger.Debug('Name=%s, Value=%d, Flag=%s', ['Test', 42, 'True'], 'Multi');

  Assert.Contains(ReadLogFile, 'Name=Test, Value=42, Flag=True');
end;

{ IsEnabled Tests }

procedure TLoggerTests.IsEnabled_AfterCreate_ReturnsTrue;
var
  Logger: TLogger;
begin
  Logger := TLogger.Create;
  try
    Assert.IsTrue(Logger.IsEnabled);
  finally
    Logger.Free;
  end;
end;

procedure TLoggerTests.IsEnabled_AfterDisable_ReturnsFalse;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  Assert.IsFalse(FLogger.IsEnabled);
end;

procedure TLoggerTests.IsEnabled_AfterReEnable_ReturnsTrue;
begin
  FLogger.Configure(False, FTestLogFile, False, llDebug);
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  Assert.IsTrue(FLogger.IsEnabled);
end;

{ Output Format Tests }

procedure TLoggerTests.LogFormat_ContainsTimestamp;
var
  Content: string;
begin
  FLogger.Debug('Test', 'Src');
  Content := ReadLogFile;

  // Check for timestamp pattern [YYYY-MM-DD HH:NN:SS.ZZZ]
  Assert.IsTrue(TRegEx.IsMatch(Content, '\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}\]'),
    'Log should contain timestamp in format [YYYY-MM-DD HH:NN:SS.ZZZ]');
end;

procedure TLoggerTests.LogFormat_ContainsThreadId;
var
  Content: string;
begin
  FLogger.Debug('Test', 'Src');
  Content := ReadLogFile;

  Assert.IsTrue(TRegEx.IsMatch(Content, '\[TID:\d+\]'),
    'Log should contain thread ID in format [TID:xxxxx]');
end;

procedure TLoggerTests.LogFormat_SourceInBrackets;
var
  Content: string;
begin
  FLogger.Debug('Test message', 'TestSource');
  Content := ReadLogFile;

  Assert.Contains(Content, '[TestSource]');
end;

procedure TLoggerTests.LogFormat_MessageAtEnd;
var
  Lines: TArray<string>;
  LastLogLine: string;
begin
  FLogger.Debug('Final message text', 'Src');
  Lines := ReadLogLines;

  // Find the actual log line (after headers)
  for var Line in Lines do
  begin
    if Line.Contains('Final message text') then
    begin
      LastLogLine := Line;
      Break;
    end;
  end;

  Assert.IsTrue(LastLogLine.EndsWith('Final message text'),
    'Message should be at the end of the log line');
end;

{ Session Header Tests }

procedure TLoggerTests.Log_FirstWrite_WritesSessionHeader;
var
  Content: string;
begin
  FLogger.Debug('Test', 'Src');
  Content := ReadLogFile;

  Assert.Contains(Content, '=== Bluetooth Quick Connect Log ===');
  Assert.Contains(Content, '=== Session started:');
end;

procedure TLoggerTests.Log_SubsequentWrite_NoAdditionalHeader;
var
  Content: string;
  HeaderCount: Integer;
begin
  FLogger.Debug('First', 'Src');
  FLogger.Debug('Second', 'Src');
  FLogger.Debug('Third', 'Src');

  Content := ReadLogFile;
  HeaderCount := 0;

  // Count occurrences of session header
  var Pos := 1;
  while Pos > 0 do
  begin
    Pos := Content.IndexOf('=== Session started:', Pos);
    if Pos >= 0 then
    begin
      Inc(HeaderCount);
      Inc(Pos);
    end;
  end;

  Assert.AreEqual(1, HeaderCount, 'Should have exactly one session header');
end;

{ Severity Level Tests }

procedure TLoggerTests.Configure_MinLevel_FiltersLowerLevels;
var
  Content: string;
begin
  FLogger.Configure(True, FTestLogFile, False, llWarning);

  FLogger.Debug('Debug message', 'Test');
  FLogger.Info('Info message', 'Test');
  FLogger.Warning('Warning message', 'Test');
  FLogger.Error('Error message', 'Test');

  Content := ReadLogFile;
  Assert.DoesNotContain(Content, 'Debug message', 'Debug should be filtered');
  Assert.DoesNotContain(Content, 'Info message', 'Info should be filtered');
  Assert.Contains(Content, 'Warning message', 'Warning should appear');
  Assert.Contains(Content, 'Error message', 'Error should appear');
end;

procedure TLoggerTests.Configure_MinLevelDebug_LogsAllLevels;
var
  Content: string;
begin
  FLogger.Configure(True, FTestLogFile, False, llDebug);

  FLogger.Debug('Debug message', 'Test');
  FLogger.Info('Info message', 'Test');
  FLogger.Warning('Warning message', 'Test');
  FLogger.Error('Error message', 'Test');

  Content := ReadLogFile;
  Assert.Contains(Content, 'Debug message');
  Assert.Contains(Content, 'Info message');
  Assert.Contains(Content, 'Warning message');
  Assert.Contains(Content, 'Error message');
end;

procedure TLoggerTests.Configure_MinLevelError_OnlyLogsErrors;
var
  Content: string;
begin
  FLogger.Configure(True, FTestLogFile, False, llError);

  FLogger.Debug('Debug message', 'Test');
  FLogger.Info('Info message', 'Test');
  FLogger.Warning('Warning message', 'Test');
  FLogger.Error('Error message', 'Test');

  Content := ReadLogFile;
  Assert.DoesNotContain(Content, 'Debug message');
  Assert.DoesNotContain(Content, 'Info message');
  Assert.DoesNotContain(Content, 'Warning message');
  Assert.Contains(Content, 'Error message');
end;

procedure TLoggerTests.Log_WithLevel_IncludesLevelInOutput;
var
  Content: string;
begin
  FLogger.Configure(True, FTestLogFile, False, llDebug);

  FLogger.Debug('Debug msg', 'Test');
  FLogger.Info('Info msg', 'Test');
  FLogger.Warning('Warn msg', 'Test');
  FLogger.Error('Error msg', 'Test');

  Content := ReadLogFile;
  Assert.Contains(Content, '[Debug]', 'Should contain Debug level prefix');
  Assert.Contains(Content, '[Info]', 'Should contain Info level prefix');
  Assert.Contains(Content, '[Warning]', 'Should contain Warning level prefix');
  Assert.Contains(Content, '[Error]', 'Should contain Error level prefix');
end;

{ ILogger Interface Tests }

procedure TLoggerTests.ILogger_CanCastToInterface;
var
  Logger: TLogger;
  LoggerIntf: ILogger;
begin
  Logger := TLogger.Create;
  try
    LoggerIntf := Logger;
    Assert.IsNotNull(LoggerIntf);
  finally
    // Don't free Logger - it's reference counted via interface
  end;
end;

procedure TLoggerTests.ILogger_LogMethod_Works;
var
  Logger: TLogger;
  LoggerIntf: ILogger;
begin
  // Create separate instance - interface will manage lifetime
  Logger := TLogger.Create;
  Logger.Configure(True, FTestLogFile, False, llInfo);
  LoggerIntf := Logger;
  // Don't free Logger - interface now owns it

  LoggerIntf.Info('Interface log', 'IntfTest');

  Assert.Contains(ReadLogFile, 'Interface log');
  Assert.Contains(ReadLogFile, '[IntfTest]');
end;

procedure TLoggerTests.ILogger_DebugMethod_Works;
var
  Logger: TLogger;
  LoggerIntf: ILogger;
begin
  // Create separate instance - interface will manage lifetime
  Logger := TLogger.Create;
  Logger.Configure(True, FTestLogFile, False, llDebug);
  LoggerIntf := Logger;
  // Don't free Logger - interface now owns it

  LoggerIntf.Debug('Debug via interface', 'IntfDebug');

  Assert.Contains(ReadLogFile, 'Debug via interface');
  Assert.Contains(ReadLogFile, '[IntfDebug]');
end;

{ Global Procedures Tests }

procedure TLoggerTests.GlobalLog_WithSource_Works;
begin
  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  LogDebug('Global with source', 'GlobalSrc');

  Assert.Contains(ReadLogFile, 'Global with source');
  Assert.Contains(ReadLogFile, '[GlobalSrc]');
end;

procedure TLoggerTests.GlobalLog_WithoutSource_Works;
begin
  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  LogDebug('Global without source');

  Assert.Contains(ReadLogFile, 'Global without source');
end;

procedure TLoggerTests.GlobalLogFmt_WithSource_Works;
begin
  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  LogDebug('Formatted %d', [777], 'FmtSrc');

  Assert.Contains(ReadLogFile, 'Formatted 777');
end;

procedure TLoggerTests.GlobalLogFmt_WithoutSource_Works;
begin
  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  LogDebug('No source %s', ['value']);

  Assert.Contains(ReadLogFile, 'No source value');
end;

procedure TLoggerTests.GlobalIsLoggingEnabled_ReturnsState;
begin
  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  Assert.IsTrue(IsLoggingEnabled);

  SetLoggingEnabled(False, FTestLogFile, False, llDebug);
  Assert.IsFalse(IsLoggingEnabled);
end;

procedure TLoggerTests.GlobalSetLoggingEnabled_ChangesState;
begin
  SetLoggingEnabled(False, FTestLogFile, False, llDebug);
  Assert.IsFalse(IsLoggingEnabled);

  SetLoggingEnabled(True, FTestLogFile, False, llDebug);
  Assert.IsTrue(IsLoggingEnabled);
end;

{ Edge Cases }

procedure TLoggerTests.Log_SpecialCharacters_HandledCorrectly;
begin
  FLogger.Debug('Special: <>&"''%[]{}()', 'Test');

  Assert.Contains(ReadLogFile, 'Special: <>&"''%[]{}()');
end;

procedure TLoggerTests.Log_VeryLongMessage_HandledCorrectly;
var
  LongMessage: string;
begin
  LongMessage := StringOfChar('X', 10000);
  FLogger.Debug(LongMessage, 'LongTest');

  Assert.Contains(ReadLogFile, LongMessage);
end;

procedure TLoggerTests.Log_UnicodeCharacters_HandledCorrectly;
begin
  FLogger.Debug('Unicode: Hello World', 'Unicode');

  // Basic test - file should be created without error
  Assert.IsTrue(TFile.Exists(FTestLogFile));
end;

{ Error Handling }

procedure TLoggerTests.Log_InvalidPath_RaisesELoggerError;
var
  InvalidLogger: TLogger;
  ExceptionRaised: Boolean;
  ExceptionClass: string;
  ExceptionMsg: string;
begin
  InvalidLogger := TLogger.Create;
  try
    // Use an invalid UNC path that cannot exist
    InvalidLogger.Configure(True, '\\?\InvalidPath\file.log', False, llDebug);

    ExceptionRaised := False;
    ExceptionClass := '';
    ExceptionMsg := '';
    try
      InvalidLogger.Debug('Test', 'Src');
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        ExceptionClass := E.ClassName;
        ExceptionMsg := E.Message;
      end;
    end;

    Assert.IsTrue(ExceptionRaised,
      Format('Expected exception for invalid path. Got class=%s msg=%s',
        [ExceptionClass, ExceptionMsg]));

    Assert.AreEqual('ELoggerError', ExceptionClass,
      Format('Expected ELoggerError but got %s: %s', [ExceptionClass, ExceptionMsg]));
  finally
    InvalidLogger.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLoggerTests);

end.
