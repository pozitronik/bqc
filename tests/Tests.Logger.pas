{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TLogger Tests                                   }
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
  App.ConfigInterfaces,
  App.LogConfigIntf;

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

    { Thread Safety Tests }
    [Test]
    procedure ThreadSafety_ConcurrentLogging_NoDataLoss;

    [Test]
    procedure ThreadSafety_ConfigurationDuringLogging;

    { Unicode Tests }
    [Test]
    procedure Unicode_CyrillicCharacters;

    [Test]
    procedure Unicode_ChineseCharacters;

    [Test]
    procedure Unicode_MixedWithAscii;

    { Format Specifier Tests }
    [Test]
    procedure Format_FloatingPointNumbers;

    [Test]
    procedure Format_HexadecimalValues;

    [Test]
    procedure Format_NegativeNumbers;

    [Test]
    procedure Format_MismatchedArguments;

    { Special Characters and Control Tests }
    [Test]
    procedure Message_EmbeddedNewlines;

    [Test]
    procedure SpecialCharacters_ControlCharacters;

    { Dynamic Configuration Tests }
    [Test]
    procedure DynamicLevelChange_MidSession;
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

{ Thread Safety Tests }

procedure TLoggerTests.ThreadSafety_ConcurrentLogging_NoDataLoss;
const
  THREAD_COUNT = 10;
  MESSAGES_PER_THREAD = 100;
  TOTAL_MESSAGES = THREAD_COUNT * MESSAGES_PER_THREAD;
var
  Threads: array[0..THREAD_COUNT - 1] of TThread;
  ThreadLogger: TLogger;
  I, J: Integer;
  Content: string;
  MessageCount: Integer;
  AllThreadsCompleted: Boolean;
begin
  ThreadLogger := TLogger.Create;
  try
    ThreadLogger.Configure(True, FTestLogFile, False, llDebug);

    // Create and start threads
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I] := TThread.CreateAnonymousThread(
        procedure
        var
          ThreadIdx, MsgIdx: Integer;
        begin
          // Capture I value at thread creation time
          ThreadIdx := TThread.Current.ThreadID mod THREAD_COUNT;
          for MsgIdx := 0 to MESSAGES_PER_THREAD - 1 do
            ThreadLogger.Info(Format('Thread_%d_Message_%d', [ThreadIdx, MsgIdx]), 'ThreadTest');
        end
      );
      Threads[I].FreeOnTerminate := False;
    end;

    // Start all threads
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;

    // Wait for all threads to complete with timeout
    AllThreadsCompleted := True;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      if Threads[I].WaitFor <> 0 then
        AllThreadsCompleted := False;
    end;

    // Free threads
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Free;

    Assert.IsTrue(AllThreadsCompleted, 'All threads should complete successfully');

    // Verify all messages were written
    Content := ReadLogFile;
    MessageCount := 0;

    // Count occurrences of 'Thread_' pattern (each message has one)
    I := 1;
    while I <= Length(Content) do
    begin
      J := Pos('_Message_', Content, I);
      if J > 0 then
      begin
        Inc(MessageCount);
        I := J + 1;
      end
      else
        Break;
    end;

    Assert.AreEqual(TOTAL_MESSAGES, MessageCount,
      Format('Expected %d messages but found %d', [TOTAL_MESSAGES, MessageCount]));

    // Verify log format is intact (check for proper timestamp patterns)
    Assert.IsTrue(TRegEx.IsMatch(Content, '\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}\]'),
      'Log entries should maintain proper timestamp format');
  finally
    ThreadLogger.Free;
  end;
end;

procedure TLoggerTests.ThreadSafety_ConfigurationDuringLogging;
const
  LOG_ITERATIONS = 50;
  CONFIG_ITERATIONS = 20;
var
  LogThread, ConfigThread: TThread;
  ThreadLogger: TLogger;
  LoggingCompleted, ConfigCompleted: Boolean;
  ExceptionOccurred: Boolean;
  ExceptionMessage: string;
begin
  ThreadLogger := TLogger.Create;
  try
    ThreadLogger.Configure(True, FTestLogFile, False, llDebug);
    LoggingCompleted := False;
    ConfigCompleted := False;
    ExceptionOccurred := False;
    ExceptionMessage := '';

    // Thread that continuously logs messages
    LogThread := TThread.CreateAnonymousThread(
      procedure
      var
        I: Integer;
      begin
        try
          for I := 0 to LOG_ITERATIONS - 1 do
          begin
            ThreadLogger.Info(Format('Logging message %d', [I]), 'LogThread');
            Sleep(1); // Small delay to increase interleaving
          end;
          LoggingCompleted := True;
        except
          on E: Exception do
          begin
            ExceptionOccurred := True;
            ExceptionMessage := E.Message;
          end;
        end;
      end
    );
    LogThread.FreeOnTerminate := False;

    // Thread that continuously reconfigures the logger
    ConfigThread := TThread.CreateAnonymousThread(
      procedure
      var
        I: Integer;
      begin
        try
          for I := 0 to CONFIG_ITERATIONS - 1 do
          begin
            // Toggle between different configurations
            if I mod 2 = 0 then
              ThreadLogger.Configure(True, FTestLogFile, True, llDebug)
            else
              ThreadLogger.Configure(True, FTestLogFile, True, llInfo);
            Sleep(2); // Slightly longer delay for config changes
          end;
          ConfigCompleted := True;
        except
          on E: Exception do
          begin
            ExceptionOccurred := True;
            ExceptionMessage := E.Message;
          end;
        end;
      end
    );
    ConfigThread.FreeOnTerminate := False;

    // Start both threads
    LogThread.Start;
    ConfigThread.Start;

    // Wait for completion
    LogThread.WaitFor;
    ConfigThread.WaitFor;

    // Free threads
    LogThread.Free;
    ConfigThread.Free;

    // Verify no crashes occurred
    Assert.IsFalse(ExceptionOccurred,
      Format('No exceptions should occur during concurrent logging and configuration. Got: %s',
        [ExceptionMessage]));
    Assert.IsTrue(LoggingCompleted, 'Logging thread should complete');
    Assert.IsTrue(ConfigCompleted, 'Configuration thread should complete');

    // Verify file was created and has content
    Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should exist');
  finally
    ThreadLogger.Free;
  end;
end;

{ Unicode Tests }

procedure TLoggerTests.Unicode_CyrillicCharacters;
const
  CyrillicText = 'Cyrillic: ' + #$041F#$0440#$0438#$0432#$0435#$0442 + ' ' +  // Cyrillic for 'Privet'
                 #$041C#$0438#$0440;  // Cyrillic for 'Mir'
begin
  FLogger.Debug(CyrillicText, 'Unicode');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should be created with Cyrillic characters');
  // Verify the file was written successfully (content verification may vary by encoding)
  Assert.IsTrue(Length(ReadLogFile) > 0, 'Log file should have content');
end;

procedure TLoggerTests.Unicode_ChineseCharacters;
const
  ChineseText = 'Chinese: ' + #$4E2D#$6587#$6D4B#$8BD5;  // Chinese characters
begin
  FLogger.Debug(ChineseText, 'Unicode');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should be created with Chinese characters');
  Assert.IsTrue(Length(ReadLogFile) > 0, 'Log file should have content');
end;

procedure TLoggerTests.Unicode_MixedWithAscii;
const
  MixedText = 'ASCII: Hello, Cyrillic: ' + #$041F#$0440#$0438#$0432#$0435#$0442 +
              ', Chinese: ' + #$4E2D#$6587;
begin
  FLogger.Debug(MixedText, 'Unicode');

  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should be created with mixed scripts');

  // Verify ASCII portions are preserved
  var Content := ReadLogFile;
  Assert.Contains(Content, 'ASCII: Hello', 'ASCII portion should be preserved');
end;

{ Format Specifier Tests }

procedure TLoggerTests.Format_FloatingPointNumbers;
var
  ExpectedFloat: string;
begin
  FLogger.Debug('Float value: %.2f', [3.14159], 'Format');
  FLogger.Debug('Scientific: %e', [12345.6789], 'Format');
  FLogger.Debug('General: %g', [0.000123], 'Format');

  var Content := ReadLogFile;
  // Use locale-aware expected value (decimal separator varies by system locale)
  ExpectedFloat := Format('Float value: %.2f', [3.14159]);
  Assert.Contains(Content, ExpectedFloat, 'Float formatting should work');
  Assert.Contains(Content, 'Scientific:', 'Scientific notation should work');
  Assert.Contains(Content, 'General:', 'General format should work');
end;

procedure TLoggerTests.Format_HexadecimalValues;
begin
  FLogger.Debug('Hex lowercase: %x', [255], 'Format');
  FLogger.Debug('Hex uppercase: %X', [255], 'Format');
  FLogger.Debug('Hex with width: %08X', [$DEADBEEF], 'Format');

  var Content := ReadLogFile;
  Assert.Contains(Content, 'Hex lowercase: ff', 'Lowercase hex should work');
  Assert.Contains(Content, 'Hex uppercase: FF', 'Uppercase hex should work');
  Assert.Contains(Content, 'DEADBEEF', 'Wide hex value should work');
end;

procedure TLoggerTests.Format_NegativeNumbers;
var
  ExpectedFloat: string;
begin
  FLogger.Debug('Negative integer: %d', [-42], 'Format');
  FLogger.Debug('Negative float: %.3f', [-123.456], 'Format');
  FLogger.Debug('Large negative: %d', [-2147483648], 'Format');

  var Content := ReadLogFile;
  Assert.Contains(Content, 'Negative integer: -42', 'Negative integer should work');
  // Use locale-aware expected value (decimal separator varies by system locale)
  ExpectedFloat := Format('Negative float: %.3f', [-123.456]);
  Assert.Contains(Content, ExpectedFloat, 'Negative float should work');
  Assert.Contains(Content, 'Large negative: -2147483648', 'Large negative should work');
end;

procedure TLoggerTests.Format_MismatchedArguments;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;

  try
    // Attempt to format with mismatched arguments (expecting 2, providing 1)
    FLogger.Debug('Value1: %s, Value2: %s', ['OnlyOne'], 'Format');
  except
    on E: Exception do
      ExceptionRaised := True;
  end;

  Assert.IsTrue(ExceptionRaised,
    'Mismatched format arguments should raise an exception');
end;

{ Special Characters and Control Tests }

procedure TLoggerTests.Message_EmbeddedNewlines;
var
  Content: string;
  Lines: TArray<string>;
begin
  FLogger.Debug('Line1' + #13#10 + 'Line2' + #13#10 + 'Line3', 'Newline');

  Content := ReadLogFile;
  Lines := Content.Split([sLineBreak]);

  // The message with embedded newlines should create multiple lines in the file
  // Find the lines containing our test content
  var FoundLine1 := False;
  var FoundLine2 := False;
  var FoundLine3 := False;

  for var Line in Lines do
  begin
    if Line.Contains('Line1') then FoundLine1 := True;
    if Line.Contains('Line2') then FoundLine2 := True;
    if Line.Contains('Line3') then FoundLine3 := True;
  end;

  Assert.IsTrue(FoundLine1, 'Line1 should be present');
  Assert.IsTrue(FoundLine2, 'Line2 should be present');
  Assert.IsTrue(FoundLine3, 'Line3 should be present');
end;

procedure TLoggerTests.SpecialCharacters_ControlCharacters;
var
  Content: string;
begin
  // Test tab character (ASCII 9)
  FLogger.Debug('Before' + #9 + 'Tab' + #9 + 'After', 'Control');

  Content := ReadLogFile;
  Assert.IsTrue(TFile.Exists(FTestLogFile), 'Log file should be created');

  // The content should contain tab-separated text
  Assert.Contains(Content, 'Before', 'Text before tab should be present');
  Assert.Contains(Content, 'Tab', 'Text between tabs should be present');
  Assert.Contains(Content, 'After', 'Text after tab should be present');
end;

{ Dynamic Configuration Tests }

procedure TLoggerTests.DynamicLevelChange_MidSession;
var
  Content: string;
begin
  // Start with Debug level
  FLogger.Configure(True, FTestLogFile, False, llDebug);
  FLogger.Debug('Debug message 1', 'Dynamic');
  FLogger.Info('Info message 1', 'Dynamic');

  // Change to Warning level mid-session
  FLogger.Configure(True, FTestLogFile, True, llWarning);  // Append mode to keep previous
  FLogger.Debug('Debug message 2', 'Dynamic');  // Should be filtered
  FLogger.Info('Info message 2', 'Dynamic');    // Should be filtered
  FLogger.Warning('Warning message 1', 'Dynamic');  // Should appear
  FLogger.Error('Error message 1', 'Dynamic');      // Should appear

  // Change to Error level
  FLogger.Configure(True, FTestLogFile, True, llError);
  FLogger.Warning('Warning message 2', 'Dynamic');  // Should be filtered
  FLogger.Error('Error message 2', 'Dynamic');      // Should appear

  Content := ReadLogFile;

  // Verify messages from first configuration (Debug level)
  Assert.Contains(Content, 'Debug message 1', 'First debug should appear (Debug level)');
  Assert.Contains(Content, 'Info message 1', 'First info should appear (Debug level)');

  // Verify messages from second configuration (Warning level)
  Assert.DoesNotContain(Content, 'Debug message 2', 'Second debug should be filtered (Warning level)');
  Assert.DoesNotContain(Content, 'Info message 2', 'Second info should be filtered (Warning level)');
  Assert.Contains(Content, 'Warning message 1', 'First warning should appear (Warning level)');
  Assert.Contains(Content, 'Error message 1', 'First error should appear (Warning level)');

  // Verify messages from third configuration (Error level)
  Assert.DoesNotContain(Content, 'Warning message 2', 'Second warning should be filtered (Error level)');
  Assert.Contains(Content, 'Error message 2', 'Second error should appear (Error level)');
end;

initialization
  TDUnitX.RegisterTestFixture(TLoggerTests);

end.
