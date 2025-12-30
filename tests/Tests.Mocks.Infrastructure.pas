{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations - Infrastructure           }
{                                                       }
{*******************************************************}

unit Tests.Mocks.Infrastructure;

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  App.ConfigEnums,
  App.Autostart,
  App.AsyncExecutor,
  App.LogConfigIntf,
  App.SystemClock,
  UI.Theme;

type
  /// <summary>
  /// Mock implementation of IAutostartManager for testing.
  /// </summary>
  TMockAutostartManager = class(TInterfacedObject, IAutostartManager)
  private
    FEnabled: Boolean;
    FRegisteredPath: string;
    FApplyCallCount: Integer;
    FIsEnabledCallCount: Integer;
    FGetRegisteredPathCallCount: Integer;
    FLastApplyValue: Boolean;
  public
    constructor Create;

    // IAutostartManager
    procedure Apply(AEnabled: Boolean);
    function IsEnabled: Boolean;
    function GetRegisteredPath: string;

    // Test configuration
    property Enabled: Boolean read FEnabled write FEnabled;
    property RegisteredPath: string read FRegisteredPath write FRegisteredPath;
    property ApplyCallCount: Integer read FApplyCallCount;
    property IsEnabledCallCount: Integer read FIsEnabledCallCount;
    property GetRegisteredPathCallCount: Integer read FGetRegisteredPathCallCount;
    property LastApplyValue: Boolean read FLastApplyValue;
  end;

  /// <summary>
  /// Mock implementation of IThemeManager for testing.
  /// </summary>
  TMockThemeManager = class(TInterfacedObject, IThemeManager)
  private
    FCurrentStyleName: string;
    FAvailableStyles: TArray<string>;
    FLoadStylesCallCount: Integer;
    FLastLoadedDirectory: string;
    FSetStyleCallCount: Integer;
    FLastSetStyle: string;
  public
    constructor Create;

    // IThemeManager
    procedure LoadStylesFromDirectory(const ADirectory: string);
    function GetAvailableStyles: TArray<string>;
    procedure SetStyle(const AStyleName: string);
    function GetCurrentStyleName: string;
    function GetStyleDisplayName(const AStyleName: string): string;
    function GetStyleNameFromDisplay(const ADisplayName: string): string;

    // Test configuration
    property CurrentStyleName: string read FCurrentStyleName write FCurrentStyleName;
    property AvailableStyles: TArray<string> read FAvailableStyles write FAvailableStyles;
    property LoadStylesCallCount: Integer read FLoadStylesCallCount;
    property LastLoadedDirectory: string read FLastLoadedDirectory;
    property SetStyleCallCount: Integer read FSetStyleCallCount;
    property LastSetStyle: string read FLastSetStyle;
  end;

  /// <summary>
  /// Mock implementation of ILogger for testing.
  /// Tracks all logged messages for verification.
  /// </summary>
  TMockLogger = class(TInterfacedObject, ILogger)
  private
    FEnabled: Boolean;
    FMinLevel: TLogLevel;
    FDebugMessages: TArray<string>;
    FInfoMessages: TArray<string>;
    FWarningMessages: TArray<string>;
    FErrorMessages: TArray<string>;
    FDebugCallCount: Integer;
    FInfoCallCount: Integer;
    FWarningCallCount: Integer;
    FErrorCallCount: Integer;
    FConfigureCallCount: Integer;
  public
    constructor Create;

    // ILogger
    procedure Debug(const AMessage: string; const ASource: string = '');
    procedure Info(const AMessage: string; const ASource: string = '');
    procedure Warning(const AMessage: string; const ASource: string = '');
    procedure Error(const AMessage: string; const ASource: string = '');
    function IsEnabled: Boolean;
    function GetMinLevel: TLogLevel;
    procedure Configure(AEnabled: Boolean; AMinLevel: TLogLevel);

    // Test helpers
    procedure Clear;
    function ContainsMessage(const AText: string): Boolean;
    function GetLastMessage: string;
    function GetAllMessages: TArray<string>;

    // Test configuration & verification
    property Enabled: Boolean read FEnabled write FEnabled;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property DebugMessages: TArray<string> read FDebugMessages;
    property InfoMessages: TArray<string> read FInfoMessages;
    property WarningMessages: TArray<string> read FWarningMessages;
    property ErrorMessages: TArray<string> read FErrorMessages;
    property DebugCallCount: Integer read FDebugCallCount;
    property InfoCallCount: Integer read FInfoCallCount;
    property WarningCallCount: Integer read FWarningCallCount;
    property ErrorCallCount: Integer read FErrorCallCount;
    property ConfigureCallCount: Integer read FConfigureCallCount;
  end;

  /// <summary>
  /// Mock implementation of IAsyncExecutor for testing.
  /// By default defers execution (non-synchronous) to match production behavior.
  /// Set Synchronous := True to execute procedures immediately for deterministic tests.
  /// </summary>
  TMockAsyncExecutor = class(TInterfacedObject, IAsyncExecutor)
  private
    FSynchronous: Boolean;
    FRunAsyncCallCount: Integer;
    FRunDelayedCallCount: Integer;
    FPendingProcs: TList<TProc>;
    FPendingDelays: TList<Integer>;
    FLastDelayMs: Integer;
    function GetPendingCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // IAsyncExecutor
    procedure RunAsync(AProc: TProc);
    procedure RunDelayed(AProc: TProc; ADelayMs: Integer);

    /// <summary>
    /// Executes all pending procedures in order.
    /// Only has effect when Synchronous = False.
    /// </summary>
    procedure ExecutePending;

    /// <summary>
    /// Clears all pending procedures without executing them.
    /// </summary>
    procedure ClearPending;

    /// <summary>
    /// When True (default), procedures execute immediately when RunAsync/RunDelayed called.
    /// When False, procedures are queued and must be executed via ExecutePending.
    /// </summary>
    property Synchronous: Boolean read FSynchronous write FSynchronous;

    /// <summary>Number of RunAsync calls made.</summary>
    property RunAsyncCallCount: Integer read FRunAsyncCallCount;

    /// <summary>Number of RunDelayed calls made.</summary>
    property RunDelayedCallCount: Integer read FRunDelayedCallCount;

    /// <summary>Number of procedures waiting to execute (when Synchronous=False).</summary>
    property PendingCount: Integer read GetPendingCount;

    /// <summary>Delay value from most recent RunDelayed call.</summary>
    property LastDelayMs: Integer read FLastDelayMs;
  end;

  /// <summary>
  /// Mock implementation of ISystemClock for testing.
  /// Provides controllable time for deterministic tests.
  /// </summary>
  TMockSystemClock = class(TInterfacedObject, ISystemClock)
  private
    FCurrentTime: TDateTime;
    FNowCallCount: Integer;
  public
    constructor Create;

    // ISystemClock
    function Now: TDateTime;

    /// <summary>
    /// Advances time by the specified number of milliseconds.
    /// </summary>
    procedure AdvanceMs(AMilliseconds: Integer);

    /// <summary>
    /// Advances time by the specified number of seconds.
    /// </summary>
    procedure AdvanceSeconds(ASeconds: Integer);

    /// <summary>
    /// The current time value returned by Now.
    /// Set this to control what time the mock reports.
    /// </summary>
    property CurrentTime: TDateTime read FCurrentTime write FCurrentTime;

    /// <summary>Number of times Now was called.</summary>
    property NowCallCount: Integer read FNowCallCount;
  end;

implementation

{ TMockAutostartManager }

constructor TMockAutostartManager.Create;
begin
  inherited Create;
  FEnabled := False;
  FRegisteredPath := '';
  FApplyCallCount := 0;
  FIsEnabledCallCount := 0;
  FGetRegisteredPathCallCount := 0;
  FLastApplyValue := False;
end;

procedure TMockAutostartManager.Apply(AEnabled: Boolean);
begin
  Inc(FApplyCallCount);
  FLastApplyValue := AEnabled;
  FEnabled := AEnabled;
  if AEnabled then
    FRegisteredPath := 'C:\MockPath\bqc.exe'
  else
    FRegisteredPath := '';
end;

function TMockAutostartManager.IsEnabled: Boolean;
begin
  Inc(FIsEnabledCallCount);
  Result := FEnabled;
end;

function TMockAutostartManager.GetRegisteredPath: string;
begin
  Inc(FGetRegisteredPathCallCount);
  Result := FRegisteredPath;
end;

{ TMockThemeManager }

constructor TMockThemeManager.Create;
begin
  inherited Create;
  FCurrentStyleName := 'Windows';
  FAvailableStyles := ['Windows', 'Dark', 'Light'];
  FLoadStylesCallCount := 0;
  FLastLoadedDirectory := '';
  FSetStyleCallCount := 0;
  FLastSetStyle := '';
end;

procedure TMockThemeManager.LoadStylesFromDirectory(const ADirectory: string);
begin
  Inc(FLoadStylesCallCount);
  FLastLoadedDirectory := ADirectory;
end;

function TMockThemeManager.GetAvailableStyles: TArray<string>;
begin
  Result := FAvailableStyles;
end;

procedure TMockThemeManager.SetStyle(const AStyleName: string);
begin
  Inc(FSetStyleCallCount);
  FLastSetStyle := AStyleName;
  FCurrentStyleName := AStyleName;
end;

function TMockThemeManager.GetCurrentStyleName: string;
begin
  Result := FCurrentStyleName;
end;

function TMockThemeManager.GetStyleDisplayName(const AStyleName: string): string;
begin
  // Mock simply returns the style name as-is
  Result := AStyleName;
end;

function TMockThemeManager.GetStyleNameFromDisplay(const ADisplayName: string): string;
begin
  // Mock simply returns the display name as-is
  Result := ADisplayName;
end;

{ TMockLogger }

constructor TMockLogger.Create;
begin
  inherited Create;
  FEnabled := True;
  FMinLevel := llDebug;
  FDebugCallCount := 0;
  FInfoCallCount := 0;
  FWarningCallCount := 0;
  FErrorCallCount := 0;
  FConfigureCallCount := 0;
  SetLength(FDebugMessages, 0);
  SetLength(FInfoMessages, 0);
  SetLength(FWarningMessages, 0);
  SetLength(FErrorMessages, 0);
end;

procedure TMockLogger.Debug(const AMessage: string; const ASource: string);
var
  FullMessage: string;
begin
  Inc(FDebugCallCount);
  if ASource <> '' then
    FullMessage := Format('[%s] %s', [ASource, AMessage])
  else
    FullMessage := AMessage;
  SetLength(FDebugMessages, Length(FDebugMessages) + 1);
  FDebugMessages[High(FDebugMessages)] := FullMessage;
end;

procedure TMockLogger.Info(const AMessage: string; const ASource: string);
var
  FullMessage: string;
begin
  Inc(FInfoCallCount);
  if ASource <> '' then
    FullMessage := Format('[%s] %s', [ASource, AMessage])
  else
    FullMessage := AMessage;
  SetLength(FInfoMessages, Length(FInfoMessages) + 1);
  FInfoMessages[High(FInfoMessages)] := FullMessage;
end;

procedure TMockLogger.Warning(const AMessage: string; const ASource: string);
var
  FullMessage: string;
begin
  Inc(FWarningCallCount);
  if ASource <> '' then
    FullMessage := Format('[%s] %s', [ASource, AMessage])
  else
    FullMessage := AMessage;
  SetLength(FWarningMessages, Length(FWarningMessages) + 1);
  FWarningMessages[High(FWarningMessages)] := FullMessage;
end;

procedure TMockLogger.Error(const AMessage: string; const ASource: string);
var
  FullMessage: string;
begin
  Inc(FErrorCallCount);
  if ASource <> '' then
    FullMessage := Format('[%s] %s', [ASource, AMessage])
  else
    FullMessage := AMessage;
  SetLength(FErrorMessages, Length(FErrorMessages) + 1);
  FErrorMessages[High(FErrorMessages)] := FullMessage;
end;

function TMockLogger.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMockLogger.GetMinLevel: TLogLevel;
begin
  Result := FMinLevel;
end;

procedure TMockLogger.Configure(AEnabled: Boolean; AMinLevel: TLogLevel);
begin
  Inc(FConfigureCallCount);
  FEnabled := AEnabled;
  FMinLevel := AMinLevel;
end;

procedure TMockLogger.Clear;
begin
  SetLength(FDebugMessages, 0);
  SetLength(FInfoMessages, 0);
  SetLength(FWarningMessages, 0);
  SetLength(FErrorMessages, 0);
  FDebugCallCount := 0;
  FInfoCallCount := 0;
  FWarningCallCount := 0;
  FErrorCallCount := 0;
end;

function TMockLogger.ContainsMessage(const AText: string): Boolean;
var
  Msg: string;
begin
  Result := False;

  for Msg in FDebugMessages do
    if Pos(AText, Msg) > 0 then
      Exit(True);

  for Msg in FInfoMessages do
    if Pos(AText, Msg) > 0 then
      Exit(True);

  for Msg in FWarningMessages do
    if Pos(AText, Msg) > 0 then
      Exit(True);

  for Msg in FErrorMessages do
    if Pos(AText, Msg) > 0 then
      Exit(True);
end;

function TMockLogger.GetLastMessage: string;
var
  AllMsgs: TArray<string>;
begin
  AllMsgs := GetAllMessages;
  if Length(AllMsgs) > 0 then
    Result := AllMsgs[High(AllMsgs)]
  else
    Result := '';
end;

function TMockLogger.GetAllMessages: TArray<string>;
var
  TotalLen, Idx: Integer;
  Msg: string;
begin
  TotalLen := Length(FDebugMessages) + Length(FInfoMessages) +
              Length(FWarningMessages) + Length(FErrorMessages);
  SetLength(Result, TotalLen);

  Idx := 0;

  for Msg in FDebugMessages do
  begin
    Result[Idx] := Msg;
    Inc(Idx);
  end;

  for Msg in FInfoMessages do
  begin
    Result[Idx] := Msg;
    Inc(Idx);
  end;

  for Msg in FWarningMessages do
  begin
    Result[Idx] := Msg;
    Inc(Idx);
  end;

  for Msg in FErrorMessages do
  begin
    Result[Idx] := Msg;
    Inc(Idx);
  end;
end;

{ TMockAsyncExecutor }

function TMockAsyncExecutor.GetPendingCount: Integer;
begin
  Result := FPendingProcs.Count;
end;

constructor TMockAsyncExecutor.Create;
begin
  inherited Create;
  // Default to async (non-synchronous) to maintain existing test behavior.
  // Tests that need synchronous execution should set Synchronous := True.
  FSynchronous := False;
  FRunAsyncCallCount := 0;
  FRunDelayedCallCount := 0;
  FPendingProcs := TList<TProc>.Create;
  FPendingDelays := TList<Integer>.Create;
  FLastDelayMs := 0;
end;

destructor TMockAsyncExecutor.Destroy;
begin
  FPendingProcs.Free;
  FPendingDelays.Free;
  inherited;
end;

procedure TMockAsyncExecutor.RunAsync(AProc: TProc);
begin
  Inc(FRunAsyncCallCount);
  if FSynchronous then
    AProc()
  else
    FPendingProcs.Add(AProc);
end;

procedure TMockAsyncExecutor.RunDelayed(AProc: TProc; ADelayMs: Integer);
begin
  Inc(FRunDelayedCallCount);
  FLastDelayMs := ADelayMs;
  if FSynchronous then
    AProc()  // Execute immediately, ignoring delay
  else
  begin
    FPendingProcs.Add(AProc);
    FPendingDelays.Add(ADelayMs);
  end;
end;

procedure TMockAsyncExecutor.ExecutePending;
var
  I: Integer;
begin
  for I := 0 to FPendingProcs.Count - 1 do
    FPendingProcs[I]();
  ClearPending;
end;

procedure TMockAsyncExecutor.ClearPending;
begin
  FPendingProcs.Clear;
  FPendingDelays.Clear;
end;

{ TMockSystemClock }

constructor TMockSystemClock.Create;
begin
  inherited Create;
  // Default to a fixed known time for predictable tests
  FCurrentTime := EncodeDateTime(2024, 6, 15, 12, 0, 0, 0);
  FNowCallCount := 0;
end;

function TMockSystemClock.Now: TDateTime;
begin
  Inc(FNowCallCount);
  Result := FCurrentTime;
end;

procedure TMockSystemClock.AdvanceMs(AMilliseconds: Integer);
begin
  FCurrentTime := IncMilliSecond(FCurrentTime, AMilliseconds);
end;

procedure TMockSystemClock.AdvanceSeconds(ASeconds: Integer);
begin
  FCurrentTime := IncSecond(FCurrentTime, ASeconds);
end;

end.
