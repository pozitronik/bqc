{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Async Executor Interface and Implementations    }
{                                                       }
{       Provides abstraction for async operations to    }
{       enable testability and reduce code duplication. }
{                                                       }
{       Design Principles:                              }
{       - DIP: High-level modules depend on abstraction }
{       - ISP: Focused interface for async execution    }
{       - OCP: New executors via interface impl         }
{       - Testability: Inject synchronous for tests     }
{                                                       }
{*******************************************************}

unit App.AsyncExecutor;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  /// Procedure type for work to be executed asynchronously.
  /// </summary>
  TAsyncWorkProc = TProc;

  /// <summary>
  /// Procedure type for callback after async work completes.
  /// Always called on the main thread.
  /// </summary>
  TAsyncCallbackProc = TProc;

  /// <summary>
  /// Procedure type for error callback.
  /// </summary>
  TAsyncErrorProc = reference to procedure(const AException: Exception);

  /// <summary>
  /// Interface for executing work asynchronously.
  /// Abstracts thread creation to enable testability.
  ///
  /// Usage in production: Inject TThreadPoolExecutor
  /// Usage in tests: Inject TSynchronousExecutor for deterministic behavior
  /// </summary>
  IAsyncExecutor = interface
    ['{E8F6C9A1-2B4D-4E8F-A1C3-5D7E9F0B2A4C}']

    /// <summary>
    /// Executes work on a background thread.
    /// Fire-and-forget pattern.
    /// </summary>
    /// <param name="AWork">The work to execute.</param>
    procedure Execute(AWork: TAsyncWorkProc);

    /// <summary>
    /// Executes work on a background thread, then executes callback on main thread.
    /// </summary>
    /// <param name="AWork">The work to execute on background thread.</param>
    /// <param name="AOnComplete">Callback to execute on main thread after work completes.</param>
    procedure ExecuteWithCallback(AWork: TAsyncWorkProc; AOnComplete: TAsyncCallbackProc);

    /// <summary>
    /// Executes work on a background thread with error handling.
    /// Callback is called on main thread; error handler on main thread if exception occurs.
    /// </summary>
    /// <param name="AWork">The work to execute on background thread.</param>
    /// <param name="AOnComplete">Callback to execute on main thread after success.</param>
    /// <param name="AOnError">Error handler to execute on main thread if exception occurs.</param>
    procedure ExecuteWithErrorHandler(AWork: TAsyncWorkProc;
      AOnComplete: TAsyncCallbackProc; AOnError: TAsyncErrorProc);
  end;

  /// <summary>
  /// Production implementation using anonymous threads.
  /// Creates a new thread for each work item.
  /// </summary>
  TThreadPoolExecutor = class(TInterfacedObject, IAsyncExecutor)
  public
    procedure Execute(AWork: TAsyncWorkProc);
    procedure ExecuteWithCallback(AWork: TAsyncWorkProc; AOnComplete: TAsyncCallbackProc);
    procedure ExecuteWithErrorHandler(AWork: TAsyncWorkProc;
      AOnComplete: TAsyncCallbackProc; AOnError: TAsyncErrorProc);
  end;

  /// <summary>
  /// Test implementation that executes work synchronously.
  /// All work is executed immediately on the calling thread.
  /// Enables deterministic unit testing without threading.
  /// </summary>
  TSynchronousExecutor = class(TInterfacedObject, IAsyncExecutor)
  private
    FLastException: Exception;
    FExecuteCount: Integer;
  public
    constructor Create;

    procedure Execute(AWork: TAsyncWorkProc);
    procedure ExecuteWithCallback(AWork: TAsyncWorkProc; AOnComplete: TAsyncCallbackProc);
    procedure ExecuteWithErrorHandler(AWork: TAsyncWorkProc;
      AOnComplete: TAsyncCallbackProc; AOnError: TAsyncErrorProc);

    /// <summary>
    /// Number of times Execute was called (for test verification).
    /// </summary>
    property ExecuteCount: Integer read FExecuteCount;

    /// <summary>
    /// Last exception that occurred during execution (for test verification).
    /// </summary>
    property LastException: Exception read FLastException;
  end;

/// <summary>
/// Creates a production async executor.
/// Factory function for dependency injection.
/// </summary>
function CreateAsyncExecutor: IAsyncExecutor;

/// <summary>
/// Creates a test (synchronous) async executor.
/// Factory function for dependency injection in tests.
/// </summary>
function CreateSynchronousExecutor: IAsyncExecutor;

implementation

uses
  App.Logger;

const
  LOG_SOURCE = 'AsyncExecutor';

{ TThreadPoolExecutor }

procedure TThreadPoolExecutor.Execute(AWork: TAsyncWorkProc);
begin
  if not Assigned(AWork) then
    Exit;

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AWork();
      except
        on E: Exception do
          LogError('Execute: Unhandled exception: %s', [E.Message], LOG_SOURCE);
      end;
    end
  ).Start;
end;

procedure TThreadPoolExecutor.ExecuteWithCallback(AWork: TAsyncWorkProc;
  AOnComplete: TAsyncCallbackProc);
var
  LOnComplete: TAsyncCallbackProc;
begin
  if not Assigned(AWork) then
  begin
    if Assigned(AOnComplete) then
      TThread.Queue(nil, AOnComplete);
    Exit;
  end;

  LOnComplete := AOnComplete;

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AWork();
        if Assigned(LOnComplete) then
          TThread.Queue(nil, LOnComplete);
      except
        on E: Exception do
          LogError('ExecuteWithCallback: Unhandled exception: %s', [E.Message], LOG_SOURCE);
      end;
    end
  ).Start;
end;

procedure TThreadPoolExecutor.ExecuteWithErrorHandler(AWork: TAsyncWorkProc;
  AOnComplete: TAsyncCallbackProc; AOnError: TAsyncErrorProc);
var
  LOnComplete: TAsyncCallbackProc;
  LOnError: TAsyncErrorProc;
begin
  if not Assigned(AWork) then
  begin
    if Assigned(AOnComplete) then
      TThread.Queue(nil, AOnComplete);
    Exit;
  end;

  LOnComplete := AOnComplete;
  LOnError := AOnError;

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AWork();
        if Assigned(LOnComplete) then
          TThread.Queue(nil, LOnComplete);
      except
        on E: Exception do
        begin
          LogError('ExecuteWithErrorHandler: Exception: %s', [E.Message], LOG_SOURCE);
          if Assigned(LOnError) then
          begin
            // Capture exception for main thread
            var LException := E;
            TThread.Queue(nil,
              procedure
              begin
                LOnError(LException);
              end
            );
          end;
        end;
      end;
    end
  ).Start;
end;

{ TSynchronousExecutor }

constructor TSynchronousExecutor.Create;
begin
  inherited Create;
  FExecuteCount := 0;
  FLastException := nil;
end;

procedure TSynchronousExecutor.Execute(AWork: TAsyncWorkProc);
begin
  Inc(FExecuteCount);

  if not Assigned(AWork) then
    Exit;

  try
    AWork();
  except
    on E: Exception do
    begin
      FLastException := E;
      LogError('Execute (sync): Exception: %s', [E.Message], LOG_SOURCE);
    end;
  end;
end;

procedure TSynchronousExecutor.ExecuteWithCallback(AWork: TAsyncWorkProc;
  AOnComplete: TAsyncCallbackProc);
begin
  Inc(FExecuteCount);

  try
    if Assigned(AWork) then
      AWork();
    if Assigned(AOnComplete) then
      AOnComplete();
  except
    on E: Exception do
    begin
      FLastException := E;
      LogError('ExecuteWithCallback (sync): Exception: %s', [E.Message], LOG_SOURCE);
    end;
  end;
end;

procedure TSynchronousExecutor.ExecuteWithErrorHandler(AWork: TAsyncWorkProc;
  AOnComplete: TAsyncCallbackProc; AOnError: TAsyncErrorProc);
begin
  Inc(FExecuteCount);

  try
    if Assigned(AWork) then
      AWork();
    if Assigned(AOnComplete) then
      AOnComplete();
  except
    on E: Exception do
    begin
      FLastException := E;
      LogError('ExecuteWithErrorHandler (sync): Exception: %s', [E.Message], LOG_SOURCE);
      if Assigned(AOnError) then
        AOnError(E);
    end;
  end;
end;

{ Factory Functions }

function CreateAsyncExecutor: IAsyncExecutor;
begin
  Result := TThreadPoolExecutor.Create;
end;

function CreateSynchronousExecutor: IAsyncExecutor;
begin
  Result := TSynchronousExecutor.Create;
end;

end.
