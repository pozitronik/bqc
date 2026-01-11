{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Async Executor Interface and Implementation     }
{                                                       }
{*******************************************************}

/// <summary>
/// Provides abstraction for asynchronous execution to enable testability.
/// Production implementation uses TThread, test mocks can execute synchronously.
/// </summary>
unit App.AsyncExecutor;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  /// Procedure type for error handling in async operations.
  /// Called on main thread when an exception occurs during async work.
  /// </summary>
  TAsyncErrorHandler = reference to procedure(const AException: Exception);

  /// <summary>
  /// Interface for executing code asynchronously.
  /// Enables dependency injection for testable async operations.
  /// </summary>
  IAsyncExecutor = interface
    ['{A7E3F8C2-5B4D-4E6A-9C1F-3D8E7A2B5C4D}']
    /// <summary>
    /// Executes a procedure asynchronously in a background thread.
    /// Returns immediately; the procedure runs in parallel.
    /// </summary>
    procedure RunAsync(AProc: TProc);

    /// <summary>
    /// Executes a procedure after a specified delay.
    /// The delay and execution both happen in a background thread.
    /// </summary>
    procedure RunDelayed(AProc: TProc; ADelayMs: Integer);

    /// <summary>
    /// Executes a procedure asynchronously with error handling.
    /// If an exception occurs during execution, the error handler is called on the main thread.
    /// </summary>
    /// <param name="AWork">The work to execute asynchronously.</param>
    /// <param name="AOnError">Error handler called on main thread if exception occurs.</param>
    procedure RunAsyncWithErrorHandler(AWork: TProc; AOnError: TAsyncErrorHandler);
  end;

  /// <summary>
  /// Production implementation using TThread.CreateAnonymousThread.
  /// Each call spawns a new thread for true parallel execution.
  /// </summary>
  TThreadAsyncExecutor = class(TInterfacedObject, IAsyncExecutor)
  public
    procedure RunAsync(AProc: TProc);
    procedure RunDelayed(AProc: TProc; ADelayMs: Integer);
    procedure RunAsyncWithErrorHandler(AWork: TProc; AOnError: TAsyncErrorHandler);
  end;

  /// <summary>
  /// Synchronous implementation for testing.
  /// Executes procedures immediately on the calling thread for deterministic behavior.
  /// Tracks execution counts for test verification.
  /// </summary>
  TSynchronousExecutor = class(TInterfacedObject, IAsyncExecutor)
  private
    FRunAsyncCount: Integer;
    FRunDelayedCount: Integer;
    FRunAsyncWithErrorHandlerCount: Integer;
    FLastException: Exception;
  public
    destructor Destroy; override;
    /// <summary>
    /// Executes procedure immediately (synchronously).
    /// </summary>
    procedure RunAsync(AProc: TProc);

    /// <summary>
    /// Executes procedure immediately, ignoring delay (synchronously).
    /// </summary>
    procedure RunDelayed(AProc: TProc; ADelayMs: Integer);

    /// <summary>
    /// Executes procedure immediately with error capture (synchronously).
    /// </summary>
    procedure RunAsyncWithErrorHandler(AWork: TProc; AOnError: TAsyncErrorHandler);

    /// <summary>
    /// Number of times RunAsync was called.
    /// </summary>
    property RunAsyncCount: Integer read FRunAsyncCount;

    /// <summary>
    /// Number of times RunDelayed was called.
    /// </summary>
    property RunDelayedCount: Integer read FRunDelayedCount;

    /// <summary>
    /// Number of times RunAsyncWithErrorHandler was called.
    /// </summary>
    property RunAsyncWithErrorHandlerCount: Integer read FRunAsyncWithErrorHandlerCount;

    /// <summary>
    /// Last exception captured by RunAsyncWithErrorHandler (for test assertions).
    /// </summary>
    property LastException: Exception read FLastException;
  end;

/// <summary>
/// Creates the default async executor for production use.
/// </summary>
function CreateAsyncExecutor: IAsyncExecutor;

implementation

{ TThreadAsyncExecutor }

procedure TThreadAsyncExecutor.RunAsync(AProc: TProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      AProc();
    end
  ).Start;
end;

procedure TThreadAsyncExecutor.RunDelayed(AProc: TProc; ADelayMs: Integer);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(ADelayMs);
      AProc();
    end
  ).Start;
end;

procedure TThreadAsyncExecutor.RunAsyncWithErrorHandler(AWork: TProc; AOnError: TAsyncErrorHandler);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      ExceptionObj: Exception;
    begin
      ExceptionObj := nil;
      try
        AWork();
      except
        // Take ownership of exception before except block exits.
        // Without AcquireExceptionObject, Delphi auto-frees the exception
        // when the except block ends, causing use-after-free in TThread.Queue.
        ExceptionObj := Exception(AcquireExceptionObject);
      end;

      if Assigned(ExceptionObj) then
      begin
        if Assigned(AOnError) then
        begin
          TThread.Queue(nil,
            procedure
            begin
              try
                AOnError(ExceptionObj);
              finally
                // We own the exception now, so we must free it
                ExceptionObj.Free;
              end;
            end
          );
        end
        else
          // No error handler provided, clean up immediately
          ExceptionObj.Free;
      end;
    end
  ).Start;
end;

{ TSynchronousExecutor }

destructor TSynchronousExecutor.Destroy;
begin
  FLastException.Free;
  inherited;
end;

procedure TSynchronousExecutor.RunAsync(AProc: TProc);
begin
  Inc(FRunAsyncCount);
  if Assigned(AProc) then
    AProc();
end;

procedure TSynchronousExecutor.RunDelayed(AProc: TProc; ADelayMs: Integer);
begin
  Inc(FRunDelayedCount);
  // Ignore delay in test mode - execute immediately
  if Assigned(AProc) then
    AProc();
end;

procedure TSynchronousExecutor.RunAsyncWithErrorHandler(AWork: TProc; AOnError: TAsyncErrorHandler);
begin
  Inc(FRunAsyncWithErrorHandlerCount);
  if Assigned(AWork) then
  begin
    try
      AWork();
    except
      on E: Exception do
      begin
        // Acquire exception object for storage (prevents auto-free)
        FreeAndNil(FLastException);  // Free previous exception if exists
        FLastException := Exception(AcquireExceptionObject);
        if Assigned(AOnError) then
          AOnError(E);  // Pass original E to handler
      end;
    end;
  end;
end;

{ Factory function }

function CreateAsyncExecutor: IAsyncExecutor;
begin
  Result := TThreadAsyncExecutor.Create;
end;

end.
