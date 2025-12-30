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
  end;

  /// <summary>
  /// Production implementation using TThread.CreateAnonymousThread.
  /// Each call spawns a new thread for true parallel execution.
  /// </summary>
  TThreadAsyncExecutor = class(TInterfacedObject, IAsyncExecutor)
  public
    procedure RunAsync(AProc: TProc);
    procedure RunDelayed(AProc: TProc; ADelayMs: Integer);
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

{ Factory function }

function CreateAsyncExecutor: IAsyncExecutor;
begin
  Result := TThreadAsyncExecutor.Create;
end;

end.
