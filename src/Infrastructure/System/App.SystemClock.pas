{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       System Clock Abstraction                        }
{                                                       }
{       Provides injectable time source for testability.}
{       Production code uses TSystemClock (real time),  }
{       tests can inject TMockSystemClock (controlled). }
{                                                       }
{*******************************************************}

unit App.SystemClock;

interface

uses
  System.SysUtils;

type
  /// <summary>
  /// Interface for system clock abstraction.
  /// Enables deterministic testing of time-dependent code.
  /// </summary>
  ISystemClock = interface
    ['{F8A3C2E1-7B4D-4E6F-9C1A-3D8E7F2B5C4E}']
    /// <summary>
    /// Returns the current date and time.
    /// </summary>
    function Now: TDateTime;
  end;

  /// <summary>
  /// Production implementation using actual system time.
  /// </summary>
  TSystemClock = class(TInterfacedObject, ISystemClock)
  public
    function Now: TDateTime;
  end;

/// <summary>
/// Returns the default system clock instance.
/// Creates a new instance each call (lightweight, reference-counted).
/// </summary>
function SystemClock: ISystemClock;

implementation

{ TSystemClock }

function TSystemClock.Now: TDateTime;
begin
  Result := System.SysUtils.Now;
end;

{ Factory function }

function SystemClock: ISystemClock;
begin
  Result := TSystemClock.Create;
end;

end.
