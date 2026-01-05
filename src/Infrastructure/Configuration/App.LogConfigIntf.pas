{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Logging Configuration Interfaces                }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines logging configuration and logger interfaces.
/// </summary>
unit App.LogConfigIntf;

interface

uses
  App.ConfigEnums;

type
  /// <summary>
  /// Logging settings.
  /// Used by: App.Logger (indirectly via TAppConfig)
  /// </summary>
  ILogConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000006}']
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    function GetLogAppend: Boolean;
    function GetLogLevel: TLogLevel;

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);
    procedure SetLogLevel(AValue: TLogLevel);

    property LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;
    property LogFilename: string read GetLogFilename write SetLogFilename;
    property LogAppend: Boolean read GetLogAppend write SetLogAppend;
    property LogLevel: TLogLevel read GetLogLevel write SetLogLevel;
  end;

  /// <summary>
  /// Interface for application logging.
  /// Provides structured logging with severity levels and source identification.
  /// </summary>
  ILogger = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000D}']

    /// <summary>
    /// Logs a debug message (verbose diagnostics).
    /// </summary>
    procedure Debug(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Logs an informational message.
    /// </summary>
    procedure Info(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Logs a warning message.
    /// </summary>
    procedure Warning(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Logs an error message.
    /// </summary>
    procedure Error(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Returns true if logging is currently enabled.
    /// </summary>
    function IsEnabled: Boolean;

    /// <summary>
    /// Returns the current minimum log level.
    /// </summary>
    function GetMinLevel: TLogLevel;
  end;

implementation

end.
