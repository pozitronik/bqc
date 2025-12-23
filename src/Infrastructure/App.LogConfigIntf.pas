{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Logging Configuration Interfaces                }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines logging configuration and logger interfaces.
/// </summary>
unit App.LogConfigIntf;

interface

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

    procedure SetLogEnabled(AValue: Boolean);
    procedure SetLogFilename(const AValue: string);
    procedure SetLogAppend(AValue: Boolean);

    property LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;
    property LogFilename: string read GetLogFilename write SetLogFilename;
    property LogAppend: Boolean read GetLogAppend write SetLogAppend;
  end;

  /// <summary>
  /// Interface for application logging.
  /// Provides structured logging with source identification.
  /// </summary>
  ILogger = interface
    ['{A1B2C3D4-1111-1111-1111-00000000000D}']

    /// <summary>
    /// Logs a message with optional source identifier.
    /// </summary>
    /// <param name="AMessage">The message to log.</param>
    /// <param name="ASource">Source identifier (e.g., 'MainForm', 'Service').</param>
    procedure Log(const AMessage: string; const ASource: string = '');

    /// <summary>
    /// Logs a formatted message with optional source identifier.
    /// </summary>
    /// <param name="AFormat">Format string.</param>
    /// <param name="AArgs">Format arguments.</param>
    /// <param name="ASource">Source identifier.</param>
    procedure LogFmt(const AFormat: string; const AArgs: array of const;
      const ASource: string = '');

    /// <summary>
    /// Returns true if logging is currently enabled.
    /// </summary>
    function IsEnabled: Boolean;
  end;

implementation

end.
