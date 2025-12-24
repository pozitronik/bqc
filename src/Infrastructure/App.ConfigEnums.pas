{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Enumeration Types                 }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines enumeration types used throughout the configuration system.
/// These are shared by multiple configuration interfaces.
/// </summary>
unit App.ConfigEnums;

interface

type
  /// <summary>
  /// Window display mode.
  /// </summary>
  TWindowMode = (
    wmMenu,    // Popup menu style, hides on focus loss (0)
    wmWindow   // Normal window with title bar (1)
  );

  /// <summary>
  /// Position mode for window/menu placement.
  /// </summary>
  TPositionMode = (
    pmCoordinates,   // Use saved X,Y coordinates (0)
    pmNearTray,      // Near system tray icon (1)
    pmNearCursor,    // Near mouse cursor (2)
    pmCenterScreen   // Center of active screen (3)
  );

  /// <summary>
  /// Polling mode for device state detection.
  /// </summary>
  TPollingMode = (
    pmDisabled,  // No polling, event watcher only (0)
    pmFallback,  // Polling as backup if watcher fails (1)
    pmPrimary    // Polling only, no event watcher (2)
  );

  /// <summary>
  /// Last seen timestamp display format.
  /// </summary>
  TLastSeenFormat = (
    lsfRelative,  // "2 hours ago", "Yesterday" (0)
    lsfAbsolute   // "2024-12-21 15:30" (1)
  );

  /// <summary>
  /// Notification mode for events.
  /// </summary>
  TNotificationMode = (
    nmNone,      // No notification
    nmBalloon    // Balloon tip notification
  );

  /// <summary>
  /// Notification event types.
  /// </summary>
  TNotificationEvent = (
    neConnect,        // Device connected
    neDisconnect,     // Device disconnected
    neConnectFailed,  // Connection attempt failed
    neAutoConnect     // Auto-connect triggered
  );

  /// <summary>
  /// Log severity levels.
  /// Messages with level below MinLogLevel are filtered out.
  /// </summary>
  TLogLevel = (
    llDebug,   // Verbose debugging information (0)
    llInfo,    // Informational messages (1)
    llWarning, // Warning messages (2)
    llError    // Error messages (3)
  );

const
  /// <summary>
  /// Display names for log levels.
  /// </summary>
  LogLevelNames: array[TLogLevel] of string = (
    'Debug',
    'Info',
    'Warning',
    'Error'
  );

implementation

end.
