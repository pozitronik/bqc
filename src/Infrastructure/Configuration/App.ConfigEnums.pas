{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Enumeration Types                 }
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

  /// <summary>
  /// Device enumeration mode for Bluetooth device discovery.
  /// </summary>
  TEnumerationMode = (
    emWin32,     // Win32 Bluetooth API only (0)
    emWinRT,     // WinRT API only (1)
    emComposite  // Combined Win32 + WinRT (default) (2)
  );

  /// <summary>
  /// Bluetooth platform mode.
  /// Determines which Bluetooth API platform to use (Classic Win32 vs WinRT).
  /// Allows explicit platform selection for debugging and Windows 7 support.
  /// </summary>
  TBluetoothPlatform = (
    bpAuto,     // Auto-detect: WinRT if available, otherwise Classic (0)
    bpClassic,  // Force Classic Bluetooth (Win32 APIs) - Works on Win7-Win11 (1)
    bpWinRT     // Force WinRT Bluetooth APIs - Requires Win8+ (2)
  );

  /// <summary>
  /// Pairing mode for WinRT Bluetooth pairing operations.
  /// Determines how pairing ceremonies are handled (automatic Windows dialogs vs custom UI).
  /// </summary>
  TPairingMode = (
    pmAutomatic,  // Use Windows system pairing dialogs (default, simple) (0)
    pmCustom      // Use custom pairing ceremony with in-app UI (advanced) (1)
  );

  /// <summary>
  /// Battery icon outline color mode for taskbar visibility.
  /// Controls how the outline color is determined.
  /// </summary>
  TOutlineColorMode = (
    ocmAuto,    // Auto-detect from Windows dark/light mode (0)
    ocmLight,   // Light outline (white) for dark taskbars (1)
    ocmDark,    // Dark outline (black) for light taskbars (2)
    ocmCustom   // User-specified custom color (3)
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
