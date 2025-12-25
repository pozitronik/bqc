{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Section Shared Types              }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Shared types for configuration section implementations.
/// </summary>
unit App.ConfigSectionTypes;

interface

type
  /// <summary>
  /// Callback type for modification notification.
  /// Called by config sections when any property changes.
  /// </summary>
  TModifiedNotifier = reference to procedure;

implementation

end.
