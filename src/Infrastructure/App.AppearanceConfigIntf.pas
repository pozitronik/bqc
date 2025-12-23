{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Appearance Configuration Interface              }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the appearance configuration interface for theme and display options.
/// </summary>
unit App.AppearanceConfigIntf;

interface

uses
  App.ConfigEnums;

type
  /// <summary>
  /// Appearance settings (theme, colors, display options).
  /// Used by: UI.Theme, TDeviceListBox
  /// </summary>
  IAppearanceConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000007}']
    function GetShowAddresses: Boolean;
    function GetTheme: string;
    function GetVsfDir: string;
    function GetShowLastSeen: Boolean;
    function GetLastSeenFormat: TLastSeenFormat;
    function GetShowDeviceIcons: Boolean;
    function GetConnectedColor: Integer;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);

    property ShowAddresses: Boolean read GetShowAddresses write SetShowAddresses;
    property Theme: string read GetTheme write SetTheme;
    property VsfDir: string read GetVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read GetShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read GetLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read GetShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read GetConnectedColor write SetConnectedColor;
  end;

implementation

end.
