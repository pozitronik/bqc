{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Appearance Configuration Interface              }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the appearance configuration interface for theme and display options.
/// </summary>
unit App.AppearanceConfigIntf;

interface

uses
  Vcl.Graphics,
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
    function GetShowBatteryLevel: Boolean;
    function GetListBackgroundSource: TListBackgroundSource;
    function GetListBackgroundCustomColor: Integer;
    function GetMainColorSource: TMainColorSource;
    function GetMainCustomColor: Integer;
    function GetSecondaryColorSource: TSecondaryColorSource;
    function GetSecondaryCustomColor: Integer;

    procedure SetShowAddresses(AValue: Boolean);
    procedure SetTheme(const AValue: string);
    procedure SetVsfDir(const AValue: string);
    procedure SetShowLastSeen(AValue: Boolean);
    procedure SetLastSeenFormat(AValue: TLastSeenFormat);
    procedure SetShowDeviceIcons(AValue: Boolean);
    procedure SetConnectedColor(AValue: Integer);
    procedure SetShowBatteryLevel(AValue: Boolean);
    procedure SetListBackgroundSource(AValue: TListBackgroundSource);
    procedure SetListBackgroundCustomColor(AValue: Integer);
    procedure SetMainColorSource(AValue: TMainColorSource);
    procedure SetMainCustomColor(AValue: Integer);
    procedure SetSecondaryColorSource(AValue: TSecondaryColorSource);
    procedure SetSecondaryCustomColor(AValue: Integer);

    property ShowAddresses: Boolean read GetShowAddresses write SetShowAddresses;
    property Theme: string read GetTheme write SetTheme;
    property VsfDir: string read GetVsfDir write SetVsfDir;
    property ShowLastSeen: Boolean read GetShowLastSeen write SetShowLastSeen;
    property LastSeenFormat: TLastSeenFormat read GetLastSeenFormat write SetLastSeenFormat;
    property ShowDeviceIcons: Boolean read GetShowDeviceIcons write SetShowDeviceIcons;
    property ConnectedColor: Integer read GetConnectedColor write SetConnectedColor;
    /// <summary>
    /// Whether to display battery level for devices supporting Bluetooth Battery Service.
    /// </summary>
    property ShowBatteryLevel: Boolean read GetShowBatteryLevel write SetShowBatteryLevel;
    /// <summary>
    /// Source of the device list background color (theme window, theme form, or custom).
    /// </summary>
    property ListBackgroundSource: TListBackgroundSource read GetListBackgroundSource write SetListBackgroundSource;
    /// <summary>
    /// Custom background color used when ListBackgroundSource is lbsCustom.
    /// </summary>
    property ListBackgroundCustomColor: Integer read GetListBackgroundCustomColor write SetListBackgroundCustomColor;
    /// <summary>
    /// Source of main text color for paired device names and icons.
    /// </summary>
    property MainColorSource: TMainColorSource read GetMainColorSource write SetMainColorSource;
    /// <summary>
    /// Custom main color used when MainColorSource is mcsCustom.
    /// </summary>
    property MainCustomColor: Integer read GetMainCustomColor write SetMainCustomColor;
    /// <summary>
    /// Source of secondary text color for other captions and unpaired device icons.
    /// </summary>
    property SecondaryColorSource: TSecondaryColorSource read GetSecondaryColorSource write SetSecondaryColorSource;
    /// <summary>
    /// Custom secondary color used when SecondaryColorSource is scsCustom.
    /// </summary>
    property SecondaryCustomColor: Integer read GetSecondaryCustomColor write SetSecondaryCustomColor;
  end;

implementation

end.
