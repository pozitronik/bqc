{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Layout Configuration Interface                  }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the layout configuration interface for UI dimensions and fonts.
/// </summary>
unit App.LayoutConfigIntf;

interface

type
  /// <summary>
  /// Layout settings (dimensions, fonts, spacing).
  /// Used by: TDeviceListBox
  /// </summary>
  ILayoutConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000008}']
    function GetItemHeight: Integer;
    function GetItemPadding: Integer;
    function GetItemMargin: Integer;
    function GetIconSize: Integer;
    function GetCornerRadius: Integer;
    function GetDeviceNameFontSize: Integer;
    function GetStatusFontSize: Integer;
    function GetAddressFontSize: Integer;
    function GetIconFontSize: Integer;
    function GetItemBorderWidth: Integer;
    function GetItemBorderColor: Integer;
    function GetShowUnpairedDevices: Boolean;
    function GetShowUnidentifiedDevices: Boolean;
    function GetScrollbarWidth: Integer;
    function GetScrollbarOpacity: Integer;

    procedure SetItemHeight(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemMargin(AValue: Integer);
    procedure SetIconSize(AValue: Integer);
    procedure SetCornerRadius(AValue: Integer);
    procedure SetDeviceNameFontSize(AValue: Integer);
    procedure SetStatusFontSize(AValue: Integer);
    procedure SetAddressFontSize(AValue: Integer);
    procedure SetIconFontSize(AValue: Integer);
    procedure SetItemBorderWidth(AValue: Integer);
    procedure SetItemBorderColor(AValue: Integer);
    procedure SetShowUnpairedDevices(AValue: Boolean);
    procedure SetShowUnidentifiedDevices(AValue: Boolean);
    procedure SetScrollbarWidth(AValue: Integer);
    procedure SetScrollbarOpacity(AValue: Integer);

    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemPadding: Integer read GetItemPadding write SetItemPadding;
    property ItemMargin: Integer read GetItemMargin write SetItemMargin;
    property IconSize: Integer read GetIconSize write SetIconSize;
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius;
    property DeviceNameFontSize: Integer read GetDeviceNameFontSize write SetDeviceNameFontSize;
    property StatusFontSize: Integer read GetStatusFontSize write SetStatusFontSize;
    property AddressFontSize: Integer read GetAddressFontSize write SetAddressFontSize;
    property IconFontSize: Integer read GetIconFontSize write SetIconFontSize;
    property ItemBorderWidth: Integer read GetItemBorderWidth write SetItemBorderWidth;
    property ItemBorderColor: Integer read GetItemBorderColor write SetItemBorderColor;
    property ShowUnpairedDevices: Boolean read GetShowUnpairedDevices write SetShowUnpairedDevices;
    property ShowUnidentifiedDevices: Boolean read GetShowUnidentifiedDevices write SetShowUnidentifiedDevices;
    property ScrollbarWidth: Integer read GetScrollbarWidth write SetScrollbarWidth;
    property ScrollbarOpacity: Integer read GetScrollbarOpacity write SetScrollbarOpacity;
  end;

implementation

end.
