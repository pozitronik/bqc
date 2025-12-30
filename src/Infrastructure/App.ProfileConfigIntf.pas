{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Profile Display Configuration Interface         }
{                                                       }
{*******************************************************}

/// <summary>
/// Defines the profile display configuration interface.
/// </summary>
unit App.ProfileConfigIntf;

interface

type
  /// <summary>
  /// Profile display settings.
  /// Used by: UI.DeviceList, UI.ListGeometry
  /// </summary>
  IProfileConfig = interface
    ['{A1B2C3D4-1111-1111-1111-000000000020}']
    function GetShowProfiles: Boolean;
    function GetProfileFontSize: Integer;

    procedure SetShowProfiles(AValue: Boolean);
    procedure SetProfileFontSize(AValue: Integer);

    /// <summary>
    /// Whether to display profile tree below device items.
    /// Only shown when device has more than one profile.
    /// </summary>
    property ShowProfiles: Boolean read GetShowProfiles write SetShowProfiles;

    /// <summary>
    /// Font size for profile text in the profile tree.
    /// </summary>
    property ProfileFontSize: Integer read GetProfileFontSize write SetProfileFontSize;
  end;

implementation

end.
