{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Profile Configuration Section                   }
{                                                       }
{*******************************************************}

/// <summary>
/// Profile display settings implementation.
/// </summary>
unit App.ConfigSection.Profile;

interface

uses
  System.SysUtils,
  App.ProfileConfigIntf,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Profile display settings implementation.
  /// </summary>
  TProfileConfigSection = class(TConfigSectionBase, IProfileConfig)
  private
    FShowProfiles: Boolean;
    FProfileFontSize: Integer;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetShowProfiles: Boolean;
    function GetProfileFontSize: Integer;

    procedure SetShowProfiles(AValue: Boolean);
    procedure SetProfileFontSize(AValue: Integer);

    procedure SetDefaults;

    property ShowProfiles: Boolean read FShowProfiles write SetShowProfiles;
    property ProfileFontSize: Integer read FProfileFontSize write SetProfileFontSize;
  end;

implementation

uses
  App.SettingsRepository,
  App.Config;

{ TProfileConfigSection }

constructor TProfileConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TProfileConfigSection.SetDefaults;
begin
  FShowProfiles := DEF_SHOW_PROFILES;
  FProfileFontSize := DEF_PROFILE_FONT_SIZE;
end;

function TProfileConfigSection.GetShowProfiles: Boolean;
begin
  Result := FShowProfiles;
end;

function TProfileConfigSection.GetProfileFontSize: Integer;
begin
  Result := FProfileFontSize;
end;

procedure TProfileConfigSection.SetShowProfiles(AValue: Boolean);
begin
  SetFieldBoolean(FShowProfiles, AValue);
end;

procedure TProfileConfigSection.SetProfileFontSize(AValue: Integer);
begin
  SetFieldInteger(FProfileFontSize, AValue);
end;

end.
