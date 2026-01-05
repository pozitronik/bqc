{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       General Configuration Section                   }
{                                                       }
{*******************************************************}

/// <summary>
/// General application settings implementation.
/// </summary>
unit App.ConfigSection.General;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// General application settings implementation.
  /// </summary>
  TGeneralConfigSection = class(TConfigSectionBase, IGeneralConfig)
  private
    FWindowMode: TWindowMode;
    FOnTop: Boolean;
    FAutostart: Boolean;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetWindowMode: TWindowMode;
    function GetOnTop: Boolean;
    function GetAutostart: Boolean;

    procedure SetWindowMode(AValue: TWindowMode);
    procedure SetOnTop(AValue: Boolean);
    procedure SetAutostart(AValue: Boolean);

    procedure SetDefaults;

    property WindowMode: TWindowMode read FWindowMode write SetWindowMode;
    property OnTop: Boolean read FOnTop write SetOnTop;
    property Autostart: Boolean read FAutostart write SetAutostart;
  end;

implementation

uses
  App.SettingsRepository;

{ TGeneralConfigSection }

constructor TGeneralConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TGeneralConfigSection.SetDefaults;
begin
  FWindowMode := DEF_WINDOW_MODE;
  FOnTop := DEF_ON_TOP;
  FAutostart := DEF_AUTOSTART;
end;

function TGeneralConfigSection.GetWindowMode: TWindowMode;
begin
  Result := FWindowMode;
end;

function TGeneralConfigSection.GetOnTop: Boolean;
begin
  Result := FOnTop;
end;

function TGeneralConfigSection.GetAutostart: Boolean;
begin
  Result := FAutostart;
end;

procedure TGeneralConfigSection.SetWindowMode(AValue: TWindowMode);
begin
  if FWindowMode <> AValue then
  begin
    FWindowMode := AValue;
    NotifyModified;
  end;
end;

procedure TGeneralConfigSection.SetOnTop(AValue: Boolean);
begin
  SetFieldBoolean(FOnTop, AValue);
end;

procedure TGeneralConfigSection.SetAutostart(AValue: Boolean);
begin
  SetFieldBoolean(FAutostart, AValue);
end;

end.
