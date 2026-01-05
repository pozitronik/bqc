{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Window Configuration Section                    }
{                                                       }
{*******************************************************}

/// <summary>
/// Window behavior settings implementation.
/// </summary>
unit App.ConfigSection.Window;

interface

uses
  System.SysUtils,
  App.ConfigInterfaces,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Window behavior settings implementation.
  /// </summary>
  TWindowConfigSection = class(TConfigSectionBase, IWindowConfig)
  private
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;
    FMenuHideOnFocusLoss: Boolean;
    FStartMinimized: Boolean;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;
    function GetStartMinimized: Boolean;

    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);
    procedure SetStartMinimized(AValue: Boolean);

    procedure SetDefaults;

    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;
    property StartMinimized: Boolean read FStartMinimized write SetStartMinimized;
  end;

implementation

uses
  App.SettingsRepository;

{ TWindowConfigSection }

constructor TWindowConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TWindowConfigSection.SetDefaults;
begin
  FMinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := DEF_CLOSE_TO_TRAY;
  FMenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;
  FStartMinimized := DEF_START_MINIMIZED;
end;

function TWindowConfigSection.GetMinimizeToTray: Boolean;
begin
  Result := FMinimizeToTray;
end;

function TWindowConfigSection.GetCloseToTray: Boolean;
begin
  Result := FCloseToTray;
end;

function TWindowConfigSection.GetMenuHideOnFocusLoss: Boolean;
begin
  Result := FMenuHideOnFocusLoss;
end;

function TWindowConfigSection.GetStartMinimized: Boolean;
begin
  Result := FStartMinimized;
end;

procedure TWindowConfigSection.SetMinimizeToTray(AValue: Boolean);
begin
  SetFieldBoolean(FMinimizeToTray, AValue);
end;

procedure TWindowConfigSection.SetCloseToTray(AValue: Boolean);
begin
  SetFieldBoolean(FCloseToTray, AValue);
end;

procedure TWindowConfigSection.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  SetFieldBoolean(FMenuHideOnFocusLoss, AValue);
end;

procedure TWindowConfigSection.SetStartMinimized(AValue: Boolean);
begin
  SetFieldBoolean(FStartMinimized, AValue);
end;

end.
