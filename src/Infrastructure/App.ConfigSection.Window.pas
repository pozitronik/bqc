{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Window Configuration Section                    }
{                                                       }
{       Copyright (c) 2024                              }
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
  TWindowConfigSection = class(TInterfacedObject, IWindowConfig)
  private
    FMinimizeToTray: Boolean;
    FCloseToTray: Boolean;
    FMenuHideOnFocusLoss: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetMinimizeToTray: Boolean;
    function GetCloseToTray: Boolean;
    function GetMenuHideOnFocusLoss: Boolean;

    procedure SetMinimizeToTray(AValue: Boolean);
    procedure SetCloseToTray(AValue: Boolean);
    procedure SetMenuHideOnFocusLoss(AValue: Boolean);

    procedure SetDefaults;

    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;
    property MenuHideOnFocusLoss: Boolean read FMenuHideOnFocusLoss write SetMenuHideOnFocusLoss;
  end;

implementation

uses
  App.SettingsRepository;

{ TWindowConfigSection }

constructor TWindowConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure TWindowConfigSection.SetDefaults;
begin
  FMinimizeToTray := DEF_MINIMIZE_TO_TRAY;
  FCloseToTray := DEF_CLOSE_TO_TRAY;
  FMenuHideOnFocusLoss := DEF_MENU_HIDE_ON_FOCUS_LOSS;
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

procedure TWindowConfigSection.SetMinimizeToTray(AValue: Boolean);
begin
  if FMinimizeToTray <> AValue then
  begin
    FMinimizeToTray := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TWindowConfigSection.SetCloseToTray(AValue: Boolean);
begin
  if FCloseToTray <> AValue then
  begin
    FCloseToTray := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure TWindowConfigSection.SetMenuHideOnFocusLoss(AValue: Boolean);
begin
  if FMenuHideOnFocusLoss <> AValue then
  begin
    FMenuHideOnFocusLoss := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
