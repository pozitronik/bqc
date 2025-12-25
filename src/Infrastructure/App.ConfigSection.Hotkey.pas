{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Hotkey Configuration Section                    }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

/// <summary>
/// Hotkey settings implementation.
/// </summary>
unit App.ConfigSection.Hotkey;

interface

uses
  System.SysUtils,
  App.ConfigInterfaces,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Hotkey settings implementation.
  /// </summary>
  THotkeyConfigSection = class(TInterfacedObject, IHotkeyConfig)
  private
    FHotkey: string;
    FUseLowLevelHook: Boolean;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;

    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);

    procedure SetDefaults;

    property Hotkey: string read FHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read FUseLowLevelHook write SetUseLowLevelHook;
  end;

implementation

uses
  App.SettingsRepository;

{ THotkeyConfigSection }

constructor THotkeyConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
  SetDefaults;
end;

procedure THotkeyConfigSection.SetDefaults;
begin
  FHotkey := DEF_HOTKEY;
  FUseLowLevelHook := DEF_USE_LOW_LEVEL_HOOK;
end;

function THotkeyConfigSection.GetHotkey: string;
begin
  Result := FHotkey;
end;

function THotkeyConfigSection.GetUseLowLevelHook: Boolean;
begin
  Result := FUseLowLevelHook;
end;

procedure THotkeyConfigSection.SetHotkey(const AValue: string);
begin
  if FHotkey <> AValue then
  begin
    FHotkey := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure THotkeyConfigSection.SetUseLowLevelHook(AValue: Boolean);
begin
  if FUseLowLevelHook <> AValue then
  begin
    FUseLowLevelHook := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
