{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Hotkey Configuration Section                    }
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
    FCastPanelHotkey: string;
    FBluetoothPanelHotkey: string;
    FOnModified: TModifiedNotifier;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetHotkey: string;
    function GetUseLowLevelHook: Boolean;
    function GetCastPanelHotkey: string;
    function GetBluetoothPanelHotkey: string;

    procedure SetHotkey(const AValue: string);
    procedure SetUseLowLevelHook(AValue: Boolean);
    procedure SetCastPanelHotkey(const AValue: string);
    procedure SetBluetoothPanelHotkey(const AValue: string);

    procedure SetDefaults;

    property Hotkey: string read FHotkey write SetHotkey;
    property UseLowLevelHook: Boolean read FUseLowLevelHook write SetUseLowLevelHook;
    property CastPanelHotkey: string read FCastPanelHotkey write SetCastPanelHotkey;
    property BluetoothPanelHotkey: string read FBluetoothPanelHotkey write SetBluetoothPanelHotkey;
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
  FCastPanelHotkey := DEF_CAST_PANEL_HOTKEY;
  FBluetoothPanelHotkey := DEF_BLUETOOTH_PANEL_HOTKEY;
end;

function THotkeyConfigSection.GetHotkey: string;
begin
  Result := FHotkey;
end;

function THotkeyConfigSection.GetUseLowLevelHook: Boolean;
begin
  Result := FUseLowLevelHook;
end;

function THotkeyConfigSection.GetCastPanelHotkey: string;
begin
  Result := FCastPanelHotkey;
end;

function THotkeyConfigSection.GetBluetoothPanelHotkey: string;
begin
  Result := FBluetoothPanelHotkey;
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

procedure THotkeyConfigSection.SetCastPanelHotkey(const AValue: string);
begin
  if FCastPanelHotkey <> AValue then
  begin
    FCastPanelHotkey := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

procedure THotkeyConfigSection.SetBluetoothPanelHotkey(const AValue: string);
begin
  if FBluetoothPanelHotkey <> AValue then
  begin
    FBluetoothPanelHotkey := AValue;
    if Assigned(FOnModified) then
      FOnModified();
  end;
end;

end.
