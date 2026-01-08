{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device List Shared Types                        }
{                                                       }
{       Shared types used by UI.DeviceList and          }
{       UI.DeviceItemRenderer to avoid circular         }
{       dependencies.                                   }
{                                                       }
{*******************************************************}

unit UI.DeviceListTypes;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Types,
  System.Generics.Collections,
  Vcl.Graphics,
  Bluetooth.Types,
  App.ConfigEnums;

type
  /// <summary>
  /// Context record for item drawing operations.
  /// Holds layout parameters and calculated positions to avoid repeated lookups.
  /// </summary>
  TItemDrawContext = record
    // Layout settings
    ItemPadding: Integer;
    IconSize: Integer;
    CornerRadius: Integer;
    DeviceNameFontSize: Integer;
    StatusFontSize: Integer;
    AddressFontSize: Integer;
    ItemBorderWidth: Integer;
    ItemBorderColor: TColor;
    // Appearance settings
    ShowDeviceIcons: Boolean;
    ShowLastSeen: Boolean;
    ShowAddresses: Boolean;
    ConnectedColor: TColor;
    // Calculated positions
    NameLineTop: Integer;
    StatusLineTop: Integer;
    TextRect: TRect;
    ItemRect: TRect;
    // State
    IsSelected: Boolean;
    IsHover: Boolean;
    IsDiscovered: Boolean;  // True for unpaired discovered devices
  end;

  /// <summary>
  /// Cached layout and appearance parameters.
  /// Populated once per paint cycle to avoid repeated config interface queries.
  /// </summary>
  TCachedLayoutParams = record
    // From ILayoutConfig
    ItemHeight: Integer;
    ItemMargin: Integer;
    ItemPadding: Integer;
    IconSize: Integer;
    IconFontSize: Integer;
    CornerRadius: Integer;
    DeviceNameFontSize: Integer;
    StatusFontSize: Integer;
    AddressFontSize: Integer;
    ItemBorderWidth: Integer;
    ItemBorderColor: TColor;
    // From IAppearanceConfig
    ShowDeviceIcons: Boolean;
    ShowLastSeen: Boolean;
    ConnectedColor: TColor;
    ListBackgroundSource: TListBackgroundSource;
    ListBackgroundCustomColor: Integer;
    MainColorSource: TMainColorSource;
    MainCustomColor: Integer;
    SecondaryColorSource: TSecondaryColorSource;
    SecondaryCustomColor: Integer;
    HoverColorSource: THoverColorSource;
    HoverCustomColor: Integer;
  end;

  /// <summary>
  /// System icon cache for Windows 7 compatibility.
  /// Extracts and caches device icons from shell32.dll and imageres.dll.
  /// On Win10+, icons are rendered using Segoe MDL2 Assets font instead.
  /// </summary>
  TSystemIconCache = class
  private
    FIconHandles: TDictionary<TBluetoothDeviceType, HICON>;
    FPinIconHandle: HICON;
    FIconSize: Integer;
    function TryExtractIcon(const AFilename: string; AIndex: Integer): HICON;
    function LoadIconForDeviceType(ADeviceType: TBluetoothDeviceType): HICON;
  public
    constructor Create(AIconSize: Integer);
    destructor Destroy; override;
    /// <summary>
    /// Gets cached icon for device type. Returns 0 if not available.
    /// </summary>
    function GetIcon(ADeviceType: TBluetoothDeviceType): HICON;
    /// <summary>
    /// Gets cached pin icon. Returns 0 if not available.
    /// </summary>
    function GetPinIcon: HICON;
  end;

implementation

const
  // System icon indices for Win7 fallback (from ddores.dll and shell32.dll)
  SYSICON_HEADPHONE_FILE = 'ddores.dll';
  SYSICON_HEADPHONE_INDEX = 2;
  SYSICON_KEYBOARD_FILE = 'ddores.dll';
  SYSICON_KEYBOARD_INDEX = 26;
  SYSICON_MOUSE_FILE = 'ddores.dll';
  SYSICON_MOUSE_INDEX = 27;
  SYSICON_PHONE_FILE = 'ddores.dll';
  SYSICON_PHONE_INDEX = 11;
  SYSICON_COMPUTER_FILE = 'ddores.dll';
  SYSICON_COMPUTER_INDEX = 12;
  SYSICON_GAMEPAD_FILE = 'ddores.dll';
  SYSICON_GAMEPAD_INDEX = 25;
  SYSICON_BLUETOOTH_FILE = 'shell32.dll';
  SYSICON_BLUETOOTH_INDEX = 274;
  SYSICON_PIN_FILE = 'imageres.dll';
  SYSICON_PIN_INDEX = 197;

{ TSystemIconCache }

constructor TSystemIconCache.Create(AIconSize: Integer);
begin
  inherited Create;
  FIconHandles := TDictionary<TBluetoothDeviceType, HICON>.Create;
  FIconSize := AIconSize;
  FPinIconHandle := 0;
end;

destructor TSystemIconCache.Destroy;
var
  IconHandle: HICON;
begin
  for IconHandle in FIconHandles.Values do
    if IconHandle <> 0 then
      DestroyIcon(IconHandle);
  FIconHandles.Free;
  if FPinIconHandle <> 0 then
    DestroyIcon(FPinIconHandle);
  inherited;
end;

function TSystemIconCache.TryExtractIcon(const AFilename: string; AIndex: Integer): HICON;
var
  LargeIcon, SmallIcon: HICON;
  SystemPath: string;
  ExtractCount: UINT;
begin
  Result := 0;
  SetLength(SystemPath, MAX_PATH);
  GetSystemDirectory(PChar(SystemPath), MAX_PATH);
  SetLength(SystemPath, StrLen(PChar(SystemPath)));
  SystemPath := IncludeTrailingPathDelimiter(SystemPath) + AFilename;

  if not FileExists(SystemPath) then
    Exit;

  ExtractCount := ExtractIconEx(PChar(SystemPath), AIndex, LargeIcon, SmallIcon, 1);
  if ExtractCount > 0 then
  begin
    // Prefer small icon for consistent size
    if SmallIcon <> 0 then
    begin
      Result := SmallIcon;
      if LargeIcon <> 0 then
        DestroyIcon(LargeIcon);
    end
    else
      Result := LargeIcon;
  end;
end;

function TSystemIconCache.LoadIconForDeviceType(ADeviceType: TBluetoothDeviceType): HICON;
begin
  Result := 0;
  case ADeviceType of
    btHeadset,
    btAudioOutput:
      Result := TryExtractIcon(SYSICON_HEADPHONE_FILE, SYSICON_HEADPHONE_INDEX);
    btKeyboard:
      Result := TryExtractIcon(SYSICON_KEYBOARD_FILE, SYSICON_KEYBOARD_INDEX);
    btMouse:
      Result := TryExtractIcon(SYSICON_MOUSE_FILE, SYSICON_MOUSE_INDEX);
    btPhone:
      Result := TryExtractIcon(SYSICON_PHONE_FILE, SYSICON_PHONE_INDEX);
    btComputer:
      Result := TryExtractIcon(SYSICON_COMPUTER_FILE, SYSICON_COMPUTER_INDEX);
    btGamepad:
      Result := TryExtractIcon(SYSICON_GAMEPAD_FILE, SYSICON_GAMEPAD_INDEX);
  end;

  if Result = 0 then
    Result := TryExtractIcon(SYSICON_BLUETOOTH_FILE, SYSICON_BLUETOOTH_INDEX);
end;

function TSystemIconCache.GetIcon(ADeviceType: TBluetoothDeviceType): HICON;
begin
  if not FIconHandles.TryGetValue(ADeviceType, Result) then
  begin
    Result := LoadIconForDeviceType(ADeviceType);
    if Result <> 0 then
      FIconHandles.Add(ADeviceType, Result);
  end;
end;

function TSystemIconCache.GetPinIcon: HICON;
begin
  if FPinIconHandle = 0 then
    FPinIconHandle := TryExtractIcon(SYSICON_PIN_FILE, SYSICON_PIN_INDEX);
  Result := FPinIconHandle;
end;

end.
