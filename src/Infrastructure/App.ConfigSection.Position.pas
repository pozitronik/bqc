{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Position Configuration Section                  }
{                                                       }
{*******************************************************}

/// <summary>
/// Window position settings implementation.
/// </summary>
unit App.ConfigSection.Position;

interface

uses
  System.SysUtils,
  App.ConfigEnums,
  App.ConfigInterfaces,
  App.ConfigSectionTypes;

type
  /// <summary>
  /// Window position settings implementation.
  /// </summary>
  TPositionConfigSection = class(TConfigSectionBase, IPositionConfig)
  private
    FPositionMode: TPositionMode;
    FPositionX: Integer;
    FPositionY: Integer;
    FPositionW: Integer;
    FPositionH: Integer;
  public
    constructor Create(AOnModified: TModifiedNotifier);

    function GetPositionMode: TPositionMode;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetPositionW: Integer;
    function GetPositionH: Integer;

    procedure SetPositionMode(AValue: TPositionMode);
    procedure SetPositionX(AValue: Integer);
    procedure SetPositionY(AValue: Integer);
    procedure SetPositionW(AValue: Integer);
    procedure SetPositionH(AValue: Integer);

    procedure SetDefaults;

    property PositionMode: TPositionMode read FPositionMode write SetPositionMode;
    property PositionX: Integer read FPositionX write SetPositionX;
    property PositionY: Integer read FPositionY write SetPositionY;
    property PositionW: Integer read FPositionW write SetPositionW;
    property PositionH: Integer read FPositionH write SetPositionH;
  end;

implementation

uses
  App.SettingsRepository;

{ TPositionConfigSection }

constructor TPositionConfigSection.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create(AOnModified);
  SetDefaults;
end;

procedure TPositionConfigSection.SetDefaults;
begin
  FPositionMode := DEF_POSITION_MODE;
  FPositionX := DEF_POSITION_X;
  FPositionY := DEF_POSITION_Y;
  FPositionW := DEF_POSITION_W;
  FPositionH := DEF_POSITION_H;
end;

function TPositionConfigSection.GetPositionMode: TPositionMode;
begin
  Result := FPositionMode;
end;

function TPositionConfigSection.GetPositionX: Integer;
begin
  Result := FPositionX;
end;

function TPositionConfigSection.GetPositionY: Integer;
begin
  Result := FPositionY;
end;

function TPositionConfigSection.GetPositionW: Integer;
begin
  Result := FPositionW;
end;

function TPositionConfigSection.GetPositionH: Integer;
begin
  Result := FPositionH;
end;

procedure TPositionConfigSection.SetPositionMode(AValue: TPositionMode);
begin
  if FPositionMode <> AValue then
  begin
    FPositionMode := AValue;
    NotifyModified;
  end;
end;

procedure TPositionConfigSection.SetPositionX(AValue: Integer);
begin
  SetFieldInteger(FPositionX, AValue);
end;

procedure TPositionConfigSection.SetPositionY(AValue: Integer);
begin
  SetFieldInteger(FPositionY, AValue);
end;

procedure TPositionConfigSection.SetPositionW(AValue: Integer);
begin
  SetFieldInteger(FPositionW, AValue);
end;

procedure TPositionConfigSection.SetPositionH(AValue: Integer);
begin
  SetFieldInteger(FPositionH, AValue);
end;

end.
