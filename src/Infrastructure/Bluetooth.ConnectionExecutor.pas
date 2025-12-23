{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Connection Executor Implementation              }
{                                                       }
{       Handles low-level Bluetooth connection          }
{       operations via Windows API.                     }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.ConnectionExecutor;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.WinAPI;

type
  /// <summary>
  /// Executes Bluetooth connection operations via Windows API.
  /// Single Responsibility: Only handles the actual connection execution.
  /// </summary>
  TBluetoothConnectionExecutor = class(TInterfacedObject, IConnectionExecutor)
  private
    FRetryDelayMs: Integer;

  protected
    { IConnectionExecutor }
    function Execute(
      const ADevice: TBluetoothDeviceInfo;
      const AServiceGuids: TArray<TGUID>;
      AEnable: Boolean;
      ARetryCount: Integer
    ): TConnectionResult;

  public
    constructor Create(ARetryDelayMs: Integer = 500);

    property RetryDelayMs: Integer read FRetryDelayMs write FRetryDelayMs;
  end;

/// <summary>
/// Creates a connection executor instance.
/// </summary>
function CreateConnectionExecutor: IConnectionExecutor;

implementation

uses
  App.Logger;

function CreateConnectionExecutor: IConnectionExecutor;
begin
  Result := TBluetoothConnectionExecutor.Create;
end;

{ TBluetoothConnectionExecutor }

constructor TBluetoothConnectionExecutor.Create(ARetryDelayMs: Integer);
begin
  inherited Create;
  FRetryDelayMs := ARetryDelayMs;
end;

function TBluetoothConnectionExecutor.Execute(
  const ADevice: TBluetoothDeviceInfo;
  const AServiceGuids: TArray<TGUID>;
  AEnable: Boolean;
  ARetryCount: Integer
): TConnectionResult;
var
  WinDeviceInfo: BLUETOOTH_DEVICE_INFO;
  ServiceFlag: DWORD;
  ServiceGuid: TGUID;
  ErrorCode: DWORD;
  AnySuccess: Boolean;
  Attempt: Integer;
begin
  ErrorCode := ERROR_SUCCESS;
  Result := TConnectionResult.Fail(ERROR_INVALID_PARAMETER);

  // Validate inputs
  if Length(AServiceGuids) = 0 then
  begin
    Log('Execute: No service GUIDs provided', ClassName);
    Exit;
  end;

  // Bounds check retry count
  if ARetryCount < 0 then
    ARetryCount := 0;
  if ARetryCount > 10 then
    ARetryCount := 10;

  // Prepare device info structure
  InitDeviceInfo(WinDeviceInfo);
  WinDeviceInfo.Address.ullLong := ADevice.AddressInt;
  Move(ADevice.Address[0], WinDeviceInfo.Address.rgBytes[0], 6);

  // Set service flag
  if AEnable then
    ServiceFlag := BLUETOOTH_SERVICE_ENABLE
  else
    ServiceFlag := BLUETOOTH_SERVICE_DISABLE;

  Log('Execute: Device=%s, Enable=%s, RetryCount=%d, GUIDs=%d', [
    ADevice.Name,
    BoolToStr(AEnable, True),
    ARetryCount,
    Length(AServiceGuids)
  ], ClassName);

  // Try connection with retries
  for Attempt := 0 to ARetryCount do
  begin
    if Attempt > 0 then
    begin
      Log('Execute: Retry attempt %d of %d', [Attempt, ARetryCount], ClassName);
      Sleep(FRetryDelayMs);
    end;

    AnySuccess := False;
    ErrorCode := ERROR_SUCCESS;

    // Try each service GUID
    for ServiceGuid in AServiceGuids do
    begin
      ErrorCode := BluetoothSetServiceState(
        0,  // Use first available radio
        @WinDeviceInfo,
        @ServiceGuid,
        ServiceFlag
      );

      if ErrorCode = ERROR_SUCCESS then
        AnySuccess := True;
    end;

    if AnySuccess then
    begin
      Log('Execute: Success on attempt %d', [Attempt], ClassName);
      Result := TConnectionResult.Ok;
      Exit;
    end;
  end;

  // All attempts failed
  Log('Execute: All attempts failed, last error=%d', [ErrorCode], ClassName);
  Result := TConnectionResult.Fail(ErrorCode);
end;

end.
