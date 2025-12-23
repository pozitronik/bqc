{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Event Debouncer                                 }
{                                                       }
{       Prevents duplicate event processing when        }
{       multiple Windows Bluetooth events fire for      }
{       the same device state change.                   }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit Bluetooth.EventDebouncer;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types;

type
  /// <summary>
  /// Event types that can be debounced.
  /// </summary>
  TDeviceEventType = (
    detConnect,
    detDisconnect,
    detAttributeChange
  );

  /// <summary>
  /// Debounces Bluetooth device events to prevent duplicate processing.
  /// Multiple Windows Bluetooth event sources (HCI, L2CAP, RadioInRange)
  /// can fire for the same device state change within milliseconds.
  /// This class filters out duplicate events within a configurable time window.
  /// </summary>
  TDeviceEventDebouncer = class
  private
    FLastEvents: TDictionary<string, TDateTime>;
    FDebounceMs: Integer;
    FLock: TObject;

    function MakeKey(AAddress: UInt64; AEventType: TDeviceEventType;
      AConnectionState: TBluetoothConnectionState): string;
  public
    /// <summary>
    /// Creates a debouncer with the specified debounce interval.
    /// </summary>
    /// <param name="ADebounceMs">Minimum milliseconds between identical events.</param>
    constructor Create(ADebounceMs: Integer = 500);

    destructor Destroy; override;

    /// <summary>
    /// Checks if an event should be processed or filtered as a duplicate.
    /// </summary>
    /// <param name="AAddress">Device Bluetooth address.</param>
    /// <param name="AEventType">Type of event.</param>
    /// <param name="AConnectionState">Current connection state.</param>
    /// <returns>True if event should be processed, False if duplicate.</returns>
    function ShouldProcess(AAddress: UInt64; AEventType: TDeviceEventType;
      AConnectionState: TBluetoothConnectionState): Boolean;

    /// <summary>
    /// Clears all recorded events, allowing immediate processing of next event.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Gets or sets the debounce interval in milliseconds.
    /// </summary>
    property DebounceMs: Integer read FDebounceMs write FDebounceMs;
  end;

implementation

uses
  System.DateUtils,
  App.Logger;

{ TDeviceEventDebouncer }

constructor TDeviceEventDebouncer.Create(ADebounceMs: Integer);
begin
  inherited Create;
  FLastEvents := TDictionary<string, TDateTime>.Create;
  FDebounceMs := ADebounceMs;
  FLock := TObject.Create;
  Log('Created with debounce interval=%d ms', [ADebounceMs], ClassName);
end;

destructor TDeviceEventDebouncer.Destroy;
begin
  FLastEvents.Free;
  FLock.Free;
  inherited Destroy;
end;

function TDeviceEventDebouncer.MakeKey(AAddress: UInt64;
  AEventType: TDeviceEventType; AConnectionState: TBluetoothConnectionState): string;
const
  EventTypeNames: array[TDeviceEventType] of string = (
    'Connect', 'Disconnect', 'AttrChange'
  );
  StateNames: array[TBluetoothConnectionState] of string = (
    'Disconnected', 'Connected', 'Connecting', 'Disconnecting', 'Unknown', 'Error'
  );
begin
  // Key format: "Address:EventType:State"
  // Include state so that rapid connect->disconnect sequences aren't filtered
  Result := Format('%.12X:%s:%s', [AAddress, EventTypeNames[AEventType], StateNames[AConnectionState]]);
end;

function TDeviceEventDebouncer.ShouldProcess(AAddress: UInt64;
  AEventType: TDeviceEventType; AConnectionState: TBluetoothConnectionState): Boolean;
var
  Key: string;
  LastTime: TDateTime;
  ElapsedMs: Int64;
begin
  TMonitor.Enter(FLock);
  try
    Key := MakeKey(AAddress, AEventType, AConnectionState);

    if FLastEvents.TryGetValue(Key, LastTime) then
    begin
      ElapsedMs := MilliSecondsBetween(Now, LastTime);
      if ElapsedMs < FDebounceMs then
      begin
        Log('Filtered duplicate event: %s (elapsed=%d ms < %d ms)',
          [Key, ElapsedMs, FDebounceMs], ClassName);
        Result := False;
        Exit;
      end;
    end;

    // Record this event and allow processing
    FLastEvents.AddOrSetValue(Key, Now);
    Log('Processing event: %s', [Key], ClassName);
    Result := True;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TDeviceEventDebouncer.Clear;
begin
  TMonitor.Enter(FLock);
  try
    FLastEvents.Clear;
    Log('Cleared all recorded events', ClassName);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
