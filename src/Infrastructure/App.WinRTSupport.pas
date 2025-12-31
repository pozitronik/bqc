{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       WinRT Support Detection                         }
{                                                       }
{       Provides runtime detection of WinRT API         }
{       availability for Windows 7 compatibility.       }
{                                                       }
{*******************************************************}

unit App.WinRTSupport;

interface

uses
  System.SysUtils,
  App.ConfigEnums;

type
  /// <summary>
  /// Exception raised when WinRT platform is requested but not available.
  /// </summary>
  EBluetoothPlatformError = class(Exception);

  /// <summary>
  /// Provides WinRT availability detection and platform selection.
  /// WinRT APIs (combase.dll) are only available on Windows 8+.
  /// </summary>
  TWinRTSupport = class
  private class var
    FChecked: Boolean;
    FAvailable: Boolean;
  private
    class procedure CheckAvailability;
  public
    /// <summary>
    /// Returns True if WinRT APIs are available on this system.
    /// Result is cached after first call.
    /// </summary>
    class function IsAvailable: Boolean;

    /// <summary>
    /// Selects the appropriate Bluetooth platform based on preference and availability.
    /// Returns the platform to use after validation.
    /// Raises EBluetoothPlatformError if WinRT is requested but not available.
    /// </summary>
    class function SelectPlatform(APreferred: TBluetoothPlatform): TBluetoothPlatform;

    /// <summary>
    /// Returns True if the current Windows version supports dark mode.
    /// Dark mode was introduced in Windows 10 version 1809.
    /// </summary>
    class function IsDarkModeSupported: Boolean;

    /// <summary>
    /// Resets the cached availability check.
    /// Mainly useful for testing.
    /// </summary>
    class procedure ResetCache;
  end;

/// <summary>
/// Shorthand function for TWinRTSupport.IsAvailable.
/// </summary>
function IsWinRTAvailable: Boolean; inline;

/// <summary>
/// Shorthand function for TWinRTSupport.IsDarkModeSupported.
/// </summary>
function IsDarkModeSupported: Boolean; inline;

implementation

uses
  Winapi.Windows;

type
  // Function pointer type for RoInitialize
  TRoInitialize = function(initType: Cardinal): HRESULT; stdcall;

const
  RO_INIT_MULTITHREADED = 1;

{ TWinRTSupport }

class procedure TWinRTSupport.CheckAvailability;
var
  Module: HMODULE;
  RoInit: TRoInitialize;
  HR: HRESULT;
begin
  FChecked := True;
  FAvailable := False;

  // Try to load combase.dll dynamically
  Module := LoadLibrary('combase.dll');
  if Module = 0 then
    Exit;

  try
    // Try to get RoInitialize function
    @RoInit := GetProcAddress(Module, 'RoInitialize');
    if not Assigned(RoInit) then
      Exit;

    // Try to initialize WinRT
    // S_OK (0) = success
    // S_FALSE (1) = already initialized (also success)
    // RPC_E_CHANGED_MODE = already initialized with different mode (also means WinRT works)
    HR := RoInit(RO_INIT_MULTITHREADED);
    FAvailable := Succeeded(HR) or (HR = 1) or (HR = HRESULT($80010106));
  finally
    FreeLibrary(Module);
  end;
end;

class function TWinRTSupport.IsAvailable: Boolean;
begin
  if not FChecked then
    CheckAvailability;
  Result := FAvailable;
end;

class function TWinRTSupport.SelectPlatform(APreferred: TBluetoothPlatform): TBluetoothPlatform;
begin
  case APreferred of
    bpAuto:
      // Auto-detect: WinRT if available, otherwise Classic
      if IsAvailable then
        Result := bpWinRT
      else
        Result := bpClassic;

    bpClassic:
      // Classic Bluetooth always allowed (Win7-Win11)
      Result := bpClassic;

    bpWinRT:
      // WinRT requires Windows 8+
      if IsAvailable then
        Result := bpWinRT
      else
        raise EBluetoothPlatformError.Create('WinRT Bluetooth APIs are not available on this system. ' +
          'WinRT requires Windows 8 or later. Switch to Classic or Auto mode in settings.');
  else
    Result := bpAuto;  // Fallback
  end;
end;

class function TWinRTSupport.IsDarkModeSupported: Boolean;
var
  VerInfo: TOSVersionInfoEx;
begin
  // Dark mode requires Windows 10 version 1809 (build 17763) or later
  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);

  {$WARN SYMBOL_DEPRECATED OFF}
  if not GetVersionEx(VerInfo) then
    Exit(False);
  {$WARN SYMBOL_DEPRECATED ON}

  // Windows 10 is version 10.0
  // Build 17763 is version 1809 which introduced dark mode
  Result := (VerInfo.dwMajorVersion > 10) or
            ((VerInfo.dwMajorVersion = 10) and (VerInfo.dwBuildNumber >= 17763));
end;

class procedure TWinRTSupport.ResetCache;
begin
  FChecked := False;
  FAvailable := False;
end;

{ Shorthand functions }

function IsWinRTAvailable: Boolean;
begin
  Result := TWinRTSupport.IsAvailable;
end;

function IsDarkModeSupported: Boolean;
begin
  Result := TWinRTSupport.IsDarkModeSupported;
end;

end.
