{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Global Hotkey Manager                           }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit UI.HotkeyManager;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

const
  /// <summary>
  /// Custom message posted by the low-level keyboard hook when hotkey is detected.
  /// </summary>
  WM_HOTKEY_DETECTED = WM_USER + 100;

type
  /// <summary>
  /// Manages global hotkey registration using either standard RegisterHotKey
  /// or a low-level keyboard hook (for overriding system hotkeys).
  /// </summary>
  THotkeyManager = class
  private
    FHotkeyId: Integer;
    FHotkeyRegistered: Boolean;
    FUsingLowLevelHook: Boolean;
    FWindowHandle: HWND;
    FOnHotkeyTriggered: TNotifyEvent;

    procedure DoHotkeyTriggered;
  public
    /// <summary>
    /// Creates the hotkey manager. Allocates a global atom for the hotkey ID.
    /// </summary>
    constructor Create;

    /// <summary>
    /// Destroys the hotkey manager. Unregisters any active hotkey.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    /// Parses a hotkey string like "Ctrl+Alt+B" into modifiers and virtual key.
    /// </summary>
    /// <param name="AHotkey">The hotkey string to parse.</param>
    /// <param name="AModifiers">Output: Modifier flags (MOD_CONTROL, MOD_ALT, etc.).</param>
    /// <param name="AVirtualKey">Output: Virtual key code.</param>
    /// <returns>True if parsing succeeded.</returns>
    class function ParseHotkeyString(const AHotkey: string;
      out AModifiers: Cardinal; out AVirtualKey: Cardinal): Boolean;

    /// <summary>
    /// Builds a hotkey string from a virtual key code and shift state.
    /// Inverse of ParseHotkeyString. Used for hotkey recording.
    /// </summary>
    /// <param name="AKey">Virtual key code (VK_*).</param>
    /// <param name="AShift">Shift state (ssCtrl, ssAlt, ssShift).</param>
    /// <returns>Hotkey string like "Ctrl+Alt+B", or empty if invalid.</returns>
    class function BuildHotkeyString(AKey: Word; AShift: TShiftState): string;

    /// <summary>
    /// Registers a global hotkey for the specified window.
    /// </summary>
    /// <param name="AWindowHandle">Handle of the window to receive hotkey messages.</param>
    /// <param name="AHotkey">Hotkey string like "Ctrl+Alt+B".</param>
    /// <param name="AUseLowLevelHook">If True, uses low-level hook to override system hotkeys.</param>
    procedure Register(AWindowHandle: HWND; const AHotkey: string; AUseLowLevelHook: Boolean);

    /// <summary>
    /// Unregisters the current hotkey.
    /// </summary>
    procedure Unregister;

    /// <summary>
    /// Handles WM_HOTKEY message. Call this from the form's WM_HOTKEY handler.
    /// </summary>
    /// <param name="AWParam">WParam from the message (hotkey ID).</param>
    /// <returns>True if this was our hotkey and event was fired.</returns>
    function HandleWMHotkey(AWParam: WPARAM): Boolean;

    /// <summary>
    /// Handles WM_HOTKEY_DETECTED message from low-level hook.
    /// Call this from the form's WM_HOTKEY_DETECTED handler.
    /// </summary>
    procedure HandleHotkeyDetected;

    /// <summary>
    /// Fired when the registered hotkey is triggered.
    /// </summary>
    property OnHotkeyTriggered: TNotifyEvent read FOnHotkeyTriggered write FOnHotkeyTriggered;

    /// <summary>
    /// Returns True if a hotkey is currently registered.
    /// </summary>
    property IsRegistered: Boolean read FHotkeyRegistered;
  end;

implementation

uses
  App.Logger;

const
  // Hotkey atom name prefix for registration (instance number appended)
  HOTKEY_ATOM_PREFIX = 'BQC_GlobalHotkey_';

  // Low-level keyboard hook constants
  WH_KEYBOARD_LL = 13;
  LLKHF_UP = $80;

var
  // Counter for generating unique hotkey IDs
  GHotkeyInstanceCounter: Integer = 0;

type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

var
  // Global state for low-level keyboard hook
  // Only one hotkey manager can use the low-level hook at a time
  // Using Integer types for TInterlocked compatibility
  GKeyboardHook: HHOOK = 0;
  GHotkeyModifiers: Integer = 0;
  GHotkeyVirtualKey: Integer = 0;
  GHotkeyFormHandle: NativeInt = 0;  // NativeInt for HWND on both 32/64-bit
  GCurrentModifiers: Integer = 0;

/// <summary>
/// Atomically sets bits in Target using compare-exchange loop.
/// </summary>
procedure AtomicOr(var Target: Integer; Mask: Integer);
var
  OldValue, NewValue: Integer;
begin
  repeat
    OldValue := TInterlocked.CompareExchange(Target, 0, 0); // Atomic read
    NewValue := OldValue or Mask;
  until TInterlocked.CompareExchange(Target, NewValue, OldValue) = OldValue;
end;

/// <summary>
/// Atomically clears bits in Target using compare-exchange loop.
/// </summary>
procedure AtomicAnd(var Target: Integer; Mask: Integer);
var
  OldValue, NewValue: Integer;
begin
  repeat
    OldValue := TInterlocked.CompareExchange(Target, 0, 0); // Atomic read
    NewValue := OldValue and Mask;
  until TInterlocked.CompareExchange(Target, NewValue, OldValue) = OldValue;
end;

function LowLevelKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KbdStruct: PKBDLLHOOKSTRUCT;
  VK: Cardinal;
  IsKeyDown: Boolean;
  LocalVirtualKey, LocalModifiers, LocalCurrentMods: Integer;
  LocalFormHandle: NativeInt;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(GKeyboardHook, nCode, wParam, lParam);
    Exit;
  end;

  KbdStruct := PKBDLLHOOKSTRUCT(lParam);
  VK := KbdStruct^.vkCode;
  IsKeyDown := (KbdStruct^.flags and LLKHF_UP) = 0;

  // Track modifier key states using atomic operations
  case VK of
    VK_LWIN, VK_RWIN:
      if IsKeyDown then
        AtomicOr(GCurrentModifiers, MOD_WIN)
      else
        AtomicAnd(GCurrentModifiers, not MOD_WIN);
    VK_LCONTROL, VK_RCONTROL:
      if IsKeyDown then
        AtomicOr(GCurrentModifiers, MOD_CONTROL)
      else
        AtomicAnd(GCurrentModifiers, not MOD_CONTROL);
    VK_LMENU, VK_RMENU:
      if IsKeyDown then
        AtomicOr(GCurrentModifiers, MOD_ALT)
      else
        AtomicAnd(GCurrentModifiers, not MOD_ALT);
    VK_LSHIFT, VK_RSHIFT:
      if IsKeyDown then
        AtomicOr(GCurrentModifiers, MOD_SHIFT)
      else
        AtomicAnd(GCurrentModifiers, not MOD_SHIFT);
  end;

  // Atomic reads of configuration - ensures we see consistent values
  LocalVirtualKey := TInterlocked.CompareExchange(GHotkeyVirtualKey, 0, 0);
  LocalModifiers := TInterlocked.CompareExchange(GHotkeyModifiers, 0, 0);
  LocalCurrentMods := TInterlocked.CompareExchange(GCurrentModifiers, 0, 0);
  LocalFormHandle := TInterlocked.Read(GHotkeyFormHandle);

  // Check if our hotkey is triggered (on key down, not modifiers themselves)
  if IsKeyDown and (LocalVirtualKey <> 0) then
  begin
    // Check if this is the main key (not a modifier) and modifiers match
    if (Integer(VK) = LocalVirtualKey) and (LocalCurrentMods = LocalModifiers) then
    begin
      // Post message to form and consume the key
      PostMessage(HWND(LocalFormHandle), WM_HOTKEY_DETECTED, 0, 0);
      Result := 1;  // Consume the key, don't pass to system
      Exit;
    end;
  end;

  Result := CallNextHookEx(GKeyboardHook, nCode, wParam, lParam);
end;

{ THotkeyManager }

constructor THotkeyManager.Create;
var
  InstanceNum: Integer;
  AtomName: string;
begin
  inherited Create;
  // Generate unique atom name for this instance
  InstanceNum := TInterlocked.Increment(GHotkeyInstanceCounter);
  AtomName := HOTKEY_ATOM_PREFIX + IntToStr(InstanceNum);
  FHotkeyId := GlobalAddAtom(PChar(AtomName));
  FHotkeyRegistered := False;
  FUsingLowLevelHook := False;
  FWindowHandle := 0;
  LogDebug('Created with atom ID=%d (instance %d)', [FHotkeyId, InstanceNum], ClassName);
end;

destructor THotkeyManager.Destroy;
begin
  Unregister;
  if FHotkeyId <> 0 then
    GlobalDeleteAtom(FHotkeyId);
  LogDebug('Destroyed', ClassName);
  inherited;
end;

class function THotkeyManager.ParseHotkeyString(const AHotkey: string;
  out AModifiers: Cardinal; out AVirtualKey: Cardinal): Boolean;
var
  Parts: TArray<string>;
  Part: string;
  KeyPart: string;
  I: Integer;
begin
  Result := False;
  AModifiers := 0;
  AVirtualKey := 0;

  if Trim(AHotkey) = '' then
    Exit;

  // Split by '+' and process each part
  Parts := AHotkey.Split(['+']);
  if Length(Parts) = 0 then
    Exit;

  // Last part is the key, rest are modifiers
  KeyPart := Trim(Parts[High(Parts)]).ToUpper;

  // Process modifiers
  for I := 0 to High(Parts) - 1 do
  begin
    Part := Trim(Parts[I]).ToUpper;
    if (Part = 'CTRL') or (Part = 'CONTROL') then
      AModifiers := AModifiers or MOD_CONTROL
    else if Part = 'ALT' then
      AModifiers := AModifiers or MOD_ALT
    else if Part = 'SHIFT' then
      AModifiers := AModifiers or MOD_SHIFT
    else if (Part = 'WIN') or (Part = 'WINDOWS') then
      AModifiers := AModifiers or MOD_WIN
    else
    begin
      LogWarning('ParseHotkeyString: Unknown modifier "%s"', [Part], ClassName);
      Exit;
    end;
  end;

  // Parse the key
  if Length(KeyPart) = 1 then
  begin
    // Single character: A-Z or 0-9
    if (KeyPart[1] >= 'A') and (KeyPart[1] <= 'Z') then
      AVirtualKey := Ord(KeyPart[1])
    else if (KeyPart[1] >= '0') and (KeyPart[1] <= '9') then
      AVirtualKey := Ord(KeyPart[1])
    else
    begin
      LogWarning('ParseHotkeyString: Unknown key "%s"', [KeyPart], ClassName);
      Exit;
    end;
  end
  else if KeyPart.StartsWith('F') and (Length(KeyPart) <= 3) then
  begin
    // Function keys F1-F12
    I := StrToIntDef(KeyPart.Substring(1), 0);
    if (I >= 1) and (I <= 12) then
      AVirtualKey := VK_F1 + I - 1
    else
    begin
      LogWarning('ParseHotkeyString: Unknown function key "%s"', [KeyPart], ClassName);
      Exit;
    end;
  end
  else if KeyPart = 'SPACE' then
    AVirtualKey := VK_SPACE
  else if KeyPart = 'ENTER' then
    AVirtualKey := VK_RETURN
  else if KeyPart = 'TAB' then
    AVirtualKey := VK_TAB
  else if KeyPart = 'ESCAPE' then
    AVirtualKey := VK_ESCAPE
  else if KeyPart = 'BACKSPACE' then
    AVirtualKey := VK_BACK
  else if KeyPart = 'DELETE' then
    AVirtualKey := VK_DELETE
  else if KeyPart = 'INSERT' then
    AVirtualKey := VK_INSERT
  else if KeyPart = 'HOME' then
    AVirtualKey := VK_HOME
  else if KeyPart = 'END' then
    AVirtualKey := VK_END
  else if KeyPart = 'PAGEUP' then
    AVirtualKey := VK_PRIOR
  else if KeyPart = 'PAGEDOWN' then
    AVirtualKey := VK_NEXT
  else if KeyPart = 'UP' then
    AVirtualKey := VK_UP
  else if KeyPart = 'DOWN' then
    AVirtualKey := VK_DOWN
  else if KeyPart = 'LEFT' then
    AVirtualKey := VK_LEFT
  else if KeyPart = 'RIGHT' then
    AVirtualKey := VK_RIGHT
  else
  begin
    LogWarning('ParseHotkeyString: Unknown key "%s"', [KeyPart], ClassName);
    Exit;
  end;

  // Need at least one modifier for global hotkeys
  if AModifiers = 0 then
  begin
    LogWarning('ParseHotkeyString: No modifiers specified', ClassName);
    Exit;
  end;

  Result := True;
  LogDebug('ParseHotkeyString: Parsed "%s" -> Modifiers=$%X, VK=$%X', [AHotkey, AModifiers, AVirtualKey], ClassName);
end;

class function THotkeyManager.BuildHotkeyString(AKey: Word; AShift: TShiftState): string;
begin
  Result := '';

  // Build modifier prefix
  if ssCtrl in AShift then
    Result := Result + 'Ctrl+';
  if ssAlt in AShift then
    Result := Result + 'Alt+';
  if ssShift in AShift then
    Result := Result + 'Shift+';

  // Check for Win key (not in standard TShiftState, but we handle it for completeness)
  if GetAsyncKeyState(VK_LWIN) < 0 then
    Result := Result + 'Win+';

  // Need at least one modifier
  if Result = '' then
    Exit('');

  // Don't process modifier-only keys
  if AKey in [VK_CONTROL, VK_MENU, VK_SHIFT, VK_LWIN, VK_RWIN, VK_LCONTROL,
    VK_RCONTROL, VK_LMENU, VK_RMENU, VK_LSHIFT, VK_RSHIFT] then
    Exit('');

  // Convert virtual key to string
  case AKey of
    VK_F1..VK_F12:
      Result := Result + 'F' + IntToStr(AKey - VK_F1 + 1);
    VK_SPACE:
      Result := Result + 'Space';
    VK_RETURN:
      Result := Result + 'Enter';
    VK_ESCAPE:
      Result := Result + 'Escape';
    VK_TAB:
      Result := Result + 'Tab';
    VK_BACK:
      Result := Result + 'Backspace';
    VK_DELETE:
      Result := Result + 'Delete';
    VK_INSERT:
      Result := Result + 'Insert';
    VK_HOME:
      Result := Result + 'Home';
    VK_END:
      Result := Result + 'End';
    VK_PRIOR:
      Result := Result + 'PageUp';
    VK_NEXT:
      Result := Result + 'PageDown';
    VK_UP:
      Result := Result + 'Up';
    VK_DOWN:
      Result := Result + 'Down';
    VK_LEFT:
      Result := Result + 'Left';
    VK_RIGHT:
      Result := Result + 'Right';
    Ord('A')..Ord('Z'):
      Result := Result + Chr(AKey);
    Ord('0')..Ord('9'):
      Result := Result + Chr(AKey);
  else
    // Unknown key
    Result := '';
  end;
end;

procedure THotkeyManager.Register(AWindowHandle: HWND; const AHotkey: string;
  AUseLowLevelHook: Boolean);
var
  Modifiers, VirtualKey: Cardinal;
begin
  // Unregister any existing hotkey first
  if FHotkeyRegistered then
    Unregister;

  if Trim(AHotkey) = '' then
  begin
    LogInfo('Register: No hotkey configured', ClassName);
    Exit;
  end;

  if not ParseHotkeyString(AHotkey, Modifiers, VirtualKey) then
  begin
    LogInfo('Register: Failed to parse hotkey "%s"', [AHotkey], ClassName);
    Exit;
  end;

  FWindowHandle := AWindowHandle;

  if AUseLowLevelHook then
  begin
    // Use low-level keyboard hook (can override system hotkeys)
    // Use atomic writes to ensure hook thread sees consistent values
    TInterlocked.Exchange(GHotkeyModifiers, Integer(Modifiers));
    TInterlocked.Exchange(GHotkeyVirtualKey, Integer(VirtualKey));
    TInterlocked.Exchange(GHotkeyFormHandle, NativeInt(AWindowHandle));
    TInterlocked.Exchange(GCurrentModifiers, 0);

    GKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);
    if GKeyboardHook <> 0 then
    begin
      FHotkeyRegistered := True;
      FUsingLowLevelHook := True;
      LogInfo('Register: Installed low-level hook for "%s"', [AHotkey], ClassName);
    end
    else
    begin
      LogInfo('Register: Failed to install low-level hook (Error=%d)', [GetLastError], ClassName);
    end;
  end
  else
  begin
    // Use standard RegisterHotKey (fallback, cannot override system hotkeys)
    // Add MOD_NOREPEAT to prevent repeated triggers when holding key
    if RegisterHotKey(AWindowHandle, FHotkeyId, Modifiers or MOD_NOREPEAT, VirtualKey) then
    begin
      FHotkeyRegistered := True;
      FUsingLowLevelHook := False;
      LogInfo('Register: Registered hotkey "%s" (RegisterHotKey)', [AHotkey], ClassName);
    end
    else
    begin
      LogInfo('Register: Failed to register hotkey "%s" (Error=%d)', [AHotkey, GetLastError], ClassName);
    end;
  end;
end;

procedure THotkeyManager.Unregister;
begin
  if FHotkeyRegistered then
  begin
    if FUsingLowLevelHook then
    begin
      // Uninstall low-level hook
      if GKeyboardHook <> 0 then
      begin
        UnhookWindowsHookEx(GKeyboardHook);
        GKeyboardHook := 0;
        // Use atomic writes to ensure hook thread (if still running briefly) sees zeros
        TInterlocked.Exchange(GHotkeyModifiers, 0);
        TInterlocked.Exchange(GHotkeyVirtualKey, 0);
        TInterlocked.Exchange(GHotkeyFormHandle, 0);
        LogInfo('Unregister: Uninstalled low-level hook', ClassName);
      end;
    end
    else
    begin
      // Unregister standard hotkey
      if FWindowHandle <> 0 then
        UnregisterHotKey(FWindowHandle, FHotkeyId);
      LogInfo('Unregister: Unregistered hotkey (RegisterHotKey)', ClassName);
    end;
    FHotkeyRegistered := False;
    FUsingLowLevelHook := False;
  end;
end;

procedure THotkeyManager.DoHotkeyTriggered;
begin
  if Assigned(FOnHotkeyTriggered) then
    FOnHotkeyTriggered(Self);
end;

function THotkeyManager.HandleWMHotkey(AWParam: WPARAM): Boolean;
begin
  Result := False;
  if FHotkeyRegistered and (not FUsingLowLevelHook) then
  begin
    if AWParam = Cardinal(FHotkeyId) then
    begin
      LogDebug('HandleWMHotkey: Hotkey triggered', ClassName);
      DoHotkeyTriggered;
      Result := True;
    end;
  end;
end;

procedure THotkeyManager.HandleHotkeyDetected;
begin
  if FHotkeyRegistered and FUsingLowLevelHook then
  begin
    LogDebug('HandleHotkeyDetected: Hotkey triggered via low-level hook', ClassName);
    DoHotkeyTriggered;
  end;
end;

end.
