{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Global Hotkey Manager                           }
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
    FInstanceId: Integer;  // Unique ID to identify this manager's entry in global list

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
    /// <param name="AWParam">WParam from the message (InstanceId of the triggered hotkey).</param>
    /// <returns>True if this was our hotkey and event was fired.</returns>
    function HandleHotkeyDetected(AWParam: WPARAM): Boolean;

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

  /// <summary>
  /// Represents a single registered hotkey in the global hook list.
  /// </summary>
  THotkeyEntry = record
    InstanceId: Integer;      // Unique ID to identify which manager owns this
    Modifiers: Integer;       // Modifier flags (MOD_CONTROL, MOD_ALT, etc.)
    VirtualKey: Integer;      // Virtual key code
    WindowHandle: NativeInt;  // Window to send WM_HOTKEY_DETECTED message
  end;

var
  // Global state for low-level keyboard hook
  // Supports multiple concurrent hotkeys - each THotkeyManager adds an entry
  GKeyboardHook: HHOOK = 0;
  GHotkeyList: array of THotkeyEntry;  // Dynamic array of registered hotkeys
  GHotkeyListLock: TCriticalSection;   // Thread-safe access to list
  GCurrentModifiers: Integer = 0;      // Current modifier key state (atomic)

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
  LocalCurrentMods: Integer;
  I: Integer;
  Entry: THotkeyEntry;
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

  // Check if any registered hotkey is triggered (on key down, not modifiers themselves)
  if IsKeyDown then
  begin
    LocalCurrentMods := TInterlocked.CompareExchange(GCurrentModifiers, 0, 0);

    // Check all registered hotkeys
    GHotkeyListLock.Enter;
    try
      for I := 0 to High(GHotkeyList) do
      begin
        Entry := GHotkeyList[I];
        // Check if this is the main key (not a modifier) and modifiers match
        if (Integer(VK) = Entry.VirtualKey) and (LocalCurrentMods = Entry.Modifiers) then
        begin
          // Post message to the corresponding window and consume the key
          // wParam contains InstanceId so the correct manager can identify its hotkey
          PostMessage(HWND(Entry.WindowHandle), WM_HOTKEY_DETECTED, Entry.InstanceId, 0);
          Result := 1;  // Consume the key, don't pass to system
          Exit;
        end;
      end;
    finally
      GHotkeyListLock.Leave;
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
  // Generate unique instance ID for this manager
  InstanceNum := TInterlocked.Increment(GHotkeyInstanceCounter);
  FInstanceId := InstanceNum;

  // Generate unique atom name for RegisterHotKey
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
    // Add this hotkey to the global list
    GHotkeyListLock.Enter;
    try
      // Add entry to list
      SetLength(GHotkeyList, Length(GHotkeyList) + 1);
      GHotkeyList[High(GHotkeyList)].InstanceId := FInstanceId;
      GHotkeyList[High(GHotkeyList)].Modifiers := Integer(Modifiers);
      GHotkeyList[High(GHotkeyList)].VirtualKey := Integer(VirtualKey);
      GHotkeyList[High(GHotkeyList)].WindowHandle := NativeInt(AWindowHandle);

      // Install hook if this is the first entry
      if Length(GHotkeyList) = 1 then
      begin
        TInterlocked.Exchange(GCurrentModifiers, 0);
        GKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);
        if GKeyboardHook = 0 then
        begin
          // Hook failed, remove the entry we just added
          SetLength(GHotkeyList, 0);
          LogInfo('Register: Failed to install low-level hook (Error=%d)', [GetLastError], ClassName);
          Exit;
        end;
      end;

      FHotkeyRegistered := True;
      FUsingLowLevelHook := True;
      LogInfo('Register: Installed low-level hook for "%s"', [AHotkey], ClassName);
    finally
      GHotkeyListLock.Leave;
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
var
  I: Integer;
  Found: Boolean;
begin
  if FHotkeyRegistered then
  begin
    if FUsingLowLevelHook then
    begin
      // Remove this hotkey from the global list
      GHotkeyListLock.Enter;
      try
        Found := False;
        // Find and remove our entry
        for I := 0 to High(GHotkeyList) do
        begin
          if GHotkeyList[I].InstanceId = FInstanceId then
          begin
            // Shift remaining entries down
            if I < High(GHotkeyList) then
              Move(GHotkeyList[I + 1], GHotkeyList[I], (Length(GHotkeyList) - I - 1) * SizeOf(THotkeyEntry));
            SetLength(GHotkeyList, Length(GHotkeyList) - 1);
            Found := True;
            Break;
          end;
        end;

        // Uninstall hook if list is now empty
        if Found and (Length(GHotkeyList) = 0) then
        begin
          if GKeyboardHook <> 0 then
          begin
            UnhookWindowsHookEx(GKeyboardHook);
            GKeyboardHook := 0;
            LogInfo('Unregister: Uninstalled low-level hook', ClassName);
          end;
        end
        else if Found then
          LogInfo('Unregister: Removed hotkey from list (%d remaining)', [Length(GHotkeyList)], ClassName);
      finally
        GHotkeyListLock.Leave;
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

function THotkeyManager.HandleHotkeyDetected(AWParam: WPARAM): Boolean;
begin
  Result := False;
  if FHotkeyRegistered and FUsingLowLevelHook then
  begin
    // Check if this message is for our instance
    if Integer(AWParam) = FInstanceId then
    begin
      LogDebug('HandleHotkeyDetected: Hotkey triggered via low-level hook', ClassName);
      DoHotkeyTriggered;
      Result := True;
    end;
  end;
end;

initialization
  GHotkeyListLock := TCriticalSection.Create;

finalization
  // Cleanup: unhook if still active and free critical section
  if GKeyboardHook <> 0 then
  begin
    UnhookWindowsHookEx(GKeyboardHook);
    GKeyboardHook := 0;
  end;
  SetLength(GHotkeyList, 0);
  FreeAndNil(GHotkeyListLock);

end.
