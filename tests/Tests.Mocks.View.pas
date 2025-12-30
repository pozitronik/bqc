{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations - Views                    }
{                                                       }
{*******************************************************}

unit Tests.Mocks.View;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  Bluetooth.Types,
  App.MainViewInterfaces,
  App.SettingsPresenter,
  App.DeviceDisplayTypes;

type
  /// <summary>
  /// Mock implementation of ISettingsView for testing the SettingsPresenter.
  /// </summary>
  TMockSettingsView = class(TInterfacedObject,
    ISettingsDialogView,
    IGeneralSettingsView,
    IHotkeySettingsView,
    IAppearanceSettingsView,
    ILayoutSettingsView,
    IConnectionSettingsView,
    ILoggingSettingsView,
    IDeviceSettingsView,
    IBatteryTraySettingsView,
    IProfileSettingsView)
  private
    FCloseWithOKCalled: Boolean;
    FCloseWithCancelCalled: Boolean;
    FLastErrorMessage: string;
    FLastInfoMessage: string;
    FApplyEnabled: Boolean;

    // General settings
    FGeneralSettings: TGeneralViewSettings;
    FSetGeneralCount: Integer;

    // Hotkey settings
    FHotkeySettings: THotkeyViewSettings;
    FSetHotkeyCount: Integer;

    // Appearance settings
    FAppearanceSettings: TAppearanceViewSettings;
    FSetAppearanceCount: Integer;
    FCurrentTheme: string;

    // Layout settings
    FLayoutSettings: TLayoutViewSettings;
    FSetLayoutCount: Integer;

    // Connection settings
    FConnectionSettings: TConnectionViewSettings;
    FSetConnectionCount: Integer;

    // Logging settings
    FLoggingSettings: TLoggingViewSettings;
    FSetLoggingCount: Integer;

    // Battery tray settings
    FBatteryTraySettings: TBatteryTrayViewSettings;
    FSetBatteryTrayCount: Integer;

    // Profile settings
    FProfileSettings: TProfileViewSettings;
    FSetProfileCount: Integer;

    // Device settings
    FDeviceListItems: TArray<string>;
    FSelectedDeviceIndex: Integer;
    FDeviceSettings: TDeviceViewSettings;
    FSetDeviceCount: Integer;
    FClearDeviceCount: Integer;
  public
    constructor Create;

    // ISettingsView (common)
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);
    procedure SetApplyEnabled(AEnabled: Boolean);

    // IGeneralSettingsView
    function GetGeneralSettings: TGeneralViewSettings;
    procedure SetGeneralSettings(const ASettings: TGeneralViewSettings);

    // IHotkeySettingsView
    function GetHotkeySettings: THotkeyViewSettings;
    procedure SetHotkeySettings(const ASettings: THotkeyViewSettings);

    // IAppearanceSettingsView
    function GetAppearanceSettings: TAppearanceViewSettings;
    procedure SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
    procedure PopulateThemeList(const ACurrentTheme: string);

    // ILayoutSettingsView
    function GetLayoutSettings: TLayoutViewSettings;
    procedure SetLayoutSettings(const ASettings: TLayoutViewSettings);

    // IConnectionSettingsView
    function GetConnectionSettings: TConnectionViewSettings;
    procedure SetConnectionSettings(const ASettings: TConnectionViewSettings);

    // ILoggingSettingsView
    function GetLoggingSettings: TLoggingViewSettings;
    procedure SetLoggingSettings(const ASettings: TLoggingViewSettings);

    // IBatteryTraySettingsView
    function GetBatteryTraySettings: TBatteryTrayViewSettings;
    procedure SetBatteryTraySettings(const ASettings: TBatteryTrayViewSettings);

    // IProfileSettingsView
    function GetProfileSettings: TProfileViewSettings;
    procedure SetProfileSettings(const ASettings: TProfileViewSettings);

    // IDeviceSettingsView
    procedure PopulateDeviceList(const AItems: TArray<string>);
    function GetSelectedDeviceIndex: Integer;
    procedure SetSelectedDeviceIndex(AIndex: Integer);
    function GetDeviceSettings: TDeviceViewSettings;
    procedure SetDeviceSettings(const ASettings: TDeviceViewSettings);
    procedure ClearDeviceSettings;

    // Test verification properties
    property CloseWithOKCalled: Boolean read FCloseWithOKCalled;
    property CloseWithCancelCalled: Boolean read FCloseWithCancelCalled;
    property LastErrorMessage: string read FLastErrorMessage;
    property LastInfoMessage: string read FLastInfoMessage;
    property ApplyEnabled: Boolean read FApplyEnabled;

    // General
    property GeneralSettings: TGeneralViewSettings read FGeneralSettings write FGeneralSettings;
    property SetGeneralCount: Integer read FSetGeneralCount;

    // Hotkey
    property HotkeySettings: THotkeyViewSettings read FHotkeySettings write FHotkeySettings;
    property SetHotkeyCount: Integer read FSetHotkeyCount;

    // Appearance
    property AppearanceSettings: TAppearanceViewSettings read FAppearanceSettings write FAppearanceSettings;
    property SetAppearanceCount: Integer read FSetAppearanceCount;
    property CurrentTheme: string read FCurrentTheme;

    // Layout
    property LayoutSettings: TLayoutViewSettings read FLayoutSettings write FLayoutSettings;
    property SetLayoutCount: Integer read FSetLayoutCount;

    // Connection
    property ConnectionSettings: TConnectionViewSettings read FConnectionSettings write FConnectionSettings;
    property SetConnectionCount: Integer read FSetConnectionCount;

    // Logging
    property LoggingSettings: TLoggingViewSettings read FLoggingSettings write FLoggingSettings;
    property SetLoggingCount: Integer read FSetLoggingCount;

    // Battery tray
    property BatteryTraySettings: TBatteryTrayViewSettings read FBatteryTraySettings write FBatteryTraySettings;
    property SetBatteryTrayCount: Integer read FSetBatteryTrayCount;

    // Profile
    property ProfileSettings: TProfileViewSettings read FProfileSettings write FProfileSettings;
    property SetProfileCount: Integer read FSetProfileCount;

    // Device
    property DeviceListItems: TArray<string> read FDeviceListItems;
    property SelectedDeviceIndex: Integer read FSelectedDeviceIndex write FSelectedDeviceIndex;
    property DeviceSettings: TDeviceViewSettings read FDeviceSettings write FDeviceSettings;
    property SetDeviceCount: Integer read FSetDeviceCount;
    property ClearDeviceCount: Integer read FClearDeviceCount;
  end;

  /// <summary>
  /// Mock implementation of IMainView for testing the MainPresenter.
  /// Implements all main view interfaces (device list, status, visibility, toggle).
  /// </summary>
  TMockMainView = class(TInterfacedObject,
    IDeviceListView,
    IStatusView,
    IVisibilityView,
    IToggleView)
  private
    FDisplayItems: TDeviceDisplayItemArray;
    FLastStatus: string;
    FVisible: Boolean;
    FMinimized: Boolean;
    FBusy: Boolean;
    FToggleState: Boolean;
    FToggleEnabled: Boolean;
    FLastNotificationTitle: string;
    FLastNotificationMessage: string;
    FLastNotificationFlags: TNotificationFlags;

    // Call counts for verification
    FShowDisplayItemsCount: Integer;
    FUpdateDisplayItemCount: Integer;
    FLastUpdatedDisplayItem: TDeviceDisplayItem;
    FShowStatusCount: Integer;
    FShowNotificationCount: Integer;
    FShowViewCalled: Boolean;
    FHideViewCalled: Boolean;
    FClearDevicesCalled: Boolean;
    FForceCloseCalled: Boolean;

    function GetShowDisplayItemsCalled: Boolean;
    procedure SetShowDisplayItemsCalled(AValue: Boolean);
  public
    constructor Create;

    // IDeviceListView
    procedure ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
    procedure UpdateDisplayItem(const AItem: TDeviceDisplayItem);
    procedure ClearDevices;

    // IToggleView
    procedure SetToggleState(AEnabled: Boolean);
    procedure SetToggleEnabled(AEnabled: Boolean);

    // IStatusView
    procedure ShowStatus(const AMessage: string);
    procedure ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);
    procedure SetBusy(ABusy: Boolean);

    // IVisibilityView
    function IsVisible: Boolean;
    function IsMinimized: Boolean;
    function GetWindowHandle: HWND;
    procedure ShowView;
    procedure HideView;
    procedure ForceClose;

    // Test verification properties
    property DisplayItems: TDeviceDisplayItemArray read FDisplayItems;
    property LastStatus: string read FLastStatus;
    property Visible: Boolean read FVisible write FVisible;
    property Minimized: Boolean read FMinimized write FMinimized;
    property Busy: Boolean read FBusy;
    property ToggleState: Boolean read FToggleState;
    property ToggleEnabled: Boolean read FToggleEnabled;
    property LastNotificationTitle: string read FLastNotificationTitle;
    property LastNotificationMessage: string read FLastNotificationMessage;
    property LastNotificationFlags: TNotificationFlags read FLastNotificationFlags;

    // Call counts
    property ShowDisplayItemsCount: Integer read FShowDisplayItemsCount write FShowDisplayItemsCount;
    property UpdateDisplayItemCount: Integer read FUpdateDisplayItemCount write FUpdateDisplayItemCount;
    property LastUpdatedDisplayItem: TDeviceDisplayItem read FLastUpdatedDisplayItem;
    property ShowStatusCount: Integer read FShowStatusCount;
    property ShowNotificationCount: Integer read FShowNotificationCount;
    property ShowViewCalled: Boolean read FShowViewCalled;
    property HideViewCalled: Boolean read FHideViewCalled;
    property ClearDevicesCalled: Boolean read FClearDevicesCalled;
    property ForceCloseCalled: Boolean read FForceCloseCalled;

    // Helper properties for test assertions
    property ShowDisplayItemsCalled: Boolean read GetShowDisplayItemsCalled write SetShowDisplayItemsCalled;
  end;

implementation

{ TMockSettingsView }

constructor TMockSettingsView.Create;
begin
  inherited Create;
  FSelectedDeviceIndex := -1;
  FCloseWithOKCalled := False;
  FCloseWithCancelCalled := False;
  FApplyEnabled := False;
  FSetGeneralCount := 0;
  FSetHotkeyCount := 0;
  FSetAppearanceCount := 0;
  FSetLayoutCount := 0;
  FSetConnectionCount := 0;
  FSetLoggingCount := 0;
  FSetDeviceCount := 0;
  FClearDeviceCount := 0;
end;

procedure TMockSettingsView.CloseWithOK;
begin
  FCloseWithOKCalled := True;
end;

procedure TMockSettingsView.CloseWithCancel;
begin
  FCloseWithCancelCalled := True;
end;

procedure TMockSettingsView.ShowError(const AMessage: string);
begin
  FLastErrorMessage := AMessage;
end;

procedure TMockSettingsView.ShowInfo(const AMessage: string);
begin
  FLastInfoMessage := AMessage;
end;

procedure TMockSettingsView.SetApplyEnabled(AEnabled: Boolean);
begin
  FApplyEnabled := AEnabled;
end;

function TMockSettingsView.GetGeneralSettings: TGeneralViewSettings;
begin
  Result := FGeneralSettings;
end;

procedure TMockSettingsView.SetGeneralSettings(const ASettings: TGeneralViewSettings);
begin
  FGeneralSettings := ASettings;
  Inc(FSetGeneralCount);
end;

function TMockSettingsView.GetHotkeySettings: THotkeyViewSettings;
begin
  Result := FHotkeySettings;
end;

procedure TMockSettingsView.SetHotkeySettings(const ASettings: THotkeyViewSettings);
begin
  FHotkeySettings := ASettings;
  Inc(FSetHotkeyCount);
end;

function TMockSettingsView.GetAppearanceSettings: TAppearanceViewSettings;
begin
  Result := FAppearanceSettings;
end;

procedure TMockSettingsView.SetAppearanceSettings(const ASettings: TAppearanceViewSettings);
begin
  FAppearanceSettings := ASettings;
  Inc(FSetAppearanceCount);
end;

function TMockSettingsView.GetLayoutSettings: TLayoutViewSettings;
begin
  Result := FLayoutSettings;
end;

procedure TMockSettingsView.SetLayoutSettings(const ASettings: TLayoutViewSettings);
begin
  FLayoutSettings := ASettings;
  Inc(FSetLayoutCount);
end;

function TMockSettingsView.GetConnectionSettings: TConnectionViewSettings;
begin
  Result := FConnectionSettings;
end;

procedure TMockSettingsView.SetConnectionSettings(const ASettings: TConnectionViewSettings);
begin
  FConnectionSettings := ASettings;
  Inc(FSetConnectionCount);
end;

function TMockSettingsView.GetLoggingSettings: TLoggingViewSettings;
begin
  Result := FLoggingSettings;
end;

procedure TMockSettingsView.SetLoggingSettings(const ASettings: TLoggingViewSettings);
begin
  FLoggingSettings := ASettings;
  Inc(FSetLoggingCount);
end;

function TMockSettingsView.GetBatteryTraySettings: TBatteryTrayViewSettings;
begin
  Result := FBatteryTraySettings;
end;

procedure TMockSettingsView.SetBatteryTraySettings(const ASettings: TBatteryTrayViewSettings);
begin
  FBatteryTraySettings := ASettings;
  Inc(FSetBatteryTrayCount);
end;

function TMockSettingsView.GetProfileSettings: TProfileViewSettings;
begin
  Result := FProfileSettings;
end;

procedure TMockSettingsView.SetProfileSettings(const ASettings: TProfileViewSettings);
begin
  FProfileSettings := ASettings;
  Inc(FSetProfileCount);
end;

procedure TMockSettingsView.PopulateThemeList(const ACurrentTheme: string);
begin
  // Mock doesn't fetch actual themes - just stores the current theme
  FCurrentTheme := ACurrentTheme;
end;

procedure TMockSettingsView.PopulateDeviceList(const AItems: TArray<string>);
begin
  FDeviceListItems := AItems;
end;

function TMockSettingsView.GetSelectedDeviceIndex: Integer;
begin
  Result := FSelectedDeviceIndex;
end;

procedure TMockSettingsView.SetSelectedDeviceIndex(AIndex: Integer);
begin
  FSelectedDeviceIndex := AIndex;
end;

function TMockSettingsView.GetDeviceSettings: TDeviceViewSettings;
begin
  Result := FDeviceSettings;
end;

procedure TMockSettingsView.SetDeviceSettings(const ASettings: TDeviceViewSettings);
begin
  FDeviceSettings := ASettings;
  Inc(FSetDeviceCount);
end;

procedure TMockSettingsView.ClearDeviceSettings;
begin
  FillChar(FDeviceSettings, SizeOf(FDeviceSettings), 0);
  Inc(FClearDeviceCount);
end;

{ TMockMainView }

constructor TMockMainView.Create;
begin
  inherited Create;
  FToggleState := False;
  FToggleEnabled := True;
  FBusy := False;
  FVisible := True;
  FMinimized := False;
  FForceCloseCalled := False;
  FShowViewCalled := False;
  FHideViewCalled := False;
  FClearDevicesCalled := False;
  FShowDisplayItemsCount := 0;
  FUpdateDisplayItemCount := 0;
  FShowStatusCount := 0;
  FShowNotificationCount := 0;
end;

function TMockMainView.GetShowDisplayItemsCalled: Boolean;
begin
  Result := FShowDisplayItemsCount > 0;
end;

procedure TMockMainView.SetShowDisplayItemsCalled(AValue: Boolean);
begin
  if AValue then
    Inc(FShowDisplayItemsCount)
  else
    FShowDisplayItemsCount := 0;
end;

procedure TMockMainView.ShowDisplayItems(const AItems: TDeviceDisplayItemArray);
begin
  FDisplayItems := AItems;
  Inc(FShowDisplayItemsCount);
end;

procedure TMockMainView.UpdateDisplayItem(const AItem: TDeviceDisplayItem);
var
  I: Integer;
begin
  Inc(FUpdateDisplayItemCount);
  FLastUpdatedDisplayItem := AItem;
  for I := 0 to High(FDisplayItems) do
    if FDisplayItems[I].Device.AddressInt = AItem.Device.AddressInt then
    begin
      FDisplayItems[I] := AItem;
      Exit;
    end;
end;

procedure TMockMainView.ClearDevices;
begin
  SetLength(FDisplayItems, 0);
  FClearDevicesCalled := True;
end;

procedure TMockMainView.SetToggleState(AEnabled: Boolean);
begin
  FToggleState := AEnabled;
end;

procedure TMockMainView.SetToggleEnabled(AEnabled: Boolean);
begin
  FToggleEnabled := AEnabled;
end;

procedure TMockMainView.ShowStatus(const AMessage: string);
begin
  FLastStatus := AMessage;
  Inc(FShowStatusCount);
end;

procedure TMockMainView.ShowNotification(const ATitle, AMessage: string; AFlags: TNotificationFlags);
begin
  FLastNotificationTitle := ATitle;
  FLastNotificationMessage := AMessage;
  FLastNotificationFlags := AFlags;
  Inc(FShowNotificationCount);
end;

procedure TMockMainView.SetBusy(ABusy: Boolean);
begin
  FBusy := ABusy;
end;

function TMockMainView.IsVisible: Boolean;
begin
  Result := FVisible;
end;

function TMockMainView.IsMinimized: Boolean;
begin
  Result := FMinimized;
end;

function TMockMainView.GetWindowHandle: HWND;
begin
  Result := 0;  // Mock returns 0
end;

procedure TMockMainView.ShowView;
begin
  FVisible := True;
  FMinimized := False;
  FShowViewCalled := True;
end;

procedure TMockMainView.HideView;
begin
  FVisible := False;
  FHideViewCalled := True;
end;

procedure TMockMainView.ForceClose;
begin
  FForceCloseCalled := True;
end;

end.
