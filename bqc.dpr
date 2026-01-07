program bqc;

uses
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  MainForm in 'src\Presentation\MainForm.pas' {FormMain},
  Bluetooth.WinAPI in 'src\Infrastructure\Bluetooth.WinAPI.pas',
  Bluetooth.Types in 'src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in 'src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in 'src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.PairingStrategies in 'src\Application\Bluetooth.PairingStrategies.pas',
  Bluetooth.PairingService in 'src\Application\Bluetooth.PairingService.pas',
  Bluetooth.EventDebouncer in 'src\Application\Bluetooth.EventDebouncer.pas',
  Bluetooth.Service in 'src\Application\Bluetooth.Service.pas',
  App.MainViewInterfaces in 'src\Application\App.MainViewInterfaces.pas',
  App.DeviceDisplayTypes in 'src\Application\App.DeviceDisplayTypes.pas',
  App.MainPresenter in 'src\Application\App.MainPresenter.pas',
  App.SettingsPresenter in 'src\Application\App.SettingsPresenter.pas',
  Bluetooth.RadioControl in 'src\Infrastructure\Bluetooth.RadioControl.pas',
  Bluetooth.DeviceWatcher in 'src\Infrastructure\Bluetooth.DeviceWatcher.pas',
  Bluetooth.DeviceMonitors in 'src\Infrastructure\Bluetooth.DeviceMonitors.pas',
  Bluetooth.WinRTDeviceQuery in 'src\Infrastructure\Bluetooth.WinRTDeviceQuery.pas',
  Bluetooth.DeviceRepository in 'src\Infrastructure\Bluetooth.DeviceRepository.pas',
  Bluetooth.ConnectionExecutor in 'src\Infrastructure\Bluetooth.ConnectionExecutor.pas',
  Bluetooth.AdapterQuery in 'src\Infrastructure\Bluetooth.AdapterQuery.pas',
  Bluetooth.DeviceConverter in 'src\Infrastructure\Bluetooth.DeviceConverter.pas',
  Bluetooth.BatteryQuery in 'src\Infrastructure\Bluetooth.BatteryQuery.pas',
  Bluetooth.ProfileQuery in 'src\Infrastructure\Bluetooth.ProfileQuery.pas',
  App.Logger in 'src\Infrastructure\App.Logger.pas',
  App.AsyncExecutor in 'src\Infrastructure\App.AsyncExecutor.pas',
  App.SystemClock in 'src\Infrastructure\App.SystemClock.pas',
  App.ConfigEnums in 'src\Infrastructure\App.ConfigEnums.pas',
  App.DeviceConfigTypes in 'src\Infrastructure\App.DeviceConfigTypes.pas',
  App.LayoutConfigIntf in 'src\Infrastructure\App.LayoutConfigIntf.pas',
  App.AppearanceConfigIntf in 'src\Infrastructure\App.AppearanceConfigIntf.pas',
  App.ConnectionConfigIntf in 'src\Infrastructure\App.ConnectionConfigIntf.pas',
  App.NotificationConfigIntf in 'src\Infrastructure\App.NotificationConfigIntf.pas',
  App.LogConfigIntf in 'src\Infrastructure\App.LogConfigIntf.pas',
  App.BatteryTrayConfigIntf in 'src\Infrastructure\App.BatteryTrayConfigIntf.pas',
  App.ProfileConfigIntf in 'src\Infrastructure\App.ProfileConfigIntf.pas',
  App.ConfigInterfaces in 'src\Infrastructure\App.ConfigInterfaces.pas',
  App.ConfigSectionTypes in 'src\Infrastructure\App.ConfigSectionTypes.pas',
  App.ConfigSection.General in 'src\Infrastructure\App.ConfigSection.General.pas',
  App.ConfigSection.Window in 'src\Infrastructure\App.ConfigSection.Window.pas',
  App.ConfigSection.Position in 'src\Infrastructure\App.ConfigSection.Position.pas',
  App.ConfigSection.Hotkey in 'src\Infrastructure\App.ConfigSection.Hotkey.pas',
  App.ConfigSection.Polling in 'src\Infrastructure\App.ConfigSection.Polling.pas',
  App.ConfigSection.Log in 'src\Infrastructure\App.ConfigSection.Log.pas',
  App.ConfigSection.Appearance in 'src\Infrastructure\App.ConfigSection.Appearance.pas',
  App.ConfigSection.Layout in 'src\Infrastructure\App.ConfigSection.Layout.pas',
  App.ConfigSection.Connection in 'src\Infrastructure\App.ConfigSection.Connection.pas',
  App.ConfigSection.Notification in 'src\Infrastructure\App.ConfigSection.Notification.pas',
  App.ConfigSection.BatteryTray in 'src\Infrastructure\App.ConfigSection.BatteryTray.pas',
  App.ConfigSection.Profile in 'src\Infrastructure\App.ConfigSection.Profile.pas',
  App.SystemThemeDetector in 'src\Infrastructure\App.SystemThemeDetector.pas',
  App.WinRTSupport in 'src\Infrastructure\App.WinRTSupport.pas',
  WinRT.AsyncHelpers in 'src\Infrastructure\WinRT.AsyncHelpers.pas',
  App.Config in 'src\Infrastructure\App.Config.pas',
  App.SettingsRepository in 'src\Infrastructure\App.SettingsRepository.pas',
  App.DeviceConfigRepository in 'src\Infrastructure\App.DeviceConfigRepository.pas',
  App.DeviceConfigProvider in 'src\Infrastructure\App.DeviceConfigProvider.pas',
  App.Bootstrap in 'src\Infrastructure\App.Bootstrap.pas',
  App.Autostart in 'src\Infrastructure\App.Autostart.pas',
  UI.Theme in 'src\Presentation\UI.Theme.pas',
  UI.DeviceList in 'src\Presentation\UI.DeviceList.pas',
  UI.ListGeometry in 'src\Presentation\UI.ListGeometry.pas',
  App.DeviceFormatter in 'src\Application\App.DeviceFormatter.pas',
  App.DeviceSorter in 'src\Application\App.DeviceSorter.pas',
  App.DeviceDisplayItemBuilder in 'src\Application\App.DeviceDisplayItemBuilder.pas',
  UI.WindowPositioner in 'src\Presentation\UI.WindowPositioner.pas',
  UI.TrayManager in 'src\Presentation\UI.TrayManager.pas',
  UI.HotkeyManager in 'src\Presentation\UI.HotkeyManager.pas',
  UI.BatteryIconRenderer in 'src\Presentation\UI.BatteryIconRenderer.pas',
  UI.BatteryTrayManager in 'src\Presentation\UI.BatteryTrayManager.pas',
  SettingsForm in 'src\Presentation\SettingsForm.pas' {FormSettings},
  HotkeyPickerForm in 'src\Presentation\HotkeyPickerForm.pas' {FormHotkeyPicker};


{$R *.res}

const
  MUTEX_NAME = 'BluetoothQuickConnect_SingleInstance_Mutex';

var
  Mutex: THandle;

begin
  // Single instance check using named mutex
  Mutex := CreateMutex(nil, True, MUTEX_NAME);
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    // Another instance is already running, exit silently
    CloseHandle(Mutex);
    Exit;
  end;

  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Bluetooth Quick Connect';

    Application.CreateForm(TFormMain, FormMain);
    Application.Run;
  finally
    CloseHandle(Mutex);
  end;
end.
