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
  Bluetooth.Service in 'src\Application\Bluetooth.Service.pas',
  App.MainPresenter in 'src\Application\App.MainPresenter.pas',
  Bluetooth.RadioControl in 'src\Infrastructure\Bluetooth.RadioControl.pas',
  Bluetooth.DeviceWatcher in 'src\Infrastructure\Bluetooth.DeviceWatcher.pas',
  App.Logger in 'src\Infrastructure\App.Logger.pas',
  App.Config in 'src\Infrastructure\App.Config.pas',
  App.Autostart in 'src\Infrastructure\App.Autostart.pas',
  System.Permissions in 'src\Infrastructure\System.Permissions.pas',
  UI.Theme in 'src\Presentation\UI.Theme.pas',
  UI.DeviceList in 'src\Presentation\UI.DeviceList.pas',
  UI.WindowPositioner in 'src\Presentation\UI.WindowPositioner.pas',
  UI.TrayManager in 'src\Presentation\UI.TrayManager.pas',
  UI.HotkeyManager in 'src\Presentation\UI.HotkeyManager.pas';


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
