program bqc;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  MainForm in 'src\Presentation\MainForm.pas' {FormMain},
  Bluetooth.WinAPI in 'src\Infrastructure\Bluetooth.WinAPI.pas',
  Bluetooth.Types in 'src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in 'src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in 'src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.Service in 'src\Application\Bluetooth.Service.pas',
  Bluetooth.RadioControl in 'src\Infrastructure\Bluetooth.RadioControl.pas',
  Bluetooth.DeviceWatcher in 'src\Infrastructure\Bluetooth.DeviceWatcher.pas',
  App.Logger in 'src\Infrastructure\App.Logger.pas',
  System.Permissions in 'src\Infrastructure\System.Permissions.pas',
  UI.Theme in 'src\Presentation\UI.Theme.pas',
  UI.DeviceList in 'src\Presentation\UI.DeviceList.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Bluetooth Quick Connect';

  // Apply VCL Style if available
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
