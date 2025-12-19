program bqc;

uses
  Vcl.Forms,
  MainForm in 'src\Presentation\MainForm.pas' {FormMain},
  Bluetooth.WinAPI in 'src\Infrastructure\Bluetooth.WinAPI.pas',
  Bluetooth.Types in 'src\Domain\Bluetooth.Types.pas',
  Bluetooth.Interfaces in 'src\Domain\Bluetooth.Interfaces.pas',
  Bluetooth.ConnectionStrategies in 'src\Application\Bluetooth.ConnectionStrategies.pas',
  Bluetooth.Service in 'src\Application\Bluetooth.Service.pas',
  UI.Theme in 'src\Presentation\UI.Theme.pas',
  UI.DeviceList in 'src\Presentation\UI.DeviceList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Bluetooth Quick Connect';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
