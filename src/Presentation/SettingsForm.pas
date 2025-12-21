unit SettingsForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  App.SettingsPresenter;

type
  /// <summary>
  /// Settings dialog form.
  /// Implements ISettingsView for MVP pattern.
  /// </summary>
  TFormSettings = class(TForm, ISettingsView)
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonApply: TButton;
    PanelContent: TPanel;
    LabelPlaceholder: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    FPresenter: TSettingsPresenter;

    { ISettingsView implementation }
    procedure CloseWithOK;
    procedure CloseWithCancel;
    procedure ShowError(const AMessage: string);
    procedure ShowInfo(const AMessage: string);

  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

uses
  App.Logger;

{$R *.dfm}

{ TFormSettings }

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  Log('[SettingsForm] FormCreate');
  FPresenter := TSettingsPresenter.Create(Self);
end;

procedure TFormSettings.FormDestroy(Sender: TObject);
begin
  Log('[SettingsForm] FormDestroy');
  FPresenter.Free;
end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Log('[SettingsForm] FormShow');
  FPresenter.LoadSettings;
end;

procedure TFormSettings.ButtonOKClick(Sender: TObject);
begin
  FPresenter.OnOKClicked;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
  FPresenter.OnCancelClicked;
end;

procedure TFormSettings.ButtonApplyClick(Sender: TObject);
begin
  FPresenter.OnApplyClicked;
end;

{ ISettingsView implementation }

procedure TFormSettings.CloseWithOK;
begin
  ModalResult := mrOk;
end;

procedure TFormSettings.CloseWithCancel;
begin
  ModalResult := mrCancel;
end;

procedure TFormSettings.ShowError(const AMessage: string);
begin
  MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure TFormSettings.ShowInfo(const AMessage: string);
begin
  MessageDlg(AMessage, mtInformation, [mbOK], 0);
end;

end.
