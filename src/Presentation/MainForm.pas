{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Form                                       }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.WinXCtrls,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  UI.Theme,
  UI.DeviceList;

type
  /// <summary>
  /// Main application form displaying Bluetooth devices.
  /// </summary>
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FBluetoothService: IBluetoothService;
    FDevices: TBluetoothDeviceInfoArray;

    // UI Components (created in code)
    FHeaderPanel: TPanel;
    FTitleLabel: TLabel;
    FBluetoothToggle: TToggleSwitch;
    FDeviceList: TDeviceListBox;
    FStatusPanel: TPanel;
    FStatusLabel: TLabel;
    FSettingsLink: TLabel;

    procedure CreateUIComponents;
    procedure ApplyTheme;
    procedure LoadDevices;
    procedure UpdateStatus(const AMessage: string);

    { Event handlers }
    procedure HandleDeviceClick(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceStateChanged(Sender: TObject; const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string; AErrorCode: Cardinal);
    procedure HandleThemeChanged(Sender: TObject);
    procedure HandleSettingsClick(Sender: TObject);
    procedure HandleBluetoothToggle(Sender: TObject);
    procedure HandleRefreshClick(Sender: TObject);

  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes,
  ShellAPI,
  Bluetooth.Service;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Set up form properties
  Caption := 'Bluetooth Quick Connect';
  Width := 380;
  Height := 500;
  Position := poScreenCenter;
  KeyPreview := True;

  // Subscribe to theme changes
  Theme.OnThemeChanged := HandleThemeChanged;

  // Create UI components
  CreateUIComponents;

  // Apply current theme
  ApplyTheme;

  // Create Bluetooth service
  FBluetoothService := CreateBluetoothService;
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;

  // Check adapter and load devices
  if FBluetoothService.IsAdapterAvailable then
  begin
    FBluetoothToggle.State := tssOn;
    UpdateStatus('Loading devices...');
    LoadDevices;
  end
  else
  begin
    FBluetoothToggle.State := tssOff;
    FBluetoothToggle.Enabled := False;
    UpdateStatus('No Bluetooth adapter found');
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Theme.OnThemeChanged := nil;
  FBluetoothService := nil;
  FDevices := nil;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_F5:
      HandleRefreshClick(nil);
  end;
end;

procedure TFormMain.CreateUIComponents;
begin
  // Header Panel
  FHeaderPanel := TPanel.Create(Self);
  FHeaderPanel.Parent := Self;
  FHeaderPanel.Align := alTop;
  FHeaderPanel.Height := 56;
  FHeaderPanel.BevelOuter := bvNone;
  FHeaderPanel.ParentBackground := False;

  // Title Label
  FTitleLabel := TLabel.Create(Self);
  FTitleLabel.Parent := FHeaderPanel;
  FTitleLabel.Left := 16;
  FTitleLabel.Top := 18;
  FTitleLabel.Caption := 'Bluetooth';
  FTitleLabel.Font.Size := 16;
  FTitleLabel.Font.Style := [fsBold];

  // Bluetooth Toggle Switch
  FBluetoothToggle := TToggleSwitch.Create(Self);
  FBluetoothToggle.Parent := FHeaderPanel;
  FBluetoothToggle.Left := FHeaderPanel.Width - 70;
  FBluetoothToggle.Top := 16;
  FBluetoothToggle.Width := 50;
  FBluetoothToggle.Anchors := [akTop, akRight];
  FBluetoothToggle.OnClick := HandleBluetoothToggle;

  // Device List
  FDeviceList := TDeviceListBox.Create(Self);
  FDeviceList.Parent := Self;
  FDeviceList.Left := 8;
  FDeviceList.Top := FHeaderPanel.Height + 8;
  FDeviceList.Width := ClientWidth - 16;
  FDeviceList.Height := ClientHeight - FDeviceList.Top - 70;
  FDeviceList.Anchors := [akLeft, akTop, akRight, akBottom];
  FDeviceList.OnDeviceClick := HandleDeviceClick;
  FDeviceList.TabOrder := 0;

  // Status Panel
  FStatusPanel := TPanel.Create(Self);
  FStatusPanel.Parent := Self;
  FStatusPanel.Align := alBottom;
  FStatusPanel.Height := 30;
  FStatusPanel.BevelOuter := bvNone;
  FStatusPanel.ParentBackground := False;

  // Status Label
  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := FStatusPanel;
  FStatusLabel.Left := 16;
  FStatusLabel.Top := 8;
  FStatusLabel.Caption := 'Ready';
  FStatusLabel.Font.Size := 8;

  // Settings Link
  FSettingsLink := TLabel.Create(Self);
  FSettingsLink.Parent := Self;
  FSettingsLink.Left := 16;
  FSettingsLink.Top := ClientHeight - 36;
  FSettingsLink.Anchors := [akLeft, akBottom];
  FSettingsLink.Caption := 'More Bluetooth settings';
  FSettingsLink.Font.Size := 9;
  FSettingsLink.Font.Style := [fsUnderline];
  FSettingsLink.Cursor := crHandPoint;
  FSettingsLink.OnClick := HandleSettingsClick;
end;

procedure TFormMain.ApplyTheme;
var
  Colors: TThemeColors;
begin
  Colors := Theme.Colors;

  // Form
  Color := Colors.Background;

  // Header
  FHeaderPanel.Color := Colors.Background;
  FTitleLabel.Font.Color := Colors.TextPrimary;

  // Status
  FStatusPanel.Color := Colors.Background;
  FStatusLabel.Font.Color := Colors.TextSecondary;

  // Settings link
  FSettingsLink.Font.Color := Colors.Accent;

  // Refresh device list to apply theme
  FDeviceList.Invalidate;
end;

procedure TFormMain.LoadDevices;
begin
  Screen.Cursor := crHourGlass;
  try
    FDevices := FBluetoothService.GetPairedDevices;
    FDeviceList.SetDevices(FDevices);

    if Length(FDevices) = 0 then
      UpdateStatus('No paired devices')
    else
      UpdateStatus(Format('%d device(s)', [Length(FDevices)]));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.UpdateStatus(const AMessage: string);
begin
  FStatusLabel.Caption := AMessage;
end;

procedure TFormMain.HandleDeviceClick(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  if ADevice.ConnectionState in [csConnecting, csDisconnecting] then
  begin
    UpdateStatus('Operation in progress...');
    Exit;
  end;

  if ADevice.IsConnected then
    UpdateStatus(Format('Disconnecting %s...', [ADevice.Name]))
  else
    UpdateStatus(Format('Connecting %s...', [ADevice.Name]));

  // Toggle connection in background thread
  TThread.CreateAnonymousThread(
    procedure
    begin
      FBluetoothService.ToggleConnection(ADevice);

      TThread.Queue(nil,
        procedure
        begin
          LoadDevices;
        end
      );
    end
  ).Start;
end;

procedure TFormMain.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
begin
  TThread.Queue(nil,
    procedure
    var
      I: Integer;
    begin
      // Update local cache
      for I := 0 to High(FDevices) do
      begin
        if FDevices[I].AddressInt = ADevice.AddressInt then
        begin
          FDevices[I] := ADevice;
          Break;
        end;
      end;

      // Update UI
      FDeviceList.UpdateDevice(ADevice);
      UpdateStatus(Format('%s: %s', [ADevice.Name, ADevice.ConnectionStateText]));
    end
  );
end;

procedure TFormMain.HandleDeviceListChanged(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      LoadDevices;
    end
  );
end;

procedure TFormMain.HandleError(Sender: TObject; const AMessage: string;
  AErrorCode: Cardinal);
begin
  TThread.Queue(nil,
    procedure
    begin
      if AErrorCode <> 0 then
        UpdateStatus(Format('Error: %s (%d)', [AMessage, AErrorCode]))
      else
        UpdateStatus(Format('Error: %s', [AMessage]));
    end
  );
end;

procedure TFormMain.HandleThemeChanged(Sender: TObject);
begin
  ApplyTheme;
end;

procedure TFormMain.HandleSettingsClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'ms-settings:bluetooth', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.HandleBluetoothToggle(Sender: TObject);
begin
  // Note: Actually enabling/disabling Bluetooth radio requires elevated privileges
  // and is complex. For now, just refresh the device list.
  if FBluetoothToggle.State = tssOn then
    LoadDevices
  else
    FDeviceList.Clear;
end;

procedure TFormMain.HandleRefreshClick(Sender: TObject);
begin
  UpdateStatus('Refreshing...');
  LoadDevices;
end;

end.
