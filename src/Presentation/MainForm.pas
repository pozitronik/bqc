{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Main Form (Device List View)                    }
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
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Bluetooth.Types,
  Bluetooth.Interfaces;

type
  /// <summary>
  /// Main application form displaying Bluetooth devices.
  /// </summary>
  TFormMain = class(TForm)
    lvDevices: TListView;
    pnlTop: TPanel;
    lblTitle: TLabel;
    btnRefresh: TButton;
    pnlStatus: TPanel;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure lvDevicesDblClick(Sender: TObject);
    procedure lvDevicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FBluetoothService: IBluetoothService;
    FDevices: TBluetoothDeviceInfoArray;

    procedure LoadDevices;
    procedure UpdateDeviceList;
    procedure UpdateStatusBar(const AMessage: string);

    { Event handlers for Bluetooth service }
    procedure HandleDeviceStateChanged(Sender: TObject;
      const ADevice: TBluetoothDeviceInfo);
    procedure HandleDeviceListChanged(Sender: TObject);
    procedure HandleError(Sender: TObject; const AMessage: string;
      AErrorCode: Cardinal);

    function FindDeviceIndex(AAddressInt: UInt64): Integer;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Bluetooth.Service;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Create Bluetooth service
  FBluetoothService := CreateBluetoothService;

  // Set up event handlers
  FBluetoothService.OnDeviceStateChanged := HandleDeviceStateChanged;
  FBluetoothService.OnDeviceListChanged := HandleDeviceListChanged;
  FBluetoothService.OnError := HandleError;

  // Configure ListView columns
  lvDevices.ViewStyle := vsReport;
  lvDevices.RowSelect := True;
  lvDevices.ReadOnly := True;
  lvDevices.HideSelection := False;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Device Name';
    Width := 200;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Type';
    Width := 100;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Status';
    Width := 100;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Address';
    Width := 140;
  end;

  // Check adapter and load devices
  if FBluetoothService.IsAdapterAvailable then
  begin
    UpdateStatusBar('Bluetooth adapter found. Loading devices...');
    LoadDevices;
  end
  else
  begin
    UpdateStatusBar('No Bluetooth adapter found!');
    btnRefresh.Enabled := False;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FBluetoothService := nil;
  FDevices := nil;
end;

procedure TFormMain.LoadDevices;
begin
  Screen.Cursor := crHourGlass;
  try
    FDevices := FBluetoothService.GetPairedDevices;
    UpdateDeviceList;

    if Length(FDevices) = 0 then
      UpdateStatusBar('No paired devices found.')
    else
      UpdateStatusBar(Format('%d paired device(s) found.', [Length(FDevices)]));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.UpdateDeviceList;
var
  Device: TBluetoothDeviceInfo;
  Item: TListItem;
begin
  lvDevices.Items.BeginUpdate;
  try
    lvDevices.Items.Clear;

    for Device in FDevices do
    begin
      Item := lvDevices.Items.Add;
      Item.Caption := Device.Name;
      Item.SubItems.Add(Device.DeviceTypeText);
      Item.SubItems.Add(Device.ConnectionStateText);
      Item.SubItems.Add(Device.AddressString);
      Item.Data := Pointer(Device.AddressInt); // Store address for lookup
    end;
  finally
    lvDevices.Items.EndUpdate;
  end;
end;

procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  UpdateStatusBar('Refreshing device list...');
  LoadDevices;
end;

procedure TFormMain.lvDevicesDblClick(Sender: TObject);
var
  Device: TBluetoothDeviceInfo;
  DeviceIndex: Integer;
begin
  if lvDevices.Selected = nil then
    Exit;

  DeviceIndex := FindDeviceIndex(UInt64(lvDevices.Selected.Data));
  if DeviceIndex < 0 then
    Exit;

  Device := FDevices[DeviceIndex];

  if Device.IsConnected then
    UpdateStatusBar(Format('Disconnecting from %s...', [Device.Name]))
  else
    UpdateStatusBar(Format('Connecting to %s...', [Device.Name]));

  // Perform connection toggle in a thread to not block UI
  TThread.CreateAnonymousThread(
    procedure
    var
      Success: Boolean;
    begin
      Success := FBluetoothService.ToggleConnection(Device);

      TThread.Synchronize(nil,
        procedure
        begin
          if Success then
          begin
            // Refresh the device to get updated state
            LoadDevices;
          end;
        end
      );
    end
  ).Start;
end;

procedure TFormMain.lvDevicesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  DeviceIndex: Integer;
  Device: TBluetoothDeviceInfo;
begin
  if Selected and (Item <> nil) then
  begin
    DeviceIndex := FindDeviceIndex(UInt64(Item.Data));
    if DeviceIndex >= 0 then
    begin
      Device := FDevices[DeviceIndex];
      if Device.IsConnected then
        UpdateStatusBar(Format('%s - Double-click to disconnect', [Device.Name]))
      else
        UpdateStatusBar(Format('%s - Double-click to connect', [Device.Name]));
    end;
  end;
end;

procedure TFormMain.UpdateStatusBar(const AMessage: string);
begin
  lblStatus.Caption := AMessage;
end;

procedure TFormMain.HandleDeviceStateChanged(Sender: TObject;
  const ADevice: TBluetoothDeviceInfo);
var
  I: Integer;
begin
  // Update the device in our cache
  for I := 0 to High(FDevices) do
  begin
    if FDevices[I].AddressInt = ADevice.AddressInt then
    begin
      FDevices[I] := ADevice;
      Break;
    end;
  end;

  // Update the list view item
  TThread.Queue(nil,
    procedure
    var
      ItemIndex: Integer;
    begin
      for ItemIndex := 0 to lvDevices.Items.Count - 1 do
      begin
        if UInt64(lvDevices.Items[ItemIndex].Data) = ADevice.AddressInt then
        begin
          lvDevices.Items[ItemIndex].SubItems[1] := ADevice.ConnectionStateText;
          Break;
        end;
      end;

      UpdateStatusBar(Format('%s: %s', [ADevice.Name, ADevice.ConnectionStateText]));
    end
  );
end;

procedure TFormMain.HandleDeviceListChanged(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      UpdateDeviceList;
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
        UpdateStatusBar(Format('Error: %s (Code: %d)', [AMessage, AErrorCode]))
      else
        UpdateStatusBar(Format('Error: %s', [AMessage]));
    end
  );
end;

function TFormMain.FindDeviceIndex(AAddressInt: UInt64): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FDevices) do
    if FDevices[I].AddressInt = AAddressInt then
      Exit(I);
  Result := -1;
end;

end.
