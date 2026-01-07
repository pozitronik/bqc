{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Device Context Menu Builder                     }
{                                                       }
{       EXTRACTED FROM: UI.DeviceList (god class)       }
{       Handles context menu building and actions       }
{       for device list items.                          }
{                                                       }
{*******************************************************}

unit UI.DeviceContextMenuBuilder;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Menus,
  Vcl.Controls,
  App.DeviceDisplayTypes;

type
  /// <summary>
  /// Event type for device context menu actions.
  /// NOTE: This is also defined in UI.DeviceList.pas - Delphi treats them as compatible.
  /// </summary>
  TDeviceAddressEvent = procedure(Sender: TObject; ADeviceAddress: UInt64) of object;

  /// <summary>
  /// Builds and manages context menus for device list items.
  /// Extracted from TDeviceListBox god class.
  /// </summary>
  TDeviceContextMenuBuilder = class
  private
    FPopupMenu: TPopupMenu;
    FContextMenuDeviceAddress: UInt64;
    FOwner: TComponent;

    // Event callbacks to coordinator (TDeviceListBox)
    FOnPinToggle: TDeviceAddressEvent;
    FOnCopyName: TDeviceAddressEvent;
    FOnCopyAddress: TDeviceAddressEvent;
    FOnConfigure: TDeviceAddressEvent;
    FOnUnpair: TDeviceAddressEvent;
    FOnHide: TDeviceAddressEvent;

    procedure BuildContextMenu(const AItem: TDeviceDisplayItem);
    procedure HandleMenuPinToggle(Sender: TObject);
    procedure HandleMenuCopyName(Sender: TObject);
    procedure HandleMenuCopyAddress(Sender: TObject);
    procedure HandleMenuConfigure(Sender: TObject);
    procedure HandleMenuUnpair(Sender: TObject);
    procedure HandleMenuHide(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    /// <summary>
    /// Shows context menu for a device item at specified screen coordinates.
    /// </summary>
    /// <param name="AItem">Display item to show menu for.</param>
    /// <param name="AScreenX">Screen X coordinate.</param>
    /// <param name="AScreenY">Screen Y coordinate.</param>
    procedure ShowMenuForItem(const AItem: TDeviceDisplayItem; AScreenX, AScreenY: Integer);

    // Event properties
    property OnPinToggle: TDeviceAddressEvent read FOnPinToggle write FOnPinToggle;
    property OnCopyName: TDeviceAddressEvent read FOnCopyName write FOnCopyName;
    property OnCopyAddress: TDeviceAddressEvent read FOnCopyAddress write FOnCopyAddress;
    property OnConfigure: TDeviceAddressEvent read FOnConfigure write FOnConfigure;
    property OnUnpair: TDeviceAddressEvent read FOnUnpair write FOnUnpair;
    property OnHide: TDeviceAddressEvent read FOnHide write FOnHide;
  end;

implementation

{ TDeviceContextMenuBuilder }

constructor TDeviceContextMenuBuilder.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FPopupMenu := TPopupMenu.Create(AOwner);
  FContextMenuDeviceAddress := 0;
end;

destructor TDeviceContextMenuBuilder.Destroy;
begin
  FPopupMenu.Free;
  inherited;
end;

procedure TDeviceContextMenuBuilder.BuildContextMenu(const AItem: TDeviceDisplayItem);
var
  MenuItem: TMenuItem;
begin
  FPopupMenu.Items.Clear;

  // Store device address for menu handlers
  FContextMenuDeviceAddress := AItem.Device.AddressInt;

  // For paired devices
  if AItem.Device.IsPaired then
  begin
    // Pin/Unpin toggle
    MenuItem := TMenuItem.Create(FPopupMenu);
    if AItem.IsPinned then
      MenuItem.Caption := 'Unpin'
    else
      MenuItem.Caption := 'Pin';
    MenuItem.OnClick := HandleMenuPinToggle;
    FPopupMenu.Items.Add(MenuItem);

    // Hide
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Hide from List';
    MenuItem.OnClick := HandleMenuHide;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Copy Name
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Name';
    MenuItem.OnClick := HandleMenuCopyName;
    FPopupMenu.Items.Add(MenuItem);

    // Copy Address
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Address';
    MenuItem.OnClick := HandleMenuCopyAddress;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Configure
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Configure...';
    MenuItem.OnClick := HandleMenuConfigure;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Unpair Device
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Unpair Device';
    MenuItem.OnClick := HandleMenuUnpair;
    FPopupMenu.Items.Add(MenuItem);
  end
  else
  begin
    // For unpaired devices: Copy commands + Hide
    // Copy Name
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Name';
    MenuItem.OnClick := HandleMenuCopyName;
    FPopupMenu.Items.Add(MenuItem);

    // Copy Address
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Copy Address';
    MenuItem.OnClick := HandleMenuCopyAddress;
    FPopupMenu.Items.Add(MenuItem);

    // Separator
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);

    // Hide
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := 'Hide from List';
    MenuItem.OnClick := HandleMenuHide;
    FPopupMenu.Items.Add(MenuItem);
  end;
end;

procedure TDeviceContextMenuBuilder.ShowMenuForItem(const AItem: TDeviceDisplayItem;
  AScreenX, AScreenY: Integer);
begin
  BuildContextMenu(AItem);
  FPopupMenu.Popup(AScreenX, AScreenY);
end;

procedure TDeviceContextMenuBuilder.HandleMenuPinToggle(Sender: TObject);
begin
  if Assigned(FOnPinToggle) and (FContextMenuDeviceAddress <> 0) then
    FOnPinToggle(FOwner, FContextMenuDeviceAddress);
end;

procedure TDeviceContextMenuBuilder.HandleMenuCopyName(Sender: TObject);
begin
  if Assigned(FOnCopyName) and (FContextMenuDeviceAddress <> 0) then
    FOnCopyName(FOwner, FContextMenuDeviceAddress);
end;

procedure TDeviceContextMenuBuilder.HandleMenuCopyAddress(Sender: TObject);
begin
  if Assigned(FOnCopyAddress) and (FContextMenuDeviceAddress <> 0) then
    FOnCopyAddress(FOwner, FContextMenuDeviceAddress);
end;

procedure TDeviceContextMenuBuilder.HandleMenuConfigure(Sender: TObject);
begin
  if Assigned(FOnConfigure) and (FContextMenuDeviceAddress <> 0) then
    FOnConfigure(FOwner, FContextMenuDeviceAddress);
end;

procedure TDeviceContextMenuBuilder.HandleMenuUnpair(Sender: TObject);
begin
  if Assigned(FOnUnpair) and (FContextMenuDeviceAddress <> 0) then
    FOnUnpair(FOwner, FContextMenuDeviceAddress);
end;

procedure TDeviceContextMenuBuilder.HandleMenuHide(Sender: TObject);
begin
  if Assigned(FOnHide) and (FContextMenuDeviceAddress <> 0) then
    FOnHide(FOwner, FContextMenuDeviceAddress);
end;

end.
