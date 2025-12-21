{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Settings Presenter (MVP Pattern)                }
{                                                       }
{       Copyright (c) 2024                              }
{                                                       }
{*******************************************************}

unit App.SettingsPresenter;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  /// Interface for the settings view (implemented by SettingsForm).
  /// </summary>
  ISettingsView = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
    /// <summary>
    /// Closes the dialog with OK result.
    /// </summary>
    procedure CloseWithOK;

    /// <summary>
    /// Closes the dialog with Cancel result.
    /// </summary>
    procedure CloseWithCancel;

    /// <summary>
    /// Shows an error message to the user.
    /// </summary>
    procedure ShowError(const AMessage: string);

    /// <summary>
    /// Shows an information message to the user.
    /// </summary>
    procedure ShowInfo(const AMessage: string);
  end;

  /// <summary>
  /// Presenter for the Settings dialog.
  /// Handles loading and saving configuration.
  /// </summary>
  TSettingsPresenter = class
  private
    FView: ISettingsView;
    FModified: Boolean;

  public
    /// <summary>
    /// Creates the settings presenter.
    /// </summary>
    /// <param name="AView">The view interface (implemented by SettingsForm).</param>
    constructor Create(AView: ISettingsView);

    /// <summary>
    /// Destroys the presenter.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    /// Loads current configuration into the view.
    /// Called when dialog is shown.
    /// </summary>
    procedure LoadSettings;

    /// <summary>
    /// Saves settings from the view to configuration.
    /// Called when user clicks OK.
    /// </summary>
    /// <returns>True if save was successful.</returns>
    function SaveSettings: Boolean;

    /// <summary>
    /// Called when user clicks OK button.
    /// Validates and saves settings.
    /// </summary>
    procedure OnOKClicked;

    /// <summary>
    /// Called when user clicks Cancel button.
    /// Discards changes and closes.
    /// </summary>
    procedure OnCancelClicked;

    /// <summary>
    /// Called when user clicks Apply button.
    /// Saves settings without closing.
    /// </summary>
    procedure OnApplyClicked;

    /// <summary>
    /// Marks settings as modified.
    /// </summary>
    procedure MarkModified;

    /// <summary>
    /// Returns True if settings have been modified.
    /// </summary>
    property IsModified: Boolean read FModified;
  end;

implementation

uses
  App.Logger,
  App.Config;

{ TSettingsPresenter }

constructor TSettingsPresenter.Create(AView: ISettingsView);
begin
  inherited Create;
  FView := AView;
  FModified := False;
  Log('[SettingsPresenter] Created');
end;

destructor TSettingsPresenter.Destroy;
begin
  Log('[SettingsPresenter] Destroyed');
  inherited;
end;

procedure TSettingsPresenter.LoadSettings;
begin
  Log('[SettingsPresenter] LoadSettings');
  // TODO: Load settings from Config into View controls
  // This will be implemented when we add controls to the form
  FModified := False;
end;

function TSettingsPresenter.SaveSettings: Boolean;
begin
  Log('[SettingsPresenter] SaveSettings');
  Result := True;
  try
    // TODO: Save settings from View controls to Config
    // This will be implemented when we add controls to the form

    // Save configuration to file
    Config.Save;
    FModified := False;
    Log('[SettingsPresenter] SaveSettings: Success');
  except
    on E: Exception do
    begin
      Log('[SettingsPresenter] SaveSettings: Error - %s', [E.Message]);
      FView.ShowError('Failed to save settings: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TSettingsPresenter.OnOKClicked;
begin
  Log('[SettingsPresenter] OnOKClicked');
  if SaveSettings then
    FView.CloseWithOK;
end;

procedure TSettingsPresenter.OnCancelClicked;
begin
  Log('[SettingsPresenter] OnCancelClicked');
  FView.CloseWithCancel;
end;

procedure TSettingsPresenter.OnApplyClicked;
begin
  Log('[SettingsPresenter] OnApplyClicked');
  if SaveSettings then
    FView.ShowInfo('Settings saved successfully.');
end;

procedure TSettingsPresenter.MarkModified;
begin
  FModified := True;
end;

end.
