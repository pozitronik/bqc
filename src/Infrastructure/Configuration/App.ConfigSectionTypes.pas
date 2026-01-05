{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       Configuration Section Shared Types              }
{                                                       }
{*******************************************************}

/// <summary>
/// Shared types and base class for configuration section implementations.
/// </summary>
unit App.ConfigSectionTypes;

interface

type
  /// <summary>
  /// Callback type for modification notification.
  /// Called by config sections when any property changes.
  /// </summary>
  TModifiedNotifier = reference to procedure;

  /// <summary>
  /// Base class for all configuration sections.
  /// Provides common constructor pattern and setter helpers to eliminate boilerplate.
  /// Subclasses should call SetFieldXxx methods in their property setters.
  /// </summary>
  TConfigSectionBase = class(TInterfacedObject)
  protected
    FOnModified: TModifiedNotifier;

    /// <summary>
    /// Notifies that a property has been modified.
    /// Called automatically by SetFieldXxx helpers.
    /// </summary>
    procedure NotifyModified;

    /// <summary>
    /// Sets a Boolean field if value changed and notifies modification.
    /// </summary>
    procedure SetFieldBoolean(var AField: Boolean; AValue: Boolean);

    /// <summary>
    /// Sets an Integer field if value changed and notifies modification.
    /// </summary>
    procedure SetFieldInteger(var AField: Integer; AValue: Integer);

    /// <summary>
    /// Sets a string field if value changed and notifies modification.
    /// </summary>
    procedure SetFieldString(var AField: string; const AValue: string);
  public
    /// <summary>
    /// Creates the config section with modification notifier.
    /// Subclasses should call inherited and then SetDefaults.
    /// </summary>
    constructor Create(AOnModified: TModifiedNotifier);
  end;

implementation

{ TConfigSectionBase }

constructor TConfigSectionBase.Create(AOnModified: TModifiedNotifier);
begin
  inherited Create;
  FOnModified := AOnModified;
end;

procedure TConfigSectionBase.NotifyModified;
begin
  if Assigned(FOnModified) then
    FOnModified();
end;

procedure TConfigSectionBase.SetFieldBoolean(var AField: Boolean; AValue: Boolean);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    NotifyModified;
  end;
end;

procedure TConfigSectionBase.SetFieldInteger(var AField: Integer; AValue: Integer);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    NotifyModified;
  end;
end;

procedure TConfigSectionBase.SetFieldString(var AField: string; const AValue: string);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    NotifyModified;
  end;
end;

end.
