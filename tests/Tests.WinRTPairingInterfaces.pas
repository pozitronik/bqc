{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       WinRT Pairing Interfaces Unit Tests             }
{                                                       }
{*******************************************************}

unit Tests.WinRTPairingInterfaces;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.WinRTPairingInterfaces;

type
  /// <summary>
  /// Test fixture for GetPairingResultMessage function.
  /// Tests all 20 DevicePairingResultStatus values for proper error messages.
  /// </summary>
  [TestFixture]
  TGetPairingResultMessageTests = class
  public
    [Test]
    procedure Success_ReturnsSuccessMessage;
    [Test]
    procedure NotReadyToPair_ReturnsPairingModeMessage;
    [Test]
    procedure NotPaired_ReturnsFailureMessage;
    [Test]
    procedure AlreadyPaired_ReturnsAlreadyPairedMessage;
    [Test]
    procedure ConnectionRejected_ReturnsRejectionMessage;
    [Test]
    procedure TooManyConnections_ReturnsConnectionLimitMessage;
    [Test]
    procedure HardwareFailure_ReturnsHardwareMessage;
    [Test]
    procedure AuthenticationTimeout_ReturnsTimeoutMessage;
    [Test]
    procedure AuthenticationNotAllowed_ReturnsPolicyMessage;
    [Test]
    procedure AuthenticationFailure_ReturnsPINMessage;
    [Test]
    procedure NoSupportedProfiles_ReturnsCompatibilityMessage;
    [Test]
    procedure ProtectionLevelCouldNotBeMet_ReturnsSecurityMessage;
    [Test]
    procedure AccessDenied_ReturnsPermissionsMessage;
    [Test]
    procedure InvalidCeremonyData_ReturnsProtocolMessage;
    [Test]
    procedure PairingCanceled_ReturnsCancelledMessage;
    [Test]
    procedure OperationAlreadyInProgress_ReturnsInProgressMessage;
    [Test]
    procedure RequiredHandlerNotRegistered_ReturnsHandlerMessage;
    [Test]
    procedure RejectedByHandler_ReturnsRejectedMessage;
    [Test]
    procedure RemoteDeviceHasAssociation_ReturnsAssociationMessage;
    [Test]
    procedure Failed_ReturnsGenericFailureMessage;
    [Test]
    procedure InvalidStatus_ReturnsUnknownMessage;
  end;

  /// <summary>
  /// Test fixture for IntToPairingResultStatus function.
  /// Tests safe conversion from integer to enum with range checking.
  /// </summary>
  [TestFixture]
  TIntToPairingResultStatusTests = class
  public
    [Test]
    procedure ValidValue_Zero_ReturnsSuccess;
    [Test]
    procedure ValidValue_Nineteen_ReturnsFailed;
    [Test]
    procedure ValidValue_Seven_ReturnsAuthenticationTimeout;
    [Test]
    procedure InvalidValue_Negative_ReturnsFailed;
    [Test]
    procedure InvalidValue_TooLarge_ReturnsFailed;
  end;

  /// <summary>
  /// Test fixture for GetDevicePairingInterface function.
  /// Tests basic nil-safety of the helper function.
  /// </summary>
  [TestFixture]
  TGetDevicePairingInterfaceTests = class
  public
    [Test]
    procedure NilDeviceInfo_ReturnsNil;
  end;

implementation

{ TGetPairingResultMessageTests }

procedure TGetPairingResultMessageTests.Success_ReturnsSuccessMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsSuccess);
  Assert.AreEqual('Device paired successfully', Message);
end;

procedure TGetPairingResultMessageTests.NotReadyToPair_ReturnsPairingModeMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsNotReadyToPair);
  Assert.Contains(Message, 'not ready to pair');
  Assert.Contains(Message, 'pairing mode');
end;

procedure TGetPairingResultMessageTests.NotPaired_ReturnsFailureMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsNotPaired);
  Assert.Contains(Message, 'Pairing failed');
  Assert.Contains(Message, 'did not complete');
end;

procedure TGetPairingResultMessageTests.AlreadyPaired_ReturnsAlreadyPairedMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsAlreadyPaired);
  Assert.AreEqual('Device is already paired with this computer', Message);
end;

procedure TGetPairingResultMessageTests.ConnectionRejected_ReturnsRejectionMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsConnectionRejected);
  Assert.Contains(Message, 'rejected the connection');
  Assert.Contains(Message, 'manual acceptance');
end;

procedure TGetPairingResultMessageTests.TooManyConnections_ReturnsConnectionLimitMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsTooManyConnections);
  Assert.Contains(Message, 'too many active connections');
  Assert.Contains(Message, 'Disconnect device');
end;

procedure TGetPairingResultMessageTests.HardwareFailure_ReturnsHardwareMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsHardwareFailure);
  Assert.Contains(Message, 'Hardware failure');
  Assert.Contains(Message, 'Bluetooth adapter');
end;

procedure TGetPairingResultMessageTests.AuthenticationTimeout_ReturnsTimeoutMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsAuthenticationTimeout);
  Assert.Contains(Message, 'Authentication timed out');
  Assert.Contains(Message, 'enter it more quickly');
end;

procedure TGetPairingResultMessageTests.AuthenticationNotAllowed_ReturnsPolicyMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsAuthenticationNotAllowed);
  Assert.Contains(Message, 'Authentication not allowed');
  Assert.Contains(Message, 'security policy');
end;

procedure TGetPairingResultMessageTests.AuthenticationFailure_ReturnsPINMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsAuthenticationFailure);
  Assert.Contains(Message, 'Authentication failed');
  Assert.Contains(Message, 'PIN');
end;

procedure TGetPairingResultMessageTests.NoSupportedProfiles_ReturnsCompatibilityMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsNoSupportedProfiles);
  Assert.Contains(Message, 'no compatible Bluetooth profiles');
  Assert.Contains(Message, 'not be compatible with Windows');
end;

procedure TGetPairingResultMessageTests.ProtectionLevelCouldNotBeMet_ReturnsSecurityMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsProtectionLevelCouldNotBeMet);
  Assert.Contains(Message, 'Required security level');
  Assert.Contains(Message, 'encryption or authentication');
end;

procedure TGetPairingResultMessageTests.AccessDenied_ReturnsPermissionsMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsAccessDenied);
  Assert.Contains(Message, 'Access denied');
  Assert.Contains(Message, 'permissions');
end;

procedure TGetPairingResultMessageTests.InvalidCeremonyData_ReturnsProtocolMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsInvalidCeremonyData);
  Assert.Contains(Message, 'Invalid pairing data');
  Assert.Contains(Message, 'protocol error');
end;

procedure TGetPairingResultMessageTests.PairingCanceled_ReturnsCancelledMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsPairingCanceled);
  Assert.AreEqual('Pairing cancelled by user', Message);
end;

procedure TGetPairingResultMessageTests.OperationAlreadyInProgress_ReturnsInProgressMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsOperationAlreadyInProgress);
  Assert.Contains(Message, 'Another pairing operation');
  Assert.Contains(Message, 'already in progress');
end;

procedure TGetPairingResultMessageTests.RequiredHandlerNotRegistered_ReturnsHandlerMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsRequiredHandlerNotRegistered);
  Assert.Contains(Message, 'Required pairing handler');
  Assert.Contains(Message, 'not registered');
end;

procedure TGetPairingResultMessageTests.RejectedByHandler_ReturnsRejectedMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsRejectedByHandler);
  Assert.Contains(Message, 'Pairing rejected by application');
  Assert.Contains(Message, 'policy blocked');
end;

procedure TGetPairingResultMessageTests.RemoteDeviceHasAssociation_ReturnsAssociationMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsRemoteDeviceHasAssociation);
  Assert.Contains(Message, 'already paired with another computer');
  Assert.Contains(Message, 'Unpair device');
end;

procedure TGetPairingResultMessageTests.Failed_ReturnsGenericFailureMessage;
var
  Message: string;
begin
  Message := GetPairingResultMessage(dprsFailed);
  Assert.Contains(Message, 'Pairing failed');
  Assert.Contains(Message, 'pairing mode');
  Assert.Contains(Message, 'within range');
end;

procedure TGetPairingResultMessageTests.InvalidStatus_ReturnsUnknownMessage;
var
  Message: string;
  InvalidStatus: TDevicePairingResultStatus;
begin
  // Cast an out-of-range value
  InvalidStatus := TDevicePairingResultStatus(999);
  Message := GetPairingResultMessage(InvalidStatus);
  Assert.Contains(Message, 'Unknown pairing status');
  Assert.Contains(Message, '999');
end;

{ TIntToPairingResultStatusTests }

procedure TIntToPairingResultStatusTests.ValidValue_Zero_ReturnsSuccess;
var
  Status: TDevicePairingResultStatus;
begin
  Status := IntToPairingResultStatus(0);
  Assert.AreEqual(Ord(dprsSuccess), Ord(Status));
end;

procedure TIntToPairingResultStatusTests.ValidValue_Nineteen_ReturnsFailed;
var
  Status: TDevicePairingResultStatus;
begin
  Status := IntToPairingResultStatus(19);
  Assert.AreEqual(Ord(dprsFailed), Ord(Status));
end;

procedure TIntToPairingResultStatusTests.ValidValue_Seven_ReturnsAuthenticationTimeout;
var
  Status: TDevicePairingResultStatus;
begin
  Status := IntToPairingResultStatus(7);
  Assert.AreEqual(Ord(dprsAuthenticationTimeout), Ord(Status));
end;

procedure TIntToPairingResultStatusTests.InvalidValue_Negative_ReturnsFailed;
var
  Status: TDevicePairingResultStatus;
begin
  Status := IntToPairingResultStatus(-1);
  Assert.AreEqual(Ord(dprsFailed), Ord(Status));
end;

procedure TIntToPairingResultStatusTests.InvalidValue_TooLarge_ReturnsFailed;
var
  Status: TDevicePairingResultStatus;
begin
  Status := IntToPairingResultStatus(100);
  Assert.AreEqual(Ord(dprsFailed), Ord(Status));
end;

{ TGetDevicePairingInterfaceTests }

procedure TGetDevicePairingInterfaceTests.NilDeviceInfo_ReturnsNil;
var
  PairingIntf: IDeviceInformationPairing;
begin
  PairingIntf := GetDevicePairingInterface(nil);
  Assert.IsNull(PairingIntf);
end;

initialization
  TDUnitX.RegisterTestFixture(TGetPairingResultMessageTests);
  TDUnitX.RegisterTestFixture(TIntToPairingResultStatusTests);
  TDUnitX.RegisterTestFixture(TGetDevicePairingInterfaceTests);

end.
