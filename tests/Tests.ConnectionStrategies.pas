{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Connection Strategy Unit Tests                  }
{                                                       }
{*******************************************************}

unit Tests.ConnectionStrategies;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ConnectionStrategies,
  Tests.Mocks;

type
  /// <summary>
  /// Test fixture for TAudioConnectionStrategy.
  /// </summary>
  [TestFixture]
  TAudioConnectionStrategyTests = class
  private
    FStrategy: IConnectionStrategy;
  public
    [Setup]
    procedure Setup;

    // CanHandle tests
    [Test]
    procedure CanHandle_AudioOutput_ReturnsTrue;
    [Test]
    procedure CanHandle_AudioInput_ReturnsTrue;
    [Test]
    procedure CanHandle_Headset_ReturnsTrue;
    [Test]
    procedure CanHandle_Keyboard_ReturnsFalse;
    [Test]
    procedure CanHandle_Mouse_ReturnsFalse;
    [Test]
    procedure CanHandle_Unknown_ReturnsFalse;

    // GetPriority tests
    [Test]
    procedure GetPriority_Returns100;

    // GetServiceGuids tests
    [Test]
    procedure GetServiceGuids_ReturnsNonEmpty;
    [Test]
    procedure GetServiceGuids_ContainsAudioSinkGuid;
  end;

  /// <summary>
  /// Test fixture for THIDConnectionStrategy.
  /// </summary>
  [TestFixture]
  THIDConnectionStrategyTests = class
  private
    FStrategy: IConnectionStrategy;
  public
    [Setup]
    procedure Setup;

    // CanHandle tests
    [Test]
    procedure CanHandle_Keyboard_ReturnsTrue;
    [Test]
    procedure CanHandle_Mouse_ReturnsTrue;
    [Test]
    procedure CanHandle_Gamepad_ReturnsTrue;
    [Test]
    procedure CanHandle_HID_ReturnsTrue;
    [Test]
    procedure CanHandle_AudioOutput_ReturnsFalse;
    [Test]
    procedure CanHandle_Headset_ReturnsFalse;

    // GetPriority tests
    [Test]
    procedure GetPriority_Returns100;

    // GetServiceGuids tests
    [Test]
    procedure GetServiceGuids_ReturnsNonEmpty;
    [Test]
    procedure GetServiceGuids_ContainsHIDGuid;
  end;

  /// <summary>
  /// Test fixture for TGenericConnectionStrategy.
  /// </summary>
  [TestFixture]
  TGenericConnectionStrategyTests = class
  private
    FStrategy: IConnectionStrategy;
  public
    [Setup]
    procedure Setup;

    // CanHandle tests - generic handles everything
    [Test]
    procedure CanHandle_AudioOutput_ReturnsTrue;
    [Test]
    procedure CanHandle_Keyboard_ReturnsTrue;
    [Test]
    procedure CanHandle_Unknown_ReturnsTrue;
    [Test]
    procedure CanHandle_Phone_ReturnsTrue;

    // GetPriority tests
    [Test]
    procedure GetPriority_ReturnsZero;

    // GetServiceGuids tests
    [Test]
    procedure GetServiceGuids_ReturnsNonEmpty;
  end;

  /// <summary>
  /// Test fixture for TConnectionStrategyFactory.
  /// </summary>
  [TestFixture]
  TConnectionStrategyFactoryTests = class
  private
    FFactory: IConnectionStrategyFactory;
  public
    [Setup]
    procedure Setup;

    // Default strategies tests
    [Test]
    procedure Create_RegistersDefaultStrategies;
    [Test]
    procedure CreateWithoutDefaults_HasNoStrategies;

    // GetStrategy tests
    [Test]
    procedure GetStrategy_AudioOutput_ReturnsAudioStrategy;
    [Test]
    procedure GetStrategy_Keyboard_ReturnsHIDStrategy;
    [Test]
    procedure GetStrategy_Unknown_ReturnsGenericStrategy;
    [Test]
    procedure GetStrategy_Phone_ReturnsGenericStrategy;

    // Priority selection tests
    [Test]
    procedure GetStrategy_SelectsHighestPriority;

    // RegisterStrategy tests
    [Test]
    procedure RegisterStrategy_AddsToList;
    [Test]
    procedure RegisterStrategy_CanOverridePriority;

    // GetAllStrategies tests
    [Test]
    procedure GetAllStrategies_ReturnsAllRegistered;

    // Clear tests
    [Test]
    procedure Clear_RemovesAllStrategies;
    [Test]
    procedure Clear_GetStrategyReturnsNil;
  end;

  /// <summary>
  /// Test fixture for mock connection strategy factory.
  /// </summary>
  [TestFixture]
  TMockConnectionStrategyFactoryTests = class
  private
    FFactory: TMockConnectionStrategyFactory;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetStrategy_IncrementsCallCount;
    [Test]
    procedure GetStrategy_RecordsLastDeviceType;
    [Test]
    procedure RegisterStrategy_AddsStrategy;
    [Test]
    procedure Clear_ResetsCallCount;
  end;

  /// <summary>
  /// Test fixture for mock connection strategy.
  /// </summary>
  [TestFixture]
  TMockConnectionStrategyTests = class
  private
    FStrategy: TMockConnectionStrategy;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CanHandle_DefaultsToTrue;
    [Test]
    procedure CanHandle_RespectsCanHandleResult;
    [Test]
    procedure CanHandle_WithSupportedTypes_ReturnsTrueForMatch;
    [Test]
    procedure CanHandle_WithSupportedTypes_ReturnsFalseForNonMatch;
    [Test]
    procedure GetPriority_ReturnsConfiguredPriority;
    [Test]
    procedure GetServiceGuids_ReturnsConfiguredGuids;
  end;

implementation

uses
  Bluetooth.WinAPI;

{ TAudioConnectionStrategyTests }

procedure TAudioConnectionStrategyTests.Setup;
begin
  FStrategy := TAudioConnectionStrategy.Create;
end;

procedure TAudioConnectionStrategyTests.CanHandle_AudioOutput_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btAudioOutput));
end;

procedure TAudioConnectionStrategyTests.CanHandle_AudioInput_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btAudioInput));
end;

procedure TAudioConnectionStrategyTests.CanHandle_Headset_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btHeadset));
end;

procedure TAudioConnectionStrategyTests.CanHandle_Keyboard_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(btKeyboard));
end;

procedure TAudioConnectionStrategyTests.CanHandle_Mouse_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(btMouse));
end;

procedure TAudioConnectionStrategyTests.CanHandle_Unknown_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(btUnknown));
end;

procedure TAudioConnectionStrategyTests.GetPriority_Returns100;
begin
  Assert.AreEqual(100, FStrategy.GetPriority);
end;

procedure TAudioConnectionStrategyTests.GetServiceGuids_ReturnsNonEmpty;
begin
  Assert.IsTrue(Length(FStrategy.GetServiceGuids) > 0);
end;

procedure TAudioConnectionStrategyTests.GetServiceGuids_ContainsAudioSinkGuid;
var
  Guids: TArray<TGUID>;
  G: TGUID;
  Found: Boolean;
begin
  Guids := FStrategy.GetServiceGuids;
  Found := False;
  for G in Guids do
    if G = AudioSinkServiceClass_UUID then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'Should contain AudioSinkServiceClass_UUID');
end;

{ THIDConnectionStrategyTests }

procedure THIDConnectionStrategyTests.Setup;
begin
  FStrategy := THIDConnectionStrategy.Create;
end;

procedure THIDConnectionStrategyTests.CanHandle_Keyboard_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btKeyboard));
end;

procedure THIDConnectionStrategyTests.CanHandle_Mouse_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btMouse));
end;

procedure THIDConnectionStrategyTests.CanHandle_Gamepad_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btGamepad));
end;

procedure THIDConnectionStrategyTests.CanHandle_HID_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btHID));
end;

procedure THIDConnectionStrategyTests.CanHandle_AudioOutput_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(btAudioOutput));
end;

procedure THIDConnectionStrategyTests.CanHandle_Headset_ReturnsFalse;
begin
  Assert.IsFalse(FStrategy.CanHandle(btHeadset));
end;

procedure THIDConnectionStrategyTests.GetPriority_Returns100;
begin
  Assert.AreEqual(100, FStrategy.GetPriority);
end;

procedure THIDConnectionStrategyTests.GetServiceGuids_ReturnsNonEmpty;
begin
  Assert.IsTrue(Length(FStrategy.GetServiceGuids) > 0);
end;

procedure THIDConnectionStrategyTests.GetServiceGuids_ContainsHIDGuid;
var
  Guids: TArray<TGUID>;
  G: TGUID;
  Found: Boolean;
begin
  Guids := FStrategy.GetServiceGuids;
  Found := False;
  for G in Guids do
    if G = HumanInterfaceDeviceServiceClass_UUID then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'Should contain HumanInterfaceDeviceServiceClass_UUID');
end;

{ TGenericConnectionStrategyTests }

procedure TGenericConnectionStrategyTests.Setup;
begin
  FStrategy := TGenericConnectionStrategy.Create;
end;

procedure TGenericConnectionStrategyTests.CanHandle_AudioOutput_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btAudioOutput));
end;

procedure TGenericConnectionStrategyTests.CanHandle_Keyboard_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btKeyboard));
end;

procedure TGenericConnectionStrategyTests.CanHandle_Unknown_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btUnknown));
end;

procedure TGenericConnectionStrategyTests.CanHandle_Phone_ReturnsTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btPhone));
end;

procedure TGenericConnectionStrategyTests.GetPriority_ReturnsZero;
begin
  Assert.AreEqual(0, FStrategy.GetPriority);
end;

procedure TGenericConnectionStrategyTests.GetServiceGuids_ReturnsNonEmpty;
begin
  Assert.IsTrue(Length(FStrategy.GetServiceGuids) > 0);
end;

{ TConnectionStrategyFactoryTests }

procedure TConnectionStrategyFactoryTests.Setup;
begin
  FFactory := TConnectionStrategyFactory.Create;
end;

procedure TConnectionStrategyFactoryTests.Create_RegistersDefaultStrategies;
begin
  Assert.AreEqual(Integer(3), Integer(Length(FFactory.GetAllStrategies)));
end;

procedure TConnectionStrategyFactoryTests.CreateWithoutDefaults_HasNoStrategies;
var
  Factory: IConnectionStrategyFactory;
begin
  Factory := TConnectionStrategyFactory.Create(False);
  Assert.AreEqual(Integer(0), Integer(Length(Factory.GetAllStrategies)));
end;

procedure TConnectionStrategyFactoryTests.GetStrategy_AudioOutput_ReturnsAudioStrategy;
var
  Strategy: IConnectionStrategy;
begin
  Strategy := FFactory.GetStrategy(btAudioOutput);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy.CanHandle(btAudioOutput));
  Assert.AreEqual(100, Strategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.GetStrategy_Keyboard_ReturnsHIDStrategy;
var
  Strategy: IConnectionStrategy;
begin
  Strategy := FFactory.GetStrategy(btKeyboard);
  Assert.IsNotNull(Strategy);
  Assert.IsTrue(Strategy.CanHandle(btKeyboard));
  Assert.AreEqual(100, Strategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.GetStrategy_Unknown_ReturnsGenericStrategy;
var
  Strategy: IConnectionStrategy;
begin
  Strategy := FFactory.GetStrategy(btUnknown);
  Assert.IsNotNull(Strategy);
  // Generic strategy has priority 0
  Assert.AreEqual(0, Strategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.GetStrategy_Phone_ReturnsGenericStrategy;
var
  Strategy: IConnectionStrategy;
begin
  Strategy := FFactory.GetStrategy(btPhone);
  Assert.IsNotNull(Strategy);
  // Phone is not handled by Audio or HID, so Generic handles it
  Assert.AreEqual(0, Strategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.GetStrategy_SelectsHighestPriority;
var
  Factory: IConnectionStrategyFactory;
  LowPriorityStrategy: TMockConnectionStrategy;
  HighPriorityStrategy: TMockConnectionStrategy;
  SelectedStrategy: IConnectionStrategy;
begin
  Factory := TConnectionStrategyFactory.Create(False);

  LowPriorityStrategy := TMockConnectionStrategy.Create(10, [btAudioOutput]);
  HighPriorityStrategy := TMockConnectionStrategy.Create(100, [btAudioOutput]);

  Factory.RegisterStrategy(LowPriorityStrategy);
  Factory.RegisterStrategy(HighPriorityStrategy);

  SelectedStrategy := Factory.GetStrategy(btAudioOutput);

  Assert.IsNotNull(SelectedStrategy);
  Assert.AreEqual(100, SelectedStrategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.RegisterStrategy_AddsToList;
var
  Factory: IConnectionStrategyFactory;
  Strategy: TMockConnectionStrategy;
begin
  Factory := TConnectionStrategyFactory.Create(False);
  Assert.AreEqual(Integer(0), Integer(Length(Factory.GetAllStrategies)));

  Strategy := TMockConnectionStrategy.Create;
  Factory.RegisterStrategy(Strategy);

  Assert.AreEqual(Integer(1), Integer(Length(Factory.GetAllStrategies)));
end;

procedure TConnectionStrategyFactoryTests.RegisterStrategy_CanOverridePriority;
var
  HighPriorityAudio: TMockConnectionStrategy;
  SelectedStrategy: IConnectionStrategy;
begin
  // Add a higher priority audio strategy
  HighPriorityAudio := TMockConnectionStrategy.Create(200, [btAudioOutput]);
  FFactory.RegisterStrategy(HighPriorityAudio);

  SelectedStrategy := FFactory.GetStrategy(btAudioOutput);

  // Should select the new higher priority strategy
  Assert.AreEqual(200, SelectedStrategy.GetPriority);
end;

procedure TConnectionStrategyFactoryTests.GetAllStrategies_ReturnsAllRegistered;
var
  Factory: IConnectionStrategyFactory;
begin
  Factory := TConnectionStrategyFactory.Create(False);

  Factory.RegisterStrategy(TMockConnectionStrategy.Create);
  Factory.RegisterStrategy(TMockConnectionStrategy.Create);
  Factory.RegisterStrategy(TMockConnectionStrategy.Create);

  Assert.AreEqual(Integer(3), Integer(Length(Factory.GetAllStrategies)));
end;

procedure TConnectionStrategyFactoryTests.Clear_RemovesAllStrategies;
begin
  Assert.AreEqual(Integer(3), Integer(Length(FFactory.GetAllStrategies)));
  FFactory.Clear;
  Assert.AreEqual(Integer(0), Integer(Length(FFactory.GetAllStrategies)));
end;

procedure TConnectionStrategyFactoryTests.Clear_GetStrategyReturnsNil;
begin
  FFactory.Clear;
  Assert.IsNull(FFactory.GetStrategy(btAudioOutput));
end;

{ TMockConnectionStrategyFactoryTests }

procedure TMockConnectionStrategyFactoryTests.Setup;
begin
  FFactory := TMockConnectionStrategyFactory.Create;
end;

procedure TMockConnectionStrategyFactoryTests.TearDown;
begin
  FFactory.Free;
end;

procedure TMockConnectionStrategyFactoryTests.GetStrategy_IncrementsCallCount;
begin
  Assert.AreEqual(0, FFactory.GetStrategyCallCount);
  FFactory.GetStrategy(btAudioOutput);
  Assert.AreEqual(1, FFactory.GetStrategyCallCount);
  FFactory.GetStrategy(btKeyboard);
  Assert.AreEqual(2, FFactory.GetStrategyCallCount);
end;

procedure TMockConnectionStrategyFactoryTests.GetStrategy_RecordsLastDeviceType;
begin
  FFactory.GetStrategy(btAudioOutput);
  Assert.AreEqual(Ord(btAudioOutput), Ord(FFactory.LastRequestedDeviceType));
  FFactory.GetStrategy(btKeyboard);
  Assert.AreEqual(Ord(btKeyboard), Ord(FFactory.LastRequestedDeviceType));
end;

procedure TMockConnectionStrategyFactoryTests.RegisterStrategy_AddsStrategy;
begin
  Assert.AreEqual(Integer(0), Integer(Length(FFactory.GetAllStrategies)));
  FFactory.RegisterStrategy(TMockConnectionStrategy.Create);
  Assert.AreEqual(Integer(1), Integer(Length(FFactory.GetAllStrategies)));
end;

procedure TMockConnectionStrategyFactoryTests.Clear_ResetsCallCount;
begin
  FFactory.GetStrategy(btAudioOutput);
  FFactory.GetStrategy(btAudioOutput);
  Assert.AreEqual(2, FFactory.GetStrategyCallCount);
  FFactory.Clear;
  Assert.AreEqual(0, FFactory.GetStrategyCallCount);
end;

{ TMockConnectionStrategyTests }

procedure TMockConnectionStrategyTests.Setup;
begin
  FStrategy := TMockConnectionStrategy.Create;
end;

procedure TMockConnectionStrategyTests.TearDown;
begin
  FStrategy.Free;
end;

procedure TMockConnectionStrategyTests.CanHandle_DefaultsToTrue;
begin
  Assert.IsTrue(FStrategy.CanHandle(btAudioOutput));
  Assert.IsTrue(FStrategy.CanHandle(btKeyboard));
  Assert.IsTrue(FStrategy.CanHandle(btUnknown));
end;

procedure TMockConnectionStrategyTests.CanHandle_RespectsCanHandleResult;
begin
  FStrategy.CanHandleResult := False;
  Assert.IsFalse(FStrategy.CanHandle(btAudioOutput));
  Assert.IsFalse(FStrategy.CanHandle(btKeyboard));
end;

procedure TMockConnectionStrategyTests.CanHandle_WithSupportedTypes_ReturnsTrueForMatch;
begin
  FStrategy.SupportedTypes := [btAudioOutput, btHeadset];
  Assert.IsTrue(FStrategy.CanHandle(btAudioOutput));
  Assert.IsTrue(FStrategy.CanHandle(btHeadset));
end;

procedure TMockConnectionStrategyTests.CanHandle_WithSupportedTypes_ReturnsFalseForNonMatch;
begin
  FStrategy.SupportedTypes := [btAudioOutput, btHeadset];
  Assert.IsFalse(FStrategy.CanHandle(btKeyboard));
  Assert.IsFalse(FStrategy.CanHandle(btMouse));
end;

procedure TMockConnectionStrategyTests.GetPriority_ReturnsConfiguredPriority;
begin
  FStrategy.Priority := 42;
  Assert.AreEqual(42, FStrategy.GetPriority);
  FStrategy.Priority := 100;
  Assert.AreEqual(100, FStrategy.GetPriority);
end;

procedure TMockConnectionStrategyTests.GetServiceGuids_ReturnsConfiguredGuids;
var
  TestGuid: TGUID;
begin
  TestGuid := StringToGUID('{12345678-1234-1234-1234-123456789012}');
  FStrategy.ServiceGuids := [TestGuid];

  Assert.AreEqual(Integer(1), Integer(Length(FStrategy.GetServiceGuids)));
  Assert.IsTrue(FStrategy.GetServiceGuids[0] = TestGuid);
end;

initialization
  TDUnitX.RegisterTestFixture(TAudioConnectionStrategyTests);
  TDUnitX.RegisterTestFixture(THIDConnectionStrategyTests);
  TDUnitX.RegisterTestFixture(TGenericConnectionStrategyTests);
  TDUnitX.RegisterTestFixture(TConnectionStrategyFactoryTests);
  TDUnitX.RegisterTestFixture(TMockConnectionStrategyFactoryTests);
  TDUnitX.RegisterTestFixture(TMockConnectionStrategyTests);

end.
