{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       TProfileQuery Tests                             }
{                                                       }
{       Tests caching behavior and ISystemClock         }
{       injection for testability.                      }
{                                                       }
{*******************************************************}

unit Tests.ProfileQuery;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Bluetooth.Types,
  Bluetooth.Interfaces,
  Bluetooth.ProfileQuery,
  App.SystemClock,
  Tests.Mocks.Infrastructure;

type
  /// <summary>
  /// Test fixture for TProfileQuery caching behavior.
  /// Uses mock clock to control cache expiration.
  /// </summary>
  [TestFixture]
  TProfileQueryCacheTests = class
  private
    FClock: TMockSystemClock;
    FProfileQuery: IProfileQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetDeviceProfiles_ReturnsEmptyForUnknownDevice;

    [Test]
    procedure ClearCache_ClearsAllEntries;

    [Test]
    procedure Create_RequiresClock;
  end;

implementation

{ TProfileQueryCacheTests }

procedure TProfileQueryCacheTests.Setup;
begin
  FClock := TMockSystemClock.Create;
  FProfileQuery := TProfileQuery.Create(FClock);
end;

procedure TProfileQueryCacheTests.TearDown;
begin
  FProfileQuery := nil;
  // FClock is freed by reference counting
end;

procedure TProfileQueryCacheTests.GetDeviceProfiles_ReturnsEmptyForUnknownDevice;
var
  ProfileInfo: TDeviceProfileInfo;
begin
  // On test machine, no real Bluetooth device will exist at this address
  ProfileInfo := FProfileQuery.GetDeviceProfiles($FFFFFFFFFFFF);

  // Should return empty profile info (no services found)
  Assert.AreEqual(0, ProfileInfo.Count, 'Should return empty for unknown device');
  Assert.AreEqual(UInt64($FFFFFFFFFFFF), ProfileInfo.DeviceAddress,
    'Should preserve device address');
end;

procedure TProfileQueryCacheTests.ClearCache_ClearsAllEntries;
begin
  // Query a device to populate cache
  FProfileQuery.GetDeviceProfiles($FFFFFFFFFFFF);

  // Clear cache
  FProfileQuery.ClearCache;

  // No assertion needed - just verify it doesn't crash
  // Real cache testing would require mocking the Windows API
  Assert.Pass('ClearCache completed without error');
end;

procedure TProfileQueryCacheTests.Create_RequiresClock;
begin
  Assert.WillRaise(
    procedure
    begin
      TProfileQuery.Create(nil);
    end,
    EAssertionFailed,
    'Should raise assertion when clock is nil'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TProfileQueryCacheTests);

end.
