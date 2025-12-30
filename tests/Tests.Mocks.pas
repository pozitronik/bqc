{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect - Tests                 }
{       Mock Implementations - Facade Unit              }
{                                                       }
{       This unit re-exports all mock types from        }
{       split mock units for backward compatibility.    }
{                                                       }
{*******************************************************}

unit Tests.Mocks;

interface

{$REGION 'Re-exports from split units'}

uses
  // Core types needed for function signature
  Bluetooth.Types,
  Bluetooth.Interfaces,
  // Re-export all mock units
  Tests.Mocks.Config,
  Tests.Mocks.View,
  Tests.Mocks.Bluetooth,
  Tests.Mocks.Infrastructure;

// Re-export types from Tests.Mocks.Config
type
  TMockLayoutConfig = Tests.Mocks.Config.TMockLayoutConfig;
  TMockAppearanceConfig = Tests.Mocks.Config.TMockAppearanceConfig;
  TMockDeviceConfigProvider = Tests.Mocks.Config.TMockDeviceConfigProvider;
  TMockBatteryTrayConfig = Tests.Mocks.Config.TMockBatteryTrayConfig;
  TMockPollingConfig = Tests.Mocks.Config.TMockPollingConfig;
  TMockConnectionConfig = Tests.Mocks.Config.TMockConnectionConfig;
  TMockGeneralConfig = Tests.Mocks.Config.TMockGeneralConfig;
  TMockWindowConfig = Tests.Mocks.Config.TMockWindowConfig;
  TMockPositionConfig = Tests.Mocks.Config.TMockPositionConfig;
  TMockHotkeyConfig = Tests.Mocks.Config.TMockHotkeyConfig;
  TMockNotificationConfig = Tests.Mocks.Config.TMockNotificationConfig;
  TMockLogConfig = Tests.Mocks.Config.TMockLogConfig;
  TMockProfileConfig = Tests.Mocks.Config.TMockProfileConfig;
  TMockAppConfig = Tests.Mocks.Config.TMockAppConfig;

// Re-export types from Tests.Mocks.View
type
  TMockSettingsView = Tests.Mocks.View.TMockSettingsView;
  TMockMainView = Tests.Mocks.View.TMockMainView;

// Re-export types from Tests.Mocks.Bluetooth
type
  TMockConnectionStrategy = Tests.Mocks.Bluetooth.TMockConnectionStrategy;
  TMockConnectionStrategyFactory = Tests.Mocks.Bluetooth.TMockConnectionStrategyFactory;
  TMockDeviceMonitor = Tests.Mocks.Bluetooth.TMockDeviceMonitor;
  TMockDeviceRepository = Tests.Mocks.Bluetooth.TMockDeviceRepository;
  TMockConnectionExecutor = Tests.Mocks.Bluetooth.TMockConnectionExecutor;
  TMockAdapterQuery = Tests.Mocks.Bluetooth.TMockAdapterQuery;
  TMockBluetoothDeviceQuery = Tests.Mocks.Bluetooth.TMockBluetoothDeviceQuery;
  TMockEventDebouncer = Tests.Mocks.Bluetooth.TMockEventDebouncer;
  TMockRadioStateManager = Tests.Mocks.Bluetooth.TMockRadioStateManager;
  TMockBatteryCache = Tests.Mocks.Bluetooth.TMockBatteryCache;
  TMockBluetoothService = Tests.Mocks.Bluetooth.TMockBluetoothService;
  TMockProfileQuery = Tests.Mocks.Bluetooth.TMockProfileQuery;

// Re-export types from Tests.Mocks.Infrastructure
type
  TMockAutostartManager = Tests.Mocks.Infrastructure.TMockAutostartManager;
  TMockThemeManager = Tests.Mocks.Infrastructure.TMockThemeManager;
  TMockLogger = Tests.Mocks.Infrastructure.TMockLogger;
  TMockAsyncExecutor = Tests.Mocks.Infrastructure.TMockAsyncExecutor;
  TMockDeviceDisplayItemBuilder = Tests.Mocks.Infrastructure.TMockDeviceDisplayItemBuilder;

{$ENDREGION}

// Re-export helper function
function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;

implementation

function CreateTestDevice(
  AAddressInt: UInt64;
  const AName: string;
  ADeviceType: TBluetoothDeviceType;
  AConnectionState: TBluetoothConnectionState
): TBluetoothDeviceInfo;
begin
  Result := Tests.Mocks.Bluetooth.CreateTestDevice(AAddressInt, AName, ADeviceType, AConnectionState);
end;

end.
