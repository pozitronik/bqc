{*******************************************************}
{                                                       }
{       Bluetooth Quick Connect                         }
{       DPI Scaling Helpers                             }
{                                                       }
{       Pure, side-effect-free conversions between      }
{       logical (96-DPI design) and physical pixels.    }
{                                                       }
{*******************************************************}

unit App.DpiScaling;

interface

const
  /// Design baseline DPI. VCL forms are authored at 96 DPI (100%).
  DESIGN_DPI = 96;

/// <summary>
/// Converts a logical (96-DPI) dimension to physical pixels at the given DPI.
/// </summary>
/// <param name="ALogical">Dimension in logical 96-DPI units.</param>
/// <param name="APPI">Target device DPI. Values &lt;= 0 are treated as DESIGN_DPI.</param>
/// <returns>The dimension in physical pixels, rounded to nearest.</returns>
function LogicalToPhysical(ALogical, APPI: Integer): Integer;

/// <summary>
/// Converts a physical-pixel dimension measured at the given DPI back to logical
/// (96-DPI) units. Used to persist window geometry in a DPI-neutral form so it does
/// not compound when restored and re-scaled on another monitor.
/// </summary>
/// <param name="APhysical">Dimension in physical pixels at APPI.</param>
/// <param name="APPI">Source device DPI the value was measured at. Values &lt;= 0 are treated as DESIGN_DPI.</param>
/// <returns>The dimension in logical 96-DPI units, rounded to nearest.</returns>
function PhysicalToLogical(APhysical, APPI: Integer): Integer;

implementation

/// Rounded integer scale: AValue * ANum / ADenom, half-up rounding (matches WinAPI MulDiv).
/// ADenom is guaranteed positive by the public callers.
function ScaleRound(AValue, ANum, ADenom: Integer): Integer;
begin
  Result := Integer((Int64(AValue) * ANum + (ADenom div 2)) div ADenom);
end;

function LogicalToPhysical(ALogical, APPI: Integer): Integer;
begin
  if APPI <= 0 then
    APPI := DESIGN_DPI;
  Result := ScaleRound(ALogical, APPI, DESIGN_DPI);
end;

function PhysicalToLogical(APhysical, APPI: Integer): Integer;
begin
  if APPI <= 0 then
    APPI := DESIGN_DPI;
  Result := ScaleRound(APhysical, DESIGN_DPI, APPI);
end;

end.
