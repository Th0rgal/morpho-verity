/-
  ConstantsLib — Protocol constants matching morpho-blue/src/libraries/ConstantsLib.sol
-/
namespace Morpho.Libraries.ConstantsLib

def WAD : Nat := 10 ^ 18
def MAX_FEE : Nat := WAD / 4                    -- 0.25e18 = 25%
def ORACLE_PRICE_SCALE : Nat := 10 ^ 36
def LIQUIDATION_CURSOR : Nat := 3 * WAD / 10    -- 0.3e18 = 30%
def MAX_LIQUIDATION_INCENTIVE_FACTOR : Nat := 115 * WAD / 100  -- 1.15e18 = 115%

end Morpho.Libraries.ConstantsLib
