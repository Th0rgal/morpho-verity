/-
  UtilsLib — Small utility functions matching morpho-blue/src/libraries/UtilsLib.sol
-/
import Verity.Core

namespace Morpho.Libraries.UtilsLib

open Verity.Core

/-- True when exactly one of x, y is zero. -/
def exactlyOneZero (x y : Uint256) : Bool :=
  (x.val == 0) != (y.val == 0)

/-- Minimum of two values. -/
def min (x y : Uint256) : Uint256 :=
  if y.val < x.val then y else x

/-- Saturating subtraction: max(0, x - y). -/
def zeroFloorSub (x y : Uint256) : Uint256 :=
  if x.val > y.val then Uint256.ofNat (x.val - y.val) else Uint256.ofNat 0

end Morpho.Libraries.UtilsLib
