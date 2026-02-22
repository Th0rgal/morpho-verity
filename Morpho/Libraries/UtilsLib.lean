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

/-! ## Properties -/

/-- Saturating subtraction never exceeds the original value. -/
theorem zeroFloorSub_le (x y : Uint256) : (zeroFloorSub x y).val ≤ x.val := by
  unfold zeroFloorSub
  split
  · -- x > y case: result = (x.val - y.val) % modulus
    simp [Uint256.val_ofNat]
    have h_lt : x.val - y.val < Uint256.modulus :=
      Nat.lt_of_le_of_lt (Nat.sub_le x.val y.val) x.isLt
    rw [Nat.mod_eq_of_lt h_lt]
    exact Nat.sub_le x.val y.val
  · -- x ≤ y case: result = 0 % modulus = 0
    simp

end Morpho.Libraries.UtilsLib
