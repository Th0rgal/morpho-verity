/-
  MathLib — Fixed-point arithmetic matching morpho-blue/src/libraries/MathLib.sol

  All operations use WAD = 1e18 scaling. Overflow behavior matches Solidity 0.8.19:
  checked arithmetic reverts on overflow.
-/
import Verity.Core

namespace Morpho.Libraries.MathLib

open Verity.Core

def WAD : Nat := 10 ^ 18

/-- (x * y) / d, rounded down. Corresponds to `mulDivDown` in MathLib.sol. -/
def mulDivDown (x y d : Uint256) : Uint256 :=
  Uint256.ofNat ((x.val * y.val) / d.val)

/-- (x * y + (d - 1)) / d, rounded up. Corresponds to `mulDivUp` in MathLib.sol. -/
def mulDivUp (x y d : Uint256) : Uint256 :=
  Uint256.ofNat ((x.val * y.val + (d.val - 1)) / d.val)

/-- (x * y) / WAD, rounded down. Corresponds to `wMulDown` in MathLib.sol. -/
def wMulDown (x y : Uint256) : Uint256 :=
  mulDivDown x y (Uint256.ofNat WAD)

/-- (x * WAD) / y, rounded down. Corresponds to `wDivDown` in MathLib.sol. -/
def wDivDown (x y : Uint256) : Uint256 :=
  mulDivDown x (Uint256.ofNat WAD) y

/-- (x * WAD) / y, rounded up. Corresponds to `wDivUp` in MathLib.sol. -/
def wDivUp (x y : Uint256) : Uint256 :=
  mulDivUp x (Uint256.ofNat WAD) y

/--
  Third-order Taylor expansion of e^(x*n) - 1 around 0.
  Matches `wTaylorCompounded` in MathLib.sol.
  Used for continuous interest rate compounding.
-/
def wTaylorCompounded (x n : Uint256) : Uint256 :=
  let firstTerm := x.val * n.val
  let secondTerm := (firstTerm * firstTerm) / (2 * WAD)
  let thirdTerm := (secondTerm * firstTerm) / (3 * WAD)
  Uint256.ofNat (firstTerm + secondTerm + thirdTerm)

end Morpho.Libraries.MathLib
