/-
  Proofs of rounding direction properties.

  These show that the Down variant of each share/asset conversion always
  produces a result ≤ the Up variant, ensuring the protocol never loses
  value through rounding.

  Note: The rounding direction proofs require no-overflow assumptions
  (the result must fit in Uint256). In Solidity, this is enforced by
  checked arithmetic — any overflow would revert the transaction.
  The round-trip proofs require deeper arithmetic reasoning about
  the composition of floor and ceiling division.
-/
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.MathLib
import Morpho.Specs.Rounding

namespace Morpho.Proofs.Rounding

open Verity.Core
open Morpho.Libraries.SharesMathLib
open Morpho.Libraries.MathLib
open Morpho.Specs.Rounding

/-! ## Core rounding lemma (proven in MathLib)

  `mulDivDown_le_mulDivUp` states that floor division ≤ ceiling division
  whenever the denominator is positive and the result doesn't overflow.
  This is the foundation for all rounding direction properties. -/

/-! ## SharesMathLib rounding direction

  Each conversion (toShares, toAssets) wraps `mulDivDown` / `mulDivUp` with
  denominators that include a virtual offset (VIRTUAL_ASSETS=1 or
  VIRTUAL_SHARES=1e6). We require:
  - The denominator doesn't wrap to zero when packed into Uint256
  - The ceiling-division result fits in Uint256 (Solidity reverts otherwise) -/

/-- Shares rounding: toSharesDown ≤ toSharesUp.
    The denominator is `totalAssets + 1` (VIRTUAL_ASSETS = 1).
    `h_denom` ensures it fits in Uint256 (so it doesn't wrap to 0).
    `h_no_overflow` ensures the ceiling result fits in Uint256
    (Solidity checked arithmetic would revert otherwise). -/
theorem toSharesDown_le_toSharesUp (assets totalAssets totalShares : Uint256)
    (h_denom : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_num : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_no_overflow : (assets.val * (totalShares.val + VIRTUAL_SHARES)
      + (totalAssets.val + VIRTUAL_ASSETS - 1))
      / (totalAssets.val + VIRTUAL_ASSETS) < Uint256.modulus) :
    sharesDownLeUp assets totalAssets totalShares := by
  unfold sharesDownLeUp toSharesDown toSharesUp
  apply mulDivDown_le_mulDivUp
  · -- Denominator > 0: (Uint256.ofNat (totalAssets.val + 1)).val > 0
    show (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val > 0
    rw [Uint256.val_ofNat, Nat.mod_eq_of_lt h_denom]
    show totalAssets.val + VIRTUAL_ASSETS > 0
    simp [VIRTUAL_ASSETS]
  · -- No overflow: ceiling division result < modulus
    show (assets.val * (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val
      + ((Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val - 1))
      / (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val < Uint256.modulus
    rw [Uint256.val_ofNat, Uint256.val_ofNat,
        Nat.mod_eq_of_lt h_denom, Nat.mod_eq_of_lt h_num]
    exact h_no_overflow

/-- Assets rounding: toAssetsDown ≤ toAssetsUp.
    The denominator is `totalShares + 1e6` (VIRTUAL_SHARES = 10^6).
    Same structure as the shares proof with swapped roles. -/
theorem toAssetsDown_le_toAssetsUp (shares totalAssets totalShares : Uint256)
    (h_denom : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_num : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_no_overflow : (shares.val * (totalAssets.val + VIRTUAL_ASSETS)
      + (totalShares.val + VIRTUAL_SHARES - 1))
      / (totalShares.val + VIRTUAL_SHARES) < Uint256.modulus) :
    assetsDownLeUp shares totalAssets totalShares := by
  unfold assetsDownLeUp toAssetsDown toAssetsUp
  apply mulDivDown_le_mulDivUp
  · -- Denominator > 0
    show (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val > 0
    rw [Uint256.val_ofNat, Nat.mod_eq_of_lt h_denom]
    show totalShares.val + VIRTUAL_SHARES > 0
    simp [VIRTUAL_SHARES]
  · -- No overflow
    show (shares.val * (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val
      + ((Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val - 1))
      / (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val < Uint256.modulus
    rw [Uint256.val_ofNat, Uint256.val_ofNat,
        Nat.mod_eq_of_lt h_denom, Nat.mod_eq_of_lt h_num]
    exact h_no_overflow

/-! ## Round-trip properties -/

/-- Converting assets→shares→assets with opposite rounding yields ≥ original. -/
theorem supply_roundtrip_no_loss (assets totalAssets totalShares : Uint256) :
    (toAssetsUp (toSharesDown assets totalAssets totalShares) totalAssets totalShares).val
      ≥ assets.val := by
  sorry

/-- Converting shares→assets→shares with opposite rounding yields ≥ original. -/
theorem withdraw_roundtrip_no_loss (shares totalAssets totalShares : Uint256) :
    (toSharesUp (toAssetsDown shares totalAssets totalShares) totalAssets totalShares).val
      ≥ shares.val := by
  sorry

end Morpho.Proofs.Rounding
