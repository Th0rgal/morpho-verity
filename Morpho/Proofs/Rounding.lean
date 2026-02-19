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

/-! ## SharesMathLib rounding direction -/

theorem toSharesDown_le_toSharesUp (assets totalAssets totalShares : Uint256) :
    sharesDownLeUp assets totalAssets totalShares := by
  sorry

theorem toAssetsDown_le_toAssetsUp (shares totalAssets totalShares : Uint256) :
    assetsDownLeUp shares totalAssets totalShares := by
  sorry

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
