/-
  Proof scaffolding for rounding properties.
  All theorems are stated but left as sorry — to be filled in.
-/
import Morpho.Libraries.SharesMathLib
import Morpho.Specs.Rounding

namespace Morpho.Proofs.Rounding

open Verity
open Morpho.Libraries.SharesMathLib
open Morpho.Specs.Rounding

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
