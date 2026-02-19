/-
  Proof scaffolding for authorization properties.
  All theorems are stated but left as sorry — to be filled in.
-/
import Morpho.Morpho
import Morpho.Specs.Authorization

namespace Morpho.Proofs.Authorization

open Verity
open Morpho.Types
open Morpho.Specs.Authorization

/-! ## Withdraw requires authorization -/

theorem withdraw_requires_authorization (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdraw s id assets shares onBehalf = none := by
  sorry

/-! ## Borrow requires authorization -/

theorem borrow_requires_authorization (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (collateralPrice lltv : Uint256)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.borrow s id assets shares onBehalf collateralPrice lltv = none := by
  sorry

/-! ## Withdraw collateral requires authorization -/

theorem withdrawCollateral_requires_authorization (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf : Address)
    (collateralPrice lltv : Uint256)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdrawCollateral s id assets onBehalf collateralPrice lltv = none := by
  sorry

/-! ## Supply doesn't require authorization -/

theorem supply_no_authorization_needed (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    : ∃ r, Morpho.supply s id assets shares onBehalf = some r := by
  sorry

end Morpho.Proofs.Authorization
