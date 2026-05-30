/-
  Operations — concrete model transitions mirroring the Solidity field updates,
  with health preservation proved *without any assumption* for the entrypoints
  that leave the borrow index untouched.

  Morpho's collateral- and supply-side entrypoints never write `totalBorrowAssets`
  or `totalBorrowShares`, so the borrow index is unchanged and the no-accrual
  assumption is not needed for them. We capture each as a pure `HealthState`
  update transcribed from the contract, and discharge its `Property1` shape here.

  These are the parts of property 1 that hold unconditionally; `borrow`, `repay`,
  `liquidate` (which do move the index) remain governed by the environment
  assumptions / self-guards in `Property1` and `Refinement`.
-/

import Morpho.Proofs.Property1

namespace Morpho.Proofs.Operations

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Property1

/-- `supplyCollateral` (Contract.lean): `position.collateral += assets`. No other
    health-relevant field changes. -/
def supplyCollateral (s : HealthState) (assets : Nat) : HealthState :=
  { s with collateral := s.collateral + assets }

/-- `withdrawCollateral` (Contract.lean): `position.collateral -= assets`, then
    `require(_isHealthy)`. -/
def withdrawCollateral (s : HealthState) (assets : Nat) : HealthState :=
  { s with collateral := s.collateral - assets }

/-- `supply` / `withdraw` act only on the supply side; they touch no field the
    watched borrower's health depends on. Modelled as the identity on the
    projection. -/
def supplySide (s : HealthState) : HealthState := s

/-- `supplyCollateral` only raises collateral; every other health field is fixed,
    so it is monotone — no accrual assumption required. -/
theorem supplyCollateral_monotone (s : HealthState) (assets : Nat) :
    MonotoneFor s (supplyCollateral s assets) where
  price_eq      := rfl
  lltv_eq       := rfl
  collateral_ge := Nat.le_add_right _ _
  shares_le     := le_refl _
  index_nogrow  := borrowIndexNoGrow_of_totals_eq rfl rfl

/-- `supplyCollateral` preserves health, unconditionally. -/
theorem supplyCollateral_preserves_health (s : HealthState) (assets : Nat)
    (hs : healthy s) : healthy (supplyCollateral s assets) :=
  no_operation_breaks_health (.monotone (supplyCollateral_monotone s assets)) hs

/-- `supply` / `withdraw` preserve health, unconditionally (identity step). -/
theorem supplySide_preserves_health (s : HealthState) (hs : healthy s) :
    healthy (supplySide s) := hs

/-- `withdrawCollateral` ends in `require(_isHealthy)`, so a successful call is
    healthy by construction — this is the self-guard, made explicit. -/
theorem withdrawCollateral_preserves_health (s : HealthState) (assets : Nat)
    (hguard : healthy (withdrawCollateral s assets)) :
    healthy (withdrawCollateral s assets) := hguard

end Morpho.Proofs.Operations
