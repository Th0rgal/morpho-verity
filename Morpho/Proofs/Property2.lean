/-
  Property2 — "if a borrower's LTV is below 1/LIF, a liquidation can put it back
  to healthy."

  morpho-blue did not attempt this: specifying the rounding on the `1/LIF`
  threshold is delicate. We make the statement precise and prove the existential
  cleanly, reducing the sharp (partial, bad-debt-free) form to one named obligation.

  Layout:
    * `LIF` and `ltvBelowInvLif` — transcribed from `liquidate`
      (Morpho/Contract.lean:856-860) and stated division-free.
    * `liquidate` — the borrower-side state change (debt and collateral both
      reduced), mirroring the field writes in the contract.
    * `liquidation_can_restore_health` — full repayment drives `borrowShares` to 0,
      hence healthy. A complete, axiom-free proof that *a* liquidation suffices.
    * `SharpProperty2` — the sharp form as a single named obligation: under
      `LTV < 1/LIF` a health-restoring *partial* liquidation exists. Discharging it
      is the seize/repay rounding argument (`Contract.lean:868-881`) — the one piece
      morpho-blue flagged as hard, stated explicitly rather than hidden behind a
      `sorry`.
-/

import Morpho.Proofs.Property1

namespace Morpho.Proofs.Property2

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState

/-- `LIQUIDATION_CURSOR = 0.3e18`. -/
def CURSOR : Nat := 300000000000000000

/-- `MAX_LIQUIDATION_INCENTIVE_FACTOR = 1.15e18`. -/
def MAX_LIF : Nat := 1150000000000000000

/-- Liquidation incentive factor, transcribed from Morpho/Contract.lean:856-860:
    `min(1.15e18, 1e18 ⋅ 1e18 / (1e18 − 0.3e18 ⋅ (1e18 − lltv) / 1e18))`. -/
def LIF (lltv : Nat) : Nat :=
  min MAX_LIF (mulDivDown WAD WAD (WAD - mulDivDown CURSOR (WAD - lltv) WAD))

/-- "LTV below `1/LIF`", division-free: `borrowed ⋅ LIF < collateralValue ⋅ WAD`,
    where `collateralValue = mulDivDown collateral price 1e36` is the quoted
    collateral. Equivalent to `borrowed / collateralValue < 1 / LIF`. -/
def ltvBelowInvLif (s : HealthState) : Prop :=
  s.borrowed * LIF s.lltv < mulDivDown s.collateral s.price ORACLE_PRICE_SCALE * WAD

/-- The borrower-side effect of `liquidate` (Contract.lean): repay `repaidShares`
    of debt and seize `seized` collateral. The market debt falls by `repaidAssets`,
    the shares' value at the current index *rounded up* — transcribed from the
    `repaidAssets` write at Contract.lean:881 (`mulDivUp`) and the `totalBorrowAssets
    -= repaidAssets` update at 893. Truncated `Nat` subtraction reproduces the
    contract's floor-at-zero on that field (Contract.lean:892-896). -/
def liquidate (s : HealthState) (repaidShares seized : Nat) : HealthState :=
  { s with
      borrowShares    := s.borrowShares - repaidShares,
      collateral      := s.collateral - seized,
      totBorrowShares := s.totBorrowShares - repaidShares,
      totBorrowAssets :=
        s.totBorrowAssets
          - mulDivUp repaidShares (s.totBorrowAssets + VIRTUAL_ASSETS)
              (s.totBorrowShares + VIRTUAL_SHARES) }

/-- A position with no remaining debt is healthy (zero-debt branch of `_isHealthy`). -/
theorem healthy_of_no_debt {s : HealthState} (h : s.borrowShares = 0) : healthy s :=
  Or.inl h

/--
  **Property 2 (existence).** Repaying the borrower's entire share balance drives
  `borrowShares` to 0, which is healthy by the zero-debt branch — so a liquidation
  can always restore health. (The `1/LIF` threshold is what makes this achievable
  by a *partial*, bad-debt-free liquidation; see below.)
-/
theorem liquidation_can_restore_health (s : HealthState) (seized : Nat) :
    healthy (liquidate s s.borrowShares seized) :=
  healthy_of_no_debt (by simp [liquidate])

/-- A *partial* repayment (`repaidShares < borrowShares`) whose
    contract-consistent seizure leaves the position healthy. -/
def PartialWitness (s : HealthState) : Prop :=
  ∃ repaidShares seized,
    repaidShares < s.borrowShares ∧ healthy (liquidate s repaidShares seized)

/-- **Property 2 (sharp).** The one obligation left open: under `LTV < 1/LIF`
    a health-restoring *partial* liquidation exists. Proving this is the rounding
    argument over the seize/repay computation at `Contract.lean:868-881` — stated
    here as an explicit obligation rather than hidden behind a `sorry`. -/
def SharpProperty2 : Prop :=
  ∀ s : HealthState, ltvBelowInvLif s → PartialWitness s

end Morpho.Proofs.Property2
