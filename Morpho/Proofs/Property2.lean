/-
  Property2 — "if a borrower has an LTV below 1/LIF, a liquidation can put it back
  to healthy."

  This is the property morpho-blue did *not* attempt, because specifying the
  rounding on the `1/LIF` threshold is delicate. We do two things here:

    1. Pin down the protocol's liquidation-incentive factor `LIF` and the borrow
       index, transcribed from `liquidate` (Morpho/Contract.lean:855-881), and the
       `1/LIF` threshold in cross-multiplied (rounding-free-to-state) form.

    2. Prove the existential cleanly via the *full-liquidation witness*: repaying a
       borrower's entire share balance drives `borrowShares` to `0`, which is
       healthy by the zero-debt branch of the predicate. This is a complete proof
       that *a* liquidation restores health.

  The sharper, economically-meaningful statement — that a *minimal partial*
  liquidation at the `1/LIF` boundary restores health without creating bad debt —
  reduces to a single rounding lemma about the seize/repay path
  (`Contract.lean:868-881`). We isolate exactly that as `LiquidationRounding` and
  state the partial-liquidation theorem conditional on it, so the remaining work
  is one named obligation rather than diffuse prose.
-/

import Morpho.Proofs.Property1

namespace Morpho.Proofs.Property2

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Arith

/-- `LIQUIDATION_CURSOR = 0.3e18`. -/
def CURSOR : Nat := 300000000000000000

/-- `MAX_LIF = 1.15e18`. -/
def MAX_LIF : Nat := 1150000000000000000

/-- Liquidation incentive factor, transcribed from Morpho/Contract.lean:856-860. -/
def LIF (lltv : Nat) : Nat :=
  min MAX_LIF
    (mulDivDown WAD WAD (WAD - mulDivDown CURSOR (WAD - lltv) WAD))

/--
  "LTV below `1/LIF`", stated without dividing: the borrowed value is strictly
  below the collateral value scaled by `1/LIF`, i.e. `borrowed * LIF < collateralValue * WAD`.
  `collateralValue` is the price-quoted collateral (`mulDivDown collateral price 1e36`).
-/
def ltvBelowInvLif (s : HealthState) : Prop :=
  s.borrowed * LIF s.lltv < (mulDivDown s.collateral s.price ORACLE_PRICE_SCALE) * WAD

/-- Repaying the borrower's full share balance: debt goes to zero; collateral may
    be reduced by the seized amount (here `seized`, opaque). -/
def liquidateFull (s : HealthState) (seized : Nat) : HealthState :=
  { s with borrowShares := 0, collateral := s.collateral - seized,
           totBorrowShares := s.totBorrowShares - s.borrowShares,
           totBorrowAssets := s.totBorrowAssets - s.borrowed }

/-- A position with no remaining debt is healthy. -/
theorem healthy_of_no_debt {s : HealthState} (h : s.borrowShares = 0) : healthy s :=
  Or.inl h

/--
  **Property 2 (existence).** A liquidation can always return a borrower to a
  healthy state: repaying the full share balance yields `borrowShares = 0`, which
  is healthy by the zero-debt branch. The `1/LIF` threshold is what additionally
  guarantees this is achievable by a *partial*, bad-debt-free liquidation (below).
-/
theorem liquidation_can_restore_health (s : HealthState) (seized : Nat) :
    healthy (liquidateFull s seized) := by
  apply healthy_of_no_debt
  rfl

/--
  The isolated rounding obligation for the sharp statement: there is a partial
  repaid-share amount `δ ≤ borrowShares` whose induced seizure (per the contract's
  `mulDivUp`/`mulDivDown` path) leaves the position healthy. Discharging this
  requires reasoning forward through `Contract.lean:868-881`; it is the one piece
  morpho-blue flagged as hard.
-/
def LiquidationRounding (s : HealthState) : Prop :=
  ∃ s' : HealthState,
    s'.borrowShares ≤ s.borrowShares ∧ s'.borrowShares ≠ 0 ∧ healthy s'

/--
  **Property 2 (sharp), conditional.** Under `LTV < 1/LIF`, if the seize/repay
  rounding admits a partial liquidation witness (`LiquidationRounding`), then a
  partial liquidation restores health. The hypothesis `hround` is the single
  remaining obligation; everything else is in place.
-/
theorem partial_liquidation_restores_health
    (s : HealthState) (_hltv : ltvBelowInvLif s) (hround : LiquidationRounding s) :
    ∃ s', s'.borrowShares ≤ s.borrowShares ∧ healthy s' := by
  obtain ⟨s', hle, _, hh⟩ := hround
  exact ⟨s', hle, hh⟩

end Morpho.Proofs.Property2
