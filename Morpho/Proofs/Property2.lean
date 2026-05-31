/-
  Property2 — "if a borrower's LTV is below 1/LIF, a liquidation can put it back
  to healthy."

  morpho-blue did not attempt this: specifying the rounding on the `1/LIF`
  threshold is delicate. We make the statement precise and prove it outright — the
  sharp form is now a theorem (`sharp_property2`), not an open obligation.

  Layout:
    * `LIF` and `ltvBelowInvLif` — transcribed from `liquidate`
      (Morpho/Contract.lean:856-860) and stated division-free. `WAD_le_LIF` /
      `LIF_le_MAX` prove the factor always lies in `[1, 1.15]`; `lltv_lt_invLif`
      proves `lltv < 1/LIF`, so the liquidatable-yet-restorable band is non-empty.
    * `liquidate` / `seizedAssets` — the borrower-side state change and the
      contract-derived seizure, mirroring the field writes in the contract.
    * `liquidation_can_restore_health` — full repayment drives `borrowShares` to 0,
      hence healthy. A complete, axiom-free proof that *a* liquidation suffices.
    * `full_liquidation_affordable` / `sharp_property2` — under `LTV < 1/LIF` the
      full liquidation seizes `≤ collateral` (so it does not revert at
      Contract.lean:885, leaving no bad debt) *and* restores health. This is the
      sharp `1/LIF` guarantee morpho-blue flagged as hard, proved in full. The
      `sharp_property2` docstring records why the *partial* phrasing is the wrong
      invariant (share indivisibility gives a counterexample at `borrowShares = 1`).
-/

import Morpho.Proofs.Property1

namespace Morpho.Proofs.Property2

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Arith

/-- `LIQUIDATION_CURSOR = 0.3e18`. -/
def CURSOR : Nat := 300000000000000000

/-- `MAX_LIQUIDATION_INCENTIVE_FACTOR = 1.15e18`. -/
def MAX_LIF : Nat := 1150000000000000000

/-- Liquidation incentive factor, transcribed from Morpho/Contract.lean:856-860:
    `min(1.15e18, 1e18 ⋅ 1e18 / (1e18 − 0.3e18 ⋅ (1e18 − lltv) / 1e18))`. -/
def LIF (lltv : Nat) : Nat :=
  min MAX_LIF (mulDivDown WAD WAD (WAD - mulDivDown CURSOR (WAD - lltv) WAD))

/-- The incentive factor never exceeds `MAX_LIF = 1.15e18` (it is a `min`). -/
theorem LIF_le_MAX (lltv : Nat) : LIF lltv ≤ MAX_LIF := Nat.min_le_left _ _

/-- The incentive factor is always at least `WAD` (i.e. `LIF ≥ 1`): the denominator
    `WAD − cursorTerm` is at most `WAD` (since `cursorTerm ≥ 0`), so dividing `WAD·WAD`
    by it yields at least `WAD`; and `WAD ≤ MAX_LIF`. Holds unconditionally — the
    `Nat` subtraction `WAD − lltv` is automatically capped at `WAD`. -/
theorem WAD_le_LIF (lltv : Nat) : WAD ≤ LIF lltv := by
  have key : ∀ d, 0 < d → d ≤ WAD → WAD ≤ mulDivDown WAD WAD d := by
    intro d hd hdle
    unfold mulDivDown
    rw [Nat.le_div_iff_mul_le hd]
    exact Nat.mul_le_mul (le_refl WAD) hdle
  have hcursor : mulDivDown CURSOR (WAD - lltv) WAD ≤ CURSOR := by
    unfold mulDivDown
    calc CURSOR * (WAD - lltv) / WAD
        ≤ CURSOR * WAD / WAD := Nat.div_le_div_right (Nat.mul_le_mul (le_refl CURSOR) (Nat.sub_le _ _))
      _ = CURSOR := by rw [Nat.mul_div_cancel _ (by decide : 0 < WAD)]
  have hden_pos : 0 < WAD - mulDivDown CURSOR (WAD - lltv) WAD := by
    have : CURSOR < WAD := by decide
    omega
  exact le_min (by decide) (key _ hden_pos (Nat.sub_le _ _))

/-- **Window non-emptiness.** For any valid market (`lltv < WAD`) the maximal
    healthy-LTV threshold sits strictly below `1/LIF`: `lltv ⋅ LIF < WAD ⋅ WAD`,
    i.e. `lltv/WAD < WAD/LIF`. So the band `(lltv, 1/LIF)` of positions that are
    liquidatable yet restorable is non-empty — `SharpProperty2`'s hypothesis is not
    vacuous. The proof needs no case split on the `min`: writing `denom` for the LIF
    denominator, `lltv < denom` (the cursor term is `< WAD − lltv`) and
    `LIF ⋅ denom ≤ WAD ⋅ WAD` (floor of `WAD⋅WAD/denom`) combine directly. -/
theorem lltv_lt_invLif {lltv : Nat} (h : lltv < WAD) :
    lltv * LIF lltv < WAD * WAD := by
  set denom := WAD - mulDivDown CURSOR (WAD - lltv) WAD with hdenom
  have hWAD : 0 < WAD := by unfold WAD; omega
  have hCW : CURSOR < WAD := by unfold CURSOR WAD; omega
  have hcursor_lt : mulDivDown CURSOR (WAD - lltv) WAD < WAD - lltv := by
    unfold mulDivDown
    have hpos : 0 < WAD - lltv := by omega
    have hmul : CURSOR * (WAD - lltv) < (WAD - lltv) * WAD := by
      rw [Nat.mul_comm (WAD - lltv) WAD]
      exact mul_lt_mul_of_pos_right hCW hpos
    exact (Nat.div_lt_iff_lt_mul hWAD).mpr hmul
  have hlt_denom : lltv < denom := by omega
  have hLIF_denom : LIF lltv * denom ≤ WAD * WAD := by
    calc LIF lltv * denom
        ≤ mulDivDown WAD WAD denom * denom := mul_le_mul_right' (Nat.min_le_right _ _) denom
      _ ≤ WAD * WAD := Nat.div_mul_le_self _ _
  have hLIFpos : 0 < LIF lltv := lt_of_lt_of_le hWAD (WAD_le_LIF lltv)
  calc lltv * LIF lltv
      < denom * LIF lltv := mul_lt_mul_of_pos_right hlt_denom hLIFpos
    _ = LIF lltv * denom := Nat.mul_comm _ _
    _ ≤ WAD * WAD := hLIF_denom

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

/-- The model's `liquidate` does not grow the borrow index. This connects the
    generic `borrowIndexNoGrow_of_repay` to the concrete `liquidate` field writes:
    a liquidation (by the borrower or any third party) lowers `totBorrowShares` by
    `repaidShares` and `totBorrowAssets` by the rounded-up `repaidAssets`, so it
    supplies the `borrowIndexNoGrow` field that `MonotoneFor` requires for a watched
    account in the same market, by the rounding direction alone rather than from
    `NoAccrual`. Like `borrowIndexNoGrow_of_repay`, this is not yet consumed by the
    headline `no_operation_breaks_health`; it is the witness the extraction layer
    will plug into the other-account case. The hypotheses are the well-formedness
    facts the contract maintains: you cannot repay more shares than the market
    holds, nor more assets than it owes. -/
theorem liquidate_borrowIndexNoGrow (s : HealthState) (repaidShares seized : Nat)
    (hsh : repaidShares ≤ s.totBorrowShares)
    (hass : mulDivUp repaidShares (s.totBorrowAssets + VIRTUAL_ASSETS)
              (s.totBorrowShares + VIRTUAL_SHARES) ≤ s.totBorrowAssets) :
    borrowIndexNoGrow s (liquidate s repaidShares seized) := by
  apply Morpho.Proofs.Property1.borrowIndexNoGrow_of_repay repaidShares
  · simp only [liquidate]; omega
  · simp only [liquidate]; omega

/--
  **Property 2 (existence).** Repaying the borrower's entire share balance drives
  `borrowShares` to 0, which is healthy by the zero-debt branch — so a liquidation
  can always restore health. (The `1/LIF` threshold is what makes this achievable
  by a *partial*, bad-debt-free liquidation; see below.)
-/
theorem liquidation_can_restore_health (s : HealthState) (seized : Nat) :
    healthy (liquidate s s.borrowShares seized) :=
  healthy_of_no_debt (by simp [liquidate])

/-- Collateral seized to repay `repaidShares`, from the `repaidShares`-specified
    branch of `liquidate` (Contract.lean:876-878):
    `repaidAssets = ⌊shares ⋅ index⌋`, `seizedValue = ⌊repaidAssets ⋅ LIF / WAD⌋`,
    `seized = ⌊seizedValue ⋅ 1e36 / price⌋` — every step rounded *down*. -/
def seizedAssets (s : HealthState) (repaidShares : Nat) : Nat :=
  mulDivDown
    (mulDivDown
      (mulDivDown repaidShares (s.totBorrowAssets + VIRTUAL_ASSETS)
        (s.totBorrowShares + VIRTUAL_SHARES))
      (LIF s.lltv) WAD)
    ORACLE_PRICE_SCALE s.price

/--
  **Affordability / no bad debt.** Under `LTV < 1/LIF`, fully repaying the
  position seizes no more collateral than the borrower has: `seized ≤ collateral`.
  So the `require(collateral ≥ seizedAssets)` at Contract.lean:885 passes — the
  full liquidation does not revert and leaves no bad debt.

  This is exactly where the `1/LIF` threshold pays off, and every rounding goes the
  favourable way. Writing `cq` for the quoted collateral and `T ≤ borrowed` for the
  floor-rounded repaid assets: `seized` is built by three floors, so
  `seized ⋅ price ≤ S ⋅ 1e36` with `S ⋅ WAD ≤ T ⋅ LIF ≤ borrowed ⋅ LIF < cq ⋅ WAD`,
  giving `S ≤ cq` and hence `seized ≤ collateral`. No case split, no extra
  hypotheses (when `price = 0` the seizure is `0`). -/
theorem full_liquidation_affordable (s : HealthState) (hltv : ltvBelowInvLif s) :
    seizedAssets s s.borrowShares ≤ s.collateral := by
  set cq := mulDivDown s.collateral s.price ORACLE_PRICE_SCALE with hcq
  set T := mulDivDown s.borrowShares (s.totBorrowAssets + VIRTUAL_ASSETS)
            (s.totBorrowShares + VIRTUAL_SHARES) with hT
  set S := mulDivDown T (LIF s.lltv) WAD with hS
  have hWAD : 0 < WAD := by unfold WAD; omega
  have hTb : T ≤ s.borrowed := mulDivDown_le_mulDivUp _ _ _
  have hSW : S * WAD ≤ T * LIF s.lltv := by
    rw [hS]; unfold mulDivDown; exact Nat.div_mul_le_self _ _
  have hScq : S ≤ cq := by
    have h1 : S * WAD ≤ s.borrowed * LIF s.lltv :=
      le_trans hSW (mul_le_mul_right' hTb _)
    have hlt : S * WAD < cq * WAD := lt_of_le_of_lt h1 hltv
    exact le_of_lt (lt_of_mul_lt_mul_right hlt (Nat.zero_le _))
  have hSO : S * ORACLE_PRICE_SCALE ≤ s.collateral * s.price := by
    calc S * ORACLE_PRICE_SCALE
        ≤ cq * ORACLE_PRICE_SCALE := mul_le_mul_right' hScq _
      _ ≤ s.collateral * s.price := by rw [hcq]; unfold mulDivDown; exact Nat.div_mul_le_self _ _
  show mulDivDown S ORACLE_PRICE_SCALE s.price ≤ s.collateral
  unfold mulDivDown
  rcases Nat.eq_zero_or_pos s.price with hp | hp
  · simp [hp]
  · calc S * ORACLE_PRICE_SCALE / s.price
        ≤ s.collateral * s.price / s.price := Nat.div_le_div_right hSO
      _ = s.collateral := Nat.mul_div_cancel _ hp

/--
  **Property 2 (sharp), proved.** Under `LTV < 1/LIF` the *full* liquidation is
  both feasible (`seized ≤ collateral`, no revert / no bad debt) and health-
  restoring (it drives `borrowShares` to `0`). No obligation remains.

  The earlier *partial* (`repaidShares < borrowShares`) phrasing is unprovable as
  stated: shares are indivisible, so at `borrowShares = 1` the only partial repay is
  `0`, which cannot restore an unhealthy position. Such a state exists (e.g.
  `borrowShares = 1`, `collateral = 120`, `totBorrowAssets = 99999999`,
  `totBorrowShares = 0`, `lltv = 0.8e18`, `price = 1e36`: `borrowed = 100`,
  `maxBorrow = 96 < 100` so unhealthy, yet `borrowed ⋅ LIF ≈ 106.4e18 < 120e18`), so
  the right invariant is *full*-liquidation feasibility, not a partial witness. -/
theorem sharp_property2 (s : HealthState) (hltv : ltvBelowInvLif s) :
    seizedAssets s s.borrowShares ≤ s.collateral ∧
      healthy (liquidate s s.borrowShares (seizedAssets s s.borrowShares)) :=
  ⟨full_liquidation_affordable s hltv, healthy_of_no_debt (by simp [liquidate])⟩

/-- The borrower-side effect of `liquidate` in the **bad-debt** branch
    (Contract.lean:900-911): when the seizure empties the position's collateral
    (`newCollateral == 0`), the contract zeroes the position's remaining borrow
    shares (`setStructMember2 ... "borrowShares" ZERO`, Contract.lean:905) and
    socializes the loss to the market totals. The position's own debt is wiped:
    `borrowShares := 0`, `collateral := 0`. -/
def liquidateBadDebt (s : HealthState) (seized : Nat) : HealthState :=
  { s with borrowShares := 0, collateral := s.collateral - seized }

/-- **Bad-debt branch ends healthy.** When collateral is fully seized and debt
    remains, the contract sets the position's `borrowShares` to `0`
    (Contract.lean:905), so the liquidated position is healthy by the zero-debt
    short-circuit — regardless of how much debt is socialized to the market. -/
theorem liquidateBadDebt_healthy (s : HealthState) (seized : Nat) :
    healthy (liquidateBadDebt s seized) :=
  healthy_of_no_debt (by simp [liquidateBadDebt])

/--
  **#4 — both liquidation branches restore the liquidated position's health.**
  Whether the liquidation leaves no bad debt (full repay drives shares to `0`,
  `liquidation_can_restore_health`) or socializes bad debt (collateral emptied,
  shares zeroed at Contract.lean:905, `liquidateBadDebt_healthy`), the liquidated
  position ends healthy. The market-level bad-debt accounting (the loss moved off
  `totalSupplyAssets`, Contract.lean:908-911) is a separate market invariant, not a
  position-health claim, so it is out of scope for Property 2.
-/
theorem liquidation_restores_health_both_branches (s : HealthState) (seized : Nat) :
    healthy (liquidate s s.borrowShares seized) ∧ healthy (liquidateBadDebt s seized) :=
  ⟨liquidation_can_restore_health s seized, liquidateBadDebt_healthy s seized⟩

/-- A concrete witness that the *partial* phrasing fails: an unhealthy position with
    `borrowShares = 1` satisfying `LTV < 1/LIF`. `borrowed = 100`, `maxBorrow = 96`
    (so unhealthy), and `borrowed ⋅ LIF ≈ 106.4e18 < 120e18` (so `ltvBelowInvLif`).
    The market totals are well-formed (`borrowShares ≤ totBorrowShares`, so the
    position does not hold more shares than the whole market exists with), to show
    the failure is not an artifact of an unreachable state. -/
def partialCounterexample : HealthState :=
  { borrowShares := 1, collateral := 120, totBorrowAssets := 99999999,
    totBorrowShares := 1, lltv := 800000000000000000, price := ORACLE_PRICE_SCALE }

/-- **Counterexample to the partial phrasing.** `partialCounterexample` satisfies
    the `1/LIF` hypothesis and is unhealthy, yet no *strict-partial* liquidation
    (`repaidShares < borrowShares`) can restore it: with `borrowShares = 1` the only
    partial repayment is `0`, which merely seizes collateral and cannot help. This is
    why `sharp_property2` is stated for the *full* liquidation. -/
theorem partial_phrasing_fails :
    ltvBelowInvLif partialCounterexample ∧ ¬ healthy partialCounterexample ∧
      ¬ ∃ repaidShares seized,
          repaidShares < partialCounterexample.borrowShares ∧
            healthy (liquidate partialCounterexample repaidShares seized) := by
  refine ⟨?_, ?_, ?_⟩
  · norm_num [ltvBelowInvLif, HealthState.borrowed, partialCounterexample, mulDivUp,
      mulDivDown, LIF, MAX_LIF, CURSOR, WAD, ORACLE_PRICE_SCALE, VIRTUAL_ASSETS,
      VIRTUAL_SHARES]
  · unfold healthy
    push_neg
    refine ⟨by simp [partialCounterexample], ?_⟩
    norm_num [HealthState.borrowed, HealthState.maxBorrow, partialCounterexample,
      mulDivUp, mulDivDown, ORACLE_PRICE_SCALE, WAD, VIRTUAL_ASSETS, VIRTUAL_SHARES]
  · rintro ⟨r, seized, hr, hh⟩
    have hr0 : r = 0 := by simp [partialCounterexample] at hr; omega
    subst hr0
    have hbor : (liquidate partialCounterexample 0 seized).borrowed = 100 := by
      norm_num [liquidate, partialCounterexample, HealthState.borrowed, mulDivUp,
        VIRTUAL_ASSETS, VIRTUAL_SHARES]
    have hmb : (liquidate partialCounterexample 0 seized).maxBorrow ≤ 96 := by
      have hstep : (liquidate partialCounterexample 0 seized).maxBorrow
          ≤ partialCounterexample.maxBorrow := by
        unfold HealthState.maxBorrow
        have hc : (liquidate partialCounterexample 0 seized).collateral
            ≤ partialCounterexample.collateral := by simp [liquidate, partialCounterexample]
        have hp : (liquidate partialCounterexample 0 seized).price
            = partialCounterexample.price := by simp [liquidate]
        have hl : (liquidate partialCounterexample 0 seized).lltv
            = partialCounterexample.lltv := by simp [liquidate]
        rw [hp, hl]
        exact mulDivDown_mono_left (mulDivDown_mono_left hc)
      have hcex : partialCounterexample.maxBorrow = 96 := by
        norm_num [HealthState.maxBorrow, partialCounterexample, mulDivDown,
          ORACLE_PRICE_SCALE, WAD]
      omega
    rcases hh with h0 | hge
    · simp [liquidate, partialCounterexample] at h0
    · omega

end Morpho.Proofs.Property2
