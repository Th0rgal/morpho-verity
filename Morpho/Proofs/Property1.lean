/-
  Property1 — "at constant prices, without considering debt accrual, no operation
  can render unhealthy an account that was healthy." (morpho-blue PR #684)

  Structure of the proof (see the design in the PR description):

  Every Morpho entrypoint, restricted to a fixed watched account, falls into one
  of two shapes:

    * monotone — the account's collateral does not decrease and its borrow shares
      do not increase, and (absent accrual, at constant price) the shared borrow
      index does not grow. `healthy_preserved_of_monotone` proves health survives.
      This covers `supply`, `withdraw`, `supplyCollateral`, `repay`, and *every*
      operation performed by some *other* account (which only moves the shared
      index).

    * self-guarded — `borrow` and `withdrawCollateral` may reduce the account's
      own health, but both end in `require(_isHealthy)`. So whenever they succeed
      the post-state is healthy outright: `healthy_preserved_of_guard`.

  The arithmetic heart is `healthy_preserved_of_monotone`, proved below from the
  generic division lemmas in `Arith`.
-/

import Morpho.Proofs.Arith
import Morpho.Proofs.Env

namespace Morpho.Proofs.Property1

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Arith
open Morpho.Proofs.Env

/-- `a * b ≤ d * ⌈a·b/d⌉` for `d > 0`: ceiling division never undershoots. -/
theorem le_mul_mulDivUp (a b d : Nat) (hd : 0 < d) : a * b ≤ d * mulDivUp a b d := by
  unfold mulDivUp
  have h := Nat.div_add_mod (a * b + (d - 1)) d
  have hmod : (a * b + (d - 1)) % d < d := Nat.mod_lt _ hd
  omega

/-- A repayment or liquidation by *any* account lowers the market totals by
    `repaidShares` shares and the matching `repaidAssets`, where the asset side is
    the share-to-asset conversion *rounded up* (the `mulDivUp` at the `borrowed`
    formula, Morpho/Contract.lean:881). Because that conversion rounds up, the
    borrow index `(totBorrowAssets+1)/(totBorrowShares+VIRTUAL)` cannot grow. This
    discharges `borrowIndexNoGrow` for a third party's repay/liquidate *without*
    appealing to no-accrual: only `_accrueInterest` can actually grow the index. -/
theorem borrowIndexNoGrow_of_repay {s s' : HealthState} (repaidShares : Nat)
    (hsh : s'.totBorrowShares + repaidShares = s.totBorrowShares)
    (hass : s'.totBorrowAssets
              + mulDivUp repaidShares (s.totBorrowAssets + VIRTUAL_ASSETS)
                  (s.totBorrowShares + VIRTUAL_SHARES)
            = s.totBorrowAssets) :
    borrowIndexNoGrow s s' := by
  unfold borrowIndexNoGrow
  have hb_pos : 0 < s.totBorrowShares + VIRTUAL_SHARES := by
    have : 0 < VIRTUAL_SHARES := by decide
    omega
  set rA := mulDivUp repaidShares (s.totBorrowAssets + VIRTUAL_ASSETS)
              (s.totBorrowShares + VIRTUAL_SHARES) with hrA
  have hkey : repaidShares * (s.totBorrowAssets + VIRTUAL_ASSETS)
                ≤ (s.totBorrowShares + VIRTUAL_SHARES) * rA := by
    rw [hrA]; exact le_mul_mulDivUp _ _ _ hb_pos
  have e1 : s.totBorrowAssets = s'.totBorrowAssets + rA := by omega
  have e2 : s.totBorrowShares = s'.totBorrowShares + repaidShares := by omega
  rw [e1, e2] at hkey ⊢
  nlinarith [hkey]

/-- The watched account's storage changed monotonically in the health-favourable
    direction, at constant price/LLTV, with a non-growing borrow index. -/
structure MonotoneFor (s s' : HealthState) : Prop where
  price_eq      : s'.price = s.price
  lltv_eq       : s'.lltv = s.lltv
  collateral_ge : s.collateral ≤ s'.collateral
  shares_le     : s'.borrowShares ≤ s.borrowShares
  index_nogrow  : borrowIndexNoGrow s s'

/-- `maxBorrow` cannot decrease when collateral does not decrease and price/LLTV
    are unchanged. -/
theorem maxBorrow_mono {s s' : HealthState} (h : MonotoneFor s s') :
    s.maxBorrow ≤ s'.maxBorrow := by
  unfold maxBorrow
  rw [h.price_eq, h.lltv_eq]
  exact mulDivDown_mono_left (mulDivDown_mono_left h.collateral_ge)

/-- `borrowed` cannot increase when borrow shares do not increase and the index
    does not grow. Combines `mulDivUp_mono_left` (shares) with
    `ceilDiv_index_antitone` (index). -/
theorem borrowed_anti {s s' : HealthState} (h : MonotoneFor s s') :
    s'.borrowed ≤ s.borrowed := by
  unfold borrowed mulDivUp
  have hV : 0 < VIRTUAL_SHARES := by decide
  have hB  : 0 < s.totBorrowShares + VIRTUAL_SHARES := by
    have := hV; omega
  have hB' : 0 < s'.totBorrowShares + VIRTUAL_SHARES := by
    have := hV; omega
  -- shares step (same index A', B'), then index step (same shares s'.borrowShares)
  calc (s'.borrowShares * (s'.totBorrowAssets + VIRTUAL_ASSETS)
          + ((s'.totBorrowShares + VIRTUAL_SHARES) - 1))
            / (s'.totBorrowShares + VIRTUAL_SHARES)
      ≤ (s'.borrowShares * (s.totBorrowAssets + VIRTUAL_ASSETS)
          + ((s.totBorrowShares + VIRTUAL_SHARES) - 1))
            / (s.totBorrowShares + VIRTUAL_SHARES) :=
        ceilDiv_index_antitone hB hB' h.index_nogrow
    _ ≤ (s.borrowShares * (s.totBorrowAssets + VIRTUAL_ASSETS)
          + ((s.totBorrowShares + VIRTUAL_SHARES) - 1))
            / (s.totBorrowShares + VIRTUAL_SHARES) :=
        mulDivUp_mono_left h.shares_le

/-- **Monotone case.** A health-favourable change preserves health. -/
theorem healthy_preserved_of_monotone {s s' : HealthState}
    (h : MonotoneFor s s') (hs : healthy s) : healthy s' := by
  rcases Nat.eq_zero_or_pos s'.borrowShares with hz | hpos
  · exact Or.inl hz
  · refine Or.inr ?_
    -- s'.borrowShares > 0 ⇒ s.borrowShares > 0 (since s'.bs ≤ s.bs), so hs is the ≥ branch
    have hsbs : 0 < s.borrowShares := lt_of_lt_of_le hpos h.shares_le
    have hge : s.maxBorrow ≥ s.borrowed := by
      rcases hs with h0 | hh
      · omega
      · exact hh
    calc s'.borrowed ≤ s.borrowed := borrowed_anti h
      _ ≤ s.maxBorrow := hge
      _ ≤ s'.maxBorrow := maxBorrow_mono h

/-- **Self-guarded case.** `borrow` / `withdrawCollateral` end in
    `require(_isHealthy)`, so a successful call lands in a healthy state by
    construction — regardless of the prior state. -/
theorem healthy_preserved_of_guard {s' : HealthState} (hguard : healthy s') :
    healthy s' := hguard

/-- The two operation shapes a single Morpho call can take, projected onto the
    watched account. -/
inductive OpShape (s s' : HealthState) : Prop where
  | monotone (h : MonotoneFor s s')
  | guarded  (h : healthy s')

/--
  **Property 1.** For any operation whose effect on the watched account is either
  monotone (under constant price + no accrual) or self-guarded, a healthy account
  stays healthy.
-/
theorem no_operation_breaks_health {s s' : HealthState}
    (shape : OpShape s s') (hs : healthy s) : healthy s' := by
  cases shape with
  | monotone h => exact healthy_preserved_of_monotone h hs
  | guarded h  => exact h

end Morpho.Proofs.Property1
