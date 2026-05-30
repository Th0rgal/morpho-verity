/-
  Env — the trusted environmental assumptions under which the health properties hold.

  Both properties from morpho-blue (PR #684 and the liquidation-restores-health
  property) are stated "at constant prices, without considering debt accrual".
  Those two qualifiers are the *only* trusted inputs to the proofs; we make them
  first-class here so every theorem names exactly what it relies on.

  - constPrice : the oracle reading does not change across the operation.
  - noAccrual  : `_accrueInterest` is the identity on the health projection
                 (equivalently elapsed = 0 or borrowRate = 0). Absent accrual,
                 the market's borrow index `totBorrowAssets / totBorrowShares`
                 cannot increase, which is the crux of property 1.
-/

import Morpho.Proofs.HealthModel

namespace Morpho.Proofs.Env

open Morpho.Proofs.HealthModel

/-- A step relates a pre-state to a post-state of the health projection. -/
abbrev Step := HealthState → HealthState → Prop

/-- The price field is preserved by the step. -/
def ConstPrice (step : Step) : Prop :=
  ∀ s s', step s s' → s'.price = s.price

/--
  No accrual: the step does not increase the market borrow index.

  We encode "the index does not grow" as a cross-multiplied inequality to avoid
  rational division, mirroring how the contract keeps the index in two `uint128`
  fields. `a/b` non-increasing to `a'/b'` ⇔ `a' * b ≤ a * b'`.
-/
def NoAccrual (step : Step) : Prop :=
  ∀ s s', step s s' →
    (s'.totBorrowAssets + VIRTUAL_ASSETS) * (s.totBorrowShares + VIRTUAL_SHARES)
      ≤ (s.totBorrowAssets + VIRTUAL_ASSETS) * (s'.totBorrowShares + VIRTUAL_SHARES)

/-- The environment under which property 1 is proved. -/
structure Assumptions (step : Step) : Prop where
  constPrice : ConstPrice step
  noAccrual  : NoAccrual step

end Morpho.Proofs.Env
