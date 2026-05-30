/-
  HealthModel — proof-only projection of Morpho storage relevant to position health.

  This is NOT a second Morpho contract. It is proof infrastructure: the smallest
  projection of contract storage that the health predicate `_isHealthyWithPrice`
  (Morpho/Contract.lean) reads, together with that predicate transcribed verbatim.

  Faithfulness to the contract is an explicit obligation, discharged per-operation
  in `Morpho/Proofs/Refinement.lean`. Nothing here may be treated as authoritative
  Morpho semantics on its own.

  We model the health arithmetic over `Nat`. The contract computes the same values
  via `mulDivDown`/`mulDivUp` (Morpho/Libraries/MathLib.lean), which wrap through
  `Uint256.ofNat`; under the no-overflow side conditions carried by every
  state-mutating entrypoint (the uint128 `require`s) that wrapping is the identity,
  so the `Nat` model and the contract agree pointwise.
-/

namespace Morpho.Proofs.HealthModel

/-- Virtual shares offset (`VIRTUAL_SHARES` in SharesMathLib.sol). -/
def VIRTUAL_SHARES : Nat := 1000000

/-- Virtual assets offset (`VIRTUAL_ASSETS` in SharesMathLib.sol). -/
def VIRTUAL_ASSETS : Nat := 1

/-- WAD = 1e18. -/
def WAD : Nat := 1000000000000000000

/-- Oracle price scale = 1e36. -/
def ORACLE_PRICE_SCALE : Nat := 1000000000000000000000000000000000000

/-- `(a * b) / d`, rounded down — mirrors `MathLib.mulDivDown` on `Nat`. -/
def mulDivDown (a b d : Nat) : Nat := (a * b) / d

/-- `(a * b + (d - 1)) / d`, rounded up — mirrors `MathLib.mulDivUp` on `Nat`. -/
def mulDivUp (a b d : Nat) : Nat := (a * b + (d - 1)) / d

/--
  Projection of the storage a single position's health depends on.

  `borrowShares` / `collateral` are the position fields `(id, account)`;
  the three market-level fields are shared across positions in market `id`;
  `price` is the oracle reading, held in the environment.
-/
structure HealthState where
  borrowShares    : Nat
  collateral      : Nat
  totBorrowAssets : Nat
  totBorrowShares : Nat
  lltv            : Nat
  price           : Nat
  deriving Repr, DecidableEq

namespace HealthState

/-- Outstanding borrowed assets attributed to the position, rounded up.
    Mirrors `borrowedAmt` at Morpho/Contract.lean:577. -/
def borrowed (s : HealthState) : Nat :=
  mulDivUp s.borrowShares (s.totBorrowAssets + VIRTUAL_ASSETS)
    (s.totBorrowShares + VIRTUAL_SHARES)

/-- Maximum borrow the collateral supports at the current price, rounded down.
    Mirrors `maxBorrow` at Morpho/Contract.lean:579-580. -/
def maxBorrow (s : HealthState) : Nat :=
  mulDivDown (mulDivDown s.collateral s.price ORACLE_PRICE_SCALE) s.lltv WAD

/--
  Health predicate, transcribed from `_isHealthyWithPrice` together with the
  `borrowShares == 0` short-circuit in `_isHealthy` (Morpho/Contract.lean:585-586):
  a position with no debt is vacuously healthy.
-/
def healthy (s : HealthState) : Prop :=
  s.borrowShares = 0 ∨ s.maxBorrow ≥ s.borrowed

/--
  The market borrow index `(totBorrowAssets+1)/(totBorrowShares+VIRTUAL)` does not
  grow from `s` to `s'`. Stated cross-multiplied (`a'/b' ≤ a/b ⇔ a'·b ≤ a·b'`) to
  stay in `Nat`. In Morpho the index rises *only* via `_accrueInterest`, so under
  the no-accrual assumption this holds for every operation. -/
def borrowIndexNoGrow (s s' : HealthState) : Prop :=
  (s'.totBorrowAssets + VIRTUAL_ASSETS) * (s.totBorrowShares + VIRTUAL_SHARES)
    ≤ (s.totBorrowAssets + VIRTUAL_ASSETS) * (s'.totBorrowShares + VIRTUAL_SHARES)

/-- An operation that leaves the market totals untouched cannot grow the index.
    This discharges the index condition for every collateral/supply-side op. -/
theorem borrowIndexNoGrow_of_totals_eq {s s' : HealthState}
    (ha : s'.totBorrowAssets = s.totBorrowAssets)
    (hs : s'.totBorrowShares = s.totBorrowShares) : borrowIndexNoGrow s s' := by
  simp [borrowIndexNoGrow, ha, hs]

instance (s : HealthState) : Decidable (healthy s) := by
  unfold healthy; infer_instance

end HealthState

end Morpho.Proofs.HealthModel
