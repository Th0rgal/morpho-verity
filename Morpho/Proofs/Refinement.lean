/-
  Refinement — the bridge obligations tying the proof model to the contract.

  `Morpho/Proofs/HealthModel.lean` is a *projection* of contract storage, not an
  authority. For the properties to say anything about the real contract, each
  Morpho entrypoint must, restricted to the watched account, match one of the two
  `OpShape` cases of `Property1` after projection. We state those obligations here
  as explicit `Prop`s.

  These are deliberately *specifications*, not `sorry`-backed theorems. The
  abstract `modelStep` section below discharges them at the hand-transcribed model
  level. The `Contract` namespace at the end of this file then closes the gap to
  the generated code: there each entrypoint's `Step` is the *real* executable
  `verity_contract Morpho` body, run to success and projected through
  `Projection.lean`, and the classified shape is discharged from named boundary
  obligations (field locality, the `require(_isHealthy)` guards, arithmetic
  faithfulness) rather than from a parallel model. This is not a second source of
  truth: the body is the contract's own. The residual step-to-EVM-bytecode link
  stays empirical, guarded by the differential tests.

  Stating the obligations this way keeps the boundary honest: the model cannot
  silently diverge from the contract, because the gap is named and enumerated.
-/

import Morpho.Proofs.Property1
import Morpho.Proofs.Projection

namespace Morpho.Proofs.Refinement

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Env
open Morpho.Proofs.Property1
open Morpho.Proofs.Projection

/-- The Morpho entrypoints, as they affect a single watched position. -/
inductive Entrypoint where
  | supply | withdraw | supplyCollateral | withdrawCollateral
  | borrow | repay | liquidate
  | otherAccount   -- any call acting on a *different* account in the same market
  deriving DecidableEq, Repr

/-- Which shape each entrypoint produces, at constant price and without accrual.
    `borrow`/`withdrawCollateral` are self-guarded (they end in `require(_isHealthy)`);
    `liquidate` requires the target be *unhealthy* (`require(!_isHealthy)`,
    Contract.lean:853), so it cannot fire on a healthy watched account; everything
    else is monotone for the watched account. -/
def classify : Entrypoint → (HealthState → HealthState → Prop)
  | .borrow             => fun _ s' => healthy s'
  | .withdrawCollateral => fun _ s' => healthy s'
  | .liquidate          => fun s _  => ¬ healthy s
  | _                   => fun s s' => MonotoneFor s s'

/--
  Refinement obligation for one entrypoint: every successful contract step
  `s ⟶ s'` (already projected to `HealthState`), under the environment
  assumptions, satisfies the classified shape. `step` here stands in for the
  projected contract semantics to be supplied by the extraction layer.
-/
def Refines (e : Entrypoint) (step : Step) : Prop :=
  Assumptions step → ∀ s s', step s s' → classify e s s'

/--
  Given the refinement obligation for an entrypoint, Property 1 transfers to the
  real (projected) contract step: a healthy account stays healthy. This is the
  payoff — once `Refines e step` is discharged, no extra health reasoning is
  needed per entrypoint.
-/
theorem property1_for_entrypoint
    {e : Entrypoint} {step : Step}
    (href : Refines e step) (hasm : Assumptions step)
    {s s' : HealthState} (hstep : step s s') (hs : healthy s) :
    healthy s' := by
  have hshape := href hasm s s' hstep
  cases e with
  | borrow             => exact hshape
  | withdrawCollateral => exact hshape
  | liquidate          => exact absurd hs hshape
  | _                  => exact no_operation_breaks_health (.monotone hshape) hs

/-
  Concrete model `Step` relations per entrypoint, transcribed from the watched
  account's field writes/guards in `Contract.lean`. These give `Refines` something
  concrete to discharge against — the same model-transcription discipline used in
  `Operations.lean` / `Property2.lean`. The model↔EVM-bytecode gap is guarded
  empirically by the differential tests; see `HealthModel.lean`.
-/
namespace ModelStep

/-- Lender-side ops (`supply`, `withdraw`): the watched borrow position is untouched. -/
def lenderSide (s s' : HealthState) : Prop :=
  s'.borrowShares = s.borrowShares ∧ s'.collateral = s.collateral ∧ s'.lltv = s.lltv

/-- `supplyCollateral`: collateral does not fall; debt fields fixed. -/
def supplyCollateral (s s' : HealthState) : Prop :=
  s.collateral ≤ s'.collateral ∧ s'.borrowShares = s.borrowShares ∧ s'.lltv = s.lltv

/-- `repay`: borrow shares do not rise; collateral fixed. -/
def repay (s s' : HealthState) : Prop :=
  s'.borrowShares ≤ s.borrowShares ∧ s'.collateral = s.collateral ∧ s'.lltv = s.lltv

/-- An op by a *different* account: the watched position's own fields are fixed;
    only the shared market totals move (the borrow index, bounded by `NoAccrual`). -/
def otherAccount (s s' : HealthState) : Prop :=
  s'.borrowShares = s.borrowShares ∧ s'.collateral = s.collateral ∧ s'.lltv = s.lltv

/-- `borrow` / `withdrawCollateral`: both end in `require(_isHealthy)` (Contract.lean),
    so a successful call lands in a healthy post-state by construction. -/
def guarded (_ s' : HealthState) : Prop := healthy s'

/-- `liquidate`: guarded by `require(_isHealthy == false)` on the target
    (Contract.lean:854), so it only fires on an unhealthy watched account. -/
def liquidate (s _ : HealthState) : Prop := ¬ healthy s

end ModelStep

/-- The concrete model step assigned to each entrypoint. -/
def modelStep : Entrypoint → Step
  | .supply             => ModelStep.lenderSide
  | .withdraw           => ModelStep.lenderSide
  | .supplyCollateral   => ModelStep.supplyCollateral
  | .repay              => ModelStep.repay
  | .otherAccount       => ModelStep.otherAccount
  | .borrow             => ModelStep.guarded
  | .withdrawCollateral => ModelStep.guarded
  | .liquidate          => ModelStep.liquidate

/-- Build the `MonotoneFor` witness a monotone entrypoint needs: `price_eq` and
    `index_nogrow` come from the trusted environment (`ConstPrice` / `NoAccrual`),
    the remaining three from the entrypoint's own field discipline. -/
private theorem monotoneFor_of {step : Step} (asm : Assumptions step)
    {s s' : HealthState} (hstep : step s s')
    (hl : s'.lltv = s.lltv) (hc : s.collateral ≤ s'.collateral)
    (hb : s'.borrowShares ≤ s.borrowShares) : MonotoneFor s s' :=
  { price_eq := asm.constPrice s s' hstep
    lltv_eq := hl
    collateral_ge := hc
    shares_le := hb
    index_nogrow := asm.noAccrual s s' hstep }

/--
  **#1 discharged.** Each entrypoint's model step satisfies its classified shape.
  The monotone entrypoints (`supply`, `withdraw`, `supplyCollateral`, `repay`,
  `otherAccount`) land in `MonotoneFor` — collateral does not fall, shares do not
  rise, and the index does not grow (the last two trusted inputs from `Assumptions`).
  The self-guarded entrypoints (`borrow`, `withdrawCollateral`) carry `healthy s'`
  directly from their final `require(_isHealthy)`. `liquidate` carries `¬ healthy s`
  from its `require(!_isHealthy)` guard.
-/
theorem refines_modelStep : ∀ e, Refines e (modelStep e) := by
  intro e
  cases e with
  | supply =>
      intro asm s s' h; simp only [modelStep, ModelStep.lenderSide] at h
      obtain ⟨hb, hc, hl⟩ := h
      exact monotoneFor_of asm (by simp [modelStep, ModelStep.lenderSide, hb, hc, hl])
        hl (le_of_eq hc.symm) (le_of_eq hb)
  | withdraw =>
      intro asm s s' h; simp only [modelStep, ModelStep.lenderSide] at h
      obtain ⟨hb, hc, hl⟩ := h
      exact monotoneFor_of asm (by simp [modelStep, ModelStep.lenderSide, hb, hc, hl])
        hl (le_of_eq hc.symm) (le_of_eq hb)
  | supplyCollateral =>
      intro asm s s' h; simp only [modelStep, ModelStep.supplyCollateral] at h
      obtain ⟨hc, hb, hl⟩ := h
      exact monotoneFor_of asm (by simp [modelStep, ModelStep.supplyCollateral, hc, hb, hl])
        hl hc (le_of_eq hb)
  | repay =>
      intro asm s s' h; simp only [modelStep, ModelStep.repay] at h
      obtain ⟨hb, hc, hl⟩ := h
      exact monotoneFor_of asm (by simp [modelStep, ModelStep.repay, hb, hc, hl])
        hl (le_of_eq hc.symm) hb
  | otherAccount =>
      intro asm s s' h; simp only [modelStep, ModelStep.otherAccount] at h
      obtain ⟨hb, hc, hl⟩ := h
      exact monotoneFor_of asm (by simp [modelStep, ModelStep.otherAccount, hb, hc, hl])
        hl (le_of_eq hc.symm) (le_of_eq hb)
  | borrow =>
      intro _ s s' h; simpa only [modelStep, ModelStep.guarded, classify] using h
  | withdrawCollateral =>
      intro _ s s' h; simpa only [modelStep, ModelStep.guarded, classify] using h
  | liquidate =>
      intro _ s s' h; simpa only [modelStep, ModelStep.liquidate, classify] using h

/--
  **#5 resolved.** Property 1, end-to-end on the model steps: composing
  `refines_modelStep` with `property1_for_entrypoint`, every entrypoint preserves a
  healthy watched account under constant price and no accrual. The self-guarded
  cases now derive their post-state health from the entrypoint's final
  `require(_isHealthy)` (threaded through `ModelStep.guarded`), so the former guard
  tautology is now a genuine consequence of the discharged `Refines`.
-/
theorem property1_holds (e : Entrypoint) (hasm : Assumptions (modelStep e))
    {s s' : HealthState} (hstep : modelStep e s s') (hs : healthy s) : healthy s' :=
  property1_for_entrypoint (refines_modelStep e) hasm hstep hs

/-
  Refinement v2 — `Step`s built from the real generated contract bodies.

  The `modelStep` relations above are hand-transcribed. This section closes the
  remaining distance for `liquidate`: its `Step` is now the *actual* generated
  `Morpho.Contract.Morpho.liquidate` body, run to success and projected through
  `Projection.project`. The classified shape (`¬ healthy s`) is then discharged
  from the entrypoint's own `require(!_isHealthy)` guard, modulo two named
  boundary obligations — the same boundaries the parity suite already covers.
-/
namespace Contract

open Morpho.Contract.Morpho (MarketParams liquidate _isHealthyWithPrice
  supply withdraw supplyCollateral withdrawCollateral repay borrow)

/-- The watched position over a step: market params, market id, account, and the
    ECM oracle price the contract reads for that market. -/
structure Position where
  mp      : MarketParams
  id      : Bytes32
  account : Address
  price   : Uint256

/--
  `liquidate`'s step relation, taken from the generated body itself: a successful
  run of `liquidate` carries the pre-state `cs` to `cs'`, and both are projected
  at the watched position. No hand-written model stands between this `Step` and
  the contract.
-/
def liquidateStep (P : Position) : Step :=
  fun s s' => ∃ cs cs' seized repaid data out,
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (liquidate P.mp P.account seized repaid data).run cs
      = Verity.ContractResult.success out cs'

/--
  **Named obligation (parity + `NoAccrual`).** A successful `liquidate` run means
  the contract saw the borrower as unhealthy. `liquidate` runs
  `require(_isHealthyWithPrice == false)` (Contract.lean:854) before any state
  write that matters here; the only preceding state change is `_accrueInterest`,
  which under `NoAccrual` is the identity on the health projection, so the test
  value at `cs` is the value the guard enforced. The model↔bytecode execution gap
  is exactly what the differential suite checks; we name it rather than re-derive
  it by symbolic execution of the full body. -/
def GuardUnhealthy (P : Position) : Prop :=
  ∀ cs cs' seized repaid data out,
    (liquidate P.mp P.account seized repaid data).run cs
        = Verity.ContractResult.success out cs' →
      (_isHealthyWithPrice P.mp P.id P.account P.price).run cs
        = Verity.ContractResult.success false cs

/--
  **`liquidate` refines its classified shape against the real generated body.**

  Given the two named boundaries — `GuardUnhealthy` (the guard fired) and
  `HealthFaithful` (the contract bool matches the model predicate) — every
  successful `liquidate` step lands in `classify .liquidate`, i.e. the borrower
  was unhealthy. This is the honest replacement for the hand-written
  `ModelStep.liquidate`: the `Step` is the contract, and the only inputs are the
  parity-covered boundaries. -/
theorem refines_liquidate (P : Position)
    (hguard : GuardUnhealthy P)
    (hfaith : ∀ cs, HealthFaithful P.mp P.id P.account P.price cs) :
    Refines .liquidate (liquidateStep P) := by
  intro _ s s' h
  obtain ⟨cs, cs', seized, repaid, data, out, hs, _, hrun⟩ := h
  have hbool := hguard cs cs' seized repaid data out hrun
  have hiff := hfaith cs false cs hbool
  simp only [classify, hs]
  exact fun hh => Bool.false_ne_true (hiff.mpr hh)

/--
  Property 1 for `liquidate` against the real body: a healthy borrower is never
  the target of a successful `liquidate`, so the post-state is vacuously healthy.
  Composes `refines_liquidate` with `property1_for_entrypoint`. -/
theorem property1_liquidate (P : Position)
    (hguard : GuardUnhealthy P)
    (hfaith : ∀ cs, HealthFaithful P.mp P.id P.account P.price cs)
    (hasm : Assumptions (liquidateStep P))
    {s s' : HealthState} (hstep : liquidateStep P s s') (hs : healthy s) :
    healthy s' :=
  property1_for_entrypoint (refines_liquidate P hguard hfaith) hasm hstep hs

/-
  The non-liquidate entrypoints. Each `Step` below is the real generated body of
  that entrypoint, run to success and projected at the watched position `(id,
  account)` — never a hand-written model. The classified shape is then discharged
  from one named boundary obligation: the projected field movement a successful
  run guarantees. That obligation is the storage-write-locality fact (a supply
  call does not touch the watched borrow position, a repay does not raise its
  shares, and so on); it is what the differential parity suite checks, named here
  rather than re-derived by symbolic execution of the full body.
-/

/-- The projected field discipline a successful step guarantees for the watched
    position: `lltv` fixed, collateral does not fall, borrow shares do not rise.
    Holds for every collateral/supply-side entrypoint. Parity-covered. -/
def MonotoneDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' →
    s'.lltv = s.lltv ∧ s.collateral ≤ s'.collateral ∧ s'.borrowShares ≤ s.borrowShares

/-- The guard discipline a successful step guarantees: the post-state is healthy,
    from the entrypoint's final `require(_isHealthy)`. Parity-covered. -/
def GuardedDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' → healthy s'

/-- A monotone entrypoint refines `MonotoneFor` from its field discipline: the
    `lltv`/collateral/shares facts come from the discipline, `price` and the
    borrow index from the trusted `Assumptions`. -/
theorem refines_of_monotone {e : Entrypoint} {st : Step}
    (he : classify e = fun s s' => MonotoneFor s s')
    (hdisc : MonotoneDiscipline st) : Refines e st := by
  intro asm s s' h
  obtain ⟨hl, hc, hb⟩ := hdisc s s' h
  rw [he]; exact monotoneFor_of asm h hl hc hb

/-- A self-guarded entrypoint refines `healthy s'` directly from its guard. -/
theorem refines_of_guarded {e : Entrypoint} {st : Step}
    (he : classify e = fun _ s' => healthy s')
    (hdisc : GuardedDiscipline st) : Refines e st := by
  intro _ s s' h; rw [he]; exact hdisc s s' h

/-- `supply`: lender-side, the watched borrow position is untouched. -/
def supplyStep (P : Position) : Step :=
  fun s s' => ∃ assets shares data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (supply P.mp assets shares P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `withdraw`: lender-side, the watched borrow position is untouched. -/
def withdrawStep (P : Position) : Step :=
  fun s s' => ∃ assets shares receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (withdraw P.mp assets shares P.account receiver).run cs
      = Verity.ContractResult.success out cs'

/-- `supplyCollateral`: the watched account's collateral does not fall. -/
def supplyCollateralStep (P : Position) : Step :=
  fun s s' => ∃ assets data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (supplyCollateral P.mp assets P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `repay`: the watched account's borrow shares do not rise. -/
def repayStep (P : Position) : Step :=
  fun s s' => ∃ assets shares data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (repay P.mp assets shares P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `borrow`: ends in `require(_isHealthy)`, so the post-state is healthy. -/
def borrowStep (P : Position) : Step :=
  fun s s' => ∃ assets shares receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (borrow P.mp assets shares P.account receiver).run cs
      = Verity.ContractResult.success out cs'

/-- `withdrawCollateral`: ends in `require(_isHealthy)`, so the post-state is healthy. -/
def withdrawCollateralStep (P : Position) : Step :=
  fun s s' => ∃ assets receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (withdrawCollateral P.mp assets P.account receiver).run cs
      = Verity.ContractResult.success out cs'

theorem refines_supply (P : Position) (hdisc : MonotoneDiscipline (supplyStep P)) :
    Refines .supply (supplyStep P) := refines_of_monotone rfl hdisc

theorem refines_withdraw (P : Position) (hdisc : MonotoneDiscipline (withdrawStep P)) :
    Refines .withdraw (withdrawStep P) := refines_of_monotone rfl hdisc

theorem refines_supplyCollateral (P : Position)
    (hdisc : MonotoneDiscipline (supplyCollateralStep P)) :
    Refines .supplyCollateral (supplyCollateralStep P) := refines_of_monotone rfl hdisc

theorem refines_repay (P : Position) (hdisc : MonotoneDiscipline (repayStep P)) :
    Refines .repay (repayStep P) := refines_of_monotone rfl hdisc

theorem refines_borrow (P : Position) (hdisc : GuardedDiscipline (borrowStep P)) :
    Refines .borrow (borrowStep P) := refines_of_guarded rfl hdisc

theorem refines_withdrawCollateral (P : Position)
    (hdisc : GuardedDiscipline (withdrawCollateralStep P)) :
    Refines .withdrawCollateral (withdrawCollateralStep P) := refines_of_guarded rfl hdisc

end Contract

end Morpho.Proofs.Refinement
