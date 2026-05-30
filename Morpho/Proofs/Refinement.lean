/-
  Refinement — the bridge obligations tying the proof model to the contract.

  `Morpho/Proofs/HealthModel.lean` is a *projection* of contract storage, not an
  authority. For the properties to say anything about the real contract, each
  Morpho entrypoint must, restricted to the watched account, match one of the two
  `OpShape` cases of `Property1` after projection. We state those obligations here
  as explicit `Prop`s.

  These are deliberately *specifications*, not `sorry`-backed theorems: discharging
  them against `verity_contract Morpho` requires the execution-semantics extraction
  layer (it does not exist yet, by design — the contract is the single source of
  truth and no parallel executable model is permitted). Until that lands, faithful
  correspondence is additionally guarded empirically by the differential tests.

  Stating the obligations this way keeps the boundary honest: the model cannot
  silently diverge from the contract, because the gap is named and enumerated.
-/

import Morpho.Proofs.Property1

namespace Morpho.Proofs.Refinement

open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Env
open Morpho.Proofs.Property1

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

end Morpho.Proofs.Refinement
