/-
  Projection — reading a `HealthState` out of the real contract state.

  `HealthModel.HealthState` is the projection the health predicate depends on.
  Until now it floated free of the generated contract: the refinement step in
  `Refinement.lean` was stated against an abstract `Step`, with no link to the
  executable `verity_contract Morpho` bodies. This file builds that link.

  `project` reads the same storage words the contract's own `_isHealthyWithPrice`
  reads, through the same generated accessors (`structMember`, `structMember2`).
  It is therefore faithful to the contract's storage layout by construction: it
  cannot drift to a different slot or packing, because it calls the generated
  reader rather than re-deriving the address.

  Two gaps remain, and are named here rather than hidden:

  - `HealthFaithful` (Uint256 ↔ Nat arithmetic). The contract evaluates the
    health test with `MathLib.mulDivUp` / `mulDivDown` over `Uint256` (mod 2^256);
    the model evaluates it over `Nat`. On the no-overflow domain the two agree
    (see the header of `HealthModel.lean`); off it the contract reverts. This
    equivalence is the documented no-overflow trust boundary, checked by the
    differential suite, not re-proved here.

  - The price word. `_isHealthyWithPrice` takes the oracle price as an argument;
    the contract obtains it through an `ecmCall` reading the ECM environment
    (`ContractState.ecmResults`). `project` takes the same word as an argument so
    the model and the contract test the *same* price.
-/

import Morpho.Proofs.HealthModel
import Morpho.Contract

namespace Morpho.Proofs.Projection

open Verity
open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Contract.Morpho (MarketParams structMember structMember2 _isHealthyWithPrice)

/-- The value a storage read returns, ignoring the (unreachable) revert branch.
    Storage reads never revert, so this is total on every reachable state. -/
def runWord (c : Contract Uint256) (cs : ContractState) : Uint256 :=
  match c cs with
  | ContractResult.success v _ => v
  | ContractResult.revert _ _  => 0

/--
  Project the contract state onto the health-relevant fields of one position.

  Each field is read through the *generated* contract accessor for that slot, so
  the projection follows the contract's storage layout automatically:

  - `borrowShares` / `collateral` are the position fields `(id, account)`;
  - the two market totals are shared across positions in market `id`;
  - `lltv` is the market parameter the contract passes to `_isHealthyWithPrice`
    (a call argument, not storage);
  - `price` is the oracle word, supplied by the ECM environment.
-/
def project (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState) : HealthState :=
  { borrowShares    := (runWord (structMember2 "positionSlot" id account "borrowShares") cs).val
    collateral      := (runWord (structMember2 "positionSlot" id account "collateral") cs).val
    totBorrowAssets := (runWord (structMember "marketSlot" id "totalBorrowAssets") cs).val
    totBorrowShares := (runWord (structMember "marketSlot" id "totalBorrowShares") cs).val
    lltv            := mp.lltv.val
    price           := price.val }

/--
  **Named obligation: arithmetic/layout faithfulness of the health test.**

  The contract's `_isHealthyWithPrice` returns a `Bool`; the model's `healthy`
  returns a `Prop`. This says they agree on every state where the contract test
  does not revert: the returned bool is `true` exactly when the projected state
  is `healthy`.

  This is the one place the `Nat` model meets the `Uint256` contract arithmetic.
  It holds on the no-overflow domain documented in `HealthModel.lean` and is
  guarded empirically by the differential tests. We state it as a hypothesis so
  every theorem that uses it names the dependency, rather than absorbing it
  silently. -/
def HealthFaithful (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState) : Prop :=
  ∀ v cs', (_isHealthyWithPrice mp id account price).run cs
      = ContractResult.success v cs' →
    (v = true ↔ healthy (project mp id account price cs))

end Morpho.Proofs.Projection
