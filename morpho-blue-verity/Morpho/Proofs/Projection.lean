/-
  Projection - reading a `HealthState` out of the real contract state.

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
    health test with `mulDiv512Up` for the borrow conversion and `mulDivDown` for
    collateral quoting; the model evaluates the same rounded formulas over `Nat`.
    Share/asset quotient fit is derived from packed `uint128` fields, while
    collateral-price multiplication remains an explicit oracle-price domain.

  - The price word. `_isHealthyWithPrice` takes the oracle price as an argument;
    the executable contract obtains it through an `ecmCall`, but the current
    Verity runtime state no longer stores ECM results. `project` takes the same
    word as an explicit argument so the model and the contract test the *same*
    assumed price.
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
  - `price` is the oracle word, supplied as an explicit external-world
    assumption.
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
  **Named theorem surface: arithmetic/layout faithfulness of the health test.**

  The contract's `_isHealthyWithPrice` returns a `Bool`; the model's `healthy`
  returns a `Prop`. This says they agree on every state where the contract test
  does not revert: the returned bool is `true` exactly when the projected state
  is `healthy`.

  This is the one place the `Nat` model meets the `Uint256` contract arithmetic.
  It holds on the no-overflow domain documented in `HealthModel.lean` and is
  proved by `HealthFaithful.healthFaithful_of_noOverflow`; this projection module
  only names the contract/model agreement surface. -/
def HealthFaithful (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState) : Prop :=
  ∀ v cs', (_isHealthyWithPrice mp id account price).run cs
      = ContractResult.success v cs' →
    (v = true ↔ healthy (project mp id account price cs))

theorem runWord_structMember2At_eq_of_storage_slot
    {κ₁ κ₂ : Type} [Contracts.StorageKey κ₁] [Contracts.StorageKey κ₂]
    (base wo : Nat) (packed : Option (Nat × Nat)) (k1 : κ₁) (k2 : κ₂)
    (cs cs' : ContractState)
    (hslot :
      cs'.storage (Contracts.structSlot2 base (Contracts.StorageKey.toWord k1)
        (Contracts.StorageKey.toWord k2) wo) =
      cs.storage (Contracts.structSlot2 base (Contracts.StorageKey.toWord k1)
        (Contracts.StorageKey.toWord k2) wo)) :
    runWord (Contracts.structMember2At base wo packed k1 k2 : Contract Uint256) cs' =
      runWord (Contracts.structMember2At base wo packed k1 k2 : Contract Uint256) cs := by
  unfold runWord Contracts.structMember2At
  simp [hslot]

end Morpho.Proofs.Projection
