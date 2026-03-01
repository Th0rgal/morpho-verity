/-!
# Semantic Bridge Discharge: Link 1 (Pure Lean ↔ EDSL)

This module proves the first link of the semantic bridge discharge chain:
the EDSL functions in `MacroSlice.lean` (produced by `verity_contract`) are
equivalent to the pure Lean model functions in `Morpho.lean`.

## Architecture

The full discharge chain for an obligation like `setOwnerSemEq` has three links:

```
   Pure Lean (Morpho.setOwner)          -- this repo, Morpho.lean
     ↕ Link 1 (this file)
   EDSL (MorphoViewSlice.setOwner)      -- this repo, MacroSlice.lean
     ↕ Link 2 (verity SemanticBridge)
   Compiled IR (interpretIR morphoIR)   -- verity, once compile pipeline lands
     ↕ Link 3 (verity EndToEnd)
   EVMYulLean (Yul execution)           -- verity, 1 keccak axiom
```

This file proves Link 1 for `setOwner` and `setFeeRecipient`.
Links 2+3 depend on upstream verity infrastructure (verity#998, #1052).

## Proof Strategy

For each function, we:
1. Define `encodeMorphoState : MorphoState → ContractState` matching MacroSlice storage layout
2. Define an EDSL-based wrapper: run `MorphoViewSlice.f` on encoded state, decode result
3. Prove the wrapper equals the pure Lean `Morpho.f`
4. Conclude `*SemEq` obligation is satisfiable
-/

import Morpho.Compiler.MacroSlice
import Morpho.Proofs.SolidityBridge

namespace Morpho.Proofs.SemanticBridgeDischarge

open Verity
open Morpho.Types
open Morpho.Compiler.MacroSlice

/-! ## State Encoding

The canonical encoding from `MorphoState` to `ContractState` matching the
storage layout declared in `MacroSlice.lean`:

| MorphoState field  | ContractState accessor | Slot |
|--------------------|------------------------|------|
| owner : Address    | storageAddr 0          | 0    |
| feeRecipient       | storageAddr 1          | 1    |
| isIrmEnabled       | storageMap 4           | 4    |
| isLltvEnabled      | storageMapUint 5       | 5    |
| isAuthorized       | storageMap2 6          | 6    |
| nonce              | storageMapUint 7       | 7    |
| sender : Address   | sender                 | —    |
-/

/-- Encode `MorphoState` to `ContractState` matching MacroSlice storage layout. -/
def encodeMorphoState (s : MorphoState) : ContractState := {
  storage := fun _ => 0
  storageAddr := fun slot =>
    if slot == 0 then s.owner
    else if slot == 1 then s.feeRecipient
    else 0
  storageMap := fun slot key =>
    if slot == 4 then (if s.isIrmEnabled key then 1 else 0)
    else 0
  storageMapUint := fun slot key =>
    if slot == 5 then (if s.isLltvEnabled key then 1 else 0)
    else if slot == 7 then s.nonce key
    else 0
  storageMap2 := fun slot key1 key2 =>
    if slot == 6 then (if s.isAuthorized key1 key2 then 1 else 0)
    else 0
  sender := s.sender
  thisAddress := 0
  msgValue := 0
  blockTimestamp := s.blockTimestamp
  knownAddresses := fun _ => Core.FiniteAddressSet.empty
  events := []
}

/-! ## Link 1: setOwner

The EDSL `MorphoViewSlice.setOwner` (MacroSlice.lean:128-133):
```
function setOwner (newOwner : Address) : Unit := do
  let sender <- msgSender
  let currentOwner <- getStorageAddr ownerSlot
  require (sender == currentOwner) "not owner"
  require (newOwner != currentOwner) "already set"
  setStorageAddr ownerSlot newOwner
```

The pure Lean `Morpho.setOwner` (Morpho.lean:124-127):
```
def setOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else if newOwner == s.owner then none
  else some { s with owner := newOwner }
```
-/

/-- Run the EDSL `setOwner` on encoded state and decode. This wraps the
    monadic EDSL execution as a pure `MorphoState → Address → Option MorphoState`
    function, which is exactly the type `SetOwnerSem` expects. -/
noncomputable def edslSetOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setOwner newOwner) state with
  | .success _ newState => some { s with owner := newState.storageAddr 0 }
  | .revert _ _ => none

/-- **Link 1 for setOwner**: The EDSL `setOwner` matches the pure Lean model.

Proof proceeds by unfolding the EDSL do-notation into its monadic chain
(`msgSender → getStorageAddr → require → require → setStorageAddr`) and
evaluating each step on `encodeMorphoState s`, then case-splitting on the
sender/owner equality conditions. -/
theorem setOwner_link1 :
    ∀ s newOwner, edslSetOwner s newOwner = Morpho.setOwner s newOwner := by
  intro s newOwner
  unfold edslSetOwner Morpho.setOwner encodeMorphoState
  -- Unfold the EDSL monadic chain: bind msgSender → bind getStorageAddr →
  -- bind require → bind require → setStorageAddr
  simp only [MorphoViewSlice.setOwner, MorphoViewSlice.ownerSlot,
    bind, Bind.bind, msgSender, getStorageAddr, setStorageAddr, require, pure, Pure.pure,
    ContractResult.success.injEq, true_and]
  -- After unfolding, the EDSL reduces to nested if-then-else matching Morpho.setOwner
  simp only [beq_iff_eq, bne_iff_ne, Bool.not_eq_true', decide_eq_true_eq, ite_not]
  split <;> simp_all

/-- The EDSL `setOwner` satisfies the semantic equivalence obligation.
    This discharges `setOwnerSemEq` for the EDSL-based implementation. -/
theorem setOwner_semEq :
    SolidityBridge.setOwnerSemEq edslSetOwner :=
  setOwner_link1

/-! ## Link 1: setFeeRecipient

The EDSL `MorphoViewSlice.setFeeRecipient` (MacroSlice.lean:135-141):
```
function setFeeRecipient (newFeeRecipient : Address) : Unit := do
  let sender <- msgSender
  let currentOwner <- getStorageAddr ownerSlot
  require (sender == currentOwner) "not owner"
  let currentFeeRecipient <- getStorageAddr feeRecipientSlot
  require (newFeeRecipient != currentFeeRecipient) "already set"
  setStorageAddr feeRecipientSlot newFeeRecipient
```
-/

/-- Run the EDSL `setFeeRecipient` on encoded state and decode. -/
noncomputable def edslSetFeeRecipient (s : MorphoState) (newFeeRecipient : Address) :
    Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setFeeRecipient newFeeRecipient) state with
  | .success _ newState => some { s with feeRecipient := newState.storageAddr 1 }
  | .revert _ _ => none

/-- **Link 1 for setFeeRecipient**: EDSL matches the pure Lean model. -/
theorem setFeeRecipient_link1 :
    ∀ s newFeeRecipient,
      edslSetFeeRecipient s newFeeRecipient = Morpho.setFeeRecipient s newFeeRecipient := by
  intro s newFeeRecipient
  unfold edslSetFeeRecipient Morpho.setFeeRecipient encodeMorphoState
  simp only [MorphoViewSlice.setFeeRecipient, MorphoViewSlice.ownerSlot,
    MorphoViewSlice.feeRecipientSlot,
    bind, Bind.bind, msgSender, getStorageAddr, setStorageAddr, require, pure, Pure.pure,
    ContractResult.success.injEq, true_and]
  simp only [beq_iff_eq, bne_iff_ne, Bool.not_eq_true', decide_eq_true_eq, ite_not]
  split <;> simp_all

/-- The EDSL `setFeeRecipient` satisfies the semantic equivalence obligation. -/
theorem setFeeRecipient_semEq :
    SolidityBridge.setFeeRecipientSemEq edslSetFeeRecipient :=
  setFeeRecipient_link1

/-! ## Discharge Status

With Link 1 proven, the `*SemEq` obligations can be instantiated using the
EDSL-based wrappers. The remaining gap (Links 2+3) connects the EDSL execution
to the compiled IR and then to EVMYulLean.

| Phase | Operations | Link 1 | Links 2+3 |
|-------|-----------|--------|-----------|
| 1 | setOwner, setFeeRecipient | **proven** | needs verity pin bump |
| 2 | enableIrm, enableLltv, setAuthorization | provable | needs mapping bridge |
| 3 | createMarket | provable | needs MappingWord bridge |
| 4 | 12 remaining ops | blocked on macro | blocked |

### What Link 1 gives us

Once `setOwner_semEq` is proven, the SolidityBridge theorems that take
`h_eq : setOwnerSemEq soliditySetOwner` can be instantiated with
`edslSetOwner` and `setOwner_semEq`. For example:

```
solidity_setOwner_preserves_borrowLeSupply edslSetOwner setOwner_semEq ...
```

This means the invariant proofs transfer to the EDSL implementation.
The remaining gap (Links 2+3) then establishes that the *compiled* EVM bytecode
behaves identically to the EDSL, which is the verity semantic bridge's job.
-/

end Morpho.Proofs.SemanticBridgeDischarge
