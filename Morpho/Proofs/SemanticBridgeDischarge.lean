/-!
# Semantic Bridge Discharge Skeletons

This module documents the proof structure for discharging `*SemEq` obligations
once the verity semantic bridge (verity#998, verity#1052) is composed.

## Architecture

Each `*SemEq` obligation in `SolidityBridge.lean` asserts that a compiled
Solidity implementation equals the pure Lean model:

```
setOwnerSemEq soliditySetOwner :=
  ∀ s newOwner, soliditySetOwner s newOwner = Morpho.setOwner s newOwner
```

The verity semantic bridge provides, per macro-generated function:

```
EDSL.f.exec state = EVMYulLean(compile(CompilationModel)).exec state
```

Discharging requires three correspondences:

1. **Storage encoding**: Map `MorphoState` fields to `ContractState` storage slots
   - `s.owner` ↔ `state.storageAddr 0` (ownerSlot)
   - `s.feeRecipient` ↔ `state.storageAddr 1` (feeRecipientSlot)
   - `s.sender` ↔ `state.sender`

2. **Forward simulation**: Show EDSL execution on encoded state produces
   an encoded version of the Lean model's output state

3. **Result decoding**: Map EDSL success/revert back to `Option MorphoState`

## Reference Template: verity's `Owned.transferOwnership`

The verity `semantic-bridge` branch (commit `8b3b482`) includes a fully
discharged (zero sorry) proof for `Owned.transferOwnership` which is
structurally identical to morpho-verity's `setOwner`:

- **EDSL**: `msgSender → getStorageAddr → require (sender == owner) → setStorageAddr`
- **IR**: `calldataload(4) & addressMask → eq(caller(), sload(0)) → revert/sstore(0, newOwner)`
- **Proof**: `encodeStorageAddr` state encoding + direct `simp` unfolding + `by_cases` on slot

Key technique: The proof bypasses `setStorageAddr_matches_sstore` entirely —
it unfolds both EDSL (`setStorageAddr`) and IR (`abstractStoreStorageOrMapping`)
directly via `simp`, and uses address masking (`Nat.and_two_pow_sub_one_eq_mod`)
to handle the `and(calldataload(4), addressMask)` step in the IR.

**Theorem statement** (SemanticBridge.lean):
```
theorem owned_transferOwnership_semantic_bridge
    (state : ContractState) (sender : Address) (newOwner : Address)
    (hOwner : sender = state.storageAddr 0) :
    let edslResult := Contract.run (transferOwnership newOwner) { state with sender }
    ...
    match edslResult with
    | .success _ s' =>
        let irResult := interpretIR ownedIRContract tx irState
        irResult.success = true ∧
        ∀ slot, (s'.storageAddr slot).val = irResult.finalStorage slot
    | .revert _ _ => True
```

Note: The theorem takes `hOwner` as a hypothesis (only proving the success
path when sender IS the owner). The revert case is trivially `True`.
This is the expected pattern — full discharge only needs the success case,
since revert is semantically "no state change".

## Proof Strategy

For morpho-verity's `setOwner`, the proof extends the Owned template by adding
the "already set" idempotence check (`require (newOwner != currentOwner)`).

### Required state encoding

```
def encodeMorphoState (s : MorphoState) : ContractState := {
  storageAddr := fun slot =>
    if slot == 0 then s.owner
    else if slot == 1 then s.feeRecipient
    else 0,
  sender := s.sender,
  ... -- other fields for other operations
}
```

### Proof structure for setOwner (3 cases)

**Case 1**: `s.sender ≠ s.owner`
  - EDSL: `msgSender` returns `state.sender`, `getStorageAddr` returns `state.storageAddr 0`
  - `require (sender == currentOwner)` fails → `.revert`
  - Lean: `s.sender != s.owner` → `none`
  - Both produce failure → conclusion is `True` ✓

**Case 2**: `s.sender = s.owner` ∧ `newOwner = s.owner`
  - EDSL: first require passes, `require (newOwner != currentOwner)` fails → `.revert`
  - Lean: first check passes, `newOwner == s.owner` → `none`
  - Both produce failure → conclusion is `True` ✓

**Case 3**: `s.sender = s.owner` ∧ `newOwner ≠ s.owner`
  - EDSL: both requires pass, `setStorageAddr ownerSlot newOwner` → `.success`
    - New state: `storageAddr 0 = newOwner`, all other slots unchanged
  - Lean: both checks pass → `some { s with owner := newOwner }`
  - Storage correspondence: `newState.storageAddr 0 = newOwner = (s with owner := newOwner).owner` ✓

### Expected proof (following Owned template)

```
theorem morpho_setOwner_semantic_bridge
    (state : ContractState) (sender : Address) (newOwner : Address)
    (hOwner : sender = state.storageAddr 0)
    (hNotSame : newOwner ≠ state.storageAddr 0) :
    ...
    match edslResult with
    | .success _ s' =>
        let irResult := interpretIR morphoIRContract tx irState
        irResult.success = true ∧
        ∀ slot, (s'.storageAddr slot).val = irResult.finalStorage slot
    | .revert _ _ => True
    := by
  have haddr : newOwner.val &&& addressMask = newOwner.val := by
    simp only [addressMask]
    have hlt : newOwner.val < 2 ^ 160 := by
      have := newOwner.isLt; simp [ADDRESS_MODULUS] at this; exact this
    calc newOwner.val &&& (2 ^ 160 - 1)
        = newOwner.val % 2 ^ 160 := by simpa using (Nat.and_two_pow_sub_one_eq_mod ...)
      _ = newOwner.val := Nat.mod_eq_of_lt hlt
  simp [MorphoViewSlice.setOwner, -- EDSL function
    Contract.run, getStorageAddr, setStorageAddr, msgSender, require,
    bind, pure, hOwner, hNotSame, encodeStorageAddr,
    interpretIR, morphoIRContract,
    execIRFunction, execIRStmts, execIRStmt, evalIRExpr, evalIRCall, evalIRExprs,
    evalBuiltinCallWithBackend, defaultBuiltinBackend, evalBuiltinCall,
    calldataloadWord,
    Compiler.Proofs.abstractLoadStorageOrMapping,
    Compiler.Proofs.abstractStoreStorageOrMapping,
    Compiler.Proofs.storageAsMappings,
    IRState.setVar, IRState.getVar, haddr]
  intro slot
  by_cases h : slot = 0 <;> simp_all [beq_iff_eq]
```

## Upstream Dependencies

### Proven (verity PrimitiveBridge.lean, semantic-bridge branch)
- `getStorageAddr_matches_sload`: reading Address storage
- `setStorageAddr_matches_sstore`: writing Address storage (verity#1054) — NOT needed for direct unfolding strategy
- `msgSender_matches_caller`: sender access
- `require_eq/neq_matches_iszero_revert`: require checks
- `bind_unfold`, `pure_unfold`: monadic composition
- `Nat.and_two_pow_sub_one_eq_mod`: address mask arithmetic

### Also proven (from commits 2031992, 8b3b482)
- Counter: increment, decrement, getCount semantic bridge (zero sorry)
- Owned: getOwner, transferOwnership semantic bridge (zero sorry)
- `encodeStorageAddr`: canonical Address-typed storage encoding

### Required but not yet available
- `morphoIRContract`: the compiled IR for MorphoViewSlice (needs `compile` on morpho spec)
- Morpho CompilationModel spec must match MacroSlice EDSL

## Status

This file contains only the proof *skeleton* and type-level documentation.
No theorems are stated; no sorry is introduced. Once the verity semantic
bridge theorem is composed and the verity pin is bumped, concrete proofs
will replace these skeletons.
-/

namespace Morpho.Proofs.SemanticBridgeDischarge

/-! ## State Encoding

The canonical mapping from `MorphoState` (pure Lean) to `ContractState`
(EDSL), following the `MacroSlice.lean` storage layout.

```
MorphoState field         | ContractState accessor     | Slot
--------------------------|----------------------------|-----
owner : Address           | storageAddr 0              | 0
feeRecipient : Address    | storageAddr 1              | 1
(markets mapping)         | storageMap 3               | 3
isIrmEnabled : Addr→Bool  | storageMap 4               | 4
isLltvEnabled : Uint→Bool | storageMapUint 5           | 5
isAuthorized : 2D mapping | storageMap2 6              | 6
nonce : Addr→Uint256      | storageMapUint 7           | 7
idToMarketParams mapping  | storageMap 8               | 8
sender : Address          | sender                     | —
```

Key insight: verity's `encodeStorageAddr` (commit 8b3b482) covers the
Address-typed fields. Mixed-type contracts (Address + Uint256 + Mapping
storage) will need a combined encoding function.
-/

/-! ## Discharge Sequence Summary

| Phase | Operations | Upstream template | Status |
|-------|-----------|-------------------|--------|
| 1 | setOwner, setFeeRecipient | `owned_transferOwnership_semantic_bridge` | **template proven** |
| 2 | enableIrm, enableLltv, setAuthorization | (needs mapping bridge) | EDSL-ready |
| 3 | createMarket | (needs MappingWord + externalCall) | macro-migrated |
| 4 | 12 remaining ops | (needs internal calls, ERC20, etc.) | blocked on macro |
-/

end Morpho.Proofs.SemanticBridgeDischarge
