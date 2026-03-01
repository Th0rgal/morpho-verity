/-!
# Semantic Bridge End-to-End: Direct Link 1 Composition + Links 2+3 Gap

This module demonstrates direct composition of Link 1 proofs with invariant
theorems, and documents the precise gap for Links 2+3 (EDSL ↔ compiled IR ↔ Yul).

## What this proves (no sorry)

For setOwner, we compose:
- Link 1 (SemanticBridgeDischarge): `edslSetOwner = Morpho.setOwner`
- Pure invariant (Invariants): `Morpho.setOwner` preserves `borrowLeSupply` etc.

Result: the EDSL `setOwner` preserves all Morpho invariants, proven by direct
composition rather than through the parametric SolidityBridge layer.

## Links 2+3 gap documentation

The current verity pin (`dccb984`) pre-dates the semantic bridge infrastructure.
The verity `semantic-bridge` branch (commit `ee4f2cf`) contains:
- `owned_transferOwnership_semantic_bridge`: fully discharged (zero sorry) proof
  that Owned.transferOwnership's EDSL matches compiled IR, structurally identical
  to our setOwner
- `Compiler.Proofs.EndToEnd`: Layer 2+3 composition theorems
- `Verity.Proofs.Stdlib.PrimitiveBridge`: per-primitive EDSL ↔ IR lemmas

To complete Links 2+3 for setOwner/setFeeRecipient:
1. Bump verity pin from `dccb984` to `semantic-bridge` branch
2. Compile `MorphoViewSlice.spec` → IR → define `morphoIRContract`
3. Prove by direct simp, following `owned_transferOwnership_semantic_bridge`
4. Compose with Link 1 for the full chain: Pure Lean ↔ EDSL ↔ IR ↔ Yul
-/

import Morpho.Proofs.SemanticBridgeDischarge
import Morpho.Proofs.Invariants

namespace Morpho.Proofs.SemanticBridgeEndToEnd

open Morpho.Types
open Morpho.Proofs.SemanticBridgeDischarge
open Morpho.Proofs.Invariants
open Morpho.Specs.Invariants

/-! ## Direct Link 1 + Invariants composition

These theorems compose Link 1 (`edslSetOwner = Morpho.setOwner`) directly
with the pure invariant theorems, bypassing the parametric SolidityBridge layer.
This is the most direct proof that the EDSL implementation preserves invariants.
-/

/-- The EDSL `setOwner` preserves borrow-le-supply, by direct composition
    of Link 1 with the pure model invariant. -/
theorem edsl_setOwner_borrowLeSupply
    (s : MorphoState) (newOwner : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetOwner s newOwner = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_borrowLeSupply s newOwner id h_solvent h_morpho

/-- The EDSL `setOwner` preserves always-collateralized. -/
theorem edsl_setOwner_alwaysCollateralized
    (s : MorphoState) (newOwner : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetOwner s newOwner = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_alwaysCollateralized s newOwner id user h_collat h_morpho

/-- The EDSL `setOwner` preserves IRM monotonicity. -/
theorem edsl_setOwner_irmMonotone
    (s : MorphoState) (newOwner irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_irmMonotone s newOwner irm h_enabled h_morpho

/-- The EDSL `setOwner` preserves LLTV monotonicity. -/
theorem edsl_setOwner_lltvMonotone
    (s : MorphoState) (newOwner : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_lltvMonotone s newOwner lltv h_enabled h_morpho

/-! ## enableIrm: Direct composition -/

theorem edsl_enableIrm_borrowLeSupply
    (s : MorphoState) (irm : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableIrm s irm = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.enableIrm s irm = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_preserves_borrowLeSupply s irm id h_solvent h_morpho

theorem edsl_enableIrm_irmMonotone
    (s : MorphoState) (irmCall irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableIrm s irmCall = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.enableIrm s irmCall = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_monotone s irmCall irm h_morpho h_enabled

/-! ## What the verity pin bump enables

After bumping verity to `semantic-bridge` (commit `ee4f2cf` or later):

### For setOwner/setFeeRecipient (Address storage):

```
theorem setOwner_edsl_matches_ir
    (state : ContractState) (sender : Address) (newOwner : Address)
    (hOwner : sender = state.storageAddr 0) :
    let edslResult := Contract.run (MorphoViewSlice.setOwner newOwner)
        { state with sender := sender }
    let tx : IRTransaction := { sender := sender.val, functionSelector := ..., args := [newOwner.val] }
    let irState : IRState := { vars := [], storage := encodeStorageAddr state, memory := fun _ => 0, ... }
    match edslResult with
    | .success _ s' =>
        let irResult := interpretIR morphoIRContract tx irState
        irResult.success = true ∧
        ∀ slot, (s'.storageAddr slot).val = irResult.finalStorage slot
    | .revert _ _ => True
```

Proof follows `owned_transferOwnership_semantic_bridge` template (same primitives).

### For mapping operations (enableIrm, enableLltv, setAuthorization):

Requires mapping bridge lemmas connecting `getMapping`/`setMapping` to
`sload(keccak256(key,slot))`/`sstore(keccak256(key,slot),value)` in compiled Yul.
The verity `semantic-bridge` branch has `getMapping_unfold`/`setMapping_unfold`
at the EDSL level; the IR→Yul bridge for these is in progress.

### createMarket:

BLOCKED: `getMappingWord`/`setMappingWord` are stubs (`pure 0`/`pure ()`) in
`MacroSlice.lean`. The EDSL implementation does not actually read/write market
struct data. This must be resolved before Link 1 can be proven for createMarket.
-/

end Morpho.Proofs.SemanticBridgeEndToEnd
