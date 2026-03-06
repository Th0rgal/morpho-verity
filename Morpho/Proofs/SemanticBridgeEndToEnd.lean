import Morpho.Proofs.SemanticBridgeDischarge
import Morpho.Proofs.Invariants
import Morpho.Proofs.CompilationCorrectness

/-!
# Semantic Bridge End-to-End Composition

This module composes all available links of the semantic bridge:

## Full pipeline (per function)

```
Pure Lean (Morpho.setOwner)              ← Link 1 (SemanticBridgeDischarge)
  ↕
EDSL (MorphoViewSlice.setOwner)          ← Link 2 (SupportedStmtList, CompilationCorrectness)
  ↕
Compiled typed-IR                        ← Link 3 (verity EndToEnd, future)
  ↕
EVMYulLean (Yul execution)
```

## What this proves (no sorry)

For all 5 admin functions with Link 1 proofs, we compose:
- **Link 1** (`SemanticBridgeDischarge`): `edslF = Morpho.F`
- **Pure invariants** (`Invariants`): `Morpho.F` preserves all 4 invariants

Result: 20 direct composition theorems showing each EDSL function preserves
each Morpho invariant, plus 2 additional monotonicity theorems for enableIrm.

## Link 2+3 status

4 of 5 admin functions have `SupportedStmtList` proofs (`CompilationCorrectness.lean`),
which gives free compilation correctness via verity's typed-IR framework:
- `compile_supported_stmt_list_direct_semantics`: `execCompiled = execSource`

| Function         | Link 1 | SupportedStmtList | Link 3 (IR→Yul) |
|------------------|--------|-------------------|-----------------|
| setOwner         | proven | proven            | verity generic  |
| setFeeRecipient  | proven | GAP (2-field-read)| —               |
| enableIrm        | proven | proven            | verity generic  |
| enableLltv       | proven | proven            | verity generic  |
| setAuthorization | proven | proven            | verity generic  |

The remaining gap for the full pipeline (EDSL → Yul) is connecting `SupportedStmtList`
to `compileFunctionToTBlock` → `interpretIR` → `interpretYulFromIR`, which requires
verity infrastructure not yet available for external contracts.

As of verity pin dab9a567 (witness theorems, simp_tir_eval tactic, event encoding,
Layer-2 theorem spine, and proof deduplication), the compilation proofs for
the 4 supported admin functions are fully compositional within the typed-IR domain.
-/

namespace Morpho.Proofs.SemanticBridgeEndToEnd

open Verity
open Morpho.Types
open Morpho.Proofs.SemanticBridgeDischarge
open Morpho.Proofs.Invariants
open Morpho.Specs.Invariants

/-! ## setOwner: Link 1 + invariants (4 theorems) -/

theorem edsl_setOwner_borrowLeSupply
    (s : MorphoState) (newOwner : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetOwner s newOwner = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_borrowLeSupply s newOwner id h_solvent h_morpho

theorem edsl_setOwner_alwaysCollateralized
    (s : MorphoState) (newOwner : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetOwner s newOwner = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_alwaysCollateralized s newOwner id user h_collat h_morpho

theorem edsl_setOwner_irmMonotone
    (s : MorphoState) (newOwner irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_irmMonotone s newOwner irm h_enabled h_morpho

theorem edsl_setOwner_lltvMonotone
    (s : MorphoState) (newOwner : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_lltvMonotone s newOwner lltv h_enabled h_morpho

/-! ## setFeeRecipient: Link 1 + invariants (4 theorems) -/

theorem edsl_setFeeRecipient_borrowLeSupply
    (s : MorphoState) (newFeeRecipient : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.setFeeRecipient s newFeeRecipient = some s' := by
    rw [← setFeeRecipient_link1]; exact h_ok
  exact setFeeRecipient_preserves_borrowLeSupply s newFeeRecipient id h_solvent h_morpho

theorem edsl_setFeeRecipient_alwaysCollateralized
    (s : MorphoState) (newFeeRecipient : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.setFeeRecipient s newFeeRecipient = some s' := by
    rw [← setFeeRecipient_link1]; exact h_ok
  exact setFeeRecipient_preserves_alwaysCollateralized s newFeeRecipient id user h_collat h_morpho

theorem edsl_setFeeRecipient_irmMonotone
    (s : MorphoState) (newFeeRecipient irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.setFeeRecipient s newFeeRecipient = some s' := by
    rw [← setFeeRecipient_link1]; exact h_ok
  exact setFeeRecipient_preserves_irmMonotone s newFeeRecipient irm h_enabled h_morpho

theorem edsl_setFeeRecipient_lltvMonotone
    (s : MorphoState) (newFeeRecipient : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.setFeeRecipient s newFeeRecipient = some s' := by
    rw [← setFeeRecipient_link1]; exact h_ok
  exact setFeeRecipient_preserves_lltvMonotone s newFeeRecipient lltv h_enabled h_morpho

/-! ## enableIrm: Link 1 + invariants (4 theorems + enableIrm_monotone) -/

theorem edsl_enableIrm_borrowLeSupply
    (s : MorphoState) (irm : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableIrm s irm = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.enableIrm s irm = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_preserves_borrowLeSupply s irm id h_solvent h_morpho

theorem edsl_enableIrm_alwaysCollateralized
    (s : MorphoState) (irm : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslEnableIrm s irm = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.enableIrm s irm = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_preserves_alwaysCollateralized s irm id user h_collat h_morpho

theorem edsl_enableIrm_irmMonotone
    (s : MorphoState) (irmCall irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableIrm s irmCall = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.enableIrm s irmCall = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_monotone s irmCall irm h_morpho h_enabled

theorem edsl_enableIrm_lltvMonotone
    (s : MorphoState) (irm : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslEnableIrm s irm = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.enableIrm s irm = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  simp only [Morpho.enableIrm] at h_morpho
  split at h_morpho <;> simp at h_morpho
  rw [← h_morpho.right]; exact h_enabled

/-! ## enableLltv: Link 1 + invariants (4 theorems + enableLltv_monotone) -/

theorem edsl_enableLltv_borrowLeSupply
    (s : MorphoState) (lltv : Uint256) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableLltv s lltv = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.enableLltv s lltv = some s' := by
    rw [← enableLltv_link1]; exact h_ok
  exact enableLltv_preserves_borrowLeSupply s lltv id h_solvent h_morpho

theorem edsl_enableLltv_alwaysCollateralized
    (s : MorphoState) (lltv : Uint256) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslEnableLltv s lltv = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.enableLltv s lltv = some s' := by
    rw [← enableLltv_link1]; exact h_ok
  exact enableLltv_preserves_alwaysCollateralized s lltv id user h_collat h_morpho

theorem edsl_enableLltv_irmMonotone
    (s : MorphoState) (lltvCall : Uint256) (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableLltv s lltvCall = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.enableLltv s lltvCall = some s' := by
    rw [← enableLltv_link1]; exact h_ok
  simp only [Morpho.enableLltv] at h_morpho
  split at h_morpho <;> simp at h_morpho
  rw [← h_morpho.right.right]; exact h_enabled

theorem edsl_enableLltv_lltvMonotone
    (s : MorphoState) (lltvCall lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslEnableLltv s lltvCall = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.enableLltv s lltvCall = some s' := by
    rw [← enableLltv_link1]; exact h_ok
  exact enableLltv_monotone s lltvCall lltv h_morpho h_enabled

/-! ## setAuthorization: Link 1 + invariants (4 theorems) -/

theorem edsl_setAuthorization_borrowLeSupply
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.setAuthorization s authorized newIsAuthorized = some s' := by
    rw [← setAuthorization_link1]; exact h_ok
  exact setAuthorization_preserves_borrowLeSupply s authorized newIsAuthorized id h_solvent h_morpho

theorem edsl_setAuthorization_alwaysCollateralized
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.setAuthorization s authorized newIsAuthorized = some s' := by
    rw [← setAuthorization_link1]; exact h_ok
  exact setAuthorization_preserves_alwaysCollateralized s authorized newIsAuthorized id user
    h_collat h_morpho

theorem edsl_setAuthorization_irmMonotone
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.setAuthorization s authorized newIsAuthorized = some s' := by
    rw [← setAuthorization_link1]; exact h_ok
  simp only [Morpho.setAuthorization] at h_morpho
  split at h_morpho <;> simp at h_morpho
  rw [← h_morpho]; exact h_enabled

theorem edsl_setAuthorization_lltvMonotone
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.setAuthorization s authorized newIsAuthorized = some s' := by
    rw [← setAuthorization_link1]; exact h_ok
  simp only [Morpho.setAuthorization] at h_morpho
  split at h_morpho <;> simp at h_morpho
  rw [← h_morpho]; exact h_enabled

/-! ## Summary

### Link 1 + Invariants: 22 theorems, zero sorry

For all 5 admin functions with Link 1 proofs, we have direct composition
showing each EDSL function preserves each invariant:

| Function         | borrowLeSupply | alwaysCollat | irmMono | lltvMono |
|------------------|:-:|:-:|:-:|:-:|
| setOwner         | proven | proven | proven | proven |
| setFeeRecipient  | proven | proven | proven | proven |
| enableIrm        | proven | proven | proven | proven |
| enableLltv       | proven | proven | proven | proven |
| setAuthorization | proven | proven | proven | proven |

### SupportedStmtList coverage (CompilationCorrectness.lean)

4 of 5 admin functions have `SupportedStmtList` proofs, giving free
compilation correctness via `compile_supported_stmt_list_direct_semantics`:

- setOwner: `letCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop`
- enableIrm: `letCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop`
- enableLltv: `letCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop`
- setAuthorization: `letCallerLetMapping2IteParamReqSetMapping2Stop`

### Remaining gaps

- **setFeeRecipient SupportedStmtList**: reads two distinct storage address fields
  (ownerSlot for auth, feeRecipientSlot for ≠ check) — needs a new fragment in verity
- **EDSL → IR → Yul bridge**: verity's `compileFunctionToTBlock` + `interpretIR` +
  `layer3_contract_preserves_semantics` pipeline is not yet instantiated for external
  contracts. The `SupportedStmtList` proof gives source/compiled equivalence within
  the typed-IR domain but the full per-contract IR bridge remains future work.
- **createMarket**: `getMappingWord`/`setMappingWord` stubs in MacroSlice.lean
- **12 remaining operations**: blocked on external call / loop / struct mapping support
-/

end Morpho.Proofs.SemanticBridgeEndToEnd
