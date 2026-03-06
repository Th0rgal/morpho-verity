import Morpho.Compiler.MacroSlice
import Morpho.Compiler.AdminAdapters
import Morpho.Proofs.SolidityBridge

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

This file proves Link 1 for `setOwner`, `setFeeRecipient`, `enableIrm`,
`enableLltv`, and `setAuthorization`.
Links 2+3 depend on upstream verity infrastructure (verity#1060 / #1065).
Verity pin: dab9a567 (witness theorems, simp_tir_eval tactic, event encoding, Layer-2 spine).

## Proof Strategy

For each function, we:
1. Define `encodeMorphoState : MorphoState → ContractState` matching MacroSlice storage layout
2. Define an EDSL-based wrapper: run `MorphoViewSlice.f` on encoded state, decode result
3. Prove the wrapper equals the pure Lean `Morpho.f`
4. Conclude `*SemEq` obligation is satisfiable

The key tactic pattern is:
- Unfold definitions including `Bind.bind` (to resolve do-notation `>>=`)
- Then `Verity.bind` + primitive definitions to reduce the monadic chain
- Then `by_cases` on the boolean conditions to case-split
- Then `simp_all` to close each case
-/

namespace Morpho.Proofs.SemanticBridgeDischarge

open Verity
open Morpho.Types
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.AdminAdapters

/-! ## Canonical State Encoding

`Morpho.Compiler.AdminAdapters` now owns the canonical encoding from
`MorphoState` to the contract-state layout consumed by `MacroSlice`. Link 1
theorems below are phrased against that adapter surface, which is also what
`Morpho.Morpho` now re-exports for the already-migrated admin operations. -/

noncomputable abbrev edslSetOwner := Morpho.Compiler.AdminAdapters.setOwner

/-- **Link 1 for setOwner**: The EDSL `setOwner` matches the pure Lean model. -/
theorem setOwner_link1 :
    ∀ s newOwner, edslSetOwner s newOwner = Morpho.setOwner s newOwner := by
  intro s newOwner
  rfl

/-- The EDSL `setOwner` satisfies the semantic equivalence obligation. -/
theorem setOwner_semEq :
    SolidityBridge.setOwnerSemEq edslSetOwner :=
  setOwner_link1

noncomputable abbrev edslSetFeeRecipient := Morpho.Compiler.AdminAdapters.setFeeRecipient

/-- **Link 1 for setFeeRecipient**: EDSL matches the pure Lean model. -/
theorem setFeeRecipient_link1 :
    ∀ s newFeeRecipient,
      edslSetFeeRecipient s newFeeRecipient = Morpho.setFeeRecipient s newFeeRecipient := by
  intro s newFeeRecipient
  rfl

/-- The EDSL `setFeeRecipient` satisfies the semantic equivalence obligation. -/
theorem setFeeRecipient_semEq :
    SolidityBridge.setFeeRecipientSemEq edslSetFeeRecipient :=
  setFeeRecipient_link1

noncomputable abbrev edslEnableIrm := Morpho.Compiler.AdminAdapters.enableIrm

/-- **Link 1 for enableIrm**: The EDSL `enableIrm` matches the pure Lean model. -/
theorem enableIrm_link1 :
    ∀ s irm, edslEnableIrm s irm = Morpho.enableIrm s irm := by
  intro s irm
  rfl

/-- The EDSL `enableIrm` satisfies the semantic equivalence obligation. -/
theorem enableIrm_semEq :
    SolidityBridge.enableIrmSemEq edslEnableIrm :=
  enableIrm_link1

noncomputable abbrev edslEnableLltv := Morpho.Compiler.AdminAdapters.enableLltv

/-- **Link 1 for enableLltv**: The EDSL `enableLltv` matches the pure Lean model. -/
theorem enableLltv_link1 :
    ∀ s lltv, edslEnableLltv s lltv = Morpho.enableLltv s lltv := by
  intro s lltv
  rfl

/-- The EDSL `enableLltv` satisfies the semantic equivalence obligation. -/
theorem enableLltv_semEq :
    SolidityBridge.enableLltvSemEq edslEnableLltv :=
  enableLltv_link1

noncomputable abbrev edslSetAuthorization := Morpho.Compiler.AdminAdapters.setAuthorization

/-- **Link 1 for setAuthorization**: The EDSL `setAuthorization` matches the pure Lean model. -/
theorem setAuthorization_link1 :
    ∀ s authorized newIsAuthorized,
      edslSetAuthorization s authorized newIsAuthorized =
        Morpho.setAuthorization s authorized newIsAuthorized := by
  intro s authorized newIsAuthorized
  rfl

/-- The EDSL `setAuthorization` satisfies the semantic equivalence obligation. -/
theorem setAuthorization_semEq :
    SolidityBridge.setAuthorizationSemEq edslSetAuthorization :=
  setAuthorization_link1

/-! ## Discharge Status

With Link 1 proven for 5 operations, the `*SemEq` obligations can be instantiated
using the EDSL-based wrappers. The remaining gap (Links 2+3) connects the EDSL
execution to the compiled IR and then to EVMYulLean.

| Phase | Operations | Link 1 | Links 2+3 |
|-------|-----------|--------|-----------|
| 1 | setOwner, setFeeRecipient | **proven** | typed-IR bridge available at pin `dab9a567` (setFeeRecipient witness gap remains) |
| 2 | enableIrm, enableLltv, setAuthorization | **proven** | typed-IR bridge available at pin `dab9a567` |
| 3 | createMarket | provable | needs MappingWord bridge |
| 4 | 12 remaining ops | blocked on macro | blocked |
-/

end Morpho.Proofs.SemanticBridgeDischarge
