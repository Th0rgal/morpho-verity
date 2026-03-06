import Morpho.Specs.ContractSemantics
import Morpho.Proofs.SolidityBridge

/-!
# Semantic Bridge Discharge: Link 1 (Pure Lean ↔ EDSL)

This module proves the first link of the semantic bridge discharge chain:
the EDSL functions produced by `verity_contract` are equivalent to the
canonical spec-facing execution surface in `Morpho.Specs.ContractSemantics`.

## Architecture

The full discharge chain for an obligation like `setOwnerSemEq` has three links:

```
   Canonical contract semantics         -- this repo, ContractSemantics.lean
     ↕ Link 1 (this file)
   EDSL (Compiler.AdminAdapters.setOwner)
     ↕ Link 2 (verity SemanticBridge)
   Compiled IR (interpretIR morphoIR)   -- verity, once compile pipeline lands
     ↕ Link 3 (verity EndToEnd)
   EVMYulLean (Yul execution)           -- verity, 1 keccak axiom
```

This file proves Link 1 for `setOwner`, `setFeeRecipient`, `enableIrm`,
`enableLltv`, `setAuthorization`, and `flashLoan`.
Links 2+3 depend on upstream verity infrastructure (verity#1060 / #1065).
Verity pin: 9d9533b2 (including the two-storage-address witness needed by `setFeeRecipient`).

## Proof Strategy

For each function, we:
1. Reuse the EDSL-backed adapter exported from `Compiler.AdminAdapters`
2. Prove that adapter is definitionally equal to `ContractSemantics.f`
3. Conclude the corresponding `*SemEq` obligation is satisfiable

The key tactic pattern is:
- Unfold definitions including `Bind.bind` (to resolve do-notation `>>=`)
- Then `Verity.bind` + primitive definitions to reduce the monadic chain
- Then `by_cases` on the boolean conditions to case-split
- Then `simp_all` to close each case
-/

namespace Morpho.Proofs.SemanticBridgeDischarge

open Verity
open Morpho.Types

/-! ## Canonical State Encoding

`Morpho.Specs.ContractSemantics` now owns the canonical spec-facing execution
surface for the already-migrated admin operations. That module points directly
at `Compiler.AdminAdapters`, while `Morpho.Morpho` merely re-exports the same
definitions for backwards compatibility. -/

noncomputable abbrev edslSetOwner := Morpho.Compiler.AdminAdapters.setOwner

/-- **Link 1 for setOwner**: The EDSL `setOwner` matches the pure Lean model. -/
theorem setOwner_link1 :
    ∀ s newOwner,
      edslSetOwner s newOwner = Morpho.Specs.ContractSemantics.setOwner s newOwner := by
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
      edslSetFeeRecipient s newFeeRecipient =
        Morpho.Specs.ContractSemantics.setFeeRecipient s newFeeRecipient := by
  intro s newFeeRecipient
  rfl

/-- The EDSL `setFeeRecipient` satisfies the semantic equivalence obligation. -/
theorem setFeeRecipient_semEq :
    SolidityBridge.setFeeRecipientSemEq edslSetFeeRecipient :=
  setFeeRecipient_link1

noncomputable abbrev edslEnableIrm := Morpho.Compiler.AdminAdapters.enableIrm

/-- **Link 1 for enableIrm**: The EDSL `enableIrm` matches the pure Lean model. -/
theorem enableIrm_link1 :
    ∀ s irm, edslEnableIrm s irm = Morpho.Specs.ContractSemantics.enableIrm s irm := by
  intro s irm
  rfl

/-- The EDSL `enableIrm` satisfies the semantic equivalence obligation. -/
theorem enableIrm_semEq :
    SolidityBridge.enableIrmSemEq edslEnableIrm :=
  enableIrm_link1

noncomputable abbrev edslEnableLltv := Morpho.Compiler.AdminAdapters.enableLltv

/-- **Link 1 for enableLltv**: The EDSL `enableLltv` matches the pure Lean model. -/
theorem enableLltv_link1 :
    ∀ s lltv, edslEnableLltv s lltv = Morpho.Specs.ContractSemantics.enableLltv s lltv := by
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
        Morpho.Specs.ContractSemantics.setAuthorization s authorized newIsAuthorized := by
  intro s authorized newIsAuthorized
  rfl

/-- The EDSL `setAuthorization` satisfies the semantic equivalence obligation. -/
theorem setAuthorization_semEq :
    SolidityBridge.setAuthorizationSemEq edslSetAuthorization :=
  setAuthorization_link1

noncomputable abbrev edslFlashLoan := Morpho.Compiler.AdminAdapters.flashLoan

/-- **Link 1 for flashLoan**: The EDSL `flashLoan` matches the canonical state model. -/
theorem flashLoan_link1 :
    ∀ s assets,
      edslFlashLoan s assets = Morpho.Specs.ContractSemantics.flashLoan s assets := by
  intro s assets
  rfl

/-- The EDSL `flashLoan` satisfies the semantic equivalence obligation. -/
theorem flashLoan_semEq :
    SolidityBridge.flashLoanSemEq edslFlashLoan :=
  flashLoan_link1

/-! ## Discharge Status

With Link 1 proven for 6 operations, the `*SemEq` obligations can be instantiated
using the EDSL-based wrappers. The remaining gap (Links 2+3) connects the EDSL
execution to the compiled IR and then to EVMYulLean.

| Phase | Operations | Link 1 | Links 2+3 |
|-------|-----------|--------|-----------|
| 1 | setOwner, setFeeRecipient | **proven** | typed-IR bridge available at pin `9d9533b2` |
| 2 | enableIrm, enableLltv, setAuthorization | **proven** | typed-IR bridge available at pin `9d9533b2` |
| 3 | flashLoan | **proven** | pending `SupportedStmtList`/primitive bridge coverage for `mstore` + `rawLog` |
| 4 | createMarket | provable | needs MappingWord bridge |
| 5 | 11 remaining ops | blocked on macro | blocked |
-/

end Morpho.Proofs.SemanticBridgeDischarge
