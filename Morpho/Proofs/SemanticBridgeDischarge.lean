import Morpho.EDSLAdapter
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
open Morpho.EDSLAdapter
open Morpho.Compiler.MacroSlice

/-! ## Link 1: setOwner -/

/-- **Link 1 for setOwner**: The EDSL `setOwner` matches the pure Lean model. -/
theorem setOwner_link1 :
    ∀ s newOwner, Morpho.EDSLAdapter.setOwner s newOwner = Morpho.setOwner s newOwner := by
  intro s newOwner
  simp only [Morpho.EDSLAdapter.setOwner, encodeMorphoState, Morpho.setOwner,
    MorphoViewSlice.setOwner, MorphoViewSlice.ownerSlot]
  simp only [Bind.bind, Verity.bind, Verity.msgSender, Verity.getStorageAddr,
    Verity.setStorageAddr, Verity.require]
  by_cases h1 : s.sender = s.owner <;> by_cases h2 : newOwner = s.owner <;> simp_all

/-- The EDSL `setOwner` satisfies the semantic equivalence obligation. -/
theorem setOwner_semEq :
    SolidityBridge.setOwnerSemEq Morpho.EDSLAdapter.setOwner :=
  setOwner_link1

/-! ## Link 1: setFeeRecipient -/

/-- **Link 1 for setFeeRecipient**: EDSL matches the pure Lean model. -/
theorem setFeeRecipient_link1 :
    ∀ s newFeeRecipient,
      Morpho.EDSLAdapter.setFeeRecipient s newFeeRecipient = Morpho.setFeeRecipient s newFeeRecipient := by
  intro s newFeeRecipient
  simp only [Morpho.EDSLAdapter.setFeeRecipient, encodeMorphoState, Morpho.setFeeRecipient,
    MorphoViewSlice.setFeeRecipient, MorphoViewSlice.ownerSlot,
    MorphoViewSlice.feeRecipientSlot]
  simp only [Bind.bind, Verity.bind, Verity.msgSender, Verity.getStorageAddr,
    Verity.setStorageAddr, Verity.require]
  by_cases h1 : s.sender = s.owner <;>
    by_cases h2 : newFeeRecipient = s.feeRecipient <;> simp_all

/-- The EDSL `setFeeRecipient` satisfies the semantic equivalence obligation. -/
theorem setFeeRecipient_semEq :
    SolidityBridge.setFeeRecipientSemEq Morpho.EDSLAdapter.setFeeRecipient :=
  setFeeRecipient_link1

/-! ## Link 1: enableIrm -/

/-- **Link 1 for enableIrm**: The EDSL `enableIrm` matches the pure Lean model. -/
theorem enableIrm_link1 :
    ∀ s irm, Morpho.EDSLAdapter.enableIrm s irm = Morpho.enableIrm s irm := by
  intro s irm
  have h01 : (1 : Uint256) ≠ 0 := by decide
  simp only [Morpho.EDSLAdapter.enableIrm, encodeMorphoState, Morpho.enableIrm,
    MorphoViewSlice.enableIrm, MorphoViewSlice.ownerSlot,
    MorphoViewSlice.isIrmEnabledSlot]
  simp only [Bind.bind, Verity.bind, Verity.msgSender, Verity.getStorageAddr,
    Verity.getMapping, Verity.setMapping, Verity.require]
  by_cases h1 : s.sender = s.owner <;>
    by_cases h2 : s.isIrmEnabled irm <;>
    simp_all

/-- The EDSL `enableIrm` satisfies the semantic equivalence obligation. -/
theorem enableIrm_semEq :
    SolidityBridge.enableIrmSemEq Morpho.EDSLAdapter.enableIrm :=
  enableIrm_link1

/-! ## Link 1: enableLltv -/

/-- **Link 1 for enableLltv**: The EDSL `enableLltv` matches the pure Lean model. -/
theorem enableLltv_link1 :
    ∀ s lltv, Morpho.EDSLAdapter.enableLltv s lltv = Morpho.enableLltv s lltv := by
  intro s lltv
  have h01 : (1 : Uint256) ≠ 0 := by decide
  simp only [Morpho.EDSLAdapter.enableLltv, encodeMorphoState, Morpho.enableLltv,
    MorphoViewSlice.enableLltv, MorphoViewSlice.ownerSlot,
    MorphoViewSlice.isLltvEnabledSlot]
  simp only [Bind.bind, Verity.bind, Verity.msgSender, Verity.getStorageAddr,
    Verity.getMappingUint, Verity.setMappingUint, Verity.require]
  simp only [Morpho.Libraries.MathLib.WAD]
  -- Reduce Uint256.val of the WAD literal to a bare Nat
  have hWadVal : Verity.Core.Uint256.val (1000000000000000000 : Uint256) = 1000000000000000000 := by
    native_decide
  -- Case split on owner check, isLltvEnabled, and WAD bound
  by_cases h1 : s.sender = s.owner <;>
    by_cases h2 : s.isLltvEnabled lltv <;>
    simp_all <;>
    -- Remaining: s.sender = owner, ¬isLltvEnabled, EDSL if-branch on WAD
    (by_cases h3 : lltv.val < (1000000000000000000 : Nat)
     · -- lltv < WAD: both sides succeed
       have hlt : lltv.val < 1000000000000000000 := h3
       simp [hlt]
     · -- lltv ≥ WAD: both sides give none
       have hge : ¬(lltv.val < 1000000000000000000) := h3
       simp [hge])

/-- The EDSL `enableLltv` satisfies the semantic equivalence obligation. -/
theorem enableLltv_semEq :
    SolidityBridge.enableLltvSemEq Morpho.EDSLAdapter.enableLltv :=
  enableLltv_link1

/-! ## Link 1: setAuthorization -/

/-- **Link 1 for setAuthorization**: The EDSL `setAuthorization` matches the pure Lean model. -/
theorem setAuthorization_link1 :
    ∀ s authorized newIsAuthorized,
      Morpho.EDSLAdapter.setAuthorization s authorized newIsAuthorized =
        Morpho.setAuthorization s authorized newIsAuthorized := by
  intro s authorized newIsAuthorized
  simp only [Morpho.EDSLAdapter.setAuthorization, encodeMorphoState, Morpho.setAuthorization,
    MorphoViewSlice.setAuthorization, MorphoViewSlice.isAuthorizedSlot]
  have h01 : (1 : Uint256) ≠ 0 := by decide
  -- Unfold the outer bind chain + reduce concrete beq
  simp only [Bind.bind, Verity.bind, Verity.msgSender, Verity.getMapping2,
    beq_self_eq_true, ite_true]
  -- Case split on newIsAuthorized and current auth state
  cases newIsAuthorized <;>
    by_cases h : s.isAuthorized s.sender authorized <;>
    simp_all [Verity.bind, Verity.require, Verity.setMapping2]

/-- The EDSL `setAuthorization` satisfies the semantic equivalence obligation. -/
theorem setAuthorization_semEq :
    SolidityBridge.setAuthorizationSemEq Morpho.EDSLAdapter.setAuthorization :=
  setAuthorization_link1

/-! ## Discharge Status

With Link 1 proven for 5 operations, the `*SemEq` obligations can be instantiated
using the EDSL-based wrappers. The remaining gap (Links 2+3) connects the EDSL
execution to the compiled IR and then to EVMYulLean.

| Phase | Operations | Link 1 | Links 2+3 |
|-------|-----------|--------|-----------|
| 1 | setOwner, setFeeRecipient | **proven** | needs verity pin bump |
| 2 | enableIrm, enableLltv, setAuthorization | **proven** | needs mapping bridge |
| 3 | createMarket | provable | needs MappingWord bridge |
| 4 | 12 remaining ops | blocked on macro | blocked |
-/

end Morpho.Proofs.SemanticBridgeDischarge
