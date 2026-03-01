import Compiler.Specs
import Compiler.DiffTestTypes
import Verity.Proofs.Stdlib.SpecInterpreter
import Verity.Proofs.Stdlib.Automation
import Morpho.Compiler.MacroSlice
import Morpho.Compiler.Generated
import Morpho.Proofs.SemanticBridgeDischarge

/-!
# SpecCorrectness for setOwner: EDSL ↔ CompilationModel (Link 2)

This module proves that the spec interpretation via `interpretSpec morphoGeneratedSpec`
matches the EDSL execution of `MorphoViewSlice.setOwner` for all cases:
- Success when sender is owner and newOwner ≠ owner
- Correct storage update on success (slot 0 = newOwner)
- Revert when sender is not owner
- Revert when newOwner equals current owner

Combined with Link 1 (SemanticBridgeDischarge.lean), this gives a full chain:
`Morpho.setOwner ↔ EDSL ↔ interpretSpec morphoGeneratedSpec`
-/

namespace Morpho.Proofs.SpecCorrectness.SetOwner

open Verity
open Verity.Proofs.Stdlib.SpecInterpreter
open Verity.Proofs.Stdlib.Automation
open Compiler.CompilationModel
open Compiler.DiffTestTypes
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.Generated

def morphoOwnerSpecStorage (state : ContractState) : SpecStorage :=
  { slots := [(0, state.storageAddr 0 |>.val)]
    mappings := []
    mappings2 := []
    events := [] }

-- Phase 1-3 of the proof: reduce 34-function List.find? to concrete setOwner body.
-- Factored into a macro_rules to avoid 20-line duplication per theorem.
-- Phase 1: unfold interpretSpec/execFunction + List.find? structure
-- Phase 2: unfold each *_model to expose concrete function names
-- Phase 3: use decide to evaluate 34 string comparisons
syntax "morpho_spec_setOwner_lookup" : tactic
macro_rules
  | `(tactic| morpho_spec_setOwner_lookup) => `(tactic| (
    simp only [interpretSpec, morphoGeneratedSpec, MorphoViewSlice.spec,
      morphoOwnerSpecStorage, execFunction, resolveFields,
      List.find?, BEq.beq, decide_eq_true_eq]
    simp only [
      MorphoViewSlice.DOMAIN_SEPARATOR_model,
      MorphoViewSlice.owner_model,
      MorphoViewSlice.feeRecipient_model,
      MorphoViewSlice.isIrmEnabled_model,
      MorphoViewSlice.isAuthorized_model,
      MorphoViewSlice.isLltvEnabled_model,
      MorphoViewSlice.nonce_model,
      MorphoViewSlice.lastUpdate_model,
      MorphoViewSlice.totalSupplyAssets_model,
      MorphoViewSlice.totalSupplyShares_model,
      MorphoViewSlice.totalBorrowAssets_model,
      MorphoViewSlice.totalBorrowShares_model,
      MorphoViewSlice.fee_model,
      MorphoViewSlice.idToMarketParams_model,
      MorphoViewSlice.market_model,
      MorphoViewSlice.position_model,
      MorphoViewSlice.extSloads_model,
      MorphoViewSlice.setOwner_model]
    simp (config := { decide := true })))

set_option maxHeartbeats 64000000 in
theorem morpho_setOwner_spec_succeeds
    (state : ContractState) (newOwner : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_new : newOwner ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setOwner"
      args := [newOwner.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoOwnerSpecStorage state) specTx).success = true := by
  morpho_spec_setOwner_lookup
  unfold MorphoViewSlice.setOwner_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.setSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq, h_owner,
    addressToNat_beq_false_of_ne _ _ h_new]

set_option maxHeartbeats 64000000 in
theorem morpho_setOwner_spec_sets_owner
    (state : ContractState) (newOwner : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_new : newOwner ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setOwner"
      args := [newOwner.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoOwnerSpecStorage state) specTx).finalStorage.getSlot 0 =
      newOwner.val := by
  morpho_spec_setOwner_lookup
  unfold MorphoViewSlice.setOwner_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.setSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq, h_owner,
    addressToNat_beq_false_of_ne _ _ h_new]

set_option maxHeartbeats 64000000 in
theorem morpho_setOwner_spec_reverts_nonowner
    (state : ContractState) (newOwner : Address) (sender : Address)
    (h_notowner : state.storageAddr 0 ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setOwner"
      args := [newOwner.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoOwnerSpecStorage state) specTx).success = false := by
  morpho_spec_setOwner_lookup
  unfold MorphoViewSlice.setOwner_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_beq_false_of_ne _ _ (Ne.symm h_notowner)]

set_option maxHeartbeats 64000000 in
theorem morpho_setOwner_spec_reverts_already_set
    (state : ContractState) (sender : Address)
    (h_owner : state.storageAddr 0 = sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setOwner"
      args := [sender.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoOwnerSpecStorage state) specTx).success = false := by
  morpho_spec_setOwner_lookup
  unfold MorphoViewSlice.setOwner_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_mod_eq, h_owner]

end Morpho.Proofs.SpecCorrectness.SetOwner
