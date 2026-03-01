import Compiler.Specs
import Compiler.DiffTestTypes
import Verity.Proofs.Stdlib.SpecInterpreter
import Verity.Proofs.Stdlib.Automation
import Morpho.Compiler.MacroSlice
import Morpho.Compiler.Generated

/-!
# SpecCorrectness for setFeeRecipient: EDSL ↔ CompilationModel (Link 2)

Proves that `interpretSpec morphoGeneratedSpec` matches the EDSL execution of
`MorphoViewSlice.setFeeRecipient` for all cases. Uses the same staged proof
strategy as SetOwner.lean (34-function lookup + statement execution).
-/

namespace Morpho.Proofs.SpecCorrectness.SetFeeRecipient

open Verity
open Verity.Proofs.Stdlib.SpecInterpreter
open Verity.Proofs.Stdlib.Automation
open Compiler.CompilationModel
open Compiler.DiffTestTypes
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.Generated

/-- SpecStorage for setFeeRecipient: needs both slot 0 (owner) and slot 1 (feeRecipient). -/
def morphoFeeRecipientSpecStorage (state : ContractState) : SpecStorage :=
  { slots := [(0, state.storageAddr 0 |>.val), (1, state.storageAddr 1 |>.val)]
    mappings := []
    mappings2 := []
    events := [] }

-- Phase 1-3: 34-function lookup to find setFeeRecipient
syntax "morpho_spec_setFeeRecipient_lookup" : tactic
macro_rules
  | `(tactic| morpho_spec_setFeeRecipient_lookup) => `(tactic| (
    simp only [interpretSpec, morphoGeneratedSpec, MorphoViewSlice.spec,
      morphoFeeRecipientSpecStorage, execFunction, resolveFields,
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
      MorphoViewSlice.setOwner_model,
      MorphoViewSlice.setFeeRecipient_model]
    simp (config := { decide := true })))

set_option maxHeartbeats 64000000 in
theorem morpho_setFeeRecipient_spec_succeeds
    (state : ContractState) (newFeeRecipient : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_new : newFeeRecipient ≠ state.storageAddr 1) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setFeeRecipient"
      args := [newFeeRecipient.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoFeeRecipientSpecStorage state) specTx).success = true := by
  morpho_spec_setFeeRecipient_lookup
  unfold MorphoViewSlice.setFeeRecipient_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.setSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq, h_owner,
    addressToNat_beq_false_of_ne _ _ h_new]

set_option maxHeartbeats 64000000 in
theorem morpho_setFeeRecipient_spec_sets_recipient
    (state : ContractState) (newFeeRecipient : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_new : newFeeRecipient ≠ state.storageAddr 1) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setFeeRecipient"
      args := [newFeeRecipient.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoFeeRecipientSpecStorage state) specTx).finalStorage.getSlot 1 =
      newFeeRecipient.val := by
  morpho_spec_setFeeRecipient_lookup
  unfold MorphoViewSlice.setFeeRecipient_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.setSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq, h_owner,
    addressToNat_beq_false_of_ne _ _ h_new]

set_option maxHeartbeats 64000000 in
theorem morpho_setFeeRecipient_spec_reverts_nonowner
    (state : ContractState) (newFeeRecipient : Address) (sender : Address)
    (h_notowner : state.storageAddr 0 ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setFeeRecipient"
      args := [newFeeRecipient.val]
    }
    (interpretSpec morphoGeneratedSpec (morphoFeeRecipientSpecStorage state) specTx).success = false := by
  morpho_spec_setFeeRecipient_lookup
  unfold MorphoViewSlice.setFeeRecipient_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_beq_false_of_ne _ _ (Ne.symm h_notowner)]

end Morpho.Proofs.SpecCorrectness.SetFeeRecipient
