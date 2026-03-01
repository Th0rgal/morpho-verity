import Compiler.Specs
import Compiler.DiffTestTypes
import Verity.Proofs.Stdlib.SpecInterpreter
import Verity.Proofs.Stdlib.Automation
import Morpho.Compiler.MacroSlice
import Morpho.Compiler.Generated

/-!
# SpecCorrectness for enableIrm: EDSL ↔ CompilationModel (Link 2)

Proves that `interpretSpec morphoGeneratedSpec` matches the EDSL execution of
`MorphoViewSlice.enableIrm` for all cases. Uses mapping storage (slot 4)
in addition to simple slot storage (slot 0 for ownership).
-/

namespace Morpho.Proofs.SpecCorrectness.EnableIrm

open Verity
open Verity.Proofs.Stdlib.SpecInterpreter
open Verity.Proofs.Stdlib.Automation
open Compiler.CompilationModel
open Compiler.DiffTestTypes
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.Generated

/-- SpecStorage for enableIrm: slot 0 (owner) + mapping at slot 4 (isIrmEnabled).
    `irmEnabled irm` indicates whether `irm` is currently enabled. -/
def enableIrmSpecStorage (state : ContractState) (irm : Address) (irmEnabled : Bool) :
    SpecStorage :=
  { slots := [(0, state.storageAddr 0 |>.val)]
    mappings := [(4, [(irm.val, if irmEnabled then 1 else 0)])]
    mappings2 := []
    events := [] }

-- Phase 1-3 function lookup tactic (same pattern as SetOwner, but including enableIrm_model)
syntax "morpho_spec_enableIrm_lookup" : tactic
macro_rules
  | `(tactic| morpho_spec_enableIrm_lookup) => `(tactic| (
    simp only [interpretSpec, morphoGeneratedSpec, MorphoViewSlice.spec,
      enableIrmSpecStorage, execFunction, resolveFields,
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
      MorphoViewSlice.setFeeRecipient_model,
      MorphoViewSlice.enableIrm_model]
    simp (config := { decide := true })))

set_option maxHeartbeats 64000000 in
theorem morpho_enableIrm_spec_succeeds
    (state : ContractState) (irm : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_not_enabled : true) :  -- irmEnabled = false in SpecStorage below
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableIrm"
      args := [irm.val]
    }
    (interpretSpec morphoGeneratedSpec (enableIrmSpecStorage state irm false) specTx).success = true := by
  morpho_spec_enableIrm_lookup
  unfold MorphoViewSlice.enableIrm_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.setSlot,
    SpecStorage.getMapping, SpecStorage.setMapping,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq, h_owner]

set_option maxHeartbeats 64000000 in
theorem morpho_enableIrm_spec_reverts_nonowner
    (state : ContractState) (irm : Address) (sender : Address)
    (h_notowner : state.storageAddr 0 ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableIrm"
      args := [irm.val]
    }
    (interpretSpec morphoGeneratedSpec (enableIrmSpecStorage state irm false) specTx).success = false := by
  morpho_spec_enableIrm_lookup
  unfold MorphoViewSlice.enableIrm_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_beq_false_of_ne _ _ (Ne.symm h_notowner)]

set_option maxHeartbeats 64000000 in
theorem morpho_enableIrm_spec_reverts_already_set
    (state : ContractState) (irm : Address) (sender : Address)
    (h_owner : state.storageAddr 0 = sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableIrm"
      args := [irm.val]
    }
    (interpretSpec morphoGeneratedSpec (enableIrmSpecStorage state irm true) specTx).success = false := by
  morpho_spec_enableIrm_lookup
  unfold MorphoViewSlice.enableIrm_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.getMapping,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_mod_eq, h_owner]

end Morpho.Proofs.SpecCorrectness.EnableIrm
