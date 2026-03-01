import Compiler.Specs
import Compiler.DiffTestTypes
import Verity.Proofs.Stdlib.SpecInterpreter
import Verity.Proofs.Stdlib.Automation
import Morpho.Compiler.MacroSlice
import Morpho.Compiler.Generated

/-!
# SpecCorrectness for enableLltv: EDSL ↔ CompilationModel (Link 2)

Proves that `interpretSpec morphoGeneratedSpec` matches the EDSL execution of
`MorphoViewSlice.enableLltv`. Uses uint-keyed mapping (slot 5) and WAD check.

The param "lltv" has type uint256, so evalExpr masks with Core.Uint256.modulus.
We need `@[simp]` lemmas for `x.val % modulus = x.val` and concrete literal reductions
(`1000000000000000000 % modulus`, `0 % modulus`, `1 % modulus`) so that simp can
fully reduce the spec interpreter's uint256 masking paths.
-/

namespace Morpho.Proofs.SpecCorrectness.EnableLltv

open Verity
open Verity.Proofs.Stdlib.SpecInterpreter
open Verity.Proofs.Stdlib.Automation
open Compiler.CompilationModel
open Compiler.DiffTestTypes
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.Generated

/-- Uint256 values are already below modulus, so mod is identity. -/
@[simp] theorem uint256_val_mod_eq (x : Uint256) :
    x.val % Core.Uint256.modulus = x.val :=
  Nat.mod_eq_of_lt x.isLt

/-- WAD literal (10^18) is below Uint256 modulus (2^256). -/
@[simp] theorem wad_mod_uint256_modulus :
    1000000000000000000 % Core.Uint256.modulus = 1000000000000000000 := by
  native_decide

/-- Literal 0 mod modulus = 0. -/
@[simp] theorem zero_mod_uint256_modulus :
    0 % Core.Uint256.modulus = 0 := by
  native_decide

/-- Literal 1 mod modulus = 1. -/
@[simp] theorem one_mod_uint256_modulus :
    1 % Core.Uint256.modulus = 1 := by
  native_decide

/-- SpecStorage for enableLltv: slot 0 (owner) + mapping at slot 5 (isLltvEnabled). -/
def enableLltvSpecStorage (state : ContractState) (lltv : Nat) (lltvEnabled : Bool) :
    SpecStorage :=
  { slots := [(0, state.storageAddr 0 |>.val)]
    mappings := [(5, [(lltv, if lltvEnabled then 1 else 0)])]
    mappings2 := []
    events := [] }

-- Phase 1-3 function lookup
syntax "morpho_spec_enableLltv_lookup" : tactic
macro_rules
  | `(tactic| morpho_spec_enableLltv_lookup) => `(tactic| (
    simp only [interpretSpec, morphoGeneratedSpec, MorphoViewSlice.spec,
      enableLltvSpecStorage, execFunction, resolveFields,
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
      MorphoViewSlice.enableIrm_model,
      MorphoViewSlice.enableLltv_model]
    simp (config := { decide := true })))

set_option maxHeartbeats 64000000 in
theorem morpho_enableLltv_spec_succeeds
    (state : ContractState) (lltv : Uint256) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_below_wad : lltv.val < 1000000000000000000) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableLltv"
      args := [lltv.val]
    }
    (interpretSpec morphoGeneratedSpec (enableLltvSpecStorage state lltv.val false) specTx).success = true := by
  morpho_spec_enableLltv_lookup
  unfold MorphoViewSlice.enableLltv_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.getMapping, SpecStorage.setMapping,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    uint256_val_mod_eq,
    h_owner, h_below_wad]

set_option maxHeartbeats 64000000 in
theorem morpho_enableLltv_spec_reverts_nonowner
    (state : ContractState) (lltv : Uint256) (sender : Address)
    (h_notowner : state.storageAddr 0 ≠ sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableLltv"
      args := [lltv.val]
    }
    (interpretSpec morphoGeneratedSpec (enableLltvSpecStorage state lltv.val false) specTx).success = false := by
  morpho_spec_enableLltv_lookup
  unfold MorphoViewSlice.enableLltv_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_beq_false_of_ne _ _ (Ne.symm h_notowner)]

set_option maxHeartbeats 64000000 in
theorem morpho_enableLltv_spec_reverts_already_set
    (state : ContractState) (lltv : Uint256) (sender : Address)
    (h_owner : state.storageAddr 0 = sender) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableLltv"
      args := [lltv.val]
    }
    (interpretSpec morphoGeneratedSpec (enableLltvSpecStorage state lltv.val true) specTx).success = false := by
  morpho_spec_enableLltv_lookup
  unfold MorphoViewSlice.enableLltv_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.getMapping,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    uint256_val_mod_eq,
    h_owner]

set_option maxHeartbeats 64000000 in
theorem morpho_enableLltv_spec_reverts_above_wad
    (state : ContractState) (lltv : Uint256) (sender : Address)
    (h_owner : state.storageAddr 0 = sender)
    (h_above_wad : ¬(lltv.val < 1000000000000000000)) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "enableLltv"
      args := [lltv.val]
    }
    (interpretSpec morphoGeneratedSpec (enableLltvSpecStorage state lltv.val false) specTx).success = false := by
  morpho_spec_enableLltv_lookup
  unfold MorphoViewSlice.enableLltv_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getSlot, SpecStorage.getMapping,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter,
    List.getD,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    uint256_val_mod_eq,
    h_owner, h_above_wad]

end Morpho.Proofs.SpecCorrectness.EnableLltv
