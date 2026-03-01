import Compiler.Specs
import Compiler.DiffTestTypes
import Verity.Proofs.Stdlib.SpecInterpreter
import Verity.Proofs.Stdlib.Automation
import Morpho.Compiler.MacroSlice
import Morpho.Compiler.Generated

/-!
# SpecCorrectness for setAuthorization: EDSL ↔ CompilationModel (Link 2)

Proves that `interpretSpec morphoGeneratedSpec` matches the EDSL execution of
`MorphoViewSlice.setAuthorization`. Uses 2D mapping at slot 6 (isAuthorized)
with key1 = sender (address), key2 = authorized (address), and a bool param
`newIsAuthorized` that determines the if/else branch.

Key simp requirements:
- `uint256_val_mod_eq`: for `x.val % Core.Uint256.modulus = x.val`
- `zero/one_mod_uint256_modulus`: for literal mod reductions
- `SpecStorage.getMapping2/setMapping2`: 2D mapping operations
-/

namespace Morpho.Proofs.SpecCorrectness.SetAuthorization

open Verity
open Verity.Proofs.Stdlib.SpecInterpreter
open Verity.Proofs.Stdlib.Automation
open Compiler.CompilationModel
open Compiler.DiffTestTypes
open Morpho.Compiler.MacroSlice
open Morpho.Compiler.Generated

/-- Literal 0 mod Uint256 modulus = 0. -/
@[simp] theorem zero_mod_uint256_modulus :
    0 % Core.Uint256.modulus = 0 := by native_decide

/-- Literal 1 mod Uint256 modulus = 1. -/
@[simp] theorem one_mod_uint256_modulus :
    1 % Core.Uint256.modulus = 1 := by native_decide

/-- SpecStorage for setAuthorization: 2D mapping at slot 6 (isAuthorized).
    key1 = sender address, key2 = authorized address. -/
def setAuthSpecStorage (sender : Address) (authorized : Address) (currentlyAuthorized : Bool) :
    SpecStorage :=
  { slots := []
    mappings := []
    mappings2 := [(6, [((sender.val, authorized.val), if currentlyAuthorized then 1 else 0)])]
    events := [] }

-- Phase 1-3 function lookup macro
syntax "morpho_spec_setAuth_lookup" : tactic
macro_rules
  | `(tactic| morpho_spec_setAuth_lookup) => `(tactic| (
    simp only [interpretSpec, morphoGeneratedSpec, MorphoViewSlice.spec,
      setAuthSpecStorage, execFunction, resolveFields,
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
      MorphoViewSlice.enableLltv_model,
      MorphoViewSlice.setAuthorization_model]
    simp (config := { decide := true })))

set_option maxHeartbeats 64000000 in
theorem morpho_setAuthorization_spec_grant_succeeds
    (sender : Address) (authorized : Address) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setAuthorization"
      args := [authorized.val, 1]
    }
    (interpretSpec morphoGeneratedSpec (setAuthSpecStorage sender authorized false) specTx).success = true := by
  morpho_spec_setAuth_lookup
  unfold MorphoViewSlice.setAuthorization_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getMapping2, SpecStorage.setMapping2,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter, List.find?,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq]

set_option maxHeartbeats 64000000 in
theorem morpho_setAuthorization_spec_revoke_succeeds
    (sender : Address) (authorized : Address) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setAuthorization"
      args := [authorized.val, 0]
    }
    (interpretSpec morphoGeneratedSpec (setAuthSpecStorage sender authorized true) specTx).success = true := by
  morpho_spec_setAuth_lookup
  unfold MorphoViewSlice.setAuthorization_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getMapping2, SpecStorage.setMapping2,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter, List.find?,
    List.getD, List.foldl,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel, dedupNatPreserve,
    addressToNat_mod_eq]

set_option maxHeartbeats 64000000 in
theorem morpho_setAuthorization_spec_grant_reverts_already_set
    (sender : Address) (authorized : Address) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setAuthorization"
      args := [authorized.val, 1]
    }
    (interpretSpec morphoGeneratedSpec (setAuthSpecStorage sender authorized true) specTx).success = false := by
  morpho_spec_setAuth_lookup
  unfold MorphoViewSlice.setAuthorization_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getMapping2,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter, List.find?,
    List.getD,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_mod_eq]

set_option maxHeartbeats 64000000 in
theorem morpho_setAuthorization_spec_revoke_reverts_already_unset
    (sender : Address) (authorized : Address) :
    let specTx : Compiler.DiffTestTypes.Transaction := {
      sender := sender
      functionName := "setAuthorization"
      args := [authorized.val, 0]
    }
    (interpretSpec morphoGeneratedSpec (setAuthSpecStorage sender authorized false) specTx).success = false := by
  morpho_spec_setAuth_lookup
  unfold MorphoViewSlice.setAuthorization_modelBody
  simp [execStmts, execStmt, evalExpr,
    SpecStorage.getMapping2,
    List.findIdx?, List.findIdx?.go, List.lookup, List.filter, List.find?,
    List.getD,
    Option.map, Option.getD,
    stmtUsesUnsupportedLowLevel,
    addressToNat_mod_eq]

end Morpho.Proofs.SpecCorrectness.SetAuthorization
