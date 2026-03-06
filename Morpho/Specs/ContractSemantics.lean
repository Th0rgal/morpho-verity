import Morpho.Compiler.AdminAdapters

namespace Morpho.Specs.ContractSemantics

open Verity
open Morpho.Types

/-!
# Contract Semantics

Canonical spec-facing execution surface for Morpho operations that already run
through the generated Verity contract semantics.

The admin operations below are the first migrated cluster. They deliberately
point at `Compiler.AdminAdapters`, so bridge/spec code can talk about the
generated contract semantics directly instead of routing through
`Morpho.Morpho` as an intermediate executable model.
-/

/-- Canonical Contract-monad-backed semantics for `setOwner`. -/
noncomputable abbrev setOwner : MorphoState → Address → Option MorphoState :=
  Morpho.Compiler.AdminAdapters.setOwner

/-- Canonical Contract-monad-backed semantics for `setFeeRecipient`. -/
noncomputable abbrev setFeeRecipient : MorphoState → Address → Option MorphoState :=
  Morpho.Compiler.AdminAdapters.setFeeRecipient

/-- Canonical Contract-monad-backed semantics for `enableIrm`. -/
noncomputable abbrev enableIrm : MorphoState → Address → Option MorphoState :=
  Morpho.Compiler.AdminAdapters.enableIrm

/-- Canonical Contract-monad-backed semantics for `enableLltv`. -/
noncomputable abbrev enableLltv : MorphoState → Uint256 → Option MorphoState :=
  Morpho.Compiler.AdminAdapters.enableLltv

/-- Canonical Contract-monad-backed semantics for `setAuthorization`. -/
noncomputable abbrev setAuthorization :
    MorphoState → Address → Bool → Option MorphoState :=
  Morpho.Compiler.AdminAdapters.setAuthorization

theorem setOwner_success_iff (s s' : MorphoState) (newOwner : Address) :
    setOwner s newOwner = some s' ↔
      s.sender = s.owner ∧ newOwner ≠ s.owner ∧ s' = { s with owner := newOwner } :=
  Morpho.Compiler.AdminAdapters.setOwner_success_iff s s' newOwner

theorem setFeeRecipient_success_iff (s s' : MorphoState) (newFeeRecipient : Address) :
    setFeeRecipient s newFeeRecipient = some s' ↔
      s.sender = s.owner ∧
      newFeeRecipient ≠ s.feeRecipient ∧
      s' = { s with feeRecipient := newFeeRecipient } :=
  Morpho.Compiler.AdminAdapters.setFeeRecipient_success_iff s s' newFeeRecipient

theorem enableIrm_success_iff (s s' : MorphoState) (irm : Address) :
    enableIrm s irm = some s' ↔
      s.sender = s.owner ∧
      ¬s.isIrmEnabled irm ∧
      s' = { s with isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a } :=
  Morpho.Compiler.AdminAdapters.enableIrm_success_iff s s' irm

theorem enableLltv_success_iff (s s' : MorphoState) (lltv : Uint256) :
    enableLltv s lltv = some s' ↔
      s.sender = s.owner ∧
      ¬s.isLltvEnabled lltv ∧
      lltv.val < Morpho.Libraries.MathLib.WAD ∧
      s' = { s with isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l } :=
  Morpho.Compiler.AdminAdapters.enableLltv_success_iff s s' lltv

theorem setAuthorization_success_iff (s s' : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) :
    setAuthorization s authorized newIsAuthorized = some s' ↔
      newIsAuthorized ≠ s.isAuthorized s.sender authorized ∧
      s' = { s with
        isAuthorized := fun authorizer auth =>
          if authorizer == s.sender && auth == authorized then newIsAuthorized
          else s.isAuthorized authorizer auth } :=
  Morpho.Compiler.AdminAdapters.setAuthorization_success_iff s s' authorized newIsAuthorized

end Morpho.Specs.ContractSemantics
