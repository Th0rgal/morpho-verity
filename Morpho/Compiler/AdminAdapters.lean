import Morpho.Types
import Morpho.Libraries.MathLib
import Morpho.Compiler.MacroSlice

namespace Morpho.Compiler.AdminAdapters

open Verity
open Morpho.Types
open Morpho.Compiler.MacroSlice

/-- Normalize a two-key boolean override to `false` into an `if` form. -/
private theorem overrideBool2False_eq_if {α β : Type} [DecidableEq α] [DecidableEq β]
    (f : α → β → Bool) (key1 : α) (key2 : β) :
    (fun a b => (!decide (a = key1) || !decide (b = key2)) && f a b) =
      (fun a b => if a = key1 ∧ b = key2 then false else f a b) := by
  funext a
  funext b
  by_cases h1 : a = key1 <;> by_cases h2 : b = key2 <;> simp [h1, h2]

/-- Normalize a two-key boolean override to `true` into an `if` form. -/
private theorem overrideBool2True_eq_if {α β : Type} [DecidableEq α] [DecidableEq β]
    (f : α → β → Bool) (key1 : α) (key2 : β) :
    (fun a b => decide (a = key1) && decide (b = key2) || f a b) =
      (fun a b => if a = key1 ∧ b = key2 then true else f a b) := by
  funext a
  funext b
  by_cases h1 : a = key1 <;> by_cases h2 : b = key2 <;> simp [h1, h2]

private theorem eq_comm_iff {α : Type} (a b : α) : (a = b ↔ b = a) := by
  constructor <;> intro h <;> simpa using h.symm

private def wordKeyId (key : Uint256) : Id :=
  key.val / 256

private def wordKeyOffset (key : Uint256) : Nat :=
  key.val % 256

private def packWord128 (lo hi : Uint256) : Uint256 :=
  Verity.Core.Uint256.add lo (Verity.Core.Uint256.shl 128 hi)

private def marketWordAt (m : Market) (offset : Nat) : Uint256 :=
  match offset with
  | 0 => packWord128 m.totalSupplyAssets m.totalSupplyShares
  | 1 => packWord128 m.totalBorrowAssets m.totalBorrowShares
  | 2 => packWord128 m.lastUpdate m.fee
  | _ => 0

private def idToParamsWordAt (params? : Option MarketParams) (offset : Nat) : Uint256 :=
  match params?, offset with
  | some params, 0 => params.loanToken
  | some params, 1 => params.collateralToken
  | some params, 2 => params.oracle
  | some params, 3 => params.irm
  | some params, 4 => params.lltv
  | _, _ => 0

/-- Encode `MorphoState` into the contract-state view expected by `MacroSlice`. -/
def encodeMorphoState (s : MorphoState) : ContractState :=
  ContractState.mk
    (fun _ => 0)
    (fun _ => 0)
    (fun n =>
      if n == 0 then s.owner
      else if n == 1 then s.feeRecipient
      else 0)
    (fun n key =>
      if n == 4 then (if s.isIrmEnabled key then 1 else 0)
      else if n == 7 then s.nonce key
      else 0)
    (fun n key =>
      if n == 3 then
        marketWordAt (s.market (wordKeyId key)) (wordKeyOffset key)
      else if n == 5 then
        if s.isLltvEnabled key then 1 else 0
      else if n == 8 then
        idToParamsWordAt (s.idToParams (wordKeyId key)) (wordKeyOffset key)
      else
        0)
    (fun n key1 key2 =>
      if n == 6 then (if s.isAuthorized key1 key2 then 1 else 0)
      else 0)
    (fun _ => [])
    s.sender
    0
    0
    s.blockTimestamp
    0
    0
    (fun _ => Core.FiniteAddressSet.empty)
    []

/-- Canonical EDSL-backed adapter for `setOwner`. -/
noncomputable def setOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setOwner newOwner) state with
  | .success _ newState => some { s with owner := newState.storageAddr 0 }
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `setFeeRecipient`. -/
noncomputable def setFeeRecipient (s : MorphoState) (newFeeRecipient : Address) :
    Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setFeeRecipient newFeeRecipient) state with
  | .success _ newState => some { s with feeRecipient := newState.storageAddr 1 }
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `enableIrm`. -/
noncomputable def enableIrm (s : MorphoState) (irm : Address) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.enableIrm irm) state with
  | .success _ _ => some { s with
      isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a }
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `enableLltv`. -/
noncomputable def enableLltv (s : MorphoState) (lltv : Uint256) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.enableLltv lltv) state with
  | .success _ _ => some { s with
      isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l }
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `setAuthorization`. -/
noncomputable def setAuthorization (s : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setAuthorization authorized newIsAuthorized) state with
  | .success _ _ => some { s with
      isAuthorized := fun authorizer auth =>
        if authorizer == s.sender && auth == authorized then newIsAuthorized
        else s.isAuthorized authorizer auth }
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `flashLoan`.
    The current state model abstracts token/callback I/O, so the adapter keeps the
    state-observable guard (`assets != 0`) and fixes the ignored token/data inputs. -/
noncomputable def flashLoan (s : MorphoState) (assets : Uint256) : Option Unit :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.flashLoan 0 assets ByteArray.empty) state with
  | .success _ _ => some ()
  | .revert _ _ => none

/-- Canonical EDSL-backed adapter for `createMarket`. -/
noncomputable def createMarket (s : MorphoState) (params : MarketParams) : Option MorphoState :=
  let state := encodeMorphoState s
  let marketParams :=
    (params.loanToken, (params.collateralToken, (params.oracle, (params.irm, params.lltv))))
  match (MorphoViewSlice.createMarket marketParams) state with
  | .success _ _ =>
      let id := marketId params
      some { s with
        market := fun id' =>
          if id' == id then
            { s.market id with lastUpdate := s.blockTimestamp }
          else
            s.market id'
        idToParams := fun id' =>
          if id' == id then some params else s.idToParams id' }
  | .revert _ _ => none

theorem setOwner_success_iff (s s' : MorphoState) (newOwner : Address) :
    setOwner s newOwner = some s' ↔
      s.sender = s.owner ∧ newOwner ≠ s.owner ∧ s' = { s with owner := newOwner } := by
  sorry

theorem setFeeRecipient_success_iff (s s' : MorphoState) (newFeeRecipient : Address) :
    setFeeRecipient s newFeeRecipient = some s' ↔
      s.sender = s.owner ∧
      newFeeRecipient ≠ s.feeRecipient ∧
      s' = { s with feeRecipient := newFeeRecipient } := by
  sorry

theorem enableIrm_success_iff (s s' : MorphoState) (irm : Address) :
    enableIrm s irm = some s' ↔
      s.sender = s.owner ∧
      ¬s.isIrmEnabled irm ∧
      s' = { s with isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a } := by
  sorry

theorem enableLltv_success_iff (s s' : MorphoState) (lltv : Uint256) :
    enableLltv s lltv = some s' ↔
      s.sender = s.owner ∧
      ¬s.isLltvEnabled lltv ∧
      lltv.val < Morpho.Libraries.MathLib.WAD ∧
      s' = { s with isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l } := by
  sorry

theorem setAuthorization_success_iff (s s' : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) :
    setAuthorization s authorized newIsAuthorized = some s' ↔
      newIsAuthorized ≠ s.isAuthorized s.sender authorized ∧
      s' = { s with
        isAuthorized := fun authorizer auth =>
          if authorizer == s.sender && auth == authorized then newIsAuthorized
          else s.isAuthorized authorizer auth } := by
  have h01 : (1 : Uint256) ≠ 0 := by decide
  cases newIsAuthorized <;>
    by_cases h : s.isAuthorized s.sender authorized <;>
    simp [setAuthorization, encodeMorphoState, MorphoViewSlice.setAuthorization,
      MorphoViewSlice.isAuthorizedSlot, Bind.bind, Verity.bind, Verity.msgSender,
      Verity.getMapping2, Verity.require, Verity.setMapping2, h,
      overrideBool2False_eq_if, overrideBool2True_eq_if, h01]
  all_goals
    first
    | exact eq_comm_iff _ _
    | assumption

-- TODO: Proof needs updating for emit/safeTransfer additions to function bodies
theorem flashLoan_success_iff (s : MorphoState) (assets : Uint256) :
    flashLoan s assets = some () ↔ assets ≠ 0 := by
  sorry

end Morpho.Compiler.AdminAdapters
