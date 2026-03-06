import Morpho.Compiler.MacroSlice
import Morpho.Types

namespace Morpho.EDSLAdapter

open Verity
open Morpho.Types
open Morpho.Compiler.MacroSlice

/--
Encode `MorphoState` to `ContractState` using the storage layout declared in
`MacroSlice.lean`. This is the executable adapter boundary for admin operations
already migrated to the canonical `verity_contract` source.
-/
def encodeMorphoState (s : MorphoState) : ContractState :=
  ContractState.mk
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
      if n == 5 then (if s.isLltvEnabled key then 1 else 0)
      else 0)
    (fun n key1 key2 =>
      if n == 6 then (if s.isAuthorized key1 key2 then 1 else 0)
      else 0)
    s.sender
    0
    0
    s.blockTimestamp
    (fun _ => Core.FiniteAddressSet.empty)
    []

/-- Execute the migrated EDSL `setOwner` transition and decode the updated state. -/
noncomputable def setOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setOwner newOwner) state with
  | .success _ newState => some { s with owner := newState.storageAddr 0 }
  | .revert _ _ => none

/-- Execute the migrated EDSL `setFeeRecipient` transition and decode the updated state. -/
noncomputable def setFeeRecipient (s : MorphoState) (newFeeRecipient : Address) :
    Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setFeeRecipient newFeeRecipient) state with
  | .success _ newState => some { s with feeRecipient := newState.storageAddr 1 }
  | .revert _ _ => none

/-- Execute the migrated EDSL `enableIrm` transition and decode the updated state. -/
noncomputable def enableIrm (s : MorphoState) (irm : Address) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.enableIrm irm) state with
  | .success _ _ => some { s with
      isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a }
  | .revert _ _ => none

/-- Execute the migrated EDSL `enableLltv` transition and decode the updated state. -/
noncomputable def enableLltv (s : MorphoState) (lltv : Uint256) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.enableLltv lltv) state with
  | .success _ _ => some { s with
      isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l }
  | .revert _ _ => none

/-- Execute the migrated EDSL `setAuthorization` transition and decode the updated state. -/
noncomputable def setAuthorization (s : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) : Option MorphoState :=
  let state := encodeMorphoState s
  match (MorphoViewSlice.setAuthorization authorized newIsAuthorized) state with
  | .success _ _ => some { s with
      isAuthorized := fun authorizer auth =>
        if authorizer == s.sender && auth == authorized then newIsAuthorized
        else s.isAuthorized authorizer auth }
  | .revert _ _ => none

end Morpho.EDSLAdapter
