/-!
# Semantic Bridge Instantiation: Concrete Invariant Proofs for EDSL Implementations

This module instantiates the parametric `SolidityBridge` theorems with the
concrete EDSL implementations proven equivalent to the pure Lean model in
`SemanticBridgeDischarge.lean`.

## What this proves

For each of the 5 Link 1-discharged operations (setOwner, setFeeRecipient,
enableIrm, enableLltv, setAuthorization), we instantiate all 4 invariant
preservation theorems (borrowLeSupply, alwaysCollateralized, irmMonotone,
lltvMonotone), producing **20 concrete, sorry-free theorems** that the EDSL
implementation preserves every Morpho invariant.

## What this validates

This is the **first real validation of the semantic bridge pipeline**:
1. `SemanticBridgeDischarge.lean` proves: EDSL impl = pure Lean model (Link 1)
2. `SolidityBridge.lean` proves: any impl satisfying SemEq preserves invariants
3. This file combines them: EDSL impl preserves all invariants

The remaining gap (Links 2+3) will eventually show that the compiled EVM
bytecode behaves identically to the EDSL. Once that lands, these theorems
extend to the on-chain Morpho contract automatically.
-/

import Morpho.Proofs.SemanticBridgeDischarge
import Morpho.Proofs.SolidityBridge

namespace Morpho.Proofs.SemanticBridgeInstantiation

open Morpho.Types
open Morpho.Proofs.SemanticBridgeDischarge
open SolidityBridge

/-! ## setOwner: EDSL preserves all invariants -/

/-- The EDSL `setOwner` preserves the borrow-le-supply invariant. -/
theorem edsl_setOwner_preserves_borrowLeSupply
    (s : MorphoState) (newOwner : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetOwner s newOwner = some s') :
    borrowLeSupply s' id :=
  solidity_setOwner_preserves_borrowLeSupply edslSetOwner setOwner_semEq s newOwner id s'
    h_solvent h_ok

/-- The EDSL `setOwner` preserves the always-collateralized invariant. -/
theorem edsl_setOwner_preserves_alwaysCollateralized
    (s : MorphoState) (newOwner : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetOwner s newOwner = some s') :
    alwaysCollateralized s' id user :=
  solidity_setOwner_preserves_alwaysCollateralized edslSetOwner setOwner_semEq s newOwner id user s'
    h_collat h_ok

/-- The EDSL `setOwner` preserves IRM monotonicity. -/
theorem edsl_setOwner_preserves_irmMonotone
    (s : MorphoState) (newOwner irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isIrmEnabled irm :=
  solidity_setOwner_preserves_irmMonotone edslSetOwner setOwner_semEq s newOwner irm s'
    h_enabled h_ok

/-- The EDSL `setOwner` preserves LLTV monotonicity. -/
theorem edsl_setOwner_preserves_lltvMonotone
    (s : MorphoState) (newOwner : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isLltvEnabled lltv :=
  solidity_setOwner_preserves_lltvMonotone edslSetOwner setOwner_semEq s newOwner lltv s'
    h_enabled h_ok

/-! ## setFeeRecipient: EDSL preserves all invariants -/

theorem edsl_setFeeRecipient_preserves_borrowLeSupply
    (s : MorphoState) (newFeeRecipient : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    borrowLeSupply s' id :=
  solidity_setFeeRecipient_preserves_borrowLeSupply edslSetFeeRecipient setFeeRecipient_semEq
    s newFeeRecipient id s' h_solvent h_ok

theorem edsl_setFeeRecipient_preserves_alwaysCollateralized
    (s : MorphoState) (newFeeRecipient : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    alwaysCollateralized s' id user :=
  solidity_setFeeRecipient_preserves_alwaysCollateralized edslSetFeeRecipient setFeeRecipient_semEq
    s newFeeRecipient id user s' h_collat h_ok

theorem edsl_setFeeRecipient_preserves_irmMonotone
    (s : MorphoState) (newFeeRecipient irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    s'.isIrmEnabled irm :=
  solidity_setFeeRecipient_preserves_irmMonotone edslSetFeeRecipient setFeeRecipient_semEq
    s newFeeRecipient irm s' h_enabled h_ok

theorem edsl_setFeeRecipient_preserves_lltvMonotone
    (s : MorphoState) (newFeeRecipient : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetFeeRecipient s newFeeRecipient = some s') :
    s'.isLltvEnabled lltv :=
  solidity_setFeeRecipient_preserves_lltvMonotone edslSetFeeRecipient setFeeRecipient_semEq
    s newFeeRecipient lltv s' h_enabled h_ok

/-! ## enableIrm: EDSL preserves all invariants -/

theorem edsl_enableIrm_preserves_borrowLeSupply
    (s : MorphoState) (irm : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableIrm s irm = some s') :
    borrowLeSupply s' id :=
  solidity_enableIrm_preserves_borrowLeSupply edslEnableIrm enableIrm_semEq
    s irm id s' h_solvent h_ok

theorem edsl_enableIrm_preserves_alwaysCollateralized
    (s : MorphoState) (irm : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslEnableIrm s irm = some s') :
    alwaysCollateralized s' id user :=
  solidity_enableIrm_preserves_alwaysCollateralized edslEnableIrm enableIrm_semEq
    s irm id user s' h_collat h_ok

theorem edsl_enableIrm_preserves_irmMonotone
    (s : MorphoState) (irmCall irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableIrm s irmCall = some s') :
    s'.isIrmEnabled irm :=
  solidity_enableIrm_preserves_irmMonotone edslEnableIrm enableIrm_semEq
    s irmCall irm s' h_enabled h_ok

theorem edsl_enableIrm_preserves_lltvMonotone
    (s : MorphoState) (irmCall : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslEnableIrm s irmCall = some s') :
    s'.isLltvEnabled lltv :=
  solidity_enableIrm_preserves_lltvMonotone edslEnableIrm enableIrm_semEq
    s irmCall lltv s' h_enabled h_ok

/-! ## enableLltv: EDSL preserves all invariants -/

theorem edsl_enableLltv_preserves_borrowLeSupply
    (s : MorphoState) (lltv : Uint256) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableLltv s lltv = some s') :
    borrowLeSupply s' id :=
  solidity_enableLltv_preserves_borrowLeSupply edslEnableLltv enableLltv_semEq
    s lltv id s' h_solvent h_ok

theorem edsl_enableLltv_preserves_alwaysCollateralized
    (s : MorphoState) (lltv : Uint256) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslEnableLltv s lltv = some s') :
    alwaysCollateralized s' id user :=
  solidity_enableLltv_preserves_alwaysCollateralized edslEnableLltv enableLltv_semEq
    s lltv id user s' h_collat h_ok

theorem edsl_enableLltv_preserves_irmMonotone
    (s : MorphoState) (lltvCall : Uint256) (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableLltv s lltvCall = some s') :
    s'.isIrmEnabled irm :=
  solidity_enableLltv_preserves_irmMonotone edslEnableLltv enableLltv_semEq
    s lltvCall irm s' h_enabled h_ok

theorem edsl_enableLltv_preserves_lltvMonotone
    (s : MorphoState) (lltvCall lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslEnableLltv s lltvCall = some s') :
    s'.isLltvEnabled lltv :=
  solidity_enableLltv_preserves_lltvMonotone edslEnableLltv enableLltv_semEq
    s lltvCall lltv s' h_enabled h_ok

/-! ## setAuthorization: EDSL preserves all invariants -/

theorem edsl_setAuthorization_preserves_borrowLeSupply
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    borrowLeSupply s' id :=
  solidity_setAuthorization_preserves_borrowLeSupply edslSetAuthorization setAuthorization_semEq
    s authorized newIsAuthorized id s' h_solvent h_ok

theorem edsl_setAuthorization_preserves_alwaysCollateralized
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    alwaysCollateralized s' id user :=
  solidity_setAuthorization_preserves_alwaysCollateralized edslSetAuthorization
    setAuthorization_semEq s authorized newIsAuthorized id user s' h_collat h_ok

theorem edsl_setAuthorization_preserves_irmMonotone
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    s'.isIrmEnabled irm :=
  solidity_setAuthorization_preserves_irmMonotone edslSetAuthorization setAuthorization_semEq
    s authorized newIsAuthorized irm s' h_enabled h_ok

theorem edsl_setAuthorization_preserves_lltvMonotone
    (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetAuthorization s authorized newIsAuthorized = some s') :
    s'.isLltvEnabled lltv :=
  solidity_setAuthorization_preserves_lltvMonotone edslSetAuthorization setAuthorization_semEq
    s authorized newIsAuthorized lltv s' h_enabled h_ok

/-! ## Summary

20 concrete invariant preservation theorems, zero sorry.

For each of the 5 Link 1-discharged operations, we have proven that the EDSL
implementation preserves:
- `borrowLeSupply`: total borrow assets ≤ total supply assets per market
- `alwaysCollateralized`: every borrower's position is adequately collateralized
- `irmMonotone`: once an IRM is enabled, it stays enabled
- `lltvMonotone`: once an LLTV is enabled, it stays enabled

### Remaining gap: Links 2+3

These theorems prove that the EDSL execution preserves Morpho invariants.
The full semantic bridge additionally requires:

- **Link 2**: EDSL ≡ compiled CompilationModel IR
  (needs verity pin bump from `dccb984` to `semantic-bridge` branch)
- **Link 3**: compiled IR ≡ EVMYulLean(Yul)
  (proven in verity `Compiler/Proofs/EndToEnd.lean`)

Once Links 2+3 are composed, these 20 theorems extend to the compiled
EVM bytecode automatically: if `edslSetOwner` preserves `borrowLeSupply`,
and the compiled bytecode behaves identically to `edslSetOwner`, then the
compiled bytecode also preserves `borrowLeSupply`.
-/

end Morpho.Proofs.SemanticBridgeInstantiation
