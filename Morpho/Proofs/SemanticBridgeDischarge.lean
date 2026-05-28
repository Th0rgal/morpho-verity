import Morpho.Specs.ContractSemantics
import Morpho.Compiler.AdminAdapters
import Morpho.Proofs.SolidityBridge

/-!
# Semantic Bridge Discharge: Link 1 (Canonical Contract Semantics ↔ EDSL)

This module proves the first link of the semantic bridge discharge chain:
the EDSL functions produced by `verity_contract` are equivalent to the
canonical spec-facing execution surface in `Morpho.Specs.ContractSemantics`.

## Architecture

The full discharge chain for an obligation like `setOwnerSemEq` has three links:

```
   Canonical contract semantics         -- this repo, ContractSemantics.lean
     ↕ Link 1 (this file)
   EDSL (Compiler.AdminAdapters.setOwner)
     ↕ Link 2 (verity SemanticBridge)
   Compiled IR (interpretIR morphoIR)   -- verity, bridge landed upstream
     ↕ Link 3 (verity EndToEnd)
   EVMYulLean (Yul execution)           -- verity, 1 keccak axiom
```

This file proves Link 1 for `setOwner`, `setFeeRecipient`, `enableIrm`,
`enableLltv`, `setAuthorization`, `setAuthorizationWithSig`, `createMarket`,
`accrueInterest`, `accrueInterestPublic`, `setFee`, `supply`, `repay`,
`withdraw`, `borrow`, `supplyCollateral`, `withdrawCollateral`, `liquidate`,
and `flashLoan`.
Links 2+3 are already provided upstream for the supported fragment (verity#1060 / #1065).
The remaining blockers here are trust-boundary proofs for raw-log payloads,
callbacks, and ECM-backed calls.
Verity pin: 00c18e3a (including static ABI/EIP-712 helpers, Solmate ERC20 ECMs,
typed oracle/IRM read ECMs, callback modules, and checked arithmetic helpers now
consumed by Morpho).

## Proof Strategy

For each function, we:
1. Reuse the EDSL-backed adapter exported from `Compiler.AdminAdapters`
2. Prove that adapter is definitionally equal to `ContractSemantics.f`
3. Conclude the corresponding `*SemEq` obligation is satisfiable

The key tactic pattern is:
- Unfold definitions including `Bind.bind` (to resolve do-notation `>>=`)
- Then `Verity.bind` + primitive definitions to reduce the monadic chain
- Then `by_cases` on the boolean conditions to case-split
- Then `simp_all` to close each case
-/

namespace Morpho.Proofs.SemanticBridgeDischarge

open Verity
open Morpho.Types

/-! ## Canonical State Encoding

`Morpho.Specs.ContractSemantics` owns the canonical spec-facing execution
surface for the already-migrated operations covered here. This file states
Link 1 directly on that canonical module so the supported semantic bridge no
longer depends on `Morpho.Morpho` for these operations. -/

noncomputable abbrev edslSetOwner := Morpho.Compiler.AdminAdapters.setOwner

/-- **Link 1 for setOwner**: The EDSL `setOwner` matches the pure Lean model. -/
theorem setOwner_link1 :
    ∀ s newOwner,
      edslSetOwner s newOwner = Morpho.Specs.ContractSemantics.setOwner s newOwner := by
  intro s newOwner
  rfl

/-- The EDSL `setOwner` satisfies the semantic equivalence obligation. -/
theorem setOwner_semEq :
    SolidityBridge.setOwnerSemEq edslSetOwner :=
  setOwner_link1

noncomputable abbrev edslSetFeeRecipient := Morpho.Compiler.AdminAdapters.setFeeRecipient

/-- **Link 1 for setFeeRecipient**: EDSL matches the pure Lean model. -/
theorem setFeeRecipient_link1 :
    ∀ s newFeeRecipient,
      edslSetFeeRecipient s newFeeRecipient =
        Morpho.Specs.ContractSemantics.setFeeRecipient s newFeeRecipient := by
  intro s newFeeRecipient
  rfl

/-- The EDSL `setFeeRecipient` satisfies the semantic equivalence obligation. -/
theorem setFeeRecipient_semEq :
    SolidityBridge.setFeeRecipientSemEq edslSetFeeRecipient :=
  setFeeRecipient_link1

noncomputable abbrev edslEnableIrm := Morpho.Compiler.AdminAdapters.enableIrm

/-- **Link 1 for enableIrm**: The EDSL `enableIrm` matches the pure Lean model. -/
theorem enableIrm_link1 :
    ∀ s irm, edslEnableIrm s irm = Morpho.Specs.ContractSemantics.enableIrm s irm := by
  intro s irm
  rfl

/-- The EDSL `enableIrm` satisfies the semantic equivalence obligation. -/
theorem enableIrm_semEq :
    SolidityBridge.enableIrmSemEq edslEnableIrm :=
  enableIrm_link1

noncomputable abbrev edslEnableLltv := Morpho.Compiler.AdminAdapters.enableLltv

/-- **Link 1 for enableLltv**: The EDSL `enableLltv` matches the pure Lean model. -/
theorem enableLltv_link1 :
    ∀ s lltv, edslEnableLltv s lltv = Morpho.Specs.ContractSemantics.enableLltv s lltv := by
  intro s lltv
  rfl

/-- The EDSL `enableLltv` satisfies the semantic equivalence obligation. -/
theorem enableLltv_semEq :
    SolidityBridge.enableLltvSemEq edslEnableLltv :=
  enableLltv_link1

noncomputable abbrev edslSetAuthorization := Morpho.Compiler.AdminAdapters.setAuthorization

/-- **Link 1 for setAuthorization**: The EDSL `setAuthorization` matches the pure Lean model. -/
theorem setAuthorization_link1 :
    ∀ s authorized newIsAuthorized,
      edslSetAuthorization s authorized newIsAuthorized =
        Morpho.Specs.ContractSemantics.setAuthorization s authorized newIsAuthorized := by
  intro s authorized newIsAuthorized
  rfl

/-- The EDSL `setAuthorization` satisfies the semantic equivalence obligation. -/
theorem setAuthorization_semEq :
    SolidityBridge.setAuthorizationSemEq edslSetAuthorization :=
  setAuthorization_link1

noncomputable abbrev edslSetAuthorizationWithSig :=
  Morpho.Compiler.AdminAdapters.setAuthorizationWithSig

theorem setAuthorizationWithSig_eq_morpho :
    ∀ s auth signatureValid,
      edslSetAuthorizationWithSig s auth signatureValid =
        Morpho.setAuthorizationWithSig s auth signatureValid := by
  intro s auth signatureValid
  unfold edslSetAuthorizationWithSig Morpho.Compiler.AdminAdapters.setAuthorizationWithSig
    Morpho.setAuthorizationWithSig Morpho.u256
  by_cases hDeadline : s.blockTimestamp.val > auth.deadline.val
  · simp [hDeadline]
  · by_cases hNonce : auth.nonce != s.nonce auth.authorizer
    · simp [hDeadline, hNonce]
    · by_cases hSig : ¬signatureValid
      · simp [hDeadline, hNonce, hSig]
      · simp [hDeadline, hNonce, hSig]

/-- The EDSL `setAuthorizationWithSig` satisfies the semantic equivalence obligation. -/
theorem setAuthorizationWithSig_semEq :
    SolidityBridge.setAuthorizationWithSigSemEq edslSetAuthorizationWithSig :=
  setAuthorizationWithSig_eq_morpho

noncomputable abbrev edslFlashLoan := Morpho.Compiler.AdminAdapters.flashLoan

noncomputable abbrev edslCreateMarket := Morpho.Compiler.AdminAdapters.createMarket

/-- **Link 1 for createMarket**: The EDSL `createMarket` matches the canonical state model. -/
theorem createMarket_link1 :
    ∀ s params,
      edslCreateMarket s params = Morpho.Specs.ContractSemantics.createMarket s params := by
  intro s params
  rfl

theorem createMarket_eq_morpho :
    ∀ s params,
      edslCreateMarket s params = Morpho.createMarket s params := by
  intro s params
  unfold edslCreateMarket Morpho.Compiler.AdminAdapters.createMarket Morpho.createMarket
  by_cases hIrm : s.isIrmEnabled params.irm
  · by_cases hLltv : s.isLltvEnabled params.lltv
    · by_cases hLast : (s.market (marketId params)).lastUpdate.val = 0
      · simp [hIrm, hLltv, hLast]
      · simp [hIrm, hLltv, hLast]
    · simp [hIrm, hLltv]
  · simp [hIrm]

/-- The EDSL `createMarket` satisfies the semantic equivalence obligation. -/
theorem createMarket_semEq :
    SolidityBridge.createMarketSemEq edslCreateMarket :=
  createMarket_eq_morpho

noncomputable abbrev edslAccrueInterest
    (s : MorphoState) (id : Id) (borrowRate : Uint256) (hasIrm : Bool) : MorphoState :=
  Morpho.Compiler.AdminAdapters.accrueInterest s id borrowRate hasIrm

theorem accrueInterest_eq_morpho :
    ∀ s id borrowRate hasIrm,
      edslAccrueInterest s id borrowRate hasIrm =
        Morpho.accrueInterest s id borrowRate hasIrm := by
  intro s id borrowRate hasIrm
  unfold edslAccrueInterest Morpho.Compiler.AdminAdapters.accrueInterest
    Morpho.accrueInterest Morpho.u256
  rfl

/-- The EDSL `accrueInterest` satisfies the semantic equivalence obligation. -/
theorem accrueInterest_semEq :
    SolidityBridge.accrueInterestSemEq edslAccrueInterest :=
  accrueInterest_eq_morpho

noncomputable abbrev edslAccrueInterestPublic
    (s : MorphoState) (id : Id) (borrowRate : Uint256) (hasIrm : Bool) : Option MorphoState :=
  Morpho.Compiler.AdminAdapters.accrueInterestPublic s id borrowRate hasIrm

theorem accrueInterestPublic_eq_morpho :
    ∀ s id borrowRate hasIrm,
      edslAccrueInterestPublic s id borrowRate hasIrm =
        Morpho.accrueInterestPublic s id borrowRate hasIrm := by
  intro s id borrowRate hasIrm
  unfold edslAccrueInterestPublic Morpho.Compiler.AdminAdapters.accrueInterestPublic
    Morpho.accrueInterestPublic
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · simp [hLast, accrueInterest_eq_morpho]

/-- The EDSL public `accrueInterest` wrapper satisfies the semantic equivalence obligation. -/
theorem accrueInterestPublic_semEq :
    SolidityBridge.accrueInterestPublicSemEq edslAccrueInterestPublic :=
  accrueInterestPublic_eq_morpho

noncomputable abbrev edslSetFee
    (s : MorphoState) (id : Id) (newFee borrowRate : Uint256) (hasIrm : Bool) :
    Option MorphoState :=
  Morpho.Compiler.AdminAdapters.setFee s id newFee borrowRate hasIrm

theorem setFee_eq_morpho :
    ∀ s id newFee borrowRate hasIrm,
      edslSetFee s id newFee borrowRate hasIrm =
        Morpho.setFee s id newFee borrowRate hasIrm := by
  intro s id newFee borrowRate hasIrm
  unfold edslSetFee Morpho.Compiler.AdminAdapters.setFee Morpho.setFee
  by_cases hOwner : s.sender = s.owner
  · by_cases hLast : (s.market id).lastUpdate.val = 0
    · simp [hOwner, hLast]
    · by_cases hFee : newFee = (s.market id).fee
      · simp [hOwner, hLast, hFee]
      · by_cases hBound : newFee.val > Morpho.Libraries.ConstantsLib.MAX_FEE
        · simp [hOwner, hLast, hFee, hBound]
        · simp [hOwner, hLast, hFee, hBound, accrueInterest_eq_morpho]
  · simp [hOwner]

/-- The EDSL `setFee` satisfies the semantic equivalence obligation. -/
theorem setFee_semEq :
    SolidityBridge.setFeeSemEq edslSetFee :=
  setFee_eq_morpho

theorem isSenderAuthorized_eq_morpho :
    ∀ s onBehalf,
      Morpho.Compiler.AdminAdapters.isSenderAuthorized s onBehalf =
        Morpho.isSenderAuthorized s onBehalf := by
  intro s onBehalf
  unfold Morpho.Compiler.AdminAdapters.isSenderAuthorized Morpho.isSenderAuthorized
  rfl

theorem isHealthy_eq_morpho :
    ∀ s id borrower collateralPrice lltv,
      Morpho.Compiler.AdminAdapters.isHealthy s id borrower collateralPrice lltv =
        Morpho.isHealthy s id borrower collateralPrice lltv := by
  intro s id borrower collateralPrice lltv
  unfold Morpho.Compiler.AdminAdapters.isHealthy Morpho.isHealthy
    Morpho.Compiler.AdminAdapters.u256 Morpho.u256
  rfl

noncomputable abbrev edslSupplyCollateral
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address) :
    Option MorphoState :=
  Morpho.Compiler.AdminAdapters.supplyCollateral s id assets onBehalf

theorem supplyCollateral_eq_morpho :
    ∀ s id assets onBehalf,
      edslSupplyCollateral s id assets onBehalf =
        Morpho.supplyCollateral s id assets onBehalf := by
  intro s id assets onBehalf
  unfold edslSupplyCollateral Morpho.Compiler.AdminAdapters.supplyCollateral
    Morpho.supplyCollateral
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hAssets : assets.val = 0
    · simp [hLast, hAssets]
    · by_cases hOnBehalf : onBehalf = 0
      · simp [hLast, hAssets, hOnBehalf]
      · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
        simp [hLast, hAssets, hOnBehalf]

/-- The EDSL `supplyCollateral` satisfies the semantic equivalence obligation. -/
theorem supplyCollateral_semEq :
    SolidityBridge.supplyCollateralSemEq edslSupplyCollateral :=
  supplyCollateral_eq_morpho

noncomputable abbrev edslSupply
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address) :
    Option (Uint256 × Uint256 × MorphoState) :=
  Morpho.Compiler.AdminAdapters.supply s id assets shares onBehalf

theorem supply_eq_morpho :
    ∀ s id assets shares onBehalf,
      edslSupply s id assets shares onBehalf =
        Morpho.supply s id assets shares onBehalf := by
  intro s id assets shares onBehalf
  unfold edslSupply Morpho.Compiler.AdminAdapters.supply Morpho.supply
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hOne : Morpho.Libraries.UtilsLib.exactlyOneZero assets shares
    · by_cases hOnBehalf : onBehalf = 0
      · simp [hLast, hOne, hOnBehalf]
      · by_cases hAssets : assets.val > 0
        · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
          simp [hLast, hOne, hOnBehalf, hAssets]
        · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
          simp [hLast, hOne, hOnBehalf, hAssets]
    · simp [hLast, hOne]

/-- The EDSL `supply` satisfies the semantic equivalence obligation. -/
theorem supply_semEq :
    SolidityBridge.supplySemEq edslSupply :=
  supply_eq_morpho

noncomputable abbrev edslRepay
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address) :
    Option (Uint256 × Uint256 × MorphoState) :=
  Morpho.Compiler.AdminAdapters.repay s id assets shares onBehalf

theorem repay_eq_morpho :
    ∀ s id assets shares onBehalf,
      edslRepay s id assets shares onBehalf =
        Morpho.repay s id assets shares onBehalf := by
  intro s id assets shares onBehalf
  unfold edslRepay Morpho.Compiler.AdminAdapters.repay Morpho.repay
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hOne : Morpho.Libraries.UtilsLib.exactlyOneZero assets shares
    · by_cases hOnBehalf : onBehalf = 0
      · simp [hLast, hOne, hOnBehalf]
      · by_cases hAssets : assets.val > 0
        · by_cases hShares : (s.position id onBehalf).borrowShares.val <
              (Morpho.Libraries.SharesMathLib.toSharesDown assets (s.market id).totalBorrowAssets
                (s.market id).totalBorrowShares).val
          · simp [hLast, hOne, hOnBehalf, hAssets, hShares]
          · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
            simp [hLast, hOne, hOnBehalf, hAssets, hShares]
        · by_cases hShares : (s.position id onBehalf).borrowShares.val < shares.val
          · simp [hLast, hOne, hOnBehalf, hAssets, hShares]
          · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
            simp [hLast, hOne, hOnBehalf, hAssets, hShares]
    · simp [hLast, hOne]

/-- The EDSL `repay` satisfies the semantic equivalence obligation. -/
theorem repay_semEq :
    SolidityBridge.repaySemEq edslRepay :=
  repay_eq_morpho

noncomputable abbrev edslWithdraw
    (s : MorphoState) (id : Id) (assets shares : Uint256)
    (onBehalf receiver : Address) :
    Option (Uint256 × Uint256 × MorphoState) :=
  Morpho.Compiler.AdminAdapters.withdraw s id assets shares onBehalf receiver

theorem withdraw_eq_morpho :
    ∀ s id assets shares onBehalf receiver,
      edslWithdraw s id assets shares onBehalf receiver =
        Morpho.withdraw s id assets shares onBehalf receiver := by
  intro s id assets shares onBehalf receiver
  unfold edslWithdraw Morpho.Compiler.AdminAdapters.withdraw Morpho.withdraw
    Morpho.Compiler.AdminAdapters.isSenderAuthorized Morpho.isSenderAuthorized
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hOne : Morpho.Libraries.UtilsLib.exactlyOneZero assets shares
    · by_cases hReceiver : receiver = 0
      · simp [hLast, hOne, hReceiver]
      · by_cases hAuth : s.sender == onBehalf || s.isAuthorized onBehalf s.sender
        · by_cases hAssets : assets.val > 0
          · by_cases hSupply : (s.position id onBehalf).supplyShares.val <
                (Morpho.Libraries.SharesMathLib.toSharesUp assets
                  (s.market id).totalSupplyAssets (s.market id).totalSupplyShares).val
            · simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
            · by_cases hLiq :
                  (s.market id).totalBorrowAssets.val >
                    (Morpho.u256 ((s.market id).totalSupplyAssets.val - assets.val)).val
              · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
                simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
              · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
                simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
          · by_cases hSupply : (s.position id onBehalf).supplyShares.val < shares.val
            · simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
            · by_cases hLiq :
                  (s.market id).totalBorrowAssets.val >
                    (Morpho.u256 ((s.market id).totalSupplyAssets.val -
                      (Morpho.Libraries.SharesMathLib.toAssetsDown shares
                        (s.market id).totalSupplyAssets (s.market id).totalSupplyShares).val)).val
              · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
                simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
              · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
                simp [hLast, hOne, hReceiver, hAuth, hAssets, hSupply]
        · simp [hLast, hOne, hReceiver, hAuth]
    · simp [hLast, hOne]

/-- The EDSL `withdraw` satisfies the semantic equivalence obligation. -/
theorem withdraw_semEq :
    SolidityBridge.withdrawSemEq edslWithdraw :=
  withdraw_eq_morpho

noncomputable abbrev edslBorrow
    (s : MorphoState) (id : Id) (assets shares : Uint256)
    (onBehalf receiver : Address) (collateralPrice lltv : Uint256) :
    Option (Uint256 × Uint256 × MorphoState) :=
  Morpho.Compiler.AdminAdapters.borrow s id assets shares onBehalf receiver collateralPrice lltv

theorem borrow_eq_morpho :
    ∀ s id assets shares onBehalf receiver collateralPrice lltv,
      edslBorrow s id assets shares onBehalf receiver collateralPrice lltv =
        Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv := by
  intro s id assets shares onBehalf receiver collateralPrice lltv
  unfold edslBorrow Morpho.Compiler.AdminAdapters.borrow Morpho.borrow
    Morpho.Compiler.AdminAdapters.isSenderAuthorized Morpho.isSenderAuthorized
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hOne : Morpho.Libraries.UtilsLib.exactlyOneZero assets shares
    · by_cases hReceiver : receiver = 0
      · simp [hLast, hOne, hReceiver]
      · by_cases hAuth : s.sender == onBehalf || s.isAuthorized onBehalf s.sender
        · by_cases hAssets : assets.val > 0
          · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
            simp [hLast, hOne, hReceiver, hAuth, hAssets, isHealthy_eq_morpho]
          · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
            simp [hLast, hOne, hReceiver, hAuth, hAssets, isHealthy_eq_morpho]
        · simp [hLast, hOne, hReceiver, hAuth]
    · simp [hLast, hOne]

/-- The EDSL `borrow` satisfies the semantic equivalence obligation. -/
theorem borrow_semEq :
    SolidityBridge.borrowSemEq edslBorrow :=
  borrow_eq_morpho

noncomputable abbrev edslWithdrawCollateral
    (s : MorphoState) (id : Id) (assets : Uint256)
    (onBehalf receiver : Address) (collateralPrice lltv : Uint256) :
    Option MorphoState :=
  Morpho.Compiler.AdminAdapters.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv

theorem withdrawCollateral_eq_morpho :
    ∀ s id assets onBehalf receiver collateralPrice lltv,
      edslWithdrawCollateral s id assets onBehalf receiver collateralPrice lltv =
        Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv := by
  intro s id assets onBehalf receiver collateralPrice lltv
  unfold edslWithdrawCollateral Morpho.Compiler.AdminAdapters.withdrawCollateral
    Morpho.withdrawCollateral
  by_cases hLast : (s.market id).lastUpdate.val = 0
  · simp [hLast]
  · by_cases hAssets : assets.val = 0
    · simp [hLast, hAssets]
    · by_cases hReceiver : receiver = 0
      · simp [hLast, hAssets, hReceiver]
      · by_cases hAuth : Morpho.isSenderAuthorized s onBehalf
        · by_cases hCollateral : (s.position id onBehalf).collateral.val < assets.val
          · simp [hLast, hAssets, hReceiver, hAuth, hCollateral, isSenderAuthorized_eq_morpho]
          · unfold Morpho.Compiler.AdminAdapters.u256 Morpho.u256
            simp [hLast, hAssets, hReceiver, hAuth, hCollateral,
              isSenderAuthorized_eq_morpho, isHealthy_eq_morpho]
        · simp [hLast, hAssets, hReceiver, hAuth, isSenderAuthorized_eq_morpho]

/-- The EDSL `withdrawCollateral` satisfies the semantic equivalence obligation. -/
theorem withdrawCollateral_semEq :
    SolidityBridge.withdrawCollateralSemEq edslWithdrawCollateral :=
  withdrawCollateral_eq_morpho

noncomputable abbrev edslLiquidate
    (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares collateralPrice lltv : Uint256) :
    Option (Uint256 × Uint256 × MorphoState) :=
  Morpho.Compiler.AdminAdapters.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv

theorem liquidate_eq_morpho :
    ∀ s id borrower seizedAssets repaidShares collateralPrice lltv,
      edslLiquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
        Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv := by
  intro s id borrower seizedAssets repaidShares collateralPrice lltv
  unfold edslLiquidate Morpho.Compiler.AdminAdapters.liquidate Morpho.liquidate
    Morpho.Compiler.AdminAdapters.u256 Morpho.u256
  simp [isHealthy_eq_morpho]

/-- The EDSL `liquidate` satisfies the semantic equivalence obligation. -/
theorem liquidate_semEq :
    SolidityBridge.liquidateSemEq edslLiquidate :=
  liquidate_eq_morpho

/-- **Link 1 for flashLoan**: The EDSL `flashLoan` matches the canonical state model. -/
theorem flashLoan_link1 :
    ∀ s assets,
      edslFlashLoan s assets = Morpho.Specs.ContractSemantics.flashLoan s assets := by
  intro s assets
  rfl

/-- The EDSL `flashLoan` satisfies the semantic equivalence obligation. -/
theorem flashLoan_semEq :
    SolidityBridge.flashLoanSemEq edslFlashLoan :=
  flashLoan_link1

/-! ## Discharge Status

With Link 1 proven for all 18 operations, the `*SemEq` obligations can be instantiated
using the EDSL-based wrappers. For the supported fragment, Links 2+3 are already
provided upstream from EDSL execution through compiled IR to EVMYulLean. The
remaining repo-local gaps are trust-boundary discharges for external I/O,
raw-log payloads, callbacks, and ECM-backed calls.

| Phase | Operations | Link 1 | Links 2+3 |
|-------|-----------|--------|-----------|
| 1 | setOwner, setFeeRecipient | **proven** | typed-IR bridge available at pin `00c18e3a` |
| 2 | enableIrm, enableLltv, setAuthorization, setAuthorizationWithSig | **proven** | typed-IR bridge available at pin `00c18e3a` |
| 3 | flashLoan | **proven** | pending external I/O bridge work for ERC20 transfer and callback ECMs |
| 4 | createMarket | **proven** | pending post-create IRM ECM trust-boundary discharge |
| 5 | accrueInterest, accrueInterestPublic | **proven** | pending typed IRM ECM trust-boundary discharge |
| 6 | setFee | **proven** | pending accrue-interest/IRM ECM trust-boundary discharge |
| 7 | supply, withdraw, repay, borrow, supplyCollateral, withdrawCollateral, liquidate | **proven** | pending callback/ERC20/oracle ECM trust-boundary discharge |
-/

end Morpho.Proofs.SemanticBridgeDischarge
