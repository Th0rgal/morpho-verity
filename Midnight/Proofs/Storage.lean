import Midnight.Proofs.Refinement

namespace Midnight.Proofs.Storage

open Midnight.Proofs.Basic
open Midnight.Proofs.RCF
open Midnight.Proofs.UnitsAccounting
open Midnight.Proofs.Refinement
open Midnight.Proofs.CollateralLoop
open Midnight.Proofs.BitmapSchedule
open Midnight.Proofs.MarketLedger

namespace LiquidateStorage

/-- Address-shaped values used by the observable event and transfer payer
    branch.  The zero value represents Solidity's `address(0)`. -/
abbrev Address := Nat

def zeroAddress : Address := 0

abbrev MarketId := Nat

abbrev Bytes32 := Nat

/-- Midnight's `CALLBACK_SUCCESS` value:
    `keccak256("morpho.midnight.callbackSuccess")`. -/
def CALLBACK_SUCCESS : Bytes32 :=
  0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2

/-- The `MarketState` fields touched by `Midnight.sol::liquidate`. -/
structure MarketState where
  totalUnits : Nat
  lossFactor : Nat
  withdrawable : Nat
  continuousFeeCredit : Nat
  totalUnitsUint128 : totalUnits ≤ UINT128_MAX
  lossFactorUint128 : lossFactor ≤ UINT128_MAX
  withdrawableUint128 : withdrawable ≤ UINT128_MAX
  continuousFeeCreditUint128 : continuousFeeCredit ≤ UINT128_MAX

/-- The liquidated borrower's `Position` fields touched by the proof model. -/
structure PositionState where
  debt : Nat
  selectedCollateral : Nat
  collateralBitmap : Nat
  debtUint128 : debt ≤ UINT128_MAX
  selectedCollateralUint128 : selectedCollateral ≤ UINT128_MAX
  collateralBitmapUint128 : collateralBitmap ≤ UINT128_MAX

def preMarket
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : MarketState :=
  { totalUnits := step.badDebtBranch.totalUnitsLocal,
    lossFactor := step.badDebtBranch.lossFactorLocal,
    withdrawable := step.rcfBranch.withdrawableLocal,
    continuousFeeCredit := step.badDebtBranch.continuousFeeCreditLocal,
    totalUnitsUint128 := step.badDebtBranch.totalUnitsLocalUint128,
    lossFactorUint128 := step.badDebtBranch.lossFactorLocalUint128,
    withdrawableUint128 := step.rcfBranch.withdrawableLocalUint128,
    continuousFeeCreditUint128 :=
      step.badDebtBranch.continuousFeeCreditLocalUint128 }

def postMarket
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : MarketState :=
  { totalUnits := step.badDebtBranch.newTotalUnitsLocal,
    lossFactor := step.badDebtBranch.newLossFactorLocal,
    withdrawable := step.rcfBranch.postWithdrawableLocal,
    continuousFeeCredit := step.badDebtBranch.newContinuousFeeCreditLocal,
    totalUnitsUint128 :=
      Liquidate.normalModeAfterBadDebtStep_newTotalUnitsUint128 p s lenders step,
    lossFactorUint128 :=
      Liquidate.normalModeAfterBadDebtStep_newLossFactorUint128 p s lenders step,
    withdrawableUint128 := step.rcfBranch.postWithdrawableLocalUint128,
    continuousFeeCreditUint128 :=
      Liquidate.normalModeAfterBadDebtStep_newContinuousFeeCreditUint128
        p s lenders step }

def prePosition
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : PositionState :=
  { debt := step.badDebtBranch.loopStep.originalDebt,
    selectedCollateral := step.rcfBranch.loopStep.collateral,
    collateralBitmap := step.rcfBranch.collateralBitmapLocal,
    debtUint128 := step.badDebtBranch.originalDebtUint128,
    selectedCollateralUint128 := step.rcfBranch.collateralLocalUint128,
    collateralBitmapUint128 := step.rcfBranch.collateralBitmapLocalUint128 }

def postBadDebtPosition
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : PositionState :=
  { debt := step.badDebtBranch.postDebtLocal,
    selectedCollateral := step.rcfBranch.loopStep.collateral,
    collateralBitmap := step.rcfBranch.collateralBitmapLocal,
    debtUint128 :=
      Liquidate.normalModeAfterBadDebtStep_badDebtPostDebtUint128 p s lenders step,
    selectedCollateralUint128 := step.rcfBranch.collateralLocalUint128,
    collateralBitmapUint128 := step.rcfBranch.collateralBitmapLocalUint128 }

def postPosition
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : PositionState :=
  { debt := step.rcfBranch.postDebtLocal,
    selectedCollateral := step.rcfBranch.postCollateralLocal,
    collateralBitmap := step.rcfBranch.postCollateralBitmapLocal,
    debtUint128 :=
      Liquidate.normalModeAfterBadDebtStep_rcfPostDebtUint128 p s lenders step,
    selectedCollateralUint128 :=
      Liquidate.normalModeAfterBadDebtStep_postCollateralUint128 p s lenders step,
    collateralBitmapUint128 :=
      Liquidate.normalModeAfterBadDebtStep_postCollateralBitmapUint128
        p s lenders step }

def otherMaxDebtContributions
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : List Nat :=
  maxDebtContributionsOf
      (slotsOfIndices step.rcfBranch.loopStep.loop.activeCollateralArrays
        step.rcfBranch.loopStep.beforeSelectedIndices) ++
    maxDebtContributionsOf
      (slotsOfIndices step.rcfBranch.loopStep.loop.activeCollateralArrays
        step.rcfBranch.loopStep.afterSelectedIndices)

def postSelectedMaxDebtContribution
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : Nat :=
  collateralMaxDebt (postPosition step).selectedCollateral
    step.rcfBranch.loopStep.price p.lltv

def postLoopMaxDebtProjection
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : Nat :=
  maxDebtAfterCollateralLoop (postSelectedMaxDebtContribution step)
    (otherMaxDebtContributions step)

theorem preLoopMaxDebtEqSelectedPlusOther
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    s.maxDebt =
      maxDebtAfterCollateralLoop
        (collateralMaxDebt step.rcfBranch.loopStep.collateral
          step.rcfBranch.loopStep.price p.lltv)
        (otherMaxDebtContributions step) := by
  have hloop :
      step.rcfBranch.loopStep.loop.maxDebt =
        maxDebtAfterCollateralLoop
          (collateralMaxDebt step.rcfBranch.loopStep.collateral
            step.rcfBranch.loopStep.price p.lltv)
          (otherMaxDebtContributions step) := by
    calc
      step.rcfBranch.loopStep.loop.maxDebt =
          sumMaxDebtContributions
            (slotsOfIndices step.rcfBranch.loopStep.loop.activeCollateralArrays
              step.rcfBranch.loopStep.loop.activeVisited) :=
          Refinement.liquidateLoopTrace_maxDebtEqSum step.rcfBranch.loopStep.loop
      _ =
          sumMaxDebtContributions
            (slotsOfIndices step.rcfBranch.loopStep.loop.activeCollateralArrays
              (step.rcfBranch.loopStep.beforeSelectedIndices ++
                step.rcfBranch.loopStep.selectedIndex ::
                  step.rcfBranch.loopStep.afterSelectedIndices)) := by
            rw [step.rcfBranch.loopStep.activeTraceEqSplit]
      _ =
          maxDebtAfterCollateralLoop
            (maxDebtContribution
              (slotAt step.rcfBranch.loopStep.loop.activeCollateralArrays
                step.rcfBranch.loopStep.selectedIndex))
            (otherMaxDebtContributions step) := by
            exact sumMaxDebtContributions_split_selectedIndex
              step.rcfBranch.loopStep.loop.activeCollateralArrays
              step.rcfBranch.loopStep.beforeSelectedIndices
              step.rcfBranch.loopStep.afterSelectedIndices
              step.rcfBranch.loopStep.selectedIndex
      _ =
          maxDebtAfterCollateralLoop
            (collateralMaxDebt step.rcfBranch.loopStep.collateral
              step.rcfBranch.loopStep.price p.lltv)
            (otherMaxDebtContributions step) := by
            unfold maxDebtContribution slotAt otherMaxDebtContributions
            rw [← step.rcfBranch.loopStep.collateralEqSelected,
              ← step.rcfBranch.loopStep.lltvEqSelected,
              ← Refinement.RCF.repaidMaxAfterBadDebtLoopStep_priceEqSelected
                p s step.rcfBranch.loopStep]
  calc
    s.maxDebt = step.rcfBranch.loopStep.loop.maxDebt :=
      step.rcfBranch.loopStep.loopMaxDebtEq.symm
    _ =
        maxDebtAfterCollateralLoop
          (collateralMaxDebt step.rcfBranch.loopStep.collateral
            step.rcfBranch.loopStep.price p.lltv)
          (otherMaxDebtContributions step) := hloop

theorem postLoopMaxDebtProjection_eq_projected
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    postLoopMaxDebtProjection step =
      (Refinement.RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch).maxDebt := by
  set oldSelected :=
    collateralMaxDebt step.rcfBranch.loopStep.collateral
      step.rcfBranch.loopStep.price p.lltv
  set newSelected :=
    collateralMaxDebt (postPosition step).selectedCollateral
      step.rcfBranch.loopStep.price p.lltv
  have hpostCollateral :
      (postPosition step).selectedCollateral =
        step.rcfBranch.loopStep.collateral - step.rcfBranch.seizedAssets := by
    change step.rcfBranch.postCollateralLocal =
      step.rcfBranch.loopStep.collateral - step.rcfBranch.seizedAssets
    exact step.rcfBranch.postCollateralLocalEq
  have hnewLe : newSelected ≤ oldSelected := by
    subst oldSelected
    subst newSelected
    rw [hpostCollateral]
    exact collateralMaxDebt_mono_collateral (Nat.sub_le _ _)
  have hpre := preLoopMaxDebtEqSelectedPlusOther step
  unfold Refinement.RCF.projectedAfterBadDebtLocalPostState liquidateWithDecrease
  unfold postLoopMaxDebtProjection postSelectedMaxDebtContribution
  simp
  rw [hpre]
  subst oldSelected
  subst newSelected
  rw [hpostCollateral]
  exact (maxDebtAfterCollateralLoop_replace_selected_sub
    (collateralMaxDebt step.rcfBranch.loopStep.collateral
      step.rcfBranch.loopStep.price p.lltv)
    (collateralMaxDebt
      (step.rcfBranch.loopStep.collateral - step.rcfBranch.seizedAssets)
      step.rcfBranch.loopStep.price p.lltv)
    (otherMaxDebtContributions step)
    (collateralMaxDebt_mono_collateral (Nat.sub_le _ _))).symm

theorem postBadDebtPosition_debtEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postBadDebtPosition step).debt =
      (prePosition step).debt - step.badDebtBranch.badDebt := by
  unfold postBadDebtPosition prePosition
  exact step.badDebtBranch.postDebtLocalEq

theorem postPosition_debtEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postPosition step).debt =
      (postBadDebtPosition step).debt - step.rcfBranch.repaidUnits := by
  change step.rcfBranch.postDebtLocal =
    step.badDebtBranch.postDebtLocal - step.rcfBranch.repaidUnits
  rw [step.rcfBranch.postDebtLocalEq, step.currentDebtEqPostBadDebt]

theorem postPosition_debtEqProjected
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postPosition step).debt =
      (Refinement.RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch).debt := by
  change step.rcfBranch.postDebtLocal =
    (Refinement.RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch).debt
  rw [step.rcfBranch.postDebtLocalEq]
  exact (Liquidate.normalModeAfterBadDebtStep_projectedDebtEqFinalDebt
    p s lenders step).symm

theorem postPosition_healthyWithinTwo
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    healthyWithin 2
      { debt := (postPosition step).debt,
        maxDebt := (Refinement.RCF.projectedAfterBadDebtLocalPostState
          p s step.rcfBranch).maxDebt } := by
  have h := Liquidate.normalModeAfterBadDebtStep_restoresLocalPostWithinTwo
    p s lenders step
  unfold healthyWithin at *
  rw [postPosition_debtEqProjected step]
  exact h

theorem postMarket_totalUnitsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postMarket step).totalUnits =
      (preMarket step).totalUnits - step.badDebtBranch.badDebt := by
  change step.badDebtBranch.newTotalUnitsLocal =
    step.badDebtBranch.totalUnitsLocal - step.badDebtBranch.badDebt
  rw [step.badDebtBranch.newTotalUnitsLocalEq,
    step.badDebtBranch.totalUnitsLocalEq]

theorem postMarket_lossFactorEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postMarket step).lossFactor =
      lossFactorAfterBadDebt (preMarket step).totalUnits
        step.badDebtBranch.badDebt (preMarket step).lossFactor := by
  change step.badDebtBranch.newLossFactorLocal =
    lossFactorAfterBadDebt step.badDebtBranch.totalUnitsLocal
      step.badDebtBranch.badDebt step.badDebtBranch.lossFactorLocal
  rw [step.badDebtBranch.newLossFactorLocalEq,
    step.badDebtBranch.totalUnitsLocalEq,
    step.badDebtBranch.lossFactorLocalEq]

theorem postMarket_withdrawableEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postMarket step).withdrawable =
      (preMarket step).withdrawable + step.rcfBranch.repaidUnits := by
  unfold postMarket preMarket
  exact step.rcfBranch.postWithdrawableLocalEq

theorem postMarket_continuousFeeCreditEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    (postMarket step).continuousFeeCredit =
      continuousFeeCreditAfterBadDebt (preMarket step).continuousFeeCredit
        (preMarket step).lossFactor (postMarket step).lossFactor := by
  change step.badDebtBranch.newContinuousFeeCreditLocal =
    continuousFeeCreditAfterBadDebt step.badDebtBranch.continuousFeeCreditLocal
      step.badDebtBranch.lossFactorLocal step.badDebtBranch.newLossFactorLocal
  exact step.badDebtBranch.newContinuousFeeCreditLocalEq

theorem postBadDebtPosition_debtNoopOfBadDebtZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    (postBadDebtPosition step).debt = (prePosition step).debt := by
  change step.badDebtBranch.postDebtLocal =
    step.badDebtBranch.loopStep.originalDebt
  exact Liquidate.normalModeAfterBadDebtStep_badDebtZeroPostDebtEqOriginal
    p s lenders step hbad

theorem postMarket_totalUnitsNoopOfBadDebtZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    (postMarket step).totalUnits = (preMarket step).totalUnits := by
  change step.badDebtBranch.newTotalUnitsLocal =
    step.badDebtBranch.totalUnitsLocal
  rw [step.badDebtBranch.totalUnitsLocalEq]
  exact Liquidate.normalModeAfterBadDebtStep_badDebtZeroTotalUnitsUnchanged
    p s lenders step hbad

theorem postMarket_lossFactorNoopOfBadDebtZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    (postMarket step).lossFactor = (preMarket step).lossFactor := by
  change step.badDebtBranch.newLossFactorLocal =
    step.badDebtBranch.lossFactorLocal
  rw [step.badDebtBranch.lossFactorLocalEq]
  exact Liquidate.normalModeAfterBadDebtStep_badDebtZeroLossFactorUnchanged
    p s lenders step hbad

theorem postMarket_continuousFeeCreditNoopOfBadDebtZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    (postMarket step).continuousFeeCredit =
      (preMarket step).continuousFeeCredit := by
  change step.badDebtBranch.newContinuousFeeCreditLocal =
    step.badDebtBranch.continuousFeeCreditLocal
  exact
    Liquidate.normalModeAfterBadDebtStep_badDebtZeroContinuousFeeCreditUnchanged
      p s lenders step hbad

theorem postMarket_coversCredits
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    totalUnitsCoversCredits
      { totalUnits := (postMarket step).totalUnits,
        lossFactor := (postMarket step).lossFactor } lenders := by
  unfold postMarket
  exact Liquidate.normalModeAfterBadDebtStep_coversCredits p s lenders step

theorem postMarket_coversPostUpdateCredits
    {p : RCFParams} {s : HealthState} {positions : List LenderPosition}
    (accrualEnd maturity : Nat)
    (step :
      Liquidate.NormalModeAfterBadDebtStep p s (lendersOfPositions positions)) :
    sumPostUpdateCreditView
        { totalUnits := (postMarket step).totalUnits,
          lossFactor := (postMarket step).lossFactor }
        accrualEnd maturity positions ≤
      (postMarket step).totalUnits := by
  unfold postMarket
  exact Liquidate.normalModeAfterBadDebtStep_coversPostUpdateCredits
    p s positions accrualEnd maturity step

theorem postMarket_coversUpdatePositionViewResultCredits
    {p : RCFParams} {s : HealthState} {positions : List LenderPosition}
    (timestamp maturity : Nat)
    (step :
      Liquidate.NormalModeAfterBadDebtStep p s (lendersOfPositions positions)) :
    sumUpdatePositionViewResultCredit
        { totalUnits := (postMarket step).totalUnits,
          lossFactor := (postMarket step).lossFactor }
        timestamp maturity positions ≤
      (postMarket step).totalUnits := by
  unfold postMarket
  exact Liquidate.normalModeAfterBadDebtStep_coversUpdatePositionViewResultCredits
    p s positions timestamp maturity step

theorem postMarket_coversStoredCreditsAfterUpdates
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    sumCredit
        (lenders.map
          (updateLender
            { totalUnits := (postMarket step).totalUnits,
              lossFactor := (postMarket step).lossFactor })) ≤
      (postMarket step).totalUnits := by
  unfold postMarket
  exact Liquidate.normalModeAfterBadDebtStep_coversStoredCreditsAfterUpdates
    p s lenders step

/-- The storage keys touched by `liquidate` after `bytes32 id =
    touchMarket(market)`: `marketState[id]` and `position[id][borrower]`. -/
structure StorageAccessFrame where
  marketId : MarketId
  borrower : Address
  beforeMarket : MarketState
  afterMarket : MarketState
  beforePosition : PositionState
  afterBadDebtPosition : PositionState
  afterPosition : PositionState

def storageAccessFrame
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (marketId : MarketId) (borrower : Address) : StorageAccessFrame :=
  { marketId := marketId,
    borrower := borrower,
    beforeMarket := preMarket step,
    afterMarket := postMarket step,
    beforePosition := prePosition step,
    afterBadDebtPosition := postBadDebtPosition step,
    afterPosition := postPosition step }

theorem storageAccessFrame_keysEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (marketId : MarketId) (borrower : Address) :
    (storageAccessFrame step marketId borrower).marketId = marketId ∧
      (storageAccessFrame step marketId borrower).borrower = borrower :=
  ⟨rfl, rfl⟩

theorem storageAccessFrame_marketEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (marketId : MarketId) (borrower : Address) :
    (storageAccessFrame step marketId borrower).beforeMarket = preMarket step ∧
      (storageAccessFrame step marketId borrower).afterMarket =
        postMarket step :=
  ⟨rfl, rfl⟩

theorem storageAccessFrame_positionEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (marketId : MarketId) (borrower : Address) :
    (storageAccessFrame step marketId borrower).beforePosition =
        prePosition step ∧
      (storageAccessFrame step marketId borrower).afterBadDebtPosition =
        postBadDebtPosition step ∧
      (storageAccessFrame step marketId borrower).afterPosition =
        postPosition step :=
  ⟨rfl, rfl, rfl⟩

/-- Normal-mode top-level `liquidate` call obligations around the proved local
    branch sequence.  The remaining external facts are the liquidator gate and
    liquidation-lock checks; arithmetic and storage-local guard facts are derived
    from the combined step. -/
structure NormalModeCall (p : RCFParams) (s : HealthState) (lenders : List Lender) where
  step : Liquidate.NormalModeAfterBadDebtStep p s lenders
  postMaturityMode : Bool
  postMaturityModeEqFalse : postMaturityMode = false
  liquidatorGateAllows : Prop
  liquidatorGateAllowsProof : liquidatorGateAllows
  liquidationUnlocked : Prop
  liquidationUnlockedProof : liquidationUnlocked

theorem normalModeCall_atMostOneNonZeroInput
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    RCF.atMostOneNonZeroInput call.step.rcfBranch.inputRepaidUnitsLocal
      call.step.rcfBranch.inputSeizedAssetsLocal :=
  Liquidate.normalModeAfterBadDebtStep_inputAtMostOneNonZero p s lenders
    call.step

theorem normalModeCall_borrowerDebtPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    (prePosition call.step).debt > 0 := by
  change call.step.badDebtBranch.loopStep.originalDebt > 0
  exact Liquidate.normalModeAfterBadDebtStep_originalDebtPositive p s lenders
    call.step

theorem normalModeCall_liquidatorGateAllows
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    call.liquidatorGateAllows :=
  call.liquidatorGateAllowsProof

theorem normalModeCall_normalMode
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    call.postMaturityMode = false :=
  call.postMaturityModeEqFalse

theorem normalModeCall_lifEqSelectedMaxLif
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    p.lif =
      call.step.rcfBranch.loopStep.loop.activeCollateralArrays.maxLif
        call.step.rcfBranch.loopStep.selectedIndex :=
  Liquidate.normalModeAfterBadDebtStep_lifEqSelectedMaxLif p s lenders
    call.step

theorem normalModeCall_liquidationUnlocked
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    call.liquidationUnlocked :=
  call.liquidationUnlockedProof

theorem normalModeCall_originalDebtExceedsLoopMaxDebt
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    (prePosition call.step).debt >
      call.step.badDebtBranch.loopStep.loop.maxDebt := by
  change call.step.badDebtBranch.loopStep.originalDebt >
    call.step.badDebtBranch.loopStep.loop.maxDebt
  exact Liquidate.normalModeAfterBadDebtStep_originalDebtExceedsLoopMaxDebt
    p s lenders call.step

theorem normalModeCall_notLiquidatableGuard
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    call.liquidationUnlocked ∧
      (prePosition call.step).debt >
        call.step.badDebtBranch.loopStep.loop.maxDebt :=
  ⟨call.liquidationUnlockedProof,
    normalModeCall_originalDebtExceedsLoopMaxDebt call⟩

theorem normalModeCall_repayBranchActive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    call.step.rcfBranch.repaidUnits > 0 ∨
      call.step.rcfBranch.seizedAssets > 0 :=
  Liquidate.normalModeAfterBadDebtStep_repayBranchActive p s lenders call.step

/-- The Solidity entry locals before the liquidation loop: call arguments,
    `touchMarket` id, keyed storage reads, and the initial input consistency
    check. -/
structure LiquidateEntryFrame
    (p : RCFParams) (s : HealthState) (lenders : List Lender) where
  call : NormalModeCall p s lenders
  msgSender : Address
  marketId : MarketId
  collateralIndex : Nat
  inputSeizedAssets : Nat
  inputRepaidUnits : Nat
  borrower : Address
  receiver : Address
  callback : Address
  storageFrame : StorageAccessFrame
  storageFrameEq :
    storageFrame = storageAccessFrame call.step marketId borrower
  collateralIndexEq :
    collateralIndex = call.step.rcfBranch.loopStep.selectedIndex
  inputSeizedAssetsEq :
    inputSeizedAssets = call.step.rcfBranch.inputSeizedAssetsLocal
  inputRepaidUnitsEq :
    inputRepaidUnits = call.step.rcfBranch.inputRepaidUnitsLocal

def liquidateEntryFrame
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId) (borrower receiver callback : Address) :
    LiquidateEntryFrame p s lenders :=
  { call := call,
    msgSender := msgSender,
    marketId := marketId,
    collateralIndex := call.step.rcfBranch.loopStep.selectedIndex,
    inputSeizedAssets := call.step.rcfBranch.inputSeizedAssetsLocal,
    inputRepaidUnits := call.step.rcfBranch.inputRepaidUnitsLocal,
    borrower := borrower,
    receiver := receiver,
    callback := callback,
    storageFrame := storageAccessFrame call.step marketId borrower,
    storageFrameEq := rfl,
    collateralIndexEq := rfl,
    inputSeizedAssetsEq := rfl,
    inputRepaidUnitsEq := rfl }

theorem liquidateEntryFrame_storageKeysEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (entry : LiquidateEntryFrame p s lenders) :
    entry.storageFrame.marketId = entry.marketId ∧
      entry.storageFrame.borrower = entry.borrower := by
  rw [entry.storageFrameEq]
  exact storageAccessFrame_keysEq entry.call.step entry.marketId entry.borrower

theorem liquidateEntryFrame_storageReadsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (entry : LiquidateEntryFrame p s lenders) :
    entry.storageFrame.beforeMarket = preMarket entry.call.step ∧
      entry.storageFrame.beforePosition = prePosition entry.call.step := by
  rw [entry.storageFrameEq]
  exact ⟨rfl, rfl⟩

theorem liquidateEntryFrame_inputAtMostOneNonZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (entry : LiquidateEntryFrame p s lenders) :
    RCF.atMostOneNonZeroInput entry.inputRepaidUnits
      entry.inputSeizedAssets := by
  rw [entry.inputRepaidUnitsEq, entry.inputSeizedAssetsEq]
  exact normalModeCall_atMostOneNonZeroInput entry.call

theorem liquidateEntryFrame_borrowerDebtPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (entry : LiquidateEntryFrame p s lenders) :
    entry.storageFrame.beforePosition.debt > 0 := by
  rw [entry.storageFrameEq]
  change (prePosition entry.call.step).debt > 0
  exact normalModeCall_borrowerDebtPositive entry.call

/-- Observable values at the end of `liquidate`: the return pair, payer, and
    accounting fields carried by `EventsLib.Liquidate`. -/
structure LiquidateOutcome where
  seizedAssets : Nat
  repaidUnits : Nat
  badDebt : Nat
  lossFactor : Nat
  continuousFeeCredit : Nat
  payer : Address

def outcome
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) : LiquidateOutcome :=
  { seizedAssets := step.rcfBranch.seizedAssets,
    repaidUnits := step.rcfBranch.repaidUnits,
    badDebt := step.badDebtBranch.badDebt,
    lossFactor := (postMarket step).lossFactor,
    continuousFeeCredit := (postMarket step).continuousFeeCredit,
    payer := if callback ≠ zeroAddress then callback else msgSender }

def returnedPair
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) : Nat × Nat :=
  (step.rcfBranch.seizedAssets, step.rcfBranch.repaidUnits)

theorem outcome_seizedAssetsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).seizedAssets =
      step.rcfBranch.seizedAssets := rfl

theorem outcome_repaidUnitsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).repaidUnits =
      step.rcfBranch.repaidUnits := rfl

theorem outcome_badDebtEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).badDebt =
      step.badDebtBranch.badDebt := rfl

theorem outcome_lossFactorEqPostMarket
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).lossFactor =
      (postMarket step).lossFactor := rfl

theorem outcome_continuousFeeCreditEqPostMarket
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).continuousFeeCredit =
      (postMarket step).continuousFeeCredit := rfl

theorem outcome_payerEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) :
    (outcome step msgSender callback).payer =
      if callback ≠ zeroAddress then callback else msgSender := rfl

theorem outcome_payerEqCallback_of_nonzero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) (hcallback : callback ≠ zeroAddress) :
    (outcome step msgSender callback).payer = callback := by
  simp [outcome, hcallback]

theorem outcome_payerEqSender_of_zero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender callback : Address) (hcallback : callback = zeroAddress) :
    (outcome step msgSender callback).payer = msgSender := by
  simp [outcome, hcallback]

theorem returnedPair_eq_solidityReturn
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders) :
    returnedPair step =
      (step.rcfBranch.seizedAssets, step.rcfBranch.repaidUnits) := rfl

theorem normalModeCall_returnedPairEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) :
    returnedPair call.step =
      (call.step.rcfBranch.seizedAssets, call.step.rcfBranch.repaidUnits) :=
  returnedPair_eq_solidityReturn call.step

theorem normalModeCall_eventAccountingEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) (msgSender callback : Address) :
    (outcome call.step msgSender callback).badDebt =
        call.step.badDebtBranch.badDebt ∧
      (outcome call.step msgSender callback).lossFactor =
        (postMarket call.step).lossFactor ∧
      (outcome call.step msgSender callback).continuousFeeCredit =
        (postMarket call.step).continuousFeeCredit :=
  ⟨rfl, rfl, rfl⟩

theorem normalModeCall_payerEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders) (msgSender callback : Address) :
    (outcome call.step msgSender callback).payer =
      if callback ≠ zeroAddress then callback else msgSender := rfl

/-- Arguments emitted by `EventsLib.Liquidate`, projected from the proved
    branch locals and the top-level call envelope. -/
structure LiquidateEvent where
  liquidator : Address
  marketId : MarketId
  collateralToken : Address
  seizedAssets : Nat
  repaidUnits : Nat
  borrower : Address
  postMaturityMode : Bool
  receiver : Address
  payer : Address
  badDebt : Nat
  lossFactor : Nat
  continuousFeeCredit : Nat

def liquidateEvent
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    LiquidateEvent :=
  { liquidator := msgSender,
    marketId := marketId,
    collateralToken := collateralToken,
    seizedAssets := call.step.rcfBranch.seizedAssets,
    repaidUnits := call.step.rcfBranch.repaidUnits,
    borrower := borrower,
    postMaturityMode := call.postMaturityMode,
    receiver := receiver,
    payer := (outcome call.step msgSender callback).payer,
    badDebt := call.step.badDebtBranch.badDebt,
    lossFactor := (postMarket call.step).lossFactor,
    continuousFeeCredit := (postMarket call.step).continuousFeeCredit }

theorem liquidateEvent_partiesEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).liquidator = msgSender ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).marketId = marketId ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).collateralToken = collateralToken ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).borrower = borrower ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).receiver = receiver :=
  ⟨rfl, rfl, rfl, rfl, rfl⟩

theorem liquidateEvent_modeEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).postMaturityMode = false := by
  change call.postMaturityMode = false
  exact call.postMaturityModeEqFalse

theorem liquidateEvent_amountsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).seizedAssets = call.step.rcfBranch.seizedAssets ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).repaidUnits = call.step.rcfBranch.repaidUnits :=
  ⟨rfl, rfl⟩

theorem liquidateEvent_accountingEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).badDebt = call.step.badDebtBranch.badDebt ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).lossFactor = (postMarket call.step).lossFactor ∧
      (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).continuousFeeCredit =
        (postMarket call.step).continuousFeeCredit :=
  ⟨rfl, rfl, rfl⟩

theorem liquidateEvent_payerEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId)
    (collateralToken borrower receiver callback : Address) :
    (liquidateEvent call msgSender marketId collateralToken borrower receiver
        callback).payer =
      if callback ≠ zeroAddress then callback else msgSender := rfl

/-- The local values passed to `ILiquidateCallback.onLiquidate`.  The full
    market calldata and arbitrary callback data are uninterpreted call inputs, so this
    projection records the market id plus the arithmetic and address locals
    proved here. -/
structure CallbackPayload where
  liquidator : Address
  marketId : MarketId
  collateralIndex : Nat
  seizedAssets : Nat
  repaidUnits : Nat
  borrower : Address
  receiver : Address
  badDebt : Nat

def callbackPayload
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId) (collateralIndex : Nat)
    (borrower receiver : Address) : CallbackPayload :=
  { liquidator := msgSender,
    marketId := marketId,
    collateralIndex := collateralIndex,
    seizedAssets := call.step.rcfBranch.seizedAssets,
    repaidUnits := call.step.rcfBranch.repaidUnits,
    borrower := borrower,
    receiver := receiver,
    badDebt := call.step.badDebtBranch.badDebt }

theorem callbackPayload_partiesEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId) (collateralIndex : Nat)
    (borrower receiver : Address) :
    (callbackPayload call msgSender marketId collateralIndex borrower receiver).liquidator =
        msgSender ∧
      (callbackPayload call msgSender marketId collateralIndex borrower receiver).marketId =
        marketId ∧
      (callbackPayload call msgSender marketId collateralIndex borrower receiver).borrower =
        borrower ∧
      (callbackPayload call msgSender marketId collateralIndex borrower receiver).receiver =
        receiver :=
  ⟨rfl, rfl, rfl, rfl⟩

theorem callbackPayload_indexEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId) (collateralIndex : Nat)
    (borrower receiver : Address) :
    (callbackPayload call msgSender marketId collateralIndex borrower receiver).collateralIndex =
      collateralIndex := rfl

theorem callbackPayload_amountsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender : Address) (marketId : MarketId) (collateralIndex : Nat)
    (borrower receiver : Address) :
    (callbackPayload call msgSender marketId collateralIndex borrower receiver).seizedAssets =
        call.step.rcfBranch.seizedAssets ∧
      (callbackPayload call msgSender marketId collateralIndex borrower receiver).repaidUnits =
        call.step.rcfBranch.repaidUnits ∧
      (callbackPayload call msgSender marketId collateralIndex borrower receiver).badDebt =
        call.step.badDebtBranch.badDebt :=
  ⟨rfl, rfl, rfl⟩

/-- The external-call schedule at the end of `liquidate`.  Token effects and
    callback execution remain environment behavior; this projection records the
    values passed to those calls by the proved local branch. -/
structure ExternalCallPlan where
  collateralToken : Address
  collateralReceiver : Address
  collateralAmount : Nat
  loanToken : Address
  loanPayer : Address
  loanReceiver : Address
  loanAmount : Nat
  callbackAddress : Address
  callbackRequired : Prop
  callbackReturnValue : Bytes32

def externalCallPlan
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    ExternalCallPlan :=
  { collateralToken := collateralToken,
    collateralReceiver := receiver,
    collateralAmount := step.rcfBranch.seizedAssets,
    loanToken := loanToken,
    loanPayer := (outcome step msgSender callback).payer,
    loanReceiver := self,
    loanAmount := step.rcfBranch.repaidUnits,
    callbackAddress := callback,
    callbackRequired := callback ≠ zeroAddress,
    callbackReturnValue := callbackReturnValue }

def callbackReturnAccepted (plan : ExternalCallPlan) : Prop :=
  plan.callbackRequired → plan.callbackReturnValue = CALLBACK_SUCCESS

theorem externalCallPlan_collateralTransferEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).collateralToken =
        collateralToken ∧
      (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).collateralReceiver =
        receiver ∧
      (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).collateralAmount =
        step.rcfBranch.seizedAssets :=
  ⟨rfl, rfl, rfl⟩

theorem externalCallPlan_loanTransferEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).loanToken = loanToken ∧
      (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).loanPayer =
        (outcome step msgSender callback).payer ∧
      (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).loanReceiver = self ∧
      (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).loanAmount =
        step.rcfBranch.repaidUnits :=
  ⟨rfl, rfl, rfl, rfl⟩

theorem externalCallPlan_callbackRequiredEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).callbackRequired =
      (callback ≠ zeroAddress) := rfl

theorem externalCallPlan_callbackReturnEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (step : Liquidate.NormalModeAfterBadDebtStep p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    (externalCallPlan step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).callbackReturnValue = callbackReturnValue := rfl

theorem callbackReturnAccepted_of_noCallback
    (plan : ExternalCallPlan) (hcallback : ¬ plan.callbackRequired) :
    callbackReturnAccepted plan := by
  intro hrequired
  exact False.elim (hcallback hrequired)

theorem callbackReturnAccepted_of_success
    (plan : ExternalCallPlan)
    (hsuccess : plan.callbackReturnValue = CALLBACK_SUCCESS) :
    callbackReturnAccepted plan := by
  intro _hrequired
  exact hsuccess

theorem normalModeCall_externalTransfersEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (call : NormalModeCall p s lenders)
    (msgSender collateralToken receiver callback loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).collateralToken = collateralToken ∧
      (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).collateralAmount =
        call.step.rcfBranch.seizedAssets ∧
      (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).loanToken = loanToken ∧
      (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).loanPayer =
        (outcome call.step msgSender callback).payer ∧
      (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self callbackReturnValue).loanReceiver = self ∧
      (externalCallPlan call.step msgSender collateralToken receiver callback
        loanToken self
        callbackReturnValue).loanAmount =
        call.step.rcfBranch.repaidUnits :=
  ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

/-- A single normal-mode projection of the Solidity `liquidate` tail, tying the
    entry locals to the returned pair, emitted event, optional callback payload,
    and transfer/callback call plan. -/
structure NormalModeLiquidateProjection
    (p : RCFParams) (s : HealthState) (lenders : List Lender) where
  entry : LiquidateEntryFrame p s lenders
  collateralToken : Address
  loanToken : Address
  self : Address
  callbackReturnValue : Bytes32
  result : LiquidateOutcome
  returnPair : Nat × Nat
  event : LiquidateEvent
  callbackPayload : CallbackPayload
  externalCalls : ExternalCallPlan
  resultEq :
    result = outcome entry.call.step entry.msgSender entry.callback
  returnPairEq :
    returnPair = returnedPair entry.call.step
  eventEq :
    event =
      liquidateEvent entry.call entry.msgSender entry.marketId collateralToken
        entry.borrower entry.receiver entry.callback
  callbackPayloadEq :
    callbackPayload =
      LiquidateStorage.callbackPayload entry.call entry.msgSender entry.marketId
        entry.collateralIndex entry.borrower entry.receiver
  externalCallsEq :
    externalCalls =
      externalCallPlan entry.call.step entry.msgSender collateralToken
        entry.receiver entry.callback loanToken self callbackReturnValue

def normalModeLiquidateProjection
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (entry : LiquidateEntryFrame p s lenders)
    (collateralToken loanToken self : Address)
    (callbackReturnValue : Bytes32) :
    NormalModeLiquidateProjection p s lenders :=
  { entry := entry,
    collateralToken := collateralToken,
    loanToken := loanToken,
    self := self,
    callbackReturnValue := callbackReturnValue,
    result := outcome entry.call.step entry.msgSender entry.callback,
    returnPair := returnedPair entry.call.step,
    event :=
      liquidateEvent entry.call entry.msgSender entry.marketId collateralToken
        entry.borrower entry.receiver entry.callback,
    callbackPayload :=
      callbackPayload entry.call entry.msgSender entry.marketId
        entry.collateralIndex entry.borrower entry.receiver,
    externalCalls :=
      externalCallPlan entry.call.step entry.msgSender collateralToken
        entry.receiver entry.callback loanToken self callbackReturnValue,
    resultEq := rfl,
    returnPairEq := rfl,
    eventEq := rfl,
    callbackPayloadEq := rfl,
    externalCallsEq := rfl }

theorem normalModeLiquidateProjection_returnEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.returnPair =
      (proj.entry.call.step.rcfBranch.seizedAssets,
        proj.entry.call.step.rcfBranch.repaidUnits) := by
  rw [proj.returnPairEq]
  exact returnedPair_eq_solidityReturn proj.entry.call.step

theorem normalModeLiquidateProjection_eventEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.event.seizedAssets = proj.entry.call.step.rcfBranch.seizedAssets ∧
      proj.event.repaidUnits = proj.entry.call.step.rcfBranch.repaidUnits ∧
      proj.event.badDebt = proj.entry.call.step.badDebtBranch.badDebt ∧
      proj.event.payer =
        if proj.entry.callback ≠ zeroAddress then
          proj.entry.callback
        else
          proj.entry.msgSender := by
  rw [proj.eventEq]
  exact ⟨rfl, rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_callbackPayloadEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.callbackPayload.collateralIndex = proj.entry.collateralIndex ∧
      proj.callbackPayload.seizedAssets =
        proj.entry.call.step.rcfBranch.seizedAssets ∧
      proj.callbackPayload.repaidUnits =
        proj.entry.call.step.rcfBranch.repaidUnits ∧
      proj.callbackPayload.badDebt =
        proj.entry.call.step.badDebtBranch.badDebt := by
  rw [proj.callbackPayloadEq]
  exact ⟨rfl, rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_externalCallsEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.externalCalls.collateralToken = proj.collateralToken ∧
      proj.externalCalls.collateralReceiver = proj.entry.receiver ∧
      proj.externalCalls.collateralAmount =
        proj.entry.call.step.rcfBranch.seizedAssets ∧
      proj.externalCalls.loanToken = proj.loanToken ∧
      proj.externalCalls.loanPayer = proj.result.payer ∧
      proj.externalCalls.loanReceiver = proj.self ∧
      proj.externalCalls.loanAmount =
        proj.entry.call.step.rcfBranch.repaidUnits := by
  rw [proj.externalCallsEq, proj.resultEq]
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_returnMatchesResult
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.returnPair =
      (proj.result.seizedAssets, proj.result.repaidUnits) := by
  rw [proj.returnPairEq, proj.resultEq]
  rfl

theorem normalModeLiquidateProjection_eventMatchesResult
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.event.seizedAssets = proj.result.seizedAssets ∧
      proj.event.repaidUnits = proj.result.repaidUnits ∧
      proj.event.badDebt = proj.result.badDebt ∧
      proj.event.lossFactor = proj.result.lossFactor ∧
      proj.event.continuousFeeCredit = proj.result.continuousFeeCredit ∧
      proj.event.payer = proj.result.payer := by
  rw [proj.eventEq, proj.resultEq]
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_callbackAndTransfersMatchResult
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.callbackPayload.seizedAssets = proj.result.seizedAssets ∧
      proj.callbackPayload.repaidUnits = proj.result.repaidUnits ∧
      proj.callbackPayload.badDebt = proj.result.badDebt ∧
      proj.externalCalls.collateralAmount = proj.result.seizedAssets ∧
      proj.externalCalls.loanAmount = proj.result.repaidUnits ∧
      proj.externalCalls.loanPayer = proj.result.payer := by
  rw [proj.callbackPayloadEq, proj.externalCallsEq, proj.resultEq]
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_resultMatchesStorageAccounting
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.result.badDebt = proj.entry.call.step.badDebtBranch.badDebt ∧
      proj.result.lossFactor = proj.entry.storageFrame.afterMarket.lossFactor ∧
      proj.result.continuousFeeCredit =
        proj.entry.storageFrame.afterMarket.continuousFeeCredit := by
  rw [proj.resultEq, proj.entry.storageFrameEq]
  exact ⟨rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_eventMatchesStorageAccounting
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.event.badDebt = proj.entry.call.step.badDebtBranch.badDebt ∧
      proj.event.lossFactor = proj.entry.storageFrame.afterMarket.lossFactor ∧
      proj.event.continuousFeeCredit =
        proj.entry.storageFrame.afterMarket.continuousFeeCredit := by
  rw [proj.eventEq, proj.entry.storageFrameEq]
  exact ⟨rfl, rfl, rfl⟩

theorem normalModeLiquidateProjection_callbackAccepted_of_noCallback
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hcallback : proj.entry.callback = zeroAddress) :
    callbackReturnAccepted proj.externalCalls := by
  rw [proj.externalCallsEq]
  exact callbackReturnAccepted_of_noCallback
    (externalCallPlan proj.entry.call.step proj.entry.msgSender
      proj.collateralToken proj.entry.receiver proj.entry.callback
      proj.loanToken proj.self proj.callbackReturnValue)
    (by
      intro hrequired
      exact hrequired hcallback)

theorem normalModeLiquidateProjection_callbackAccepted_of_success
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hsuccess : proj.callbackReturnValue = CALLBACK_SUCCESS) :
    callbackReturnAccepted proj.externalCalls := by
  rw [proj.externalCallsEq]
  exact callbackReturnAccepted_of_success
    (externalCallPlan proj.entry.call.step proj.entry.msgSender
      proj.collateralToken proj.entry.receiver proj.entry.callback
      proj.loanToken proj.self proj.callbackReturnValue)
    hsuccess

theorem normalModeLiquidateProjection_storageHealthyWithinTwo
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    healthyWithin 2
      { debt := proj.entry.storageFrame.afterPosition.debt,
        maxDebt := (Refinement.RCF.projectedAfterBadDebtLocalPostState
          p s proj.entry.call.step.rcfBranch).maxDebt } := by
  rw [proj.entry.storageFrameEq]
  change healthyWithin 2
    { debt := (postPosition proj.entry.call.step).debt,
      maxDebt := (Refinement.RCF.projectedAfterBadDebtLocalPostState
        p s proj.entry.call.step.rcfBranch).maxDebt }
  exact postPosition_healthyWithinTwo proj.entry.call.step

theorem normalModeLiquidateProjection_inputAtMostOneNonZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    RCF.atMostOneNonZeroInput proj.entry.inputRepaidUnits
      proj.entry.inputSeizedAssets :=
  liquidateEntryFrame_inputAtMostOneNonZero proj.entry

theorem normalModeLiquidateProjection_inputRepaidUnitsEqMaxRepaid
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.inputRepaidUnits = maxRepaid p s := by
  rw [proj.entry.inputRepaidUnitsEq]
  exact Liquidate.normalModeAfterBadDebtStep_inputRepaidUnitsEqFormula
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_inputSeizedAssetsEqZero
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.inputSeizedAssets = 0 := by
  rw [proj.entry.inputSeizedAssetsEq]
  exact Liquidate.normalModeAfterBadDebtStep_inputSeizedAssetsEqZero
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_normalMode
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.postMaturityMode = false :=
  normalModeCall_normalMode proj.entry.call

theorem normalModeLiquidateProjection_notLiquidatableGuard
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.liquidationUnlocked ∧
      proj.entry.storageFrame.beforePosition.debt >
        proj.entry.call.step.badDebtBranch.loopStep.loop.maxDebt := by
  constructor
  · exact normalModeCall_liquidationUnlocked proj.entry.call
  · rw [proj.entry.storageFrameEq]
    change (prePosition proj.entry.call.step).debt >
      proj.entry.call.step.badDebtBranch.loopStep.loop.maxDebt
    exact normalModeCall_originalDebtExceedsLoopMaxDebt proj.entry.call

theorem normalModeLiquidateProjection_loopEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop =
      proj.entry.call.step.badDebtBranch.loopStep.loop :=
  Liquidate.normalModeAfterBadDebtStep_loopEq p s lenders
    proj.entry.call.step

theorem normalModeLiquidateProjection_lifEqSelectedMaxLif
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    p.lif =
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays.maxLif
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex :=
  normalModeCall_lifEqSelectedMaxLif proj.entry.call

theorem normalModeLiquidateProjection_collateralIndexEqSelected
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.collateralIndex =
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex :=
  proj.entry.collateralIndexEq

theorem normalModeLiquidateProjection_selectedCollateralEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterBadDebtPosition.selectedCollateral =
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays.collateral
        proj.entry.call.step.rcfBranch.loopStep.selectedIndex := by
  rw [proj.entry.storageFrameEq]
  exact proj.entry.call.step.rcfBranch.loopStep.collateralEqSelected

theorem normalModeLiquidateProjection_selectedPriceEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.price =
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays.price
        proj.entry.call.step.rcfBranch.loopStep.selectedIndex :=
  Refinement.RCF.repaidMaxAfterBadDebtLoopStep_priceEqSelected
    p s proj.entry.call.step.rcfBranch.loopStep

theorem normalModeLiquidateProjection_selectedLltvEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    p.lltv =
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays.lltv
        proj.entry.call.step.rcfBranch.loopStep.selectedIndex :=
  proj.entry.call.step.rcfBranch.loopStep.lltvEqSelected

theorem normalModeLiquidateProjection_activeTraceEqSchedule
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited =
      bitmapSchedule
        proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap :=
  msbClearTrace_eq_bitmapSchedule
    proj.entry.call.step.rcfBranch.loopStep.loop.activeTrace

theorem normalModeLiquidateProjection_activeScheduleValid
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    ValidSchedule proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited :=
  Refinement.liquidateLoopTrace_validSchedule
    proj.entry.call.step.rcfBranch.loopStep.loop

theorem normalModeLiquidateProjection_activeSlotsBound
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    (slotsOfIndices
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays
      proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited).length ≤
        MAX_COLLATERALS_PER_BORROWER :=
  Refinement.liquidateLoopTrace_slotsBound
    proj.entry.call.step.rcfBranch.loopStep.loop

theorem normalModeLiquidateProjection_activeBitmapUint128
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap <
      UINT128_BOUND :=
  proj.entry.call.step.rcfBranch.loopStep.loop.activeBitmapValid.1

theorem normalModeLiquidateProjection_activeBitmapCountBound
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    countBits
      proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap ≤
        MAX_COLLATERALS_PER_BORROWER :=
  proj.entry.call.step.rcfBranch.loopStep.loop.activeBitmapValid.2

theorem normalModeLiquidateProjection_activeScheduleLengthEqCountBits
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited.length =
      countBits
        proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap :=
  msbClearTrace_length_eq_countBits
    proj.entry.call.step.rcfBranch.loopStep.loop.activeTrace

theorem normalModeLiquidateProjection_selectedBitSet
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap.testBit
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex = true := by
  have hmem :
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex ∈
        proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited := by
    rw [proj.entry.call.step.rcfBranch.loopStep.activeTraceEqSplit]
    simp
  rw [normalModeLiquidateProjection_activeTraceEqSchedule proj] at hmem
  exact bitmapSchedule_mem_testBit hmem

theorem normalModeLiquidateProjection_selectedHighestAfterClearingBefore
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    HighestBitmapBit
      (clearBitmapBits
        proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralBitmap
        proj.entry.call.step.rcfBranch.loopStep.beforeSelectedIndices)
      MAX_COLLATERALS
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex :=
  msbClearTrace_highestAfterPrefix
    proj.entry.call.step.rcfBranch.loopStep.loop.activeTrace
    proj.entry.call.step.rcfBranch.loopStep.activeTraceEqSplit

theorem normalModeLiquidateProjection_activeIndicesNodup
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited.Nodup :=
  validSchedule_nodup
    (normalModeLiquidateProjection_activeScheduleValid proj)

theorem normalModeLiquidateProjection_selectedIndexLt128
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.loopStep.selectedIndex < MAX_COLLATERALS := by
  have hmem :
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex ∈
        proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited := by
    rw [proj.entry.call.step.rcfBranch.loopStep.activeTraceEqSplit]
    simp
  exact validSchedule_index_lt_128
    (normalModeLiquidateProjection_activeScheduleValid proj) hmem

theorem normalModeLiquidateProjection_activeSlotIndicesNodup
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    (slotIndicesOf
      (slotsOfIndices
        proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays
        proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited)).Nodup :=
  validSchedule_slotIndices_nodup
    proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays
    (normalModeLiquidateProjection_activeScheduleValid proj)

theorem normalModeLiquidateProjection_activeSlotIndexLt128
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    {slot : CollateralSlot}
    (hmem : slot ∈
      slotsOfIndices
        proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays
        proj.entry.call.step.rcfBranch.loopStep.loop.activeVisited) :
    slot.index < MAX_COLLATERALS :=
  validSchedule_slotIndex_lt_128
    proj.entry.call.step.rcfBranch.loopStep.loop.activeCollateralArrays
    (normalModeLiquidateProjection_activeScheduleValid proj) hmem

theorem normalModeLiquidateProjection_badDebtLeOriginalDebt
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.badDebtBranch.badDebt ≤
      proj.entry.storageFrame.beforePosition.debt := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.badDebtBranch.badDebt ≤
    proj.entry.call.step.badDebtBranch.loopStep.originalDebt
  exact Refinement.Accounting.badDebtLoopStep_badDebtLeOriginalDebt
    proj.entry.call.step.badDebtBranch.oldTotalUnits
    proj.entry.call.step.badDebtBranch.badDebt
    proj.entry.call.step.badDebtBranch.oldLossFactor
    lenders proj.entry.call.step.badDebtBranch.loopStep

theorem normalModeLiquidateProjection_originalDebtLeTotalUnits
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.beforePosition.debt ≤
      proj.entry.storageFrame.beforeMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.badDebtBranch.loopStep.originalDebt ≤
    proj.entry.call.step.badDebtBranch.totalUnitsLocal
  rw [proj.entry.call.step.badDebtBranch.totalUnitsLocalEq]
  exact Refinement.Accounting.badDebtLoopStep_originalDebtLeTotalUnits
    proj.entry.call.step.badDebtBranch.oldTotalUnits
    proj.entry.call.step.badDebtBranch.badDebt
    proj.entry.call.step.badDebtBranch.oldLossFactor
    lenders proj.entry.call.step.badDebtBranch.loopStep

theorem normalModeLiquidateProjection_badDebtLeTotalUnits
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.badDebtBranch.badDebt ≤
      proj.entry.storageFrame.beforeMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.badDebtBranch.badDebt ≤
    proj.entry.call.step.badDebtBranch.totalUnitsLocal
  rw [proj.entry.call.step.badDebtBranch.totalUnitsLocalEq]
  exact Refinement.Accounting.badDebtLoopStep_badDebtLeTotalUnits
    proj.entry.call.step.badDebtBranch.oldTotalUnits
    proj.entry.call.step.badDebtBranch.badDebt
    proj.entry.call.step.badDebtBranch.oldLossFactor
    lenders proj.entry.call.step.badDebtBranch.loopStep

theorem normalModeLiquidateProjection_preLendersSynced
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    syncedAt
      { totalUnits := proj.entry.storageFrame.beforeMarket.totalUnits,
        lossFactor := proj.entry.storageFrame.beforeMarket.lossFactor }
      lenders := by
  rw [proj.entry.storageFrameEq]
  change syncedAt
    { totalUnits := (preMarket proj.entry.call.step).totalUnits,
      lossFactor := (preMarket proj.entry.call.step).lossFactor }
    lenders
  exact Refinement.Accounting.badDebtLocalStep_lendersSynced
    lenders proj.entry.call.step.badDebtBranch

theorem normalModeLiquidateProjection_preLendersCreditCovered
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    sumCredit lenders ≤ proj.entry.storageFrame.beforeMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change sumCredit lenders ≤ (preMarket proj.entry.call.step).totalUnits
  exact Refinement.Accounting.badDebtLocalStep_oldCover
    lenders proj.entry.call.step.badDebtBranch

theorem normalModeLiquidateProjection_finalMarketDebtsEqProjected
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    Liquidate.finalMarketDebts p s lenders proj.entry.call.step =
      proj.entry.call.step.badDebtBranch.loopStep.marketDebtsBefore ++
        (Refinement.RCF.projectedAfterBadDebtLocalPostState
          p s proj.entry.call.step.rcfBranch).debt ::
          proj.entry.call.step.badDebtBranch.loopStep.marketDebtsAfter :=
  Liquidate.normalModeAfterBadDebtStep_finalMarketDebtsEqProjected
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_finalDebtsCovered
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    DebtsCovered proj.entry.storageFrame.afterMarket.totalUnits
      (Liquidate.finalMarketDebts p s lenders proj.entry.call.step) := by
  rw [proj.entry.storageFrameEq]
  change DebtsCovered (postMarket proj.entry.call.step).totalUnits
    (Liquidate.finalMarketDebts p s lenders proj.entry.call.step)
  exact Liquidate.normalModeAfterBadDebtStep_finalDebtsCovered
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_currentDebtEqPostBadDebt
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.currentDebtLocal =
      proj.entry.storageFrame.afterBadDebtPosition.debt := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.rcfBranch.currentDebtLocal =
    proj.entry.call.step.badDebtBranch.postDebtLocal
  exact Liquidate.normalModeAfterBadDebtStep_currentDebtEqPostBadDebt
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_currentDebtEqOriginalMinusBadDebt
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.currentDebtLocal =
      proj.entry.storageFrame.beforePosition.debt -
        proj.entry.call.step.badDebtBranch.badDebt := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.rcfBranch.currentDebtLocal =
    proj.entry.call.step.badDebtBranch.loopStep.originalDebt -
      proj.entry.call.step.badDebtBranch.badDebt
  exact Liquidate.normalModeAfterBadDebtStep_currentDebtEqOriginalMinusBadDebt
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_currentDebtEqZeroFloor
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.currentDebtLocal =
      UnitsAccounting.zeroFloorSub proj.entry.storageFrame.beforePosition.debt
        proj.entry.call.step.badDebtBranch.badDebt := by
  rw [normalModeLiquidateProjection_currentDebtEqOriginalMinusBadDebt proj,
    UnitsAccounting.zeroFloorSub_eq_sub]

theorem normalModeLiquidateProjection_currentDebtPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.currentDebtLocal > 0 :=
  Liquidate.normalModeAfterBadDebtStep_currentDebtPositive
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_originalDebtPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.beforePosition.debt > 0 := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.badDebtBranch.loopStep.originalDebt > 0
  exact Liquidate.normalModeAfterBadDebtStep_originalDebtPositive
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_preLoopMaxDebtEqSelectedPlusOther
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    s.maxDebt =
      maxDebtAfterCollateralLoop
        (collateralMaxDebt
          proj.entry.storageFrame.afterBadDebtPosition.selectedCollateral
          proj.entry.call.step.rcfBranch.loopStep.price p.lltv)
        (otherMaxDebtContributions proj.entry.call.step) := by
  rw [proj.entry.storageFrameEq]
  change s.maxDebt =
    maxDebtAfterCollateralLoop
      (collateralMaxDebt proj.entry.call.step.rcfBranch.loopStep.collateral
        proj.entry.call.step.rcfBranch.loopStep.price p.lltv)
      (otherMaxDebtContributions proj.entry.call.step)
  exact preLoopMaxDebtEqSelectedPlusOther proj.entry.call.step

theorem normalModeLiquidateProjection_postLoopMaxDebtProjectionEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    postLoopMaxDebtProjection proj.entry.call.step =
      (Refinement.RCF.projectedAfterBadDebtLocalPostState
        p s proj.entry.call.step.rcfBranch).maxDebt :=
  postLoopMaxDebtProjection_eq_projected proj.entry.call.step

theorem normalModeLiquidateProjection_postLoopMaxDebtEqStorageProjection
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    maxDebtAfterCollateralLoop
        (collateralMaxDebt
          proj.entry.storageFrame.afterPosition.selectedCollateral
          proj.entry.call.step.rcfBranch.loopStep.price p.lltv)
        (otherMaxDebtContributions proj.entry.call.step) =
      (Refinement.RCF.projectedAfterBadDebtLocalPostState
        p s proj.entry.call.step.rcfBranch).maxDebt := by
  rw [proj.entry.storageFrameEq]
  change postLoopMaxDebtProjection proj.entry.call.step =
    (Refinement.RCF.projectedAfterBadDebtLocalPostState
      p s proj.entry.call.step.rcfBranch).maxDebt
  exact postLoopMaxDebtProjection_eq_projected proj.entry.call.step

theorem normalModeLiquidateProjection_repayBranchActive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.repaidUnits > 0 ∨
      proj.entry.call.step.rcfBranch.seizedAssets > 0 :=
  normalModeCall_repayBranchActive proj.entry.call

theorem normalModeLiquidateProjection_repaidUnitsPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.repaidUnits > 0 :=
  Liquidate.normalModeAfterBadDebtStep_repaidUnitsPositive
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_seizedAssetsEqFormula
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.seizedAssets =
      seizedAssetsFromRepayValue
        (mulDivDown proj.entry.call.step.rcfBranch.repaidUnits p.lif WAD)
        proj.entry.call.step.rcfBranch.loopStep.price :=
  Refinement.RCF.repaidMaxAfterBadDebtLocalStep_seizedAssetsEqFormula
    p s proj.entry.call.step.rcfBranch

theorem normalModeLiquidateProjection_withdrawableIncreases
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.beforeMarket.withdrawable ≤
      proj.entry.storageFrame.afterMarket.withdrawable := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.rcfBranch.withdrawableLocal ≤
    proj.entry.call.step.rcfBranch.postWithdrawableLocal
  exact Liquidate.normalModeAfterBadDebtStep_withdrawableIncreases
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_finalDebtEqProjected
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterPosition.debt =
      (Refinement.RCF.projectedAfterBadDebtLocalPostState
        p s proj.entry.call.step.rcfBranch).debt := by
  rw [proj.entry.storageFrameEq]
  change (postPosition proj.entry.call.step).debt =
    (Refinement.RCF.projectedAfterBadDebtLocalPostState
      p s proj.entry.call.step.rcfBranch).debt
  exact postPosition_debtEqProjected proj.entry.call.step

theorem normalModeLiquidateProjection_maxRepaidLocalEqSolidityFormula
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.maxRepaidLocal =
      mulDivUp
        (proj.entry.call.step.rcfBranch.currentDebtLocal -
          proj.entry.call.step.rcfBranch.loopStep.loop.maxDebt)
        scale (denominator p) :=
  Liquidate.normalModeAfterBadDebtStep_maxRepaidLocalEqSolidityFormula
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_rcfDenominatorPositive
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    denominator p > 0 :=
  Liquidate.normalModeAfterBadDebtStep_rcfDenominatorPositive
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_passesRcfGuard
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) (rcfThreshold : Nat) :
    rcfAllows proj.entry.call.step.rcfBranch.repaidUnits
      proj.entry.call.step.rcfBranch.maxRepaidLocal
      (collateralRepayCapacity
        proj.entry.call.step.rcfBranch.loopStep.collateral
        proj.entry.call.step.rcfBranch.loopStep.price p.lif)
      rcfThreshold :=
  Liquidate.normalModeAfterBadDebtStep_passesRcfGuard
    p s lenders proj.entry.call.step rcfThreshold

theorem normalModeLiquidateProjection_passesRcfGuardZeroFloor
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) (rcfThreshold : Nat) :
    rcfAllowsZeroFloor proj.entry.call.step.rcfBranch.repaidUnits
      proj.entry.call.step.rcfBranch.maxRepaidLocal
      (collateralRepayCapacity
        proj.entry.call.step.rcfBranch.loopStep.collateral
        proj.entry.call.step.rcfBranch.loopStep.price p.lif)
      rcfThreshold :=
  Liquidate.normalModeAfterBadDebtStep_passesRcfGuardZeroFloor
    p s lenders proj.entry.call.step rcfThreshold

theorem normalModeLiquidateProjection_generalRcfAccepted
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) (rcfThreshold : Nat) :
    (proj.entry.call.step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).rcfAccepted :=
  Liquidate.normalModeAfterBadDebtStep_generalRcfAccepted
    p s lenders proj.entry.call.step rcfThreshold

theorem normalModeLiquidateProjection_rcfSubtractionsSafe
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.repaidUnits ≤
        proj.entry.call.step.rcfBranch.currentDebtLocal ∧
      proj.entry.call.step.rcfBranch.seizedAssets ≤
        proj.entry.call.step.rcfBranch.loopStep.collateral :=
  Liquidate.normalModeAfterBadDebtStep_rcfSubtractionsSafe
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_badDebtSubtractionsSafe
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.badDebtBranch.badDebt ≤
        proj.entry.call.step.badDebtBranch.loopStep.originalDebt ∧
      proj.entry.call.step.badDebtBranch.badDebt ≤
        proj.entry.call.step.badDebtBranch.oldTotalUnits :=
  Liquidate.normalModeAfterBadDebtStep_badDebtSubtractionsSafe
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_transferAmountsUint128
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.call.step.rcfBranch.repaidUnits ≤ UINT128_MAX ∧
      proj.entry.call.step.rcfBranch.seizedAssets ≤ UINT128_MAX ∧
      proj.entry.call.step.badDebtBranch.badDebt ≤ UINT128_MAX :=
  ⟨Liquidate.normalModeAfterBadDebtStep_repaidUnitsUint128
      p s lenders proj.entry.call.step,
    Liquidate.normalModeAfterBadDebtStep_seizedAssetsUint128
      p s lenders proj.entry.call.step,
    Liquidate.normalModeAfterBadDebtStep_badDebtUint128
      p s lenders proj.entry.call.step⟩

theorem normalModeLiquidateProjection_postStorageUint128
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterPosition.debt ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterPosition.selectedCollateral ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterPosition.collateralBitmap ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterMarket.totalUnits ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterMarket.lossFactor ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterMarket.withdrawable ≤ UINT128_MAX ∧
      proj.entry.storageFrame.afterMarket.continuousFeeCredit ≤ UINT128_MAX := by
  exact
    ⟨proj.entry.storageFrame.afterPosition.debtUint128,
      proj.entry.storageFrame.afterPosition.selectedCollateralUint128,
      proj.entry.storageFrame.afterPosition.collateralBitmapUint128,
      proj.entry.storageFrame.afterMarket.totalUnitsUint128,
      proj.entry.storageFrame.afterMarket.lossFactorUint128,
      proj.entry.storageFrame.afterMarket.withdrawableUint128,
      proj.entry.storageFrame.afterMarket.continuousFeeCreditUint128⟩

theorem normalModeLiquidateProjection_badDebtDebtWriteEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterBadDebtPosition.debt =
      proj.entry.storageFrame.beforePosition.debt -
        proj.entry.call.step.badDebtBranch.badDebt := by
  rw [proj.entry.storageFrameEq]
  exact postBadDebtPosition_debtEq proj.entry.call.step

theorem normalModeLiquidateProjection_badDebtDebtWriteEqZeroFloor
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterBadDebtPosition.debt =
      UnitsAccounting.zeroFloorSub proj.entry.storageFrame.beforePosition.debt
        proj.entry.call.step.badDebtBranch.badDebt := by
  rw [normalModeLiquidateProjection_badDebtDebtWriteEq proj,
    UnitsAccounting.zeroFloorSub_eq_sub]

theorem normalModeLiquidateProjection_finalDebtWriteEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterPosition.debt =
      proj.entry.storageFrame.afterBadDebtPosition.debt -
        proj.entry.call.step.rcfBranch.repaidUnits := by
  rw [proj.entry.storageFrameEq]
  exact postPosition_debtEq proj.entry.call.step

theorem normalModeLiquidateProjection_selectedCollateralWriteEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterPosition.selectedCollateral =
      proj.entry.storageFrame.afterBadDebtPosition.selectedCollateral -
        proj.entry.call.step.rcfBranch.seizedAssets := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.rcfBranch.postCollateralLocal =
    proj.entry.call.step.rcfBranch.loopStep.collateral -
      proj.entry.call.step.rcfBranch.seizedAssets
  exact proj.entry.call.step.rcfBranch.postCollateralLocalEq

theorem normalModeLiquidateProjection_collateralBitmapWriteEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterPosition.collateralBitmap =
      RCF.collateralBitmapAfterSeizure
        proj.entry.storageFrame.afterBadDebtPosition.collateralBitmap
        proj.entry.call.step.rcfBranch.loopStep.selectedIndex
        proj.entry.storageFrame.afterPosition.selectedCollateral
        proj.entry.call.step.rcfBranch.seizedAssets := by
  rw [proj.entry.storageFrameEq]
  change proj.entry.call.step.rcfBranch.postCollateralBitmapLocal =
    RCF.collateralBitmapAfterSeizure
      proj.entry.call.step.rcfBranch.collateralBitmapLocal
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex
      proj.entry.call.step.rcfBranch.postCollateralLocal
      proj.entry.call.step.rcfBranch.seizedAssets
  exact Liquidate.normalModeAfterBadDebtStep_postCollateralBitmapEq
    p s lenders proj.entry.call.step

theorem normalModeLiquidateProjection_collateralBitmapCleared
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hempty : proj.entry.storageFrame.afterPosition.selectedCollateral = 0)
    (hseized : proj.entry.call.step.rcfBranch.seizedAssets > 0) :
    proj.entry.storageFrame.afterPosition.collateralBitmap =
      clearBitmapBit proj.entry.storageFrame.afterBadDebtPosition.collateralBitmap
        proj.entry.call.step.rcfBranch.loopStep.selectedIndex := by
  rw [proj.entry.storageFrameEq] at hempty ⊢
  exact Liquidate.normalModeAfterBadDebtStep_postCollateralBitmapCleared
    p s lenders proj.entry.call.step hempty hseized

theorem normalModeLiquidateProjection_collateralBitmapClearedBitFalse
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hempty : proj.entry.storageFrame.afterPosition.selectedCollateral = 0)
    (hseized : proj.entry.call.step.rcfBranch.seizedAssets > 0) :
    proj.entry.storageFrame.afterPosition.collateralBitmap.testBit
      proj.entry.call.step.rcfBranch.loopStep.selectedIndex = false := by
  rw [normalModeLiquidateProjection_collateralBitmapCleared
    proj hempty hseized]
  exact clearBitmapBit_testBit_cleared
    proj.entry.storageFrame.afterBadDebtPosition.collateralBitmap
    (normalModeLiquidateProjection_selectedIndexLt128 proj)

theorem normalModeLiquidateProjection_collateralBitmapUnchanged
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hkeep :
      ¬ (proj.entry.storageFrame.afterPosition.selectedCollateral = 0 ∧
        proj.entry.call.step.rcfBranch.seizedAssets > 0)) :
    proj.entry.storageFrame.afterPosition.collateralBitmap =
      proj.entry.storageFrame.afterBadDebtPosition.collateralBitmap := by
  rw [proj.entry.storageFrameEq] at hkeep ⊢
  exact Liquidate.normalModeAfterBadDebtStep_postCollateralBitmapUnchanged
    p s lenders proj.entry.call.step hkeep

theorem normalModeLiquidateProjection_marketWritesEq
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    proj.entry.storageFrame.afterMarket.totalUnits =
        proj.entry.storageFrame.beforeMarket.totalUnits -
          proj.entry.call.step.badDebtBranch.badDebt ∧
      proj.entry.storageFrame.afterMarket.lossFactor =
        lossFactorAfterBadDebt
          proj.entry.storageFrame.beforeMarket.totalUnits
          proj.entry.call.step.badDebtBranch.badDebt
          proj.entry.storageFrame.beforeMarket.lossFactor ∧
      proj.entry.storageFrame.afterMarket.withdrawable =
        proj.entry.storageFrame.beforeMarket.withdrawable +
          proj.entry.call.step.rcfBranch.repaidUnits ∧
      proj.entry.storageFrame.afterMarket.continuousFeeCredit =
        continuousFeeCreditAfterBadDebt
          proj.entry.storageFrame.beforeMarket.continuousFeeCredit
          proj.entry.storageFrame.beforeMarket.lossFactor
          proj.entry.storageFrame.afterMarket.lossFactor := by
  rw [proj.entry.storageFrameEq]
  exact
    ⟨postMarket_totalUnitsEq proj.entry.call.step,
      postMarket_lossFactorEq proj.entry.call.step,
      postMarket_withdrawableEq proj.entry.call.step,
      postMarket_continuousFeeCreditEq proj.entry.call.step⟩

theorem normalModeLiquidateProjection_badDebtZeroNoop
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders)
    (hbad : proj.entry.call.step.badDebtBranch.badDebt = 0) :
    proj.entry.storageFrame.afterBadDebtPosition.debt =
        proj.entry.storageFrame.beforePosition.debt ∧
      proj.entry.storageFrame.afterMarket.totalUnits =
        proj.entry.storageFrame.beforeMarket.totalUnits ∧
      proj.entry.storageFrame.afterMarket.lossFactor =
        proj.entry.storageFrame.beforeMarket.lossFactor ∧
      proj.entry.storageFrame.afterMarket.continuousFeeCredit =
        proj.entry.storageFrame.beforeMarket.continuousFeeCredit := by
  rw [proj.entry.storageFrameEq]
  exact
    ⟨postBadDebtPosition_debtNoopOfBadDebtZero proj.entry.call.step hbad,
      postMarket_totalUnitsNoopOfBadDebtZero proj.entry.call.step hbad,
      postMarket_lossFactorNoopOfBadDebtZero proj.entry.call.step hbad,
      postMarket_continuousFeeCreditNoopOfBadDebtZero
        proj.entry.call.step hbad⟩

theorem normalModeLiquidateProjection_coversCredits
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    totalUnitsCoversCredits
      { totalUnits := proj.entry.storageFrame.afterMarket.totalUnits,
        lossFactor := proj.entry.storageFrame.afterMarket.lossFactor }
      lenders := by
  rw [proj.entry.storageFrameEq]
  change totalUnitsCoversCredits
    { totalUnits := (postMarket proj.entry.call.step).totalUnits,
      lossFactor := (postMarket proj.entry.call.step).lossFactor }
    lenders
  exact postMarket_coversCredits proj.entry.call.step

theorem normalModeLiquidateProjection_coversPostUpdateCredits
    {p : RCFParams} {s : HealthState} {positions : List LenderPosition}
    (accrualEnd maturity : Nat)
    (proj :
      NormalModeLiquidateProjection p s (lendersOfPositions positions)) :
    sumPostUpdateCreditView
        { totalUnits := proj.entry.storageFrame.afterMarket.totalUnits,
          lossFactor := proj.entry.storageFrame.afterMarket.lossFactor }
        accrualEnd maturity positions ≤
      proj.entry.storageFrame.afterMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change sumPostUpdateCreditView
      { totalUnits := (postMarket proj.entry.call.step).totalUnits,
        lossFactor := (postMarket proj.entry.call.step).lossFactor }
      accrualEnd maturity positions ≤
    (postMarket proj.entry.call.step).totalUnits
  exact postMarket_coversPostUpdateCredits accrualEnd maturity
    proj.entry.call.step

theorem normalModeLiquidateProjection_coversUpdatePositionViewResultCredits
    {p : RCFParams} {s : HealthState} {positions : List LenderPosition}
    (timestamp maturity : Nat)
    (proj :
      NormalModeLiquidateProjection p s (lendersOfPositions positions)) :
    sumUpdatePositionViewResultCredit
        { totalUnits := proj.entry.storageFrame.afterMarket.totalUnits,
          lossFactor := proj.entry.storageFrame.afterMarket.lossFactor }
        timestamp maturity positions ≤
      proj.entry.storageFrame.afterMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change sumUpdatePositionViewResultCredit
      { totalUnits := (postMarket proj.entry.call.step).totalUnits,
        lossFactor := (postMarket proj.entry.call.step).lossFactor }
      timestamp maturity positions ≤
    (postMarket proj.entry.call.step).totalUnits
  exact postMarket_coversUpdatePositionViewResultCredits timestamp maturity
    proj.entry.call.step

theorem normalModeLiquidateProjection_coversStoredCreditsAfterUpdates
    {p : RCFParams} {s : HealthState} {lenders : List Lender}
    (proj : NormalModeLiquidateProjection p s lenders) :
    sumCredit
        (lenders.map
          (updateLender
            { totalUnits := proj.entry.storageFrame.afterMarket.totalUnits,
              lossFactor := proj.entry.storageFrame.afterMarket.lossFactor })) ≤
      proj.entry.storageFrame.afterMarket.totalUnits := by
  rw [proj.entry.storageFrameEq]
  change sumCredit
      (lenders.map
        (updateLender
          { totalUnits := (postMarket proj.entry.call.step).totalUnits,
            lossFactor := (postMarket proj.entry.call.step).lossFactor })) ≤
    (postMarket proj.entry.call.step).totalUnits
  exact postMarket_coversStoredCreditsAfterUpdates proj.entry.call.step

end LiquidateStorage

end Midnight.Proofs.Storage
