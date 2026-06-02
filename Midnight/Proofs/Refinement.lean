/-
  Refinement — named generated-body obligations for the focused Midnight model.

  The arithmetic theorems in `RCF.lean` and `UnitsAccounting.lean` are proved
  over small projections. This file is the explicit boundary a future extraction
  from `MidnightRCF` / full `Midnight.sol` must discharge.
-/

import Midnight.Proofs.RCF
import Midnight.Proofs.UnitsAccounting
import Midnight.Proofs.CollateralLoop
import Midnight.Proofs.BitmapSchedule
import Midnight.Proofs.MarketLedger

namespace Midnight.Proofs.Refinement

open Midnight.Proofs.CollateralLoop
open Midnight.Proofs.BitmapSchedule

/-- Shared extraction target for the `liquidate` active-collateral loop.
    Solidity initializes `maxDebt = 0`, `liquidatedCollatPrice = 0`, and
    `badDebt = originalDebt`, then visits the concrete `msb` / `clearBit`
    trace.  This structure records the final local variables from that exact
    loop in one place so both RCF and accounting refinements can consume them. -/
structure LiquidateLoopTrace where
  liquidatedIndex : Nat
  activeCollateralArrays : CollateralArrays
  activeCollateralBitmap : Nat
  activeVisited : List Nat
  initialDebt : Nat
  maxDebt : Nat
  liquidatedCollatPrice : Nat
  badDebt : Nat
  activeBitmapValid : ValidCollateralBitmap activeCollateralBitmap
  activeTrace : MsbClearTrace activeCollateralBitmap activeVisited
  outputEq :
    run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := initialDebt }
        (slotsOfIndices activeCollateralArrays activeVisited) =
      { maxDebt := maxDebt,
        liquidatedCollatPrice := liquidatedCollatPrice,
        badDebt := badDebt }

theorem liquidateLoopTrace_maxDebtEq (loop : LiquidateLoopTrace) :
    (run loop.liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
        (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).maxDebt =
      loop.maxDebt := by
  exact congrArg LoopState.maxDebt loop.outputEq

theorem liquidateLoopTrace_liquidatedCollatPriceEq (loop : LiquidateLoopTrace) :
    (run loop.liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
        (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).liquidatedCollatPrice =
      loop.liquidatedCollatPrice := by
  exact congrArg LoopState.liquidatedCollatPrice loop.outputEq

theorem liquidateLoopTrace_badDebtEq (loop : LiquidateLoopTrace) :
    (run loop.liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
        (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).badDebt =
      loop.badDebt := by
  exact congrArg LoopState.badDebt loop.outputEq

theorem liquidateLoopTrace_validSchedule (loop : LiquidateLoopTrace) :
    ValidSchedule loop.activeVisited :=
  msbClearTrace_valid loop.activeBitmapValid loop.activeTrace

theorem liquidateLoopTrace_slotsBound (loop : LiquidateLoopTrace) :
    (slotsOfIndices loop.activeCollateralArrays loop.activeVisited).length ≤
      MAX_COLLATERALS_PER_BORROWER :=
  msbClearTrace_slots_length_le_16 loop.activeCollateralArrays
    loop.activeBitmapValid loop.activeTrace

theorem liquidateLoopTrace_maxDebtEqSum (loop : LiquidateLoopTrace) :
    loop.maxDebt =
      sumMaxDebtContributions
        (slotsOfIndices loop.activeCollateralArrays loop.activeVisited) := by
  calc
    loop.maxDebt =
        (run loop.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).maxDebt :=
        (liquidateLoopTrace_maxDebtEq loop).symm
    _ =
        sumMaxDebtContributions
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited) := by
        rw [run_maxDebt_from_zero_eq]

theorem liquidateLoopTrace_badDebtLeInitial (loop : LiquidateLoopTrace) :
    loop.badDebt ≤ loop.initialDebt := by
  calc
    loop.badDebt =
        (run loop.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).badDebt :=
        (liquidateLoopTrace_badDebtEq loop).symm
    _ ≤ loop.initialDebt :=
        run_badDebt_le_initial loop.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)

theorem liquidateLoopTrace_selectedPriceEq
    (loop : LiquidateLoopTrace) {before after : List Nat}
    (hsplit : loop.activeVisited = before ++ loop.liquidatedIndex :: after) :
    loop.liquidatedCollatPrice =
      loop.activeCollateralArrays.price loop.liquidatedIndex := by
  have hnodup : (before ++ loop.liquidatedIndex :: after).Nodup := by
    have hvalid : ValidSchedule loop.activeVisited :=
      liquidateLoopTrace_validSchedule loop
    rw [hsplit] at hvalid
    exact validSchedule_nodup hvalid
  calc
    loop.liquidatedCollatPrice =
        (run loop.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := loop.initialDebt }
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited)).liquidatedCollatPrice :=
        (liquidateLoopTrace_liquidatedCollatPriceEq loop).symm
    _ =
        selectedPriceAfter loop.liquidatedIndex 0
          (slotsOfIndices loop.activeCollateralArrays loop.activeVisited) := by
        rw [run_liquidatedCollatPrice_from_zero_eq]
    _ =
        selectedPriceAfter loop.liquidatedIndex 0
          (slotsOfIndices loop.activeCollateralArrays
            (before ++ loop.liquidatedIndex :: after)) := by
        rw [hsplit]
    _ = loop.activeCollateralArrays.price loop.liquidatedIndex :=
        selectedPriceAfter_split_selectedIndex loop.activeCollateralArrays
          before after loop.liquidatedIndex 0 hnodup

namespace RCF

open Midnight.Proofs.Basic
open Midnight.Proofs.RCF
open Midnight.Proofs.CollateralLoop
open Midnight.Proofs.BitmapSchedule

/-- Generated-body obligations for normal-mode repaid-input liquidation at
    `repaidUnits = maxRepaid`: the selected collateral seizure follows the
    Solidity repaid-input formula, and the health-loop `maxDebt` projection is
    supplied by the same active-collateral bitmap schedule used by
    `liquidate`/`isHealthy`. -/
structure RepaidMaxStep (p : RCFParams) (s : HealthState) where
  collateral : Nat
  price : Nat
  activeCollateralArrays : CollateralArrays
  activeCollateralBitmap : Nat
  beforeSelectedIndices : List Nat
  selectedIndex : Nat
  afterSelectedIndices : List Nat
  activeBitmapValid : ValidCollateralBitmap activeCollateralBitmap
  activeScheduleEq :
    beforeSelectedIndices ++ selectedIndex :: afterSelectedIndices =
      bitmapSchedule activeCollateralBitmap
  collateralEqSelected : collateral = activeCollateralArrays.collateral selectedIndex
  priceEqSelected : price = activeCollateralArrays.price selectedIndex
  lltvEqSelected : p.lltv = activeCollateralArrays.lltv selectedIndex
  lifEqSelectedMaxLif : p.lif = activeCollateralArrays.maxLif selectedIndex
  unhealthy : s.debt > s.maxDebt
  lltvValid : p.lltv < WAD
  coeffValid : debtDecreaseCoeff p < scale
  repaidWithinDebtDomain : s.debt * debtDecreaseCoeff p ≤ s.maxDebt * scale
  repaidWithinCollateralCapacity :
    maxRepaid p s ≤ collateralRepayCapacity collateral price p.lif
  maxDebtEqBitmapSchedule :
    s.maxDebt =
      sumMaxDebtContributions
        (slotsOfIndices activeCollateralArrays
          (beforeSelectedIndices ++ selectedIndex :: afterSelectedIndices))

/-- More contract-shaped obligations for `liquidate`: instead of requiring a
    precomputed bitmap schedule equality, extraction may provide the actual
    visited index trace produced by repeated `msb` / `clearBit` loop steps. -/
structure RepaidMaxTraceStep (p : RCFParams) (s : HealthState) where
  collateral : Nat
  price : Nat
  activeCollateralArrays : CollateralArrays
  activeCollateralBitmap : Nat
  activeVisited : List Nat
  beforeSelectedIndices : List Nat
  selectedIndex : Nat
  afterSelectedIndices : List Nat
  activeBitmapValid : ValidCollateralBitmap activeCollateralBitmap
  activeTrace : MsbClearTrace activeCollateralBitmap activeVisited
  activeTraceEqSplit :
    activeVisited = beforeSelectedIndices ++ selectedIndex :: afterSelectedIndices
  collateralEqSelected : collateral = activeCollateralArrays.collateral selectedIndex
  priceEqSelected : price = activeCollateralArrays.price selectedIndex
  lltvEqSelected : p.lltv = activeCollateralArrays.lltv selectedIndex
  lifEqSelectedMaxLif : p.lif = activeCollateralArrays.maxLif selectedIndex
  unhealthy : s.debt > s.maxDebt
  lltvValid : p.lltv < WAD
  coeffValid : debtDecreaseCoeff p < scale
  repaidWithinDebtDomain : s.debt * debtDecreaseCoeff p ≤ s.maxDebt * scale
  repaidWithinCollateralCapacity :
    maxRepaid p s ≤ collateralRepayCapacity collateral price p.lif
  maxDebtEqTraceLoop :
    (run selectedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
        (slotsOfIndices activeCollateralArrays activeVisited)).maxDebt =
      s.maxDebt

/-- RCF branch obligations when extraction provides the shared Solidity loop
    locals.  The only branch-specific equalities are that the loop selected the
    requested collateral, was initialized from the RCF debt projection, and
    produced the `maxDebt`/price values consumed by the RCF check. -/
structure RepaidMaxLoopStep (p : RCFParams) (s : HealthState) where
  collateral : Nat
  price : Nat
  loop : LiquidateLoopTrace
  beforeSelectedIndices : List Nat
  selectedIndex : Nat
  afterSelectedIndices : List Nat
  loopInitialDebtEq : loop.initialDebt = s.debt
  loopSelectedIndexEq : loop.liquidatedIndex = selectedIndex
  activeTraceEqSplit :
    loop.activeVisited = beforeSelectedIndices ++ selectedIndex :: afterSelectedIndices
  loopMaxDebtEq : loop.maxDebt = s.maxDebt
  loopPriceEq : loop.liquidatedCollatPrice = price
  collateralEqSelected : collateral = loop.activeCollateralArrays.collateral selectedIndex
  lltvEqSelected : p.lltv = loop.activeCollateralArrays.lltv selectedIndex
  lifEqSelectedMaxLif : p.lif = loop.activeCollateralArrays.maxLif selectedIndex
  unhealthy : s.debt > s.maxDebt
  lltvValid : p.lltv < WAD
  coeffValid : debtDecreaseCoeff p < scale
  repaidWithinDebtDomain : s.debt * debtDecreaseCoeff p ≤ s.maxDebt * scale
  repaidWithinCollateralCapacity :
    maxRepaid p s ≤ collateralRepayCapacity collateral price p.lif

/-- RCF branch obligations for the exact Solidity sequencing when bad debt may
    have been realized before the repaid-input branch.  The active-collateral
    loop is still initialized from `originalDebt`, but the `maxRepaid` local is
    computed from the current `_position.debt`, namely `originalDebt - badDebt`.
    The max-debt accumulator is independent of the loop's bad-debt seed, so this
    structure can still discharge the same RCF recovery theorem over `s`. -/
structure RepaidMaxAfterBadDebtLoopStep (p : RCFParams) (s : HealthState) where
  collateral : Nat
  price : Nat
  loop : LiquidateLoopTrace
  originalDebt : Nat
  badDebt : Nat
  beforeSelectedIndices : List Nat
  selectedIndex : Nat
  afterSelectedIndices : List Nat
  loopInitialDebtEq : loop.initialDebt = originalDebt
  loopBadDebtEq : loop.badDebt = badDebt
  currentDebtEq : s.debt = originalDebt - badDebt
  loopSelectedIndexEq : loop.liquidatedIndex = selectedIndex
  activeTraceEqSplit :
    loop.activeVisited = beforeSelectedIndices ++ selectedIndex :: afterSelectedIndices
  loopMaxDebtEq : loop.maxDebt = s.maxDebt
  loopPriceEq : loop.liquidatedCollatPrice = price
  collateralEqSelected : collateral = loop.activeCollateralArrays.collateral selectedIndex
  lltvEqSelected : p.lltv = loop.activeCollateralArrays.lltv selectedIndex
  lifEqSelectedMaxLif : p.lif = loop.activeCollateralArrays.maxLif selectedIndex
  unhealthy : s.debt > s.maxDebt
  lltvValid : p.lltv < WAD
  coeffValid : debtDecreaseCoeff p < scale
  repaidWithinDebtDomain : s.debt * debtDecreaseCoeff p ≤ s.maxDebt * scale
  repaidWithinCollateralCapacity :
    maxRepaid p s ≤ collateralRepayCapacity collateral price p.lif

def RepaidMaxLoopStep.toRepaidMaxTraceStep
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    RepaidMaxTraceStep p s :=
  { collateral := loopStep.collateral,
    price := loopStep.price,
    activeCollateralArrays := loopStep.loop.activeCollateralArrays,
    activeCollateralBitmap := loopStep.loop.activeCollateralBitmap,
    activeVisited := loopStep.loop.activeVisited,
    beforeSelectedIndices := loopStep.beforeSelectedIndices,
    selectedIndex := loopStep.selectedIndex,
    afterSelectedIndices := loopStep.afterSelectedIndices,
    activeBitmapValid := loopStep.loop.activeBitmapValid,
    activeTrace := loopStep.loop.activeTrace,
    activeTraceEqSplit := loopStep.activeTraceEqSplit,
    collateralEqSelected := loopStep.collateralEqSelected,
    priceEqSelected := by
      have hsplit :
          loopStep.loop.activeVisited =
            loopStep.beforeSelectedIndices ++ loopStep.loop.liquidatedIndex ::
              loopStep.afterSelectedIndices := by
        rw [loopStep.loopSelectedIndexEq]
        exact loopStep.activeTraceEqSplit
      calc
        loopStep.price = loopStep.loop.liquidatedCollatPrice :=
          loopStep.loopPriceEq.symm
        _ = loopStep.loop.activeCollateralArrays.price loopStep.loop.liquidatedIndex :=
            liquidateLoopTrace_selectedPriceEq loopStep.loop hsplit
        _ = loopStep.loop.activeCollateralArrays.price loopStep.selectedIndex := by
            rw [loopStep.loopSelectedIndexEq],
    lltvEqSelected := loopStep.lltvEqSelected,
    lifEqSelectedMaxLif := loopStep.lifEqSelectedMaxLif,
    unhealthy := loopStep.unhealthy,
    lltvValid := loopStep.lltvValid,
    coeffValid := loopStep.coeffValid,
    repaidWithinDebtDomain := loopStep.repaidWithinDebtDomain,
    repaidWithinCollateralCapacity := loopStep.repaidWithinCollateralCapacity,
    maxDebtEqTraceLoop := by
      calc
        (run loopStep.selectedIndex
            { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
            (slotsOfIndices loopStep.loop.activeCollateralArrays
              loopStep.loop.activeVisited)).maxDebt =
            (run loopStep.loop.liquidatedIndex
              { maxDebt := 0,
                liquidatedCollatPrice := 0,
                badDebt := loopStep.loop.initialDebt }
              (slotsOfIndices loopStep.loop.activeCollateralArrays
                loopStep.loop.activeVisited)).maxDebt := by
            rw [← loopStep.loopSelectedIndexEq, ← loopStep.loopInitialDebtEq]
        _ = loopStep.loop.maxDebt :=
            liquidateLoopTrace_maxDebtEq loopStep.loop
        _ = s.maxDebt := loopStep.loopMaxDebtEq }

def RepaidMaxAfterBadDebtLoopStep.toRepaidMaxTraceStep
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    RepaidMaxTraceStep p s :=
  { collateral := loopStep.collateral,
    price := loopStep.price,
    activeCollateralArrays := loopStep.loop.activeCollateralArrays,
    activeCollateralBitmap := loopStep.loop.activeCollateralBitmap,
    activeVisited := loopStep.loop.activeVisited,
    beforeSelectedIndices := loopStep.beforeSelectedIndices,
    selectedIndex := loopStep.selectedIndex,
    afterSelectedIndices := loopStep.afterSelectedIndices,
    activeBitmapValid := loopStep.loop.activeBitmapValid,
    activeTrace := loopStep.loop.activeTrace,
    activeTraceEqSplit := loopStep.activeTraceEqSplit,
    collateralEqSelected := loopStep.collateralEqSelected,
    priceEqSelected := by
      have hsplit :
          loopStep.loop.activeVisited =
            loopStep.beforeSelectedIndices ++ loopStep.loop.liquidatedIndex ::
              loopStep.afterSelectedIndices := by
        rw [loopStep.loopSelectedIndexEq]
        exact loopStep.activeTraceEqSplit
      calc
        loopStep.price = loopStep.loop.liquidatedCollatPrice :=
          loopStep.loopPriceEq.symm
        _ = loopStep.loop.activeCollateralArrays.price loopStep.loop.liquidatedIndex :=
            liquidateLoopTrace_selectedPriceEq loopStep.loop hsplit
        _ = loopStep.loop.activeCollateralArrays.price loopStep.selectedIndex := by
            rw [loopStep.loopSelectedIndexEq],
    lltvEqSelected := loopStep.lltvEqSelected,
    lifEqSelectedMaxLif := loopStep.lifEqSelectedMaxLif,
    unhealthy := loopStep.unhealthy,
    lltvValid := loopStep.lltvValid,
    coeffValid := loopStep.coeffValid,
    repaidWithinDebtDomain := loopStep.repaidWithinDebtDomain,
    repaidWithinCollateralCapacity := loopStep.repaidWithinCollateralCapacity,
    maxDebtEqTraceLoop := by
      calc
        (run loopStep.selectedIndex
            { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
            (slotsOfIndices loopStep.loop.activeCollateralArrays
              loopStep.loop.activeVisited)).maxDebt =
            sumMaxDebtContributions
              (slotsOfIndices loopStep.loop.activeCollateralArrays
                loopStep.loop.activeVisited) := by
            rw [run_maxDebt_from_zero_eq]
        _ = loopStep.loop.maxDebt :=
            (liquidateLoopTrace_maxDebtEqSum loopStep.loop).symm
        _ = s.maxDebt := loopStep.loopMaxDebtEq }

/-- Solidity-local normal-mode RCF branch values after the active-collateral
    loop: the contract computes a `maxRepaid` local and accepts the branch when
    the caller chooses `repaidUnits = maxRepaid`. -/
structure RepaidMaxLocalStep (p : RCFParams) (s : HealthState) where
  loopStep : RepaidMaxLoopStep p s
  maxRepaidLocal : Nat
  repaidUnits : Nat
  seizedAssets : Nat
  postDebtLocal : Nat
  postCollateralLocal : Nat
  maxRepaidLocalEq : maxRepaidLocal = maxRepaid p s
  repaidUnitsEqMaxRepaidLocal : repaidUnits = maxRepaidLocal
  seizedAssetsEq :
    seizedAssets =
      seizedAssetsFromRepayValue (mulDivDown repaidUnits p.lif WAD)
        loopStep.price
  postDebtLocalEq : postDebtLocal = s.debt - repaidUnits
  postCollateralLocalEq :
    postCollateralLocal = loopStep.collateral - seizedAssets

/-- Solidity-local RCF branch values after the optional bad-debt write.  This
    keeps the concrete current debt local separate from the loop's original debt
    seed, matching the order of writes in `liquidate`. -/
def collateralBitmapAfterSeizure
    (bitmap selectedIndex postCollateral seizedAssets : Nat) : Nat :=
  if postCollateral = 0 ∧ seizedAssets > 0 then
    clearBitmapBit bitmap selectedIndex
  else
    bitmap

def atMostOneNonZeroInput (repaidUnits seizedAssets : Nat) : Prop :=
  repaidUnits = 0 ∨ seizedAssets = 0

def repaidFromSeized (seizedAssets lif price : Nat) : Nat :=
  mulDivUp (mulDivUp seizedAssets price ORACLE_PRICE_SCALE) WAD lif

def repaySeizeAmounts
    (inputRepaidUnits inputSeizedAssets lif price : Nat) : Nat × Nat :=
  if inputSeizedAssets > 0 then
    (repaidFromSeized inputSeizedAssets lif price, inputSeizedAssets)
  else
    (inputRepaidUnits,
      seizedAssetsFromRepayValue (mulDivDown inputRepaidUnits lif WAD) price)

/-- Solidity-local normal-mode RCF branch for arbitrary caller amount mode.
    The max-repay recovery proof later specializes this structure to
    `inputRepaidUnits = maxRepaid` and `inputSeizedAssets = 0`. -/
structure NormalModeRcfLocalStep (p : RCFParams) (s : HealthState) where
  loopStep : RepaidMaxAfterBadDebtLoopStep p s
  currentDebtLocal : Nat
  collateralBitmapLocal : Nat
  withdrawableLocal : Nat
  inputRepaidUnitsLocal : Nat
  inputSeizedAssetsLocal : Nat
  maxRepaidLocal : Nat
  repaidUnits : Nat
  seizedAssets : Nat
  collateralRepayCapacityLocal : Nat
  capacityShortfallLocal : Nat
  rcfThresholdLocal : Nat
  rcfAccepted : Prop
  postWithdrawableLocal : Nat
  postDebtLocal : Nat
  postCollateralLocal : Nat
  postCollateralBitmapLocal : Nat
  currentDebtLocalEq : currentDebtLocal = s.debt
  amountBranchEq :
    (repaidUnits, seizedAssets) =
      repaySeizeAmounts inputRepaidUnitsLocal inputSeizedAssetsLocal p.lif
        loopStep.price
  maxRepaidLocalEq : maxRepaidLocal = maxRepaid p s
  collateralRepayCapacityLocalEq :
    collateralRepayCapacityLocal =
      collateralRepayCapacity loopStep.collateral loopStep.price p.lif
  capacityShortfallLocalEq :
    capacityShortfallLocal =
      zeroFloorSub collateralRepayCapacityLocal maxRepaidLocal
  rcfAcceptedEq :
    rcfAccepted =
      rcfAllowsZeroFloor repaidUnits maxRepaidLocal collateralRepayCapacityLocal
        rcfThresholdLocal
  postWithdrawableLocalEq :
    postWithdrawableLocal = withdrawableLocal + repaidUnits
  postDebtLocalEq : postDebtLocal = currentDebtLocal - repaidUnits
  postCollateralLocalEq :
    postCollateralLocal = loopStep.collateral - seizedAssets
  postCollateralBitmapLocalEq :
    postCollateralBitmapLocal =
      collateralBitmapAfterSeizure collateralBitmapLocal loopStep.selectedIndex
        postCollateralLocal seizedAssets

structure RepaidMaxAfterBadDebtLocalStep (p : RCFParams) (s : HealthState) where
  loopStep : RepaidMaxAfterBadDebtLoopStep p s
  currentDebtLocal : Nat
  collateralBitmapLocal : Nat
  withdrawableLocal : Nat
  inputRepaidUnitsLocal : Nat
  inputSeizedAssetsLocal : Nat
  maxRepaidLocal : Nat
  repaidUnits : Nat
  seizedAssets : Nat
  postWithdrawableLocal : Nat
  postDebtLocal : Nat
  postCollateralLocal : Nat
  postCollateralBitmapLocal : Nat
  currentDebtLocalUint128 : currentDebtLocal ≤ UINT128_MAX
  collateralLocalUint128 : loopStep.collateral ≤ UINT128_MAX
  collateralBitmapLocalUint128 : collateralBitmapLocal ≤ UINT128_MAX
  withdrawableLocalUint128 : withdrawableLocal ≤ UINT128_MAX
  postWithdrawableLocalUint128 : postWithdrawableLocal ≤ UINT128_MAX
  postCollateralBitmapLocalUint128 : postCollateralBitmapLocal ≤ UINT128_MAX
  currentDebtLocalEq : currentDebtLocal = s.debt
  inputRepaidUnitsLocalEq : inputRepaidUnitsLocal = repaidUnits
  inputSeizedAssetsLocalEq : inputSeizedAssetsLocal = 0
  maxRepaidLocalEq : maxRepaidLocal = maxRepaid p s
  repaidUnitsEqMaxRepaidLocal : repaidUnits = maxRepaidLocal
  seizedAssetsEq :
    seizedAssets =
      seizedAssetsFromRepayValue (mulDivDown repaidUnits p.lif WAD)
        loopStep.price
  postWithdrawableLocalEq :
    postWithdrawableLocal = withdrawableLocal + repaidUnits
  postDebtLocalEq : postDebtLocal = currentDebtLocal - repaidUnits
  postCollateralLocalEq :
    postCollateralLocal = loopStep.collateral - seizedAssets
  postCollateralBitmapLocalEq :
    postCollateralBitmapLocal =
      collateralBitmapAfterSeizure collateralBitmapLocal loopStep.selectedIndex
        postCollateralLocal seizedAssets

def RepaidMaxAfterBadDebtLocalStep.toNormalModeRcfLocalStep
    {p : RCFParams} {s : HealthState}
    (branch : RepaidMaxAfterBadDebtLocalStep p s) (rcfThreshold : Nat) :
    NormalModeRcfLocalStep p s :=
  { loopStep := branch.loopStep,
    currentDebtLocal := branch.currentDebtLocal,
    collateralBitmapLocal := branch.collateralBitmapLocal,
    withdrawableLocal := branch.withdrawableLocal,
    inputRepaidUnitsLocal := branch.inputRepaidUnitsLocal,
    inputSeizedAssetsLocal := branch.inputSeizedAssetsLocal,
    maxRepaidLocal := branch.maxRepaidLocal,
    repaidUnits := branch.repaidUnits,
    seizedAssets := branch.seizedAssets,
    collateralRepayCapacityLocal :=
      collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif,
    capacityShortfallLocal :=
      zeroFloorSub
        (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
        branch.maxRepaidLocal,
    rcfThresholdLocal := rcfThreshold,
    rcfAccepted :=
      rcfAllowsZeroFloor branch.repaidUnits branch.maxRepaidLocal
        (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
        rcfThreshold,
    postWithdrawableLocal := branch.postWithdrawableLocal,
    postDebtLocal := branch.postDebtLocal,
    postCollateralLocal := branch.postCollateralLocal,
    postCollateralBitmapLocal := branch.postCollateralBitmapLocal,
    currentDebtLocalEq := branch.currentDebtLocalEq,
    amountBranchEq := by
      simp [repaySeizeAmounts, branch.inputRepaidUnitsLocalEq,
        branch.inputSeizedAssetsLocalEq, branch.seizedAssetsEq],
    maxRepaidLocalEq := branch.maxRepaidLocalEq,
    collateralRepayCapacityLocalEq := rfl,
    capacityShortfallLocalEq := rfl,
    rcfAcceptedEq := rfl,
    postWithdrawableLocalEq := branch.postWithdrawableLocalEq,
    postDebtLocalEq := branch.postDebtLocalEq,
    postCollateralLocalEq := branch.postCollateralLocalEq,
    postCollateralBitmapLocalEq := branch.postCollateralBitmapLocalEq }

def RepaidMaxTraceStep.toRepaidMaxStep
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s) :
    RepaidMaxStep p s :=
  { collateral := trace.collateral,
    price := trace.price,
    activeCollateralArrays := trace.activeCollateralArrays,
    activeCollateralBitmap := trace.activeCollateralBitmap,
    beforeSelectedIndices := trace.beforeSelectedIndices,
    selectedIndex := trace.selectedIndex,
    afterSelectedIndices := trace.afterSelectedIndices,
    activeBitmapValid := trace.activeBitmapValid,
    activeScheduleEq := by
      calc
        trace.beforeSelectedIndices ++ trace.selectedIndex :: trace.afterSelectedIndices =
            trace.activeVisited := trace.activeTraceEqSplit.symm
        _ = bitmapSchedule trace.activeCollateralBitmap :=
            msbClearTrace_eq_bitmapSchedule trace.activeTrace,
    collateralEqSelected := trace.collateralEqSelected,
    priceEqSelected := trace.priceEqSelected,
    lltvEqSelected := trace.lltvEqSelected,
    lifEqSelectedMaxLif := trace.lifEqSelectedMaxLif,
    unhealthy := trace.unhealthy,
    lltvValid := trace.lltvValid,
    coeffValid := trace.coeffValid,
    repaidWithinDebtDomain := trace.repaidWithinDebtDomain,
    repaidWithinCollateralCapacity := trace.repaidWithinCollateralCapacity,
    maxDebtEqBitmapSchedule := by
      calc
        s.maxDebt =
            (run trace.selectedIndex
              { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
              (slotsOfIndices trace.activeCollateralArrays trace.activeVisited)).maxDebt :=
            trace.maxDebtEqTraceLoop.symm
        _ = sumMaxDebtContributions
              (slotsOfIndices trace.activeCollateralArrays trace.activeVisited) := by
            rw [run_maxDebt_from_zero_eq]
        _ = sumMaxDebtContributions
              (slotsOfIndices trace.activeCollateralArrays
                (trace.beforeSelectedIndices ++ trace.selectedIndex ::
                  trace.afterSelectedIndices)) := by
            rw [trace.activeTraceEqSplit] }

theorem repaidMaxStep_activeScheduleValid
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    ValidSchedule (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices) := by
  rw [step.activeScheduleEq]
  exact validCollateralBitmap_valid step.activeBitmapValid

theorem repaidMaxStep_activeBitmapUint128
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    step.activeCollateralBitmap < UINT128_BOUND :=
  step.activeBitmapValid.1

theorem repaidMaxStep_activeBitmapCountBound
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    countBits step.activeCollateralBitmap ≤ MAX_COLLATERALS_PER_BORROWER :=
  step.activeBitmapValid.2

theorem repaidMaxStep_activeScheduleLengthEqCountBits
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    (bitmapSchedule step.activeCollateralBitmap).length =
      countBits step.activeCollateralBitmap :=
  bitmapSchedule_length_eq_countBits step.activeCollateralBitmap

theorem repaidMaxStep_selectedBitSet
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    step.activeCollateralBitmap.testBit step.selectedIndex = true := by
  have hmem :
      step.selectedIndex ∈ bitmapSchedule step.activeCollateralBitmap := by
    rw [← step.activeScheduleEq]
    simp
  exact bitmapSchedule_mem_testBit hmem

theorem repaidMaxStep_scheduleAfterClearingBefore
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    bitmapSchedule
        (clearBitmapBits step.activeCollateralBitmap step.beforeSelectedIndices) =
      step.selectedIndex :: step.afterSelectedIndices :=
  bitmapSchedule_clearBitmapBits_prefix_eq_suffix
    step.beforeSelectedIndices (step.selectedIndex :: step.afterSelectedIndices)
    step.activeScheduleEq.symm

theorem repaidMaxStep_selectedHighestAfterClearingBefore
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    HighestBitmapBit
      (clearBitmapBits step.activeCollateralBitmap step.beforeSelectedIndices)
      MAX_COLLATERALS step.selectedIndex :=
  bitmapSchedule_cons_highestBitmapBit
    (repaidMaxStep_scheduleAfterClearingBefore p s step)

theorem repaidMaxStep_scheduleAfterClearingThroughSelected
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    bitmapSchedule
        (clearBitmapBits step.activeCollateralBitmap
          (step.beforeSelectedIndices ++ [step.selectedIndex])) =
      step.afterSelectedIndices := by
  have hsplit :
      bitmapSchedule step.activeCollateralBitmap =
        (step.beforeSelectedIndices ++ [step.selectedIndex]) ++
          step.afterSelectedIndices := by
    rw [← step.activeScheduleEq]
    simp [List.append_assoc]
  exact bitmapSchedule_clearBitmapBits_prefix_eq_suffix
    (step.beforeSelectedIndices ++ [step.selectedIndex])
    step.afterSelectedIndices hsplit

theorem repaidMaxStep_msbTraceEqSchedule
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    visited =
      step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices := by
  rw [msbClearTrace_eq_bitmapSchedule htrace]
  exact step.activeScheduleEq.symm

theorem repaidMaxStep_msbTraceValid
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    ValidSchedule visited :=
  msbClearTrace_valid step.activeBitmapValid htrace

theorem repaidMaxStep_msbTraceNodup
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    visited.Nodup :=
  msbClearTrace_nodup step.activeBitmapValid htrace

theorem repaidMaxStep_msbTraceSlotsBound
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    (slotsOfIndices step.activeCollateralArrays visited).length ≤
      MAX_COLLATERALS_PER_BORROWER :=
  msbClearTrace_slots_length_le_16 step.activeCollateralArrays
    step.activeBitmapValid htrace

theorem repaidMaxStep_selectedHighestFromMsbTraceSplit
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited)
    (hsplit :
      visited =
        step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices) :
    HighestBitmapBit
      (clearBitmapBits step.activeCollateralBitmap step.beforeSelectedIndices)
      MAX_COLLATERALS step.selectedIndex :=
  msbClearTrace_highestAfterPrefix htrace hsplit

theorem repaidMaxStep_msbTraceAfterClearingBefore
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited)
    (hsplit :
      visited =
        step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices) :
    MsbClearTrace
      (clearBitmapBits step.activeCollateralBitmap step.beforeSelectedIndices)
      (step.selectedIndex :: step.afterSelectedIndices) :=
  msbClearTrace_clearPrefix htrace hsplit

theorem repaidMaxStep_slotsOfMsbTrace
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    slotsOfIndices step.activeCollateralArrays visited =
      slotsOfIndices step.activeCollateralArrays
        (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices) := by
  rw [repaidMaxStep_msbTraceEqSchedule p s step htrace]

theorem repaidMaxStep_loopMaxDebtFromMsbTrace
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    s.maxDebt =
      sumMaxDebtContributions
        (slotsOfIndices step.activeCollateralArrays visited) := by
  rw [repaidMaxStep_slotsOfMsbTrace p s step htrace]
  exact step.maxDebtEqBitmapSchedule

theorem repaidMaxStep_runMaxDebtFromMsbTrace
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    (run step.selectedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
        (slotsOfIndices step.activeCollateralArrays visited)).maxDebt =
      s.maxDebt := by
  rw [run_maxDebt_from_zero_eq]
  exact (repaidMaxStep_loopMaxDebtFromMsbTrace p s step htrace).symm

def projectedPostState (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    HealthState :=
  liquidateWithDecrease s (maxRepaid p s)
    (maxDebtDecreaseFromSeizure step.collateral
      (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) step.price)
      step.price p.lltv)

def projectedLocalPostState
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    HealthState :=
  liquidateWithDecrease s branch.repaidUnits
    (maxDebtDecreaseFromSeizure branch.loopStep.collateral
      branch.seizedAssets
      branch.loopStep.price p.lltv)

def projectedAfterBadDebtLocalPostState
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    HealthState :=
  liquidateWithDecrease s branch.repaidUnits
    (maxDebtDecreaseFromSeizure branch.loopStep.collateral
      branch.seizedAssets
      branch.loopStep.price p.lltv)

theorem repaidMaxStep_passesRcfGuard
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    (rcfThreshold : Nat) :
    rcfAllows (maxRepaid p s) (maxRepaid p s)
      (collateralRepayCapacity step.collateral step.price p.lif) rcfThreshold :=
  rcfAllows_maxRepaid_with_collateral_capacity
    (maxRepaid p s) step.collateral step.price p.lif rcfThreshold

theorem repaidMaxTraceStep_passesRcfGuard
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s)
    (rcfThreshold : Nat) :
    rcfAllows (maxRepaid p s) (maxRepaid p s)
      (collateralRepayCapacity trace.collateral trace.price p.lif) rcfThreshold :=
  repaidMaxStep_passesRcfGuard p s (trace.toRepaidMaxStep p s) rcfThreshold

theorem repaidMaxLoopStep_passesRcfGuard
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s)
    (rcfThreshold : Nat) :
    rcfAllows (maxRepaid p s) (maxRepaid p s)
      (collateralRepayCapacity loopStep.collateral loopStep.price p.lif) rcfThreshold :=
  repaidMaxTraceStep_passesRcfGuard p s
    (loopStep.toRepaidMaxTraceStep p s) rcfThreshold

theorem repaidMaxAfterBadDebtLoopStep_passesRcfGuard
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s)
    (rcfThreshold : Nat) :
    rcfAllows (maxRepaid p s) (maxRepaid p s)
      (collateralRepayCapacity loopStep.collateral loopStep.price p.lif) rcfThreshold :=
  repaidMaxTraceStep_passesRcfGuard p s
    (loopStep.toRepaidMaxTraceStep p s) rcfThreshold

theorem repaidMaxLocalStep_repaidUnitsEqFormula
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.repaidUnits = maxRepaid p s := by
  rw [branch.repaidUnitsEqMaxRepaidLocal, branch.maxRepaidLocalEq]

theorem repaidMaxAfterBadDebtLoopStep_currentDebtEq
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    s.debt = loopStep.originalDebt - loopStep.badDebt :=
  loopStep.currentDebtEq

theorem repaidMaxAfterBadDebtLoopStep_badDebtEq
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    loopStep.loop.badDebt = loopStep.badDebt :=
  loopStep.loopBadDebtEq

theorem repaidMaxAfterBadDebtLoopStep_currentDebtLeOriginalDebt
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    s.debt ≤ loopStep.originalDebt := by
  rw [loopStep.currentDebtEq]
  exact Nat.sub_le loopStep.originalDebt loopStep.badDebt

theorem repaidMaxAfterBadDebtLoopStep_currentDebtPositive
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    s.debt > 0 := by
  exact lt_of_le_of_lt (Nat.zero_le s.maxDebt) loopStep.unhealthy

theorem repaidMaxAfterBadDebtLoopStep_originalDebtPositive
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    loopStep.originalDebt > 0 := by
  exact lt_of_lt_of_le
    (repaidMaxAfterBadDebtLoopStep_currentDebtPositive p s loopStep)
    (repaidMaxAfterBadDebtLoopStep_currentDebtLeOriginalDebt p s loopStep)

theorem repaidMaxAfterBadDebtLoopStep_originalDebtExceedsLoopMaxDebt
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    loopStep.originalDebt > loopStep.loop.maxDebt := by
  have hcurrent : s.debt ≤ loopStep.originalDebt :=
    repaidMaxAfterBadDebtLoopStep_currentDebtLeOriginalDebt p s loopStep
  have hmax : loopStep.loop.maxDebt = s.maxDebt := loopStep.loopMaxDebtEq
  have hunhealthy : s.debt > s.maxDebt := loopStep.unhealthy
  omega

theorem repaidMaxAfterBadDebtLocalStep_currentDebtEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.currentDebtLocal = branch.loopStep.originalDebt - branch.loopStep.badDebt := by
  rw [branch.currentDebtLocalEq, branch.loopStep.currentDebtEq]

theorem repaidMaxAfterBadDebtLocalStep_currentDebtPositive
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.currentDebtLocal > 0 := by
  rw [branch.currentDebtLocalEq]
  exact repaidMaxAfterBadDebtLoopStep_currentDebtPositive p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_originalDebtPositive
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.loopStep.originalDebt > 0 :=
  repaidMaxAfterBadDebtLoopStep_originalDebtPositive p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_inputRepaidUnitsEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.inputRepaidUnitsLocal = branch.repaidUnits :=
  branch.inputRepaidUnitsLocalEq

theorem repaidMaxAfterBadDebtLocalStep_inputSeizedAssetsEqZero
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.inputSeizedAssetsLocal = 0 :=
  branch.inputSeizedAssetsLocalEq

theorem repaidMaxAfterBadDebtLocalStep_inputAtMostOneNonZero
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    atMostOneNonZeroInput branch.inputRepaidUnitsLocal
      branch.inputSeizedAssetsLocal :=
  Or.inr branch.inputSeizedAssetsLocalEq

theorem repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits = maxRepaid p s := by
  rw [branch.repaidUnitsEqMaxRepaidLocal, branch.maxRepaidLocalEq]

theorem repaidMaxAfterBadDebtLocalStep_inputRepaidUnitsEqFormula
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.inputRepaidUnitsLocal = maxRepaid p s := by
  rw [branch.inputRepaidUnitsLocalEq,
    repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula p s branch]

theorem repaidMaxAfterBadDebtLoopStep_maxRepaidEqSolidityFormula
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    maxRepaid p s =
      mulDivUp (s.debt - s.maxDebt) scale (denominator p) :=
  maxRepaid_eq_solidity_formula p s loopStep.lltvValid loopStep.coeffValid

theorem repaidMaxAfterBadDebtLoopStep_denominatorPositive
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    denominator p > 0 :=
  denominator_pos_of_coeffValid p loopStep.coeffValid

theorem repaidMaxAfterBadDebtLocalStep_maxRepaidLocalEqSolidityFormula
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.maxRepaidLocal =
      mulDivUp
        (branch.currentDebtLocal - branch.loopStep.loop.maxDebt)
        scale (denominator p) := by
  rw [branch.maxRepaidLocalEq]
  rw [repaidMaxAfterBadDebtLoopStep_maxRepaidEqSolidityFormula p s branch.loopStep]
  rw [branch.currentDebtLocalEq, branch.loopStep.loopMaxDebtEq]

theorem repaidMaxAfterBadDebtLocalStep_denominatorPositive
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    denominator p > 0 :=
  repaidMaxAfterBadDebtLoopStep_denominatorPositive p s branch.loopStep

theorem repaidMaxAfterBadDebtLoopStep_maxRepaidPositive
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    maxRepaid p s > 0 :=
  maxRepaid_pos_of_unhealthy p s loopStep.unhealthy loopStep.lltvValid
    loopStep.coeffValid

theorem repaidMaxAfterBadDebtLocalStep_repaidUnitsPositive
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits > 0 := by
  rw [repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula p s branch]
  exact repaidMaxAfterBadDebtLoopStep_maxRepaidPositive p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_repayBranchActive
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits > 0 ∨ branch.seizedAssets > 0 :=
  Or.inl (repaidMaxAfterBadDebtLocalStep_repaidUnitsPositive p s branch)

theorem repaidMaxLocalStep_passesRcfGuard
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s)
    (rcfThreshold : Nat) :
    rcfAllows branch.repaidUnits branch.maxRepaidLocal
      (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
      rcfThreshold := by
  rw [branch.repaidUnitsEqMaxRepaidLocal]
  exact rcfAllows_maxRepaid branch.maxRepaidLocal
    (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
    rcfThreshold

theorem repaidMaxLocalStep_passesRcfGuardZeroFloor
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s)
    (rcfThreshold : Nat) :
    rcfAllowsZeroFloor branch.repaidUnits branch.maxRepaidLocal
      (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
      rcfThreshold := by
  rw [branch.repaidUnitsEqMaxRepaidLocal]
  exact rcfAllowsZeroFloor_maxRepaid branch.maxRepaidLocal
    (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
    rcfThreshold

theorem repaidMaxAfterBadDebtLocalStep_passesRcfGuard
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s)
    (rcfThreshold : Nat) :
    rcfAllows branch.repaidUnits branch.maxRepaidLocal
      (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
      rcfThreshold := by
  rw [branch.repaidUnitsEqMaxRepaidLocal]
  exact rcfAllows_maxRepaid branch.maxRepaidLocal
    (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
    rcfThreshold

theorem repaidMaxAfterBadDebtLocalStep_passesRcfGuardZeroFloor
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s)
    (rcfThreshold : Nat) :
    rcfAllowsZeroFloor branch.repaidUnits branch.maxRepaidLocal
      (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
      rcfThreshold := by
  rw [branch.repaidUnitsEqMaxRepaidLocal]
  exact rcfAllowsZeroFloor_maxRepaid branch.maxRepaidLocal
    (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
    rcfThreshold

theorem normalModeRcfLocalStep_amountBranch
    (p : RCFParams) (s : HealthState)
    (branch : NormalModeRcfLocalStep p s) :
    (branch.repaidUnits, branch.seizedAssets) =
      repaySeizeAmounts branch.inputRepaidUnitsLocal
        branch.inputSeizedAssetsLocal p.lif branch.loopStep.price :=
  branch.amountBranchEq

theorem normalModeRcfLocalStep_rcfAcceptedEq
    (p : RCFParams) (s : HealthState)
    (branch : NormalModeRcfLocalStep p s) :
    branch.rcfAccepted =
      rcfAllowsZeroFloor branch.repaidUnits branch.maxRepaidLocal
        branch.collateralRepayCapacityLocal branch.rcfThresholdLocal :=
  branch.rcfAcceptedEq

theorem repaidMaxAfterBadDebtLocalStep_generalRcfAccepted
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s)
    (rcfThreshold : Nat) :
    (branch.toNormalModeRcfLocalStep rcfThreshold).rcfAccepted := by
  change rcfAllowsZeroFloor branch.repaidUnits branch.maxRepaidLocal
    (collateralRepayCapacity branch.loopStep.collateral branch.loopStep.price p.lif)
    rcfThreshold
  exact repaidMaxAfterBadDebtLocalStep_passesRcfGuardZeroFloor p s branch
    rcfThreshold

theorem repaidMaxLocalStep_seizedAssetsEqFormula
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.seizedAssets =
      seizedAssetsFromRepayValue (mulDivDown branch.repaidUnits p.lif WAD)
        branch.loopStep.price :=
  branch.seizedAssetsEq

theorem repaidMaxAfterBadDebtLocalStep_seizedAssetsEqFormula
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.seizedAssets =
      seizedAssetsFromRepayValue (mulDivDown branch.repaidUnits p.lif WAD)
        branch.loopStep.price :=
  branch.seizedAssetsEq

theorem repaidMaxAfterBadDebtLocalStep_postWithdrawableEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postWithdrawableLocal =
      branch.withdrawableLocal + branch.repaidUnits :=
  branch.postWithdrawableLocalEq

theorem repaidMaxAfterBadDebtLocalStep_withdrawableIncreases
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.withdrawableLocal ≤ branch.postWithdrawableLocal := by
  rw [branch.postWithdrawableLocalEq]
  exact Nat.le_add_right branch.withdrawableLocal branch.repaidUnits

theorem repaidMaxLocalStep_postDebtEq
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.postDebtLocal = s.debt - branch.repaidUnits :=
  branch.postDebtLocalEq

theorem repaidMaxAfterBadDebtLocalStep_postDebtEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postDebtLocal = s.debt - branch.repaidUnits := by
  rw [branch.postDebtLocalEq, branch.currentDebtLocalEq]

theorem repaidMaxLocalStep_postCollateralEq
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.postCollateralLocal = branch.loopStep.collateral - branch.seizedAssets :=
  branch.postCollateralLocalEq

theorem repaidMaxAfterBadDebtLocalStep_postCollateralEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postCollateralLocal =
      branch.loopStep.collateral - branch.seizedAssets :=
  branch.postCollateralLocalEq

theorem repaidMaxAfterBadDebtLocalStep_postCollateralBitmapEq
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postCollateralBitmapLocal =
      collateralBitmapAfterSeizure branch.collateralBitmapLocal
        branch.loopStep.selectedIndex branch.postCollateralLocal
        branch.seizedAssets :=
  branch.postCollateralBitmapLocalEq

theorem repaidMaxAfterBadDebtLocalStep_postCollateralBitmapCleared
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s)
    (hempty : branch.postCollateralLocal = 0)
    (hseized : branch.seizedAssets > 0) :
    branch.postCollateralBitmapLocal =
      clearBitmapBit branch.collateralBitmapLocal branch.loopStep.selectedIndex := by
  rw [branch.postCollateralBitmapLocalEq]
  simp [collateralBitmapAfterSeizure, hempty, hseized]

theorem repaidMaxAfterBadDebtLocalStep_postCollateralBitmapUnchanged
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s)
    (hkeep : ¬ (branch.postCollateralLocal = 0 ∧ branch.seizedAssets > 0)) :
    branch.postCollateralBitmapLocal = branch.collateralBitmapLocal := by
  rw [branch.postCollateralBitmapLocalEq]
  simp [collateralBitmapAfterSeizure, hkeep]

theorem repaidMaxLoopStep_priceEqSelected
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    loopStep.price =
      loopStep.loop.activeCollateralArrays.price loopStep.selectedIndex :=
  (loopStep.toRepaidMaxTraceStep p s).priceEqSelected

theorem repaidMaxAfterBadDebtLoopStep_priceEqSelected
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    loopStep.price =
      loopStep.loop.activeCollateralArrays.price loopStep.selectedIndex :=
  (loopStep.toRepaidMaxTraceStep p s).priceEqSelected

theorem repaidMaxLoopStep_lifEqSelectedMaxLif
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    p.lif =
      loopStep.loop.activeCollateralArrays.maxLif loopStep.selectedIndex :=
  loopStep.lifEqSelectedMaxLif

theorem repaidMaxAfterBadDebtLoopStep_lifEqSelectedMaxLif
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    p.lif =
      loopStep.loop.activeCollateralArrays.maxLif loopStep.selectedIndex :=
  loopStep.lifEqSelectedMaxLif

theorem repaidMaxStep_subtractionsSafe
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    maxRepaid p s ≤ s.debt ∧
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) step.price ≤
        step.collateral :=
  ⟨maxRepaid_le_debt_of_coeff_debt_le_maxDebt p s
      (le_of_lt step.unhealthy) step.lltvValid step.coeffValid
      step.repaidWithinDebtDomain,
    seizedAssetsFromRepayValue_le_collateral_of_repaid_le_capacity
      (maxRepaid p s) step.collateral step.price p.lif
      step.repaidWithinCollateralCapacity⟩

theorem repaidMaxTraceStep_subtractionsSafe
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s) :
    maxRepaid p s ≤ s.debt ∧
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) trace.price ≤
        trace.collateral :=
  repaidMaxStep_subtractionsSafe p s (trace.toRepaidMaxStep p s)

theorem repaidMaxLoopStep_subtractionsSafe
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    maxRepaid p s ≤ s.debt ∧
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) loopStep.price ≤
        loopStep.collateral :=
  repaidMaxTraceStep_subtractionsSafe p s
    (loopStep.toRepaidMaxTraceStep p s)

theorem repaidMaxAfterBadDebtLoopStep_subtractionsSafe
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    maxRepaid p s ≤ s.debt ∧
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD)
          loopStep.price ≤
        loopStep.collateral :=
  repaidMaxTraceStep_subtractionsSafe p s
    (loopStep.toRepaidMaxTraceStep p s)

theorem repaidMaxStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    maxRepaid p s ≤ s.debt :=
  (repaidMaxStep_subtractionsSafe p s step).1

theorem repaidMaxStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) step.price ≤
      step.collateral :=
  (repaidMaxStep_subtractionsSafe p s step).2

theorem repaidMaxTraceStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s) :
    maxRepaid p s ≤ s.debt :=
  (repaidMaxTraceStep_subtractionsSafe p s trace).1

theorem repaidMaxTraceStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s) :
    seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) trace.price ≤
      trace.collateral :=
  (repaidMaxTraceStep_subtractionsSafe p s trace).2

theorem repaidMaxLoopStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    maxRepaid p s ≤ s.debt :=
  (repaidMaxLoopStep_subtractionsSafe p s loopStep).1

theorem repaidMaxAfterBadDebtLoopStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    maxRepaid p s ≤ s.debt :=
  (repaidMaxAfterBadDebtLoopStep_subtractionsSafe p s loopStep).1

theorem repaidMaxLoopStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) loopStep.price ≤
      loopStep.collateral :=
  (repaidMaxLoopStep_subtractionsSafe p s loopStep).2

theorem repaidMaxAfterBadDebtLoopStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD)
        loopStep.price ≤
      loopStep.collateral :=
  (repaidMaxAfterBadDebtLoopStep_subtractionsSafe p s loopStep).2

theorem repaidMaxLocalStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.repaidUnits ≤ s.debt := by
  rw [repaidMaxLocalStep_repaidUnitsEqFormula p s branch]
  exact repaidMaxLoopStep_repaidWithinDebt p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_repaidWithinDebt
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits ≤ branch.currentDebtLocal := by
  rw [branch.currentDebtLocalEq,
    repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula p s branch]
  exact repaidMaxAfterBadDebtLoopStep_repaidWithinDebt p s branch.loopStep

theorem repaidMaxLocalStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.seizedAssets ≤ branch.loopStep.collateral := by
  rw [branch.seizedAssetsEq]
  rw [repaidMaxLocalStep_repaidUnitsEqFormula p s branch]
  exact repaidMaxLoopStep_seizedWithinCollateral p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_seizedWithinCollateral
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.seizedAssets ≤ branch.loopStep.collateral := by
  rw [branch.seizedAssetsEq]
  rw [repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula p s branch]
  exact repaidMaxAfterBadDebtLoopStep_seizedWithinCollateral p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_repaidUnitsUint128
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits ≤ UINT128_MAX := by
  have hrepay := repaidMaxAfterBadDebtLocalStep_repaidWithinDebt p s branch
  exact le_trans hrepay branch.currentDebtLocalUint128

theorem repaidMaxAfterBadDebtLocalStep_seizedAssetsUint128
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.seizedAssets ≤ UINT128_MAX := by
  have hseized := repaidMaxAfterBadDebtLocalStep_seizedWithinCollateral p s branch
  exact le_trans hseized branch.collateralLocalUint128

theorem repaidMaxAfterBadDebtLocalStep_postDebtUint128
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postDebtLocal ≤ UINT128_MAX := by
  rw [branch.postDebtLocalEq]
  exact le_trans (Nat.sub_le branch.currentDebtLocal branch.repaidUnits)
    branch.currentDebtLocalUint128

theorem repaidMaxAfterBadDebtLocalStep_postCollateralUint128
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postCollateralLocal ≤ UINT128_MAX := by
  rw [branch.postCollateralLocalEq]
  exact le_trans (Nat.sub_le branch.loopStep.collateral branch.seizedAssets)
    branch.collateralLocalUint128

theorem repaidMaxAfterBadDebtLocalStep_postCollateralBitmapUint128
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postCollateralBitmapLocal ≤ UINT128_MAX :=
  branch.postCollateralBitmapLocalUint128

theorem repaidMaxLocalStep_subtractionsSafe
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.repaidUnits ≤ s.debt ∧
      branch.seizedAssets ≤ branch.loopStep.collateral :=
  ⟨repaidMaxLocalStep_repaidWithinDebt p s branch,
    repaidMaxLocalStep_seizedWithinCollateral p s branch⟩

theorem repaidMaxAfterBadDebtLocalStep_subtractionsSafe
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.repaidUnits ≤ branch.currentDebtLocal ∧
      branch.seizedAssets ≤ branch.loopStep.collateral :=
  ⟨repaidMaxAfterBadDebtLocalStep_repaidWithinDebt p s branch,
    repaidMaxAfterBadDebtLocalStep_seizedWithinCollateral p s branch⟩

theorem repaidMaxStep_activeSlotsBound
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    (slotsOfIndices step.activeCollateralArrays
        (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices)).length ≤
      MAX_COLLATERALS_PER_BORROWER :=
  validSchedule_slots_length_le_16 step.activeCollateralArrays
    (repaidMaxStep_activeScheduleValid p s step)

theorem repaidMaxStep_activeIndicesNodup
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices).Nodup :=
  validSchedule_nodup (repaidMaxStep_activeScheduleValid p s step)

theorem repaidMaxStep_selectedIndexLt128
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    step.selectedIndex < MAX_COLLATERALS :=
  validSchedule_index_lt_128 (repaidMaxStep_activeScheduleValid p s step) (by simp)

theorem repaidMaxStep_activeSlotIndicesNodup
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    (slotIndicesOf
      (slotsOfIndices step.activeCollateralArrays
        (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices))).Nodup :=
  validSchedule_slotIndices_nodup step.activeCollateralArrays
    (repaidMaxStep_activeScheduleValid p s step)

theorem repaidMaxStep_activeSlotIndexLt128
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {slot : CollateralSlot}
    (hmem : slot ∈ slotsOfIndices step.activeCollateralArrays
      (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices)) :
    slot.index < MAX_COLLATERALS :=
  validSchedule_slotIndex_lt_128 step.activeCollateralArrays
    (repaidMaxStep_activeScheduleValid p s step) hmem

theorem repaidMaxStep_selectedPriceFromSchedule
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    selectedPriceAfter step.selectedIndex 0
        (slotsOfIndices step.activeCollateralArrays
          (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices)) =
      step.price := by
  calc
    selectedPriceAfter step.selectedIndex 0
        (slotsOfIndices step.activeCollateralArrays
          (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices))
        = step.activeCollateralArrays.price step.selectedIndex :=
          selectedPriceAfter_split_selectedIndex step.activeCollateralArrays
            step.beforeSelectedIndices step.afterSelectedIndices step.selectedIndex 0
            (validSchedule_nodup (repaidMaxStep_activeScheduleValid p s step))
    _ = step.price := by
          rw [step.priceEqSelected]

theorem repaidMaxStep_selectedPriceFromCollateralLoop
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    (run step.selectedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
        (slotsOfIndices step.activeCollateralArrays
          (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices))).liquidatedCollatPrice =
      step.price := by
  rw [run_liquidatedCollatPrice_from_zero_eq]
  exact repaidMaxStep_selectedPriceFromSchedule p s step

theorem repaidMaxStep_selectedPriceFromMsbTrace
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    (run step.selectedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := s.debt }
        (slotsOfIndices step.activeCollateralArrays visited)).liquidatedCollatPrice =
      step.price := by
  rw [repaidMaxStep_slotsOfMsbTrace p s step htrace]
  exact repaidMaxStep_selectedPriceFromCollateralLoop p s step

theorem repaidMaxStep_maxDebtEqCollateralLoop
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    s.maxDebt =
      maxDebtAfterCollateralLoop
        (collateralMaxDebt step.collateral step.price p.lltv)
        (maxDebtContributionsOf (slotsOfIndices step.activeCollateralArrays step.beforeSelectedIndices) ++
          maxDebtContributionsOf (slotsOfIndices step.activeCollateralArrays step.afterSelectedIndices)) := by
  calc
    s.maxDebt =
        sumMaxDebtContributions
          (slotsOfIndices step.activeCollateralArrays
            (step.beforeSelectedIndices ++ step.selectedIndex :: step.afterSelectedIndices)) :=
          step.maxDebtEqBitmapSchedule
    _ =
        maxDebtAfterCollateralLoop
          (maxDebtContribution (slotAt step.activeCollateralArrays step.selectedIndex))
          (maxDebtContributionsOf
              (slotsOfIndices step.activeCollateralArrays step.beforeSelectedIndices) ++
            maxDebtContributionsOf
              (slotsOfIndices step.activeCollateralArrays step.afterSelectedIndices)) :=
          sumMaxDebtContributions_split_selectedIndex step.activeCollateralArrays
            step.beforeSelectedIndices step.afterSelectedIndices step.selectedIndex
    _ =
        maxDebtAfterCollateralLoop
          (collateralMaxDebt step.collateral step.price p.lltv)
          (maxDebtContributionsOf
              (slotsOfIndices step.activeCollateralArrays step.beforeSelectedIndices) ++
            maxDebtContributionsOf
              (slotsOfIndices step.activeCollateralArrays step.afterSelectedIndices)) := by
          congr 1
          simp [maxDebtContribution, slotAt, collateralMaxDebt,
            step.collateralEqSelected, step.priceEqSelected, step.lltvEqSelected]

/-- Refinement payoff for RCF: once generated-body extraction supplies
    `RepaidMaxStep`, the projected post-state is healthy within the proved
    one-unit rounding slack. -/
theorem repaidMaxStep_restoresWithinOne
    (p : RCFParams) (s : HealthState) (step : RepaidMaxStep p s) :
    healthyWithin 1 (projectedPostState p s step) := by
  unfold projectedPostState
  exact rcf_maxRepaid_restores_unhealthy_with_one_slack_of_collateral_seizure
    p s step.collateral step.price step.unhealthy step.lltvValid step.coeffValid
    (maxDebtDecreaseFromSeizure_le_loop step.collateral
      (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) step.price)
      step.price p.lltv
      (maxDebtContributionsOf (slotsOfIndices step.activeCollateralArrays step.beforeSelectedIndices) ++
        maxDebtContributionsOf (slotsOfIndices step.activeCollateralArrays step.afterSelectedIndices))
      s.maxDebt
      (repaidMaxStep_maxDebtEqCollateralLoop p s step))
    (repaidMaxStep_seizedWithinCollateral p s step)

theorem repaidMaxTraceStep_restoresWithinOne
    (p : RCFParams) (s : HealthState) (trace : RepaidMaxTraceStep p s) :
    healthyWithin 1
      (projectedPostState p s (trace.toRepaidMaxStep p s)) :=
  repaidMaxStep_restoresWithinOne p s (trace.toRepaidMaxStep p s)

theorem repaidMaxLoopStep_restoresWithinOne
    (p : RCFParams) (s : HealthState) (loopStep : RepaidMaxLoopStep p s) :
    healthyWithin 1
      (projectedPostState p s
        ((loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s)) :=
  repaidMaxTraceStep_restoresWithinOne p s
    (loopStep.toRepaidMaxTraceStep p s)

theorem repaidMaxAfterBadDebtLoopStep_restoresWithinOne
    (p : RCFParams) (s : HealthState)
    (loopStep : RepaidMaxAfterBadDebtLoopStep p s) :
    healthyWithin 1
      (projectedPostState p s
        ((loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s)) :=
  repaidMaxTraceStep_restoresWithinOne p s
    (loopStep.toRepaidMaxTraceStep p s)

theorem repaidMaxLocalStep_restoresWithinOne
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    healthyWithin 1
      (projectedPostState p s
        (((branch.loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s))) :=
  repaidMaxLoopStep_restoresWithinOne p s branch.loopStep

theorem repaidMaxAfterBadDebtLocalStep_restoresWithinOne
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    healthyWithin 1
      (projectedPostState p s
        (((branch.loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s))) :=
  repaidMaxAfterBadDebtLoopStep_restoresWithinOne p s branch.loopStep

theorem repaidMaxLocalStep_restoresLocalPostWithinOne
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    healthyWithin 1 (projectedLocalPostState p s branch) := by
  unfold projectedLocalPostState
  rw [branch.seizedAssetsEq]
  rw [repaidMaxLocalStep_repaidUnitsEqFormula p s branch]
  change healthyWithin 1
    (projectedPostState p s
      (((branch.loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s)))
  exact repaidMaxLocalStep_restoresWithinOne p s branch

theorem repaidMaxAfterBadDebtLocalStep_restoresLocalPostWithinOne
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    healthyWithin 1 (projectedAfterBadDebtLocalPostState p s branch) := by
  unfold projectedAfterBadDebtLocalPostState
  rw [branch.seizedAssetsEq]
  rw [repaidMaxAfterBadDebtLocalStep_repaidUnitsEqFormula p s branch]
  change healthyWithin 1
    (projectedPostState p s
      (((branch.loopStep.toRepaidMaxTraceStep p s).toRepaidMaxStep p s)))
  exact repaidMaxAfterBadDebtLocalStep_restoresWithinOne p s branch

theorem repaidMaxLocalStep_postDebtEqProjected
    (p : RCFParams) (s : HealthState) (branch : RepaidMaxLocalStep p s) :
    branch.postDebtLocal = (projectedLocalPostState p s branch).debt := by
  unfold projectedLocalPostState
  exact branch.postDebtLocalEq

theorem repaidMaxAfterBadDebtLocalStep_postDebtEqProjected
    (p : RCFParams) (s : HealthState)
    (branch : RepaidMaxAfterBadDebtLocalStep p s) :
    branch.postDebtLocal =
      (projectedAfterBadDebtLocalPostState p s branch).debt := by
  unfold projectedAfterBadDebtLocalPostState
  rw [branch.postDebtLocalEq, branch.currentDebtLocalEq]
  simp [liquidateWithDecrease]

end RCF

namespace Accounting

open Midnight.Proofs.Basic
open Midnight.Proofs.UnitsAccounting
open Midnight.Proofs.CollateralLoop
open Midnight.Proofs.BitmapSchedule
open Midnight.Proofs.MarketLedger

/-- Generated-body obligations for the bad-debt branch of `liquidate`: the branch
    starts `badDebt` from the borrower's original debt, decreases it through the
    collateral loop, then uses Midnight's exact loss-factor formula and decreases
    `totalUnits` by `badDebt`; lenders in scope are synced to the old loss
    factor. -/
structure BadDebtStep (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender) where
  originalDebt : Nat
  liquidatedIndex : Nat
  activeCollateralArrays : CollateralArrays
  activeCollateralBitmap : Nat
  activeCollateralIndices : List Nat
  activeBitmapValid : ValidCollateralBitmap activeCollateralBitmap
  activeScheduleEq : activeCollateralIndices = bitmapSchedule activeCollateralBitmap
  oldLossFactorNotMax : oldLossFactor < UINT128_MAX
  badDebtEqCollateralLoop :
    badDebt =
      (run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt }
        (slotsOfIndices activeCollateralArrays activeCollateralIndices)).badDebt
  marketDebts : List Nat
  marketDebtsBefore : List Nat
  marketDebtsAfter : List Nat
  marketDebtsEqSplit : marketDebts = marketDebtsBefore ++ originalDebt :: marketDebtsAfter
  marketDebtsCovered : DebtsCovered oldTotalUnits marketDebts
  storedLenders : List Lender
  lendersEqStoredUpdates :
    lenders =
      storedLenders.map
        (updateLender { totalUnits := oldTotalUnits, lossFactor := oldLossFactor })
  storedCover :
    totalUnitsCoversCredits
      { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } storedLenders

/-- Contract-shaped bad-debt obligations from the concrete bitmap loop trace.
    This matches extraction that records the indices visited by Solidity's
    repeated `msb` / `clearBit` loop and the resulting `badDebt` accumulator. -/
structure BadDebtTraceStep
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender) where
  originalDebt : Nat
  liquidatedIndex : Nat
  activeCollateralArrays : CollateralArrays
  activeCollateralBitmap : Nat
  activeVisited : List Nat
  activeBitmapValid : ValidCollateralBitmap activeCollateralBitmap
  activeTrace : MsbClearTrace activeCollateralBitmap activeVisited
  oldLossFactorNotMax : oldLossFactor < UINT128_MAX
  badDebtEqTraceLoop :
    badDebt =
      (run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt }
        (slotsOfIndices activeCollateralArrays activeVisited)).badDebt
  marketDebts : List Nat
  marketDebtsBefore : List Nat
  marketDebtsAfter : List Nat
  marketDebtsEqSplit : marketDebts = marketDebtsBefore ++ originalDebt :: marketDebtsAfter
  marketDebtsCovered : DebtsCovered oldTotalUnits marketDebts
  storedLenders : List Lender
  lendersEqStoredUpdates :
    lenders =
      storedLenders.map
        (updateLender { totalUnits := oldTotalUnits, lossFactor := oldLossFactor })
  storedCover :
    totalUnitsCoversCredits
      { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } storedLenders

/-- Bad-debt branch obligations when extraction provides the shared Solidity
    loop locals.  The branch-specific facts are that the loop started from the
    borrower's original debt and the final Solidity `badDebt` local is the one
    used in the market loss-factor / total-units update. -/
structure BadDebtLoopStep
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender) where
  originalDebt : Nat
  loop : LiquidateLoopTrace
  loopInitialDebtEq : loop.initialDebt = originalDebt
  loopBadDebtEq : loop.badDebt = badDebt
  oldLossFactorNotMax : oldLossFactor < UINT128_MAX
  marketDebts : List Nat
  marketDebtsBefore : List Nat
  marketDebtsAfter : List Nat
  marketDebtsEqSplit : marketDebts = marketDebtsBefore ++ originalDebt :: marketDebtsAfter
  marketDebtsCovered : DebtsCovered oldTotalUnits marketDebts
  storedLenders : List Lender
  lendersEqStoredUpdates :
    lenders =
      storedLenders.map
        (updateLender { totalUnits := oldTotalUnits, lossFactor := oldLossFactor })
  storedCover :
    totalUnitsCoversCredits
      { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } storedLenders

/-- Solidity-local bad-debt branch values: the branch snapshots `_totalUnits`
    and `_lossFactor`, then writes the updated market `totalUnits` and
    `lossFactor` using the computed `badDebt` local. -/
structure BadDebtLocalStep (lenders : List Lender) where
  oldTotalUnits : Nat
  badDebt : Nat
  oldLossFactor : Nat
  loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders
  totalUnitsLocal : Nat
  lossFactorLocal : Nat
  continuousFeeCreditLocal : Nat
  postDebtLocal : Nat
  newTotalUnitsLocal : Nat
  newLossFactorLocal : Nat
  newContinuousFeeCreditLocal : Nat
  originalDebtUint128 : loopStep.originalDebt ≤ UINT128_MAX
  oldTotalUnitsUint128 : oldTotalUnits ≤ UINT128_MAX
  totalUnitsLocalUint128 : totalUnitsLocal ≤ UINT128_MAX
  lossFactorLocalUint128 : lossFactorLocal ≤ UINT128_MAX
  continuousFeeCreditLocalUint128 : continuousFeeCreditLocal ≤ UINT128_MAX
  totalUnitsLocalEq : totalUnitsLocal = oldTotalUnits
  lossFactorLocalEq : lossFactorLocal = oldLossFactor
  postDebtLocalEq : postDebtLocal = loopStep.originalDebt - badDebt
  newTotalUnitsLocalEq : newTotalUnitsLocal = oldTotalUnits - badDebt
  newLossFactorLocalEq :
    newLossFactorLocal = lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor
  newContinuousFeeCreditLocalEq :
    newContinuousFeeCreditLocal =
      continuousFeeCreditAfterBadDebt continuousFeeCreditLocal lossFactorLocal
        newLossFactorLocal

def BadDebtLoopStep.toBadDebtTraceStep
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders :=
  { originalDebt := loopStep.originalDebt,
    liquidatedIndex := loopStep.loop.liquidatedIndex,
    activeCollateralArrays := loopStep.loop.activeCollateralArrays,
    activeCollateralBitmap := loopStep.loop.activeCollateralBitmap,
    activeVisited := loopStep.loop.activeVisited,
    activeBitmapValid := loopStep.loop.activeBitmapValid,
    activeTrace := loopStep.loop.activeTrace,
    oldLossFactorNotMax := loopStep.oldLossFactorNotMax,
    badDebtEqTraceLoop := by
      calc
        badDebt = loopStep.loop.badDebt := loopStep.loopBadDebtEq.symm
        _ =
            (run loopStep.loop.liquidatedIndex
              { maxDebt := 0,
                liquidatedCollatPrice := 0,
                badDebt := loopStep.loop.initialDebt }
              (slotsOfIndices loopStep.loop.activeCollateralArrays
                loopStep.loop.activeVisited)).badDebt :=
            (liquidateLoopTrace_badDebtEq loopStep.loop).symm
        _ =
            (run loopStep.loop.liquidatedIndex
              { maxDebt := 0,
                liquidatedCollatPrice := 0,
                badDebt := loopStep.originalDebt }
              (slotsOfIndices loopStep.loop.activeCollateralArrays
                loopStep.loop.activeVisited)).badDebt := by
            rw [loopStep.loopInitialDebtEq],
    marketDebts := loopStep.marketDebts,
    marketDebtsBefore := loopStep.marketDebtsBefore,
    marketDebtsAfter := loopStep.marketDebtsAfter,
    marketDebtsEqSplit := loopStep.marketDebtsEqSplit,
    marketDebtsCovered := loopStep.marketDebtsCovered,
    storedLenders := loopStep.storedLenders,
    lendersEqStoredUpdates := loopStep.lendersEqStoredUpdates,
    storedCover := loopStep.storedCover }

def BadDebtTraceStep.toBadDebtStep
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    BadDebtStep oldTotalUnits badDebt oldLossFactor lenders :=
  { originalDebt := trace.originalDebt,
    liquidatedIndex := trace.liquidatedIndex,
    activeCollateralArrays := trace.activeCollateralArrays,
    activeCollateralBitmap := trace.activeCollateralBitmap,
    activeCollateralIndices := trace.activeVisited,
    activeBitmapValid := trace.activeBitmapValid,
    activeScheduleEq := msbClearTrace_eq_bitmapSchedule trace.activeTrace,
    oldLossFactorNotMax := trace.oldLossFactorNotMax,
    badDebtEqCollateralLoop := trace.badDebtEqTraceLoop,
    marketDebts := trace.marketDebts,
    marketDebtsBefore := trace.marketDebtsBefore,
    marketDebtsAfter := trace.marketDebtsAfter,
    marketDebtsEqSplit := trace.marketDebtsEqSplit,
    marketDebtsCovered := trace.marketDebtsCovered,
    storedLenders := trace.storedLenders,
    lendersEqStoredUpdates := trace.lendersEqStoredUpdates,
    storedCover := trace.storedCover }

theorem badDebtStep_activeScheduleValid
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    ValidSchedule step.activeCollateralIndices := by
  rw [step.activeScheduleEq]
  exact validCollateralBitmap_valid step.activeBitmapValid

theorem badDebtStep_activeBitmapUint128
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    step.activeCollateralBitmap < UINT128_BOUND :=
  step.activeBitmapValid.1

theorem badDebtStep_activeBitmapCountBound
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    countBits step.activeCollateralBitmap ≤ MAX_COLLATERALS_PER_BORROWER :=
  step.activeBitmapValid.2

theorem badDebtStep_activeScheduleLengthEqCountBits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    (bitmapSchedule step.activeCollateralBitmap).length =
      countBits step.activeCollateralBitmap :=
  bitmapSchedule_length_eq_countBits step.activeCollateralBitmap

theorem badDebtStep_activeIndexBitSet
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {index : Nat} (hmem : index ∈ step.activeCollateralIndices) :
    step.activeCollateralBitmap.testBit index = true := by
  rw [step.activeScheduleEq] at hmem
  exact bitmapSchedule_mem_testBit hmem

theorem badDebtStep_scheduleAfterClearingPrefix
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {pref suffix : List Nat}
    (hsplit : step.activeCollateralIndices = pref ++ suffix) :
    bitmapSchedule (clearBitmapBits step.activeCollateralBitmap pref) = suffix := by
  have hschedule :
      bitmapSchedule step.activeCollateralBitmap = pref ++ suffix := by
    rw [← step.activeScheduleEq]
    exact hsplit
  exact bitmapSchedule_clearBitmapBits_prefix_eq_suffix pref suffix hschedule

theorem badDebtStep_highestAfterClearingPrefix
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {pref rest : List Nat} {index : Nat}
    (hsplit : step.activeCollateralIndices = pref ++ index :: rest) :
    HighestBitmapBit (clearBitmapBits step.activeCollateralBitmap pref)
      MAX_COLLATERALS index :=
  bitmapSchedule_cons_highestBitmapBit
    (badDebtStep_scheduleAfterClearingPrefix oldTotalUnits badDebt oldLossFactor
      lenders step hsplit)

theorem badDebtStep_scheduleAfterClearingPrefixValid
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {pref suffix : List Nat}
    (hsplit : step.activeCollateralIndices = pref ++ suffix) :
    ValidSchedule
      (bitmapSchedule (clearBitmapBits step.activeCollateralBitmap pref)) := by
  have hschedule :
      bitmapSchedule step.activeCollateralBitmap = pref ++ suffix := by
    rw [← step.activeScheduleEq]
    exact hsplit
  exact bitmapSchedule_clearBitmapBits_prefix_valid pref suffix
    step.activeBitmapValid hschedule

theorem badDebtStep_msbTraceEqSchedule
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    visited = step.activeCollateralIndices := by
  rw [msbClearTrace_eq_bitmapSchedule htrace]
  exact step.activeScheduleEq.symm

theorem badDebtStep_msbTraceValid
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    ValidSchedule visited :=
  msbClearTrace_valid step.activeBitmapValid htrace

theorem badDebtStep_msbTraceNodup
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    visited.Nodup :=
  msbClearTrace_nodup step.activeBitmapValid htrace

theorem badDebtStep_msbTraceSlotsBound
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    (slotsOfIndices step.activeCollateralArrays visited).length ≤
      MAX_COLLATERALS_PER_BORROWER :=
  msbClearTrace_slots_length_le_16 step.activeCollateralArrays
    step.activeBitmapValid htrace

theorem badDebtStep_msbTraceAfterClearingPrefix
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited pref suffix : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited)
    (hsplit : visited = pref ++ suffix) :
    MsbClearTrace (clearBitmapBits step.activeCollateralBitmap pref) suffix :=
  msbClearTrace_clearPrefix htrace hsplit

theorem badDebtStep_highestFromMsbTraceSplit
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited pref rest : List Nat} {index : Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited)
    (hsplit : visited = pref ++ index :: rest) :
    HighestBitmapBit (clearBitmapBits step.activeCollateralBitmap pref)
      MAX_COLLATERALS index :=
  msbClearTrace_highestAfterPrefix htrace hsplit

theorem badDebtStep_slotsOfMsbTrace
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    slotsOfIndices step.activeCollateralArrays visited =
      slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices := by
  rw [badDebtStep_msbTraceEqSchedule oldTotalUnits badDebt oldLossFactor
    lenders step htrace]

theorem badDebtStep_badDebtEqMsbTraceLoop
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    badDebt =
      (run step.liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := step.originalDebt }
        (slotsOfIndices step.activeCollateralArrays visited)).badDebt := by
  rw [badDebtStep_slotsOfMsbTrace oldTotalUnits badDebt oldLossFactor
    lenders step htrace]
  exact step.badDebtEqCollateralLoop

theorem badDebtStep_badDebtLeOriginalDebtFromMsbTrace
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    badDebt ≤ step.originalDebt := by
  calc
    badDebt =
        (run step.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := step.originalDebt }
          (slotsOfIndices step.activeCollateralArrays visited)).badDebt :=
        badDebtStep_badDebtEqMsbTraceLoop oldTotalUnits badDebt oldLossFactor
          lenders step htrace
    _ ≤ step.originalDebt :=
        run_badDebt_le_initial step.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := step.originalDebt }
          (slotsOfIndices step.activeCollateralArrays visited)

theorem badDebtStep_marketDebtsCoveredSplit
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    DebtsCovered oldTotalUnits
      (step.marketDebtsBefore ++ step.originalDebt :: step.marketDebtsAfter) := by
  rw [← step.marketDebtsEqSplit]
  exact step.marketDebtsCovered

def postBadDebtMarket (oldTotalUnits badDebt oldLossFactor : Nat) : Market :=
  { totalUnits := oldTotalUnits - badDebt,
    lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }

def postBadDebtLocalMarket (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    Market :=
  { totalUnits := branch.newTotalUnitsLocal,
    lossFactor := branch.newLossFactorLocal }

def oldBadDebtLocalMarket (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    Market :=
  { totalUnits := branch.totalUnitsLocal,
    lossFactor := branch.lossFactorLocal }

theorem badDebtStep_badDebtLeOriginalDebt
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ step.originalDebt := by
  calc
    badDebt =
        (run step.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := step.originalDebt }
          (slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices)).badDebt :=
        step.badDebtEqCollateralLoop
    _ ≤ step.originalDebt :=
        run_badDebt_le_initial step.liquidatedIndex
          { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := step.originalDebt }
          (slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices)

theorem badDebtStep_activeSlotsBound
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    (slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices).length ≤
      MAX_COLLATERALS_PER_BORROWER :=
  validSchedule_slots_length_le_16 step.activeCollateralArrays
    (badDebtStep_activeScheduleValid oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtStep_activeIndicesNodup
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    step.activeCollateralIndices.Nodup :=
  validSchedule_nodup
    (badDebtStep_activeScheduleValid oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtStep_activeIndexLt128
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {index : Nat} (hmem : index ∈ step.activeCollateralIndices) :
    index < MAX_COLLATERALS :=
  validSchedule_index_lt_128
    (badDebtStep_activeScheduleValid oldTotalUnits badDebt oldLossFactor lenders step) hmem

theorem badDebtStep_activeSlotIndicesNodup
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    (slotIndicesOf
      (slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices)).Nodup :=
  validSchedule_slotIndices_nodup step.activeCollateralArrays
    (badDebtStep_activeScheduleValid oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtStep_activeSlotIndexLt128
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {slot : CollateralSlot}
    (hmem : slot ∈ slotsOfIndices step.activeCollateralArrays step.activeCollateralIndices) :
    slot.index < MAX_COLLATERALS :=
  validSchedule_slotIndex_lt_128 step.activeCollateralArrays
    (badDebtStep_activeScheduleValid oldTotalUnits badDebt oldLossFactor lenders step) hmem

theorem badDebtStep_originalDebtLeTotalUnits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
  step.originalDebt ≤ oldTotalUnits :=
  debt_le_totalUnits_of_mem_covered
    (by simp)
    (badDebtStep_marketDebtsCoveredSplit oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtLoopStep_badDebtLeOriginalDebt
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ loopStep.originalDebt := by
  calc
    badDebt = loopStep.loop.badDebt := loopStep.loopBadDebtEq.symm
    _ ≤ loopStep.loop.initialDebt := liquidateLoopTrace_badDebtLeInitial loopStep.loop
    _ = loopStep.originalDebt := loopStep.loopInitialDebtEq

theorem badDebtLoopStep_originalDebtLeTotalUnits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    loopStep.originalDebt ≤ oldTotalUnits := by
  have hmem :
      loopStep.originalDebt ∈
        loopStep.marketDebtsBefore ++ loopStep.originalDebt :: loopStep.marketDebtsAfter := by
    simp
  have hcovered :
      DebtsCovered oldTotalUnits
        (loopStep.marketDebtsBefore ++ loopStep.originalDebt :: loopStep.marketDebtsAfter) := by
    rw [← loopStep.marketDebtsEqSplit]
    exact loopStep.marketDebtsCovered
  exact debt_le_totalUnits_of_mem_covered hmem hcovered

theorem badDebtLoopStep_badDebtLeTotalUnits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ oldTotalUnits :=
  le_trans
    (badDebtLoopStep_badDebtLeOriginalDebt oldTotalUnits badDebt oldLossFactor
      lenders loopStep)
    (badDebtLoopStep_originalDebtLeTotalUnits oldTotalUnits badDebt oldLossFactor
      lenders loopStep)

def postBadDebtMarketDebts
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) : List Nat :=
  step.marketDebtsBefore ++ (step.originalDebt - badDebt) :: step.marketDebtsAfter

def postBadDebtLocalMarketDebts
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) : List Nat :=
  branch.loopStep.marketDebtsBefore ++
    branch.postDebtLocal :: branch.loopStep.marketDebtsAfter

theorem badDebtStep_debtsCoveredAfterBadDebt
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    DebtsCovered (oldTotalUnits - badDebt)
      (postBadDebtMarketDebts oldTotalUnits badDebt oldLossFactor lenders step) := by
  unfold postBadDebtMarketDebts
  exact debtsCovered_after_equal_decrease_split
    (badDebtStep_marketDebtsCoveredSplit oldTotalUnits badDebt oldLossFactor lenders step)
    (badDebtStep_badDebtLeOriginalDebt oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtStep_debtsCoveredAfterBadDebtFromMsbTrace
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders)
    {visited : List Nat}
    (htrace : MsbClearTrace step.activeCollateralBitmap visited) :
    DebtsCovered (oldTotalUnits - badDebt)
      (postBadDebtMarketDebts oldTotalUnits badDebt oldLossFactor lenders step) := by
  unfold postBadDebtMarketDebts
  exact debtsCovered_after_equal_decrease_split
    (badDebtStep_marketDebtsCoveredSplit oldTotalUnits badDebt oldLossFactor lenders step)
    (badDebtStep_badDebtLeOriginalDebtFromMsbTrace oldTotalUnits badDebt
      oldLossFactor lenders step htrace)

theorem badDebtTraceStep_debtsCoveredAfterBadDebt
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    DebtsCovered (oldTotalUnits - badDebt)
      (postBadDebtMarketDebts oldTotalUnits badDebt oldLossFactor lenders
        (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)) :=
  badDebtStep_debtsCoveredAfterBadDebt oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_debtsCoveredAfterBadDebt
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    DebtsCovered (oldTotalUnits - badDebt)
      (postBadDebtMarketDebts oldTotalUnits badDebt oldLossFactor lenders
        (BadDebtTraceStep.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders
          (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders))) :=
  badDebtTraceStep_debtsCoveredAfterBadDebt oldTotalUnits badDebt oldLossFactor lenders
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLocalStep_postDebtEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.postDebtLocal = branch.loopStep.originalDebt - branch.badDebt :=
  branch.postDebtLocalEq

theorem badDebtLocalStep_postDebtEqZeroFloor
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.postDebtLocal =
      UnitsAccounting.zeroFloorSub branch.loopStep.originalDebt branch.badDebt := by
  rw [branch.postDebtLocalEq, UnitsAccounting.zeroFloorSub_eq_sub]

theorem badDebtLocalStep_postMarketDebtsEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    postBadDebtLocalMarketDebts lenders branch =
      branch.loopStep.marketDebtsBefore ++
        (branch.loopStep.originalDebt - branch.badDebt) ::
          branch.loopStep.marketDebtsAfter := by
  unfold postBadDebtLocalMarketDebts
  rw [branch.postDebtLocalEq]

theorem badDebtLocalStep_debtsCoveredAfterBadDebt
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    DebtsCovered branch.newTotalUnitsLocal
      (postBadDebtLocalMarketDebts lenders branch) := by
  rw [branch.newTotalUnitsLocalEq, badDebtLocalStep_postMarketDebtsEq lenders branch]
  exact debtsCovered_after_equal_decrease_split
    (by
      rw [← branch.loopStep.marketDebtsEqSplit]
      exact branch.loopStep.marketDebtsCovered)
    (badDebtLoopStep_badDebtLeOriginalDebt branch.oldTotalUnits branch.badDebt
      branch.oldLossFactor lenders branch.loopStep)

theorem badDebtStep_lendersSynced
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    syncedAt { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } lenders := by
  rw [step.lendersEqStoredUpdates]
  exact syncedAt_map_update
    { totalUnits := oldTotalUnits, lossFactor := oldLossFactor }
    step.storedLenders

theorem badDebtTraceStep_lendersSynced
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    syncedAt { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } lenders :=
  badDebtStep_lendersSynced oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_lendersSynced
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    syncedAt { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } lenders :=
  badDebtTraceStep_lendersSynced oldTotalUnits badDebt oldLossFactor lenders
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtStep_oldCover
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit lenders ≤ oldTotalUnits := by
  rw [step.lendersEqStoredUpdates]
  exact totalUnits_covers_synced_credit_after_slashing
    { totalUnits := oldTotalUnits, lossFactor := oldLossFactor }
    step.storedLenders
    (le_of_lt step.oldLossFactorNotMax)
    step.storedCover

theorem badDebtTraceStep_oldCover
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit lenders ≤ oldTotalUnits :=
  badDebtStep_oldCover oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_oldCover
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit lenders ≤ oldTotalUnits :=
  badDebtTraceStep_oldCover oldTotalUnits badDebt oldLossFactor lenders
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLocalStep_oldMarketEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    oldBadDebtLocalMarket lenders branch =
      { totalUnits := branch.oldTotalUnits,
        lossFactor := branch.oldLossFactor } := by
  unfold oldBadDebtLocalMarket
  simp [branch.totalUnitsLocalEq, branch.lossFactorLocalEq]

theorem badDebtLocalStep_lendersSynced
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    syncedAt (oldBadDebtLocalMarket lenders branch) lenders := by
  rw [badDebtLocalStep_oldMarketEq lenders branch]
  exact badDebtLoopStep_lendersSynced branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor lenders branch.loopStep

theorem badDebtLocalStep_oldCover
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    sumCredit lenders ≤ (oldBadDebtLocalMarket lenders branch).totalUnits := by
  rw [badDebtLocalStep_oldMarketEq lenders branch]
  exact badDebtLoopStep_oldCover branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor lenders branch.loopStep

theorem badDebtStep_subtractionsSafe
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ step.originalDebt ∧ badDebt ≤ oldTotalUnits :=
  ⟨badDebtStep_badDebtLeOriginalDebt oldTotalUnits badDebt oldLossFactor lenders step,
    le_trans
      (badDebtStep_badDebtLeOriginalDebt oldTotalUnits badDebt oldLossFactor lenders step)
      (badDebtStep_originalDebtLeTotalUnits oldTotalUnits badDebt oldLossFactor lenders step)⟩

theorem badDebtTraceStep_subtractionsSafe
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ trace.originalDebt ∧ badDebt ≤ oldTotalUnits :=
  badDebtStep_subtractionsSafe oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_subtractionsSafe
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    badDebt ≤ loopStep.originalDebt ∧ badDebt ≤ oldTotalUnits :=
  ⟨badDebtLoopStep_badDebtLeOriginalDebt oldTotalUnits badDebt oldLossFactor
      lenders loopStep,
    badDebtLoopStep_badDebtLeTotalUnits oldTotalUnits badDebt oldLossFactor
      lenders loopStep⟩

/-- Refinement payoff for accounting: once generated-body extraction supplies the
    bad-debt branch obligations, `totalUnits` covers all up-to-date lender credit
    after slashing. -/
theorem badDebtStep_coversCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    totalUnitsCoversCredits (postBadDebtMarket oldTotalUnits badDebt oldLossFactor) lenders := by
  unfold postBadDebtMarket
  exact totalUnits_cover_after_badDebt_branch oldTotalUnits badDebt oldLossFactor lenders
    step.oldLossFactorNotMax
    (badDebtStep_subtractionsSafe oldTotalUnits badDebt oldLossFactor lenders step).2
    (badDebtStep_lendersSynced oldTotalUnits badDebt oldLossFactor lenders step)
    (badDebtStep_oldCover oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtTraceStep_coversCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    totalUnitsCoversCredits (postBadDebtMarket oldTotalUnits badDebt oldLossFactor) lenders :=
  badDebtStep_coversCredits oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_coversCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    totalUnitsCoversCredits (postBadDebtMarket oldTotalUnits badDebt oldLossFactor) lenders :=
  badDebtTraceStep_coversCredits oldTotalUnits badDebt oldLossFactor lenders
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLocalStep_marketEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    postBadDebtLocalMarket lenders branch =
      postBadDebtMarket branch.oldTotalUnits branch.badDebt branch.oldLossFactor := by
  unfold postBadDebtLocalMarket postBadDebtMarket
  simp [branch.newTotalUnitsLocalEq, branch.newLossFactorLocalEq]

theorem badDebtLocalStep_newTotalUnitsEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newTotalUnitsLocal = branch.oldTotalUnits - branch.badDebt :=
  branch.newTotalUnitsLocalEq

theorem badDebtLocalStep_badDebtUint128
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.badDebt ≤ UINT128_MAX := by
  have hbad :
      branch.badDebt ≤ branch.loopStep.originalDebt :=
    badDebtLoopStep_badDebtLeOriginalDebt branch.oldTotalUnits branch.badDebt
      branch.oldLossFactor lenders branch.loopStep
  exact le_trans hbad branch.originalDebtUint128

theorem badDebtLocalStep_postDebtUint128
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.postDebtLocal ≤ UINT128_MAX := by
  rw [branch.postDebtLocalEq]
  exact le_trans (Nat.sub_le branch.loopStep.originalDebt branch.badDebt)
    branch.originalDebtUint128

theorem badDebtLocalStep_newTotalUnitsUint128
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newTotalUnitsLocal ≤ UINT128_MAX := by
  rw [branch.newTotalUnitsLocalEq]
  exact le_trans (Nat.sub_le branch.oldTotalUnits branch.badDebt)
    branch.oldTotalUnitsUint128

theorem badDebtLocalStep_oldTotalUnitsPositiveOfBadDebt
    (lenders : List Lender) (branch : BadDebtLocalStep lenders)
    (hbad : branch.badDebt > 0) :
    branch.oldTotalUnits > 0 := by
  have hle :
      branch.badDebt ≤ branch.oldTotalUnits :=
    badDebtLoopStep_badDebtLeTotalUnits branch.oldTotalUnits branch.badDebt
      branch.oldLossFactor lenders branch.loopStep
  exact lt_of_lt_of_le hbad hle

theorem badDebtLocalStep_totalUnitsLocalPositiveOfBadDebt
    (lenders : List Lender) (branch : BadDebtLocalStep lenders)
    (hbad : branch.badDebt > 0) :
    branch.totalUnitsLocal > 0 := by
  rw [branch.totalUnitsLocalEq]
  exact badDebtLocalStep_oldTotalUnitsPositiveOfBadDebt lenders branch hbad

theorem badDebtLocalStep_newLossFactorEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newLossFactorLocal =
      lossFactorAfterBadDebt branch.oldTotalUnits branch.badDebt branch.oldLossFactor :=
  branch.newLossFactorLocalEq

theorem badDebtLocalStep_newLossFactorUint128
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newLossFactorLocal ≤ UINT128_MAX := by
  rw [branch.newLossFactorLocalEq]
  exact lossFactorAfterBadDebt_le_max branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor

theorem badDebtLocalStep_newLossFactorEqSolidityFormula
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newLossFactorLocal =
      UINT128_MAX -
        mulDivDown (UINT128_MAX - branch.lossFactorLocal)
          (branch.totalUnitsLocal - branch.badDebt) branch.totalUnitsLocal := by
  rw [branch.newLossFactorLocalEq, branch.totalUnitsLocalEq,
    branch.lossFactorLocalEq]
  rfl

theorem badDebtLocalStep_marketTotalUnitsEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    (postBadDebtLocalMarket lenders branch).totalUnits =
      branch.oldTotalUnits - branch.badDebt := by
  unfold postBadDebtLocalMarket
  exact branch.newTotalUnitsLocalEq

theorem badDebtLocalStep_marketLossFactorEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    (postBadDebtLocalMarket lenders branch).lossFactor =
      lossFactorAfterBadDebt branch.oldTotalUnits branch.badDebt branch.oldLossFactor := by
  unfold postBadDebtLocalMarket
  exact branch.newLossFactorLocalEq

theorem badDebtLocalStep_marketLossFactorEqSolidityFormula
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    (postBadDebtLocalMarket lenders branch).lossFactor =
      UINT128_MAX -
        mulDivDown (UINT128_MAX - branch.lossFactorLocal)
          (branch.totalUnitsLocal - branch.badDebt) branch.totalUnitsLocal := by
  unfold postBadDebtLocalMarket
  exact badDebtLocalStep_newLossFactorEqSolidityFormula lenders branch

theorem badDebtLocalStep_continuousFeeCreditEq
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newContinuousFeeCreditLocal =
      continuousFeeCreditAfterBadDebt branch.continuousFeeCreditLocal
        branch.lossFactorLocal branch.newLossFactorLocal :=
  branch.newContinuousFeeCreditLocalEq

theorem badDebtLocalStep_continuousFeeCreditEqSolidityFormula
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newContinuousFeeCreditLocal =
      if branch.lossFactorLocal < UINT128_MAX then
        mulDivDown branch.continuousFeeCreditLocal
          (UINT128_MAX - branch.newLossFactorLocal)
          (UINT128_MAX - branch.lossFactorLocal)
      else
        0 := by
  rw [branch.newContinuousFeeCreditLocalEq]
  rfl

theorem badDebtLocalStep_continuousFeeCreditEqWhenLossFactorNotMax
    (lenders : List Lender) (branch : BadDebtLocalStep lenders)
    (hold : branch.lossFactorLocal < UINT128_MAX) :
    branch.newContinuousFeeCreditLocal =
      mulDivDown branch.continuousFeeCreditLocal
        (UINT128_MAX - branch.newLossFactorLocal)
        (UINT128_MAX - branch.lossFactorLocal) := by
  rw [badDebtLocalStep_continuousFeeCreditEqSolidityFormula lenders branch]
  simp [hold]

theorem badDebtLocalStep_lossFactorLocalNotMax
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.lossFactorLocal < UINT128_MAX := by
  rw [branch.lossFactorLocalEq]
  exact branch.loopStep.oldLossFactorNotMax

theorem badDebtLocalStep_continuousFeeCreditDenominatorPositive
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    UINT128_MAX - branch.lossFactorLocal > 0 := by
  have h := badDebtLocalStep_lossFactorLocalNotMax lenders branch
  omega

theorem badDebtLocalStep_continuousFeeCreditEqActiveBranch
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newContinuousFeeCreditLocal =
      mulDivDown branch.continuousFeeCreditLocal
        (UINT128_MAX - branch.newLossFactorLocal)
        (UINT128_MAX - branch.lossFactorLocal) :=
  badDebtLocalStep_continuousFeeCreditEqWhenLossFactorNotMax lenders branch
    (badDebtLocalStep_lossFactorLocalNotMax lenders branch)

theorem badDebtLocalStep_continuousFeeCreditEqWhenLossFactorMax
    (lenders : List Lender) (branch : BadDebtLocalStep lenders)
    (hold : ¬ branch.lossFactorLocal < UINT128_MAX) :
    branch.newContinuousFeeCreditLocal = 0 := by
  rw [badDebtLocalStep_continuousFeeCreditEqSolidityFormula lenders branch]
  simp [hold]

theorem badDebtLocalStep_continuousFeeCreditLeCurrent
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newContinuousFeeCreditLocal ≤ branch.continuousFeeCreditLocal := by
  rw [branch.newContinuousFeeCreditLocalEq, branch.lossFactorLocalEq,
    branch.newLossFactorLocalEq]
  exact continuousFeeCreditAfterActualBadDebt_le_current
    branch.continuousFeeCreditLocal branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor
    (badDebtLoopStep_badDebtLeTotalUnits branch.oldTotalUnits branch.badDebt
      branch.oldLossFactor lenders branch.loopStep)

theorem badDebtLocalStep_newContinuousFeeCreditUint128
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    branch.newContinuousFeeCreditLocal ≤ UINT128_MAX := by
  exact le_trans (badDebtLocalStep_continuousFeeCreditLeCurrent lenders branch)
    branch.continuousFeeCreditLocalUint128

theorem badDebtLocalStep_coversCredits
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    totalUnitsCoversCredits (postBadDebtLocalMarket lenders branch) lenders := by
  rw [badDebtLocalStep_marketEq lenders branch]
  exact badDebtLoopStep_coversCredits branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor lenders branch.loopStep

/-- Refinement payoff for the actual `updatePositionView` returned credit:
    accrued continuous fee is subtracted after slashing, so the same bad-debt
    step also covers post-fee returned credit values. -/
theorem badDebtStep_coversPostUpdateCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (positions : List LenderPosition)
    (accrualEnd maturity : Nat)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor (lendersOfPositions positions)) :
    sumPostUpdateCreditView (postBadDebtMarket oldTotalUnits badDebt oldLossFactor)
        accrualEnd maturity positions ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits := by
  apply totalUnits_covers_postUpdateCredit_after_slashing
  exact badDebtStep_coversCredits oldTotalUnits badDebt oldLossFactor
    (lendersOfPositions positions) step

theorem badDebtTraceStep_coversPostUpdateCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (positions : List LenderPosition)
    (accrualEnd maturity : Nat)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor
      (lendersOfPositions positions)) :
    sumPostUpdateCreditView (postBadDebtMarket oldTotalUnits badDebt oldLossFactor)
        accrualEnd maturity positions ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits :=
  badDebtStep_coversPostUpdateCredits oldTotalUnits badDebt oldLossFactor
    positions accrualEnd maturity
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor
      (lendersOfPositions positions))

theorem badDebtLoopStep_coversPostUpdateCredits
    (oldTotalUnits badDebt oldLossFactor : Nat) (positions : List LenderPosition)
    (accrualEnd maturity : Nat)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor
      (lendersOfPositions positions)) :
    sumPostUpdateCreditView (postBadDebtMarket oldTotalUnits badDebt oldLossFactor)
        accrualEnd maturity positions ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits :=
  badDebtTraceStep_coversPostUpdateCredits oldTotalUnits badDebt oldLossFactor
    positions accrualEnd maturity
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor
      (lendersOfPositions positions))

theorem badDebtLocalStep_coversPostUpdateCredits
    (positions : List LenderPosition) (accrualEnd maturity : Nat)
    (branch : BadDebtLocalStep (lendersOfPositions positions)) :
    sumPostUpdateCreditView (postBadDebtLocalMarket (lendersOfPositions positions) branch)
        accrualEnd maturity positions ≤
      (postBadDebtLocalMarket (lendersOfPositions positions) branch).totalUnits := by
  rw [badDebtLocalStep_marketEq (lendersOfPositions positions) branch]
  exact badDebtLoopStep_coversPostUpdateCredits branch.oldTotalUnits branch.badDebt
    branch.oldLossFactor positions accrualEnd maturity branch.loopStep

theorem badDebtLocalStep_coversUpdatePositionViewResultCredits
    (positions : List LenderPosition) (timestamp maturity : Nat)
    (branch : BadDebtLocalStep (lendersOfPositions positions)) :
    sumUpdatePositionViewResultCredit
        (postBadDebtLocalMarket (lendersOfPositions positions) branch)
        timestamp maturity positions ≤
      (postBadDebtLocalMarket (lendersOfPositions positions) branch).totalUnits := by
  rw [sumUpdatePositionViewResultCredit_eq_sumPostUpdateCreditView]
  exact badDebtLocalStep_coversPostUpdateCredits positions
    (accrualEnd timestamp maturity) maturity branch

/-- Refinement payoff after every lender position has been synchronized with
    the new market loss factor: the stored credit fields themselves are covered
    by `totalUnits`. -/
theorem badDebtStep_coversStoredCreditsAfterUpdates
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (step : BadDebtStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit
        (lenders.map (updateLender (postBadDebtMarket oldTotalUnits badDebt oldLossFactor))) ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits := by
  exact totalUnits_covers_synced_credit_after_slashing
    (postBadDebtMarket oldTotalUnits badDebt oldLossFactor) lenders
    (by
      unfold postBadDebtMarket
      exact lossFactorAfterBadDebt_le_max oldTotalUnits badDebt oldLossFactor)
    (badDebtStep_coversCredits oldTotalUnits badDebt oldLossFactor lenders step)

theorem badDebtTraceStep_coversStoredCreditsAfterUpdates
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (trace : BadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit
        (lenders.map (updateLender (postBadDebtMarket oldTotalUnits badDebt oldLossFactor))) ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits :=
  badDebtStep_coversStoredCreditsAfterUpdates oldTotalUnits badDebt oldLossFactor lenders
    (trace.toBadDebtStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLoopStep_coversStoredCreditsAfterUpdates
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (loopStep : BadDebtLoopStep oldTotalUnits badDebt oldLossFactor lenders) :
    sumCredit
        (lenders.map (updateLender (postBadDebtMarket oldTotalUnits badDebt oldLossFactor))) ≤
      (postBadDebtMarket oldTotalUnits badDebt oldLossFactor).totalUnits :=
  badDebtTraceStep_coversStoredCreditsAfterUpdates oldTotalUnits badDebt oldLossFactor lenders
    (loopStep.toBadDebtTraceStep oldTotalUnits badDebt oldLossFactor lenders)

theorem badDebtLocalStep_coversStoredCreditsAfterUpdates
    (lenders : List Lender) (branch : BadDebtLocalStep lenders) :
    sumCredit
        (lenders.map (updateLender (postBadDebtLocalMarket lenders branch))) ≤
      (postBadDebtLocalMarket lenders branch).totalUnits := by
  rw [badDebtLocalStep_marketEq lenders branch]
  exact badDebtLoopStep_coversStoredCreditsAfterUpdates branch.oldTotalUnits
    branch.badDebt branch.oldLossFactor lenders branch.loopStep

end Accounting

namespace Liquidate

open Midnight.Proofs.Basic
open Midnight.Proofs.RCF
open Midnight.Proofs.UnitsAccounting
open Midnight.Proofs.MarketLedger

/-- Combined normal-mode `liquidate` local shape after the shared collateral
    loop.  The bad-debt branch and the later repaid-input RCF branch are tied to
    the same loop output, and the RCF branch's current debt local is the
    borrower debt after the optional bad-debt write. -/
structure NormalModeAfterBadDebtStep
    (p : RCFParams) (s : HealthState) (lenders : List Lender) where
  badDebtBranch : Accounting.BadDebtLocalStep lenders
  rcfBranch : RCF.RepaidMaxAfterBadDebtLocalStep p s
  sameLoop : rcfBranch.loopStep.loop = badDebtBranch.loopStep.loop
  sameOriginalDebt :
    rcfBranch.loopStep.originalDebt = badDebtBranch.loopStep.originalDebt
  sameBadDebt : rcfBranch.loopStep.badDebt = badDebtBranch.badDebt
  currentDebtEqPostBadDebt :
    rcfBranch.currentDebtLocal = badDebtBranch.postDebtLocal

theorem normalModeAfterBadDebtStep_currentDebtEqPostBadDebt
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.currentDebtLocal = step.badDebtBranch.postDebtLocal :=
  step.currentDebtEqPostBadDebt

theorem normalModeAfterBadDebtStep_currentDebtEqOriginalMinusBadDebt
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.currentDebtLocal =
      step.badDebtBranch.loopStep.originalDebt - step.badDebtBranch.badDebt := by
  rw [step.currentDebtEqPostBadDebt]
  exact step.badDebtBranch.postDebtLocalEq

theorem normalModeAfterBadDebtStep_currentDebtEqZeroFloor
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.currentDebtLocal =
      UnitsAccounting.zeroFloorSub step.badDebtBranch.loopStep.originalDebt
        step.badDebtBranch.badDebt := by
  rw [normalModeAfterBadDebtStep_currentDebtEqOriginalMinusBadDebt
    p s lenders step, UnitsAccounting.zeroFloorSub_eq_sub]

theorem normalModeAfterBadDebtStep_loopEq
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.loopStep.loop = step.badDebtBranch.loopStep.loop :=
  step.sameLoop

theorem normalModeAfterBadDebtStep_originalDebtExceedsLoopMaxDebt
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.loopStep.originalDebt >
      step.badDebtBranch.loopStep.loop.maxDebt := by
  have h :=
    RCF.repaidMaxAfterBadDebtLoopStep_originalDebtExceedsLoopMaxDebt p s
      step.rcfBranch.loopStep
  rw [step.sameOriginalDebt, step.sameLoop] at h
  exact h

theorem normalModeAfterBadDebtStep_currentDebtPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.currentDebtLocal > 0 :=
  RCF.repaidMaxAfterBadDebtLocalStep_currentDebtPositive p s step.rcfBranch

theorem normalModeAfterBadDebtStep_originalDebtPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.loopStep.originalDebt > 0 := by
  have h :=
    RCF.repaidMaxAfterBadDebtLoopStep_originalDebtPositive p s
      step.rcfBranch.loopStep
  rw [step.sameOriginalDebt] at h
  exact h

theorem normalModeAfterBadDebtStep_oldTotalUnitsPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.oldTotalUnits > 0 := by
  have horiginal :
      step.badDebtBranch.loopStep.originalDebt > 0 :=
    normalModeAfterBadDebtStep_originalDebtPositive p s lenders step
  have hle :
      step.badDebtBranch.loopStep.originalDebt ≤
        step.badDebtBranch.oldTotalUnits :=
    Accounting.badDebtLoopStep_originalDebtLeTotalUnits
      step.badDebtBranch.oldTotalUnits step.badDebtBranch.badDebt
      step.badDebtBranch.oldLossFactor lenders step.badDebtBranch.loopStep
  exact lt_of_lt_of_le horiginal hle

theorem normalModeAfterBadDebtStep_totalUnitsLocalPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.totalUnitsLocal > 0 := by
  rw [step.badDebtBranch.totalUnitsLocalEq]
  exact normalModeAfterBadDebtStep_oldTotalUnitsPositive p s lenders step

theorem normalModeAfterBadDebtStep_passesRcfGuard
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) (rcfThreshold : Nat) :
    rcfAllows step.rcfBranch.repaidUnits step.rcfBranch.maxRepaidLocal
      (collateralRepayCapacity step.rcfBranch.loopStep.collateral
        step.rcfBranch.loopStep.price p.lif)
      rcfThreshold :=
  RCF.repaidMaxAfterBadDebtLocalStep_passesRcfGuard p s step.rcfBranch
    rcfThreshold

theorem normalModeAfterBadDebtStep_passesRcfGuardZeroFloor
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) (rcfThreshold : Nat) :
    rcfAllowsZeroFloor step.rcfBranch.repaidUnits step.rcfBranch.maxRepaidLocal
      (collateralRepayCapacity step.rcfBranch.loopStep.collateral
        step.rcfBranch.loopStep.price p.lif)
      rcfThreshold :=
  RCF.repaidMaxAfterBadDebtLocalStep_passesRcfGuardZeroFloor p s step.rcfBranch
    rcfThreshold

theorem normalModeAfterBadDebtStep_generalRcfAmountBranch
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) (rcfThreshold : Nat) :
    ((step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).repaidUnits,
        (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).seizedAssets) =
      RCF.repaySeizeAmounts
        (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).inputRepaidUnitsLocal
        (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).inputSeizedAssetsLocal
        p.lif
        (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).loopStep.price :=
  RCF.normalModeRcfLocalStep_amountBranch p s
    (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold)

theorem normalModeAfterBadDebtStep_generalRcfAccepted
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) (rcfThreshold : Nat) :
    (step.rcfBranch.toNormalModeRcfLocalStep rcfThreshold).rcfAccepted :=
  RCF.repaidMaxAfterBadDebtLocalStep_generalRcfAccepted p s step.rcfBranch
    rcfThreshold

theorem normalModeAfterBadDebtStep_maxRepaidLocalEqSolidityFormula
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.maxRepaidLocal =
      mulDivUp
        (step.rcfBranch.currentDebtLocal - step.rcfBranch.loopStep.loop.maxDebt)
        scale (denominator p) :=
  RCF.repaidMaxAfterBadDebtLocalStep_maxRepaidLocalEqSolidityFormula p s
    step.rcfBranch

theorem normalModeAfterBadDebtStep_rcfDenominatorPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    denominator p > 0 :=
  RCF.repaidMaxAfterBadDebtLocalStep_denominatorPositive p s step.rcfBranch

theorem normalModeAfterBadDebtStep_lifEqSelectedMaxLif
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    p.lif =
      step.rcfBranch.loopStep.loop.activeCollateralArrays.maxLif
        step.rcfBranch.loopStep.selectedIndex :=
  RCF.repaidMaxAfterBadDebtLoopStep_lifEqSelectedMaxLif p s
    step.rcfBranch.loopStep

theorem normalModeAfterBadDebtStep_inputAtMostOneNonZero
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    RCF.atMostOneNonZeroInput step.rcfBranch.inputRepaidUnitsLocal
      step.rcfBranch.inputSeizedAssetsLocal :=
  RCF.repaidMaxAfterBadDebtLocalStep_inputAtMostOneNonZero p s step.rcfBranch

theorem normalModeAfterBadDebtStep_inputRepaidUnitsEqFormula
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.inputRepaidUnitsLocal = maxRepaid p s :=
  RCF.repaidMaxAfterBadDebtLocalStep_inputRepaidUnitsEqFormula p s
    step.rcfBranch

theorem normalModeAfterBadDebtStep_inputSeizedAssetsEqZero
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.inputSeizedAssetsLocal = 0 :=
  RCF.repaidMaxAfterBadDebtLocalStep_inputSeizedAssetsEqZero p s
    step.rcfBranch

theorem normalModeAfterBadDebtStep_repaidUnitsPositive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.repaidUnits > 0 :=
  RCF.repaidMaxAfterBadDebtLocalStep_repaidUnitsPositive p s step.rcfBranch

theorem normalModeAfterBadDebtStep_repayBranchActive
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.repaidUnits > 0 ∨ step.rcfBranch.seizedAssets > 0 :=
  RCF.repaidMaxAfterBadDebtLocalStep_repayBranchActive p s step.rcfBranch

theorem normalModeAfterBadDebtStep_rcfSubtractionsSafe
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.repaidUnits ≤ step.rcfBranch.currentDebtLocal ∧
      step.rcfBranch.seizedAssets ≤ step.rcfBranch.loopStep.collateral :=
  RCF.repaidMaxAfterBadDebtLocalStep_subtractionsSafe p s step.rcfBranch

theorem normalModeAfterBadDebtStep_repaidUnitsUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.repaidUnits ≤ UINT128_MAX :=
  RCF.repaidMaxAfterBadDebtLocalStep_repaidUnitsUint128 p s step.rcfBranch

theorem normalModeAfterBadDebtStep_seizedAssetsUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.seizedAssets ≤ UINT128_MAX :=
  RCF.repaidMaxAfterBadDebtLocalStep_seizedAssetsUint128 p s step.rcfBranch

theorem normalModeAfterBadDebtStep_rcfPostDebtUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.postDebtLocal ≤ UINT128_MAX :=
  RCF.repaidMaxAfterBadDebtLocalStep_postDebtUint128 p s step.rcfBranch

theorem normalModeAfterBadDebtStep_postCollateralUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.postCollateralLocal ≤ UINT128_MAX :=
  RCF.repaidMaxAfterBadDebtLocalStep_postCollateralUint128 p s step.rcfBranch

theorem normalModeAfterBadDebtStep_postCollateralBitmapUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.postCollateralBitmapLocal ≤ UINT128_MAX :=
  RCF.repaidMaxAfterBadDebtLocalStep_postCollateralBitmapUint128 p s
    step.rcfBranch

theorem normalModeAfterBadDebtStep_postWithdrawableEq
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.postWithdrawableLocal =
      step.rcfBranch.withdrawableLocal + step.rcfBranch.repaidUnits :=
  RCF.repaidMaxAfterBadDebtLocalStep_postWithdrawableEq p s step.rcfBranch

theorem normalModeAfterBadDebtStep_withdrawableIncreases
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.withdrawableLocal ≤ step.rcfBranch.postWithdrawableLocal :=
  RCF.repaidMaxAfterBadDebtLocalStep_withdrawableIncreases p s step.rcfBranch

theorem normalModeAfterBadDebtStep_postCollateralBitmapEq
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.postCollateralBitmapLocal =
      RCF.collateralBitmapAfterSeizure step.rcfBranch.collateralBitmapLocal
        step.rcfBranch.loopStep.selectedIndex step.rcfBranch.postCollateralLocal
        step.rcfBranch.seizedAssets :=
  RCF.repaidMaxAfterBadDebtLocalStep_postCollateralBitmapEq p s step.rcfBranch

theorem normalModeAfterBadDebtStep_postCollateralBitmapCleared
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hempty : step.rcfBranch.postCollateralLocal = 0)
    (hseized : step.rcfBranch.seizedAssets > 0) :
    step.rcfBranch.postCollateralBitmapLocal =
      clearBitmapBit step.rcfBranch.collateralBitmapLocal
        step.rcfBranch.loopStep.selectedIndex :=
  RCF.repaidMaxAfterBadDebtLocalStep_postCollateralBitmapCleared p s
    step.rcfBranch hempty hseized

theorem normalModeAfterBadDebtStep_postCollateralBitmapUnchanged
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hkeep :
      ¬ (step.rcfBranch.postCollateralLocal = 0 ∧
        step.rcfBranch.seizedAssets > 0)) :
    step.rcfBranch.postCollateralBitmapLocal =
      step.rcfBranch.collateralBitmapLocal :=
  RCF.repaidMaxAfterBadDebtLocalStep_postCollateralBitmapUnchanged p s
    step.rcfBranch hkeep

theorem normalModeAfterBadDebtStep_badDebtSubtractionsSafe
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.badDebt ≤ step.badDebtBranch.loopStep.originalDebt ∧
      step.badDebtBranch.badDebt ≤ step.badDebtBranch.oldTotalUnits :=
  Accounting.badDebtLoopStep_subtractionsSafe step.badDebtBranch.oldTotalUnits
    step.badDebtBranch.badDebt step.badDebtBranch.oldLossFactor lenders
    step.badDebtBranch.loopStep

theorem normalModeAfterBadDebtStep_badDebtUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.badDebt ≤ UINT128_MAX :=
  Accounting.badDebtLocalStep_badDebtUint128 lenders step.badDebtBranch

theorem normalModeAfterBadDebtStep_badDebtPostDebtUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.postDebtLocal ≤ UINT128_MAX :=
  Accounting.badDebtLocalStep_postDebtUint128 lenders step.badDebtBranch

theorem normalModeAfterBadDebtStep_newTotalUnitsUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.newTotalUnitsLocal ≤ UINT128_MAX :=
  Accounting.badDebtLocalStep_newTotalUnitsUint128 lenders step.badDebtBranch

theorem normalModeAfterBadDebtStep_newLossFactorUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.newLossFactorLocal ≤ UINT128_MAX :=
  Accounting.badDebtLocalStep_newLossFactorUint128 lenders step.badDebtBranch

theorem normalModeAfterBadDebtStep_newContinuousFeeCreditUint128
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.badDebtBranch.newContinuousFeeCreditLocal ≤ UINT128_MAX :=
  Accounting.badDebtLocalStep_newContinuousFeeCreditUint128 lenders
    step.badDebtBranch

theorem normalModeAfterBadDebtStep_badDebtZeroPostDebtEqOriginal
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    step.badDebtBranch.postDebtLocal =
      step.badDebtBranch.loopStep.originalDebt := by
  rw [step.badDebtBranch.postDebtLocalEq]
  omega

theorem normalModeAfterBadDebtStep_badDebtZeroTotalUnitsUnchanged
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    step.badDebtBranch.newTotalUnitsLocal =
      step.badDebtBranch.oldTotalUnits := by
  rw [step.badDebtBranch.newTotalUnitsLocalEq]
  omega

theorem normalModeAfterBadDebtStep_badDebtZeroLossFactorUnchanged
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    step.badDebtBranch.newLossFactorLocal =
      step.badDebtBranch.oldLossFactor := by
  rw [step.badDebtBranch.newLossFactorLocalEq, hbad]
  exact lossFactorAfterBadDebt_zero step.badDebtBranch.oldTotalUnits
    step.badDebtBranch.oldLossFactor
    (normalModeAfterBadDebtStep_oldTotalUnitsPositive p s lenders step)
    (le_of_lt step.badDebtBranch.loopStep.oldLossFactorNotMax)

theorem normalModeAfterBadDebtStep_badDebtZeroContinuousFeeCreditUnchanged
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders)
    (hbad : step.badDebtBranch.badDebt = 0) :
    step.badDebtBranch.newContinuousFeeCreditLocal =
      step.badDebtBranch.continuousFeeCreditLocal := by
  rw [step.badDebtBranch.newContinuousFeeCreditLocalEq]
  have hloss :
      step.badDebtBranch.newLossFactorLocal =
        step.badDebtBranch.oldLossFactor :=
    normalModeAfterBadDebtStep_badDebtZeroLossFactorUnchanged p s lenders step hbad
  rw [hloss, step.badDebtBranch.lossFactorLocalEq]
  exact continuousFeeCreditAfterBadDebt_noop
    step.badDebtBranch.continuousFeeCreditLocal step.badDebtBranch.oldLossFactor
    step.badDebtBranch.loopStep.oldLossFactorNotMax

def finalMarketDebts
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) : List Nat :=
  step.badDebtBranch.loopStep.marketDebtsBefore ++
    (step.rcfBranch.currentDebtLocal - step.rcfBranch.repaidUnits) ::
      step.badDebtBranch.loopStep.marketDebtsAfter

theorem normalModeAfterBadDebtStep_finalDebtEq
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    step.rcfBranch.currentDebtLocal - step.rcfBranch.repaidUnits =
      step.badDebtBranch.postDebtLocal - step.rcfBranch.repaidUnits := by
  rw [step.currentDebtEqPostBadDebt]

theorem normalModeAfterBadDebtStep_projectedDebtEqFinalDebt
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    (RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch).debt =
      step.rcfBranch.currentDebtLocal - step.rcfBranch.repaidUnits := by
  unfold RCF.projectedAfterBadDebtLocalPostState
  simp [liquidateWithDecrease, step.rcfBranch.currentDebtLocalEq]

theorem normalModeAfterBadDebtStep_finalMarketDebtsEqProjected
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    finalMarketDebts p s lenders step =
      step.badDebtBranch.loopStep.marketDebtsBefore ++
        (RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch).debt ::
          step.badDebtBranch.loopStep.marketDebtsAfter := by
  unfold finalMarketDebts
  rw [normalModeAfterBadDebtStep_projectedDebtEqFinalDebt p s lenders step]

theorem normalModeAfterBadDebtStep_finalDebtsCovered
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    DebtsCovered step.badDebtBranch.newTotalUnitsLocal
      (finalMarketDebts p s lenders step) := by
  unfold finalMarketDebts
  rw [step.currentDebtEqPostBadDebt]
  exact debtsCovered_after_decrease_split
    (Accounting.badDebtLocalStep_debtsCoveredAfterBadDebt lenders
      step.badDebtBranch)

theorem normalModeAfterBadDebtStep_restoresLocalPostWithinOne
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    healthyWithin 1
      (RCF.projectedAfterBadDebtLocalPostState p s step.rcfBranch) :=
  RCF.repaidMaxAfterBadDebtLocalStep_restoresLocalPostWithinOne p s
    step.rcfBranch

theorem normalModeAfterBadDebtStep_coversCredits
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    totalUnitsCoversCredits
      (Accounting.postBadDebtLocalMarket lenders step.badDebtBranch) lenders :=
  Accounting.badDebtLocalStep_coversCredits lenders step.badDebtBranch

theorem normalModeAfterBadDebtStep_coversPostUpdateCredits
    (p : RCFParams) (s : HealthState) (positions : List LenderPosition)
    (accrualEnd maturity : Nat)
    (step : NormalModeAfterBadDebtStep p s (lendersOfPositions positions)) :
    sumPostUpdateCreditView
        (Accounting.postBadDebtLocalMarket (lendersOfPositions positions)
          step.badDebtBranch)
        accrualEnd maturity positions ≤
      (Accounting.postBadDebtLocalMarket (lendersOfPositions positions)
        step.badDebtBranch).totalUnits :=
  Accounting.badDebtLocalStep_coversPostUpdateCredits positions accrualEnd
    maturity step.badDebtBranch

theorem normalModeAfterBadDebtStep_coversUpdatePositionViewResultCredits
    (p : RCFParams) (s : HealthState) (positions : List LenderPosition)
    (timestamp maturity : Nat)
    (step : NormalModeAfterBadDebtStep p s (lendersOfPositions positions)) :
    sumUpdatePositionViewResultCredit
        (Accounting.postBadDebtLocalMarket (lendersOfPositions positions)
          step.badDebtBranch)
        timestamp maturity positions ≤
      (Accounting.postBadDebtLocalMarket (lendersOfPositions positions)
        step.badDebtBranch).totalUnits :=
  Accounting.badDebtLocalStep_coversUpdatePositionViewResultCredits positions
    timestamp maturity step.badDebtBranch

theorem normalModeAfterBadDebtStep_coversStoredCreditsAfterUpdates
    (p : RCFParams) (s : HealthState) (lenders : List Lender)
    (step : NormalModeAfterBadDebtStep p s lenders) :
    sumCredit
        (lenders.map
          (updateLender
            (Accounting.postBadDebtLocalMarket lenders step.badDebtBranch))) ≤
      (Accounting.postBadDebtLocalMarket lenders step.badDebtBranch).totalUnits :=
  Accounting.badDebtLocalStep_coversStoredCreditsAfterUpdates lenders
    step.badDebtBranch

end Liquidate

end Midnight.Proofs.Refinement
