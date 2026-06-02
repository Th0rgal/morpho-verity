/-
  CollateralLoop — projection of Midnight's active-collateral liquidation loop.

  Solidity anchor: `src/Midnight.sol::liquidate` initializes
  `maxDebt`, `liquidatedCollatPrice`, and `badDebt`, then iterates the
  borrower's `collateralBitmap` with `UtilsLib.msb` / `clearBit`.  For each
  active collateral it:

    * adds `collateral.mulDivDown(price, ORACLE_PRICE_SCALE).mulDivDown(lltv, WAD)`
      to `maxDebt`;
    * records `price` when the active index is the liquidated collateral;
    * subtracts, with zero floor,
      `collateral.mulDivUp(price, ORACLE_PRICE_SCALE).mulDivUp(WAD, maxLif)`
      from `badDebt`.

  This file proves those three accumulator projections over the active
  collateral schedule.  The separate extraction obligation is to show that the
  schedule is exactly the sequence produced by the Solidity bitmap loop.
-/

import Midnight.Proofs.RCF
import Midnight.Proofs.UnitsAccounting

namespace Midnight.Proofs.CollateralLoop

open Midnight.Proofs.Basic

structure CollateralSlot where
  index : Nat
  collateral : Nat
  price : Nat
  lltv : Nat
  maxLif : Nat
deriving Repr, DecidableEq

structure LoopState where
  maxDebt : Nat
  liquidatedCollatPrice : Nat
  badDebt : Nat
deriving Repr, DecidableEq

def maxDebtContribution (slot : CollateralSlot) : Nat :=
  RCF.collateralMaxDebt slot.collateral slot.price slot.lltv

def badDebtRepayable (slot : CollateralSlot) : Nat :=
  UnitsAccounting.collateralRepayableValue slot.collateral slot.price slot.maxLif

def step (liquidatedIndex : Nat) (s : LoopState) (slot : CollateralSlot) : LoopState :=
  { maxDebt := s.maxDebt + maxDebtContribution slot,
    liquidatedCollatPrice :=
      if slot.index = liquidatedIndex then slot.price else s.liquidatedCollatPrice,
    badDebt := s.badDebt - badDebtRepayable slot }

theorem step_badDebt_eq_zeroFloorSub
    (liquidatedIndex : Nat) (s : LoopState) (slot : CollateralSlot) :
    (step liquidatedIndex s slot).badDebt =
      UnitsAccounting.zeroFloorSub s.badDebt (badDebtRepayable slot) := by
  simp [step, UnitsAccounting.zeroFloorSub_eq_sub]

def run (liquidatedIndex : Nat) : LoopState → List CollateralSlot → LoopState
  | s, [] => s
  | s, slot :: rest => run liquidatedIndex (step liquidatedIndex s slot) rest

def sumMaxDebtContributions : List CollateralSlot → Nat
  | [] => 0
  | slot :: rest => maxDebtContribution slot + sumMaxDebtContributions rest

def maxDebtContributionsOf : List CollateralSlot → List Nat
  | [] => []
  | slot :: rest => maxDebtContribution slot :: maxDebtContributionsOf rest

def sumNat : List Nat → Nat
  | [] => 0
  | x :: xs => x + sumNat xs

def badDebtRepayables : List CollateralSlot → List Nat
  | [] => []
  | slot :: rest => badDebtRepayable slot :: badDebtRepayables rest

def selectedPriceAfter (liquidatedIndex : Nat) : Nat → List CollateralSlot → Nat
  | price, [] => price
  | price, slot :: rest =>
      selectedPriceAfter liquidatedIndex
        (if slot.index = liquidatedIndex then slot.price else price) rest

def noSlotWithIndex (index : Nat) : List CollateralSlot → Prop
  | [] => True
  | slot :: rest => slot.index ≠ index ∧ noSlotWithIndex index rest

theorem selectedPriceAfter_append
    (liquidatedIndex price : Nat) (left right : List CollateralSlot) :
    selectedPriceAfter liquidatedIndex price (left ++ right) =
      selectedPriceAfter liquidatedIndex
        (selectedPriceAfter liquidatedIndex price left) right := by
  induction left generalizing price with
  | nil =>
      simp [selectedPriceAfter]
  | cons slot rest ih =>
      simp [selectedPriceAfter, ih]

theorem selectedPriceAfter_noSlotWithIndex
    (liquidatedIndex price : Nat) (slots : List CollateralSlot)
    (h : noSlotWithIndex liquidatedIndex slots) :
    selectedPriceAfter liquidatedIndex price slots = price := by
  induction slots generalizing price with
  | nil =>
      simp [selectedPriceAfter]
  | cons slot rest ih =>
      rcases h with ⟨hslot, hrest⟩
      simp [selectedPriceAfter, hslot, ih _ hrest]

theorem selectedPriceAfter_split_selected
    (initialPrice : Nat) (before after : List CollateralSlot) (selectedSlot : CollateralSlot)
    (hafter : noSlotWithIndex selectedSlot.index after) :
    selectedPriceAfter selectedSlot.index initialPrice (before ++ selectedSlot :: after) =
      selectedSlot.price := by
  rw [selectedPriceAfter_append]
  simp [selectedPriceAfter]
  exact selectedPriceAfter_noSlotWithIndex selectedSlot.index selectedSlot.price after hafter

theorem run_maxDebt_eq
    (liquidatedIndex : Nat) (s : LoopState) (slots : List CollateralSlot) :
    (run liquidatedIndex s slots).maxDebt =
      s.maxDebt + sumMaxDebtContributions slots := by
  induction slots generalizing s with
  | nil =>
      simp [run, sumMaxDebtContributions]
  | cons slot rest ih =>
      simp [run, sumMaxDebtContributions, step, ih, Nat.add_assoc]

theorem sumMaxDebtContributions_eq_loop
    (slots : List CollateralSlot) :
    sumMaxDebtContributions slots =
      RCF.maxDebtAfterCollateralLoop 0 (maxDebtContributionsOf slots) := by
  induction slots with
  | nil =>
      simp [sumMaxDebtContributions, maxDebtContributionsOf, RCF.maxDebtAfterCollateralLoop]
  | cons slot rest ih =>
      simp [sumMaxDebtContributions, maxDebtContributionsOf, RCF.maxDebtAfterCollateralLoop, ih]

theorem maxDebtAfterCollateralLoop_eq_sumNat
    (base : Nat) (contributions : List Nat) :
    RCF.maxDebtAfterCollateralLoop base contributions = sumNat contributions + base := by
  induction contributions generalizing base with
  | nil =>
      simp [RCF.maxDebtAfterCollateralLoop, sumNat]
  | cons contribution rest ih =>
      simp [RCF.maxDebtAfterCollateralLoop, sumNat, ih]
      omega

theorem sumMaxDebtContributions_eq_sumNat
    (slots : List CollateralSlot) :
    sumMaxDebtContributions slots = sumNat (maxDebtContributionsOf slots) := by
  induction slots with
  | nil =>
      simp [sumMaxDebtContributions, maxDebtContributionsOf, sumNat]
  | cons slot rest ih =>
      simp [sumMaxDebtContributions, maxDebtContributionsOf, sumNat, ih]

theorem sumNat_append (left right : List Nat) :
    sumNat (left ++ right) = sumNat left + sumNat right := by
  induction left with
  | nil =>
      simp [sumNat]
  | cons x xs ih =>
      simp [sumNat, ih]
      omega

theorem maxDebtContributionsOf_append
    (left right : List CollateralSlot) :
    maxDebtContributionsOf (left ++ right) =
      maxDebtContributionsOf left ++ maxDebtContributionsOf right := by
  induction left with
  | nil =>
      simp [maxDebtContributionsOf]
  | cons slot rest ih =>
      simp [maxDebtContributionsOf, ih]

theorem sumMaxDebtContributions_split_selected
    (before after : List CollateralSlot) (selectedSlot : CollateralSlot) :
    sumMaxDebtContributions (before ++ selectedSlot :: after) =
      RCF.maxDebtAfterCollateralLoop
        (maxDebtContribution selectedSlot)
        (maxDebtContributionsOf before ++ maxDebtContributionsOf after) := by
  rw [sumMaxDebtContributions_eq_sumNat,
    maxDebtAfterCollateralLoop_eq_sumNat,
    sumNat_append]
  rw [maxDebtContributionsOf_append]
  simp [maxDebtContributionsOf, sumNat, sumNat_append]
  omega

theorem run_badDebt_eq
    (liquidatedIndex : Nat) (s : LoopState) (slots : List CollateralSlot) :
    (run liquidatedIndex s slots).badDebt =
      UnitsAccounting.badDebtAfterCollateralLoop s.badDebt
        (badDebtRepayables slots) := by
  induction slots generalizing s with
  | nil =>
      simp [run, badDebtRepayables, UnitsAccounting.badDebtAfterCollateralLoop]
  | cons slot rest ih =>
      simp [run, badDebtRepayables, step, UnitsAccounting.badDebtAfterCollateralLoop, ih]

theorem run_liquidatedCollatPrice_eq
    (liquidatedIndex : Nat) (s : LoopState) (slots : List CollateralSlot) :
    (run liquidatedIndex s slots).liquidatedCollatPrice =
      selectedPriceAfter liquidatedIndex s.liquidatedCollatPrice slots := by
  induction slots generalizing s with
  | nil =>
      simp [run, selectedPriceAfter]
  | cons slot rest ih =>
      simp [run, selectedPriceAfter, step, ih]

theorem run_badDebt_le_initial
    (liquidatedIndex : Nat) (s : LoopState) (slots : List CollateralSlot) :
    (run liquidatedIndex s slots).badDebt ≤ s.badDebt := by
  rw [run_badDebt_eq]
  exact UnitsAccounting.badDebtAfterCollateralLoop_le_start s.badDebt
    (badDebtRepayables slots)

theorem run_maxDebt_from_zero_eq
    (liquidatedIndex : Nat) (badDebt : Nat) (slots : List CollateralSlot) :
    (run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := badDebt } slots).maxDebt =
      sumMaxDebtContributions slots := by
  simpa using
    run_maxDebt_eq liquidatedIndex
      { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := badDebt } slots

theorem run_badDebt_from_original_eq
    (liquidatedIndex originalDebt : Nat) (slots : List CollateralSlot) :
    (run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt } slots).badDebt =
      UnitsAccounting.badDebtAfterCollateralLoop originalDebt
        (badDebtRepayables slots) := by
  exact run_badDebt_eq liquidatedIndex
    { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt } slots

theorem run_liquidatedCollatPrice_from_zero_eq
    (liquidatedIndex originalDebt : Nat) (slots : List CollateralSlot) :
    (run liquidatedIndex
        { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt } slots).liquidatedCollatPrice =
      selectedPriceAfter liquidatedIndex 0 slots := by
  exact run_liquidatedCollatPrice_eq liquidatedIndex
    { maxDebt := 0, liquidatedCollatPrice := 0, badDebt := originalDebt } slots

end Midnight.Proofs.CollateralLoop
