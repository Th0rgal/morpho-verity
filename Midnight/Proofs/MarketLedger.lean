/-
  MarketLedger — debt-side market cover invariant used by bad-debt extraction.

  Solidity anchor: `src/Midnight.sol` keeps `marketState[id].totalUnits` as the
  market-wide units measure.  `repay` decreases borrower debt without decreasing
  `totalUnits`; bad-debt realization decreases borrower debt and `totalUnits` by
  the same `badDebt`; `withdraw` decreases lender credit and `totalUnits` only
  after units are withdrawable.  The bad-debt branch needs the local fact
  `originalDebt <= totalUnits` for `_totalUnits - badDebt`.

  This file isolates the debt-side invariant that supplies that fact:
  every borrower debt in the market debt list is covered when the sum of market
  debts is covered by `totalUnits`.
-/

import Midnight.Proofs.Basic

namespace Midnight.Proofs.MarketLedger

def sumDebts : List Nat → Nat
  | [] => 0
  | debt :: rest => debt + sumDebts rest

def DebtsCovered (totalUnits : Nat) (debts : List Nat) : Prop :=
  sumDebts debts ≤ totalUnits

theorem sumDebts_append (left right : List Nat) :
    sumDebts (left ++ right) = sumDebts left + sumDebts right := by
  induction left with
  | nil =>
      simp [sumDebts]
  | cons debt rest ih =>
      simp [sumDebts, ih]
      omega

theorem debt_le_sumDebts_of_mem
    {debt : Nat} {debts : List Nat} (hmem : debt ∈ debts) :
    debt ≤ sumDebts debts := by
  induction debts with
  | nil =>
      cases hmem
  | cons head rest ih =>
      simp [sumDebts] at *
      rcases hmem with hhead | htail
      · omega
      · have h := ih htail
        omega

theorem debt_le_totalUnits_of_mem_covered
    {totalUnits debt : Nat} {debts : List Nat}
    (hmem : debt ∈ debts)
    (hcovered : DebtsCovered totalUnits debts) :
    debt ≤ totalUnits := by
  unfold DebtsCovered at hcovered
  exact le_trans (debt_le_sumDebts_of_mem hmem) hcovered

def decreaseFirstDebt (amount : Nat) : List Nat → List Nat
  | [] => []
  | debt :: rest => (debt - amount) :: rest

theorem sumDebts_decreaseFirstDebt_le
    (amount : Nat) (debts : List Nat) :
    sumDebts (decreaseFirstDebt amount debts) ≤ sumDebts debts := by
  cases debts with
  | nil =>
      simp [decreaseFirstDebt, sumDebts]
  | cons debt rest =>
      simp [decreaseFirstDebt, sumDebts]

theorem debtsCovered_after_decreaseFirstDebt
    {totalUnits amount : Nat} {debts : List Nat}
    (hcovered : DebtsCovered totalUnits debts) :
    DebtsCovered totalUnits (decreaseFirstDebt amount debts) := by
  unfold DebtsCovered at *
  exact le_trans (sumDebts_decreaseFirstDebt_le amount debts) hcovered

theorem debtsCovered_after_equal_decrease_first
    {totalUnits amount debt : Nat} {rest : List Nat}
    (hcovered : DebtsCovered totalUnits (debt :: rest))
    (hamountDebt : amount ≤ debt) :
    DebtsCovered (totalUnits - amount)
      (decreaseFirstDebt amount (debt :: rest)) := by
  unfold DebtsCovered at *
  simp [decreaseFirstDebt, sumDebts]
  have hdebtTotal : debt ≤ totalUnits := by
    calc
      debt ≤ debt + sumDebts rest := Nat.le_add_right debt (sumDebts rest)
      _ ≤ totalUnits := hcovered
  have hamountTotal : amount ≤ totalUnits := le_trans hamountDebt hdebtTotal
  rw [Nat.le_sub_iff_add_le hamountTotal]
  have hcancel : debt - amount + sumDebts rest + amount = debt + sumDebts rest := by
    omega
  rw [hcancel]
  exact hcovered

theorem debtsCovered_after_equal_decrease_split
    {totalUnits amount debt : Nat} {before after : List Nat}
    (hcovered : DebtsCovered totalUnits (before ++ debt :: after))
    (hamountDebt : amount ≤ debt) :
    DebtsCovered (totalUnits - amount)
      (before ++ (debt - amount) :: after) := by
  unfold DebtsCovered at *
  rw [sumDebts_append] at hcovered
  simp [sumDebts] at hcovered
  rw [sumDebts_append]
  simp [sumDebts]
  have hdebtTotal : debt ≤ totalUnits := by
    calc
      debt ≤ sumDebts before + (debt + sumDebts after) := by omega
      _ ≤ totalUnits := hcovered
  have hamountTotal : amount ≤ totalUnits := le_trans hamountDebt hdebtTotal
  rw [Nat.le_sub_iff_add_le hamountTotal]
  have hcancel :
      sumDebts before + (debt - amount + sumDebts after) + amount =
        sumDebts before + (debt + sumDebts after) := by
    omega
  rw [hcancel]
  exact hcovered

theorem debtsCovered_after_decrease_split
    {totalUnits amount debt : Nat} {before after : List Nat}
    (hcovered : DebtsCovered totalUnits (before ++ debt :: after)) :
    DebtsCovered totalUnits (before ++ (debt - amount) :: after) := by
  unfold DebtsCovered at *
  rw [sumDebts_append] at hcovered
  simp [sumDebts] at hcovered
  rw [sumDebts_append]
  simp [sumDebts]
  omega

end Midnight.Proofs.MarketLedger
