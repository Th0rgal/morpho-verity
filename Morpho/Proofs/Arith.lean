/-
  Arith — Nat lemmas about the floor/ceil division used by the health predicate.

  These are the mathematical core of property 1. They are generic facts about
  `mulDivDown` (floor) and `mulDivUp` (ceil) and carry no Morpho-specific content.
-/

import Mathlib.Tactic
import Morpho.Proofs.HealthModel

namespace Morpho.Proofs.Arith

open Morpho.Proofs.HealthModel

/-- Ceil-division characterization: `⌈n/d⌉ ≤ m ↔ n ≤ m*d` for `d > 0`.
    The ceiling is written `(n + (d-1))/d`, matching `MathLib.mulDivUp`. -/
theorem ceilDiv_le_iff {n d m : Nat} (hd : 0 < d) :
    (n + (d - 1)) / d ≤ m ↔ n ≤ m * d := by
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have hstep : m + 1 ≤ (n + (d - 1)) / d := by
      rw [Nat.le_div_iff_mul_le hd]
      have hexp : (m + 1) * d = m * d + d := by ring
      rw [hexp]; omega
    omega
  · intro h
    calc (n + (d - 1)) / d
        ≤ (m * d + (d - 1)) / d := Nat.div_le_div_right (by omega)
      _ = m := by
          rw [Nat.mul_comm m d, Nat.mul_add_div hd, Nat.div_eq_of_lt (by omega)]; omega

/-- `mulDivDown` is monotone in its first argument. -/
theorem mulDivDown_mono_left {a₁ a₂ b d : Nat} (h : a₁ ≤ a₂) :
    mulDivDown a₁ b d ≤ mulDivDown a₂ b d := by
  unfold mulDivDown
  exact Nat.div_le_div_right (Nat.mul_le_mul h (le_refl b))

/-- `mulDivUp` is monotone in its first argument. -/
theorem mulDivUp_mono_left {a₁ a₂ b d : Nat} (h : a₁ ≤ a₂) :
    mulDivUp a₁ b d ≤ mulDivUp a₂ b d := by
  unfold mulDivUp
  exact Nat.div_le_div_right (by
    have := Nat.mul_le_mul h (le_refl b)
    omega)

/--
  Crux lemma for property 1's index direction.

  If the borrow index does not grow (`A' * B ≤ A * B'`, the cross-multiplied form
  of `A'/B' ≤ A/B`) then the rounded-up borrowed amount does not grow either,
  for a fixed share balance `x`. This is what makes another account's `borrow`
  or `repay` — which only move the shared index — unable to worsen *this*
  account's health, absent accrual.
-/
theorem ceilDiv_index_antitone
    {x A B A' B' : Nat} (hB : 0 < B) (hB' : 0 < B')
    (hidx : A' * B ≤ A * B') :
    (x * A' + (B' - 1)) / B' ≤ (x * A + (B - 1)) / B := by
  set m := (x * A + (B - 1)) / B with hm
  rw [ceilDiv_le_iff hB']
  have hxA : x * A ≤ m * B := by
    have hle : (x * A + (B - 1)) / B ≤ m := le_of_eq hm.symm
    rwa [ceilDiv_le_iff hB] at hle
  have hstep : (x * A') * B ≤ (m * B') * B := by
    calc (x * A') * B = x * (A' * B) := by ring
      _ ≤ x * (A * B') := Nat.mul_le_mul (le_refl x) hidx
      _ = (x * A) * B' := by ring
      _ ≤ (m * B) * B' := Nat.mul_le_mul hxA (le_refl B')
      _ = (m * B') * B := by ring
  exact Nat.le_of_mul_le_mul_right hstep hB

end Morpho.Proofs.Arith
