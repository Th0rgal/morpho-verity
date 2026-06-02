import Mathlib.Tactic

namespace Midnight.Proofs.Basic

/-- Midnight's fixed-point scale, matching `WAD` in `ConstantsLib.sol`. -/
def WAD : Nat := 1000000000000000000

/-- Oracle price scale, matching `ORACLE_PRICE_SCALE` in `ConstantsLib.sol`. -/
def ORACLE_PRICE_SCALE : Nat := 1000000000000000000000000000000000000

/-- `type(uint128).max`, used as the saturated loss-factor denominator. -/
def UINT128_MAX : Nat := 340282366920938463463374607431768211455

/-- `type(uint256).max`, used by Midnight's `maxRepaid` fallback. -/
def UINT256_MAX : Nat :=
  115792089237316195423570985008687907853269984665640564039457584007913129639935

/-- `UtilsLib.mulDivDown`: floor `(a * b) / d`. -/
def mulDivDown (a b d : Nat) : Nat := a * b / d

/-- `UtilsLib.mulDivUp`: ceil `(a * b) / d`, for positive `d`. -/
def mulDivUp (a b d : Nat) : Nat := (a * b + (d - 1)) / d

theorem mulDivDown_mul_le (a b d : Nat) :
    mulDivDown a b d * d ≤ a * b := by
  unfold mulDivDown
  exact Nat.div_mul_le_self _ _

theorem mulDivDown_le_left {a b d : Nat} (hbd : b ≤ d) :
    mulDivDown a b d ≤ a := by
  by_cases hd : d = 0
  · simp [mulDivDown, hd]
  · have hdpos : 0 < d := Nat.pos_of_ne_zero hd
    calc a * b / d
        ≤ a * d / d := Nat.div_le_div_right (Nat.mul_le_mul_left a hbd)
      _ = a := by rw [Nat.mul_comm a d, Nat.mul_div_right a hdpos]

theorem mulDivDown_mono_left {a₁ a₂ b d : Nat} (h : a₁ ≤ a₂) :
    mulDivDown a₁ b d ≤ mulDivDown a₂ b d := by
  unfold mulDivDown
  exact Nat.div_le_div_right (Nat.mul_le_mul h (le_refl b))

theorem mulDivDown_same_den (a d : Nat) (hd : 0 < d) :
    mulDivDown a d d = a := by
  unfold mulDivDown
  rw [Nat.mul_comm a d, Nat.mul_div_right a hd]

/-- Ceil-division characterization: `ceil(n / d) ≤ m ↔ n ≤ m*d`. -/
theorem ceilDiv_le_iff {n d m : Nat} (hd : 0 < d) :
    (n + (d - 1)) / d ≤ m ↔ n ≤ m * d := by
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have hstep : m + 1 ≤ (n + (d - 1)) / d := by
      rw [Nat.le_div_iff_mul_le hd]
      have hexp : (m + 1) * d = m * d + d := by ring
      rw [hexp]
      omega
    omega
  · intro h
    calc (n + (d - 1)) / d
        ≤ (m * d + (d - 1)) / d := Nat.div_le_div_right (by omega)
      _ = m := by
        rw [Nat.mul_comm m d, Nat.mul_add_div hd, Nat.div_eq_of_lt (by omega)]
        omega

theorem le_mulDivUp_mul {a b d : Nat} (hd : 0 < d) :
    a * b ≤ mulDivUp a b d * d := by
  have h : (a * b + (d - 1)) / d ≤ mulDivUp a b d := by
    rfl
  rwa [ceilDiv_le_iff hd] at h

end Midnight.Proofs.Basic
