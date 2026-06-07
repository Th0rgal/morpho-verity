import Contracts.Common
import Mathlib.Data.Nat.Bitwise

namespace Contracts

open Verity.Core

/-- Public proof-side copy of Verity's packed-word mask helper. The executable
    helpers in `Contracts.Common` use the same body internally but keep the
    helper private. -/
def packedMask (width : Nat) : Nat :=
  2 ^ width - 1

/-- Public proof-side copy of Verity's packed-word decoder. -/
def decodePackedWord (word : Uint256) (offset width : Nat) : Uint256 :=
  Uint256.and (Uint256.shr offset word) (packedMask width)

/-- Public proof-side copy of Verity's packed-word encoder. -/
def encodePackedWord (current value : Uint256) (offset width : Nat) : Uint256 :=
  let mask := packedMask width
  let shiftedMask := Uint256.shl offset mask
  let packedValue := Uint256.and value mask
  Uint256.or (Uint256.and current (Uint256.not shiftedMask)) (Uint256.shl offset packedValue)

namespace PackedWord

private lemma ofNat_val (n : Nat) : (Uint256.ofNat n).val = n % 2 ^ 256 := rfl

private lemma testBit_mod (x i : Nat) :
    Nat.testBit (x % 2 ^ 256) i = (decide (i < 256) && Nat.testBit x i) :=
  Nat.testBit_mod_two_pow x 256 i

private lemma testBit_ofNat (x i : Nat) :
    Nat.testBit (Uint256.ofNat x).val i = (decide (i < 256) && Nat.testBit x i) := by
  rw [ofNat_val]; exact testBit_mod x i

private lemma testBit_and (a b : Uint256) (i : Nat) :
    Nat.testBit (Uint256.and a b).val i
      = (decide (i < 256) && (Nat.testBit a.val i && Nat.testBit b.val i)) := by
  show Nat.testBit (Uint256.ofNat (a.val &&& b.val)).val i = _
  rw [testBit_ofNat, Nat.testBit_and]

private lemma testBit_or (a b : Uint256) (i : Nat) :
    Nat.testBit (Uint256.or a b).val i
      = (decide (i < 256) && (Nat.testBit a.val i || Nat.testBit b.val i)) := by
  show Nat.testBit (Uint256.ofNat (a.val ||| b.val)).val i = _
  rw [testBit_ofNat, Nat.testBit_or]

private lemma testBit_shl (s v : Uint256) (i : Nat) :
    Nat.testBit (Uint256.shl s v).val i
      = (decide (i < 256) && (decide (s.val ≤ i) && Nat.testBit v.val (i - s.val))) := by
  show Nat.testBit (Uint256.ofNat (v.val <<< s.val)).val i = _
  rw [testBit_ofNat, Nat.testBit_shiftLeft]

private lemma testBit_shr (s v : Uint256) (i : Nat) :
    Nat.testBit (Uint256.shr s v).val i
      = (decide (i < 256) && Nat.testBit v.val (s.val + i)) := by
  show Nat.testBit (Uint256.ofNat (v.val >>> s.val)).val i = _
  rw [testBit_ofNat, Nat.testBit_shiftRight]

private lemma val_not (a : Uint256) :
    (Uint256.not a).val = 2 ^ 256 - (a.val + 1) := by
  show (MAX_UINT256 - a.val) % Uint256.modulus = 2 ^ 256 - (a.val + 1)
  have ha : a.val < 2 ^ 256 := a.isLt
  have heq : MAX_UINT256 - a.val = 2 ^ 256 - (a.val + 1) := by
    show 2 ^ 256 - 1 - a.val = 2 ^ 256 - (a.val + 1)
    omega
  rw [heq]
  apply Nat.mod_eq_of_lt
  show 2 ^ 256 - (a.val + 1) < 2 ^ 256
  have hpos : 0 < 2 ^ 256 := Nat.two_pow_pos 256
  omega

private lemma testBit_not (a : Uint256) (i : Nat) :
    Nat.testBit (Uint256.not a).val i = (decide (i < 256) && !Nat.testBit a.val i) := by
  rw [val_not]
  exact Nat.testBit_two_pow_sub_succ a.isLt i

private lemma testBit_val_of_ge_256 (a : Uint256) {i : Nat} (hi : 256 ≤ i) :
    Nat.testBit a.val i = false := by
  apply Nat.testBit_lt_two_pow
  exact lt_of_lt_of_le a.isLt (Nat.pow_le_pow_right (by decide) hi)

private lemma offset_coe_val {offset : Nat} (h : offset ≤ 256) :
    (Uint256.ofNat offset).val = offset := by
  rw [ofNat_val]
  apply Nat.mod_eq_of_lt
  have h256 : (256 : Nat) < 2 ^ 256 := by decide
  omega

private lemma testBit_packedMask (w i : Nat) :
    Nat.testBit (packedMask w) i = decide (i < w) := by
  unfold packedMask; exact Nat.testBit_two_pow_sub_one w i

private lemma testBit_packedMask_coe (w i : Nat) :
    Nat.testBit (Uint256.ofNat (packedMask w)).val i
      = (decide (i < 256) && decide (i < w)) := by
  rw [testBit_ofNat, testBit_packedMask]

theorem public_decode_encode_eq (current value : Uint256) (offset width : Nat)
    (hoff : offset + width ≤ 256) (hval : value.val < 2 ^ width) :
    decodePackedWord (encodePackedWord current value offset width) offset width = value := by
  apply Uint256.ext
  apply Nat.eq_of_testBit_eq
  intro j
  have hoff_le : offset ≤ 256 := Nat.le_of_add_right_le hoff
  have hocoe : (Uint256.ofNat offset).val = offset := offset_coe_val hoff_le
  show Nat.testBit
    (Uint256.and
      (Uint256.shr (↑offset) (Uint256.or
        (Uint256.and current
          (Uint256.not (Uint256.shl (↑offset) (↑(packedMask width)))))
        (Uint256.shl (↑offset) (Uint256.and value (↑(packedMask width))))))
      (↑(packedMask width))).val j = Nat.testBit value.val j
  simp only [testBit_and, testBit_shr, testBit_or, testBit_not, testBit_shl,
             testBit_packedMask_coe, hocoe]
  by_cases hj256 : j < 256
  · by_cases hjw : j < width
    · have hofj256 : offset + j < 256 := by omega
      have hofj_ge : offset ≤ offset + j := Nat.le_add_right _ _
      have hsub : (offset + j) - offset = j := by omega
      simp only [hj256, hjw, hofj256, hofj_ge, hsub, decide_true, Bool.true_and, Bool.and_true]
      cases hb : Nat.testBit value.val j <;> simp
    · push_neg at hjw
      have hval_bit : Nat.testBit value.val j = false := by
        apply Nat.testBit_lt_two_pow
        exact lt_of_lt_of_le hval (Nat.pow_le_pow_right (by decide) hjw)
      have hdw : decide (j < width) = false := by simp [Nat.not_lt.mpr hjw]
      simp [hval_bit, hdw]
  · push_neg at hj256
    have hval_bit : Nat.testBit value.val j = false := testBit_val_of_ge_256 value hj256
    have hd256 : decide (j < 256) = false := by simp [Nat.not_lt.mpr hj256]
    simp [hval_bit, hd256]

theorem public_decode_encode_disjoint (current value : Uint256) (o1 w1 o2 w2 : Nat)
    (hbound1 : o1 + w1 ≤ 256) (hbound2 : o2 + w2 ≤ 256)
    (hdisj : o1 + w1 ≤ o2 ∨ o2 + w2 ≤ o1) :
    decodePackedWord (encodePackedWord current value o2 w2) o1 w1
      = decodePackedWord current o1 w1 := by
  apply Uint256.ext
  apply Nat.eq_of_testBit_eq
  intro j
  have ho1_le : o1 ≤ 256 := Nat.le_of_add_right_le hbound1
  have ho2_le : o2 ≤ 256 := Nat.le_of_add_right_le hbound2
  have ho1coe : (Uint256.ofNat o1).val = o1 := offset_coe_val ho1_le
  have ho2coe : (Uint256.ofNat o2).val = o2 := offset_coe_val ho2_le
  show Nat.testBit
    (Uint256.and
      (Uint256.shr (↑o1) (Uint256.or
        (Uint256.and current
          (Uint256.not (Uint256.shl (↑o2) (↑(packedMask w2)))))
        (Uint256.shl (↑o2) (Uint256.and value (↑(packedMask w2))))))
      (↑(packedMask w1))).val j =
    Nat.testBit (Uint256.and (Uint256.shr (↑o1) current) (↑(packedMask w1))).val j
  simp only [testBit_and, testBit_shr, testBit_or, testBit_not, testBit_shl,
             testBit_packedMask_coe, ho1coe, ho2coe]
  by_cases hj256 : j < 256
  · by_cases hjw1 : j < w1
    · have ho1j256 : o1 + j < 256 := by omega
      rcases hdisj with h | h
      · have hf : decide (o2 ≤ o1 + j) = false := by
          simp only [decide_eq_false_iff_not, Nat.not_le]; omega
        simp [hj256, hjw1, ho1j256, hf]
      · have hf : decide (o1 + j - o2 < w2) = false := by
          simp only [decide_eq_false_iff_not, Nat.not_lt]; omega
        simp [hj256, hjw1, ho1j256, hf]
    · have hdw1 : decide (j < w1) = false := by simp [hjw1]
      simp [hdw1]
  · push_neg at hj256
    have hd256 : decide (j < 256) = false := by simp [Nat.not_lt.mpr hj256]
    simp [hd256]

theorem public_decode_lt_width (word : Uint256) (offset width : Nat) :
    (decodePackedWord word offset width).val < 2 ^ width := by
  unfold decodePackedWord
  apply Nat.lt_of_testBit width
  · simp only [testBit_and, testBit_shr, testBit_packedMask_coe]
    simp
  · simp
  · intro j hj
    simp only [testBit_and, testBit_shr, testBit_packedMask_coe]
    have hjw : ¬ j < width := by omega
    have hne : width ≠ j := by omega
    simp [hjw, Nat.testBit_two_pow_of_ne hne]

end PackedWord

end Contracts
