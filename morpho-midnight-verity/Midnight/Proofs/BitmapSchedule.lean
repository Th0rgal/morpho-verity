/-
  BitmapSchedule - shape of Midnight's collateral bitmap iteration.

  Solidity anchor: `src/Midnight.sol::liquidate` and `isHealthy` iterate a
  `uint128 collateralBitmap` by repeatedly taking `UtilsLib.msb(bitmap)` and
  clearing that bit.  `ConstantsLib.sol` fixes `MAX_COLLATERALS = 128`, and
  `supplyCollateral` enforces at most `MAX_COLLATERALS_PER_BORROWER = 16`
  activated collateral bits.

  The executable `bitmapSchedule` below is the high-to-low projection of a
  `uint128` collateral bitmap: for each index below 128, include it exactly when
  `Nat.testBit bitmap index` is true.  This is the mathematical schedule that
  Solidity's `msb` / `clearBit` loop is meant to realize.  Once the generated
  body connects storage to this bitmap schedule, the existing collateral-loop
  accumulator proofs consume the corresponding slots directly.
-/

import Midnight.Proofs.CollateralLoop

namespace Midnight.Proofs.BitmapSchedule

open Midnight.Proofs.CollateralLoop

def MAX_COLLATERALS : Nat := 128
def MAX_COLLATERALS_PER_BORROWER : Nat := 16

def UINT128_BOUND : Nat := 2 ^ MAX_COLLATERALS

/-- `UtilsLib.msb` followed by `clearBit` visits set bits from high to low. -/
def StrictDescendingBelow : Nat → List Nat → Prop
  | _bound, [] => True
  | bound, i :: rest => i < bound ∧ StrictDescendingBelow i rest

theorem StrictDescendingBelow.length_le_bound
    {bound : Nat} {indices : List Nat}
    (h : StrictDescendingBelow bound indices) :
    indices.length ≤ bound := by
  induction indices generalizing bound with
  | nil =>
      simp
  | cons i rest ih =>
      rcases h with ⟨hi, hrest⟩
      have htail : rest.length ≤ i := ih hrest
      simp
      omega

theorem StrictDescendingBelow.mem_lt_bound
    {bound index : Nat} {indices : List Nat}
    (h : StrictDescendingBelow bound indices)
    (hmem : index ∈ indices) :
    index < bound := by
  induction indices generalizing bound with
  | nil =>
      cases hmem
  | cons i rest ih =>
      rcases h with ⟨hi, hrest⟩
      simp at hmem
      rcases hmem with hhead | htail
      · omega
      · have hindex : index < i := ih hrest htail
        omega

theorem StrictDescendingBelow.nodup
    {bound : Nat} {indices : List Nat}
    (h : StrictDescendingBelow bound indices) :
    indices.Nodup := by
  induction indices generalizing bound with
  | nil =>
      simp
  | cons i rest ih =>
      rcases h with ⟨_hi, hrest⟩
      have hnotMem : i ∉ rest := by
        intro hmem
        have hlt : i < i := StrictDescendingBelow.mem_lt_bound hrest hmem
        omega
      exact List.nodup_cons.mpr ⟨hnotMem, ih hrest⟩

theorem StrictDescendingBelow.mono_bound
    {small large : Nat} {indices : List Nat}
    (h : StrictDescendingBelow small indices)
    (hle : small ≤ large) :
    StrictDescendingBelow large indices := by
  induction indices generalizing small large with
  | nil =>
      simp [StrictDescendingBelow]
  | cons i rest ih =>
      rcases h with ⟨hi, hrest⟩
      constructor
      · omega
      · exact ih hrest (Nat.le_refl i)

def ValidSchedule (indices : List Nat) : Prop :=
  StrictDescendingBelow MAX_COLLATERALS indices ∧
    indices.length ≤ MAX_COLLATERALS_PER_BORROWER

/-- Scan candidate bit positions from high to low.  `bitmapScheduleFrom active n`
    lists all active indices below `n` in the same order as repeated
    `msb(bitmap)` followed by `clearBit`. -/
def bitmapScheduleFrom (active : Nat → Bool) : Nat → List Nat
  | 0 => []
  | n + 1 =>
      if active n then
        n :: bitmapScheduleFrom active n
      else
        bitmapScheduleFrom active n

def bitmapSchedule (bitmap : Nat) : List Nat :=
  bitmapScheduleFrom (fun index => bitmap.testBit index) MAX_COLLATERALS

/-- Solidity's `uint128(bitmap & ~(1 << clearedIndex))` mask on the modeled
    128-bit collateral bitmap. Out-of-domain indices leave the bitmap unchanged;
    all loop theorems prove `clearedIndex < 128` before using the clearing
    behavior. -/
def clearBitmapBit (bitmap clearedIndex : Nat) : Nat :=
  if clearedIndex < MAX_COLLATERALS then
    bitmap &&& (UINT128_BOUND - (2 ^ clearedIndex + 1))
  else
    bitmap

def clearBitmapBits : Nat → List Nat → Nat
  | bitmap, [] => bitmap
  | bitmap, index :: rest => clearBitmapBits (clearBitmapBit bitmap index) rest

def clearActiveBit (active : Nat → Bool) (clearedIndex : Nat) : Nat → Bool :=
  fun index => if index = clearedIndex then false else active index

def HighestActiveBit (active : Nat → Bool) (bound index : Nat) : Prop :=
  index < bound ∧ active index = true ∧
    ∀ j, index < j → j < bound → active j = false

def HighestBitmapBit (bitmap bound index : Nat) : Prop :=
  HighestActiveBit (fun index => bitmap.testBit index) bound index

/-- Contract-shaped trace of the Solidity loop that repeatedly computes
    `msb(bitmap)` and then applies `clearBit`. -/
def MsbClearTrace : Nat → List Nat → Prop
  | bitmap, [] => bitmapSchedule bitmap = []
  | bitmap, index :: rest =>
      HighestBitmapBit bitmap MAX_COLLATERALS index ∧
        MsbClearTrace (clearBitmapBit bitmap index) rest

/-- Count active candidate bits below a bound, matching `UtilsLib.countBits`
    over a `uint128` value when the bound is 128. -/
def countBitsFrom (active : Nat → Bool) : Nat → Nat
  | 0 => 0
  | n + 1 =>
      (if active n then 1 else 0) + countBitsFrom active n

def countBits (bitmap : Nat) : Nat :=
  countBitsFrom (fun index => bitmap.testBit index) MAX_COLLATERALS

def ValidBitmapSchedule (bitmap : Nat) : Prop :=
  bitmap < UINT128_BOUND ∧ (bitmapSchedule bitmap).length ≤ MAX_COLLATERALS_PER_BORROWER

def ValidCollateralBitmap (bitmap : Nat) : Prop :=
  bitmap < UINT128_BOUND ∧ countBits bitmap ≤ MAX_COLLATERALS_PER_BORROWER

theorem bitmapScheduleFrom_strict
    (active : Nat → Bool) (bound : Nat) :
    StrictDescendingBelow bound (bitmapScheduleFrom active bound) := by
  induction bound with
  | zero =>
      simp [bitmapScheduleFrom, StrictDescendingBelow]
  | succ n ih =>
      simp [bitmapScheduleFrom]
      split
      · constructor
        · exact Nat.lt_succ_self n
        · exact ih
      · exact StrictDescendingBelow.mono_bound ih (Nat.le_succ n)

theorem bitmapSchedule_strict (bitmap : Nat) :
    StrictDescendingBelow MAX_COLLATERALS (bitmapSchedule bitmap) := by
  unfold bitmapSchedule
  exact bitmapScheduleFrom_strict (fun index => bitmap.testBit index) MAX_COLLATERALS

theorem bitmapSchedule_valid
    (bitmap : Nat)
    (hcount : (bitmapSchedule bitmap).length ≤ MAX_COLLATERALS_PER_BORROWER) :
    ValidSchedule (bitmapSchedule bitmap) :=
  ⟨bitmapSchedule_strict bitmap, hcount⟩

theorem bitmapScheduleFrom_length_eq_countBitsFrom
    (active : Nat → Bool) (bound : Nat) :
    (bitmapScheduleFrom active bound).length = countBitsFrom active bound := by
  induction bound with
  | zero =>
      simp [bitmapScheduleFrom, countBitsFrom]
  | succ n ih =>
      simp [bitmapScheduleFrom, countBitsFrom]
      split
      · simp [ih, Nat.add_comm]
      · simp [ih]

theorem bitmapSchedule_length_eq_countBits (bitmap : Nat) :
    (bitmapSchedule bitmap).length = countBits bitmap := by
  unfold bitmapSchedule countBits
  exact bitmapScheduleFrom_length_eq_countBitsFrom
    (fun index => bitmap.testBit index) MAX_COLLATERALS

theorem validBitmapSchedule_valid
    {bitmap : Nat} (h : ValidBitmapSchedule bitmap) :
    ValidSchedule (bitmapSchedule bitmap) :=
  bitmapSchedule_valid bitmap h.2

theorem validCollateralBitmap_validBitmapSchedule
    {bitmap : Nat} (h : ValidCollateralBitmap bitmap) :
    ValidBitmapSchedule bitmap := by
  constructor
  · exact h.1
  · rw [bitmapSchedule_length_eq_countBits]
    exact h.2

theorem validCollateralBitmap_valid
    {bitmap : Nat} (h : ValidCollateralBitmap bitmap) :
    ValidSchedule (bitmapSchedule bitmap) :=
  validBitmapSchedule_valid (validCollateralBitmap_validBitmapSchedule h)

theorem bitmapSchedule_index_lt_128
    (bitmap : Nat) {index : Nat}
    (hmem : index ∈ bitmapSchedule bitmap) :
    index < MAX_COLLATERALS :=
  StrictDescendingBelow.mem_lt_bound (bitmapSchedule_strict bitmap) hmem

theorem bitmapScheduleFrom_mem_iff
    (active : Nat → Bool) (bound index : Nat) :
    index ∈ bitmapScheduleFrom active bound ↔ index < bound ∧ active index = true := by
  induction bound generalizing index with
  | zero =>
      simp [bitmapScheduleFrom]
  | succ n ih =>
      by_cases htop : active n = true
      · simp [bitmapScheduleFrom, htop]
        constructor
        · intro h
          rcases h with hhead | htail
          · subst index
            exact ⟨Nat.lt_succ_self n, htop⟩
          · have htail' := (ih index).mp htail
            exact ⟨by omega, htail'.2⟩
        · intro h
          rcases h with ⟨hindex, hactive⟩
          by_cases hindexTop : index = n
          · exact Or.inl hindexTop
          · exact Or.inr ((ih index).mpr ⟨by omega, hactive⟩)
      · have hactiveTopFalse : active n = false := by
          cases h : active n <;> simp_all
        simp [bitmapScheduleFrom, hactiveTopFalse]
        constructor
        · intro h
          have h' := (ih index).mp h
          exact ⟨by omega, h'.2⟩
        · intro h
          rcases h with ⟨hindex, hactive⟩
          by_cases htop : index = n
          · subst htop
            simp [hactiveTopFalse] at hactive
          · exact (ih index).mpr ⟨by omega, hactive⟩

theorem bitmapSchedule_mem_iff_testBit
    (bitmap index : Nat) :
    index ∈ bitmapSchedule bitmap ↔
      index < MAX_COLLATERALS ∧ bitmap.testBit index = true := by
  unfold bitmapSchedule
  exact bitmapScheduleFrom_mem_iff (fun index => bitmap.testBit index)
    MAX_COLLATERALS index

theorem bitmapSchedule_mem_testBit
    {bitmap index : Nat} (hmem : index ∈ bitmapSchedule bitmap) :
    bitmap.testBit index = true :=
  (bitmapSchedule_mem_iff_testBit bitmap index).mp hmem |>.2

theorem clearBitmapBit_testBit_below
    (bitmap : Nat) {clearedIndex index : Nat}
    (hcleared : clearedIndex < MAX_COLLATERALS)
    (hindex : index < MAX_COLLATERALS) :
    (clearBitmapBit bitmap clearedIndex).testBit index =
      clearActiveBit (fun index => bitmap.testBit index) clearedIndex index := by
  have hpow : 2 ^ clearedIndex < 2 ^ MAX_COLLATERALS :=
    Nat.pow_lt_pow_right (by decide : 1 < 2) hcleared
  rw [clearBitmapBit]
  simp only [hcleared, ↓reduceIte]
  rw [Nat.testBit_and]
  unfold UINT128_BOUND
  rw [Nat.testBit_two_pow_sub_succ hpow]
  by_cases hsame : index = clearedIndex
  · subst hsame
    simp [clearActiveBit]
  · have hbit : (2 ^ clearedIndex).testBit index = false :=
      Nat.testBit_two_pow_of_ne (by
        intro h
        exact hsame h.symm)
    simp [clearActiveBit, hsame, hindex, hbit]

theorem clearBitmapBit_testBit_cleared
    (bitmap : Nat) {clearedIndex : Nat}
    (hcleared : clearedIndex < MAX_COLLATERALS) :
    (clearBitmapBit bitmap clearedIndex).testBit clearedIndex = false := by
  have h :=
    clearBitmapBit_testBit_below bitmap hcleared hcleared
  simpa [clearActiveBit] using h

theorem bitmapScheduleFrom_congr_below
    {active other : Nat → Bool} {bound : Nat}
    (h : ∀ index, index < bound → active index = other index) :
    bitmapScheduleFrom active bound = bitmapScheduleFrom other bound := by
  induction bound with
  | zero =>
      simp [bitmapScheduleFrom]
  | succ n ih =>
      have htop : active n = other n := h n (Nat.lt_succ_self n)
      have hbelow : ∀ index, index < n → active index = other index := by
        intro index hindex
        exact h index (Nat.lt_trans hindex (Nat.lt_succ_self n))
      simp [bitmapScheduleFrom, htop, ih hbelow]

theorem bitmapSchedule_clearBitmapBit_eq_clearActiveBit
    (bitmap : Nat) {clearedIndex : Nat}
    (hcleared : clearedIndex < MAX_COLLATERALS) :
    bitmapSchedule (clearBitmapBit bitmap clearedIndex) =
      bitmapScheduleFrom
        (clearActiveBit (fun index => bitmap.testBit index) clearedIndex)
        MAX_COLLATERALS := by
  unfold bitmapSchedule
  exact bitmapScheduleFrom_congr_below (by
    intro index hindex
    exact clearBitmapBit_testBit_below bitmap hcleared hindex)

theorem bitmapScheduleFrom_clear_above_eq
    (active : Nat → Bool) {bound clearedIndex : Nat}
    (hbound : bound ≤ clearedIndex) :
    bitmapScheduleFrom (clearActiveBit active clearedIndex) bound =
      bitmapScheduleFrom active bound := by
  induction bound with
  | zero =>
      simp [bitmapScheduleFrom]
  | succ n ih =>
      have hne : n ≠ clearedIndex := by omega
      have hboundN : n ≤ clearedIndex := by omega
      simp [bitmapScheduleFrom, clearActiveBit, hne, ih hboundN]

theorem bitmapScheduleFrom_highest_eq_cons
    (active : Nat → Bool) {bound index : Nat}
    (hindex : index < bound)
    (hactive : active index = true)
    (hhighest : ∀ j, index < j → j < bound → active j = false) :
    bitmapScheduleFrom active bound = index :: bitmapScheduleFrom active index := by
  induction bound with
  | zero =>
      omega
  | succ n ih =>
      by_cases hindexTop : index = n
      · subst hindexTop
        simp [bitmapScheduleFrom, hactive]
      · have hindexLtN : index < n := by omega
        have htopInactive : active n = false := hhighest n hindexLtN (Nat.lt_succ_self n)
        have hhighestN : ∀ j, index < j → j < n → active j = false := by
          intro j hij hjn
          exact hhighest j hij (Nat.lt_trans hjn (Nat.lt_succ_self n))
        simp [bitmapScheduleFrom, htopInactive, ih hindexLtN hhighestN]

theorem bitmapScheduleFrom_clear_highest_eq_tail
    (active : Nat → Bool) {bound index : Nat}
    (hindex : index < bound)
    (hhighest : ∀ j, index < j → j < bound → active j = false) :
    bitmapScheduleFrom (clearActiveBit active index) bound =
      bitmapScheduleFrom active index := by
  induction bound with
  | zero =>
      omega
  | succ n ih =>
      by_cases hindexTop : index = n
      · subst hindexTop
        simp [bitmapScheduleFrom, clearActiveBit,
          bitmapScheduleFrom_clear_above_eq active (Nat.le_refl index)]
      · have hindexLtN : index < n := by omega
        have htopInactive : active n = false := hhighest n hindexLtN (Nat.lt_succ_self n)
        have hclearTopInactive : clearActiveBit active index n = false := by
          have hne : n ≠ index := by
            intro h
            exact hindexTop h.symm
          simp [clearActiveBit, hne, htopInactive]
        have hhighestN : ∀ j, index < j → j < n → active j = false := by
          intro j hij hjn
          exact hhighest j hij (Nat.lt_trans hjn (Nat.lt_succ_self n))
        simp [bitmapScheduleFrom, hclearTopInactive, ih hindexLtN hhighestN]

theorem bitmapScheduleFrom_clear_highest_eq_tail_of_cons
    (active : Nat → Bool) {bound index : Nat}
    (hindex : index < bound)
    (hactive : active index = true)
    (hhighest : ∀ j, index < j → j < bound → active j = false) :
    bitmapScheduleFrom active bound =
      index :: bitmapScheduleFrom (clearActiveBit active index) bound := by
  rw [bitmapScheduleFrom_highest_eq_cons active hindex hactive hhighest,
    bitmapScheduleFrom_clear_highest_eq_tail active hindex hhighest]

theorem bitmapScheduleFrom_highestActiveBit_eq_cons
    (active : Nat → Bool) {bound index : Nat}
    (h : HighestActiveBit active bound index) :
    bitmapScheduleFrom active bound =
      index :: bitmapScheduleFrom (clearActiveBit active index) bound :=
  bitmapScheduleFrom_clear_highest_eq_tail_of_cons active h.1 h.2.1 h.2.2

theorem bitmapScheduleFrom_cons_highestActiveBit
    (active : Nat → Bool) {bound index : Nat} {rest : List Nat}
    (h : bitmapScheduleFrom active bound = index :: rest) :
    HighestActiveBit active bound index := by
  have hmem : index ∈ bitmapScheduleFrom active bound := by
    rw [h]
    simp
  have hindexActive := (bitmapScheduleFrom_mem_iff active bound index).mp hmem
  refine ⟨hindexActive.1, hindexActive.2, ?_⟩
  intro j hij hjbound
  cases hactiveJ : active j with
  | false =>
      rfl
  | true =>
      have hjmem : j ∈ bitmapScheduleFrom active bound :=
        (bitmapScheduleFrom_mem_iff active bound j).mpr ⟨hjbound, hactiveJ⟩
      have hstrict : StrictDescendingBelow bound (index :: rest) := by
        rw [← h]
        exact bitmapScheduleFrom_strict active bound
      rw [h] at hjmem
      simp at hjmem
      rcases hjmem with hhead | htail
      · omega
      · rcases hstrict with ⟨_hindexBound, hrestStrict⟩
        have hjlt : j < index :=
          StrictDescendingBelow.mem_lt_bound hrestStrict htail
        omega

theorem bitmapSchedule_highestBitmapBit_eq_cons
    {bitmap index : Nat}
    (h : HighestBitmapBit bitmap MAX_COLLATERALS index) :
    bitmapSchedule bitmap =
      index ::
        bitmapScheduleFrom
          (clearActiveBit (fun index => bitmap.testBit index) index)
          MAX_COLLATERALS := by
  unfold bitmapSchedule HighestBitmapBit at *
  exact bitmapScheduleFrom_highestActiveBit_eq_cons
    (fun index => bitmap.testBit index) h

theorem bitmapSchedule_clearBitmapBit_highest_eq_tail
    {bitmap index : Nat}
    (h : HighestBitmapBit bitmap MAX_COLLATERALS index) :
    bitmapSchedule bitmap =
      index :: bitmapSchedule (clearBitmapBit bitmap index) := by
  rw [bitmapSchedule_highestBitmapBit_eq_cons h,
    bitmapSchedule_clearBitmapBit_eq_clearActiveBit bitmap h.1]

theorem countBits_clearBitmapBit_highest_eq_tail_count
    {bitmap index : Nat}
    (h : HighestBitmapBit bitmap MAX_COLLATERALS index) :
    countBits bitmap = countBits (clearBitmapBit bitmap index) + 1 := by
  rw [← bitmapSchedule_length_eq_countBits bitmap,
    ← bitmapSchedule_length_eq_countBits (clearBitmapBit bitmap index),
    bitmapSchedule_clearBitmapBit_highest_eq_tail h]
  simp [Nat.add_comm]

theorem bitmapSchedule_clearBitmapBit_highest_valid
    {bitmap index : Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : HighestBitmapBit bitmap MAX_COLLATERALS index) :
    ValidSchedule (bitmapSchedule (clearBitmapBit bitmap index)) := by
  constructor
  · exact bitmapSchedule_strict (clearBitmapBit bitmap index)
  · rw [bitmapSchedule_length_eq_countBits]
    have hcount := hvalid.2
    rw [countBits_clearBitmapBit_highest_eq_tail_count h] at hcount
    omega

theorem bitmapSchedule_cons_highestBitmapBit
    {bitmap index : Nat} {rest : List Nat}
    (h : bitmapSchedule bitmap = index :: rest) :
    HighestBitmapBit bitmap MAX_COLLATERALS index := by
  unfold bitmapSchedule HighestBitmapBit at *
  exact bitmapScheduleFrom_cons_highestActiveBit
    (fun index => bitmap.testBit index) h

theorem bitmapSchedule_clearBitmapBit_cons_eq_tail
    {bitmap index : Nat} {rest : List Nat}
    (h : bitmapSchedule bitmap = index :: rest) :
    bitmapSchedule (clearBitmapBit bitmap index) = rest := by
  have htail := bitmapSchedule_clearBitmapBit_highest_eq_tail
    (bitmapSchedule_cons_highestBitmapBit h)
  rw [h] at htail
  exact (List.cons.inj htail.symm).2

theorem countBits_clearBitmapBit_cons_eq_tail_count
    {bitmap index : Nat} {rest : List Nat}
    (h : bitmapSchedule bitmap = index :: rest) :
    countBits (clearBitmapBit bitmap index) = rest.length := by
  rw [← bitmapSchedule_length_eq_countBits (clearBitmapBit bitmap index),
    bitmapSchedule_clearBitmapBit_cons_eq_tail h]

theorem bitmapSchedule_clearBitmapBit_cons_valid
    {bitmap index : Nat} {rest : List Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : bitmapSchedule bitmap = index :: rest) :
    ValidSchedule (bitmapSchedule (clearBitmapBit bitmap index)) :=
  bitmapSchedule_clearBitmapBit_highest_valid hvalid
    (bitmapSchedule_cons_highestBitmapBit h)

theorem bitmapSchedule_clearBitmapBits_prefix_eq_suffix
    {bitmap : Nat} (pref suff : List Nat)
    (h : bitmapSchedule bitmap = pref ++ suff) :
    bitmapSchedule (clearBitmapBits bitmap pref) = suff := by
  induction pref generalizing bitmap suff with
  | nil =>
      simpa [clearBitmapBits] using h
  | cons index rest ih =>
      have hcons : bitmapSchedule bitmap = index :: (rest ++ suff) := by
        simpa using h
      have htail :
          bitmapSchedule (clearBitmapBit bitmap index) = rest ++ suff :=
        bitmapSchedule_clearBitmapBit_cons_eq_tail hcons
      simpa [clearBitmapBits] using
        ih (bitmap := clearBitmapBit bitmap index) (suff := suff) htail

theorem countBits_clearBitmapBits_prefix_eq_suffix_length
    {bitmap : Nat} (pref suff : List Nat)
    (h : bitmapSchedule bitmap = pref ++ suff) :
    countBits (clearBitmapBits bitmap pref) = suff.length := by
  rw [← bitmapSchedule_length_eq_countBits (clearBitmapBits bitmap pref),
    bitmapSchedule_clearBitmapBits_prefix_eq_suffix pref suff h]

theorem bitmapSchedule_clearBitmapBits_prefix_valid
    {bitmap : Nat} (pref suff : List Nat)
    (hvalid : ValidCollateralBitmap bitmap)
    (h : bitmapSchedule bitmap = pref ++ suff) :
    ValidSchedule (bitmapSchedule (clearBitmapBits bitmap pref)) := by
  constructor
  · exact bitmapSchedule_strict (clearBitmapBits bitmap pref)
  · rw [bitmapSchedule_clearBitmapBits_prefix_eq_suffix pref suff h]
    have hlen :
        (pref ++ suff).length ≤ MAX_COLLATERALS_PER_BORROWER := by
      rw [← h, bitmapSchedule_length_eq_countBits]
      exact hvalid.2
    rw [List.length_append] at hlen
    omega

theorem msbClearTrace_eq_bitmapSchedule
    {bitmap : Nat} {indices : List Nat}
    (h : MsbClearTrace bitmap indices) :
    indices = bitmapSchedule bitmap := by
  induction indices generalizing bitmap with
  | nil =>
      simpa [MsbClearTrace] using h.symm
  | cons index rest ih =>
      rcases h with ⟨hhighest, hrest⟩
      have hhead := bitmapSchedule_clearBitmapBit_highest_eq_tail hhighest
      have htail : rest = bitmapSchedule (clearBitmapBit bitmap index) :=
        ih hrest
      rw [htail, hhead]

theorem msbClearTrace_of_bitmapSchedule
    (bitmap : Nat) :
    MsbClearTrace bitmap (bitmapSchedule bitmap) := by
  induction hsched : bitmapSchedule bitmap generalizing bitmap with
  | nil =>
      exact hsched
  | cons index rest ih =>
      have hhighest : HighestBitmapBit bitmap MAX_COLLATERALS index :=
        bitmapSchedule_cons_highestBitmapBit hsched
      constructor
      · exact hhighest
      · have htail :
          bitmapSchedule (clearBitmapBit bitmap index) = rest :=
          bitmapSchedule_clearBitmapBit_cons_eq_tail hsched
        exact ih (bitmap := clearBitmapBit bitmap index) htail

theorem msbClearTrace_iff_eq_bitmapSchedule
    (bitmap : Nat) (indices : List Nat) :
    MsbClearTrace bitmap indices ↔ indices = bitmapSchedule bitmap :=
  ⟨msbClearTrace_eq_bitmapSchedule, by
    intro h
    rw [h]
    exact msbClearTrace_of_bitmapSchedule bitmap⟩

theorem msbClearTrace_clearPrefix
    {bitmap : Nat} {indices pref suff : List Nat}
    (htrace : MsbClearTrace bitmap indices)
    (hsplit : indices = pref ++ suff) :
    MsbClearTrace (clearBitmapBits bitmap pref) suff := by
  have hschedule : bitmapSchedule bitmap = pref ++ suff := by
    rw [← msbClearTrace_eq_bitmapSchedule htrace]
    exact hsplit
  rw [msbClearTrace_iff_eq_bitmapSchedule]
  exact (bitmapSchedule_clearBitmapBits_prefix_eq_suffix pref suff hschedule).symm

theorem msbClearTrace_highestAfterPrefix
    {bitmap : Nat} {indices pref rest : List Nat} {index : Nat}
    (htrace : MsbClearTrace bitmap indices)
    (hsplit : indices = pref ++ index :: rest) :
    HighestBitmapBit (clearBitmapBits bitmap pref) MAX_COLLATERALS index := by
  have hremaining : MsbClearTrace (clearBitmapBits bitmap pref) (index :: rest) :=
    msbClearTrace_clearPrefix htrace hsplit
  rcases hremaining with ⟨hhighest, _hrest⟩
  exact hhighest

structure CollateralArrays where
  collateral : Nat → Nat
  price : Nat → Nat
  lltv : Nat → Nat
  maxLif : Nat → Nat

def slotAt (arrays : CollateralArrays) (index : Nat) : CollateralSlot :=
  { index := index,
    collateral := arrays.collateral index,
    price := arrays.price index,
    lltv := arrays.lltv index,
    maxLif := arrays.maxLif index }

def slotsOfIndices (arrays : CollateralArrays) : List Nat → List CollateralSlot
  | [] => []
  | i :: rest => slotAt arrays i :: slotsOfIndices arrays rest

def slotIndicesOf : List CollateralSlot → List Nat
  | [] => []
  | slot :: rest => slot.index :: slotIndicesOf rest

theorem slotsOfIndices_length
    (arrays : CollateralArrays) (indices : List Nat) :
    (slotsOfIndices arrays indices).length = indices.length := by
  induction indices with
  | nil =>
      simp [slotsOfIndices]
  | cons i rest ih =>
      simp [slotsOfIndices, ih]

theorem slotIndicesOf_slotsOfIndices
    (arrays : CollateralArrays) (indices : List Nat) :
    slotIndicesOf (slotsOfIndices arrays indices) = indices := by
  induction indices with
  | nil =>
      simp [slotIndicesOf, slotsOfIndices]
  | cons i rest ih =>
      simp [slotIndicesOf, slotsOfIndices, slotAt, ih]

theorem validSchedule_length_le_128
    {indices : List Nat} (h : ValidSchedule indices) :
    indices.length ≤ MAX_COLLATERALS :=
  StrictDescendingBelow.length_le_bound h.1

theorem validSchedule_index_lt_128
    {indices : List Nat} (h : ValidSchedule indices)
    {index : Nat} (hmem : index ∈ indices) :
    index < MAX_COLLATERALS :=
  StrictDescendingBelow.mem_lt_bound h.1 hmem

theorem validSchedule_nodup
    {indices : List Nat} (h : ValidSchedule indices) :
    indices.Nodup :=
  StrictDescendingBelow.nodup h.1

theorem validSchedule_slots_length_le_16
    (arrays : CollateralArrays) {indices : List Nat}
    (h : ValidSchedule indices) :
    (slotsOfIndices arrays indices).length ≤ MAX_COLLATERALS_PER_BORROWER := by
  rw [slotsOfIndices_length]
  exact h.2

theorem msbClearTrace_length_eq_countBits
    {bitmap : Nat} {indices : List Nat}
    (h : MsbClearTrace bitmap indices) :
    indices.length = countBits bitmap := by
  rw [msbClearTrace_eq_bitmapSchedule h, bitmapSchedule_length_eq_countBits]

theorem msbClearTrace_valid
    {bitmap : Nat} {indices : List Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : MsbClearTrace bitmap indices) :
    ValidSchedule indices := by
  rw [msbClearTrace_eq_bitmapSchedule h]
  exact validCollateralBitmap_valid hvalid

theorem msbClearTrace_nodup
    {bitmap : Nat} {indices : List Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : MsbClearTrace bitmap indices) :
    indices.Nodup :=
  validSchedule_nodup (msbClearTrace_valid hvalid h)

theorem msbClearTrace_index_lt_128
    {bitmap : Nat} {indices : List Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : MsbClearTrace bitmap indices)
    {index : Nat} (hmem : index ∈ indices) :
    index < MAX_COLLATERALS :=
  validSchedule_index_lt_128 (msbClearTrace_valid hvalid h) hmem

theorem msbClearTrace_slots_length_le_16
    (arrays : CollateralArrays)
    {bitmap : Nat} {indices : List Nat}
    (hvalid : ValidCollateralBitmap bitmap)
    (h : MsbClearTrace bitmap indices) :
    (slotsOfIndices arrays indices).length ≤ MAX_COLLATERALS_PER_BORROWER :=
  validSchedule_slots_length_le_16 arrays (msbClearTrace_valid hvalid h)

theorem validSchedule_slots_length_le_128
    (arrays : CollateralArrays) {indices : List Nat}
    (h : ValidSchedule indices) :
    (slotsOfIndices arrays indices).length ≤ MAX_COLLATERALS := by
  rw [slotsOfIndices_length]
  exact validSchedule_length_le_128 h

theorem validSchedule_slotIndex_lt_128
    (arrays : CollateralArrays) {indices : List Nat}
    (h : ValidSchedule indices)
    {slot : CollateralSlot} (hmem : slot ∈ slotsOfIndices arrays indices) :
    slot.index < MAX_COLLATERALS := by
  induction indices with
  | nil =>
      simp [slotsOfIndices] at hmem
  | cons i rest ih =>
      simp [slotsOfIndices, slotAt] at hmem
      rcases hmem with hhead | htail
      · rcases hhead
        exact validSchedule_index_lt_128 h (by simp)
      · have htailSchedule : ValidSchedule rest := by
          rcases h with ⟨hdesc, hlen⟩
          rcases hdesc with ⟨_hi, hrest⟩
          constructor
          · exact StrictDescendingBelow.mono_bound hrest (by omega)
          · simp at hlen
            omega
        exact ih htailSchedule htail

theorem validSchedule_slotIndices_nodup
    (arrays : CollateralArrays) {indices : List Nat}
    (h : ValidSchedule indices) :
    (slotIndicesOf (slotsOfIndices arrays indices)).Nodup := by
  rw [slotIndicesOf_slotsOfIndices]
  exact validSchedule_nodup h

theorem slotsOfIndices_append
    (arrays : CollateralArrays) (left right : List Nat) :
    slotsOfIndices arrays (left ++ right) =
      slotsOfIndices arrays left ++ slotsOfIndices arrays right := by
  induction left with
  | nil =>
      simp [slotsOfIndices]
  | cons i rest ih =>
      simp [slotsOfIndices, ih]

theorem slotsOfIndices_split_selected
    (arrays : CollateralArrays)
    (before after : List Nat) (selectedIndex : Nat) :
    slotsOfIndices arrays (before ++ selectedIndex :: after) =
      slotsOfIndices arrays before ++ slotAt arrays selectedIndex ::
        slotsOfIndices arrays after := by
  rw [slotsOfIndices_append]
  simp [slotsOfIndices]

theorem noSlotWithIndex_slotsOfIndices_of_not_mem
    (arrays : CollateralArrays) (selectedIndex : Nat) {indices : List Nat}
    (hnotMem : selectedIndex ∉ indices) :
    noSlotWithIndex selectedIndex (slotsOfIndices arrays indices) := by
  induction indices with
  | nil =>
      simp [slotsOfIndices, noSlotWithIndex]
  | cons i rest ih =>
      simp [slotsOfIndices, slotAt, noSlotWithIndex] at *
      rcases hnotMem with ⟨hneq, hnotRest⟩
      constructor
      · intro h
        exact hneq h.symm
      · exact ih hnotRest

theorem selectedPriceAfter_split_selectedIndex
    (arrays : CollateralArrays)
    (before after : List Nat) (selectedIndex initialPrice : Nat)
    (hnodup : (before ++ selectedIndex :: after).Nodup) :
    selectedPriceAfter selectedIndex initialPrice
        (slotsOfIndices arrays (before ++ selectedIndex :: after)) =
      arrays.price selectedIndex := by
  rw [slotsOfIndices_split_selected]
  have hnotAfter : selectedIndex ∉ after := by
    have htail :
        (selectedIndex :: after).Nodup := by
      exact (List.nodup_append.mp hnodup).2.1
    exact (List.nodup_cons.mp htail).1
  simpa [slotAt] using
    selectedPriceAfter_split_selected initialPrice
      (slotsOfIndices arrays before) (slotsOfIndices arrays after)
      (slotAt arrays selectedIndex)
      (noSlotWithIndex_slotsOfIndices_of_not_mem arrays selectedIndex hnotAfter)

/-- Bridge from the bitmap schedule to the selected-plus-other RCF max-debt
    projection. If the liquidated collateral index appears in the schedule, the
    folded active-collateral max-debt equals the selected contribution plus all
    other visited contributions. -/
theorem sumMaxDebtContributions_split_selectedIndex
    (arrays : CollateralArrays)
    (before after : List Nat) (selectedIndex : Nat) :
    sumMaxDebtContributions
        (slotsOfIndices arrays (before ++ selectedIndex :: after)) =
      RCF.maxDebtAfterCollateralLoop
        (maxDebtContribution (slotAt arrays selectedIndex))
        (maxDebtContributionsOf (slotsOfIndices arrays before) ++
          maxDebtContributionsOf (slotsOfIndices arrays after)) := by
  rw [slotsOfIndices_split_selected]
  exact sumMaxDebtContributions_split_selected
    (slotsOfIndices arrays before) (slotsOfIndices arrays after)
    (slotAt arrays selectedIndex)

end Midnight.Proofs.BitmapSchedule
