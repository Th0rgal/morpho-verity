/-
  HealthFaithful — discharging the `HealthFaithful` obligation under an explicit
  no-overflow side condition.
-/

import Morpho.Proofs.Projection
import Verity.Proofs.Stdlib.Math
import Contracts.PackedWordLemmas
import Mathlib.Data.Nat.Bitwise

namespace Morpho.Proofs.HealthFaithful

open Verity
open Verity.Stdlib.Math
open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Projection
open Morpho.Contract.Morpho (MarketParams structMember structMember2 _isHealthyWithPrice)

private lemma uint256_ofNat_testBit (x i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.ofNat x).val i
      = (decide (i < 256) && Nat.testBit x i) := by
  show Nat.testBit (x % 2 ^ 256) i = _
  exact Nat.testBit_mod_two_pow x 256 i

private lemma uint256_and_testBit (a b : Uint256) (i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.and a b).val i
      = (decide (i < 256) && (Nat.testBit a.val i && Nat.testBit b.val i)) := by
  show Nat.testBit (_root_.Verity.Core.Uint256.ofNat (a.val &&& b.val)).val i = _
  rw [uint256_ofNat_testBit, Nat.testBit_and]

private lemma uint256_shr_testBit (s v : Uint256) (i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.shr s v).val i
      = (decide (i < 256) && Nat.testBit v.val (s.val + i)) := by
  show Nat.testBit (_root_.Verity.Core.Uint256.ofNat (v.val >>> s.val)).val i = _
  rw [uint256_ofNat_testBit, Nat.testBit_shiftRight]

private lemma packedMask_testBit (w i : Nat) :
    Nat.testBit (Contracts.packedMask w) i = decide (i < w) := by
  unfold Contracts.packedMask
  exact Nat.testBit_two_pow_sub_one w i

private lemma uint256_packedMask_testBit (w i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.ofNat (Contracts.packedMask w)).val i
      = (decide (i < 256) && decide (i < w)) := by
  rw [uint256_ofNat_testBit, packedMask_testBit]

private theorem decodePackedWord_lt_width (word : Uint256) (offset width : Nat) :
    (Contracts.decodePackedWord word offset width).val < 2 ^ width := by
  unfold Contracts.decodePackedWord
  apply Nat.lt_of_testBit width
  · simp only [uint256_and_testBit, uint256_shr_testBit, uint256_packedMask_testBit]
    simp
  · simp
  · intro j hj
    simp only [uint256_and_testBit, uint256_shr_testBit, uint256_packedMask_testBit]
    have hjw : ¬ j < width := by omega
    have hne : width ≠ j := by omega
    simp [hjw, Nat.testBit_two_pow_of_ne hne]

private theorem runWord_structMember2At_packed_lt {κ₁ κ₂ : Type}
    [Contracts.StorageKey κ₁] [Contracts.StorageKey κ₂] (base wo offset width : Nat)
    (k1 : κ₁) (k2 : κ₂) (cs : ContractState) :
    (runWord (Contracts.structMember2At base wo ((some (offset, width)) : Option (Nat × Nat))
      k1 k2 : Contract Uint256) cs).val < 2 ^ width := by
  unfold runWord Contracts.structMember2At
  dsimp only
  exact decodePackedWord_lt_width _ _ _

private theorem runWord_structMemberAt_packed_lt {κ : Type}
    [Contracts.StorageKey κ] (base wo offset width : Nat)
    (k : κ) (cs : ContractState) :
    (runWord (Contracts.structMemberAt base wo ((some (offset, width)) : Option (Nat × Nat))
      k : Contract Uint256) cs).val < 2 ^ width := by
  unfold runWord Contracts.structMemberAt
  dsimp only
  exact decodePackedWord_lt_width _ _ _

private theorem runWord_borrowShares_lt_128 (id : Bytes32) (account : Address)
    (cs : ContractState) :
    (runWord (Contracts.structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs).val < 2 ^ 128 :=
  runWord_structMember2At_packed_lt 2 1 0 128 id account cs

private theorem runWord_totalBorrowAssets_lt_128 (id : Bytes32) (cs : ContractState) :
    (runWord (Contracts.structMemberAt 3 1 ((some (0, 128)) : Option (Nat × Nat))
      id : Contract Uint256) cs).val < 2 ^ 128 :=
  runWord_structMemberAt_packed_lt 3 1 0 128 id cs

private theorem runWord_totalBorrowShares_lt_128 (id : Bytes32) (cs : ContractState) :
    (runWord (Contracts.structMemberAt 3 1 ((some (128, 128)) : Option (Nat × Nat))
      id : Contract Uint256) cs).val < 2 ^ 128 :=
  runWord_structMemberAt_packed_lt 3 1 128 128 id cs

/-! ## Concrete reductions of the four storage reads done by `_isHealthyWithPrice`. -/

private lemma sm2_borrowShares (id : Bytes32) (account : Address) (cs : ContractState) :
    Morpho.Contract.Morpho.structMember2 (α := Uint256) "positionSlot" id account "borrowShares" cs
      = ContractResult.success
          (runWord (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "borrowShares") cs)
          cs := by
  unfold runWord Morpho.Contract.Morpho.structMember2
  simp [Contracts.structMember2At]

private lemma sm2_collateral (id : Bytes32) (account : Address) (cs : ContractState) :
    Morpho.Contract.Morpho.structMember2 (α := Uint256) "positionSlot" id account "collateral" cs
      = ContractResult.success
          (runWord (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "collateral") cs)
          cs := by
  unfold runWord Morpho.Contract.Morpho.structMember2
  simp [Contracts.structMember2At]

private lemma sm_totalBorrowAssets (id : Bytes32) (cs : ContractState) :
    Morpho.Contract.Morpho.structMember (α := Uint256) "marketSlot" id "totalBorrowAssets" cs
      = ContractResult.success
          (runWord (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets") cs)
          cs := by
  unfold runWord Morpho.Contract.Morpho.structMember
  simp [Contracts.structMemberAt]

private lemma sm_totalBorrowShares (id : Bytes32) (cs : ContractState) :
    Morpho.Contract.Morpho.structMember (α := Uint256) "marketSlot" id "totalBorrowShares" cs
      = ContractResult.success
          (runWord (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares") cs)
          cs := by
  unfold runWord Morpho.Contract.Morpho.structMember
  simp [Contracts.structMemberAt]

/-! ## `addPanic` reduction (no-overflow → identity). -/

private lemma addPanic_run_succ (a b : Uint256) (cs : ContractState)
    (h : a.val + b.val < Verity.Core.Uint256.modulus) :
    addPanic a b cs = ContractResult.success (Verity.Core.Uint256.ofNat (a.val + b.val)) cs := by
  have hle : ¬ (a.val + b.val > MAX_UINT256) := by
    show ¬ (a.val + b.val > Verity.Core.MAX_UINT256)
    have hmax := Verity.Core.Uint256.max_uint256_succ_eq_modulus
    show ¬ (a.val + b.val > Verity.Core.MAX_UINT256)
    have : Verity.Core.MAX_UINT256 + 1 = Verity.Core.Uint256.modulus := hmax
    omega
  unfold addPanic requireSomeUint safeAdd
  simp only [hle, ↓reduceIte]
  -- Goal: (do pure (a + b)) cs = success (Verity.Core.Uint256.ofNat (a.val+b.val)) cs
  have : (a + b : Uint256) = Verity.Core.Uint256.ofNat (a.val + b.val) := by
    apply Verity.Core.Uint256.ext
    show (Verity.Core.Uint256.add a b).val = (Verity.Core.Uint256.ofNat (a.val + b.val)).val
    simp [Verity.Core.Uint256.add]
  -- Need to step through bind/pure
  show ContractResult.success (a + b) cs = ContractResult.success _ cs
  rw [this]

/-! ## `Morpho.Contract.mulDivDown` / `mulDivUp` reduce to the model under bounds. -/

/-- `.val` of `Verity.Core.Uint256.mul x y` when the product fits. -/
private lemma uint256_mul_val_of_lt (x y : Uint256)
    (h : x.val * y.val < Verity.Core.Uint256.modulus) :
    (Verity.Core.Uint256.mul x y).val = x.val * y.val := by
  unfold Verity.Core.Uint256.mul
  rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h]

private lemma uint256_add_val_of_lt (a b : Uint256)
    (h : a.val + b.val < Verity.Core.Uint256.modulus) :
    (Verity.Core.Uint256.add a b).val = a.val + b.val := by
  unfold Verity.Core.Uint256.add
  rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h]

private lemma uint256_sub_one_val (d : Uint256) (hd : d.val ≠ 0) :
    (Verity.Core.Uint256.sub d 1).val = d.val - 1 := by
  have hd_pos : 0 < d.val := Nat.pos_of_ne_zero hd
  have h1 : (1 : Uint256).val = 1 := rfl
  have hle : (1 : Uint256).val ≤ d.val := h1 ▸ hd_pos
  have hlt : d.val - 1 < Verity.Core.Uint256.modulus :=
    Nat.lt_of_le_of_lt (Nat.sub_le _ _) d.isLt
  show (if (1 : Uint256).val ≤ d.val then
          Verity.Core.Uint256.ofNat (d.val - (1 : Uint256).val)
        else
          Verity.Core.Uint256.ofNat
            (Verity.Core.Uint256.modulus - ((1 : Uint256).val - d.val))).val = d.val - 1
  rw [if_pos hle, h1, Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt hlt]

private lemma uint256_div_val_of_ne (a d : Uint256) (hd : d.val ≠ 0)
    (h : a.val / d.val < Verity.Core.Uint256.modulus) :
    (Verity.Core.Uint256.div a d).val = a.val / d.val := by
  unfold Verity.Core.Uint256.div
  simp only [hd, ↓reduceIte]
  rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h]

private lemma mulDivDown_val
    (x y d : Uint256)
    (h_xy : x.val * y.val < Verity.Core.Uint256.modulus)
    (h_q : (x.val * y.val) / d.val < Verity.Core.Uint256.modulus) :
    (Morpho.Contract.mulDivDown x y d).val =
      Morpho.Proofs.HealthModel.mulDivDown x.val y.val d.val := by
  unfold Morpho.Contract.mulDivDown Morpho.Contract.div Morpho.Contract.mul Morpho.Proofs.HealthModel.mulDivDown
  by_cases hd : d.val = 0
  · -- d.val = 0: `Uint256.div` returns `ofNat 0`; `Nat` division by 0 is 0.
    unfold Verity.Core.Uint256.div
    simp [hd]
  · have hmul : (Verity.Core.Uint256.mul x y).val = x.val * y.val :=
      uint256_mul_val_of_lt x y h_xy
    have hq' : (Verity.Core.Uint256.mul x y).val / d.val
              < Verity.Core.Uint256.modulus := by
      rw [hmul]; exact h_q
    have : (Verity.Core.Uint256.div (Verity.Core.Uint256.mul x y) d).val
        = (Verity.Core.Uint256.mul x y).val / d.val :=
      uint256_div_val_of_ne _ _ hd hq'
    rw [this, hmul]

private lemma mulDivUp_val
    (x y d : Uint256)
    (h_xy : x.val * y.val < Verity.Core.Uint256.modulus)
    (h_num : x.val * y.val + (d.val - 1) < Verity.Core.Uint256.modulus)
    (h_q : (x.val * y.val + (d.val - 1)) / d.val < Verity.Core.Uint256.modulus) :
    (Morpho.Contract.mulDivUp x y d).val =
      Morpho.Proofs.HealthModel.mulDivUp x.val y.val d.val := by
  unfold Morpho.Contract.mulDivUp Morpho.Contract.div Morpho.Contract.add
    Morpho.Contract.mul Morpho.Contract.sub Morpho.Proofs.HealthModel.mulDivUp
  by_cases hd : d.val = 0
  · -- Both sides reduce to 0 when d.val = 0.
    unfold Verity.Core.Uint256.div
    simp [hd]
  · have hmul : (Verity.Core.Uint256.mul x y).val = x.val * y.val :=
      uint256_mul_val_of_lt x y h_xy
    have hsub : (Verity.Core.Uint256.sub d 1).val = d.val - 1 :=
      uint256_sub_one_val d hd
    have hadd : (Verity.Core.Uint256.add (Verity.Core.Uint256.mul x y)
                  (Verity.Core.Uint256.sub d 1)).val
              = x.val * y.val + (d.val - 1) := by
      have := uint256_add_val_of_lt (Verity.Core.Uint256.mul x y)
                (Verity.Core.Uint256.sub d 1) (by rw [hmul, hsub]; exact h_num)
      rw [this, hmul, hsub]
    have hdivq : (x.val * y.val + (d.val - 1)) / d.val < Verity.Core.Uint256.modulus := h_q
    have := uint256_div_val_of_ne
              (Verity.Core.Uint256.add (Verity.Core.Uint256.mul x y) (Verity.Core.Uint256.sub d 1))
              d hd (by rw [hadd]; exact hdivq)
    rw [this, hadd]

private lemma mulDiv512Up_val
    (x y d : Uint256)
    (hd : d.val ≠ 0)
    (h_q : (x.val * y.val + (d.val - 1)) / d.val < Verity.Core.Uint256.modulus) :
    (Verity.Stdlib.Math.mulDiv512Up x y d).val =
      Morpho.Proofs.HealthModel.mulDivUp x.val y.val d.val := by
  unfold Verity.Stdlib.Math.mulDiv512Up Morpho.Proofs.HealthModel.mulDivUp
  have hfit :
      (x.val * y.val + (d.val - 1)) / d.val ≤ Verity.Stdlib.Math.MAX_UINT256 := by
    have hmax := Verity.Core.Uint256.max_uint256_succ_eq_modulus
    show (x.val * y.val + (d.val - 1)) / d.val ≤ Verity.Core.MAX_UINT256
    omega
  rw [Verity.Proofs.Stdlib.Math.mulDiv512Up?_some x y d hd hfit]
  rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h_q]

/-! ## No-overflow predicate -/

/--
  Side condition on the projected state ensuring that every `Uint256.ofNat` wrap
  inside `_isHealthyWithPrice` is the identity (i.e. the Solidity contract would
  not have reverted on a `Panic(0x11)` either).

  Stated in terms of the projected fields (`borrowShares`, …), which equal the
  `.val` of the contract-side `Uint256` words by definition of `project`.

  The four conjuncts cover, in order:
  1. `addPanic totalBorrowAssets 1` does not revert.
  2. `addPanic totalBorrowShares 1000000` does not revert.
  3. The product `collateral * price` for `mulDivDown … 1e36`.
  4. The product `collateralQuoted * lltv` for `mulDivDown … 1e18`.

  The borrow-side share/asset conversion uses the full-precision `mulDiv512Up`
  helper. Its quotient fit is derived from the packed `uint128` storage guards
  in `Disciplines.lean`, so no explicit borrow-side no-overflow assumption is
  needed here. The two final `mulDivDown` quotients are implied by their numerator
  bounds (quotient ≤ numerator when divisor ≥ 1), so they are not stated
  separately.
-/
def NoOverflow (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState) : Prop :=
  let s := project mp id account price cs
  s.totBorrowAssets + 1 < Verity.Core.Uint256.modulus ∧
  s.totBorrowShares + 1000000 < Verity.Core.Uint256.modulus ∧
  s.collateral * s.price < Verity.Core.Uint256.modulus ∧
  Morpho.Proofs.HealthModel.mulDivDown s.collateral s.price ORACLE_PRICE_SCALE * s.lltv
    < Verity.Core.Uint256.modulus

/-! ## `Contract` bind helpers -/

/-- Stepwise reduction of a `Contract` bind whose source is state-preserving.
Stated against `Bind.bind` (the typeclass method) so it matches the term that
the macro-emitted `do` notation desugars to. -/
private lemma bind_run_success {α β : Type}
    (ma : Contract α) (f : α → Contract β) (cs : ContractState) (a : α)
    (h : ma cs = ContractResult.success a cs) :
    (ma >>= f) cs = f a cs := by
  show Verity.bind ma f cs = f a cs
  unfold Verity.bind; rw [h]

/-! ## Specialisation of the contract body -/

/-- `_isHealthyWithPrice` evaluated at `cs` under `NoOverflow`: it returns
`success` with state `cs` and the boolean computed from the model arithmetic. -/
private lemma isHealthyWithPrice_eval
    (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState)
    (h : NoOverflow mp id account price cs) :
    let s := project mp id account price cs
    (_isHealthyWithPrice mp id account price) cs
      = ContractResult.success
          (decide (HealthState.maxBorrow s ≥
            Morpho.Proofs.HealthModel.mulDivUp s.borrowShares (s.totBorrowAssets + 1)
              (s.totBorrowShares + 1000000)))
          cs := by
  -- Unpack `NoOverflow`.
  obtain ⟨h_addA, h_addS, h_xyC, h_xyM⟩ := h
  -- Equate the four storage reads with `(project …).*` via `runWord`.
  have hs_bs : (runWord (Morpho.Contract.Morpho.structMember2
                  "positionSlot" id account "borrowShares") cs).val
              = (project mp id account price cs).borrowShares := rfl
  have hs_tba : (runWord (Morpho.Contract.Morpho.structMember
                  "marketSlot" id "totalBorrowAssets") cs).val
              = (project mp id account price cs).totBorrowAssets := rfl
  have hs_tbs : (runWord (Morpho.Contract.Morpho.structMember
                  "marketSlot" id "totalBorrowShares") cs).val
              = (project mp id account price cs).totBorrowShares := rfl
  have hs_col : (runWord (Morpho.Contract.Morpho.structMember2
                  "positionSlot" id account "collateral") cs).val
              = (project mp id account price cs).collateral := rfl
  have hs_price : price.val = (project mp id account price cs).price := rfl
  have hs_lltv : mp.lltv.val = (project mp id account price cs).lltv := rfl
  set s : HealthState := project mp id account price cs with hs_def
  -- Push the bounds through to `Uint256` words.
  set bs : Uint256 := runWord (Morpho.Contract.Morpho.structMember2
        "positionSlot" id account "borrowShares") cs with hbs_def
  set tba : Uint256 := runWord (Morpho.Contract.Morpho.structMember
        "marketSlot" id "totalBorrowAssets") cs with htba_def
  set tbs : Uint256 := runWord (Morpho.Contract.Morpho.structMember
        "marketSlot" id "totalBorrowShares") cs with htbs_def
  set col : Uint256 := runWord (Morpho.Contract.Morpho.structMember2
        "positionSlot" id account "collateral") cs with hcol_def
  -- Word-level no-overflow hypotheses.
  have hAdd_tba : tba.val + 1 < Verity.Core.Uint256.modulus := by
    rw [hs_tba]; exact h_addA
  have hAdd_tbs : tbs.val + 1000000 < Verity.Core.Uint256.modulus := by
    rw [hs_tbs]; exact h_addS
  -- Unfold the contract body and step through the bind chain.
  unfold _isHealthyWithPrice
  -- bs read
  rw [bind_run_success _ _ cs bs (sm2_borrowShares id account cs)]
  -- tba read
  rw [bind_run_success _ _ cs tba (sm_totalBorrowAssets id cs)]
  -- tbs read
  rw [bind_run_success _ _ cs tbs (sm_totalBorrowShares id cs)]
  -- addPanic tba 1
  rw [bind_run_success _ _ cs (Verity.Core.Uint256.ofNat (tba.val + 1))
        (addPanic_run_succ tba 1 cs hAdd_tba)]
  -- addPanic tbs 1000000
  rw [bind_run_success _ _ cs (Verity.Core.Uint256.ofNat (tbs.val + 1000000))
        (addPanic_run_succ tbs 1000000 cs hAdd_tbs)]
  -- col read
  rw [bind_run_success _ _ cs col (sm2_collateral id account cs)]
  -- Name the contract's intermediate `Uint256` values.
  set totBAplus1 : Uint256 := Verity.Core.Uint256.ofNat (tba.val + 1) with hTBA1
  set totBSplus1e6 : Uint256 := Verity.Core.Uint256.ofNat (tbs.val + 1000000) with hTBS1e6
  set cq : Uint256 := Morpho.Contract.mulDivDown col price
                        1000000000000000000000000000000000000 with hcq_def
  set cmax : Uint256 := Morpho.Contract.mulDivDown cq mp.lltv 1000000000000000000 with hcmax_def
  set cbrw : Uint256 := Verity.Stdlib.Math.mulDiv512Up bs totBAplus1 totBSplus1e6 with hcbrw_def
  -- `.val`-level equalities for each intermediate.
  have hTBA1_val : totBAplus1.val = s.totBorrowAssets + 1 := by
    show (Verity.Core.Uint256.ofNat (tba.val + 1)).val = _
    rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt hAdd_tba, hs_tba]
  have hTBS1e6_val : totBSplus1e6.val = s.totBorrowShares + 1000000 := by
    show (Verity.Core.Uint256.ofNat (tbs.val + 1000000)).val = _
    rw [Verity.Core.Uint256.val_ofNat, Nat.mod_eq_of_lt hAdd_tbs, hs_tbs]
  -- mulDiv512Up borrow conversion: the quotient fits from packed uint128 fields.
  have h_qB' : (bs.val * totBAplus1.val + (totBSplus1e6.val - 1)) / totBSplus1e6.val
              < Verity.Core.Uint256.modulus := by
    rw [hs_bs, hTBA1_val, hTBS1e6_val]
    have hbs : s.borrowShares < 2 ^ 128 := by
      subst s
      exact runWord_borrowShares_lt_128 id account cs
    have htba : s.totBorrowAssets < 2 ^ 128 := by
      subst s
      exact runWord_totalBorrowAssets_lt_128 id cs
    have htbs : s.totBorrowShares < 2 ^ 128 := by
      subst s
      exact runWord_totalBorrowShares_lt_128 id cs
    have hbs_le : s.borrowShares ≤ 2 ^ 128 - 1 := by omega
    have htba_le : s.totBorrowAssets + 1 ≤ 2 ^ 128 := by omega
    have htbs_le : s.totBorrowShares + 1000000 - 1 ≤ (2 ^ 128 - 1) + 1000000 - 1 := by
      omega
    have hnum :
        s.borrowShares * (s.totBorrowAssets + 1) +
            (s.totBorrowShares + 1000000 - 1)
          ≤ (2 ^ 128 - 1) * 2 ^ 128 + ((2 ^ 128 - 1) + 1000000 - 1) := by
      exact Nat.add_le_add (Nat.mul_le_mul hbs_le htba_le) htbs_le
    have hden : 1000000 ≤ s.totBorrowShares + 1000000 := by omega
    have hdiv :
        (s.borrowShares * (s.totBorrowAssets + 1) +
            (s.totBorrowShares + 1000000 - 1)) /
            (s.totBorrowShares + 1000000)
          ≤ ((2 ^ 128 - 1) * 2 ^ 128 + ((2 ^ 128 - 1) + 1000000 - 1)) /
            1000000 :=
      Nat.div_le_div hnum hden (by decide)
    have hbound :
        ((2 ^ 128 - 1 : Nat) * 2 ^ 128 + ((2 ^ 128 - 1) + 1000000 - 1)) /
            1000000 < Verity.Core.Uint256.modulus := by
      native_decide
    exact lt_of_le_of_lt hdiv hbound
  have h_dB_ne : totBSplus1e6.val ≠ 0 := by
    rw [hTBS1e6_val]
    omega
  have hcbrw_val : cbrw.val = Morpho.Proofs.HealthModel.mulDivUp s.borrowShares
                    (s.totBorrowAssets + 1) (s.totBorrowShares + 1000000) := by
    show (Verity.Stdlib.Math.mulDiv512Up bs totBAplus1 totBSplus1e6).val = _
    rw [mulDiv512Up_val bs totBAplus1 totBSplus1e6 h_dB_ne h_qB',
        hs_bs, hTBA1_val, hTBS1e6_val]
  -- mulDivDown collateral price 1e36.
  have hVal_1e36 :
      ((1000000000000000000000000000000000000 : Uint256) : Uint256).val
        = ORACLE_PRICE_SCALE := by decide
  have hVal_1e18 :
      ((1000000000000000000 : Uint256) : Uint256).val
        = Morpho.Proofs.HealthModel.WAD := by decide
  have h_xyC' : col.val * price.val < Verity.Core.Uint256.modulus := by
    rw [hs_col, hs_price]; exact h_xyC
  have h_qC' : (col.val * price.val) /
                  ((1000000000000000000000000000000000000 : Uint256) : Uint256).val
                < Verity.Core.Uint256.modulus := by
    rw [hVal_1e36]
    exact Nat.lt_of_le_of_lt (Nat.div_le_self _ _) h_xyC'
  have hcq_val : cq.val
        = Morpho.Proofs.HealthModel.mulDivDown s.collateral s.price ORACLE_PRICE_SCALE := by
    show (Morpho.Contract.mulDivDown col price
              1000000000000000000000000000000000000).val = _
    rw [mulDivDown_val col price (1000000000000000000000000000000000000 : Uint256)
              h_xyC' h_qC',
        hs_col, hs_price, hVal_1e36]
  -- mulDivDown collateralQuoted lltv 1e18.
  have h_xyM' : cq.val * mp.lltv.val < Verity.Core.Uint256.modulus := by
    rw [hcq_val, hs_lltv]; exact h_xyM
  have h_qM' : (cq.val * mp.lltv.val) / ((1000000000000000000 : Uint256) : Uint256).val
              < Verity.Core.Uint256.modulus := by
    rw [hVal_1e18]
    exact Nat.lt_of_le_of_lt (Nat.div_le_self _ _) h_xyM'
  have hcmax_val : cmax.val = HealthState.maxBorrow s := by
    show (Morpho.Contract.mulDivDown cq mp.lltv 1000000000000000000).val = _
    rw [mulDivDown_val cq mp.lltv (1000000000000000000 : Uint256) h_xyM' h_qM',
        hcq_val, hs_lltv, hVal_1e18]
    rfl
  -- Now reduce the `pure` and equate the two booleans.
  -- `(a ≥ b)` on `Uint256` unfolds to `b.val ≤ a.val`; rewriting the two `.val`s
  -- gives the model-side proposition.
  have hbool : (cmax ≥ cbrw)
              ↔ (HealthState.maxBorrow s ≥
                  Morpho.Proofs.HealthModel.mulDivUp s.borrowShares
                    (s.totBorrowAssets + 1) (s.totBorrowShares + 1000000)) := by
    simp only [ge_iff_le, Verity.Core.Uint256.le_def]
    rw [hcbrw_val, hcmax_val]
  exact congrArg (fun b => ContractResult.success b cs) (decide_eq_decide.mpr hbool)

/-! ## Main theorem -/

theorem healthFaithful_of_noOverflow
    (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) (cs : ContractState)
    (h : NoOverflow mp id account price cs) :
    HealthFaithful mp id account price cs := by
  intro v cs' hrun
  -- Extract the body's evaluation.
  have heval := isHealthyWithPrice_eval mp id account price cs h
  unfold Contract.run at hrun
  rw [heval] at hrun
  -- hrun: ContractResult.success (decide …) cs = ContractResult.success v cs'
  injection hrun with hv hcs
  subst hv
  -- Goal: decide (...) = true ↔ healthy (project …)
  set s : HealthState := project mp id account price cs with hs_def
  -- `healthy s = (s.borrowShares = 0 ∨ s.maxBorrow ≥ s.borrowed)`.
  show decide (HealthState.maxBorrow s ≥
        Morpho.Proofs.HealthModel.mulDivUp s.borrowShares (s.totBorrowAssets + 1)
          (s.totBorrowShares + 1000000)) = true ↔ HealthState.healthy s
  -- Unfold `borrowed` for the right side.
  have hborrowed : HealthState.borrowed s
        = Morpho.Proofs.HealthModel.mulDivUp s.borrowShares (s.totBorrowAssets + 1)
            (s.totBorrowShares + 1000000) := by
    unfold HealthState.borrowed VIRTUAL_ASSETS VIRTUAL_SHARES; rfl
  rw [decide_eq_true_iff]
  unfold HealthState.healthy
  -- Goal: s.maxBorrow ≥ mulDivUp ... ↔ s.borrowShares = 0 ∨ s.maxBorrow ≥ s.borrowed
  -- Replace mulDivUp on the LHS with s.borrowed.
  rw [show Morpho.Proofs.HealthModel.mulDivUp s.borrowShares (s.totBorrowAssets + 1)
            (s.totBorrowShares + 1000000) = s.borrowed from hborrowed.symm]
  constructor
  · intro hle
    exact Or.inr hle
  · intro hh
    rcases hh with hz | hge
    · -- borrowShares = 0 ⇒ borrowed = mulDivUp 0 _ _ = 0 ⇒ maxBorrow ≥ 0
      have h_borrowed_zero : s.borrowed = 0 := by
        rw [hborrowed]
        unfold Morpho.Proofs.HealthModel.mulDivUp
        rw [hz, Nat.zero_mul, Nat.zero_add]
        apply Nat.div_eq_of_lt
        omega
      show s.maxBorrow ≥ s.borrowed
      rw [h_borrowed_zero]
      exact Nat.zero_le _
    · exact hge

end Morpho.Proofs.HealthFaithful
