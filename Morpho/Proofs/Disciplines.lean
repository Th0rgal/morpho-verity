import Morpho.Proofs.FramePreserve
import Morpho.Proofs.Env
import Morpho.Proofs.Projection
import Morpho.Proofs.HealthFaithful
import Morpho.Contract
import Contracts.PackedWordLemmas
import Mathlib.Data.Nat.Bitwise

namespace Morpho.Proofs.Disciplines

open Verity
open Contracts
open Morpho.Proofs.StorageFrame
open Morpho.Proofs.FramePreserve
open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Env
open Morpho.Proofs.Projection
open Morpho.Proofs.HealthFaithful
open Morpho.Contract.Morpho

theorem addPanic_left_le {a b out : Uint256} {cs cs' : ContractState}
    (h : Verity.Stdlib.Math.addPanic a b cs = ContractResult.success out cs') :
    a.val ≤ out.val := by
  unfold Verity.Stdlib.Math.addPanic Verity.Stdlib.Math.requireSomeUint
    Verity.Stdlib.Math.safeAdd at h
  by_cases hover : a.val + b.val > Verity.Stdlib.Math.MAX_UINT256
  · simp [hover] at h
    change (Verity.bind (Verity.require false "Panic(0x11): arithmetic overflow")
        (fun _ => Verity.pure (0 : Uint256))) cs = ContractResult.success out cs' at h
    unfold Verity.bind Verity.require at h
    simp at h
  · simp only [hover, ↓reduceIte, Verity.pure, Pure.pure] at h
    injection h with hout _
    subst hout
    change a.val ≤ (Verity.Core.Uint256.add a b).val
    unfold Verity.Core.Uint256.add
    rw [Verity.Core.Uint256.val_ofNat]
    have hmod : a.val + b.val < Verity.Core.Uint256.modulus := by
      have hmax := Verity.Core.Uint256.max_uint256_succ_eq_modulus
      show a.val + b.val < Verity.Core.Uint256.modulus
      have : Verity.Core.MAX_UINT256 + 1 = Verity.Core.Uint256.modulus := hmax
      change ¬ a.val + b.val > Verity.Core.MAX_UINT256 at hover
      omega
    rw [Nat.mod_eq_of_lt hmod]
    omega

theorem subPanic_le_left {a b out : Uint256} {cs cs' : ContractState}
    (h : Verity.Stdlib.Math.subPanic a b cs = ContractResult.success out cs') :
    out.val ≤ a.val := by
  unfold Verity.Stdlib.Math.subPanic Verity.Stdlib.Math.requireSomeUint
    Verity.Stdlib.Math.safeSub at h
  by_cases hunder : b.val > a.val
  · simp [hunder] at h
    change (Verity.bind (Verity.require false "Panic(0x11): arithmetic underflow")
        (fun _ => Verity.pure (0 : Uint256))) cs = ContractResult.success out cs' at h
    unfold Verity.bind Verity.require at h
    simp at h
  · simp only [hunder, ↓reduceIte, Verity.pure, Pure.pure] at h
    injection h with hout _
    subst hout
    change (Verity.Core.Uint256.sub a b).val ≤ a.val
    unfold Verity.Core.Uint256.sub
    have hle : b.val ≤ a.val := by omega
    simp only [hle, ↓reduceIte]
    rw [Verity.Core.Uint256.val_ofNat]
    have hlt : a.val - b.val < Verity.Core.Uint256.modulus :=
      Nat.lt_of_le_of_lt (Nat.sub_le _ _) a.isLt
    rw [Nat.mod_eq_of_lt hlt]
    omega

def UINT128_MAX_WORD : Uint256 :=
  340282366920938463463374607431768211455

theorem uint128_lt_of_le_max (v : Uint256) (h : v ≤ UINT128_MAX_WORD) :
    v.val < 2 ^ 128 := by
  have hmax : UINT128_MAX_WORD.val = 2 ^ 128 - 1 := by decide
  simp only [Verity.Core.Uint256.le_def] at h
  rw [hmax] at h
  omega

theorem subPanic_lt_128_of_left {a b out : Uint256} {cs cs' : ContractState}
    (h : Verity.Stdlib.Math.subPanic a b cs = ContractResult.success out cs')
    (ha : a.val < 2 ^ 128) :
    out.val < 2 ^ 128 :=
  Nat.lt_of_le_of_lt (subPanic_le_left h) ha

theorem addPanic_left_le_and_lt_128 {a b out : Uint256} {cs cs' : ContractState}
    (hadd : Verity.Stdlib.Math.addPanic a b cs = ContractResult.success out cs')
    (hmax : out ≤ UINT128_MAX_WORD) :
    a.val ≤ out.val ∧ out.val < 2 ^ 128 :=
  ⟨addPanic_left_le hadd, uint128_lt_of_le_max out hmax⟩

/-! ### Slot-distinctness leaves for the watched position word (offset 1). -/

/-- The watched position word `(id, account, 1)` differs from any position word at
    offset `0` (e.g. `supplyShares`, fee-recipient shares), regardless of id or
    account. The id may differ because an entrypoint computes its own market id
    internally, independent of the watched position's id. -/
theorem watched_ne_pos0 (idn an idn2 bn : Nat) :
    structSlot2 2 idn an 1 ≠ structSlot2 2 idn2 bn 0 := by
  have h := Loc.slot_ne (a := Loc.pos idn an 1) (b := Loc.pos idn2 bn 0)
    (show (1:Nat) < MAX_WORDS by decide) (show (0:Nat) < MAX_WORDS by decide)
    (by intro hc; injection hc with _ _ h3; exact absurd h3 (by decide))
  simpa [Loc.slot] using h

/-- The watched position word differs from any market word. -/
theorem watched_ne_mkt (idn an idn2 wo : Nat) (hwo : wo < MAX_WORDS) :
    structSlot2 2 idn an 1 ≠ structSlot 3 idn2 wo := by
  have h := Loc.slot_ne (a := Loc.pos idn an 1) (b := Loc.mkt idn2 wo)
    (show (1:Nat) < MAX_WORDS by decide) hwo
    (by intro hc; exact absurd hc (by simp))
  simpa [Loc.slot] using h

macro "watched_ne" : tactic =>
  `(tactic| first
    | exact watched_ne_pos0 _ _ _ _
    | exact watched_ne_mkt _ _ _ _ (by decide))

/-- One step of the compositional frame traversal: a `bind`/`pure`/`ite` node, a
    write to a slot `≠ S` (discharged by `watched_ne`), or a state-preserving
    primitive leaf. -/
macro "frame_leaf" : tactic =>
  `(tactic| first
    | with_reducible (apply PreservesSlot.bind)
    | with_reducible (apply PreservesSlot.ite')
    | with_reducible (exact PreservesSlot.pure _ _)
    | (with_reducible (apply PreservesSlot.setStructMember2At); watched_ne)
    | (with_reducible (apply PreservesSlot.setStructMemberAt); watched_ne)
    | (with_reducible (apply PreservesSlot.of_storage); first
        | with_reducible (apply StoragePreserving.structMemberAt)
        | with_reducible (apply StoragePreserving.structMember2At)
        | with_reducible (apply StoragePreserving.require)
        | with_reducible (apply StoragePreserving.contractAddress)
        | with_reducible (apply StoragePreserving.msgSender)
        | with_reducible (apply StoragePreserving.ecmEnvRead)
        | with_reducible (apply StoragePreserving.addPanic)
        | with_reducible (apply StoragePreserving.subPanic)
        | with_reducible (apply StoragePreserving.mulPanic)
        | with_reducible (apply StoragePreserving.blockTimestamp)
        | with_reducible (apply StoragePreserving.getStorageAddr)
        | with_reducible (apply StoragePreserving.getMapping2)
        | with_reducible (apply StoragePreserving.emitEvent)))

macro "frame_simp_body" x:ident : tactic =>
  `(tactic| simp only [$x:ident, Bind.bind, Pure.pure,
      Morpho.Contract.blockTimestamp, Morpho.Contract.contractAddress,
      Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
      Morpho.Contract.Morpho.setStructMember, Morpho.Contract.Morpho.setStructMember2,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq, reduceIte, reduceCtorEq,
      and_true, and_false, false_and, if_true, if_false])

set_option maxHeartbeats 4000000 in
theorem preservesSlot_isSenderAuthorized (onBehalf : Address) (S : Nat) :
    PreservesSlot S (_isSenderAuthorized onBehalf) := by
  frame_simp_body _isSenderAuthorized
  repeat' first | frame_leaf | with_reducible (intro _)

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 1000000 in
theorem preservesSlot_accrueInterest (mp : MarketParams) (id : Bytes32) (idn an : Nat) :
    PreservesSlot (structSlot2 2 idn an 1) (_accrueInterest mp id) := by
  frame_simp_body _accrueInterest
  repeat' first | frame_leaf | with_reducible (intro _)

set_option maxHeartbeats 4000000 in
theorem storagePreserving_isHealthyWithPrice (mp : MarketParams) (id : Bytes32)
    (account : Address) (price : Uint256) :
    StoragePreserving (_isHealthyWithPrice mp id account price) := by
  unfold _isHealthyWithPrice
  simp only [Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
    Bool.and_eq_true, beq_iff_eq, String.reduceEq, and_true, and_false,
    if_true, if_false, Bind.bind, Pure.pure]
  apply StoragePreserving.bind
  · exact StoragePreserving.structMember2At 2 1 (some (0, 128)) id account
  · intro borrowShares_
    apply StoragePreserving.bind
    · exact StoragePreserving.structMemberAt 3 1 (some (0, 128)) id
    · intro totalBorrowAssets_
      apply StoragePreserving.bind
      · exact StoragePreserving.structMemberAt 3 1 (some (128, 128)) id
      · intro totalBorrowShares_
        apply StoragePreserving.bind
        · exact StoragePreserving.addPanic totalBorrowAssets_ 1
        · intro totalBorrowAssetsWithVirtual
          apply StoragePreserving.bind
          · exact StoragePreserving.addPanic totalBorrowShares_ 1000000
          · intro totalBorrowSharesWithVirtual
            apply StoragePreserving.bind
            · exact StoragePreserving.structMember2At 2 1 (some (128, 128)) id account
            · intro collateral_
              exact StoragePreserving.pure _

set_option maxHeartbeats 4000000 in
theorem storagePreserving_isHealthy (mp : MarketParams) (id : Bytes32)
    (account : Address) :
    StoragePreserving (_isHealthy mp id account) := by
  unfold _isHealthy
  simp only [Morpho.Contract.Morpho.structMember2, Bool.and_eq_true, beq_iff_eq,
    String.reduceEq, and_true, and_false, if_true, if_false, Bind.bind, Pure.pure]
  apply StoragePreserving.bind
  · exact StoragePreserving.structMember2At 2 1 (some (0, 128)) id account
  · intro borrowShares_
    by_cases hgt : borrowShares_ > ZERO
    · simp only [hgt, if_true]
      apply StoragePreserving.bind
      · exact StoragePreserving.ecmEnvRead
          (Compiler.Modules.Oracle.oracleReadUint256Module "_ecm" 0xa035b1fe 0)
          [addressToWord mp.oracle] 0
      · intro collateralPrice
        apply StoragePreserving.bind
        · exact storagePreserving_isHealthyWithPrice mp id account collateralPrice
        · intro healthy
          exact StoragePreserving.pure _
    · simp only [hgt, if_false]
      exact StoragePreserving.pure _

/- Lender-side `supply` leaves every position word at offset 1 (the watched
    borrow/collateral word) untouched: its only writes are `supplyShares` (offset
    0), market totals, and the embedded `_accrueInterest`. -/
set_option maxHeartbeats 4000000 in
set_option maxRecDepth 1000000 in
theorem preservesSlot_supply (mp : MarketParams) (assets shares : Uint256)
    (onBehalf : Address) (data : ByteArray) (idn an : Nat) :
    PreservesSlot (structSlot2 2 idn an 1) (supply mp assets shares onBehalf data) := by
  frame_simp_body supply
  repeat' first
    | frame_leaf
    | with_reducible (apply preservesSlot_accrueInterest)
    | with_reducible (intro _)

/- Lender-side `withdraw` likewise leaves every position word at offset 1
    untouched. -/
set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem preservesSlot_withdraw (mp : MarketParams) (assets shares : Uint256)
    (onBehalf receiver : Address) (idn an : Nat) :
    PreservesSlot (structSlot2 2 idn an 1) (withdraw mp assets shares onBehalf receiver) := by
  frame_simp_body withdraw
  repeat' first
    | frame_leaf
    | with_reducible (apply preservesSlot_accrueInterest)
    | with_reducible (apply preservesSlot_isSenderAuthorized)
    | with_reducible (intro _)

/-! ### Bridge: a preserved position word fixes the projected health fields. -/

/-- `runWord` of a position read depends only on the storage word at its slot, so
    two states agreeing on that slot read the same field value. -/
theorem runWord_structMember2At_congr {κ₁ κ₂ : Type} [StorageKey κ₁] [StorageKey κ₂]
    (base wo : Nat) (packed : Option (Nat × Nat)) (k1 : κ₁) (k2 : κ₂)
    {cs cs' : ContractState}
    (h : cs'.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) wo)
       = cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) wo)) :
    runWord (structMember2At base wo packed k1 k2) cs'
      = runWord (structMember2At base wo packed k1 k2) cs := by
  unfold runWord Contracts.structMember2At
  dsimp only
  rw [h]

/-- If two states agree on the watched position word (offset 1), they agree on the
    projected `collateral` and `borrowShares` fields (both packed in that word). -/
theorem project_word1_congr (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) {cs cs' : ContractState}
    (h : cs'.storage (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1)
       = cs.storage (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1)) :
    (project mp id account price cs').collateral = (project mp id account price cs).collateral
    ∧ (project mp id account price cs').borrowShares
        = (project mp id account price cs).borrowShares := by
  simp only [project, Morpho.Contract.Morpho.structMember2,
    Bool.and_eq_true, beq_iff_eq, String.reduceEq,
    and_true, and_false, if_true, if_false]
  constructor <;>
    rw [runWord_structMember2At_congr _ _ _ _ _ h]

theorem project_storage_congr (mp : MarketParams) (id : Bytes32) (account : Address)
    (price : Uint256) {cs cs' : ContractState} (h : cs'.storage = cs.storage) :
    project mp id account price cs' = project mp id account price cs := by
  unfold project
  simp only [Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
    Bool.and_eq_true, beq_iff_eq, String.reduceEq, and_true, and_false, if_true, if_false]
  unfold runWord Contracts.structMemberAt Contracts.structMember2At
  simp only
  rw [h]

/-! ### Packed-field facts for the watched position word. -/

theorem decode_encode_borrowShares_eq (current value : Uint256)
    (hval : value.val < 2 ^ 128) :
    decodePackedWord (encodePackedWord current value 0 128) 0 128 = value :=
  Contracts.PackedWord.decode_encode_eq current value 0 128 (by decide) hval

theorem decode_encode_borrowShares_preserves_collateral (current value : Uint256) :
    decodePackedWord (encodePackedWord current value 0 128) 128 128 =
      decodePackedWord current 128 128 :=
  Contracts.PackedWord.decode_encode_disjoint current value 128 128 0 128
    (by decide) (by decide) (Or.inr (by decide))

theorem decode_encode_collateral_eq (current value : Uint256)
    (hval : value.val < 2 ^ 128) :
    decodePackedWord (encodePackedWord current value 128 128) 128 128 = value :=
  Contracts.PackedWord.decode_encode_eq current value 128 128 (by decide) hval

theorem decode_encode_collateral_preserves_borrowShares (current value : Uint256) :
    decodePackedWord (encodePackedWord current value 128 128) 0 128 =
      decodePackedWord current 0 128 :=
  Contracts.PackedWord.decode_encode_disjoint current value 0 128 128 128
    (by decide) (by decide) (Or.inl (by decide))

private lemma uint256_ofNat_testBit (x i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.ofNat x).val i =
      (decide (i < 256) && Nat.testBit x i) := by
  rw [_root_.Verity.Core.Uint256.val_ofNat]
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
    Nat.testBit (packedMask w) i = decide (i < w) := by
  unfold packedMask
  exact Nat.testBit_two_pow_sub_one w i

private lemma uint256_packedMask_testBit (w i : Nat) :
    Nat.testBit (_root_.Verity.Core.Uint256.ofNat (packedMask w)).val i
      = (decide (i < 256) && decide (i < w)) := by
  rw [uint256_ofNat_testBit, packedMask_testBit]

theorem decodePackedWord_lt_width (word : Uint256) (offset width : Nat) :
    (decodePackedWord word offset width).val < 2 ^ width := by
  unfold decodePackedWord
  apply Nat.lt_of_testBit width
  · simp only [uint256_and_testBit, uint256_shr_testBit, uint256_packedMask_testBit]
    simp
  · simp
  · intro j hj
    simp only [uint256_and_testBit, uint256_shr_testBit, uint256_packedMask_testBit]
    have hjw : ¬ j < width := by omega
    have hne : width ≠ j := by omega
    simp [hjw, Nat.testBit_two_pow_of_ne hne]

theorem runWord_structMember2At_packed_lt {κ₁ κ₂ : Type}
    [StorageKey κ₁] [StorageKey κ₂] (base wo offset width : Nat)
    (k1 : κ₁) (k2 : κ₂) (cs : ContractState) :
    (runWord (structMember2At base wo ((some (offset, width)) : Option (Nat × Nat))
      k1 k2 : Contract Uint256) cs).val < 2 ^ width := by
  unfold runWord Contracts.structMember2At
  dsimp only
  exact decodePackedWord_lt_width _ _ _

theorem runWord_borrowShares_lt_128 (id : Bytes32) (account : Address)
    (cs : ContractState) :
    (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs).val < 2 ^ 128 :=
  runWord_structMember2At_packed_lt 2 1 0 128 id account cs

theorem runWord_collateral_lt_128 (id : Bytes32) (account : Address)
    (cs : ContractState) :
    (runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs).val < 2 ^ 128 :=
  runWord_structMember2At_packed_lt 2 1 128 128 id account cs

theorem runWord_setCollateral_same {κ₁ κ₂ : Type} [StorageKey κ₁] [StorageKey κ₂]
    (base : Nat) (id : κ₁) (account : κ₂) (value : Uint256) {cs cs' : ContractState}
    (hval : value.val < 2 ^ 128)
    (hrun : setStructMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        id account value cs = ContractResult.success () cs') :
    runWord (structMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs' = value := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  simp only [beq_iff_eq, ↓reduceIte]
  exact decode_encode_collateral_eq _ _ hval

theorem runWord_setCollateral_preserves_borrowShares {κ₁ κ₂ : Type}
    [StorageKey κ₁] [StorageKey κ₂]
    (base : Nat) (id : κ₁) (account : κ₂) (value : Uint256) {cs cs' : ContractState}
    (hrun : setStructMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        id account value cs = ContractResult.success () cs') :
    runWord (structMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs' =
      runWord (structMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  simp only [beq_iff_eq, ↓reduceIte]
  exact decode_encode_collateral_preserves_borrowShares _ _

theorem runWord_setBorrowShares_same {κ₁ κ₂ : Type} [StorageKey κ₁] [StorageKey κ₂]
    (base : Nat) (id : κ₁) (account : κ₂) (value : Uint256) {cs cs' : ContractState}
    (hval : value.val < 2 ^ 128)
    (hrun : setStructMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        id account value cs = ContractResult.success () cs') :
    runWord (structMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs' = value := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  simp only [beq_iff_eq, ↓reduceIte]
  exact decode_encode_borrowShares_eq _ _ hval

theorem runWord_setBorrowShares_preserves_collateral {κ₁ κ₂ : Type}
    [StorageKey κ₁] [StorageKey κ₂]
    (base : Nat) (id : κ₁) (account : κ₂) (value : Uint256) {cs cs' : ContractState}
    (hrun : setStructMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        id account value cs = ContractResult.success () cs') :
    runWord (structMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs' =
      runWord (structMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  simp only [beq_iff_eq, ↓reduceIte]
  exact decode_encode_borrowShares_preserves_collateral _ _

theorem runWord_setBorrowShares_le {κ₁ κ₂ : Type} [StorageKey κ₁] [StorageKey κ₂]
    (id : Bytes32) (account : Address) (base : Nat) (k1 : κ₁) (k2 : κ₂)
    (value : Uint256) {cs cs' : ContractState}
    (hval : value.val < 2 ^ 128)
    (hle : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1 →
      value.val ≤
        (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
          id account : Contract Uint256) cs).val)
    (hrun : setStructMember2At base 1 ((some (0, 128)) : Option (Nat × Nat))
        k1 k2 value cs = ContractResult.success () cs') :
    (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs').val ≤
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  by_cases hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = true := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq, hslot]
    have hle' := hle hslot
    unfold runWord Contracts.structMember2At at hle'
    dsimp only at hle'
    rw [hslot] at hle'
    change value.val ≤
      (decodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
        0 128).val at hle'
    rw [if_pos (by rfl)]
    change (decodePackedWord
      (encodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
        value 0 128) 0 128).val ≤
      (decodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
        0 128).val
    rw [decode_encode_borrowShares_eq _ _ hval]
    exact hle'
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = false := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq]
    rw [if_neg (by decide)]

theorem runWord_setCollateral_ge {κ₁ κ₂ : Type} [StorageKey κ₁] [StorageKey κ₂]
    (id : Bytes32) (account : Address) (base : Nat) (k1 : κ₁) (k2 : κ₂)
    (value : Uint256) {cs cs' : ContractState}
    (hval : value.val < 2 ^ 128)
    (hle : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1 →
      (runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val ≤ value.val)
    (hrun : setStructMember2At base 1 ((some (128, 128)) : Option (Nat × Nat))
        k1 k2 value cs = ContractResult.success () cs') :
    (runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs).val ≤
      (runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs').val := by
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  by_cases hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = true := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq, hslot]
    have hle' := hle hslot
    unfold runWord Contracts.structMember2At at hle'
    dsimp only at hle'
    rw [hslot] at hle'
    change
      (decodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
        128 128).val ≤ value.val at hle'
    rw [if_pos (by rfl)]
    change
      (decodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
        128 128).val ≤
      (decodePackedWord
        (encodePackedWord (cs.storage (structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1))
          value 128 128) 128 128).val
    rw [decode_encode_collateral_eq _ _ hval]
    exact hle'
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = false := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq]
    rw [if_neg (by decide)]

/-! ### Compositional preservation of the watched `borrowShares` packed field. -/

def PreservesBorrowShares {α : Type} (id : Bytes32) (account : Address) (c : Contract α) : Prop :=
  ∀ cs v cs', c cs = ContractResult.success v cs' →
    runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs' =
    runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs

theorem PreservesBorrowShares.of_storage {α : Type} {id : Bytes32} {account : Address}
    {c : Contract α} (h : StoragePreserving c) :
    PreservesBorrowShares id account c :=
  fun cs v cs' hrun => by
    apply runWord_structMember2At_congr
    rw [h cs v cs' hrun]

theorem PreservesBorrowShares.pure (id : Bytes32) (account : Address) {α : Type} (a : α) :
    PreservesBorrowShares id account (Verity.pure a) :=
  PreservesBorrowShares.of_storage (StoragePreserving.pure a)

theorem PreservesBorrowShares.bind {α β : Type} {id : Bytes32} {account : Address}
    {c : Contract α} {k : α → Contract β}
    (hc : PreservesBorrowShares id account c)
    (hk : ∀ a, PreservesBorrowShares id account (k a)) :
    PreservesBorrowShares id account (Verity.bind c k) := by
  intro cs v cs' h
  simp only [Verity.bind] at h
  split at h
  · next a t hcs =>
      exact (hk a t v cs' h).trans (hc cs a t hcs)
  · next => exact absurd h (by simp)

theorem PreservesBorrowShares.ite' {id : Bytes32} {account : Address}
    {α : Type} {c : Prop} [Decidable c] {t e : Contract α}
    (ht : PreservesBorrowShares id account t) (he : PreservesBorrowShares id account e) :
    PreservesBorrowShares id account (if c then t else e) := by
  split
  · exact ht
  · exact he

theorem PreservesBorrowShares.setStructMemberAt {κ : Type} [StorageKey κ]
    (id : Bytes32) (account : Address) (base wo : Nat) (packed : Option (Nat × Nat))
    (key : κ) (value : Uint256)
    (hne : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1
       ≠ structSlot base (StorageKey.toWord key) wo) :
    PreservesBorrowShares id account (setStructMemberAt base wo packed key value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMemberAt at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

theorem PreservesBorrowShares.setStructMember2At_other {κ₁ κ₂ : Type}
    [StorageKey κ₁] [StorageKey κ₂]
    (id : Bytes32) (account : Address) (base wo : Nat) (packed : Option (Nat × Nat))
    (k1 : κ₁) (k2 : κ₂) (value : Uint256)
    (hne : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1
       ≠ structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) wo) :
    PreservesBorrowShares id account (setStructMember2At base wo packed k1 k2 value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

theorem PreservesBorrowShares.setCollateral
    (id : Bytes32) (account : Address) (k1 : Bytes32) (k2 : Address) (value : Uint256) :
    PreservesBorrowShares id account
      (setStructMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat)) k1 k2 value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  by_cases hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = true := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq, hslot]
    exact decode_encode_collateral_preserves_borrowShares _ _
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = false := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq]
    simp

macro "borrowShares_leaf" : tactic =>
  `(tactic| first
    | with_reducible (apply PreservesBorrowShares.bind)
    | with_reducible (apply PreservesBorrowShares.ite')
    | with_reducible (exact PreservesBorrowShares.pure _ _ _)
    | with_reducible (apply PreservesBorrowShares.setCollateral)
    | (with_reducible (apply PreservesBorrowShares.setStructMember2At_other); watched_ne)
    | (with_reducible (apply PreservesBorrowShares.setStructMemberAt); watched_ne)
    | (with_reducible (apply PreservesBorrowShares.of_storage); first
        | with_reducible (apply StoragePreserving.structMemberAt)
        | with_reducible (apply StoragePreserving.structMember2At)
        | with_reducible (apply StoragePreserving.require)
        | with_reducible (apply StoragePreserving.contractAddress)
        | with_reducible (apply StoragePreserving.msgSender)
        | with_reducible (apply StoragePreserving.ecmEnvRead)
        | with_reducible (apply StoragePreserving.addPanic)
        | with_reducible (apply StoragePreserving.subPanic)
        | with_reducible (apply StoragePreserving.mulPanic)
        | with_reducible (apply StoragePreserving.blockTimestamp)
        | with_reducible (apply StoragePreserving.getStorageAddr)
        | with_reducible (apply StoragePreserving.getMapping2)
        | with_reducible (apply StoragePreserving.emitEvent)))

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem preservesBorrowShares_supplyCollateral (mp : MarketParams) (assets : Uint256)
    (onBehalf : Address) (data : ByteArray) (id : Bytes32) (account : Address) :
    PreservesBorrowShares id account (supplyCollateral mp assets onBehalf data) := by
  frame_simp_body supplyCollateral
  repeat' first
    | borrowShares_leaf
    | with_reducible (intro _)

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem collateral_nondec_supplyCollateral (mp : MarketParams) (assets : Uint256)
    (onBehalf : Address) (data : ByteArray) (id : Bytes32) (account : Address)
    (price : Uint256) :
    ∀ cs out cs',
      (supplyCollateral mp assets onBehalf data).run cs = ContractResult.success out cs' →
      (project mp id account price cs).collateral ≤
        (project mp id account price cs').collateral := by
  intro cs out cs' hrun
  have h := run_eq_of_success hrun
  simp only [supplyCollateral, Bind.bind, Pure.pure,
      Morpho.Contract.contractAddress,
      Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
      Morpho.Contract.Morpho.setStructMember2,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq,
      and_true, and_false, if_true, if_false] at h
  simp only [Verity.bind, Contracts.ecmEnvRead, Contracts.structMemberAt,
    Verity.require, Verity.contractAddress] at h
  repeat' first | split at h | split at *
  all_goals try contradiction
  case h_1 =>
    simp_all [Verity.pure]
    rename_i x8 u17 st16 x7 u15 st14 x6 u13 st12 x5 cur st10 x4 new st8
      x3 u7 st6 x2 u5 st4 x1 sender st2 x0 u1 st0 hread hadd hset hmsg hemit
      hmarket hcs hassets hreqassets honbehalf hreqbehalf hmax hreqmax
    let mid : Bytes32 := st12.ecmResults
      (Compiler.Modules.Hashing.abiEncodeStaticWordsModule "_ecm" 5).name
      [Verity.Core.Uint256.ofNat (Verity.Core.Address.toNat mp.loanToken),
        Verity.Core.Uint256.ofNat (Verity.Core.Address.toNat mp.collateralToken),
        Verity.Core.Uint256.ofNat (Verity.Core.Address.toNat mp.oracle),
        Verity.Core.Uint256.ofNat (Verity.Core.Address.toNat mp.irm), mp.lltv] 0
    have hread_run :
        runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
          mid onBehalf : Contract Uint256) st12 = cur := by
      unfold runWord
      unfold mid
      rw [hread]
    have hread_storage : st10.storage = st12.storage := by
      exact StoragePreserving.structMember2At 2 1
        ((some (128, 128)) : Option (Nat × Nat)) mid onBehalf st12 cur st10 hread
    have hadd_storage : st6.storage = st10.storage :=
      StoragePreserving.addPanic cur assets st10 new st6 hadd
    have hpre_storage : st6.storage = st12.storage := hadd_storage.trans hread_storage
    have hfit : new.val < 2 ^ 128 := by
      apply uint128_lt_of_le_max new
      simp only [UINT128_MAX_WORD, Verity.Core.Uint256.le_def]
      exact hmax
    have hleft : cur.val ≤ new.val := addPanic_left_le hadd
    have hwrite := runWord_setCollateral_ge id account 2 mid onBehalf new hfit ?hle hset
    · have hmsg_storage : st2.storage = st4.storage :=
        StoragePreserving.msgSender st4 sender st2 hmsg
      have hemit_storage : cs'.storage = st2.storage :=
        StoragePreserving.emitEvent _ _ _ st2 u1 cs' hemit
      have hfinal_storage : cs'.storage = st4.storage := hemit_storage.trans hmsg_storage
      have hpre_read := runWord_structMember2At_congr 2 1
        ((some (128, 128)) : Option (Nat × Nat)) id account
        (cs := st12) (cs' := st6) (by exact congrFun hpre_storage _)
      have hpost_read := runWord_structMember2At_congr 2 1
        ((some (128, 128)) : Option (Nat × Nat)) id account
        (cs := st4) (cs' := cs') (by exact congrFun hfinal_storage _)
      simp only [project, Morpho.Contract.Morpho.structMember2, Bool.and_eq_true,
        beq_iff_eq, String.reduceEq, and_true, and_false, if_true, if_false]
      change (runWord (structMember2At 2 1 (some (128, 128)) id account :
          Contract Uint256) st12).val ≤
        (runWord (structMember2At 2 1 (some (128, 128)) id account :
          Contract Uint256) cs').val
      rw [← hpre_read, hpost_read]
      exact hwrite
    · intro hslot
      have hcur_watch :
          (runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
            id account : Contract Uint256) st6).val = cur.val := by
        unfold runWord Contracts.structMember2At at hread_run ⊢
        dsimp only at hread_run ⊢
        rw [hslot, hpre_storage]
        exact congrArg (fun x : Uint256 => x.val) hread_run
      rw [hcur_watch]
      exact hleft

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem preservesBorrowShares_repayAfterPositionWrite
    (mid : Bytes32) (onBehalf : Address)
    (finalAssets finalShares totalBorrowAssets_ newTotalBorrowShares : Uint256)
    (id : Bytes32) (account : Address) :
    PreservesBorrowShares id account (do
      setStructMemberAt 3 1 ((some (128, 128)) : Option (Nat × Nat))
        mid newTotalBorrowShares
      if totalBorrowAssets_ ≥ finalAssets then do
        let newTotalBorrowAssets ←
          Verity.Stdlib.Math.subPanic totalBorrowAssets_ finalAssets
        setStructMemberAt 3 1 ((some (0, 128)) : Option (Nat × Nat))
          mid newTotalBorrowAssets
        let sender ← msgSender
        emit "Repay" [mid, sender, onBehalf, finalAssets, finalShares]
        Verity.pure (finalAssets, finalShares)
      else do
        setStructMemberAt 3 1 ((some (0, 128)) : Option (Nat × Nat)) mid ZERO
        let sender ← msgSender
        emit "Repay" [mid, sender, onBehalf, finalAssets, finalShares]
        Verity.pure (finalAssets, finalShares)) := by
  simp only [Bind.bind, Pure.pure, Contracts.emit, Contracts.EventArg.toWord,
    List.mapM_cons, List.mapM_nil, Verity.Contract.bind_pure_left]
  repeat' first
    | borrowShares_leaf
    | with_reducible (intro _)

def repayFromShares (mid : Bytes32) (onBehalf : Address)
    (finalAssets finalShares totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let currentBorrowShares ←
    structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat)) mid onBehalf
  Verity.require (decide (currentBorrowShares ≥ finalShares)) "insufficient balance"
  let newPositionBorrowShares ←
    Verity.Stdlib.Math.subPanic currentBorrowShares finalShares
  let newTotalBorrowShares ←
    Verity.Stdlib.Math.subPanic totalBorrowShares_ finalShares
  setStructMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
    mid onBehalf newPositionBorrowShares
  setStructMemberAt 3 1 ((some (128, 128)) : Option (Nat × Nat))
    mid newTotalBorrowShares
  if totalBorrowAssets_ ≥ finalAssets then do
    let newTotalBorrowAssets ←
      Verity.Stdlib.Math.subPanic totalBorrowAssets_ finalAssets
    setStructMemberAt 3 1 ((some (0, 128)) : Option (Nat × Nat))
      mid newTotalBorrowAssets
    let sender ← msgSender
    emit "Repay" [mid, sender, onBehalf, finalAssets, finalShares]
    Verity.pure (finalAssets, finalShares)
  else do
    setStructMemberAt 3 1 ((some (0, 128)) : Option (Nat × Nat)) mid ZERO
    let sender ← msgSender
    emit "Repay" [mid, sender, onBehalf, finalAssets, finalShares]
    Verity.pure (finalAssets, finalShares)

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem borrowShares_noninc_repayFromShares
    (mid : Bytes32) (onBehalf : Address)
    (finalAssets finalShares totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (id : Bytes32) (account : Address) :
    ∀ cs out cs',
      (repayFromShares mid onBehalf finalAssets finalShares totalBorrowAssets_
        totalBorrowShares_).run cs = ContractResult.success out cs' →
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs').val ≤
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val := by
  intro cs out cs' hrun
  have h := run_eq_of_success hrun
  unfold repayFromShares at h
  simp only [Bind.bind, Pure.pure, Contracts.emit, Contracts.EventArg.toWord,
    List.mapM_cons, List.mapM_nil, Verity.Contract.bind_pure_left] at h
  simp only [Verity.bind, Verity.require] at h
  split at h
  · next current stRead hread =>
    split at h
    · next hguardEq =>
      by_cases hcond : finalShares.val ≤ current.val
      · simp [hcond] at hguardEq
        rw [← hguardEq] at h
        split at h
        · next newPos stSubPos hsubPos =>
          split at h
          · next newTotal stSubTotal hsubTotal =>
            split at h
            · next _ stSet hset =>
              have hread_storage : stRead.storage = cs.storage :=
                StoragePreserving.structMember2At 2 1
                  ((some (0, 128)) : Option (Nat × Nat))
                  mid onBehalf cs current stRead hread
              have hsubPos_storage : stSubPos.storage = stRead.storage :=
                StoragePreserving.subPanic current finalShares
                  stRead newPos stSubPos hsubPos
              have hsubTotal_storage : stSubTotal.storage = stSubPos.storage :=
                StoragePreserving.subPanic totalBorrowShares_ finalShares
                  stSubPos newTotal stSubTotal hsubTotal
              have hpre_storage : stSubTotal.storage = cs.storage :=
                hsubTotal_storage.trans (hsubPos_storage.trans hread_storage)
              have hcurrent :
                  runWord (structMember2At 2 1
                    ((some (0, 128)) : Option (Nat × Nat))
                    mid onBehalf : Contract Uint256) cs = current := by
                unfold runWord
                rw [hread]
              have hfit : newPos.val < 2 ^ 128 := by
                apply subPanic_lt_128_of_left hsubPos
                rw [← hcurrent]
                exact runWord_borrowShares_lt_128 mid onBehalf cs
              have hleft : newPos.val ≤ current.val := subPanic_le_left hsubPos
              have hwrite :=
                runWord_setBorrowShares_le id account 2 mid onBehalf newPos
                  hfit ?hle hset
              have htail :=
                preservesBorrowShares_repayAfterPositionWrite mid onBehalf
                  finalAssets finalShares totalBorrowAssets_ newTotal
                  id account stSet out cs' h
              rw [htail]
              have hpre_read := runWord_structMember2At_congr 2 1
                ((some (0, 128)) : Option (Nat × Nat)) id account
                (cs := cs) (cs' := stSubTotal) (by exact congrFun hpre_storage _)
              rw [hpre_read] at hwrite
              exact hwrite
              · intro hslot
                have hcur_watch :
                    (runWord (structMember2At 2 1
                      ((some (0, 128)) : Option (Nat × Nat))
                      id account : Contract Uint256) stSubTotal).val =
                      current.val := by
                  unfold runWord Contracts.structMember2At at hcurrent ⊢
                  dsimp only at hcurrent ⊢
                  rw [hslot, hpre_storage]
                  exact congrArg (fun x : Uint256 => x.val) hcurrent
                rw [hcur_watch]
                exact hleft
            · simp at h
          · simp at h
        · simp at h
      · simp [hcond] at hguardEq
    · simp at h
  · simp at h

def repayCoreAssets (mid : Bytes32) (onBehalf : Address)
    (assets totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowSharesWithVirtual ←
    Verity.Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let totalBorrowAssetsWithVirtual ←
    Verity.Stdlib.Math.addPanic totalBorrowAssets_ 1
  let finalShares :=
    Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
      totalBorrowAssetsWithVirtual
  repayFromShares mid onBehalf assets finalShares totalBorrowAssets_
    totalBorrowShares_

def repayCoreShares (mid : Bytes32) (onBehalf : Address)
    (shares totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowAssetsWithVirtual ←
    Verity.Stdlib.Math.addPanic totalBorrowAssets_ 1
  let totalBorrowSharesWithVirtual ←
    Verity.Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let finalAssets :=
    Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
      totalBorrowSharesWithVirtual
  repayFromShares mid onBehalf finalAssets shares totalBorrowAssets_
    totalBorrowShares_

def repayCore (mid : Bytes32) (onBehalf : Address)
    (assets shares totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) :=
  if assets > 0 then
    repayCoreAssets mid onBehalf assets totalBorrowAssets_ totalBorrowShares_
  else
    repayCoreShares mid onBehalf shares totalBorrowAssets_ totalBorrowShares_

/-!
The generated body branches on whether the caller supplied assets or shares.
Keeping these two tiny suffixes separate avoids producing a very large proof term
when the full `repay` body is reduced.
-/
def NonIncBorrowShares {α : Type} (id : Bytes32) (account : Address)
    (c : Contract α) : Prop :=
  ∀ cs v cs', c cs = ContractResult.success v cs' →
    (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs').val ≤
    (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs).val

theorem NonIncBorrowShares.bind_storage {α β : Type}
    {id : Bytes32} {account : Address} {c : Contract α} {k : α → Contract β}
    (hc : StoragePreserving c)
    (hk : ∀ a, NonIncBorrowShares id account (k a)) :
    NonIncBorrowShares id account (Verity.bind c k) := by
  intro cs v cs' h
  simp only [Verity.bind] at h
  split at h
  · next a t hcs =>
      have htail := hk a t v cs' h
      have hpre := runWord_structMember2At_congr 2 1
        ((some (0, 128)) : Option (Nat × Nat)) id account
        (cs := cs) (cs' := t) (by rw [hc cs a t hcs])
      rw [hpre] at htail
      exact htail
  · simp at h

set_option maxHeartbeats 4000000 in
theorem borrowShares_noninc_repayCoreAssets
    (mid : Bytes32) (onBehalf : Address)
    (assets totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (id : Bytes32) (account : Address) :
    ∀ cs out cs',
      (repayCoreAssets mid onBehalf assets totalBorrowAssets_
        totalBorrowShares_).run cs = ContractResult.success out cs' →
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs').val ≤
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val := by
  intro cs out cs' hrun
  have hraw := run_eq_of_success hrun
  have hnon :
      NonIncBorrowShares id account
        (repayCoreAssets mid onBehalf assets totalBorrowAssets_
          totalBorrowShares_) := by
    unfold repayCoreAssets
    apply NonIncBorrowShares.bind_storage
    · exact StoragePreserving.addPanic totalBorrowShares_ 1000000
    · intro totalSharesVirtual
      apply NonIncBorrowShares.bind_storage
      · exact StoragePreserving.addPanic totalBorrowAssets_ 1
      · intro totalAssetsVirtual
        intro cs out cs' h
        let finalShares :=
          Morpho.Contract.mulDivDown assets totalSharesVirtual totalAssetsVirtual
        have hrunTail :
            (repayFromShares mid onBehalf assets finalShares totalBorrowAssets_
              totalBorrowShares_).run cs = ContractResult.success out cs' := by
          rw [Contract.run, h]
        exact borrowShares_noninc_repayFromShares mid onBehalf assets finalShares
          totalBorrowAssets_ totalBorrowShares_ id account cs out cs' hrunTail
  exact hnon cs out cs' hraw

set_option maxHeartbeats 4000000 in
theorem borrowShares_noninc_repayCoreShares
    (mid : Bytes32) (onBehalf : Address)
    (shares totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (id : Bytes32) (account : Address) :
    ∀ cs out cs',
      (repayCoreShares mid onBehalf shares totalBorrowAssets_
        totalBorrowShares_).run cs = ContractResult.success out cs' →
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs').val ≤
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val := by
  intro cs out cs' hrun
  have hraw := run_eq_of_success hrun
  have hnon :
      NonIncBorrowShares id account
        (repayCoreShares mid onBehalf shares totalBorrowAssets_
          totalBorrowShares_) := by
    unfold repayCoreShares
    apply NonIncBorrowShares.bind_storage
    · exact StoragePreserving.addPanic totalBorrowAssets_ 1
    · intro totalAssetsVirtual
      apply NonIncBorrowShares.bind_storage
      · exact StoragePreserving.addPanic totalBorrowShares_ 1000000
      · intro totalSharesVirtual
        intro cs out cs' h
        let finalAssets :=
          Morpho.Contract.mulDivUp shares totalAssetsVirtual totalSharesVirtual
        have hrunTail :
            (repayFromShares mid onBehalf finalAssets shares totalBorrowAssets_
              totalBorrowShares_).run cs = ContractResult.success out cs' := by
          rw [Contract.run, h]
        exact borrowShares_noninc_repayFromShares mid onBehalf finalAssets shares
          totalBorrowAssets_ totalBorrowShares_ id account cs out cs' hrunTail
  exact hnon cs out cs' hraw

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem borrowShares_noninc_repayCore
    (mid : Bytes32) (onBehalf : Address)
    (assets shares totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (id : Bytes32) (account : Address) :
    ∀ cs out cs',
      (repayCore mid onBehalf assets shares totalBorrowAssets_
        totalBorrowShares_).run cs = ContractResult.success out cs' →
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs').val ≤
      (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
        id account : Contract Uint256) cs).val := by
  intro cs out cs' hrun
  unfold repayCore at hrun
  by_cases hassets : assets > 0
  · rw [if_pos hassets] at hrun
    exact borrowShares_noninc_repayCoreAssets mid onBehalf assets
      totalBorrowAssets_ totalBorrowShares_ id account cs out cs' hrun
  · rw [if_neg hassets] at hrun
    exact borrowShares_noninc_repayCoreShares mid onBehalf shares
      totalBorrowAssets_ totalBorrowShares_ id account cs out cs' hrun

/-! ### Compositional preservation of the watched `collateral` packed field. -/

def PreservesCollateral {α : Type} (id : Bytes32) (account : Address) (c : Contract α) : Prop :=
  ∀ cs v cs', c cs = ContractResult.success v cs' →
    runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs' =
    runWord (structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
      id account : Contract Uint256) cs

theorem PreservesCollateral.of_storage {α : Type} {id : Bytes32} {account : Address}
    {c : Contract α} (h : StoragePreserving c) :
    PreservesCollateral id account c :=
  fun cs v cs' hrun => by
    apply runWord_structMember2At_congr
    rw [h cs v cs' hrun]

theorem PreservesCollateral.of_slot {α : Type} {id : Bytes32} {account : Address}
    {c : Contract α}
    (h : PreservesSlot (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1) c) :
    PreservesCollateral id account c :=
  fun cs v cs' hrun => runWord_structMember2At_congr _ _ _ _ _ (h cs v cs' hrun)

theorem PreservesCollateral.pure (id : Bytes32) (account : Address) {α : Type} (a : α) :
    PreservesCollateral id account (Verity.pure a) :=
  PreservesCollateral.of_storage (StoragePreserving.pure a)

theorem PreservesCollateral.bind {α β : Type} {id : Bytes32} {account : Address}
    {c : Contract α} {k : α → Contract β}
    (hc : PreservesCollateral id account c)
    (hk : ∀ a, PreservesCollateral id account (k a)) :
    PreservesCollateral id account (Verity.bind c k) := by
  intro cs v cs' h
  simp only [Verity.bind] at h
  split at h
  · next a t hcs =>
      exact (hk a t v cs' h).trans (hc cs a t hcs)
  · next => exact absurd h (by simp)

theorem PreservesCollateral.ite' {id : Bytes32} {account : Address}
    {α : Type} {c : Prop} [Decidable c] {t e : Contract α}
    (ht : PreservesCollateral id account t) (he : PreservesCollateral id account e) :
    PreservesCollateral id account (if c then t else e) := by
  split
  · exact ht
  · exact he

theorem PreservesCollateral.setStructMemberAt {κ : Type} [StorageKey κ]
    (id : Bytes32) (account : Address) (base wo : Nat) (packed : Option (Nat × Nat))
    (key : κ) (value : Uint256)
    (hne : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1
       ≠ structSlot base (StorageKey.toWord key) wo) :
    PreservesCollateral id account (setStructMemberAt base wo packed key value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMemberAt at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

theorem PreservesCollateral.setStructMember2At_other {κ₁ κ₂ : Type}
    [StorageKey κ₁] [StorageKey κ₂]
    (id : Bytes32) (account : Address) (base wo : Nat) (packed : Option (Nat × Nat))
    (k1 : κ₁) (k2 : κ₂) (value : Uint256)
    (hne : structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1
       ≠ structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) wo) :
    PreservesCollateral id account (setStructMember2At base wo packed k1 k2 value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

theorem PreservesCollateral.setBorrowShares
    (id : Bytes32) (account : Address) (k1 : Bytes32) (k2 : Address) (value : Uint256) :
    PreservesCollateral id account
      (setStructMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat)) k1 k2 value) := by
  intro cs v cs' hrun
  unfold Contracts.setStructMember2At at hrun
  unfold runWord Contracts.structMember2At
  injection hrun with _ hcs
  subst hcs
  dsimp only
  by_cases hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = true := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq, hslot]
    exact decode_encode_borrowShares_preserves_collateral _ _
  · have hbeq :
        (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ==
          structSlot2 2 (StorageKey.toWord k1) (StorageKey.toWord k2) 1) = false := by
      simpa [beq_iff_eq] using hslot
    rw [hbeq]
    simp

macro "collateral_leaf" : tactic =>
  `(tactic| first
    | with_reducible (apply PreservesCollateral.bind)
    | with_reducible (apply PreservesCollateral.ite')
    | with_reducible (exact PreservesCollateral.pure _ _ _)
    | with_reducible (apply PreservesCollateral.setBorrowShares)
    | (with_reducible (apply PreservesCollateral.setStructMember2At_other); watched_ne)
    | (with_reducible (apply PreservesCollateral.setStructMemberAt); watched_ne)
    | (with_reducible (apply PreservesCollateral.of_slot); with_reducible (apply preservesSlot_accrueInterest))
    | (with_reducible (apply PreservesCollateral.of_storage); first
        | with_reducible (apply StoragePreserving.structMemberAt)
        | with_reducible (apply StoragePreserving.structMember2At)
        | with_reducible (apply StoragePreserving.require)
        | with_reducible (apply StoragePreserving.contractAddress)
        | with_reducible (apply StoragePreserving.msgSender)
        | with_reducible (apply StoragePreserving.ecmEnvRead)
        | with_reducible (apply StoragePreserving.addPanic)
        | with_reducible (apply StoragePreserving.subPanic)
        | with_reducible (apply StoragePreserving.mulPanic)
        | with_reducible (apply StoragePreserving.blockTimestamp)
        | with_reducible (apply StoragePreserving.getStorageAddr)
        | with_reducible (apply StoragePreserving.getMapping2)
        | with_reducible (apply StoragePreserving.emitEvent)))

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem preservesCollateral_repay (mp : MarketParams) (assets shares : Uint256)
    (onBehalf : Address) (data : ByteArray) (id : Bytes32) (account : Address) :
    PreservesCollateral id account (repay mp assets shares onBehalf data) := by
  frame_simp_body repay
  repeat' first
    | collateral_leaf
    | with_reducible (intro _)

/-! ### Generated-body steps and proved structural disciplines. -/

/-- The watched position over a generated-body step: market params, market id,
    account, and the ECM oracle price the contract reads for that market. -/
structure Position where
  mp      : MarketParams
  id      : Bytes32
  account : Address
  price   : Uint256

def marketIdRead (mp : MarketParams) (cs : ContractState) : Bytes32 :=
  cs.ecmResults (Compiler.Modules.Hashing.abiEncodeStaticWordsModule "_ecm" 5).name
    [addressToWord mp.loanToken, addressToWord mp.collateralToken,
      addressToWord mp.oracle, addressToWord mp.irm, mp.lltv] 0

def oraclePriceRead (mp : MarketParams) (cs : ContractState) : Uint256 :=
  cs.ecmResults (Compiler.Modules.Oracle.oracleReadUint256Module "_ecm" 0xa035b1fe 0).name
    [addressToWord mp.oracle] 0

def MarketIdAligned (P : Position) : Prop :=
  ∀ cs, marketIdRead P.mp cs = P.id

def OraclePriceAligned (P : Position) : Prop :=
  ∀ (cs : ContractState) (selector : Nat),
    cs.ecmResults (Compiler.Modules.Oracle.oracleReadUint256Module "_ecm" selector 0).name
      [addressToWord P.mp.oracle] 0 = P.price

def NoOverflowFor (P : Position) : Prop :=
  ∀ cs, NoOverflow P.mp P.id P.account P.price cs

/-- The projected field discipline a successful step guarantees for the watched
    position: `lltv` fixed, collateral does not fall, borrow shares do not rise. -/
def MonotoneDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' →
    s'.lltv = s.lltv ∧ s.collateral ≤ s'.collateral ∧ s'.borrowShares ≤ s.borrowShares

/-- The guard discipline a successful step guarantees: the post-state is healthy,
    from the entrypoint's final `require(_isHealthy)`. -/
def GuardedDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' → healthy s'

/-- `supply`: lender-side, the watched borrow position is untouched. -/
def supplyStep (P : Position) : Step :=
  fun s s' => ∃ assets shares data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (supply P.mp assets shares P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `withdraw`: lender-side, the watched borrow position is untouched. -/
def withdrawStep (P : Position) : Step :=
  fun s s' => ∃ assets shares receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (withdraw P.mp assets shares P.account receiver).run cs
      = Verity.ContractResult.success out cs'

/-- `supplyCollateral`: the watched account's collateral does not fall. -/
def supplyCollateralStep (P : Position) : Step :=
  fun s s' => ∃ assets data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (supplyCollateral P.mp assets P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `repay`: the watched account's borrow shares do not rise. -/
def repayStep (P : Position) : Step :=
  fun s s' => ∃ assets shares data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (repay P.mp assets shares P.account data).run cs
      = Verity.ContractResult.success out cs'

/-- `borrow`: ends in `require(_isHealthy)`, so the post-state is healthy. -/
def borrowStep (P : Position) : Step :=
  fun s s' => ∃ assets shares receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (borrow P.mp assets shares P.account receiver).run cs
      = Verity.ContractResult.success out cs'

/-- `withdrawCollateral`: ends in `require(_isHealthy)`, so the post-state is healthy. -/
def withdrawCollateralStep (P : Position) : Step :=
  fun s s' => ∃ assets receiver out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (withdrawCollateral P.mp assets P.account receiver).run cs
      = Verity.ContractResult.success out cs'

/-- `liquidate`: the real generated body, run to success and projected. -/
def liquidateStep (P : Position) : Step :=
  fun s s' => ∃ seized repaid data out cs cs',
    s  = project P.mp P.id P.account P.price cs ∧
    s' = project P.mp P.id P.account P.price cs' ∧
    (liquidate P.mp P.account seized repaid data).run cs
      = Verity.ContractResult.success out cs'

set_option maxRecDepth 100000 in
theorem healthy_of_isHealthy_success (P : Position) (hprice : OraclePriceAligned P)
    (hno : NoOverflowFor P) :
    ∀ cs cs',
      (_isHealthy P.mp P.id P.account).run cs = ContractResult.success true cs' →
      healthy (project P.mp P.id P.account P.price cs) := by
  intro cs cs' hrun
  unfold Contract.run at hrun
  split at hrun
  · next x hbody =>
    injection hrun with hv hcs
    subst hv
    subst hcs
    have hbody' := hbody
    unfold _isHealthy at hbody'
    simp only [Morpho.Contract.Morpho.structMember2, Bool.and_eq_true, beq_iff_eq,
      String.reduceEq, and_true, and_false, if_true, if_false, Bind.bind, Pure.pure,
      Verity.bind, Contracts.ecmEnvRead, Verity.pure] at hbody'
    split at hbody'
    · next bs st hread =>
      have hst : st = cs := by
        unfold Contracts.structMember2At at hread
        injection hread with _ hst
        exact hst.symm
      subst st
      split at hbody'
      · next hgt =>
        have hfaith := healthFaithful_of_noOverflow P.mp P.id P.account P.price cs (hno cs)
        have hwith :
            (_isHealthyWithPrice P.mp P.id P.account P.price).run cs =
              ContractResult.success true x := by
          unfold Verity.bind Contracts.ecmEnvRead at hbody'
          simp only [hprice cs] at hbody'
          unfold Verity.pure at hbody'
          unfold Contract.run
          cases hx : _isHealthyWithPrice P.mp P.id P.account P.price cs <;>
            simp [hx] at hbody' ⊢
          exact hbody'
        exact (hfaith true x hwith).mp rfl
      · next hgt =>
        have hzero :
            (project P.mp P.id P.account P.price cs).borrowShares = 0 := by
          simp only [project, Morpho.Contract.Morpho.structMember2, Bool.and_eq_true,
            beq_iff_eq, String.reduceEq, and_true, and_false, if_true, if_false]
          change (runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
            P.id P.account : Contract Uint256) cs).val = 0
          have hreadWord :
              runWord (structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
                P.id P.account : Contract Uint256) cs = bs := by
            unfold runWord
            rw [hread]
          have hnle : ¬ bs > ZERO := hgt
          simp only [gt_iff_lt, Verity.Core.Uint256.lt_def] at hnle
          rw [hreadWord]
          have hzval : (ZERO : Uint256).val = 0 := rfl
          omega
        exact Or.inl hzero
    · simp at hbody'
  · simp at hrun

set_option maxHeartbeats 4000000 in
theorem monotoneDiscipline_supply (P : Position) : MonotoneDiscipline (supplyStep P) := by
  intro s s' h
  obtain ⟨assets, shares, data, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  let idn : Nat := StorageKey.toWord (α := Bytes32) P.id
  let an : Nat := StorageKey.toWord (α := Address) P.account
  have hslot := preservesSlot_supply P.mp assets shares P.account data
      idn an cs out cs' hbody
  have hfields := project_word1_congr P.mp P.id P.account P.price hslot
  constructor
  · rfl
  · constructor
    · rw [hfields.1]
    · rw [hfields.2]

set_option maxHeartbeats 4000000 in
theorem monotoneDiscipline_withdraw (P : Position) : MonotoneDiscipline (withdrawStep P) := by
  intro s s' h
  obtain ⟨assets, shares, receiver, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  let idn : Nat := StorageKey.toWord (α := Bytes32) P.id
  let an : Nat := StorageKey.toWord (α := Address) P.account
  have hslot := preservesSlot_withdraw P.mp assets shares P.account receiver
      idn an cs out cs' hbody
  have hfields := project_word1_congr P.mp P.id P.account P.price hslot
  constructor
  · rfl
  · constructor
    · rw [hfields.1]
    · rw [hfields.2]

theorem borrowShares_preserved_supplyCollateralStep (P : Position) :
    ∀ s s', supplyCollateralStep P s s' → s'.borrowShares = s.borrowShares := by
  intro s s' h
  obtain ⟨assets, data, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  have hb := preservesBorrowShares_supplyCollateral P.mp assets P.account data
      P.id P.account cs out cs' hbody
  exact congrArg (fun x : Uint256 => x.val) hb

theorem collateral_ge_supplyCollateralStep (P : Position) :
    ∀ s s', supplyCollateralStep P s s' → s.collateral ≤ s'.collateral := by
  intro s s' h
  obtain ⟨assets, data, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  exact collateral_nondec_supplyCollateral P.mp assets P.account data
    P.id P.account P.price cs out cs' hrun

theorem monotoneDiscipline_supplyCollateral (P : Position) :
    MonotoneDiscipline (supplyCollateralStep P) := by
  intro s s' h
  have hc := collateral_ge_supplyCollateralStep P s s' h
  have hb := borrowShares_preserved_supplyCollateralStep P s s' h
  constructor
  · cases h with
    | intro assets htail =>
        rcases htail with ⟨data, out, cs, cs', hs, hs', hrun⟩
        subst s
        subst s'
        rfl
  · constructor
    · exact hc
    · exact le_of_eq hb

theorem collateral_preserved_repayStep (P : Position) :
    ∀ s s', repayStep P s s' → s'.collateral = s.collateral := by
  intro s s' h
  obtain ⟨assets, shares, data, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  have hc := preservesCollateral_repay P.mp assets shares P.account data
      P.id P.account cs out cs' hbody
  exact congrArg (fun x : Uint256 => x.val) hc

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem borrowShares_noninc_repay (mp : MarketParams) (assets shares : Uint256)
    (onBehalf : Address) (data : ByteArray) (id : Bytes32) (account : Address)
    (price : Uint256) :
    ∀ cs out cs',
      (repay mp assets shares onBehalf data).run cs = ContractResult.success out cs' →
      (project mp id account price cs').borrowShares ≤
        (project mp id account price cs).borrowShares := by
  intro cs out cs' hrun
  have h := run_eq_of_success hrun
  simp only [repay, Bind.bind, Pure.pure,
      Morpho.Contract.contractAddress,
      Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
      Morpho.Contract.Morpho.setStructMember, Morpho.Contract.Morpho.setStructMember2,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq,
      and_true, and_false, if_true, if_false] at h
  simp only [Verity.bind, Contracts.ecmEnvRead, Contracts.structMemberAt,
    Verity.require, Verity.contractAddress] at h
  split at h
  · next x0 st0 h0 =>
    split at h
    · next x1 st1 h1 =>
      split at h
      · next x2 st2 h2 =>
        split at h
        · next accrued stAccrue haccrue =>
          let mid : Bytes32 :=
            cs.ecmResults (Compiler.Modules.Hashing.abiEncodeStaticWordsModule "_ecm" 5).name
              [addressToWord mp.loanToken, addressToWord mp.collateralToken,
                addressToWord mp.oracle, addressToWord mp.irm, mp.lltv] 0
          let tba : Uint256 :=
            StorageWord.fromWord (decodePackedWord
              (stAccrue.storage (structSlot 3 (StorageKey.toWord mid) 1)) 0 128)
          let tbs : Uint256 :=
            StorageWord.fromWord (decodePackedWord
              (stAccrue.storage (structSlot 3 (StorageKey.toWord mid) 1)) 128 128)
          have hreq0_storage : st0.storage = cs.storage :=
            StoragePreserving.require _ _ cs x0 st0 h0
          have hreq1_storage : st1.storage = st0.storage :=
            StoragePreserving.require _ _ st0 x1 st1 h1
          have hreq2_storage : st2.storage = st1.storage :=
            StoragePreserving.require _ _ st1 x2 st2 h2
          let idn : Nat := StorageKey.toWord (α := Bytes32) id
          let an : Nat := StorageKey.toWord (α := Address) account
          have haccrue_slot :
              stAccrue.storage (structSlot2 2 idn an 1) =
                st2.storage (structSlot2 2 idn an 1) := by
            exact preservesSlot_accrueInterest mp mid idn an st2 accrued stAccrue
              (by simpa [mid] using haccrue)
          have hprefix_slot :
              stAccrue.storage
                  (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1) =
                cs.storage
                  (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1) := by
            change stAccrue.storage (structSlot2 2 idn an 1) =
              cs.storage (structSlot2 2 idn an 1)
            rw [haccrue_slot, hreq2_storage, hreq1_storage, hreq0_storage]
          have hcoreRaw :
              (repayCore mid onBehalf assets shares tba tbs) stAccrue =
                ContractResult.success out cs' := by
            change (repayCore mid onBehalf assets shares tba tbs) stAccrue =
              ContractResult.success out cs' at h
            exact h
          have hcoreRun :
              (repayCore mid onBehalf assets shares tba tbs).run stAccrue =
                ContractResult.success out cs' := by
            rw [Contract.run, hcoreRaw]
          have htail := borrowShares_noninc_repayCore mid onBehalf assets shares
            tba tbs id account stAccrue out cs' hcoreRun
          have hpre_read := runWord_structMember2At_congr 2 1
            ((some (0, 128)) : Option (Nat × Nat)) id account
            (cs := cs) (cs' := stAccrue) hprefix_slot
          simp only [project, Morpho.Contract.Morpho.structMember2, Bool.and_eq_true,
            beq_iff_eq, String.reduceEq, and_true, and_false, if_true, if_false]
          change (runWord (structMember2At 2 1 (some (0, 128)) id account :
              Contract Uint256) cs').val ≤
            (runWord (structMember2At 2 1 (some (0, 128)) id account :
              Contract Uint256) cs).val
          rw [← hpre_read]
          exact htail
        · simp at h
      · simp at h
    · simp at h
  · simp at h

theorem borrowShares_noninc_repayStep (P : Position) :
    ∀ s s', repayStep P s s' → s'.borrowShares ≤ s.borrowShares := by
  intro s s' h
  obtain ⟨assets, shares, data, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  exact borrowShares_noninc_repay P.mp assets shares P.account data
    P.id P.account P.price cs out cs' hrun

theorem monotoneDiscipline_repay (P : Position) :
    MonotoneDiscipline (repayStep P) := by
  intro s s' h
  have hb := borrowShares_noninc_repayStep P s s' h
  have hc := collateral_preserved_repayStep P s s' h
  constructor
  · cases h with
    | intro assets htail =>
        rcases htail with ⟨shares, data, out, cs, cs', hs, hs', hrun⟩
        subst s
        subst s'
        rfl
  · constructor
    · exact le_of_eq hc.symm
    · exact hb

private theorem storagePreserving_borrowTail (id : Bytes32) (newTotalBorrowAssets : Uint256)
    (eventArgs : List Uint256) {α : Type} (ret : α) :
    StoragePreserving
      (Verity.bind (structMemberAt 3 0 (some (0, 128)) id : Contract Uint256)
        (fun totalSupplyAssets_ =>
          Verity.bind
            (Verity.require (decide (newTotalBorrowAssets ≤ totalSupplyAssets_))
              "insufficient liquidity")
            (fun _ =>
              Verity.bind (Verity.emitEvent "Borrow" eventArgs [])
                (fun _ => Verity.pure ret)))) := by
  apply StoragePreserving.bind
  · exact StoragePreserving.structMemberAt 3 0 (some (0, 128)) id
  intro totalSupplyAssets_
  apply StoragePreserving.bind
  · exact StoragePreserving.require (decide (newTotalBorrowAssets ≤ totalSupplyAssets_))
      "insufficient liquidity"
  intro _
  apply StoragePreserving.bind
  · exact StoragePreserving.emitEvent _ _ _
  intro _
  exact StoragePreserving.pure _

private theorem healthy_of_borrow_tail (P : Position)
    (hprice : OraclePriceAligned P) (hno : NoOverflowFor P)
    (newTotalBorrowAssets : Uint256) (eventArgs : List Uint256)
    {α : Type} (ret out : α) (stSetAssets stHealth cs' : ContractState)
    (hhealth :
      (_isHealthy P.mp P.id P.account).run stSetAssets =
        ContractResult.success true stHealth)
    (htail :
      (Verity.bind (structMemberAt 3 0 (some (0, 128)) P.id : Contract Uint256)
        (fun totalSupplyAssets_ =>
          Verity.bind
            (Verity.require (decide (newTotalBorrowAssets ≤ totalSupplyAssets_))
              "insufficient liquidity")
            (fun _ =>
              Verity.bind (Verity.emitEvent "Borrow" eventArgs [])
                (fun _ => Verity.pure ret)))) stHealth =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  have hmodel :=
    healthy_of_isHealthy_success P hprice hno stSetAssets stHealth hhealth
  have hhealth_storage : stHealth.storage = stSetAssets.storage :=
    storagePreserving_isHealthy P.mp P.id P.account stSetAssets true stHealth
      (run_eq_of_success hhealth)
  have htail_storage : cs'.storage = stHealth.storage :=
    (storagePreserving_borrowTail P.id newTotalBorrowAssets eventArgs ret)
      stHealth out cs' htail
  have hfinal_storage : cs'.storage = stSetAssets.storage := by
    rw [htail_storage, hhealth_storage]
  simpa [project_storage_congr P.mp P.id P.account P.price hfinal_storage]
    using hmodel

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem guardedDiscipline_borrowCommitAndCheck (P : Position)
    (hprice : OraclePriceAligned P) (hno : NoOverflowFor P)
    (finalAssets finalShares : Uint256) (receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    ∀ out cs cs',
      (_borrowCommitAndCheck P.mp P.id finalAssets finalShares P.account receiver sender
          totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs' →
      healthy (project P.mp P.id P.account P.price cs') := by
  intro out cs cs' hrun
  have hbody := run_eq_of_success hrun
  simp only [_borrowCommitAndCheck, Bind.bind, Pure.pure,
      Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
      Morpho.Contract.Morpho.setStructMember, Morpho.Contract.Morpho.setStructMember2,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq,
      and_true, and_false, if_true, if_false] at hbody
  simp only [Verity.bind, Contracts.structMemberAt, Verity.require] at hbody
  split at hbody
  · next currentBorrowShares stBorrowShares hborrowShares =>
    split at hbody
    · next newPosBorrowShares stNewPos hnewPos =>
      split at hbody
      · next newTotalBorrowShares stNewTotalShares hnewTotalShares =>
        split at hbody
        · next newTotalBorrowAssets stNewTotalAssets hnewTotalAssets =>
          split at hbody
          · next _ stReqPos hreqPos =>
            split at hbody
            · next _ stReqShares hreqShares =>
              split at hbody
              · next _ stReqAssets hreqAssets =>
                split at hbody
                · next _ stSetPos hsetPos =>
                  split at hbody
                  · next _ stSetShares hsetShares =>
                    split at hbody
                    · next _ stSetAssets hsetAssets =>
                      split at hbody
                      · next healthBool stHealth hhealth =>
                        split at hbody
                        · next _ stReqHealthy hreqHealthy =>
                          cases healthBool
                          · exact False.elim (by cases hreqHealthy)
                          · injection hreqHealthy with _ hstReqHealthy
                            subst hstReqHealthy
                            have hhealthyTrue :
                                (_isHealthy P.mp P.id P.account).run stSetAssets =
                                  ContractResult.success true stHealth := by
                              rw [Contract.run, hhealth]
                            exact healthy_of_borrow_tail P hprice hno newTotalBorrowAssets
                              [P.id,
                                (Core.Int256.ofUint256
                                  (Core.Uint256.ofNat sender.val)).toUint256,
                                (Core.Int256.ofUint256
                                  (Core.Uint256.ofNat P.account.val)).toUint256,
                                (Core.Int256.ofUint256
                                  (Core.Uint256.ofNat receiver.val)).toUint256,
                                finalAssets, finalShares]
                              ()
                              out stSetAssets stHealth cs' hhealthyTrue hbody
                        · exact False.elim (by cases hbody)
                      · exact False.elim (by cases hbody)
                    · exact False.elim (by cases hbody)
                  · exact False.elim (by cases hbody)
                · exact False.elim (by cases hbody)
              · exact False.elim (by cases hbody)
            · exact False.elim (by cases hbody)
          · exact False.elim (by cases hbody)
        · exact False.elim (by cases hbody)
      · exact False.elim (by cases hbody)
    · exact False.elim (by cases hbody)
  · exact False.elim (by cases hbody)

private theorem healthy_of_bind_suffix {α β : Type} (c : Contract α)
    (k : α → Contract β) (Q : ContractState → Prop)
    (hk : ∀ a out cs cs', (k a).run cs = ContractResult.success out cs' → Q cs') :
    ∀ out cs cs',
      (Verity.bind c k) cs = ContractResult.success out cs' →
      Q cs' := by
  intro out cs cs' h
  cases hc : c cs with
  | success a st =>
      have h' : k a st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      have hrun : (k a).run st = ContractResult.success out cs' := by
        rw [Contract.run, h']
      exact hk a out st cs' hrun
  | _ msg st =>
      have h' : ContractResult.revert msg st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      cases h'

private theorem healthy_of_commit_then_pure (P : Position)
    (hprice : OraclePriceAligned P) (hno : NoOverflowFor P)
    (finalAssets finalShares : Uint256) (receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256) {β : Type} (ret : β) :
    ∀ out cs cs',
      (Verity.bind
        (_borrowCommitAndCheck P.mp P.id finalAssets finalShares P.account receiver sender
          totalBorrowAssets_ totalBorrowShares_)
        (fun _ => Verity.pure ret)) cs = ContractResult.success out cs' →
      healthy (project P.mp P.id P.account P.price cs') := by
  intro out cs cs' h
  cases hc :
      _borrowCommitAndCheck P.mp P.id finalAssets finalShares P.account receiver sender
        totalBorrowAssets_ totalBorrowShares_ cs with
  | success u st =>
      have h' : Verity.pure ret st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      have hrun :
          (_borrowCommitAndCheck P.mp P.id finalAssets finalShares P.account receiver sender
            totalBorrowAssets_ totalBorrowShares_).run cs =
          ContractResult.success u st := by
        rw [Contract.run, hc]
      have hhealthy :=
        guardedDiscipline_borrowCommitAndCheck P hprice hno finalAssets finalShares
          receiver sender totalBorrowAssets_ totalBorrowShares_ u cs st hrun
      unfold Verity.pure at h'
      injection h' with _ hcs
      subst cs'
      exact hhealthy
  | _ msg st =>
      have h' : ContractResult.revert msg st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      cases h'

private theorem healthy_of_bind_pure_suffix {α : Type} (c : Contract α)
    (Q : ContractState → Prop)
    (hcprop : ∀ out cs cs', c.run cs = ContractResult.success out cs' → Q cs') :
    ∀ out cs cs',
      (Verity.bind c (fun x => Verity.pure x)) cs =
        ContractResult.success out cs' →
      Q cs' := by
  intro out cs cs' h
  cases hc : c cs with
  | success a st =>
      have h' : Verity.pure a st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      have hrun : c.run cs = ContractResult.success a st := by
        rw [Contract.run, hc]
      have hq := hcprop a cs st hrun
      unfold Verity.pure at h'
      injection h' with _ hcs
      subst cs'
      exact hq
  | _ msg st =>
      have h' : ContractResult.revert msg st = ContractResult.success out cs' := by
        simpa [Verity.bind, hc] using h
      cases h'

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem guardedDiscipline_borrowAssetsMode (P : Position)
    (hprice : OraclePriceAligned P) (hno : NoOverflowFor P)
    (assets : Uint256) (receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    ∀ out cs cs',
      (_borrowAssetsMode P.mp P.id assets P.account receiver sender
          totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs' →
      healthy (project P.mp P.id P.account P.price cs') := by
  intro out cs cs' hrun
  have hbody := run_eq_of_success hrun
  unfold _borrowAssetsMode at hbody
  simp only [Bind.bind, Pure.pure, Verity.Contract.bind_pure_left] at hbody
  exact healthy_of_bind_suffix
    (Verity.Stdlib.Math.addPanic totalBorrowShares_ 1000000)
    (fun totalBorrowSharesWithVirtual =>
      Verity.bind (Verity.Stdlib.Math.addPanic totalBorrowAssets_ 1)
        (fun totalBorrowAssetsWithVirtual =>
          Verity.bind
            (_borrowCommitAndCheck P.mp P.id assets
              (Contract.mulDivUp assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual)
              P.account receiver sender totalBorrowAssets_ totalBorrowShares_)
            (fun _ =>
              Verity.pure
                (assets,
                  Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual))))
    (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
    (by
      intro totalBorrowSharesWithVirtual out cs cs' h
      have hraw := run_eq_of_success h
      exact healthy_of_bind_suffix
        (Verity.Stdlib.Math.addPanic totalBorrowAssets_ 1)
        (fun totalBorrowAssetsWithVirtual =>
          Verity.bind
            (_borrowCommitAndCheck P.mp P.id assets
              (Contract.mulDivUp assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual)
              P.account receiver sender totalBorrowAssets_ totalBorrowShares_)
            (fun _ =>
              Verity.pure
                (assets,
                  Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual)))
        (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
        (by
          intro totalBorrowAssetsWithVirtual out cs cs' h
          have hraw := run_eq_of_success h
          exact healthy_of_commit_then_pure P hprice hno assets
            (Contract.mulDivUp assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual)
            receiver sender totalBorrowAssets_ totalBorrowShares_
            (assets,
              Contract.mulDivUp assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual)
            out cs cs' hraw)
        out cs cs' hraw)
    out cs cs' hbody

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem guardedDiscipline_borrowSharesMode (P : Position)
    (hprice : OraclePriceAligned P) (hno : NoOverflowFor P)
    (shares : Uint256) (receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    ∀ out cs cs',
      (_borrowSharesMode P.mp P.id shares P.account receiver sender
          totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs' →
      healthy (project P.mp P.id P.account P.price cs') := by
  intro out cs cs' hrun
  have hbody := run_eq_of_success hrun
  unfold _borrowSharesMode at hbody
  simp only [Bind.bind, Pure.pure, Verity.Contract.bind_pure_left] at hbody
  exact healthy_of_bind_suffix
    (Verity.Stdlib.Math.addPanic totalBorrowAssets_ 1)
    (fun totalBorrowAssetsWithVirtual =>
      Verity.bind (Verity.Stdlib.Math.addPanic totalBorrowShares_ 1000000)
        (fun totalBorrowSharesWithVirtual =>
          Verity.bind
            (_borrowCommitAndCheck P.mp P.id
              (Contract.mulDivDown shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual)
              shares P.account receiver sender totalBorrowAssets_ totalBorrowShares_)
            (fun _ =>
              Verity.pure
                (Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                  totalBorrowSharesWithVirtual,
                  shares))))
    (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
    (by
      intro totalBorrowAssetsWithVirtual out cs cs' h
      have hraw := run_eq_of_success h
      exact healthy_of_bind_suffix
        (Verity.Stdlib.Math.addPanic totalBorrowShares_ 1000000)
        (fun totalBorrowSharesWithVirtual =>
          Verity.bind
            (_borrowCommitAndCheck P.mp P.id
              (Contract.mulDivDown shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual)
              shares P.account receiver sender totalBorrowAssets_ totalBorrowShares_)
            (fun _ =>
              Verity.pure
                (Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                  totalBorrowSharesWithVirtual,
                  shares)))
        (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
        (by
          intro totalBorrowSharesWithVirtual out cs cs' h
          have hraw := run_eq_of_success h
          exact healthy_of_commit_then_pure P hprice hno
            (Contract.mulDivDown shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual)
            shares receiver sender totalBorrowAssets_ totalBorrowShares_
            (Contract.mulDivDown shares totalBorrowAssetsWithVirtual
              totalBorrowSharesWithVirtual,
              shares)
            out cs cs' hraw)
        out cs cs' hraw)
    out cs cs' hbody

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem guardedDiscipline_borrow (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P) (hno : NoOverflowFor P) :
    GuardedDiscipline (borrowStep P) := by
  intro s s' h
  obtain ⟨assets, shares, receiver, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  simp only [borrow, Bind.bind, Pure.pure,
      Morpho.Contract.Morpho.structMember,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq,
      and_true, and_false, if_true, if_false] at hbody
  simp only [Verity.bind, Contracts.ecmEnvRead, Contracts.structMemberAt,
    Verity.require, Verity.msgSender] at hbody
  have hidEq := hid cs
  simp only [marketIdRead] at hidEq
  rw [hidEq] at hbody
  split at hbody
  · next _ st0 hreq0 =>
    split at hbody
    · next _ st1 hreq1 =>
      split at hbody
      · next _ st2 hreq2 =>
        split at hbody
        · next sender stSender hsender =>
          split at hbody
          · next auth stAuth hauth =>
            split at hbody
            · next _ stReqAuth hreqAuth =>
              split at hbody
              · next hassets =>
                exact healthy_of_bind_pure_suffix
                  (_borrowAssetsMode P.mp P.id assets P.account receiver st2.sender
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 0 128))
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 128 128)))
                  (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
                  (guardedDiscipline_borrowAssetsMode P hprice hno assets receiver st2.sender
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 0 128))
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 128 128)))
                  out stReqAuth cs' hbody
              · next hassets =>
                exact healthy_of_bind_pure_suffix
                  (_borrowSharesMode P.mp P.id shares P.account receiver st2.sender
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 0 128))
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 128 128)))
                  (fun cs' => healthy (project P.mp P.id P.account P.price cs'))
                  (guardedDiscipline_borrowSharesMode P hprice hno shares receiver st2.sender
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 0 128))
                    (StorageWord.fromWord
                      (decodePackedWord
                        (stReqAuth.storage (structSlot 3 (StorageKey.toWord P.id) 1)) 128 128)))
                  out stReqAuth cs' hbody
            · exact False.elim (by cases hbody)
          · exact False.elim (by cases hbody)
        · exact False.elim (by cases hbody)
      · exact False.elim (by cases hbody)
    · exact False.elim (by cases hbody)
  · exact False.elim (by cases hbody)

set_option maxHeartbeats 4000000 in
set_option maxRecDepth 100000 in
theorem guardedDiscipline_withdrawCollateral (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P) (hno : NoOverflowFor P) :
    GuardedDiscipline (withdrawCollateralStep P) := by
  intro s s' h
  obtain ⟨assets, receiver, out, cs, cs', hs, hs', hrun⟩ := h
  subst s
  subst s'
  have hbody := run_eq_of_success hrun
  simp only [withdrawCollateral, Bind.bind, Pure.pure,
      Morpho.Contract.Morpho.structMember, Morpho.Contract.Morpho.structMember2,
      Morpho.Contract.Morpho.setStructMember2,
      Contracts.emit, Contracts.EventArg.toWord, List.mapM_cons, List.mapM_nil,
      Verity.Contract.bind_pure_left,
      Bool.and_eq_true, beq_iff_eq, String.reduceEq,
      and_true, and_false, if_true, if_false] at hbody
  simp only [Verity.bind, Contracts.ecmEnvRead, Contracts.structMemberAt,
    Verity.require, Verity.msgSender] at hbody
  have hidEq := hid cs
  simp only [marketIdRead] at hidEq
  rw [hidEq] at hbody
  split at hbody
  · next _ st0 hreq0 =>
    split at hbody
    · next _ st1 hreq1 =>
      split at hbody
      · next _ st2 hreq2 =>
        split at hbody
        · next sender stSender hsender =>
          split at hbody
          · next auth stAuth hauth =>
            split at hbody
            · next _ stReqAuth hreqAuth =>
              split at hbody
              · next accrued stAccrue haccrue =>
                split at hbody
                · next currentCollateral stCollateral hcollateral =>
                  split at hbody
                  · next _ stReqCollateral hreqCollateral =>
                    split at hbody
                    · next newCollateral stSub hsub =>
                      split at hbody
                      · next healthBool stSet hset =>
                        split at hbody
                        · next _ stHealthy hhealthy =>
                          cases healthBool
                          · simp at hhealthy
                          · simp at hhealthy
                            rw [← hhealthy] at hbody
                            have hhealthyTrue :
                                (_isHealthy P.mp P.id P.account).run stSub =
                                  ContractResult.success true stSet := by
                              rw [Contract.run, hset]
                            have hmodel :=
                              healthy_of_isHealthy_success P hprice hno stSub stSet hhealthyTrue
                            have hhealth_storage : stSet.storage = stSub.storage :=
                              storagePreserving_isHealthy P.mp P.id P.account stSub true stSet hset
                            have hemit_storage : cs'.storage = stSet.storage := by
                              change (Verity.bind
                                (Verity.emitEvent "WithdrawCollateral"
                                  [P.id, (Core.Int256.ofUint256
                                        (Core.Uint256.ofNat st2.sender.val)).toUint256,
                                    (Core.Int256.ofUint256
                                        (Core.Uint256.ofNat P.account.val)).toUint256,
                                    (Core.Int256.ofUint256
                                        (Core.Uint256.ofNat receiver.val)).toUint256,
                                    assets] [])
                                (fun _ => Verity.pure ())) stSet =
                                  ContractResult.success out cs' at hbody
                              exact (StoragePreserving.bind
                                (StoragePreserving.emitEvent _ _ _)
                                (fun _ => StoragePreserving.pure ())) stSet out cs' hbody
                            have hfinal_storage : cs'.storage = stSub.storage := by
                              rw [hemit_storage, hhealth_storage]
                            simpa [project_storage_congr P.mp P.id P.account P.price hfinal_storage]
                              using hmodel
                        · exact False.elim (by cases hbody)
                      · exact False.elim (by cases hbody)
                    · exact False.elim (by cases hbody)
                  · exact False.elim (by cases hbody)
                · exact False.elim (by cases hbody)
              · exact False.elim (by cases hbody)
            · exact False.elim (by cases hbody)
          · exact False.elim (by cases hbody)
        · exact False.elim (by cases hbody)
      · exact False.elim (by cases hbody)
    · exact False.elim (by cases hbody)
  · exact False.elim (by cases hbody)

end Morpho.Proofs.Disciplines
