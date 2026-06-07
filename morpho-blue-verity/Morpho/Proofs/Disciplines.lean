import Morpho.Proofs.Env
import Morpho.Proofs.Projection
import Morpho.Proofs.HealthFaithful
import Morpho.Proofs.FramePreserve
import Morpho.Proofs.PackedWordLemmas
import Morpho.Contract

namespace Morpho.Proofs.Disciplines

set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false

open Verity
open Contracts
open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Env
open Morpho.Proofs.Projection
open Morpho.Proofs.HealthFaithful
open Morpho.Proofs.FramePreserve
open Morpho.Contract.Morpho

/-!
  Generated-body discipline interface.

  This module is the boundary between the compact health model and the generated
  Morpho Blue contract bodies. The `Step` definitions below are intentionally
  real executable entrypoint runs projected into `HealthState`; the named
  discipline facts are explicit local obligations over those generated bodies.

  The generated-body obligations are factored into smaller structural lemmas
  below and into `Refinement.lean`. In particular, the `liquidate` pre-state
  unhealthy bridge is no longer an axiom: the generated guard is extracted after
  `_accrueInterest`, then the no-accrual identity carries the projected state
  back to the original pre-state.
-/

structure Position where
  mp      : MarketParams
  id      : Bytes32
  account : Address
  price   : Uint256

def marketIdRead (mp : MarketParams) : Contract Bytes32 := do
  ecmCall
    (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
    [addressToWord mp.loanToken, addressToWord mp.collateralToken,
      addressToWord mp.oracle, addressToWord mp.irm, mp.lltv]

def oraclePriceRead (mp : MarketParams) : Contract Uint256 := do
  ecmCall
    (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
    [addressToWord mp.oracle]

/-- Executable ECM boundary consumed by generated-body proofs.

The current Lean execution model for ECMs is a stub. This predicate names the
explicit bridge from the stubbed contract run used in `rfl` proofs to the
source-level projected id. It is intentionally not called `MarketIdAligned`,
because the source statement is `MarketParamsLib.id(P.mp) = P.id`; that hash is
still an ECM/compiler boundary in this proof layer. -/
def ExecutableMarketIdReadAligned (P : Position) : Prop :=
  ∀ cs id cs',
    (marketIdRead P.mp).run cs = ContractResult.success id cs' →
      id = P.id

/-- Executable ECM boundary consumed by generated-body proofs.

Kept explicit for the same reason as `ExecutableMarketIdReadAligned`: the
executable ECM run is not itself a proof of the source oracle value. -/
def ExecutableOraclePriceReadAligned (P : Position) : Prop :=
  ∀ cs price cs',
    (oraclePriceRead P.mp).run cs = ContractResult.success price cs' →
      price = P.price

/- Short aliases for theorem sites that discuss the ECM boundary directly. -/
abbrev MarketIdEcmAligned := ExecutableMarketIdReadAligned
abbrev OraclePriceEcmAligned := ExecutableOraclePriceReadAligned

def OraclePriceNoOverflow (P : Position) (cs : ContractState) : Prop :=
  let s := project P.mp P.id P.account P.price cs
  s.collateral * s.price < Verity.Core.Uint256.modulus ∧
  Morpho.Proofs.HealthModel.mulDivDown s.collateral s.price ORACLE_PRICE_SCALE * s.lltv
    < Verity.Core.Uint256.modulus

def BorrowTotalsNoOverflow (P : Position) (cs : ContractState) : Prop :=
  let s := project P.mp P.id P.account P.price cs
  s.totBorrowAssets + 1 < Verity.Core.Uint256.modulus ∧
  s.totBorrowShares + 1000000 < Verity.Core.Uint256.modulus

def LocalNoOverflow (P : Position) (cs : ContractState) : Prop :=
  BorrowTotalsNoOverflow P cs ∧ OraclePriceNoOverflow P cs

def LocalNoOverflowFor (P : Position) : Prop :=
  ∀ cs, LocalNoOverflow P cs

def AccrueInterestIdentityFor (P : Position) : Prop :=
  ∀ out cs cs',
    (_accrueInterest P.mp P.id).run cs = ContractResult.success out cs' →
      project P.mp P.id P.account P.price cs' =
        project P.mp P.id P.account P.price cs

def MonotoneDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' →
    s'.lltv = s.lltv ∧ s.collateral ≤ s'.collateral ∧ s'.borrowShares ≤ s.borrowShares

def GuardedDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' → healthy s'

def supplyStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (supply P.mp assets shares onBehalf data).run cs = ContractResult.success out cs'

def withdrawStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (withdraw P.mp assets shares onBehalf receiver).run cs =
        ContractResult.success out cs'

def supplyCollateralStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (supplyCollateral P.mp assets onBehalf data).run cs =
        ContractResult.success out cs'

def repayStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (repay P.mp assets shares onBehalf data).run cs = ContractResult.success out cs'

def borrowStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (borrow P.mp assets shares P.account receiver).run cs =
        ContractResult.success out cs'

def withdrawCollateralStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (withdrawCollateral P.mp assets P.account receiver).run cs =
        ContractResult.success out cs'

def liquidateStep (P : Position) : Step :=
  fun s s' =>
    ∃ seized repaid data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (liquidate P.mp P.account seized repaid data).run cs =
        ContractResult.success out cs'

theorem noOverflow_of_localNoOverflow (P : Position) (cs : ContractState) :
    LocalNoOverflow P cs →
      NoOverflow P.mp P.id P.account P.price cs := by
  intro h
  unfold LocalNoOverflow BorrowTotalsNoOverflow OraclePriceNoOverflow at h
  unfold NoOverflow
  exact ⟨h.1.1, h.1.2, h.2.1, h.2.2⟩

def positionHealthWordSlot (P : Position) : Nat :=
  Contracts.structSlot2 2 (Contracts.StorageKey.toWord P.id)
    (Contracts.StorageKey.toWord P.account) 1

theorem project_borrowShares_eq_of_positionHealthWord_eq (P : Position)
    {cs cs' : ContractState}
    (hslot : cs'.storage (positionHealthWordSlot P) =
      cs.storage (positionHealthWordSlot P)) :
    (project P.mp P.id P.account P.price cs').borrowShares =
      (project P.mp P.id P.account P.price cs).borrowShares := by
  have hread :=
    runWord_structMember2At_eq_of_storage_slot 2 1
      ((some (0, 128)) : Option (Nat × Nat)) P.id P.account cs cs' hslot
  simpa [project, positionHealthWordSlot, Morpho.Contract.Morpho.structMember2]
    using congrArg (fun x : Uint256 => x.val) hread

theorem project_collateral_eq_of_positionHealthWord_eq (P : Position)
    {cs cs' : ContractState}
    (hslot : cs'.storage (positionHealthWordSlot P) =
      cs.storage (positionHealthWordSlot P)) :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral := by
  have hread :=
    runWord_structMember2At_eq_of_storage_slot 2 1
      ((some (128, 128)) : Option (Nat × Nat)) P.id P.account cs cs' hslot
  simpa [project, positionHealthWordSlot, Morpho.Contract.Morpho.structMember2]
    using congrArg (fun x : Uint256 => x.val) hread

theorem project_borrowShares_eq_of_preserves_positionHealthWord
    (P : Position) {α : Type} {c : Contract α} {cs cs' : ContractState}
    {out : α} (hpres : PreservesSlot (positionHealthWordSlot P) c)
    (hrun : c.run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').borrowShares =
      (project P.mp P.id P.account P.price cs).borrowShares :=
  project_borrowShares_eq_of_positionHealthWord_eq P
    (hpres cs out cs' (run_eq_of_success hrun))

theorem project_collateral_eq_of_preserves_positionHealthWord
    (P : Position) {α : Type} {c : Contract α} {cs cs' : ContractState}
    {out : α} (hpres : PreservesSlot (positionHealthWordSlot P) c)
    (hrun : c.run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral :=
  project_collateral_eq_of_positionHealthWord_eq P
    (hpres cs out cs' (run_eq_of_success hrun))

theorem project_eq_of_storage_eq (P : Position) {cs cs' : ContractState}
    (hstorage : cs'.storage = cs.storage) :
    project P.mp P.id P.account P.price cs' =
      project P.mp P.id P.account P.price cs := by
  unfold project runWord Morpho.Contract.Morpho.structMember
    Morpho.Contract.Morpho.structMember2 Contracts.structMemberAt Contracts.structMember2At
  simp [hstorage]

theorem runWord_collateral_after_setCollateral_same
    (id : Bytes32) (account : Address) (value : Uint256) (cs : ContractState)
    (hval : value.val < 2 ^ 128) :
    runWord
        (Contracts.structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
          id account : Contract Uint256)
        ({ cs with
          «storage» := fun target =>
            if target = Contracts.structSlot2 2 (Contracts.StorageKey.toWord id)
                (Contracts.StorageKey.toWord account) 1 then
              Contracts.encodePackedWord
                (cs.storage (Contracts.structSlot2 2 (Contracts.StorageKey.toWord id)
                  (Contracts.StorageKey.toWord account) 1)) value 128 128
            else cs.storage target }) =
      value := by
  unfold runWord Contracts.structMember2At
  simp only
  simp only [if_true]
  exact Contracts.PackedWord.public_decode_encode_eq _ value 128 128 (by norm_num) hval

theorem runWord_borrowShares_after_setCollateral_same
    (id : Bytes32) (account : Address) (value : Uint256) (cs : ContractState) :
    runWord
        (Contracts.structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
          id account : Contract Uint256)
        ({ cs with
          «storage» := fun target =>
            if target = Contracts.structSlot2 2 (Contracts.StorageKey.toWord id)
                (Contracts.StorageKey.toWord account) 1 then
              Contracts.encodePackedWord
                (cs.storage (Contracts.structSlot2 2 (Contracts.StorageKey.toWord id)
                  (Contracts.StorageKey.toWord account) 1)) value 128 128
            else cs.storage target }) =
      runWord
        (Contracts.structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
          id account : Contract Uint256) cs := by
  unfold runWord Contracts.structMember2At
  simp only
  simp only [if_true]
  exact Contracts.PackedWord.public_decode_encode_disjoint
    (cs.storage (Contracts.structSlot2 2 (Contracts.StorageKey.toWord id)
      (Contracts.StorageKey.toWord account) 1)) value 0 128 128 128
    (by norm_num) (by norm_num) (by omega)

theorem project_borrowShares_after_setCollateral_same
    (P : Position) (value : Uint256) (cs : ContractState) :
    (project P.mp P.id P.account P.price
        ({ cs with
          «storage» := fun target =>
            if target = positionHealthWordSlot P then
              Contracts.encodePackedWord
                (cs.storage (positionHealthWordSlot P)) value 128 128
            else cs.storage target })).borrowShares =
      (project P.mp P.id P.account P.price cs).borrowShares := by
  have hread :=
    runWord_borrowShares_after_setCollateral_same P.id P.account value cs
  simpa [project, positionHealthWordSlot, Morpho.Contract.Morpho.structMember2]
    using congrArg (fun x : Uint256 => x.val) hread

theorem project_collateral_after_setCollateral_same
    (P : Position) (value : Uint256) (cs : ContractState)
    (hval : value.val < 2 ^ 128) :
    (project P.mp P.id P.account P.price
        ({ cs with
          «storage» := fun target =>
            if target = positionHealthWordSlot P then
              Contracts.encodePackedWord
                (cs.storage (positionHealthWordSlot P)) value 128 128
            else cs.storage target })).collateral =
      value.val := by
  have hread :=
    runWord_collateral_after_setCollateral_same P.id P.account value cs hval
  simpa [project, positionHealthWordSlot, Morpho.Contract.Morpho.structMember2]
    using congrArg (fun x : Uint256 => x.val) hread

theorem project_after_generated_setCollateral_same
    (P : Position) (value : Uint256) (cs cs' : ContractState)
    (hval : value.val < 2 ^ 128)
    (hrun :
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" P.id P.account "collateral" value)
        cs = ContractResult.success () cs') :
    (project P.mp P.id P.account P.price cs').collateral = value.val ∧
    (project P.mp P.id P.account P.price cs').borrowShares =
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold Morpho.Contract.Morpho.setStructMember2 at hrun
  simp at hrun
  injection hrun with _ hcs
  subst hcs
  constructor
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_eq
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 128 128 (by norm_num) hval)
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_disjoint
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 0 128 128 128 (by norm_num) (by norm_num) (by omega))

theorem project_after_generated_setCollateral_of_slot_eq
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (cs cs' : ContractState)
    (hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        positionHealthWordSlot P)
    (hval : value.val < 2 ^ 128)
    (hrun :
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id account "collateral" value)
        cs = ContractResult.success () cs') :
    (project P.mp P.id P.account P.price cs').collateral = value.val ∧
    (project P.mp P.id P.account P.price cs').borrowShares =
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold Morpho.Contract.Morpho.setStructMember2 at hrun
  simp at hrun
  injection hrun with _ hcs
  subst hcs
  constructor
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp [positionHealthWordSlot] at hslot
    simp [hslot]
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_eq
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 128 128 (by norm_num) hval)
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp [positionHealthWordSlot] at hslot
    simp [hslot]
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_disjoint
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 0 128 128 128 (by norm_num) (by norm_num) (by omega))

theorem project_after_generated_setBorrowShares_of_slot_eq
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (cs cs' : ContractState)
    (hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        positionHealthWordSlot P)
    (hval : value.val < 2 ^ 128)
    (hrun :
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id account "borrowShares" value)
        cs = ContractResult.success () cs') :
    (project P.mp P.id P.account P.price cs').borrowShares = value.val ∧
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral := by
  unfold Morpho.Contract.Morpho.setStructMember2 at hrun
  simp at hrun
  injection hrun with _ hcs
  subst hcs
  constructor
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp [positionHealthWordSlot] at hslot
    simp [hslot]
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_eq
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 0 128 (by norm_num) hval)
  · unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
    simp [positionHealthWordSlot] at hslot
    simp [hslot]
    exact congrArg (fun x : Uint256 => x.val)
      (Contracts.PackedWord.public_decode_encode_disjoint
        (cs.storage (structSlot2 2 (StorageKey.toWord P.id)
          (StorageKey.toWord P.account) 1))
        value 128 128 0 128 (by norm_num) (by norm_num) (by omega))

theorem project_collateral_eq_of_generated_collateral_read_slot_eq
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (cs csRead : ContractState)
    (hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        positionHealthWordSlot P)
    (hread :
      (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "collateral" :
        Contract Uint256) cs = ContractResult.success value csRead) :
    (project P.mp P.id P.account P.price cs).collateral = value.val := by
  unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
  simp at hread
  rcases hread with ⟨hvalue, _⟩
  unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
  simp [positionHealthWordSlot] at hslot
  simp [← hslot, hvalue]

theorem project_borrowShares_eq_of_generated_borrowShares_read_slot_eq
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (cs csRead : ContractState)
    (hslot :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 =
        positionHealthWordSlot P)
    (hread :
      (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "borrowShares" :
        Contract Uint256) cs = ContractResult.success value csRead) :
    (project P.mp P.id P.account P.price cs).borrowShares = value.val := by
  unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
  simp at hread
  rcases hread with ⟨hvalue, _⟩
  unfold project runWord Morpho.Contract.Morpho.structMember2 Contracts.structMember2At
  simp [positionHealthWordSlot] at hslot
  simp [← hslot, hvalue]

theorem generated_borrowShares_read_lt_128
    (id : Bytes32) (account : Address) (value : Uint256)
    (cs csRead : ContractState)
    (hread :
      (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "borrowShares" :
        Contract Uint256) cs = ContractResult.success value csRead) :
    value.val < 2 ^ 128 := by
  unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
  simp at hread
  rcases hread with ⟨hvalue, _⟩
  rw [← hvalue]
  exact Contracts.PackedWord.public_decode_lt_width
    (cs.storage (structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1))
    0 128

theorem preserves_positionHealthWord_setCollateral_of_slot_ne
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (hne :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ≠
        positionHealthWordSlot P) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id account "collateral" value) := by
  intro cs out cs' hrun
  unfold Morpho.Contract.Morpho.setStructMember2 at hrun
  simp at hrun
  injection hrun with _ hcs
  subst hcs
  have hne' :
      positionHealthWordSlot P ≠
        structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 := by
    intro h
    exact hne h.symm
  simp [hne']

theorem preserves_positionHealthWord_setBorrowShares_of_slot_ne
    (P : Position) (id : Bytes32) (account : Address) (value : Uint256)
    (hne :
      structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 ≠
        positionHealthWordSlot P) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id account "borrowShares" value) := by
  intro cs out cs' hrun
  unfold Morpho.Contract.Morpho.setStructMember2 at hrun
  simp at hrun
  injection hrun with _ hcs
  subst hcs
  have hne' :
      positionHealthWordSlot P ≠
        structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord account) 1 := by
    intro h
    exact hne h.symm
  simp [hne']

theorem addPanic_success_val_eq (a b out : Uint256) (cs cs' : ContractState)
    (h : Stdlib.Math.addPanic a b cs = ContractResult.success out cs') :
    cs' = cs ∧ out.val = a.val + b.val := by
  unfold Stdlib.Math.addPanic Stdlib.Math.requireSomeUint Stdlib.Math.safeAdd at h
  by_cases hgt : a.val + b.val > Verity.Core.MAX_UINT256
  · simp [hgt] at h
    unfold Functor.map instMonadContract at h
    unfold Verity.bind Verity.require at h
    injection h
  · simp [hgt] at h
    injection h with hout hcs
    subst hout
    subst hcs
    constructor
    · rfl
    · change (Verity.Core.Uint256.add a b).val = a.val + b.val
      unfold Verity.Core.Uint256.add
      rw [Verity.Core.Uint256.val_ofNat]
      apply Nat.mod_eq_of_lt
      have hle : a.val + b.val ≤ Verity.Core.MAX_UINT256 := Nat.le_of_not_gt hgt
      have hmax := Verity.Core.Uint256.max_uint256_succ_eq_modulus
      omega

theorem addPanic_success_left_le (a b out : Uint256) (cs cs' : ContractState)
    (h : Stdlib.Math.addPanic a b cs = ContractResult.success out cs') :
    a.val ≤ out.val := by
  have hval := (addPanic_success_val_eq a b out cs cs' h).2
  rw [hval]
  exact Nat.le_add_right a.val b.val

theorem subPanic_success_val_eq (a b out : Uint256) (cs cs' : ContractState)
    (h : Stdlib.Math.subPanic a b cs = ContractResult.success out cs') :
    cs' = cs ∧ out.val = a.val - b.val := by
  unfold Stdlib.Math.subPanic Stdlib.Math.requireSomeUint Stdlib.Math.safeSub at h
  by_cases hgt : b.val > a.val
  · simp [hgt] at h
    unfold Functor.map instMonadContract at h
    unfold Verity.bind Verity.require at h
    injection h
  · simp [hgt] at h
    injection h with hout hcs
    subst hout
    subst hcs
    constructor
    · rfl
    · change (Verity.Core.Uint256.sub a b).val = a.val - b.val
      have hle : b.val ≤ a.val := Nat.le_of_not_gt hgt
      unfold Verity.Core.Uint256.sub
      simp [hle]
      apply Nat.mod_eq_of_lt
      exact lt_of_le_of_lt (Nat.sub_le a.val b.val) a.isLt

theorem subPanic_success_le_left (a b out : Uint256) (cs cs' : ContractState)
    (h : Stdlib.Math.subPanic a b cs = ContractResult.success out cs') :
    out.val ≤ a.val := by
  have hval := (subPanic_success_val_eq a b out cs cs' h).2
  rw [hval]
  exact Nat.sub_le a.val b.val

theorem subPanic_success_lt_of_left_lt (a b out : Uint256) (cs cs' : ContractState)
    (hleft : a.val < 2 ^ 128)
    (h : Stdlib.Math.subPanic a b cs = ContractResult.success out cs') :
    out.val < 2 ^ 128 := by
  exact lt_of_le_of_lt (subPanic_success_le_left a b out cs cs' h) hleft

theorem bind_run_success_same_state {α β : Type}
    (ma : Contract α) (f : α → Contract β) (cs : ContractState) (a : α)
    (h : ma cs = ContractResult.success a cs)
    (out : β) (cs' : ContractState)
    (hrun : (ma >>= f).run cs = ContractResult.success out cs') :
    (f a).run cs = ContractResult.success out cs' := by
  have hraw := run_eq_of_success hrun
  show (f a).run cs = ContractResult.success out cs'
  change Verity.bind ma f cs = ContractResult.success out cs' at hraw
  unfold Verity.bind at hraw
  rw [h] at hraw
  simp [Contract.run, hraw]

theorem bind_run_success {α β : Type}
    (ma : Contract α) (f : α → Contract β) (cs csMid : ContractState) (a : α)
    (h : ma cs = ContractResult.success a csMid)
    (out : β) (cs' : ContractState)
    (hrun : (ma >>= f).run cs = ContractResult.success out cs') :
    (f a).run csMid = ContractResult.success out cs' := by
  have hraw := run_eq_of_success hrun
  show (f a).run csMid = ContractResult.success out cs'
  change Verity.bind ma f cs = ContractResult.success out cs' at hraw
  unfold Verity.bind at hraw
  rw [h] at hraw
  simp [Contract.run, hraw]

theorem bind_run_success_revert_absurd {α β : Type}
    (ma : Contract α) (f : α → Contract β) (cs csRevert : ContractState)
    (msg : String)
    (h : ma cs = ContractResult.revert msg csRevert)
    (out : β) (cs' : ContractState)
    (hrun : (ma >>= f).run cs = ContractResult.success out cs') :
    False := by
  have hraw := run_eq_of_success hrun
  change Verity.bind ma f cs = ContractResult.success out cs' at hraw
  unfold Verity.bind at hraw
  rw [h] at hraw
  contradiction

theorem bind_pure_run_success_source {α : Type}
    (c : Contract α) (out : α) (cs cs' : ContractState)
    (hrun : (do
      let x ← c
      return x).run cs = ContractResult.success out cs') :
    c.run cs = ContractResult.success out cs' := by
  have hraw := run_eq_of_success hrun
  change Verity.bind c (fun x => Verity.pure x) cs = ContractResult.success out cs' at hraw
  unfold Verity.bind at hraw
  cases hc : c cs with
  | success x csMid =>
      rw [hc] at hraw
      simp [Verity.pure] at hraw
      rcases hraw with ⟨hx, hcs⟩
      subst hx
      subst hcs
      simp [Contract.run, hc]
  | «revert» msg csMid =>
      rw [hc] at hraw
      contradiction

theorem isHealthy_success_true_implies_healthy
    (P : Position) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (hno : NoOverflow P.mp P.id P.account P.price cs)
    (hrun :
      (_isHealthy P.mp P.id P.account).run cs =
        ContractResult.success true cs') :
    healthy (project P.mp P.id P.account P.price cs) := by
  unfold _isHealthy at hrun
  cases hread :
      ((Morpho.Contract.Morpho.structMember2 "positionSlot" P.id P.account "borrowShares" :
        Contract Uint256) cs) with
  | success borrowShares csRead =>
      have hreadOrig := hread
      unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
      simp at hread
      rcases hread with ⟨_, hcsRead⟩
      subst csRead
      have hafterReadRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember2 "positionSlot" P.id P.account "borrowShares" :
            Contract Uint256)
          (fun borrowShares =>
            if borrowShares > ZERO then
              do
                let collateralPrice ← ecmCall
                  (fun resultVar =>
                    Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
                  [addressToWord P.mp.oracle]
                let healthy ← _isHealthyWithPrice P.mp P.id P.account collateralPrice
                return healthy
            else
              return true)
          cs borrowShares hreadOrig true cs' hrun
      by_cases hborrow : 0 < borrowShares.val
      · have hbranchRun :
            (do
              let collateralPrice ← ecmCall
                (fun resultVar =>
                  Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
                [addressToWord P.mp.oracle]
              let healthy ← _isHealthyWithPrice P.mp P.id P.account collateralPrice
              return healthy).run cs = ContractResult.success true cs' := by
            have hborrowZero : ZERO.val < borrowShares.val := by
              simpa [ZERO] using hborrow
            simpa [hborrowZero] using hafterReadRun
        have horacleRun :
            (oraclePriceRead P.mp).run cs = ContractResult.success (0 : Uint256) cs := by
          unfold oraclePriceRead
          rfl
        have hp : (0 : Uint256) = P.price := hprice cs (0 : Uint256) cs horacleRun
        have hecm :
            (ecmCall
              (fun resultVar =>
                Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
              [addressToWord P.mp.oracle] : Contract Uint256) cs =
              ContractResult.success (0 : Uint256) cs := by
          rfl
        have hafterOracleRun :=
          bind_run_success_same_state
            (ecmCall
              (fun resultVar =>
                Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
              [addressToWord P.mp.oracle] : Contract Uint256)
            (fun collateralPrice => do
              let healthy ← _isHealthyWithPrice P.mp P.id P.account collateralPrice
              return healthy)
            cs (0 : Uint256) hecm true cs' hbranchRun
        cases hhealth :
            ((_isHealthyWithPrice P.mp P.id P.account (0 : Uint256)) cs) with
        | success healthyVal csHealth =>
            have hafterHealthRun :=
              bind_run_success
                (_isHealthyWithPrice P.mp P.id P.account (0 : Uint256))
                (fun healthy => return healthy)
                cs csHealth healthyVal hhealth true cs' hafterOracleRun
            rw [Contract.run] at hafterHealthRun
            simp [Verity.pure] at hafterHealthRun
            injection hafterHealthRun with hval hcs
            subst healthyVal
            have hhealthRun :
                (_isHealthyWithPrice P.mp P.id P.account P.price).run cs =
                  ContractResult.success true csHealth := by
              rw [← hp]
              simp [Contract.run, hhealth]
            have hfaithful :=
              healthFaithful_of_noOverflow P.mp P.id P.account P.price cs hno
                true csHealth hhealthRun
            exact hfaithful.mp rfl
        | «revert» msg csHealth =>
            exact False.elim
              (bind_run_success_revert_absurd
                (_isHealthyWithPrice P.mp P.id P.account (0 : Uint256))
                (fun healthy => return healthy)
                cs csHealth msg hhealth true cs' hafterOracleRun)
      · have hzero : borrowShares.val = 0 := by omega
        have hslot :
            structSlot2 2 (StorageKey.toWord P.id) (StorageKey.toWord P.account) 1 =
              positionHealthWordSlot P := by
          rfl
        have hprojBorrow :=
          project_borrowShares_eq_of_generated_borrowShares_read_slot_eq P P.id P.account
            borrowShares cs cs hslot hreadOrig
        unfold healthy
        left
        exact hprojBorrow.trans hzero
  | «revert» msg csRead =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember2 "positionSlot" P.id P.account "borrowShares" :
            Contract Uint256)
          (fun borrowShares =>
            if borrowShares > ZERO then
              do
                let collateralPrice ← ecmCall
                  (fun resultVar =>
                    Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
                  [addressToWord P.mp.oracle]
                let healthy ← _isHealthyWithPrice P.mp P.id P.account collateralPrice
                return healthy
            else
              return true)
          cs csRead msg hread true cs' hrun)

theorem positionHealthWordSlot_ne_market (P : Position) (id wo : Nat)
    (hwo : wo < Morpho.Proofs.StorageFrame.MAX_WORDS) :
    positionHealthWordSlot P ≠ Contracts.structSlot 3 id wo := by
  simpa [positionHealthWordSlot, Morpho.Proofs.StorageFrame.Loc.slot]
    using
      (Morpho.Proofs.StorageFrame.Loc.slot_ne
        (a := Morpho.Proofs.StorageFrame.Loc.pos
          (Contracts.StorageKey.toWord P.id) (Contracts.StorageKey.toWord P.account) 1)
        (b := Morpho.Proofs.StorageFrame.Loc.mkt id wo)
        (by simp [Morpho.Proofs.StorageFrame.Loc.valid,
          Morpho.Proofs.StorageFrame.MAX_WORDS])
        (by simpa [Morpho.Proofs.StorageFrame.Loc.valid] using hwo)
        (by intro h; cases h))

theorem positionHealthWordSlot_ne_positionSupplyShares
    (P : Position) (id account : Nat) :
    positionHealthWordSlot P ≠ Contracts.structSlot2 2 id account 0 := by
  simpa [positionHealthWordSlot, Morpho.Proofs.StorageFrame.Loc.slot]
    using
      (Morpho.Proofs.StorageFrame.Loc.slot_ne
        (a := Morpho.Proofs.StorageFrame.Loc.pos
          (Contracts.StorageKey.toWord P.id) (Contracts.StorageKey.toWord P.account) 1)
        (b := Morpho.Proofs.StorageFrame.Loc.pos id account 0)
        (by simp [Morpho.Proofs.StorageFrame.Loc.valid,
          Morpho.Proofs.StorageFrame.MAX_WORDS])
        (by simp [Morpho.Proofs.StorageFrame.Loc.valid,
          Morpho.Proofs.StorageFrame.MAX_WORDS])
        (by intro h; cases h))

theorem positionHealthWordSlot_ne_positionHealthWord_of_word_ne
    (P : Position) (id account : Nat)
    (hne :
      id ≠ Contracts.StorageKey.toWord P.id ∨
        account ≠ Contracts.StorageKey.toWord P.account) :
    positionHealthWordSlot P ≠ Contracts.structSlot2 2 id account 1 := by
  simpa [positionHealthWordSlot, Morpho.Proofs.StorageFrame.Loc.slot]
    using
      (Morpho.Proofs.StorageFrame.Loc.slot_ne
        (a := Morpho.Proofs.StorageFrame.Loc.pos
          (Contracts.StorageKey.toWord P.id) (Contracts.StorageKey.toWord P.account) 1)
        (b := Morpho.Proofs.StorageFrame.Loc.pos id account 1)
        (by simp [Morpho.Proofs.StorageFrame.Loc.valid,
          Morpho.Proofs.StorageFrame.MAX_WORDS])
        (by simp [Morpho.Proofs.StorageFrame.Loc.valid,
          Morpho.Proofs.StorageFrame.MAX_WORDS])
        (by
          intro h
          cases h
          exact hne.elim (fun h => h rfl) (fun h => h rfl)))

theorem preserves_positionHealthWord_setMarketAt
    {κ α : Type} [Contracts.StorageKey κ] [Contracts.StorageWord α]
    (P : Position) (wo : Nat) (packed : Option (Nat × Nat)) (key : κ)
    (value : α) (hwo : wo < Morpho.Proofs.StorageFrame.MAX_WORDS) :
    PreservesSlot (positionHealthWordSlot P)
      (Contracts.setStructMemberAt 3 wo packed key value) :=
  PreservesSlot.setStructMemberAt 3 wo packed key value
    (positionHealthWordSlot P)
    (positionHealthWordSlot_ne_market P (Contracts.StorageKey.toWord key) wo hwo)

theorem storagePreserving_marketStructMember_totalSupplyAssets (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "totalSupplyAssets" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem storagePreserving_marketStructMember_totalSupplyShares (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "totalSupplyShares" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem storagePreserving_marketStructMember_totalBorrowAssets (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem storagePreserving_marketStructMember_totalBorrowShares (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem storagePreserving_marketStructMember_lastUpdate (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem storagePreserving_marketStructMember_fee (id : Bytes32) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember "marketSlot" id "fee" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember
  simp
  exact StoragePreserving.structMemberAt _ _ _ _

theorem preserves_positionHealthWord_setMarket_totalSupplyAssets
    (P : Position) (id : Bytes32) (value : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyAssets" value) := by
  unfold Morpho.Contract.Morpho.setStructMember
  simp
  exact preserves_positionHealthWord_setMarketAt P 0 (some (0, 128)) id value
    (by simp [Morpho.Proofs.StorageFrame.MAX_WORDS])

theorem preserves_positionHealthWord_setMarket_totalSupplyShares
    (P : Position) (id : Bytes32) (value : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyShares" value) := by
  unfold Morpho.Contract.Morpho.setStructMember
  simp
  exact preserves_positionHealthWord_setMarketAt P 0 (some (128, 128)) id value
    (by simp [Morpho.Proofs.StorageFrame.MAX_WORDS])

theorem preserves_positionHealthWord_setMarket_totalBorrowAssets
    (P : Position) (id : Bytes32) (value : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets" value) := by
  unfold Morpho.Contract.Morpho.setStructMember
  simp
  exact preserves_positionHealthWord_setMarketAt P 1 (some (0, 128)) id value
    (by simp [Morpho.Proofs.StorageFrame.MAX_WORDS])

theorem preserves_positionHealthWord_setMarket_totalBorrowShares
    (P : Position) (id : Bytes32) (value : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares" value) := by
  unfold Morpho.Contract.Morpho.setStructMember
  simp
  exact preserves_positionHealthWord_setMarketAt P 1 (some (128, 128)) id value
    (by simp [Morpho.Proofs.StorageFrame.MAX_WORDS])

theorem preserves_positionHealthWord_setMarket_lastUpdate
    (P : Position) (id : Bytes32) (value : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "lastUpdate" value) := by
  unfold Morpho.Contract.Morpho.setStructMember
  simp
  exact preserves_positionHealthWord_setMarketAt P 2 (some (0, 128)) id value
    (by simp [Morpho.Proofs.StorageFrame.MAX_WORDS])

theorem preserves_positionHealthWord_setSupplyShares
    {κ₁ κ₂ α : Type} [Contracts.StorageKey κ₁] [Contracts.StorageKey κ₂]
    [Contracts.StorageWord α] (P : Position) (key1 : κ₁) (key2 : κ₂)
    (value : α) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" key1 key2 "supplyShares" value) := by
  unfold Morpho.Contract.Morpho.setStructMember2
  simp only [Bool.and_eq_true, beq_iff_eq, and_self, ↓reduceIte]
  apply PreservesSlot.setStructMember2At
  exact positionHealthWordSlot_ne_positionSupplyShares P
    (Contracts.StorageKey.toWord key1) (Contracts.StorageKey.toWord key2)

theorem preserves_positionHealthWord_setCollateral_of_word_ne
    {κ₁ κ₂ α : Type} [Contracts.StorageKey κ₁] [Contracts.StorageKey κ₂]
    [Contracts.StorageWord α] (P : Position) (key1 : κ₁) (key2 : κ₂)
    (value : α)
    (hne :
      Contracts.StorageKey.toWord key1 ≠ Contracts.StorageKey.toWord P.id ∨
        Contracts.StorageKey.toWord key2 ≠ Contracts.StorageKey.toWord P.account) :
    PreservesSlot (positionHealthWordSlot P)
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" key1 key2 "collateral" value) := by
  unfold Morpho.Contract.Morpho.setStructMember2
  simp
  apply PreservesSlot.setStructMember2At
  exact positionHealthWordSlot_ne_positionHealthWord_of_word_ne P
    (Contracts.StorageKey.toWord key1) (Contracts.StorageKey.toWord key2) hne

theorem storagePreserving_positionSupplyShares (id : Bytes32) (account : Address) :
    StoragePreserving
      (Morpho.Contract.Morpho.structMember2 "positionSlot" id account "supplyShares" :
        Contract Uint256) := by
  unfold Morpho.Contract.Morpho.structMember2
  simp only [Bool.and_eq_true, beq_iff_eq, and_self, ↓reduceIte]
  exact StoragePreserving.structMember2At _ _ _ _ _

theorem storagePreserving_isSenderAuthorized (onBehalf : Address) :
    StoragePreserving (_isSenderAuthorized onBehalf) := by
  unfold _isSenderAuthorized
  apply StoragePreserving.bind StoragePreserving.msgSender
  intro sender
  apply StoragePreserving.bind (StoragePreserving.getMapping2 _ _ _)
  intro authWord
  exact StoragePreserving.pure _

theorem storagePreserving_isHealthyWithPrice
    (mp : MarketParams) (id : Bytes32) (borrower : Address) (price : Uint256) :
    StoragePreserving (_isHealthyWithPrice mp id borrower price) := by
  unfold _isHealthyWithPrice
  apply StoragePreserving.bind
    (StoragePreserving.structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id borrower)
  intro borrowShares_
  apply StoragePreserving.bind (storagePreserving_marketStructMember_totalBorrowAssets id)
  intro totalBorrowAssets_
  apply StoragePreserving.bind (storagePreserving_marketStructMember_totalBorrowShares id)
  intro totalBorrowShares_
  apply StoragePreserving.bind (StoragePreserving.addPanic totalBorrowAssets_ 1)
  intro totalBorrowAssetsWithVirtual
  apply StoragePreserving.bind (StoragePreserving.addPanic totalBorrowShares_ 1000000)
  intro totalBorrowSharesWithVirtual
  apply StoragePreserving.bind
    (StoragePreserving.structMember2At 2 1 ((some (128, 128)) : Option (Nat × Nat))
      id borrower)
  intro collateral_
  exact StoragePreserving.pure _

theorem storagePreserving_isHealthy
    (mp : MarketParams) (id : Bytes32) (borrower : Address) :
    StoragePreserving (_isHealthy mp id borrower) := by
  unfold _isHealthy
  apply StoragePreserving.bind
    (StoragePreserving.structMember2At 2 1 ((some (0, 128)) : Option (Nat × Nat))
      id borrower)
  intro borrowShares_
  by_cases hborrow : borrowShares_ > ZERO
  · simp [hborrow]
    apply StoragePreserving.bind
    · unfold _oraclePrice
      exact StoragePreserving.pure _
    · intro collateralPrice
      exact storagePreserving_isHealthyWithPrice mp id borrower collateralPrice
  · simp [hborrow]
    exact StoragePreserving.pure _

theorem storagePreserving_emitAccrueInterest
    (id borrowRateVal interest feeShares : Uint256) :
    StoragePreserving
      (Contracts.emit "AccrueInterest" [id, borrowRateVal, interest, feeShares]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitSupply
    (id sender onBehalf finalAssets finalShares : Uint256) :
    StoragePreserving
      (Contracts.emit "Supply" [id, sender, onBehalf, finalAssets, finalShares]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitWithdraw
    (id sender onBehalf receiver finalAssets finalShares : Uint256) :
    StoragePreserving
      (Contracts.emit "Withdraw" [id, sender, onBehalf, receiver, finalAssets, finalShares]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitRepay
    (id sender onBehalf finalAssets finalShares : Uint256) :
    StoragePreserving
      (Contracts.emit "Repay" [id, sender, onBehalf, finalAssets, finalShares]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitBorrow
    (id sender onBehalf receiver finalAssets finalShares : Uint256) :
    StoragePreserving
      (Contracts.emit "Borrow" [id, sender, onBehalf, receiver, finalAssets, finalShares]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitSupplyCollateral
    (id sender onBehalf assets : Uint256) :
    StoragePreserving
      (Contracts.emit "SupplyCollateral" [id, sender, onBehalf, assets]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem storagePreserving_emitWithdrawCollateral
    (id sender onBehalf receiver assets : Uint256) :
    StoragePreserving
      (Contracts.emit "WithdrawCollateral" [id, sender, onBehalf, receiver, assets]) := by
  apply StoragePreserving.emit
  intro e he
  simp [EventArg.toWord] at he ⊢
  rcases he with h | h | h | h | h <;> subst h <;> exact StoragePreserving.pure _

theorem preserves_positionHealthWord_emitAccrueInterest_then_lastUpdate
    (P : Position) (id borrowRateVal interest feeShares currentTimestamp : Uint256) :
    PreservesSlot (positionHealthWordSlot P)
      (do
        let y ←
          Contracts.emit "AccrueInterest"
            [id, borrowRateVal, interest, feeShares]
        let y ← Morpho.Contract.Morpho.setStructMember
          "marketSlot" id "lastUpdate" currentTimestamp
        Verity.pure ZERO) := by
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _
      (storagePreserving_emitAccrueInterest id borrowRateVal interest feeShares)
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_lastUpdate P id currentTimestamp
  intro y
  exact PreservesSlot.pure _ _

theorem preserves_positionHealthWord_accrueInterestAt (P : Position) (id : Bytes32) :
    PreservesSlot (positionHealthWordSlot P)
      (_accrueInterest P.mp id) := by
  unfold _accrueInterest
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_lastUpdate id)
  intro currentLastUpdate
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (by
      unfold Morpho.Contract.blockTimestamp
      exact StoragePreserving.blockTimestamp)
  intro currentTimestamp
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic currentTimestamp currentLastUpdate)
  intro elapsed
  apply PreservesSlot.ite'
  · apply PreservesSlot.ite'
    · apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalBorrowAssets id)
      intro totalBorrowAssets_
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalBorrowShares id)
      intro totalBorrowShares_
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyAssets id)
      intro totalSupplyAssets_
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyShares id)
      intro totalSupplyShares_
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_fee id)
      intro currentFee
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.pure 0)
      intro borrowRateVal
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.mulPanic borrowRateVal elapsed)
      intro firstTerm
      let secondTerm := Morpho.Contract.mulDivDown firstTerm firstTerm 2000000000000000000
      let thirdTerm := Morpho.Contract.mulDivDown secondTerm firstTerm 3000000000000000000
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic secondTerm thirdTerm)
      intro secondPlusThird
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic firstTerm secondPlusThird)
      intro compounded
      let interest := Morpho.Contract.mulDivDown totalBorrowAssets_ compounded 1000000000000000000
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalBorrowAssets_ interest)
      intro newTotalBorrowAssets
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ interest)
      intro newTotalSupplyAssets
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
      intro y
      apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
      intro y
      apply PreservesSlot.bind
      · exact preserves_positionHealthWord_setMarket_totalBorrowAssets P id newTotalBorrowAssets
      intro y
      apply PreservesSlot.bind
      · exact preserves_positionHealthWord_setMarket_totalSupplyAssets P id newTotalSupplyAssets
      intro y
      apply PreservesSlot.ite'
      · let feeAmount := Morpho.Contract.mulDivDown interest currentFee 1000000000000000000
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ 1000000)
        intro totalSupplySharesWithVirtual
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic newTotalSupplyAssets feeAmount)
        intro supplyAssetsAfterFee
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic supplyAssetsAfterFee 1)
        intro supplyAssetsAfterFeeWithVirtual
        let feeShares := Morpho.Contract.mulDivDown feeAmount totalSupplySharesWithVirtual
          supplyAssetsAfterFeeWithVirtual
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.getStorageAddr feeRecipientSlot)
        intro feeRecipient_
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (storagePreserving_positionSupplyShares id feeRecipient_)
        intro currentFeeRecipientShares
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic currentFeeRecipientShares feeShares)
        intro newFeeRecipientShares
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ feeShares)
        intro newTotalSupplyShares
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
        intro y
        apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
        intro y
        apply PreservesSlot.bind
        · exact preserves_positionHealthWord_setSupplyShares P id feeRecipient_ newFeeRecipientShares
        intro y
        apply PreservesSlot.bind
        · exact preserves_positionHealthWord_setMarket_totalSupplyShares P id newTotalSupplyShares
        intro y
        exact preserves_positionHealthWord_emitAccrueInterest_then_lastUpdate P id borrowRateVal interest
          feeShares currentTimestamp
      · apply PreservesSlot.bind
        · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
        intro y
        exact preserves_positionHealthWord_emitAccrueInterest_then_lastUpdate P id borrowRateVal interest
          ZERO currentTimestamp
    · apply PreservesSlot.bind
      · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
      intro y
      apply PreservesSlot.bind
      · exact preserves_positionHealthWord_setMarket_lastUpdate P id currentTimestamp
      intro y
      exact PreservesSlot.pure _ _
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
    intro y
    exact PreservesSlot.pure _ _

theorem preserves_positionHealthWord_accrueInterest (P : Position) :
    PreservesSlot (positionHealthWordSlot P)
      (_accrueInterest P.mp P.id) :=
  preserves_positionHealthWord_accrueInterestAt P P.id

def supplyCommit (mp : MarketParams) (id totalSupplyAssets_ totalSupplyShares_
    finalAssets finalShares : Uint256) (onBehalf : Address) (thisAddress : Address) :
    Contract (Uint256 × Uint256) := do
  let currentSupplyShares <-
    Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "supplyShares"
  let newPosSupplyShares ← Stdlib.Math.addPanic currentSupplyShares finalShares
  let newTotalSupplyShares ← Stdlib.Math.addPanic totalSupplyShares_ finalShares
  let newTotalSupplyAssets ← Stdlib.Math.addPanic totalSupplyAssets_ finalAssets
  require (newPosSupplyShares <= 340282366920938463463374607431768211455) "uint128 overflow"
  require (newTotalSupplyShares <= 340282366920938463463374607431768211455) "uint128 overflow"
  require (newTotalSupplyAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "supplyShares"
    newPosSupplyShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyShares"
    newTotalSupplyShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyAssets"
    newTotalSupplyAssets
  let sender <- msgSender
  emit "Supply" [id, sender, onBehalf, finalAssets, finalShares]
  ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0x2075be03 1 "data")
    [addressToWord sender, finalAssets]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferFromModule
    [addressToWord mp.loanToken, addressToWord sender, addressToWord thisAddress, finalAssets]
  return (finalAssets, finalShares)

theorem preserves_positionHealthWord_supplyCommit
    (P : Position) (id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares : Uint256)
    (onBehalf : Address) (thisAddress : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (supplyCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
        onBehalf thisAddress) := by
  unfold supplyCommit
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_positionSupplyShares id onBehalf)
  intro currentSupplyShares
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic currentSupplyShares finalShares)
  intro newPosSupplyShares
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ finalShares)
  intro newTotalSupplyShares
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ finalAssets)
  intro newTotalSupplyAssets
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setSupplyShares P id onBehalf newPosSupplyShares
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_totalSupplyShares P id newTotalSupplyShares
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_totalSupplyAssets P id newTotalSupplyAssets
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ StoragePreserving.msgSender
  intro sender
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _
      (storagePreserving_emitSupply id (addressToWord sender) (addressToWord onBehalf)
        finalAssets finalShares)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
  intro y
  exact PreservesSlot.pure _ _

theorem preserves_positionHealthWord_supplyAfterTotals
    (P : Position) (id totalSupplyAssets_ totalSupplyShares_ assets shares : Uint256)
    (onBehalf : Address) (thisAddress : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (have finalAssets := assets
       have finalShares := shares
       if assets > 0 then
         do
           let totalSupplySharesWithVirtual ← Stdlib.Math.addPanic totalSupplyShares_ 1000000
           let totalSupplyAssetsWithVirtual ← Stdlib.Math.addPanic totalSupplyAssets_ 1
           let finalShares :=
             Morpho.Contract.mulDivDown assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual
           supplyCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
             onBehalf thisAddress
       else
         do
           let totalSupplyAssetsWithVirtual ← Stdlib.Math.addPanic totalSupplyAssets_ 1
           let totalSupplySharesWithVirtual ← Stdlib.Math.addPanic totalSupplyShares_ 1000000
           let finalAssets :=
             Morpho.Contract.mulDivUp shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual
           supplyCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
             onBehalf thisAddress) := by
  apply PreservesSlot.ite'
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ 1000000)
    intro totalSupplySharesWithVirtual
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ 1)
    intro totalSupplyAssetsWithVirtual
    exact preserves_positionHealthWord_supplyCommit P id totalSupplyAssets_ totalSupplyShares_
      assets
      (Morpho.Contract.mulDivDown assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual)
      onBehalf thisAddress
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ 1)
    intro totalSupplyAssetsWithVirtual
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ 1000000)
    intro totalSupplySharesWithVirtual
    exact preserves_positionHealthWord_supplyCommit P id totalSupplyAssets_ totalSupplyShares_
      (Morpho.Contract.mulDivUp shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual)
      shares onBehalf thisAddress

theorem preserves_positionHealthWord_supply
    (P : Position) (assets shares : Uint256) (onBehalf : Address) (data : ByteArray) :
    PreservesSlot (positionHealthWordSlot P)
      (supply P.mp assets shares onBehalf data) := by
  unfold supply
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (by
      unfold Morpho.Contract.contractAddress
      exact StoragePreserving.contractAddress)
  intro thisAddress
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure 0)
  intro id
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_lastUpdate id)
  intro currentLastUpdate
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_accrueInterestAt P id
  intro accrued
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyAssets id)
  intro totalSupplyAssets_
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyShares id)
  intro totalSupplyShares_
  exact preserves_positionHealthWord_supplyAfterTotals P id totalSupplyAssets_ totalSupplyShares_
    assets shares onBehalf thisAddress

theorem monotoneDiscipline_supply_of_preserves_positionHealthWord
    (P : Position)
    (hpres : ∀ assets shares onBehalf data,
      PreservesSlot (positionHealthWordSlot P)
        (supply P.mp assets shares onBehalf data)) :
    MonotoneDiscipline (supplyStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, shares, onBehalf, data, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  have hb :=
    project_borrowShares_eq_of_preserves_positionHealthWord P
      (hpres assets shares onBehalf data) hrun
  have hc :=
    project_collateral_eq_of_preserves_positionHealthWord P
      (hpres assets shares onBehalf data) hrun
  constructor
  · rfl
  constructor
  · rw [hc]
  · rw [hb]

theorem monotoneDiscipline_supply (P : Position) :
    MonotoneDiscipline (supplyStep P) :=
  monotoneDiscipline_supply_of_preserves_positionHealthWord P
    (preserves_positionHealthWord_supply P)

def withdrawCommit (mp : MarketParams) (id totalSupplyAssets_ totalSupplyShares_
    finalAssets finalShares : Uint256) (onBehalf receiver sender : Address) :
    Contract (Uint256 × Uint256) := do
  let currentSupplyShares <-
    Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "supplyShares"
  require (currentSupplyShares >= finalShares) "insufficient balance"
  let newTotalSupplyAssets ← Stdlib.Math.subPanic totalSupplyAssets_ finalAssets
  let newPositionSupplyShares ← Stdlib.Math.subPanic currentSupplyShares finalShares
  let newTotalSupplyShares ← Stdlib.Math.subPanic totalSupplyShares_ finalShares
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "supplyShares"
    newPositionSupplyShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyShares"
    newTotalSupplyShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyAssets"
    newTotalSupplyAssets
  let totalBorrowAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets"
  require (totalBorrowAssets_ <= newTotalSupplyAssets) "insufficient liquidity"
  emit "Withdraw" [id, sender, onBehalf, receiver, finalAssets, finalShares]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferModule
    [addressToWord mp.loanToken, addressToWord receiver, finalAssets]
  return (finalAssets, finalShares)

theorem preserves_positionHealthWord_withdrawCommit
    (P : Position) (id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (withdrawCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
        onBehalf receiver sender) := by
  unfold withdrawCommit
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_positionSupplyShares id onBehalf)
  intro currentSupplyShares
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic totalSupplyAssets_ finalAssets)
  intro newTotalSupplyAssets
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic currentSupplyShares finalShares)
  intro newPositionSupplyShares
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic totalSupplyShares_ finalShares)
  intro newTotalSupplyShares
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setSupplyShares P id onBehalf newPositionSupplyShares
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_totalSupplyShares P id newTotalSupplyShares
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_totalSupplyAssets P id newTotalSupplyAssets
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalBorrowAssets id)
  intro totalBorrowAssets_
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _
      (storagePreserving_emitWithdraw id (addressToWord sender) (addressToWord onBehalf)
        (addressToWord receiver) finalAssets finalShares)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
  intro y
  exact PreservesSlot.pure _ _

theorem monotoneDiscipline_withdraw_of_preserves_positionHealthWord
    (P : Position)
    (hpres : ∀ assets shares onBehalf receiver,
      PreservesSlot (positionHealthWordSlot P)
        (withdraw P.mp assets shares onBehalf receiver)) :
    MonotoneDiscipline (withdrawStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, shares, onBehalf, receiver, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  have hb :=
    project_borrowShares_eq_of_preserves_positionHealthWord P
      (hpres assets shares onBehalf receiver) hrun
  have hc :=
    project_collateral_eq_of_preserves_positionHealthWord P
      (hpres assets shares onBehalf receiver) hrun
  constructor
  · rfl
  constructor
  · rw [hc]
  · rw [hb]

theorem preserves_positionHealthWord_withdrawAfterTotals
    (P : Position) (id totalSupplyAssets_ totalSupplyShares_ assets shares : Uint256)
    (onBehalf receiver sender : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (have finalAssets := assets
       have finalShares := shares
       if assets > 0 then
         do
           let totalSupplySharesWithVirtual ← Stdlib.Math.addPanic totalSupplyShares_ 1000000
           let totalSupplyAssetsWithVirtual ← Stdlib.Math.addPanic totalSupplyAssets_ 1
           let finalShares :=
             Morpho.Contract.mulDivUp assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual
           withdrawCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
             onBehalf receiver sender
       else
         do
           let totalSupplyAssetsWithVirtual ← Stdlib.Math.addPanic totalSupplyAssets_ 1
           let totalSupplySharesWithVirtual ← Stdlib.Math.addPanic totalSupplyShares_ 1000000
           let finalAssets :=
             Morpho.Contract.mulDivDown shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual
           withdrawCommit P.mp id totalSupplyAssets_ totalSupplyShares_ finalAssets finalShares
             onBehalf receiver sender) := by
  apply PreservesSlot.ite'
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ 1000000)
    intro totalSupplySharesWithVirtual
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ 1)
    intro totalSupplyAssetsWithVirtual
    exact preserves_positionHealthWord_withdrawCommit P id totalSupplyAssets_ totalSupplyShares_
      assets
      (Morpho.Contract.mulDivUp assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual)
      onBehalf receiver sender
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyAssets_ 1)
    intro totalSupplyAssetsWithVirtual
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic totalSupplyShares_ 1000000)
    intro totalSupplySharesWithVirtual
    exact preserves_positionHealthWord_withdrawCommit P id totalSupplyAssets_ totalSupplyShares_
      (Morpho.Contract.mulDivDown shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual)
      shares onBehalf receiver sender

theorem preserves_positionHealthWord_withdraw
    (P : Position) (assets shares : Uint256) (onBehalf receiver : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (withdraw P.mp assets shares onBehalf receiver) := by
  unfold withdraw
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure 0)
  intro id
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_lastUpdate id)
  intro currentLastUpdate
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ StoragePreserving.msgSender
  intro sender
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_isSenderAuthorized onBehalf)
  intro isAuthorizedSender
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_accrueInterestAt P id
  intro accrued
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyAssets id)
  intro totalSupplyAssets_
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (storagePreserving_marketStructMember_totalSupplyShares id)
  intro totalSupplyShares_
  exact preserves_positionHealthWord_withdrawAfterTotals P id totalSupplyAssets_
    totalSupplyShares_ assets shares onBehalf receiver sender

theorem monotoneDiscipline_withdraw (P : Position) :
    MonotoneDiscipline (withdrawStep P) :=
  monotoneDiscipline_withdraw_of_preserves_positionHealthWord P
    (preserves_positionHealthWord_withdraw P)

def supplyCollateralCommit (mp : MarketParams) (id assets : Uint256)
    (onBehalf : Address) (thisAddress : Address) : Contract Unit := do
  let currentCollateral <-
    Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "collateral"
  let newCollateral ← Stdlib.Math.addPanic currentCollateral assets
  require (newCollateral <= 340282366920938463463374607431768211455) "uint128 overflow"
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral"
    newCollateral
  let sender <- msgSender
  emit "SupplyCollateral" [id, sender, onBehalf, assets]
  ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0xb1022fdf 1 "data")
    [addressToWord sender, assets]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferFromModule
    [addressToWord mp.collateralToken, addressToWord sender, addressToWord thisAddress, assets]

theorem preserves_positionHealthWord_supplyCollateralCommit_of_word_ne
    (P : Position) (id assets : Uint256) (onBehalf : Address) (thisAddress : Address)
    (hne :
      Contracts.StorageKey.toWord id ≠ Contracts.StorageKey.toWord P.id ∨
        Contracts.StorageKey.toWord onBehalf ≠ Contracts.StorageKey.toWord P.account) :
    PreservesSlot (positionHealthWordSlot P)
      (supplyCollateralCommit P.mp id assets onBehalf thisAddress) := by
  unfold supplyCollateralCommit
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (by
      unfold Morpho.Contract.Morpho.structMember2
      simp only [Bool.and_eq_true, beq_iff_eq, and_self, ↓reduceIte]
      exact StoragePreserving.structMember2At
        (κ₁ := Uint256) (κ₂ := Address) (α := Uint256)
        2 1 (some (128, 128)) id onBehalf)
  intro currentCollateral
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.addPanic currentCollateral assets)
  intro newCollateral
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.require _ _)
  intro y
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setCollateral_of_word_ne P id onBehalf newCollateral hne
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ StoragePreserving.msgSender
  intro sender
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _
      (storagePreserving_emitSupplyCollateral id (addressToWord sender) (addressToWord onBehalf)
        assets)
  intro y
  apply PreservesSlot.bind
  · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
  intro y
  exact PreservesSlot.of_storage _ (StoragePreserving.pure ())

theorem supplyCollateral_success_shape
    (P : Position) (assets : Uint256) (onBehalf : Address) (data : ByteArray)
    (out : Unit) (cs cs' : ContractState)
    (hrun : (supplyCollateral P.mp assets onBehalf data).run cs =
      ContractResult.success out cs') :
    ∃ currentCollateral newCollateral csSet,
      (Morpho.Contract.Morpho.structMember2 "positionSlot" (0 : Bytes32)
        onBehalf "collateral" : Contract Uint256) cs =
          ContractResult.success currentCollateral cs ∧
      Stdlib.Math.addPanic currentCollateral assets cs =
          ContractResult.success newCollateral cs ∧
      newCollateral.val ≤
        Core.Uint256.val 340282366920938463463374607431768211455 ∧
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" (0 : Bytes32)
        onBehalf "collateral" newCollateral) cs =
          ContractResult.success () csSet ∧
      cs'.storage = csSet.storage := by
  unfold supplyCollateral at hrun
  simp [Contract.run, instMonadContract, Verity.bind, Verity.pure,
    Morpho.Contract.contractAddress, Verity.contractAddress,
    Morpho.Contract.Morpho._marketParamsId] at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" (0 : Bytes32) "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      rw [hlast] at hrun
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      by_cases hmarket : currentLastUpdate = ZERO
      · simp [hmarket, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
      · simp [hmarket, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
        by_cases hassets : 0 < assets.val
        · simp [hassets, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
          by_cases haddr : onBehalf = 0
          · simp [haddr, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
          · simp [haddr, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
            cases hcoll :
                ((Morpho.Contract.Morpho.structMember2 "positionSlot" (0 : Bytes32)
                  onBehalf "collateral" : Contract Uint256) cs) with
            | success currentCollateral csColl =>
                have hcollOrig := hcoll
                rw [hcoll] at hrun
                unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hcoll
                simp at hcoll
                rcases hcoll with ⟨_, hcsColl⟩
                subst csColl
                simp at hrun
                cases hadd : Stdlib.Math.addPanic currentCollateral assets cs with
                | success newCollateral csAdd =>
                    rw [hadd] at hrun
                    have hcsAdd := (addPanic_success_val_eq currentCollateral assets
                      newCollateral cs csAdd hadd).1
                    subst csAdd
                    by_cases huint :
                        newCollateral.val ≤
                          Core.Uint256.val 340282366920938463463374607431768211455
                    · simp [huint, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
                      cases hset :
                          (Morpho.Contract.Morpho.setStructMember2 "positionSlot" (0 : Bytes32)
                            onBehalf "collateral" newCollateral) cs with
                      | success y csSet =>
                          rw [hset] at hrun
                          cases y
                          simp [Contracts.emit, Contracts.EventArg.toWord, instMonadContract,
                            Verity.bind, Verity.pure, Verity.msgSender, Verity.emitEvent] at hrun
                          refine ⟨currentCollateral, newCollateral, csSet, ?_, hadd, huint, hset, ?_⟩
                          · rfl
                          · rw [← hrun]
                      | «revert» msg csSet =>
                          unfold Morpho.Contract.Morpho.setStructMember2 Contracts.setStructMember2At at hset
                          simp at hset
                    · simp [huint, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
                | «revert» msg csAdd =>
                    rw [hadd] at hrun
                    simp at hrun
            | «revert» msg csColl =>
                unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hcoll
                simp at hcoll
        · simp [hassets, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun
  | «revert» msg csLast =>
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast

theorem monotoneDiscipline_supplyCollateral (P : Position) :
    MonotoneDiscipline (supplyCollateralStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, onBehalf, data, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  rcases supplyCollateral_success_shape P assets onBehalf data out cs cs' hrun with
    ⟨currentCollateral, newCollateral, csSet, hread, hadd, huint, hset, hstorage⟩
  have hsetRun :
      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" (0 : Bytes32)
        onBehalf "collateral" newCollateral).run cs =
          ContractResult.success () csSet := by
    rw [Contract.run, hset]
  have hfinalBorrow :
      (project P.mp P.id P.account P.price cs').borrowShares =
        (project P.mp P.id P.account P.price csSet).borrowShares :=
    project_borrowShares_eq_of_positionHealthWord_eq P (by rw [hstorage])
  have hfinalCollateral :
      (project P.mp P.id P.account P.price cs').collateral =
        (project P.mp P.id P.account P.price csSet).collateral :=
    project_collateral_eq_of_positionHealthWord_eq P (by rw [hstorage])
  constructor
  · rfl
  constructor
  · by_cases hslot :
        structSlot2 2 (StorageKey.toWord (0 : Bytes32)) (StorageKey.toWord onBehalf) 1 =
          positionHealthWordSlot P
    · have hval : newCollateral.val < 2 ^ 128 := by
        change newCollateral.val ≤
          (Verity.Core.Uint256.ofNat 340282366920938463463374607431768211455).val
          at huint
        rw [Verity.Core.Uint256.val_ofNat] at huint
        norm_num [Verity.Core.Uint256.modulus, Verity.Core.UINT256_MODULUS] at huint ⊢
        omega
      have hold :=
        project_collateral_eq_of_generated_collateral_read_slot_eq P (0 : Bytes32)
          onBehalf currentCollateral cs cs hslot hread
      have hwrite :=
        project_after_generated_setCollateral_of_slot_eq P (0 : Bytes32) onBehalf
          newCollateral cs csSet hslot hval hset
      have hmono := addPanic_success_left_le currentCollateral assets newCollateral cs cs hadd
      calc
        (project P.mp P.id P.account P.price cs).collateral = currentCollateral.val := hold
        _ ≤ newCollateral.val := hmono
        _ = (project P.mp P.id P.account P.price csSet).collateral := hwrite.1.symm
        _ = (project P.mp P.id P.account P.price cs').collateral := hfinalCollateral.symm
    · have hpres :=
        preserves_positionHealthWord_setCollateral_of_slot_ne P (0 : Bytes32) onBehalf
          newCollateral hslot
      have hsame :=
        project_collateral_eq_of_positionHealthWord_eq P
          (hpres cs () csSet hset)
      apply Nat.le_of_eq
      calc
        (project P.mp P.id P.account P.price cs).collateral =
            (project P.mp P.id P.account P.price csSet).collateral := hsame.symm
        _ = (project P.mp P.id P.account P.price cs').collateral := hfinalCollateral.symm
  · by_cases hslot :
        structSlot2 2 (StorageKey.toWord (0 : Bytes32)) (StorageKey.toWord onBehalf) 1 =
          positionHealthWordSlot P
    · have hval : newCollateral.val < 2 ^ 128 := by
        change newCollateral.val ≤
          (Verity.Core.Uint256.ofNat 340282366920938463463374607431768211455).val
          at huint
        rw [Verity.Core.Uint256.val_ofNat] at huint
        norm_num [Verity.Core.Uint256.modulus, Verity.Core.UINT256_MODULUS] at huint ⊢
        omega
      have hwrite :=
        project_after_generated_setCollateral_of_slot_eq P (0 : Bytes32) onBehalf
          newCollateral cs csSet hslot hval hset
      apply Nat.le_of_eq
      calc
        (project P.mp P.id P.account P.price cs').borrowShares =
            (project P.mp P.id P.account P.price csSet).borrowShares := hfinalBorrow
        _ = (project P.mp P.id P.account P.price cs).borrowShares := hwrite.2
    · have hpres :=
        preserves_positionHealthWord_setCollateral_of_slot_ne P (0 : Bytes32) onBehalf
          newCollateral hslot
      have hsame :=
        project_borrowShares_eq_of_positionHealthWord_eq P
          (hpres cs () csSet hset)
      apply Nat.le_of_eq
      calc
        (project P.mp P.id P.account P.price cs').borrowShares =
            (project P.mp P.id P.account P.price csSet).borrowShares := hfinalBorrow
        _ = (project P.mp P.id P.account P.price cs).borrowShares := hsame

def repayEmitTransferTail (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf thisAddress : Address) : Contract (Uint256 × Uint256) := do
  let sender <- msgSender
  emit "Repay" [id, sender, onBehalf, finalAssets, finalShares]
  ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0x05b4591c 1 "data")
    [addressToWord sender, finalAssets]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferFromModule
    [addressToWord mp.loanToken, addressToWord sender, addressToWord thisAddress, finalAssets]
  return (finalAssets, finalShares)

def repayAfterBorrowWriteTail (mp : MarketParams) (id totalBorrowAssets_
    finalAssets finalShares newTotalBorrowShares : Uint256) (onBehalf thisAddress : Address) :
    Contract (Uint256 × Uint256) := do
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
    newTotalBorrowShares
  if totalBorrowAssets_ >= finalAssets then
    let newTotalBorrowAssets ← Stdlib.Math.subPanic totalBorrowAssets_ finalAssets
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
      newTotalBorrowAssets
    repayEmitTransferTail mp id finalAssets finalShares onBehalf thisAddress
  else
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets" ZERO
    repayEmitTransferTail mp id finalAssets finalShares onBehalf thisAddress

def repayAfterFinal (mp : MarketParams) (id totalBorrowAssets_ totalBorrowShares_
    finalAssets finalShares : Uint256) (onBehalf thisAddress : Address) :
    Contract (Uint256 × Uint256) := do
  let currentBorrowShares <-
    Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares"
  require (currentBorrowShares >= finalShares) "insufficient balance"
  let newPositionBorrowShares ← Stdlib.Math.subPanic currentBorrowShares finalShares
  let newTotalBorrowShares ← Stdlib.Math.subPanic totalBorrowShares_ finalShares
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
    newPositionBorrowShares
  repayAfterBorrowWriteTail mp id totalBorrowAssets_ finalAssets finalShares
    newTotalBorrowShares onBehalf thisAddress

def repayAfterTotalsAssets (mp : MarketParams) (id totalBorrowAssets_ totalBorrowShares_
    assets : Uint256) (onBehalf thisAddress : Address) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
  let finalShares :=
    Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual
  repayAfterFinal mp id totalBorrowAssets_ totalBorrowShares_ assets finalShares
    onBehalf thisAddress

def repayAfterTotalsShares (mp : MarketParams) (id totalBorrowAssets_ totalBorrowShares_
    shares : Uint256) (onBehalf thisAddress : Address) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
  let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let finalAssets :=
    Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
  repayAfterFinal mp id totalBorrowAssets_ totalBorrowShares_ finalAssets shares
    onBehalf thisAddress

def repayAfterTotals (mp : MarketParams) (id totalBorrowAssets_ totalBorrowShares_
    assets shares : Uint256) (onBehalf thisAddress : Address) :
    Contract (Uint256 × Uint256) :=
  if assets > 0 then
    repayAfterTotalsAssets mp id totalBorrowAssets_ totalBorrowShares_ assets
      onBehalf thisAddress
  else
    repayAfterTotalsShares mp id totalBorrowAssets_ totalBorrowShares_ shares
      onBehalf thisAddress

def repayAfterAccrue (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf thisAddress : Address) : Contract (Uint256 × Uint256) := do
  let totalBorrowAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets"
  let totalBorrowShares_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
  repayAfterTotals mp id totalBorrowAssets_ totalBorrowShares_ assets shares onBehalf thisAddress

def repayAfterId (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf thisAddress : Address) : Contract (Uint256 × Uint256) := do
  let currentLastUpdate <- Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate"
  require (currentLastUpdate != ZERO) "market not created"
  require ((assets == 0) != (shares == 0)) "inconsistent input"
  require (onBehalf != 0) "zero address"
  let _accrued ← _accrueInterest mp id
  repayAfterAccrue mp id assets shares onBehalf thisAddress

def repayAfterThisAddress (mp : MarketParams) (assets shares : Uint256)
    (onBehalf thisAddress : Address) : Contract (Uint256 × Uint256) := do
  let _ignoredMarketParams := mp
  let id ← ecmCall
    (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
    [addressToWord mp.loanToken, addressToWord mp.collateralToken,
      addressToWord mp.oracle, addressToWord mp.irm, mp.lltv]
  repayAfterId mp id assets shares onBehalf thisAddress

theorem preserves_positionHealthWord_repayAfterBorrowWriteTail
    (P : Position) (id totalBorrowAssets_ finalAssets finalShares newTotalBorrowShares : Uint256)
    (onBehalf thisAddress : Address) :
    PreservesSlot (positionHealthWordSlot P)
      (repayAfterBorrowWriteTail P.mp id totalBorrowAssets_ finalAssets finalShares
        newTotalBorrowShares onBehalf thisAddress) := by
  have htail :
      PreservesSlot (positionHealthWordSlot P)
        (repayEmitTransferTail P.mp id finalAssets finalShares onBehalf thisAddress) := by
    unfold repayEmitTransferTail
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ StoragePreserving.msgSender
    intro sender
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _
        (storagePreserving_emitRepay id (addressToWord sender) (addressToWord onBehalf)
          finalAssets finalShares)
    intro y
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
    intro y
    apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.pure ())
    intro y
    exact PreservesSlot.pure _ _
  unfold repayAfterBorrowWriteTail
  apply PreservesSlot.bind
  · exact preserves_positionHealthWord_setMarket_totalBorrowShares P id newTotalBorrowShares
  intro y
  apply PreservesSlot.ite'
  · apply PreservesSlot.bind
    · exact PreservesSlot.of_storage _ (StoragePreserving.subPanic totalBorrowAssets_ finalAssets)
    intro newTotalBorrowAssets
    apply PreservesSlot.bind
    · exact preserves_positionHealthWord_setMarket_totalBorrowAssets P id newTotalBorrowAssets
    intro y
    exact htail
  · apply PreservesSlot.bind
    · exact preserves_positionHealthWord_setMarket_totalBorrowAssets P id ZERO
    intro y
    exact htail

theorem repayAfterFinal_monotone
    (P : Position) (id totalBorrowAssets_ totalBorrowShares_ finalAssets finalShares : Uint256)
    (onBehalf thisAddress : Address) (out : Uint256 × Uint256)
    (cs cs' : ContractState)
    (hrun :
      (repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ finalAssets finalShares
        onBehalf thisAddress).run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  have hrun' := run_eq_of_success hrun
  unfold repayAfterFinal at hrun'
  cases hread :
      ((Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares" :
        Contract Uint256) cs) with
  | success currentBorrowShares csRead =>
      simp [instMonadContract, Verity.bind, hread] at hrun'
      have hreadOrig := hread
      unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
      simp at hread
      rcases hread with ⟨_, hcsRead⟩
      subst csRead
      by_cases hge : finalShares.val ≤ currentBorrowShares.val
      · simp [hge, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun'
        cases hsubPos :
            Stdlib.Math.subPanic currentBorrowShares finalShares cs with
        | success newPositionBorrowShares csSubPos =>
            simp [hsubPos] at hrun'
            have hcsSubPos :=
              (subPanic_success_val_eq currentBorrowShares finalShares
                newPositionBorrowShares cs csSubPos hsubPos).1
            subst csSubPos
            cases hsubTot :
                Stdlib.Math.subPanic totalBorrowShares_ finalShares cs with
            | success newTotalBorrowShares csSubTot =>
                simp [hsubTot] at hrun'
                have hcsSubTot :=
                  (subPanic_success_val_eq totalBorrowShares_ finalShares
                    newTotalBorrowShares cs csSubTot hsubTot).1
                subst csSubTot
                cases hset :
                    (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                      "borrowShares" newPositionBorrowShares) cs with
                | success y csSet =>
                    simp [hset] at hrun'
                    cases y
                    have htailPres :=
                      preserves_positionHealthWord_repayAfterBorrowWriteTail P id
                        totalBorrowAssets_ finalAssets finalShares newTotalBorrowShares
                        onBehalf thisAddress
                    have htailStorage :
                        cs'.storage (positionHealthWordSlot P) =
                          csSet.storage (positionHealthWordSlot P) :=
                      htailPres csSet out cs' hrun'
                    have hfinalCollateral :
                        (project P.mp P.id P.account P.price cs').collateral =
                          (project P.mp P.id P.account P.price csSet).collateral :=
                      project_collateral_eq_of_positionHealthWord_eq P htailStorage
                    have hfinalBorrow :
                        (project P.mp P.id P.account P.price cs').borrowShares =
                          (project P.mp P.id P.account P.price csSet).borrowShares :=
                      project_borrowShares_eq_of_positionHealthWord_eq P htailStorage
                    constructor
                    · by_cases hslot :
                          structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord onBehalf) 1 =
                            positionHealthWordSlot P
                      · have hleft :=
                            generated_borrowShares_read_lt_128 id onBehalf
                              currentBorrowShares cs cs hreadOrig
                        have hnewLt :=
                          subPanic_success_lt_of_left_lt currentBorrowShares finalShares
                            newPositionBorrowShares cs cs hleft hsubPos
                        have hwrite :=
                          project_after_generated_setBorrowShares_of_slot_eq P id onBehalf
                            newPositionBorrowShares cs csSet hslot hnewLt hset
                        calc
                          (project P.mp P.id P.account P.price cs').collateral =
                              (project P.mp P.id P.account P.price csSet).collateral :=
                            hfinalCollateral
                          _ = (project P.mp P.id P.account P.price cs).collateral :=
                            hwrite.2
                      · have hpres :=
                            preserves_positionHealthWord_setBorrowShares_of_slot_ne P id onBehalf
                              newPositionBorrowShares hslot
                        have hsame :=
                          project_collateral_eq_of_positionHealthWord_eq P
                            (hpres cs () csSet hset)
                        calc
                          (project P.mp P.id P.account P.price cs').collateral =
                              (project P.mp P.id P.account P.price csSet).collateral :=
                            hfinalCollateral
                          _ = (project P.mp P.id P.account P.price cs).collateral := hsame
                    · by_cases hslot :
                          structSlot2 2 (StorageKey.toWord id) (StorageKey.toWord onBehalf) 1 =
                            positionHealthWordSlot P
                      · have hcurrent :=
                          project_borrowShares_eq_of_generated_borrowShares_read_slot_eq P id
                            onBehalf currentBorrowShares cs cs hslot hreadOrig
                        have hleft :=
                            generated_borrowShares_read_lt_128 id onBehalf
                              currentBorrowShares cs cs hreadOrig
                        have hnewLt :=
                          subPanic_success_lt_of_left_lt currentBorrowShares finalShares
                            newPositionBorrowShares cs cs hleft hsubPos
                        have hwrite :=
                          project_after_generated_setBorrowShares_of_slot_eq P id onBehalf
                            newPositionBorrowShares cs csSet hslot hnewLt hset
                        calc
                          (project P.mp P.id P.account P.price cs').borrowShares =
                              (project P.mp P.id P.account P.price csSet).borrowShares :=
                            hfinalBorrow
                          _ = newPositionBorrowShares.val := hwrite.1
                          _ ≤ currentBorrowShares.val :=
                            subPanic_success_le_left currentBorrowShares finalShares
                              newPositionBorrowShares cs cs hsubPos
                          _ = (project P.mp P.id P.account P.price cs).borrowShares :=
                            hcurrent.symm
                      · have hpres :=
                            preserves_positionHealthWord_setBorrowShares_of_slot_ne P id onBehalf
                              newPositionBorrowShares hslot
                        have hsame :=
                          project_borrowShares_eq_of_positionHealthWord_eq P
                            (hpres cs () csSet hset)
                        apply Nat.le_of_eq
                        calc
                          (project P.mp P.id P.account P.price cs').borrowShares =
                              (project P.mp P.id P.account P.price csSet).borrowShares :=
                            hfinalBorrow
                          _ = (project P.mp P.id P.account P.price cs).borrowShares := hsame
                | «revert» msg csSet =>
                    simp [hset] at hrun'
            | «revert» msg csSubTot =>
                simp [hsubTot] at hrun'
        | «revert» msg csSubPos =>
            simp [hsubPos] at hrun'
      · simp [hge, Verity.require, instMonadContract, Verity.bind, Contract.run] at hrun'
  | «revert» msg csRead =>
      simp [instMonadContract, Verity.bind, hread] at hrun'

theorem repayAfterTotals_assets_monotone
    (P : Position) (id totalBorrowAssets_ totalBorrowShares_ assets shares : Uint256)
    (onBehalf thisAddress : Address) (out : Uint256 × Uint256)
    (cs cs' : ContractState)
    (hassets : 0 < assets.val)
    (hrun :
      (repayAfterTotalsAssets P.mp id totalBorrowAssets_ totalBorrowShares_ assets
        onBehalf thisAddress).run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterTotalsAssets at hrun
  cases haddShares :
      Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
  | success totalBorrowSharesWithVirtual csAddShares =>
      have hcsAddShares :=
        (addPanic_success_val_eq totalBorrowShares_ 1000000
          totalBorrowSharesWithVirtual cs csAddShares haddShares).1
      subst csAddShares
      have hafterSharesRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ assets finalShares
              onBehalf thisAddress)
          cs totalBorrowSharesWithVirtual haddShares out cs' hrun
      cases haddAssets :
          Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
      | success totalBorrowAssetsWithVirtual csAddAssets =>
          have hcsAddAssets :=
            (addPanic_success_val_eq totalBorrowAssets_ 1
              totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
          subst csAddAssets
          have hfinalRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual =>
                repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ assets
                  (Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual)
                  onBehalf thisAddress)
              cs totalBorrowAssetsWithVirtual haddAssets out cs' hafterSharesRun
          exact repayAfterFinal_monotone P id totalBorrowAssets_ totalBorrowShares_
            assets
            (Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
              totalBorrowAssetsWithVirtual)
            onBehalf thisAddress out cs cs' hfinalRun
      | «revert» msg csAddAssets =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual =>
                repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ assets
                  (Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual)
                  onBehalf thisAddress)
              cs csAddAssets msg haddAssets out cs' hafterSharesRun)
  | «revert» msg csAddShares =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivDown assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ assets finalShares
              onBehalf thisAddress)
          cs csAddShares msg haddShares out cs' hrun)

theorem repayAfterTotals_shares_monotone
    (P : Position) (id totalBorrowAssets_ totalBorrowShares_ assets shares : Uint256)
    (onBehalf thisAddress : Address) (out : Uint256 × Uint256)
    (cs cs' : ContractState)
    (hassets : ¬ 0 < assets.val)
    (hrun :
      (repayAfterTotalsShares P.mp id totalBorrowAssets_ totalBorrowShares_ shares
        onBehalf thisAddress).run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterTotalsShares at hrun
  cases haddAssets :
      Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
  | success totalBorrowAssetsWithVirtual csAddAssets =>
      have hcsAddAssets :=
        (addPanic_success_val_eq totalBorrowAssets_ 1
          totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
      subst csAddAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ finalAssets shares
              onBehalf thisAddress)
          cs totalBorrowAssetsWithVirtual haddAssets out cs' hrun
      cases haddShares :
          Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
      | success totalBorrowSharesWithVirtual csAddShares =>
          have hcsAddShares :=
            (addPanic_success_val_eq totalBorrowShares_ 1000000
              totalBorrowSharesWithVirtual cs csAddShares haddShares).1
          subst csAddShares
          have hfinalRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual =>
                repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_
                  (Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual)
                  shares onBehalf thisAddress)
              cs totalBorrowSharesWithVirtual haddShares out cs' hafterAssetsRun
          exact repayAfterFinal_monotone P id totalBorrowAssets_ totalBorrowShares_
            (Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
              totalBorrowSharesWithVirtual)
            shares onBehalf thisAddress out cs cs' hfinalRun
      | «revert» msg csAddShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual =>
                repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_
                  (Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual)
                  shares onBehalf thisAddress)
              cs csAddShares msg haddShares out cs' hafterAssetsRun)
  | «revert» msg csAddAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivUp shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            repayAfterFinal P.mp id totalBorrowAssets_ totalBorrowShares_ finalAssets shares
              onBehalf thisAddress)
          cs csAddAssets msg haddAssets out cs' hrun)

theorem repayAfterTotals_monotone
    (P : Position) (id totalBorrowAssets_ totalBorrowShares_ assets shares : Uint256)
    (onBehalf thisAddress : Address) (out : Uint256 × Uint256)
    (cs cs' : ContractState)
    (hrun :
      (repayAfterTotals P.mp id totalBorrowAssets_ totalBorrowShares_ assets shares
        onBehalf thisAddress).run cs = ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterTotals at hrun
  by_cases hassets : 0 < assets.val
  · exact repayAfterTotals_assets_monotone P id totalBorrowAssets_ totalBorrowShares_
      assets shares onBehalf thisAddress out cs cs' hassets (by
        simpa [hassets] using hrun)
  · exact repayAfterTotals_shares_monotone P id totalBorrowAssets_ totalBorrowShares_
      assets shares onBehalf thisAddress out cs cs' hassets (by
        simpa [hassets] using hrun)

theorem repayAfterAccrue_monotone
    (P : Position) (id assets shares : Uint256) (onBehalf thisAddress : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hrun :
      (repayAfterAccrue P.mp id assets shares onBehalf thisAddress).run cs =
        ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterAccrue at hrun
  cases hassetsRead :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
        Contract Uint256) cs) with
  | success totalBorrowAssets_ csAssets =>
      have hassetsReadOrig := hassetsRead
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hassetsRead
      simp at hassetsRead
      rcases hassetsRead with ⟨_, hcsAssets⟩
      subst csAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            repayAfterTotals P.mp id totalBorrowAssets_ totalBorrowShares_ assets shares
              onBehalf thisAddress)
          cs totalBorrowAssets_ hassetsReadOrig out cs' hrun
      cases hsharesRead :
          ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
            Contract Uint256) cs) with
      | success totalBorrowShares_ csShares =>
          have hsharesReadOrig := hsharesRead
          unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hsharesRead
          simp at hsharesRead
          rcases hsharesRead with ⟨_, hcsShares⟩
          subst csShares
          have htotalsRun :=
            bind_run_success_same_state
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                repayAfterTotals P.mp id totalBorrowAssets_ totalBorrowShares_ assets shares
                  onBehalf thisAddress)
              cs totalBorrowShares_ hsharesReadOrig out cs' hafterAssetsRun
          exact repayAfterTotals_monotone P id totalBorrowAssets_ totalBorrowShares_
            assets shares onBehalf thisAddress out cs cs' htotalsRun
      | «revert» msg csShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                repayAfterTotals P.mp id totalBorrowAssets_ totalBorrowShares_ assets shares
                  onBehalf thisAddress)
              cs csShares msg hsharesRead out cs' hafterAssetsRun)
  | «revert» msg csAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            repayAfterTotals P.mp id totalBorrowAssets_ totalBorrowShares_ assets shares
              onBehalf thisAddress)
          cs csAssets msg hassetsRead out cs' hrun)

theorem repayAfterId_monotone
    (P : Position) (id assets shares : Uint256) (onBehalf thisAddress : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hrun :
      (repayAfterId P.mp id assets shares onBehalf thisAddress).run cs =
        ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterId at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      have hlastOrig := hlast
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      have hafterLastRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (onBehalf != 0) "zero address"
            let _accrued ← _accrueInterest P.mp id
            repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
          cs currentLastUpdate hlastOrig out cs' hrun
      cases hreqMarket :
          (require (currentLastUpdate != ZERO) "market not created" cs) with
      | success y csReqMarket =>
          cases y
          have hreqMarketOrig := hreqMarket
          unfold Verity.require at hreqMarket
          split at hreqMarket
          · injection hreqMarket with _ hcsReqMarket
            subst csReqMarket
            have hafterReqMarketRun :=
              bind_run_success_same_state
                (require (currentLastUpdate != ZERO) "market not created")
                (fun _ => do
                  require ((assets == 0) != (shares == 0)) "inconsistent input"
                  require (onBehalf != 0) "zero address"
                  let _accrued ← _accrueInterest P.mp id
                  repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                cs () hreqMarketOrig out cs' hafterLastRun
            cases hreqInput :
                (require ((assets == 0) != (shares == 0)) "inconsistent input" cs) with
            | success y csReqInput =>
                cases y
                have hreqInputOrig := hreqInput
                unfold Verity.require at hreqInput
                split at hreqInput
                · injection hreqInput with _ hcsReqInput
                  subst csReqInput
                  have hafterReqInputRun :=
                    bind_run_success_same_state
                      (require ((assets == 0) != (shares == 0)) "inconsistent input")
                      (fun _ => do
                        require (onBehalf != 0) "zero address"
                        let _accrued ← _accrueInterest P.mp id
                        repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                      cs () hreqInputOrig out cs' hafterReqMarketRun
                  cases hreqOnBehalf :
                      (require (onBehalf != 0) "zero address" cs) with
                  | success y csReqOnBehalf =>
                      cases y
                      have hreqOnBehalfOrig := hreqOnBehalf
                      unfold Verity.require at hreqOnBehalf
                      split at hreqOnBehalf
                      · injection hreqOnBehalf with _ hcsReqOnBehalf
                        subst csReqOnBehalf
                        have hafterReqOnBehalfRun :=
                          bind_run_success_same_state
                            (require (onBehalf != 0) "zero address")
                            (fun _ => do
                              let _accrued ← _accrueInterest P.mp id
                              repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                            cs () hreqOnBehalfOrig out cs' hafterReqInputRun
                        cases haccrue : (_accrueInterest P.mp id cs) with
                        | success accrued csAccrued =>
                            have hafterAccrueRun :=
                              bind_run_success
                                (_accrueInterest P.mp id)
                                (fun _ =>
                                  repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                                cs csAccrued accrued haccrue out cs' hafterReqOnBehalfRun
                            have hsuffix :=
                              repayAfterAccrue_monotone P id assets shares onBehalf
                                thisAddress out csAccrued cs' hafterAccrueRun
                            have haccSlot :
                                csAccrued.storage (positionHealthWordSlot P) =
                                  cs.storage (positionHealthWordSlot P) :=
                              preserves_positionHealthWord_accrueInterestAt P id
                                cs accrued csAccrued haccrue
                            have haccCollateral :=
                              project_collateral_eq_of_positionHealthWord_eq P haccSlot
                            have haccBorrow :=
                              project_borrowShares_eq_of_positionHealthWord_eq P haccSlot
                            constructor
                            · calc
                                (project P.mp P.id P.account P.price cs').collateral =
                                    (project P.mp P.id P.account P.price csAccrued).collateral :=
                                  hsuffix.1
                                _ = (project P.mp P.id P.account P.price cs).collateral :=
                                  haccCollateral
                            · calc
                                (project P.mp P.id P.account P.price cs').borrowShares ≤
                                    (project P.mp P.id P.account P.price csAccrued).borrowShares :=
                                  hsuffix.2
                                _ = (project P.mp P.id P.account P.price cs).borrowShares :=
                                  haccBorrow
                        | «revert» msg csAccrued =>
                            exact False.elim
                              (bind_run_success_revert_absurd
                                (_accrueInterest P.mp id)
                                (fun _ =>
                                  repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                                cs csAccrued msg haccrue out cs' hafterReqOnBehalfRun)
                      · injection hreqOnBehalf
                  | «revert» msg csReqOnBehalf =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (require (onBehalf != 0) "zero address")
                          (fun _ => do
                            let _accrued ← _accrueInterest P.mp id
                            repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                          cs csReqOnBehalf msg hreqOnBehalf out cs' hafterReqInputRun)
                · injection hreqInput
            | «revert» msg csReqInput =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (require ((assets == 0) != (shares == 0)) "inconsistent input")
                    (fun _ => do
                      require (onBehalf != 0) "zero address"
                      let _accrued ← _accrueInterest P.mp id
                      repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
                    cs csReqInput msg hreqInput out cs' hafterReqMarketRun)
          · injection hreqMarket
      | «revert» msg csReqMarket =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentLastUpdate != ZERO) "market not created")
              (fun _ => do
                require ((assets == 0) != (shares == 0)) "inconsistent input"
                require (onBehalf != 0) "zero address"
                let _accrued ← _accrueInterest P.mp id
                repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
              cs csReqMarket msg hreqMarket out cs' hafterLastRun)
  | «revert» msg csLast =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (onBehalf != 0) "zero address"
            let _accrued ← _accrueInterest P.mp id
            repayAfterAccrue P.mp id assets shares onBehalf thisAddress)
          cs csLast msg hlast out cs' hrun)

theorem repayAfterThisAddress_monotone
    (P : Position) (assets shares : Uint256) (onBehalf thisAddress : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hrun :
      (repayAfterThisAddress P.mp assets shares onBehalf thisAddress).run cs =
        ContractResult.success out cs') :
    (project P.mp P.id P.account P.price cs').collateral =
      (project P.mp P.id P.account P.price cs).collateral ∧
    (project P.mp P.id P.account P.price cs').borrowShares ≤
      (project P.mp P.id P.account P.price cs).borrowShares := by
  unfold repayAfterThisAddress at hrun
  have hecm :
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256) cs =
        ContractResult.success (0 : Uint256) cs := by
    rfl
  have hafterIdRun :=
    bind_run_success_same_state
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256)
      (fun id => repayAfterId P.mp id assets shares onBehalf thisAddress)
      cs (0 : Uint256) hecm out cs' hrun
  exact repayAfterId_monotone P (0 : Uint256) assets shares onBehalf thisAddress
    out cs cs' hafterIdRun

theorem monotoneDiscipline_repay (P : Position) :
    MonotoneDiscipline (repayStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, shares, onBehalf, data, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  unfold repay at hrun
  change
    (do
      let thisAddress ← Morpho.Contract.contractAddress
      repayAfterThisAddress P.mp assets shares onBehalf thisAddress).run cs =
        ContractResult.success out cs' at hrun
  cases hthis : Morpho.Contract.contractAddress cs with
  | success thisAddress csThis =>
      have hthisOrig := hthis
      unfold Morpho.Contract.contractAddress Verity.contractAddress at hthis
      injection hthis with _ hcsThis
      subst csThis
      have hafterThisRun :=
        bind_run_success_same_state
          Morpho.Contract.contractAddress
          (fun thisAddress =>
            repayAfterThisAddress P.mp assets shares onBehalf thisAddress)
          cs thisAddress hthisOrig out cs' hrun
      rcases repayAfterThisAddress_monotone P assets shares onBehalf thisAddress
        out cs cs' hafterThisRun with ⟨hcollateral, hborrowShares⟩
      constructor
      · rfl
      constructor
      · rw [hcollateral]
      · exact hborrowShares
  | «revert» msg csThis =>
      exact False.elim
        (bind_run_success_revert_absurd
          Morpho.Contract.contractAddress
          (fun thisAddress =>
            repayAfterThisAddress P.mp assets shares onBehalf thisAddress)
          cs csThis msg hthis out cs' hrun)

def borrowAfterHealthTail (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newTotalBorrowAssets : Uint256)
    (healthy : Bool) : Contract (Uint256 × Uint256) := do
  require healthy "insufficient collateral"
  let totalSupplyAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalSupplyAssets"
  require (newTotalBorrowAssets <= totalSupplyAssets_) "insufficient liquidity"
  emit "Borrow" [id, sender, onBehalf, receiver, finalAssets, finalShares]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferModule
    [addressToWord mp.loanToken, addressToWord receiver, finalAssets]
  return (finalAssets, finalShares)

def borrowCommitAndCheck (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let currentBorrowShares <-
    Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares"
  let newPosBorrowShares ← Stdlib.Math.addPanic currentBorrowShares finalShares
  let newTotalBorrowShares ← Stdlib.Math.addPanic totalBorrowShares_ finalShares
  let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
  require (newPosBorrowShares <= 340282366920938463463374607431768211455) "uint128 overflow"
  require (newTotalBorrowShares <= 340282366920938463463374607431768211455) "uint128 overflow"
  require (newTotalBorrowAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
    newPosBorrowShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
    newTotalBorrowShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
    newTotalBorrowAssets
  let healthy ← _isHealthy mp id onBehalf
  borrowAfterHealthTail mp id finalAssets finalShares onBehalf receiver sender
    newTotalBorrowAssets healthy

def borrowAssetsMode (mp : MarketParams) (id assets : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
  let finalShares :=
    Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual
  borrowCommitAndCheck mp id assets finalShares onBehalf receiver sender
    totalBorrowAssets_ totalBorrowShares_

def borrowSharesMode (mp : MarketParams) (id shares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) := do
  let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
  let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
  let finalAssets :=
    Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
  borrowCommitAndCheck mp id finalAssets shares onBehalf receiver sender
    totalBorrowAssets_ totalBorrowShares_

def borrowAfterTotals (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256) :
    Contract (Uint256 × Uint256) :=
  if assets > 0 then
    borrowAssetsMode mp id assets onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_
  else
    borrowSharesMode mp id shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_

def borrowAfterAccrue (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf receiver sender : Address) : Contract (Uint256 × Uint256) := do
  let totalBorrowAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets"
  let totalBorrowShares_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
  borrowAfterTotals mp id assets shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_

def borrowAfterMarketId (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf receiver : Address) : Contract (Uint256 × Uint256) := do
  let currentLastUpdate <- Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate"
  require (currentLastUpdate != ZERO) "market not created"
  require ((assets == 0) != (shares == 0)) "inconsistent input"
  require (receiver != 0) "zero address"
  let sender <- msgSender
  let isAuthorizedSender ← _isSenderAuthorized onBehalf
  require isAuthorizedSender "unauthorized"
  let _accrued ← _accrueInterest mp id
  borrowAfterAccrue mp id assets shares onBehalf receiver sender

def generatedBorrowAfterAccrueReturn (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf receiver sender : Address) : Contract (Uint256 × Uint256) := do
  let (finalAssets, finalShares) ←
    _borrowAfterAccrue mp id assets shares onBehalf receiver sender
  return (finalAssets, finalShares)

def generatedBorrowAfterMarketId (mp : MarketParams) (id assets shares : Uint256)
    (onBehalf receiver : Address) : Contract (Uint256 × Uint256) := do
  let currentLastUpdate <- Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate"
  require (currentLastUpdate != ZERO) "market not created"
  require ((assets == 0) != (shares == 0)) "inconsistent input"
  require (receiver != 0) "zero address"
  let sender <- msgSender
  let isAuthorizedSender ← _isSenderAuthorized onBehalf
  require isAuthorizedSender "unauthorized"
  let _accrued ← _accrueInterest mp id
  generatedBorrowAfterAccrueReturn mp id assets shares onBehalf receiver sender

def withdrawCollateralAfterHealthTail (mp : MarketParams) (id assets : Uint256)
    (onBehalf receiver sender : Address) (healthyFlag : Bool) : Contract Unit := do
  require healthyFlag "insufficient collateral"
  emit "WithdrawCollateral" [id, sender, onBehalf, receiver, assets]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferModule
    [addressToWord mp.collateralToken, addressToWord receiver, assets]

def withdrawCollateralAfterWrite (mp : MarketParams) (id assets : Uint256)
    (onBehalf receiver sender : Address) : Contract Unit := do
  let healthy ← _isHealthy mp id onBehalf
  withdrawCollateralAfterHealthTail mp id assets onBehalf receiver sender healthy

def generatedWithdrawCollateralAfterAccrue (mp : MarketParams) (id assets : Uint256)
    (onBehalf receiver sender : Address) : Contract Unit := do
  let currentCollateral <- Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "collateral"
  require (currentCollateral >= assets) "insufficient collateral"
  let newCollateral ← Stdlib.Math.subPanic currentCollateral assets
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral" newCollateral
  withdrawCollateralAfterWrite mp id assets onBehalf receiver sender

def generatedWithdrawCollateralAfterMarketId (mp : MarketParams) (id assets : Uint256)
    (onBehalf receiver : Address) : Contract Unit := do
  let currentLastUpdate <- Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate"
  require (currentLastUpdate != ZERO) "market not created"
  require (assets > 0) "zero assets"
  require (receiver != 0) "zero address"
  let sender <- msgSender
  let isAuthorizedSender ← _isSenderAuthorized onBehalf
  require isAuthorizedSender "unauthorized"
  let _accrued ← _accrueInterest mp id
  generatedWithdrawCollateralAfterAccrue mp id assets onBehalf receiver sender

def liquidateAfterUnhealthyGuard (mp : MarketParams) (id : Bytes32) (borrower : Address)
    (seizedAssets repaidShares : Uint256) (data : Bytes) (collateralPrice : Uint256) :
    Contract (Uint256 × Uint256) := do
  let lltv := mp.lltv
  let wadMinusLltv ← Stdlib.Math.subPanic 1000000000000000000 lltv
  let cursorTerm := Morpho.Contract.mulDivDown 300000000000000000 wadMinusLltv 1000000000000000000
  let denominator ← Stdlib.Math.subPanic 1000000000000000000 cursorTerm
  let computedLIF :=
    Morpho.Contract.mulDivDown 1000000000000000000 1000000000000000000 denominator
  let lif := min 1150000000000000000 computedLIF
  let totalBorrowAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets"
  let totalBorrowShares_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
  let totalSupplyAssets_ <- Morpho.Contract.Morpho.structMember "marketSlot" id "totalSupplyAssets"
  let mut finalSeizedAssets := seizedAssets
  let mut finalRepaidShares := repaidShares
  let mut repaidAssets := ZERO
  if seizedAssets > 0 then
    let seizedQuoted :=
      Morpho.Contract.mulDivUp seizedAssets collateralPrice 1000000000000000000000000000000000000
    repaidAssets := Morpho.Contract.mulDivUp seizedQuoted 1000000000000000000 lif
    let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
    let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
    finalRepaidShares :=
      Morpho.Contract.mulDivUp repaidAssets totalBorrowSharesWithVirtual
        totalBorrowAssetsWithVirtual
  else
    let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
    let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
    repaidAssets :=
      Morpho.Contract.mulDivDown finalRepaidShares totalBorrowAssetsWithVirtual
        totalBorrowSharesWithVirtual
    let seizedValue := Morpho.Contract.mulDivDown repaidAssets lif 1000000000000000000
    finalSeizedAssets :=
      Morpho.Contract.mulDivDown seizedValue 1000000000000000000000000000000000000
        collateralPrice
  let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
  let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
  repaidAssets :=
    Morpho.Contract.mulDivUp finalRepaidShares totalBorrowAssetsWithVirtual
      totalBorrowSharesWithVirtual
  let currentBorrowShares <- Morpho.Contract.Morpho.structMember2 "positionSlot" id borrower "borrowShares"
  require (currentBorrowShares >= finalRepaidShares) "insufficient borrow"
  let currentCollateral <- Morpho.Contract.Morpho.structMember2 "positionSlot" id borrower "collateral"
  require (currentCollateral >= finalSeizedAssets) "insufficient collateral"
  let newBorrowShares ← Stdlib.Math.subPanic currentBorrowShares finalRepaidShares
  let newCollateral ← Stdlib.Math.subPanic currentCollateral finalSeizedAssets
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id borrower "borrowShares" newBorrowShares
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id borrower "collateral" newCollateral
  let newTotalBorrowShares ← Stdlib.Math.subPanic totalBorrowShares_ finalRepaidShares
  let mut newTotalBorrowAssets := ZERO
  if totalBorrowAssets_ >= repaidAssets then
    let reducedTotalBorrowAssets ← Stdlib.Math.subPanic totalBorrowAssets_ repaidAssets
    newTotalBorrowAssets := reducedTotalBorrowAssets
  else
    newTotalBorrowAssets := ZERO
  let mut badDebtShares := ZERO
  let mut badDebtAssets := ZERO
  if newCollateral == 0 then
    badDebtShares := newBorrowShares
    let newTotalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic newTotalBorrowAssets 1
    let newTotalBorrowSharesWithVirtual ← Stdlib.Math.addPanic newTotalBorrowShares 1000000
    badDebtAssets :=
      min newTotalBorrowAssets
        (Morpho.Contract.mulDivUp badDebtShares newTotalBorrowAssetsWithVirtual
          newTotalBorrowSharesWithVirtual)
    Morpho.Contract.Morpho.setStructMember2 "positionSlot" id borrower "borrowShares" ZERO
    let remainingBorrowShares ← Stdlib.Math.subPanic newTotalBorrowShares badDebtShares
    let remainingBorrowAssets ← Stdlib.Math.subPanic newTotalBorrowAssets badDebtAssets
    let remainingSupplyAssets ← Stdlib.Math.subPanic totalSupplyAssets_ badDebtAssets
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares" remainingBorrowShares
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets" remainingBorrowAssets
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalSupplyAssets" remainingSupplyAssets
  else
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares" newTotalBorrowShares
    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets" newTotalBorrowAssets
  let sender <- msgSender
  emit "Liquidate"
    [id, sender, borrower, repaidAssets, finalRepaidShares, finalSeizedAssets,
      badDebtAssets, badDebtShares]
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferModule
    [addressToWord mp.collateralToken, addressToWord sender, finalSeizedAssets]
  ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0xcf7ea196 1 "data")
    [addressToWord sender, repaidAssets]
  let thisAddress ← Morpho.Contract.contractAddress
  ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferFromModule
    [addressToWord mp.loanToken, addressToWord sender, addressToWord thisAddress, repaidAssets]
  return (finalSeizedAssets, repaidAssets)

def generatedLiquidateAfterAccrue (mp : MarketParams) (id : Bytes32) (borrower : Address)
    (seizedAssets repaidShares : Uint256) (data : Bytes) : Contract (Uint256 × Uint256) := do
  let collateralPrice ← ecmCall
    (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
    [addressToWord mp.oracle]
  let healthy ← _isHealthyWithPrice mp id borrower collateralPrice
  require (healthy == false) "position is healthy"
  liquidateAfterUnhealthyGuard mp id borrower seizedAssets repaidShares data collateralPrice

def generatedLiquidateAfterMarketId (mp : MarketParams)
    (id : Bytes32) (borrower : Address) (seizedAssets repaidShares : Uint256) (data : Bytes) :
    Contract (Uint256 × Uint256) := do
  let currentLastUpdate <- Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate"
  require (currentLastUpdate != ZERO) "market not created"
  require ((seizedAssets == 0) != (repaidShares == 0)) "inconsistent input"
  let _accrued ← _accrueInterest mp id
  generatedLiquidateAfterAccrue mp id borrower seizedAssets repaidShares data

def borrowAfterAllWrites (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newTotalBorrowAssets : Uint256) :
    Contract (Uint256 × Uint256) := do
  let healthy ← _isHealthy mp id onBehalf
  borrowAfterHealthTail mp id finalAssets finalShares onBehalf receiver sender
    newTotalBorrowAssets healthy

def borrowAfterFirstUintCheck (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newPosBorrowShares newTotalBorrowShares
      newTotalBorrowAssets : Uint256) :
    Contract (Uint256 × Uint256) := do
  require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
    "uint128 overflow"
  require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
    "uint128 overflow"
  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
    newPosBorrowShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
    newTotalBorrowShares
  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
    newTotalBorrowAssets
  borrowAfterAllWrites mp id finalAssets finalShares onBehalf receiver sender
    newTotalBorrowAssets

theorem storagePreserving_borrowAfterHealthTail
    (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newTotalBorrowAssets : Uint256)
    (healthy : Bool) :
    StoragePreserving
      (borrowAfterHealthTail mp id finalAssets finalShares onBehalf receiver sender
        newTotalBorrowAssets healthy) := by
  unfold borrowAfterHealthTail
  apply StoragePreserving.bind (StoragePreserving.require _ _)
  intro y
  apply StoragePreserving.bind (storagePreserving_marketStructMember_totalSupplyAssets id)
  intro totalSupplyAssets_
  apply StoragePreserving.bind (StoragePreserving.require _ _)
  intro y
  apply StoragePreserving.bind
    (storagePreserving_emitBorrow id (addressToWord sender) (addressToWord onBehalf)
      (addressToWord receiver) finalAssets finalShares)
  intro y
  apply StoragePreserving.bind (StoragePreserving.pure ())
  intro y
  exact StoragePreserving.pure _

theorem borrowAfterHealthTail_preserves_healthy
    (P : Position) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newTotalBorrowAssets : Uint256)
    (healthyFlag : Bool) (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hhealthy : healthy (project P.mp P.id P.account P.price cs))
    (hrun :
      (borrowAfterHealthTail P.mp id finalAssets finalShares onBehalf receiver sender
        newTotalBorrowAssets healthyFlag).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  have hstorage :=
    storagePreserving_borrowAfterHealthTail P.mp id finalAssets finalShares onBehalf
      receiver sender newTotalBorrowAssets healthyFlag cs out cs'
      (run_eq_of_success hrun)
  have hproj := project_eq_of_storage_eq P hstorage
  rw [hproj]
  exact hhealthy

theorem storagePreserving_withdrawCollateralAfterHealthTail
    (mp : MarketParams) (id assets : Uint256) (onBehalf receiver sender : Address)
    (healthyFlag : Bool) :
    StoragePreserving
      (withdrawCollateralAfterHealthTail mp id assets onBehalf receiver sender healthyFlag) := by
  unfold withdrawCollateralAfterHealthTail
  apply StoragePreserving.bind (StoragePreserving.require _ _)
  intro y
  apply StoragePreserving.bind
    (storagePreserving_emitWithdrawCollateral id (addressToWord sender)
      (addressToWord onBehalf) (addressToWord receiver) assets)
  intro y
  apply StoragePreserving.bind
    (StoragePreserving.ecmDo Morpho.Contract.MorphoSafeTransfer.safeTransferModule
      [addressToWord mp.collateralToken, addressToWord receiver, assets])
  intro y
  exact StoragePreserving.pure _

theorem withdrawCollateralAfterHealthTail_preserves_healthy
    (P : Position) (id assets : Uint256) (onBehalf receiver sender : Address)
    (healthyFlag : Bool) (out : Unit) (cs cs' : ContractState)
    (hhealthy : healthy (project P.mp P.id P.account P.price cs))
    (hrun :
      (withdrawCollateralAfterHealthTail P.mp id assets onBehalf receiver sender healthyFlag).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  have hstorage :=
    storagePreserving_withdrawCollateralAfterHealthTail P.mp id assets onBehalf receiver sender
      healthyFlag cs out cs' (run_eq_of_success hrun)
  have hproj := project_eq_of_storage_eq P hstorage
  rw [hproj]
  exact hhealthy

theorem withdrawCollateralAfterWrite_guarded
    (P : Position) (id assets : Uint256) (onBehalf receiver sender : Address)
    (out : Unit) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  subst id
  subst onBehalf
  unfold withdrawCollateralAfterWrite at hrun
  cases hhealth :
      ((_isHealthy P.mp P.id P.account) cs) with
  | success healthyFlag csHealth =>
      have hafterHealthRun :=
        bind_run_success
          (_isHealthy P.mp P.id P.account)
          (fun healthy =>
            withdrawCollateralAfterHealthTail P.mp P.id assets P.account receiver sender healthy)
          cs csHealth healthyFlag hhealth out cs' hrun
      cases healthyFlag
      · have hraw := run_eq_of_success hafterHealthRun
        unfold withdrawCollateralAfterHealthTail at hraw
        simp [Verity.require, instMonadContract, Verity.bind] at hraw
      · have hhealthy :
            healthy (project P.mp P.id P.account P.price cs) :=
          isHealthy_success_true_implies_healthy P cs csHealth hprice
            (noOverflow_of_localNoOverflow P cs (horacleFits cs)) (by
              simp [Contract.run, hhealth])
        have hhealthStorage :
            csHealth.storage = cs.storage :=
          storagePreserving_isHealthy P.mp P.id P.account cs true csHealth hhealth
        have hhealthyAtHealth :
            healthy (project P.mp P.id P.account P.price csHealth) := by
          rw [project_eq_of_storage_eq P hhealthStorage]
          exact hhealthy
        exact withdrawCollateralAfterHealthTail_preserves_healthy P P.id assets
          P.account receiver sender true out csHealth cs' hhealthyAtHealth hafterHealthRun
  | «revert» msg csHealth =>
      exact False.elim
        (bind_run_success_revert_absurd
          (_isHealthy P.mp P.id P.account)
          (fun healthy =>
            withdrawCollateralAfterHealthTail P.mp P.id assets P.account receiver sender healthy)
          cs csHealth msg hhealth out cs' hrun)

theorem generatedWithdrawCollateralAfterAccrue_guarded
    (P : Position) (id assets : Uint256) (onBehalf receiver sender : Address)
    (out : Unit) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold generatedWithdrawCollateralAfterAccrue at hrun
  cases hcoll :
      ((Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "collateral" :
        Contract Uint256) cs) with
  | success currentCollateral csColl =>
      have hcollOrig := hcoll
      unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hcoll
      simp at hcoll
      rcases hcoll with ⟨_, hcsColl⟩
      subst csColl
      have hafterCollRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "collateral" :
            Contract Uint256)
          (fun currentCollateral => do
            require (currentCollateral >= assets) "insufficient collateral"
            let newCollateral ← Stdlib.Math.subPanic currentCollateral assets
            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral"
              newCollateral
            withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
          cs currentCollateral hcollOrig out cs' hrun
      cases hreq :
          (require (currentCollateral >= assets) "insufficient collateral" cs) with
      | success y csReq =>
          cases y
          have hreqOrig := hreq
          unfold Verity.require at hreq
          split at hreq
          · injection hreq with _ hcsReq
            subst csReq
            have hafterReqRun :=
              bind_run_success_same_state
                (require (currentCollateral >= assets) "insufficient collateral")
                (fun _ => do
                  let newCollateral ← Stdlib.Math.subPanic currentCollateral assets
                  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral"
                    newCollateral
                  withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
                cs () hreqOrig out cs' hafterCollRun
            cases hsub : (Stdlib.Math.subPanic currentCollateral assets cs) with
            | success newCollateral csSub =>
                have hsubState :=
                  (subPanic_success_val_eq currentCollateral assets newCollateral cs csSub hsub).1
                subst csSub
                have hafterSubRun :=
                  bind_run_success_same_state
                    (Stdlib.Math.subPanic currentCollateral assets)
                    (fun newCollateral => do
                      Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                        "collateral" newCollateral
                      withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
                    cs newCollateral hsub out cs' hafterReqRun
                cases hset :
                    ((Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                      "collateral" newCollateral) cs) with
                | success y csSet =>
                    cases y
                    have hafterSetRun :=
                      bind_run_success
                        (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                          "collateral" newCollateral)
                        (fun _ => withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
                        cs csSet () hset out cs' hafterSubRun
                    exact withdrawCollateralAfterWrite_guarded P id assets onBehalf receiver sender
                      out csSet cs' hprice horacleFits hid honBehalf hafterSetRun
                | «revert» msg csSet =>
                    exact False.elim
                      (bind_run_success_revert_absurd
                        (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                          "collateral" newCollateral)
                        (fun _ => withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
                        cs csSet msg hset out cs' hafterSubRun)
            | «revert» msg csSub =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (Stdlib.Math.subPanic currentCollateral assets)
                    (fun newCollateral => do
                      Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                        "collateral" newCollateral
                      withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
                    cs csSub msg hsub out cs' hafterReqRun)
          · injection hreq
      | «revert» msg csReq =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentCollateral >= assets) "insufficient collateral")
              (fun _ => do
                let newCollateral ← Stdlib.Math.subPanic currentCollateral assets
                Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral"
                  newCollateral
                withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
              cs csReq msg hreq out cs' hafterCollRun)
  | «revert» msg csColl =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "collateral" :
            Contract Uint256)
          (fun currentCollateral => do
            require (currentCollateral >= assets) "insufficient collateral"
            let newCollateral ← Stdlib.Math.subPanic currentCollateral assets
            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "collateral"
              newCollateral
            withdrawCollateralAfterWrite P.mp id assets onBehalf receiver sender)
          cs csColl msg hcoll out cs' hrun)

theorem borrowHealthAndTail_guarded
    (P : Position) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (newTotalBorrowAssets : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
        newTotalBorrowAssets).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  subst id
  subst onBehalf
  unfold borrowAfterAllWrites at hrun
  cases hhealth :
      ((_isHealthy P.mp P.id P.account) cs) with
  | success healthyFlag csHealth =>
      have hafterHealthRun :=
        bind_run_success
          (_isHealthy P.mp P.id P.account)
          (fun healthy =>
            borrowAfterHealthTail P.mp P.id finalAssets finalShares P.account receiver sender
              newTotalBorrowAssets healthy)
          cs csHealth healthyFlag hhealth out cs' hrun
      cases healthyFlag
      · have hraw := run_eq_of_success hafterHealthRun
        unfold borrowAfterHealthTail at hraw
        simp [Verity.require, instMonadContract, Verity.bind] at hraw
      · have hhealthy :
            healthy (project P.mp P.id P.account P.price cs) :=
          isHealthy_success_true_implies_healthy P cs csHealth hprice
            (noOverflow_of_localNoOverflow P cs (horacleFits cs)) (by
              simp [Contract.run, hhealth])
        have hhealthStorage :
            csHealth.storage = cs.storage :=
          storagePreserving_isHealthy P.mp P.id P.account cs true csHealth hhealth
        have hhealthyAtHealth :
            healthy (project P.mp P.id P.account P.price csHealth) := by
          rw [project_eq_of_storage_eq P hhealthStorage]
          exact hhealthy
        exact borrowAfterHealthTail_preserves_healthy P P.id finalAssets finalShares
          P.account receiver sender newTotalBorrowAssets true out csHealth cs'
          hhealthyAtHealth hafterHealthRun
  | «revert» msg csHealth =>
      exact False.elim
        (bind_run_success_revert_absurd
          (_isHealthy P.mp P.id P.account)
          (fun healthy =>
            borrowAfterHealthTail P.mp P.id finalAssets finalShares P.account receiver sender
              newTotalBorrowAssets healthy)
          cs csHealth msg hhealth out cs' hrun)

theorem borrowAfterFirstUintCheck_guarded
    (P : Position) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address)
    (newPosBorrowShares newTotalBorrowShares newTotalBorrowAssets : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAfterFirstUintCheck P.mp id finalAssets finalShares onBehalf receiver sender
        newPosBorrowShares newTotalBorrowShares newTotalBorrowAssets).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowAfterFirstUintCheck at hrun
  cases hreqShares :
      (require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
        "uint128 overflow" cs) with
  | success y csReqShares =>
      cases y
      have hreqSharesOrig := hreqShares
      unfold Verity.require at hreqShares
      split at hreqShares
      · injection hreqShares with _ hcsReqShares
        subst csReqShares
        have hafterReqSharesRun :=
          bind_run_success_same_state
            (require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
              "uint128 overflow")
            (fun _ => do
              require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                "uint128 overflow"
              Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
                newPosBorrowShares
              Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                newTotalBorrowShares
              Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                newTotalBorrowAssets
              borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                newTotalBorrowAssets)
            cs () hreqSharesOrig out cs' hrun
        cases hreqAssets :
            (require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
              "uint128 overflow" cs) with
        | success y csReqAssets =>
            cases y
            have hreqAssetsOrig := hreqAssets
            unfold Verity.require at hreqAssets
            split at hreqAssets
            · injection hreqAssets with _ hcsReqAssets
              subst csReqAssets
              have hafterReqAssetsRun :=
                bind_run_success_same_state
                  (require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                    "uint128 overflow")
                  (fun _ => do
                    Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
                      newPosBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                      newTotalBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                      newTotalBorrowAssets
                    borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                      newTotalBorrowAssets)
                  cs () hreqAssetsOrig out cs' hafterReqSharesRun
              cases hsetPos :
                  (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                    "borrowShares" newPosBorrowShares) cs with
              | success y csSetPos =>
                  cases y
                  have hafterSetPosRun :=
                    bind_run_success
                      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                        "borrowShares" newPosBorrowShares)
                      (fun _ => do
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                          newTotalBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                          newTotalBorrowAssets
                        borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                          newTotalBorrowAssets)
                      cs csSetPos () hsetPos out cs' hafterReqAssetsRun
                  cases hsetShares :
                      (Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                        newTotalBorrowShares) csSetPos with
                  | success y csSetShares =>
                      cases y
                      have hafterSetSharesRun :=
                        bind_run_success
                          (Morpho.Contract.Morpho.setStructMember "marketSlot" id
                            "totalBorrowShares" newTotalBorrowShares)
                          (fun _ => do
                            Morpho.Contract.Morpho.setStructMember "marketSlot" id
                              "totalBorrowAssets" newTotalBorrowAssets
                            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                              newTotalBorrowAssets)
                          csSetPos csSetShares () hsetShares out cs' hafterSetPosRun
                      cases hsetAssets :
                          (Morpho.Contract.Morpho.setStructMember "marketSlot" id
                            "totalBorrowAssets" newTotalBorrowAssets) csSetShares with
                      | success y csSetAssets =>
                          cases y
                          have hafterSetAssetsRun :=
                            bind_run_success
                              (Morpho.Contract.Morpho.setStructMember "marketSlot" id
                                "totalBorrowAssets" newTotalBorrowAssets)
                              (fun _ =>
                                borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver
                                  sender newTotalBorrowAssets)
                              csSetShares csSetAssets () hsetAssets out cs' hafterSetSharesRun
                          exact borrowHealthAndTail_guarded P id finalAssets finalShares onBehalf
                            receiver sender newTotalBorrowAssets out csSetAssets cs' hprice
                            horacleFits hid honBehalf hafterSetAssetsRun
                      | «revert» msg csSetAssets =>
                          exact False.elim
                            (bind_run_success_revert_absurd
                              (Morpho.Contract.Morpho.setStructMember "marketSlot" id
                                "totalBorrowAssets" newTotalBorrowAssets)
                              (fun _ =>
                                borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver
                                  sender newTotalBorrowAssets)
                              csSetShares csSetAssets msg hsetAssets out cs' hafterSetSharesRun)
                  | «revert» msg csSetShares =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (Morpho.Contract.Morpho.setStructMember "marketSlot" id
                            "totalBorrowShares" newTotalBorrowShares)
                          (fun _ => do
                            Morpho.Contract.Morpho.setStructMember "marketSlot" id
                              "totalBorrowAssets" newTotalBorrowAssets
                            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                              newTotalBorrowAssets)
                          csSetPos csSetShares msg hsetShares out cs' hafterSetPosRun)
              | «revert» msg csSetPos =>
                  exact False.elim
                    (bind_run_success_revert_absurd
                      (Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                        "borrowShares" newPosBorrowShares)
                      (fun _ => do
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                          newTotalBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                          newTotalBorrowAssets
                        borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                          newTotalBorrowAssets)
                      cs csSetPos msg hsetPos out cs' hafterReqAssetsRun)
            · injection hreqAssets
        | «revert» msg csReqAssets =>
            exact False.elim
              (bind_run_success_revert_absurd
                (require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                  "uint128 overflow")
                (fun _ => do
                  Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
                    newPosBorrowShares
                  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                    newTotalBorrowShares
                  Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                    newTotalBorrowAssets
                  borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                    newTotalBorrowAssets)
                cs csReqAssets msg hreqAssets out cs' hafterReqSharesRun)
      · injection hreqShares
  | «revert» msg csReqShares =>
      exact False.elim
        (bind_run_success_revert_absurd
          (require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
            "uint128 overflow")
          (fun _ => do
            require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
              newPosBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
              newTotalBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
              newTotalBorrowAssets
            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
              newTotalBorrowAssets)
          cs csReqShares msg hreqShares out cs' hrun)

theorem borrowCommitAndCheck_guarded
    (P : Position) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowCommitAndCheck P.mp id finalAssets finalShares onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowCommitAndCheck at hrun
  cases hread :
      ((Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares" :
        Contract Uint256) cs) with
  | success currentBorrowShares csRead =>
      have hreadOrig := hread
      unfold Morpho.Contract.Morpho.structMember2 Contracts.structMember2At at hread
      simp at hread
      rcases hread with ⟨_, hcsRead⟩
      subst csRead
      have hafterReadRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares" :
            Contract Uint256)
          (fun currentBorrowShares => do
            let newPosBorrowShares ← Stdlib.Math.addPanic currentBorrowShares finalShares
            let newTotalBorrowShares ← Stdlib.Math.addPanic totalBorrowShares_ finalShares
            let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
            require (newPosBorrowShares <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
              newPosBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
              newTotalBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
              newTotalBorrowAssets
            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
              newTotalBorrowAssets)
          cs currentBorrowShares hreadOrig out cs' hrun
      cases haddPos : Stdlib.Math.addPanic currentBorrowShares finalShares cs with
      | success newPosBorrowShares csAddPos =>
          have hcsAddPos :=
            (addPanic_success_val_eq currentBorrowShares finalShares
              newPosBorrowShares cs csAddPos haddPos).1
          subst csAddPos
          have hafterAddPosRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic currentBorrowShares finalShares)
              (fun newPosBorrowShares => do
                let newTotalBorrowShares ← Stdlib.Math.addPanic totalBorrowShares_ finalShares
                let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
                require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                  "borrowShares" newPosBorrowShares
                Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                  newTotalBorrowShares
                Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                  newTotalBorrowAssets
                borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                  newTotalBorrowAssets)
              cs newPosBorrowShares haddPos out cs' hafterReadRun
          cases haddShares : Stdlib.Math.addPanic totalBorrowShares_ finalShares cs with
          | success newTotalBorrowShares csAddShares =>
              have hcsAddShares :=
                (addPanic_success_val_eq totalBorrowShares_ finalShares
                  newTotalBorrowShares cs csAddShares haddShares).1
              subst csAddShares
              have hafterAddSharesRun :=
                bind_run_success_same_state
                  (Stdlib.Math.addPanic totalBorrowShares_ finalShares)
                  (fun newTotalBorrowShares => do
                    let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
                    require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                      "borrowShares" newPosBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                      newTotalBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                      newTotalBorrowAssets
                    borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                      newTotalBorrowAssets)
                  cs newTotalBorrowShares haddShares out cs' hafterAddPosRun
              cases haddAssets : Stdlib.Math.addPanic totalBorrowAssets_ finalAssets cs with
              | success newTotalBorrowAssets csAddAssets =>
                  have hcsAddAssets :=
                    (addPanic_success_val_eq totalBorrowAssets_ finalAssets
                      newTotalBorrowAssets cs csAddAssets haddAssets).1
                  subst csAddAssets
                  have hafterAddAssetsRun :=
                    bind_run_success_same_state
                      (Stdlib.Math.addPanic totalBorrowAssets_ finalAssets)
                      (fun newTotalBorrowAssets => do
                        require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                          "borrowShares" newPosBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                          newTotalBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                          newTotalBorrowAssets
                        borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                          newTotalBorrowAssets)
                      cs newTotalBorrowAssets haddAssets out cs' hafterAddSharesRun
                  cases hreqPos :
                      (require
                        (newPosBorrowShares <= 340282366920938463463374607431768211455)
                        "uint128 overflow" cs) with
                  | success y csReqPos =>
                      cases y
                      have hreqPosOrig := hreqPos
                      unfold Verity.require at hreqPos
                      split at hreqPos
                      · injection hreqPos with _ hcsReqPos
                        subst csReqPos
                        have hafterReqPosRun :=
                          bind_run_success_same_state
                            (require
                              (newPosBorrowShares <= 340282366920938463463374607431768211455)
                              "uint128 overflow")
                            (fun _ => do
                              require
                                (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                                "uint128 overflow"
                              require
                                (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                                "uint128 overflow"
                              Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                                "borrowShares" newPosBorrowShares
                              Morpho.Contract.Morpho.setStructMember "marketSlot" id
                                "totalBorrowShares" newTotalBorrowShares
                              Morpho.Contract.Morpho.setStructMember "marketSlot" id
                                "totalBorrowAssets" newTotalBorrowAssets
                              borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver
                                sender newTotalBorrowAssets)
                            cs () hreqPosOrig out cs' hafterAddAssetsRun
                        change
                          (borrowAfterFirstUintCheck P.mp id finalAssets finalShares onBehalf
                            receiver sender newPosBorrowShares newTotalBorrowShares
                            newTotalBorrowAssets).run cs =
                            ContractResult.success out cs' at hafterReqPosRun
                        exact borrowAfterFirstUintCheck_guarded P id finalAssets finalShares
                          onBehalf receiver sender newPosBorrowShares newTotalBorrowShares
                          newTotalBorrowAssets out cs cs' hprice horacleFits hid honBehalf
                          hafterReqPosRun
                      · injection hreqPos
                  | «revert» msg csReqPos =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (require
                            (newPosBorrowShares <= 340282366920938463463374607431768211455)
                            "uint128 overflow")
                          (fun _ => do
                            require
                              (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                              "uint128 overflow"
                            require
                              (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                              "uint128 overflow"
                            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                              "borrowShares" newPosBorrowShares
                            Morpho.Contract.Morpho.setStructMember "marketSlot" id
                              "totalBorrowShares" newTotalBorrowShares
                            Morpho.Contract.Morpho.setStructMember "marketSlot" id
                              "totalBorrowAssets" newTotalBorrowAssets
                            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver
                              sender newTotalBorrowAssets)
                          cs csReqPos msg hreqPos out cs' hafterAddAssetsRun)
              | «revert» msg csAddAssets =>
                  exact False.elim
                    (bind_run_success_revert_absurd
                      (Stdlib.Math.addPanic totalBorrowAssets_ finalAssets)
                      (fun newTotalBorrowAssets => do
                        require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                          "uint128 overflow"
                        Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                          "borrowShares" newPosBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                          newTotalBorrowShares
                        Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                          newTotalBorrowAssets
                        borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                          newTotalBorrowAssets)
                      cs csAddAssets msg haddAssets out cs' hafterAddSharesRun)
          | «revert» msg csAddShares =>
              exact False.elim
                (bind_run_success_revert_absurd
                  (Stdlib.Math.addPanic totalBorrowShares_ finalShares)
                  (fun newTotalBorrowShares => do
                    let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
                    require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                      "uint128 overflow"
                    Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                      "borrowShares" newPosBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                      newTotalBorrowShares
                    Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                      newTotalBorrowAssets
                    borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                      newTotalBorrowAssets)
                  cs csAddShares msg haddShares out cs' hafterAddPosRun)
      | «revert» msg csAddPos =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic currentBorrowShares finalShares)
              (fun newPosBorrowShares => do
                let newTotalBorrowShares ← Stdlib.Math.addPanic totalBorrowShares_ finalShares
                let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
                require (newPosBorrowShares <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
                  "uint128 overflow"
                Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf
                  "borrowShares" newPosBorrowShares
                Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
                  newTotalBorrowShares
                Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
                  newTotalBorrowAssets
                borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
                  newTotalBorrowAssets)
              cs csAddPos msg haddPos out cs' hafterReadRun)
  | «revert» msg csRead =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember2 "positionSlot" id onBehalf "borrowShares" :
            Contract Uint256)
          (fun currentBorrowShares => do
            let newPosBorrowShares ← Stdlib.Math.addPanic currentBorrowShares finalShares
            let newTotalBorrowShares ← Stdlib.Math.addPanic totalBorrowShares_ finalShares
            let newTotalBorrowAssets ← Stdlib.Math.addPanic totalBorrowAssets_ finalAssets
            require (newPosBorrowShares <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            require (newTotalBorrowShares <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            require (newTotalBorrowAssets <= 340282366920938463463374607431768211455)
              "uint128 overflow"
            Morpho.Contract.Morpho.setStructMember2 "positionSlot" id onBehalf "borrowShares"
              newPosBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowShares"
              newTotalBorrowShares
            Morpho.Contract.Morpho.setStructMember "marketSlot" id "totalBorrowAssets"
              newTotalBorrowAssets
            borrowAfterAllWrites P.mp id finalAssets finalShares onBehalf receiver sender
              newTotalBorrowAssets)
          cs csRead msg hread out cs' hrun)

theorem generated_borrowCommitAndCheck_eq
    (mp : MarketParams) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (cs : ContractState) :
    (_borrowCommitAndCheck mp id finalAssets finalShares onBehalf receiver sender
      totalBorrowAssets_ totalBorrowShares_).run cs =
    (borrowCommitAndCheck mp id finalAssets finalShares onBehalf receiver sender
      totalBorrowAssets_ totalBorrowShares_).run cs := by
  rfl

theorem generated_borrowCommitAndCheck_guarded
    (P : Position) (id finalAssets finalShares : Uint256)
    (onBehalf receiver sender : Address) (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (_borrowCommitAndCheck P.mp id finalAssets finalShares onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  apply borrowCommitAndCheck_guarded P id finalAssets finalShares onBehalf receiver sender
    totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
  rw [← generated_borrowCommitAndCheck_eq P.mp id finalAssets finalShares onBehalf
    receiver sender totalBorrowAssets_ totalBorrowShares_ cs]
  exact hrun

theorem generated_borrowAssetsMode_guarded
    (P : Position) (id assets : Uint256) (onBehalf receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (_borrowAssetsMode P.mp id assets onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold _borrowAssetsMode at hrun
  cases haddShares : Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
  | success totalBorrowSharesWithVirtual csAddShares =>
      have hcsAddShares :=
        (addPanic_success_val_eq totalBorrowShares_ 1000000
          totalBorrowSharesWithVirtual cs csAddShares haddShares).1
      subst csAddShares
      have hafterSharesRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            let (finalAssets, checkedShares) ←
              _borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
                totalBorrowAssets_ totalBorrowShares_
            return (finalAssets, checkedShares))
          cs totalBorrowSharesWithVirtual haddShares out cs' hrun
      cases haddAssets : Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
      | success totalBorrowAssetsWithVirtual csAddAssets =>
          have hcsAddAssets :=
            (addPanic_success_val_eq totalBorrowAssets_ 1
              totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
          subst csAddAssets
          have hafterAssetsRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual => do
                let finalShares :=
                  Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual
                let (finalAssets, checkedShares) ←
                  _borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
                    totalBorrowAssets_ totalBorrowShares_
                return (finalAssets, checkedShares))
              cs totalBorrowAssetsWithVirtual haddAssets out cs' hafterSharesRun
          have hcommitRun :=
            bind_pure_run_success_source
              (_borrowCommitAndCheck P.mp id assets
                (Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                  totalBorrowAssetsWithVirtual)
                onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              out cs cs' hafterAssetsRun
          exact generated_borrowCommitAndCheck_guarded P id assets
            (Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
              totalBorrowAssetsWithVirtual)
            onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_ out cs cs'
            hprice horacleFits hid honBehalf hcommitRun
      | «revert» msg csAddAssets =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual => do
                let finalShares :=
                  Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual
                let (finalAssets, checkedShares) ←
                  _borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
                    totalBorrowAssets_ totalBorrowShares_
                return (finalAssets, checkedShares))
              cs csAddAssets msg haddAssets out cs' hafterSharesRun)
  | «revert» msg csAddShares =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            let (finalAssets, checkedShares) ←
              _borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
                totalBorrowAssets_ totalBorrowShares_
            return (finalAssets, checkedShares))
          cs csAddShares msg haddShares out cs' hrun)

theorem generated_borrowSharesMode_guarded
    (P : Position) (id shares : Uint256) (onBehalf receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (_borrowSharesMode P.mp id shares onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold _borrowSharesMode at hrun
  cases haddAssets : Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
  | success totalBorrowAssetsWithVirtual csAddAssets =>
      have hcsAddAssets :=
        (addPanic_success_val_eq totalBorrowAssets_ 1
          totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
      subst csAddAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            let (checkedAssets, finalShares) ←
              _borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
                totalBorrowAssets_ totalBorrowShares_
            return (checkedAssets, finalShares))
          cs totalBorrowAssetsWithVirtual haddAssets out cs' hrun
      cases haddShares : Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
      | success totalBorrowSharesWithVirtual csAddShares =>
          have hcsAddShares :=
            (addPanic_success_val_eq totalBorrowShares_ 1000000
              totalBorrowSharesWithVirtual cs csAddShares haddShares).1
          subst csAddShares
          have hafterSharesRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual => do
                let finalAssets :=
                  Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual
                let (checkedAssets, finalShares) ←
                  _borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
                    totalBorrowAssets_ totalBorrowShares_
                return (checkedAssets, finalShares))
              cs totalBorrowSharesWithVirtual haddShares out cs' hafterAssetsRun
          have hcommitRun :=
            bind_pure_run_success_source
              (_borrowCommitAndCheck P.mp id
                (Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                  totalBorrowSharesWithVirtual)
                shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              out cs cs' hafterSharesRun
          exact generated_borrowCommitAndCheck_guarded P id
            (Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
              totalBorrowSharesWithVirtual)
            shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_ out cs cs'
            hprice horacleFits hid honBehalf hcommitRun
      | «revert» msg csAddShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual => do
                let finalAssets :=
                  Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual
                let (checkedAssets, finalShares) ←
                  _borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
                    totalBorrowAssets_ totalBorrowShares_
                return (checkedAssets, finalShares))
              cs csAddShares msg haddShares out cs' hafterAssetsRun)
  | «revert» msg csAddAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            let (checkedAssets, finalShares) ←
              _borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
                totalBorrowAssets_ totalBorrowShares_
            return (checkedAssets, finalShares))
          cs csAddAssets msg haddAssets out cs' hrun)

theorem generated_borrowAfterAccrue_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver sender : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (_borrowAfterAccrue P.mp id assets shares onBehalf receiver sender).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold _borrowAfterAccrue at hrun
  cases hassetsRead :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
        Contract Uint256) cs) with
  | success totalBorrowAssets_ csAssets =>
      have hassetsReadOrig := hassetsRead
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hassetsRead
      simp at hassetsRead
      rcases hassetsRead with ⟨_, hcsAssets⟩
      subst csAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            if assets > 0 then
              do
                let (borrowedAssets, borrowedShares) ←
                  _borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                    totalBorrowShares_
                return (borrowedAssets, borrowedShares)
            else
              do
                let (borrowedAssets, borrowedShares) ←
                  _borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                    totalBorrowShares_
                return (borrowedAssets, borrowedShares))
          cs totalBorrowAssets_ hassetsReadOrig out cs' hrun
      cases hsharesRead :
          ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
            Contract Uint256) cs) with
      | success totalBorrowShares_ csShares =>
          have hsharesReadOrig := hsharesRead
          unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hsharesRead
          simp at hsharesRead
          rcases hsharesRead with ⟨_, hcsShares⟩
          subst csShares
          have hafterSharesReadRun :=
            bind_run_success_same_state
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                if assets > 0 then
                  do
                    let (borrowedAssets, borrowedShares) ←
                      _borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                        totalBorrowShares_
                    return (borrowedAssets, borrowedShares)
                else
                  do
                    let (borrowedAssets, borrowedShares) ←
                      _borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                        totalBorrowShares_
                    return (borrowedAssets, borrowedShares))
              cs totalBorrowShares_ hsharesReadOrig out cs' hafterAssetsRun
          by_cases hassets : 0 < assets.val
          · have hmodeRun :
                (do
                  let (borrowedAssets, borrowedShares) ←
                    _borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                      totalBorrowShares_
                  return (borrowedAssets, borrowedShares)).run cs =
                  ContractResult.success out cs' := by
                simpa [hassets] using hafterSharesReadRun
            have hsource :=
              bind_pure_run_success_source
                (_borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                  totalBorrowShares_)
                out cs cs' hmodeRun
            exact generated_borrowAssetsMode_guarded P id assets onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
              hsource
          · have hmodeRun :
                (do
                  let (borrowedAssets, borrowedShares) ←
                    _borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                      totalBorrowShares_
                  return (borrowedAssets, borrowedShares)).run cs =
                  ContractResult.success out cs' := by
                simpa [hassets] using hafterSharesReadRun
            have hsource :=
              bind_pure_run_success_source
                (_borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                  totalBorrowShares_)
                out cs cs' hmodeRun
            exact generated_borrowSharesMode_guarded P id shares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
              hsource
      | «revert» msg csShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                if assets > 0 then
                  do
                    let (borrowedAssets, borrowedShares) ←
                      _borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                        totalBorrowShares_
                    return (borrowedAssets, borrowedShares)
                else
                  do
                    let (borrowedAssets, borrowedShares) ←
                      _borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                        totalBorrowShares_
                    return (borrowedAssets, borrowedShares))
              cs csShares msg hsharesRead out cs' hafterAssetsRun)
  | «revert» msg csAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            if assets > 0 then
              do
                let (borrowedAssets, borrowedShares) ←
                  _borrowAssetsMode P.mp id assets onBehalf receiver sender totalBorrowAssets_
                    totalBorrowShares_
                return (borrowedAssets, borrowedShares)
            else
              do
                let (borrowedAssets, borrowedShares) ←
                  _borrowSharesMode P.mp id shares onBehalf receiver sender totalBorrowAssets_
                    totalBorrowShares_
                return (borrowedAssets, borrowedShares))
          cs csAssets msg hassetsRead out cs' hrun)

theorem generatedBorrowAfterAccrueReturn_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver sender : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf receiver sender).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold generatedBorrowAfterAccrueReturn at hrun
  have hsource :=
    bind_pure_run_success_source
      (_borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
      out cs cs' hrun
  exact generated_borrowAfterAccrue_guarded P id assets shares onBehalf receiver sender
    out cs cs' hprice horacleFits hid honBehalf hsource

theorem borrowAssetsMode_guarded
    (P : Position) (id assets : Uint256) (onBehalf receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAssetsMode P.mp id assets onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowAssetsMode at hrun
  cases haddShares : Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
  | success totalBorrowSharesWithVirtual csAddShares =>
      have hcsAddShares :=
        (addPanic_success_val_eq totalBorrowShares_ 1000000
          totalBorrowSharesWithVirtual cs csAddShares haddShares).1
      subst csAddShares
      have hafterSharesRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs totalBorrowSharesWithVirtual haddShares out cs' hrun
      cases haddAssets : Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
      | success totalBorrowAssetsWithVirtual csAddAssets =>
          have hcsAddAssets :=
            (addPanic_success_val_eq totalBorrowAssets_ 1
              totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
          subst csAddAssets
          have hcommitRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual =>
                borrowCommitAndCheck P.mp id assets
                  (Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual)
                  onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              cs totalBorrowAssetsWithVirtual haddAssets out cs' hafterSharesRun
          exact borrowCommitAndCheck_guarded P id assets
            (Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
              totalBorrowAssetsWithVirtual)
            onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_ out cs cs'
            hprice horacleFits hid honBehalf hcommitRun
      | «revert» msg csAddAssets =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowAssets_ 1)
              (fun totalBorrowAssetsWithVirtual =>
                borrowCommitAndCheck P.mp id assets
                  (Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                    totalBorrowAssetsWithVirtual)
                  onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              cs csAddAssets msg haddAssets out cs' hafterSharesRun)
  | «revert» msg csAddShares =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
          (fun totalBorrowSharesWithVirtual => do
            let totalBorrowAssetsWithVirtual ← Stdlib.Math.addPanic totalBorrowAssets_ 1
            let finalShares :=
              Morpho.Contract.mulDivUp assets totalBorrowSharesWithVirtual
                totalBorrowAssetsWithVirtual
            borrowCommitAndCheck P.mp id assets finalShares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs csAddShares msg haddShares out cs' hrun)

theorem borrowSharesMode_guarded
    (P : Position) (id shares : Uint256) (onBehalf receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowSharesMode P.mp id shares onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowSharesMode at hrun
  cases haddAssets : Stdlib.Math.addPanic totalBorrowAssets_ 1 cs with
  | success totalBorrowAssetsWithVirtual csAddAssets =>
      have hcsAddAssets :=
        (addPanic_success_val_eq totalBorrowAssets_ 1
          totalBorrowAssetsWithVirtual cs csAddAssets haddAssets).1
      subst csAddAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs totalBorrowAssetsWithVirtual haddAssets out cs' hrun
      cases haddShares : Stdlib.Math.addPanic totalBorrowShares_ 1000000 cs with
      | success totalBorrowSharesWithVirtual csAddShares =>
          have hcsAddShares :=
            (addPanic_success_val_eq totalBorrowShares_ 1000000
              totalBorrowSharesWithVirtual cs csAddShares haddShares).1
          subst csAddShares
          have hcommitRun :=
            bind_run_success_same_state
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual =>
                borrowCommitAndCheck P.mp id
                  (Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual)
                  shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              cs totalBorrowSharesWithVirtual haddShares out cs' hafterAssetsRun
          exact borrowCommitAndCheck_guarded P id
            (Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
              totalBorrowSharesWithVirtual)
            shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_ out cs cs'
            hprice horacleFits hid honBehalf hcommitRun
      | «revert» msg csAddShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Stdlib.Math.addPanic totalBorrowShares_ 1000000)
              (fun totalBorrowSharesWithVirtual =>
                borrowCommitAndCheck P.mp id
                  (Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                    totalBorrowSharesWithVirtual)
                  shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_)
              cs csAddShares msg haddShares out cs' hafterAssetsRun)
  | «revert» msg csAddAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Stdlib.Math.addPanic totalBorrowAssets_ 1)
          (fun totalBorrowAssetsWithVirtual => do
            let totalBorrowSharesWithVirtual ← Stdlib.Math.addPanic totalBorrowShares_ 1000000
            let finalAssets :=
              Morpho.Contract.mulDivDown shares totalBorrowAssetsWithVirtual
                totalBorrowSharesWithVirtual
            borrowCommitAndCheck P.mp id finalAssets shares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs csAddAssets msg haddAssets out cs' hrun)

theorem borrowAfterTotals_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver sender : Address)
    (totalBorrowAssets_ totalBorrowShares_ : Uint256)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAfterTotals P.mp id assets shares onBehalf receiver sender
        totalBorrowAssets_ totalBorrowShares_).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowAfterTotals at hrun
  by_cases hassets : 0 < assets.val
  · exact borrowAssetsMode_guarded P id assets onBehalf receiver sender
      totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
      (by simpa [hassets] using hrun)
  · exact borrowSharesMode_guarded P id shares onBehalf receiver sender
      totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
      (by simpa [hassets] using hrun)

theorem borrowAfterAccrue_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver sender : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAfterAccrue P.mp id assets shares onBehalf receiver sender).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowAfterAccrue at hrun
  cases hassetsRead :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
        Contract Uint256) cs) with
  | success totalBorrowAssets_ csAssets =>
      have hassetsReadOrig := hassetsRead
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hassetsRead
      simp at hassetsRead
      rcases hassetsRead with ⟨_, hcsAssets⟩
      subst csAssets
      have hafterAssetsRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            borrowAfterTotals P.mp id assets shares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs totalBorrowAssets_ hassetsReadOrig out cs' hrun
      cases hsharesRead :
          ((Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
            Contract Uint256) cs) with
      | success totalBorrowShares_ csShares =>
          have hsharesReadOrig := hsharesRead
          unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hsharesRead
          simp at hsharesRead
          rcases hsharesRead with ⟨_, hcsShares⟩
          subst csShares
          have htotalsRun :=
            bind_run_success_same_state
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                borrowAfterTotals P.mp id assets shares onBehalf receiver sender
                  totalBorrowAssets_ totalBorrowShares_)
              cs totalBorrowShares_ hsharesReadOrig out cs' hafterAssetsRun
          exact borrowAfterTotals_guarded P id assets shares onBehalf receiver sender
            totalBorrowAssets_ totalBorrowShares_ out cs cs' hprice horacleFits hid honBehalf
            htotalsRun
      | «revert» msg csShares =>
          exact False.elim
            (bind_run_success_revert_absurd
              (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares" :
                Contract Uint256)
              (fun totalBorrowShares_ =>
                borrowAfterTotals P.mp id assets shares onBehalf receiver sender
                  totalBorrowAssets_ totalBorrowShares_)
              cs csShares msg hsharesRead out cs' hafterAssetsRun)
  | «revert» msg csAssets =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowAssets" :
            Contract Uint256)
          (fun totalBorrowAssets_ => do
            let totalBorrowShares_ <-
              Morpho.Contract.Morpho.structMember "marketSlot" id "totalBorrowShares"
            borrowAfterTotals P.mp id assets shares onBehalf receiver sender
              totalBorrowAssets_ totalBorrowShares_)
          cs csAssets msg hassetsRead out cs' hrun)

theorem borrowAfterMarketId_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (borrowAfterMarketId P.mp id assets shares onBehalf receiver).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold borrowAfterMarketId at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      have hlastOrig := hlast
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      have hafterLastRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
          cs currentLastUpdate hlastOrig out cs' hrun
      cases hreqMarket :
          (require (currentLastUpdate != ZERO) "market not created" cs) with
      | success y csReqMarket =>
          cases y
          have hreqMarketOrig := hreqMarket
          unfold Verity.require at hreqMarket
          split at hreqMarket
          · injection hreqMarket with _ hcsReqMarket
            subst csReqMarket
            have hafterReqMarketRun :=
              bind_run_success_same_state
                (require (currentLastUpdate != ZERO) "market not created")
                (fun _ => do
                  require ((assets == 0) != (shares == 0)) "inconsistent input"
                  require (receiver != 0) "zero address"
                  let sender <- msgSender
                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                  require isAuthorizedSender "unauthorized"
                  let _accrued ← _accrueInterest P.mp id
                  borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                cs () hreqMarketOrig out cs' hafterLastRun
            cases hreqInput :
                (require ((assets == 0) != (shares == 0)) "inconsistent input" cs) with
            | success y csReqInput =>
                cases y
                have hreqInputOrig := hreqInput
                unfold Verity.require at hreqInput
                split at hreqInput
                · injection hreqInput with _ hcsReqInput
                  subst csReqInput
                  have hafterReqInputRun :=
                    bind_run_success_same_state
                      (require ((assets == 0) != (shares == 0)) "inconsistent input")
                      (fun _ => do
                        require (receiver != 0) "zero address"
                        let sender <- msgSender
                        let isAuthorizedSender ← _isSenderAuthorized onBehalf
                        require isAuthorizedSender "unauthorized"
                        let _accrued ← _accrueInterest P.mp id
                        borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                      cs () hreqInputOrig out cs' hafterReqMarketRun
                  cases hreqReceiver :
                      (require (receiver != 0) "zero address" cs) with
                  | success y csReqReceiver =>
                      cases y
                      have hreqReceiverOrig := hreqReceiver
                      unfold Verity.require at hreqReceiver
                      split at hreqReceiver
                      · injection hreqReceiver with _ hcsReqReceiver
                        subst csReqReceiver
                        have hafterReqReceiverRun :=
                          bind_run_success_same_state
                            (require (receiver != 0) "zero address")
                            (fun _ => do
                              let sender <- msgSender
                              let isAuthorizedSender ← _isSenderAuthorized onBehalf
                              require isAuthorizedSender "unauthorized"
                              let _accrued ← _accrueInterest P.mp id
                              borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                            cs () hreqReceiverOrig out cs' hafterReqInputRun
                        cases hsender : (msgSender cs) with
                        | success sender csSender =>
                            have hsenderOrig := hsender
                            unfold Verity.msgSender at hsender
                            injection hsender with _ hcsSender
                            subst csSender
                            have hafterSenderRun :=
                              bind_run_success_same_state
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                                cs sender hsenderOrig out cs' hafterReqReceiverRun
                            cases hauth : (_isSenderAuthorized onBehalf cs) with
                            | success isAuthorizedSender csAuth =>
                                have hafterAuthRun :=
                                  bind_run_success
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                                    cs csAuth isAuthorizedSender hauth out cs' hafterSenderRun
                                cases hreqAuth :
                                    (require isAuthorizedSender "unauthorized" csAuth) with
                                | success y csReqAuth =>
                                    cases y
                                    have hreqAuthOrig := hreqAuth
                                    unfold Verity.require at hreqAuth
                                    split at hreqAuth
                                    · injection hreqAuth with _ hcsReqAuth
                                      subst csReqAuth
                                      have hafterReqAuthRun :=
                                        bind_run_success_same_state
                                          (require isAuthorizedSender "unauthorized")
                                          (fun _ => do
                                            let _accrued ← _accrueInterest P.mp id
                                            borrowAfterAccrue P.mp id assets shares onBehalf receiver
                                              sender)
                                          csAuth () hreqAuthOrig out cs' hafterAuthRun
                                      cases haccrue : (_accrueInterest P.mp id csAuth) with
                                      | success accrued csAccrued =>
                                          have hafterAccrueRun :=
                                            bind_run_success
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                borrowAfterAccrue P.mp id assets shares onBehalf
                                                  receiver sender)
                                              csAuth csAccrued accrued haccrue out cs'
                                              hafterReqAuthRun
                                          exact borrowAfterAccrue_guarded P id assets shares onBehalf
                                            receiver sender out csAccrued cs' hprice horacleFits
                                            hid honBehalf hafterAccrueRun
                                      | «revert» msg csAccrued =>
                                          exact False.elim
                                            (bind_run_success_revert_absurd
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                borrowAfterAccrue P.mp id assets shares onBehalf
                                                  receiver sender)
                                              csAuth csAccrued msg haccrue out cs' hafterReqAuthRun)
                                    · injection hreqAuth
                                | «revert» msg csReqAuth =>
                                    exact False.elim
                                      (bind_run_success_revert_absurd
                                        (require isAuthorizedSender "unauthorized")
                                        (fun _ => do
                                          let _accrued ← _accrueInterest P.mp id
                                          borrowAfterAccrue P.mp id assets shares onBehalf receiver
                                            sender)
                                        csAuth csReqAuth msg hreqAuth out cs' hafterAuthRun)
                            | «revert» msg csAuth =>
                                exact False.elim
                                  (bind_run_success_revert_absurd
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      borrowAfterAccrue P.mp id assets shares onBehalf receiver
                                        sender)
                                    cs csAuth msg hauth out cs' hafterSenderRun)
                        | «revert» msg csSender =>
                            exact False.elim
                              (bind_run_success_revert_absurd
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                                cs csSender msg hsender out cs' hafterReqReceiverRun)
                      · injection hreqReceiver
                  | «revert» msg csReqReceiver =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (require (receiver != 0) "zero address")
                          (fun _ => do
                            let sender <- msgSender
                            let isAuthorizedSender ← _isSenderAuthorized onBehalf
                            require isAuthorizedSender "unauthorized"
                            let _accrued ← _accrueInterest P.mp id
                            borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                          cs csReqReceiver msg hreqReceiver out cs' hafterReqInputRun)
                · injection hreqInput
            | «revert» msg csReqInput =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (require ((assets == 0) != (shares == 0)) "inconsistent input")
                    (fun _ => do
                      require (receiver != 0) "zero address"
                      let sender <- msgSender
                      let isAuthorizedSender ← _isSenderAuthorized onBehalf
                      require isAuthorizedSender "unauthorized"
                      let _accrued ← _accrueInterest P.mp id
                      borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
                    cs csReqInput msg hreqInput out cs' hafterReqMarketRun)
          · injection hreqMarket
      | «revert» msg csReqMarket =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentLastUpdate != ZERO) "market not created")
              (fun _ => do
                require ((assets == 0) != (shares == 0)) "inconsistent input"
                require (receiver != 0) "zero address"
                let sender <- msgSender
                let isAuthorizedSender ← _isSenderAuthorized onBehalf
                require isAuthorizedSender "unauthorized"
                let _accrued ← _accrueInterest P.mp id
                borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
              cs csReqMarket msg hreqMarket out cs' hafterLastRun)
  | «revert» msg csLast =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            borrowAfterAccrue P.mp id assets shares onBehalf receiver sender)
          cs csLast msg hlast out cs' hrun)

theorem generatedBorrowAfterMarketId_guarded
    (P : Position) (id assets shares : Uint256) (onBehalf receiver : Address)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (generatedBorrowAfterMarketId P.mp id assets shares onBehalf receiver).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold generatedBorrowAfterMarketId at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      have hlastOrig := hlast
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      have hafterLastRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
              receiver sender)
          cs currentLastUpdate hlastOrig out cs' hrun
      cases hreqMarket :
          (require (currentLastUpdate != ZERO) "market not created" cs) with
      | success y csReqMarket =>
          cases y
          have hreqMarketOrig := hreqMarket
          unfold Verity.require at hreqMarket
          split at hreqMarket
          · injection hreqMarket with _ hcsReqMarket
            subst csReqMarket
            have hafterReqMarketRun :=
              bind_run_success_same_state
                (require (currentLastUpdate != ZERO) "market not created")
                (fun _ => do
                  require ((assets == 0) != (shares == 0)) "inconsistent input"
                  require (receiver != 0) "zero address"
                  let sender <- msgSender
                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                  require isAuthorizedSender "unauthorized"
                  let _accrued ← _accrueInterest P.mp id
                  generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                    receiver sender)
                cs () hreqMarketOrig out cs' hafterLastRun
            cases hreqInput :
                (require ((assets == 0) != (shares == 0)) "inconsistent input" cs) with
            | success y csReqInput =>
                cases y
                have hreqInputOrig := hreqInput
                unfold Verity.require at hreqInput
                split at hreqInput
                · injection hreqInput with _ hcsReqInput
                  subst csReqInput
                  have hafterReqInputRun :=
                    bind_run_success_same_state
                      (require ((assets == 0) != (shares == 0)) "inconsistent input")
                      (fun _ => do
                        require (receiver != 0) "zero address"
                        let sender <- msgSender
                        let isAuthorizedSender ← _isSenderAuthorized onBehalf
                        require isAuthorizedSender "unauthorized"
                        let _accrued ← _accrueInterest P.mp id
                        generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                          receiver sender)
                      cs () hreqInputOrig out cs' hafterReqMarketRun
                  cases hreqReceiver :
                      (require (receiver != 0) "zero address" cs) with
                  | success y csReqReceiver =>
                      cases y
                      have hreqReceiverOrig := hreqReceiver
                      unfold Verity.require at hreqReceiver
                      split at hreqReceiver
                      · injection hreqReceiver with _ hcsReqReceiver
                        subst csReqReceiver
                        have hafterReqReceiverRun :=
                          bind_run_success_same_state
                            (require (receiver != 0) "zero address")
                            (fun _ => do
                              let sender <- msgSender
                              let isAuthorizedSender ← _isSenderAuthorized onBehalf
                              require isAuthorizedSender "unauthorized"
                              let _accrued ← _accrueInterest P.mp id
                              generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                                receiver sender)
                            cs () hreqReceiverOrig out cs' hafterReqInputRun
                        cases hsender : (msgSender cs) with
                        | success sender csSender =>
                            have hsenderOrig := hsender
                            unfold Verity.msgSender at hsender
                            injection hsender with _ hcsSender
                            subst csSender
                            have hafterSenderRun :=
                              bind_run_success_same_state
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                                    receiver sender)
                                cs sender hsenderOrig out cs' hafterReqReceiverRun
                            cases hauth : (_isSenderAuthorized onBehalf cs) with
                            | success isAuthorizedSender csAuth =>
                                have hafterAuthRun :=
                                  bind_run_success
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                                        receiver sender)
                                    cs csAuth isAuthorizedSender hauth out cs' hafterSenderRun
                                cases hreqAuth :
                                    (require isAuthorizedSender "unauthorized" csAuth) with
                                | success y csReqAuth =>
                                    cases y
                                    have hreqAuthOrig := hreqAuth
                                    unfold Verity.require at hreqAuth
                                    split at hreqAuth
                                    · injection hreqAuth with _ hcsReqAuth
                                      subst csReqAuth
                                      have hafterReqAuthRun :=
                                        bind_run_success_same_state
                                          (require isAuthorizedSender "unauthorized")
                                          (fun _ => do
                                            let _accrued ← _accrueInterest P.mp id
                                            generatedBorrowAfterAccrueReturn P.mp id assets shares
                                              onBehalf receiver sender)
                                          csAuth () hreqAuthOrig out cs' hafterAuthRun
                                      cases haccrue : (_accrueInterest P.mp id csAuth) with
                                      | success accrued csAccrued =>
                                          have hafterAccrueRun :=
                                            bind_run_success
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                generatedBorrowAfterAccrueReturn P.mp id assets shares
                                                  onBehalf receiver sender)
                                              csAuth csAccrued accrued haccrue out cs'
                                              hafterReqAuthRun
                                          exact generatedBorrowAfterAccrueReturn_guarded P id assets shares
                                            onBehalf receiver sender out csAccrued cs' hprice horacleFits
                                            hid honBehalf hafterAccrueRun
                                      | «revert» msg csAccrued =>
                                          exact False.elim
                                            (bind_run_success_revert_absurd
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                generatedBorrowAfterAccrueReturn P.mp id assets shares
                                                  onBehalf receiver sender)
                                              csAuth csAccrued msg haccrue out cs' hafterReqAuthRun)
                                    · injection hreqAuth
                                | «revert» msg csReqAuth =>
                                    exact False.elim
                                      (bind_run_success_revert_absurd
                                        (require isAuthorizedSender "unauthorized")
                                        (fun _ => do
                                          let _accrued ← _accrueInterest P.mp id
                                          generatedBorrowAfterAccrueReturn P.mp id assets shares
                                            onBehalf receiver sender)
                                        csAuth csReqAuth msg hreqAuth out cs' hafterAuthRun)
                            | «revert» msg csAuth =>
                                exact False.elim
                                  (bind_run_success_revert_absurd
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      generatedBorrowAfterAccrueReturn P.mp id assets shares
                                        onBehalf receiver sender)
                                    cs csAuth msg hauth out cs' hafterSenderRun)
                        | «revert» msg csSender =>
                            exact False.elim
                              (bind_run_success_revert_absurd
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                                    receiver sender)
                                cs csSender msg hsender out cs' hafterReqReceiverRun)
                      · injection hreqReceiver
                  | «revert» msg csReqReceiver =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (require (receiver != 0) "zero address")
                          (fun _ => do
                            let sender <- msgSender
                            let isAuthorizedSender ← _isSenderAuthorized onBehalf
                            require isAuthorizedSender "unauthorized"
                            let _accrued ← _accrueInterest P.mp id
                            generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                              receiver sender)
                          cs csReqReceiver msg hreqReceiver out cs' hafterReqInputRun)
                · injection hreqInput
            | «revert» msg csReqInput =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (require ((assets == 0) != (shares == 0)) "inconsistent input")
                    (fun _ => do
                      require (receiver != 0) "zero address"
                      let sender <- msgSender
                      let isAuthorizedSender ← _isSenderAuthorized onBehalf
                      require isAuthorizedSender "unauthorized"
                      let _accrued ← _accrueInterest P.mp id
                      generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                        receiver sender)
                    cs csReqInput msg hreqInput out cs' hafterReqMarketRun)
          · injection hreqMarket
      | «revert» msg csReqMarket =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentLastUpdate != ZERO) "market not created")
              (fun _ => do
                require ((assets == 0) != (shares == 0)) "inconsistent input"
                require (receiver != 0) "zero address"
                let sender <- msgSender
                let isAuthorizedSender ← _isSenderAuthorized onBehalf
                require isAuthorizedSender "unauthorized"
                let _accrued ← _accrueInterest P.mp id
                generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
                  receiver sender)
              cs csReqMarket msg hreqMarket out cs' hafterLastRun)
  | «revert» msg csLast =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((assets == 0) != (shares == 0)) "inconsistent input"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            generatedBorrowAfterAccrueReturn P.mp id assets shares onBehalf
              receiver sender)
          cs csLast msg hlast out cs' hrun)

theorem generatedWithdrawCollateralAfterMarketId_guarded
    (P : Position) (id assets : Uint256) (onBehalf receiver : Address)
    (out : Unit) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P) (horacleFits : LocalNoOverflowFor P)
    (hid : id = P.id) (honBehalf : onBehalf = P.account)
    (hrun :
      (generatedWithdrawCollateralAfterMarketId P.mp id assets onBehalf receiver).run cs =
        ContractResult.success out cs') :
    healthy (project P.mp P.id P.account P.price cs') := by
  unfold generatedWithdrawCollateralAfterMarketId at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      have hlastOrig := hlast
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      have hafterLastRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require (assets > 0) "zero assets"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
          cs currentLastUpdate hlastOrig out cs' hrun
      cases hreqMarket :
          (require (currentLastUpdate != ZERO) "market not created" cs) with
      | success y csReqMarket =>
          cases y
          have hreqMarketOrig := hreqMarket
          unfold Verity.require at hreqMarket
          split at hreqMarket
          · injection hreqMarket with _ hcsReqMarket
            subst csReqMarket
            have hafterReqMarketRun :=
              bind_run_success_same_state
                (require (currentLastUpdate != ZERO) "market not created")
                (fun _ => do
                  require (assets > 0) "zero assets"
                  require (receiver != 0) "zero address"
                  let sender <- msgSender
                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                  require isAuthorizedSender "unauthorized"
                  let _accrued ← _accrueInterest P.mp id
                  generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                cs () hreqMarketOrig out cs' hafterLastRun
            cases hreqInput :
                (require (assets > 0) "zero assets" cs) with
            | success y csReqInput =>
                cases y
                have hreqInputOrig := hreqInput
                unfold Verity.require at hreqInput
                split at hreqInput
                · injection hreqInput with _ hcsReqInput
                  subst csReqInput
                  have hafterReqInputRun :=
                    bind_run_success_same_state
                      (require (assets > 0) "zero assets")
                      (fun _ => do
                        require (receiver != 0) "zero address"
                        let sender <- msgSender
                        let isAuthorizedSender ← _isSenderAuthorized onBehalf
                        require isAuthorizedSender "unauthorized"
                        let _accrued ← _accrueInterest P.mp id
                        generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                      cs () hreqInputOrig out cs' hafterReqMarketRun
                  cases hreqReceiver :
                      (require (receiver != 0) "zero address" cs) with
                  | success y csReqReceiver =>
                      cases y
                      have hreqReceiverOrig := hreqReceiver
                      unfold Verity.require at hreqReceiver
                      split at hreqReceiver
                      · injection hreqReceiver with _ hcsReqReceiver
                        subst csReqReceiver
                        have hafterReqReceiverRun :=
                          bind_run_success_same_state
                            (require (receiver != 0) "zero address")
                            (fun _ => do
                              let sender <- msgSender
                              let isAuthorizedSender ← _isSenderAuthorized onBehalf
                              require isAuthorizedSender "unauthorized"
                              let _accrued ← _accrueInterest P.mp id
                              generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                            cs () hreqReceiverOrig out cs' hafterReqInputRun
                        cases hsender : (msgSender cs) with
                        | success sender csSender =>
                            have hsenderOrig := hsender
                            unfold Verity.msgSender at hsender
                            injection hsender with _ hcsSender
                            subst csSender
                            have hafterSenderRun :=
                              bind_run_success_same_state
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                                cs sender hsenderOrig out cs' hafterReqReceiverRun
                            cases hauth : (_isSenderAuthorized onBehalf cs) with
                            | success isAuthorizedSender csAuth =>
                                have hafterAuthRun :=
                                  bind_run_success
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                                    cs csAuth isAuthorizedSender hauth out cs' hafterSenderRun
                                cases hreqAuth :
                                    (require isAuthorizedSender "unauthorized" csAuth) with
                                | success y csReqAuth =>
                                    cases y
                                    have hreqAuthOrig := hreqAuth
                                    unfold Verity.require at hreqAuth
                                    split at hreqAuth
                                    · injection hreqAuth with _ hcsReqAuth
                                      subst csReqAuth
                                      have hafterReqAuthRun :=
                                        bind_run_success_same_state
                                          (require isAuthorizedSender "unauthorized")
                                          (fun _ => do
                                            let _accrued ← _accrueInterest P.mp id
                                            generatedWithdrawCollateralAfterAccrue P.mp id assets
                                              onBehalf receiver sender)
                                          csAuth () hreqAuthOrig out cs' hafterAuthRun
                                      cases haccrue : (_accrueInterest P.mp id csAuth) with
                                      | success accrued csAccrued =>
                                          have hafterAccrueRun :=
                                            bind_run_success
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                generatedWithdrawCollateralAfterAccrue P.mp id assets
                                                  onBehalf receiver sender)
                                              csAuth csAccrued accrued haccrue out cs'
                                              hafterReqAuthRun
                                          exact generatedWithdrawCollateralAfterAccrue_guarded P id assets
                                            onBehalf receiver sender out csAccrued cs' hprice horacleFits
                                            hid honBehalf hafterAccrueRun
                                      | «revert» msg csAccrued =>
                                          exact False.elim
                                            (bind_run_success_revert_absurd
                                              (_accrueInterest P.mp id)
                                              (fun _ =>
                                                generatedWithdrawCollateralAfterAccrue P.mp id assets
                                                  onBehalf receiver sender)
                                              csAuth csAccrued msg haccrue out cs' hafterReqAuthRun)
                                    · injection hreqAuth
                                | «revert» msg csReqAuth =>
                                    exact False.elim
                                      (bind_run_success_revert_absurd
                                        (require isAuthorizedSender "unauthorized")
                                        (fun _ => do
                                          let _accrued ← _accrueInterest P.mp id
                                          generatedWithdrawCollateralAfterAccrue P.mp id assets
                                            onBehalf receiver sender)
                                        csAuth csReqAuth msg hreqAuth out cs' hafterAuthRun)
                            | «revert» msg csAuth =>
                                exact False.elim
                                  (bind_run_success_revert_absurd
                                    (_isSenderAuthorized onBehalf)
                                    (fun isAuthorizedSender => do
                                      require isAuthorizedSender "unauthorized"
                                      let _accrued ← _accrueInterest P.mp id
                                      generatedWithdrawCollateralAfterAccrue P.mp id assets
                                        onBehalf receiver sender)
                                    cs csAuth msg hauth out cs' hafterSenderRun)
                        | «revert» msg csSender =>
                            exact False.elim
                              (bind_run_success_revert_absurd
                                msgSender
                                (fun sender => do
                                  let isAuthorizedSender ← _isSenderAuthorized onBehalf
                                  require isAuthorizedSender "unauthorized"
                                  let _accrued ← _accrueInterest P.mp id
                                  generatedWithdrawCollateralAfterAccrue P.mp id assets
                                    onBehalf receiver sender)
                                cs csSender msg hsender out cs' hafterReqReceiverRun)
                      · injection hreqReceiver
                  | «revert» msg csReqReceiver =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (require (receiver != 0) "zero address")
                          (fun _ => do
                            let sender <- msgSender
                            let isAuthorizedSender ← _isSenderAuthorized onBehalf
                            require isAuthorizedSender "unauthorized"
                            let _accrued ← _accrueInterest P.mp id
                            generatedWithdrawCollateralAfterAccrue P.mp id assets
                              onBehalf receiver sender)
                          cs csReqReceiver msg hreqReceiver out cs' hafterReqInputRun)
                · injection hreqInput
            | «revert» msg csReqInput =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (require (assets > 0) "zero assets")
                    (fun _ => do
                      require (receiver != 0) "zero address"
                      let sender <- msgSender
                      let isAuthorizedSender ← _isSenderAuthorized onBehalf
                      require isAuthorizedSender "unauthorized"
                      let _accrued ← _accrueInterest P.mp id
                      generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
                    cs csReqInput msg hreqInput out cs' hafterReqMarketRun)
          · injection hreqMarket
      | «revert» msg csReqMarket =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentLastUpdate != ZERO) "market not created")
              (fun _ => do
                require (assets > 0) "zero assets"
                require (receiver != 0) "zero address"
                let sender <- msgSender
                let isAuthorizedSender ← _isSenderAuthorized onBehalf
                require isAuthorizedSender "unauthorized"
                let _accrued ← _accrueInterest P.mp id
                generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
              cs csReqMarket msg hreqMarket out cs' hafterLastRun)
  | «revert» msg csLast =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require (assets > 0) "zero assets"
            require (receiver != 0) "zero address"
            let sender <- msgSender
            let isAuthorizedSender ← _isSenderAuthorized onBehalf
            require isAuthorizedSender "unauthorized"
            let _accrued ← _accrueInterest P.mp id
            generatedWithdrawCollateralAfterAccrue P.mp id assets onBehalf receiver sender)
          cs csLast msg hlast out cs' hrun)

theorem generatedLiquidateAfterAccrue_guardUnhealthy
    (P : Position) (id : Bytes32) (borrower : Address)
    (seized repaid : Uint256) (data : Bytes)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (hid : id = P.id) (hborrower : borrower = P.account)
    (hrun :
      (generatedLiquidateAfterAccrue P.mp id borrower seized repaid data).run cs =
        ContractResult.success out cs') :
    ∃ stHealth,
      (_isHealthyWithPrice P.mp P.id P.account P.price).run cs =
        ContractResult.success false stHealth := by
  subst id
  subst borrower
  unfold generatedLiquidateAfterAccrue at hrun
  have horacleRun :
      (oraclePriceRead P.mp).run cs = ContractResult.success (0 : Uint256) cs := by
    unfold oraclePriceRead
    rfl
  have hp : (0 : Uint256) = P.price := hprice cs (0 : Uint256) cs horacleRun
  have hecm :
      (ecmCall
        (fun resultVar =>
          Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
        [addressToWord P.mp.oracle] : Contract Uint256) cs =
        ContractResult.success (0 : Uint256) cs := by
    rfl
  have hafterOracleRun :=
    bind_run_success_same_state
      (ecmCall
        (fun resultVar =>
          Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
        [addressToWord P.mp.oracle] : Contract Uint256)
      (fun collateralPrice => do
        let healthy ← _isHealthyWithPrice P.mp P.id P.account collateralPrice
        require (healthy == false) "position is healthy"
        liquidateAfterUnhealthyGuard P.mp P.id P.account seized repaid data collateralPrice)
      cs (0 : Uint256) hecm out cs' hrun
  cases hhealth :
      ((_isHealthyWithPrice P.mp P.id P.account (0 : Uint256)) cs) with
  | success healthyVal csHealth =>
      have hafterHealthRun :=
        bind_run_success
          (_isHealthyWithPrice P.mp P.id P.account (0 : Uint256))
          (fun healthy => do
            require (healthy == false) "position is healthy"
            liquidateAfterUnhealthyGuard P.mp P.id P.account seized repaid data (0 : Uint256))
          cs csHealth healthyVal hhealth out cs' hafterOracleRun
      cases healthyVal
      · have hhealthRun :
            (_isHealthyWithPrice P.mp P.id P.account P.price).run cs =
              ContractResult.success false csHealth := by
          rw [← hp]
          simp [Contract.run, hhealth]
        exact ⟨csHealth, hhealthRun⟩
      · have hraw := run_eq_of_success hafterHealthRun
        unfold Verity.require at hraw
        simp [instMonadContract, Verity.bind] at hraw
  | «revert» msg csHealth =>
      exact False.elim
        (bind_run_success_revert_absurd
          (_isHealthyWithPrice P.mp P.id P.account (0 : Uint256))
          (fun healthy => do
            require (healthy == false) "position is healthy"
            liquidateAfterUnhealthyGuard P.mp P.id P.account seized repaid data (0 : Uint256))
          cs csHealth msg hhealth out cs' hafterOracleRun)

theorem generatedLiquidateAfterMarketId_accrueGuardUnhealthy
    (P : Position) (id : Bytes32) (seized repaid : Uint256) (data : Bytes)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (hid : id = P.id)
    (hrun :
      (generatedLiquidateAfterMarketId P.mp id P.account seized repaid data).run cs =
        ContractResult.success out cs') :
    ∃ accrued stPostAccrue stHealth,
      (_accrueInterest P.mp id) cs = ContractResult.success accrued stPostAccrue ∧
      (_isHealthyWithPrice P.mp P.id P.account P.price).run stPostAccrue =
        ContractResult.success false stHealth := by
  unfold generatedLiquidateAfterMarketId at hrun
  cases hlast :
      ((Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
        Contract Uint256) cs) with
  | success currentLastUpdate csLast =>
      have hlastOrig := hlast
      unfold Morpho.Contract.Morpho.structMember Contracts.structMemberAt at hlast
      simp at hlast
      rcases hlast with ⟨_, hcsLast⟩
      subst csLast
      have hafterLastRun :=
        bind_run_success_same_state
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((seized == 0) != (repaid == 0)) "inconsistent input"
            let _accrued ← _accrueInterest P.mp id
            generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
          cs currentLastUpdate hlastOrig out cs' hrun
      cases hreqMarket :
          (require (currentLastUpdate != ZERO) "market not created" cs) with
      | success y csReqMarket =>
          cases y
          have hreqMarketOrig := hreqMarket
          unfold Verity.require at hreqMarket
          split at hreqMarket
          · injection hreqMarket with _ hcsReqMarket
            subst csReqMarket
            have hafterReqMarketRun :=
              bind_run_success_same_state
                (require (currentLastUpdate != ZERO) "market not created")
                (fun _ => do
                  require ((seized == 0) != (repaid == 0)) "inconsistent input"
                  let _accrued ← _accrueInterest P.mp id
                  generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
                cs () hreqMarketOrig out cs' hafterLastRun
            cases hreqInput :
                (require ((seized == 0) != (repaid == 0)) "inconsistent input" cs) with
            | success y csReqInput =>
                cases y
                have hreqInputOrig := hreqInput
                unfold Verity.require at hreqInput
                split at hreqInput
                · injection hreqInput with _ hcsReqInput
                  subst csReqInput
                  have hafterReqInputRun :=
                    bind_run_success_same_state
                      (require ((seized == 0) != (repaid == 0)) "inconsistent input")
                      (fun _ => do
                        let _accrued ← _accrueInterest P.mp id
                        generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
                      cs () hreqInputOrig out cs' hafterReqMarketRun
                  cases haccrue : (_accrueInterest P.mp id cs) with
                  | success accrued csAccrued =>
                      have hafterAccrueRun :=
                        bind_run_success
                          (_accrueInterest P.mp id)
                          (fun _ => generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
                          cs csAccrued accrued haccrue out cs' hafterReqInputRun
                      rcases generatedLiquidateAfterAccrue_guardUnhealthy P id P.account
                          seized repaid data out csAccrued cs' hprice hid rfl hafterAccrueRun with
                        ⟨stHealth, hhealth⟩
                      exact ⟨accrued, csAccrued, stHealth, rfl, hhealth⟩
                  | «revert» msg csAccrued =>
                      exact False.elim
                        (bind_run_success_revert_absurd
                          (_accrueInterest P.mp id)
                          (fun _ => generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
                          cs csAccrued msg haccrue out cs' hafterReqInputRun)
                · injection hreqInput
            | «revert» msg csReqInput =>
                exact False.elim
                  (bind_run_success_revert_absurd
                    (require ((seized == 0) != (repaid == 0)) "inconsistent input")
                    (fun _ => do
                      let _accrued ← _accrueInterest P.mp id
                      generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
                    cs csReqInput msg hreqInput out cs' hafterReqMarketRun)
          · injection hreqMarket
      | «revert» msg csReqMarket =>
          exact False.elim
            (bind_run_success_revert_absurd
              (require (currentLastUpdate != ZERO) "market not created")
              (fun _ => do
                require ((seized == 0) != (repaid == 0)) "inconsistent input"
                let _accrued ← _accrueInterest P.mp id
                generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
              cs csReqMarket msg hreqMarket out cs' hafterLastRun)
  | «revert» msg csLast =>
      exact False.elim
        (bind_run_success_revert_absurd
          (Morpho.Contract.Morpho.structMember "marketSlot" id "lastUpdate" :
            Contract Uint256)
          (fun currentLastUpdate => do
            require (currentLastUpdate != ZERO) "market not created"
            require ((seized == 0) != (repaid == 0)) "inconsistent input"
            let _accrued ← _accrueInterest P.mp id
            generatedLiquidateAfterAccrue P.mp id P.account seized repaid data)
          cs csLast msg hlast out cs' hrun)

theorem generatedLiquidateAfterMarketId_guardUnhealthy
    (P : Position) (id : Bytes32) (seized repaid : Uint256) (data : Bytes)
    (out : Uint256 × Uint256) (cs cs' : ContractState)
    (hprice : ExecutableOraclePriceReadAligned P)
    (hid : id = P.id)
    (hrun :
      (generatedLiquidateAfterMarketId P.mp id P.account seized repaid data).run cs =
        ContractResult.success out cs') :
    ∃ stPostAccrue stHealth,
      (_isHealthyWithPrice P.mp P.id P.account P.price).run stPostAccrue =
        ContractResult.success false stHealth := by
  rcases generatedLiquidateAfterMarketId_accrueGuardUnhealthy P id seized repaid data
      out cs cs' hprice hid hrun with
    ⟨_, stPostAccrue, stHealth, _, hhealth⟩
  exact ⟨stPostAccrue, stHealth, hhealth⟩

theorem guardedDiscipline_borrow (P : Position)
    (hid : ExecutableMarketIdReadAligned P) (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P) :
    GuardedDiscipline (borrowStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, shares, receiver, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  unfold borrow at hrun
  change
    (do
      let _ignoredMarketParams := P.mp
      let id ← ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv]
      generatedBorrowAfterMarketId P.mp id assets shares P.account receiver).run cs =
        ContractResult.success out cs' at hrun
  have hecm :
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256) cs =
        ContractResult.success (0 : Uint256) cs := by
    rfl
  have hmarketRun : (marketIdRead P.mp).run cs =
      ContractResult.success (0 : Uint256) cs := by
    unfold marketIdRead
    rfl
  have hid0 : (0 : Uint256) = P.id := hid cs (0 : Uint256) cs hmarketRun
  have hafterIdRun :=
    bind_run_success_same_state
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256)
      (fun id => generatedBorrowAfterMarketId P.mp id assets shares P.account receiver)
      cs (0 : Uint256) hecm out cs' hrun
  exact generatedBorrowAfterMarketId_guarded P (0 : Uint256) assets shares P.account receiver
    out cs cs' hprice horacleFits hid0 rfl hafterIdRun

theorem guardedDiscipline_withdrawCollateral (P : Position)
    (hid : ExecutableMarketIdReadAligned P) (hprice : ExecutableOraclePriceReadAligned P)
    (horacleFits : LocalNoOverflowFor P) :
    GuardedDiscipline (withdrawCollateralStep P) := by
  intro s s' hstep
  rcases hstep with ⟨assets, receiver, out, cs, cs', hs, hs', hrun⟩
  subst s
  subst s'
  unfold withdrawCollateral at hrun
  change
    (do
      let _ignoredMarketParams := P.mp
      let id ← ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv]
      generatedWithdrawCollateralAfterMarketId P.mp id assets P.account receiver).run cs =
        ContractResult.success out cs' at hrun
  have hecm :
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256) cs =
        ContractResult.success (0 : Uint256) cs := by
    rfl
  have hmarketRun : (marketIdRead P.mp).run cs =
      ContractResult.success (0 : Uint256) cs := by
    unfold marketIdRead
    rfl
  have hid0 : (0 : Uint256) = P.id := hid cs (0 : Uint256) cs hmarketRun
  have hafterIdRun :=
    bind_run_success_same_state
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256)
      (fun id => generatedWithdrawCollateralAfterMarketId P.mp id assets P.account receiver)
      cs (0 : Uint256) hecm out cs' hrun
  exact generatedWithdrawCollateralAfterMarketId_guarded P (0 : Uint256) assets P.account receiver
    out cs cs' hprice horacleFits hid0 rfl hafterIdRun

theorem liquidate_guardUnhealthy_afterAccrue_price (P : Position)
    (hid : ExecutableMarketIdReadAligned P) (hprice : ExecutableOraclePriceReadAligned P) :
    ∀ seized repaid data out cs cs',
      (liquidate P.mp P.account seized repaid data).run cs
          = Verity.ContractResult.success out cs' →
        ∃ stPostAccrue stHealth,
          (_isHealthyWithPrice P.mp P.id P.account P.price).run stPostAccrue
            = Verity.ContractResult.success false stHealth := by
  intro seized repaid data out cs cs' hrun
  unfold liquidate at hrun
  change
    (do
      let _ignoredMarketParams := P.mp
      let id ← ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv]
      generatedLiquidateAfterMarketId P.mp id P.account seized repaid data).run cs =
        ContractResult.success out cs' at hrun
  have hecm :
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256) cs =
        ContractResult.success (0 : Uint256) cs := by
    rfl
  have hmarketRun : (marketIdRead P.mp).run cs =
      ContractResult.success (0 : Uint256) cs := by
    unfold marketIdRead
    rfl
  have hid0 : (0 : Uint256) = P.id := hid cs (0 : Uint256) cs hmarketRun
  have hafterIdRun :=
    bind_run_success_same_state
      (ecmCall
        (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
        [addressToWord P.mp.loanToken, addressToWord P.mp.collateralToken,
          addressToWord P.mp.oracle, addressToWord P.mp.irm, P.mp.lltv] :
          Contract Uint256)
      (fun id => generatedLiquidateAfterMarketId P.mp id P.account seized repaid data)
      cs (0 : Uint256) hecm out cs' hrun
  exact generatedLiquidateAfterMarketId_guardUnhealthy P (0 : Uint256) seized repaid data
    out cs cs' hprice hid0 hafterIdRun

end Morpho.Proofs.Disciplines
