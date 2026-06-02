/-
  FramePreserve - compositional storage-slot framing for Morpho entrypoints.

  A successful entrypoint run is a tree of `bind`/`pure`/`if` over primitives:
  storage reads (`structMemberAt`/`structMember2At`), context reads, guards
  (`require`), arithmetic (`addPanic`/`subPanic`/`mulPanic`), events, external
  calls (`ecmEnvRead`/`ecmDo`), and storage writes (`setStructMemberAt`/
  `setStructMember2At`). All but the writes leave the whole storage function
  unchanged; a write changes exactly one slot.

  `PreservesSlot S c` says: every successful run of `c` leaves storage slot `S`
  untouched. It is closed under `bind`/`pure`/`if`, implied by whole-storage
  preservation (every read/guard/event/call), and granted by a write to a slot
  `≠ S`. Composed over a body, it proves that a supply/withdraw call does not move
  the watched borrower's position word - the frame fact the disciplines need -
  using only `StorageFrame.Loc.slot_ne` for the slot-distinctness leaves.
-/

import Morpho.Proofs.StorageFrame

namespace Morpho.Proofs.FramePreserve

open Verity
open Contracts
open Morpho.Proofs.StorageFrame

/-- `c` leaves the whole storage map unchanged on every successful run. -/
def StoragePreserving {α : Type} (c : Contract α) : Prop :=
  ∀ cs v cs', c cs = ContractResult.success v cs' → cs'.storage = cs.storage

/-- `c` leaves storage slot `S` unchanged on every successful run. -/
def PreservesSlot (S : Nat) {α : Type} (c : Contract α) : Prop :=
  ∀ cs v cs', c cs = ContractResult.success v cs' → cs'.storage S = cs.storage S

/-- On a successful `Contract.run`, the underlying step succeeds identically
    (run only rewrites the revert branch). -/
theorem run_eq_of_success {α : Type} {c : Contract α} {s : ContractState}
    {v : α} {s' : ContractState}
    (h : c.run s = ContractResult.success v s') :
    c s = ContractResult.success v s' := by
  rw [Contract.run] at h
  split at h <;> simp_all

/-- Whole-storage preservation implies single-slot preservation. -/
theorem PreservesSlot.of_storage {α : Type} {c : Contract α} (S : Nat)
    (h : StoragePreserving c) : PreservesSlot S c :=
  fun cs v cs' hrun => by rw [h cs v cs' hrun]

@[simp] theorem StoragePreserving.pure {α : Type} (a : α) :
    StoragePreserving (Verity.pure a) := by
  intro cs v cs' h; simp only [Verity.pure] at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.bind {α β : Type} {c : Contract α} {k : α → Contract β}
    (hc : StoragePreserving c) (hk : ∀ a, StoragePreserving (k a)) :
    StoragePreserving (Verity.bind c k) := by
  intro cs v cs' h
  simp only [Verity.bind] at h
  split at h
  · next a t hcs => exact (hk a t v cs' h).trans (hc cs a t hcs)
  · next => exact absurd h (by simp)

theorem PreservesSlot.pure (S : Nat) {α : Type} (a : α) :
    PreservesSlot S (Verity.pure a) :=
  PreservesSlot.of_storage S (StoragePreserving.pure a)

theorem PreservesSlot.bind (S : Nat) {α β : Type} {c : Contract α} {k : α → Contract β}
    (hc : PreservesSlot S c) (hk : ∀ a, PreservesSlot S (k a)) :
    PreservesSlot S (Verity.bind c k) := by
  intro cs v cs' h
  simp only [Verity.bind] at h
  split at h
  · next a t hcs => exact (hk a t v cs' h).trans (hc cs a t hcs)
  · next => exact absurd h (by simp)

theorem PreservesSlot.ite (S : Nat) {α : Type} {b : Bool} {t e : Contract α}
    (ht : PreservesSlot S t) (he : PreservesSlot S e) :
    PreservesSlot S (if b then t else e) := by
  split
  · exact ht
  · exact he

theorem PreservesSlot.ite' (S : Nat) {α : Type} {c : Prop} [Decidable c] {t e : Contract α}
    (ht : PreservesSlot S t) (he : PreservesSlot S e) :
    PreservesSlot S (if c then t else e) := by
  split
  · exact ht
  · exact he

theorem StoragePreserving.ite {α : Type} {b : Bool} {t e : Contract α}
    (ht : StoragePreserving t) (he : StoragePreserving e) :
    StoragePreserving (if b then t else e) := by
  split
  · exact ht
  · exact he

/-! ### Whole-storage preservation for the state-reading primitives. -/

theorem StoragePreserving.structMemberAt {κ α : Type} [StorageKey κ] [StorageWord α]
    (base wo : Nat) (packed : Option (Nat × Nat)) (key : κ) :
    StoragePreserving (structMemberAt base wo packed key : Contract α) := by
  intro cs v cs' h; unfold Contracts.structMemberAt at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.structMember2At {κ₁ κ₂ α : Type}
    [StorageKey κ₁] [StorageKey κ₂] [StorageWord α]
    (base wo : Nat) (packed : Option (Nat × Nat)) (k1 : κ₁) (k2 : κ₂) :
    StoragePreserving (structMember2At base wo packed k1 k2 : Contract α) := by
  intro cs v cs' h; unfold Contracts.structMember2At at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.contractAddress :
    StoragePreserving Verity.contractAddress := by
  intro cs v cs' h; unfold Verity.contractAddress at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.contractAddress' :
    StoragePreserving Contracts.contractAddress := StoragePreserving.contractAddress

theorem StoragePreserving.msgSender :
    StoragePreserving Verity.msgSender := by
  intro cs v cs' h; unfold Verity.msgSender at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.require (cond : Bool) (msg : String) :
    StoragePreserving (Verity.require cond msg) := by
  intro cs v cs' h
  unfold Verity.require at h
  split at h
  · injection h with _ hcs; subst hcs; rfl
  · exact absurd h (by simp)

theorem StoragePreserving.blockTimestamp :
    StoragePreserving Verity.blockTimestamp := by
  intro cs v cs' h; unfold Verity.blockTimestamp at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.getStorageAddr (s : StorageSlot Address) :
    StoragePreserving (Verity.getStorageAddr s) := by
  intro cs v cs' h; unfold Verity.getStorageAddr at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.getMapping2 (s : StorageSlot (Address → Address → Uint256))
    (k1 k2 : Address) :
    StoragePreserving (Verity.getMapping2 s k1 k2) := by
  intro cs v cs' h; unfold Verity.getMapping2 at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.ecmEnvRead (mod : Compiler.ECM.ExternalCallModule)
    (args : List Uint256) (idx : Nat) :
    StoragePreserving (ecmEnvRead mod args idx) := by
  intro cs v cs' h; unfold Contracts.ecmEnvRead at h; injection h with _ hcs; subst hcs; rfl

theorem StoragePreserving.requireSomeUint (opt : Option Uint256) (msg : String) :
    StoragePreserving (Stdlib.Math.requireSomeUint opt msg) := by
  intro cs v cs' h
  unfold Stdlib.Math.requireSomeUint at h
  cases opt <;>
    simp_all [Verity.bind, Verity.pure, Verity.require, Pure.pure, Bind.bind]

theorem StoragePreserving.addPanic (a b : Uint256) :
    StoragePreserving (Stdlib.Math.addPanic a b) :=
  StoragePreserving.requireSomeUint _ _

theorem StoragePreserving.subPanic (a b : Uint256) :
    StoragePreserving (Stdlib.Math.subPanic a b) :=
  StoragePreserving.requireSomeUint _ _

theorem StoragePreserving.mulPanic (a b : Uint256) :
    StoragePreserving (Stdlib.Math.mulPanic a b) :=
  StoragePreserving.requireSomeUint _ _

instance : LawfulMonad Contract := LawfulMonad.mk'
  (id_map := fun x => Verity.Contract.bind_pure_right x)
  (pure_bind := fun _ _ => rfl)
  (bind_assoc := fun x f g => Verity.Contract.bind_assoc x f g)

theorem StoragePreserving.emitEvent (name : String) (args idx : List Uint256) :
    StoragePreserving (Verity.emitEvent name args idx) := by
  intro cs v cs' h; unfold Verity.emitEvent at h; injection h with _ hcs; subst hcs; rfl

/-- `mapM` of state-preserving contracts is state-preserving. -/
theorem StoragePreserving.listMapM {α β : Type} (f : α → Contract β) (l : List α)
    (hf : ∀ a ∈ l, StoragePreserving (f a)) :
    StoragePreserving (l.mapM f) := by
  induction l with
  | nil =>
    intro cs v cs' h
    simp only [List.mapM_nil, Verity.pure, Pure.pure] at h
    injection h with _ hcs; subst hcs; rfl
  | cons a t ih =>
    rw [List.mapM_cons]
    apply StoragePreserving.bind (hf a (by simp))
    intro b
    apply StoragePreserving.bind (ih (fun x hx => hf x (by simp [hx])))
    intro bs
    exact StoragePreserving.pure _

theorem StoragePreserving.emit (name : String) (args : List Contracts.EventArg)
    (hargs : ∀ e ∈ args, StoragePreserving e.toWord) :
    StoragePreserving (Contracts.emit name args) := by
  unfold Contracts.emit
  apply StoragePreserving.bind (StoragePreserving.listMapM _ args hargs)
  intro words
  exact StoragePreserving.emitEvent _ _ _

/-! ### Writes: preserve every slot other than the one written. -/

theorem PreservesSlot.setStructMemberAt {κ α : Type} [StorageKey κ] [StorageWord α]
    (base wo : Nat) (packed : Option (Nat × Nat)) (key : κ) (val : α) (S : Nat)
    (hne : S ≠ structSlot base (StorageKey.toWord key) wo) :
    PreservesSlot S (setStructMemberAt base wo packed key val) := by
  intro cs v cs' h
  unfold Contracts.setStructMemberAt at h
  injection h with _ hcs; subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

theorem PreservesSlot.setStructMember2At {κ₁ κ₂ α : Type}
    [StorageKey κ₁] [StorageKey κ₂] [StorageWord α]
    (base wo : Nat) (packed : Option (Nat × Nat)) (k1 : κ₁) (k2 : κ₂) (val : α) (S : Nat)
    (hne : S ≠ structSlot2 base (StorageKey.toWord k1) (StorageKey.toWord k2) wo) :
    PreservesSlot S (setStructMember2At base wo packed k1 k2 val) := by
  intro cs v cs' h
  unfold Contracts.setStructMember2At at h
  injection h with _ hcs; subst hcs
  dsimp only
  rw [if_neg (by intro hh; exact hne (by simpa using hh))]

end Morpho.Proofs.FramePreserve
