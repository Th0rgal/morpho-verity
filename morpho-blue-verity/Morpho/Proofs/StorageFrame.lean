/-
  StorageFrame - storage non-aliasing for the Morpho layout.

  The refinement disciplines (`Refinement.lean`) need to know that a write to one
  storage location leaves an *unrelated* location unchanged: e.g. that supplying
  liquidity does not move the watched borrower's `borrowShares`/`collateral`, that
  a market-total write does not touch a position word, and so on.

  Every storage location Morpho touches is one of two shapes:

  - a *position* word `(id, account, wordOffset)` - base slot 2, nested mapping;
  - a *market* word `(id, wordOffset)` - base slot 3, single mapping.

  Their physical slots are `keccak`-derived (`structSlot` / `structSlot2`). Two
  semantically distinct locations occupy distinct slots. That fact rests on keccak
  collision-resistance together with the standard Solidity storage-layout spacing
  (in-struct word offsets stay below `MAX_WORDS`); it is the single named
  cryptographic/layout trust boundary, the same one the differential parity suite
  exercises empirically. We isolate it as one axiom (`Loc.slot_inj`) rather than
  scatter unprovable slot-distinctness facts through the proofs.

  Note: two *packed* fields sharing one word (e.g. `borrowShares` and `collateral`,
  both word 1) are the SAME `Loc`. Distinguishing them is bit-level algebra
  (`Contracts.decode_encode_*`), not a framing question, and is handled separately.
-/

import Morpho.Proofs.Projection

namespace Morpho.Proofs.StorageFrame

open Verity
open Contracts
open Compiler.Proofs (abstractMappingSlot)
open Compiler.Constants (evmModulus)

/-- A generous upper bound on the number of words in any Morpho storage struct
    (`positionSlot` uses 2, `marketSlot` uses 3). In-struct word offsets stay
    strictly below this. -/
def MAX_WORDS : Nat := 8

/-- A storage location Morpho reads or writes: either a position word
    `(id, account, wordOffset)` at base slot 2, or a market word `(id, wordOffset)`
    at base slot 3. Keys are stored already reduced to their `Nat` word. -/
inductive Loc where
  | pos (id account wordOffset : Nat)
  | mkt (id wordOffset : Nat)
  deriving DecidableEq

/-- The physical storage slot of a location, through the *generated* slot
    derivation (`structSlot` / `structSlot2`) - faithful to the layout by
    construction. -/
def Loc.slot : Loc → Nat
  | .pos id account wo => structSlot2 2 id account wo
  | .mkt id wo         => structSlot 3 id wo

/-- A location is *valid* when its word offset is within `MAX_WORDS`. Every
    location Morpho actually accesses is valid. -/
def Loc.valid : Loc → Prop
  | .pos _ _ wo => wo < MAX_WORDS
  | .mkt _ wo   => wo < MAX_WORDS

/--
  **Named trust boundary: keccak/layout storage non-aliasing.**

  Distinct valid storage locations occupy distinct physical slots. Equivalently,
  the slot map is injective on valid locations. This is keccak collision-resistance
  plus the Solidity layout's in-struct spacing; it is exercised by the differential
  parity suite, named here rather than re-derived. -/
axiom Loc.slot_inj {a b : Loc} (ha : a.valid) (hb : b.valid)
    (h : a.slot = b.slot) : a = b

/-- Distinct valid locations have distinct slots - the contrapositive of
    `Loc.slot_inj`, the form the frame proofs consume. -/
theorem Loc.slot_ne {a b : Loc} (ha : a.valid) (hb : b.valid) (hne : a ≠ b) :
    a.slot ≠ b.slot := fun h => hne (Loc.slot_inj ha hb h)

/-- A position word's physical slot, as the generated derivation produces it. -/
theorem pos_slot (id account wo : Nat) :
    structSlot2 2 id account wo = (Loc.pos id account wo).slot := rfl

/-- A market word's physical slot, as the generated derivation produces it. -/
theorem mkt_slot (id wo : Nat) :
    structSlot 3 id wo = (Loc.mkt id wo).slot := rfl

end Morpho.Proofs.StorageFrame
