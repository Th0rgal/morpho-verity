/-
  Proofs of rounding direction properties.

  These show that the Down variant of each share/asset conversion always
  produces a result ≤ the Up variant, ensuring the protocol never loses
  value through rounding.

  Note: The rounding direction proofs require no-overflow assumptions
  (the result must fit in Uint256). In Solidity, this is enforced by
  checked arithmetic — any overflow would revert the transaction.
  The round-trip proofs show that the protocol never returns more than
  was deposited: `floor(floor(a*N/D)*D/N) ≤ a` for natural division.
-/
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.MathLib
import Morpho.Specs.Rounding

namespace Morpho.Proofs.Rounding

open Verity.Core
open Morpho.Libraries.SharesMathLib
open Morpho.Libraries.MathLib
open Morpho.Specs.Rounding

/-! ## Core rounding lemma (proven in MathLib)

  `mulDivDown_le_mulDivUp` states that floor division ≤ ceiling division
  whenever the denominator is positive and the result doesn't overflow.
  This is the foundation for all rounding direction properties. -/

/-! ## SharesMathLib rounding direction

  Each conversion (toShares, toAssets) wraps `mulDivDown` / `mulDivUp` with
  denominators that include a virtual offset (VIRTUAL_ASSETS=1 or
  VIRTUAL_SHARES=1e6). We require:
  - The denominator doesn't wrap to zero when packed into Uint256
  - The ceiling-division result fits in Uint256 (Solidity reverts otherwise) -/

/-- Shares rounding: toSharesDown ≤ toSharesUp.
    The denominator is `totalAssets + 1` (VIRTUAL_ASSETS = 1).
    `h_denom` ensures it fits in Uint256 (so it doesn't wrap to 0).
    `h_no_overflow` ensures the ceiling result fits in Uint256
    (Solidity checked arithmetic would revert otherwise). -/
theorem toSharesDown_le_toSharesUp (assets totalAssets totalShares : Uint256)
    (h_denom : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_num : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_no_overflow : (assets.val * (totalShares.val + VIRTUAL_SHARES)
      + (totalAssets.val + VIRTUAL_ASSETS - 1))
      / (totalAssets.val + VIRTUAL_ASSETS) < Uint256.modulus) :
    sharesDownLeUp assets totalAssets totalShares := by
  unfold sharesDownLeUp toSharesDown toSharesUp
  apply mulDivDown_le_mulDivUp
  · -- Denominator > 0: (Uint256.ofNat (totalAssets.val + 1)).val > 0
    show (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val > 0
    rw [Uint256.val_ofNat, Nat.mod_eq_of_lt h_denom]
    show totalAssets.val + VIRTUAL_ASSETS > 0
    simp [VIRTUAL_ASSETS]
  · -- No overflow: ceiling division result < modulus
    show (assets.val * (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val
      + ((Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val - 1))
      / (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val < Uint256.modulus
    rw [Uint256.val_ofNat, Uint256.val_ofNat,
        Nat.mod_eq_of_lt h_denom, Nat.mod_eq_of_lt h_num]
    exact h_no_overflow

/-- Assets rounding: toAssetsDown ≤ toAssetsUp.
    The denominator is `totalShares + 1e6` (VIRTUAL_SHARES = 10^6).
    Same structure as the shares proof with swapped roles. -/
theorem toAssetsDown_le_toAssetsUp (shares totalAssets totalShares : Uint256)
    (h_denom : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_num : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_no_overflow : (shares.val * (totalAssets.val + VIRTUAL_ASSETS)
      + (totalShares.val + VIRTUAL_SHARES - 1))
      / (totalShares.val + VIRTUAL_SHARES) < Uint256.modulus) :
    assetsDownLeUp shares totalAssets totalShares := by
  unfold assetsDownLeUp toAssetsDown toAssetsUp
  apply mulDivDown_le_mulDivUp
  · -- Denominator > 0
    show (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val > 0
    rw [Uint256.val_ofNat, Nat.mod_eq_of_lt h_denom]
    show totalShares.val + VIRTUAL_SHARES > 0
    simp [VIRTUAL_SHARES]
  · -- No overflow
    show (shares.val * (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)).val
      + ((Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val - 1))
      / (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)).val < Uint256.modulus
    rw [Uint256.val_ofNat, Uint256.val_ofNat,
        Nat.mod_eq_of_lt h_denom, Nat.mod_eq_of_lt h_num]
    exact h_no_overflow

/-! ## Round-trip properties — Protocol safety

  When a user supplies assets and then withdraws, the protocol must never return
  more than was deposited. The rounding directions guarantee this:

  1. Supply: user deposits `a` assets, receives `s = toSharesDown(a)` shares (rounded down)
  2. Withdraw: user burns `s` shares, receives `toAssetsDown(s)` assets (rounded down)

  The protocol keeps `a - toAssetsDown(toSharesDown(a))` ≥ 0. Symmetrically for shares.

  This is the correct safety property. The converse (`ceil(floor(a*N/D)*D/N) ≥ a`)
  does NOT hold in general — rounding can cause small losses per round-trip. The
  Morpho protocol is designed so these losses always favor the protocol, not the user. -/

/-- Supply round-trip: the protocol never returns more assets than were deposited.
    `floor(floor(a*N/D)*D/N) ≤ a` always holds for natural number division. With Uint256
    wrapping, we need the intermediate and final results to fit (no overflow). -/
theorem supply_roundtrip_protocol_safe (assets totalAssets totalShares : Uint256)
    (h_denom_a : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_denom_s : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_no_overflow_1 : assets.val * (totalShares.val + VIRTUAL_SHARES)
      / (totalAssets.val + VIRTUAL_ASSETS) < Uint256.modulus)
    (h_no_overflow_2 : assets.val * (totalShares.val + VIRTUAL_SHARES)
      / (totalAssets.val + VIRTUAL_ASSETS)
      * (totalAssets.val + VIRTUAL_ASSETS)
      / (totalShares.val + VIRTUAL_SHARES) < Uint256.modulus) :
    (toAssetsDown (toSharesDown assets totalAssets totalShares) totalAssets totalShares).val
      ≤ assets.val := by
  -- Reduce to pure Nat: show floor(floor(a*N/D)*D/N) ≤ a
  show (mulDivDown
    (mulDivDown assets (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))
      (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS)))
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))).val ≤ assets.val
  simp only [mulDivDown, Uint256.val_ofNat,
    Nat.mod_eq_of_lt h_denom_a, Nat.mod_eq_of_lt h_denom_s,
    Nat.mod_eq_of_lt h_no_overflow_1, Nat.mod_eq_of_lt h_no_overflow_2]
  -- Goal: a * N / D * D / N ≤ a (pure Nat arithmetic)
  apply Nat.div_le_of_le_mul
  calc assets.val * (totalShares.val + VIRTUAL_SHARES)
        / (totalAssets.val + VIRTUAL_ASSETS) * (totalAssets.val + VIRTUAL_ASSETS)
      ≤ assets.val * (totalShares.val + VIRTUAL_SHARES) :=
          Nat.div_mul_le_self _ _
    _ = (totalShares.val + VIRTUAL_SHARES) * assets.val := Nat.mul_comm _ _

/-- Withdraw round-trip: the protocol never returns more shares than were burned.
    Symmetric to supply — `floor(floor(s*D/N)*N/D) ≤ s`. -/
theorem withdraw_roundtrip_protocol_safe (shares totalAssets totalShares : Uint256)
    (h_denom_a : totalAssets.val + VIRTUAL_ASSETS < Uint256.modulus)
    (h_denom_s : totalShares.val + VIRTUAL_SHARES < Uint256.modulus)
    (h_no_overflow_1 : shares.val * (totalAssets.val + VIRTUAL_ASSETS)
      / (totalShares.val + VIRTUAL_SHARES) < Uint256.modulus)
    (h_no_overflow_2 : shares.val * (totalAssets.val + VIRTUAL_ASSETS)
      / (totalShares.val + VIRTUAL_SHARES)
      * (totalShares.val + VIRTUAL_SHARES)
      / (totalAssets.val + VIRTUAL_ASSETS) < Uint256.modulus) :
    (toSharesDown (toAssetsDown shares totalAssets totalShares) totalAssets totalShares).val
      ≤ shares.val := by
  show (mulDivDown
    (mulDivDown shares (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))
      (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES)))
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))).val ≤ shares.val
  simp only [mulDivDown, Uint256.val_ofNat,
    Nat.mod_eq_of_lt h_denom_a, Nat.mod_eq_of_lt h_denom_s,
    Nat.mod_eq_of_lt h_no_overflow_1, Nat.mod_eq_of_lt h_no_overflow_2]
  apply Nat.div_le_of_le_mul
  calc shares.val * (totalAssets.val + VIRTUAL_ASSETS)
        / (totalShares.val + VIRTUAL_SHARES) * (totalShares.val + VIRTUAL_SHARES)
      ≤ shares.val * (totalAssets.val + VIRTUAL_ASSETS) :=
          Nat.div_mul_le_self _ _
    _ = (totalAssets.val + VIRTUAL_ASSETS) * shares.val := Nat.mul_comm _ _

end Morpho.Proofs.Rounding
