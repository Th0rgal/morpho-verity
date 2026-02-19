/-
  Proofs of Morpho Blue invariants.
-/
import Morpho.Morpho
import Morpho.Specs.Invariants

namespace Morpho.Proofs.Invariants

open Verity
open Morpho.Types
open Morpho.Specs.Invariants

/-! ## IRM/LLTV monotonicity

  Once an IRM or LLTV is enabled, enableIrm/enableLltv never disable it.
  The new state's isIrmEnabled/isLltvEnabled is a superset of the old one. -/

theorem enableIrm_monotone (s : MorphoState) (irm irm' : Address)
    (h : Morpho.enableIrm s irm = some s')
    (h_enabled : s.isIrmEnabled irm') :
    s'.isIrmEnabled irm' := by
  unfold Morpho.enableIrm at h
  split at h <;> simp at h
  -- After first split+simp, the isTrue case (owner check failed) is closed.
  -- In the isFalse case, h is a conjunction: h.left = ¬already_set, h.right = s' = ...
  -- But simp may have combined differently. Let's extract s'.
  rw [← h.right]
  simp
  by_cases h_eq : irm' = irm
  · simp [h_eq]
  · simp [h_eq, h_enabled]

theorem enableLltv_monotone (s : MorphoState) (lltv lltv' : Uint256)
    (h : Morpho.enableLltv s lltv = some s')
    (h_enabled : s.isLltvEnabled lltv') :
    s'.isLltvEnabled lltv' := by
  unfold Morpho.enableLltv at h
  split at h <;> simp at h
  -- h : ... ∧ ... ∧ { ... } = s'
  rw [← h.right.right]
  simp
  by_cases h_eq : lltv' = lltv
  · simp [h_eq]
  · simp [h_eq, h_enabled]

/-! ## LLTV < WAD -/

theorem enableLltv_lt_wad (s : MorphoState) (lltv : Uint256)
    (h : Morpho.enableLltv s lltv = some s') :
    s'.isLltvEnabled lltv → lltv.val < Libraries.MathLib.WAD := by
  unfold Morpho.enableLltv at h
  split at h <;> simp at h
  -- h : ... ∧ lltv.val < WAD ∧ { ... } = s'
  intro _
  exact h.right.left

/-! ## Market creation validity -/

theorem createMarket_requires_enabled (s : MorphoState) (params : MarketParams) (id : Id)
    (h : Morpho.createMarket s params id = some s') :
    s.isIrmEnabled params.irm ∧ s.isLltvEnabled params.lltv := by
  unfold Morpho.createMarket at h
  simp at h
  -- h is a conjunction: isIrmEnabled ∧ isLltvEnabled ∧ lastUpdate = 0 ∧ s' = ...
  exact ⟨h.left, h.right.left⟩

/-! ## Fee bounds preserved by setFee -/

theorem setFee_preserves_feeInRange (s : MorphoState) (id : Id) (newFee : Uint256)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h : Morpho.setFee s id newFee borrowRate hasIrm = some s') :
    feeInRange s' id := by
  unfold Morpho.setFee at h
  split at h <;> simp at h
  -- h : ¬lastUpdate=0 ∧ ¬newFee=fee ∧ newFee.val ≤ MAX_FEE ∧ { ... fee := newFee ... } = s'
  unfold feeInRange
  rw [← h.right.right.right]
  -- Goal: ({ ... market := fun id' => if id' = id then { ... fee := newFee } else ... }).market id).fee.val ≤ MAX_FEE
  simp
  exact h.right.right.left

/-! ## Solvency preserved by core operations -/

/-- Supply adds to totalSupplyAssets without changing totalBorrowAssets.

    In Solidity 0.8+, `totalSupplyAssets += assets` uses checked arithmetic
    and reverts on overflow. We model this revert condition as `h_no_overflow`:
    the raw sum fits in Uint256 (i.e., `< 2^256`). Without this, the modular
    wrapping could make the new totalSupplyAssets smaller than totalBorrowAssets. -/
theorem supply_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s'))
    (h_no_overflow : (s.market id).totalSupplyAssets.val + a.val < Verity.Core.Uint256.modulus) :
    borrowLeSupply s' id := by
  unfold Morpho.supply at h_ok
  simp at h_ok
  obtain ⟨_, _, _, h_a_eq, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]
  simp [Morpho.u256_val]
  -- Goal: totalBorrowAssets.val ≤ (totalSupplyAssets.val + effectiveAssets.val) % modulus
  rw [← h_a_eq] at h_no_overflow
  rw [Nat.mod_eq_of_lt h_no_overflow]
  calc (s.market id).totalBorrowAssets.val
      ≤ (s.market id).totalSupplyAssets.val := h_solvent
    _ ≤ (s.market id).totalSupplyAssets.val + _ := Nat.le_add_right _ _

theorem withdraw_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    borrowLeSupply s' id := by
  unfold Morpho.withdraw at h_ok
  simp at h_ok
  -- h_ok is a 9-element conjunction; destructure to get the liquidity check and state equality
  obtain ⟨_, _, _, _, _, h_liq, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply
  rw [← h_eq]
  simp
  exact h_liq

theorem repay_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    borrowLeSupply s' id := by
  unfold Morpho.repay at h_ok
  simp at h_ok
  -- Repay: totalBorrowAssets decreases via zeroFloorSub, totalSupplyAssets unchanged
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]
  simp
  -- Goal: (zeroFloorSub totalBorrowAssets computed_assets).val ≤ totalSupplyAssets.val
  -- By zeroFloorSub_le: zeroFloorSub x y ≤ x
  -- By h_solvent: x ≤ totalSupplyAssets
  calc (Libraries.UtilsLib.zeroFloorSub (s.market id).totalBorrowAssets _).val
      ≤ (s.market id).totalBorrowAssets.val := Libraries.UtilsLib.zeroFloorSub_le _ _
    _ ≤ (s.market id).totalSupplyAssets.val := h_solvent

/-- Borrow adds to totalBorrowAssets but the liquidity check (Morpho.sol:259)
    ensures totalBorrowAssets ≤ totalSupplyAssets in the resulting state. -/
theorem borrow_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv = some (a, sh, s')) :
    borrowLeSupply s' id := by
  unfold Morpho.borrow at h_ok
  simp at h_ok
  obtain ⟨_, _, _, _, _, h_liq, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply
  rw [← h_eq]
  simp
  exact h_liq

/-! ## Timestamp monotonicity -/

/-- Interest accrual never decreases a market's lastUpdate timestamp.
    In the elapsed=0 case, state is unchanged. Otherwise, lastUpdate is set
    to blockTimestamp which is ≥ lastUpdate (time moves forward). -/
theorem accrueInterest_lastUpdate_monotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h_time : (s.market id).lastUpdate.val ≤ s.blockTimestamp.val) :
    (s.market id).lastUpdate.val ≤
      ((Morpho.accrueInterest s id borrowRate hasIrm).market id).lastUpdate.val := by
  unfold Morpho.accrueInterest
  simp
  split
  · -- elapsed = 0: state unchanged
    exact Nat.le_refl _
  · split
    · -- ¬hasIrm: lastUpdate := blockTimestamp
      simp
      exact h_time
    · -- hasIrm: lastUpdate := blockTimestamp (in the full interest case)
      simp
      exact h_time

/-! ## Collateralization preserved by liquidation

  Bad debt socialization ensures that when collateral hits zero,
  borrowShares are also zeroed out. So no position ever has debt
  without collateral after a successful liquidation. -/

/-- Liquidation preserves the `alwaysCollateralized` invariant for the borrower.
    When collateral is fully seized (= 0), bad debt socialization sets
    borrowShares to 0. When collateral remains, it's > 0. -/
theorem liquidate_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    alwaysCollateralized s' id borrower := by
  unfold alwaysCollateralized
  intro h_borrow
  unfold Morpho.liquidate at h_ok
  -- Close guard branches (none = some) and split seizedAssets > 0.
  -- `<;> simp at h_ok` closes isTrue branches and flattens isFalse conjunctions.
  -- After 3 rounds, two goals remain (seizedAssets > 0 and ≤ 0).
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  -- Three remaining goals after split/simp. Address each by case label.
  case isFalse.isTrue.isTrue =>
    -- seizedAssets > 0, bad-debt condition = true (newCollateral = 0).
    -- borrowShares = u256 0, so h_borrow gives 0 < 0: contradiction.
    rw [← h_ok.2.2.2.2.2.2] at h_borrow; simp at h_borrow
  case isFalse.isTrue.isFalse =>
    -- seizedAssets > 0, bad-debt condition = false (newCollateral ≠ 0).
    rw [← h_ok.2.2.2.2.2.2] at h_borrow ⊢; simp at h_borrow ⊢; omega
  case isFalse.isFalse =>
    -- seizedAssets = 0. Bad-debt if-then-else still in h_borrow.
    rw [← h_ok.2.2.2.2.2.2] at h_borrow ⊢; simp at h_borrow ⊢
    split at h_borrow
    · simp at h_borrow  -- bad-debt: borrowShares = 0, contradicts h_borrow > 0
    · omega             -- non-bad-debt: newCollateral ≠ 0 in context

end Morpho.Proofs.Invariants
