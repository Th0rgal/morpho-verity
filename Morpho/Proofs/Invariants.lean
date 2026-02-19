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

/-- Helper: if a ≤ b and both a,b < modulus, then (a-d)%mod ≤ (b-d)%mod. -/
private theorem sub_mod_le_sub_mod {a b d modulus : Nat}
    (h_le : a ≤ b) (h_a_lt : a < modulus) (h_b_lt : b < modulus) :
    (a - d) % modulus ≤ (b - d) % modulus := by
  rw [Nat.mod_eq_of_lt (Nat.lt_of_le_of_lt (Nat.sub_le a d) h_a_lt),
      Nat.mod_eq_of_lt (Nat.lt_of_le_of_lt (Nat.sub_le b d) h_b_lt)]
  exact Nat.sub_le_sub_right h_le d

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

/-! ## Solvency preserved by interest accrual

  Interest accrual adds the same `interest` amount to both totalBorrowAssets and
  totalSupplyAssets (Morpho.sol:490-491). Since both sides increase equally,
  the solvency invariant `totalBorrowAssets ≤ totalSupplyAssets` is preserved.

  Together with `liquidate_preserves_borrowLeSupply`, this closes the solvency invariant
  chain: every state-mutating operation (supply, withdraw, borrow, repay, liquidate,
  accrueInterest) preserves `totalBorrowAssets ≤ totalSupplyAssets`. -/

/-- Interest accrual preserves solvency: both borrow and supply increase by
    the same amount, so borrow ≤ supply is maintained. -/
theorem accrueInterest_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h_solvent : borrowLeSupply s id)
    (h_no_overflow : (s.market id).totalSupplyAssets.val +
      (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Verity.Core.Uint256.modulus) :
    borrowLeSupply (Morpho.accrueInterest s id borrowRate hasIrm) id := by
  unfold Morpho.accrueInterest
  simp
  split
  · -- elapsed = 0: state unchanged
    exact h_solvent
  · split
    · -- ¬hasIrm: only lastUpdate changes, totals unchanged
      unfold borrowLeSupply; simp; exact h_solvent
    · -- hasIrm: both totalBorrowAssets and totalSupplyAssets increase by interest
      unfold borrowLeSupply; simp
      -- Goal: (totalBorrowAssets + interest) % modulus ≤ (totalSupplyAssets + interest) % modulus
      -- h_no_overflow ensures totalSupplyAssets + interest < modulus.
      -- Since borrow ≤ supply, totalBorrowAssets + interest ≤ totalSupplyAssets + interest < modulus.
      have h_borrow_no_overflow :
        (s.market id).totalBorrowAssets.val +
          (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
            (Libraries.MathLib.wTaylorCompounded borrowRate
              (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
          < Verity.Core.Uint256.modulus :=
        Nat.lt_of_le_of_lt (Nat.add_le_add_right h_solvent _) h_no_overflow
      rw [Nat.mod_eq_of_lt h_no_overflow, Nat.mod_eq_of_lt h_borrow_no_overflow]
      exact Nat.add_le_add_right h_solvent _

/-- Liquidation preserves solvency. In the non-bad-debt path, `zeroFloorSub`
    reduces borrow without touching supply. In the bad-debt path, both borrow
    and supply decrease by the same `badDebtAssets`, preserving the order.

    Unlike `supply` and `accrueInterest`, no overflow hypotheses are needed:
    all subtractions produce values ≤ the original Uint256 fields, which are
    already < 2^256 by construction. -/
theorem liquidate_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    borrowLeSupply s' id := by
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  -- After 3 rounds of split/simp we have 3 cases based on seizedAssets > 0
  -- and the bad-debt condition. Each case's h_ok ends with a state record = s'.
  case isFalse.isTrue.isFalse =>
    -- seizedAssets > 0, non-bad-debt: totalBorrowAssets = zeroFloorSub, supply unchanged
    unfold borrowLeSupply; rw [← h_ok.2.2.2.2.2.2]; simp only [beq_self_eq_true, ite_true]
    exact Nat.le_trans (Libraries.UtilsLib.zeroFloorSub_le _ _) h_solvent
  case isFalse.isTrue.isTrue =>
    -- seizedAssets > 0, bad-debt: both sides decrease by badDebtAssets
    unfold borrowLeSupply; rw [← h_ok.2.2.2.2.2.2]; simp only [beq_self_eq_true, ite_true]
    simp only [Morpho.u256_val]
    exact sub_mod_le_sub_mod
      (Nat.le_trans (Libraries.UtilsLib.zeroFloorSub_le _ _) h_solvent)
      (Libraries.UtilsLib.zeroFloorSub _ _).isLt
      (s.market id).totalSupplyAssets.isLt
  case isFalse.isFalse =>
    -- seizedAssets = 0: bad-debt if-then-else still present
    unfold borrowLeSupply; rw [← h_ok.2.2.2.2.2.2]; simp only [beq_self_eq_true, ite_true]
    split
    · -- bad-debt path (newCollateral = 0)
      simp only [Morpho.u256_val]
      exact sub_mod_le_sub_mod
        (Nat.le_trans (Libraries.UtilsLib.zeroFloorSub_le _ _) h_solvent)
        (Libraries.UtilsLib.zeroFloorSub _ _).isLt
        (s.market id).totalSupplyAssets.isLt
    · -- non-bad-debt path (newCollateral > 0)
      exact Nat.le_trans (Libraries.UtilsLib.zeroFloorSub_le _ _) h_solvent

/-! ## Solvency preserved by collateral and admin operations

  Collateral operations (supplyCollateral, withdrawCollateral) only modify positions,
  never market-level totals. Admin operations (setOwner, enableIrm, enableLltv,
  setFeeRecipient, createMarket, setAuthorization, setAuthorizationWithSig) either
  don't touch markets at all, or only modify non-total fields (lastUpdate, fee,
  idToParams, isIrmEnabled, isLltvEnabled). All trivially preserve borrowLeSupply. -/

/-- Supply collateral never changes market totals, so solvency is trivially preserved. -/
theorem supplyCollateral_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf : Address)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]; exact h_solvent

/-- Withdraw collateral never changes market totals, so solvency is trivially preserved. -/
theorem withdrawCollateral_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address) (collateralPrice lltv : Uint256)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]; exact h_solvent

/-- enableIrm doesn't touch market records, so solvency is trivially preserved. -/
theorem enableIrm_preserves_borrowLeSupply (s : MorphoState) (irm : Address) (id : Id)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.enableIrm s irm = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.enableIrm at h_ok; split at h_ok <;> simp at h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_ok.right]; exact h_solvent

/-- enableLltv doesn't touch market records, so solvency is trivially preserved. -/
theorem enableLltv_preserves_borrowLeSupply (s : MorphoState) (lltv : Uint256) (id : Id)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.enableLltv s lltv = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.enableLltv at h_ok; split at h_ok <;> simp at h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_ok.right.right]; exact h_solvent

/-- setOwner doesn't touch market records, so solvency is trivially preserved. -/
theorem setOwner_preserves_borrowLeSupply (s : MorphoState) (newOwner : Address) (id : Id)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.setOwner at h_ok; split at h_ok <;> simp at h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_ok.right]; exact h_solvent

/-- setFeeRecipient doesn't touch market records, so solvency is trivially preserved. -/
theorem setFeeRecipient_preserves_borrowLeSupply (s : MorphoState) (newRecipient : Address)
    (id : Id) (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.setFeeRecipient s newRecipient = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.setFeeRecipient at h_ok; split at h_ok <;> simp at h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_ok.right]; exact h_solvent

/-- createMarket only sets lastUpdate and idToParams, so solvency is trivially preserved. -/
theorem createMarket_preserves_borrowLeSupply (s : MorphoState) (params : MarketParams)
    (marketId id : Id) (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]; simp
  by_cases h : id = marketId
  · subst h; simp; exact h_solvent
  · simp [h]; exact h_solvent

/-- setAuthorization doesn't touch market records, so solvency is trivially preserved. -/
theorem setAuthorization_preserves_borrowLeSupply (s : MorphoState) (authorized : Address)
    (newIsAuth : Bool) (id : Id)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.setAuthorization s authorized newIsAuth = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.setAuthorization at h_ok; split at h_ok <;> simp at h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_ok]; exact h_solvent

/-- setAuthorizationWithSig doesn't touch market records, so solvency is trivially preserved. -/
theorem setAuthorizationWithSig_preserves_borrowLeSupply (s : MorphoState)
    (auth : Authorization) (sig : Bool) (id : Id)
    (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    borrowLeSupply s' id := by
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]; exact h_solvent

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

/-! ## Market isolation

  Each Morpho operation only modifies the market it targets. All other markets
  and their positions are completely unaffected. This allows reasoning about
  each market independently. -/

/-- Interest accrual on market `id` does not change market `id'`. -/
theorem accrueInterest_market_isolated (s : MorphoState) (id id' : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (h_ne : id ≠ id') :
    marketIsolated s (Morpho.accrueInterest s id borrowRate hasIrm) id id' := by
  intro _
  unfold Morpho.accrueInterest
  simp
  split
  · rfl  -- elapsed = 0: state unchanged
  · split <;> simp [Ne.symm h_ne]

/-- Supply on market `id` does not change market `id'`. -/
theorem supply_market_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Withdraw on market `id` does not change market `id'`. -/
theorem withdraw_market_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf receiver : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Borrow on market `id` does not change market `id'`. -/
theorem borrow_market_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256) (h_ne : id ≠ id')
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Repay on market `id` does not change market `id'`. -/
theorem repay_market_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Liquidation on market `id` does not change market `id'`. -/
theorem liquidate_market_isolated (s : MorphoState) (id id' : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_ne : id ≠ id')
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (rw [← h_ok.2.2.2.2.2.2]; simp [Ne.symm h_ne])

/-- Supply collateral only changes positions, never market records. -/
theorem supplyCollateral_market_isolated (s : MorphoState) (id id' : Id)
    (assets : Uint256) (onBehalf : Address)
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]

/-- Withdraw collateral only changes positions, never market records. -/
theorem withdrawCollateral_market_isolated (s : MorphoState) (id id' : Id)
    (assets : Uint256) (onBehalf receiver : Address) (collateralPrice lltv : Uint256)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    marketIsolated s s' id id' := by
  intro _
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]

/-! ## Position isolation

  Each user's position is independent. Operations targeting user `onBehalf`
  in market `id` leave every other user's position unchanged. This guarantees
  that your supply, debt, and collateral cannot be modified by someone else's
  transactions. Even liquidation — which can seize collateral and reduce debt —
  only modifies the targeted borrower's position, never anyone else's.

  Interest accrual only modifies the fee recipient's position (to credit fee shares).
  All other users' positions are completely unchanged. -/

/-- Interest accrual does not change any non-fee-recipient's position. -/
theorem accrueInterest_position_isolated (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (user' : Address)
    (h_ne : s.feeRecipient ≠ user') :
    (Morpho.accrueInterest s id borrowRate hasIrm).position id user' =
      s.position id user' := by
  unfold Morpho.accrueInterest
  simp
  split
  · rfl  -- elapsed = 0: state unchanged
  · split
    · rfl  -- ¬hasIrm: only market changes, positions unchanged
    · simp [Ne.symm h_ne]

/-- Supply on behalf of `onBehalf` does not change `user'`'s position. -/
theorem supply_position_isolated (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf user' : Address) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Withdraw on behalf of `onBehalf` does not change `user'`'s position. -/
theorem withdraw_position_isolated (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver user' : Address) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Borrow on behalf of `onBehalf` does not change `user'`'s position. -/
theorem borrow_position_isolated (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver user' : Address)
    (collateralPrice lltv : Uint256) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Repay on behalf of `onBehalf` does not change `user'`'s position. -/
theorem repay_position_isolated (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf user' : Address) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Supply collateral on behalf of `onBehalf` does not change `user'`'s position. -/
theorem supplyCollateral_position_isolated (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf user' : Address) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Withdraw collateral on behalf of `onBehalf` does not change `user'`'s position. -/
theorem withdrawCollateral_position_isolated (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver user' : Address)
    (collateralPrice lltv : Uint256) (h_ne : onBehalf ≠ user')
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    positionIsolated s s' id onBehalf user' := by
  intro _
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Liquidation of `borrower` does not change `user'`'s position.
    Liquidation can only modify the targeted borrower's position (reducing their
    collateral and debt). All other users' positions are completely untouched. -/
theorem liquidate_position_isolated (s : MorphoState) (id : Id)
    (borrower user' : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_ne : borrower ≠ user')
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    positionIsolated s s' id borrower user' := by
  intro _
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (rw [← h_ok.2.2.2.2.2.2]; simp [Ne.symm h_ne])

/-! ## Cross-market position isolation

  Operations on market `id` only modify positions in that market. All positions
  in every other market `id'` are completely untouched. This is the cross-market
  counterpart of the same-market position isolation above.

  Every state update uses `if id' == id && addr == target then ... else s.position id' addr`,
  so when `id' ≠ id` the condition is false and the old position is returned.

  Combined with `marketIsolated`, this proves that markets are fully independent:
  neither market-level state nor user positions in other markets can be affected. -/

/-- Interest accrual on market `id` does not change any position in market `id'`. -/
theorem accrueInterest_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (h_ne : id ≠ id') :
    crossMarketPositionIsolated s (Morpho.accrueInterest s id borrowRate hasIrm) id id' := by
  intro _ user
  unfold Morpho.accrueInterest
  simp
  split
  · rfl
  · split <;> simp [Ne.symm h_ne]

/-- Supply on market `id` does not change any position in market `id'`. -/
theorem supply_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Withdraw on market `id` does not change any position in market `id'`. -/
theorem withdraw_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf receiver : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Borrow on market `id` does not change any position in market `id'`. -/
theorem borrow_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256) (h_ne : id ≠ id')
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Repay on market `id` does not change any position in market `id'`. -/
theorem repay_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets shares : Uint256) (onBehalf : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Supply collateral on market `id` does not change any position in market `id'`. -/
theorem supplyCollateral_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets : Uint256) (onBehalf : Address) (h_ne : id ≠ id')
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Withdraw collateral on market `id` does not change any position in market `id'`. -/
theorem withdrawCollateral_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (assets : Uint256) (onBehalf receiver : Address) (collateralPrice lltv : Uint256)
    (h_ne : id ≠ id')
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]; simp [Ne.symm h_ne]

/-- Liquidation on market `id` does not change any position in market `id'`. -/
theorem liquidate_crossMarket_position_isolated (s : MorphoState) (id id' : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_ne : id ≠ id')
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    crossMarketPositionIsolated s s' id id' := by
  intro _ user
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (rw [← h_ok.2.2.2.2.2.2]; simp [Ne.symm h_ne])

end Morpho.Proofs.Invariants
