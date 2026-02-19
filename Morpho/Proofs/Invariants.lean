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

/-- setFee accrues interest (preserving solvency) then changes only the fee field,
    which does not affect totalBorrowAssets or totalSupplyAssets. -/
theorem setFee_preserves_borrowLeSupply (s : MorphoState) (id : Id) (newFee borrowRate : Uint256)
    (hasIrm : Bool) (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.setFee s id newFee borrowRate hasIrm = some s')
    (h_no_overflow : (s.market id).totalSupplyAssets.val +
      (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Verity.Core.Uint256.modulus) :
    borrowLeSupply s' id := by
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  unfold borrowLeSupply at h_solvent ⊢
  rw [← h_eq]; simp
  -- After accrueInterest, only fee changes. Market totals come from accrueInterest.
  -- accrueInterest preserves solvency (already proven).
  have h_ai := accrueInterest_preserves_borrowLeSupply s id borrowRate hasIrm h_solvent
    h_no_overflow
  unfold borrowLeSupply at h_ai
  exact h_ai

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

/-! ## Collateralization preserved by all operations

  `alwaysCollateralized` says: if a position has debt (borrowShares > 0),
  it must have collateral (collateral > 0). This is preserved because:

  - **supply/withdraw**: only touch supplyShares, never borrowShares or collateral
  - **repay**: decreases borrowShares (weakens the antecedent, so trivially preserved)
  - **supplyCollateral**: increases collateral (strengthens the consequent)
  - **liquidation**: bad debt socialization zeros borrowShares when collateral hits zero
  - **Admin functions**: don't touch positions at all

  `borrow` (increases debt) and `withdrawCollateral` (decreases collateral) are the
  non-trivial cases: they are guarded by `isHealthy`, which ensures collateral > 0
  whenever borrowShares > 0 (zero collateral would make maxBorrow = 0, failing the check).
  These proofs require a `h_borrowed_pos` hypothesis (toAssetsUp gives positive result
  for non-zero borrowShares, guaranteed by virtual shares in practice).
  - **accrueInterest / admin functions**: don't touch positions at all -/

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

/-- Supply only modifies supplyShares; borrowShares and collateral are unchanged. -/
theorem supply_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
  by_cases h : user = onBehalf
  · subst h; simp at h_borrow ⊢; exact h_collat h_borrow
  · simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- Withdraw only modifies supplyShares; borrowShares and collateral are unchanged. -/
theorem withdraw_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
  by_cases h : user = onBehalf
  · subst h; simp at h_borrow ⊢; exact h_collat h_borrow
  · simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- Repay decreases borrowShares — can only weaken the antecedent.
    For `user = onBehalf`: new borrowShares = (old - shares) % mod. Since the repay
    function checks `shares ≤ borrowShares` (underflow guard), the subtraction doesn't
    wrap, so new borrowShares ≤ old borrowShares. If new > 0, then old > 0. -/
theorem repay_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, h_underflow, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
  by_cases h : user = onBehalf
  · subst h; simp at h_borrow ⊢
    -- h_borrow: (borrowShares - shares) % mod > 0
    -- If borrowShares = 0, Nat subtraction gives 0, so 0 % mod = 0, contradiction.
    rcases Nat.eq_zero_or_pos (s.position id user).borrowShares.val with h_zero | h_pos
    · simp [h_zero] at h_borrow
    · exact h_collat h_pos
  · simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- Supply collateral increases collateral — can only strengthen the consequent.
    Needs no-overflow: Solidity checked arithmetic reverts if collateral + assets > 2^256. -/
theorem supplyCollateral_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s')
    (h_no_overflow : user = onBehalf →
      (s.position id user).collateral.val + assets.val < Verity.Core.Uint256.modulus) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
  by_cases h : user = onBehalf
  · subst h; simp at h_borrow ⊢
    rw [Nat.mod_eq_of_lt (h_no_overflow rfl)]
    exact Nat.lt_of_lt_of_le (h_collat h_borrow) (Nat.le_add_right _ _)
  · simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- Key health-check lemma: if `isHealthy` returns true and borrowShares > 0,
    then collateral > 0. The health check computes maxBorrow from collateral; if
    collateral were 0, maxBorrow would be 0 and the check would require borrowed = 0.
    The `h_borrowed_pos` hypothesis says that converting positive borrowShares to
    assets gives a positive result (true in practice due to virtual shares preventing
    zero-rounding, modeled here as a no-overflow assumption). -/
private theorem isHealthy_collateral_pos (s : MorphoState) (id : Id) (user : Address)
    (collateralPrice lltv : Uint256)
    (h_healthy : Morpho.isHealthy s id user collateralPrice lltv = true)
    (h_borrow_pos : (s.position id user).borrowShares.val > 0)
    (h_borrowed_pos : (Libraries.SharesMathLib.toAssetsUp
      (s.position id user).borrowShares
      (s.market id).totalBorrowAssets
      (s.market id).totalBorrowShares).val > 0) :
    (s.position id user).collateral.val > 0 := by
  -- isHealthy checks: if borrowShares == 0 then true else maxBorrow >= borrowed
  -- Since borrowShares > 0, we're in the else branch.
  unfold Morpho.isHealthy at h_healthy; dsimp at h_healthy
  -- Split the if on borrowShares == 0
  split at h_healthy
  · -- borrowShares == 0, contradicts h_borrow_pos
    rename_i h_eq; simp at h_eq; omega
  · -- borrowShares != 0: h_healthy says maxBorrow >= borrowed
    -- If collateral = 0, then maxBorrow = 0, but borrowed > 0, contradiction.
    rcases Nat.eq_zero_or_pos (s.position id user).collateral.val with h_coll_zero | h_pos
    · -- collateral = 0 => maxBorrow = wMulDown(mulDivDown(0, _, _), _) = 0
      -- Unfold everything and rewrite with collateral = 0
      unfold Libraries.MathLib.wMulDown Libraries.MathLib.mulDivDown u256 at h_healthy
      simp [Core.Uint256.val_ofNat, h_coll_zero] at h_healthy
      -- h_healthy now says 0 >= borrowed, but borrowed > 0 → contradiction
      omega
    · exact h_pos

/-- Borrow preserves collateralization: the `isHealthy` guard ensures that
    after increasing debt, the position still has collateral.
    `h_borrowed_pos` says toAssetsUp gives a positive result for the post-borrow
    borrowShares (always true in practice due to virtual shares). -/
theorem borrow_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver user : Address)
    (collateralPrice lltv : Uint256)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s'))
    (h_borrowed_pos : user = onBehalf →
      (Libraries.SharesMathLib.toAssetsUp
        (s'.position id user).borrowShares
        (s'.market id).totalBorrowAssets
        (s'.market id).totalBorrowShares).val > 0) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.borrow at h_ok; simp at h_ok
  -- Extract state equality and health check from the conjunction
  obtain ⟨_, _, _, _, h_healthy, _, _, _, h_eq⟩ := h_ok
  -- Rewrite s' as the constructed state in health check
  have h_healthy' : Morpho.isHealthy s' id onBehalf collateralPrice lltv = true := by
    rw [← h_eq]; exact h_healthy
  by_cases h : user = onBehalf
  · subst h
    -- isHealthy was checked on the post-state s'; collateral unchanged by borrow.
    exact isHealthy_collateral_pos s' id user collateralPrice lltv h_healthy' h_borrow
      (h_borrowed_pos rfl)
  · rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
    simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- WithdrawCollateral preserves collateralization: the `isHealthy` guard ensures
    that after decreasing collateral, the position still has sufficient backing.
    `h_borrowed_pos` says toAssetsUp gives a positive result for borrowShares
    (always true in practice due to virtual shares). -/
theorem withdrawCollateral_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver user : Address)
    (collateralPrice lltv : Uint256)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s')
    (h_borrowed_pos : user = onBehalf →
      (Libraries.SharesMathLib.toAssetsUp
        (s'.position id user).borrowShares
        (s'.market id).totalBorrowAssets
        (s'.market id).totalBorrowShares).val > 0) :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢
  intro h_borrow
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_healthy, h_eq⟩ := h_ok
  have h_healthy' : Morpho.isHealthy s' id onBehalf collateralPrice lltv = true := by
    rw [← h_eq]; exact h_healthy
  by_cases h : user = onBehalf
  · subst h
    exact isHealthy_collateral_pos s' id user collateralPrice lltv h_healthy' h_borrow
      (h_borrowed_pos rfl)
  · rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
    simp [h] at h_borrow ⊢; exact h_collat h_borrow

/-- AccrueInterest only modifies fee recipient's supplyShares and market totals;
    borrowShares and collateral of all positions are unchanged. -/
theorem accrueInterest_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (user : Address)
    (h_collat : alwaysCollateralized s id user) :
    alwaysCollateralized (Morpho.accrueInterest s id borrowRate hasIrm) id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  -- Unfold accrueInterest in both h_borrow and goal separately
  show ((Morpho.accrueInterest s id borrowRate hasIrm).position id user).collateral.val > 0
  have h_borrow' : ((Morpho.accrueInterest s id borrowRate hasIrm).position id user).borrowShares.val > 0 := h_borrow
  clear h_borrow
  -- Now both are about accrueInterest. accrueInterest only modifies feeRecipient's supplyShares.
  unfold Morpho.accrueInterest at h_borrow' ⊢
  simp at h_borrow' ⊢
  split at h_borrow'
  · -- elapsed = 0: state unchanged
    rename_i h_elapsed
    simp [h_elapsed]; exact h_collat h_borrow'
  · rename_i h_elapsed
    simp [h_elapsed]
    split at h_borrow'
    · -- ¬hasIrm: position unchanged
      rename_i h_irm
      simp [h_irm]; exact h_collat h_borrow'
    · -- hasIrm: fee recipient gets supplyShares, but borrowShares/collateral unchanged
      rename_i h_irm
      simp [h_irm]
      by_cases h : user = s.feeRecipient
      · subst h; simp at h_borrow' ⊢; exact h_collat h_borrow'
      · simp [h] at h_borrow' ⊢; exact h_collat h_borrow'

/-- enableIrm doesn't touch positions. -/
theorem enableIrm_preserves_alwaysCollateralized (s : MorphoState) (irm : Address) (id : Id)
    (user : Address) (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.enableIrm s irm = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.enableIrm at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right] at h_borrow ⊢; exact h_collat h_borrow

/-- enableLltv doesn't touch positions. -/
theorem enableLltv_preserves_alwaysCollateralized (s : MorphoState) (lltv : Uint256) (id : Id)
    (user : Address) (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.enableLltv s lltv = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.enableLltv at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right.right] at h_borrow ⊢; exact h_collat h_borrow

/-- setOwner doesn't touch positions. -/
theorem setOwner_preserves_alwaysCollateralized (s : MorphoState) (newOwner : Address) (id : Id)
    (user : Address) (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.setOwner at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right] at h_borrow ⊢; exact h_collat h_borrow

/-- setFee accrues interest (preserving positions for non-fee-recipients)
    then changes only the fee field. -/
theorem setFee_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (newFee borrowRate : Uint256) (hasIrm : Bool) (user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.setFee s id newFee borrowRate hasIrm = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; simp at h_borrow ⊢
  -- After accrueInterest + fee change, positions are same as after accrueInterest.
  have h_ai := accrueInterest_preserves_alwaysCollateralized s id borrowRate hasIrm user h_collat
  unfold alwaysCollateralized at h_ai
  exact h_ai h_borrow

/-- setFeeRecipient doesn't touch positions. -/
theorem setFeeRecipient_preserves_alwaysCollateralized (s : MorphoState)
    (newRecipient : Address) (id : Id) (user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.setFeeRecipient s newRecipient = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.setFeeRecipient at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right] at h_borrow ⊢; exact h_collat h_borrow

/-- createMarket doesn't touch positions. -/
theorem createMarket_preserves_alwaysCollateralized (s : MorphoState) (params : MarketParams)
    (marketId : Id) (id : Id) (user : Address) (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; exact h_collat h_borrow

/-- setAuthorization doesn't touch positions. -/
theorem setAuthorization_preserves_alwaysCollateralized (s : MorphoState)
    (authorized : Address) (newIsAuth : Bool) (id : Id) (user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.setAuthorization s authorized newIsAuth = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.setAuthorization at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok] at h_borrow ⊢; exact h_collat h_borrow

/-- setAuthorizationWithSig doesn't touch positions. -/
theorem setAuthorizationWithSig_preserves_alwaysCollateralized (s : MorphoState)
    (auth : Authorization) (sig : Bool) (id : Id) (user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    alwaysCollateralized s' id user := by
  unfold alwaysCollateralized at h_collat ⊢; intro h_borrow
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq] at h_borrow ⊢; exact h_collat h_borrow

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

/-! ## Flash loan safety

  Flash loans (`flashLoan`) return `Option Unit`, not `Option MorphoState` — they
  never modify protocol state. All invariants (solvency, collateralization, isolation,
  etc.) are trivially preserved because the state is unchanged. The only property
  to verify is the zero-assets guard. -/

/-- Flash loans reject zero assets. -/
theorem flashLoan_rejects_zero_assets (s : MorphoState) :
    Morpho.flashLoan s 0 = none := by
  unfold Morpho.flashLoan; simp

/-! ## Public accrueInterest

  `accrueInterestPublic` wraps `accrueInterest` with a `lastUpdate == 0` guard
  (uninitialized markets are rejected). All invariant proofs compose directly
  with the internal `accrueInterest` proofs. -/

/-- Public accrueInterest rejects uninitialized markets. -/
theorem accrueInterestPublic_rejects_uninitialized (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h_uninit : (s.market id).lastUpdate.val = 0) :
    Morpho.accrueInterestPublic s id borrowRate hasIrm = none := by
  unfold Morpho.accrueInterestPublic; simp [h_uninit]

/-- Public accrueInterest preserves solvency. -/
theorem accrueInterestPublic_preserves_borrowLeSupply (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (h_solvent : borrowLeSupply s id)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s')
    (h_no_overflow : (s.market id).totalSupplyAssets.val +
      (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Verity.Core.Uint256.modulus) :
    borrowLeSupply s' id := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  exact accrueInterest_preserves_borrowLeSupply s id borrowRate hasIrm h_solvent h_no_overflow

/-- Public accrueInterest preserves collateralization. -/
theorem accrueInterestPublic_preserves_alwaysCollateralized (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (user : Address)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s') :
    alwaysCollateralized s' id user := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  exact accrueInterest_preserves_alwaysCollateralized s id borrowRate hasIrm user h_collat

/-! ## Exchange rate monotonicity

  The "exchange rate" of a supply share is `(totalSupplyAssets + VA) / (totalSupplyShares + VS)`
  where VA = 1 (VIRTUAL_ASSETS) and VS = 10^6 (VIRTUAL_SHARES).

  Interest accrual must never decrease this ratio. We use cross-multiplication to avoid division:
  `(oldAssets + VA) * (newShares + VS) ≤ (newAssets + VA) * (oldShares + VS)`.

  Three cases:
  - elapsed = 0: state unchanged, trivially ≤.
  - ¬hasIrm: assets/shares unchanged, only lastUpdate changes, trivially ≤.
  - hasIrm, fee = 0: shares unchanged, assets increase by interest → ≤.
  - hasIrm, fee ≠ 0: `feeShares = toSharesDown(feeAmount, newAssets - feeAmount, oldShares)`.
    By `Nat.div_mul_le_self`, `feeShares * denom ≤ feeAmount * (oldShares + VS)`.
    Since `denom ≥ oldAssets + VA`, we get `(oldAssets + VA) * feeShares ≤ feeAmount * (S + VS)`.
    Since `feeAmount ≤ interest`, the result follows. -/

/-- Pure arithmetic: `(A + VA) * (S + F + VS) ≤ (A + I + VA) * (S + VS)`
    when `F * (A + I - feeAmt + VA) ≤ feeAmt * (S + VS)` and `feeAmt ≤ I`.
    This is the core exchange rate inequality, stated on natural numbers. -/
private theorem exchange_rate_ineq (A S I F feeAmt VA VS : Nat)
    (h_floor : F * (A + I - feeAmt + VA) ≤ feeAmt * (S + VS))
    (h_fee_le : feeAmt ≤ I)
    (h_sub_ok : feeAmt ≤ A + I) :
    (A + VA) * (S + F + VS) ≤ (A + I + VA) * (S + VS) := by
  -- Suffices: (A+VA)*F ≤ I*(S+VS), then the rest is algebra
  suffices h : (A + VA) * F ≤ I * (S + VS) by
    -- (A+VA)*(S+F+VS) = (A+VA)*(S+VS) + (A+VA)*F
    -- (A+I+VA)*(S+VS) = (A+VA)*(S+VS) + I*(S+VS)
    have h1 : (A + VA) * (S + F + VS) = (A + VA) * (S + VS) + (A + VA) * F := by
      have : S + F + VS = (S + VS) + F := by omega
      rw [this, Nat.mul_add]
    have h2 : (A + I + VA) * (S + VS) = (A + VA) * (S + VS) + I * (S + VS) := by
      have : A + I + VA = (A + VA) + I := by omega
      rw [this, Nat.add_mul]
    rw [h1, h2]
    exact Nat.add_le_add_left h _
  -- Prove (A+VA)*F ≤ I*(S+VS) via the chain:
  -- (A+VA)*F ≤ F*(A+I-feeAmt+VA) ≤ feeAmt*(S+VS) ≤ I*(S+VS)
  have h_denom_ge : A + VA ≤ A + I - feeAmt + VA := by omega
  calc (A + VA) * F
      = F * (A + VA) := Nat.mul_comm _ _
    _ ≤ F * (A + I - feeAmt + VA) := Nat.mul_le_mul_left F h_denom_ge
    _ ≤ feeAmt * (S + VS) := h_floor
    _ ≤ I * (S + VS) := Nat.mul_le_mul_right _ h_fee_le

/-- Interest accrual preserves supply exchange rate monotonicity.
    After `accrueInterest`, existing shareholders' per-share value never decreases. -/
theorem accrueInterest_preserves_supplyExchangeRateMonotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    -- No overflow for totalSupplyAssets + interest
    (h_supply_no_overflow : (s.market id).totalSupplyAssets.val +
      (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Core.Uint256.modulus)
    -- No overflow for totalSupplyShares + feeShares
    (h_shares_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      let newSupplyAssets := Morpho.u256 ((s.market id).totalSupplyAssets.val + interest.val)
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (newSupplyAssets.val - feeAmount.val)) (s.market id).totalSupplyShares
      (s.market id).totalSupplyShares.val + feeShares.val < Core.Uint256.modulus)
    -- feeAmount ≤ interest (follows from fee < WAD)
    (h_fee_le_interest :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      (Libraries.MathLib.wMulDown interest (s.market id).fee).val ≤ interest.val)
    -- totalSupplyShares + VIRTUAL_SHARES doesn't overflow
    (h_shares_vs_no_overflow :
      (s.market id).totalSupplyShares.val + Libraries.SharesMathLib.VIRTUAL_SHARES
        < Core.Uint256.modulus)
    -- toSharesDown denominator doesn't overflow
    (h_denom_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      (s.market id).totalSupplyAssets.val + interest.val - feeAmount.val +
        Libraries.SharesMathLib.VIRTUAL_ASSETS < Core.Uint256.modulus) :
    supplyExchangeRateMonotone s (Morpho.accrueInterest s id borrowRate hasIrm) id := by
  unfold supplyExchangeRateMonotone
  unfold Morpho.accrueInterest
  simp
  split
  · -- elapsed = 0: state unchanged
    exact Nat.le_refl _
  · split
    · -- ¬hasIrm: only lastUpdate changes, totals unchanged
      simp
    · -- hasIrm: interest accrues, optional fee shares minted
      simp  -- reduces if id=id, struct projections etc.
      split -- split on if fee.val = 0 (isTrue = fee=0, isFalse = fee≠0)
      · -- fee = 0: no new shares, shares unchanged, assets increase by interest
        -- Normalize u256 everywhere to expose % modulus, then eliminate
        simp only [Morpho.u256] at h_supply_no_overflow
        simp only [Morpho.u256, Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h_supply_no_overflow]
        -- Now: (A + VA) * (S + VS) ≤ (A + I + VA) * (S + VS)
        apply Nat.mul_le_mul_right
        omega
      · -- fee ≠ 0: feeShares minted
        -- Step 1: Normalize u256 → Core.Uint256.ofNat in all hypotheses
        simp only [Morpho.u256] at h_supply_no_overflow h_shares_no_overflow h_denom_no_overflow
        -- Step 2: Eliminate supply (A + I) % modulus in h_shares_no_overflow
        simp only [Core.Uint256.val_ofNat, Nat.mod_eq_of_lt h_supply_no_overflow]
          at h_shares_no_overflow
        -- Step 3: Apply same simplifications to goal
        simp only [Morpho.u256, Core.Uint256.val_ofNat,
          Nat.mod_eq_of_lt h_supply_no_overflow,
          Nat.mod_eq_of_lt h_shares_no_overflow]
        apply exchange_rate_ineq
        · -- h_floor: feeShares * denom ≤ feeAmount * (S + VS)
          unfold Libraries.SharesMathLib.toSharesDown Libraries.MathLib.mulDivDown
          simp only [Core.Uint256.val_ofNat]
          -- Eliminate (S + VS) % modulus using h_shares_vs_no_overflow
          rw [Nat.mod_eq_of_lt h_shares_vs_no_overflow]
          -- Eliminate inner (A + I - feeAmt) % modulus
          have h_sub_lt : (s.market id).totalSupplyAssets.val +
              (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
                (Libraries.MathLib.wTaylorCompounded borrowRate
                  (Core.Uint256.ofNat (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val -
              (Libraries.MathLib.wMulDown
                (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
                  (Libraries.MathLib.wTaylorCompounded borrowRate
                    (Core.Uint256.ofNat (s.blockTimestamp.val - (s.market id).lastUpdate.val))))
                (s.market id).fee).val < Core.Uint256.modulus :=
            Nat.lt_of_le_of_lt (Nat.sub_le _ _) h_supply_no_overflow
          rw [Nat.mod_eq_of_lt h_sub_lt]
          -- Eliminate outer (A + I - feeAmt + VA) % modulus using h_denom_no_overflow
          rw [Nat.mod_eq_of_lt h_denom_no_overflow]
          -- Now: ((feeAmt * (S + VS)) / (A+I-feeAmt+VA)) % modulus * (A+I-feeAmt+VA) ≤ feeAmt * (S+VS)
          -- Use Nat.mod_le to bound the % modulus, then Nat.div_mul_le_self
          exact Nat.le_trans
            (Nat.mul_le_mul_right _
              (Nat.mod_le _ _))
            (Nat.div_mul_le_self _ _)
        · exact h_fee_le_interest
        · exact Nat.le_trans h_fee_le_interest (Nat.le_add_left _ _)


/-- Public accrueInterest preserves exchange rate monotonicity. -/
theorem accrueInterestPublic_preserves_supplyExchangeRateMonotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s')
    (h_supply_no_overflow : (s.market id).totalSupplyAssets.val +
      (Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Core.Uint256.modulus)
    (h_shares_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      let newSupplyAssets := Morpho.u256 ((s.market id).totalSupplyAssets.val + interest.val)
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (newSupplyAssets.val - feeAmount.val)) (s.market id).totalSupplyShares
      (s.market id).totalSupplyShares.val + feeShares.val < Core.Uint256.modulus)
    (h_fee_le_interest :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      (Libraries.MathLib.wMulDown interest (s.market id).fee).val ≤ interest.val)
    (h_shares_vs_no_overflow :
      (s.market id).totalSupplyShares.val + Libraries.SharesMathLib.VIRTUAL_SHARES
        < Core.Uint256.modulus)
    (h_denom_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      (s.market id).totalSupplyAssets.val + interest.val - feeAmount.val +
        Libraries.SharesMathLib.VIRTUAL_ASSETS < Core.Uint256.modulus) :
    supplyExchangeRateMonotone s s' id := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  exact accrueInterest_preserves_supplyExchangeRateMonotone s id borrowRate hasIrm
    h_supply_no_overflow h_shares_no_overflow h_fee_le_interest
    h_shares_vs_no_overflow h_denom_no_overflow

/-! ## Comprehensive monotonicity preservation

  IRM and LLTV monotonicity: once enabled, they stay enabled across ALL operations.
  `enableIrm_monotone` and `enableLltv_monotone` handle the non-trivial cases above.
  The remaining operations don't touch `isIrmEnabled` or `isLltvEnabled` at all,
  so preservation is trivial: the new state's `isIrmEnabled`/`isLltvEnabled` is
  definitionally equal to the old one after extracting the state equality.

  LastUpdate monotonicity: market timestamps never decrease. `accrueInterest`
  sets `lastUpdate := blockTimestamp` (which is ≥ old lastUpdate since elapsed ≥ 0).
  All other operations either don't touch lastUpdate or go through accrueInterest. -/

-- IRM monotonicity for operations that don't touch isIrmEnabled

theorem supply_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem withdraw_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem borrow_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem repay_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem supplyCollateral_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf : Address) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem withdrawCollateral_preserves_irmMonotone (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem liquidate_preserves_irmMonotone (s : MorphoState) (id : Id)
    (borrower : Address) (seizedAssets repaidShares : Uint256)
    (collateralPrice lltv : Uint256) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    s'.isIrmEnabled irm := by
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (rw [← h_ok.2.2.2.2.2.2]; exact h_enabled)

theorem setOwner_preserves_irmMonotone (s : MorphoState) (newOwner : Address) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.setOwner at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right]; exact h_enabled

theorem setFee_preserves_irmMonotone (s : MorphoState) (id : Id) (newFee borrowRate : Uint256)
    (hasIrm : Bool) (irm : Address) (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.setFee s id newFee borrowRate hasIrm = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp; unfold Morpho.accrueInterest; simp
  split
  · exact h_enabled
  · split <;> exact h_enabled

theorem setFeeRecipient_preserves_irmMonotone (s : MorphoState) (newRecipient : Address)
    (irm : Address) (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.setFeeRecipient s newRecipient = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.setFeeRecipient at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right]; exact h_enabled

theorem createMarket_preserves_irmMonotone (s : MorphoState) (params : MarketParams)
    (marketId : Id) (irm : Address) (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem setAuthorization_preserves_irmMonotone (s : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) (irm : Address) (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.setAuthorization s authorized newIsAuthorized = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.setAuthorization at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok]; exact h_enabled

theorem setAuthorizationWithSig_preserves_irmMonotone (s : MorphoState)
    (auth : Authorization) (sig : Bool) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem accrueInterestPublic_preserves_irmMonotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (irm : Address)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s') :
    s'.isIrmEnabled irm := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]; unfold Morpho.accrueInterest; simp
  split
  · exact h_enabled
  · split <;> exact h_enabled

-- LLTV monotonicity for operations that don't touch isLltvEnabled

theorem supply_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.supply s id assets shares onBehalf = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem withdraw_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem borrow_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltvParam
      = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem repay_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.repay s id assets shares onBehalf = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem supplyCollateral_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf : Address) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.supplyCollateral s id assets onBehalf = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem withdrawCollateral_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltvParam
      = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem liquidate_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (borrower : Address) (seizedAssets repaidShares : Uint256)
    (collateralPrice lltvParam lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltvParam
      = some (seized, repaid, s')) :
    s'.isLltvEnabled lltv := by
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (rw [← h_ok.2.2.2.2.2.2]; exact h_enabled)

theorem setOwner_preserves_lltvMonotone (s : MorphoState) (newOwner : Address)
    (lltv : Uint256) (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.setOwner at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right]; exact h_enabled

theorem setFee_preserves_lltvMonotone (s : MorphoState) (id : Id) (newFee borrowRate : Uint256)
    (hasIrm : Bool) (lltv : Uint256) (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.setFee s id newFee borrowRate hasIrm = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp; unfold Morpho.accrueInterest; simp
  split
  · exact h_enabled
  · split <;> exact h_enabled

theorem setFeeRecipient_preserves_lltvMonotone (s : MorphoState) (newRecipient : Address)
    (lltv : Uint256) (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.setFeeRecipient s newRecipient = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.setFeeRecipient at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok.right]; exact h_enabled

theorem createMarket_preserves_lltvMonotone (s : MorphoState) (params : MarketParams)
    (marketId : Id) (lltv : Uint256) (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem setAuthorization_preserves_lltvMonotone (s : MorphoState) (authorized : Address)
    (newIsAuthorized : Bool) (lltv : Uint256) (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.setAuthorization s authorized newIsAuthorized = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.setAuthorization at h_ok; split at h_ok <;> simp at h_ok
  rw [← h_ok]; exact h_enabled

theorem setAuthorizationWithSig_preserves_lltvMonotone (s : MorphoState)
    (auth : Authorization) (sig : Bool) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_enabled

theorem accrueInterestPublic_preserves_lltvMonotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool) (lltv : Uint256)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s') :
    s'.isLltvEnabled lltv := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]; unfold Morpho.accrueInterest; simp
  split
  · exact h_enabled
  · split <;> exact h_enabled

-- LastUpdate monotonicity: operations that go through accrueInterest

/-- setFee accrues interest first, which updates lastUpdate to blockTimestamp. -/
theorem setFee_lastUpdate_monotone (s : MorphoState) (id : Id) (newFee borrowRate : Uint256)
    (hasIrm : Bool)
    (h_ok : Morpho.setFee s id newFee borrowRate hasIrm = some s')
    (h_elapsed : s.blockTimestamp.val ≥ (s.market id).lastUpdate.val) :
    lastUpdateMonotone s s' id := by
  unfold lastUpdateMonotone
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; simp
  have h_ai := accrueInterest_lastUpdate_monotone s id borrowRate hasIrm h_elapsed
  unfold lastUpdateMonotone at h_ai
  exact h_ai

/-- accrueInterestPublic accrues interest, which updates lastUpdate to blockTimestamp. -/
theorem accrueInterestPublic_lastUpdate_monotone (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (h_ok : Morpho.accrueInterestPublic s id borrowRate hasIrm = some s')
    (h_elapsed : s.blockTimestamp.val ≥ (s.market id).lastUpdate.val) :
    lastUpdateMonotone s s' id := by
  unfold lastUpdateMonotone
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  have h_ai := accrueInterest_lastUpdate_monotone s id borrowRate hasIrm h_elapsed
  unfold lastUpdateMonotone at h_ai
  exact h_ai

end Morpho.Proofs.Invariants
