/-
  Proofs of share accounting consistency.

  Each theorem shows that if `totalSupplyShares = Σ supplyShares` (resp. borrow)
  before an operation, it still holds after. The `allUsers` list is universally
  quantified — no changes to Types.lean or Morpho.lean.
-/
import Morpho.Morpho
import Morpho.Specs.Invariants
import Morpho.Proofs.NatListSum

namespace Morpho.Proofs.ShareConsistency

open Verity
open Morpho.Types
open Morpho.Specs.Invariants

/-! ## Helpers -/

private theorem supplyConsistent_of_eq_market_position {s s' : MorphoState}
    {id : Id} {allUsers : List Address}
    (h_market : (s'.market id).totalSupplyShares = (s.market id).totalSupplyShares)
    (h_pos : ∀ u, (s'.position id u).supplyShares = (s.position id u).supplyShares)
    (h_consistent : supplySharesConsistent s id allUsers) :
    supplySharesConsistent s' id allUsers := by
  unfold supplySharesConsistent at *
  have h_map : allUsers.map (fun u => (s'.position id u).supplyShares.val) =
    allUsers.map (fun u => (s.position id u).supplyShares.val) :=
    List.map_congr_left (fun u _ => congrArg Core.Uint256.val (h_pos u))
  rw [congrArg Core.Uint256.val h_market, h_map]; exact h_consistent

private theorem borrowConsistent_of_eq_market_position {s s' : MorphoState}
    {id : Id} {allUsers : List Address}
    (h_market : (s'.market id).totalBorrowShares = (s.market id).totalBorrowShares)
    (h_pos : ∀ u, (s'.position id u).borrowShares = (s.position id u).borrowShares)
    (h_consistent : borrowSharesConsistent s id allUsers) :
    borrowSharesConsistent s' id allUsers := by
  unfold borrowSharesConsistent at *
  have h_map : allUsers.map (fun u => (s'.position id u).borrowShares.val) =
    allUsers.map (fun u => (s.position id u).borrowShares.val) :=
    List.map_congr_left (fun u _ => congrArg Core.Uint256.val (h_pos u))
  rw [congrArg Core.Uint256.val h_market, h_map]; exact h_consistent

/-! ## Trivial operations — don't touch any shares at all -/

theorem enableIrm_preserves_supplySharesConsistent (s : MorphoState) (irm : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.enableIrm s irm = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.enableIrm at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem enableIrm_preserves_borrowSharesConsistent (s : MorphoState) (irm : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.enableIrm s irm = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.enableIrm at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem enableLltv_preserves_supplySharesConsistent (s : MorphoState) (lltv : Uint256)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.enableLltv s lltv = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.enableLltv at h_ok; simp at h_ok
  rw [← h_ok.right.right.right]; exact h_consistent

theorem enableLltv_preserves_borrowSharesConsistent (s : MorphoState) (lltv : Uint256)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.enableLltv s lltv = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.enableLltv at h_ok; simp at h_ok
  rw [← h_ok.right.right.right]; exact h_consistent

theorem setOwner_preserves_supplySharesConsistent (s : MorphoState) (newOwner : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.setOwner at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem setOwner_preserves_borrowSharesConsistent (s : MorphoState) (newOwner : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.setOwner s newOwner = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.setOwner at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem setFeeRecipient_preserves_supplySharesConsistent (s : MorphoState) (addr : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.setFeeRecipient s addr = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.setFeeRecipient at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem setFeeRecipient_preserves_borrowSharesConsistent (s : MorphoState) (addr : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.setFeeRecipient s addr = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.setFeeRecipient at h_ok; simp at h_ok
  rw [← h_ok.right.right]; exact h_consistent

theorem createMarket_preserves_supplySharesConsistent (s : MorphoState)
    (params : MarketParams) (marketId : Id) (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply supplyConsistent_of_eq_market_position _ _ h_consistent
  · simp; split <;> simp_all
  · intro u; rfl

theorem createMarket_preserves_borrowSharesConsistent (s : MorphoState)
    (params : MarketParams) (marketId : Id) (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.createMarket s params marketId = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.createMarket at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply borrowConsistent_of_eq_market_position _ _ h_consistent
  · simp; split <;> simp_all
  · intro u; rfl

theorem setAuthorization_preserves_supplySharesConsistent (s : MorphoState)
    (authorized : Address) (newIsAuth : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.setAuthorization s authorized newIsAuth = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.setAuthorization at h_ok; simp at h_ok
  rw [← h_ok.right]; exact h_consistent

theorem setAuthorization_preserves_borrowSharesConsistent (s : MorphoState)
    (authorized : Address) (newIsAuth : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.setAuthorization s authorized newIsAuth = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.setAuthorization at h_ok; simp at h_ok
  rw [← h_ok.right]; exact h_consistent

theorem setAuthorizationWithSig_preserves_supplySharesConsistent (s : MorphoState)
    (auth : Authorization) (sig : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_consistent

theorem setAuthorizationWithSig_preserves_borrowSharesConsistent (s : MorphoState)
    (auth : Authorization) (sig : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.setAuthorizationWithSig at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]; exact h_consistent

theorem supplyCollateral_preserves_supplySharesConsistent (s : MorphoState)
    (scId : Id) (assets : Uint256) (onBehalf : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.supplyCollateral s scId assets onBehalf = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply supplyConsistent_of_eq_market_position _ _ h_consistent
  · rfl
  · intro u; simp; split
    · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
    · rfl

theorem supplyCollateral_preserves_borrowSharesConsistent (s : MorphoState)
    (scId : Id) (assets : Uint256) (onBehalf : Address)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.supplyCollateral s scId assets onBehalf = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.supplyCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply borrowConsistent_of_eq_market_position _ _ h_consistent
  · rfl
  · intro u; simp; split
    · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
    · rfl

theorem withdrawCollateral_preserves_supplySharesConsistent (s : MorphoState)
    (wcId : Id) (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (id : Id) (allUsers : List Address)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_ok : Morpho.withdrawCollateral s wcId assets onBehalf receiver collateralPrice lltv = some s') :
    supplySharesConsistent s' id allUsers := by
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]
  apply supplyConsistent_of_eq_market_position _ _ h_consistent
  · rfl
  · intro u; simp; split
    · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
    · rfl

theorem withdrawCollateral_preserves_borrowSharesConsistent (s : MorphoState)
    (wcId : Id) (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.withdrawCollateral s wcId assets onBehalf receiver collateralPrice lltv = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.withdrawCollateral at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, ⟨_, _, h_eq⟩⟩ := h_ok
  rw [← h_eq]
  apply borrowConsistent_of_eq_market_position _ _ h_consistent
  · rfl
  · intro u; simp; split
    · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
    · rfl

/-! ## Supply / Withdraw — modify supplyShares only -/

theorem supply_preserves_supplySharesConsistent (s : MorphoState) (sid : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem : onBehalf ∈ allUsers)
    (h_consistent : supplySharesConsistent s sid allUsers)
    (h_ok : Morpho.supply s sid assets shares onBehalf = some (a, sh, s'))
    (h_no_overflow : (s.market sid).totalSupplyShares.val + sh.val < Core.Uint256.modulus)
    (h_pos_no_overflow : (s.position sid onBehalf).supplyShares.val + sh.val < Core.Uint256.modulus) :
    supplySharesConsistent s' sid allUsers := by
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_sh, h_eq⟩ := h_ok
  subst h_sh; rw [← h_eq]; unfold supplySharesConsistent
  by_cases h_assets : 0 < assets.val <;>
    simp only [h_assets, ite_true, ite_false] at h_no_overflow h_pos_no_overflow ⊢ <;> (
    simp only [true_and, apply_ite, Morpho.u256_val,
      Nat.mod_eq_of_lt h_no_overflow, Nat.mod_eq_of_lt h_pos_no_overflow]
    rw [list_sum_map_add allUsers (fun u => (s.position sid u).supplyShares.val) onBehalf _ h_mem h_nodup]
    unfold supplySharesConsistent at h_consistent; omega)

theorem supply_preserves_borrowSharesConsistent (s : MorphoState) (sid : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s sid allUsers)
    (h_ok : Morpho.supply s sid assets shares onBehalf = some (a, sh, s')) :
    borrowSharesConsistent s' sid allUsers := by
  unfold Morpho.supply at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply borrowConsistent_of_eq_market_position _ _ h_consistent
  · simp
  · intro u; simp [true_and]; split
    · subst ‹u = onBehalf›; rfl
    · rfl

theorem withdraw_preserves_supplySharesConsistent (s : MorphoState) (wid : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem : onBehalf ∈ allUsers)
    (h_consistent : supplySharesConsistent s wid allUsers)
    (h_ok : Morpho.withdraw s wid assets shares onBehalf receiver = some (a, sh, s')) :
    supplySharesConsistent s' wid allUsers := by
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, h_sh, h_eq⟩ := h_ok
  have h_pos_no_overflow :
      (s.position wid onBehalf).supplyShares.val - sh.val < Core.Uint256.modulus :=
    Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.position wid onBehalf).supplyShares.isLt
  have h_total_no_overflow :
      (s.market wid).totalSupplyShares.val - sh.val < Core.Uint256.modulus :=
    Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.market wid).totalSupplyShares.isLt
  subst h_sh; rw [← h_eq]; unfold supplySharesConsistent
  by_cases h_assets : 0 < assets.val <;> simp only [h_assets, ite_true, ite_false] at * <;> (
    simp only [true_and, apply_ite, Morpho.u256_val,
      Nat.mod_eq_of_lt h_total_no_overflow, Nat.mod_eq_of_lt h_pos_no_overflow]
    rw [list_sum_map_sub allUsers (fun u => (s.position wid u).supplyShares.val) onBehalf _ h_mem h_nodup (by dsimp; omega)]
    unfold supplySharesConsistent at h_consistent; omega)

theorem withdraw_preserves_borrowSharesConsistent (s : MorphoState) (wid : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s wid allUsers)
    (h_ok : Morpho.withdraw s wid assets shares onBehalf receiver = some (a, sh, s')) :
    borrowSharesConsistent s' wid allUsers := by
  unfold Morpho.withdraw at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply borrowConsistent_of_eq_market_position _ _ h_consistent
  · simp
  · intro u; simp [true_and]; split
    · subst ‹u = onBehalf›; rfl
    · rfl

/-! ## Borrow / Repay — modify borrowShares only -/

theorem borrow_preserves_borrowSharesConsistent (s : MorphoState) (bid : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem : onBehalf ∈ allUsers)
    (h_consistent : borrowSharesConsistent s bid allUsers)
    (h_ok : Morpho.borrow s bid assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s'))
    (h_no_overflow : (s.market bid).totalBorrowShares.val + sh.val < Core.Uint256.modulus)
    (h_pos_no_overflow : (s.position bid onBehalf).borrowShares.val + sh.val < Core.Uint256.modulus) :
    borrowSharesConsistent s' bid allUsers := by
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, h_sh, h_eq⟩ := h_ok
  subst h_sh; rw [← h_eq]; unfold borrowSharesConsistent
  by_cases h_assets : 0 < assets.val <;>
    simp only [h_assets, ite_true, ite_false] at h_no_overflow h_pos_no_overflow ⊢ <;> (
    simp only [true_and, apply_ite, Morpho.u256_val,
      Nat.mod_eq_of_lt h_no_overflow, Nat.mod_eq_of_lt h_pos_no_overflow]
    rw [list_sum_map_add allUsers (fun u => (s.position bid u).borrowShares.val) onBehalf _ h_mem h_nodup]
    unfold borrowSharesConsistent at h_consistent; omega)

theorem borrow_preserves_supplySharesConsistent (s : MorphoState) (bid : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (allUsers : List Address)
    (h_consistent : supplySharesConsistent s bid allUsers)
    (h_ok : Morpho.borrow s bid assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    supplySharesConsistent s' bid allUsers := by
  unfold Morpho.borrow at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply supplyConsistent_of_eq_market_position _ _ h_consistent
  · simp
  · intro u; simp [true_and]; split
    · subst ‹u = onBehalf›; rfl
    · rfl

theorem repay_preserves_borrowSharesConsistent (s : MorphoState) (rid : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem : onBehalf ∈ allUsers)
    (h_consistent : borrowSharesConsistent s rid allUsers)
    (h_ok : Morpho.repay s rid assets shares onBehalf = some (a, sh, s')) :
    borrowSharesConsistent s' rid allUsers := by
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, h_sh, h_eq⟩ := h_ok
  have h_pos_no_overflow :
      (s.position rid onBehalf).borrowShares.val - sh.val < Core.Uint256.modulus :=
    Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.position rid onBehalf).borrowShares.isLt
  have h_total_no_overflow :
      (s.market rid).totalBorrowShares.val - sh.val < Core.Uint256.modulus :=
    Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.market rid).totalBorrowShares.isLt
  subst h_sh; rw [← h_eq]; unfold borrowSharesConsistent
  by_cases h_assets : 0 < assets.val <;> simp only [h_assets, ite_true, ite_false] at * <;> (
    simp only [true_and, apply_ite, Morpho.u256_val,
      Nat.mod_eq_of_lt h_total_no_overflow, Nat.mod_eq_of_lt h_pos_no_overflow]
    rw [list_sum_map_sub allUsers (fun u => (s.position rid u).borrowShares.val) onBehalf _ h_mem h_nodup (by dsimp; omega)]
    unfold borrowSharesConsistent at h_consistent; omega)

theorem repay_preserves_supplySharesConsistent (s : MorphoState) (rid : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (allUsers : List Address)
    (h_consistent : supplySharesConsistent s rid allUsers)
    (h_ok : Morpho.repay s rid assets shares onBehalf = some (a, sh, s')) :
    supplySharesConsistent s' rid allUsers := by
  unfold Morpho.repay at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  apply supplyConsistent_of_eq_market_position _ _ h_consistent
  · simp
  · intro u; simp [true_and]; split
    · subst ‹u = onBehalf›; rfl
    · rfl

/-! ## Liquidate -/

theorem liquidate_preserves_supplySharesConsistent (s : MorphoState) (lid : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (allUsers : List Address)
    (h_consistent : supplySharesConsistent s lid allUsers)
    (h_ok : Morpho.liquidate s lid borrower seizedAssets repaidShares collateralPrice lltv
      = some (sa, ra, s')) :
    supplySharesConsistent s' lid allUsers := by
  unfold Morpho.liquidate at h_ok; simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (
    try (split at h_ok <;> simp at h_ok)
    all_goals (
      try (split at h_ok <;> simp at h_ok)
      all_goals (
        obtain ⟨_, _, _, _, _, _, _, h_eq⟩ := h_ok
        rw [← h_eq]
        apply supplyConsistent_of_eq_market_position _ _ h_consistent
        · simp
        · intro u; simp [true_and]; split
          · subst ‹u = borrower›; rfl
          · rfl)))

theorem liquidate_preserves_borrowSharesConsistent (s : MorphoState) (lid : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem : borrower ∈ allUsers)
    (h_consistent : borrowSharesConsistent s lid allUsers)
    (h_ok : Morpho.liquidate s lid borrower seizedAssets repaidShares collateralPrice lltv
      = some (sa, ra, s')) :
    borrowSharesConsistent s' lid allUsers := by
  unfold Morpho.liquidate at h_ok; simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals (
    try (split at h_ok <;> simp at h_ok)
    all_goals (
      try (split at h_ok <;> simp at h_ok)
      all_goals (
        obtain ⟨_, _, _, _, _, _, _, h_eq⟩ := h_ok
        rw [← h_eq]
        unfold borrowSharesConsistent
        simp only [ite_true, true_and, apply_ite, Morpho.u256_val, Nat.zero_mod]
        have elim_mod_pos (n : Nat) :
            (s.position lid borrower).borrowShares.val - n < Core.Uint256.modulus :=
          Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.position lid borrower).borrowShares.isLt
        have elim_mod_total (n : Nat) :
            (s.market lid).totalBorrowShares.val - n < Core.Uint256.modulus :=
          Nat.lt_of_le_of_lt (Nat.sub_le _ _) (s.market lid).totalBorrowShares.isLt
        simp only [Nat.mod_eq_of_lt (elim_mod_pos _), Nat.mod_eq_of_lt (elim_mod_total _)]
        try simp only [Nat.mod_eq_of_lt (Nat.lt_of_le_of_lt (Nat.sub_le _ _) (elim_mod_total _))]
        first
        | (rw [list_sum_map_sub allUsers (fun u => (s.position lid u).borrowShares.val) borrower _
             h_mem h_nodup (by dsimp; omega)]
           unfold borrowSharesConsistent at h_consistent; omega)
        | (rw [list_sum_map_zero allUsers (fun u => (s.position lid u).borrowShares.val) borrower
             h_mem h_nodup]
           unfold borrowSharesConsistent at h_consistent; omega))))

/-! ## accrueInterest -/

theorem accrueInterest_preserves_borrowSharesConsistent (s : MorphoState) (aid : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers) :
    borrowSharesConsistent (Morpho.accrueInterest s aid borrowRate hasIrm) id allUsers := by
  unfold Morpho.accrueInterest
  simp only []; split
  · exact h_consistent  -- elapsed = 0
  · split
    · -- ¬hasIrm: only market.lastUpdate changes
      apply borrowConsistent_of_eq_market_position _ _ h_consistent
      · simp [beq_iff_eq]; split
        · next h => rw [h]
        · rfl
      · intro u; rfl
    · -- hasIrm
      split
      · -- fee ≠ 0
        apply borrowConsistent_of_eq_market_position _ _ h_consistent
        · simp [beq_iff_eq]; split
          · next h => rw [h]
          · rfl
        · intro u; simp [beq_iff_eq]; split
          · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
          · rfl
      · -- fee = 0
        apply borrowConsistent_of_eq_market_position _ _ h_consistent
        · simp [beq_iff_eq]; split
          · next h => rw [h]
          · rfl
        · intro u; simp [beq_iff_eq]; split
          · obtain ⟨rfl, rfl⟩ := ‹_›; rfl
          · rfl

theorem accrueInterest_preserves_supplySharesConsistent (s : MorphoState) (id : Id)
    (borrowRate : Uint256) (hasIrm : Bool)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem_fee : s.feeRecipient ∈ allUsers)
    (h_consistent : supplySharesConsistent s id allUsers)
    (h_pos_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market id).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market id).totalSupplyShares
      (s.position id s.feeRecipient).supplyShares.val + feeShares.val < Core.Uint256.modulus)
    (h_total_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market id).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market id).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market id).totalSupplyShares
      (s.market id).totalSupplyShares.val + feeShares.val < Core.Uint256.modulus) :
    supplySharesConsistent (Morpho.accrueInterest s id borrowRate hasIrm) id allUsers := by
  unfold Morpho.accrueInterest
  simp only []; split
  · exact h_consistent  -- elapsed = 0
  · split
    · -- ¬hasIrm: only lastUpdate changes
      apply supplyConsistent_of_eq_market_position _ _ h_consistent
      · simp
      · intro u; rfl
    · -- hasIrm: feeShares added to both totalSupplyShares and feeRecipient.supplyShares
      -- Both fee ≠ 0 and fee = 0 branches use the same +feeShares pattern
      unfold supplySharesConsistent
      dsimp only [] at h_pos_no_overflow h_total_no_overflow
      split
      · -- fee ≠ 0
        simp only [beq_self_eq_true, Bool.true_and, beq_iff_eq, ite_true,
          apply_ite, Morpho.u256_val,
          Nat.mod_eq_of_lt h_total_no_overflow, Nat.mod_eq_of_lt h_pos_no_overflow]
        rw [list_sum_map_add allUsers (fun u => (s.position id u).supplyShares.val) s.feeRecipient _ h_mem_fee h_nodup]
        unfold supplySharesConsistent at h_consistent; omega
      · -- fee = 0: feeShares.val = 0 so positions and total effectively unchanged
        simp only [beq_self_eq_true, Bool.true_and, beq_iff_eq, ite_true,
          apply_ite, Morpho.u256_val, Nat.zero_mod, Nat.add_zero]
        -- The map still has (pos.val % modulus) for feeRecipient; show it equals (pos.val)
        have h_map_eq : allUsers.map (fun u =>
            if u = s.feeRecipient then (s.position id s.feeRecipient).supplyShares.val % Core.Uint256.modulus
            else (s.position id u).supplyShares.val) =
          allUsers.map (fun u => (s.position id u).supplyShares.val) :=
          List.map_congr_left (fun u _ => by
            split
            · subst ‹u = s.feeRecipient›; exact Nat.mod_eq_of_lt (s.position id s.feeRecipient).supplyShares.isLt
            · rfl)
        rw [h_map_eq]; unfold supplySharesConsistent at h_consistent; exact h_consistent

/-! ## setFee / accrueInterestPublic -/

theorem setFee_preserves_borrowSharesConsistent (s : MorphoState) (fid : Id)
    (newFee borrowRate : Uint256) (hasIrm : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.setFee s fid newFee borrowRate hasIrm = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  have h_ai := accrueInterest_preserves_borrowSharesConsistent s fid borrowRate hasIrm id allUsers
    h_consistent
  apply borrowConsistent_of_eq_market_position _ _ h_ai
  · simp; split
    · next h => rw [h]
    · rfl
  · intro u; rfl

theorem setFee_preserves_supplySharesConsistent (s : MorphoState) (fid : Id)
    (newFee borrowRate : Uint256) (hasIrm : Bool)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem_fee : s.feeRecipient ∈ allUsers)
    (h_consistent : supplySharesConsistent s fid allUsers)
    (h_ok : Morpho.setFee s fid newFee borrowRate hasIrm = some s')
    (h_pos_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market fid).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market fid).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market fid).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market fid).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market fid).totalSupplyShares
      (s.position fid s.feeRecipient).supplyShares.val + feeShares.val < Core.Uint256.modulus)
    (h_total_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market fid).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market fid).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market fid).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market fid).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market fid).totalSupplyShares
      (s.market fid).totalSupplyShares.val + feeShares.val < Core.Uint256.modulus) :
    supplySharesConsistent s' fid allUsers := by
  unfold Morpho.setFee at h_ok; simp at h_ok
  obtain ⟨_, _, _, _, h_eq⟩ := h_ok
  rw [← h_eq]
  have h_ai := accrueInterest_preserves_supplySharesConsistent s fid borrowRate hasIrm allUsers
    h_nodup h_mem_fee h_consistent h_pos_no_overflow h_total_no_overflow
  apply supplyConsistent_of_eq_market_position _ _ h_ai
  · simp
  · intro u; rfl

theorem accrueInterestPublic_preserves_supplySharesConsistent (s : MorphoState)
    (aid : Id) (borrowRate : Uint256) (hasIrm : Bool)
    (allUsers : List Address)
    (h_nodup : allUsers.Nodup) (h_mem_fee : s.feeRecipient ∈ allUsers)
    (h_consistent : supplySharesConsistent s aid allUsers)
    (h_ok : Morpho.accrueInterestPublic s aid borrowRate hasIrm = some s')
    (h_pos_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market aid).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market aid).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market aid).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market aid).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market aid).totalSupplyShares
      (s.position aid s.feeRecipient).supplyShares.val + feeShares.val < Core.Uint256.modulus)
    (h_total_no_overflow :
      let interest := Libraries.MathLib.wMulDown (s.market aid).totalBorrowAssets
        (Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market aid).lastUpdate.val)))
      let feeAmount := Libraries.MathLib.wMulDown interest (s.market aid).fee
      let feeShares := Libraries.SharesMathLib.toSharesDown feeAmount
        (Morpho.u256 (((s.market aid).totalSupplyAssets.val + interest.val) % Core.Uint256.modulus - feeAmount.val))
        (s.market aid).totalSupplyShares
      (s.market aid).totalSupplyShares.val + feeShares.val < Core.Uint256.modulus) :
    supplySharesConsistent s' aid allUsers := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  exact accrueInterest_preserves_supplySharesConsistent s aid borrowRate hasIrm allUsers
    h_nodup h_mem_fee h_consistent h_pos_no_overflow h_total_no_overflow

theorem accrueInterestPublic_preserves_borrowSharesConsistent (s : MorphoState)
    (aid : Id) (borrowRate : Uint256) (hasIrm : Bool)
    (id : Id) (allUsers : List Address)
    (h_consistent : borrowSharesConsistent s id allUsers)
    (h_ok : Morpho.accrueInterestPublic s aid borrowRate hasIrm = some s') :
    borrowSharesConsistent s' id allUsers := by
  unfold Morpho.accrueInterestPublic at h_ok; simp at h_ok
  rw [← h_ok.right]
  exact accrueInterest_preserves_borrowSharesConsistent s aid borrowRate hasIrm id allUsers
    h_consistent

end Morpho.Proofs.ShareConsistency
