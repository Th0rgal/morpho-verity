/-
  Proofs of authorization properties.

  These theorems show that withdraw, borrow, and withdrawCollateral require
  the sender to be authorized, while supply does not.
-/
import Morpho.Morpho
import Morpho.Specs.Authorization

namespace Morpho.Proofs.Authorization

open Verity
open Morpho.Types
open Morpho.Specs.Authorization

/-- Helper: if sender ≠ onBehalf and delegation is false, isSenderAuthorized is false. -/
private theorem isSenderAuthorized_false (s : MorphoState) (onBehalf : Address)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.isSenderAuthorized s onBehalf = false := by
  unfold Morpho.isSenderAuthorized
  have h1 : (s.sender == onBehalf) = false := by
    rw [beq_eq_false_iff_ne]
    exact h_not_auth
  simp [h1, h_not_delegated]

/-! ## Withdraw requires authorization -/

theorem withdraw_requires_authorization (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_recv : receiver ≠ "")
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdraw s id assets shares onBehalf receiver = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.withdraw
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (receiver == "") = false := by
    rw [beq_eq_false_iff_ne]; exact h_recv
  simp [h1, h_valid, h3, h_auth]

/-! ## Borrow requires authorization -/

theorem borrow_requires_authorization (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_recv : receiver ≠ "")
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.borrow
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (receiver == "") = false := by
    rw [beq_eq_false_iff_ne]; exact h_recv
  simp [h1, h_valid, h3, h_auth]

/-! ## Withdraw collateral requires authorization

  This theorem needs fewer preconditions because the result is `none` regardless
  of which branch causes it. If the market doesn't exist, assets are zero, or
  receiver is empty, the function returns `none` from an earlier check. If those
  checks pass, the authorization check returns `none`. -/

theorem withdrawCollateral_requires_authorization (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.withdrawCollateral
  -- The function has several early-return checks before the auth check.
  -- In every branch, the result is none: either an early check returns none,
  -- or the auth check (which we know fails) returns none.
  simp only [h_auth]
  -- After substituting auth = false, the auth branch always returns none.
  -- We need to show that regardless of the earlier conditions, result is none.
  -- Case split on each early-exit condition.
  by_cases h1 : (s.market id).lastUpdate.val = 0
  · -- Market not created → none from first check
    have : ((s.market id).lastUpdate.val == 0) = true := by
      rw [beq_iff_eq]; exact h1
    simp [this]
  · have h1' : ((s.market id).lastUpdate.val == 0) = false := by
      rw [beq_eq_false_iff_ne]; exact h1
    by_cases h2 : assets.val = 0
    · have : (assets.val == 0) = true := by rw [beq_iff_eq]; exact h2
      simp [h1', this]
    · have h2' : (assets.val == 0) = false := by rw [beq_eq_false_iff_ne]; exact h2
      by_cases h3 : receiver = ""
      · have : (receiver == "") = true := by rw [beq_iff_eq]; exact h3
        simp [h1', h2', this]
      · have h3' : (receiver == "") = false := by rw [beq_eq_false_iff_ne]; exact h3
        simp [h1', h2', h3']

/-! ## Supply doesn't require authorization

  Supply has no authorization check at all — anyone can deposit on your behalf.
  Given valid preconditions (market exists, exactly one zero, non-empty address),
  the function always succeeds. -/

theorem supply_no_authorization_needed (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    (h_addr : onBehalf ≠ "")
    : ∃ r, Morpho.supply s id assets shares onBehalf = some r := by
  unfold Morpho.supply
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (onBehalf == "") = false := by
    rw [beq_eq_false_iff_ne]; exact h_addr
  simp [h1, h_valid, h3]

/-! ## Signature-based authorization proofs

  `setAuthorizationWithSig` allows gasless delegation via EIP-712 signatures.
  We prove three properties about its pure state logic:
  - Expired deadlines are rejected
  - Wrong nonces are rejected
  - Successful calls increment the authorizer's nonce -/

/-- setAuthorizationWithSig rejects expired deadlines. -/
theorem sig_rejects_expired_deadline (s : MorphoState) (auth : Authorization) (sig : Bool)
    (h_expired : s.blockTimestamp.val > auth.deadline.val) :
    Morpho.setAuthorizationWithSig s auth sig = none := by
  unfold Morpho.setAuthorizationWithSig
  simp
  intro h_le
  omega

/-- setAuthorizationWithSig rejects wrong nonces. -/
theorem sig_rejects_wrong_nonce (s : MorphoState) (auth : Authorization) (sig : Bool)
    (h_nonce : auth.nonce ≠ s.nonce auth.authorizer) :
    Morpho.setAuthorizationWithSig s auth sig = none := by
  unfold Morpho.setAuthorizationWithSig
  simp
  intro _
  intro h_eq
  exact absurd h_eq h_nonce

/-- Successful setAuthorizationWithSig increments the authorizer's nonce. -/
theorem sig_increments_nonce (s : MorphoState) (auth : Authorization) (sig : Bool)
    (h_ok : Morpho.setAuthorizationWithSig s auth sig = some s') :
    nonceIncremented s s' auth.authorizer := by
  unfold Morpho.setAuthorizationWithSig at h_ok
  simp at h_ok
  obtain ⟨_, _, _, h_eq⟩ := h_ok
  unfold nonceIncremented
  rw [← h_eq]
  simp [Morpho.u256, Nat.add_comm]

end Morpho.Proofs.Authorization
