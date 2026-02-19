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
    (h_recv : receiver ≠ 0)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdraw s id assets shares onBehalf receiver = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.withdraw
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (receiver == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_recv
  simp [h1, h_valid, h3, h_auth]

/-! ## Borrow requires authorization -/

theorem borrow_requires_authorization (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_recv : receiver ≠ 0)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.borrow
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (receiver == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_recv
  simp [h1, h_valid, h3, h_auth]

/-! ## Withdraw collateral requires authorization

  Unlike withdraw/borrow (which need preconditions to reach the auth check),
  this theorem needs no preconditions: if the sender is unauthorized,
  `withdrawCollateral` returns `none` regardless of input. Either an earlier
  guard returns `none`, or the authorization check itself does. -/

theorem withdrawCollateral_requires_authorization (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_not_auth : s.sender ≠ onBehalf)
    (h_not_delegated : s.isAuthorized onBehalf s.sender = false)
    : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv = none := by
  have h_auth := isSenderAuthorized_false s onBehalf h_not_auth h_not_delegated
  unfold Morpho.withdrawCollateral
  simp [h_auth]

/-! ## Supply doesn't require authorization

  Supply has no authorization check at all — anyone can deposit on your behalf.
  Given valid preconditions (market exists, exactly one zero, non-empty address),
  the function always succeeds. -/

theorem supply_no_authorization_needed (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf : Address)
    (h_valid : Libraries.UtilsLib.exactlyOneZero assets shares)
    (h_market : (s.market id).lastUpdate.val ≠ 0)
    (h_addr : onBehalf ≠ 0)
    : ∃ r, Morpho.supply s id assets shares onBehalf = some r := by
  unfold Morpho.supply
  have h1 : ((s.market id).lastUpdate.val == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_market
  have h3 : (onBehalf == 0) = false := by
    rw [beq_eq_false_iff_ne]; exact h_addr
  simp [h1, h_valid, h3]

/-! ## Postcondition-style authorization proofs

  The specs define authorization as postconditions: "if a position field changed
  adversely, the sender was authorized." The proofs above show the precondition
  form ("if unauthorized, returns none"). These theorems connect the two:
  for any successful operation, the spec postcondition holds. -/

/-- Helper: if isSenderAuthorized is true, the spec's Bool disjunction holds. -/
private theorem auth_of_isSenderAuthorized (s : MorphoState) (onBehalf : Address)
    (h : Morpho.isSenderAuthorized s onBehalf = true) :
    (s.sender == onBehalf || s.isAuthorized onBehalf s.sender) = true := by
  unfold Morpho.isSenderAuthorized at h; exact h

/-- Withdraw satisfies onlyAuthorizedDecreaseSupply: if supply shares decreased,
    the sender was authorized to act on behalf of the user. -/
theorem withdraw_onlyAuthorizedDecreaseSupply (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (h_ok : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    onlyAuthorizedDecreaseSupply s s' id onBehalf := by
  unfold onlyAuthorizedDecreaseSupply
  intro _
  unfold Morpho.withdraw at h_ok
  simp at h_ok
  exact auth_of_isSenderAuthorized s onBehalf h_ok.2.2.2.1

/-- Borrow satisfies onlyAuthorizedIncreaseBorrow: if borrow shares increased,
    the sender was authorized to act on behalf of the user. -/
theorem borrow_onlyAuthorizedIncreaseBorrow (s : MorphoState) (id : Id)
    (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_ok : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv
      = some (a, sh, s')) :
    onlyAuthorizedIncreaseBorrow s s' id onBehalf := by
  unfold onlyAuthorizedIncreaseBorrow
  intro _
  unfold Morpho.borrow at h_ok
  simp at h_ok
  exact auth_of_isSenderAuthorized s onBehalf h_ok.2.2.2.1

/-- WithdrawCollateral satisfies onlyAuthorizedDecreaseCollateral: if collateral
    decreased, the sender was authorized to act on behalf of the user. -/
theorem withdrawCollateral_onlyAuthorizedDecreaseCollateral (s : MorphoState) (id : Id)
    (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (h_ok : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv
      = some s') :
    onlyAuthorizedDecreaseCollateral s s' id onBehalf := by
  unfold onlyAuthorizedDecreaseCollateral
  intro _
  unfold Morpho.withdrawCollateral at h_ok
  simp at h_ok
  exact auth_of_isSenderAuthorized s onBehalf h_ok.2.2.2.1

/-! ## Liquidation requires unhealthy position

  Liquidation bypasses the normal authorization check — anyone can liquidate anyone.
  But it requires the borrower's position to be unhealthy. This is the protocol's
  safety valve: bad positions must be liquidatable regardless of the borrower's consent. -/

/-- A successful liquidation implies the borrower's position was unhealthy. -/
theorem liquidate_requires_unhealthy (s : MorphoState) (id : Id)
    (borrower : Address) (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (h_ok : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv
      = some (seized, repaid, s')) :
    Morpho.isHealthy s id borrower collateralPrice lltv = false := by
  unfold Morpho.liquidate at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  split at h_ok <;> simp at h_ok
  all_goals exact h_ok.2.1

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
