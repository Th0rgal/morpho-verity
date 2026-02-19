/-
  Authorization specs — Only authorized users can modify positions.

  Morpho Blue uses a delegation system: address A can authorize address B to manage
  A's positions (via `setAuthorization`). The core security guarantee is that nobody
  can touch your funds unless you explicitly authorized them.

  Three operations modify positions in ways that hurt the owner:
  - `withdraw` (decreases supply shares = takes your deposited assets)
  - `borrow` (increases borrow shares = takes on debt in your name)
  - `withdrawCollateral` (decreases collateral = removes your safety margin)

  Conversely, `supply`, `repay`, and `supplyCollateral` only help the position owner,
  so they require no authorization — anyone can deposit on your behalf.

  `liquidate` is special: it can decrease collateral and borrow shares, but only
  when the position is unhealthy. This is by design — liquidations protect the protocol.
-/
import Morpho.Types

namespace Morpho.Specs.Authorization

open Verity
open Morpho.Types

/-- No one can decrease your supply shares without your authorization. -/
def onlyAuthorizedDecreaseSupply (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).supplyShares.val < (s.position id user).supplyShares.val →
    s.sender == user || s.isAuthorized user s.sender

/-- No one can increase your debt without your authorization. -/
def onlyAuthorizedIncreaseBorrow (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).borrowShares.val > (s.position id user).borrowShares.val →
    s.sender == user || s.isAuthorized user s.sender

/-- No one can remove your collateral via `withdrawCollateral` without your authorization.

  Liquidation is the one exception: it can seize collateral without the borrower's consent,
  but only when the position is unhealthy. See `liquidate_requires_unhealthy` in
  `Proofs/Authorization.lean` for the complementary guarantee. -/
def onlyAuthorizedDecreaseCollateral (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).collateral.val < (s.position id user).collateral.val →
    s.sender == user || s.isAuthorized user s.sender

/-! ## Signature-based authorization

  `setAuthorizationWithSig` allows gasless delegation via EIP-712 signatures.
  It has two pure state-logic guarantees:
  - **Replay protection**: the nonce must match and is incremented on every call
  - **Expiry**: the authorization is rejected if the deadline has passed

  The cryptographic guarantee (valid ecrecover) is modeled as a parameter. -/

/-- After a signature-based authorization, the authorizer's nonce has increased. -/
def nonceIncremented (s s' : MorphoState) (authorizer : Address) : Prop :=
  s'.nonce authorizer = Verity.Core.Uint256.ofNat ((s.nonce authorizer).val + 1)

end Morpho.Specs.Authorization
