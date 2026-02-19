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

/-- No one can remove your collateral without your authorization (except liquidation). -/
def onlyAuthorizedDecreaseCollateral (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).collateral.val < (s.position id user).collateral.val →
    s.sender == user || s.isAuthorized user s.sender

end Morpho.Specs.Authorization
