/-
  Authorization specs — Only authorized users can modify positions.
-/
import Morpho.Types

namespace Morpho.Specs.Authorization

open Verity
open Morpho.Types

/-- Only authorized users can decrease supply shares. -/
def onlyAuthorizedDecreaseSupply (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).supplyShares.val < (s.position id user).supplyShares.val →
    s.sender == user || s.isAuthorized user s.sender

/-- Only authorized users can increase borrow shares. -/
def onlyAuthorizedIncreaseBorrow (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).borrowShares.val > (s.position id user).borrowShares.val →
    s.sender == user || s.isAuthorized user s.sender

/-- Only authorized users can decrease collateral (except via liquidation). -/
def onlyAuthorizedDecreaseCollateral (s s' : MorphoState) (id : Id) (user : Address) : Prop :=
  (s'.position id user).collateral.val < (s.position id user).collateral.val →
    s.sender == user || s.isAuthorized user s.sender

end Morpho.Specs.Authorization
