/-
  Rounding specs — The protocol always rounds against the user.

  This is the fundamental economic safety property of Morpho Blue. In DeFi,
  integer division loses fractional amounts. The direction of rounding determines
  who benefits from the lost fraction: the user or the protocol.

  Morpho Blue consistently rounds in the protocol's favor:

  | Operation          | Computed value    | Rounding | Why                            |
  |--------------------|-------------------|----------|--------------------------------|
  | Supply by assets   | shares minted     | DOWN     | Fewer shares = less ownership  |
  | Supply by shares   | assets pulled     | UP       | More tokens taken from user    |
  | Withdraw by assets | shares burned     | UP       | More shares destroyed          |
  | Withdraw by shares | assets received   | DOWN     | Fewer tokens returned to user  |
  | Borrow by assets   | shares owed       | UP       | More debt tracked              |
  | Borrow by shares   | assets received   | DOWN     | Fewer tokens given to user     |
  | Repay by assets    | shares reduced    | DOWN     | Less debt forgiven             |
  | Repay by shares    | assets pulled     | UP       | More tokens taken from user    |

  This consistent rounding ensures the protocol never loses value through rounding
  errors, no matter how many operations are performed. Without this property, an
  attacker could profit by repeatedly converting between assets and shares.

  The properties below are the building blocks: they state that the Down variant
  of each conversion always produces a result ≤ the Up variant.
-/
import Morpho.Types
import Morpho.Libraries.SharesMathLib

namespace Morpho.Specs.Rounding

open Verity
open Morpho.Types
open Morpho.Libraries.SharesMathLib

/-- Rounding down yields fewer shares than rounding up. -/
def sharesDownLeUp (assets totalAssets totalShares : Uint256) : Prop :=
  (toSharesDown assets totalAssets totalShares).val ≤
    (toSharesUp assets totalAssets totalShares).val

/-- Rounding down yields fewer assets than rounding up. -/
def assetsDownLeUp (shares totalAssets totalShares : Uint256) : Prop :=
  (toAssetsDown shares totalAssets totalShares).val ≤
    (toAssetsUp shares totalAssets totalShares).val

end Morpho.Specs.Rounding
