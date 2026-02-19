/-
  Rounding specs — The protocol always rounds against the user.
  This is the fundamental economic safety property of Morpho Blue.
-/
import Morpho.Types
import Morpho.Libraries.SharesMathLib

namespace Morpho.Specs.Rounding

open Verity
open Morpho.Types
open Morpho.Libraries.SharesMathLib

/-! ## Rounding direction properties

  Every conversion between assets and shares rounds in the direction
  that favors the protocol (i.e., against the user):

  | Operation          | Computed value    | Rounding |
  |--------------------|-------------------|----------|
  | Supply by assets   | shares minted     | DOWN     |
  | Supply by shares   | assets pulled     | UP       |
  | Withdraw by assets | shares burned     | UP       |
  | Withdraw by shares | assets received   | DOWN     |
  | Borrow by assets   | shares owed       | UP       |
  | Borrow by shares   | assets received   | DOWN     |
  | Repay by assets    | shares reduced    | DOWN     |
  | Repay by shares    | assets pulled     | UP       |

  These are structural properties of the toSharesDown/Up and toAssetsDown/Up
  functions in SharesMathLib. They ensure the protocol never loses value
  through rounding errors.
-/

/-- toSharesDown ≤ toSharesUp for the same inputs. -/
def sharesDownLeUp (assets totalAssets totalShares : Uint256) : Prop :=
  (toSharesDown assets totalAssets totalShares).val ≤
    (toSharesUp assets totalAssets totalShares).val

/-- toAssetsDown ≤ toAssetsUp for the same inputs. -/
def assetsDownLeUp (shares totalAssets totalShares : Uint256) : Prop :=
  (toAssetsDown shares totalAssets totalShares).val ≤
    (toAssetsUp shares totalAssets totalShares).val

end Morpho.Specs.Rounding
