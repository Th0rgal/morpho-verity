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

/-! ## Round-trip safety

  The rounding directions ensure a fundamental economic invariant: the protocol
  never gives back more than it received in a supply-then-withdraw cycle.

  When a user supplies `a` assets:
  1. They receive `toSharesDown(a)` shares (fewer shares = protocol's favor)
  2. If they immediately withdraw those shares, they get `toAssetsDown(toSharesDown(a))` assets

  The protocol keeps the difference: `a - toAssetsDown(toSharesDown(a)) ≥ 0`.
  This prevents rounding exploitation where an attacker repeatedly converts
  between assets and shares to extract value. -/

/-- Supply round-trip: assets returned ≤ assets deposited. -/
def supplyRoundtripSafe (assets totalAssets totalShares : Uint256) : Prop :=
  (toAssetsDown (toSharesDown assets totalAssets totalShares) totalAssets totalShares).val
    ≤ assets.val

/-- Withdraw round-trip: shares re-minted ≤ shares burned. -/
def withdrawRoundtripSafe (shares totalAssets totalShares : Uint256) : Prop :=
  (toSharesDown (toAssetsDown shares totalAssets totalShares) totalAssets totalShares).val
    ≤ shares.val

end Morpho.Specs.Rounding
