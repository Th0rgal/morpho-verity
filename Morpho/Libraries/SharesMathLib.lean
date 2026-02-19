/-
  SharesMathLib — Share/asset conversion with virtual offset.
  Matches morpho-blue/src/libraries/SharesMathLib.sol

  The virtual shares (1e6) and virtual assets (1) prevent inflation attacks
  where an attacker can manipulate the share price by being the first depositor.
-/
import Morpho.Libraries.MathLib

namespace Morpho.Libraries.SharesMathLib

open Verity.Core
open Morpho.Libraries.MathLib (mulDivDown mulDivUp)

def VIRTUAL_SHARES : Nat := 10 ^ 6
def VIRTUAL_ASSETS : Nat := 1

/-- Convert assets to shares, rounding down. -/
def toSharesDown (assets totalAssets totalShares : Uint256) : Uint256 :=
  mulDivDown assets
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))

/-- Convert shares to assets, rounding down. -/
def toAssetsDown (shares totalAssets totalShares : Uint256) : Uint256 :=
  mulDivDown shares
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))

/-- Convert assets to shares, rounding up. -/
def toSharesUp (assets totalAssets totalShares : Uint256) : Uint256 :=
  mulDivUp assets
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))

/-- Convert shares to assets, rounding up. -/
def toAssetsUp (shares totalAssets totalShares : Uint256) : Uint256 :=
  mulDivUp shares
    (Uint256.ofNat (totalAssets.val + VIRTUAL_ASSETS))
    (Uint256.ofNat (totalShares.val + VIRTUAL_SHARES))

end Morpho.Libraries.SharesMathLib
