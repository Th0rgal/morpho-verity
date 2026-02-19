/-
  Morpho Blue invariants — Properties that must hold across all state transitions.
  Derived from Morpho's Certora formal verification specs.
-/
import Morpho.Types
import Morpho.Libraries.MathLib
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.ConstantsLib

namespace Morpho.Specs.Invariants

open Verity
open Morpho.Types
open Morpho.Libraries

/-! ## Fee bounds -/

/-- Market fee never exceeds 25%. -/
def feeInRange (s : MorphoState) (id : Id) : Prop :=
  (s.market id).fee.val ≤ ConstantsLib.MAX_FEE

/-! ## Solvency -/

/-- Total borrows never exceed total supply. -/
def borrowLeSupply (s : MorphoState) (id : Id) : Prop :=
  (s.market id).totalBorrowAssets.val ≤ (s.market id).totalSupplyAssets.val

/-! ## Share accounting -/

/-- Total supply shares equals sum of all individual supply shares. -/
def supplySharesConsistent (s : MorphoState) (id : Id) (allUsers : List Address) : Prop :=
  (s.market id).totalSupplyShares.val =
    (allUsers.map (fun u => (s.position id u).supplyShares.val)).sum

/-- Total borrow shares equals sum of all individual borrow shares. -/
def borrowSharesConsistent (s : MorphoState) (id : Id) (allUsers : List Address) : Prop :=
  (s.market id).totalBorrowShares.val =
    (allUsers.map (fun u => (s.position id u).borrowShares.val)).sum

/-! ## Market creation -/

/-- Created markets have enabled IRM and LLTV. -/
def createdMarketsValid (s : MorphoState) (id : Id) (params : MarketParams) : Prop :=
  s.idToParams id = some params →
    s.isIrmEnabled params.irm ∧ s.isLltvEnabled params.lltv

/-- Enabled LLTVs are strictly less than 100%. -/
def lltvLtWad (s : MorphoState) (lltv : Uint256) : Prop :=
  s.isLltvEnabled lltv → lltv.val < MathLib.WAD

/-! ## Collateralization -/

/-- Positions with debt always have collateral (bad debt is realized immediately). -/
def alwaysCollateralized (s : MorphoState) (id : Id) (user : Address) : Prop :=
  (s.position id user).borrowShares.val > 0 →
    (s.position id user).collateral.val > 0

/-! ## Monotonicity -/

/-- Once an IRM is enabled, it stays enabled. -/
def irmMonotone (s s' : MorphoState) (irm : Address) : Prop :=
  s.isIrmEnabled irm → s'.isIrmEnabled irm

/-- Once an LLTV is enabled, it stays enabled. -/
def lltvMonotone (s s' : MorphoState) (lltv : Uint256) : Prop :=
  s.isLltvEnabled lltv → s'.isLltvEnabled lltv

/-- Market lastUpdate is monotonically non-decreasing. -/
def lastUpdateMonotone (s s' : MorphoState) (id : Id) : Prop :=
  (s.market id).lastUpdate.val ≤ (s'.market id).lastUpdate.val

end Morpho.Specs.Invariants
