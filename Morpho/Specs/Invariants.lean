/-
  Morpho Blue invariants — Properties that must hold across all state transitions.

  These are the core safety properties of the protocol. If any of these are violated,
  the protocol has a bug. They are derived from Morpho's own Certora formal verification
  specs (https://github.com/morpho-org/morpho-blue/tree/main/certora).

  Each property is stated as a predicate on `MorphoState`. To prove it, we show that:
  (a) it holds in the initial state, and
  (b) every state transition preserves it.
-/
import Morpho.Types
import Morpho.Libraries.MathLib
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.ConstantsLib

namespace Morpho.Specs.Invariants

open Verity
open Morpho.Types
open Morpho.Libraries

/-! ## Fee bounds

  The protocol takes a fee on interest accrued in each market. This fee is capped at 25%
  to prevent a malicious or compromised owner from extracting all interest. The fee can
  only be changed by the owner, and `setFee` rejects values above `MAX_FEE`.
-/

/-- Market fee never exceeds 25%. -/
def feeInRange (s : MorphoState) (id : Id) : Prop :=
  (s.market id).fee.val ≤ ConstantsLib.MAX_FEE

/-! ## Solvency

  The fundamental accounting invariant: a market can never owe more than it holds.
  `totalBorrowAssets ≤ totalSupplyAssets` is checked after every withdraw and borrow.
  If this breaks, lenders cannot fully withdraw — the protocol is insolvent.
-/

/-- Total borrows never exceed total supply in any market. -/
def borrowLeSupply (s : MorphoState) (id : Id) : Prop :=
  (s.market id).totalBorrowAssets.val ≤ (s.market id).totalSupplyAssets.val

/-! ## Share accounting

  The share system tracks each user's pro-rata ownership of the supply/borrow pools.
  The sum of all individual shares must equal the market's total shares. If this
  invariant breaks, users could extract more than their fair share.
-/

/-- Total supply shares = sum of all individual supply shares. -/
def supplySharesConsistent (s : MorphoState) (id : Id) (allUsers : List Address) : Prop :=
  (s.market id).totalSupplyShares.val =
    (allUsers.map (fun u => (s.position id u).supplyShares.val)).sum

/-- Total borrow shares = sum of all individual borrow shares. -/
def borrowSharesConsistent (s : MorphoState) (id : Id) (allUsers : List Address) : Prop :=
  (s.market id).totalBorrowShares.val =
    (allUsers.map (fun u => (s.position id u).borrowShares.val)).sum

/-! ## Market creation

  Markets can only be created with pre-approved parameters. The owner must first
  whitelist the IRM (interest rate model) and LLTV (liquidation loan-to-value ratio).
  LLTVs must be strictly below 100% — otherwise positions could never be liquidated.
-/

/-- Created markets use an enabled IRM and an enabled LLTV. -/
def createdMarketsValid (s : MorphoState) (id : Id) (params : MarketParams) : Prop :=
  s.idToParams id = some params →
    s.isIrmEnabled params.irm ∧ s.isLltvEnabled params.lltv

/-- Enabled LLTVs are strictly less than 100% (WAD = 1e18). -/
def lltvLtWad (s : MorphoState) (lltv : Uint256) : Prop :=
  s.isLltvEnabled lltv → lltv.val < MathLib.WAD

/-! ## Collateralization

  Every position with outstanding debt must have collateral backing it.
  When a liquidation drives collateral to zero, remaining debt is immediately
  "socialized" (subtracted from the supply pool), so borrowShares are set to 0.
  This ensures no position can have debt without collateral.
-/

/-- Positions with debt always have collateral (bad debt is realized immediately). -/
def alwaysCollateralized (s : MorphoState) (id : Id) (user : Address) : Prop :=
  (s.position id user).borrowShares.val > 0 →
    (s.position id user).collateral.val > 0

/-! ## Monotonicity

  Certain configuration is append-only: once an IRM or LLTV is enabled, it cannot
  be disabled. This prevents the owner from breaking existing markets by pulling
  the rug on their configuration. Market timestamps only increase (interest accrual
  is monotonic in time).
-/

/-- Once an IRM is enabled, it stays enabled across any state transition. -/
def irmMonotone (s s' : MorphoState) (irm : Address) : Prop :=
  s.isIrmEnabled irm → s'.isIrmEnabled irm

/-- Once an LLTV is enabled, it stays enabled across any state transition. -/
def lltvMonotone (s s' : MorphoState) (lltv : Uint256) : Prop :=
  s.isLltvEnabled lltv → s'.isLltvEnabled lltv

/-- Market lastUpdate timestamp never decreases. -/
def lastUpdateMonotone (s s' : MorphoState) (id : Id) : Prop :=
  (s.market id).lastUpdate.val ≤ (s'.market id).lastUpdate.val

end Morpho.Specs.Invariants
