/-
  Midnight.Proofs — focused Lean model for the two requested Morpho Midnight
  invariants.

  The model is intentionally small but follows the storage quantities and update
  equations in `midnight/src/Midnight.sol`:

    * `RCF` models the normal-mode `liquidate` recovery-close-factor check and
      proves that using `repaidUnits = maxRepaid` can restore health when the
      Solidity rounding obligation is available.
    * `CollateralLoop` models the active-collateral liquidation loop
      accumulators for `maxDebt`, `liquidatedCollatPrice`, and `badDebt`.
    * `BitmapSchedule` models the descending `msb` / `clearBit` index schedule
      consumed by the collateral-loop projection.
    * `MarketLedger` models the debt-side cover invariant that supplies
      `originalDebt <= totalUnits` for the bad-debt branch.
    * `UnitsAccounting` models bad-debt loss-factor slashing and proves that a
      market's `totalUnits` covers the sum of lenders' up-to-date credit.
    * `Storage` projects the combined local `liquidate` step back onto the
      touched `MarketState` and borrower `Position` storage fields.
-/

import Midnight.Proofs.RCF
import Midnight.Proofs.UnitsAccounting
import Midnight.Proofs.CollateralLoop
import Midnight.Proofs.BitmapSchedule
import Midnight.Proofs.MarketLedger
import Midnight.Proofs.Refinement
import Midnight.Proofs.Storage
import Midnight.Proofs.ContractShape
