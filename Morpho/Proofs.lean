/-
  Morpho.Proofs — protocol-property proof layer for the Morpho contract.

  This is proof infrastructure built *around* `verity_contract Morpho`, not a
  competing model of it. See `Morpho/Proofs/HealthModel.lean` for the projection
  discipline and `Morpho/Proofs/Refinement.lean` for the contract-link obligations.

  Contents:
    * HealthModel  — storage projection + the `healthy` predicate (verbatim).
    * Env          — constant-price / no-accrual environment assumptions.
    * Arith        — generic floor/ceil division monotonicity lemmas.
    * Property1    — "no operation makes a healthy account unhealthy" (proved).
    * Operations   — concrete Solidity-mirroring transitions; collateral/supply
                     ops shown to preserve health with no assumptions.
    * Property2    — "a liquidation restores a sub-1/LIF borrower": the sharp
                     full-liquidation guarantee proved outright, for both input
                     modes (repaid-shares and seized-assets), with the partial
                     phrasing refuted by counterexample.
    * Projection   — reads a `HealthState` out of the real contract state through
                     the generated accessors; names the arithmetic-faithfulness
                     boundary (`HealthFaithful`).
    * Refinement   — per-entrypoint obligations bridging model and contract; for
                     `liquidate` the step is the real generated body (see
                     `Refinement.Contract`).
-/

import Morpho.Proofs.HealthModel
import Morpho.Proofs.Env
import Morpho.Proofs.Arith
import Morpho.Proofs.Property1
import Morpho.Proofs.Operations
import Morpho.Proofs.Property2
import Morpho.Proofs.Projection
import Morpho.Proofs.Refinement
