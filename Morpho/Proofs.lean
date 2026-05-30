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
    * Property2    — "a liquidation can restore a sub-1/LIF borrower" (existence
                     proved; sharp partial-liquidation statement reduced to one
                     named rounding obligation).
    * Refinement   — per-entrypoint obligations bridging model and contract.
-/

import Morpho.Proofs.HealthModel
import Morpho.Proofs.Env
import Morpho.Proofs.Arith
import Morpho.Proofs.Property1
import Morpho.Proofs.Property2
import Morpho.Proofs.Refinement
