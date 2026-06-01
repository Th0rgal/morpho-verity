# Morpho Verity

A [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) model of
[Morpho Blue](https://morpho.org/), written with
[Verity](https://github.com/Th0rgal/verity), carrying two machine-checked safety
properties and a differential parity harness against the pinned Solidity source.

The contract is a single `verity_contract Morpho` declaration in
[`Morpho/Contract.lean`](Morpho/Contract.lean) — at once the executable
implementation, the compiler-facing surface, and the source the proofs project
from. The proofs reason about that contract's health arithmetic; bytecode-level
fidelity is checked empirically against Morpho Blue's own test suite.

## What is proven

Every theorem below is checked by `lake build Morpho.Proofs`.

| Property | Statement | Theorem |
|----------|-----------|---------|
| Health preservation | At constant price and without interest accrual, no operation can turn a healthy account unhealthy. | `no_operation_breaks_health` ([Property1.lean](Morpho/Proofs/Property1.lean)) |
| Liquidation guarantee (sharp) | When loan-to-value is below `1/LIF`, full liquidation is affordable (no revert, no bad debt) and restores the position to healthy. | `sharp_property2` ([Property2.lean](Morpho/Proofs/Property2.lean)) |
| Counterexample | The weaker "some strictly-partial repayment restores health" phrasing is false — borrow shares are indivisible — shown on a well-formed market state. | `partial_phrasing_fails` ([Property2.lean](Morpho/Proofs/Property2.lean)) |

Both properties are the ones the Morpho team raised, and left as hard to state
precisely, in [morpho-blue #684](https://github.com/morpho-org/morpho-blue/pull/684).

The proofs are stated over [`HealthModel`](Morpho/Proofs/HealthModel.lean), the
projection of contract storage that the health predicate reads, with the
predicate `healthy` transcribed verbatim from `_isHealthyWithPrice`. The
environment assumptions (constant price, no accrual) are named in
[`Env.lean`](Morpho/Proofs/Env.lean). See
[`Morpho/Proofs.lean`](Morpho/Proofs.lean) for a file-by-file map of the proof
layer.

## Scope and trust boundaries

- **Per-entrypoint correspondence.** Each Morpho entrypoint must match one of the
  two proof shapes once projected onto a watched account. The `Refinement.Contract`
  namespace in [`Refinement.lean`](Morpho/Proofs/Refinement.lean) builds each
  entrypoint's `Step` from the *real* generated contract body, run to success and
  projected through [`Projection.lean`](Morpho/Proofs/Projection.lean), which reads
  the watched position through the same generated accessors the contract uses.
  The monotone collateral/supply-side calls are now discharged in Lean from
  generated-body storage-framing lemmas (`Disciplines.lean`), and
  `withdrawCollateral` and `borrow` are connected to their generated
  `require(_isHealthy)` guards under explicit market-id, oracle-price, and
  localized no-overflow side conditions. The `borrow` proof factors the post-accrual
  commit-and-health-check block plus both amount modes
  (`guardedDiscipline_borrowCommitAndCheck`,
  `guardedDiscipline_borrowAssetsMode`, and
  `guardedDiscipline_borrowSharesMode`). `liquidate` now proves the generated
  post-accrual `require(!_isHealthy)` extraction as `guardUnhealthy_liquidate`;
  the remaining bridge is the explicitly named pre-state condition
  `LiquidatePreStateUnhealthy`.
  The remaining step-to-EVM-bytecode link is empirical, not yet a Lean extraction
  layer.
- **Arithmetic.** Health arithmetic is modeled over `Nat`. It agrees with the
  contract's fixed-width word arithmetic on the localized no-overflow domain used
  by `LocalNoOverflowFor`: borrow-side share/asset bounds are derived from the
  packed `uint128` storage reads and the full-precision `mulDiv512Up` health
  conversion, leaving the oracle-price multiplication bounds explicit (see
  [`Disciplines.lean`](Morpho/Proofs/Disciplines.lean) and
  [`HealthModel.lean`](Morpho/Proofs/HealthModel.lean)).
- **External components.** Oracle, IRM, ERC-20, callbacks, and keccak/ecrecover
  behavior are environment assumptions. Full inventory:
  [`docs/TRUST_BOUNDARIES.md`](docs/TRUST_BOUNDARIES.md).

## Verify it yourself

Requires Lean 4 (v4.22.0), installed via [elan](https://github.com/leanprover/elan).

```bash
git clone https://github.com/Th0rgal/morpho-verity
cd morpho-verity
lake build Morpho.Proofs
```

To build the whole project (contract, libraries, compiler):

```bash
lake build
```

## Differential parity

The same Morpho Blue test suite runs against both the Solidity source and the
Verity-compiled artifact, so the model is continuously checked against the
reference implementation.

```bash
git submodule update --init --recursive
./scripts/run_morpho_blue_parity.sh
```

The Solidity compilation context (solc version, optimizer, viaIR, EVM version) is
pinned in [`config/parity-target.json`](config/parity-target.json); see
[`docs/PARITY_TARGET.md`](docs/PARITY_TARGET.md). Artifact preparation, the
Yul-identity gap report, and the full set of CI environment and timeout knobs are
documented in [`docs/CI.md`](docs/CI.md).

## Layout

```
Morpho/
  Contract.lean          # canonical verity_contract Morpho source (single source of truth)
  Libraries/             # WAD math, shares math, constants, utils (translated from MathLib.sol etc.)
  Compiler/              # artifact packaging + Yul codegen config over Morpho.Contract.Morpho.spec
  Proofs/                # the health/liquidation proof layer (HealthModel, Property1/2, Refinement)
morpho-blue/             # Morpho Blue Solidity reference (git submodule)
verity-foundry/          # Foundry project for testing the Verity-compiled artifact
scripts/                 # artifact build, parity runner, Yul-identity report, CI checks
config/                  # parity target + Yul rewrite/identity manifests
docs/                    # trust boundaries, parity target, CI configuration
```

## License

MIT
