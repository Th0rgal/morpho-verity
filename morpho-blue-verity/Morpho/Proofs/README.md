# Morpho Proofs

The health and liquidation proof layer, built around `verity_contract Morpho`
(`morpho-blue-verity/Morpho/Contract.lean`). Every theorem here is checked by `lake build Morpho.Proofs`.

The proofs reason over `HealthModel`, the projection of contract storage the
health predicate reads, with `healthy` transcribed verbatim from
`_isHealthyWithPrice`. Environment assumptions (constant price, no interest
accrual) live in `Env.lean`.

## Results

| File | Property | Headline theorem | Status |
|------|----------|------------------|--------|
| `Property1.lean` | At constant price and without accrual, no operation turns a healthy account unhealthy. | `no_operation_breaks_health` | Proved |
| `Property2.lean` | When loan-to-value is below `1/LIF`, full liquidation is affordable and restores the position to healthy. | `sharp_property2` | Proved |
| `Property2.lean` | The weaker "some strictly-partial repayment restores health" phrasing is false, since borrow shares are indivisible. | `partial_phrasing_fails` | Disproved (counterexample) |

Both properties are the ones the Morpho team raised in
[morpho-blue #684](https://github.com/morpho-org/morpho-blue/pull/684).

## Files

| File | Role |
|------|------|
| `HealthModel.lean` | Storage projection and the `healthy` predicate. |
| `Env.lean` | Constant-price and no-accrual environment assumptions. |
| `Arith.lean` | Floor/ceil division monotonicity lemmas. |
| `Property1.lean` | Health preservation. |
| `Operations.lean` | Concrete Solidity-mirroring transitions; collateral and supply ops preserve health with no assumptions. |
| `Property2.lean` | Sharp liquidation guarantee for both input modes, plus the partial-phrasing counterexample. |
| `Refinement.lean` | Per-entrypoint assembly from generated-body step obligations; some step disciplines remain named boundaries in `Disciplines.lean`. |

The model-to-bytecode link is covered empirically by the differential parity
suite, not yet by a Lean extraction layer. See
[`../../docs/TRUST_BOUNDARIES.md`](../../docs/TRUST_BOUNDARIES.md).
