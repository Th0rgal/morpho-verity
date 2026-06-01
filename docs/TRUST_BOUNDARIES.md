# Trust Boundaries

This document inventories the Morpho-specific assumptions the proofs and parity
gates rely on. The repository's claim is implementation fidelity plus empirical
parity against the pinned Morpho Blue target, on top of the two machine-checked
health properties. Everything below is what those claims rest on.

## Artifact Packaging

`Morpho/Compiler/ArtifactConfig.lean` is not a second contract definition. The
single Morpho source of truth is `Morpho/Contract.lean`, where
`verity_contract Morpho` produces `Morpho.Contract.Morpho.spec`.

`ArtifactConfig.lean` adapts that macro-produced spec for the standalone compiler
CLI by setting the emitted artifact name, filtering internal helper functions
out of the external selector/ABI surface, and making the linked external
dependency set explicit. That dependency set is empty: `keccakMarketParams`,
`borrowRate`, and `oraclePrice` are implemented in `Morpho/Contract.lean`
through Verity ECM modules. CI enforces this boundary through
`scripts/check_morpho_artifact_boundary.py`.

## Local Obligations

The macro contract names local obligations at usage sites where Morpho still
crosses a low-level or external boundary:

| Local obligation | Usage |
|------------------|-------|
| `set_authorization_event` | `SetAuthorization` raw-log memory encoding in `setAuthorization`. |
| `authorization_post_ecrecover_write` | Intentional nonce increment before signature recovery and authorization write after ecrecover, matching Solidity ordering. |
| `create_market_irm_init` | Post-create IRM initialization call in `createMarket`. |
| `supply_callback`, `repay_callback`, `supply_collateral_callback`, `liquidate_callback` | Morpho callback ordering plus token transfer mechanics around the callback boundary. |
| `flash_loan_transfers` | Flash-loan token transfer and callback mechanics. |

## Still Assumed At The Current Pin

- ERC-20 optional-return SafeTransferLib target behavior, token no-fee/no-reentrancy
  economic assumptions, and external callee liveness.
- ECDSA/ecrecover cryptographic correctness.
- Static ABI/EIP-712/Keccak memory-slice correctness as surfaced by Verity ECM
  trust reports.
- Callback ECM target behavior, including the flash-loan callback between
  `safeTransfer` and `safeTransferFrom`.
- Event/log memory mechanics for raw-log paths, including `CreateMarket`'s
  encoded `MarketParams` payload.
- Low-level returndata and revert bubbling for external calls.
- Compilation correctness is delegated to Verity's compiler framework and the
  pinned artifact/parity gates in this repository.

## Verity Coverage

Verity supplies the EVM-shaped primitives the contract builds on: static ABI
Keccak helpers, EIP-712 digest helpers, Solmate-compatible ERC-20
optional-return ECMs, bubbling call/callback modules, raw-log validation, and
Solidity-0.8 checked-arithmetic helpers. The Morpho source uses these directly
rather than ad hoc stand-ins, so the remaining trust-report entries stay narrow
and auditable.

Keccak memory-slice correctness, raw `rawLog` mechanics, low-level call
execution, and precompile correctness are delegated to Verity and can appear in
its trust reports. The fidelity discipline is to keep replacing broad local
obligations with narrow Verity modules, preserve Solidity source ordering around
callbacks and signature recovery, and keep arithmetic translated through checked
operations or explicit guards.

## Refinement Proof Status

The refinement layer now assembles generated-body steps from
`Morpho/Proofs/Disciplines.lean` instead of keeping all entrypoint obligations as
opaque assumptions:

- `supply`, `withdraw`, `supplyCollateral`, and `repay` discharge their monotone
  field discipline in Lean through storage-framing and packed-field lemmas.
- `withdrawCollateral` and `borrow` discharge their generated `_isHealthy` guards
  in Lean, assuming explicit market-id alignment, oracle-price alignment, and the
  local oracle-price/collateral-fit domain required by `healthFaithful_of_noOverflow`. The `borrow`
  proof factors the post-accrual commit-and-health-check block and both amount
  modes as `guardedDiscipline_borrowCommitAndCheck`,
  `guardedDiscipline_borrowAssetsMode`, and
  `guardedDiscipline_borrowSharesMode`, then connects the public entrypoint
  through `guardedDiscipline_borrow`.
- `liquidate` no longer carries the structural generated-guard boundary:
  `guardUnhealthy_liquidate` proves that a successful generated body reached
  `require(!_isHealthy)` after `_accrueInterest` and that the health check
  returned `false`. The post-accrual/pre-state bridge is also proved in Lean:
  `liquidate_preStateUnhealthy_of_accrueInterest_identity` uses the explicit
  contract-side no-accrual discipline `AccrueInterestIdentityFor` to replay the
  generated guard on the original projected pre-state.
- Health arithmetic now exposes `LocalNoOverflowFor` with only the oracle-price
  multiplication bounds explicit: the watched collateral times oracle price, and
  that quoted collateral times LLTV, must fit in `uint256`. The borrow-side
  share/asset conversion uses the full-precision `mulDiv512Up` helper, and its
  quotient-fit proof is derived from the packed `uint128` storage reads at the
  `healthFaithful_of_noOverflow` call site.

## Current Gates

- `lake build` validates Lean elaboration and internal consistency for the
  currently imported implementation files.
- `scripts/check_morpho_event_surface.py` checks contract event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_morpho_artifact_boundary.py` checks that artifact packaging
  stays thin and any linked externals keep stable axiom names.
- `scripts/run_morpho_blue_parity.sh` runs the empirical Solidity-vs-Verity
  differential suite for the pinned Morpho Blue checkout.
