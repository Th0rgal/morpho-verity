# Midnight Verity Plan

This file is the active completion plan for the Morpho Midnight Verity track.
It separates the focused proof-model artifact that exists today from the full
`IMidnight` artifact required for upstream Foundry parity.

## Definition Of Done

The Midnight track is complete only when every item below has concrete evidence:

| Requirement | Required evidence | Current status |
|-------------|-------------------|----------------|
| Full executable Midnight Verity artifact. | Non-empty `artifacts/midnight/Midnight.bin.raw` generated from a Verity contract named `Midnight`, plus Yul/ABI artifacts and an input manifest. | Present. `./scripts/prepare_midnight_artifact.sh` emits `artifacts/midnight/Midnight.bin.raw`, `Midnight.yul`, `Midnight.abi.json`, and `Midnight.artifact-manifest.env` with `complete_imidnight_artifact=1` and `parity_ready=1`. |
| Original Midnight tests pass against Verity. | `MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh` exits `0` and writes `out/parity/morpho_midnight_verity.log` showing 0 failures. | Present locally: 373 tests passed, 0 failed, 0 skipped. |
| Original Midnight Solidity baseline remains green. | `MORPHO_MIDNIGHT_PARITY_MODE=solidity ./scripts/run_morpho_midnight_parity.sh` exits `0`. | Present locally: 373 tests passed, 0 failed, 0 skipped. |
| Mechanical mapping exists. | `MORPHO_MIDNIGHT_MAPPING.md` covers every `IMidnight` function and distinguishes focused from full artifacts. | Present; checked by `python3 scripts/check_morpho_midnight_mapping.py`. |
| Focused artifact boundary is fail-closed. | `python3 scripts/check_morpho_midnight_artifact_boundary.py` passes and rejects focused bytecode copied into the full parity path. | Present. |
| Yul drift gating exists for the full Midnight artifact. | A Midnight-specific Yul identity/drift report compares Solidity `Midnight.sol` against Verity-generated `Midnight.yul`, with a checked manifest or exact equality gate. | Present. `python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate` passes with `unsupportedManifestOk: True`; `fullyExact` remains `False`, so this is a drift gate and not a strict Yul equivalence claim. |
| Requested proof 1: RCF permits recovery with `repaidUnits = maxRepaid`. | `lake build Midnight.Proofs` checks the RCF theorem chain and assumptions are documented. | Present for the focused model. |
| Requested proof 2: `totalUnits` covers lender credit after slashing/updating. | `lake build Midnight.Proofs` checks the accounting theorem chain and assumptions are documented. | Present for the focused model. |
| Minimal documented assumptions. | `morpho-midnight-verity/Midnight/Proofs/TRUST_BOUNDARIES.md` maps remaining generated-body/storage/external boundaries, and placeholder-proof scans are clean. | Present for the focused model. |

## Artifact Status

Focused proof artifact:

```bash
./scripts/prepare_focused_midnight_artifact.sh
```

This emits `artifacts/midnight-focused/MidnightRCF.*`. It remains useful for
reviewing the focused RCF/accounting proof surface and is intentionally kept
separate from the full parity artifact.

Full Midnight artifact:

```bash
./scripts/prepare_midnight_artifact.sh
```

This emits:

- `artifacts/midnight/Midnight.yul`
- `artifacts/midnight/Midnight.abi.json`
- `artifacts/midnight/Midnight.bin.raw`
- `artifacts/midnight/Midnight.artifact-manifest.env`

The full artifact is the one consumed by
`MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh`.

## Completed Implementation Slices

The executable Midnight artifact covers the upstream `IMidnight` ABI exercised
by the original suite:

- market identity and SSTORE2/CREATE2-style market storage: `toId`, `toMarket`, `touchMarket`
- admin, fee, authorization, consumed, and market-state getters/setters
- `take`, including buy/sell asset math, settlement fee, callbacks, ratifiers, gates, reduce-only, liquidation lock, health checks, and max-amount paths
- `supplyCollateral`, `withdrawCollateral`, `withdraw`, `repay`, `flashLoan`, and `multicall`
- `liquidate`, including bad debt, RCF, post-maturity mode, callbacks, multi-collateral loops, bitmap clearing, and event/return behavior as covered by upstream tests
- continuous-fee and settlement-fee flows covered by upstream tests

Current local parity evidence:

```text
Solidity Midnight: 373 passed, 0 failed, 0 skipped
Verity Midnight:   373 passed, 0 failed, 0 skipped
```

## Yul Drift Gate

Midnight does not claim strict Yul equivalence. The current gate is a
function-level drift manifest:

```bash
python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate
```

Expected evidence:

```text
fullyExact: False
midnightGateMode: unsupported-manifest
unsupportedManifestOk: True
```

The allowed drift is recorded in
`config/midnight-yul-identity-unsupported.json`. Any added, removed, or changed
function-level Yul drift fails the gate until the manifest is deliberately
updated.

## Proof Status

The requested theorem chains are present under `morpho-midnight-verity/Midnight/Proofs`
and build with `lake build Midnight.Proofs`:

- RCF recovery with `repaidUnits = maxRepaid`: `RCF.lean`, `Refinement.lean`, and `Storage.lean` prove the max-repaid guard, local post-state health payoff, and storage projection wrappers.
- `totalUnits` covers lender credit after slashing/updating: `UnitsAccounting.lean`, `MarketLedger.lean`, `CollateralLoop.lean`, `BitmapSchedule.lean`, `Refinement.lean`, and `Storage.lean` prove the cover preservation chain over the named projections.

Remaining proof boundaries are documented in
`morpho-midnight-verity/Midnight/Proofs/TRUST_BOUNDARIES.md`: the theorem chains
are attached to faithful executable/projection surfaces, but some generated-body
extraction obligations remain semantic assumptions rather than discharged from
the full Yul bytecode.

## Required Commands

Proof and focused artifact:

```bash
lake build Midnight.Proofs midnight-verity-compiler
./scripts/prepare_focused_midnight_artifact.sh
./scripts/prepare_midnight_artifact.sh
python3 scripts/check_morpho_midnight_mapping.py
python3 scripts/check_morpho_midnight_artifact_boundary.py
```

Full parity target:

```bash
MORPHO_MIDNIGHT_PARITY_MODE=solidity ./scripts/run_morpho_midnight_parity.sh
MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh
python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate
```

General repository gates:

```bash
lake build
python3 scripts/report_yul_identity_gap.py --enforce-configured-gate
python3 scripts/check_no_arg_check_scripts.py
```
