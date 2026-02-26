# Parity Target

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document defines the planned canonical target tuple used for artifact identity checks between Verity-generated Yul and Solidity-generated Yul.

## Status

Partially implemented:
1. Canonical tuple file: `config/parity-target.json`.
2. Validation script: `scripts/check_parity_target.py`.
3. CI gate: `parity-target` job in `.github/workflows/verify.yml`.

Not implemented yet:
1. Yul AST identity comparison artifacts under `out/parity-target/`.
2. CI failure artifacts with mismatch localization.

## Purpose

Parity and identity claims are only meaningful when the Solidity compilation context is fully pinned.
This file is the single source of truth for that context.

## Canonical Tuple Fields

1. `solcVersion`: exact version and commit hash.
2. `optimizer`: enabled/disabled + runs.
3. `viaIR`: true/false.
4. `evmVersion`: explicit value (no default inference).
5. `metadataMode`: bytecode metadata settings.
6. `stdlib/dependency revisions`: pinned commits for relevant submodules.

## Repo Artifacts

1. `config/parity-target.json`: machine-readable tuple (implemented).
2. `scripts/check_parity_target.py`: tuple drift checker (implemented).
3. `out/parity-target/`: generated Yul fixtures and identity reports (planned).
4. `docs/RELEASE_CRITERIA.md`: gate definitions tied to this tuple.

## CI Expectations

1. CI prints the active tuple in logs (`parity-target` job).
2. Tuple drift between `config/parity-target.json` and `morpho-blue/foundry.toml` fails CI.
3. solc version drift fails CI.
4. Future identity checks must reference this tuple explicitly.
