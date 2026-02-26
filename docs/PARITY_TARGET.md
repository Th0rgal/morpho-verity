# Parity Target

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document defines the planned canonical target tuple used for artifact identity checks between Verity-generated Yul and Solidity-generated Yul.

## Status

Partially implemented:
1. Canonical tuple file: `config/parity-target.json`.
2. Validation script: `scripts/check_parity_target.py`.
3. CI gate: `parity-target` job in `.github/workflows/verify.yml`.
4. Yul identity gap report script + CI artifacts: `scripts/report_yul_identity_gap.py` and `yul-identity-report` job.
5. Structural Yul AST comparator (deterministic tokenizer + delimiter-validated parser) with function-level mismatch localization.
6. Enforced unsupported-manifest drift gate (`config/yul-identity-unsupported.json`).

Not implemented yet:
1. Full semantic AST path localization (current comparator is structural token AST).
2. Blocking CI gate that fails on mismatch for the supported fragment.

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
3. `config/parity-target.json -> verity.parityPackId`: pinned Verity parity-pack binding (implemented).
4. `out/parity-target/`: generated Yul fixtures and identity reports (implemented, non-blocking).
5. `config/yul-identity-unsupported.json`: machine-tracked known unsupported function-level deltas.
6. `docs/RELEASE_CRITERIA.md`: gate definitions tied to this tuple.

## CI Expectations

1. CI prints the active tuple in logs (`parity-target` job).
2. Tuple drift between `config/parity-target.json` and `morpho-blue/foundry.toml` fails CI.
3. solc version drift fails CI.
4. CI publishes `out/parity-target/` identity artifacts (`report.json`, `normalized.diff`) for each run.
5. `report.json` includes structural AST equality status, top-level token mismatch location (token index + line/column), function-level mismatch keys, name-insensitive function-body pairing diagnostics (`functionBlocks.nameInsensitivePairs`), deterministic mismatch family grouping (`functionBlocks.familySummary`), and unsupported-manifest drift diagnostics.
6. Unsupported manifest checks also validate `parityTarget` equality with the active tuple.
7. Future strict identity checks must reference this tuple explicitly.
