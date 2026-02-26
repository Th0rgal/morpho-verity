# Parity Target (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document defines the planned canonical target tuple used for artifact identity checks between Verity-generated Yul and Solidity-generated Yul.

## Status

Groundwork only. The tuple and enforcement workflow below are proposed and must be implemented in CI before release claims depend on them.

## Purpose

Parity and identity claims are only meaningful when the Solidity compilation context is fully pinned.
This file is the single source of truth for that context.

## Proposed Canonical Tuple

1. `solcVersion`: exact version and commit hash.
2. `optimizer`: enabled/disabled + runs.
3. `viaIR`: true/false.
4. `evmVersion`: explicit value (no default inference).
5. `metadataMode`: bytecode metadata settings.
6. `stdlib/dependency revisions`: pinned commits for relevant submodules.

## Proposed Repo Artifacts

1. `config/parity-target.json`: machine-readable tuple.
2. `out/parity-target/`: generated Yul fixtures and identity reports.
3. `docs/RELEASE_CRITERIA.md`: gate that references this tuple.

## CI Expectations

1. CI prints the active tuple in logs.
2. Any tuple drift fails CI unless updated in a dedicated PR.
3. Identity checks must reference this tuple explicitly.

