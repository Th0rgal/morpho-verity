# Release Criteria

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document defines proposed release gates required before making strong "safe to use" claims tied to formal proofs.

## Status

Partially enforced:
1. `build-proofs` (`verity-proofs` job) is enforced in CI.
2. `differential-parity` (`morpho-blue-parity` job) is enforced in CI.
3. `parity-target` tuple drift gate is enforced in CI.
4. `unsupported-gap-manifest` drift gate is enforced in CI (`yul-identity-report` with `--enforce-unsupported-manifest`).
5. `yul-identity-report` emits structural AST diagnostics in CI artifacts.
6. `macro-migration-blockers` drift gate is enforced in CI (`scripts/check_macro_migration_blockers.py`).
7. Long differential lane reuses a verified EDSL artifact bundle from `verity-compiled-tests` (reduced duplicate prep/timeout surface).
8. EDSL-only parity naming gate is enforced in CI (`scripts/check_parity_edsl_naming.py`).

Not yet enforced:
1. strict `yul-identity-check` (zero structural AST mismatch for supported fragment).
2. machine-tracked `equivalence-obligations` status in CI output.

## Required Gates (Target State)

1. `build-proofs`: `lake build` must pass on pinned toolchain.
2. `yul-identity-check`: Verity Yul AST must match Solidity Yul AST for supported contracts and pinned tuple.
3. `differential-parity`: Foundry behavior parity across Solidity baseline, Verity default, and Verity parity-pack artifacts.
4. `equivalence-obligations`: obligation table must be reported with explicit `proved` vs `assumed` counts.
5. `parity-target`: tuple drift checks against `config/parity-target.json` must pass.
6. `unsupported-gap-manifest`: function-level mismatch keys must match `config/yul-identity-unsupported.json` for non-identity phases.
7. `macro-migration-blockers`: unsupported constructor usage in `Morpho/Compiler/Spec.lean` must match reviewed baseline in `config/macro-migration-blockers.json`.

## Claim Levels

1. **Model-level safety**: only Lean invariants/specs, with explicit assumptions.
2. **Artifact-level parity**: model-level + identity gate pass for pinned tuple.
3. **Release-grade claim**: artifact-level parity + no critical unresolved equivalence obligations for claimed surface.

## Documentation Requirements

1. `README.md` must include proved-vs-assumed summary.
2. `docs/PARITY_TARGET.md` must define the active tuple.
3. `docs/EQUIVALENCE_OBLIGATIONS.md` must be up to date.
4. CI artifacts must include identity report on failure.
