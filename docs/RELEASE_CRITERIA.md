# Release Criteria (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document defines proposed release gates required before making strong "safe to use" claims tied to formal proofs.

## Status

Groundwork only. These criteria are not fully enforced yet.

## Proposed Required Gates

1. `build-proofs`: `lake build` must pass on pinned toolchain.
2. `yul-identity-check`: Verity Yul AST must match Solidity Yul AST for supported contracts and pinned tuple.
3. `differential-parity`: Foundry behavior parity across Solidity baseline, Verity default, and Verity parity-pack artifacts.
4. `equivalence-obligations`: obligation table must be reported with explicit `proved` vs `assumed` counts.

## Claim Levels

1. **Model-level safety**: only Lean invariants/specs, with explicit assumptions.
2. **Artifact-level parity**: model-level + identity gate pass for pinned tuple.
3. **Release-grade claim**: artifact-level parity + no critical unresolved equivalence obligations for claimed surface.

## Documentation Requirements

1. `README.md` must include proved-vs-assumed summary.
2. `docs/PARITY_TARGET.md` must define the active tuple.
3. `docs/EQUIVALENCE_OBLIGATIONS.md` must be up to date.
4. CI artifacts must include identity report on failure.

