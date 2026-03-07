# Verity Pin

`morpho-verity` currently pins Verity to:

- Repo: `https://github.com/Th0rgal/verity.git`
- Short rev: `9d9533b2`
- Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
- Tracking issue: `#118`

## Why this pin

This pin is the current deterministic base for morpho-verity's typed-IR
semantic-bridge work. It includes the upstream bridge surface and witness
support already consumed by the repo, including the two-storage-address proof
path used by setFeeRecipient.

## Remaining repo-local divergence at this pin

### Local generated-contract boundary

Morpho still routes through repo-local MacroSlice/Generated compiler
entrypoints instead of importing a direct upstream canonical contract
definition.

Relevant files:
- `Morpho/Compiler/MacroSlice.lean`
- `Morpho/Compiler/Generated.lean`

### Repo-local state encoding wrappers

Semantic-bridge proofs still rely on repo-local MorphoState-to-ContractState
encoders and wrapper theorems around the upstream contract semantics surface.

Relevant files:
- `Morpho/Compiler/AdminAdapters.lean`
- `Morpho/Proofs/SemanticBridgeDischarge.lean`
- `Morpho/Proofs/SemanticBridgeInstantiation.lean`

### Upstream macro/frontend gaps still block operation migration

Several operations remain blocked on current upstream frontend support for
tuple destructuring, externalCall, usable blockTimestamp values, and
dynamic-topic rawLog witnesses.

Relevant files:
- `Morpho/Compiler/MacroSlice.lean`
- `Morpho/Proofs/SemanticBridgeReadiness.lean`
- `scripts/check_macro_migration_blockers.py`

## Enforcement

The machine-readable source of truth is
`config/verity-pin-provenance.json`. CI checks that it stays in sync with
`lakefile.lean` and `lake-manifest.json` via
`scripts/check_verity_pin_provenance.py`.
