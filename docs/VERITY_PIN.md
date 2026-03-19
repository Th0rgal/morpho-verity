# Verity Pin

`morpho-verity` currently pins Verity to:

- Repo: `https://github.com/Th0rgal/verity.git`
- Short rev: `4ebe4931`
- Full rev: `4ebe4931d25e5a1594fcd3f43ff040ecc3c4225a`
- Tracking issue: `#118`

## Why this pin

This pin is the current deterministic base for morpho-verity's typed-IR
semantic-bridge work. It keeps the upstream bridge surface already consumed by
the repo and adds the newer macro/frontend features Morpho now uses directly,
including linked externals, direct ERC20 helper syntax, executable tuple
params, struct-mapping storage declarations, internal calls, and
macro-backed `ecrecover` support.

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

## Enforcement

The machine-readable source of truth is
`config/verity-pin-provenance.json`. CI checks that it stays in sync with
`lakefile.lean` and `lake-manifest.json` via
`scripts/check_verity_pin_provenance.py`.
