# Verity Pin

`morpho-verity` currently pins Verity to:

- Repo: `https://github.com/Th0rgal/verity.git`
- Short rev: `f026ac0a`
- Full rev: `f026ac0a2e584638841f988ca05d9e0d0008ee91`
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

### Upstream macro/frontend gaps still block operation migration

Several operations remain blocked at the current pin on callbacks,
target-aware external contract calls, and dynamic-topic
`rawLog` witnesses.

Current blocker families at this pin:
- callbacks
- target-aware external contract calls
- dynamic-topic `rawLog` witnesses

Tracked migration issue clusters:
- `#123`
- `#124`

Relevant files:
- `Morpho/Compiler/MacroSlice.lean`
- `Morpho/Proofs/SemanticBridgeReadiness.lean`
- `scripts/check_macro_migration_blockers.py`

## Enforcement

The machine-readable source of truth is
`config/verity-pin-provenance.json`. CI checks that it stays in sync with
`lakefile.lean` and `lake-manifest.json` via
`scripts/check_verity_pin_provenance.py`.
