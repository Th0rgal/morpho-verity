# Verity Pin

morpho-verity currently pins Verity to:

- Repo: `https://github.com/Th0rgal/verity.git`
- Short rev: `00c18e3a694201cc0dfd8d8f52abaa0bf308887c`
- Full rev: `00c18e3a694201cc0dfd8d8f52abaa0bf308887c`
- Tracking issue: `#1939`

## Why this pin

This pin is the current deterministic base for morpho-verity's post-#1939
Solidity-fidelity work. It keeps the bridge surface already consumed by the repo
and adds the Verity ECM modules Morpho now uses directly, including static ABI
Keccak helpers, EIP-712 digest hashing, Solmate ERC20 transfer wrappers,
callback and bubbling low-level-call support, typed one-word oracle reads, and
Solidity-0.8 checked arithmetic helpers.

## Remaining repo-local divergence at this pin

### Local generated-contract boundary

Morpho still routes through repo-local MacroSlice/Generated compiler
entrypoints instead of importing a direct upstream canonical contract
definition.

Relevant files:
- `Morpho/Contract.lean`
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
