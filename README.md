# Morpho Verity

Lean 4 and Verity work for Morpho protocol properties.

The repository keeps Morpho's upstream Solidity code separate from this
repository's Verity work:

| Path | Purpose | License |
|------|---------|---------|
| `morpho-blue/` | Morpho Blue upstream submodule. This is the Solidity reference and original test suite. | Morpho upstream license, kept in `morpho-blue/LICENSE` |
| `morpho-midnight/` | Morpho Midnight upstream submodule. This is the Solidity reference and original test suite. | Morpho upstream license, kept in `morpho-midnight/LICENSE` and `morpho-midnight/LICENSE-SECONDARY` |
| `morpho-blue-verity/` | Verity implementation, compiler adapter, and Lean proofs for Morpho Blue. | This repository's license |
| `morpho-midnight-verity/` | Separate Verity and Lean proof package for Morpho Midnight. | This repository's license |
| `artifacts/`, `config/`, `scripts/`, `docs/` | Reproducibility, parity, CI, and audit-support tooling. | This repository's license |

## Verify Morpho Blue

Install submodules:

```bash
git submodule update --init --recursive
```

Build the Verity model and proofs:

```bash
lake build Morpho.Proofs
```

Run Morpho Blue's original tests against the Verity-compiled artifact:

```bash
./scripts/run_morpho_blue_parity.sh
```

The parity target is pinned in `config/parity-target.json`. The target tuple is
checked against `morpho-blue/foundry.toml`.

## Verify Morpho Midnight

Build the focused Midnight proof package:

```bash
lake build Midnight.Proofs
```

Run Morpho Midnight's original Foundry tests against upstream Solidity:

```bash
MORPHO_MIDNIGHT_PARITY_MODE=solidity ./scripts/run_morpho_midnight_parity.sh
```

Run the same original tests against a complete Verity-compiled Midnight
creation bytecode artifact:

```bash
MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh
```

That command expects `artifacts/midnight/Midnight.bin.raw`, or a file supplied
through `MORPHO_MIDNIGHT_ARTIFACT_RAW`. The current Midnight package is a
focused proof model for liquidation and slashing arithmetic. It is not yet a
complete compiled replacement for `morpho-midnight/src/Midnight.sol`.

## Compare With Morpho Blue

Start from these files:

| Morpho upstream | Verity implementation |
|-----------------|-----------------------|
| `morpho-blue/src/Morpho.sol` | `morpho-blue-verity/Morpho/Contract.lean` |
| `morpho-blue/src/libraries/MathLib.sol` | `morpho-blue-verity/Morpho/Libraries/MathLib.lean` |
| `morpho-blue/src/libraries/SharesMathLib.sol` | `morpho-blue-verity/Morpho/Libraries/SharesMathLib.lean` |
| `morpho-blue/src/libraries/ConstantsLib.sol` | `morpho-blue-verity/Morpho/Libraries/ConstantsLib.lean` |
| `morpho-blue/src/libraries/UtilsLib.sol` | `morpho-blue-verity/Morpho/Libraries/UtilsLib.lean` |

## Compare With Morpho Midnight

Start from these files:

| Morpho upstream | Verity implementation |
|-----------------|-----------------------|
| `morpho-midnight/src/Midnight.sol` | `morpho-midnight-verity/Midnight/Contract.lean` |
| `morpho-midnight/src/interfaces/IMidnight.sol` | `morpho-midnight-verity/Midnight/Proofs/Storage.lean` |
| `morpho-midnight/src/libraries/ConstantsLib.sol` | constants in `morpho-midnight-verity/Midnight/Contract.lean` and proof files |
| `morpho-midnight/src/libraries/UtilsLib.sol` | arithmetic lemmas in `morpho-midnight-verity/Midnight/Proofs/Basic.lean` |

Proof entrypoints:

- `morpho-blue-verity/Morpho/Proofs/Property1.lean`
- `morpho-blue-verity/Morpho/Proofs/Property2.lean`
- `morpho-blue-verity/Morpho/Proofs/Refinement.lean`
- `morpho-midnight-verity/Midnight/Proofs/RCF.lean`
- `morpho-midnight-verity/Midnight/Proofs/UnitsAccounting.lean`

Operational docs:

- `docs/PARITY_TARGET.md`
- `docs/CI.md`
- `docs/TRUST_BOUNDARIES.md`
- `docs/ARCHITECTURE_REVIEW.md`

## License

This repository's original work is licensed under MIT. Third-party code keeps
its original license. In particular, `morpho-blue/` and `morpho-midnight/` are
Morpho upstream code and are governed by their own license files.
