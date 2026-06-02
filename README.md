# Morpho Verity

Lean 4 and Verity work for Morpho protocol properties.

The repository keeps Morpho's upstream Solidity code separate from this
repository's Verity work:

| Path | Purpose | License |
|------|---------|---------|
| `morpho-blue/` | Morpho Blue upstream submodule. This is the Solidity reference and original test suite. | Morpho upstream license, kept in `morpho-blue/LICENSE` |
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

```bash
lake build Midnight.Proofs
```

## Compare With Morpho Blue

Start from these files:

| Morpho upstream | Verity implementation |
|-----------------|-----------------------|
| `morpho-blue/src/Morpho.sol` | `morpho-blue-verity/Morpho/Contract.lean` |
| `morpho-blue/src/libraries/MathLib.sol` | `morpho-blue-verity/Morpho/Libraries/MathLib.lean` |
| `morpho-blue/src/libraries/SharesMathLib.sol` | `morpho-blue-verity/Morpho/Libraries/SharesMathLib.lean` |
| `morpho-blue/src/libraries/ConstantsLib.sol` | `morpho-blue-verity/Morpho/Libraries/ConstantsLib.lean` |
| `morpho-blue/src/libraries/UtilsLib.sol` | `morpho-blue-verity/Morpho/Libraries/UtilsLib.lean` |

Proof entrypoints:

- `morpho-blue-verity/Morpho/Proofs/Property1.lean`
- `morpho-blue-verity/Morpho/Proofs/Property2.lean`
- `morpho-blue-verity/Morpho/Proofs/Refinement.lean`

Operational docs:

- `docs/PARITY_TARGET.md`
- `docs/CI.md`
- `docs/TRUST_BOUNDARIES.md`
- `docs/ARCHITECTURE_REVIEW.md`

## License

This repository's original work is licensed under MIT. Third-party code keeps
its original license. In particular, `morpho-blue/` is Morpho upstream code and
is governed by `morpho-blue/LICENSE`, not by this repository's MIT license.
