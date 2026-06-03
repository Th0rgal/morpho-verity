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

The generated-entrypoint refinement surface is explicit: `Morpho.Proofs`
builds against typed local discipline obligations in
`morpho-blue-verity/Morpho/Proofs/Disciplines.lean` for generated-body field
movement, health guards, and local no-overflow facts. The compact health,
refinement, and property theorems are checked by Lean; replacing those
generated-body obligations with smaller internal proofs is still tracked as a
proof-engineering task.

Run Morpho Blue's original tests against the Verity-compiled artifact:

```bash
./scripts/run_morpho_blue_parity.sh
```

The parity target is pinned in `config/parity-target.json`. The target tuple is
checked against `morpho-blue/foundry.toml`. The latest local parity evidence in
this workspace is green: both `out/parity/morpho_blue_solidity.log` and
`out/parity/morpho_blue_verity.log` report 145 passing tests, 0 failures, and 0
skipped on 2026-06-02 10:10 UTC. The mechanical review manifest is
`MORPH_BLUE_MAPPING.md`.

## Verify Morpho Midnight

Build the focused Midnight proof package:

```bash
lake build Midnight.Proofs
```

Build the executable artifact for the focused `MidnightRCF` proof model:

```bash
./scripts/prepare_focused_midnight_artifact.sh
```

This emits `artifacts/midnight-focused/MidnightRCF.yul`,
`MidnightRCF.abi.json`, and `MidnightRCF.bin.raw`. It is executable bytecode for
the focused proof model, not a full `IMidnight` implementation.

Run Morpho Midnight's original Foundry tests against upstream Solidity:

```bash
MORPHO_MIDNIGHT_PARITY_MODE=solidity ./scripts/run_morpho_midnight_parity.sh
```

Build the complete Verity-compiled Midnight artifact:

```bash
./scripts/prepare_midnight_artifact.sh
```

Run the same original tests against that Verity artifact:

```bash
MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh
```

That command expects `artifacts/midnight/Midnight.bin.raw`, or a file supplied
through `MORPHO_MIDNIGHT_ARTIFACT_RAW`. The current local parity evidence is
green: both Solidity and Verity Midnight modes report 373 passing tests, 0
failures, and 0 skipped.

Midnight Yul identity is not exact. The checked gate is a fail-closed drift
manifest:

```bash
python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate
```

That report compares Solidity `Midnight.sol` `irOptimized` Yul with
`artifacts/midnight/Midnight.yul` and requires the function-level drift to match
`config/midnight-yul-identity-unsupported.json`.

The current Midnight mapping manifest is `MORPHO_MIDNIGHT_MAPPING.md`; validate
it with:

```bash
python3 scripts/check_morpho_midnight_mapping.py
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

- `MORPH_BLUE_MAPPING.md`
- `MORPHO_MIDNIGHT_MAPPING.md`
- `docs/MIDNIGHT_VERITY_PLAN.md`
- `docs/PARITY_TARGET.md`
- `docs/CI.md`
- `docs/TRUST_BOUNDARIES.md`
- `docs/ARCHITECTURE_REVIEW.md`

## License

This repository's original work is licensed under MIT. Third-party code keeps
its original license. In particular, `morpho-blue/` and `morpho-midnight/` are
Morpho upstream code and are governed by their own license files.
