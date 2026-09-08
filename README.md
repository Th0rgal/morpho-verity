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
skipped on 2026-06-05 17:54 Europe/Berlin. The mechanical review manifest is
`MORPH_BLUE_MAPPING.md`.

## Verify Morpho Midnight

Pinned Solidity → typed solc AST/storage layout → generated Lean model → Yul →
bytecode. The full artifact uses `Midnight/Generated/FullModel.lean`, not a merge
of handwritten bodies. Prerequisites: Node 20+, Python 3, Lean (repository pin),
Foundry, and initialized submodules (`git submodule update --init --recursive`).
The importer fetches and checksum-verifies its pinned solc.

```bash
node scripts/report_midnight_support.mjs          # representation / proof gaps
node scripts/report_midnight_support.mjs --json   # origins, callers, tests, actions
python3 scripts/verify_midnight_pipeline.py       # rebuild + test fresh bytecode
```

The verifier writes `out/midnight/`: command logs, support report, source
and compiler pins, bytecode hashes/size, and actual test results. CI uploads this
bundle as `midnight-build`, including failure evidence. No checked-in test
snapshot is a substitute for rerunning it.

**Limits:** test success is not source-to-model equivalence. Custom ABI/memory,
opcode and compiler-check policies remain conditional; current bytecode exceeds
normal Ethereum deployment limits. See the Midnight section of
[`docs/TRUST_BOUNDARIES.md`](docs/TRUST_BOUNDARIES.md).

The separate focused proofs remain available via `lake build Midnight.Proofs`
and `./scripts/prepare_focused_midnight_artifact.sh`; they do not prove the full
generated implementation. To prepare only the full artifact, run
`./scripts/prepare_midnight_artifact.sh`.

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
| `morpho-midnight/src/Midnight.sol` and reachable libraries | `morpho-midnight-verity/Midnight/Generated/FullModel.lean` and its source manifest |
| `morpho-midnight/src/interfaces/IMidnight.sol` | `morpho-midnight-verity/Midnight/Generated/FullModel.abi.json` |
| Focused liquidation/accounting properties (separate model) | `morpho-midnight-verity/Midnight/Proofs/` |

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
