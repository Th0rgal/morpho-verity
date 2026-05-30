# Morpho Verity

Source-shaped Morpho Blue implementation in [Verity](https://github.com/Th0rgal/verity), a Lean 4 framework for smart-contract compilation.

This repository is currently an implementation and empirical parity project. It
does not claim a complete Morpho-specific invariant proof over the compiled
artifact.

## Goal

Maintain a source-shaped Verity implementation of Morpho Blue and continuously
check it against the pinned Solidity baseline.

The legacy repo-local proof inventory has been removed. It was written for
older Verity APIs and should be redesigned cleanly against the current
`verity_contract` compiler surface rather than carried forward as stale proof
scaffolding.

### Current Guarantees

| Area | Current status | Gate/condition |
|------|----------------|----------------|
| Verity contract build | Lean elaboration/build | `lake build` succeeds |
| Verity artifact generation | Reproducible artifact preparation | `scripts/prepare_verity_morpho_artifact.sh` |
| Morpho Blue parity | Empirical/differential | `scripts/run_morpho_blue_parity.sh` |
| Yul identity | Manifest-gated | `scripts/report_yul_identity_gap.py --enforce-configured-gate` |
| External dependencies | Assumed | Oracle, IRM, ERC-20, callback, Keccak/ecrecover behavior remain environment/trust boundaries |

This repository currently makes an implementation/parity claim, not a formal
invariant-proof claim. New proofs should be added back as a fresh layer over the
canonical `verity_contract Morpho` source.

Groundwork docs:
- [`docs/PARITY_TARGET.md`](docs/PARITY_TARGET.md)
- [`docs/TRUST_BOUNDARIES.md`](docs/TRUST_BOUNDARIES.md)

Machine-readable parity target artifacts:
- [`config/parity-target.json`](config/parity-target.json)
- [`config/yul-identity-unsupported.json`](config/yul-identity-unsupported.json)
- [`config/yul-rewrite-pipeline.json`](config/yul-rewrite-pipeline.json)
- [`config/yul-rewrite-proof-obligations.json`](config/yul-rewrite-proof-obligations.json) is legacy-named rewrite-obligation metadata. It tracks planned obligations for Yul rewrite families; it is not a Morpho invariant proof inventory.
- [`scripts/check_parity_target.py`](scripts/check_parity_target.py)
- [`scripts/apply_yul_rewrite_pipeline.py`](scripts/apply_yul_rewrite_pipeline.py)
- [`scripts/report_yul_identity_gap.py`](scripts/report_yul_identity_gap.py)

## Key Modeling Notes

- `createMarket` now derives `id = marketId(params)` inside the transition (matching Solidity), so callers cannot provide arbitrary ids.
- `marketId` is no longer a constant placeholder; it is a deterministic function of `MarketParams`.
- Public market operations call `_accrueInterest` in the contract body in the
  same order as the Solidity source.

## Structure

```
morpho-blue/              # Morpho Blue Solidity (git submodule)
Morpho/
  Contract.lean           # Canonical verity_contract Morpho source
  Libraries/
    MathLib.lean          # WAD arithmetic (mulDivDown/Up, wMulDown, wTaylorCompounded)
    SharesMathLib.lean    # Share/asset conversion with virtual offset
    UtilsLib.lean         # exactlyOneZero, min, zeroFloorSub
    ConstantsLib.lean     # MAX_FEE, ORACLE_PRICE_SCALE, LIQUIDATION_CURSOR
  Compiler/
    ArtifactConfig.lean   # Artifact name, ABI boundary, and compiler packaging
    Main.lean             # Yul codegen patches for Solidity storage/event compatibility
artifacts/
  inputs/                 # Tracked external Yul libraries (MarketParamsHash, etc.)
  yul/                    # Generated Yul output (ignored)
verity-foundry/           # Foundry project for Verity-compiled artifact testing
scripts/
  prepare_verity_morpho_artifact.sh  # Build + compile Verity Morpho artifact
  run_morpho_blue_parity.sh          # Run full Morpho Blue suite (Solidity vs Verity)
```

## Build

The repository intentionally has no checked-in hand-written
`Morpho.Compiler.Spec`, `Morpho.Types`, or sidecar protocol-state model. The
compiler-facing contract is the macro-produced `Morpho.Contract.Morpho.spec`
from `Morpho/Contract.lean`; `Morpho/Compiler/ArtifactConfig.lean` only applies
artifact packaging such as the emitted name and ABI boundary. The
`scripts/check_morpho_artifact_boundary.py` gate fails closed if migration-era
compiler models, direct selector/spec surfaces, or second protocol models are
reintroduced.

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.22.0).

```
lake build
```

## Test

Initialize the Morpho Blue submodule (required for Solidity tests and parity suite):

```bash
git submodule update --init --recursive
```

Run Solidity Morpho Blue tests (default deployment path):

```bash
cd morpho-blue
forge test -vvv
```

Run the exact same Morpho Blue suite against both implementations (Solidity and Verity-compiled):

```bash
./scripts/run_morpho_blue_parity.sh
```

CI can skip the internal artifact preflight in this script by setting
`MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1` because parity is already enforced in
dedicated lanes. Outside CI, this skip is fail-closed unless explicitly
overridden with `MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1`.
CI can also bypass both preflight and prep by reusing a previously validated
artifact bundle via `MORPHO_VERITY_PREPARED_ARTIFACT_DIR`.
`MORPHO_VERITY_SKIP_PARITY_PREFLIGHT`,
`MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP`, and
`MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP` are fail-closed toggles and must be
set to `0` or `1`.
Prepared bundle reuse is also fail-closed behind
`python3 scripts/check_prepared_verity_artifact_bundle.py`, which validates the
bundle manifest, parity-pack pin, and rewrite metadata before downstream jobs
trust it.
Even in skip mode, artifact preparation stays fail-closed behind the shared
timeout wrapper via `MORPHO_VERITY_PREP_TIMEOUT_SEC` (default `8400`).
When parity preflight is enabled, the full preflight command is also bounded by
`MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC` (default `1800`).
`run_morpho_blue_parity.sh` also fails closed if the parity/prep stage does not
produce non-empty `Morpho.yul`, `Morpho.bin`, and `Morpho.abi.json` artifacts.
The full differential suite can also be fail-closed with
`MORPHO_BLUE_SUITE_TIMEOUT_SEC` (script default `0`/disabled; CI sets `3300` via `ci-timeout-defaults.env`).
`MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC` and
`MORPHO_BLUE_SUITE_TIMEOUT_SEC` must be non-negative integers.
When this guard is enabled, `timeout` must be available in `PATH`.

Validate pinned parity tuple (solc + Foundry profile):

```bash
python3 scripts/check_parity_target.py
```

Generate Solidity-vs-Verity Yul identity report artifacts:

```bash
python3 scripts/report_yul_identity_gap.py
```

Validate that current function-level Yul gaps match the tracked unsupported manifest:

```bash
python3 scripts/report_yul_identity_gap.py --enforce-unsupported-manifest
```

Honor the gate mode pinned in `config/parity-target.json` (what CI uses):

```bash
python3 scripts/report_yul_identity_gap.py --enforce-configured-gate
```

Fail closed once rewritten Verity Yul reaches exact Solidity parity:

```bash
python3 scripts/report_yul_identity_gap.py --exact
```

Build the Morpho Verity artifact:

```bash
./scripts/prepare_verity_morpho_artifact.sh
```

Artifact preparation is fail-closed for invalid toggle values and missing required tooling:
- `MORPHO_VERITY_SKIP_BUILD` / `MORPHO_VERITY_SKIP_SOLC` must be `0` or `1`.
- `MORPHO_VERITY_ARTIFACT_MODE` may be omitted or set to `edsl` only.
- `MORPHO_VERITY_INPUT_MODE` is a legacy alias; when both are set they must match.
- `python3` is required to read `config/parity-target.json` when present and to apply the Yul rewrite pipeline.
- `config/parity-target.json` must include a non-empty `verity.parityPackId`.
- `config/parity-target.json` must also pin `yulIdentity.gateMode` to `unsupported-manifest` or `exact`; CI uses that setting to decide whether Yul identity is still manifest-gated or fully exact.
- `artifacts/inputs/MarketParamsHash.yul` must be present.
- `lake` is required for compiler build/exec.
- `solc` and `awk` are required unless `MORPHO_VERITY_SKIP_SOLC=1`.

The artifact builder also emits:
- `Morpho.rewritten.yul`, produced by the configured rewrite pipeline in `config/yul-rewrite-pipeline.json`
- `Morpho.rewrite-report.json`, describing the ordered rewrite stages applied to the raw Verity Yul
- `Morpho.stage-times.log` with stage-level timing diagnostics (`lake-build`, `lake-exe`, `rewrite-yul`, `solc-bin`, or manifest reuse)
- `Morpho.artifact-manifest.env` so unchanged input sets can reuse an already-prepared artifact directory without rerunning the compiler

Generate only Yul + ABI (skip `solc` bytecode generation):

```bash
MORPHO_VERITY_SKIP_SOLC=1 ./scripts/prepare_verity_morpho_artifact.sh
```

Override artifact output directory when needed:

```bash
MORPHO_VERITY_OUT_DIR=/tmp/morpho-artifact ./scripts/prepare_verity_morpho_artifact.sh
```

Validate a prepared artifact bundle before reusing it:

```bash
python3 scripts/check_prepared_verity_artifact_bundle.py --artifact-dir out/parity-shared --require-rewrite
```

Prepared rewrite bundles are validated fail-closed against manifest paths and
manifest content digests. If `config/yul-rewrite-pipeline.json` changes in
place, an older `Morpho.rewrite-report.json` will no longer be accepted for
reuse.

Enforce artifact readiness for generated Morpho artifacts (`Morpho.yul`, `Morpho.bin`, `Morpho.abi.json`):

```bash
./scripts/check_input_mode_parity.sh
```

The artifact gate is fail-closed: all three artifacts must exist and be non-empty.
Artifact preparation is also fail-closed with a timeout guard (`MORPHO_VERITY_PREP_TIMEOUT_SEC`, default `8400`; set `0` to disable). When this guard is enabled, `setsid` must be available in `PATH`.
When `MORPHO_VERITY_PREPARED_ARTIFACT_DIR` is set, the differential runner
uses that validated bundle (`Morpho.yul`, `Morpho.bin`, `Morpho.abi.json`) and
still fails closed if any file is missing or empty.
Workflow long-lane commands also use fail-closed timeout guards via a shared timeout wrapper:
- `MORPHO_LEAN_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_VERITY_BUILD_TIMEOUT_SEC` (default `2400`)
- `MORPHO_VERITY_MAINTEST_TIMEOUT_SEC` (default `300`)
- `MORPHO_FOUNDRY_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_SOLC_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC` (default `300`)
- `MORPHO_PARITY_TARGET_TEST_TIMEOUT_SEC` (default `900`)
- `MORPHO_TIMEOUT_WRAPPER_TEST_TIMEOUT_SEC` (default `180`)
- `MORPHO_VERITY_PREP_TIMEOUT_SEC` (default `8400`)
- `MORPHO_VERITY_PREPARED_ARTIFACT_DIR` (optional path; reuse validated EDSL artifacts)
- `MORPHO_SOLIDITY_IR_BUILD_TIMEOUT_SEC` (default `900`)
- `MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC` (default `9000`)
- `MORPHO_BLUE_PARITY_SCRIPT_TIMEOUT_SEC` (default `6900`)
- `MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC` (default `1800`)
- `MORPHO_YUL_IDENTITY_TIMEOUT_SEC` (default `9000`)
- `MORPHO_SOLIDITY_TEST_TIMEOUT_SEC` (default `5100`)
- `MORPHO_VERITY_SMOKE_TIMEOUT_SEC` (default `3000`)
- `MORPHO_TIMEOUT_KILL_AFTER_SEC` (default `30`)
- `0` disables timeout for each respective command
The shared timeout wrapper enforces hard fail-closed termination by running the command in its own session via `setsid --wait` and escalating from `TERM` to `KILL` after `${MORPHO_TIMEOUT_KILL_AFTER_SEC:-30}s`, so TERM-ignoring subprocesses cannot hang CI indefinitely.
`MORPHO_TIMEOUT_KILL_AFTER_SEC` must stay strictly greater than `0` to preserve hard-kill fail-closed behavior.
The Yul identity report script wraps both internal Solidity IR build and Verity artifact-prep sub-steps with this same timeout wrapper (`MORPHO_SOLIDITY_IR_BUILD_TIMEOUT_SEC`, `MORPHO_VERITY_PREP_TIMEOUT_SEC`) so long sub-step stalls fail closed with stage-specific diagnostics. When `MORPHO_VERITY_PREPARED_ARTIFACT_DIR` is provided, the report reuses that validated bundle and still fails closed if `Morpho.yul` is missing.
CI sets stricter non-conflicting outer budgets for nested timeout-wrapped stages:
- `MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=9000` with `MORPHO_VERITY_PREP_TIMEOUT_SEC=8400`
- `MORPHO_YUL_IDENTITY_TIMEOUT_SEC=9000` with `MORPHO_VERITY_PREP_TIMEOUT_SEC=8400`

Compile using a specific Verity parity pack:

```bash
MORPHO_VERITY_PARITY_PACK=solc-0.8.28-o999999-viair-true-evm-paris \
  ./scripts/prepare_verity_morpho_artifact.sh
```

Run Foundry smoke tests on the compiled Verity artifact:

```bash
cd verity-foundry
forge test -vvv
```

Current status:
- `./scripts/run_morpho_blue_parity.sh` passes `MORPHO_IMPL=solidity|verity` into the Morpho Blue suite and now fails closed unless `morpho-blue/test/BaseTest.sol` reads that selector via an explicit Foundry env lookup and no test bypasses it with direct `new Morpho(...)` deployments.
- The currently checked-in `morpho-blue` submodule is wired for the selector under the `difftest` Foundry profile, including artifact read permissions and Shanghai opcode support for the generated Verity bytecode.
- Differential pass/fail depends on the currently checked-out `morpho-blue` submodule revision; use the logs under `out/parity/` as the source of truth for a given run.
- `scripts/report_yul_identity_gap.py` emits machine-readable identity artifacts under `out/parity-target/` (`report.json` + `normalized.diff`) including structural-AST mismatch localization, name-insensitive function-body pairing diagnostics, deterministic mismatch family grouping, and a rewrite-oriented prioritization view. The report compares Solidity IR against the rewritten Verity artifact, persists both `verity/Morpho.raw.yul` and `verity/Morpho.yul`, and records the ordered rewrite pipeline under `rewritePipeline`.

## Status

- [x] Canonical macro-native `verity_contract Morpho` source
- [x] Core contract logic (supply, withdraw, borrow, repay, liquidate, supplyCollateral, withdrawCollateral, createMarket, setAuthorization, setAuthorizationWithSig, owner functions, interest accrual, flash loans)
- [x] Math libraries (MathLib, SharesMathLib, UtilsLib, ConstantsLib)
- [x] Verity artifact generation
- [x] Morpho Blue differential test harness
- [ ] Rebuild formal proofs from semantics extracted from the canonical `verity_contract Morpho`

The last item is intentionally unchecked. The current checked-in proof-like Lean
content is limited to small library facts needed by the implementation; the old
Morpho invariant-proof layer has been removed. Future protocol reasoning should
derive from the executable contract source rather than from a parallel
hand-written Morpho state model.

## License

MIT
