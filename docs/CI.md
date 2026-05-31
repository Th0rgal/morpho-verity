# CI, Artifacts, and Configuration

Operational reference for building the Verity artifact, running the parity and
Yul-identity gates, and the environment/timeout knobs CI uses. None of this is
needed to verify the proofs — for that see the top-level `README.md`.

## Artifact build

Build the Morpho Verity artifact (`Morpho.yul`, `Morpho.bin`, `Morpho.abi.json`):

```bash
./scripts/prepare_verity_morpho_artifact.sh
```

Generate only Yul + ABI (skip `solc` bytecode generation):

```bash
MORPHO_VERITY_SKIP_SOLC=1 ./scripts/prepare_verity_morpho_artifact.sh
```

Override artifact output directory:

```bash
MORPHO_VERITY_OUT_DIR=/tmp/morpho-artifact ./scripts/prepare_verity_morpho_artifact.sh
```

Compile using a specific Verity parity pack:

```bash
MORPHO_VERITY_PARITY_PACK=solc-0.8.28-o999999-viair-true-evm-paris \
  ./scripts/prepare_verity_morpho_artifact.sh
```

The artifact builder also emits:

- `Morpho.rewritten.yul`, produced by the configured rewrite pipeline in `config/yul-rewrite-pipeline.json`
- `Morpho.rewrite-report.json`, describing the ordered rewrite stages applied to the raw Verity Yul
- `Morpho.stage-times.log` with stage-level timing diagnostics (`lake-build`, `lake-exe`, `rewrite-yul`, `solc-bin`, or manifest reuse)
- `Morpho.artifact-manifest.env` so unchanged input sets can reuse an already-prepared artifact directory without rerunning the compiler

Artifact preparation is fail-closed for invalid toggle values and missing tooling:

- `MORPHO_VERITY_SKIP_BUILD` / `MORPHO_VERITY_SKIP_SOLC` must be `0` or `1`.
- `MORPHO_VERITY_ARTIFACT_MODE` may be omitted or set to `edsl` only.
- `MORPHO_VERITY_INPUT_MODE` is a legacy alias; when both are set they must match.
- `python3` is required to read `config/parity-target.json` and to apply the Yul rewrite pipeline.
- `config/parity-target.json` must include a non-empty `verity.parityPackId`.
- `config/parity-target.json` must pin `yulIdentity.gateMode` to `unsupported-manifest` or `exact`; CI uses that to decide whether Yul identity is still manifest-gated or fully exact.
- `artifacts/inputs/MarketParamsHash.yul` must be present.
- `lake` is required for compiler build/exec.
- `solc` and `awk` are required unless `MORPHO_VERITY_SKIP_SOLC=1`.

Validate a prepared artifact bundle before reusing it:

```bash
python3 scripts/check_prepared_verity_artifact_bundle.py --artifact-dir out/parity-shared --require-rewrite
```

Prepared rewrite bundles are validated fail-closed against manifest paths and
content digests. If `config/yul-rewrite-pipeline.json` changes in place, an older
`Morpho.rewrite-report.json` is no longer accepted for reuse.

Enforce artifact readiness (all three artifacts must exist and be non-empty):

```bash
./scripts/check_input_mode_parity.sh
```

## Parity target and Yul identity

Validate the pinned parity tuple (solc + Foundry profile):

```bash
python3 scripts/check_parity_target.py
```

Generate Solidity-vs-Verity Yul identity report artifacts (under `out/parity-target/`):

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

The report compares Solidity IR against the rewritten Verity artifact, persists
both `verity/Morpho.raw.yul` and `verity/Morpho.yul`, and records the ordered
rewrite pipeline under `rewritePipeline`. Pass/fail of the differential suite
depends on the checked-out `morpho-blue` submodule revision; treat the logs under
`out/parity/` as the source of truth for a given run.

## Differential suite knobs

`run_morpho_blue_parity.sh` passes `MORPHO_IMPL=solidity|verity` into the Morpho
Blue suite and fails closed unless `morpho-blue/test/BaseTest.sol` reads that
selector via an explicit Foundry env lookup (no test may bypass it with a direct
`new Morpho(...)` deployment). The checked-in submodule is wired for the selector
under the `difftest` Foundry profile, with artifact read permissions and Shanghai
opcode support for the generated Verity bytecode.

CI can skip the internal artifact preflight by setting
`MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1` because parity is enforced in dedicated
lanes. Outside CI this skip is fail-closed unless explicitly overridden with
`MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1`. CI can also bypass both
preflight and prep by reusing a previously validated bundle via
`MORPHO_VERITY_PREPARED_ARTIFACT_DIR`. `MORPHO_VERITY_SKIP_PARITY_PREFLIGHT`,
`MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP`, and
`MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP` are fail-closed toggles and must be set
to `0` or `1`. Prepared bundle reuse is fail-closed behind
`python3 scripts/check_prepared_verity_artifact_bundle.py`, which validates the
bundle manifest, parity-pack pin, and rewrite metadata before downstream jobs
trust it. `run_morpho_blue_parity.sh` also fails closed if the parity/prep stage
does not produce non-empty `Morpho.yul`, `Morpho.bin`, and `Morpho.abi.json`.

## Timeout guards

Long-lane commands use fail-closed timeout guards via a shared timeout wrapper.
`0` disables the timeout for the respective command.

| Variable | Default (s) |
|----------|-------------|
| `MORPHO_LEAN_INSTALL_TIMEOUT_SEC` | 600 |
| `MORPHO_VERITY_BUILD_TIMEOUT_SEC` | 2400 |
| `MORPHO_VERITY_MAINTEST_TIMEOUT_SEC` | 300 |
| `MORPHO_FOUNDRY_INSTALL_TIMEOUT_SEC` | 600 |
| `MORPHO_SOLC_INSTALL_TIMEOUT_SEC` | 600 |
| `MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC` | 300 |
| `MORPHO_PARITY_TARGET_TEST_TIMEOUT_SEC` | 900 |
| `MORPHO_TIMEOUT_WRAPPER_TEST_TIMEOUT_SEC` | 180 |
| `MORPHO_VERITY_PREP_TIMEOUT_SEC` | 8400 |
| `MORPHO_SOLIDITY_IR_BUILD_TIMEOUT_SEC` | 900 |
| `MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC` | 9000 |
| `MORPHO_BLUE_PARITY_SCRIPT_TIMEOUT_SEC` | 6900 |
| `MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC` | 1800 |
| `MORPHO_YUL_IDENTITY_TIMEOUT_SEC` | 9000 |
| `MORPHO_SOLIDITY_TEST_TIMEOUT_SEC` | 5100 |
| `MORPHO_VERITY_SMOKE_TIMEOUT_SEC` | 3000 |
| `MORPHO_BLUE_SUITE_TIMEOUT_SEC` | 0 (CI sets 3300 via `ci-timeout-defaults.env`) |
| `MORPHO_TIMEOUT_KILL_AFTER_SEC` | 30 |

`MORPHO_VERITY_PREPARED_ARTIFACT_DIR` is an optional path for reusing validated
EDSL artifacts. The shared wrapper enforces hard fail-closed termination by
running each command in its own session via `setsid --wait`, escalating from
`TERM` to `KILL` after `MORPHO_TIMEOUT_KILL_AFTER_SEC` (which must stay strictly
greater than `0`). When a timeout guard is enabled, `timeout`/`setsid` must be on
`PATH`. Timeout-valued variables must be non-negative integers. CI sets stricter
non-conflicting outer budgets for nested stages, e.g.
`MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=9000` with
`MORPHO_VERITY_PREP_TIMEOUT_SEC=8400`, and `MORPHO_YUL_IDENTITY_TIMEOUT_SEC=9000`
with `MORPHO_VERITY_PREP_TIMEOUT_SEC=8400`.

## Foundry smoke tests

Run Foundry smoke tests on the compiled Verity artifact:

```bash
cd verity-foundry
forge test -vvv
```

Run the Solidity Morpho Blue tests directly:

```bash
cd morpho-blue
forge test -vvv
```
