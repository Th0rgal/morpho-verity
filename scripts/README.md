# Scripts

This directory holds three kinds of scripts: the artifact and parity tooling that
produces and checks the compiled Verity artifact, the contract-boundary checks
that guard the proof and packaging surface, and the CI self-linting and test
files that keep the workflow honest. An auditor reviewing the verification work
needs the first two groups; the rest support CI.

## Artifact build and parity

| Script | Purpose |
|--------|---------|
| `prepare_verity_morpho_artifact.sh` | Build `Morpho.yul`, `Morpho.bin`, `Morpho.abi.json`. |
| `run_morpho_blue_parity.sh` | Run the Morpho Blue suite against both Solidity and the Verity artifact. |
| `run_morpho_midnight_parity.sh` | Run the Morpho Midnight suite against upstream Solidity or a supplied Verity artifact. |
| `report_yul_identity_gap.py` | Generate the Solidity-vs-Verity Yul identity report. |
| `apply_yul_rewrite_pipeline.py` | Apply the configured Yul rewrite stages to raw Verity Yul. |
| `check_prepared_verity_artifact_bundle.py` | Validate a prepared artifact bundle before reuse. |
| `check_parity_target.py` | Validate the pinned solc + Foundry parity tuple. |
| `check_input_mode_parity.sh` | Enforce that all three artifacts exist and are non-empty. |
| `uniquify_yul_shadows.py`, `fix_market_params_hash.py`, `keccak256.py`, `parity_target_config.py`, `patch_morpho_blue_harness.py`, `patch_morpho_midnight_harness.py` | Supporting helpers for the steps above. |

See [`../docs/CI.md`](../docs/CI.md) for the environment and timeout knobs these
scripts read, and [`../docs/PARITY_TARGET.md`](../docs/PARITY_TARGET.md) for the
pinned tuple.

## Contract and proof boundary checks

| Script | Purpose |
|--------|---------|
| `check_morpho_artifact_boundary.py` | Keep artifact packaging thin and linked externals stable. |
| `check_morpho_event_surface.py` | Check contract event metadata against Morpho Blue `EventsLib.sol`. |
| `check_artifact_layout_boundary.py` | Guard the artifact directory layout. |
| `check_duplicate_top_level_defs.py` | Reject duplicate top-level definitions. |
| `check_verity_pin_sync.py` | Keep the Verity pin in sync across the repo. |
| `check_parity_edsl_naming.py` | Enforce EDSL naming conventions. |
| `check_markdown_repo_links.py` | Validate intra-repo Markdown links. |
| `macro_blocker_regression_cases.py` | Regression cases for the contract macro. |

## Environment setup

`install_lean.sh`, `install_foundry.sh`, `install_solc.sh`,
`check_toolchain_readiness.sh`, `clear_stale_verity_lake_state.sh`, and
`run_with_timeout.sh` install toolchains and wrap long-running commands. CI
calls these; they are not needed to verify the proofs.

## CI self-linting and tests

The `check_ci_*.py` scripts lint `.github/workflows/verify.yml` itself: they
check that every referenced script exists, that each script has a matching test,
that timeout budgets fit, and that no check script runs without arguments. They
share helpers through `ci_workflow_helpers.py` and `workflow_run_parser.py`, and
are driven by `check_script_test_pairs.py`, `check_no_arg_check_scripts.py`,
`check_no_arg_shell_check_scripts.py`, and `check_shell_script_executable.py`.

The `test_*.py` and `test_*.sh` files are the unit tests for the scripts above,
discovered by CI through `python3 -m unittest discover -s scripts -p 'test_*.py'`
and the `scripts/test_*.sh` loop.
