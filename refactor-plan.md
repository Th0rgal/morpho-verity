# Morpho Verity Source-Of-Truth Audit

## Target Shape

`Morpho/Contract.lean` is the single Morpho source of truth. Its
`verity_contract Morpho` declaration is simultaneously:

- the executable Verity implementation,
- the compiler-facing contract surface,
- the protocol-level specification surface exposed to reviewers, and
- the source from which future proof semantics must be extracted.

No checked-in Lean file should define a second Morpho `CompilationModel`,
selector list, or sidecar protocol implementation that can drift from
`Morpho.Contract.Morpho.spec`.

## Current Layer Names

- `Morpho/Contract.lean`: canonical contract source.
- `Morpho/Libraries/*.lean`: translated helper libraries used by the canonical
  contract.
- `Morpho/Compiler/ArtifactConfig.lean`: artifact-packaging adapter over
  `Morpho.Contract.Morpho.spec`.
- `Morpho/Compiler/Main.lean`: artifact builder and codegen configuration.
- `verity-foundry/` and `scripts/run_morpho_blue_parity.sh`: test harness
  support.
- `docs/TRUST_BOUNDARIES.md` and future proof modules: proof infrastructure and
  assumption inventory.

## Removed Migration Artifacts

The legacy hand-written compiler model and migration slice have been removed:

- `Morpho/Compiler/Spec.lean`
- `Morpho/Compiler/MacroSlice.lean`
- `Morpho/Compiler/Generated.lean`
- `Morpho/Types.lean`
- `scripts/check_macro_migration_surface.py`
- `scripts/check_macro_migration_slice.py`
- `scripts/check_morpho_selectors_sync.py`
- `config/macro-migration-*.json`

The CI workflow now relies on `scripts/check_morpho_artifact_boundary.py` to
reject reintroduction of those files and to ensure compiler packaging goes
through `Morpho.Contract.Morpho.spec`.

## Completion Checklist

- [x] Canonical source is `verity_contract Morpho` in `Morpho/Contract.lean`.
- [x] Artifact packaging is explicitly named in `Morpho/Compiler/ArtifactConfig.lean`
  and `Morpho/Compiler/Main.lean`.
- [x] Test harness support is isolated under `verity-foundry/` and parity
  scripts.
- [x] Proof infrastructure is documented as future work over the generated
  contract source, not as an existing full invariant-proof claim.
- [x] Legacy second-model files are absent and guarded by CI.
