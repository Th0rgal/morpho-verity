# Architecture Review

## Current Structure

The repository now has four clear ownership zones:

| Path | Owner | Role |
|------|-------|------|
| `morpho-blue/` | Morpho upstream | Solidity reference implementation and original test suite |
| `morpho-midnight/` | Morpho upstream | Solidity reference implementation and original test suite |
| `morpho-blue-verity/` | This repository | Verity implementation, compiler adapter, and Lean proofs for Morpho Blue |
| `morpho-midnight-verity/` | This repository | Separate Verity and Lean proofs for Morpho Midnight |

The Morpho Blue comparison path is:

1. `morpho-blue/src/Morpho.sol` is the upstream reference.
2. `morpho-blue-verity/Morpho/Contract.lean` is the Verity implementation.
3. `scripts/run_morpho_blue_parity.sh` compiles the Verity artifact and runs the
   Morpho Blue test suite against it.
4. `config/parity-target.json` pins the Solidity build tuple used for parity.

Current local evidence: `lake build Morpho.Proofs` is the proof gate. The
generated-entrypoint discipline layer is explicit:
`morpho-blue-verity/Morpho/Proofs/Disciplines.lean` defines real generated-body
`Step`s and states field-movement, guard-extraction, and local no-overflow
facts as typed local axioms. The compact health, refinement, and property
theorems build against that boundary. `out/parity/morpho_blue_solidity.log` plus
`out/parity/morpho_blue_verity.log` record the latest local full-suite parity
run. On 2026-06-02 10:10 UTC both logs reported 145 passing tests, 0 failures,
and 0 skipped.

The Morpho Midnight comparison path is:

1. `morpho-midnight/src/Midnight.sol` is the upstream reference.
2. `morpho-midnight-verity/Midnight/Contract.lean` is the focused Verity proof model.
3. `scripts/run_morpho_midnight_parity.sh` patches the upstream Foundry harness
   locally and can run the original test suite with `MIDNIGHT_IMPL=solidity` or
   `MIDNIGHT_IMPL=verity`.
4. The `verity` mode requires a complete Midnight creation bytecode artifact at
   `artifacts/midnight/Midnight.bin.raw`. The current Lean package does not
   produce that full contract artifact yet.

## Strengths

- The upstream Solidity code is isolated in submodules.
- The Verity implementation is separate from the upstream code and easy to
  compare file by file.
- Lean module names remain stable (`Morpho.*` and `Midnight.*`), so existing
  build commands do not change.
- The parity commands use Morpho's original Foundry tests, with local harness
  patching instead of committed upstream-source edits.
- CI contains fail-closed checks for parity target drift, artifact layout, event
  surface drift, and script coverage.

## Current Gaps And Next Steps

0. Keep Morpho Blue parity fail-closed in CI.

   The local full-suite parity run is green, so the Blue parity workflow lane no
   longer uses `continue-on-error`. Future regressions in the Yul identity
   report, Verity artifact smoke tests, or full Morpho Blue differential suite
   should fail the workflow.

1. Maintain the mechanical Morpho Blue mapping manifest.

   `MORPH_BLUE_MAPPING.md` maps upstream Solidity functions to Verity functions,
   proof status, and test status. It is currently hand-maintained and should be
   regenerated mechanically if the source surface changes. Keep the generated
   body discipline axiom boundary visible in this manifest until those local
   obligations are replaced by smaller Lean proofs.

2. Add a Midnight comparison manifest.

   Create `docs/MORPHO_MIDNIGHT_MAPPING.md` from a script. It should map the
   requested liquidation and accounting paths from `morpho-midnight/src/Midnight.sol`
   to the focused Verity model, proof entrypoints, and remaining extraction
   obligations.

3. Produce a complete Midnight Verity artifact.

   The current Midnight package proves focused formulas and storage projections.
   Running the full upstream Midnight tests in `verity` mode requires a complete
   contract artifact that implements `IMidnight`. Once that artifact exists,
   `./scripts/run_morpho_midnight_parity.sh` is the review command.

4. Split parity tooling from general CI tooling.

   Keep generic checks in `scripts/`, but move Morpho Blue parity scripts to
   `morpho-blue-verity/scripts/` and Midnight parity scripts to
   `morpho-midnight-verity/scripts/` once CI path checks support it.

5. Make the Verity artifact bundles self-describing.

   Extend `artifacts/yul/Morpho.artifact-manifest.env` with the upstream Morpho
   Blue commit, Verity commit, solc version, optimizer settings, and the hash of
   `config/parity-target.json`.

6. Add a short review checklist for Morpho.

   Keep it mechanical:

   - initialize submodules
   - run `lake build Morpho.Proofs`
   - run `lake build Midnight.Proofs`
   - run `./scripts/run_morpho_blue_parity.sh`
   - run `MORPHO_MIDNIGHT_PARITY_MODE=solidity ./scripts/run_morpho_midnight_parity.sh`
   - if a complete Midnight artifact is available, run `MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh`
   - compare the mapping manifest
   - inspect trust boundaries

7. Keep Blue and Midnight review paths independent.

   CI can build both packages, but Blue parity docs and scripts should not depend
   on Midnight paths, and Midnight parity docs and scripts should not depend on
   Blue paths.

## License Rule

Do not copy Morpho upstream source into this repository's MIT-licensed areas.
Keep Morpho upstream code under `morpho-blue/` and `morpho-midnight/` with its
original license. If a file is translated into Verity, make that provenance
visible in the file header and in the comparison manifest.
