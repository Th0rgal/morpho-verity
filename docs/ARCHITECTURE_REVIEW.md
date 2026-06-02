# Architecture Review

## Current Structure

The repository now has three clear ownership zones:

| Path | Owner | Role |
|------|-------|------|
| `morpho-blue/` | Morpho upstream | Solidity reference implementation and original test suite |
| `morpho-blue-verity/` | This repository | Verity implementation, compiler adapter, and Lean proofs for Morpho Blue |
| `morpho-midnight-verity/` | This repository | Separate Verity and Lean proofs for Morpho Midnight |

The Morpho Blue comparison path is:

1. `morpho-blue/src/Morpho.sol` is the upstream reference.
2. `morpho-blue-verity/Morpho/Contract.lean` is the Verity implementation.
3. `scripts/run_morpho_blue_parity.sh` compiles the Verity artifact and runs the
   Morpho Blue test suite against it.
4. `config/parity-target.json` pins the Solidity build tuple used for parity.

## Strengths

- The upstream Solidity code is isolated in its own submodule.
- The Verity implementation is separate from the upstream code and easy to
  compare file by file.
- Lean module names remain stable (`Morpho.*` and `Midnight.*`), so existing
  build commands do not change.
- The parity command uses Morpho Blue's original Foundry tests, not a forked
  test suite.
- CI contains fail-closed checks for parity target drift, artifact layout, event
  surface drift, and script coverage.

## Proposed Next Steps

0. Fix full Morpho Blue parity before external handoff.

   The repository has the right parity command, but the current Verity artifact
   does not yet pass the full Morpho Blue suite. The latest local run completed
   the Solidity reference pass, then failed on the Verity pass with 95 passing
   tests and 50 failing tests. Do not present the implementation as full-test
   compatible until this is green.

1. Add a generated comparison manifest.

   Create `docs/MORPHO_BLUE_MAPPING.md` from a script. It should map each
   upstream Solidity file, function, event, and library to its Verity file and
   proof coverage status. This gives Morpho a review checklist instead of a
   narrative.

2. Split parity tooling from general CI tooling.

   Keep generic checks in `scripts/`, but move Morpho Blue parity scripts to
   `morpho-blue-verity/scripts/` once CI path checks support it. That would make
   ownership even clearer.

3. Make the Verity artifact bundle self-describing.

   Extend `artifacts/yul/Morpho.artifact-manifest.env` with the upstream Morpho
   Blue commit, Verity commit, solc version, optimizer settings, and the hash of
   `config/parity-target.json`.

4. Add a short "how to review" checklist for Morpho.

   Keep it mechanical:

   - initialize submodules
   - run `lake build Morpho.Proofs`
   - run `./scripts/run_morpho_blue_parity.sh`
   - compare the mapping manifest
   - inspect trust boundaries

5. Keep Midnight out of Morpho Blue review paths.

   CI can still build both packages, but Morpho Blue parity docs and scripts
   should not reference `morpho-midnight-verity/` except in the top-level layout.

## License Rule

Do not copy Morpho upstream source into this repository's MIT-licensed areas.
Keep Morpho upstream code under `morpho-blue/` with its original license. If a
file is translated into Verity, make that provenance visible in the file header
and in the comparison manifest.
