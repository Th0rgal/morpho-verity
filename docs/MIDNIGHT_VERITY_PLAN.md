# Midnight follow-up work

Usage and current verification: [README](../README.md#verify-morpho-midnight).
Representation decisions and actionable gaps live in
`config/midnight-support-policies.json`; inspect them with
`node scripts/report_midnight_support.mjs --json`.

Priorities:

1. Reduce initcode/runtime size and check actual network deployment limits.
2. Prove the allocator/ABI/source-model relations; close Verity's codegen-only
   call/intrinsic semantics and minimum-fork enforcement gaps.
3. Replace each custom adaptation or check exception with native support or a
   justified, tested translation. Every new emitted policy needs a registry row.
4. Connect full generated entrypoints to the relevant theorem premises. Existing
   focused projections are not whole-implementation equivalence proofs.

A fresh evidence bundle, not a prose checklist, records build and runtime status:
`python3 scripts/verify_midnight_pipeline.py` → `out/midnight/`.
