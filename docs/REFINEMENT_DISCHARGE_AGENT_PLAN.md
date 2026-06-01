# Refinement Discharge Agent Plan

## Short `/goal`

Implement the remaining refinement-discharge work for Morpho: prove the structural discipline obligations in Lean instead of assuming them, integrate `HealthFaithful` through explicit no-overflow conditions where needed, keep `Refinement.lean` lightweight, and update the article/docs only after the Lean build and status scripts are green.

## Current Diagnosis

The previous direct strategy tried to reduce complete generated contract bodies inside `Morpho/Proofs/Refinement.lean`. That is conceptually right but too heavy for Lean performance. The path forward is to move body-reduction work into small, specialized helper lemmas and leave `Refinement.lean` as an assembly layer.

Current state to re-check before editing:

- `Morpho/Proofs/HealthFaithful.lean` exists and `healthFaithful_of_noOverflow`
  is used by refinement instead of an opaque `HealthFaithful` assumption.
- `Morpho/Proofs/Disciplines.lean` discharges the monotone entrypoints
  (`supply`, `withdraw`, `supplyCollateral`, `repay`) and the generated
  `_isHealthy` guards for `withdrawCollateral` and `borrow`.
- `borrow` has been factored so the post-accrual commit-and-health-check block and
  both amount modes are proved by `guardedDiscipline_borrowCommitAndCheck`,
  `guardedDiscipline_borrowAssetsMode`, and
  `guardedDiscipline_borrowSharesMode`; `Morpho/Proofs/Refinement.lean` now uses
  `guardedDiscipline_borrow` instead of a `GuardedDiscipline` assumption.
- `liquidate` has the easy arithmetic-faithfulness side integrated through
  `healthFaithful_of_noOverflow`, but still carries the harder generated
  `GuardUnhealthy` extraction boundary.
- Remaining hard areas are liquidation guard alignment, no-accrual alignment for
  liquidation, and any further no-overflow propagation.

Always start with:

```bash
cd /workspaces/mission-8fd3c4f4/temp/morpho-verity
lake build
python3 -m pytest scripts -q
```

If the full build is slow, first run targeted builds on the touched Lean files.

## Target Outcome

Palier 1:

- Replace assumed `MonotoneDiscipline`, `GuardedDiscipline`, and `GuardUnhealthy` obligations with proved Lean lemmas.
- Make the relevant `refines_*` theorems no longer require these discipline assumptions.
- Accept standard environment boundaries only where they are genuinely needed: slot/keccak injectivity, price/id ECM alignment, and no-accrual.

Palier 2:

- Integrate `HealthFaithful` through proved Lean facts as far as the current arithmetic model allows.
- At minimum, replace opaque `HealthFaithful` assumptions with `HealthFaithful_of_noOverflow` plus explicit no-overflow hypotheses.
- Do not claim zero arithmetic assumptions unless the Uint256 modular-vs-Nat arithmetic bridge is actually proved for the needed `mulDivUp` / `mulDivDown` domains.

Docs:

- Update the article/docs only after Lean and scripts are green.
- The final wording should say: generated entrypoint bodies are used directly and refinement is proved in Lean, subject only to the remaining explicit environment/arithmetic boundaries.

## Recommended File Structure

Keep heavyweight proof reductions out of `Refinement.lean`.

Suggested modules:

- `Morpho/Proofs/StorageFrame.lean`
  - Slot/key injectivity assumptions.
  - Low-level storage equality/framing lemmas.

- `Morpho/Proofs/FramePreserve.lean`
  - `PreservesSlot` / `StoragePreserving` lemmas.
  - Helpers for events, raw logs, callbacks, ECM reads, and external-call-like operations that do not mutate Morpho storage.

- `Morpho/Proofs/Disciplines.lean`
  - Entry-point-specific proofs of monotonicity/framing/guard facts.
  - This should import generated contract bodies and helper modules, but expose small theorem statements.

- `Morpho/Proofs/HealthFaithful.lean`
  - Keep the arithmetic bridge here.
  - Expose `healthFaithful_of_noOverflow` and any smaller bounded `mulDiv` lemmas.

- `Morpho/Proofs/Refinement.lean`
  - Import the above.
  - Assemble `Refines` theorems.
  - Avoid reducing full generated bodies here.

## Dependency Order

1. **Storage and framing first**
   - Prove that irrelevant slots are preserved by operations and generated helper calls.
   - Add one explicit keccak/slot-injectivity axiom if needed. Do not hide it inside unrelated lemmas.

2. **Monotone entrypoints**
   - Finish/prove:
     - `supply`
     - `withdraw`
     - `supplyCollateral`
     - `repay`
   - `supply` / `withdraw` mainly need slot preservation.
   - `supplyCollateral` / `repay` also need packed-field lemmas for collateral and borrow shares.

3. **Guarded entrypoints**
   - Prove `borrow` and `withdrawCollateral` use the generated `_isHealthy` guard.
   - Show the projected state after the guard is the projected final state, modulo storage-preserving suffixes.
   - Apply `healthFaithful_of_noOverflow` rather than assuming opaque `HealthFaithful`.

4. **Liquidation**
   - Prove the generated `liquidate` body reaches the unhealthy guard.
   - Align `_accrueInterest` / no-accrual with the projected pre-state used by the model.
   - Use `healthFaithful_of_noOverflow` to connect the generated guard to model unhealthiness.

5. **Refinement assembly**
   - Remove discipline assumptions from `refines_*` signatures.
   - Keep only explicit standard assumptions that are still real:
     - ECM market-id / oracle-price alignment.
     - no-accrual where liquidation depends on pre-state health.
     - slot injectivity.
     - no-overflow if Palier 2 is not fully discharged.

6. **Docs/article**
   - Update trust boundaries and the article after proofs pass.
   - Do not mention non-discharged `*Discipline` obligations if they have been proved.
   - If no-overflow remains, state it as the only arithmetic bridge boundary.

## Parallel Worker Plan

Use workers only on disjoint write scopes to avoid conflicts.

### Worker A: Storage/Framing Core

Scope:

- `Morpho/Proofs/StorageFrame.lean`
- `Morpho/Proofs/FramePreserve.lean`

Task:

- Define reusable slot/key framing lemmas.
- Add the explicit slot/keccak injectivity assumption if needed.
- Prove storage-preserving facts for generated operations that are known not to touch projected Morpho slots.

Output:

- A small API usable by other proof files.
- No edits to `Refinement.lean` except imports if absolutely necessary.

### Worker B: Monotone Disciplines

Scope:

- `Morpho/Proofs/Disciplines.lean`
- Possibly one helper file for packed-field lemmas.

Task:

- Prove monotone discipline facts for `supply`, `withdraw`, `supplyCollateral`, and `repay`.
- For packed fields, prove field-level consequences instead of repeatedly unfolding generated storage code.

Depends on:

- Worker A framing API.

### Worker C: Borrow/WithdrawCollateral Guards

Scope:

- `Morpho/Proofs/Disciplines.lean` or `Morpho/Proofs/GuardDiscipline.lean`

Task:

- Prove generated-body guard extraction for `borrow` and `withdrawCollateral`.
- Show suffix operations preserve the projected post-guard state.
- Integrate `healthFaithful_of_noOverflow`.

Depends on:

- Worker A framing API.
- Existing `HealthFaithful.lean`.

### Worker D: Liquidation Guard and Accrual

Scope:

- `Morpho/Proofs/LiquidationDiscipline.lean` or the liquidation section of `Disciplines.lean`

Task:

- Prove generated `liquidate` guard extraction.
- Make the no-accrual dependency explicit.
- Connect the generated unhealthy guard to model unhealthiness through `healthFaithful_of_noOverflow`.

Depends on:

- Worker A framing API.
- Existing `HealthFaithful.lean`.

### Worker E: HealthFaithful / Arithmetic Boundary

Scope:

- `Morpho/Proofs/HealthFaithful.lean`
- Any arithmetic helper module it imports.

Task:

- Strengthen the no-overflow API and reduce opaque `HealthFaithful` assumptions.
- Attempt bounded `mulDivUp` / `mulDivDown` equivalence only if the local definitions make it tractable.
- If full discharge is not realistic, leave a precise no-overflow theorem boundary and document it.

### Worker F: Integration and Docs

Scope:

- `Morpho/Proofs/Refinement.lean`
- status JSON/docs/checker files
- article/docs, including `docs/TRUST_BOUNDARIES.md`

Task:

- Integrate all proved discipline lemmas into `Refinement.lean`.
- Remove obsolete assumptions from theorem signatures.
- Run full verification.
- Update article/docs to match exactly what Lean proves.

Start Worker F only after A-D are merged/building.

## Acceptance Criteria

Lean:

- `lake build` passes.
- No new `axiom` or `constant` is introduced except the explicitly named slot/keccak injectivity boundary if it is truly required.
- `Refinement.lean` no longer assumes `MonotoneDiscipline` or
  `GuardedDiscipline` for the target entrypoints; `liquidate` may still carry
  the explicitly deferred `GuardUnhealthy` extraction boundary.
- `HealthFaithful` is not used as an opaque assumption where `healthFaithful_of_noOverflow` can be used instead.

Scripts:

```bash
python3 -m pytest scripts -q
```

Docs/article:

- No claim says "zero arithmetic assumptions" unless the modular Uint256-vs-Nat bridge is fully proved.
- No claim says `*Discipline` remains empirical if those obligations are now discharged.
- The article distinguishes clearly between:
  - generated body refinement proved in Lean;
  - environment assumptions;
  - remaining arithmetic/no-overflow boundary, if any.
