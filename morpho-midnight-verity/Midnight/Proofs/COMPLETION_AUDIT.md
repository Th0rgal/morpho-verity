# Midnight Completion Audit

Objective: build a Verity/Lean model for Morpho Midnight, as faithful as
practical, and prove the requested RCF recovery and total-units lender-credit
coverage properties.

## Success Criteria

| Criterion | Current artifact | Status |
|-----------|------------------|--------|
| Verity model of Midnight. | `morpho-midnight-verity/Midnight/Contract.lean` defines `verity_contract MidnightRCF`; `ContractShape.lean` pins every exposed generated body, including the focused input guard, borrower and liquidator-gate entry guards, full and normal-mode liquidatability guards, one-step collateral loop update, LIF selection, seized-input and repaid-input amount branches, bad-debt and repay/seize branch predicates, bad-debt debt write, combined bad-debt branch sequence, withdrawable write arithmetic, collateral bitmap clear, payer/callback branch shapes, fixed callback-success value, callback-return acceptance, return pair, combined normal-mode repaid-input local sequence, and `updatePositionView` returned triple sequence. The Lean side also models the Solidity `maxRepaid` fallback, the shared liquidation loop trace, combined bad-debt/RCF sequencing, and touched market/position storage projections. | Focused arithmetic and storage-local surface for the requested liquidation/accounting paths. |
| RCF permits recovery with `repaidUnits = maxRepaid`. | `Contract.lean` exposes `normalModeMaxRepaidHealthyWithin3` and the Solidity zero-floor RCF guard shape; `ContractShape.lean` pins them. `RCF.lean` proves the health-tolerance theorem, Solidity formula match, `type(uint256).max` fallback, positive-`maxRepaid` branch fact, raw and zero-floor RCF guard facts, and repay-input seized-assets capacity bounds. `Refinement.lean` proves the traced-loop, selected-price, selected `lif`, denominator, branch, subtraction, `uint128`, final-debt, and combined normal-mode-after-bad-debt health facts. `Storage.lean` lifts the payoff through `postPosition_healthyWithinOne`, `postLoopMaxDebtProjection_eq_projected`, `NormalModeCall`, and `LiquidateOutcome`. | Proved for the modeled normal-mode projection with explicit generated-body and storage-extraction obligations. |
| Market `totalUnits` covers lender credit after slashing/updating. | `Contract.lean` exposes `badDebtCoversTwoPostSlashCredits`, `badDebtCoversTwoPostUpdateCredits`, and `badDebtCoversTwoStoredCreditsAfterUpdates`; `ContractShape.lean` pins them. `BitmapSchedule.lean`, `CollateralLoop.lean`, `MarketLedger.lean`, and `UnitsAccounting.lean` prove the bitmap schedule, loop accumulator, debt-ledger, bad-debt slash, lender-update, and stored-credit cover lemmas. `Refinement.lean` proves the local bad-debt branch cover, debt/`totalUnits` subtraction safety, `uint128` write bounds, zero-bad-debt no-op facts, combined final debt-list cover, and post-update/stored-credit cover. `Storage.lean` lifts those facts through `postMarket_coversCredits`, `postMarket_coversPostUpdateCredits`, and `postMarket_coversStoredCreditsAfterUpdates`. | Proved for the modeled bad-debt/update-position and storage projection with explicit extraction obligations. |
| Minimal proof obligations. | `Refinement.RCF.RepaidMaxStep`, `Refinement.RCF.RepaidMaxAfterBadDebtLocalStep`, `Refinement.Accounting.BadDebtLocalStep`, `Storage.LiquidateStorage.NormalModeCall`, and `Storage.LiquidateStorage.LiquidateOutcome` name the remaining generated-body, local, guard, storage, and observable-output obligations. `TRUST_BOUNDARIES.md` maps prompt requirements to evidence. | Local arithmetic, bitmap/list accounting, storage projection, and observable-output obligations are discharged; full generated contract extraction obligations remain. |
| Build and proof hygiene. | `lake build Midnight.Proofs`, `lake build`, and the repository placeholder-proof scan. | Passing locally. |

## Remaining Work

- Translate or extract enough of `src/Midnight.sol::liquidate` to discharge
  `RepaidMaxStep` and `BadDebtStep` from actual generated bodies and storage
  projections.
- Extract the storage bitmap and prove Solidity's `msb` result satisfies
  `BitmapSchedule.HighestBitmapBit` at each loop iteration.
- Extract the full market debt list from storage and prove its split equality
  around the liquidated borrower's original debt and its
  `MarketLedger.DebtsCovered` invariant.
- Extract the stored-lender cover/snapshot and prove the in-scope lenders are
  its `updateLender` image at the old market loss factor.
- Keep external calls, oracle behavior, callbacks, token transfers, liquidator
  gate, and maturity/touch-market behavior as explicit environment boundaries or
  model them in a fuller contract extraction.

The bitmap trace boundary now also proves `msbClearTrace_length_eq_countBits`,
`msbClearTrace_valid`, `msbClearTrace_nodup`,
`msbClearTrace_index_lt_128`, `msbClearTrace_slots_length_le_16`, and the
corresponding RCF/accounting refinement lifts for trace validity, no-duplicate
indices, and slot-count bounds.
It additionally proves `msbClearTrace_clearPrefix` and
`msbClearTrace_highestAfterPrefix`, with RCF/accounting refinement lifts that
turn an extracted trace split into the current cleared bitmap and next highest
index.
Direct traced-loop consequences now include
`repaidMaxStep_runMaxDebtFromMsbTrace`,
`badDebtStep_badDebtLeOriginalDebtFromMsbTrace`, and
`badDebtStep_debtsCoveredAfterBadDebtFromMsbTrace`.
The remaining generated-body work may use `LiquidateLoopTrace`, then
`RepaidMaxLoopStep`, `RepaidMaxAfterBadDebtLoopStep`, and `BadDebtLoopStep`,
before relying on their conversions to trace-shaped and older schedule-shaped
refinement obligations plus direct wrappers for guard, selected-price equality,
cover, synchronization, ledger, and subtraction-safety facts.
`LiquidateLoopTrace` itself now exposes max-debt sum, bad-debt monotonicity, and
selected-price facts from the shared Solidity loop output, and
`BadDebtLoopStep` combines loop monotonicity with market-debt cover to prove
borrower-debt and total-units subtraction bounds directly.
`RepaidMaxLocalStep` records the concrete Solidity `maxRepaid`, `repaidUnits`,
and `seizedAssets` locals and proves the branch guard from `repaidUnits =
maxRepaid`; `projectedLocalPostState` states the health payoff over those
concrete locals, with checked debt and collateral subtraction bounds and
post-write `debt`/selected-collateral locals. `RepaidMaxAfterBadDebtLocalStep`
records the Solidity ordering where the loop starts from `originalDebt`, the
bad-debt branch may write the current debt to `originalDebt - badDebt`, and the
later RCF branch computes `maxRepaid` from that current debt.
`Liquidate.NormalModeAfterBadDebtStep` ties that RCF branch to the bad-debt
accounting branch through the same loop output and post-bad-debt borrower debt
local, and proves the current-debt health condition implies the normal-mode
`originalDebt > maxDebt` liquidatability guard over that loop output. It also
derives the positive original borrower debt needed for the `_position.debt > 0`
entry guard, and positive `_totalUnits` for the bad-debt loss-factor division.
It proves the concrete `maxRepaid` local equals the Solidity formula over
current debt and the loop's `maxDebt` with a positive RCF denominator, proves the initial
repaid-input call satisfies
`atMostOneNonZero` with zero seized-assets input, and proves `maxRepaid > 0`, so
the `repaidUnits > 0 || seizedAssets > 0` branch is active for `repaidUnits =
maxRepaid`. It also models the repay branch's conditional selected-collateral
bitmap clear and `withdrawable += repaidUnits` write, and proves the final
market debt-list cover after the later borrower-only `repaidUnits` debt
decrease, with that final debt identified as the RCF projected post-state debt.
It records the `uint128` bounds for the repay/seize casts and post-write debt,
selected collateral, selected bitmap, and withdrawable locals.
`BadDebtLocalStep` records the Solidity bad-debt branch market locals and
proves lender-credit coverage over the local updated `totalUnits`/`lossFactor`
state; it also expands the local updated `lossFactor` into the exact Solidity
formula over `_totalUnits`, `_lossFactor`, and `badDebt`, deriving positive
`_totalUnits` when the branch condition has `badDebt > 0`. It exposes the
pre-update `_totalUnits`/`_lossFactor` snapshot used for lender synchronization
and old-credit cover, plus the exact conditional `continuousFeeCredit` update
formula. The active `< UINT128_MAX` branch is derived from the old-loss-factor
extraction obligation, yielding the positive continuous-fee denominator; the
monotonicity bound and borrower debt write are reflected in the post-bad-debt
market debt list. It records the `uint128` bounds for `badDebt`,
post-bad-debt borrower debt, new `totalUnits`, new `lossFactor`, and new
`continuousFeeCredit`.
`Storage.lean` packages the combined local step as the touched `MarketState`
and borrower `Position` storage fields, proves the two-stage borrower debt
write, and states the health and lender-credit payoffs over those projected
storage records. It also packages the `marketState[id]` and
`position[id][borrower]` access keys in a storage frame, and an entry frame
ties those keys to the call inputs, initial input consistency check, and
positive borrower-debt read. It proves the post-write selected-collateral loop
`maxDebt` projection equals the `maxDebt` used by the RCF health proof.
`NormalModeCall` records the top-level normal-mode guard envelope; only the
liquidator-gate and liquidation-lock facts remain external to the arithmetic and
storage-local proof.
The `badDebt = 0` path is proved to leave the bad-debt branch's borrower debt
and market accounting fields unchanged in both the combined and storage
projections.
The observable `liquidate` return pair, emitted event fields,
`callback != address(0) ? callback : msg.sender` payer branch, and the final
collateral-transfer token/receiver/amount, optional-callback condition, and
loan-transfer token/payer/receiver/amount arguments are projected from the same
final branch/storage locals. The optional callback return check is also
represented as a selector-equality obligation over the external callback return
value, with the callback's local payload arguments recorded separately from the
uninterpreted market calldata and callback data. The market id itself is threaded
through the event and callback projections; proving that it is the exact
`touchMarket(market)` result remains a generated-body extraction obligation.
`NormalModeLiquidateProjection` packages those entry, shared-loop/current-debt,
selected-parameter, active-loop schedule/bitmap, pre/post max-debt, return,
event, callback, transfer, normal-mode/RCF guard, subtraction-safety,
uint128-bound, storage-write-equation, observable/storage-consistency,
conditional collateral-bitmap clear, zero-bad-debt no-op, health-payoff, and
lender-credit-cover projections into one generated-body target.
The normal-mode `lif` local is tied to the selected collateral's `maxLif`;
post-maturity `lif` interpolation remains outside this normal-mode proof.
The `maxRepaid` helper follows Solidity's branch condition: formula for
`lltv < WAD`, `type(uint256).max` for `lltv >= WAD`. The recovery theorem only
uses the positive-denominator subdomain `lif * lltv < WAD^2`.

## Verification Commands

```bash
lake build Midnight.Proofs
lake build
```
