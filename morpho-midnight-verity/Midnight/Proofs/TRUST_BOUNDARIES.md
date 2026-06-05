# Midnight Trust Boundaries

This file records what the focused Midnight package proves today and which
generated-body extraction work remains for `src/Midnight.sol`.

## Retained Runtime ECM Surface

The complete executable `Midnight` artifact intentionally keeps several
protocol-specific ECMs where the current Verity source surface cannot yet express
the Solidity construct as typed code without hiding byte-level behavior. These
are trust-report assumptions, not Lean proof-system axioms.

| ECM axiom(s) | Solidity construct represented | Why it remains an ECM |
|--------------|--------------------------------|------------------------|
| `midnight_multicall_bytes_array_abi`, `self_delegatecall_storage_context` | `multicall(bytes[] calldata)` self-`delegatecall` loop with revert-data bubbling. | Verity can emit the low-level loop, but `bytes[] calldata` iteration, delegatecall storage context, and returndata bubbling are still low-level mechanics rather than typed source. |
| `midnight_ticklib_tick_to_price_exact_yul` | `TickLib.tickToPrice` fixed-point exponential approximation. | The emitted Yul is the source algorithm ported as arithmetic plumbing; a typed Verity library plus proof of equivalence remains future work. |
| `midnight_*_callback_*_dynamic_abi`, `midnight_ratifier_offer_bytes_dynamic_abi` | Buy/sell/repay/liquidate/flash-loan callback interfaces and ratifier call payloads. | Callback ordering is source-shaped at call sites, but nested dynamic ABI payloads (`Market`, `Offer`, `bytes`) still require byte-layout ECMs. |
| `midnight_take_event_payload_memory`, `midnight_take_event_log4` | `Take` event payload preparation and indexed log emission. | The event fields are source-projected, but dynamic payload memory layout and `log4` emission are still raw memory/log mechanics. |
| `midnight_liquidation_lock_transient_slot`, `midnight_liquidation_lock_transient_depth_slot` | Transient liquidation-lock reads, swaps, clears, and depth accounting. | The surrounding control flow is source-shaped, but exact transient-slot addressing and exchange/clear sequences remain low-level mechanics. |
| `midnight_offer_market_isHealthy_helper` | Post-callback seller health check in `take`. | This packages a source-level health call over calldata-derived market fields until nested dynamic calldata projections are typed enough to call the helper directly. |
| `midnight_idlib_toid_market_abi_create2_preimage`, `midnight_sstore2_market_initcode_layout`, `create2_address_derivation`, `midnight_sstore2_market_runtime_abi_decode`, `midnight_sstore2_market_runtime_abi_return` | `IdLib.toId`, `touchMarket`, and `toMarket` CREATE2/SSTORE2 code-as-data behavior. | The current modules implement the real low-level algorithm, but typed `CodeData.store[Market]` / `CodeData.read[Market]` and dynamic `Market` return support are not yet available as source-level Verity. |
| `solidity_panic_revert_payload` | Solidity panic payloads for array bounds and checked arithmetic branches that must revert with standard panic selectors. | The source branches are explicit, but exact panic ABI payload emission is still represented by a small ECM. |

Local obligations in the full `Midnight` contract are intentionally narrower
than these ECMs: they cover calldata scalar extraction for `take` and
liquidation debt/collateral snapshots where the proof model needs named source
values before full nested ABI projection is available.

## Prompt-to-Artifact Checklist

| Request | Artifact | Current evidence | Remaining boundary |
|---------|----------|------------------|--------------------|
| Implement a Verity model faithful as much as possible to Morpho Midnight. | `morpho-midnight-verity/Midnight/Contract.lean` defines `verity_contract MidnightRCF` for the liquidation and slashing arithmetic used by the requested properties, including the focused input guard, borrower and liquidator-gate entry guards, full and normal-mode liquidatability guards, one-step collateral loop update, LIF selection, seized-input and repaid-input amount branches, a general normal-mode RCF local sequence, bad-debt and repay/seize branch predicates, bad-debt debt write, combined bad-debt branch sequence, withdrawable write arithmetic, collateral bitmap clear, payer/callback branch shapes, fixed callback-success value, callback-return acceptance, return pair, combined normal-mode repaid-input local sequence, a single combined normal-mode local `liquidate` sequence, and `updatePositionView` returned triple sequence. | `morpho-midnight-verity/Midnight/Proofs/ContractShape.lean` pins every exposed generated model body with `rfl` checks, including `normalModeRcfLocalSequence_modelBody` and `normalModeLiquidateLocalSequence_modelBody`. The specialized `repaidUnits = maxRepaid` local sequences omit the RCF threshold because the left disjunct of the guard is satisfied by construction; the full threshold branch remains in `normalModeRcfLocalSequence` and `rcfAllowsZeroFloor`. | The current surface targets the requested liquidation/accounting paths. Storage maps, callbacks, token transfers, token-transfer effects, oracle calls, maturity/touch-market effects, and full collateral-loop extraction remain outside this focused surface. |
| Prove RCF does not prevent recovery when liquidation uses `repaidUnits = maxRepaid`. | `morpho-midnight-verity/Midnight/Contract.lean` exposes `normalModeMaxRepaidHealthyWithin3`; `ContractShape.lean` pins its generated body. `morpho-midnight-verity/Midnight/Proofs/RCF.lean` proves `normalModeMaxRepaidHealthyWithin3_of_projection_domain`, `normalModeMaxRepaidProjection_matches_solidity_test_tolerance`, `normalModeMaxRepaidProjection_singleOther_matches_solidity_test_tolerance`, `rcf_maxRepaid_matches_solidity_test_tolerance`, `rcfAllows_maxRepaid`, `rcfAllowsZeroFloor_maxRepaid`, `rcfAllows_maxRepaid_with_collateral_capacity`, `seizedAssetsFromRepayValue_le_collateral_of_repaid_le_capacity`, and `repayValue_le_collateralQuote_of_repaid_le_capacity`; `morpho-midnight-verity/Midnight/Proofs/Refinement.lean` proves `NormalModeRcfLocalStep`, `RepaidMaxAfterBadDebtLocalStep.toNormalModeRcfLocalStep`, `normalModeRcfLocalStep_amountBranch`, `normalModeRcfLocalStep_rcfAcceptedEq`, `repaidMaxAfterBadDebtLocalStep_generalRcfAccepted`, `repaidMaxStep_restoresWithinOne`, `repaidMaxStep_passesRcfGuard`, zero-floor guard wrappers for local/combined/storage projections, `repaidMaxStep_repaidWithinDebt`, `repaidMaxStep_seizedWithinCollateral`, `repaidMaxStep_activeBitmapUint128`, `repaidMaxStep_activeBitmapCountBound`, `repaidMaxStep_activeScheduleLengthEqCountBits`, `repaidMaxStep_activeSlotsBound`, `repaidMaxStep_selectedBitSet`, `repaidMaxStep_scheduleAfterClearingBefore`, `repaidMaxStep_selectedHighestAfterClearingBefore`, `repaidMaxStep_scheduleAfterClearingThroughSelected`, `repaidMaxStep_msbTraceEqSchedule`, `repaidMaxStep_slotsOfMsbTrace`, `repaidMaxStep_loopMaxDebtFromMsbTrace`, `repaidMaxStep_selectedPriceFromMsbTrace`, `repaidMaxStep_maxDebtEqCollateralLoop`, `repaidMaxStep_selectedPriceFromSchedule`, `repaidMaxStep_selectedPriceFromCollateralLoop`, and `repaidMaxStep_subtractionsSafe` from named extraction obligations. | The proof uses the Solidity `maxRepaid` expression through the general normal-mode RCF branch shape, the RCF guard's left disjunct with the concrete collateral repayment capacity and zero-floor shortfall formula, the repaid-input seized-assets formula, the checked debt/collateral subtraction bounds derived from the RCF coefficient-domain inequality plus `maxRepaid <= collateralRepayCapacity`, the concrete `bitmapSchedule` for the health-loop `maxDebt`, selected collateral price from the collateral loop, selected bit membership and highest-bit status after clearing the earlier schedule prefix, an `MsbClearTrace` equivalence for the full Solidity-shaped loop trace, direct trace-to-slot and trace-to-accumulator bridges, the `countBits <= 16` borrower cap, the selected-plus-other max-debt bridge, and explicit one-unit rounding slack. | Full extraction still has to identify the selected collateral, oracle price, `lltv`, `lif`, coefficient-domain bound, collateral-capacity bound, storage bitmap value, and prove Solidity's `msb` result satisfies `HighestBitmapBit` for each loop iteration required by `Refinement.RCF.RepaidMaxStep`. |
| Prove market `totalUnits` covers the sum of all lenders' credit after slashing/updating. | `morpho-midnight-verity/Midnight/Contract.lean` exposes `badDebtCoversTwoPostSlashCredits`, `badDebtCoversTwoPostUpdateCredits`, and `badDebtCoversTwoStoredCreditsAfterUpdates`; `ContractShape.lean` pins their generated bodies. `morpho-midnight-verity/Midnight/Proofs/BitmapSchedule.lean` proves `MsbClearTrace`, `msbClearTrace_eq_bitmapSchedule`, `msbClearTrace_of_bitmapSchedule`, `msbClearTrace_iff_eq_bitmapSchedule`, `bitmapSchedule_length_eq_countBits`, `bitmapSchedule_mem_iff_testBit`, `validCollateralBitmap_valid`, `HighestBitmapBit`, `bitmapSchedule_cons_highestBitmapBit`, `bitmapSchedule_highestBitmapBit_eq_cons`, `clearBitmapBit_testBit_below`, `bitmapSchedule_clearBitmapBit_eq_clearActiveBit`, `bitmapSchedule_clearBitmapBit_highest_eq_tail`, `countBits_clearBitmapBit_highest_eq_tail_count`, `bitmapSchedule_clearBitmapBit_highest_valid`, `bitmapSchedule_clearBitmapBit_cons_eq_tail`, `countBits_clearBitmapBit_cons_eq_tail_count`, `bitmapSchedule_clearBitmapBit_cons_valid`, `bitmapSchedule_clearBitmapBits_prefix_eq_suffix`, `countBits_clearBitmapBits_prefix_eq_suffix_length`, `bitmapSchedule_clearBitmapBits_prefix_valid`, `bitmapSchedule_valid`, `bitmapSchedule_index_lt_128`, `StrictDescendingBelow.length_le_bound`, `StrictDescendingBelow.nodup`, `validSchedule_slots_length_le_16`, `validSchedule_slots_length_le_128`, `validSchedule_slotIndex_lt_128`, `validSchedule_slotIndices_nodup`, and `sumMaxDebtContributions_split_selectedIndex`; `morpho-midnight-verity/Midnight/Proofs/CollateralLoop.lean` proves `run_maxDebt_eq`, `run_liquidatedCollatPrice_eq`, `run_badDebt_eq`, `step_badDebt_eq_zeroFloorSub`, `sumMaxDebtContributions_split_selected`, and `run_badDebt_le_initial`; `morpho-midnight-verity/Midnight/Proofs/MarketLedger.lean` proves `debt_le_totalUnits_of_mem_covered`, `debtsCovered_after_decreaseFirstDebt`, `debtsCovered_after_equal_decrease_first`, and `debtsCovered_after_equal_decrease_split`; `morpho-midnight-verity/Midnight/Proofs/UnitsAccounting.lean` proves `zeroFloorSub_eq_sub`, `totalUnits_cover_after_badDebt_branch`, `badDebtCoversTwoPostSlashCredits_of_synced_cover`, `badDebtCoversTwoPostUpdateCredits_of_synced_cover`, `badDebtCoversTwoStoredCreditsAfterUpdates_of_synced_cover`, `badDebt_covers_two_storedCreditsAfterUpdates`, `totalUnits_covers_postUpdateCredit_after_slashing`, `badDebt_covers_two_postUpdateCredits`, stored-credit cover after `updateLender`, and per-position/list-level returned-credit subtraction-safety lemmas; `Refinement.lean` proves `badDebtStep_msbTraceEqSchedule`, `badDebtStep_slotsOfMsbTrace`, `badDebtStep_badDebtEqMsbTraceLoop`, `badDebtStep_scheduleAfterClearingPrefix`, `badDebtStep_highestAfterClearingPrefix`, `badDebtStep_scheduleAfterClearingPrefixValid`, `badDebtStep_marketDebtsCoveredSplit`, `badDebtStep_originalDebtLeTotalUnits`, `badDebtStep_debtsCoveredAfterBadDebt`, `badDebtStep_lendersSynced`, `badDebtStep_oldCover`, `badDebtStep_activeBitmapUint128`, `badDebtStep_activeBitmapCountBound`, `badDebtStep_activeScheduleLengthEqCountBits`, `badDebtStep_activeIndexBitSet`, `badDebtStep_activeSlotsBound`, `badDebtStep_activeIndicesNodup`, `badDebtStep_activeIndexLt128`, `badDebtStep_activeSlotIndicesNodup`, `badDebtStep_activeSlotIndexLt128`, `badDebtStep_coversPostUpdateCredits`, `badDebtStep_coversStoredCreditsAfterUpdates`, and bad-debt subtraction-safety theorems from the concrete bitmap schedule, full debt-list cover plus split equality, stored-lender cover/snapshot, and loop-shaped bad-debt step. | The proof uses the exact bad-debt `lossFactor`, `totalUnits`, and continuous-fee-credit updates, a concrete uint128 bitmap schedule below 128 with the `countBits <= 16` borrower cap, an `MsbClearTrace` equivalence for full Solidity-shaped loop traces, direct trace-to-slot and trace-to-accumulator bridges, a schedule-membership bit-set theorem, both directions of a named `HighestBitmapBit`/schedule-head bridge, a Solidity-shaped uint128 clear mask for the tail step, direct tail/count/validity lemmas from the current schedule head, prefix-clearing lemmas for multiple loop iterations, and refinement-level head-after-prefix theorems for any active-index split, preservation of those bounds on the concrete `CollateralSlot` list consumed by the loop, the loop accumulators for `maxDebt`, liquidated collateral price, and Solidity zero-floor `badDebt`, the selected-plus-other max-debt bridge used by the RCF proof, the market debt-ledger full-list cover plus split equality for `originalDebt <= totalUnits` and preservation after equal bad-debt debt/total-units decreases, the stored-lender cover invariant that derives old synced cover after `updateLender`, the stored-lender update snapshot that derives `syncedAt`, the per-collateral bad-debt repayable formula, the checked borrower-debt/total-units subtraction bounds, the `updatePositionView` post-slash credit formula, concrete pending-fee slashing, concrete accrued-fee subtraction, and the returned-credit fee bound derived from loss-factor ordering for all positions in scope. | Full extraction still has to prove Solidity's `msb` result satisfies `HighestBitmapBit` for each loop iteration, provide the concrete full market debt list, split equality, and cover invariant from storage, and provide the stored-lender cover/snapshot/equality from storage. |
| Keep proof obligations minimal and explicit. | `Refinement.RCF.RepaidMaxStep` and `Refinement.Accounting.BadDebtStep` name the remaining proof obligations. | `lake build Midnight.Proofs` and `lake build` pass; the proof tree is free of placeholder proof markers. | The remaining obligations are semantic extraction/invariant obligations, not discharged from the full generated contract body. |

Additional trace-bound evidence: `BitmapSchedule.lean` proves
`msbClearTrace_length_eq_countBits`, `msbClearTrace_valid`,
`msbClearTrace_nodup`, `msbClearTrace_index_lt_128`, and
`msbClearTrace_slots_length_le_16`; `Refinement.lean` lifts these as
`repaidMaxStep_msbTraceValid`, `repaidMaxStep_msbTraceNodup`,
`repaidMaxStep_msbTraceSlotsBound`, `badDebtStep_msbTraceValid`,
`badDebtStep_msbTraceNodup`, and `badDebtStep_msbTraceSlotsBound`.
It also proves `msbClearTrace_clearPrefix` and
`msbClearTrace_highestAfterPrefix`, lifted as
`repaidMaxStep_msbTraceAfterClearingBefore`,
`repaidMaxStep_selectedHighestFromMsbTraceSplit`,
`badDebtStep_msbTraceAfterClearingPrefix`, and
`badDebtStep_highestFromMsbTraceSplit`, so an extracted loop prefix gives the
current cleared bitmap and next highest index directly.
Direct accumulator consequences are also proved from traced loops:
`repaidMaxStep_runMaxDebtFromMsbTrace`,
`badDebtStep_badDebtLeOriginalDebtFromMsbTrace`, and
`badDebtStep_debtsCoveredAfterBadDebtFromMsbTrace`.
The extraction boundary can now target the shared Solidity-local
`LiquidateLoopTrace`, then branch-specific `RepaidMaxLoopStep`,
`RepaidMaxAfterBadDebtLoopStep`, and `BadDebtLoopStep`; their conversions derive
the trace-shaped and older schedule-shaped obligations, plus direct wrappers for
RCF guard/subtraction safety, the selected-price equality from
`liquidatedCollatPrice`, and accounting cover/synchronization/subtraction facts.
Generic
`LiquidateLoopTrace` theorems also expose max-debt sum, bad-debt monotonicity,
and selected-price consequences from the single Solidity loop output; the
bad-debt loop step combines that monotonicity with the market-debt cover
invariant to prove the borrower-debt and total-units subtraction bounds
directly. `RepaidMaxLocalStep` records the concrete Solidity `maxRepaid`,
`repaidUnits`, and `seizedAssets` locals and proves the branch guard from
`repaidUnits = maxRepaid`; `projectedLocalPostState` states the health payoff
over those concrete locals, with checked debt and collateral subtraction bounds
and post-write `debt`/selected-collateral locals. `RepaidMaxAfterBadDebtLocalStep`
adds the exact sequencing where the loop uses `originalDebt`, then the optional
bad-debt write sets the current debt before the RCF `maxRepaid` local is read.
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
# Run the repository's placeholder-proof scan over Midnight, README.md, and lakefile.lean.
```
