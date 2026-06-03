# Morpho Midnight Mapping

This manifest maps the upstream Morpho Midnight interface to the current
Midnight Verity proof and parity artifacts. The repository now contains a full
compiled `IMidnight` replacement that passes the original upstream Foundry
suite, plus focused proof projections for the requested liquidation/accounting
properties.

Complete artifact status: present
Focused artifact status: present

## Source Pair

| Upstream source | Current Verity artifact |
|-----------------|-------------------------|
| `morpho-midnight/src/Midnight.sol` | `morpho-midnight-verity/Midnight/Contract.lean` |
| `morpho-midnight/src/interfaces/IMidnight.sol` | `morpho-midnight-verity/Midnight/Proofs/Storage.lean` |
| `morpho-midnight/src/libraries/UtilsLib.sol` | `morpho-midnight-verity/Midnight/Proofs/Basic.lean` and `morpho-midnight-verity/Midnight/Contract.lean` |
| `morpho-midnight/src/libraries/ConstantsLib.sol` | constants in `morpho-midnight-verity/Midnight/Contract.lean` and proof files |

## Artifact Outputs

| Artifact | Status | Scope |
|----------|--------|-------|
| `artifacts/midnight-focused/MidnightRCF.yul` | Present after `./scripts/prepare_focused_midnight_artifact.sh` | Focused proof-model Yul for `MidnightRCF`. |
| `artifacts/midnight-focused/MidnightRCF.bin.raw` | Present after `./scripts/prepare_focused_midnight_artifact.sh` | Executable focused proof-model creation bytecode. |
| `artifacts/midnight/Midnight.bin.raw` | Present full parity artifact | Generated from `verity_contract Midnight`; `MORPHO_MIDNIGHT_PARITY_MODE=verity ./scripts/run_morpho_midnight_parity.sh` reports 373 passing tests, 0 failures, 0 skipped. |

## Function Surface

| Interface function | Current coverage | Evidence |
|--------------------|------------------|----------|
| `INITIAL_CHAIN_ID()` | Full test-parity getter. | `FOUNDRY_PROFILE=difftest MIDNIGHT_IMPL=verity forge test --match-contract SettersTest --use $(command -v solc) -vvv` |
| `position()` | Focused storage projection for liquidation/accounting paths. | `Midnight/Proofs/Storage.lean` |
| `marketState()` | Focused storage projection for liquidation/accounting paths. | `Midnight/Proofs/Storage.lean` |
| `consumed()` | Full test-parity getter. | `Midnight/Contract.lean` |
| `isAuthorized()` | Full test-parity getter and setter; focused entry-guard shape in proofs. | `SettersTest`, `AuthorizationTest` authorization helper passes, `liquidatorGateAllows`, `normalModeEntryGuards` |
| `defaultSettlementFeeCbp()` | Full test-parity getter and setter storage. | `SettersTest` |
| `defaultContinuousFee()` | Full test-parity getter and setter storage. | `SettersTest` |
| `claimableSettlementFee()` | Full test-parity getter and claim flow. | `Midnight/Contract.lean` |
| `roleSetter()` | Full test-parity getter and setter storage. | `SettersTest` |
| `feeSetter()` | Full test-parity getter and setter storage. | `SettersTest` |
| `feeClaimer()` | Full test-parity getter and setter storage. | `SettersTest` |
| `tickSpacingSetter()` | Full test-parity getter and setter storage. | `SettersTest` |
| `multicall()` | Full test-parity coverage in the executable artifact. | `MulticallTest` passes under `MIDNIGHT_IMPL=verity`. |
| `setRoleSetter()` | Full test-parity role check and storage write. | `SettersTest` |
| `setFeeSetter()` | Full test-parity role check and storage write. | `SettersTest` |
| `setFeeClaimer()` | Full test-parity role check and storage write. | `SettersTest` |
| `setTickSpacingSetter()` | Full test-parity role check and storage write. | `SettersTest` |
| `setMarketTickSpacing()` | Full test-parity role, market-created, and uint8-bound checks. | `SettersTest` |
| `setMarketSettlementFee()` | Full test-parity role, index, fee-bound, CBP-multiple, market-created checks and storage write. | `SettersTest` |
| `setDefaultSettlementFee()` | Full test-parity role, index, fee-bound, CBP-multiple checks and storage write. | `SettersTest` |
| `setMarketContinuousFee()` | Full test-parity role, max-fee, market-created checks and storage write. | `SettersTest` |
| `setDefaultContinuousFee()` | Full test-parity role, max-fee checks and storage write. | `SettersTest` |
| `claimSettlementFee()` | Full test-parity coverage in the executable artifact. | `SettlementFeeTest` passes under `MIDNIGHT_IMPL=verity`. |
| `claimContinuousFee()` | Full test-parity coverage in the executable artifact. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`. |
| `take()` | Full test-parity coverage in the executable artifact, including buy/sell pricing, callbacks, ratification, reduce-only, gates, liquidation lock, settlement fees, consumed limits, and health checks. | `TakeTest`, `TakeAmountsTest`, `TickGatingTest`, `GateTest`, `EcrecoverRatifierIntegrationTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `withdraw()` | Full test-parity coverage in the executable artifact. | `AuthorizationTest`, `OtherFunctionsTest`, and `ContinuousFeeTest` pass under `MIDNIGHT_IMPL=verity`. |
| `repay()` | Full test-parity coverage in the executable artifact, including callback and referral-fee bundle paths. | `AuthorizationTest`, `OtherFunctionsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `supplyCollateral()` | Full test-parity coverage in the executable artifact, including bitmap activation, collateral count, no-oracle paths, and bundle collateral supplies. | `AuthorizationTest`, `OtherFunctionsTest`, `MaxAmountsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `withdrawCollateral()` | Full test-parity coverage in the executable artifact, including bitmap clearing, receiver path, no-oracle zero-debt path, and borrowed-collateral health checks. | `AuthorizationTest`, `OtherFunctionsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `liquidate()` | Full test-parity coverage in the executable artifact; focused proofs cover the RCF and total-units/accounting properties over named projections. | `LiquidationTest` passes 38/38 under `MIDNIGHT_IMPL=verity`; proofs in `Midnight/Proofs/RCF.lean`, `Refinement.lean`, `Storage.lean`, and `UnitsAccounting.lean`. |
| `setConsumed()` | Full test-parity authorization check, monotonic consumed-amount check, storage write, and getter. | `OtherFunctionsTest` consumed cases; `Midnight/Contract.lean` |
| `setIsAuthorized()` | Full test-parity authorization setter; focused entry-guard proof shape remains available. | `AuthorizationTest` and `SetIsAuthorizedWithSigTest` pass under `MIDNIGHT_IMPL=verity`; `normalModeEntryGuards`. |
| `flashLoan()` | Full test-parity coverage in the executable artifact. | `FlashLoanTest` passes under `MIDNIGHT_IMPL=verity`. |
| `touchMarket()` | Full test-parity coverage in the executable artifact, including CREATE2/SSTORE2-style market identity, validation, and positive `toMarket` round trip. | `OtherFunctionsTest`, `SettersTest`, and `IdLibTest` pass under `MIDNIGHT_IMPL=verity`. |
| `updatePositionView()` | Full test-parity coverage in the executable artifact; focused fee/slash/update returned-credit proofs remain available. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`; `updatePositionViewSequence`, `Midnight/Proofs/UnitsAccounting.lean`. |
| `updatePosition()` | Full test-parity coverage in the executable artifact; focused post-update credit cover proofs remain available. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`; `badDebtCoversTwoPostUpdateCredits`, `badDebtCoversTwoStoredCreditsAfterUpdates`. |
| `lastLossFactor()` | Focused storage projection for lender-credit proofs. | `Midnight/Proofs/Storage.lean` |
| `collateralBitmap()` | Full test-parity getter/effects in the executable artifact; focused bitmap schedule and clear-step proofs remain available. | `OtherFunctionsTest` bitmap cases pass under `MIDNIGHT_IMPL=verity`; `Midnight/Proofs/BitmapSchedule.lean`. |
| `collateral()` | Full test-parity getter/effects in the executable artifact; focused selected-collateral projection proofs remain available. | `OtherFunctionsTest`, `LiquidationTest`, and `MaxAmountsTest` pass under `MIDNIGHT_IMPL=verity`; `Midnight/Proofs/CollateralLoop.lean`, `Midnight/Proofs/Storage.lean`. |
| `toId()` | Full test-parity coverage in the executable artifact. | `OtherFunctionsTest.testToId`, `testToIdStableAcrossHardfork`, and `IdLibTest` pass under `MIDNIGHT_IMPL=verity`. |
| `toMarket()` | Full test-parity coverage in the executable artifact, including positive round trip and `MarketNotCreated`. | `OtherFunctionsTest.testToMarket` and `testToMarketRevertsIfNotCreated` pass under `MIDNIGHT_IMPL=verity`. |
| `creditOf()` | Focused storage projection for lender-credit proofs. | `Midnight/Proofs/Storage.lean` |
| `debtOf()` | Focused storage projection for liquidation/accounting proofs. | `Midnight/Proofs/Storage.lean` |
| `totalUnits()` | Focused total-units coverage proofs. | `Midnight/Proofs/UnitsAccounting.lean`, `Midnight/Proofs/MarketLedger.lean` |
| `lossFactor()` | Focused bad-debt loss-factor update proofs. | `badDebtLocalStep_newLossFactorEqSolidityFormula` |
| `tickSpacing()` | Full test-parity getter over initialized market state. | `SettersTest` |
| `withdrawable()` | Focused withdrawable-increase proof in liquidation branch. | `withdrawableAfterRepay`, `normalModeAfterBadDebtStep_withdrawableIncreases` |
| `settlementFeeCbps()` | Full test-parity getter over the seven stored CBP fee slots. | `SettersTest`; `Midnight/Contract.lean` |
| `continuousFee()` | Focused continuous-fee update branch only. | `badDebtLocalStep_continuousFeeCreditEqSolidityFormula` |
| `continuousFeeCredit()` | Focused continuous-fee-credit slash/update proofs. | `continuousFeeCreditAfterBadDebt`, `badDebtLocalStep_continuousFeeCreditEqSolidityFormula` |
| `pendingFee()` | Focused pending-fee slashing proof. | `postSlashPendingFee` |
| `lastAccrual()` | Focused `updatePositionView` returned triple model. | `updatePositionViewSequence` |
| `liquidationLocked()` | Full test-parity coverage in the executable artifact; proof model still treats lock fact as an entry guard. | `TakeTest` liquidation-lock callback cases pass under `MIDNIGHT_IMPL=verity`; `NormalModeCall` in `Midnight/Proofs/Storage.lean`. |
| `isHealthy()` | Full test-parity coverage in the executable artifact; focused normal-mode health projection remains the proof anchor. | `TakeTest`, `OtherFunctionsTest`, and `LiquidationTest` pass under `MIDNIGHT_IMPL=verity`; `normalModeMaxRepaidHealthyWithin3`, `normalModeLiquidateProjection_storageHealthyWithinOne`. |
| `settlementFee()` | Full test-parity coverage in the executable artifact. | `SettersTest` and `SettlementFeeTest` pass under `MIDNIGHT_IMPL=verity`. |

## Requested Proof Anchors

| Requested property | Evidence | Remaining extraction boundary |
|--------------------|----------|-------------------------------|
| RCF does not prevent recovery with `repaidUnits = maxRepaid`. | `rcf_maxRepaid_restores_unhealthy`, `rcfAllows_maxRepaid`, `rcfAllowsZeroFloor_maxRepaid`, `normalModeMaxRepaidHealthyWithin3_of_projection_domain`, `normalModeLiquidateProjection_storageHealthyWithinOne` | Full generated-body extraction from `src/Midnight.sol::liquidate` and concrete storage/oracle/collateral-loop witnesses. |
| Market `totalUnits` covers lender credit after slashing/updating. | `totalUnits_cover_after_badDebt_branch`, `badDebtCoversTwoPostSlashCredits_of_synced_cover`, `badDebtCoversTwoPostUpdateCredits_of_synced_cover`, `badDebtCoversTwoStoredCreditsAfterUpdates_of_synced_cover`, `normalModeLiquidateProjection_coversStoredCreditsAfterUpdates` | Full storage extraction for debt list cover, lender snapshots, and generated update path. |

## Mechanical Checks

Run:

```bash
python3 scripts/check_morpho_midnight_mapping.py
python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate
```

The check fails if an `IMidnight` function is missing from this manifest or if
the artifact status disagrees with `artifacts/midnight/Midnight.bin.raw`. The
Yul drift gate fails if the Midnight function-level Yul drift diverges from
`config/midnight-yul-identity-unsupported.json`.
