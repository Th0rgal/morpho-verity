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

| Interface function | Test parity / proof coverage | Source faithfulness | Evidence |
|--------------------|------------------------------|---------------------|----------|
| `INITIAL_CHAIN_ID()` | Full test-parity getter. | Medium: behavior matches tests, but Solidity immutable storage absence is represented by an isolated Verity slot until immutable support lands. | `FOUNDRY_PROFILE=difftest MIDNIGHT_IMPL=verity forge test --match-contract SettersTest --use $(command -v solc) -vvv` |
| `position()` | Focused storage projection for liquidation/accounting paths. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/Storage.lean` |
| `marketState()` | Focused storage projection for liquidation/accounting paths. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/Storage.lean` |
| `consumed()` | Full test-parity getter. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `Midnight/Contract.lean` |
| `isAuthorized()` | Full test-parity getter and setter; focused entry-guard shape in proofs. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest`, `AuthorizationTest` authorization helper passes, `liquidatorGateAllows`, `normalModeEntryGuards` |
| `defaultSettlementFeeCbp()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `defaultContinuousFee()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `claimableSettlementFee()` | Full test-parity getter and claim flow. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `Midnight/Contract.lean` |
| `roleSetter()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `feeSetter()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `feeClaimer()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `tickSpacingSetter()` | Full test-parity getter and setter storage. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `multicall()` | Full test-parity coverage in the executable artifact. | Low: behavior/artifact path present, but source still relies on scaffold or low-level ECM boundary. | `MulticallTest` passes under `MIDNIGHT_IMPL=verity`. |
| `setRoleSetter()` | Full test-parity role check and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setFeeSetter()` | Full test-parity role check and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setFeeClaimer()` | Full test-parity role check and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setTickSpacingSetter()` | Full test-parity role check and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setMarketTickSpacing()` | Full test-parity role, market-created, and uint8-bound checks. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setMarketSettlementFee()` | Full test-parity role, index, fee-bound, CBP-multiple, market-created checks and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setDefaultSettlementFee()` | Full test-parity role, index, fee-bound, CBP-multiple checks and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setMarketContinuousFee()` | Full test-parity role, max-fee, market-created checks and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `setDefaultContinuousFee()` | Full test-parity role, max-fee checks and storage write. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `claimSettlementFee()` | Full test-parity coverage in the executable artifact. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettlementFeeTest` passes under `MIDNIGHT_IMPL=verity`. |
| `claimContinuousFee()` | Full test-parity coverage in the executable artifact. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`. |
| `take()` | Full test-parity coverage in the executable artifact, including buy/sell pricing, callbacks, ratification, reduce-only, gates, liquidation lock, settlement fees, consumed limits, and health checks. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `TakeTest`, `TakeAmountsTest`, `TickGatingTest`, `GateTest`, `EcrecoverRatifierIntegrationTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `withdraw()` | Full test-parity coverage in the executable artifact. | Medium: source-shaped path with remaining proof extraction and dynamic ABI/callback trust boundaries. | `AuthorizationTest`, `OtherFunctionsTest`, and `ContinuousFeeTest` pass under `MIDNIGHT_IMPL=verity`. |
| `repay()` | Full test-parity coverage in the executable artifact, including callback and referral-fee bundle paths. | Medium: source-shaped path with remaining proof extraction and dynamic ABI/callback trust boundaries. | `AuthorizationTest`, `OtherFunctionsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `supplyCollateral()` | Full test-parity coverage in the executable artifact, including bitmap activation, collateral count, no-oracle paths, and bundle collateral supplies. | Medium: source-shaped path, but collateral storage uses 128 word slots rather than exact uint128 packing until Verity supports packed fixed-array elements. | `AuthorizationTest`, `OtherFunctionsTest`, `MaxAmountsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `withdrawCollateral()` | Full test-parity coverage in the executable artifact, including bitmap clearing, receiver path, no-oracle zero-debt path, and borrowed-collateral health checks. | Medium: source-shaped path, but collateral storage uses 128 word slots rather than exact uint128 packing until Verity supports packed fixed-array elements. | `AuthorizationTest`, `OtherFunctionsTest`, and `MidnightBundlesTest` pass under `MIDNIGHT_IMPL=verity`. |
| `liquidate()` | Full test-parity coverage in the executable artifact; focused proofs cover the RCF and total-units/accounting properties over named projections. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `LiquidationTest` passes 38/38 under `MIDNIGHT_IMPL=verity`; proofs in `Midnight/Proofs/RCF.lean`, `Refinement.lean`, `Storage.lean`, and `UnitsAccounting.lean`. |
| `setConsumed()` | Full test-parity authorization check, monotonic consumed-amount check, storage write, and getter. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `OtherFunctionsTest` consumed cases; `Midnight/Contract.lean` |
| `setIsAuthorized()` | Full test-parity authorization setter; focused entry-guard proof shape remains available. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `AuthorizationTest` and `SetIsAuthorizedWithSigTest` pass under `MIDNIGHT_IMPL=verity`; `normalModeEntryGuards`. |
| `flashLoan()` | Full test-parity coverage in the executable artifact. | Medium: source-shaped path with callback ABI ECM boundary. | `FlashLoanTest` passes under `MIDNIGHT_IMPL=verity`. |
| `touchMarket()` | Source-level CREATE2/SSTORE2 ECM surface is now emitted directly by Verity; exact market initcode layout and decode remain trust-surface obligations. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Contract.lean`; `python3 scripts/report_yul_identity_gap.py --midnight --enforce-configured-gate`; direct artifact generation without Midnight Yul patch. |
| `updatePositionView()` | Full test-parity coverage in the executable artifact; focused fee/slash/update returned-credit proofs remain available. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`; `updatePositionViewSequence`, `Midnight/Proofs/UnitsAccounting.lean`. |
| `updatePosition()` | Full test-parity coverage in the executable artifact; focused post-update credit cover proofs remain available. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `ContinuousFeeTest` passes under `MIDNIGHT_IMPL=verity`; `badDebtCoversTwoPostUpdateCredits`, `badDebtCoversTwoStoredCreditsAfterUpdates`. |
| `lastLossFactor()` | Focused storage projection for lender-credit proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/Storage.lean` |
| `collateralBitmap()` | Full test-parity getter/effects in the executable artifact; focused bitmap schedule and clear-step proofs remain available. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `OtherFunctionsTest` bitmap cases pass under `MIDNIGHT_IMPL=verity`; `Midnight/Proofs/BitmapSchedule.lean`. |
| `collateral()` | Full test-parity getter/effects in the executable artifact; focused selected-collateral projection proofs remain available. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `OtherFunctionsTest`, `LiquidationTest`, and `MaxAmountsTest` pass under `MIDNIGHT_IMPL=verity`; `Midnight/Proofs/CollateralLoop.lean`, `Midnight/Proofs/Storage.lean`. |
| `toId()` | Full test-parity coverage, including hardfork-stability and injectivity paths. | Medium: source-level ECM computes the Solidity `IdLib.toId` CREATE2/SSTORE2 preimage; the remaining boundary is proving the low-level ABI/initcode byte layout, not a scaffolded id. | `OtherFunctionsTest.testToId`, `OtherFunctionsTest.testToIdStableAcrossHardfork`; `midnightMarketId` in `Midnight/Contract.lean`. |
| `toMarket()` | Full test-parity coverage, including revert-if-not-created and SSTORE2 roundtrip. | Medium: source-level ECM returns the exact SSTORE2 runtime payload from code; dynamic `Market` return typing remains a low-level Verity ECM boundary, with no Midnight-specific Yul patch. | `OtherFunctionsTest.testToMarket`, `OtherFunctionsTest.testToMarketRevertsIfNotCreated`; `midnightMarketReturnFromCode` in `Midnight/Contract.lean`. |
| `creditOf()` | Focused storage projection for lender-credit proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/Storage.lean` |
| `debtOf()` | Focused storage projection for liquidation/accounting proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/Storage.lean` |
| `totalUnits()` | Focused total-units coverage proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `Midnight/Proofs/UnitsAccounting.lean`, `Midnight/Proofs/MarketLedger.lean` |
| `lossFactor()` | Focused bad-debt loss-factor update proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `badDebtLocalStep_newLossFactorEqSolidityFormula` |
| `tickSpacing()` | Full test-parity getter over initialized market state. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` |
| `withdrawable()` | Focused withdrawable-increase proof in liquidation branch. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `withdrawableAfterRepay`, `normalModeAfterBadDebtStep_withdrawableIncreases` |
| `settlementFeeCbps()` | Full test-parity getter over the seven stored CBP fee slots. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest`; `Midnight/Contract.lean` |
| `continuousFee()` | Focused continuous-fee update branch only. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `badDebtLocalStep_continuousFeeCreditEqSolidityFormula` |
| `continuousFeeCredit()` | Focused continuous-fee-credit slash/update proofs. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `continuousFeeCreditAfterBadDebt`, `badDebtLocalStep_continuousFeeCreditEqSolidityFormula` |
| `pendingFee()` | Focused pending-fee slashing proof. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `postSlashPendingFee` |
| `lastAccrual()` | Focused `updatePositionView` returned triple model. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `updatePositionViewSequence` |
| `liquidationLocked()` | Full test-parity coverage in the executable artifact; proof model still treats lock fact as an entry guard. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `TakeTest` liquidation-lock callback cases pass under `MIDNIGHT_IMPL=verity`; `NormalModeCall` in `Midnight/Proofs/Storage.lean`. |
| `isHealthy()` | Full test-parity coverage in the executable artifact; focused normal-mode health projection remains the proof anchor. | Medium: Solidity-shaped control flow/projection, with remaining ABI/storage/proof extraction boundary. | `TakeTest`, `OtherFunctionsTest`, and `LiquidationTest` pass under `MIDNIGHT_IMPL=verity`; `normalModeMaxRepaidHealthyWithin3`, `normalModeLiquidateProjection_storageHealthyWithinOne`. |
| `settlementFee()` | Full test-parity coverage in the executable artifact. | High: source-shaped scalar getter/setter or helper under current Verity surface. | `SettersTest` and `SettlementFeeTest` pass under `MIDNIGHT_IMPL=verity`. |

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
