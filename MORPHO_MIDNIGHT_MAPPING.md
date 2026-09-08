# Morpho Midnight interface mapping

Full implementation: `morpho-midnight-verity/Midnight/Generated/FullModel.lean`,
derived from pinned Solidity; no handwritten-body merge. Separate focused
projections remain under `Midnight/Proofs/` and are not full equivalence proofs.

Complete artifact status: present
Focused artifact status: present

This table is the existing CI interface-completeness index, not a support score
or a test-results snapshot. IDs refer to the pinned typed AST recorded in
`Midnight/Generated/FullModel.manifest.json`. Representation policies, caller
obligations and tests come from `node scripts/report_midnight_support.mjs --json`.
Fresh execution evidence: `python3 scripts/verify_midnight_pipeline.py`.

| Interface function | Source declaration ID | Full implementation |
|---|---|---|
| `INITIAL_CHAIN_ID()` | 39 | Generated FullModel |
| `position()` | 47 | Generated FullModel |
| `marketState()` | 52 | Generated FullModel |
| `consumed()` | 58 | Generated FullModel |
| `isAuthorized()` | 64 | Generated FullModel |
| `defaultSettlementFeeCbp()` | 70 | Generated FullModel |
| `defaultContinuousFee()` | 74 | Generated FullModel |
| `claimableSettlementFee()` | 78 | Generated FullModel |
| `roleSetter()` | 80 | Generated FullModel |
| `feeSetter()` | 82 | Generated FullModel |
| `feeClaimer()` | 84 | Generated FullModel |
| `tickSpacingSetter()` | 86 | Generated FullModel |
| `multicall()` | 149 | Generated FullModel |
| `setRoleSetter()` | 175 | Generated FullModel |
| `setFeeSetter()` | 200 | Generated FullModel |
| `setFeeClaimer()` | 225 | Generated FullModel |
| `setTickSpacingSetter()` | 250 | Generated FullModel |
| `setMarketTickSpacing()` | 313 | Generated FullModel |
| `setMarketSettlementFee()` | 463 | Generated FullModel |
| `setDefaultSettlementFee()` | 531 | Generated FullModel |
| `setMarketContinuousFee()` | 588 | Generated FullModel |
| `setDefaultContinuousFee()` | 629 | Generated FullModel |
| `claimSettlementFee()` | 672 | Generated FullModel |
| `claimContinuousFee()` | 760 | Generated FullModel |
| `take()` | 1531 | Generated FullModel |
| `withdraw()` | 1664 | Generated FullModel |
| `repay()` | 1785 | Generated FullModel |
| `supplyCollateral()` | 1916 | Generated FullModel |
| `withdrawCollateral()` | 2040 | Generated FullModel |
| `liquidate()` | 2643 | Generated FullModel |
| `setConsumed()` | 2700 | Generated FullModel |
| `setIsAuthorized()` | 2745 | Generated FullModel |
| `flashLoan()` | 2850 | Generated FullModel |
| `touchMarket()` | 3092 | Generated FullModel |
| `updatePositionView()` | 3235 | Generated FullModel |
| `updatePosition()` | 3274 | Generated FullModel |
| `lastLossFactor()` | 3420 | Generated FullModel |
| `collateralBitmap()` | 3437 | Generated FullModel |
| `collateral()` | 3458 | Generated FullModel |
| `toId()` | 3477 | Generated FullModel |
| `toMarket()` | 3519 | Generated FullModel |
| `creditOf()` | 3536 | Generated FullModel |
| `debtOf()` | 3553 | Generated FullModel |
| `totalUnits()` | 3566 | Generated FullModel |
| `lossFactor()` | 3579 | Generated FullModel |
| `tickSpacing()` | 3592 | Generated FullModel |
| `withdrawable()` | 3605 | Generated FullModel |
| `settlementFeeCbps()` | 3646 | Generated FullModel |
| `continuousFee()` | 3660 | Generated FullModel |
| `continuousFeeCredit()` | 3673 | Generated FullModel |
| `pendingFee()` | 3690 | Generated FullModel |
| `lastAccrual()` | 3707 | Generated FullModel |
| `liquidationLocked()` | 3724 | Generated FullModel |
| `isHealthy()` | 3821 | Generated FullModel |
| `settlementFee()` | 3972 | Generated FullModel |
