# Morpho Blue Mapping

Mechanical manifest for the active Morpho Blue parity target. The test status
below refers to the local parity run in `out/parity/` on
2026-06-05 17:54 Europe/Berlin:
`morpho_blue_solidity.log` and `morpho_blue_verity.log` both report 145 passing
tests, 0 failures, 0 skipped.

Proof status is scoped to the Lean surface checked by `lake build
Morpho.Proofs`; it does not mean byte-for-byte Solidity equivalence. Empirical
Solidity-vs-Verity behavior is covered by `./scripts/run_morpho_blue_parity.sh`.
For the generated Morpho entrypoint bodies, `Morpho.Proofs.Disciplines` now
states the field-movement, guard-extraction, and local no-overflow facts as
typed local axioms. The top-level refinement and property theorems build against
that explicit interface; replacing those axioms with smaller generated-body
proofs remains future work.

| Solidity function | Verity function | Proof status | Test status |
|-------------------|-----------------|--------------|-------------|
| `Morpho.constructor` | `Morpho.constructor` | Model compiles; owner initialization shape is in the generated contract body. | Covered by original Foundry setup in both Solidity and Verity runs. |
| `Morpho.setOwner` | `Morpho.setOwner` | Model compiles; owner-gated admin write, no protocol-health theorem. | Covered by `OnlyOwnerIntegrationTest`; parity run green. |
| `Morpho.enableIrm` | `Morpho.enableIrm` | Model compiles; owner-gated config write, no protocol-health theorem. | Covered by setup/admin paths in original tests; parity run green. |
| `Morpho.enableLltv` | `Morpho.enableLltv` | Model compiles; owner-gated config write, no protocol-health theorem. | Covered by setup/admin paths in original tests; parity run green. |
| `Morpho.setFee` | `Morpho.setFee` | Model compiles; accrual/config path, no standalone property theorem. | Covered by original integration/invariant suite; parity run green. |
| `Morpho.setFeeRecipient` | `Morpho.setFeeRecipient` | Model compiles; owner-gated config write, no protocol-health theorem. | Covered by original integration paths; parity run green. |
| `Morpho.createMarket` | `Morpho.createMarket` | Model compiles with explicit local obligation for IRM initialization call boundary. | Covered by `CreateMarketIntegrationTest`; parity run green. |
| `Morpho.supply` | `Morpho.supply` | `Morpho.Proofs.Refinement.refines_supply`; Property 1 transfer via `monotoneDiscipline_supply`, currently a generated-body local axiom. | Covered by `SupplyIntegrationTest`, library, and invariant tests; parity run green. |
| `Morpho.withdraw` | `Morpho.withdraw` | `Morpho.Proofs.Refinement.refines_withdraw`; Property 1 transfer via `monotoneDiscipline_withdraw`, currently a generated-body local axiom. | Covered by `WithdrawIntegrationTest`, library, and invariant tests; parity run green. |
| `Morpho.borrow` | `Morpho.borrow` | `Morpho.Proofs.Refinement.refines_borrow`; self-guarded post-health fact via `guardedDiscipline_borrow`, currently a generated-body local axiom. | Covered by `BorrowIntegrationTest` and invariant tests; parity run green. |
| `Morpho.repay` | `Morpho.repay` | `Morpho.Proofs.Refinement.refines_repay`; Property 1 transfer via `monotoneDiscipline_repay`, currently a generated-body local axiom. | Covered by `RepayIntegrationTest` and invariant tests; parity run green. |
| `Morpho.supplyCollateral` | `Morpho.supplyCollateral` | `Morpho.Proofs.Refinement.refines_supplyCollateral`; collateral-increase discipline is currently a generated-body local axiom. | Covered by `SupplyCollateralIntegrationTest` and invariant tests; parity run green. |
| `Morpho.withdrawCollateral` | `Morpho.withdrawCollateral` | `Morpho.Proofs.Refinement.refines_withdrawCollateral`; self-guarded post-health fact via `guardedDiscipline_withdrawCollateral`, currently a generated-body local axiom. | Covered by `WithdrawCollateralIntegrationTest` and invariant tests; parity run green. |
| `Morpho.liquidate` | `Morpho.liquidate` | `Morpho.Proofs.Refinement.refines_liquidate`; `Property2.sharp_property2` builds, with generated unhealthy-guard and pre-state bridge facts currently local axioms. | Covered by `LiquidateIntegrationTest` and invariant tests; parity run green. |
| `Morpho.flashLoan` | `Morpho.flashLoan` | Model compiles with explicit ECM/callback local obligation; no health theorem. | Covered by callback/integration paths in original suite; parity run green. |
| `Morpho.setAuthorization` | `Morpho.setAuthorization` | Model compiles; authorization storage write, no protocol-health theorem. | Covered by `AuthorizationIntegrationTest`; parity run green. |
| `Morpho.setAuthorizationWithSig` | `Morpho.setAuthorizationWithSig` | Model compiles with explicit local obligation for nonce/ecrecover ordering. | Covered by `AuthorizationIntegrationTest`; parity run green. |
| `Morpho.accrueInterest` | `Morpho.accrueInterest` | Model compiles; proof environment tracks no-accrual/borrow-index assumptions separately. | Covered by `AccrueInterestIntegrationTest`; parity run green. |
| `Morpho.extSloads` | `Morpho.extSloads` | Model compiles; storage-read utility, no protocol-health theorem. | Covered by `ExtSloadIntegrationTest`; parity run green. |
| `Morpho._accrueInterest` | `Morpho._accrueInterest` | Internal Verity implementation used by public accrual and market operations; no standalone theorem. | Exercised through public functions in parity run. |
| `Morpho._isHealthy` / overload with price | `Morpho._isHealthy`, `_isHealthyWithPrice` | `Morpho.Proofs.HealthFaithful.healthFaithful_of_noOverflow` links projected storage arithmetic to the health model. | Exercised by borrow, withdraw-collateral, and liquidation tests; parity run green. |
| `Morpho._isSenderAuthorized` | `Morpho._isSenderAuthorized` | Model compiles; authorization helper, no protocol-health theorem. | Exercised by authorization and on-behalf tests; parity run green. |
| `MathLib.wMulDown` | `Morpho.Libraries.MathLib.wMulDown` | Library model compiles; arithmetic support lemmas live under `Morpho.Proofs.Arith` and health proofs. | Covered by `MathLibTest` and protocol parity run. |
| `MathLib.wDivDown` | `Morpho.Libraries.MathLib.wDivDown` | Library model compiles; arithmetic support lemmas live under `Morpho.Proofs.Arith` and health proofs. | Covered by `MathLibTest` and protocol parity run. |
| `MathLib.wDivUp` | `Morpho.Libraries.MathLib.wDivUp` | Library model compiles; arithmetic support lemmas live under `Morpho.Proofs.Arith` and health proofs. | Covered by `MathLibTest` and protocol parity run. |
| `MathLib.mulDivDown` | `Morpho.Contract.mulDivDown` / `Morpho.Libraries.MathLib.mulDivDown` | Used by health/liquidation proofs, including floor/ceil monotonicity lemmas. | Covered by `MathLibTest` and protocol parity run. |
| `MathLib.mulDivUp` | `Morpho.Contract.mulDivUp` / `Morpho.Libraries.MathLib.mulDivUp` | Used by health/liquidation proofs, including floor/ceil monotonicity lemmas. | Covered by `MathLibTest` and protocol parity run. |
| `MathLib.wTaylorCompounded` | `Morpho.Libraries.MathLib.wTaylorCompounded` | Library model compiles; accrual behavior tested empirically. | Covered by `MathLibTest` and accrual parity run. |
| `SharesMathLib.toSharesDown` | `Morpho.Libraries.SharesMathLib.toSharesDown` | Library model compiles; share conversions exercised in operation bodies. | Covered by original library/protocol tests; parity run green. |
| `SharesMathLib.toAssetsDown` | `Morpho.Libraries.SharesMathLib.toAssetsDown` | Library model compiles; share conversions exercised in operation bodies. | Covered by original library/protocol tests; parity run green. |
| `SharesMathLib.toSharesUp` | `Morpho.Libraries.SharesMathLib.toSharesUp` | Library model compiles; share conversions exercised in operation bodies. | Covered by original library/protocol tests; parity run green. |
| `SharesMathLib.toAssetsUp` | `Morpho.Libraries.SharesMathLib.toAssetsUp` | Library model compiles; share conversions exercised in operation bodies. | Covered by original library/protocol tests; parity run green. |
| `UtilsLib.exactlyOneZero` | `Morpho.Libraries.UtilsLib.exactlyOneZero` | Library model compiles; input-mode checks exercised in operation bodies. | Covered by original library/protocol tests; parity run green. |
| `UtilsLib.min` | `Morpho.Libraries.UtilsLib.min` | Library model compiles; arithmetic helper. | Covered by original library/protocol tests; parity run green. |
| `UtilsLib.toUint128` | `Morpho.Libraries.UtilsLib.toUint128` | Library model compiles; packed-word bounds are used by health faithful lemmas. | Covered by original library/protocol tests; parity run green. |
| `UtilsLib.zeroFloorSub` | `Morpho.Libraries.UtilsLib.zeroFloorSub` | Library model compiles; arithmetic helper. | Covered by original library/protocol tests; parity run green. |
| `MarketParamsLib.id` | Verity market id computation in `Morpho.Contract` | Model compiles; market id is used throughout storage projections and parity tests. | Covered by market creation and all market operation tests; parity run green. |
| `SafeTransferLib.safeTransfer` | `MorphoSafeTransfer.safeTransferModule` ECM call | External-call trust boundary; model delegates ERC20 mechanics to ECM. | Covered empirically through original tests; parity run green. |
| `SafeTransferLib.safeTransferFrom` | `MorphoSafeTransfer.safeTransferFromModule` ECM call | External-call trust boundary; model delegates ERC20 mechanics to ECM. | Covered empirically through original tests; parity run green. |
| Periphery `MorphoLib.*` / `MorphoBalancesLib.*` / `MorphoStorageLib.*` | No direct Verity replacement; storage projections and getters live in `Morpho.Contract` and `Morpho.Proofs.Projection`. | Review helper surface, not part of the core Verity contract proof target. | Original periphery tests passed in both Solidity and Verity parity runs. |
