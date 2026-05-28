# Arithmetic Fidelity

Morpho Blue is compiled with Solidity 0.8 checked arithmetic. The current Lean
model and macro slice do not make every bare arithmetic operation a Solidity
panic path automatically, so arithmetic fidelity is tracked explicitly here.

## Current Model

- `Morpho/Libraries/MathLib.lean` documents that `mulDivDown` and `mulDivUp`
  use ordinary checked-multiplication semantics at the Solidity layer, not
  full-precision `mulDiv512` semantics.
- Proofs that need Solidity checked-arithmetic reachability carry explicit
  `h_no_overflow`-style hypotheses. Those are not discharged globally today.
- `Morpho/Contract.lean` uses Verity's Solidity-0.8 panic helpers
  (`addPanic`, `subPanic`, and `mulPanic`) for translated checked arithmetic
  sites where the Solidity source would revert on overflow or underflow.
- The macro slice also adds explicit `uint128 overflow` guards for macro writes
  that model Solidity `toUint128()` casts after additions or interest accrual.

## Checked/Panic Helper Coverage

The macro slice now uses checked helpers for these source-level arithmetic
families:

| Solidity family | Macro helper coverage |
|-----------------|-----------------------|
| Timestamp elapsed calculation | `subPanic currentTimestamp currentLastUpdate` |
| Accrual Taylor first term | `mulPanic borrowRateVal elapsed` |
| Accrual term summation | `addPanic secondTerm thirdTerm`, `addPanic firstTerm secondPlusThird` |
| Accrual asset growth | `addPanic totalBorrowAssets_ interest`, `addPanic totalSupplyAssets_ interest` |
| Fee-share denominator construction | `addPanic totalSupplyShares_ 1000000`, `subPanic newTotalSupplyAssets feeAmount`, `addPanic supplyAssetsAfterFee 1` |
| Position and market share/asset updates | `addPanic` / `subPanic` around supply, withdraw, borrow, repay, and liquidation field updates |
| Virtual asset/share terms | `addPanic` for `+ 1` and `+ 1000000` denominators before share conversions |
| Liquidation LIF and bad-debt reductions | `subPanic` for WAD/LLTV terms and total/position reductions |

## Guarded `uint128` Cast Sites

The macro slice currently guards these write families against exceeding
`2^128 - 1`:

| Solidity family | Macro guard coverage |
|-----------------|----------------------|
| Interest accrual assets | `newTotalBorrowAssets`, `newTotalSupplyAssets` |
| Fee-share minting | `newFeeRecipientShares`, `newTotalSupplyShares` |
| Collateral supply | `newCollateral` |
| Supply shares/assets | `newPosSupplyShares`, `newTotalSupplyShares`, `newTotalSupplyAssets` |
| Borrow shares/assets | `newPosBorrowShares`, `newTotalBorrowShares`, `newTotalBorrowAssets` |

Subtractions from existing packed `uint128` fields are guarded by Solidity-style
underflow checks in the surrounding logic rather than separate `toUint128`
upper-bound checks. Zero-floor reductions are explicitly modeled where Morpho
uses `UtilsLib.zeroFloorSub(...).toUint128()`.

## Proof-Side Overflow Assumptions

The following proof areas still rely on explicit overflow assumptions instead
of fully discharged reachability facts:

| File | Representative assumptions |
|------|----------------------------|
| `Morpho/Proofs/Invariants.lean` | `h_no_overflow`, `h_supply_no_overflow`, `h_borrow_no_overflow`, `h_shares_no_overflow`, `h_shares_vs_no_overflow`, `h_denom_no_overflow` |
| `Morpho/Proofs/ShareConsistency.lean` | `h_no_overflow`, `h_pos_no_overflow`, `h_total_no_overflow` for position/total share no-overflow assumptions across supply, borrow, accrue-interest, and set-fee paths |
| `Morpho/Proofs/SolidityBridge.lean` | `h_no_overflow` transfer theorem conditions required before applying pure-model invariants |

These hypotheses are valid proof obligations, not unconditional Solidity
equivalence claims. A future discharge should either prove they are implied by
the translated Solidity checked-arithmetic paths or convert the corresponding
macro operations to checked/panic helpers with bridge lemmas.

## Gates And Gaps

- `config/yul-rewrite-proof-obligations.json` tracks the current
  `checked_add` width-alignment rewrite proof obligation used by the Yul
  identity pipeline.
- `scripts/check_yul_rewrite_proof_obligations.py` verifies that rewrite proof
  placeholders remain synchronized with `Morpho/Proofs/YulRewriteProofs.lean`.
- `scripts/check_primitive_coverage.py` reports missing/partial arithmetic and
  low-level primitives for macro-migrated operations.

Remaining work:

- Audit each macro `add`, `sub`, `mul`, and `div` against the exact Solidity
  source statement that produced it.
- Replace bare arithmetic with Verity checked/panic helpers where those helpers
  preserve Morpho v1 Solidity behavior.
- Keep `mulDiv512Down/Up` out of executable Morpho v1 semantics unless the
  target Solidity semantics deliberately changes.
