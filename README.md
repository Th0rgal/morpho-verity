# Morpho Verity

Formal verification of [Morpho Blue](https://github.com/morpho-org/morpho-blue) using [Verity](https://github.com/Th0rgal/verity), a Lean 4 framework for verified smart contracts.

## Goal

Audit Morpho Blue by building an equivalent implementation in Lean 4 and proving it correct.

The approach: translate Morpho's Solidity logic line-by-line into Verity's contract DSL, state the key safety properties as formal specs, then prove them mechanically. A successful proof gives stronger guarantees than traditional auditing — if the translation is faithful, bugs in the spec are bugs in the original.

### What this proves

- **Solvency**: total borrows never exceed total supply, preserved by all 16 state-mutating operations (supply, withdraw, borrow, repay, liquidate, accrueInterest, supplyCollateral, withdrawCollateral, createMarket, setFee, and all admin functions)
- **Rounding safety**: all share/asset conversions round against the user; round-trip supply-withdraw never returns more than deposited
- **Authorization**: only authorized addresses can withdraw, borrow, or remove collateral; liquidation requires an unhealthy position; signature-based delegation requires valid nonce and unexpired deadline
- **Fee bounds**: market fees stay within the 25% cap
- **Collateralization**: positions with debt always have collateral, preserved by all 16 operations including borrow and withdrawCollateral (guarded by health checks; bad debt is socialized by liquidation)
- **Monotonicity**: enabled IRMs/LLTVs cannot be disabled across all operations; market timestamps only increase through accrueInterest, setFee, and accrueInterestPublic
- **Exchange rate safety**: supply share exchange rate never decreases after interest accrual (accrueInterest and accrueInterestPublic); existing shareholders' per-share value is protected
- **Market isolation**: operations on one market never affect any other market's state, same-market user positions, or any position in other markets

### What this does not prove

The Lean implementation targets logical equivalence with Morpho's Solidity, not bytecode equivalence. The compiled Yul output will differ. External call behavior (oracle prices, IRM rates, ERC20 transfers, EIP-712 signature verification) is modeled as parameters, not verified end-to-end.

## Structure

```
morpho-blue/              # Morpho Blue Solidity (git submodule)
Morpho/
  Types.lean              # MarketParams, Position, Market, MorphoState, Authorization
  Morpho.lean             # Core logic: supply, withdraw, borrow, repay, liquidate, setAuthorizationWithSig
  Libraries/
    MathLib.lean          # WAD arithmetic (mulDivDown/Up, wMulDown, wTaylorCompounded)
    SharesMathLib.lean    # Share/asset conversion with virtual offset
    UtilsLib.lean         # exactlyOneZero, min, zeroFloorSub
    ConstantsLib.lean     # MAX_FEE, ORACLE_PRICE_SCALE, LIQUIDATION_CURSOR
  Specs/
    Invariants.lean       # Formal property definitions
    Rounding.lean         # Rounding direction specs
    Authorization.lean    # Access control specs
  Proofs/
    Invariants.lean       # Invariant proofs (98/98 proven)
    Rounding.lean         # Rounding proofs (4/4 proven)
    Authorization.lean    # Authorization proofs (11/11 proven)
```

## Build

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.15.0).

```
lake build
```

## Proof progress

**113 theorems proven, 0 sorry remaining.**

| Category | Proven | Total | Status |
|----------|--------|-------|--------|
| Authorization | 11 | 11 | Done |
| Invariants | 98 | 98 | Done |
| Rounding | 4 | 4 | Done |

Also proven in supporting libraries:
- `mulDivDown_le_mulDivUp` — floor division ≤ ceiling division (MathLib)
- `zeroFloorSub_le` — saturating subtraction never exceeds original (UtilsLib)
- `u256_val` — simp lemma for Uint256 wrapping arithmetic

Invariant theorems include:
- IRM monotonicity preserved by all 16 operations (enableIrm + 15 trivial) (16)
- LLTV monotonicity preserved by all 16 operations (enableLltv + 15 trivial) (16)
- LLTV < WAD (1), market creation validity (1)
- Fee bounds (1), solvency for all 16 operations: supply/withdraw/borrow/repay/accrueInterest/liquidate/supplyCollateral/withdrawCollateral/createMarket/setFee/enableIrm/enableLltv/setOwner/setFeeRecipient/setAuthorization/setAuthorizationWithSig (16)
- Timestamp monotonicity for accrueInterest/setFee/accrueInterestPublic (3)
- Collateralization preserved by all 16 operations: supply/withdraw/borrow/repay/accrueInterest/liquidate/supplyCollateral/withdrawCollateral/enableIrm/enableLltv/setOwner/setFee/setFeeRecipient/createMarket/setAuthorization/setAuthorizationWithSig (16)
- Market isolation for all 8 operations: accrueInterest/supply/withdraw/borrow/repay/liquidate/supplyCollateral/withdrawCollateral (8)
- Same-market position isolation for all 8 operations: accrueInterest/supply/withdraw/borrow/repay/supplyCollateral/withdrawCollateral/liquidate (8)
- Cross-market position isolation for all 8 operations: accrueInterest/supply/withdraw/borrow/repay/supplyCollateral/withdrawCollateral/liquidate (8)
- Exchange rate monotonicity for accrueInterest/accrueInterestPublic (2)
- Flash loan rejects zero assets (1), accrueInterestPublic rejects uninitialized markets (1)
- accrueInterestPublic preserves solvency and collateralization (2)

Authorization theorems include:
- Precondition style: unauthorized sender → withdraw/borrow/withdrawCollateral return none (3)
- Postcondition style: successful withdraw/borrow/withdrawCollateral → sender was authorized (3)
- Supply requires no authorization (1)
- Liquidation requires unhealthy position (1)
- Signature-based: expired deadline rejected, wrong nonce rejected, nonce incremented (3)

## Status

- [x] Morpho types and state model
- [x] Core contract logic (supply, withdraw, borrow, repay, liquidate, supplyCollateral, withdrawCollateral, createMarket, setAuthorization, setAuthorizationWithSig, owner functions, interest accrual, flash loans)
- [x] Math libraries (MathLib, SharesMathLib, UtilsLib, ConstantsLib)
- [x] Formal specs with human-readable documentation (invariants, rounding, authorization)
- [x] Authorization proofs (11/11: withdraw/borrow/withdrawCollateral require auth, supply doesn't, withdraw/borrow/withdrawCollateral satisfy postcondition specs, liquidation requires unhealthy position, sig rejects expired deadline, sig rejects wrong nonce, sig increments nonce)
- [x] Invariant proofs (98/98: IRM/LLTV monotonicity preserved by all 16 operations, LLTV < WAD, fee bounds, market creation, solvency for all 16 operations, timestamp monotonicity for accrueInterest/setFee/accrueInterestPublic, exchange rate monotonicity for accrueInterest/accrueInterestPublic, collateralization preserved by all 16 operations, market isolation for all 8 operations, same-market position isolation for all 8 operations, cross-market position isolation for all 8 operations, flashLoan rejects zero assets, accrueInterestPublic rejects uninitialized/preserves solvency/preserves collateralization)
- [x] Rounding proofs (4/4: toSharesDown ≤ toSharesUp, toAssetsDown ≤ toAssetsUp, supply round-trip protocol-safe, withdraw round-trip protocol-safe)

## License

MIT
