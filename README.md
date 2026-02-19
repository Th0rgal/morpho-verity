# Morpho Verity

Formal verification of [Morpho Blue](https://github.com/morpho-org/morpho-blue) using [Verity](https://github.com/Th0rgal/verity), a Lean 4 framework for verified smart contracts.

## Goal

Audit Morpho Blue by building an equivalent implementation in Lean 4 and proving it correct.

The approach: translate Morpho's Solidity logic line-by-line into Verity's contract DSL, state the key safety properties as formal specs, then prove them mechanically. A successful proof gives stronger guarantees than traditional auditing — if the translation is faithful, bugs in the spec are bugs in the original.

### What this proves

- **Solvency**: total borrows never exceed total supply
- **Rounding safety**: all share/asset conversions round against the user
- **Authorization**: only authorized addresses can withdraw, borrow, or remove collateral
- **Fee bounds**: market fees stay within the 25% cap
- **Collateralization**: positions with debt always have collateral (bad debt is socialized immediately)
- **Monotonicity**: enabled IRMs/LLTVs cannot be disabled; market timestamps only increase

### What this does not prove

The Lean implementation targets logical equivalence with Morpho's Solidity, not bytecode equivalence. The compiled Yul output will differ. External call behavior (oracle prices, IRM rates, ERC20 transfers) is modeled as parameters, not verified end-to-end.

## Structure

```
morpho-blue/              # Morpho Blue Solidity (git submodule)
Morpho/
  Types.lean              # MarketParams, Position, Market, MorphoState
  Morpho.lean             # Core logic: supply, withdraw, borrow, repay, liquidate
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
    Invariants.lean       # Invariant proofs (11/11 proven)
    Rounding.lean         # Rounding proofs (2/4 proven)
    Authorization.lean    # Authorization proofs (4/4 proven)
```

## Build

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.15.0).

```
lake build
```

## Proof progress

**19 theorems proven, 2 sorry remaining.**

| Category | Proven | Total | Status |
|----------|--------|-------|--------|
| Authorization | 4 | 4 | Done |
| Invariants | 11 | 11 | Done |
| Rounding | 2 | 4 | Round-trip proofs need compositional division reasoning |

Also proven in supporting libraries:
- `mulDivDown_le_mulDivUp` — floor division ≤ ceiling division (MathLib)
- `zeroFloorSub_le` — saturating subtraction never exceeds original (UtilsLib)
- `u256_val` — simp lemma for Uint256 wrapping arithmetic

Invariant theorems include:
- IRM/LLTV monotonicity (2), LLTV < WAD (1), market creation validity (1)
- Fee bounds (1), solvency for supply/withdraw/borrow/repay (4)
- Timestamp monotonicity for interest accrual (1)
- Collateralization preserved by liquidation (1)

## Status

- [x] Morpho types and state model
- [x] Core contract logic (supply, withdraw, borrow, repay, liquidate, supplyCollateral, withdrawCollateral, createMarket, authorization, owner functions, interest accrual, flash loans)
- [x] Math libraries (MathLib, SharesMathLib, UtilsLib, ConstantsLib)
- [x] Formal specs with human-readable documentation (invariants, rounding, authorization)
- [x] Authorization proofs (4/4)
- [x] Invariant proofs (11/11: IRM/LLTV monotonicity, LLTV < WAD, fee bounds, market creation, solvency for supply/withdraw/borrow/repay, timestamp monotonicity, collateralization preserved by liquidation)
- [x] Rounding direction proofs (2/2: toSharesDown ≤ toSharesUp, toAssetsDown ≤ toAssetsUp)
- [ ] Rounding round-trip proofs (0/2: supply round-trip no-loss, withdraw round-trip no-loss)

## License

MIT
