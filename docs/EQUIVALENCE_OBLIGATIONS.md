# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

Groundwork only. Tracking format is proposed; current bridge assumptions are still assumptions.

## Scope

`Morpho/Proofs/SolidityBridge.lean` currently transfers invariants under semantic-equivalence hypotheses.
Each hypothesis must be tracked as a proof obligation with owner and status.

## Obligation Table

| Obligation ID | Bridge hypothesis | Operation | Macro migrated | Status |
|---------------|-------------------|-----------|:--------------:|--------|
| `OBL-SUPPLY-SEM-EQ` | `supplySemEq` | `supply` | | `assumed` |
| `OBL-WITHDRAW-SEM-EQ` | `withdrawSemEq` | `withdraw` | | `assumed` |
| `OBL-BORROW-SEM-EQ` | `borrowSemEq` | `borrow` | | `assumed` |
| `OBL-REPAY-SEM-EQ` | `repaySemEq` | `repay` | | `assumed` |
| `OBL-SUPPLY-COLLATERAL-SEM-EQ` | `supplyCollateralSemEq` | `supplyCollateral` | | `assumed` |
| `OBL-WITHDRAW-COLLATERAL-SEM-EQ` | `withdrawCollateralSemEq` | `withdrawCollateral` | | `assumed` |
| `OBL-LIQUIDATE-SEM-EQ` | `liquidateSemEq` | `liquidate` | | `assumed` |
| `OBL-ACCRUE-INTEREST-SEM-EQ` | `accrueInterestSemEq` | `accrueInterest` | | `assumed` |
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | Y | `assumed` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | Y | `assumed` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | Y | `assumed` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | | `assumed` |
| `OBL-SET-OWNER-SEM-EQ` | `setOwnerSemEq` | `setOwner` | Y | `assumed` |
| `OBL-SET-FEE-RECIPIENT-SEM-EQ` | `setFeeRecipientSemEq` | `setFeeRecipient` | Y | `assumed` |
| `OBL-CREATE-MARKET-SEM-EQ` | `createMarketSemEq` | `createMarket` | Y | `assumed` |
| `OBL-SET-FEE-SEM-EQ` | `setFeeSemEq` | `setFee` | | `assumed` |
| `OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ` | `accrueInterestPublicSemEq` | `accrueInterestPublic` | | `assumed` |
| `OBL-FLASH-LOAN-SEM-EQ` | `flashLoanSemEq` | `flashLoan` | | `assumed` |

**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in
`MacroSlice.lean` and is ready for end-to-end semantic bridge composition once verity#998
lands. 6/18 operations are macro-migrated; the remaining 12 are blocked on upstream macro
primitive support (internal calls, ERC20 module, callbacks, oracle calls, 2D struct access).

CI enforces macro migration status consistency: `scripts/check_semantic_bridge_obligations.py`
cross-references `macroMigrated` flags in config against stub detection in `MacroSlice.lean`.
`createMarket` uses `setMappingWord`/`getMappingWord` with manual word-offset addressing as a
workaround for the `.mappingStruct` storage type gap — the same pattern used by view functions.

## Semantic Bridge Discharge Path

The obligations above will be discharged via the Verity semantic bridge
([verity#998](https://github.com/Th0rgal/verity/issues/998),
[verity#1052](https://github.com/Th0rgal/verity/pull/1052)).

The discharge requires a three-layer correspondence for each operation:

```
Morpho.f args                        -- pure Lean model (this repo)
  = MorphoViewSlice.f.exec state     -- EDSL macro output (verity_contract)
  = EVMYulLean(compile(spec)).exec   -- verified EVM semantics (verity bridge)
```

- **Link 1** (pure Lean ↔ EDSL): requires that `MorphoViewSlice` functions in
  `MacroSlice.lean` have full implementations matching `Morpho.*`. Currently,
  simple view functions are fully implemented; complex operations (supply,
  borrow, liquidate, etc.) are stub/noop — these must be completed as macro
  primitive support grows in verity.

- **Link 2** (EDSL ↔ EVMYulLean): provided by the verity semantic bridge once
  Layers 2+3 are composed into per-function theorems. This eliminates the
  hand-rolled `interpretSpec` from the TCB.

Once both links are established, each `*SemEq` hypothesis in
`SolidityBridge.lean` becomes a provable lemma and the 67 bridge theorems
hold unconditionally against formally verified EVM semantics.

Machine-readable obligation status: [`config/semantic-bridge-obligations.json`](../config/semantic-bridge-obligations.json)

Lean-level obligation registry: `Morpho/Proofs/SemanticBridgeReadiness.lean`

## Spec Correspondence

For macro-migrated operations, `scripts/check_spec_correspondence.py` validates structural
correspondence between the macro-generated CompilationModel (`MacroSlice.lean`) and the
manual spec (`Spec.lean`):

- **Storage slots**: slot numbers must match between macro `storage` declarations and
  `Spec.lean` field definitions
- **Parameter count**: macro function parameters must match spec function parameters
- **Mutation count**: macro must not introduce spurious state mutations beyond if/else
  branching expansion (2x tolerance)
- **Stub detection**: macro-migrated operations must not be stubs

Known expected differences (not checked, handled by semantic bridge):
- **Events**: `Spec.lean` has `Stmt.emit` calls that MacroSlice omits (events don't affect state)
- **Stop**: `Spec.lean` ends functions with `Stmt.stop` (implicit in EDSL do-blocks)
- **Require expansion**: MacroSlice expands `requireOwner` into explicit `msgSender` + `require`

## Macro Migration Blockers

The 13 unmigrated operations depend on upstream verity macro capabilities.

**Resolved blockers** (verity `semantic-bridge` as of 2026-03-01): tuple destructuring,
`setStructMember`/`structMember` statement/expression primitives, `keccakMarketParams`
(via `externalCall`), `blockTimestamp`, `mstore`/`mload`, `getMappingUint`/`setMappingUint`
explicit translators, `Bytes32`/`Bool` type support.

**Remaining blockers**:

| Blocker | Operations affected | Count |
|---------|-------------------|:-----:|
| Internal function calls (`Stmt.internalCall`) | supply, withdraw, borrow, repay, liquidate, setFee, accrueInterest, accrueInterestPublic | 8 |
| ERC20 module (`ERC20.safeTransfer/From`) | supply, withdraw, borrow, repay, supplyCollateral, withdrawCollateral, liquidate, flashLoan | 8 |
| 2D struct mapping read/write (`structMember2`) | supply, withdraw, borrow, repay, supplyCollateral, withdrawCollateral, liquidate | 7 |
| External callbacks (`Callbacks.callback`) | supply, supplyCollateral, repay, liquidate, flashLoan | 5 |
| External contract calls (`Calls.withReturn`) | accrueInterest, accrueInterestPublic, withdrawCollateral, borrow, liquidate | 5 |
| `.mappingStruct` storage field type declarations | setFee, accrueInterest, accrueInterestPublic, + all struct-accessing ops | 3+ |
| Memory management (`mstore/mload`) | setAuthorizationWithSig, liquidate | 2 |
| Precompile access (`ecrecover`) | setAuthorizationWithSig | 1 |

**Note on createMarket**: Now fully macro-migrated using `setMappingWord`/`getMappingWord`
with manual word-offset addressing (the same pattern used by the view functions). All 4
previously identified primitive blockers (tuple destructuring, `setStructMember`,
`keccakMarketParams`, `blockTimestamp`) are resolved in the macro translator. The market
struct initialization uses word-level writes: word 0 = 0 (totalSupplyAssets|totalSupplyShares),
word 1 = 0 (totalBorrowAssets|totalBorrowShares), word 2 = blockTimestamp (lastUpdate in low
128 bits, fee=0 in high 128 bits). The `idToMarketParams` fields are unpacked (one per word).

## Primitive Coverage & Discharge Readiness

For macro-migrated operations, `scripts/check_primitive_coverage.py` analyzes which EDSL
primitives each function uses and cross-references against proven `PrimitiveBridge` lemmas
in the upstream verity semantic-bridge branch.

### Current status (verity `semantic-bridge` branch, 2026-03-01)

**Proven PrimitiveBridge lemmas** (EDSL ↔ compiled Yul): `getStorage`, `setStorage`,
`getStorageAddr`, `setStorageAddr` ([verity#1054](https://github.com/Th0rgal/verity/pull/1054)),
`require` (eq/neq/lt/gt), `msgSender`, `if_then_else`, `uint256.add/sub/mul/div/mod`,
`uint256.lt/gt/eq`, `calldataloadWord` (encoding/decoding)

**Proven semantic bridge theorems** (zero sorry, verity `semantic-bridge` at `8b3b482`):
- SimpleStorage: `store`, `retrieve` — Uint256-typed storage
- Counter: `increment`, `decrement`, `getCount` — arithmetic + Uint256 storage
- Owned: `getOwner`, `transferOwnership` — **Address-typed storage + access control**

The `owned_transferOwnership_semantic_bridge` theorem is the direct template for
morpho-verity's `setOwner` discharge. It proves EDSL ≡ compiled IR for an ownership
transfer pattern using `encodeStorageAddr`, direct `simp` unfolding, and
`Nat.and_two_pow_sub_one_eq_mod` for address masking.

**Proven MappingAutomation lemmas** (EDSL-level, zero sorry): `getMapping`,
`setMapping` (read-after-write, non-interference), `getMappingUint`, `setMappingUint`,
`getMapping2`, `setMapping2` — complete read/write/cross-slot preservation.
These operate at the `ContractState` level; the EDSL-to-Yul bridge for mapping
operations (keccak-based slot computation) is not yet in PrimitiveBridge.

| Operation | Primitives used | All proven? | Blocking |
|-----------|----------------|:-----------:|----------|
| `setOwner` | getStorageAddr, setStorageAddr, msgSender, require | READY | — |
| `setFeeRecipient` | getStorageAddr, setStorageAddr, msgSender, require | READY | — |
| `enableIrm` | getMapping, setMapping, getStorageAddr, msgSender, require | EDSL-READY | mapping bridge |
| `enableLltv` | getMappingUint, setMappingUint, getStorageAddr, msgSender, require | EDSL-READY | mapping bridge |
| `setAuthorization` | getMapping2, setMapping2, if_then_else, msgSender, require | EDSL-READY | mapping bridge |
| `createMarket` | getMappingWord, setMappingWord, externalCall, blockTimestamp, ... | GAPS | MappingWord + externalCall |

**Summary**: 2/6 migrated operations (setOwner, setFeeRecipient) are fully covered by
proven primitive lemmas and have a proven upstream template (`owned_transferOwnership`).
3/6 are EDSL-ready (mapping lemmas needed). 1/6 has additional gaps.

### Discharge proof structure

For detailed proof skeletons, see `Morpho/Proofs/SemanticBridgeDischarge.lean`.

The discharge follows the Owned template from verity's `SemanticBridge.lean`:
1. **State encoding**: `encodeStorageAddr` maps `ContractState.storageAddr` to IR `Nat → Nat`
2. **Address mask**: `Nat.and_two_pow_sub_one_eq_mod` proves `addr &&& addressMask = addr`
3. **Direct simp**: Unfold both EDSL and IR execution simultaneously
4. **Storage equality**: `by_cases` on slot index, `simp_all [beq_iff_eq]`

Key insight: The proofs do NOT need `setStorageAddr_matches_sstore` — direct
`simp` with `setStorageAddr` definition suffices. The proof bypasses the
Layer 3 sorry by unfolding directly (valid for small functions).

### Discharge sequence

Once verity#1052 merges and morpho-verity bumps the verity pin:

1. **After verity pin bump**: `setOwner`, `setFeeRecipient` — extend
   `owned_transferOwnership_semantic_bridge` pattern with idempotence check.
   All required lemmas proven, template exists. See `SemanticBridgeDischarge.lean`.
2. **After mapping bridge lemmas**: `enableIrm`, `enableLltv`, `setAuthorization` —
   MappingAutomation.lean already provides EDSL-level correctness; need bridge-level
   lemmas connecting `getMapping`/`setMapping` to `sload(keccak256(key,slot))`/
   `sstore(keccak256(key,slot),value)` in compiled Yul. MappingSlot.lean has the
   slot encoding infrastructure (`solidityMappingSlot` via real keccak256 FFI)
3. **After mapping bridge + MappingWord lemmas**: `createMarket` — now macro-migrated
   using `setMappingWord`/`getMappingWord`; needs bridge-level lemmas for word-offset
   mapping access in addition to the mapping bridge lemmas from step 2
4. **After remaining macro expansion**: 12 operations — requires internal calls, ERC20,
   callbacks, external contract calls

Machine-readable primitive coverage: `scripts/check_primitive_coverage.py --json-out`

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.

