# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

5/18 obligations have Link 1 (Pure Lean ↔ EDSL) proven: setOwner, setFeeRecipient,
enableIrm, enableLltv, setAuthorization. The proofs are in
`Morpho/Proofs/SemanticBridgeDischarge.lean`.

4 of the 5 also have Link 2 (EDSL ↔ SupportedStmtList witness) proven in
`Morpho/Proofs/CompilationCorrectness.lean` (setOwner, enableIrm, enableLltv,
setAuthorization). setFeeRecipient is missing Link 2 because it reads two
distinct storage address fields — requires a new `SupportedStmtFragment`
constructor in verity. Link 3 (CompilationModel ↔ EVMYulLean) depends on
upstream verity infrastructure.

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
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | Y | `link1_proven` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | Y | `link1_proven` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | Y | `link1_proven` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | | `assumed` |
| `OBL-SET-OWNER-SEM-EQ` | `setOwnerSemEq` | `setOwner` | Y | `link1_proven` |
| `OBL-SET-FEE-RECIPIENT-SEM-EQ` | `setFeeRecipientSemEq` | `setFeeRecipient` | Y | `link1_proven` |
| `OBL-CREATE-MARKET-SEM-EQ` | `createMarketSemEq` | `createMarket` | | `assumed` |
| `OBL-SET-FEE-SEM-EQ` | `setFeeSemEq` | `setFee` | | `assumed` |
| `OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ` | `accrueInterestPublicSemEq` | `accrueInterestPublic` | | `assumed` |
| `OBL-FLASH-LOAN-SEM-EQ` | `flashLoanSemEq` | `flashLoan` | | `assumed` |

**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in
`MacroSlice.lean` and is ready for end-to-end semantic bridge composition once verity#1065
lands. 5/18 operations are macro-migrated; the remaining 13 are blocked on upstream macro
primitive support (internal calls, ERC20 module, callbacks, oracle calls, 2D struct access).

CI enforces macro migration status consistency: `scripts/check_semantic_bridge_obligations.py`
cross-references `macroMigrated` flags in config against stub detection in `MacroSlice.lean`.
`createMarket` is a hard stub pending upstream verity EDSL support (tuple element access,
`externalCall` primitive, `blockTimestamp` value expression).

## Semantic Bridge Discharge Path

The obligations above will be discharged via the Verity hybrid canonical-semantics migration
([verity#1060](https://github.com/Th0rgal/verity/issues/1060),
[verity#1065](https://github.com/Th0rgal/verity/pull/1065)).

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

**Resolved blockers** (verity roadmap branch as of 2026-03-01): tuple destructuring,
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

**Note on createMarket**: Currently a hard stub (`require (0 == 1) "createMarket stub"`).
A full implementation using `setMappingWord`/`getMappingWord` with manual word-offset
addressing was attempted but reverted (preserved in git history, commit 82e5572).
The remaining blockers are: tuple element access, `externalCall` primitive, and
`blockTimestamp` as a value expression. These constructs are not yet supported by
the current pinned verity revision (08d942a5).

## Primitive Coverage & Discharge Readiness

For macro-migrated operations, `scripts/check_primitive_coverage.py` analyzes which EDSL
primitives each function uses and cross-references against proven `PrimitiveBridge` lemmas
in the upstream verity roadmap branch.

### Current status (verity `roadmap/1060-hybrid-migration` branch, 2026-03-01)

**Proven PrimitiveBridge lemmas** (EDSL ↔ compiled Yul): `getStorage`, `setStorage`,
`getStorageAddr`, `setStorageAddr`, `require` (eq/neq/lt/gt), `msgSender`, `if_then_else`,
`uint256.add/sub/mul/div/mod`, `uint256.lt/gt/eq`, `calldataloadWord` (encoding/decoding),
`safeAdd`/`safeSub` (overflow/underflow), `getMapping_unfold`/`setMapping_unfold`

**Proven semantic bridge theorems** (zero sorry, as tracked in verity roadmap snapshots):
- SimpleStorage: `store`, `retrieve` — Uint256-typed storage
- Counter: `increment`, `decrement`, `getCount` — arithmetic + Uint256 storage
- Owned: `getOwner`, `transferOwnership` — **Address-typed storage + access control**
- SafeCounter: `increment`, `decrement`, `getCount` — checked arithmetic, revert cases
- OwnedCounter: mixed-type multi-slot encoding with access control

The `owned_transferOwnership_semantic_bridge` theorem is the direct template for
morpho-verity's `setOwner` discharge. It proves EDSL ≡ compiled IR for an ownership
transfer pattern using `encodeStorageAddr`, direct `simp` unfolding, and
`Nat.and_two_pow_sub_one_eq_mod` for address masking.

The `ownedCounter` proofs demonstrate mixed Address+Uint256 slots via `encodeMixedStorage`
and mapping operations via `getMapping_unfold`/`setMapping_unfold`, which are directly
applicable to morpho-verity's enableIrm/enableLltv/setAuthorization operations.

**Proven MappingAutomation lemmas** (EDSL-level, zero sorry): `getMapping`,
`setMapping` (read-after-write, non-interference), `getMappingUint`, `setMappingUint`,
`getMapping2`, `setMapping2` — complete read/write/cross-slot preservation.
These operate at the `ContractState` level; the EDSL-to-Yul bridge for mapping
operations (keccak-based slot computation) is not yet in PrimitiveBridge.

| Operation | Primitives used | Link 1 | Link 2 (CompilationCorrectness) | Link 3 |
|-----------|----------------|:------:|:-------------------------------:|--------|
| `setOwner` | getStorageAddr, setStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | after verity pin bump |
| `setFeeRecipient` | getStorageAddr (×2), setStorageAddr, msgSender, require | **PROVEN** | GAP (2-field read) | needs verity support |
| `enableIrm` | getMapping, setMapping, getStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | after verity pin bump |
| `enableLltv` | getMappingUint, setMappingUint, getStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | after verity pin bump |
| `setAuthorization` | getMapping2, setMapping2, if_then_else, msgSender, require | **PROVEN** | **PROVEN** | after verity pin bump |
| `createMarket` | getMappingWord, setMappingWord, externalCall, blockTimestamp, ... | pending | pending | MappingWord + externalCall |

**Summary**: All 5 migrated operations have Link 1 (Pure Lean ↔ EDSL) fully proven.
4/5 (setOwner, enableIrm, enableLltv, setAuthorization) also have
Link 2 (EDSL ↔ SupportedStmtList) proven in `Morpho/Proofs/CompilationCorrectness.lean`.
setFeeRecipient has Link 1 proven but Link 2 is blocked on verity multi-field-read support.
createMarket is a hard stub (not macro-migrated) — Link 1 not yet provable.

### Discharge proof structure

`Morpho/Proofs/SemanticBridgeDischarge.lean` contains actual proofs for the
first link of the discharge chain: **Pure Lean ↔ EDSL equivalence**.

The discharge has three links per obligation:
1. **Link 1** (this repo): `Morpho.f ↔ MorphoViewSlice.f` — proven for setOwner, setFeeRecipient, enableIrm, enableLltv, setAuthorization
2. **Link 2** (this repo, current pin): `EDSL ↔ SupportedStmtList witness` — proven for setOwner, enableIrm, enableLltv, setAuthorization in `Morpho/Proofs/CompilationCorrectness.lean`; missing for setFeeRecipient (requires verity multi-field-read support)
3. **Link 3** (verity): `CompilationModel ↔ EVMYulLean(Yul)` — EndToEnd theorem

After bumping to a post-`verity#1065` revision, Link 2 transitions from this
legacy `interpretSpec` checkpoint to the typed-IR semantic bridge path.

**Link 1 proof pattern** (for all 5 proven operations):
1. Define `encodeMorphoState : MorphoState → ContractState` matching MacroSlice storage
2. Run EDSL function on encoded state, decode result to `Option MorphoState`
3. Unfold EDSL monadic chain (`bind`, `msgSender`, `getStorageAddr`, `require`, etc.)
4. `split <;> simp_all` closes all cases after `beq_iff_eq`/`bne_iff_ne` normalization

**Link 2 proof pattern** (for 4 proven operations in `Morpho/Proofs/CompilationCorrectness.lean`):
1. Define `morphoFields : List Field` matching the `verity_contract MorphoViewSlice` storage layout
2. State theorem: `SupportedStmtList morphoFields <function_body_stmts>`
3. Construct witness using the appropriate `SupportedStmtFragment` constructor
4. Close obligations via `native_decide` (field resolution) and `decide` (literal checks)
5. setFeeRecipient is excluded: reads two `getStorageAddr` fields (ownerSlot + feeRecipientSlot), which exceeds the single-field-read `SupportedStmtFragment` constructors available in verity 08d942a5

### Discharge sequence

Once verity#1065 merges and morpho-verity bumps the verity pin:

1. **Links 1+2 proven (4 ops), Link 3 after verity pin bump**: `setOwner`,
   `enableIrm`, `enableLltv`, `setAuthorization` — Link 1 proven in
   `SemanticBridgeDischarge.lean`, Link 2 proven in `CompilationCorrectness.lean`.
   `setFeeRecipient` has Link 1 proven but Link 2 blocked on verity multi-field-read
   support. Link 3 needs verity pin bump for compiled IR ↔ EVMYulLean composition.
2. **After macro migration + mapping bridge**: `createMarket` — currently a hard stub;
   once verity supports tuple access, externalCall, and blockTimestamp, the implementation
   can be restored and will need bridge-level lemmas for word-offset mapping access
3. **After remaining macro expansion**: 12 operations — requires internal calls, ERC20,
   callbacks, external contract calls

Machine-readable primitive coverage: `scripts/check_primitive_coverage.py --json-out`

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.
