# Morpho Verity Refactor Plan

## Scope and Evidence

This plan targets `Th0rgal/morpho-verity` as checked out in `temp/morpho-verity`, with its Verity dependency updated locally from `4ebe4931d25e5a1594fcd3f43ff040ecc3c4225a` to current upstream `b699e30060cb80b3fd26c62010ecf5b151f31ab2`. `lake update verity` completed and refreshed the dependency graph.

Compared sources:

- Morpho Verity: `Morpho/Morpho.lean`, `Morpho/Types.lean`, `Morpho/Compiler/MacroSlice.lean`, `Morpho/Compiler/Generated.lean`, docs/config proof-boundary files.
- Original Solidity: `morpho-blue/src/Morpho.sol`, `src/interfaces/IMorpho.sol`, `src/libraries/{SharesMathLib,MathLib,UtilsLib,MarketParamsLib,EventsLib,ErrorsLib}.sol`.
- Faithful Verity reference: `unlink-monorepo/protocol/contracts/verity/UnlinkXyz/Pool/{Contract,VerifierRouter,Compile,FormalAudit}.lean` and `protocol/contracts/verity/README.md`.
- Current Verity upstream docs: `README.md`, `docs/VERIFICATION_STATUS.md`, `docs/EXTERNAL_CALL_MODULES.md`, `docs/INTERPRETER_FEATURE_MATRIX.md`, `docs/UNLINK_AUDIT_VERITY_CORE_PROMPT.md`.

The main conclusion is that Morpho Verity should stop treating its current hand-written pure model as the primary translation. It should instead make the `verity_contract` macro model the canonical Solidity-facing artifact, like Unlink does, and keep a separate proof/spec layer that is explicitly connected to the generated entrypoints.

## Current Verity Features Since the Morpho Pin

Features now available and directly relevant to a faithful Morpho port:

- `verity_contract` is a much richer Solidity-facing frontend: named `struct` declarations, custom `errors`, `event_defs`, `constants`, `immutables`, `modifiers`, `view` functions, `initializer`, `nonreentrant`, role-style `requires(...)`, storage namespaces, and local obligations.
- Typed storage layout is more expressive: packed fields, mapping structs, nested storage structs, explicit slots, ERC-7201 namespaces, named struct-member access, and nested storage struct accessors.
- ABI/data support has improved: tuple returns, fixed arrays, dynamic arrays, dynamic struct-array parameters, array-return helper binds, dynamic array event payloads, projected static composite event payloads, event ABI parity for indexed dynamic/tuple payloads, and ABI JSON artifact generation.
- External-call modeling is no longer limited to ad hoc stubs. Verity now has External Call Modules (ECMs), standard ERC-20 transfer/transferFrom/approve/balance helpers, generic call/staticcall/value-call modules with revert-data bubbling, callback modules, precompile modules including ecrecover and BN254, and trust-report metadata.
- Hashing support is better: kernel-computable selector/Keccak literals, `keccak256_lit`, `abiEncodePackedWords`, static-segment packed hashing, SHA-256 helpers, and a documented memory-slice hashing boundary.
- Arithmetic support is closer to Solidity: explicit `safeAdd`/`safeSub`/`safeMul`/`safeDiv`, Solidity-0.8 panic-style checked arithmetic helpers, `pow`, fixed-point helpers, and full-precision `mulDiv512Down` / `mulDiv512Up` codegen with FullMath Yul helper support.
- Native proof backend moved forward: the old Verity-only Yul executor was removed as authority; the public target is native EVMYulLean dispatcher execution. Layer 2 has a generic whole-contract theorem for the supported fragment, zero Lean axioms in core, and helper-aware proof surfaces, though helper compositional proofs remain incomplete for the full macro surface.
- Trust reporting is now first-class: `--trust-report`, `--deny-unchecked-dependencies`, `--deny-assumed-dependencies`, `--deny-local-obligations`, `--deny-linear-memory-mechanics`, `--deny-event-emission`, `--deny-low-level-mechanics`, `--deny-runtime-introspection`, and related gates make assumptions reviewable per usage site.

Relevant current limitations:

- ECMs, low-level calls, raw memory/revert-data plumbing, full memory-slice Keccak, and some dynamic ABI decoding remain compiler/test-supported but not fully modeled in proof interpreters.
- Events/logs are supported for ABI/codegen parity, but are not part of the same proof-complete fragment as simple storage transitions.
- `externalCall` is still an assumed boundary unless replaced by a concrete ECM with a named assumption.
- Internal helper calls execute and compile, but helper-level theorem reuse across callers is not fully first-class in the generic proof stack.
- Verity arithmetic is EVM wrapping by default. Solidity 0.8 checked arithmetic must be represented explicitly with checked/panic helpers or require-guarded safe operations.

## Where Morpho Verity Is Not Faithful Today

The pure model in `Morpho/Morpho.lean` is useful for invariant proofs but is not a close line-by-line translation of `Morpho.sol`.

Major simplifications:

- `Id` is `Nat` and `marketId` is a Cantor-pair function. Solidity `Id` is `bytes32` and `MarketParamsLib.id()` is `keccak256(abi.encode(marketParams))` over 160 bytes.
- `MorphoState` is an abstract record of maps/functions. Solidity storage is packed structs and mappings at concrete slots.
- Reverts are modeled as `Option`, losing revert strings/custom payload shape and making sequencing less Solidity-like.
- External calls are parameters or booleans: IRM borrow rate, oracle price, ERC-20 transfer behavior, callbacks, ecrecover, chain id, address(this), and DOMAIN_SEPARATOR are not modeled as call sites.
- `_accrueInterest` is separated compositionally in the pure model. Solidity calls it internally in `supply`, `withdraw`, `borrow`, `repay`, `withdrawCollateral`, `liquidate`, `setFee`, and public `accrueInterest`.
- `setAuthorizationWithSig` takes `signatureValid : Bool` in the pure model instead of modeling EIP-712 hashing, nonce post-increment sequencing, and ecrecover.
- Events and callbacks are omitted in the pure model.
- `MarketParams` matching to `id` is assumed in helpers like `_isHealthy` and `_accrueInterest`; Solidity receives both only internally after computing `id`.
- Checked arithmetic and `uint128` casts are represented by side conditions or manual bounds rather than explicit Solidity 0.8 checked semantics.

The macro slice in `Morpho/Compiler/MacroSlice.lean` is closer to Solidity but still has major non-faithful surfaces:

- Top-level helper defs such as `mstore`, `rawLog`, `ecrecover`, `safeTransfer`, `safeTransferFrom`, `returnStorageWords`, `structMember`, `setStructMember`, `keccak256`, and `externalCall` are stubs or deterministic placeholders in the executable source semantics.
- `externalCall "borrowRate"` passes only `[irm, totalBorrowAssets]`; Solidity calls `IIrm(irm).borrowRate(marketParams, market[id])`, a richer ABI struct call.
- Oracle call is `externalCall "oraclePrice" [oracle]`; Solidity calls `IOracle(oracle).price()`.
- ERC-20 transfers and callbacks are unconditional no-ops in the model; Solidity conditionally calls callbacks when `data.length > 0` and uses SafeTransferLib semantics with optional boolean return handling.
- `createMarket` omits the post-create IRM initialization call `IIrm(irm).borrowRate(marketParams, market[id])` when `irm != address(0)`.
- `setFee` lacks `require(newFee != market[id].fee, ALREADY_SET)` in the macro slice.
- `setAuthorization` lacks `require(newIsAuthorized != isAuthorized[msg.sender][authorized], ALREADY_SET)`.
- Constructor lacks `require(newOwner != address(0))`, `DOMAIN_SEPARATOR` immutable assignment, and `SetOwner` event.
- `DOMAIN_SEPARATOR()` is computed via stubbed `mstore`/`keccak256` helpers and should be replaced by a real immutable/constant-backed value or explicit local obligation.
- Event declarations are manually patched in `Generated.lean` rather than declared in the macro source, unlike Unlink.
- `CreateMarket` event is missing the `MarketParams` payload; the generated event definition lists only indexed `id`.
- `IncrementNonce` event is absent from `morphoEvents`.
- Several event indexed/unindexed choices are patched manually and should be derived from `event_defs`.
- Health and liquidation logic is hand-inlined repeatedly rather than expressed as internal helpers mirroring `_isHealthy`, `_accrueInterest`, and library calls.
- Solidity library calls are flattened into ad hoc arithmetic expressions; this is acceptable only if each helper is separately translated and cross-checked.
- `mulDivDown` / `mulDivUp` currently use 256-bit multiply/divide helpers, while latest Verity has full-precision helpers that should be considered for exact Solidity behavior and proof obligations.
- The compiler boundary still uses local stubs and manual post-processing for Yul compatibility rather than a fully generated, trust-reported artifact.

## Lessons From the Unlink Verity Translation

The Unlink translation is the better pattern for Morpho:

- It makes the `verity_contract` declaration the source of the generated artifact, not a sidecar to a separate hand-written model.
- It preserves Solidity-facing ABI/storage shapes: named structs, packed fields, mapping structs, fixed/dynamic arrays, bytes/calldata surfaces, custom errors, events, constructors, and storage namespaces.
- It declares unresolved dependencies as named local obligations or ECM assumptions at the exact usage site instead of hiding them behind generic stub functions.
- It keeps generated artifacts and trust reports in CI, and rejects fallback artifacts that are not Verity-generated.
- It uses concrete Lean checks over generated spec metadata, artifact wrappers, constants, storage layout, and entrypoint bodies.
- It records out-of-model boundaries explicitly: crypto, circuit, token, liveness, event/data availability, upgrade layout, and low-level call semantics are not conflated with proven generated execution.

For Morpho, “faithful” should mean: the main Verity contract reads like `Morpho.sol`, preserves the ABI/storage/event/error surface, uses generated Verity Yul as the test artifact, and has a named assumption/proof boundary for every external dependency or Verity feature outside the proof-complete fragment.

## Target Architecture

Use two layers now, with proofs added back later only after they are redesigned:

1. `Morpho/Contract.lean`: canonical `verity_contract Morpho`, line-by-line with `src/Morpho.sol`.
2. `Morpho/Compiler/*.lean`: generated compiler boundary and artifact production.

The old pure model/spec/proof layer has been removed. The next proof milestone
should introduce fresh modules that clearly state whether each invariant is
proved over:

- generated macro contract execution,
- the pure model only,
- or the pure model plus a named semantic bridge assumption.

## What Must Be Translated

Translate these Solidity surfaces directly into `verity_contract`:

- Storage:
  - `DOMAIN_SEPARATOR` as immutable or compile-time/context-derived value with exact EIP-712 boundary.
  - `owner`, `feeRecipient`.
  - `position : mapping(Id => mapping(address => Position))`, preserving packed `borrowShares:uint128` and `collateral:uint128`.
  - `market : mapping(Id => Market)`, preserving packed `uint128` pairs.
  - `isIrmEnabled`, `isLltvEnabled`, `isAuthorized`, `nonce`, `idToMarketParams`.
- Types:
  - `Id` as `Bytes32`, not `Nat`.
  - `MarketParams`, `Position`, `Market`, `Authorization`, `Signature` as named macro structs matching `IMorpho.sol`.
- Errors/reverts:
  - Keep Solidity revert strings if targeting exact Morpho v1, or introduce named error wrappers only if the bytecode target changes. Do not silently replace revert surfaces.
- Events:
  - Declare all events in `event_defs`: `SetOwner`, `SetFee`, `SetFeeRecipient`, `EnableIrm`, `EnableLltv`, `CreateMarket`, `Supply`, `Withdraw`, `Borrow`, `Repay`, `SupplyCollateral`, `WithdrawCollateral`, `Liquidate`, `FlashLoan`, `SetAuthorization`, `IncrementNonce`, `AccrueInterest`.
  - Preserve indexed fields and full payloads, especially `CreateMarket(Id indexed id, MarketParams marketParams)`.
- Constructor:
  - `require(newOwner != address(0), ZERO_ADDRESS)`.
  - set `DOMAIN_SEPARATOR = keccak256(abi.encode(DOMAIN_TYPEHASH, block.chainid, address(this)))`.
  - set `owner`.
  - emit `SetOwner`.
- Owner functions:
  - `onlyOwner`, `setOwner`, `enableIrm`, `enableLltv`, `setFee`, `setFeeRecipient`.
- Market creation:
  - `createMarket`, including `id = marketParams.id()`, enabled checks, `lastUpdate`, `idToMarketParams`, event, and optional IRM initialization call.
- Supply/borrow/collateral/liquidation/flash-loan entrypoints:
  - `supply`, `withdraw`, `borrow`, `repay`, `supplyCollateral`, `withdrawCollateral`, `liquidate`, `flashLoan`.
  - Preserve callback ordering and transfer ordering.
  - Preserve “do not accrue interest” in `supplyCollateral`.
- Authorization:
  - `setAuthorization` including ALREADY_SET check.
  - `setAuthorizationWithSig`, including deadline, post-increment nonce semantics, EIP-712 hash, ecrecover, `IncrementNonce`, and `SetAuthorization`.
- Internal helpers:
  - `_isSenderAuthorized`.
  - `_accrueInterest`.
  - `_isHealthy` overloads.
  - `extSloads` loop and storage-word return behavior.
- Libraries:
  - `MarketParamsLib.id`.
  - `MathLib`: `wMulDown`, `wDivDown`, `wDivUp`, `mulDivDown`, `mulDivUp`, `wTaylorCompounded`.
  - `SharesMathLib`: virtual shares/assets conversions.
  - `UtilsLib`: `exactlyOneZero`, `min`, `toUint128`, `zeroFloorSub`.
  - `SafeTransferLib`: ERC-20 optional-bool-return behavior as ECM or named assumption.

## What May Remain Assumed

Assumptions should be usage-scoped and emitted in trust reports:

- ERC-20 compliance, no fee-on-transfer, no reentrancy on token calls, optional-bool-return behavior. The call mechanics should be translated with ERC-20 ECMs; token economic properties remain assumptions.
- IRM behavior: no reentrancy, no revert, bounded borrow rate, and semantics of `borrowRate(marketParams, market)`. The ABI call should be translated; the returned value remains an environment assumption.
- Oracle behavior: no revert, correct price scale, bounded price, no instant unsafe price drops. The ABI call should be translated; returned price remains an assumption.
- ECDSA/ecrecover cryptographic correctness. Use the precompile ECM; cryptographic soundness stays an assumption.
- Exact EIP-712 domain/digest layout until full memory-slice Keccak and ABI encoding are proof-modeled. Use concrete helper code plus literal/selector guards.
- Solidity compiler after Yul: Verity still trusts solc Yul-to-bytecode.
- Gas/liveness and external callee behavior.

These should not be assumed:

- Market id computation. It must be `keccak256(abi.encode(marketParams))` over the exact 5-word layout, not Cantor pairing.
- Storage packing/layout. It must be encoded in typed storage declarations and checked against Solidity.
- Event signatures/indexing/payload shape.
- Entry-point ordering around accrue-interest, state mutation, callback, transfer, and event emission.
- `uint128` casts and checked arithmetic. They must be explicit operations or guards.

## Refactor Phases

### Phase 1: Upgrade and Compile Baseline

- Keep Verity pinned to `b699e30060cb80b3fd26c62010ecf5b151f31ab2` or a tag if upstream cuts one.
- Replace stale imports and macro syntax in Morpho after the package split/current API changes.
- Run `lake build` and record all breakages as migration tasks.
- Add a `docs/VERITY_UPGRADE_b699e300.md` note listing new features used and old local stubs being removed.

Exit criteria:

- Lake resolves the latest Verity dependency.
- The old proof model still builds, even if the generated contract does not yet compile.

### Phase 2: Establish Canonical Contract Surface

- Create `Morpho/Contract.lean` modeled on Unlink’s `Contract.lean`.
- Move event declarations from `Generated.lean` into `event_defs`.
- Add named macro structs for `MarketParams`, `Position`, `Market`, `Authorization`, and `Signature`.
- Change generated artifact boundary so `Morpho.Compiler.Generated` wraps `Morpho.Contract.Morpho.spec` without hand-patched externals/events except where unavoidable and documented.
- Add compile-time guards for selectors and event signatures.

Exit criteria:

- All public selectors match Morpho Solidity.
- ABI/event metadata is generated from the macro contract.

### Phase 3: Replace Stubbed Helpers With Verity Features

- Replace stubbed `safeTransfer`/`safeTransferFrom` with standard ERC-20 ECMs or a Morpho-specific SafeTransfer ECM that implements SafeTransferLib’s exact optional-return behavior and revert strings.
- Replace `ecrecover` with `Compiler.Modules.Precompiles.ecrecover`.
- Replace ad hoc callback stubs with `Callbacks.callback` or focused ECMs for each Morpho callback interface.
- Replace `externalCall "borrowRate"` and `externalCall "oraclePrice"` with typed ABI ECMs for `IIrm.borrowRate` and `IOracle.price`.
- Replace `keccakMarketParams` placeholder with a real ABI-encoding/hash helper over five static words.
- Replace raw `mstore`/`rawLog` event surfaces with macro event emission where possible.
- Keep unavoidable memory-slice hashing as local obligations with exact names.

Exit criteria:

- No top-level no-op stubs remain in the canonical contract.
- Trust report lists only real external/proof boundaries.

### Phase 4: Translate Internal Helpers Line-by-Line

- Implement `_isSenderAuthorized` as an internal helper or modifier-friendly function.
- Implement `_accrueInterest` once and call it from all Solidity entrypoints in the same order as Solidity.
- Implement both `_isHealthy` overload shapes. The no-price overload should call oracle ECM, then delegate to the price overload.
- Implement `MarketParamsLib.id` as a reusable helper.
- Implement library helpers as small Verity functions or stdlib aliases, with comments mapping each to Solidity.
- Use current Verity internal helper call support, but document any generic helper-proof gaps separately from functional codegen.

Exit criteria:

- Entry-point bodies are no longer hand-inlining `_accrueInterest` and `_isHealthy` logic repeatedly.
- The contract source has a recognizable line-by-line mapping to `Morpho.sol`.

### Phase 5: Restore Exact Entry-Point Semantics

Fix known mismatches:

- Add constructor zero-owner check and `SetOwner`.
- Add `setFee` ALREADY_SET check.
- Add `setAuthorization` ALREADY_SET check.
- Add `createMarket` post-create IRM initialization call.
- Add conditional callbacks only when `data.length > 0`.
- Ensure transfer/callback/event order matches Solidity.
- Preserve `zeroFloorSub` in `repay` and `liquidate`.
- Preserve bad-debt liquidation order exactly.
- Preserve `extSloads` dynamic return behavior.

Exit criteria:

- A per-function correspondence table marks every Solidity statement as translated or explicitly assumed.

### Phase 6: Arithmetic and Casting Fidelity

- Audit every `+`, `-`, `*`, `/`, `toUint128`, and `uint128(...)`.
- Use Verity checked/panic helpers or explicit `require` guards to model Solidity 0.8 checked arithmetic.
- Use Verity full-precision `mulDiv512Down` / `mulDiv512Up` only if needed for exact desired semantics. Morpho v1 Solidity `MathLib.mulDivDown` uses ordinary checked `x * y / d`, so replacing it with full precision would be a semantic change unless done only in proof lemmas.
- Prove or assume only the boundedness facts Morpho Solidity itself relies on.

Exit criteria:

- No arithmetic side condition is hidden in theorem hypotheses without a corresponding Solidity checked-arithmetic path or explicit assumption.

### Phase 7: Generated Artifact and Differential Parity

- Generate Morpho Yul/bytecode only from the canonical macro contract.
- Build a Foundry harness equivalent to Unlink’s generated-artifact gate.
- Run Morpho Blue tests once against Solidity and once against Verity-generated artifacts.
- Keep a Yul identity/diff report, but do not require byte-for-byte identity where Verity deliberately emits different Yul.
- Gate artifacts on trust-report expectations.

Exit criteria:

- CI rejects fallback artifacts and stale generated artifacts.
- Differential tests cover the original Morpho Blue suite under both implementations.

### Phase 8: Proof Bridge Cleanup

- Split pure model proofs from generated execution proofs.
- For each invariant, state whether it is:
  - proved directly over generated contract execution,
  - proved over the pure model and transferred by a discharged bridge lemma,
  - or conditional on a named semantic bridge assumption.
- If a new bridge tracker is added, make generated entrypoints the source of truth.
- Remove outdated references to manual `CompilationModel` specs and stale Link 1 assumptions once bridge lemmas target the macro contract.

Exit criteria:

- The repo no longer claims Solidity equivalence from the pure model unless the generated-contract bridge is discharged or explicitly assumed.

## Verity Features Still Lacking or Needing Work

These are likely upstream Verity needs for a fully faithful Morpho port:

- First-class ABI encoding for `abi.encode(MarketParams)` and EIP-712 `abi.encode(typehash, struct)` with proof-modeled memory-slice Keccak.
- A standard typed ABI-call ECM generator for external interfaces returning structured values, not bespoke per-call Yul.
- Proof-modeled ECM semantics for common ERC-20 optional-return safe transfer patterns.
- Full proof interpreter support for low-level calls, returndata bubbling, dynamic memory arrays, and memory-slice hashing.
- Better helper-composition theorem reuse for internal helper calls used pervasively in line-by-line Solidity translations.
- A first-class `extSloads(bytes32[] calldata) returns (bytes32[] memory)` pattern with dynamic storage-word array returns.
- Cleaner support for Solidity revert strings if exact Morpho v1 revert payload parity is required. Current custom error support is stronger than string-revert proof parity.
- A standard EIP-712 helper library with documented assumptions around chain id and contract address.

## Features That Exist But Need Careful Semantic Mapping

- `emit` / `event_defs`: use this instead of raw logs, but remember proof coverage for event emission is not the same as storage-state proof coverage.
- `requireError` / `revertError`: useful for newer custom-error contracts, but Morpho v1 uses revert strings. Do not change the external revert surface unless accepted.
- `storage_namespace erc7201`: useful for Unlink/OZ, not needed for Morpho v1 singleton storage.
- `safeAdd`/`safeSub`/`safeMul` and panic helpers: Solidity 0.8 arithmetic checks are not automatic for bare `add`/`sub`/`mul`.
- `mulDiv512Down/Up`: stronger arithmetic than Morpho v1’s `x * y / d`; use only if explicitly preserving or proving the intended checked multiplication behavior.
- `externalCall` / `externalCallBind`: convenient but assumed. Prefer ECMs with named proof status and trust-report entries.
- `keccak256_lit`: good for constants/typehashes, not a substitute for runtime `keccak256(abi.encode(...))`.
- Dynamic array and struct-array ABI support: codegen is available, but proof interpreters have gaps for dynamic head-word decoding.
- `chainid`, `contractAddress`, and `selfBalance`: usable in codegen/source paths, but some runtime-introspection proof surfaces are partial and should remain trust-reported.

## Recommended Final Deliverables

- `Morpho/Contract.lean`: canonical Solidity-facing macro contract.
- `Morpho/Libraries/*.lean`: line-by-line library translations with proof lemmas.
- `Morpho/External/*.lean`: Morpho-specific ECMs for IRM, oracle, callbacks, and any SafeTransferLib gaps.
- `Morpho/Generated/*.lean`: artifact wrappers generated from `Morpho.spec`.
- `docs/TRUST_BOUNDARIES.md`: exact assumed/proved/unchecked surfaces from trust reports.
- Foundry parity gate equivalent to Unlink’s `test:duality`.

## Suggested Priority Order

1. Make the latest Verity pin build.
2. Create the canonical macro contract surface with correct storage/types/events/selectors.
3. Remove no-op stubs and replace them with ECMs or local obligations.
4. Translate internal helpers and entrypoints line-by-line.
5. Add generated artifact and differential test gates.
6. Reconnect invariant proofs to generated execution.
7. Only then retire the old pure-model equivalence claims.
