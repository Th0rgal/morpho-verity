# Midnight Verity Faithfulness Report

Status date: 2026-06-03

Reviewed Morpho state: `morpho-verity` commit `fccecaa` (`Add executable Morpho Midnight Verity artifact`).

Reviewed Verity source: `lfglabs-dev/verity` / `Th0rgal/verity` `main` at
`e405b21f78c6a8376d7fa5d0959e51e7dc07bcaf` (`Model keccak256 opcode,
unaligned calldata, and SHA-256 engine in executable semantics`).

## Executive Summary

The current Midnight Verity artifact is useful as a parity artifact and as a
proof anchor, but it is not yet an extremely faithful, near line-by-line port of
`morpho-midnight/src/Midnight.sol`.

The strongest evidence today is behavioral: the repository contains
`artifacts/midnight/Midnight.bin.raw`, a harness path for
`MORPHO_MIDNIGHT_PARITY_MODE=verity`, and a manifest that records local
`373 passed, 0 failed, 0 skipped` parity evidence. There are no detected
`sorry`, `admit`, or direct Lean `axiom` declarations in
`morpho-midnight-verity`.

The weak point is structural fidelity. The Verity source still relies on:

- a separate focused proof model, `MidnightRCF`;
- a full `Midnight` contract whose source layout and some source-level bodies
  differ materially from Solidity;
- source-level ECM assumptions for CREATE2/SSTORE2 and callback ABI mechanics
  whose protocol-specific memory/layout obligations are not fully discharged;
- a Yul drift gate with `75` functions only in Solidity and `240` functions
  only in Verity;
- proofs attached to named projections rather than fully discharged from the
  generated full-contract body/Yul/bytecode.

The conclusion is precise:

- yes, there is an executable Verity-compiled Midnight artifact;
- yes, the focused RCF and `totalUnits` proof package is meaningful and
  currently complete at the Lean level;
- no, the current Verity source is not yet close enough to Solidity to call it a
  near line-by-line port;
- no, the current Yul gate is not strict Yul equivalence;
- no, the two requested proofs are not yet fully discharged from the complete
  executable `IMidnight` implementation.

## Current Artifact Status

The commit adds:

- `artifacts/midnight/Midnight.yul`
- `artifacts/midnight/Midnight.bin.raw`
- `artifacts/midnight/Midnight.abi.json`
- `scripts/prepare_midnight_artifact.sh`
- `config/midnight-yul-identity.json`
- `config/midnight-yul-identity-unsupported.json`
- `MORPHO_MIDNIGHT_MAPPING.md`

`scripts/prepare_midnight_artifact.sh` compiles `Midnight/Contract.lean`,
then runs:

1. `uniquify_yul_shadows.py`
2. `solc --strict-assembly --bin`

That means the committed `Midnight.bin.raw` is built from direct Verity output
plus the generic shadow-name canonicalization pass; there is no longer a
Midnight-specific Yul behavior patch in the artifact path.

## What Is Missing For Near Line-By-Line Faithfulness

### 1. Source-Level `toId` And `toMarket` Are Not Faithful

Solidity:

- `toId(Market)` computes `IdLib.toId(market, INITIAL_CHAIN_ID, address(this))`.
- `IdLib.toId` hashes `0xff`, the Midnight address, the chain id, and the
  `keccak256` of `SSTORE2_PREFIX ++ abi.encode(market)`.
- `touchMarket` stores the encoded market in contract code via CREATE2/SSTORE2.
- `toMarket(bytes32)` reconstructs the `Market` from the code-backed payload.

Current Verity source:

- `toId` returns `market.maturity`.
- `touchMarket` and `toMarket` now use source-level CREATE2/SSTORE2 ECMs, but
  the exact `SSTORE2_PREFIX ++ abi.encode(market)` initcode layout and return
  decoding are still ECM trust-surface obligations.
- `toMarket` still checks `marketState[id].tickSpacing > 0` and returns `Unit`;
  it does not reconstruct the full `Market` value in source.

This is the single clearest sign that the source is not yet faithful. A reviewer
reading `Midnight/Contract.lean` now sees the low-level CREATE2/SSTORE2
boundaries, but does not yet see the full `IdLib` preimage construction or
dynamic market return decoder as ordinary Verity source.

Needed:

- first-class Verity support for dynamic `abi.encode(market)`;
- fully typed dynamic `abi.encode(market)` support for the SSTORE2 preimage;
- a source-level SSTORE2 helper library whose generated body is pinned by Lean;
- CREATE2 address derivation and code-backed market decoding proofs.

### 2. Storage Layout Does Not Match Solidity

Solidity storage order in `Midnight.sol`:

- `position` at slot 0;
- `marketState` at slot 1;
- `consumed` at slot 2;
- `isAuthorized` at slot 3;
- `defaultSettlementFeeCbp` at slot 4;
- `defaultContinuousFee` at slot 5;
- `claimableSettlementFee` at slot 6;
- role addresses after that;
- `INITIAL_CHAIN_ID` is immutable, not ordinary storage.

Current Verity storage:

- `initialChainIdSlot` is stored at slot 0;
- roles are stored at slots 1-4;
- `consumedSlot` starts at slot 5;
- `marketStateSlot` is slot 10;
- `positionSlot` is slot 11;
- collateral values are split into `collateral0Slot` through
  `collateral15Slot`, slots 12-27.

That is functionally usable for a standalone replacement, but it is not the same
storage effect as Solidity. It also prevents a direct storage-layout audit
against `Midnight.sol`.

Needed:

- Verity storage declarations that can mirror Solidity layout exactly;
- immutable handling that compiles as constructor-immediate/bytecode data, not a
  normal storage slot unless Solidity does the same;
- mapping-to-struct layouts with nested fixed arrays inside the struct;
- exact `Position.collateral[128]` storage indexing inside
  `position[id][user]`, not 16 separate top-level mappings.

### 3. Collateral Storage Is Capped To 16 Explicit Slots

Solidity supports:

- up to 128 market collaterals;
- a borrower can activate up to 16 at a time;
- `position[id][user].collateral[index]` is a `uint128[128]` fixed array inside
  `Position`.

Current Verity source implements only `collateral0Slot` through
`collateral15Slot`. That tracks the borrower active-collateral cap, but it does
not preserve the actual `uint128[128]` fixed-array storage layout or allow a
clean source-level port of all index behavior.

Needed:

- fixed-size storage arrays inside mapping struct values;
- generated packed `uint128` read/write paths for
  `position[id][user].collateral[index]`;
- proof lemmas for array slot calculation and packed subword preservation.

### 4. Multi-Collateral Health Is Simplified In Source

Solidity `isHealthy` loops over the borrower collateral bitmap, uses `msb`, reads
each active collateral, reads each oracle price, and accumulates max debt.

Current Verity `isHealthy` reads only collateral index `0`:

```lean
let collateralValue <- collateralAmount id borrower ZERO
let lltv <- collateralLltvAt market.collateralParams ZERO
let maxDebt := mulDivDown collateralValue lltv WAD
return debt <= maxDebt
```

That is not equivalent to the Solidity loop. It may be sufficient for focused
proof projections or tests that do not expose the gap, but it is not a
line-by-line translation.

Needed:

- source-level bitmap loop matching Solidity:
  `while (_collateralBitmap != 0) { i = msb(_collateralBitmap); ...; clearBit(i); }`;
- CLZ/MSB support either as a proved primitive or a documented intrinsic with a
  local proof;
- array-of-struct access into `market.collateralParams[i]`;
- oracle call semantics for every active collateral.

### 5. `take` Uses Manual Calldata Introspection

The Verity `take` body contains manual offsets:

```lean
let offerBase := add (calldataload 4) 4
let marketBase := add offerBase (calldataload offerBase)
let loanToken := wordToAddress (calldataload marketBase)
let maturity := calldataload (add marketBase 64)
```

The helper `calldataload` defined at the top of the file is source-semantically
`def calldataload (offset : Uint256) : Uint256 := offset`, which is not the EVM
operation. This is another sign that source semantics, compiler semantics, and
artifact behavior are not aligned enough for a line-by-line review.

Needed:

- direct, typed projections for nested dynamic calldata structs:
  `offer.market.loanToken`, `offer.market.collateralParams.length`,
  `offer.callbackData`, etc.;
- no handwritten calldata offset arithmetic in ordinary contract code;
- source semantics for any remaining low-level calldata operation that matches
  generated Yul semantics.

### 6. Callback And External Call Behavior Is Mostly Yul-Patched

Solidity has callback interfaces for:

- `onBuy`
- `onSell`
- `onRepay`
- `onLiquidate`
- `onFlashLoan`

The patch script injects helper functions for those calls and their return-value
checks, including revert bubbling and callback-success selectors. The Verity
source has higher-level calls such as `safeTransfer` and `safeTransferFrom`,
plus `ecmCall` wrappers for oracles/gates, but the exact dynamic ABI call frames
for Midnight callbacks are not expressed as ordinary Verity source.

Needed:

- generated external-call modules from Solidity-like interface signatures;
- dynamic ABI encoding for structs and bytes arguments;
- revert bubbling as first-class source syntax or typed low-level primitive;
- return-data length checks and fixed selector comparisons expressed in source;
- call-plan proof objects tied to each generated external call site.

### 7. Events Are Not Yet Source-Faithful

Solidity emits many events through `EventsLib`, including dynamic or multi-field
events such as `Take`, `Liquidate`, `UpdatePosition`, collateral events, fee
events, and constructor/setter events.

The current Verity source does not expose a complete event surface in ordinary
contract source. If tests do not assert every event payload, this can remain
hidden behind parity success.

Needed:

- event definitions for every `EventsLib` event;
- source-level `emit` calls at the same points as Solidity;
- ABI encoding for dynamic or tuple event data;
- proof/gate that the ABI event surface and emitted topics/data match the
  Solidity event surface.

### 8. `multicall` / `delegatecall` Semantics Are Not Source-Faithful

Solidity `multicall` loops over `bytes[] calldata calls` and performs
`address(this).delegatecall(calls[i])`, bubbling returndata on failure.

Verity latest has first-class `delegatecall` as a low-level expression and
`revertReturndata`, but the current Midnight source does not contain a faithful
line-by-line `multicall` implementation. A source-faithful port should express
the delegatecall loop in Verity itself, not rely on harness-level coverage.

Needed:

- dynamic `bytes[] calldata` iteration;
- `delegatecall` statement sugar with returndata handling;
- precise memory layout for forwarding calldata bytes;
- source semantics for self-delegatecall and storage persistence.

### 9. Solidity Library Shapes Are Not Preserved

Midnight relies on:

- `UtilsLib`: `mulDivDown`, `mulDivUp`, `toUint128`, bitmap bit operations,
  `msb`, transient storage helpers, WAD exponentials;
- `IdLib`: SSTORE2/CREATE2 market identity;
- `TickLib`: tick/price conversions;
- `SafeTransferLib`: optional-return ERC-20 transfers;
- `EventsLib`: all event declarations.

Current Verity inlines or reimplements many formulas, and the patch script adds
Yul helpers such as `__midnight_wexp`, `__midnight_tick_to_price`,
`__midnight_mul_div_up`, and callback helpers. That may be adequate for tests,
but it is not a reviewable line-by-line port of the library calls.

Needed:

- Verity library modules mirroring each Solidity library;
- either source-level functions with direct call sites matching Solidity or a
  documented automated inliner;
- per-library proof/identity artifacts;
- no protocol-specific Yul helpers injected after compilation.

### 10. Proofs Are Complete Locally But Not Fully Extracted

No `sorry`/`admit`/direct `axiom` was detected in `morpho-midnight-verity`.
However, the requested theorems are still connected to focused projections and
named extraction boundaries:

- RCF proof anchors include `normalModeMaxRepaidHealthyWithin3`,
  `normalModeLiquidateProjection_storageHealthyWithinOne`, and refinement
  obligations around selected collateral, oracle price, bitmap schedule, and
  max-debt loop witnesses.
- `totalUnits` proof anchors include two-position/list-level cover lemmas,
  bad-debt update formulas, lender synchronization hypotheses, and storage
  projections.

This is not the same as proving:

```text
For every execution of the full compiled IMidnight.liquidate body, the property
holds over the real generated storage, calls, memory, loops, events, and
returndata behavior.
```

Needed:

- generated-body extraction for full `Midnight.liquidate`;
- generated-body extraction for `_updatePosition` / `updatePositionView`;
- loop refinement from the real bitmap/MSB loop;
- storage-layout refinement from the exact Solidity-compatible layout;
- oracle/callback/token-call assumptions localized at real call sites;
- theorem statements whose hypotheses quantify over the executable full
  contract state, not hand-provided projections.

## Yul Equivalence Status

There is no strict Yul equivalence today.

`config/midnight-yul-identity.json` sets:

```json
"gateMode": "unsupported-manifest"
```

`config/midnight-yul-identity-unsupported.json` currently records:

- `allowedHashMismatchKeys`: `0`
- `allowedOnlyInSolidityKeys`: `75`
- `allowedOnlyInVerityKeys`: `240`

Interpretation:

- good: there are no explicitly allowed hash mismatches for functions present on
  both sides;
- not enough: the Solidity and Verity Yul function universes are very different;
- therefore: the gate is a fail-closed drift manifest, not an equivalence proof
  and not exact identity.

To claim stronger Yul equivalence, the next gate must move from
`unsupported-manifest` to one of:

- `exact`: same function set and same body hashes after stable canonicalization;
- `proved-rewrite`: every rewrite stage has a Lean theorem or a small checked
  semantic proof;
- `bisimulation`: generated Verity Yul and Solidity Yul differ structurally but
  are related by a machine-checked equivalence relation.

## Verity Base: What Already Exists

The Verity base analyzed for this report, `e405b21f`, already contains relevant
pieces:

- ABI value types for `uint8`, `uint16`, `address`, `bytes32`, `bytes`,
  dynamic arrays, fixed arrays, tuples, named structs, ADTs, and newtypes.
- Storage support for scalar fields, mappings, mapping chains, mapping structs,
  nested mapping structs, dynamic storage arrays, packed bits, and word-offset
  writes.
- Internal helper calls through `FunctionSpec.isInternal`,
  `Expr.internalCall`, `Stmt.internalCall`, `Stmt.internalCallAssign`, generated
  `internal_<name>` helper specs, qualified helper inclusion, multi-return
  internal helpers, and higher-order helper monomorphization.
- Low-level expressions for `call`, `staticcall`, `delegatecall`,
  `calldataload`, `calldatasize`, `returndataSize`, `extcodesize`, `tload`,
  `keccak256`, memory loads, and fork-gated intrinsics.
- Statements for `mstore`, `tstore`, `calldatacopy`, `returndatacopy`,
  `revertReturndata`, `rawLog`, `forEach`, `externalCallBind`,
  `tryExternalCallBind`, and typed `unsafeYul`.
- A typed `UnsafeYulFragment` model carrying mechanics, local obligations,
  control-flow summaries, and scope effects.
- Consumer-owned intrinsics for new opcodes such as CLZ, with fork gates and
  explicit obligations.

So the missing work is not a blank slate. Verity is close enough that a faithful
Midnight port should be feasible, but several Solidity-grade features need to be
promoted from ad hoc or raw-Yul boundaries into typed, auditable, proofable
language/compiler features.

## Verity Additions Needed For A Faithful Midnight Port

### Immediate Upstream Work

Opened Verity PR: <https://github.com/lfglabs-dev/verity/pull/1945>

That PR intentionally does not add a new internal-call mechanism: internal
helper calls already existed in Verity and are covered by the existing
CompilationModel and macro tests. The PR instead focuses on Midnight-specific
source gaps that were not already covered cleanly: fixed arrays inside mapping
structs, CREATE2/SSTORE2 code-as-data surfaces, callback ABI usage, and trust
surface reporting for the new low-level mechanics.

### A. Solidity ABI Codec As A First-Class Library

Required capabilities:

- `abi.encode` for nested structs with dynamic members;
- `abi.encodePacked`;
- calldata and memory projections for nested dynamic structs;
- dynamic return encoding for `toMarket(bytes32) returns (Market memory)`;
- bytes and bytes-array forwarding for callbacks and multicall;
- event data encoding for tuple/dynamic payloads.

Why Midnight needs it:

- `Market` contains `CollateralParams[]`;
- `Offer` contains `Market`, `bytes callbackData`, and many scalar fields;
- callbacks forward `Market memory` and `bytes memory`;
- `IdLib.toId` depends on exact `abi.encode(market)`;
- `toMarket` returns a dynamic `Market` object.

Implementation sketch:

1. Add an ABI layout IR for static head words and dynamic tail segments.
2. Add verified builders for calldata-to-memory and memory-to-return encoding.
3. Generate field projection code from `struct` declarations instead of using
   manual `calldataload` offsets.
4. Provide executable Lean semantics for each ABI builder.

### B. Exact Solidity Storage Layout Support

Required capabilities:

- immutable fields compiled as Solidity-style immutables;
- mapping values containing structs with fixed-size arrays;
- fixed-size arrays inside storage structs;
- packed fixed-array elements, especially `uint128[128]`;
- layout reports comparing Verity slots against `solc --storage-layout`.

Why Midnight needs it:

- `Position` contains six packed scalar fields plus `uint128[128] collateral`;
- the current Verity port splits collateral into 16 separate mappings;
- `INITIAL_CHAIN_ID` is immutable in Solidity but storage in Verity.

Implementation sketch:

1. Extend `StorageType` with `fixedArray` members inside `mappingStruct`.
2. Add member paths such as `position[id][user].collateral[index]`.
3. Add compiler checks that generated slots match a solc storage-layout JSON.
4. Add preservation lemmas for packed fixed-array reads/writes.

### C. Code-As-Data And CREATE2/SSTORE2

Required capabilities:

- source-level `create2`;
- source-level `extcodecopy`;
- source-level `codecopy` or deploy-code builders;
- a typed `runtimeCode(bytes)` / SSTORE2 helper;
- CREATE2 address derivation matching EVM semantics;
- extcode-size/read failure behavior.

Why Midnight needs it:

- `IdLib.storeInCode` writes encoded markets as code;
- `IdLib.toId` derives ids from the SSTORE2 init code hash;
- `toMarket` reconstructs the market from deployed code.

Implementation sketch:

1. Add low-level primitives for `create2`, `extcodecopy`, and `mstore8`.
2. Extend the standard SSTORE2 module from source-level mechanics to typed
   market initcode/decode helpers.
3. Keep Midnight artifact generation free of protocol-specific Yul patches.
4. Prove the generated helper matches the SSTORE2 prefix and address formula.

### D. Memory-Safe Assembly Surface

Required capabilities:

- `mstore8`;
- `mcopy`;
- arbitrary `return(offset, size)`;
- memory allocation/free-pointer helpers;
- raw revert with memory slices, preferably typed;
- exact revert bubbling and returndata forwarding;
- memory frame specs for assembly blocks.

Why Midnight needs it:

- Solidity uses `assembly ("memory-safe")` in `multicall`, `IdLib`,
  `SafeTransferLib`, and `UtilsLib`;
- callback and SSTORE2 helpers need byte-level memory layout;
- exact event and return-data behavior depends on memory.

Implementation sketch:

1. Promote stable memory helpers into typed statements.
2. Keep only rare cases as `UnsafeYulFragment`.
3. Require every unsafe fragment to name memory reads/writes and termination.
4. Add proof rules for common memory-copy and byte-store idioms.

### E. Interface-Driven External Calls And Callbacks

Required capabilities:

- generate external-call modules from Solidity-like interface signatures;
- support dynamic arguments and dynamic return values;
- optional-return ERC-20 calls;
- callback-success value checks;
- revert bubbling on failed low-level calls;
- event/call ordering checks.

Why Midnight needs it:

- token transfers use optional-return ERC-20 behavior;
- oracles and gates are external calls;
- `take`, `repay`, `liquidate`, and `flashLoan` callbacks have dynamic ABI
  frames and exact success selectors.

Implementation sketch:

1. Add `interface` declarations to Verity or a generator from Solidity ABI.
2. Generate `externalCallBind`/`tryExternalCallBind` wrappers with ABI codecs.
3. Attach local obligations for external callee behavior.
4. Emit a machine-readable call-surface manifest.

### F. Solidity-Like Loop Forms

Required capabilities:

- `while` loops with decreasing bitmap variants;
- `for` loops with arbitrary start/condition/post;
- loop invariants or generated local obligations;
- CLZ/MSB primitives with fork-aware semantics;
- proof support for loops beyond current limited `forEach` fragments.

Why Midnight needs it:

- liquidation and health use `while (_collateralBitmap != 0)`;
- `UtilsLib.msb` uses CLZ;
- `multicall` and fee/tick functions use loop-like array traversal.

Implementation sketch:

1. Add `while` syntax to the macro surface.
2. Lower to a CompilationModel loop with explicit invariant metadata.
3. Provide common bitmap-loop proof templates.
4. Replace all fixed `forEach MAX_COLLATERALS` scans where Solidity uses
   `while bitmap != 0`.

### G. Source/Yul Alignment Checks

Required capabilities:

- compare Verity source function names to Solidity function names;
- detect protocol-specific post-generation rewrites;
- fail when an artifact depends on non-source Yul helpers;
- produce source-to-Yul proof obligations for every helper.

Why Midnight needs it:

- reviewers need to know whether a behavior is in Verity source or in a Yul
  post-generation pass.

Implementation sketch:

1. Generate a helper-origin table for every Yul function.
2. Mark each helper as `source`, `compiler-runtime`, `linked-library`, or
   `post-generation`.
3. Fail presentation-mode CI if any protocol behavior is post-generated.
4. Allow post-generation passes only for verified canonicalization.

## Migration Plan To Near Line-By-Line

### Phase 1: Remove Yul Patch Dependence

Goal: `scripts/prepare_midnight_artifact.sh` no longer calls a
Midnight-specific Yul behavior patch.

Tasks:

- keep SSTORE2/CREATE2 on the Verity source/ECM trust surface;
- keep callback ABI builders in Verity source/ECMs;
- implement dynamic `Market` return encoding;
- keep the current parity artifact as a regression target.

Exit criteria:

- `Midnight.yul` generated directly by Verity compiles without protocol-specific
  patching;
- parity still passes;
- every helper appears in source or compiler runtime.

### Phase 2: Match Solidity Storage Layout

Goal: storage effects are byte-for-byte aligned with Solidity.

Tasks:

- move `position`, `marketState`, and all mappings to Solidity slots;
- model `INITIAL_CHAIN_ID` as immutable;
- replace collateral split mappings with `Position.collateral[128]`;
- add a storage-layout diff against solc.

Exit criteria:

- Verity storage layout report matches Solidity;
- all storage getters and mutation paths still pass parity;
- proofs are updated to use exact storage paths.

### Phase 3: Replace Simplified Bodies

Goal: source bodies match Solidity control flow and formulas closely enough for
line-by-line review.

Tasks:

- replace `toId`, `toMarket`, `isHealthy`, `take`, `liquidate`, and
  `multicall` with Solidity-shaped bodies;
- use library calls that mirror `UtilsLib`, `TickLib`, `IdLib`, and
  `SafeTransferLib`;
- emit all events at Solidity-equivalent program points.

Exit criteria:

- `MORPHO_MIDNIGHT_MAPPING.md` can mark each function as
  `source-faithful`, not merely `test-parity`;
- source reviewers can compare Verity and Solidity function-by-function without
  understanding a protocol-specific post-generation pass.

### Phase 4: Tighten Yul Gate

Goal: move beyond `unsupported-manifest`.

Tasks:

- canonicalize naming and harmless helper differences;
- prove or eliminate rewrite passes;
- reduce `allowedOnlyInSolidityKeys` and `allowedOnlyInVerityKeys` toward zero.

Exit criteria:

- either `gateMode = exact`, or each remaining difference has a proved rewrite
  or bisimulation record.

### Phase 5: Fully Discharge The Requested Proofs

Goal: the RCF and `totalUnits` theorems are stated over the executable full
contract.

Tasks:

- extract the full generated `liquidate` body into a proof-facing transition;
- extract full `_updatePosition` / `updatePositionView`;
- prove the bitmap/MSB loop refinement;
- localize oracle/token/callback assumptions to exact call sites;
- remove projection-only theorem endpoints or make them intermediate lemmas.

Exit criteria:

- theorem statements mention the full `Midnight.liquidate` / update functions;
- no trusted projection is required for core arithmetic/storage behavior;
- remaining assumptions are only external-world assumptions: oracle values,
  token behavior, callback behavior, and chain fork semantics.

## Definition Of "Extremely Faithful"

For Midnight, "extremely faithful" should mean all of the following:

1. Every `IMidnight` function has a Verity function with the same ABI.
2. Storage layout matches Solidity, including immutables and nested arrays.
3. Public/view getters are not hand-simulated; they read the same storage words.
4. `toId` and `toMarket` implement the real `IdLib`/SSTORE2 algorithm in source.
5. Control flow for `take`, `liquidate`, `isHealthy`, `updatePosition`,
   `touchMarket`, and `multicall` follows Solidity block structure.
6. Events and custom errors match Solidity selectors and payloads.
7. External calls and callbacks have the same calldata, returndata, revert, and
   ordering behavior.
8. No protocol behavior is injected by a Midnight-specific Yul patch.
9. Yul drift is exact or justified by proved rewrites.
10. RCF and `totalUnits` proofs are discharged from the full executable model.

The current repo satisfies item 1 at the artifact/harness level and now
satisfies item 8 for the artifact build path. It partially satisfies items 6-7
through source-level ECMs and remaining assumptions, and does not yet satisfy
items 2, 4, 9, or 10.

## Recommended Next Work Items

1. Add a `source-faithfulness` column to `MORPHO_MIDNIGHT_MAPPING.md`, separate
   from `test-parity`.
2. Keep CI failing if `prepare_midnight_artifact.sh` uses a protocol-specific
   Yul patch in presentation mode.
3. Implement exact Solidity storage layout for `Position` and `MarketState`.
4. Discharge the remaining SSTORE2/CREATE2 market initcode and decode
   assumptions.
5. Replace `isHealthy` with the real bitmap/MSB/oracle loop.
6. Replace `take` calldata offset introspection with typed nested struct
   projections.
7. Generate callback modules from `ICallbacks.sol`.
8. Add event-surface parity tests that assert topics and data for all
   `EventsLib` events.
9. Shrink the Midnight Yul unsupported manifest after each source-fidelity
   improvement.
10. Move RCF and `totalUnits` theorem endpoints from focused projections to the
    full generated `Midnight` transition.
