# Morpho Fidelity Actions After Verity #1939

Verity PR `lfglabs-dev/verity#1939` landed in upstream Verity at merge commit
`00c18e3a694201cc0dfd8d8f52abaa0bf308887c`. This changes the Morpho plan:
several items that were previously Verity feature gaps are now repo-local
translation work once `morpho-verity` advances its Verity pin to `00c18e3a`.

The target is the same standard used by the Unlink Verity translation: keep the
Solidity control flow, ABI boundaries, revert behavior, event surface, and
external-call choreography recognizable at the source level, while leaving only
true environment assumptions explicit.

## Directly Actionable In Morpho

These changes no longer require new Verity framework features. The Morpho pin is
now `00c18e3a`; the remaining work is replacing any remaining Morpho-local
placeholders and discharging proof obligations.

| Area | Current Morpho shape | Direct replacement |
|------|----------------------|--------------------|
| Market id hashing | `keccakMarketParams` linked external plus `market_id_deterministic` axiom | Encode `MarketParams` as five ABI words and hash with `Compiler.Modules.Hashing.abiEncodeStaticWords`; keep only the global Keccak memory-slice primitive boundary. |
| EIP-712 digest | Manual `mstore`/placeholder digest choreography plus `authorization_sig_memory` | Use `Hashing.abiEncodeStaticWords` for the authorization struct hash and `Hashing.eip712Digest` for `keccak256(abi.encodePacked(hex"1901", domainSeparator, structHash))`. |
| Domain separator | `DOMAIN_SEPARATOR()` local memory/hash obligation | Compute the domain separator with static ABI hashing over domain typehash, chain id, and contract address. The runtime `chainid` and `address(this)` assumptions remain explicit. |
| ERC-20 transfers | Generic `safeTransfer` / `safeTransferFrom` boundary | Use `Compiler.Modules.ERC20.solmateSafeTransfer` and `solmateSafeTransferFrom`, because Morpho Blue imports Solmate `SafeTransferLib`. |
| Callback calls | Local obligations for callback ordering and low-level behavior | Use `Compiler.Modules.Callbacks.callbackModule`; it now allocates through the free-memory pointer, aligns `0x40`, and bubbles revert returndata. |
| IRM init and rate calls | Generic `externalCall "borrowRate"` with a named local obligation | Add a typed IRM ECM for `borrowRate(MarketParams, Market)` using the generic bubbling call helpers, with exact selector/calldata layout and one-word return decoding. |
| Oracle price calls | Generic `externalCall "oraclePrice"` | Add or use a typed oracle ECM for `IOracle.price()` with staticcall, exact one-word return decoding, and returndata bubbling. |
| Low-level revert behavior | Broad assumed boundary for returndata/revert bubbling | Use `Compiler.Modules.Calls.bubblingValueCall` / `bubblingValueCallNoOutput` where Morpho needs generic low-level calls. |
| Raw event checks | `rawLog` obligations for event payload mechanics | Prefer `emit` for fixed-shape events where Verity supports it; keep `rawLog` only for Solidity-shaped manual payloads such as `CreateMarket`, with topic count and data-size tests. |
| Solidity 0.8 arithmetic | Bare arithmetic plus selected side-condition theorems | Replace unchecked arithmetic in translated Solidity statements with Verity panic/checked helpers or explicit require-guarded helpers, then discharge remaining `h_no_overflow` hypotheses operation by operation. |

## Translation Work Needed To Match Solidity More Closely

1. Advance the Verity dependency to a revision containing `#1939`.
2. Replace `keccakMarketParams` in `Morpho/Compiler/MacroSlice.lean` and
   `Morpho/Compiler/Spec.lean` with direct static ABI hashing. The translated
   word order must be exactly:
   `loanToken`, `collateralToken`, `oracle`, `irm`, `lltv`.
3. Rewrite `DOMAIN_SEPARATOR()` and `setAuthorizationWithSig` around the new
   hashing helpers. The Solidity ordering to preserve is:
   deadline check, nonce equality check, nonce increment, digest construction,
   `ecrecover`, signatory check, authorization write, nonce/auth events.
4. Replace Morpho-local ERC-20 wrappers with Solmate-compatible ERC-20 ECMs.
   This is important because Solmate accepts empty returndata or a first
   returned word equal to true when at least 32 bytes are returned; it does not
   require OpenZeppelin-style code-existence semantics on the empty-returndata
   success path.
5. Keep callback placement line-for-line with Solidity:
   `supply` and `repay` callback before `transferFrom`,
   `supplyCollateral` callback before collateral `transferFrom`,
   `liquidate` collateral transfer, callback, then loan-token `transferFrom`,
   and `flashLoan` transfer, callback, then `transferFrom`.
6. Introduce typed ECMs for IRM and oracle calls instead of passing loosely
   packed word lists through generic `externalCall`. The ECMs should preserve
   selector, argument layout, call kind, returndata size check, and bubbling.
7. Convert fixed events to `emit` where possible, and isolate the remaining
   raw-log paths with tests that assert exact topics and payload sizes against
   Morpho Blue `EventsLib.sol`.
8. Audit every Solidity arithmetic expression in `Morpho.sol`, `MarketParamsLib`,
   `SharesMathLib`, `UtilsLib`, and `MathLib`. For each expression, choose one
   of:
   proved safe by a preceding require/branch,
   translated with a checked/panic helper,
   translated inside Solidity's explicit `unchecked` region,
   or retained as a documented assumption with a proof obligation.
9. Discharge the repo-local Link 1 semantic-equivalence proofs for the 12
   remaining operations after the above translation changes. The priority order
   should be `createMarket`, `setAuthorizationWithSig`, `accrueInterest`,
   `setFee`, `supplyCollateral`, `withdrawCollateral`, `supply`, `withdraw`,
   `borrow`, `repay`, `liquidate`, then any adapter-only leftovers.

## What Remains Assumed

The Verity merge reduces framework gaps, but it does not make external systems
part of Morpho's proof:

- Keccak and `ecrecover` remain EVM/precompile primitives. Morpho can prove it
  builds the same preimages Solidity builds; it cannot prove the cryptography
  from first principles in this repo.
- IRM and oracle return values, no-revert behavior, price/rate freshness,
  bounds, and non-reentrancy remain environment assumptions unless those
  contracts are verified too.
- ERC-20 economic behavior remains external: fee-on-transfer, rebasing,
  malicious callbacks, and token balance consistency are not Morpho state
  transitions unless token contracts are modeled.
- Gas-sensitive behavior is still outside the Morpho functional model.

## Updated Claim

After adopting Verity `#1939`, Morpho now expresses market-id hashing,
EIP-712 digest construction, ERC-20 transfers, and IRM/oracle reads through
Verity ECMs instead of linked placeholders. It is not yet correct to say the
current branch is Unlink-level faithful: non-flash callback paths, raw-log
obligations, checked-arithmetic discharge, and several assumption-backed
core-flow equivalence proofs remain.

The next faithful-translation milestone is:

> All Morpho source-level mechanics are expressed with Verity primitives or
> typed ECMs matching the Solidity ABI/control-flow shape; remaining assumptions
> are only external environment behavior and named primitive/precompile
> semantics.
