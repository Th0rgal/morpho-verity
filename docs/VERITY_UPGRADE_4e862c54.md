# Verity Upgrade Notes (`4e862c54`)

This note records what changed when `morpho-verity` moved its `verity` pin from
`ad03fc64` to `4e862c54`, what new upstream surface is now available, and what
that means for the remaining Morpho migration plan.

## Upstream changes that matter for Morpho

From the upstream `verity` history between `ad03fc64` and `4e862c54`, the most
relevant additions are:

- typed address mapping helpers and preservation of explicit address-storage ops
- struct-mapping storage declarations and better struct-mapping diagnostics
- ABI-level `string` support
- first-class low-level call / returndata helpers in `CompilationModel`
- machine-readable trust reporting for low-level call sites

These are visible in the upstream merged changes around PRs `#1385`, `#1388`,
`#1398`, `#1399`, `#1400`, and `#1406`.

## What this can simplify in morpho-verity

The upgrade does not make the repo macro-only, but it does narrow some of the
repo-local surface:

1. Address-keyed storage and double-address mappings are now a firmer upstream
   path. That helps keep admin operations like `setFeeRecipient`,
   `enableIrm`, `enableLltv`, and `setAuthorization` on the macro/typed-IR
   path with less bespoke adaptation risk.

2. Struct-mapping support is the clearest opportunity for the next Morpho
   cleanup. The hand-authored `CompilationModel` in `Morpho/Compiler/Spec.lean`
   already models `position`, `market`, and `idToMarketParams` with structured
   members. That upstream surface is now close enough that `MacroSlice` can use
   direct struct storage accessors instead of word-offset shims for those views,
   and `createMarket` can write the same layout directly.

3. Low-level call and returndata helpers reduce the amount of raw Yul-style
   reasoning needed for ERC20 compatibility and callback-heavy flows. That is
   directly relevant to `supply`, `withdraw`, `repay`, `liquidate`, and
   `flashLoan`, which currently stay outside the fully discharged macro path.

## What still blocks a pure `verity_contract` path

`morpho-verity` still cannot drop its custom Lean modeling and rely on the EDSL
macro alone for the whole contract.

The current blockers are structural, not just pin drift:

1. The repo still depends on repo-local state encoders and wrappers in
   `Morpho/Compiler/AdminAdapters.lean` to relate `MorphoState` to the macro
   contract state used by `MorphoViewSlice`.

2. The semantic-bridge proofs in
   `Morpho/Proofs/SemanticBridgeDischarge.lean` are only discharged for 6
   operations. The rest still rely on the custom model in `Morpho.Morpho` and
   `Morpho.Specs.ContractSemantics`.

3. Upstream proof coverage is still intentionally narrower than compiler
   coverage for some Morpho-critical constructs. In upstream
   `docs/INTERPRETER_FEATURE_MATRIX.md`, `Stmt.rawLog`, `Stmt.externalCallBind`,
   `Stmt.returndataCopy`, `Stmt.revertReturndata`, and linear-memory features
   remain outside the current proof model even though the compiler supports
   them.

4. Morpho still has non-trivial internal-call and callback-heavy flows in the
   legacy `CompilationModel` spec. Those are exactly the areas where the repo's
   custom Lean model carries semantic intent today.

## Practical conclusion

The upgrade is worth taking now, and it is safe: the repo builds cleanly on the
new pin.

But the answer to "can we use the EDSL macro only and not custom Lean
modelization now?" is still **no**.

The best near-term simplification target after this upgrade is not deleting the
custom model wholesale. It is shrinking the gap incrementally:

1. migrate getter/storage shims (`position`, `market`, `idToMarketParams`) onto
   the newer struct-mapping surface
2. retest `createMarket` and `accrueInterest` against the newer low-level call
   / returndata support
3. only remove repo-local wrappers once Links 1-3 are discharged for the
   corresponding operations
