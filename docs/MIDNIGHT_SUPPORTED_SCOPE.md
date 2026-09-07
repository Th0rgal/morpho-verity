# Midnight: bounded source-import scope

> Historical scope-triage snapshot. Subsequent user authorization resumed bytecode repair and translation review. See `MIDNIGHT_TRANSLATION_REVIEW.md` for the current result: bytecode compilation and the 373-test suite pass, with explicit memory-contract assumptions and a deployment-size blocker. The classifications below remain triage, not proof certification.

## Decision

The experimental full emitter is not an approved complete implementation replacement. No memory-backed temporary-variable compiler pass is authorized. Full bytecode compilation and the 373-test full-suite gate remain incomplete, not waived or passed.

The Solidity source remains the semantic authority. Do not preserve handwritten errors. Do not exclude protocol functions solely because emitted Yul exceeds backend stack limits.

## Executed inventory

Run `node scripts/report_midnight_support.mjs --output docs/MIDNIGHT_SUPPORT_INVENTORY.json` from the repository root. The report derives declaration identities, spans and dependency edges from the pinned typed AST. Two executions produced byte-identical reports.

The inventory covers 68 reachable source function bodies, including the constructor. It is NOT the entire ABI inventory: automatically generated state getters, generated internal clones, and synthetic scaffolding require separate review using the full manifest.

- 25 bodies are candidates for focused semantic review.
- 43 bodies directly or transitively require boundary review.
- ZERO functions are presently approved by this inventory as proved source-equivalent.

Candidate means absence of the particular source boundary flags below, not absence of compiler assumptions, correctness bugs, or difficult arithmetic. For example, constructor immutable-as-storage scaffolding and checked-arithmetic panic lowering still need review. Math helpers such as wExp are not automatically easy or proved.

## Explicit review boundaries

| Feature | Typed source origin | Required review | Current scope |
|---|---|---|---|
| Scalar arithmetic, storage reads, bitmap helpers | Operators, referenced declarations and solc layout | Widths, checkedness, index bounds, source/model execution relation | Focused-review candidates only |
| Composite parameters and returns | AST parameter/return type descriptions | ABI-image vs Solidity memory, lazy decoder, allocation and non-aliasing | Excluded from unconditional claims pending review |
| Inline assembly | InlineAssembly AST and externalReferences | Each actual opcode and assumed semantics | Excluded pending review |
| External calls/code access | Typed call targets, low-level call nodes, code members | Call ordering, returndata, callbacks and environment assumptions | Excluded pending review |
| Specialized while loops | WhileStatement plus typed helper dependencies | Source-derived decreasing measure and iteration bound | Excluded pending review |
| Builtins/low-level operations | Call graph classification | Individually distinguish ordinary guards from hash/ABI/call boundaries | Conservatively excluded pending refinement |

Exclusions propagate through source function dependencies. A function calling an excluded helper cannot be presented as independently supported. The builtin classifier is intentionally broad: it may flag ordinary require/revert calls. Refine by typed AST semantics, not name-based function exceptions.

## Acceptance for a future supported slice

1. Select exact declaration IDs and include the full dependency closure and relevant storage/constructor invariants.
2. Review every emitted primitive, synthetic helper and trust assumption, not only source AST categories.
3. Reject unsupported constructs/dependencies with source diagnostics; no stubs, assumed business-function bodies, or fallback to handwritten code.
4. Retain deterministic emission and declaration provenance; use source mutations as negative tests.
5. Prove only explicitly scoped properties from the generated execution model. State any environmental/primitive assumptions and audit theorem axioms. Do not imply an unproved source-to-model bridge.
6. Keep generation, Lean build, Yul compilation, bytecode and runtime evidence separate. Existing tests and proofs are not weakened.

No compiler wiring was reverted as part of this inventory. The existing experimental full-artifact wiring therefore remains unsuitable for release until explicitly restored or replaced by a reviewed, separately named slice. This report is a review boundary, not an enforced restricted generator or a proof bundle.
