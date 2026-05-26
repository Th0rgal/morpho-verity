# Verity Upgrade Notes (`b699e300`)

This note records the current refactor-plan upgrade from
`4ebe4931d25e5a1594fcd3f43ff040ecc3c4225a` to
`b699e30060cb80b3fd26c62010ecf5b151f31ab2`.

## Upstream changes that matter for Morpho

The new pin includes the macro/frontend and proof-surface changes that the
Morpho refactor plan depends on:

- runtime context accessors such as `chainid`, `blockTimestamp`, and
  `contractAddress` are now monadic in `verity_contract` source, preventing
  accidental use of block/contract environment values as compile-time constants
- typed router ABI surface support
- expanded bridged call fragments for the native EVMYulLean path
- split native primitive operation proof support
- continued support for linked externals, struct-mapping storage, executable
  tuple params, internal calls, ERC20 helper syntax, and macro-backed
  `ecrecover`

## Morpho migration impact

The immediate source migration is small but semantically useful:

1. `Morpho/Compiler/MacroSlice.lean` now binds runtime context with
   `let ... ← ...` before hashing or passing the contract address to transfer
   helpers.
2. Pin provenance in `config/verity-pin-provenance.json` and
   `docs/VERITY_PIN.md` is updated to the same full revision consumed by Lake.
3. The remaining macro-contract cleanup is still the same work tracked in
   `refactor-plan.md`: make the macro contract the canonical Solidity-facing
   surface, remove no-op stubs where Verity now has ECM/precompile support, and
   discharge Link 1 semantic bridge obligations operation by operation.

## What still blocks a pure `verity_contract` path

This upgrade does not by itself prove full Solidity equivalence.

The remaining blockers are local and explicit:

- several Morpho operations still rely on assumed Link 1 semantic equivalence
  obligations in `config/semantic-bridge-obligations.json`
- ERC20, IRM, oracle, callback, EIP-712 hashing, and low-level revert-data
  behavior remain named trust boundaries until they are moved onto concrete
  Verity modules and discharged
- event/log and linear-memory mechanics are still outside the fully proved
  fragment for some Morpho-critical paths

The practical target remains incremental: keep the upgraded Verity pin, make
the macro source closer to `Morpho.sol`, and only remove repo-local wrappers
after the corresponding semantic bridge obligations are discharged.
