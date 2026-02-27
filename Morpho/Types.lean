/-
  Morpho Blue types — Matches morpho-blue/src/interfaces/IMorpho.sol

  In Solidity, `Id` is `bytes32 = keccak256(abi.encode(marketParams))`.
  Here `Id` stays abstract as `Nat`, produced by a deterministic hash
  over `MarketParams`.
-/
import Verity.Core

namespace Morpho.Types

open Verity

abbrev Id := Nat  -- Abstract market identifier (keccak256 hash in Solidity)

/-- The 5-tuple that uniquely identifies a lending market. -/
structure MarketParams where
  loanToken       : Address
  collateralToken : Address
  oracle          : Address
  irm             : Address   -- Interest Rate Model contract
  lltv            : Uint256   -- Liquidation Loan-To-Value (WAD-scaled)
  deriving DecidableEq, Repr

/-- Per-user position within a market. -/
structure Position where
  supplyShares : Uint256 := 0
  borrowShares : Uint256 := 0
  collateral   : Uint256 := 0
  deriving Repr

instance : Inhabited Position := ⟨{ supplyShares := 0, borrowShares := 0, collateral := 0 }⟩

/-- Aggregate market state. -/
structure Market where
  totalSupplyAssets : Uint256 := 0
  totalSupplyShares : Uint256 := 0
  totalBorrowAssets : Uint256 := 0
  totalBorrowShares : Uint256 := 0
  lastUpdate        : Uint256 := 0  -- block.timestamp of last interest accrual
  fee               : Uint256 := 0  -- WAD-scaled, max 25%
  deriving Repr

instance : Inhabited Market := ⟨{}⟩

/-- Full Morpho Blue protocol state. -/
structure MorphoState where
  owner        : Address
  feeRecipient : Address

  market       : Id → Market
  position     : Id → Address → Position
  idToParams   : Id → Option MarketParams

  isIrmEnabled  : Address → Bool
  isLltvEnabled : Uint256 → Bool
  isAuthorized  : Address → Address → Bool   -- authorizer → authorized → bool
  nonce         : Address → Uint256

  sender         : Address       -- msg.sender
  blockTimestamp : Uint256       -- block.timestamp

instance : Inhabited MorphoState := ⟨{
  owner := 0, feeRecipient := 0,
  market := fun _ => default, position := fun _ _ => default,
  idToParams := fun _ => none,
  isIrmEnabled := fun _ => false, isLltvEnabled := fun _ => false,
  isAuthorized := fun _ _ => false, nonce := fun _ => 0,
  sender := 0, blockTimestamp := 0
}⟩

/-- Compute market Id from params. Mirrors Solidity's `id()` at the model level. -/
private def cantorPair (a b : Nat) : Nat :=
  let s := a + b
  (s * (s + 1)) / 2 + b

def marketId (params : MarketParams) : Id :=
  cantorPair
    (cantorPair
      (cantorPair
        (cantorPair params.loanToken.toNat params.collateralToken.toNat)
        params.oracle.toNat)
      params.irm.toNat)
    params.lltv.val

/-- EIP-712 authorization struct. Matches `Authorization` (IMorpho.sol:35).
    Used by `setAuthorizationWithSig` for gasless delegation via signatures. -/
structure Authorization where
  authorizer   : Address
  authorized   : Address
  isAuthorized : Bool
  nonce        : Uint256
  deadline     : Uint256
  deriving DecidableEq, Repr

end Morpho.Types
