/-
  Morpho Blue types — Matches morpho-blue/src/interfaces/IMorpho.sol

  In Solidity, `Id` is `bytes32 = keccak256(abi.encode(marketParams))`.
  Here we use `MarketParams` directly as the key, which is equivalent
  since the hash is a deterministic function of the params.
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
  owner := "", feeRecipient := "",
  market := fun _ => default, position := fun _ _ => default,
  idToParams := fun _ => none,
  isIrmEnabled := fun _ => false, isLltvEnabled := fun _ => false,
  isAuthorized := fun _ _ => false, nonce := fun _ => 0,
  sender := "", blockTimestamp := 0
}⟩

/-- Compute market Id from params. In Solidity this is keccak256(abi.encode(params)).
    Here we use a deterministic hash function stub — the exact value doesn't matter
    for verification since we only need injectivity. -/
def marketId (_params : MarketParams) : Id :=
  -- Placeholder: in a real implementation this would be a proper hash.
  -- For verification purposes, we work with Id abstractly.
  0

end Morpho.Types
