/-
  Morpho Blue — Core lending protocol.
  Faithful translation of morpho-blue/src/Morpho.sol (Solidity 0.8.19).

  This is a singleton contract: one instance manages all lending markets.
  Each market is identified by (loanToken, collateralToken, oracle, irm, lltv).

  Key differences from Solidity:
  - External calls (oracle.price, irm.borrowRate, ERC20 transfers) are modeled
    as parameters rather than actual calls, since we verify logic not I/O.
  - State is passed explicitly rather than stored in contract storage slots.
  - We use Option for operations that can revert in Solidity.
-/
import Morpho.Types
import Morpho.Libraries.MathLib
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.UtilsLib
import Morpho.Libraries.ConstantsLib

namespace Morpho

open Verity
open Morpho.Types
open Morpho.Libraries

private def u256 (n : Nat) : Uint256 := Verity.Core.Uint256.ofNat n

/-! ## Internal helpers -/

/-- Check if sender is authorized to act on behalf of `onBehalf`. -/
def isSenderAuthorized (s : MorphoState) (onBehalf : Address) : Bool :=
  s.sender == onBehalf || s.isAuthorized onBehalf s.sender

/-- Check if a position is healthy given the collateral price.
    Matches `_isHealthy` in Morpho.sol. -/
def isHealthy (s : MorphoState) (id : Id) (borrower : Address) (collateralPrice : Uint256)
    (lltv : Uint256) : Bool :=
  let m := s.market id
  let pos := s.position id borrower
  let borrowed := SharesMathLib.toAssetsUp pos.borrowShares m.totalBorrowAssets m.totalBorrowShares
  let maxBorrow := MathLib.wMulDown
    (MathLib.mulDivDown pos.collateral collateralPrice
      (u256 ConstantsLib.ORACLE_PRICE_SCALE))
    lltv
  maxBorrow.val ≥ borrowed.val

/-! ## Interest accrual -/

/--
  Accrue interest on a market. Matches `_accrueInterest` in Morpho.sol.

  `borrowRate` is the per-second rate returned by the IRM oracle. In Solidity
  this is fetched via `IIrm(irm).borrowRate(marketParams, market[id])`.
  Here it is passed as a parameter since we verify logic, not external calls.
-/
def accrueInterest (s : MorphoState) (id : Id) (borrowRate : Uint256) : MorphoState :=
  let m := s.market id
  let elapsed := u256 (s.blockTimestamp.val - m.lastUpdate.val)
  if elapsed.val == 0 then s
  else
    let interest := MathLib.wMulDown m.totalBorrowAssets (MathLib.wTaylorCompounded borrowRate elapsed)
    let newTotalBorrowAssets := u256 (m.totalBorrowAssets.val + interest.val)
    let newTotalSupplyAssets := u256 (m.totalSupplyAssets.val + interest.val)
    -- Fee handling
    let (newTotalSupplyShares, feeRecipientShares) :=
      if m.fee.val != 0 then
        let feeAmount := MathLib.wMulDown interest m.fee
        let feeShares := SharesMathLib.toSharesDown feeAmount
          (u256 (newTotalSupplyAssets.val - feeAmount.val))
          m.totalSupplyShares
        (u256 (m.totalSupplyShares.val + feeShares.val), feeShares)
      else
        (m.totalSupplyShares, u256 0)
    -- Update fee recipient position
    let feePos := s.position id s.feeRecipient
    let newFeePos := { feePos with
      supplyShares := u256 (feePos.supplyShares.val + feeRecipientShares.val) }
    let newMarket := { m with
      totalBorrowAssets := newTotalBorrowAssets
      totalSupplyAssets := newTotalSupplyAssets
      totalSupplyShares := newTotalSupplyShares
      lastUpdate := s.blockTimestamp }
    { s with
      market := fun id' => if id' == id then newMarket else s.market id'
      position := fun id' addr =>
        if id' == id && addr == s.feeRecipient then newFeePos
        else s.position id' addr }

/-! ## Owner functions -/

/-- Set a new owner. Reverts if sender is not current owner. -/
def setOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else some { s with owner := newOwner }

/-- Enable an IRM. Owner-only. -/
def enableIrm (s : MorphoState) (irm : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else some { s with isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a }

/-- Enable an LLTV. Owner-only. LLTV must be < WAD. -/
def enableLltv (s : MorphoState) (lltv : Uint256) : Option MorphoState :=
  if s.sender != s.owner then none
  else if lltv.val ≥ MathLib.WAD then none
  else some { s with isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l }

/-- Set fee for a market. Owner-only. Fee ≤ MAX_FEE. Market must exist. -/
def setFee (s : MorphoState) (id : Id) (newFee : Uint256) : Option MorphoState :=
  if s.sender != s.owner then none
  else if newFee.val > ConstantsLib.MAX_FEE then none
  else let m := s.market id
    if m.lastUpdate.val == 0 then none  -- market not created
    else some { s with
      market := fun id' => if id' == id then { m with fee := newFee } else s.market id' }

/-- Set fee recipient. Owner-only. -/
def setFeeRecipient (s : MorphoState) (newFeeRecipient : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else some { s with feeRecipient := newFeeRecipient }

/-! ## Market creation -/

/-- Create a new lending market. Anyone can call.
    The IRM and LLTV must be pre-enabled. The market must not already exist. -/
def createMarket (s : MorphoState) (params : MarketParams) (id : Id) : Option MorphoState :=
  if ¬(s.isIrmEnabled params.irm) then none
  else if ¬(s.isLltvEnabled params.lltv) then none
  else let m := s.market id
    if m.lastUpdate.val != 0 then none  -- already created
    else some { s with
      market := fun id' => if id' == id
        then { m with lastUpdate := s.blockTimestamp }
        else s.market id'
      idToParams := fun id' => if id' == id then some params else s.idToParams id' }

/-! ## Authorization -/

/-- Authorize or deauthorize another address to act on your behalf. -/
def setAuthorization (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    : MorphoState :=
  { s with isAuthorized := fun authorizer auth =>
      if authorizer == s.sender && auth == authorized then newIsAuthorized
      else s.isAuthorized authorizer auth }

/-! ## Core operations -/

/--
  Supply assets to a market. Matches `supply` in Morpho.sol.
  Exactly one of `assets` or `shares` must be zero.
  Returns (suppliedAssets, suppliedShares, newState).
-/
def supply (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else
    let m := s.market id
    if m.lastUpdate.val == 0 then none  -- market not created
    else
      -- Compute the other value from whichever was provided
      let (assets, shares) :=
        if assets.val > 0 then
          (assets, SharesMathLib.toSharesDown assets m.totalSupplyAssets m.totalSupplyShares)
        else
          (SharesMathLib.toAssetsUp shares m.totalSupplyAssets m.totalSupplyShares, shares)
      let pos := s.position id onBehalf
      let newPos := { pos with
        supplyShares := u256 (pos.supplyShares.val + shares.val) }
      let newMarket := { m with
        totalSupplyShares := u256 (m.totalSupplyShares.val + shares.val)
        totalSupplyAssets := u256 (m.totalSupplyAssets.val + assets.val) }
      some (assets, shares, { s with
        market := fun id' => if id' == id then newMarket else s.market id'
        position := fun id' addr =>
          if id' == id && addr == onBehalf then newPos
          else s.position id' addr })

/--
  Withdraw assets from a market. Matches `withdraw` in Morpho.sol.
  Exactly one of `assets` or `shares` must be zero.
  Sender must be authorized to act on behalf of `onBehalf`.
  Returns (withdrawnAssets, withdrawnShares, newState).
-/
def withdraw (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  if ¬(isSenderAuthorized s onBehalf) then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else
    let m := s.market id
    if m.lastUpdate.val == 0 then none
    else
      let (assets, shares) :=
        if assets.val > 0 then
          (assets, SharesMathLib.toSharesUp assets m.totalSupplyAssets m.totalSupplyShares)
        else
          (SharesMathLib.toAssetsDown shares m.totalSupplyAssets m.totalSupplyShares, shares)
      let pos := s.position id onBehalf
      -- Check: shares don't exceed position
      if shares.val > pos.supplyShares.val then none
      else
        let newPos := { pos with
          supplyShares := u256 (pos.supplyShares.val - shares.val) }
        let newMarket := { m with
          totalSupplyShares := u256 (m.totalSupplyShares.val - shares.val)
          totalSupplyAssets := u256 (m.totalSupplyAssets.val - assets.val) }
        -- Liquidity check: totalBorrowAssets ≤ totalSupplyAssets
        if newMarket.totalBorrowAssets.val > newMarket.totalSupplyAssets.val then none
        else
          some (assets, shares, { s with
            market := fun id' => if id' == id then newMarket else s.market id'
            position := fun id' addr =>
              if id' == id && addr == onBehalf then newPos
              else s.position id' addr })

/--
  Borrow assets from a market. Matches `borrow` in Morpho.sol.
  Exactly one of `assets` or `shares` must be zero.
  Sender must be authorized. Position must remain healthy after borrowing.
  Returns (borrowedAssets, borrowedShares, newState).
-/
def borrow (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (collateralPrice : Uint256) (lltv : Uint256)
    : Option (Uint256 × Uint256 × MorphoState) :=
  if ¬(isSenderAuthorized s onBehalf) then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else
    let m := s.market id
    if m.lastUpdate.val == 0 then none
    else
      let (assets, shares) :=
        if assets.val > 0 then
          (assets, SharesMathLib.toSharesUp assets m.totalBorrowAssets m.totalBorrowShares)
        else
          (SharesMathLib.toAssetsDown shares m.totalBorrowAssets m.totalBorrowShares, shares)
      let pos := s.position id onBehalf
      let newPos := { pos with
        borrowShares := u256 (pos.borrowShares.val + shares.val) }
      let newMarket := { m with
        totalBorrowShares := u256 (m.totalBorrowShares.val + shares.val)
        totalBorrowAssets := u256 (m.totalBorrowAssets.val + assets.val) }
      let s' := { s with
        market := fun id' => if id' == id then newMarket else s.market id'
        position := fun id' addr =>
          if id' == id && addr == onBehalf then newPos
          else s.position id' addr }
      -- Health check
      if ¬(isHealthy s' id onBehalf collateralPrice lltv) then none
      -- Liquidity check
      else if newMarket.totalBorrowAssets.val > newMarket.totalSupplyAssets.val then none
      else some (assets, shares, s')

/--
  Repay borrowed assets. Matches `repay` in Morpho.sol.
  Exactly one of `assets` or `shares` must be zero.
  Returns (repaidAssets, repaidShares, newState).
-/
def repay (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else
    let m := s.market id
    if m.lastUpdate.val == 0 then none
    else
      let (assets, shares) :=
        if assets.val > 0 then
          (assets, SharesMathLib.toSharesDown assets m.totalBorrowAssets m.totalBorrowShares)
        else
          (SharesMathLib.toAssetsUp shares m.totalBorrowAssets m.totalBorrowShares, shares)
      let pos := s.position id onBehalf
      if shares.val > pos.borrowShares.val then none
      else
        let newPos := { pos with
          borrowShares := u256 (pos.borrowShares.val - shares.val) }
        let newMarket := { m with
          totalBorrowShares := u256 (m.totalBorrowShares.val - shares.val)
          totalBorrowAssets := UtilsLib.zeroFloorSub m.totalBorrowAssets assets }
        some (assets, shares, { s with
          market := fun id' => if id' == id then newMarket else s.market id'
          position := fun id' addr =>
            if id' == id && addr == onBehalf then newPos
            else s.position id' addr })

/--
  Deposit collateral. Matches `supplyCollateral` in Morpho.sol.
  Does NOT accrue interest (gas optimization in Solidity).
-/
def supplyCollateral (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    : Option MorphoState :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else
    let pos := s.position id onBehalf
    let newPos := { pos with
      collateral := u256 (pos.collateral.val + assets.val) }
    some { s with
      position := fun id' addr =>
        if id' == id && addr == onBehalf then newPos
        else s.position id' addr }

/--
  Withdraw collateral. Matches `withdrawCollateral` in Morpho.sol.
  Sender must be authorized. Position must remain healthy after withdrawal.
-/
def withdrawCollateral (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    (collateralPrice : Uint256) (lltv : Uint256)
    : Option MorphoState :=
  if ¬(isSenderAuthorized s onBehalf) then none
  else
    let m := s.market id
    if m.lastUpdate.val == 0 then none
    else
      let pos := s.position id onBehalf
      if assets.val > pos.collateral.val then none
      else
        let newPos := { pos with
          collateral := u256 (pos.collateral.val - assets.val) }
        let s' := { s with
          position := fun id' addr =>
            if id' == id && addr == onBehalf then newPos
            else s.position id' addr }
        if ¬(isHealthy s' id onBehalf collateralPrice lltv) then none
        else some s'

/-! ## Liquidation -/

/--
  Liquidate an unhealthy position. Matches `liquidate` in Morpho.sol.
  Exactly one of `seizedAssets` or `repaidShares` must be zero.
  The borrower must be unhealthy. Bad debt is socialized if collateral hits zero.
  Returns (seizedAssets, repaidAssets, newState).
-/
def liquidate (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares : Uint256) (collateralPrice : Uint256) (lltv : Uint256)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  -- Borrower must be unhealthy
  else if isHealthy s id borrower collateralPrice lltv then none
  else
    -- Compute liquidation incentive factor
    -- = min(MAX_LIF, WAD / (WAD - CURSOR * (WAD - lltv)))
    let wadMinusLltv := ConstantsLib.WAD - lltv.val
    let cursorTerm := (ConstantsLib.LIQUIDATION_CURSOR * wadMinusLltv) / ConstantsLib.WAD
    let denominator := ConstantsLib.WAD - cursorTerm
    let computedLIF := (ConstantsLib.WAD * ConstantsLib.WAD) / denominator
    let lif := if computedLIF > ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR
               then ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR
               else computedLIF
    let lifU := u256 lif
    -- Compute seized/repaid pair
    let (seizedAssets, repaidShares) :=
      if seizedAssets.val > 0 then
        -- Given seized collateral, compute repaid shares
        let seizedQuoted := MathLib.mulDivUp seizedAssets collateralPrice
          (u256 ConstantsLib.ORACLE_PRICE_SCALE)
        let repaidAmount := MathLib.wDivUp seizedQuoted lifU
        let repShares := SharesMathLib.toSharesUp repaidAmount m.totalBorrowAssets m.totalBorrowShares
        (seizedAssets, repShares)
      else
        -- Given repaid shares, compute seized collateral
        let repaidAmount := SharesMathLib.toAssetsDown repaidShares m.totalBorrowAssets m.totalBorrowShares
        let seizedValue := MathLib.wMulDown repaidAmount lifU
        let seized := MathLib.mulDivDown seizedValue
          (u256 ConstantsLib.ORACLE_PRICE_SCALE) collateralPrice
        (seized, repaidShares)
    let repaidAssets := SharesMathLib.toAssetsUp repaidShares m.totalBorrowAssets m.totalBorrowShares
    let pos := s.position id borrower
    -- Check borrower has enough shares and collateral
    if repaidShares.val > pos.borrowShares.val then none
    else if seizedAssets.val > pos.collateral.val then none
    else
      let newBorrowShares := u256 (pos.borrowShares.val - repaidShares.val)
      let newCollateral := u256 (pos.collateral.val - seizedAssets.val)
      let newTotalBorrowShares := u256 (m.totalBorrowShares.val - repaidShares.val)
      let newTotalBorrowAssets := UtilsLib.zeroFloorSub m.totalBorrowAssets repaidAssets
      -- Bad debt socialization: if collateral hits 0, socialize remaining debt
      let (finalBorrowShares, finalTotalBorrowShares, finalTotalBorrowAssets, finalTotalSupplyAssets) :=
        if newCollateral.val == 0 && newBorrowShares.val > 0 then
          let badDebtShares := newBorrowShares
          let badDebtAssets := UtilsLib.min newTotalBorrowAssets
            (SharesMathLib.toAssetsUp badDebtShares newTotalBorrowAssets newTotalBorrowShares)
          ( u256 0,
            u256 (newTotalBorrowShares.val - badDebtShares.val),
            u256 (newTotalBorrowAssets.val - badDebtAssets.val),
            u256 (m.totalSupplyAssets.val - badDebtAssets.val) )
        else
          (newBorrowShares, newTotalBorrowShares, newTotalBorrowAssets, m.totalSupplyAssets)
      let newPos := { pos with
        borrowShares := finalBorrowShares
        collateral := newCollateral }
      let newMarket := { m with
        totalBorrowShares := finalTotalBorrowShares
        totalBorrowAssets := finalTotalBorrowAssets
        totalSupplyAssets := finalTotalSupplyAssets }
      some (seizedAssets, repaidAssets, { s with
        market := fun id' => if id' == id then newMarket else s.market id'
        position := fun id' addr =>
          if id' == id && addr == borrower then newPos
          else s.position id' addr })

end Morpho
