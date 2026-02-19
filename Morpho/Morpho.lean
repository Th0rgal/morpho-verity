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
  - Events and callbacks are omitted (I/O, not state logic).
  - Interest accrual: In Solidity, supply/withdraw/borrow/repay/liquidate
    each call `_accrueInterest` atomically at the start. Here, `accrueInterest`
    is a separate function. Callers compose it externally:
    `accrueInterest s id rate >>= supply ... `. This separates concerns and
    simplifies proofs without losing any state-transition coverage.
  - `collateralPrice` and `lltv` are explicit parameters (not read from
    stored market params) because oracle lookups and param-to-id mapping
    are external. Proofs can assume they match the market's actual values.
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

/-- Convert a natural number to Uint256 (wrapping modulo 2^256). -/
def u256 (n : Nat) : Uint256 := Verity.Core.Uint256.ofNat n

@[simp] theorem u256_val (n : Nat) : (u256 n).val = n % Verity.Core.Uint256.modulus := rfl

/-! ## Internal helpers -/

/-- Check if sender is authorized to act on behalf of `onBehalf`.
    Matches `_isSenderAuthorized` (Morpho.sol:466). -/
def isSenderAuthorized (s : MorphoState) (onBehalf : Address) : Bool :=
  s.sender == onBehalf || s.isAuthorized onBehalf s.sender

/-- Check if a position is healthy given the collateral price.
    Matches `_isHealthy(MarketParams,Id,address,uint256)` (Morpho.sol:526).
    `collateralPrice` and `lltv` are parameters because oracle/params lookup is external. -/
def isHealthy (s : MorphoState) (id : Id) (borrower : Address) (collateralPrice : Uint256)
    (lltv : Uint256) : Bool :=
  let m := s.market id
  let pos := s.position id borrower
  -- Short-circuit: no borrows => always healthy (Morpho.sol:515)
  if pos.borrowShares.val == 0 then true
  else
    let borrowed := SharesMathLib.toAssetsUp pos.borrowShares m.totalBorrowAssets m.totalBorrowShares
    let maxBorrow := MathLib.wMulDown
      (MathLib.mulDivDown pos.collateral collateralPrice
        (u256 ConstantsLib.ORACLE_PRICE_SCALE))
      lltv
    maxBorrow.val ≥ borrowed.val

/-! ## Interest accrual -/

/--
  Accrue interest on a market. Matches `_accrueInterest` (Morpho.sol:482).

  `borrowRate` is the per-second rate returned by the IRM oracle. In Solidity
  this is fetched via `IIrm(irm).borrowRate(marketParams, market[id])`.
  Here it is passed as a parameter since we verify logic, not external calls.

  When `hasIrm = false` (modeling `irm == address(0)`), no interest is computed
  but `lastUpdate` is still set.
-/
def accrueInterest (s : MorphoState) (id : Id) (borrowRate : Uint256) (hasIrm : Bool := true)
    : MorphoState :=
  let m := s.market id
  let elapsed := u256 (s.blockTimestamp.val - m.lastUpdate.val)
  if elapsed.val == 0 then s
  else if ¬hasIrm then
    -- irm == address(0): no interest, just update timestamp (Morpho.sol:486,507)
    { s with market := fun id' =>
        if id' == id then { m with lastUpdate := s.blockTimestamp } else s.market id' }
  else
    let interest := MathLib.wMulDown m.totalBorrowAssets (MathLib.wTaylorCompounded borrowRate elapsed)
    let newTotalBorrowAssets := u256 (m.totalBorrowAssets.val + interest.val)
    let newTotalSupplyAssets := u256 (m.totalSupplyAssets.val + interest.val)
    -- Fee handling (Morpho.sol:493-501)
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

/-- Public accrueInterest. Matches `accrueInterest` (Morpho.sol:473). -/
def accrueInterestPublic (s : MorphoState) (id : Id) (borrowRate : Uint256)
    (hasIrm : Bool := true) : Option MorphoState :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else some (accrueInterest s id borrowRate hasIrm)

/-! ## Owner functions -/

/-- Set a new owner. Matches `setOwner` (Morpho.sol:95). -/
def setOwner (s : MorphoState) (newOwner : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else if newOwner == s.owner then none  -- ALREADY_SET
  else some { s with owner := newOwner }

/-- Enable an IRM. Matches `enableIrm` (Morpho.sol:104). -/
def enableIrm (s : MorphoState) (irm : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else if s.isIrmEnabled irm then none  -- ALREADY_SET
  else some { s with isIrmEnabled := fun a => if a == irm then true else s.isIrmEnabled a }

/-- Enable an LLTV. Matches `enableLltv` (Morpho.sol:113). -/
def enableLltv (s : MorphoState) (lltv : Uint256) : Option MorphoState :=
  if s.sender != s.owner then none
  else if s.isLltvEnabled lltv then none  -- ALREADY_SET
  else if lltv.val ≥ MathLib.WAD then none
  else some { s with isLltvEnabled := fun l => if l == lltv then true else s.isLltvEnabled l }

/-- Set fee for a market. Matches `setFee` (Morpho.sol:123).
    Accrues interest using the OLD fee before changing it.
    `borrowRate` is needed for accrual. -/
def setFee (s : MorphoState) (id : Id) (newFee : Uint256) (borrowRate : Uint256)
    (hasIrm : Bool := true) : Option MorphoState :=
  if s.sender != s.owner then none
  else let m := s.market id
    if m.lastUpdate.val == 0 then none
    else if newFee == m.fee then none  -- ALREADY_SET
    else if newFee.val > ConstantsLib.MAX_FEE then none
    else
      -- Accrue interest with the old fee BEFORE changing it (Morpho.sol:130)
      let s' := accrueInterest s id borrowRate hasIrm
      let m' := s'.market id
      some { s' with
        market := fun id' => if id' == id then { m' with fee := newFee } else s'.market id' }

/-- Set fee recipient. Matches `setFeeRecipient` (Morpho.sol:139). -/
def setFeeRecipient (s : MorphoState) (newFeeRecipient : Address) : Option MorphoState :=
  if s.sender != s.owner then none
  else if newFeeRecipient == s.feeRecipient then none  -- ALREADY_SET
  else some { s with feeRecipient := newFeeRecipient }

/-! ## Market creation -/

/-- Create a new lending market. Matches `createMarket` (Morpho.sol:150). -/
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

/-- Authorize or deauthorize another address. Matches `setAuthorization` (Morpho.sol:436). -/
def setAuthorization (s : MorphoState) (authorized : Address) (newIsAuthorized : Bool)
    : Option MorphoState :=
  -- ALREADY_SET check (Morpho.sol:437)
  if newIsAuthorized == s.isAuthorized s.sender authorized then none
  else some { s with isAuthorized := fun authorizer auth =>
      if authorizer == s.sender && auth == authorized then newIsAuthorized
      else s.isAuthorized authorizer auth }

/-- Authorize via EIP-712 signature. Matches `setAuthorizationWithSig` (Morpho.sol:445).
    The signature verification (EIP-712 digest + ecrecover) is cryptographic and cannot
    be modeled in pure state logic. We take `signatureValid : Bool` as a parameter,
    consistent with how oracle prices and IRM rates are externalized.
    The pure state logic — nonce check, nonce increment, deadline check — is fully modeled.

    Note: unlike `setAuthorization`, there is NO `ALREADY_SET` check. The Solidity comment
    says "Do not check whether authorization is already set because the nonce increment
    is a desired side effect." (Morpho.sol:446). -/
def setAuthorizationWithSig (s : MorphoState) (auth : Authorization) (signatureValid : Bool)
    : Option MorphoState :=
  -- Deadline check (Morpho.sol:447)
  if s.blockTimestamp.val > auth.deadline.val then none
  -- Nonce check (Morpho.sol:448)
  else if auth.nonce != s.nonce auth.authorizer then none
  -- Signature validity (Morpho.sol:450-454, modeled as parameter)
  else if ¬signatureValid then none
  else some { s with
    -- Nonce increment (Morpho.sol:448, the ++ in nonce[authorizer]++)
    nonce := fun addr =>
      if addr == auth.authorizer then u256 ((s.nonce auth.authorizer).val + 1)
      else s.nonce addr
    -- Authorization update (Morpho.sol:458)
    isAuthorized := fun authorizer authorized =>
      if authorizer == auth.authorizer && authorized == auth.authorized then auth.isAuthorized
      else s.isAuthorized authorizer authorized }

/-! ## Core operations -/

/-- Supply assets to a market. Matches `supply` (Morpho.sol:169).
    Exactly one of `assets` or `shares` must be zero.
    Caller must accrue interest first (see module header).
    Returns (suppliedAssets, suppliedShares, newState). -/
def supply (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else if onBehalf == "" then none  -- ZERO_ADDRESS (Morpho.sol:179)
  else
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

/-- Withdraw assets from a market. Matches `withdraw` (Morpho.sol:200).
    Exactly one of `assets` or `shares` must be zero.
    `receiver` gets the tokens; `onBehalf`'s position is debited.
    Caller must accrue interest first (see module header).
    Returns (withdrawnAssets, withdrawnShares, newState). -/
def withdraw (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else if receiver == "" then none  -- ZERO_ADDRESS (Morpho.sol:210)
  else if ¬(isSenderAuthorized s onBehalf) then none
  else
    let (assets, shares) :=
      if assets.val > 0 then
        (assets, SharesMathLib.toSharesUp assets m.totalSupplyAssets m.totalSupplyShares)
      else
        (SharesMathLib.toAssetsDown shares m.totalSupplyAssets m.totalSupplyShares, shares)
    let pos := s.position id onBehalf
    -- Underflow check (Solidity reverts on uint underflow)
    if shares.val > pos.supplyShares.val then none
    else
      let newPos := { pos with
        supplyShares := u256 (pos.supplyShares.val - shares.val) }
      let newMarket := { m with
        totalSupplyShares := u256 (m.totalSupplyShares.val - shares.val)
        totalSupplyAssets := u256 (m.totalSupplyAssets.val - assets.val) }
      -- Liquidity check (Morpho.sol:223)
      if newMarket.totalBorrowAssets.val > newMarket.totalSupplyAssets.val then none
      else
        some (assets, shares, { s with
          market := fun id' => if id' == id then newMarket else s.market id'
          position := fun id' addr =>
            if id' == id && addr == onBehalf then newPos
            else s.position id' addr })

/-- Borrow assets from a market. Matches `borrow` (Morpho.sol:235).
    Exactly one of `assets` or `shares` must be zero.
    `receiver` gets the tokens; `onBehalf`'s position is debited.
    Caller must accrue interest first (see module header).
    Returns (borrowedAssets, borrowedShares, newState). -/
def borrow (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice : Uint256) (lltv : Uint256)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else if receiver == "" then none  -- ZERO_ADDRESS (Morpho.sol:245)
  else if ¬(isSenderAuthorized s onBehalf) then none
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
    -- Health check (Morpho.sol:258)
    if ¬(isHealthy s' id onBehalf collateralPrice lltv) then none
    -- Liquidity check (Morpho.sol:259)
    else if newMarket.totalBorrowAssets.val > newMarket.totalSupplyAssets.val then none
    else some (assets, shares, s')

/-- Repay borrowed assets. Matches `repay` (Morpho.sol:269).
    Exactly one of `assets` or `shares` must be zero.
    Caller must accrue interest first (see module header).
    Returns (repaidAssets, repaidShares, newState). -/
def repay (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if ¬(UtilsLib.exactlyOneZero assets shares) then none
  else if onBehalf == "" then none  -- ZERO_ADDRESS (Morpho.sol:279)
  else
    let (assets, shares) :=
      if assets.val > 0 then
        (assets, SharesMathLib.toSharesDown assets m.totalBorrowAssets m.totalBorrowShares)
      else
        (SharesMathLib.toAssetsUp shares m.totalBorrowAssets m.totalBorrowShares, shares)
    let pos := s.position id onBehalf
    -- Underflow check
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

/-! ## Collateral management -/

/-- Deposit collateral. Matches `supplyCollateral` (Morpho.sol:303).
    Does NOT accrue interest (gas optimization in Solidity). -/
def supplyCollateral (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    : Option MorphoState :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if assets.val == 0 then none  -- ZERO_ASSETS (Morpho.sol:308)
  else if onBehalf == "" then none   -- ZERO_ADDRESS (Morpho.sol:309)
  else
    let pos := s.position id onBehalf
    let newPos := { pos with
      collateral := u256 (pos.collateral.val + assets.val) }
    some { s with
      position := fun id' addr =>
        if id' == id && addr == onBehalf then newPos
        else s.position id' addr }

/-- Withdraw collateral. Matches `withdrawCollateral` (Morpho.sol:323).
    `receiver` gets the tokens; `onBehalf`'s position is debited.
    Note: caller must accrue interest before calling (Morpho.sol:333). -/
def withdrawCollateral (s : MorphoState) (id : Id) (assets : Uint256)
    (onBehalf receiver : Address) (collateralPrice : Uint256) (lltv : Uint256)
    : Option MorphoState :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  else if assets.val == 0 then none    -- ZERO_ASSETS (Morpho.sol:328)
  else if receiver == "" then none     -- ZERO_ADDRESS (Morpho.sol:329)
  else if ¬(isSenderAuthorized s onBehalf) then none
  else
    let pos := s.position id onBehalf
    -- Underflow check
    if assets.val > pos.collateral.val then none
    else
      let newPos := { pos with
        collateral := u256 (pos.collateral.val - assets.val) }
      let s' := { s with
        position := fun id' addr =>
          if id' == id && addr == onBehalf then newPos
          else s.position id' addr }
      -- Health check (Morpho.sol:337)
      if ¬(isHealthy s' id onBehalf collateralPrice lltv) then none
      else some s'

/-! ## Liquidation -/

/-- Liquidate an unhealthy position. Matches `liquidate` (Morpho.sol:347).
    Exactly one of `seizedAssets` or `repaidShares` must be zero.
    The borrower must be unhealthy. Bad debt is socialized if collateral hits zero.
    Caller must accrue interest first (see module header).
    Returns (seizedAssets, repaidAssets, newState). -/
def liquidate (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares : Uint256) (collateralPrice : Uint256) (lltv : Uint256)
    : Option (Uint256 × Uint256 × MorphoState) :=
  let m := s.market id
  if m.lastUpdate.val == 0 then none
  -- exactlyOneZero check (Morpho.sol:356)
  else if ¬(UtilsLib.exactlyOneZero seizedAssets repaidShares) then none
  -- Borrower must be unhealthy (Morpho.sol:363)
  else if isHealthy s id borrower collateralPrice lltv then none
  else
    -- Liquidation incentive factor (Morpho.sol:366-369)
    -- = min(MAX_LIF, WAD.wDivDown(WAD - CURSOR.wMulDown(WAD - lltv)))
    let wadU := u256 ConstantsLib.WAD
    let wadMinusLltv := u256 (ConstantsLib.WAD - lltv.val)
    let cursorTerm := MathLib.wMulDown (u256 ConstantsLib.LIQUIDATION_CURSOR) wadMinusLltv
    let denominator := u256 (wadU.val - cursorTerm.val)
    let computedLIF := MathLib.wDivDown wadU denominator
    let lifU := UtilsLib.min (u256 ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR) computedLIF
    -- Compute seized/repaid pair (Morpho.sol:371-379)
    let (seizedAssets, repaidShares) :=
      if seizedAssets.val > 0 then
        let seizedQuoted := MathLib.mulDivUp seizedAssets collateralPrice
          (u256 ConstantsLib.ORACLE_PRICE_SCALE)
        let repaidAmount := MathLib.wDivUp seizedQuoted lifU
        let repShares := SharesMathLib.toSharesUp repaidAmount m.totalBorrowAssets m.totalBorrowShares
        (seizedAssets, repShares)
      else
        let repaidAmount := SharesMathLib.toAssetsDown repaidShares m.totalBorrowAssets m.totalBorrowShares
        let seizedValue := MathLib.wMulDown repaidAmount lifU
        let seized := MathLib.mulDivDown seizedValue
          (u256 ConstantsLib.ORACLE_PRICE_SCALE) collateralPrice
        (seized, repaidShares)
    let repaidAssets := SharesMathLib.toAssetsUp repaidShares m.totalBorrowAssets m.totalBorrowShares
    let pos := s.position id borrower
    -- Underflow checks (Solidity reverts on uint underflow)
    if repaidShares.val > pos.borrowShares.val then none
    else if seizedAssets.val > pos.collateral.val then none
    else
      let newBorrowShares := u256 (pos.borrowShares.val - repaidShares.val)
      let newCollateral := u256 (pos.collateral.val - seizedAssets.val)
      let newTotalBorrowShares := u256 (m.totalBorrowShares.val - repaidShares.val)
      let newTotalBorrowAssets := UtilsLib.zeroFloorSub m.totalBorrowAssets repaidAssets
      -- Bad debt socialization (Morpho.sol:391-402)
      -- Matches Solidity: condition is collateral == 0 (not && borrowShares > 0)
      let (finalBorrowShares, finalTotalBorrowShares, finalTotalBorrowAssets, finalTotalSupplyAssets) :=
        if newCollateral.val == 0 then
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

/-! ## Flash loans -/

/-- Flash loan. Matches `flashLoan` (Morpho.sol:421).
    The actual token transfer and callback are external I/O — here we only
    verify that `assets != 0`. The state is unchanged (flash loans are atomic). -/
def flashLoan (_s : MorphoState) (assets : Uint256) : Option Unit :=
  if assets.val == 0 then none  -- ZERO_ASSETS (Morpho.sol:422)
  else some ()

end Morpho
