import Compiler.ContractSpec
import Compiler.Modules.ERC20
import Compiler.Modules.Callbacks
import Compiler.Modules.Calls
import Compiler.Modules.Precompiles

namespace Morpho.Compiler.Spec

open Compiler.ContractSpec
open Compiler.Modules

-- ============================================================================
-- Constants
-- ============================================================================

private def wad : Nat := 1000000000000000000
private def oracleScale : Nat := 1000000000000000000000000000000000000 -- 1e36
private def maxFee : Nat := 250000000000000000
private def virtualShares : Nat := 1000000
private def virtualAssets : Nat := 1
private def maxLIF : Nat := 1150000000000000000  -- 1.15e18
private def betaLIF : Nat := 300000000000000000  -- 0.3e18

-- ============================================================================
-- Common helpers
-- ============================================================================

private def requireOwner : Stmt :=
  Stmt.require (Expr.eq Expr.caller (Expr.storage "owner")) "not owner"

private def marketParamsTy : ParamType :=
  .tuple [.address, .address, .address, .address, .uint256]

private def marketIdExpr (params : Array Expr) : Expr :=
  Expr.externalCall "keccakMarketParams" params.toList

private def marketIdFromTupleParam (paramName : String) : Expr :=
  marketIdExpr #[
    Expr.param s!"{paramName}_0",
    Expr.param s!"{paramName}_1",
    Expr.param s!"{paramName}_2",
    Expr.param s!"{paramName}_3",
    Expr.param s!"{paramName}_4"
  ]

/-- Unpack market params tuple into local variables and compute id. -/
private def unpackMarketParams (paramName : String := "marketParams") : List Stmt := [
  Stmt.letVar "loanToken" (Expr.param s!"{paramName}_0"),
  Stmt.letVar "collateralToken" (Expr.param s!"{paramName}_1"),
  Stmt.letVar "oracle" (Expr.param s!"{paramName}_2"),
  Stmt.letVar "irm" (Expr.param s!"{paramName}_3"),
  Stmt.letVar "lltv" (Expr.param s!"{paramName}_4"),
  Stmt.letVar "id" (marketIdFromTupleParam paramName)
]

/-- Require that a market has been created (lastUpdate > 0). -/
private def requireMarketCreated : Stmt :=
  Stmt.require
    (Expr.gt (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0))
    "market not created"

/-- Require exactly one of assets/shares is nonzero. -/
private def requireConsistentInput (a b : Expr) : Stmt :=
  Stmt.require
    (Expr.logicalNot (Expr.eq
      (Expr.bitXor
        (Expr.eq a (Expr.literal 0))
        (Expr.eq b (Expr.literal 0)))
      (Expr.literal 0)))
    "inconsistent input"

/-- Require address is not zero. -/
private def requireNonZeroAddr (e : Expr) : Stmt :=
  Stmt.require
    (Expr.logicalNot (Expr.eq e (Expr.literal 0)))
    "zero address"

/-- Require caller is authorized for onBehalf. -/
private def requireAuthorized (onBehalf : Expr) : Stmt :=
  Stmt.require
    (Expr.logicalOr
      (Expr.eq Expr.caller onBehalf)
      (Expr.eq (Expr.mapping2 "isAuthorized" onBehalf Expr.caller) (Expr.literal 1)))
    "unauthorized"

-- ============================================================================
-- Event topic hashes (precomputed keccak256 of event signatures)
-- ============================================================================

-- Supply(bytes32,address,address,uint256,uint256)
private def supplyEventTopic : Nat := 0xedf8870433c83823eb071d3df1caa8d008f12f6440918c20d75a3602cda30fe0
-- Withdraw(bytes32,address,address,address,uint256,uint256)
private def withdrawEventTopic : Nat := 0xa56fc0ad5702ec05ce63666221f796fb62437c32db1aa1aa075fc6484cf58fbf
-- SupplyCollateral(bytes32,address,address,uint256)
private def supplyCollateralEventTopic : Nat := 0xa3b9472a1399e17e123f3c2e6586c23e504184d504de59cdaa2b375e880c6184
-- WithdrawCollateral(bytes32,address,address,address,uint256)
private def withdrawCollateralEventTopic : Nat := 0xe80ebd7cc9223d7382aab2e0d1d6155c65651f83d53c8b9b06901d167e321142
-- Borrow(bytes32,address,address,address,uint256,uint256)
private def borrowEventTopic : Nat := 0x570954540bed6b1304a87dfe815a5eda4a648f7097a16240dcd85c9b5fd42a43
-- Repay(bytes32,address,address,uint256,uint256)
private def repayEventTopic : Nat := 0x52acb05cebbd3cd39715469f22afbf5a17496295ef3bc9bb5944056c63ccaa09
-- Liquidate(bytes32,address,address,uint256,uint256,uint256,uint256,uint256)
private def liquidateEventTopic : Nat := 0xa4946ede45d0c6f06a0f5ce92c9ad3b4751452d2fe0e25010783bcab57a67e41
-- FlashLoan(address,address,uint256)
private def flashLoanEventTopic : Nat := 0xc76f1b4fe4396ac07a9fa55a415d4ca430e72651d37d3401f3bed7cb13fc4f12
-- AccrueInterest(bytes32,uint256,uint256,uint256)
private def accrueInterestEventTopic : Nat := 0x9d9bd501d0657d7dfe415f779a620a62b78bc508ddc0891fbbd8b7ac0f8fce87
-- IncrementNonce(address,address,uint256)
private def incrementNonceEventTopic : Nat := 0xa58af1a0c70dba0c7aa60d1a1a147ebd61000d1690a968828ac718bca927f2c7
-- SetAuthorization(address,address,address,bool) — raw topic for setAuthorizationWithSig
private def setAuthEventTopic : Nat := 0xd5e969f01efe921d3f766bdebad25f0a05e3f237311f56482bf132d0326309c0

-- ============================================================================
-- Callback selectors (4-byte)
-- ============================================================================

private def onMorphoSupplySelector : Nat := 0x2075be03
private def onMorphoRepaySelector : Nat := 0x05b4591c
private def onMorphoSupplyCollateralSelector : Nat := 0xb1022fdf
private def onMorphoLiquidateSelector : Nat := 0xcf7ea196
private def onMorphoFlashLoanSelector : Nat := 0x31f57072

-- IRM borrowRate selector
private def borrowRateSelector : Nat := 0x9451fed4
-- Oracle price() selector
private def oraclePriceSelector : Nat := 0xa035b1fe

-- ============================================================================
-- Operation function bodies
-- ============================================================================

/-- supply((address,address,address,address,uint256),uint256,uint256,address,bytes)
    Returns (assetsSupplied, sharesSupplied). -/
private def supplyBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    -- accrueInterest is handled by injectStorageCompat patch
    requireConsistentInput (Expr.param "assets") (Expr.param "shares"),
    requireNonZeroAddr (Expr.param "onBehalf"),
    -- Load totals
    Stmt.letVar "totalSupplyAssets" (Expr.mappingUint "marketTotalSupplyAssets" (Expr.localVar "id")),
    Stmt.letVar "totalSupplyShares" (Expr.mappingUint "marketTotalSupplyShares" (Expr.localVar "id")),
    -- Compute shares/assets
    Stmt.letVar "denomShares" (Expr.add (Expr.localVar "totalSupplyShares") (Expr.literal virtualShares)),
    Stmt.letVar "denomAssets" (Expr.add (Expr.localVar "totalSupplyAssets") (Expr.literal virtualAssets)),
    Stmt.letVar "assetsSupplied" (Expr.param "assets"),
    Stmt.letVar "sharesSupplied" (Expr.param "shares"),
    Stmt.ite (Expr.gt (Expr.localVar "assetsSupplied") (Expr.literal 0))
      [Stmt.assignVar "sharesSupplied" (Expr.mulDivDown (Expr.localVar "assetsSupplied") (Expr.localVar "denomShares") (Expr.localVar "denomAssets"))]
      [],
    Stmt.ite (Expr.gt (Expr.localVar "sharesSupplied") (Expr.literal 0))
      [Stmt.ite (Expr.eq (Expr.gt (Expr.localVar "assetsSupplied") (Expr.literal 0)) (Expr.literal 0))
        [Stmt.assignVar "assetsSupplied" (Expr.mulDivUp (Expr.localVar "sharesSupplied") (Expr.localVar "denomAssets") (Expr.localVar "denomShares"))]
        []]
      [],
    -- Update position
    Stmt.letVar "posShares" (Expr.mapping2 "positionSupplyShares" (Expr.localVar "id") (Expr.param "onBehalf")),
    Stmt.setMapping2 "positionSupplyShares" (Expr.localVar "id") (Expr.param "onBehalf")
      (Expr.add (Expr.localVar "posShares") (Expr.localVar "sharesSupplied")),
    -- Update totals
    Stmt.letVar "newTotalSupplyAssets" (Expr.add (Expr.localVar "totalSupplyAssets") (Expr.localVar "assetsSupplied")),
    Stmt.letVar "newTotalSupplyShares" (Expr.add (Expr.localVar "totalSupplyShares") (Expr.localVar "sharesSupplied")),
    Stmt.setMappingUint "marketTotalSupplyAssets" (Expr.localVar "id") (Expr.localVar "newTotalSupplyAssets"),
    Stmt.setMappingUint "marketTotalSupplyShares" (Expr.localVar "id") (Expr.localVar "newTotalSupplyShares"),
    -- Emit Supply event via rawLog: log3(0, 96, topic, id, onBehalf)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.localVar "assetsSupplied"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "sharesSupplied"),
    Stmt.rawLog [Expr.literal supplyEventTopic, Expr.localVar "id", Expr.param "onBehalf"] (Expr.literal 0) (Expr.literal 96),
    -- Callback
    Callbacks.callback Expr.caller onMorphoSupplySelector [Expr.localVar "assetsSupplied"] "data",
    -- TransferFrom
    ERC20.safeTransferFrom (Expr.localVar "loanToken") Expr.caller Expr.contractAddress (Expr.localVar "assetsSupplied"),
    -- Return
    Stmt.returnValues [Expr.localVar "assetsSupplied", Expr.localVar "sharesSupplied"]
  ]

/-- withdraw((address,address,address,address,uint256),uint256,uint256,address,address)
    Returns (assetsWithdrawn, sharesWithdrawn). -/
private def withdrawBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    requireConsistentInput (Expr.param "assets") (Expr.param "shares"),
    requireNonZeroAddr (Expr.param "receiver"),
    requireAuthorized (Expr.param "onBehalf"),
    -- Load totals
    Stmt.letVar "totalSupplyAssets" (Expr.mappingUint "marketTotalSupplyAssets" (Expr.localVar "id")),
    Stmt.letVar "totalSupplyShares" (Expr.mappingUint "marketTotalSupplyShares" (Expr.localVar "id")),
    -- Compute shares/assets
    Stmt.letVar "denomShares" (Expr.add (Expr.localVar "totalSupplyShares") (Expr.literal virtualShares)),
    Stmt.letVar "denomAssets" (Expr.add (Expr.localVar "totalSupplyAssets") (Expr.literal virtualAssets)),
    Stmt.letVar "assetsWithdrawn" (Expr.param "assets"),
    Stmt.letVar "sharesWithdrawn" (Expr.param "shares"),
    Stmt.ite (Expr.gt (Expr.localVar "assetsWithdrawn") (Expr.literal 0))
      [Stmt.assignVar "sharesWithdrawn" (Expr.mulDivUp (Expr.localVar "assetsWithdrawn") (Expr.localVar "denomShares") (Expr.localVar "denomAssets"))]
      [],
    Stmt.ite (Expr.gt (Expr.localVar "sharesWithdrawn") (Expr.literal 0))
      [Stmt.ite (Expr.eq (Expr.gt (Expr.localVar "assetsWithdrawn") (Expr.literal 0)) (Expr.literal 0))
        [Stmt.assignVar "assetsWithdrawn" (Expr.mulDivDown (Expr.localVar "sharesWithdrawn") (Expr.localVar "denomAssets") (Expr.localVar "denomShares"))]
        []]
      [],
    -- Check position
    Stmt.letVar "posShares" (Expr.mapping2 "positionSupplyShares" (Expr.localVar "id") (Expr.param "onBehalf")),
    Stmt.require (Expr.ge (Expr.localVar "posShares") (Expr.localVar "sharesWithdrawn")) "insufficient position",
    -- Check totals don't underflow
    Stmt.require
      (Expr.logicalNot (Expr.logicalOr
        (Expr.lt (Expr.localVar "totalSupplyAssets") (Expr.localVar "assetsWithdrawn"))
        (Expr.lt (Expr.localVar "totalSupplyShares") (Expr.localVar "sharesWithdrawn"))))
      "insufficient total",
    -- Check liquidity
    Stmt.letVar "newTotalSupplyAssets" (Expr.sub (Expr.localVar "totalSupplyAssets") (Expr.localVar "assetsWithdrawn")),
    Stmt.letVar "newTotalSupplyShares" (Expr.sub (Expr.localVar "totalSupplyShares") (Expr.localVar "sharesWithdrawn")),
    Stmt.require
      (Expr.ge (Expr.localVar "newTotalSupplyAssets") (Expr.mappingUint "marketTotalBorrowAssets" (Expr.localVar "id")))
      "insufficient liquidity",
    -- Update position
    Stmt.setMapping2 "positionSupplyShares" (Expr.localVar "id") (Expr.param "onBehalf")
      (Expr.sub (Expr.localVar "posShares") (Expr.localVar "sharesWithdrawn")),
    -- Update totals
    Stmt.setMappingUint "marketTotalSupplyAssets" (Expr.localVar "id") (Expr.localVar "newTotalSupplyAssets"),
    Stmt.setMappingUint "marketTotalSupplyShares" (Expr.localVar "id") (Expr.localVar "newTotalSupplyShares"),
    -- Emit Withdraw event: log4(0, 96, topic, id, onBehalf, receiver)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.localVar "assetsWithdrawn"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "sharesWithdrawn"),
    Stmt.rawLog [Expr.literal withdrawEventTopic, Expr.localVar "id", Expr.param "onBehalf", Expr.param "receiver"] (Expr.literal 0) (Expr.literal 96),
    -- Transfer
    ERC20.safeTransfer (Expr.localVar "loanToken") (Expr.param "receiver") (Expr.localVar "assetsWithdrawn"),
    -- Return
    Stmt.returnValues [Expr.localVar "assetsWithdrawn", Expr.localVar "sharesWithdrawn"]
  ]

/-- supplyCollateral((address,address,address,address,uint256),uint256,address,bytes) -/
private def supplyCollateralBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    Stmt.require (Expr.gt (Expr.param "assets") (Expr.literal 0)) "zero assets",
    requireNonZeroAddr (Expr.param "onBehalf"),
    -- Update position
    Stmt.letVar "newCollateral" (Expr.add (Expr.mapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "onBehalf")) (Expr.param "assets")),
    Stmt.setMapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "onBehalf") (Expr.localVar "newCollateral"),
    -- Emit SupplyCollateral: log3(0, 64, topic, id, onBehalf)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.param "assets"),
    Stmt.rawLog [Expr.literal supplyCollateralEventTopic, Expr.localVar "id", Expr.param "onBehalf"] (Expr.literal 0) (Expr.literal 64),
    -- Callback
    Callbacks.callback Expr.caller onMorphoSupplyCollateralSelector [Expr.param "assets"] "data",
    -- TransferFrom
    ERC20.safeTransferFrom (Expr.localVar "collateralToken") Expr.caller Expr.contractAddress (Expr.param "assets"),
    Stmt.stop
  ]

/-- withdrawCollateral((address,address,address,address,uint256),uint256,address,address) -/
private def withdrawCollateralBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    Stmt.require (Expr.gt (Expr.param "assets") (Expr.literal 0)) "zero assets",
    requireNonZeroAddr (Expr.param "receiver"),
    requireAuthorized (Expr.param "onBehalf"),
    -- accrueInterest patched in
    -- Check position
    Stmt.letVar "currentCollateral" (Expr.mapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "onBehalf")),
    Stmt.require (Expr.ge (Expr.localVar "currentCollateral") (Expr.param "assets")) "insufficient collateral",
    Stmt.letVar "newCollateral" (Expr.sub (Expr.localVar "currentCollateral") (Expr.param "assets")),
    Stmt.setMapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "onBehalf") (Expr.localVar "newCollateral"),
    -- Health check: if borrower has borrow shares, check collateral sufficiency
    Stmt.letVar "borrowShares" (Expr.mapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "onBehalf")),
    Stmt.ite (Expr.gt (Expr.localVar "borrowShares") (Expr.literal 0))
      [
        -- Oracle price call
        Calls.withReturn "collateralPrice" (Expr.localVar "oracle") oraclePriceSelector [] (isStatic := true),
        Stmt.letVar "totalBorrowAssets" (Expr.mappingUint "marketTotalBorrowAssets" (Expr.localVar "id")),
        Stmt.letVar "totalBorrowShares" (Expr.mappingUint "marketTotalBorrowShares" (Expr.localVar "id")),
        Stmt.letVar "borrowedAssets" (Expr.mulDivUp (Expr.localVar "borrowShares") (Expr.add (Expr.localVar "totalBorrowAssets") (Expr.literal virtualAssets)) (Expr.add (Expr.localVar "totalBorrowShares") (Expr.literal virtualShares))),
        Stmt.letVar "maxBorrow" (Expr.wMulDown (Expr.mulDivDown (Expr.localVar "newCollateral") (Expr.localVar "collateralPrice") (Expr.literal oracleScale)) (Expr.localVar "lltv")),
        Stmt.require (Expr.le (Expr.localVar "borrowedAssets") (Expr.localVar "maxBorrow")) "insufficient collateral"
      ]
      [],
    -- Emit WithdrawCollateral: log4(0, 64, topic, id, onBehalf, receiver)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.param "assets"),
    Stmt.rawLog [Expr.literal withdrawCollateralEventTopic, Expr.localVar "id", Expr.param "onBehalf", Expr.param "receiver"] (Expr.literal 0) (Expr.literal 64),
    -- Transfer
    ERC20.safeTransfer (Expr.localVar "collateralToken") (Expr.param "receiver") (Expr.param "assets"),
    Stmt.stop
  ]

/-- borrow((address,address,address,address,uint256),uint256,uint256,address,address)
    Returns (assetsBorrowed, sharesBorrowed). -/
private def borrowBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    requireConsistentInput (Expr.param "assets") (Expr.param "shares"),
    requireNonZeroAddr (Expr.param "receiver"),
    requireAuthorized (Expr.param "onBehalf"),
    -- Load totals
    Stmt.letVar "totalBorrowAssets" (Expr.mappingUint "marketTotalBorrowAssets" (Expr.localVar "id")),
    Stmt.letVar "totalBorrowShares" (Expr.mappingUint "marketTotalBorrowShares" (Expr.localVar "id")),
    Stmt.letVar "denomShares" (Expr.add (Expr.localVar "totalBorrowShares") (Expr.literal virtualShares)),
    Stmt.letVar "denomAssets" (Expr.add (Expr.localVar "totalBorrowAssets") (Expr.literal virtualAssets)),
    -- Compute shares/assets
    Stmt.letVar "assetsBorrowed" (Expr.param "assets"),
    Stmt.letVar "sharesBorrowed" (Expr.param "shares"),
    Stmt.ite (Expr.gt (Expr.localVar "assetsBorrowed") (Expr.literal 0))
      [Stmt.assignVar "sharesBorrowed" (Expr.mulDivUp (Expr.localVar "assetsBorrowed") (Expr.localVar "denomShares") (Expr.localVar "denomAssets"))]
      [],
    Stmt.ite (Expr.gt (Expr.localVar "sharesBorrowed") (Expr.literal 0))
      [Stmt.ite (Expr.eq (Expr.gt (Expr.localVar "assetsBorrowed") (Expr.literal 0)) (Expr.literal 0))
        [Stmt.assignVar "assetsBorrowed" (Expr.mulDivDown (Expr.localVar "sharesBorrowed") (Expr.localVar "denomAssets") (Expr.localVar "denomShares"))]
        []]
      [],
    -- Update position
    Stmt.letVar "newBorrowShares" (Expr.add (Expr.mapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "onBehalf")) (Expr.localVar "sharesBorrowed")),
    Stmt.setMapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "onBehalf") (Expr.localVar "newBorrowShares"),
    -- Update totals
    Stmt.letVar "newTotalBorrowAssets" (Expr.add (Expr.localVar "totalBorrowAssets") (Expr.localVar "assetsBorrowed")),
    Stmt.letVar "newTotalBorrowShares" (Expr.add (Expr.localVar "totalBorrowShares") (Expr.localVar "sharesBorrowed")),
    Stmt.setMappingUint "marketTotalBorrowAssets" (Expr.localVar "id") (Expr.localVar "newTotalBorrowAssets"),
    Stmt.setMappingUint "marketTotalBorrowShares" (Expr.localVar "id") (Expr.localVar "newTotalBorrowShares"),
    -- Health check
    Stmt.letVar "collateralValue" (Expr.mapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "onBehalf")),
    Calls.withReturn "collateralPrice" (Expr.localVar "oracle") oraclePriceSelector [] (isStatic := true),
    Stmt.letVar "borrowedAssets" (Expr.mulDivUp (Expr.localVar "newBorrowShares") (Expr.add (Expr.localVar "newTotalBorrowAssets") (Expr.literal virtualAssets)) (Expr.add (Expr.localVar "newTotalBorrowShares") (Expr.literal virtualShares))),
    Stmt.letVar "maxBorrow" (Expr.wMulDown (Expr.mulDivDown (Expr.localVar "collateralValue") (Expr.localVar "collateralPrice") (Expr.literal oracleScale)) (Expr.localVar "lltv")),
    Stmt.require (Expr.le (Expr.localVar "borrowedAssets") (Expr.localVar "maxBorrow")) "insufficient collateral",
    -- Check liquidity
    Stmt.require
      (Expr.ge (Expr.mappingUint "marketTotalSupplyAssets" (Expr.localVar "id")) (Expr.localVar "newTotalBorrowAssets"))
      "insufficient liquidity",
    -- Emit Borrow: log4(0, 96, topic, id, onBehalf, receiver)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.localVar "assetsBorrowed"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "sharesBorrowed"),
    Stmt.rawLog [Expr.literal borrowEventTopic, Expr.localVar "id", Expr.param "onBehalf", Expr.param "receiver"] (Expr.literal 0) (Expr.literal 96),
    -- Transfer
    ERC20.safeTransfer (Expr.localVar "loanToken") (Expr.param "receiver") (Expr.localVar "assetsBorrowed"),
    -- Return
    Stmt.returnValues [Expr.localVar "assetsBorrowed", Expr.localVar "sharesBorrowed"]
  ]

/-- repay((address,address,address,address,uint256),uint256,uint256,address,bytes)
    Returns (assetsRepaid, sharesRepaid). -/
private def repayBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    requireConsistentInput (Expr.param "assets") (Expr.param "shares"),
    requireNonZeroAddr (Expr.param "onBehalf"),
    -- Load totals
    Stmt.letVar "totalBorrowAssets" (Expr.mappingUint "marketTotalBorrowAssets" (Expr.localVar "id")),
    Stmt.letVar "totalBorrowShares" (Expr.mappingUint "marketTotalBorrowShares" (Expr.localVar "id")),
    Stmt.letVar "denomShares" (Expr.add (Expr.localVar "totalBorrowShares") (Expr.literal virtualShares)),
    Stmt.letVar "denomAssets" (Expr.add (Expr.localVar "totalBorrowAssets") (Expr.literal virtualAssets)),
    -- Compute shares/assets
    Stmt.letVar "assetsRepaid" (Expr.param "assets"),
    Stmt.letVar "sharesRepaid" (Expr.param "shares"),
    Stmt.ite (Expr.gt (Expr.localVar "assetsRepaid") (Expr.literal 0))
      [Stmt.assignVar "sharesRepaid" (Expr.mulDivDown (Expr.localVar "assetsRepaid") (Expr.localVar "denomShares") (Expr.localVar "denomAssets"))]
      [],
    Stmt.ite (Expr.gt (Expr.localVar "sharesRepaid") (Expr.literal 0))
      [Stmt.ite (Expr.eq (Expr.gt (Expr.localVar "assetsRepaid") (Expr.literal 0)) (Expr.literal 0))
        [Stmt.assignVar "assetsRepaid" (Expr.mulDivUp (Expr.localVar "sharesRepaid") (Expr.localVar "denomAssets") (Expr.localVar "denomShares"))]
        []]
      [],
    -- Check position
    Stmt.letVar "currentBorrowShares" (Expr.mapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "onBehalf")),
    Stmt.require (Expr.ge (Expr.localVar "currentBorrowShares") (Expr.localVar "sharesRepaid")) "insufficient borrow",
    -- Update position
    Stmt.letVar "newBorrowShares" (Expr.sub (Expr.localVar "currentBorrowShares") (Expr.localVar "sharesRepaid")),
    Stmt.setMapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "onBehalf") (Expr.localVar "newBorrowShares"),
    -- Update totals (saturating sub for borrow assets)
    Stmt.letVar "newTotalBorrowAssets" (Expr.mul (Expr.gt (Expr.localVar "totalBorrowAssets") (Expr.localVar "assetsRepaid")) (Expr.sub (Expr.localVar "totalBorrowAssets") (Expr.localVar "assetsRepaid"))),
    Stmt.letVar "newTotalBorrowShares" (Expr.sub (Expr.localVar "totalBorrowShares") (Expr.localVar "sharesRepaid")),
    Stmt.setMappingUint "marketTotalBorrowShares" (Expr.localVar "id") (Expr.localVar "newTotalBorrowShares"),
    Stmt.setMappingUint "marketTotalBorrowAssets" (Expr.localVar "id") (Expr.localVar "newTotalBorrowAssets"),
    -- Emit Repay: log3(0, 96, topic, id, onBehalf)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.localVar "assetsRepaid"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "sharesRepaid"),
    Stmt.rawLog [Expr.literal repayEventTopic, Expr.localVar "id", Expr.param "onBehalf"] (Expr.literal 0) (Expr.literal 96),
    -- Callback
    Callbacks.callback Expr.caller onMorphoRepaySelector [Expr.localVar "assetsRepaid"] "data",
    -- TransferFrom
    ERC20.safeTransferFrom (Expr.localVar "loanToken") Expr.caller Expr.contractAddress (Expr.localVar "assetsRepaid"),
    -- Return
    Stmt.returnValues [Expr.localVar "assetsRepaid", Expr.localVar "sharesRepaid"]
  ]

/-- liquidate((address,address,address,address,uint256),address,uint256,uint256,bytes)
    Returns (seizedAssetsOut, repaidAssetsOut). -/
-- Memory layout for spilling liquidate intermediate values (high offsets to avoid collision)
-- 0x300: totalBorrowAssetsBefore, 0x320: totalBorrowSharesBefore
-- 0x340: totalSupplyAssetsBefore, 0x360: borrowerBorrowSharesBefore
-- 0x380: borrowerCollateralBefore
private def liqMem (offset : Nat) : Expr := Expr.mload (Expr.literal offset)

private def liquidateBody : List Stmt :=
  unpackMarketParams ++ [
    requireMarketCreated,
    requireConsistentInput (Expr.param "seizedAssets") (Expr.param "repaidShares"),
    -- accrueInterest patched in
    -- Oracle price
    Calls.withReturn "collateralPrice" (Expr.localVar "oracle") oraclePriceSelector [] (isStatic := true),
    -- Load state and spill to memory to avoid stack-too-deep
    Stmt.mstore (Expr.literal 0x300) (Expr.mappingUint "marketTotalBorrowAssets" (Expr.localVar "id")),
    Stmt.mstore (Expr.literal 0x320) (Expr.mappingUint "marketTotalBorrowShares" (Expr.localVar "id")),
    Stmt.mstore (Expr.literal 0x340) (Expr.mappingUint "marketTotalSupplyAssets" (Expr.localVar "id")),
    Stmt.mstore (Expr.literal 0x360) (Expr.mapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "borrower")),
    Stmt.mstore (Expr.literal 0x380) (Expr.mapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "borrower")),
    -- Check unhealthy
    Stmt.letVar "borrowedAssets" (Expr.mulDivUp (liqMem 0x360) (Expr.add (liqMem 0x300) (Expr.literal virtualAssets)) (Expr.add (liqMem 0x320) (Expr.literal virtualShares))),
    Stmt.letVar "maxBorrow" (Expr.wMulDown (Expr.mulDivDown (liqMem 0x380) (Expr.localVar "collateralPrice") (Expr.literal oracleScale)) (Expr.localVar "lltv")),
    Stmt.require (Expr.gt (Expr.localVar "borrowedAssets") (Expr.localVar "maxBorrow")) "position is healthy",
    -- Compute liquidation incentive factor
    -- LIF = WAD * WAD / (WAD - beta * (WAD - lltv) / WAD)
    Stmt.letVar "lifDenom" (Expr.sub (Expr.literal wad) (Expr.mulDivDown (Expr.literal betaLIF) (Expr.sub (Expr.literal wad) (Expr.localVar "lltv")) (Expr.literal wad))),
    Stmt.require (Expr.gt (Expr.localVar "lifDenom") (Expr.literal 0)) "zero LIF denom",
    Stmt.letVar "lif" (Expr.mulDivDown (Expr.literal wad) (Expr.literal wad) (Expr.localVar "lifDenom")),
    Stmt.ite (Expr.gt (Expr.localVar "lif") (Expr.literal maxLIF))
      [Stmt.assignVar "lif" (Expr.literal maxLIF)]
      [],
    -- Compute repaid/seized
    Stmt.letVar "repaidSharesOut" (Expr.param "repaidShares"),
    Stmt.letVar "seizedAssetsOut" (Expr.param "seizedAssets"),
    Stmt.letVar "repaidAssetsOut" (Expr.literal 0),
    Stmt.ite (Expr.gt (Expr.localVar "seizedAssetsOut") (Expr.literal 0))
      [
        -- seizedAssets → repaidShares → repaidAssets
        Stmt.letVar "seizedAssetsQuoted" (Expr.mulDivUp (Expr.localVar "seizedAssetsOut") (Expr.localVar "collateralPrice") (Expr.literal oracleScale)),
        Stmt.letVar "repaidAssetsQuoted" (Expr.mulDivUp (Expr.localVar "seizedAssetsQuoted") (Expr.literal wad) (Expr.localVar "lif")),
        Stmt.assignVar "repaidSharesOut" (Expr.mulDivDown (Expr.localVar "repaidAssetsQuoted") (Expr.add (liqMem 0x320) (Expr.literal virtualShares)) (Expr.add (liqMem 0x300) (Expr.literal virtualAssets))),
        Stmt.assignVar "repaidAssetsOut" (Expr.mulDivUp (Expr.localVar "repaidSharesOut") (Expr.add (liqMem 0x300) (Expr.literal virtualAssets)) (Expr.add (liqMem 0x320) (Expr.literal virtualShares)))
      ]
      [],
    Stmt.ite (Expr.eq (Expr.gt (Expr.localVar "seizedAssetsOut") (Expr.literal 0)) (Expr.literal 0))
      [
        -- repaidShares → repaidAssets, seizedAssets
        Stmt.assignVar "repaidAssetsOut" (Expr.mulDivDown (Expr.localVar "repaidSharesOut") (Expr.add (liqMem 0x300) (Expr.literal virtualAssets)) (Expr.add (liqMem 0x320) (Expr.literal virtualShares))),
        Stmt.require (Expr.gt (Expr.localVar "collateralPrice") (Expr.literal 0)) "zero price",
        Stmt.assignVar "seizedAssetsOut" (Expr.mulDivDown (Expr.mulDivDown (Expr.localVar "repaidAssetsOut") (Expr.localVar "lif") (Expr.literal wad)) (Expr.literal oracleScale) (Expr.localVar "collateralPrice"))
      ]
      [],
    -- Validate and update borrower state
    Stmt.require (Expr.ge (liqMem 0x360) (Expr.localVar "repaidSharesOut")) "repaid exceeds borrow",
    Stmt.letVar "newBorrowerBorrowShares" (Expr.sub (liqMem 0x360) (Expr.localVar "repaidSharesOut")),
    Stmt.require (Expr.ge (liqMem 0x380) (Expr.localVar "seizedAssetsOut")) "seized exceeds collateral",
    Stmt.letVar "newBorrowerCollateral" (Expr.sub (liqMem 0x380) (Expr.localVar "seizedAssetsOut")),
    -- Update totals
    Stmt.letVar "newTotalBorrowShares" (Expr.sub (liqMem 0x320) (Expr.localVar "repaidSharesOut")),
    Stmt.letVar "newTotalBorrowAssets" (Expr.mul (Expr.gt (liqMem 0x300) (Expr.localVar "repaidAssetsOut")) (Expr.sub (liqMem 0x300) (Expr.localVar "repaidAssetsOut"))),
    Stmt.letVar "newTotalSupplyAssets" (liqMem 0x340),
    -- Bad debt handling
    Stmt.letVar "badDebtShares" (Expr.literal 0),
    Stmt.letVar "badDebtAssets" (Expr.literal 0),
    Stmt.ite (Expr.eq (Expr.localVar "newBorrowerCollateral") (Expr.literal 0))
      [
        Stmt.assignVar "badDebtShares" (Expr.localVar "newBorrowerBorrowShares"),
        Stmt.ite (Expr.gt (Expr.localVar "badDebtShares") (Expr.literal 0))
          [
            Stmt.assignVar "badDebtAssets" (Expr.localVar "newTotalBorrowAssets"),
            Stmt.letVar "badDebtAssetsCandidate" (Expr.mulDivUp (Expr.localVar "badDebtShares") (Expr.add (Expr.localVar "newTotalBorrowAssets") (Expr.literal virtualAssets)) (Expr.add (Expr.localVar "newTotalBorrowShares") (Expr.literal virtualShares))),
            Stmt.ite (Expr.lt (Expr.localVar "badDebtAssetsCandidate") (Expr.localVar "badDebtAssets"))
              [Stmt.assignVar "badDebtAssets" (Expr.localVar "badDebtAssetsCandidate")]
              []
          ]
          [],
        Stmt.assignVar "newTotalBorrowAssets" (Expr.sub (Expr.localVar "newTotalBorrowAssets") (Expr.localVar "badDebtAssets")),
        Stmt.assignVar "newTotalSupplyAssets" (Expr.sub (Expr.localVar "newTotalSupplyAssets") (Expr.localVar "badDebtAssets")),
        Stmt.assignVar "newTotalBorrowShares" (Expr.sub (Expr.localVar "newTotalBorrowShares") (Expr.localVar "badDebtShares")),
        Stmt.assignVar "newBorrowerBorrowShares" (Expr.literal 0)
      ]
      [],
    -- Write state
    Stmt.setMapping2 "positionBorrowShares" (Expr.localVar "id") (Expr.param "borrower") (Expr.localVar "newBorrowerBorrowShares"),
    Stmt.setMapping2 "positionCollateral" (Expr.localVar "id") (Expr.param "borrower") (Expr.localVar "newBorrowerCollateral"),
    Stmt.setMappingUint "marketTotalBorrowShares" (Expr.localVar "id") (Expr.localVar "newTotalBorrowShares"),
    Stmt.setMappingUint "marketTotalBorrowAssets" (Expr.localVar "id") (Expr.localVar "newTotalBorrowAssets"),
    Stmt.setMappingUint "marketTotalSupplyAssets" (Expr.localVar "id") (Expr.localVar "newTotalSupplyAssets"),
    -- Emit Liquidate: log3(0, 192, topic, id, borrower)
    Stmt.mstore (Expr.literal 0) Expr.caller,
    Stmt.mstore (Expr.literal 32) (Expr.localVar "repaidAssetsOut"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "repaidSharesOut"),
    Stmt.mstore (Expr.literal 96) (Expr.localVar "seizedAssetsOut"),
    Stmt.mstore (Expr.literal 128) (Expr.localVar "badDebtAssets"),
    Stmt.mstore (Expr.literal 160) (Expr.localVar "badDebtShares"),
    Stmt.rawLog [Expr.literal liquidateEventTopic, Expr.localVar "id", Expr.param "borrower"] (Expr.literal 0) (Expr.literal 192),
    -- Transfer collateral to liquidator, callback, then transferFrom loan token
    ERC20.safeTransfer (Expr.localVar "collateralToken") Expr.caller (Expr.localVar "seizedAssetsOut"),
    Callbacks.callback Expr.caller onMorphoLiquidateSelector [Expr.localVar "repaidAssetsOut"] "data",
    ERC20.safeTransferFrom (Expr.localVar "loanToken") Expr.caller Expr.contractAddress (Expr.localVar "repaidAssetsOut"),
    -- Return
    Stmt.returnValues [Expr.localVar "seizedAssetsOut", Expr.localVar "repaidAssetsOut"]
  ]

/-- flashLoan(address,uint256,bytes) -/
private def flashLoanBody : List Stmt := [
    Stmt.require (Expr.gt (Expr.param "assets") (Expr.literal 0)) "zero assets",
    -- Emit FlashLoan: log3(0, 32, topic, caller, token)
    Stmt.mstore (Expr.literal 0) (Expr.param "assets"),
    Stmt.rawLog [Expr.literal flashLoanEventTopic, Expr.caller, Expr.param "token"] (Expr.literal 0) (Expr.literal 32),
    -- Transfer tokens to caller
    ERC20.safeTransfer (Expr.param "token") Expr.caller (Expr.param "assets"),
    -- Callback: onMorphoFlashLoan(assets, data)
    Callbacks.callback Expr.caller onMorphoFlashLoanSelector [Expr.param "assets"] "data",
    -- Transfer back
    ERC20.safeTransferFrom (Expr.param "token") Expr.caller Expr.contractAddress (Expr.param "assets"),
    Stmt.stop
  ]

/-- setAuthorizationWithSig((address,address,bool,uint256,uint256),(uint8,bytes32,bytes32)) -/
private def setAuthorizationWithSigBody : List Stmt := [
    Stmt.letVar "authorizer" (Expr.param "authorization_0"),
    Stmt.letVar "authorized" (Expr.param "authorization_1"),
    Stmt.letVar "newIsAuthorized" (Expr.param "authorization_2"),
    Stmt.letVar "expectedNonce" (Expr.param "authorization_3"),
    Stmt.letVar "deadline" (Expr.param "authorization_4"),
    Stmt.letVar "v" (Expr.param "signature_0"),
    Stmt.letVar "r" (Expr.param "signature_1"),
    Stmt.letVar "s" (Expr.param "signature_2"),
    -- Check deadline
    Stmt.require (Expr.ge (Expr.localVar "deadline") Expr.blockTimestamp) "signature expired",
    -- Check nonce
    Stmt.letVar "currentNonce" (Expr.mapping "nonce" (Expr.localVar "authorizer")),
    Stmt.require (Expr.eq (Expr.localVar "expectedNonce") (Expr.localVar "currentNonce")) "invalid nonce",
    -- Increment nonce
    Stmt.setMapping "nonce" (Expr.localVar "authorizer") (Expr.add (Expr.localVar "currentNonce") (Expr.literal 1)),
    -- Compute EIP-712 hash
    -- hashStruct = keccak256(AUTHORIZATION_TYPEHASH, authorizer, authorized, newIsAuthorized, nonce, deadline)
    Stmt.mstore (Expr.literal 0) (Expr.literal 0x81d0284fb0e2cde18d0553b06189d6f7613c96a01bb5b5e7828eade6a0dcac91),
    Stmt.mstore (Expr.literal 32) (Expr.localVar "authorizer"),
    Stmt.mstore (Expr.literal 64) (Expr.localVar "authorized"),
    Stmt.mstore (Expr.literal 96) (Expr.localVar "newIsAuthorized"),
    Stmt.mstore (Expr.literal 128) (Expr.localVar "expectedNonce"),
    Stmt.mstore (Expr.literal 160) (Expr.localVar "deadline"),
    Stmt.letVar "hashStruct" (Expr.keccak256 (Expr.literal 0) (Expr.literal 192)),
    -- domainSeparator = keccak256(EIP712_DOMAIN_TYPEHASH, chainid, address(this))
    Stmt.mstore (Expr.literal 32) (Expr.literal 0x47e79534a245952e8b16893a336b85a3d9ea9fa8c573f3d803afb92a79469218),
    Stmt.mstore (Expr.literal 64) Expr.chainid,
    Stmt.mstore (Expr.literal 96) Expr.contractAddress,
    Stmt.letVar "domainSeparator" (Expr.keccak256 (Expr.literal 32) (Expr.literal 96)),
    -- digest = keccak256("\x19\x01" || domainSeparator || hashStruct)
    Stmt.mstore (Expr.literal 0) (Expr.literal 0x1901000000000000000000000000000000000000000000000000000000000000),
    Stmt.mstore (Expr.literal 2) (Expr.localVar "domainSeparator"),
    Stmt.mstore (Expr.literal 34) (Expr.localVar "hashStruct"),
    Stmt.letVar "digest" (Expr.keccak256 (Expr.literal 0) (Expr.literal 66)),
    -- Ecrecover
    Precompiles.ecrecover "signatory" (Expr.localVar "digest") (Expr.localVar "v") (Expr.localVar "r") (Expr.localVar "s"),
    -- Validate signatory
    Stmt.require
      (Expr.logicalAnd
        (Expr.logicalNot (Expr.eq (Expr.localVar "signatory") (Expr.literal 0)))
        (Expr.eq (Expr.localVar "signatory") (Expr.localVar "authorizer")))
      "invalid signature",
    -- Emit IncrementNonce: log3(0, 32, topic, caller, authorizer)
    Stmt.mstore (Expr.literal 0) (Expr.localVar "currentNonce"),
    Stmt.rawLog [Expr.literal incrementNonceEventTopic, Expr.caller, Expr.localVar "authorizer"] (Expr.literal 0) (Expr.literal 32),
    -- Update authorization
    Stmt.setMapping2 "isAuthorized" (Expr.localVar "authorizer") (Expr.localVar "authorized") (Expr.localVar "newIsAuthorized"),
    -- Emit SetAuthorization: log4(0, 32, topic, caller, authorizer, authorized)
    Stmt.mstore (Expr.literal 0) (Expr.localVar "newIsAuthorized"),
    Stmt.rawLog [Expr.literal setAuthEventTopic, Expr.caller, Expr.localVar "authorizer", Expr.localVar "authorized"] (Expr.literal 0) (Expr.literal 32),
    Stmt.stop
  ]

-- ============================================================================
-- Contract Specification
-- ============================================================================

/--
Morpho Blue ContractSpec — full DSL specification covering all 26 external functions.

The 9 core operations (supply, withdraw, borrow, repay, supplyCollateral,
withdrawCollateral, liquidate, flashLoan, setAuthorizationWithSig) are now
expressed entirely in the ContractSpec DSL, using ECM modules for external
calls (ERC-20 transfers, callbacks, oracle price, ecrecover).

Notes:
- ABI selectors are set manually to match IMorpho tuple signatures.
- AccrueInterest logic is injected post-codegen by Main.lean since the DSL
  cannot express the full IRM call + Taylor series + fee distribution inline
  without internal function support for this specific pattern.
-/
def morphoSpec : ContractSpec := {
  name := "Morpho"
  fields := [
    { name := "owner", ty := .address },
    { name := "feeRecipient", ty := .address },
    { name := "isIrmEnabled", ty := .mappingTyped (.simple .address) },
    { name := "isLltvEnabled", ty := .mappingTyped (.simple .uint256) },
    { name := "isAuthorized", ty := .mappingTyped (.nested .address .address) },
    { name := "nonce", ty := .mappingTyped (.simple .address) },
    { name := "marketLastUpdate", ty := .mappingTyped (.simple .uint256) },
    { name := "marketFee", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalSupplyAssets", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalSupplyShares", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalBorrowAssets", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalBorrowShares", ty := .mappingTyped (.simple .uint256) },
    { name := "idToLoanToken", ty := .mappingTyped (.simple .uint256) },
    { name := "idToCollateralToken", ty := .mappingTyped (.simple .uint256) },
    { name := "idToOracle", ty := .mappingTyped (.simple .uint256) },
    { name := "idToIrm", ty := .mappingTyped (.simple .uint256) },
    { name := "idToLltv", ty := .mappingTyped (.simple .uint256) },
    { name := "positionSupplyShares", ty := .mappingTyped (.nested .uint256 .address) },
    { name := "positionBorrowShares", ty := .mappingTyped (.nested .uint256 .address) },
    { name := "positionCollateral", ty := .mappingTyped (.nested .uint256 .address) }
  ]
  constructor := some {
    params := [{ name := "initialOwner", ty := .address }]
    body := [
      Stmt.require
        (Expr.logicalNot (Expr.eq (Expr.constructorArg 0) (Expr.literal 0)))
        "zero address",
      Stmt.setStorage "owner" (Expr.constructorArg 0),
      Stmt.setStorage "feeRecipient" (Expr.literal 0),
      Stmt.emit "SetOwner" [Expr.constructorArg 0]
    ]
  }
  externals := [
    { name := "keccakMarketParams"
      params := [.address, .address, .address, .address, .uint256]
      returnType := some .uint256
      axiomNames := ["market_id_deterministic"] }
  ]
  functions := [
    -- ================================================================
    -- Views
    -- ================================================================
    {
      name := "DOMAIN_SEPARATOR"
      params := []
      returnType := some .uint256
      isView := true
      body := [
        Stmt.mstore (Expr.literal 0) (Expr.literal 0x47e79534a245952e8b16893a336b85a3d9ea9fa8c573f3d803afb92a79469218),
        Stmt.mstore (Expr.literal 32) Expr.chainid,
        Stmt.mstore (Expr.literal 64) Expr.contractAddress,
        Stmt.return (Expr.keccak256 (Expr.literal 0) (Expr.literal 96))
      ]
    },
    {
      name := "owner"
      params := []
      returnType := some .address
      body := [Stmt.return (Expr.storage "owner")]
    },
    {
      name := "feeRecipient"
      params := []
      returnType := some .address
      body := [Stmt.return (Expr.storage "feeRecipient")]
    },
    {
      name := "isIrmEnabled"
      params := [{ name := "irm", ty := .address }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping "isIrmEnabled" (Expr.param "irm"))]
    },
    {
      name := "isLltvEnabled"
      params := [{ name := "lltv", ty := .uint256 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv"))]
    },
    {
      name := "isAuthorized"
      params := [
        { name := "authorizer", ty := .address },
        { name := "authorized", ty := .address }
      ]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping2 "isAuthorized" (Expr.param "authorizer") (Expr.param "authorized"))]
    },
    {
      name := "nonce"
      params := [{ name := "authorizer", ty := .address }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping "nonce" (Expr.param "authorizer"))]
    },
    {
      name := "lastUpdate"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketLastUpdate" (Expr.param "id"))]
    },
    {
      name := "totalSupplyAssets"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalSupplyAssets" (Expr.param "id"))]
    },
    {
      name := "totalSupplyShares"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalSupplyShares" (Expr.param "id"))]
    },
    {
      name := "totalBorrowAssets"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalBorrowAssets" (Expr.param "id"))]
    },
    {
      name := "totalBorrowShares"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalBorrowShares" (Expr.param "id"))]
    },
    {
      name := "fee"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketFee" (Expr.param "id"))]
    },
    {
      name := "idToMarketParams"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := none
      returns := [.address, .address, .address, .address, .uint256]
      body := [
        Stmt.letVar "loanToken" (Expr.mappingUint "idToLoanToken" (Expr.param "id")),
        Stmt.letVar "collateralToken" (Expr.mappingUint "idToCollateralToken" (Expr.param "id")),
        Stmt.letVar "oracle" (Expr.mappingUint "idToOracle" (Expr.param "id")),
        Stmt.letVar "irm" (Expr.mappingUint "idToIrm" (Expr.param "id")),
        Stmt.letVar "lltv" (Expr.mappingUint "idToLltv" (Expr.param "id")),
        Stmt.returnValues [
          Expr.localVar "loanToken",
          Expr.localVar "collateralToken",
          Expr.localVar "oracle",
          Expr.localVar "irm",
          Expr.localVar "lltv"
        ]
      ]
    },
    {
      name := "position"
      params := [
        { name := "id", ty := .bytes32 },
        { name := "user", ty := .address }
      ]
      returnType := none
      returns := [.uint256, .uint256, .uint256]
      body := [
        Stmt.returnValues [
          Expr.mapping2 "positionSupplyShares" (Expr.param "id") (Expr.param "user"),
          Expr.mapping2 "positionBorrowShares" (Expr.param "id") (Expr.param "user"),
          Expr.mapping2 "positionCollateral" (Expr.param "id") (Expr.param "user")
        ]
      ]
    },
    {
      name := "market"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := none
      returns := [.uint256, .uint256, .uint256, .uint256, .uint256, .uint256]
      body := [
        Stmt.letVar "totalSupplyAssets" (Expr.mappingUint "marketTotalSupplyAssets" (Expr.param "id")),
        Stmt.letVar "totalSupplyShares" (Expr.mappingUint "marketTotalSupplyShares" (Expr.param "id")),
        Stmt.letVar "totalBorrowAssets" (Expr.mappingUint "marketTotalBorrowAssets" (Expr.param "id")),
        Stmt.letVar "totalBorrowShares" (Expr.mappingUint "marketTotalBorrowShares" (Expr.param "id")),
        Stmt.letVar "lastUpdate" (Expr.mappingUint "marketLastUpdate" (Expr.param "id")),
        Stmt.letVar "fee" (Expr.mappingUint "marketFee" (Expr.param "id")),
        Stmt.returnValues [
          Expr.localVar "totalSupplyAssets",
          Expr.localVar "totalSupplyShares",
          Expr.localVar "totalBorrowAssets",
          Expr.localVar "totalBorrowShares",
          Expr.localVar "lastUpdate",
          Expr.localVar "fee"
        ]
      ]
    },
    {
      name := "extSloads"
      params := [{ name := "slots", ty := .array .bytes32 }]
      returnType := none
      returns := [.array .uint256]
      body := [Stmt.returnStorageWords "slots"]
    },

    -- ================================================================
    -- Owner/configuration
    -- ================================================================
    {
      name := "setOwner"
      params := [{ name := "newOwner", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.logicalNot (Expr.eq (Expr.param "newOwner") (Expr.storage "owner")))
          "already set",
        Stmt.setStorage "owner" (Expr.param "newOwner"),
        Stmt.emit "SetOwner" [Expr.param "newOwner"],
        Stmt.stop
      ]
    },
    {
      name := "enableIrm"
      params := [{ name := "irm", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.eq (Expr.mapping "isIrmEnabled" (Expr.param "irm")) (Expr.literal 0))
          "already set",
        Stmt.setMapping "isIrmEnabled" (Expr.param "irm") (Expr.literal 1),
        Stmt.emit "EnableIrm" [Expr.param "irm"],
        Stmt.stop
      ]
    },
    {
      name := "enableLltv"
      params := [{ name := "lltv", ty := .uint256 }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.eq (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv")) (Expr.literal 0))
          "already set",
        Stmt.require (Expr.lt (Expr.param "lltv") (Expr.literal wad)) "max LLTV exceeded",
        Stmt.setMappingUint "isLltvEnabled" (Expr.param "lltv") (Expr.literal 1),
        Stmt.emit "EnableLltv" [Expr.param "lltv"],
        Stmt.stop
      ]
    },
    {
      name := "setFeeRecipient"
      params := [{ name := "newFeeRecipient", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.logicalNot (Expr.eq (Expr.param "newFeeRecipient") (Expr.storage "feeRecipient")))
          "already set",
        Stmt.setStorage "feeRecipient" (Expr.param "newFeeRecipient"),
        Stmt.emit "SetFeeRecipient" [Expr.param "newFeeRecipient"],
        Stmt.stop
      ]
    },
    {
      name := "setAuthorization"
      params := [
        { name := "authorized", ty := .address },
        { name := "newIsAuthorized", ty := .bool }
      ]
      returnType := none
      body := [
        Stmt.require
          (Expr.logicalNot (Expr.eq
            (Expr.mapping2 "isAuthorized" Expr.caller (Expr.param "authorized"))
            (Expr.param "newIsAuthorized")))
          "already set",
        Stmt.setMapping2 "isAuthorized" Expr.caller (Expr.param "authorized") (Expr.param "newIsAuthorized"),
        Stmt.emit "SetAuthorization" [
          Expr.caller,
          Expr.caller,
          Expr.param "authorized",
          Expr.param "newIsAuthorized"
        ],
        Stmt.stop
      ]
    },

    -- ================================================================
    -- Market creation/config
    -- ================================================================
    {
      name := "createMarket"
      params := [
        { name := "marketParams", ty := marketParamsTy }
      ]
      returnType := none
      body := [
        Stmt.letVar "loanToken" (Expr.param "marketParams_0"),
        Stmt.letVar "collateralToken" (Expr.param "marketParams_1"),
        Stmt.letVar "oracle" (Expr.param "marketParams_2"),
        Stmt.letVar "irm" (Expr.param "marketParams_3"),
        Stmt.letVar "lltv" (Expr.param "marketParams_4"),
        Stmt.letVar "id" (marketIdFromTupleParam "marketParams"),
        Stmt.require (Expr.eq (Expr.mapping "isIrmEnabled" (Expr.localVar "irm")) (Expr.literal 1)) "IRM not enabled",
        Stmt.require (Expr.eq (Expr.mappingUint "isLltvEnabled" (Expr.localVar "lltv")) (Expr.literal 1)) "LLTV not enabled",
        Stmt.require (Expr.eq (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0)) "market already created",
        Stmt.setMappingUint "marketLastUpdate" (Expr.localVar "id") Expr.blockTimestamp,
        Stmt.setMappingUint "marketFee" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalSupplyAssets" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalSupplyShares" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalBorrowAssets" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalBorrowShares" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "idToLoanToken" (Expr.localVar "id") (Expr.localVar "loanToken"),
        Stmt.setMappingUint "idToCollateralToken" (Expr.localVar "id") (Expr.localVar "collateralToken"),
        Stmt.setMappingUint "idToOracle" (Expr.localVar "id") (Expr.localVar "oracle"),
        Stmt.setMappingUint "idToIrm" (Expr.localVar "id") (Expr.localVar "irm"),
        Stmt.setMappingUint "idToLltv" (Expr.localVar "id") (Expr.localVar "lltv"),
        Stmt.emit "CreateMarket" [
          Expr.localVar "id",
          Expr.param "marketParams"
        ],
        Stmt.stop
      ]
    },
    {
      name := "setFee"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "newFee", ty := .uint256 }
      ]
      returnType := none
      body := [
        requireOwner,
        Stmt.letVar "loanToken" (Expr.param "marketParams_0"),
        Stmt.letVar "collateralToken" (Expr.param "marketParams_1"),
        Stmt.letVar "oracle" (Expr.param "marketParams_2"),
        Stmt.letVar "irm" (Expr.param "marketParams_3"),
        Stmt.letVar "lltv" (Expr.param "marketParams_4"),
        Stmt.letVar "id" (marketIdFromTupleParam "marketParams"),
        Stmt.require
          (Expr.gt (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0))
          "market not created",
        Stmt.require (Expr.le (Expr.param "newFee") (Expr.literal maxFee)) "max fee exceeded",
        Stmt.setMappingUint "marketFee" (Expr.localVar "id") (Expr.param "newFee"),
        Stmt.emit "SetFee" [Expr.localVar "id", Expr.param "newFee"],
        Stmt.stop
      ]
    },
    {
      name := "accrueInterest"
      params := [
        { name := "marketParams", ty := marketParamsTy }
      ]
      returnType := none
      body := [
        Stmt.letVar "loanToken" (Expr.param "marketParams_0"),
        Stmt.letVar "collateralToken" (Expr.param "marketParams_1"),
        Stmt.letVar "oracle" (Expr.param "marketParams_2"),
        Stmt.letVar "irm" (Expr.param "marketParams_3"),
        Stmt.letVar "lltv" (Expr.param "marketParams_4"),
        Stmt.letVar "id" (marketIdFromTupleParam "marketParams"),
        Stmt.require
          (Expr.gt (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0))
          "market not created",
        Stmt.ite
          (Expr.gt Expr.blockTimestamp (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")))
          [
            Stmt.setMappingUint "marketLastUpdate" (Expr.localVar "id") Expr.blockTimestamp,
            Stmt.ite
              (Expr.logicalNot (Expr.eq (Expr.localVar "irm") (Expr.literal 0)))
              [Stmt.emit "AccrueInterest" [Expr.localVar "id", Expr.literal 0, Expr.literal 0, Expr.literal 0]]
              [],
            Stmt.stop
          ]
          [Stmt.stop]
      ]
    },

    -- ================================================================
    -- Core operations (NEW — DSL-native)
    -- ================================================================
    {
      name := "supply"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "shares", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "data", ty := .bytes }
      ]
      returnType := none
      returns := [.uint256, .uint256]
      body := supplyBody
    },
    {
      name := "withdraw"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "shares", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "receiver", ty := .address }
      ]
      returnType := none
      returns := [.uint256, .uint256]
      body := withdrawBody
    },
    {
      name := "supplyCollateral"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "data", ty := .bytes }
      ]
      returnType := none
      body := supplyCollateralBody
    },
    {
      name := "withdrawCollateral"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "receiver", ty := .address }
      ]
      returnType := none
      body := withdrawCollateralBody
    },
    {
      name := "borrow"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "shares", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "receiver", ty := .address }
      ]
      returnType := none
      returns := [.uint256, .uint256]
      body := borrowBody
    },
    {
      name := "repay"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "assets", ty := .uint256 },
        { name := "shares", ty := .uint256 },
        { name := "onBehalf", ty := .address },
        { name := "data", ty := .bytes }
      ]
      returnType := none
      returns := [.uint256, .uint256]
      body := repayBody
    },
    {
      name := "liquidate"
      params := [
        { name := "marketParams", ty := marketParamsTy },
        { name := "borrower", ty := .address },
        { name := "seizedAssets", ty := .uint256 },
        { name := "repaidShares", ty := .uint256 },
        { name := "data", ty := .bytes }
      ]
      returnType := none
      returns := [.uint256, .uint256]
      body := liquidateBody
    },
    {
      name := "flashLoan"
      params := [
        { name := "token", ty := .address },
        { name := "assets", ty := .uint256 },
        { name := "data", ty := .bytes }
      ]
      returnType := none
      body := flashLoanBody
    },
    {
      name := "setAuthorizationWithSig"
      params := [
        { name := "authorization", ty := .tuple [.address, .address, .bool, .uint256, .uint256] },
        { name := "signature", ty := .tuple [.uint256, .bytes32, .bytes32] }
      ]
      returnType := none
      body := setAuthorizationWithSigBody
    }
  ]
  events := [
    {
      name := "SetOwner"
      params := [
        { name := "newOwner", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "SetFee"
      params := [
        { name := "id", ty := .bytes32, kind := .indexed },
        { name := "newFee", ty := .uint256, kind := .unindexed }
      ]
    },
    {
      name := "SetFeeRecipient"
      params := [
        { name := "newFeeRecipient", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "EnableIrm"
      params := [
        { name := "irm", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "EnableLltv"
      params := [
        { name := "lltv", ty := .uint256, kind := .unindexed }
      ]
    },
    {
      name := "CreateMarket"
      params := [
        { name := "id", ty := .bytes32, kind := .indexed },
        { name := "marketParams", ty := .tuple [.address, .address, .address, .address, .uint256], kind := .unindexed }
      ]
    },
    {
      name := "AccrueInterest"
      params := [
        { name := "id", ty := .bytes32, kind := .indexed },
        { name := "prevBorrowRate", ty := .uint256, kind := .unindexed },
        { name := "interest", ty := .uint256, kind := .unindexed },
        { name := "feeShares", ty := .uint256, kind := .unindexed }
      ]
    },
    {
      name := "SetAuthorization"
      params := [
        { name := "caller", ty := .address, kind := .indexed },
        { name := "authorizer", ty := .address, kind := .indexed },
        { name := "authorized", ty := .address, kind := .indexed },
        { name := "newIsAuthorized", ty := .bool, kind := .unindexed }
      ]
    }
  ]
}

/-- Function selectors aligned with IMorpho signatures. -/
def morphoSelectors : List Nat := [
  0x3644e515, -- DOMAIN_SEPARATOR()
  0x8da5cb5b, -- owner()
  0x46904840, -- feeRecipient()
  0xf2b863ce, -- isIrmEnabled(address)
  0xb485f3b8, -- isLltvEnabled(uint256)
  0x65e4ad9e, -- isAuthorized(address,address)
  0x70ae92d2, -- nonce(address)
  0x0acefb7b, -- lastUpdate(bytes32)
  0x417737d6, -- totalSupplyAssets(bytes32)
  0x0d2db074, -- totalSupplyShares(bytes32)
  0x12022eb9, -- totalBorrowAssets(bytes32)
  0x3f13c692, -- totalBorrowShares(bytes32)
  0x27cdab06, -- fee(bytes32)
  0x2c3c9157, -- idToMarketParams(bytes32)
  0x93c52062, -- position(bytes32,address)
  0x5c60e39a, -- market(bytes32)
  0x7784c685, -- extSloads(bytes32[])
  0x13af4035, -- setOwner(address)
  0x5a64f51e, -- enableIrm(address)
  0x4d98a93b, -- enableLltv(uint256)
  0xe74b981b, -- setFeeRecipient(address)
  0xeecea000, -- setAuthorization(address,bool)
  0x8c1358a2, -- createMarket((address,address,address,address,uint256))
  0x2b4f013c, -- setFee((address,address,address,address,uint256),uint256)
  0x151c1ade, -- accrueInterest((address,address,address,address,uint256))
  0xa99aad89, -- supply((address,address,address,address,uint256),uint256,uint256,address,bytes)
  0x5c2bea49, -- withdraw((address,address,address,address,uint256),uint256,uint256,address,address)
  0x238d6579, -- supplyCollateral((address,address,address,address,uint256),uint256,address,bytes)
  0x8720316d, -- withdrawCollateral((address,address,address,address,uint256),uint256,address,address)
  0x50d8cd4b, -- borrow((address,address,address,address,uint256),uint256,uint256,address,address)
  0x20b76e81, -- repay((address,address,address,address,uint256),uint256,uint256,address,bytes)
  0xd8eabcb8, -- liquidate((address,address,address,address,uint256),address,uint256,uint256,bytes)
  0xe0232b42, -- flashLoan(address,uint256,bytes)
  0x8069218f  -- setAuthorizationWithSig((address,address,bool,uint256,uint256),(uint8,bytes32,bytes32))
]

end Morpho.Compiler.Spec
