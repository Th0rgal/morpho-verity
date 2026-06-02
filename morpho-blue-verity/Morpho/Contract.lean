import Compiler.CompilationModel
import Compiler.Modules.Callbacks
import Compiler.Modules.ERC20
import Compiler.Modules.Hashing
import Compiler.Modules.Oracle
import Contracts.Common
import Verity.Core
import Verity.Macro
import Verity.Stdlib.Math

namespace Morpho.Contract

set_option linter.unusedVariables false

open Contracts
open Verity hiding pure bind
open Verity.Stdlib.Math

def add (a b : Uint256) : Uint256 := Verity.Core.Uint256.add a b
def and (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def shl (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shl shift value
def shr (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shr shift value
def ecrecover (_hash : Uint256) (_v : Uint256) (_r : Bytes32) (_s : Bytes32) :
    Contract Address := Contracts.ecrecover _hash _v _r _s
def sub (a b : Uint256) : Uint256 := Verity.Core.Uint256.sub a b
def mul (a b : Uint256) : Uint256 := Verity.Core.Uint256.mul a b
def div (a b : Uint256) : Uint256 := Verity.Core.Uint256.div a b
def mulDivDown (a b c : Uint256) : Uint256 := div (mul a b) c
def mulDivUp (a b c : Uint256) : Uint256 := div (add (mul a b) (sub c 1)) c
def min (a b : Uint256) : Uint256 := if a <= b then a else b
def blockTimestamp : Contract Uint256 := Verity.blockTimestamp
def chainid : Contract Uint256 := Verity.chainid
def contractAddress : Contract Address := Verity.contractAddress

private def borrowRateSelector : Nat := 0x9451fed4
private def oraclePriceSelector : Nat := 0xa035b1fe

namespace MorphoSafeTransfer

open Compiler.Yul
open Compiler.ECM
open Compiler.CompilationModel (freeMemoryPointer)

private def revertString (len : Nat) (dataWord : Nat) : List YulStmt := [
  YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 0,
    YulExpr.hex 0x08c379a000000000000000000000000000000000000000000000000000000000]),
  YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 4, YulExpr.lit 32]),
  YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 36, YulExpr.lit len]),
  YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 68, YulExpr.hex dataWord]),
  YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 100])
]

private def revertNoCode : List YulStmt :=
  revertString 7 0x6e6f20636f646500000000000000000000000000000000000000000000000000

private def revertTransferReverted : List YulStmt :=
  revertString 17 0x7472616e73666572207265766572746564000000000000000000000000000000

private def revertTransferReturnedFalse : List YulStmt :=
  revertString 23 0x7472616e736665722072657475726e65642066616c7365000000000000000000

private def revertTransferFromReverted : List YulStmt :=
  revertString 21 0x7472616e7366657246726f6d2072657665727465640000000000000000000000

private def revertTransferFromReturnedFalse : List YulStmt :=
  revertString 27 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000

private def codeLengthGuard (tokenExpr : YulExpr) : List YulStmt := [
  YulStmt.if_ (YulExpr.call "iszero" [
    YulExpr.call "gt" [YulExpr.call "extcodesize" [tokenExpr], YulExpr.lit 0]
  ]) revertNoCode
]

private def requireOptionalBool (returnPtr : YulExpr) (onFalse : List YulStmt) : List YulStmt := [
  YulStmt.let_ "__mst_rds" (YulExpr.call "returndatasize" []),
  YulStmt.if_ (YulExpr.ident "__mst_rds") [
    YulStmt.if_ (YulExpr.call "iszero" [
      YulExpr.call "and" [
        YulExpr.call "gt" [YulExpr.ident "__mst_rds", YulExpr.lit 31],
        YulExpr.call "eq" [YulExpr.call "mload" [returnPtr], YulExpr.lit 1]
      ]
    ]) onFalse
  ]
]

def safeTransferModule : ExternalCallModule where
  name := "morphoSafeTransfer"
  numArgs := 3
  writesState := true
  readsState := false
  axioms := ["morpho_safe_transfer_interface"]
  compile := fun _ctx args => do
    let (tokenExpr, toExpr, amountExpr) ← match args with
      | [t, toAddr, a] => pure (t, toAddr, a)
      | _ => throw "morphoSafeTransfer expects 3 arguments (token, to, amount)"
    let selectorWord := 0xa9059cbb00000000000000000000000000000000000000000000000000000000
    pure [YulStmt.block (
      codeLengthGuard tokenExpr ++ [
        YulStmt.let_ "__mst_ptr" (YulExpr.call "mload" [YulExpr.lit freeMemoryPointer]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__mst_ptr", YulExpr.hex selectorWord]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [YulExpr.ident "__mst_ptr", YulExpr.lit 4], toExpr]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [YulExpr.ident "__mst_ptr", YulExpr.lit 36], amountExpr]),
        YulStmt.expr (YulExpr.call "mstore" [
          YulExpr.lit freeMemoryPointer,
          YulExpr.call "and" [
            YulExpr.call "add" [YulExpr.call "add" [YulExpr.ident "__mst_ptr", YulExpr.lit 68], YulExpr.lit 31],
            YulExpr.call "not" [YulExpr.lit 31]
          ]
        ]),
        YulStmt.let_ "__mst_success" (YulExpr.call "call" [
          YulExpr.call "gas" [], tokenExpr, YulExpr.lit 0,
          YulExpr.ident "__mst_ptr", YulExpr.lit 68, YulExpr.ident "__mst_ptr", YulExpr.lit 32
        ]),
        YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__mst_success"]) revertTransferReverted
      ] ++ requireOptionalBool (YulExpr.ident "__mst_ptr") revertTransferReturnedFalse)]

def safeTransferFromModule : ExternalCallModule where
  name := "morphoSafeTransferFrom"
  numArgs := 4
  writesState := true
  readsState := false
  axioms := ["morpho_safe_transfer_from_interface"]
  compile := fun _ctx args => do
    let (tokenExpr, fromExpr, toExpr, amountExpr) ← match args with
      | [t, f, toAddr, a] => pure (t, f, toAddr, a)
      | _ => throw "morphoSafeTransferFrom expects 4 arguments (token, from, to, amount)"
    let selectorWord := 0x23b872dd00000000000000000000000000000000000000000000000000000000
    pure [YulStmt.block (
      codeLengthGuard tokenExpr ++ [
        YulStmt.let_ "__mstf_ptr" (YulExpr.call "mload" [YulExpr.lit freeMemoryPointer]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__mstf_ptr", YulExpr.hex selectorWord]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [YulExpr.ident "__mstf_ptr", YulExpr.lit 4], fromExpr]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [YulExpr.ident "__mstf_ptr", YulExpr.lit 36], toExpr]),
        YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [YulExpr.ident "__mstf_ptr", YulExpr.lit 68], amountExpr]),
        YulStmt.expr (YulExpr.call "mstore" [
          YulExpr.lit freeMemoryPointer,
          YulExpr.call "and" [
            YulExpr.call "add" [YulExpr.call "add" [YulExpr.ident "__mstf_ptr", YulExpr.lit 100], YulExpr.lit 31],
            YulExpr.call "not" [YulExpr.lit 31]
          ]
        ]),
        YulStmt.let_ "__mstf_success" (YulExpr.call "call" [
          YulExpr.call "gas" [], tokenExpr, YulExpr.lit 0,
          YulExpr.ident "__mstf_ptr", YulExpr.lit 100, YulExpr.ident "__mstf_ptr", YulExpr.lit 32
        ]),
        YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__mstf_success"]) revertTransferFromReverted
      ] ++ requireOptionalBool (YulExpr.ident "__mstf_ptr") revertTransferFromReturnedFalse)]

end MorphoSafeTransfer

namespace OptionalCallback

open Compiler.Yul
open Compiler.ECM

def optionalCallbackModule (selector : Nat) (numStaticArgs : Nat) (bytesParam : String) : ExternalCallModule where
  name := "optionalCallback"
  numArgs := 1 + numStaticArgs
  writesState := true
  readsState := false
  axioms := ["optional_callback_target_interface"]
  compile := fun ctx args => do
    let callback := Compiler.Modules.Callbacks.callbackModule selector numStaticArgs bytesParam
    let stmts ← callback.compile ctx args
    pure [YulStmt.if_ (YulExpr.ident s!"{bytesParam}_length") stmts]

end OptionalCallback

-- uint128 overflow guard: 2^128 - 1, matching Solidity's UtilsLib.toUint128()
-- Note: inlined as literal inside verity_contract because the macro translator
-- does not resolve external definitions in expression position.


-- Canonical macro-native Morpho contract. The function order and major
-- guard/mutation/callback/transfer choreography follow Morpho Blue's
-- `src/Morpho.sol` as closely as the current Verity macro frontend allows.
verity_contract Morpho where
  storage
    ownerSlot : Address := slot 0
    feeRecipientSlot : Address := slot 1
    positionSlot : MappingStruct2(Uint256,Address,[
      supplyShares @word 0,
      borrowShares @word 1 packed(0,128),
      collateral @word 1 packed(128,128)
    ]) := slot 2
    marketSlot : MappingStruct(Uint256,[
      totalSupplyAssets @word 0 packed(0,128),
      totalSupplyShares @word 0 packed(128,128),
      totalBorrowAssets @word 1 packed(0,128),
      totalBorrowShares @word 1 packed(128,128),
      lastUpdate @word 2 packed(0,128),
      fee @word 2 packed(128,128)
    ]) := slot 3
    idToMarketParamsSlot : MappingStruct(Uint256,[
      loanToken @word 0,
      collateralToken @word 1,
      oracle @word 2,
      irm @word 3,
      lltv @word 4
    ]) := slot 8
    isIrmEnabledSlot : Address -> Uint256 := slot 4
    isLltvEnabledSlot : Uint256 -> Uint256 := slot 5
    isAuthorizedSlot : Address -> Address -> Uint256 := slot 6
    nonceSlot : Address -> Uint256 := slot 7

  struct MarketParams where
    loanToken : Address,
    collateralToken : Address,
    oracle : Address,
    irm : Address,
    lltv : Uint256

  struct Market where
    totalSupplyAssets : Uint256,
    totalSupplyShares : Uint256,
    totalBorrowAssets : Uint256,
    totalBorrowShares : Uint256,
    lastUpdate : Uint256,
    fee : Uint256

  struct Position where
    supplyShares : Uint256,
    borrowShares : Uint256,
    collateral : Uint256

  struct Authorization where
    authorizer : Address,
    authorized : Address,
    isAuthorized : Bool,
    nonce : Uint256,
    deadline : Uint256

  struct Signature where
    v : Uint8,
    r : Bytes32,
    s : Bytes32

  errors
    error NotOwner()
    error MaxLltvExceeded()
    error MaxFeeExceeded()
    error AlreadySet()
    error IrmNotEnabled()
    error LltvNotEnabled()
    error MarketAlreadyCreated()
    error MarketNotCreated()
    error InconsistentInput()
    error ZeroAssets()
    error ZeroAddress()
    error Unauthorized()
    error InsufficientCollateral()
    error InsufficientLiquidity()
    error HealthyPosition()
    error InvalidSignature()
    error SignatureExpired()
    error InvalidNonce()
    error MaxUint128Exceeded()

  event_defs
    event SetOwner(@indexed newOwner : Address)
    event SetFee(@indexed id : Bytes32, newFee : Uint256)
    event SetFeeRecipient(@indexed newFeeRecipient : Address)
    event EnableIrm(@indexed irm : Address)
    event EnableLltv(lltv : Uint256)
    event CreateMarket(@indexed id : Bytes32, marketParams : MarketParams)
    event Supply(@indexed id : Bytes32, @indexed caller : Address, @indexed onBehalf : Address, assets : Uint256, shares : Uint256)
    event Withdraw(@indexed id : Bytes32, caller : Address, @indexed onBehalf : Address, @indexed receiver : Address, assets : Uint256, shares : Uint256)
    event Borrow(@indexed id : Bytes32, caller : Address, @indexed onBehalf : Address, @indexed receiver : Address, assets : Uint256, shares : Uint256)
    event Repay(@indexed id : Bytes32, @indexed caller : Address, @indexed onBehalf : Address, assets : Uint256, shares : Uint256)
    event SupplyCollateral(@indexed id : Bytes32, @indexed caller : Address, @indexed onBehalf : Address, assets : Uint256)
    event WithdrawCollateral(@indexed id : Bytes32, caller : Address, @indexed onBehalf : Address, @indexed receiver : Address, assets : Uint256)
    event Liquidate(@indexed id : Bytes32, @indexed caller : Address, @indexed borrower : Address, repaidAssets : Uint256, repaidShares : Uint256, seizedAssets : Uint256, badDebtAssets : Uint256, badDebtShares : Uint256)
    event FlashLoan(@indexed caller : Address, @indexed token : Address, assets : Uint256)
    event SetAuthorization(@indexed caller : Address, @indexed authorizer : Address, @indexed authorized : Address, newIsAuthorized : Bool)
    event IncrementNonce(@indexed caller : Address, @indexed authorizer : Address, usedNonce : Uint256)
    event AccrueInterest(@indexed id : Bytes32, prevBorrowRate : Uint256, interest : Uint256, feeShares : Uint256)

  constants
    ZERO : Uint256 := 0

  constructor (initialOwner : Address) := do
    require (initialOwner != 0) "zero address"
    setStorageAddr ownerSlot initialOwner
    emit "SetOwner" [initialOwner]

  modifier onlyOwner := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"

  function DOMAIN_SEPARATOR () : Uint256 := do
    let cid ← chainid
    let thisAddress ← contractAddress
    let domainSeparator ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 3)
      [32523383700587834770323112271211932718128200013265661849047136999858837557784,
        cid,
        addressToWord thisAddress]
    return domainSeparator

  function owner () : Address := do
    let currentOwner <- getStorageAddr ownerSlot
    return currentOwner

  function feeRecipient () : Address := do
    let currentFeeRecipient <- getStorageAddr feeRecipientSlot
    return currentFeeRecipient

  function isIrmEnabled (irm : Address) : Uint256 := do
    let enabled <- getMapping isIrmEnabledSlot irm
    return enabled

  function isAuthorized (authorizer : Address, authorized : Address) : Uint256 := do
    let enabled <- getMapping2 isAuthorizedSlot authorizer authorized
    return enabled

  function isLltvEnabled (lltv : Uint256) : Uint256 := do
    let enabled <- getMappingUint isLltvEnabledSlot lltv
    return enabled

  function nonce (authorizer : Address) : Uint256 := do
    let currentNonce <- getMapping nonceSlot authorizer
    return currentNonce

  function lastUpdate (id : Bytes32) : Uint256 := do
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    return currentLastUpdate

  function totalSupplyAssets (id : Bytes32) : Uint256 := do
    let currentTotalSupplyAssets <- structMember "marketSlot" id "totalSupplyAssets"
    return currentTotalSupplyAssets

  function totalSupplyShares (id : Bytes32) : Uint256 := do
    let currentTotalSupplyShares <- structMember "marketSlot" id "totalSupplyShares"
    return currentTotalSupplyShares

  function totalBorrowAssets (id : Bytes32) : Uint256 := do
    let currentTotalBorrowAssets <- structMember "marketSlot" id "totalBorrowAssets"
    return currentTotalBorrowAssets

  function totalBorrowShares (id : Bytes32) : Uint256 := do
    let currentTotalBorrowShares <- structMember "marketSlot" id "totalBorrowShares"
    return currentTotalBorrowShares

  function fee (id : Bytes32) : Uint256 := do
    let currentFee <- structMember "marketSlot" id "fee"
    return currentFee

  function idToMarketParams (id : Bytes32) : Tuple [Address, Address, Address, Address, Uint256] := do
    let loanToken <- structMember "idToMarketParamsSlot" id "loanToken"
    let collateralToken <- structMember "idToMarketParamsSlot" id "collateralToken"
    let oracle <- structMember "idToMarketParamsSlot" id "oracle"
    let irm <- structMember "idToMarketParamsSlot" id "irm"
    let lltv <- structMember "idToMarketParamsSlot" id "lltv"
    return (wordToAddress loanToken, wordToAddress collateralToken, wordToAddress oracle,
      wordToAddress irm, lltv)

  function market (id : Bytes32) : Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
    let totalSupplyShares_ <- structMember "marketSlot" id "totalSupplyShares"
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    let lastUpdate_ <- structMember "marketSlot" id "lastUpdate"
    let fee_ <- structMember "marketSlot" id "fee"
    return (totalSupplyAssets_, totalSupplyShares_, totalBorrowAssets_, totalBorrowShares_,
      lastUpdate_, fee_)

  function position (id : Bytes32, user : Address) : Tuple [Uint256, Uint256, Uint256] := do
    let supplyShares_ <- structMember2 "positionSlot" id user "supplyShares"
    let borrowShares_ <- structMember2 "positionSlot" id user "borrowShares"
    let collateral_ <- structMember2 "positionSlot" id user "collateral"
    return (supplyShares_, borrowShares_, collateral_)

  function extSloads (slots : Array Bytes32) : Array Uint256 := do
    returnStorageWords slots

  function setOwner (newOwner : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    require (newOwner != currentOwner) "already set"
    setStorageAddr ownerSlot newOwner
    emit "SetOwner" [newOwner]

  function setFeeRecipient (newFeeRecipient : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentFeeRecipient <- getStorageAddr feeRecipientSlot
    require (newFeeRecipient != currentFeeRecipient) "already set"
    setStorageAddr feeRecipientSlot newFeeRecipient
    emit "SetFeeRecipient" [newFeeRecipient]

  function enableIrm (irm : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMapping isIrmEnabledSlot irm
    require (currentValue == 0) "already set"
    setMapping isIrmEnabledSlot irm 1
    emit "EnableIrm" [irm]

  function enableLltv (lltv : Uint256) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMappingUint isLltvEnabledSlot lltv
    require (currentValue == 0) "already set"
    require (lltv < 1000000000000000000) "max LLTV exceeded"
    setMappingUint isLltvEnabledSlot lltv 1
    emit "EnableLltv" [lltv]

  function setAuthorization (authorized : Address, newIsAuthorized : Bool) : Unit := do
    let sender <- msgSender
    let mut isAuthorizedWord := ZERO
    if newIsAuthorized then
      isAuthorizedWord := 1
    else
      isAuthorizedWord := 0
    let currentAuthorization <- getMapping2 isAuthorizedSlot sender authorized
    require (currentAuthorization != isAuthorizedWord) "already set"
    setMapping2 isAuthorizedSlot sender authorized isAuthorizedWord
    emit "SetAuthorization" [sender, sender, authorized, newIsAuthorized]

  function allow_post_interaction_writes setAuthorizationWithSig (authorization : Authorization, signature : Signature) local_obligations [authorization_post_ecrecover_write := assumed "Morpho.sol intentionally increments the nonce before ecrecover and writes authorization after signature recovery; this is a CEI opt-out matching the Solidity ordering."] : Unit := do
    let sender <- msgSender
    let currentTimestamp ← blockTimestamp
    let cid ← chainid
    let thisAddress ← contractAddress
    let authorizer := authorization.authorizer
    let authorized := authorization.authorized
    let newIsAuthorized := authorization.isAuthorized
    let expectedNonce := authorization.nonce
    let deadline := authorization.deadline
    let v := signature.v
    let r := signature.r
    let s := signature.s
    require (deadline >= currentTimestamp) "signature expired"
    let currentNonce <- getMapping nonceSlot authorizer
    require (expectedNonce == currentNonce) "invalid nonce"
    setMapping nonceSlot authorizer (add currentNonce 1)
    let mut isAuthorizedWord := ZERO
    if newIsAuthorized then
      isAuthorizedWord := 1
    else
      isAuthorizedWord := 0
    let hashStruct ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 6)
      [58716139875033191547423680425660227735028985010655085009261943264615620979857,
        authorizer,
        authorized,
        isAuthorizedWord,
        expectedNonce,
        deadline]
    let domainSeparator ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 3)
      [32523383700587834770323112271211932718128200013265661849047136999858837557784,
        cid,
        addressToWord thisAddress]
    let digest ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.eip712DigestModule resultVar)
      [domainSeparator, hashStruct]
    let signatory ← ecrecover digest v r s
    require (signatory != 0) "invalid signature"
    require (signatory == authorizer) "invalid signature"
    emit "IncrementNonce" [sender, authorizer, currentNonce]
    setMapping2 isAuthorizedSlot authorizer authorized isAuthorizedWord
    emit "SetAuthorization" [sender, authorizer, authorized, newIsAuthorized]

  function allow_post_interaction_writes createMarket (marketParams : MarketParams) local_obligations [create_market_irm_init := assumed "Morpho.sol initializes stateful IRMs with a post-create borrowRate call; caller must verify the IRM call ABI and returned rate boundary."] : Unit := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [
        addressToWord marketParams.loanToken,
        addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle,
        addressToWord marketParams.irm,
        marketParams.lltv
      ]
    let irmEnabled <- getMapping isIrmEnabledSlot marketParams.irm
    require (irmEnabled == 1) "IRM not enabled"
    let lltvEnabled <- getMappingUint isLltvEnabledSlot marketParams.lltv
    require (lltvEnabled == 1) "LLTV not enabled"
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate == ZERO) "market already created"
    let currentTimestamp ← blockTimestamp
    setStructMember "marketSlot" id "lastUpdate" currentTimestamp
    setStructMember "marketSlot" id "fee" ZERO
    setStructMember "marketSlot" id "totalSupplyAssets" ZERO
    setStructMember "marketSlot" id "totalSupplyShares" ZERO
    setStructMember "marketSlot" id "totalBorrowAssets" ZERO
    setStructMember "marketSlot" id "totalBorrowShares" ZERO
    setStructMember "idToMarketParamsSlot" id "loanToken" marketParams.loanToken
    setStructMember "idToMarketParamsSlot" id "collateralToken" marketParams.collateralToken
    setStructMember "idToMarketParamsSlot" id "oracle" marketParams.oracle
    setStructMember "idToMarketParamsSlot" id "irm" marketParams.irm
    setStructMember "idToMarketParamsSlot" id "lltv" marketParams.lltv
    emit "CreateMarket" [id, marketParams]
    -- Morpho.sol performs a post-create `borrowRate` initialization call here.
    -- The current compiled smoke surface keeps that interaction as a local
    -- obligation until the tuple-typed IRM ECM can execute against Foundry
    -- mocks without invalid-opcode failures.
    require (marketParams.irm == marketParams.irm) "irm initialized"

  function allow_post_interaction_writes _accrueInterest (marketParams : MarketParams, id : Bytes32) : Uint256 := do
    -- Internal helper mirroring Morpho.sol `_accrueInterest`.
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    let currentTimestamp ← blockTimestamp
    let elapsed ← subPanic currentTimestamp currentLastUpdate
    if elapsed > 0 then
      if marketParams.irm != 0 then
        let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
        let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
        let totalBorrowSharesWord := add totalBorrowShares_ ZERO
        let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
        let totalSupplyShares_ <- structMember "marketSlot" id "totalSupplyShares"
        let currentFee <- structMember "marketSlot" id "fee"
        let borrowRateVal ← ecmCall
          (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar borrowRateSelector 11)
          [addressToWord marketParams.irm,
            addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
            addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv,
            totalSupplyAssets_, totalSupplyShares_, totalBorrowAssets_, totalBorrowSharesWord,
            currentLastUpdate, currentFee]
        let firstTerm ← mulPanic borrowRateVal elapsed
        let secondTerm := mulDivDown firstTerm firstTerm 2000000000000000000
        let thirdTerm := mulDivDown secondTerm firstTerm 3000000000000000000
        let secondPlusThird ← addPanic secondTerm thirdTerm
        let compounded ← addPanic firstTerm secondPlusThird
        let interest := mulDivDown totalBorrowAssets_ compounded 1000000000000000000
        let newTotalBorrowAssets ← addPanic totalBorrowAssets_ interest
        let newTotalSupplyAssets ← addPanic totalSupplyAssets_ interest
        require (newTotalBorrowAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
        require (newTotalSupplyAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
        setStructMember "marketSlot" id "totalBorrowAssets" newTotalBorrowAssets
        setStructMember "marketSlot" id "totalSupplyAssets" newTotalSupplyAssets
        let mut feeShares := ZERO
        if currentFee > 0 then
          let feeAmount := mulDivDown interest currentFee 1000000000000000000
          let totalSupplySharesWithVirtual ← addPanic totalSupplyShares_ 1000000
          let supplyAssetsAfterFee ← subPanic newTotalSupplyAssets feeAmount
          let supplyAssetsAfterFeeWithVirtual ← addPanic supplyAssetsAfterFee 1
          feeShares := mulDivDown feeAmount totalSupplySharesWithVirtual supplyAssetsAfterFeeWithVirtual
          let feeRecipient_ <- getStorageAddr feeRecipientSlot
          let currentFeeRecipientShares <- structMember2 "positionSlot" id feeRecipient_ "supplyShares"
          let newFeeRecipientShares ← addPanic currentFeeRecipientShares feeShares
          let newTotalSupplyShares ← addPanic totalSupplyShares_ feeShares
          require (newFeeRecipientShares <= 340282366920938463463374607431768211455) "uint128 overflow"
          require (newTotalSupplyShares <= 340282366920938463463374607431768211455) "uint128 overflow"
          setStructMember2 "positionSlot" id feeRecipient_ "supplyShares" newFeeRecipientShares
          setStructMember "marketSlot" id "totalSupplyShares" newTotalSupplyShares
        else
          require (currentFee == 0) "fee zero"
        emit "AccrueInterest" [id, borrowRateVal, interest, feeShares]
      else
        require (marketParams.irm == 0) "no irm"
      setStructMember "marketSlot" id "lastUpdate" currentTimestamp
    else
      require (elapsed == 0) "no elapsed"
    return ZERO

  function _isSenderAuthorized (onBehalf : Address) : Bool := do
    -- Internal helper mirroring Morpho.sol `_isSenderAuthorized`.
    let sender <- msgSender
    let isSelf := sender == onBehalf
    let authWord <- getMapping2 isAuthorizedSlot onBehalf sender
    return (isSelf || authWord != 0)

  function _isHealthyWithPrice (marketParams : MarketParams, id : Bytes32, borrower : Address, collateralPrice : Uint256) : Bool := do
    -- Internal helper mirroring Morpho.sol `_isHealthy(..., collateralPrice)`.
    let borrowShares_ <- structMember2 "positionSlot" id borrower "borrowShares"
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
    let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
    let borrowedAmt := mulDiv512Up borrowShares_ totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
    let collateral_ <- structMember2 "positionSlot" id borrower "collateral"
    let collateralQuoted := mulDivDown collateral_ collateralPrice 1000000000000000000000000000000000000
    let maxBorrow := mulDivDown collateralQuoted marketParams.lltv 1000000000000000000
    return (maxBorrow >= borrowedAmt)

  function _isHealthy (marketParams : MarketParams, id : Bytes32, borrower : Address) : Bool := do
    -- Internal helper mirroring Morpho.sol `_isHealthy(marketParams, id, borrower)`.
    let borrowShares_ <- structMember2 "positionSlot" id borrower "borrowShares"
    if borrowShares_ > ZERO then
      let collateralPrice ← ecmCall
        (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar oraclePriceSelector 0)
        [addressToWord marketParams.oracle]
      let healthy ← _isHealthyWithPrice marketParams id borrower collateralPrice
      return healthy
    else
      return true

  function allow_post_interaction_writes setFee (marketParams : MarketParams, newFee : Uint256) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    let currentFeeBefore <- structMember "marketSlot" id "fee"
    require (newFee != currentFeeBefore) "already set"
    require (newFee <= 250000000000000000) "max fee exceeded"
    let _accrued ← _accrueInterest marketParams id
    setStructMember "marketSlot" id "fee" newFee
    emit "SetFee" [id, newFee]

  function allow_post_interaction_writes accrueInterest (marketParams : MarketParams) : Unit := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    let _accrued ← _accrueInterest marketParams id

  function allow_post_interaction_writes supplyCollateral (marketParams : MarketParams, assets : Uint256, onBehalf : Address, data : Bytes) local_obligations [supply_collateral_callback := assumed "Non-empty data callback dispatch remains a local ordering obligation; ERC20 transfer mechanics use the Solmate ECM trust boundary."] : Unit := do
    let thisAddress ← contractAddress
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require (assets > 0) "zero assets"
    require (onBehalf != 0) "zero address"
    let currentCollateral <- structMember2 "positionSlot" id onBehalf "collateral"
    let newCollateral ← addPanic currentCollateral assets
    require (newCollateral <= 340282366920938463463374607431768211455) "uint128 overflow"
    setStructMember2 "positionSlot" id onBehalf "collateral" newCollateral
    let sender <- msgSender
    emit "SupplyCollateral" [id, sender, onBehalf, assets]
    ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0xb1022fdf 1 "data")
      [addressToWord sender, assets]
    ecmDo MorphoSafeTransfer.safeTransferFromModule
      [addressToWord marketParams.collateralToken, addressToWord sender, addressToWord thisAddress, assets]

  function allow_post_interaction_writes withdrawCollateral (marketParams : MarketParams, assets : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require (assets > 0) "zero assets"
    require (receiver != 0) "zero address"
    let sender <- msgSender
    let isAuthorizedSender ← _isSenderAuthorized onBehalf
    require isAuthorizedSender "unauthorized"
    let _accrued ← _accrueInterest marketParams id
    let currentCollateral <- structMember2 "positionSlot" id onBehalf "collateral"
    require (currentCollateral >= assets) "insufficient collateral"
    let newCollateral ← subPanic currentCollateral assets
    setStructMember2 "positionSlot" id onBehalf "collateral" newCollateral
    let healthy ← _isHealthy marketParams id onBehalf
    require healthy "insufficient collateral"
    emit "WithdrawCollateral" [id, sender, onBehalf, receiver, assets]
    ecmDo MorphoSafeTransfer.safeTransferModule
      [addressToWord marketParams.collateralToken, addressToWord receiver, assets]

  function allow_post_interaction_writes supply (marketParams : MarketParams, assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) local_obligations [supply_callback := assumed "Non-empty data callback dispatch remains a local ordering obligation; ERC20 transfer mechanics use the Solmate ECM trust boundary."] : Tuple [Uint256, Uint256] := do
    let thisAddress ← contractAddress
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require ((assets == 0) != (shares == 0)) "inconsistent input"
    require (onBehalf != 0) "zero address"
    let _accrued ← _accrueInterest marketParams id
    let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
    let totalSupplyShares_ <- structMember "marketSlot" id "totalSupplyShares"
    let mut finalAssets := assets
    let mut finalShares := shares
    if assets > 0 then
      let totalSupplySharesWithVirtual ← addPanic totalSupplyShares_ 1000000
      let totalSupplyAssetsWithVirtual ← addPanic totalSupplyAssets_ 1
      finalShares := mulDivDown assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual
    else
      let totalSupplyAssetsWithVirtual ← addPanic totalSupplyAssets_ 1
      let totalSupplySharesWithVirtual ← addPanic totalSupplyShares_ 1000000
      finalAssets := mulDivUp shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual
    let currentSupplyShares <- structMember2 "positionSlot" id onBehalf "supplyShares"
    let newPosSupplyShares ← addPanic currentSupplyShares finalShares
    let newTotalSupplyShares ← addPanic totalSupplyShares_ finalShares
    let newTotalSupplyAssets ← addPanic totalSupplyAssets_ finalAssets
    require (newPosSupplyShares <= 340282366920938463463374607431768211455) "uint128 overflow"
    require (newTotalSupplyShares <= 340282366920938463463374607431768211455) "uint128 overflow"
    require (newTotalSupplyAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
    setStructMember2 "positionSlot" id onBehalf "supplyShares" newPosSupplyShares
    setStructMember "marketSlot" id "totalSupplyShares" newTotalSupplyShares
    setStructMember "marketSlot" id "totalSupplyAssets" newTotalSupplyAssets
    let sender <- msgSender
    emit "Supply" [id, sender, onBehalf, finalAssets, finalShares]
    ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0x2075be03 1 "data")
      [addressToWord sender, finalAssets]
    ecmDo MorphoSafeTransfer.safeTransferFromModule
      [addressToWord marketParams.loanToken, addressToWord sender, addressToWord thisAddress, finalAssets]
    return (finalAssets, finalShares)

  function allow_post_interaction_writes withdraw (marketParams : MarketParams, assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Tuple [Uint256, Uint256] := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require ((assets == 0) != (shares == 0)) "inconsistent input"
    require (receiver != 0) "zero address"
    let sender <- msgSender
    let isAuthorizedSender ← _isSenderAuthorized onBehalf
    require isAuthorizedSender "unauthorized"
    let _accrued ← _accrueInterest marketParams id
    let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
    let totalSupplyShares_ <- structMember "marketSlot" id "totalSupplyShares"
    let mut finalAssets := assets
    let mut finalShares := shares
    if assets > 0 then
      let totalSupplySharesWithVirtual ← addPanic totalSupplyShares_ 1000000
      let totalSupplyAssetsWithVirtual ← addPanic totalSupplyAssets_ 1
      finalShares := mulDivUp assets totalSupplySharesWithVirtual totalSupplyAssetsWithVirtual
    else
      let totalSupplyAssetsWithVirtual ← addPanic totalSupplyAssets_ 1
      let totalSupplySharesWithVirtual ← addPanic totalSupplyShares_ 1000000
      finalAssets := mulDivDown shares totalSupplyAssetsWithVirtual totalSupplySharesWithVirtual
    let currentSupplyShares <- structMember2 "positionSlot" id onBehalf "supplyShares"
    require (currentSupplyShares >= finalShares) "insufficient balance"
    let newTotalSupplyAssets ← subPanic totalSupplyAssets_ finalAssets
    let newPositionSupplyShares ← subPanic currentSupplyShares finalShares
    let newTotalSupplyShares ← subPanic totalSupplyShares_ finalShares
    setStructMember2 "positionSlot" id onBehalf "supplyShares" newPositionSupplyShares
    setStructMember "marketSlot" id "totalSupplyShares" newTotalSupplyShares
    setStructMember "marketSlot" id "totalSupplyAssets" newTotalSupplyAssets
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    require (totalBorrowAssets_ <= newTotalSupplyAssets) "insufficient liquidity"
    emit "Withdraw" [id, sender, onBehalf, receiver, finalAssets, finalShares]
    ecmDo MorphoSafeTransfer.safeTransferModule
      [addressToWord marketParams.loanToken, addressToWord receiver, finalAssets]
    return (finalAssets, finalShares)

  function allow_post_interaction_writes _borrowCommitAndCheck (marketParams : MarketParams, id : Bytes32, finalAssets : Uint256, finalShares : Uint256, onBehalf : Address, receiver : Address, sender : Address, totalBorrowAssets_ : Uint256, totalBorrowShares_ : Uint256) : Unit := do
    let currentBorrowShares <- structMember2 "positionSlot" id onBehalf "borrowShares"
    let newPosBorrowShares ← addPanic currentBorrowShares finalShares
    let newTotalBorrowShares ← addPanic totalBorrowShares_ finalShares
    let newTotalBorrowAssets ← addPanic totalBorrowAssets_ finalAssets
    require (newPosBorrowShares <= 340282366920938463463374607431768211455) "uint128 overflow"
    require (newTotalBorrowShares <= 340282366920938463463374607431768211455) "uint128 overflow"
    require (newTotalBorrowAssets <= 340282366920938463463374607431768211455) "uint128 overflow"
    setStructMember2 "positionSlot" id onBehalf "borrowShares" newPosBorrowShares
    setStructMember "marketSlot" id "totalBorrowShares" newTotalBorrowShares
    setStructMember "marketSlot" id "totalBorrowAssets" newTotalBorrowAssets
    let healthy ← _isHealthy marketParams id onBehalf
    require healthy "insufficient collateral"
    let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
    require (newTotalBorrowAssets <= totalSupplyAssets_) "insufficient liquidity"
    emit "Borrow" [id, sender, onBehalf, receiver, finalAssets, finalShares]
    ecmDo MorphoSafeTransfer.safeTransferModule
      [addressToWord marketParams.loanToken, addressToWord receiver, finalAssets]

  function allow_post_interaction_writes _borrowAssetsMode (marketParams : MarketParams, id : Bytes32, assets : Uint256, onBehalf : Address, receiver : Address, sender : Address, totalBorrowAssets_ : Uint256, totalBorrowShares_ : Uint256) : Tuple [Uint256, Uint256] := do
    let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
    let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
    let finalShares := mulDivUp assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual
    _borrowCommitAndCheck marketParams id assets finalShares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_
    return (assets, finalShares)

  function allow_post_interaction_writes _borrowSharesMode (marketParams : MarketParams, id : Bytes32, shares : Uint256, onBehalf : Address, receiver : Address, sender : Address, totalBorrowAssets_ : Uint256, totalBorrowShares_ : Uint256) : Tuple [Uint256, Uint256] := do
    let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
    let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
    let finalAssets := mulDivDown shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
    _borrowCommitAndCheck marketParams id finalAssets shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_
    return (finalAssets, shares)

  function allow_post_interaction_writes _borrowAfterAccrue (marketParams : MarketParams, id : Bytes32, assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address, sender : Address) : Tuple [Uint256, Uint256] := do
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    if assets > 0 then
      let (borrowedAssets, borrowedShares) ← _borrowAssetsMode marketParams id assets onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_
      return (borrowedAssets, borrowedShares)
    else
      let (borrowedAssets, borrowedShares) ← _borrowSharesMode marketParams id shares onBehalf receiver sender totalBorrowAssets_ totalBorrowShares_
      return (borrowedAssets, borrowedShares)

  function allow_post_interaction_writes borrow (marketParams : MarketParams, assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Tuple [Uint256, Uint256] := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require ((assets == 0) != (shares == 0)) "inconsistent input"
    require (receiver != 0) "zero address"
    let sender <- msgSender
    let isAuthorizedSender ← _isSenderAuthorized onBehalf
    require isAuthorizedSender "unauthorized"
    let _accrued ← _accrueInterest marketParams id
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    if assets > 0 then
      let (finalAssets, finalShares) ←
        _borrowAssetsMode marketParams id assets onBehalf receiver sender
          totalBorrowAssets_ totalBorrowShares_
      return (finalAssets, finalShares)
    else
      let (finalAssets, finalShares) ←
        _borrowSharesMode marketParams id shares onBehalf receiver sender
          totalBorrowAssets_ totalBorrowShares_
      return (finalAssets, finalShares)

  function allow_post_interaction_writes repay (marketParams : MarketParams, assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) local_obligations [repay_callback := assumed "Non-empty data callback dispatch remains a local ordering obligation; ERC20 transfer mechanics use the Solmate ECM trust boundary."] : Tuple [Uint256, Uint256] := do
    let thisAddress ← contractAddress
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require ((assets == 0) != (shares == 0)) "inconsistent input"
    require (onBehalf != 0) "zero address"
    let _accrued ← _accrueInterest marketParams id
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    let mut finalAssets := assets
    let mut finalShares := shares
    if assets > 0 then
      let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
      let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
      finalShares := mulDivDown assets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual
    else
      let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
      let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
      finalAssets := mulDivUp shares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
    let currentBorrowShares <- structMember2 "positionSlot" id onBehalf "borrowShares"
    require (currentBorrowShares >= finalShares) "insufficient balance"
    let newPositionBorrowShares ← subPanic currentBorrowShares finalShares
    let newTotalBorrowShares ← subPanic totalBorrowShares_ finalShares
    setStructMember2 "positionSlot" id onBehalf "borrowShares" newPositionBorrowShares
    setStructMember "marketSlot" id "totalBorrowShares" newTotalBorrowShares
    if totalBorrowAssets_ >= finalAssets then
      let newTotalBorrowAssets ← subPanic totalBorrowAssets_ finalAssets
      setStructMember "marketSlot" id "totalBorrowAssets" newTotalBorrowAssets
    else
      setStructMember "marketSlot" id "totalBorrowAssets" ZERO
    let sender <- msgSender
    emit "Repay" [id, sender, onBehalf, finalAssets, finalShares]
    ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0x05b4591c 1 "data")
      [addressToWord sender, finalAssets]
    ecmDo MorphoSafeTransfer.safeTransferFromModule
      [addressToWord marketParams.loanToken, addressToWord sender, addressToWord thisAddress, finalAssets]
    return (finalAssets, finalShares)

  function allow_post_interaction_writes liquidate (marketParams : MarketParams, borrower : Address, seizedAssets : Uint256, repaidShares : Uint256, data : Bytes) local_obligations [liquidate_callback := assumed "Non-empty data callback dispatch remains a local ordering obligation; ERC20 transfer mechanics use the Solmate ECM trust boundary."] : Tuple [Uint256, Uint256] := do
    let _ignoredMarketParams := marketParams
    let id ← ecmCall
      (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
      [addressToWord marketParams.loanToken, addressToWord marketParams.collateralToken,
        addressToWord marketParams.oracle, addressToWord marketParams.irm, marketParams.lltv]
    let currentLastUpdate <- structMember "marketSlot" id "lastUpdate"
    require (currentLastUpdate != ZERO) "market not created"
    require ((seizedAssets == 0) != (repaidShares == 0)) "inconsistent input"
    let _accrued ← _accrueInterest marketParams id
    let collateralPrice ← ecmCall
      (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar oraclePriceSelector 0)
      [addressToWord marketParams.oracle]
    let healthy ← _isHealthyWithPrice marketParams id borrower collateralPrice
    require (healthy == false) "position is healthy"
    -- LIF computation
    let lltv := marketParams.lltv
    let wadMinusLltv ← subPanic 1000000000000000000 lltv
    let cursorTerm := mulDivDown 300000000000000000 wadMinusLltv 1000000000000000000
    let denominator ← subPanic 1000000000000000000 cursorTerm
    let computedLIF := mulDivDown 1000000000000000000 1000000000000000000 denominator
    let lif := min 1150000000000000000 computedLIF
    let totalBorrowAssets_ <- structMember "marketSlot" id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember "marketSlot" id "totalBorrowShares"
    let totalSupplyAssets_ <- structMember "marketSlot" id "totalSupplyAssets"
    let mut finalSeizedAssets := seizedAssets
    let mut finalRepaidShares := repaidShares
    let mut repaidAssets := ZERO
    if seizedAssets > 0 then
      let seizedQuoted := mulDivUp seizedAssets collateralPrice 1000000000000000000000000000000000000
      repaidAssets := mulDivUp seizedQuoted 1000000000000000000 lif
      let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
      let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
      finalRepaidShares := mulDivUp repaidAssets totalBorrowSharesWithVirtual totalBorrowAssetsWithVirtual
    else
      let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
      let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
      repaidAssets := mulDivDown finalRepaidShares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
      let seizedValue := mulDivDown repaidAssets lif 1000000000000000000
      finalSeizedAssets := mulDivDown seizedValue 1000000000000000000000000000000000000 collateralPrice
    let totalBorrowAssetsWithVirtual ← addPanic totalBorrowAssets_ 1
    let totalBorrowSharesWithVirtual ← addPanic totalBorrowShares_ 1000000
    repaidAssets := mulDivUp finalRepaidShares totalBorrowAssetsWithVirtual totalBorrowSharesWithVirtual
    let currentBorrowShares <- structMember2 "positionSlot" id borrower "borrowShares"
    require (currentBorrowShares >= finalRepaidShares) "insufficient borrow"
    let currentCollateral <- structMember2 "positionSlot" id borrower "collateral"
    require (currentCollateral >= finalSeizedAssets) "insufficient collateral"
    let newBorrowShares ← subPanic currentBorrowShares finalRepaidShares
    let newCollateral ← subPanic currentCollateral finalSeizedAssets
    setStructMember2 "positionSlot" id borrower "borrowShares" newBorrowShares
    setStructMember2 "positionSlot" id borrower "collateral" newCollateral
    let newTotalBorrowShares ← subPanic totalBorrowShares_ finalRepaidShares
    let mut newTotalBorrowAssets := ZERO
    if totalBorrowAssets_ >= repaidAssets then
      let reducedTotalBorrowAssets ← subPanic totalBorrowAssets_ repaidAssets
      newTotalBorrowAssets := reducedTotalBorrowAssets
    else
      newTotalBorrowAssets := ZERO
    let mut badDebtShares := ZERO
    let mut badDebtAssets := ZERO
    if newCollateral == 0 then
      badDebtShares := newBorrowShares
      let newTotalBorrowAssetsWithVirtual ← addPanic newTotalBorrowAssets 1
      let newTotalBorrowSharesWithVirtual ← addPanic newTotalBorrowShares 1000000
      badDebtAssets := min newTotalBorrowAssets (mulDivUp badDebtShares newTotalBorrowAssetsWithVirtual newTotalBorrowSharesWithVirtual)
      setStructMember2 "positionSlot" id borrower "borrowShares" ZERO
      let remainingBorrowShares ← subPanic newTotalBorrowShares badDebtShares
      let remainingBorrowAssets ← subPanic newTotalBorrowAssets badDebtAssets
      let remainingSupplyAssets ← subPanic totalSupplyAssets_ badDebtAssets
      setStructMember "marketSlot" id "totalBorrowShares" remainingBorrowShares
      setStructMember "marketSlot" id "totalBorrowAssets" remainingBorrowAssets
      setStructMember "marketSlot" id "totalSupplyAssets" remainingSupplyAssets
    else
      setStructMember "marketSlot" id "totalBorrowShares" newTotalBorrowShares
      setStructMember "marketSlot" id "totalBorrowAssets" newTotalBorrowAssets
    let sender <- msgSender
    emit "Liquidate" [id, sender, borrower, repaidAssets, finalRepaidShares, finalSeizedAssets, badDebtAssets, badDebtShares]
    ecmDo MorphoSafeTransfer.safeTransferModule
      [addressToWord marketParams.collateralToken, addressToWord sender, finalSeizedAssets]
    ecmDo (_root_.Morpho.Contract.OptionalCallback.optionalCallbackModule 0xcf7ea196 1 "data")
      [addressToWord sender, repaidAssets]
    let thisAddress ← contractAddress
    ecmDo MorphoSafeTransfer.safeTransferFromModule
      [addressToWord marketParams.loanToken, addressToWord sender, addressToWord thisAddress, repaidAssets]
    return (finalSeizedAssets, repaidAssets)

  function allow_post_interaction_writes flashLoan (token : Address, assets : Uint256, data : Bytes) local_obligations [flash_loan_transfers := assumed "Flash-loan ERC20 transfer target behavior remains external; transfer and callback mechanics use Verity ECMs."] : Unit := do
    let thisAddress ← contractAddress
    require (assets > 0) "zero assets"
    let sender <- msgSender
    emit "FlashLoan" [sender, token, assets]
    ecmDo MorphoSafeTransfer.safeTransferModule
      [addressToWord token, addressToWord sender, assets]
    ecmDo (Compiler.Modules.Callbacks.callbackModule 0x31f57072 1 "data")
      [addressToWord sender, assets]
    ecmDo MorphoSafeTransfer.safeTransferFromModule
      [addressToWord token, addressToWord sender, addressToWord thisAddress, assets]

end Morpho.Contract
