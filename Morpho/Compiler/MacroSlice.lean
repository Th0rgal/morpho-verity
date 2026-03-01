import Verity.Core
import Verity.Macro

namespace Morpho.Compiler.MacroSlice

open Verity

def add (a b : Uint256) : Uint256 := Verity.Core.Uint256.add a b
def and (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def shr (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shr shift value
def mstore (_offset _value : Uint256) : Contract Unit := Verity.pure ()
def returnStorageWords (_slots : Array Uint256) : Contract (Array Uint256) := Verity.pure #[]
def returnValues (_values : List Uint256) : Contract Unit := Verity.pure ()
def getMappingWord (_slot : StorageSlot (Uint256 → Uint256)) (_key _wordOffset : Uint256) :
    Contract Uint256 := Verity.pure 0
def keccak256 (offset size : Uint256) : Uint256 := add offset size
def chainid : Uint256 := 0
def contractAddress : Uint256 := 0

-- Incremental macro-native Morpho slice for migration progress tracking.
-- This intentionally models a selector-exact subset with supported constructs.
verity_contract MorphoViewSlice where
  storage
    ownerSlot : Address := slot 0
    feeRecipientSlot : Address := slot 1
    marketSlot : Uint256 -> Uint256 := slot 3
    idToMarketParamsSlot : Uint256 -> Uint256 := slot 8
    isIrmEnabledSlot : Address -> Uint256 := slot 4
    isLltvEnabledSlot : Uint256 -> Uint256 := slot 5
    isAuthorizedSlot : Address -> Address -> Uint256 := slot 6
    nonceSlot : Address -> Uint256 := slot 7

  function DOMAIN_SEPARATOR () : Uint256 := do
    mstore 0 32523383700587834770323112271211932718128200013265661849047136999858837557784
    mstore 32 chainid
    mstore 64 contractAddress
    let domainSeparator := keccak256 0 96
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
    let word <- getMappingWord marketSlot id 2
    return (and word 340282366920938463463374607431768211455)

  function totalSupplyAssets (id : Bytes32) : Uint256 := do
    let word <- getMappingWord marketSlot id 0
    return (and word 340282366920938463463374607431768211455)

  function totalSupplyShares (id : Bytes32) : Uint256 := do
    let word <- getMappingWord marketSlot id 0
    return (and (shr 128 word) 340282366920938463463374607431768211455)

  function totalBorrowAssets (id : Bytes32) : Uint256 := do
    let word <- getMappingWord marketSlot id 1
    return (and word 340282366920938463463374607431768211455)

  function totalBorrowShares (id : Bytes32) : Uint256 := do
    let word <- getMappingWord marketSlot id 1
    return (and (shr 128 word) 340282366920938463463374607431768211455)

  function fee (id : Bytes32) : Uint256 := do
    let word <- getMappingWord marketSlot id 2
    return (and (shr 128 word) 340282366920938463463374607431768211455)

  function idToMarketParams (id : Bytes32) : Tuple [Address, Address, Address, Address, Uint256] := do
    let loanToken <- getMappingWord idToMarketParamsSlot id 0
    let collateralToken <- getMappingWord idToMarketParamsSlot id 1
    let oracle <- getMappingWord idToMarketParamsSlot id 2
    let irm <- getMappingWord idToMarketParamsSlot id 3
    let lltv <- getMappingWord idToMarketParamsSlot id 4
    let addrMask := 1461501637330902918203684832716283019655932542975
    returnValues [
      and loanToken addrMask,
      and collateralToken addrMask,
      and oracle addrMask,
      and irm addrMask,
      lltv
    ]

  function market (id : Bytes32) : Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let word0 <- getMappingWord marketSlot id 0
    let word1 <- getMappingWord marketSlot id 1
    let word2 <- getMappingWord marketSlot id 2
    let loMask := 340282366920938463463374607431768211455
    returnValues [
      and word0 loMask,
      and (shr 128 word0) loMask,
      and word1 loMask,
      and (shr 128 word1) loMask,
      and word2 loMask,
      and (shr 128 word2) loMask
    ]

  function position (id : Bytes32, user : Address) : Tuple [Uint256, Uint256, Uint256] := do
    let _ignoredId := id
    let _ignoredUser := user
    -- Pending upstream storage-typing support for mapping(uint256 => mapping(address => ...)).
    returnValues [0, 0, 0]

  function extSloads (slots : Array Bytes32) : Array Uint256 := do
    returnStorageWords slots

  function setOwner (newOwner : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    require (newOwner != currentOwner) "already set"
    setStorageAddr ownerSlot newOwner

  function setFeeRecipient (newFeeRecipient : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentFeeRecipient <- getStorageAddr feeRecipientSlot
    require (newFeeRecipient != currentFeeRecipient) "already set"
    setStorageAddr feeRecipientSlot newFeeRecipient

  function enableIrm (irm : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMapping isIrmEnabledSlot irm
    require (currentValue == 0) "already set"
    setMapping isIrmEnabledSlot irm 1

  function enableLltv (lltv : Uint256) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMappingUint isLltvEnabledSlot lltv
    require (currentValue == 0) "already set"
    require (lltv < 1000000000000000000) "max LLTV exceeded"
    setMappingUint isLltvEnabledSlot lltv 1

  function setAuthorization (authorized : Address, newIsAuthorized : Bool) : Unit := do
    let sender <- msgSender
    let currentValue <- getMapping2 isAuthorizedSlot sender authorized
    if newIsAuthorized then
      require (currentValue == 0) "already set"
      setMapping2 isAuthorizedSlot sender authorized 1
    else
      require (currentValue != 0) "already set"
      setMapping2 isAuthorizedSlot sender authorized 0
    let currentNonce <- getMapping nonceSlot sender
    setMapping nonceSlot sender (add currentNonce 1)

  function setAuthorizationWithSig (authorization : Tuple [Address, Address, Bool, Uint256, Uint256], signature : Tuple [Uint8, Bytes32, Bytes32]) : Unit := do
    let sender <- msgSender
    let authorization' := authorization
    let signature' := signature
    let _ignoredAuthorization := authorization'
    let _ignoredSignature := signature'
    require (sender == sender) "setAuthorizationWithSig noop"

  function createMarket (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let marketParams' := marketParams
    let _ignored := marketParams'
    require (sender == sender) "createMarket noop"

  function setFee (marketParams : Tuple [Address, Address, Address, Address, Uint256], newFee : Uint256) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let marketParams' := marketParams
    let _ignored := marketParams'
    let _ignoredFee := newFee
    require (sender == sender) "setFee noop"

  function accrueInterest (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignored := marketParams'
    require (sender == sender) "accrueInterest noop"

  function supplyCollateral (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "supplyCollateral noop"

  function withdrawCollateral (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "withdrawCollateral noop"

  function supply (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "supply noop"

  function withdraw (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "withdraw noop"

  function borrow (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "borrow noop"

  function repay (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "repay noop"

  function liquidate (marketParams : Tuple [Address, Address, Address, Address, Uint256], borrower : Address, seizedAssets : Uint256, repaidShares : Uint256, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredBorrower := borrower
    let _ignoredSeizedAssets := seizedAssets
    let _ignoredRepaidShares := repaidShares
    let _ignoredData := data
    require (sender == sender) "liquidate noop"

  function flashLoan (token : Address, assets : Uint256, data : Bytes) : Unit := do
    require (assets > 0) "zero assets"
    let sender <- msgSender
    let _ignoredToken := token
    let _ignoredData := data
    require (sender == sender) "flashLoan noop"

end Morpho.Compiler.MacroSlice
