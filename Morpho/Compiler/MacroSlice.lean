import Compiler.CompilationModel
import Verity.Core
import Verity.Macro

namespace Morpho.Compiler.MacroSlice

open Verity

def add (a b : Uint256) : Uint256 := Verity.Core.Uint256.add a b
def and (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def shl (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shl shift value
def shr (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shr shift value
def mstore (_offset _value : Uint256) : Contract Unit := Verity.pure ()
def returnStorageWords (_slots : Array Uint256) : Contract (Array Uint256) := Verity.pure #[]
def rawLog (_topics : List Uint256) (_dataOffset _dataSize : Uint256) : Contract Unit := Verity.pure ()
def ecrecover (_hash : Uint256) (_v : Uint8) (_r : Bytes32) (_s : Bytes32) :
    Contract Address := Verity.pure 0
def structMember (_slot : StorageSlot (Uint256 → Uint256)) (_key : Uint256) (_member : String) :
    Contract Uint256 := Verity.pure 0
def structMember2
    (_slot : StorageSlot (Uint256 → Address → Uint256))
    (_key : Uint256) (_subKey : Address) (_member : String) :
    Contract Uint256 := Verity.pure 0
def setStructMember
    {_α : Type}
    (_slot : StorageSlot (Uint256 → Uint256))
    (_key : Uint256) (_member : String) (_value : _α) :
    Contract Unit := Verity.pure ()
def setStructMember2
    {_α : Type}
    (_slot : StorageSlot (Uint256 → Address → Uint256))
    (_key : Uint256) (_subKey : Address) (_member : String) (_value : _α) :
    Contract Unit := Verity.pure ()
def keccak256 (offset size : Uint256) : Uint256 := add offset size
def keccakMarketParams : String := "keccakMarketParams"
private def cantorPair (a b : Nat) : Nat :=
  let s := a + b
  (s * (s + 1)) / 2 + b
private def marketIdWord (args : List Uint256) : Uint256 :=
  match args with
  | [loanToken, collateralToken, oracle, irm, lltv] =>
      Verity.Core.Uint256.ofNat <|
        cantorPair
          (cantorPair
            (cantorPair
              (cantorPair loanToken.val collateralToken.val)
              oracle.val)
            irm.val)
          lltv.val
  | _ => 0
def externalCall (name : String) (args : List Uint256) : Uint256 :=
  if name = keccakMarketParams then marketIdWord args else 0
def blockTimestamp : Uint256 := 0
def chainid : Uint256 := 0
def contractAddress : Uint256 := 0

-- Incremental macro-native Morpho slice for migration progress tracking.
-- This intentionally models a selector-exact subset with supported constructs.
verity_contract MorphoViewSlice where
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
    let currentLastUpdate <- structMember marketSlot id "lastUpdate"
    return currentLastUpdate

  function totalSupplyAssets (id : Bytes32) : Uint256 := do
    let currentTotalSupplyAssets <- structMember marketSlot id "totalSupplyAssets"
    return currentTotalSupplyAssets

  function totalSupplyShares (id : Bytes32) : Uint256 := do
    let currentTotalSupplyShares <- structMember marketSlot id "totalSupplyShares"
    return currentTotalSupplyShares

  function totalBorrowAssets (id : Bytes32) : Uint256 := do
    let currentTotalBorrowAssets <- structMember marketSlot id "totalBorrowAssets"
    return currentTotalBorrowAssets

  function totalBorrowShares (id : Bytes32) : Uint256 := do
    let currentTotalBorrowShares <- structMember marketSlot id "totalBorrowShares"
    return currentTotalBorrowShares

  function fee (id : Bytes32) : Uint256 := do
    let currentFee <- structMember marketSlot id "fee"
    return currentFee

  function idToMarketParams (id : Bytes32) : Tuple [Address, Address, Address, Address, Uint256] := do
    let loanToken <- structMember idToMarketParamsSlot id "loanToken"
    let collateralToken <- structMember idToMarketParamsSlot id "collateralToken"
    let oracle <- structMember idToMarketParamsSlot id "oracle"
    let irm <- structMember idToMarketParamsSlot id "irm"
    let lltv <- structMember idToMarketParamsSlot id "lltv"
    return (wordToAddress loanToken, wordToAddress collateralToken, wordToAddress oracle,
      wordToAddress irm, lltv)

  function market (id : Bytes32) : Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let totalSupplyAssets_ <- structMember marketSlot id "totalSupplyAssets"
    let totalSupplyShares_ <- structMember marketSlot id "totalSupplyShares"
    let totalBorrowAssets_ <- structMember marketSlot id "totalBorrowAssets"
    let totalBorrowShares_ <- structMember marketSlot id "totalBorrowShares"
    let lastUpdate_ <- structMember marketSlot id "lastUpdate"
    let fee_ <- structMember marketSlot id "fee"
    return (totalSupplyAssets_, totalSupplyShares_, totalBorrowAssets_, totalBorrowShares_,
      lastUpdate_, fee_)

  function position (id : Bytes32, user : Address) : Tuple [Uint256, Uint256, Uint256] := do
    let supplyShares_ <- structMember2 positionSlot id user "supplyShares"
    let borrowShares_ <- structMember2 positionSlot id user "borrowShares"
    let collateral_ <- structMember2 positionSlot id user "collateral"
    return (supplyShares_, borrowShares_, collateral_)

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

  function setAuthorizationWithSig (authorization : Tuple [Address, Address, Bool, Uint256, Uint256], signature : Tuple [Uint8, Bytes32, Bytes32]) : Unit := do
    let sender <- msgSender
    let authorizer := authorization_0
    let authorized := authorization_1
    let newIsAuthorized := authorization_2
    let expectedNonce := authorization_3
    let deadline := authorization_4
    let v := signature_0
    let r := signature_1
    let s := signature_2
    require (deadline >= blockTimestamp) "signature expired"
    let currentNonce <- getMapping nonceSlot authorizer
    require (expectedNonce == currentNonce) "invalid nonce"
    setMapping nonceSlot authorizer (add currentNonce 1)
    let mut isAuthorizedWord := 0
    if newIsAuthorized then
      isAuthorizedWord := 1
    else
      isAuthorizedWord := 0
    mstore 0 58375287309530710162933305390054840987079945985439883494330239665215553039505
    mstore 32 authorizer
    mstore 64 authorized
    mstore 96 isAuthorizedWord
    mstore 128 expectedNonce
    mstore 160 deadline
    let hashStruct := keccak256 0 192
    mstore 32 32523383700587834770323112271211932718128200013265661849047136999858837557784
    mstore 64 chainid
    mstore 96 contractAddress
    let domainSeparator := keccak256 32 96
    mstore 0 11376154489267527918027507597793705913714985631324067728021276794371439513600
    mstore 2 domainSeparator
    mstore 34 hashStruct
    let digest := keccak256 0 66
    let signatory ← ecrecover digest v r s
    require (signatory != 0) "invalid signature"
    require (signatory == authorizer) "invalid signature"
    mstore 0 currentNonce
    rawLog [76232593431660337877225143138868361523028064348553599308396852457510026547815,
      sender, authorizer] 0 32
    setMapping2 isAuthorizedSlot authorizer authorized isAuthorizedWord
    mstore 0 isAuthorizedWord
    rawLog [96781178765147248776732963005526932015340179640649679610324219398565412161984,
      sender, authorizer, authorized] 0 32

  function createMarket (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let _ignoredMarketParams := marketParams
    let id := externalCall keccakMarketParams [
      addressToWord marketParams_0,
      addressToWord marketParams_1,
      addressToWord marketParams_2,
      addressToWord marketParams_3,
      marketParams_4
    ]
    let irmEnabled <- getMapping isIrmEnabledSlot marketParams_3
    require (irmEnabled == 1) "IRM not enabled"
    let lltvEnabled <- getMappingUint isLltvEnabledSlot marketParams_4
    require (lltvEnabled == 1) "LLTV not enabled"
    let currentLastUpdate <- structMember marketSlot id "lastUpdate"
    require (currentLastUpdate == 0) "market already created"
    setStructMember marketSlot id "lastUpdate" blockTimestamp
    setStructMember marketSlot id "fee" 0
    setStructMember marketSlot id "totalSupplyAssets" 0
    setStructMember marketSlot id "totalSupplyShares" 0
    setStructMember marketSlot id "totalBorrowAssets" 0
    setStructMember marketSlot id "totalBorrowShares" 0
    setStructMember idToMarketParamsSlot id "loanToken" marketParams_0
    setStructMember idToMarketParamsSlot id "collateralToken" marketParams_1
    setStructMember idToMarketParamsSlot id "oracle" marketParams_2
    setStructMember idToMarketParamsSlot id "irm" marketParams_3
    setStructMember idToMarketParamsSlot id "lltv" marketParams_4

  function setFee (marketParams : Tuple [Address, Address, Address, Address, Uint256], newFee : Uint256) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let marketParamsArg := marketParams
    let _ignored := marketParamsArg
    let _ignoredFee := newFee
    require (sender == sender) "setFee noop"

  function accrueInterest (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignored := marketParamsArg
    require (sender == sender) "accrueInterest noop"

  function supplyCollateral (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "supplyCollateral noop"

  function withdrawCollateral (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "withdrawCollateral noop"

  function supply (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "supply noop"

  function withdraw (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "withdraw noop"

  function borrow (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, receiver : Address) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredReceiver := receiver
    require (sender == sender) "borrow noop"

  function repay (marketParams : Tuple [Address, Address, Address, Address, Uint256], assets : Uint256, shares : Uint256, onBehalf : Address, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredAssets := assets
    let _ignoredShares := shares
    let _ignoredOnBehalf := onBehalf
    let _ignoredData := data
    require (sender == sender) "repay noop"

  function liquidate (marketParams : Tuple [Address, Address, Address, Address, Uint256], borrower : Address, seizedAssets : Uint256, repaidShares : Uint256, data : Bytes) : Unit := do
    let sender <- msgSender
    let marketParamsArg := marketParams
    let _ignoredMarket := marketParamsArg
    let _ignoredBorrower := borrower
    let _ignoredSeizedAssets := seizedAssets
    let _ignoredRepaidShares := repaidShares
    let _ignoredData := data
    require (sender == sender) "liquidate noop"

  function flashLoan (token : Address, assets : Uint256, data : Bytes) : Unit := do
    require (assets > 0) "zero assets"
    let sender <- msgSender
    let _ignoredData := data
    mstore 0 assets
    rawLog [90206565393282384481013871153915153991969900064758434107982401003955406262034, sender, token] 0 32

end Morpho.Compiler.MacroSlice
