import Verity.Core
import Verity.Macro

namespace Morpho.Compiler.MacroSlice

open Verity

def add (a b : Uint256) : Uint256 := Verity.Core.Uint256.add a b
def and (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def shr (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shr shift value
def mstore (_offset _value : Uint256) : Contract Unit := Verity.pure ()
def keccak256 (offset size : Uint256) : Uint256 := add offset size
def chainid : Uint256 := 0
def contractAddress : Uint256 := 0

-- Incremental macro-native Morpho slice for migration progress tracking.
-- This intentionally models a selector-exact subset with supported constructs.
verity_contract MorphoViewSlice where
  storage
    ownerSlot : Address := slot 0
    feeRecipientSlot : Address := slot 1
    marketWord0Slot : Uint256 -> Uint256 := slot 3
    marketWord1Slot : Uint256 -> Uint256 := slot 9
    marketWord2Slot : Uint256 -> Uint256 := slot 10
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
    let word <- getMappingUint marketWord2Slot id
    return (and word 340282366920938463463374607431768211455)

  function totalSupplyAssets (id : Bytes32) : Uint256 := do
    let word <- getMappingUint marketWord0Slot id
    return (and word 340282366920938463463374607431768211455)

  function totalSupplyShares (id : Bytes32) : Uint256 := do
    let word <- getMappingUint marketWord0Slot id
    return (and (shr 128 word) 340282366920938463463374607431768211455)

  function totalBorrowAssets (id : Bytes32) : Uint256 := do
    let word <- getMappingUint marketWord1Slot id
    return (and word 340282366920938463463374607431768211455)

  function totalBorrowShares (id : Bytes32) : Uint256 := do
    let word <- getMappingUint marketWord1Slot id
    return (and (shr 128 word) 340282366920938463463374607431768211455)

  function fee (id : Bytes32) : Uint256 := do
    let word <- getMappingUint marketWord2Slot id
    return (and (shr 128 word) 340282366920938463463374607431768211455)

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

end Morpho.Compiler.MacroSlice
