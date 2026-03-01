import Verity.Core
import Verity.Macro

namespace Morpho.Compiler.MacroSlice

open Verity

-- Incremental macro-native Morpho slice for migration progress tracking.
-- This intentionally models a selector-exact subset of read-only functions.
verity_contract MorphoViewSlice where
  storage
    ownerSlot : Address := slot 0
    feeRecipientSlot : Address := slot 1
    isIrmEnabledSlot : Address -> Uint256 := slot 4
    isAuthorizedSlot : Address -> Address -> Uint256 := slot 6
    nonceSlot : Address -> Uint256 := slot 7

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

  function nonce (authorizer : Address) : Uint256 := do
    let currentNonce <- getMapping nonceSlot authorizer
    return currentNonce

end Morpho.Compiler.MacroSlice
