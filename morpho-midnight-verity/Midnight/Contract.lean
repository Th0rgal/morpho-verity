import Contracts.Common
import Compiler.Modules.Callbacks
import Compiler.Modules.Calls
import Compiler.Modules.Create2SSTORE2
import Compiler.Modules.Oracle
import Verity.Core
import Verity.Stdlib.Math

namespace Midnight.Contract

set_option linter.unusedVariables false

open Verity hiding pure bind
open Verity.EVM.Uint256
open Verity.Stdlib.Math

def arrayLength {α : Type} (values : Array α) : Uint256 := Contracts.arrayLength values
def arrayElement {α : Type} [Inhabited α] (values : Array α) (index : Uint256) : α :=
  Contracts.arrayElement values index
def safeTransfer (token toAddr : Address) (amount : Uint256) : Contract Unit :=
  Contracts.safeTransfer token toAddr amount
def safeTransferFrom (token fromAddr toAddr : Address) (amount : Uint256) : Contract Unit :=
  Contracts.safeTransferFrom token fromAddr toAddr amount
def bitAnd (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def bitOr (a b : Uint256) : Uint256 := Verity.Core.Uint256.or a b
def bitNot (a : Uint256) : Uint256 := Verity.Core.Uint256.not a
def shl (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shl shift value
def div (a b : Uint256) : Uint256 := Verity.Core.Uint256.div a b
def mod (a b : Uint256) : Uint256 := Verity.Core.Uint256.mod a b
def calldataload (offset : Uint256) : Uint256 := offset

/-
  A focused Verity-facing Midnight model for the RCF arithmetic used by
  `src/Midnight.sol::liquidate`.

  The surface targets the normal-mode arithmetic that the Lean proofs in
  `Midnight.Proofs.RCF` reason about:

    * `maxRepaid` lines 655-661,
    * seized-input `repaidUnits` lines 649-650,
    * repaid-input `seizedAssets` lines 649-653,
    * `zeroFloorSub` shape used by the RCF guard,
    * `isHealthy` collateral contribution lines 953-955,
    * a focused normal-mode max-repaid post-health projection,
    * bad-debt collateral-loop repayable value lines 610-616,
    * one-step collateral-loop accumulator update lines 609-616,
    * bad-debt `lossFactor`, `totalUnits`, and continuous-fee-credit updates
      lines 626-640,
    * the `atMostOneNonZero` input guard line 592,
    * the borrower and liquidator-gate guards lines 593-597,
    * the full and normal-mode `NotLiquidatable` guard lines 620-623,
    * the bad-debt branch predicate line 625,
    * the bad-debt borrower debt write line 628,
    * a combined bad-debt branch local sequencing surface for lines 625-640,
    * post-maturity and normal-mode LIF selection lines 644-647,
    * the repay/seize branch predicate line 645,
    * normal-mode RCF guard lines 662-667,
    * a general normal-mode RCF local sequence for both repay-input and
      seize-input amount branches,
    * collateral bitmap clearing lines 679-681,
    * withdrawable increase line 682,
    * a combined normal-mode repaid-input local sequencing surface for lines
      625-717, specialized to `repaidUnits = maxRepaid` after the RCF guard's
      left disjunct is already satisfied,
    * a single normal-mode local `liquidate` sequence that combines the
      bad-debt market writes, max-repay branch, borrower/collateral writes,
      payer/callback branch, and return pair from lines 625-717 under the same
      `repaidUnits = maxRepaid` specialization,
    * payer/callback branch and returned pair lines 686, 704-717,
    * `updatePositionView` post-slash credit, pending-fee slashing, and
      accrued-fee subtraction lines 802-818, including the returned triple.
-/
verity_contract MidnightRCF where
  storage
    unusedSlot : Uint256 := slot 0

  function maxRepaid (debt : Uint256, maxDebt : Uint256, lif : Uint256, lltv : Uint256) :
      Uint256 := do
    if lltv < 1000000000000000000 then
      return mulDivUp
        (sub debt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      return 115792089237316195423570985008687907853269984665640564039457584007913129639935

  function seizedFromRepaid
      (repaidUnits : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256) :
      Uint256 := do
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    return mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice

  function repaidFromSeized
      (seizedAssets : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256) :
      Uint256 := do
    let seizedValue := mulDivUp seizedAssets liquidatedCollatPrice
      1000000000000000000000000000000000000
    return mulDivUp seizedValue 1000000000000000000 lif

  function repaySeizeAmounts
      (inputRepaidUnits : Uint256, inputSeizedAssets : Uint256, lif : Uint256,
        liquidatedCollatPrice : Uint256) : Tuple [Uint256, Uint256] := do
    if inputSeizedAssets > 0 then
      let repaidUnits := mulDivUp
        (mulDivUp inputSeizedAssets liquidatedCollatPrice
          1000000000000000000000000000000000000)
        1000000000000000000
        lif
      return (repaidUnits, inputSeizedAssets)
    else
      let seizedAssets := mulDivDown
        (mulDivDown inputRepaidUnits lif 1000000000000000000)
        1000000000000000000000000000000000000
        liquidatedCollatPrice
      return (inputRepaidUnits, seizedAssets)

  function collateralMaxDebt (collateral : Uint256, price : Uint256, lltv : Uint256) :
      Uint256 := do
    let quoted := mulDivDown collateral price
      1000000000000000000000000000000000000
    return mulDivDown quoted lltv 1000000000000000000

  function postDebt (debt : Uint256, repaidUnits : Uint256) : Uint256 := do
    return sub debt repaidUnits

  function postCollateral (collateral : Uint256, seizedAssets : Uint256) : Uint256 := do
    return sub collateral seizedAssets

  function zeroFloorSub (x : Uint256, y : Uint256) : Uint256 := do
    if x > y then
      return sub x y
    else
      return 0

  function withdrawableAfterRepay (withdrawable : Uint256, repaidUnits : Uint256) :
      Uint256 := do
    return add withdrawable repaidUnits

  function clearBitmapBit (bitmap : Uint256, selectedIndex : Uint256) : Uint256 := do
    let mask := bitNot (shl selectedIndex 1)
    return bitAnd bitmap mask

  function atMostOneNonZero (repaidUnits : Uint256, seizedAssets : Uint256) :
      Bool := do
    return (repaidUnits == 0 || seizedAssets == 0)

  function borrowerDebtPositive (debt : Uint256) : Bool := do
    return debt > 0

  function liquidatorGateAllows (liquidatorGate : Address, canLiquidate : Bool) :
      Bool := do
    return (liquidatorGate == 0 || canLiquidate)

  function normalModeLiquidatable
      (liquidationUnlocked : Bool, originalDebt : Uint256, maxDebt : Uint256) :
      Bool := do
    return (liquidationUnlocked && originalDebt > maxDebt)

  function liquidatableGuard
      (liquidationLocked : Bool, postMaturityMode : Bool, blockTimestamp : Uint256,
        maturity : Uint256, originalDebt : Uint256, maxDebt : Uint256) :
      Bool := do
    if postMaturityMode then
      return (liquidationLocked == false && blockTimestamp > maturity)
    else
      return (liquidationLocked == false && originalDebt > maxDebt)

  function normalModeEntryGuards
      (repaidUnits : Uint256, seizedAssets : Uint256, debt : Uint256,
        liquidatorGate : Address, canLiquidate : Bool, liquidationUnlocked : Bool,
        originalDebt : Uint256, maxDebt : Uint256) : Bool := do
    return ((repaidUnits == 0 || seizedAssets == 0) && debt > 0 &&
      (liquidatorGate == 0 || canLiquidate) &&
      (liquidationUnlocked && originalDebt > maxDebt))

  function repayBranchActive (repaidUnits : Uint256, seizedAssets : Uint256) :
      Bool := do
    return (repaidUnits > 0 || seizedAssets > 0)

  function liquidationLif
      (postMaturityMode : Bool, maxLif : Uint256, blockTimestamp : Uint256,
        maturity : Uint256) : Uint256 := do
    if postMaturityMode then
      let elapsed := sub blockTimestamp maturity
      let postMaturityLif := add 1000000000000000000
        (mulDivDown
          (sub maxLif 1000000000000000000)
          elapsed
          900)
      if maxLif <= postMaturityLif then
        return maxLif
      else
        return postMaturityLif
    else
      return maxLif

  function badDebtBranchActive (badDebt : Uint256) : Bool := do
    return badDebt > 0

  function badDebtPostDebt (originalDebt : Uint256, badDebt : Uint256) : Uint256 := do
    return sub originalDebt badDebt

  function badDebtLocalSequence
      (originalDebt : Uint256, totalUnits : Uint256, oldLossFactor : Uint256,
        continuousFeeCredit : Uint256, badDebt : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Bool] := do
    let mut postDebt := originalDebt
    let mut newTotalUnits := totalUnits
    let mut newLossFactor := oldLossFactor
    let mut newContinuousFeeCredit := continuousFeeCredit
    let branchActive := badDebt > 0
    if branchActive then
      postDebt := sub originalDebt badDebt
      newLossFactor := sub
        340282366920938463463374607431768211455
        (mulDivDown
          (sub 340282366920938463463374607431768211455 oldLossFactor)
          (sub totalUnits badDebt)
          totalUnits)
      newTotalUnits := sub totalUnits badDebt
      if oldLossFactor < 340282366920938463463374607431768211455 then
        newContinuousFeeCredit := mulDivDown continuousFeeCredit
          (sub 340282366920938463463374607431768211455 newLossFactor)
          (sub 340282366920938463463374607431768211455 oldLossFactor)
      else
        newContinuousFeeCredit := 0
    else
      postDebt := postDebt
    return (postDebt, newTotalUnits, newLossFactor, newContinuousFeeCredit, branchActive)

  function callbackRequired (callback : Address) : Bool := do
    return callback != 0

  function liquidatePayer (callback : Address, msgSender : Address) : Address := do
    if callback != 0 then
      return callback
    else
      return msgSender

  function callbackSuccessValue () : Uint256 := do
    return 57683088179238363159977504707935902064464440500167392520350201799917296135842

  function callbackReturnAccepted (callback : Address, returnValue : Uint256) :
      Bool := do
    return (callback == 0 ||
      returnValue ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842)

  function liquidateReturn (seizedAssets : Uint256, repaidUnits : Uint256) :
      Tuple [Uint256, Uint256] := do
    return (seizedAssets, repaidUnits)

  function normalModeRcfLocalSequence
      (currentDebt : Uint256, maxDebt : Uint256, inputRepaidUnits : Uint256,
        inputSeizedAssets : Uint256, lltv : Uint256, lif : Uint256,
        liquidatedCollatPrice : Uint256, collateral : Uint256,
        collateralBitmap : Uint256, collateralIndex : Uint256, withdrawable : Uint256,
        rcfThreshold : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Bool, Uint256] := do
    let mut repaidUnits := inputRepaidUnits
    let mut seizedAssets := inputSeizedAssets
    if inputSeizedAssets > 0 then
      repaidUnits := mulDivUp
        (mulDivUp inputSeizedAssets liquidatedCollatPrice
          1000000000000000000000000000000000000)
        1000000000000000000
        lif
    else
      seizedAssets := mulDivDown
        (mulDivDown inputRepaidUnits lif 1000000000000000000)
        1000000000000000000000000000000000000
        liquidatedCollatPrice
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    let mut capacityShortfall := 0
    if collateralRepayCapacity > maxRepaid then
      capacityShortfall := sub collateralRepayCapacity maxRepaid
    else
      capacityShortfall := capacityShortfall
    let rcfAccepted := repaidUnits <= maxRepaid ||
      capacityShortfall < rcfThreshold
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    return (repaidUnits, seizedAssets, maxRepaid, newCollateral, postBitmap,
      postWithdrawable, finalDebt, rcfAccepted, capacityShortfall)

  function normalModeRepaidInputLocalSequence
      (originalDebt : Uint256, badDebt : Uint256, maxDebt : Uint256,
        lltv : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256,
        collateral : Uint256, collateralBitmap : Uint256, collateralIndex : Uint256,
        withdrawable : Uint256, callback : Address, msgSender : Address,
        callbackReturn : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Address, Bool, Uint256, Uint256] := do
    let currentDebt := sub originalDebt badDebt
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let repaidUnits := maxRepaid
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    let mut payer := msgSender
    if callback != 0 then
      payer := callback
    else
      payer := payer
    let callbackAccepted := callback == 0 ||
      callbackReturn ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842
    return (currentDebt, maxRepaid, seizedAssets, newCollateral, postBitmap,
      postWithdrawable, finalDebt, payer, callbackAccepted,
      seizedAssets, repaidUnits)

  function normalModeLiquidateLocalSequence
      (originalDebt : Uint256, totalUnits : Uint256, oldLossFactor : Uint256,
        continuousFeeCredit : Uint256, badDebt : Uint256, maxDebt : Uint256,
        lltv : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256,
        collateral : Uint256, collateralBitmap : Uint256, collateralIndex : Uint256,
        withdrawable : Uint256, callback : Address, msgSender : Address,
        callbackReturn : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Uint256, Uint256, Uint256, Uint256, Address, Bool, Uint256, Uint256] := do
    let mut postBadDebtDebt := originalDebt
    let mut newTotalUnits := totalUnits
    let mut newLossFactor := oldLossFactor
    let mut newContinuousFeeCredit := continuousFeeCredit
    if badDebt > 0 then
      postBadDebtDebt := sub originalDebt badDebt
      newLossFactor := sub
        340282366920938463463374607431768211455
        (mulDivDown
          (sub 340282366920938463463374607431768211455 oldLossFactor)
          (sub totalUnits badDebt)
          totalUnits)
      newTotalUnits := sub totalUnits badDebt
      if oldLossFactor < 340282366920938463463374607431768211455 then
        newContinuousFeeCredit := mulDivDown continuousFeeCredit
          (sub 340282366920938463463374607431768211455 newLossFactor)
          (sub 340282366920938463463374607431768211455 oldLossFactor)
      else
        newContinuousFeeCredit := 0
    else
      postBadDebtDebt := postBadDebtDebt
    let currentDebt := postBadDebtDebt
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let repaidUnits := maxRepaid
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    let mut payer := msgSender
    if callback != 0 then
      payer := callback
    else
      payer := payer
    let callbackAccepted := callback == 0 ||
      callbackReturn ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842
    return (postBadDebtDebt, newTotalUnits, newLossFactor, newContinuousFeeCredit,
      currentDebt, maxRepaid, seizedAssets, newCollateral, postBitmap,
      postWithdrawable, finalDebt, payer, callbackAccepted,
      seizedAssets, repaidUnits)

  function badDebtCollateralRepayable
      (collateral : Uint256, price : Uint256, maxLif : Uint256) :
      Uint256 := do
    let quoted := mulDivUp collateral price
      1000000000000000000000000000000000000
    return mulDivUp quoted 1000000000000000000 maxLif

  function collateralLoopStep
      (maxDebt : Uint256, liquidatedCollatPrice : Uint256, badDebt : Uint256,
        collateralBitmap : Uint256, slotIndex : Uint256, selectedIndex : Uint256,
        collateral : Uint256, price : Uint256, lltv : Uint256, maxLif : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256] := do
    let collateralQuote := mulDivDown collateral price
      1000000000000000000000000000000000000
    let nextMaxDebt := add maxDebt
      (mulDivDown collateralQuote lltv 1000000000000000000)
    let mut nextLiquidatedCollatPrice := liquidatedCollatPrice
    if slotIndex == selectedIndex then
      nextLiquidatedCollatPrice := price
    else
      nextLiquidatedCollatPrice := nextLiquidatedCollatPrice
    let repayableQuote := mulDivUp collateral price
      1000000000000000000000000000000000000
    let repayable := mulDivUp repayableQuote 1000000000000000000 maxLif
    let mut nextBadDebt := 0
    if badDebt > repayable then
      nextBadDebt := sub badDebt repayable
    else
      nextBadDebt := nextBadDebt
    let nextBitmap := bitAnd collateralBitmap (bitNot (shl slotIndex 1))
    return (nextMaxDebt, nextLiquidatedCollatPrice, nextBadDebt, nextBitmap)

  function rcfAllows
      (repaidUnits : Uint256, maxRepaid : Uint256, collateral : Uint256,
        liquidatedCollatPrice : Uint256, lif : Uint256, rcfThreshold : Uint256) :
      Bool := do
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    return (repaidUnits <= maxRepaid ||
      sub collateralRepayCapacity maxRepaid < rcfThreshold)

  function rcfAllowsZeroFloor
      (repaidUnits : Uint256, maxRepaid : Uint256, collateral : Uint256,
        liquidatedCollatPrice : Uint256, lif : Uint256, rcfThreshold : Uint256) :
      Bool := do
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    let mut capacityShortfall := 0
    if collateralRepayCapacity > maxRepaid then
      capacityShortfall := sub collateralRepayCapacity maxRepaid
    else
      capacityShortfall := capacityShortfall
    return (repaidUnits <= maxRepaid || capacityShortfall < rcfThreshold)

  function normalModeMaxRepaidHealthyWithin3
      (debt : Uint256, otherMaxDebt : Uint256, collateral : Uint256, price : Uint256,
        lif : Uint256, lltv : Uint256) :
      Bool := do
    let selectedQuote := mulDivDown collateral price
      1000000000000000000000000000000000000
    let selectedMaxDebt := mulDivDown selectedQuote lltv 1000000000000000000
    let maxDebt := add otherMaxDebt selectedMaxDebt
    let repaid := mulDivUp
      (sub debt maxDebt)
      (mul 1000000000000000000 1000000000000000000)
      (sub
        (mul 1000000000000000000 1000000000000000000)
        (mul lif lltv))
    let repayValue := mulDivDown repaid lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      price
    let postDebt := sub debt repaid
    let postCollateral := sub collateral seizedAssets
    let postQuote := mulDivDown postCollateral price
      1000000000000000000000000000000000000
    let postSelectedMaxDebt := mulDivDown postQuote lltv 1000000000000000000
    let postMaxDebt := add otherMaxDebt postSelectedMaxDebt
    return postDebt <= add postMaxDebt 3

  function lossFactorAfterBadDebt
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256) :
      Uint256 := do
    return sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)

  function totalUnitsAfterBadDebt (totalUnits : Uint256, badDebt : Uint256) : Uint256 := do
    return sub totalUnits badDebt

  function continuousFeeCreditAfterBadDebt
      (continuousFeeCredit : Uint256, oldLossFactor : Uint256, newLossFactor : Uint256) :
      Uint256 := do
    if oldLossFactor < 340282366920938463463374607431768211455 then
      return mulDivDown continuousFeeCredit
        (sub 340282366920938463463374607431768211455 newLossFactor)
        (sub 340282366920938463463374607431768211455 oldLossFactor)
    else
      return 0

  function badDebtCoversTwoPostSlashCredits
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, credit1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let postSlashCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postSlashCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    return add postSlashCredit0 postSlashCredit1 <= newTotalUnits

  function badDebtCoversTwoPostUpdateCredits
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, accruedFee0 : Uint256, credit1 : Uint256, accruedFee1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let postSlashCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postSlashCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postUpdateCredit0 := sub postSlashCredit0 accruedFee0
    let postUpdateCredit1 := sub postSlashCredit1 accruedFee1
    return add postUpdateCredit0 postUpdateCredit1 <= newTotalUnits

  -- Same slashing formula as `badDebtCoversTwoPostSlashCredits`, kept under a
  -- storage-oriented name because the proof consumes it after lender updates.
  function badDebtCoversTwoStoredCreditsAfterUpdates
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, credit1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let storedCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let storedCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    return add storedCredit0 storedCredit1 <= newTotalUnits

  function postSlashCredit
      (credit : Uint256, marketLossFactor : Uint256, lastLossFactor : Uint256) :
      Uint256 := do
    if lastLossFactor < 340282366920938463463374607431768211455 then
      return mulDivDown credit
        (sub 340282366920938463463374607431768211455 marketLossFactor)
            (sub 340282366920938463463374607431768211455 lastLossFactor)
    else
      return 0

  function postSlashPendingFee
      (pendingFee : Uint256, credit : Uint256, postSlashCredit : Uint256) :
      Uint256 := do
    if 0 < credit then
      return sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      return 0

  function accruedFee
      (postSlashPendingFee : Uint256, lastAccrual : Uint256, accrualEnd : Uint256,
        maturity : Uint256) :
      Uint256 := do
    if lastAccrual < maturity then
      return mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub maturity lastAccrual)
    else
      return 0

  function postUpdateCreditAfterFee (postSlashCredit : Uint256, accruedFee : Uint256) :
      Uint256 := do
    return sub postSlashCredit accruedFee

  function updatePositionViewSequence
      (credit : Uint256, lastLossFactor : Uint256, marketLossFactor : Uint256,
        pendingFee : Uint256, lastAccrual : Uint256, blockTimestamp : Uint256,
        maturity : Uint256) : Tuple [Uint256, Uint256, Uint256] := do
    let mut postSlashCredit := 0
    if lastLossFactor < 340282366920938463463374607431768211455 then
      postSlashCredit := mulDivDown credit
        (sub 340282366920938463463374607431768211455 marketLossFactor)
        (sub 340282366920938463463374607431768211455 lastLossFactor)
    else
      postSlashCredit := postSlashCredit
    let mut postSlashPendingFee := 0
    if credit > 0 then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      postSlashPendingFee := postSlashPendingFee
    let mut accrualEnd := maturity
    if blockTimestamp <= maturity then
      accrualEnd := blockTimestamp
    else
      accrualEnd := accrualEnd
    let mut fee := 0
    if lastAccrual < maturity then
      fee := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub maturity lastAccrual)
    else
      fee := fee
    return (sub postSlashCredit fee, sub postSlashPendingFee fee, fee)

/-
  Initial full-artifact scaffold for `morpho-midnight/src/Midnight.sol`.

  This is intentionally separate from `MidnightRCF`. It starts the executable
  `Midnight` surface required by upstream Foundry parity with constructor-visible
  role slots, simple mappings/getters, `setIsAuthorized`, `setConsumed`, and the
  low-risk fee/role setter paths. Behavioral entrypoints such as `take`,
  `withdraw`, `repay`, `touchMarket`, and `liquidate` still need the remaining
  full-contract implementation work documented in `docs/MIDNIGHT_VERITY_PLAN.md`.
-/
verity_contract Midnight where
  storage
    initialChainIdSlot : Uint256 := slot 0
    roleSetterSlot : Address := slot 1
    feeSetterSlot : Address := slot 2
    feeClaimerSlot : Address := slot 3
    tickSpacingSetterSlot : Address := slot 4
    consumedSlot : MappingStruct2(Address,Bytes32,[amount @word 0]) := slot 5
    isAuthorizedSlot : MappingStruct2(Address,Address,[flag @word 0]) := slot 6
    defaultSettlementFeeSlot : MappingStruct2(Address,Uint256,[fee @word 0]) := slot 7
    defaultContinuousFeeSlot : Address -> Uint256 := slot 8
    claimableSettlementFeeSlot : Address -> Uint256 := slot 9
    marketStateSlot : MappingStruct(Bytes32,[
      totalUnits @word 0 packed(0,128),
      lossFactor @word 0 packed(128,128),
      withdrawable @word 1 packed(0,128),
      continuousFeeCredit @word 1 packed(128,128),
      settlementFeeCbp0 @word 2 packed(0,16),
      settlementFeeCbp1 @word 2 packed(16,16),
      settlementFeeCbp2 @word 2 packed(32,16),
      settlementFeeCbp3 @word 2 packed(48,16),
      settlementFeeCbp4 @word 2 packed(64,16),
      settlementFeeCbp5 @word 2 packed(80,16),
      settlementFeeCbp6 @word 2 packed(96,16),
      continuousFee @word 2 packed(112,32),
      tickSpacing @word 2 packed(144,8)
    ]) := slot 10
    positionSlot : MappingStruct2(Bytes32,Address,[
      credit @word 0 packed(0,128),
      pendingFee @word 0 packed(128,128),
      lastLossFactor @word 1 packed(0,128),
      lastAccrual @word 1 packed(128,128),
      debt @word 2 packed(0,128),
      collateralBitmap @word 2 packed(128,128)
    ]) := slot 11
    collateralSlot : MappingStruct2(Bytes32,Address,[
      amounts : FixedArray Uint256 16 @word 0
    ]) := slot 12

  struct CollateralParams where
    token : Address,
    lltv : Uint256,
    maxLif : Uint256,
    oracle : Address

  struct Market where
    loanToken : Address,
    collateralParams : Array CollateralParams,
    maturity : Uint256,
    rcfThreshold : Uint256,
    enterGate : Address,
    liquidatorGate : Address

  struct Offer where
    market : Market,
    buy : Bool,
    maker : Address,
    start : Uint256,
    expiry : Uint256,
    tick : Uint256,
    group : Bytes32,
    callback : Address,
    callbackData : Bytes,
    receiverIfMakerIsSeller : Address,
    ratifier : Address,
    reduceOnly : Bool,
    maxUnits : Uint256,
    maxAssets : Uint256

  errors
    error AlreadyConsumed()
    error CannotIncreaseDebtPostMaturity()
    error CastOverflow()
    error CollateralParamsNotSorted()
    error ContinuousFeeTooHigh()
    error ConsumedAssets()
    error ConsumedUnits()
    error BuyerGatedFromIncreasingCredit()
    error FeeNotMultipleOfFeeCbp()
    error InconsistentInput()
    error InvalidFeeIndex()
    error InvalidMaxLif()
    error InvalidTickSpacing()
    error LiquidatorGatedFromLiquidating()
    error LltvNotAllowed()
    error MarketNotCreated()
    error MarketLossFactorMaxedOut()
    error MaturityTooFar()
    error MultipleNonZero()
    error NoCollateralParams()
    error NotBorrower()
    error NotLiquidatable()
    error OfferExpired()
    error OfferNotStarted()
    error OnlyFeeClaimer()
    error OnlyFeeSetter()
    error OnlyRoleSetter()
    error OnlyTickSpacingSetter()
    error MakerCreditOrDebtIncreased()
    error RatifierUnauthorized()
    error RatifierFail()
    error RecoveryCloseFactorConditionsViolated()
    error SelfTake()
    error SellerGatedFromIncreasingDebt()
    error SellerIsLiquidatable()
    error SettlementFeeTooHigh()
    error TakerUnauthorized()
    error TickNotAccessible()
    error TooManyActivatedCollaterals()
    error TooManyCollateralParams()
    error Unauthorized()
    error UnhealthyBorrower()
    error WrongBuyCallbackReturnValue()
    error WrongFlashLoanCallbackReturnValue()
    error WrongLiquidateCallbackReturnValue()
    error WrongSellCallbackReturnValue()

  constants
    ZERO : Uint256 := 0
    ONE : Uint256 := 1
    TWO : Uint256 := 2
    THREE : Uint256 := 3
    FOUR : Uint256 := 4
    FIVE : Uint256 := 5
    SIX : Uint256 := 6
    CBP : Uint256 := 1000000000000
    DEFAULT_TICK_SPACING : Uint256 := 4
    MAX_CONTINUOUS_FEE : Uint256 := 317097919
    MAX_LOSS_FACTOR : Uint256 := 340282366920938463463374607431768211455
    MAX_SETTLEMENT_FEE_0_DAYS : Uint256 := 14000000000000
    MAX_SETTLEMENT_FEE_1_DAY : Uint256 := 14000000000000
    MAX_SETTLEMENT_FEE_7_DAYS : Uint256 := 98000000000000
    MAX_SETTLEMENT_FEE_30_DAYS : Uint256 := 417000000000000
    MAX_SETTLEMENT_FEE_90_DAYS : Uint256 := 1250000000000000
    MAX_SETTLEMENT_FEE_180_DAYS : Uint256 := 2500000000000000
    MAX_SETTLEMENT_FEE_360_DAYS : Uint256 := 5000000000000000
    ONE_DAY : Uint256 := 86400
    SEVEN_DAYS : Uint256 := 604800
    THIRTY_DAYS : Uint256 := 2592000
    NINETY_DAYS : Uint256 := 7776000
    ONE_EIGHTY_DAYS : Uint256 := 15552000
    THREE_SIXTY_DAYS : Uint256 := 31104000
    HUNDRED_YEARS : Uint256 := 3153600000
    WAD : Uint256 := 1000000000000000000
    ORACLE_PRICE_SCALE : Uint256 := 1000000000000000000000000000000000000
    TIME_TO_MAX_LIF : Uint256 := 900
    MAX_COLLATERALS : Uint256 := 128
    MAX_COLLATERALS_PER_BORROWER : Uint256 := 16
    MAX_TICK : Uint256 := 5820
    LIQUIDATION_CURSOR_LOW : Uint256 := 250000000000000000
    LIQUIDATION_CURSOR_HIGH : Uint256 := 500000000000000000
    LLTV_0 : Uint256 := 385000000000000000
    LLTV_1 : Uint256 := 625000000000000000
    LLTV_2 : Uint256 := 770000000000000000
    LLTV_3 : Uint256 := 860000000000000000
    LLTV_4 : Uint256 := 915000000000000000
    LLTV_5 : Uint256 := 945000000000000000
    LLTV_6 : Uint256 := 965000000000000000
    LLTV_7 : Uint256 := 980000000000000000
    LLTV_8 : Uint256 := 1000000000000000000

  constructor () := do
    let cid ← Verity.chainid
    let sender ← msgSender
    setStorage initialChainIdSlot cid
    setStorageAddr roleSetterSlot sender
    setStorageAddr feeSetterSlot sender
    setStorageAddr feeClaimerSlot sender
    setStorageAddr tickSpacingSetterSlot sender

  modifier onlyRoleSetter := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()

  modifier onlyFeeSetter := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    require (sender == currentFeeSetter) "OnlyFeeSetter"

  modifier onlyFeeClaimer := do
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    require (sender == currentFeeClaimer) "OnlyFeeClaimer"

  modifier onlyTickSpacingSetter := do
    let sender ← msgSender
    let currentTickSpacingSetter ← getStorageAddr tickSpacingSetterSlot
    require (sender == currentTickSpacingSetter) "OnlyTickSpacingSetter"

  function INITIAL_CHAIN_ID () : Uint256 := do
    let cid ← getStorage initialChainIdSlot
    return cid

  function roleSetter () : Address := do
    let value ← getStorageAddr roleSetterSlot
    return value

  function feeSetter () : Address := do
    let value ← getStorageAddr feeSetterSlot
    return value

  function feeClaimer () : Address := do
    let value ← getStorageAddr feeClaimerSlot
    return value

  function tickSpacingSetter () : Address := do
    let value ← getStorageAddr tickSpacingSetterSlot
    return value

  function consumed (user : Address, group : Bytes32) : Uint256 := do
    let value ← structMember2 "consumedSlot" user group "amount"
    return value

  function isAuthorized (authorizer : Address, authorized : Address) : Bool := do
    let value ← structMember2 "isAuthorizedSlot" authorizer authorized "flag"
    return value != ZERO

  function defaultSettlementFeeCbp (loanToken : Address, index : Uint256) : Uint256 := do
    let value ← structMember2 "defaultSettlementFeeSlot" loanToken index "fee"
    return value

  function defaultContinuousFee (loanToken : Address) : Uint256 := do
    let value ← getMapping defaultContinuousFeeSlot loanToken
    return value

  function claimableSettlementFee (token : Address) : Uint256 := do
    let value ← getMapping claimableSettlementFeeSlot token
    return value

  function maxSettlementFee (index : Uint256) : Uint256 := do
    let mut value := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      value := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      value := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      value := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      value := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      value := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      value := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    return value

  function isLltvAllowed (lltv : Uint256) : Bool := do
    return (lltv == LLTV_0 || lltv == LLTV_1 || lltv == LLTV_2 ||
      lltv == LLTV_3 || lltv == LLTV_4 || lltv == LLTV_5 ||
      lltv == LLTV_6 || lltv == LLTV_7 || lltv == LLTV_8)

  function maxLif (lltv : Uint256, cursor : Uint256) : Uint256 := do
    return mulDivDown WAD WAD (sub WAD (mulDivDown cursor (sub WAD lltv) WAD))

  function min (a : Uint256, b : Uint256) : Uint256 := do
    if a < b then
      return a
    else
      return b

  function validateCollateralParams (collateralParams : Array CollateralParams) : Unit := do
    let collateralCount := arrayLength collateralParams
    requireError (collateralCount > ZERO) NoCollateralParams()
    requireError (collateralCount <= MAX_COLLATERALS) TooManyCollateralParams()
    let mut previousCollateralToken := 0
    forEach "i" collateralCount (do
      let collateralToken := addressToWord (arrayElement collateralParams i).token
      requireError (collateralToken > previousCollateralToken) CollateralParamsNotSorted()
      let lltv := (arrayElement collateralParams i).lltv
      let allowed ← isLltvAllowed lltv
      requireError allowed LltvNotAllowed()
      let lowMaxLif ← maxLif lltv LIQUIDATION_CURSOR_LOW
      let highMaxLif ← maxLif lltv LIQUIDATION_CURSOR_HIGH
      let lif := (arrayElement collateralParams i).maxLif
      requireError (lif == lowMaxLif || lif == highMaxLif) InvalidMaxLif()
      previousCollateralToken := collateralToken)

  function countBits128 (bitmap : Uint256) : Uint256 := do
    let mut count := ZERO
    forEach "i" MAX_COLLATERALS (do
      let mask := shl i ONE
      if bitAnd bitmap mask != ZERO then
        count := add count ONE
      else
        pure ())
    return count

  function toId (market : Market) : Bytes32 := do
    /- Scaffold-only market id. Full parity still requires the Solidity
       `IdLib.toId` preimage: SSTORE2 prefix + `abi.encode(market)`. -/
    return market.maturity

  function toMarket (id : Bytes32) : Unit := do
    ecmDo Compiler.Modules.Create2SSTORE2.readCodeModule
      [addressToWord (wordToAddress id), ZERO, ZERO, ZERO]
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()

  function position (id : Bytes32, user : Address) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let debt ← structMember2 "positionSlot" id user "debt"
    let collateralBitmap ← structMember2 "positionSlot" id user "collateralBitmap"
    return (credit, pendingFee, lastLossFactor, lastAccrual, debt, collateralBitmap)

  function marketState (id : Bytes32) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let totalUnits ← structMember "marketStateSlot" id "totalUnits"
    let lossFactor ← structMember "marketStateSlot" id "lossFactor"
    let withdrawable ← structMember "marketStateSlot" id "withdrawable"
    let continuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    let continuousFee ← structMember "marketStateSlot" id "continuousFee"
    let tickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    return (totalUnits, lossFactor, withdrawable, continuousFeeCredit,
      settlementFeeCbp0, settlementFeeCbp1, settlementFeeCbp2, settlementFeeCbp3,
      settlementFeeCbp4, settlementFeeCbp5, settlementFeeCbp6, continuousFee,
      tickSpacing)

  function updatePositionView (market : Market, id : Bytes32, user : Address) :
      Tuple [Uint256, Uint256, Uint256] := do
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown credit
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if credit > ZERO then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    return (sub postSlashCredit accrued, sub postSlashPendingFee accrued, accrued)

  function allow_post_interaction_writes updatePosition (market : Market, user : Address) :
      Tuple [Uint256, Uint256, Uint256] := do
    let id ← toId market
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown credit
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if credit > ZERO then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    let newCredit := sub postSlashCredit accrued
    let newPendingFee := sub postSlashPendingFee accrued
    setStructMember2 "positionSlot" id user "credit" newCredit
    setStructMember2 "positionSlot" id user "lastLossFactor" marketLossFactor
    setStructMember2 "positionSlot" id user "pendingFee" newPendingFee
    setStructMember2 "positionSlot" id user "lastAccrual" now
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (add currentContinuousFeeCredit accrued)
    return (newCredit, newPendingFee, accrued)

  function setRoleSetter (newRoleSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr roleSetterSlot newRoleSetter

  function setFeeSetter (newFeeSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr feeSetterSlot newFeeSetter

  function setFeeClaimer (newFeeClaimer : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr feeClaimerSlot newFeeClaimer

  function setTickSpacingSetter (newTickSpacingSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr tickSpacingSetterSlot newTickSpacingSetter

  function setIsAuthorized (authorized : Address, newIsAuthorized : Bool, onBehalf : Address) :
      Unit := do
    let sender ← msgSender
    let currentAuth ← structMember2 "isAuthorizedSlot" onBehalf sender "flag"
    requireError (sender == onBehalf || currentAuth != ZERO) Unauthorized()
    let mut flag := ZERO
    if newIsAuthorized then
      flag := 1
    else
      flag := ZERO
    setStructMember2 "isAuthorizedSlot" onBehalf authorized "flag" flag

  function setConsumed (group : Bytes32, amount : Uint256, onBehalf : Address) : Unit := do
    let sender ← msgSender
    let currentAuth ← structMember2 "isAuthorizedSlot" onBehalf sender "flag"
    requireError (sender == onBehalf || currentAuth != ZERO) Unauthorized()
    let current ← structMember2 "consumedSlot" onBehalf group "amount"
    requireError (amount >= current) AlreadyConsumed()
    setStructMember2 "consumedSlot" onBehalf group "amount" amount

  function setDefaultSettlementFee
      (loanToken : Address, index : Uint256, newSettlementFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (index < 7) InvalidFeeIndex()
    let mut maxFee := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      maxFee := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      maxFee := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      maxFee := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      maxFee := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      maxFee := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      maxFee := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    requireError (newSettlementFee <= maxFee) SettlementFeeTooHigh()
    requireError (mod newSettlementFee CBP == ZERO) FeeNotMultipleOfFeeCbp()
    setStructMember2 "defaultSettlementFeeSlot" loanToken index "fee" (div newSettlementFee CBP)

  function setDefaultContinuousFee (loanToken : Address, newContinuousFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (newContinuousFee <= MAX_CONTINUOUS_FEE) ContinuousFeeTooHigh()
    setMapping defaultContinuousFeeSlot loanToken newContinuousFee

  function claimSettlementFee (token : Address, amount : Uint256, receiver : Address)
      : Unit := do
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    requireError (sender == currentFeeClaimer) OnlyFeeClaimer()
    let claimable ← getMapping claimableSettlementFeeSlot token
    requireError (amount <= claimable) ConsumedAssets()
    setMapping claimableSettlementFeeSlot token (sub claimable amount)
    safeTransfer token receiver amount

  function allow_post_interaction_writes claimContinuousFee
      (market : Market, amount : Uint256, receiver : Address)
      : Unit := do
    let id ← toId market
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    requireError (sender == currentFeeClaimer) OnlyFeeClaimer()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (sub currentContinuousFeeCredit amount)
    let currentTotalUnits ← structMember "marketStateSlot" id "totalUnits"
    setStructMember "marketStateSlot" id "totalUnits" (sub currentTotalUnits amount)
    let currentWithdrawable ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (sub currentWithdrawable amount)
    safeTransfer market.loanToken receiver amount

  function setMarketTickSpacing (id : Bytes32, newTickSpacing : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentTickSpacingSetter ← getStorageAddr tickSpacingSetterSlot
    requireError (sender == currentTickSpacingSetter) OnlyTickSpacingSetter()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    requireError (newTickSpacing > ZERO) InvalidTickSpacing()
    requireError (mod currentTickSpacing newTickSpacing == ZERO) InvalidTickSpacing()
    setStructMember "marketStateSlot" id "tickSpacing" newTickSpacing

  function setMarketSettlementFee
      (id : Bytes32, index : Uint256, newSettlementFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (index < 7) InvalidFeeIndex()
    let mut maxFee := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      maxFee := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      maxFee := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      maxFee := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      maxFee := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      maxFee := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      maxFee := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    requireError (newSettlementFee <= maxFee) SettlementFeeTooHigh()
    requireError (mod newSettlementFee CBP == ZERO) FeeNotMultipleOfFeeCbp()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let newSettlementFeeCbp := div newSettlementFee CBP
    if index == ZERO then
      setStructMember "marketStateSlot" id "settlementFeeCbp0" newSettlementFeeCbp
    else
      pure ()
    if index == ONE then
      setStructMember "marketStateSlot" id "settlementFeeCbp1" newSettlementFeeCbp
    else
      pure ()
    if index == TWO then
      setStructMember "marketStateSlot" id "settlementFeeCbp2" newSettlementFeeCbp
    else
      pure ()
    if index == THREE then
      setStructMember "marketStateSlot" id "settlementFeeCbp3" newSettlementFeeCbp
    else
      pure ()
    if index == FOUR then
      setStructMember "marketStateSlot" id "settlementFeeCbp4" newSettlementFeeCbp
    else
      pure ()
    if index == FIVE then
      setStructMember "marketStateSlot" id "settlementFeeCbp5" newSettlementFeeCbp
    else
      pure ()
    if index == SIX then
      setStructMember "marketStateSlot" id "settlementFeeCbp6" newSettlementFeeCbp
    else
      pure ()

  function setMarketContinuousFee (id : Bytes32, newContinuousFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (newContinuousFee <= MAX_CONTINUOUS_FEE) ContinuousFeeTooHigh()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    setStructMember "marketStateSlot" id "continuousFee" newContinuousFee

  function allow_post_interaction_writes touchMarket (market : Market) : Bytes32 := do
    let id ← toId market
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    if currentTickSpacing == ZERO then
      let now ← blockTimestamp
      requireError (market.maturity <= add now HUNDRED_YEARS) MaturityTooFar()
      validateCollateralParams market.collateralParams
      let salt ← getStorage initialChainIdSlot
      let _marketPointer ← ecmCall Compiler.Modules.Create2SSTORE2.deployModule
        [ZERO, ZERO, ZERO, salt]
      setStructMember "marketStateSlot" id "tickSpacing" DEFAULT_TICK_SPACING
      let settlementFeeCbp0 ← defaultSettlementFeeCbp market.loanToken ZERO
      let settlementFeeCbp1 ← defaultSettlementFeeCbp market.loanToken ONE
      let settlementFeeCbp2 ← defaultSettlementFeeCbp market.loanToken TWO
      let settlementFeeCbp3 ← defaultSettlementFeeCbp market.loanToken THREE
      let settlementFeeCbp4 ← defaultSettlementFeeCbp market.loanToken FOUR
      let settlementFeeCbp5 ← defaultSettlementFeeCbp market.loanToken FIVE
      let settlementFeeCbp6 ← defaultSettlementFeeCbp market.loanToken SIX
      let continuous ← getMapping defaultContinuousFeeSlot market.loanToken
      setStructMember "marketStateSlot" id "settlementFeeCbp0" settlementFeeCbp0
      setStructMember "marketStateSlot" id "settlementFeeCbp1" settlementFeeCbp1
      setStructMember "marketStateSlot" id "settlementFeeCbp2" settlementFeeCbp2
      setStructMember "marketStateSlot" id "settlementFeeCbp3" settlementFeeCbp3
      setStructMember "marketStateSlot" id "settlementFeeCbp4" settlementFeeCbp4
      setStructMember "marketStateSlot" id "settlementFeeCbp5" settlementFeeCbp5
      setStructMember "marketStateSlot" id "settlementFeeCbp6" settlementFeeCbp6
      setStructMember "marketStateSlot" id "continuousFee" continuous
    else
      pure ()
    return id

  function settlementFee (id : Bytes32, timeToMaturity : Uint256) : Uint256 := do
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    let mut start := ONE_EIGHTY_DAYS
    let mut finish := THREE_SIXTY_DAYS
    let mut feeLower := mul settlementFeeCbp5 CBP
    let mut feeUpper := mul settlementFeeCbp6 CBP
    if timeToMaturity < ONE_DAY then
      start := ZERO
      finish := ONE_DAY
      feeLower := mul settlementFeeCbp0 CBP
      feeUpper := mul settlementFeeCbp1 CBP
    else
      pure ()
    if timeToMaturity >= ONE_DAY && timeToMaturity < SEVEN_DAYS then
      start := ONE_DAY
      finish := SEVEN_DAYS
      feeLower := mul settlementFeeCbp1 CBP
      feeUpper := mul settlementFeeCbp2 CBP
    else
      pure ()
    if timeToMaturity >= SEVEN_DAYS && timeToMaturity < THIRTY_DAYS then
      start := SEVEN_DAYS
      finish := THIRTY_DAYS
      feeLower := mul settlementFeeCbp2 CBP
      feeUpper := mul settlementFeeCbp3 CBP
    else
      pure ()
    if timeToMaturity >= THIRTY_DAYS && timeToMaturity < NINETY_DAYS then
      start := THIRTY_DAYS
      finish := NINETY_DAYS
      feeLower := mul settlementFeeCbp3 CBP
      feeUpper := mul settlementFeeCbp4 CBP
    else
      pure ()
    if timeToMaturity >= NINETY_DAYS && timeToMaturity < ONE_EIGHTY_DAYS then
      start := NINETY_DAYS
      finish := ONE_EIGHTY_DAYS
      feeLower := mul settlementFeeCbp4 CBP
      feeUpper := mul settlementFeeCbp5 CBP
    else
      pure ()
    if timeToMaturity >= THREE_SIXTY_DAYS then
      return mul settlementFeeCbp6 CBP
    else
      return div
        (add
          (mul feeLower (sub finish timeToMaturity))
          (mul feeUpper (sub timeToMaturity start)))
        (sub finish start)

  function allow_post_interaction_writes take
      (offer : Offer, ratifierData : Bytes, units : Uint256, taker : Address,
        receiverIfTakerIsSeller : Address, takerCallback : Address,
        takerCallbackData : Bytes)
      local_obligations [take_calldata_market_scalars := assumed
        "Temporary Midnight scaffold reads loanToken and maturity from calldata because nested dynamic struct projection is not yet supported by the Verity contract macro."] :
      Tuple [Uint256, Uint256] := do
    let sender ← msgSender
    let authorized ← isAuthorized taker sender
    requireError (taker == sender || authorized) TakerUnauthorized()
    let offerBase := add (calldataload 4) 4
    let marketBase := add offerBase (calldataload offerBase)
    let loanToken := wordToAddress (calldataload marketBase)
    let maturity := calldataload (add marketBase 64)
    let id := maturity
    let currentMarketTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    if currentMarketTickSpacing == ZERO then
      let now ← blockTimestamp
      requireError (maturity <= add now HUNDRED_YEARS) MaturityTooFar()
      setStructMember "marketStateSlot" id "tickSpacing" DEFAULT_TICK_SPACING
      let settlementFeeCbp0 ← defaultSettlementFeeCbp loanToken ZERO
      let settlementFeeCbp1 ← defaultSettlementFeeCbp loanToken ONE
      let settlementFeeCbp2 ← defaultSettlementFeeCbp loanToken TWO
      let settlementFeeCbp3 ← defaultSettlementFeeCbp loanToken THREE
      let settlementFeeCbp4 ← defaultSettlementFeeCbp loanToken FOUR
      let settlementFeeCbp5 ← defaultSettlementFeeCbp loanToken FIVE
      let settlementFeeCbp6 ← defaultSettlementFeeCbp loanToken SIX
      let continuous ← getMapping defaultContinuousFeeSlot loanToken
      setStructMember "marketStateSlot" id "settlementFeeCbp0" settlementFeeCbp0
      setStructMember "marketStateSlot" id "settlementFeeCbp1" settlementFeeCbp1
      setStructMember "marketStateSlot" id "settlementFeeCbp2" settlementFeeCbp2
      setStructMember "marketStateSlot" id "settlementFeeCbp3" settlementFeeCbp3
      setStructMember "marketStateSlot" id "settlementFeeCbp4" settlementFeeCbp4
      setStructMember "marketStateSlot" id "settlementFeeCbp5" settlementFeeCbp5
      setStructMember "marketStateSlot" id "settlementFeeCbp6" settlementFeeCbp6
      setStructMember "marketStateSlot" id "continuousFee" continuous
    else
      pure ()
    let lossFactorValue ← structMember "marketStateSlot" id "lossFactor"
    requireError (lossFactorValue < MAX_LOSS_FACTOR)
      MarketLossFactorMaxedOut()
    requireError (offer.maxAssets == ZERO || offer.maxUnits == ZERO) MultipleNonZero()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (mod offer.tick currentTickSpacing == ZERO) TickNotAccessible()
    let now ← blockTimestamp
    requireError (now >= offer.start) OfferNotStarted()
    requireError (now <= offer.expiry) OfferExpired()
    requireError (offer.maker != taker) SelfTake()
    let ratifierAuthorized ← isAuthorized offer.maker offer.ratifier
    requireError ratifierAuthorized RatifierUnauthorized()
    requireError (units <= MAX_LOSS_FACTOR) CastOverflow()

    let mut newConsumed := ZERO
    if offer.maxAssets > ZERO then
      let currentConsumed ← structMember2 "consumedSlot" offer.maker offer.group "amount"
      newConsumed := add currentConsumed units
      requireError (newConsumed <= offer.maxAssets) ConsumedAssets()
      setStructMember2 "consumedSlot" offer.maker offer.group "amount" newConsumed
    else
      let currentConsumed ← structMember2 "consumedSlot" offer.maker offer.group "amount"
      newConsumed := add currentConsumed units
      requireError (newConsumed <= offer.maxUnits) ConsumedUnits()
      setStructMember2 "consumedSlot" offer.maker offer.group "amount" newConsumed

    let mut buyer := taker
    let mut seller := offer.maker
    if offer.buy then
      buyer := offer.maker
      seller := taker
    else
      pure ()

    let buyerDebt ← structMember2 "positionSlot" id buyer "debt"
    let buyerDebtDecrease ← min units buyerDebt
    let buyerCreditIncrease := sub units buyerDebtDecrease
    let continuousFeeValue ← structMember "marketStateSlot" id "continuousFee"
    let mut timeToMaturity := ZERO
    if maturity > now then
      timeToMaturity := sub maturity now
    else
      pure ()
    let buyerPendingFeeIncrease :=
      mulDivDown buyerCreditIncrease (mul continuousFeeValue timeToMaturity) WAD
    let buyerCredit ← structMember2 "positionSlot" id buyer "credit"
    let buyerPendingFee ← structMember2 "positionSlot" id buyer "pendingFee"
    let buyerLastLossFactor ← structMember2 "positionSlot" id buyer "lastLossFactor"
    let buyerLastAccrual ← structMember2 "positionSlot" id buyer "lastAccrual"
    let mut buyerPostSlashCredit := ZERO
    if buyerLastLossFactor < MAX_LOSS_FACTOR then
      buyerPostSlashCredit := mulDivDown buyerCredit
        (sub MAX_LOSS_FACTOR lossFactorValue)
        (sub MAX_LOSS_FACTOR buyerLastLossFactor)
    else
      pure ()
    let mut buyerPostSlashPendingFee := ZERO
    if buyerCredit > ZERO then
      buyerPostSlashPendingFee := sub buyerPendingFee
        (mulDivUp buyerPendingFee (sub buyerCredit buyerPostSlashCredit) buyerCredit)
    else
      pure ()
    let mut buyerAccrualEnd := maturity
    if now <= maturity then
      buyerAccrualEnd := now
    else
      pure ()
    let mut buyerAccrued := ZERO
    if buyerLastAccrual < maturity then
      buyerAccrued := mulDivDown buyerPostSlashPendingFee
        (sub buyerAccrualEnd buyerLastAccrual)
        (sub maturity buyerLastAccrual)
    else
      pure ()
    let buyerCreditAfterUpdate := sub buyerPostSlashCredit buyerAccrued
    let buyerPendingFeeAfterUpdate := sub buyerPostSlashPendingFee buyerAccrued
    if buyerCredit > ZERO || buyerCreditIncrease > ZERO then
      setStructMember2 "positionSlot" id buyer "credit" buyerCreditAfterUpdate
      setStructMember2 "positionSlot" id buyer "lastLossFactor" lossFactorValue
      setStructMember2 "positionSlot" id buyer "pendingFee" buyerPendingFeeAfterUpdate
      setStructMember2 "positionSlot" id buyer "lastAccrual" now
      let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        (add currentContinuousFeeCredit buyerAccrued)
    else
      pure ()
    setStructMember2 "positionSlot" id buyer "debt" (sub buyerDebt buyerDebtDecrease)
    setStructMember2 "positionSlot" id buyer "pendingFee"
      (add buyerPendingFeeAfterUpdate buyerPendingFeeIncrease)
    setStructMember2 "positionSlot" id buyer "credit"
      (add buyerCreditAfterUpdate buyerCreditIncrease)

    let sellerCredit ← structMember2 "positionSlot" id seller "credit"
    let sellerPendingFee ← structMember2 "positionSlot" id seller "pendingFee"
    let sellerLastLossFactor ← structMember2 "positionSlot" id seller "lastLossFactor"
    let sellerLastAccrual ← structMember2 "positionSlot" id seller "lastAccrual"
    let mut sellerPostSlashCredit := ZERO
    if sellerLastLossFactor < MAX_LOSS_FACTOR then
      sellerPostSlashCredit := mulDivDown sellerCredit
        (sub MAX_LOSS_FACTOR lossFactorValue)
        (sub MAX_LOSS_FACTOR sellerLastLossFactor)
    else
      pure ()
    let mut sellerPostSlashPendingFee := ZERO
    if sellerCredit > ZERO then
      sellerPostSlashPendingFee := sub sellerPendingFee
        (mulDivUp sellerPendingFee (sub sellerCredit sellerPostSlashCredit)
          sellerCredit)
    else
      pure ()
    let mut sellerAccrualEnd := maturity
    if now <= maturity then
      sellerAccrualEnd := now
    else
      pure ()
    let mut sellerAccrued := ZERO
    if sellerLastAccrual < maturity then
      sellerAccrued := mulDivDown sellerPostSlashPendingFee
        (sub sellerAccrualEnd sellerLastAccrual)
        (sub maturity sellerLastAccrual)
    else
      pure ()
    let sellerCreditAfterUpdate := sub sellerPostSlashCredit sellerAccrued
    let sellerPendingFeeAfterUpdate := sub sellerPostSlashPendingFee sellerAccrued
    if sellerCredit > ZERO then
      setStructMember2 "positionSlot" id seller "credit" sellerCreditAfterUpdate
      setStructMember2 "positionSlot" id seller "lastLossFactor" lossFactorValue
      setStructMember2 "positionSlot" id seller "pendingFee" sellerPendingFeeAfterUpdate
      setStructMember2 "positionSlot" id seller "lastAccrual" now
      let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        (add currentContinuousFeeCredit sellerAccrued)
    else
      pure ()
    let sellerCreditDecrease ← min units sellerCreditAfterUpdate
    let sellerDebtIncrease := sub units sellerCreditDecrease
    let sellerDebt ← structMember2 "positionSlot" id seller "debt"
    let mut sellerPendingFeeDecrease := ZERO
    if sellerCreditAfterUpdate > ZERO then
      sellerPendingFeeDecrease :=
        mulDivUp sellerPendingFeeAfterUpdate sellerCreditDecrease sellerCreditAfterUpdate
    else
      pure ()
    requireError (now <= maturity || sellerDebtIncrease == ZERO)
      CannotIncreaseDebtPostMaturity()
    let mut reduceOnlyAllowed := true
    if offer.reduceOnly then
      if offer.buy then
        reduceOnlyAllowed := buyerCreditIncrease == ZERO
      else
        reduceOnlyAllowed := sellerDebtIncrease == ZERO
    else
      pure ()
    requireError reduceOnlyAllowed MakerCreditOrDebtIncreased()
    setStructMember2 "positionSlot" id seller "pendingFee"
      (sub sellerPendingFeeAfterUpdate sellerPendingFeeDecrease)
    setStructMember2 "positionSlot" id seller "credit"
      (sub sellerCreditAfterUpdate sellerCreditDecrease)
    setStructMember2 "positionSlot" id seller "debt" (add sellerDebt sellerDebtIncrease)

    let currentTotalUnits ← structMember "marketStateSlot" id "totalUnits"
    let mut newTotalUnits := add currentTotalUnits buyerCreditIncrease
    newTotalUnits := sub newTotalUnits sellerCreditDecrease
    setStructMember "marketStateSlot" id "totalUnits" newTotalUnits

    let mut receiver := offer.receiverIfMakerIsSeller
    if offer.buy then
      receiver := receiverIfTakerIsSeller
    else
      pure ()
    let mut payer := buyer
    if offer.buy then
      pure ()
    else
      payer := sender
    let mut transferAssets := units
    if offer.buy then
      transferAssets := ZERO
    else
      pure ()
    safeTransferFrom loanToken payer receiver transferAssets
    return (units, units)

  function creditOf (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "credit"
    return value

  function debtOf (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "debt"
    return value

  function totalUnits (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "totalUnits"
    return value

  function lossFactor (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "lossFactor"
    return value

  function tickSpacing (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "tickSpacing"
    return value

  function settlementFeeCbps (id : Bytes32) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    return (settlementFeeCbp0, settlementFeeCbp1, settlementFeeCbp2, settlementFeeCbp3,
      settlementFeeCbp4, settlementFeeCbp5, settlementFeeCbp6)

  function withdrawable (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "withdrawable"
    return value

  function continuousFee (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "continuousFee"
    return value

  function continuousFeeCredit (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "continuousFeeCredit"
    return value

  function allow_post_interaction_writes withdraw
      (market : Market, units : Uint256, onBehalf : Address,
        receiver : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let creditBeforeUpdate ← structMember2 "positionSlot" id onBehalf "credit"
    let pendingFeeBeforeUpdate ← structMember2 "positionSlot" id onBehalf "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id onBehalf "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id onBehalf "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown creditBeforeUpdate
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if creditBeforeUpdate > ZERO then
      postSlashPendingFee := sub pendingFeeBeforeUpdate
        (mulDivUp pendingFeeBeforeUpdate (sub creditBeforeUpdate postSlashCredit)
          creditBeforeUpdate)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    let creditAfterUpdate := sub postSlashCredit accrued
    let pendingFeeAfterUpdate := sub postSlashPendingFee accrued
    setStructMember2 "positionSlot" id onBehalf "credit" creditAfterUpdate
    setStructMember2 "positionSlot" id onBehalf "lastLossFactor" marketLossFactor
    setStructMember2 "positionSlot" id onBehalf "pendingFee" pendingFeeAfterUpdate
    setStructMember2 "positionSlot" id onBehalf "lastAccrual" now
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (add currentContinuousFeeCredit accrued)
    let mut pendingFeeDecrease := ZERO
    if creditAfterUpdate > ZERO then
      pendingFeeDecrease := mulDivUp pendingFeeAfterUpdate units creditAfterUpdate
    else
      pure ()
    setStructMember2 "positionSlot" id onBehalf "pendingFee"
      (sub pendingFeeAfterUpdate pendingFeeDecrease)
    let credit ← structMember2 "positionSlot" id onBehalf "credit"
    setStructMember2 "positionSlot" id onBehalf "credit" (sub credit units)
    let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (sub withdrawableAmount units)
    let total ← structMember "marketStateSlot" id "totalUnits"
    setStructMember "marketStateSlot" id "totalUnits" (sub total units)
    safeTransfer market.loanToken receiver units

  function allow_post_interaction_writes repay
      (market : Market, units : Uint256, onBehalf : Address,
        callback : Address, data : Bytes) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let debt ← structMember2 "positionSlot" id onBehalf "debt"
    setStructMember2 "positionSlot" id onBehalf "debt" (sub debt units)
    let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (add withdrawableAmount units)
    let mut payer := sender
    if callback != 0 then
      payer := callback
      ecmDo (Compiler.Modules.Callbacks.callbackModule 0xfc56f72e 3 "data")
        [addressToWord callback, id, units, addressToWord onBehalf]
    else
      pure ()
    let self ← contractAddress
    safeTransferFrom market.loanToken payer self units

  function collateralTokenAt
      (collateralParams : Array CollateralParams, index : Uint256) : Address := do
    return (arrayElement collateralParams index).token

  function collateralLltvAt
      (collateralParams : Array CollateralParams, index : Uint256) : Uint256 := do
    return (arrayElement collateralParams index).lltv

  function collateralMaxLifAt
      (collateralParams : Array CollateralParams, index : Uint256) : Uint256 := do
    return (arrayElement collateralParams index).maxLif

  function collateralOracleAt
      (collateralParams : Array CollateralParams, index : Uint256) : Address := do
    return (arrayElement collateralParams index).oracle

  function oraclePrice (oracle : Address)
      : Uint256 := do
    let price ← ecmCall
      (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
      [oracle]
    return price

  function liquidatorGateCanLiquidate (gate : Address, account : Address)
      : Uint256 := do
    let allowed ← ecmCall
      (fun resultVar => Compiler.Modules.Calls.withReturnModule resultVar 0xb9f4ff55 1 true)
      [gate, addressToWord account]
    return allowed

  function liquidatorGateCanLiquidateOrDefault (gate : Address, account : Address)
      : Uint256 := do
    let mut allowed := ONE
    if gate != 0 then
      let loaded ← liquidatorGateCanLiquidate gate account
      allowed := loaded
    else
      pure ()
    return allowed

  function collateralAmount (id : Bytes32, user : Address, index : Uint256) : Uint256 := do
    let mut value := ZERO
    if index == 0 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[0]"
      value := loaded
    else
      pure ()
    if index == 1 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[1]"
      value := loaded
    else
      pure ()
    if index == 2 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[2]"
      value := loaded
    else
      pure ()
    if index == 3 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[3]"
      value := loaded
    else
      pure ()
    if index == 4 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[4]"
      value := loaded
    else
      pure ()
    if index == 5 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[5]"
      value := loaded
    else
      pure ()
    if index == 6 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[6]"
      value := loaded
    else
      pure ()
    if index == 7 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[7]"
      value := loaded
    else
      pure ()
    if index == 8 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[8]"
      value := loaded
    else
      pure ()
    if index == 9 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[9]"
      value := loaded
    else
      pure ()
    if index == 10 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[10]"
      value := loaded
    else
      pure ()
    if index == 11 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[11]"
      value := loaded
    else
      pure ()
    if index == 12 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[12]"
      value := loaded
    else
      pure ()
    if index == 13 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[13]"
      value := loaded
    else
      pure ()
    if index == 14 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[14]"
      value := loaded
    else
      pure ()
    if index == 15 then
      let loaded ← structMember2 "collateralSlot" id user "amounts[15]"
      value := loaded
    else
      pure ()
    return value

  function allow_post_interaction_writes writeCollateralAmount
      (id : Bytes32, user : Address, index : Uint256, value : Uint256) : Unit := do
    if index == 0 then
      setStructMember2 "collateralSlot" id user "amounts[0]" value
    else
      pure ()
    if index == 1 then
      setStructMember2 "collateralSlot" id user "amounts[1]" value
    else
      pure ()
    if index == 2 then
      setStructMember2 "collateralSlot" id user "amounts[2]" value
    else
      pure ()
    if index == 3 then
      setStructMember2 "collateralSlot" id user "amounts[3]" value
    else
      pure ()
    if index == 4 then
      setStructMember2 "collateralSlot" id user "amounts[4]" value
    else
      pure ()
    if index == 5 then
      setStructMember2 "collateralSlot" id user "amounts[5]" value
    else
      pure ()
    if index == 6 then
      setStructMember2 "collateralSlot" id user "amounts[6]" value
    else
      pure ()
    if index == 7 then
      setStructMember2 "collateralSlot" id user "amounts[7]" value
    else
      pure ()
    if index == 8 then
      setStructMember2 "collateralSlot" id user "amounts[8]" value
    else
      pure ()
    if index == 9 then
      setStructMember2 "collateralSlot" id user "amounts[9]" value
    else
      pure ()
    if index == 10 then
      setStructMember2 "collateralSlot" id user "amounts[10]" value
    else
      pure ()
    if index == 11 then
      setStructMember2 "collateralSlot" id user "amounts[11]" value
    else
      pure ()
    if index == 12 then
      setStructMember2 "collateralSlot" id user "amounts[12]" value
    else
      pure ()
    if index == 13 then
      setStructMember2 "collateralSlot" id user "amounts[13]" value
    else
      pure ()
    if index == 14 then
      setStructMember2 "collateralSlot" id user "amounts[14]" value
    else
      pure ()
    if index == 15 then
      setStructMember2 "collateralSlot" id user "amounts[15]" value
    else
      pure ()

  function allow_post_interaction_writes liquidate
      (market : Market, collateralIndex : Uint256, seizedAssets : Uint256,
        repaidUnits : Uint256, borrower : Address, postMaturityMode : Bool,
        receiver : Address, callback : Address, data : Bytes) :
      Tuple [Uint256, Uint256] := do
    let sender ← msgSender
    let id ← toId market
    let debtLoaded ← structMember2 "positionSlot" id borrower "debt"
    let mut debt := debtLoaded
    let totalUnitsValue ← structMember "marketStateSlot" id "totalUnits"
    requireError (seizedAssets == ZERO || repaidUnits == ZERO) InconsistentInput()
    requireError (debt > ZERO) NotBorrower()
    let canLiquidate ←
      liquidatorGateCanLiquidateOrDefault market.liquidatorGate sender
    requireError (market.liquidatorGate == 0 || canLiquidate != ZERO)
      LiquidatorGatedFromLiquidating()
    let collateralCount := arrayLength market.collateralParams
    require (collateralIndex < collateralCount) "collateral index out of bounds"
    let collateralBitmapValue ← structMember2 "positionSlot" id borrower "collateralBitmap"
    let collateralMask := shl collateralIndex ONE
    if seizedAssets > ZERO || repaidUnits > ZERO then
      require (bitAnd collateralBitmapValue collateralMask > ZERO) "inactive collateral"
    else
      pure ()
    let originalDebt := debt
    let mut maxDebtValue := ZERO
    let mut badDebt := originalDebt
    let mut liquidatedCollatPrice := ZERO
    forEach "i" collateralCount (do
      let mask := shl i ONE
      if bitAnd collateralBitmapValue mask > ZERO then
        let activeCollateral ← collateralAmount id borrower i
        let oracle ← collateralOracleAt market.collateralParams i
        let price ← oraclePrice oracle
        let lltv ← collateralLltvAt market.collateralParams i
        let maxLifValue ← collateralMaxLifAt market.collateralParams i
        if i == collateralIndex then
          liquidatedCollatPrice := price
        else
          pure ()
        let collateralDebtValue :=
          mulDivDown (mulDivDown activeCollateral price ORACLE_PRICE_SCALE) lltv WAD
        maxDebtValue := add maxDebtValue collateralDebtValue
        let repayable :=
          mulDivUp (mulDivUp activeCollateral price ORACLE_PRICE_SCALE) WAD maxLifValue
        if badDebt > repayable then
          badDebt := sub badDebt repayable
        else
          badDebt := ZERO
      else
        pure ())
    let now ← blockTimestamp
    if postMaturityMode then
      requireError (now > market.maturity) NotLiquidatable()
    else
      requireError (originalDebt > maxDebtValue) NotLiquidatable()
    if badDebt > ZERO then
      setStructMember2 "positionSlot" id borrower "debt" (sub debt badDebt)
      debt := sub debt badDebt
      let oldLossFactor ← structMember "marketStateSlot" id "lossFactor"
      let newLossFactor :=
        sub MAX_LOSS_FACTOR
          (mulDivDown (sub MAX_LOSS_FACTOR oldLossFactor)
            (sub totalUnitsValue badDebt)
            totalUnitsValue)
      setStructMember "marketStateSlot" id "lossFactor" newLossFactor
      setStructMember "marketStateSlot" id "totalUnits"
        (sub totalUnitsValue badDebt)
      let oldContinuousFeeCredit ←
        structMember "marketStateSlot" id "continuousFeeCredit"
      let mut newContinuousFeeCredit := ZERO
      if oldLossFactor < MAX_LOSS_FACTOR then
        newContinuousFeeCredit :=
          mulDivDown oldContinuousFeeCredit
            (sub MAX_LOSS_FACTOR newLossFactor)
            (sub MAX_LOSS_FACTOR oldLossFactor)
      else
        pure ()
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        newContinuousFeeCredit
    else
      pure ()
    let mut outSeizedAssets := seizedAssets
    let mut outRepaidUnits := repaidUnits
    if outRepaidUnits > ZERO || outSeizedAssets > ZERO then
      let maxLifValue ← collateralMaxLifAt market.collateralParams collateralIndex
      let mut lif := maxLifValue
      if postMaturityMode then
        let elapsed := sub now market.maturity
        let postMaturityLif := add WAD
          (div (mul (sub maxLifValue WAD) elapsed) TIME_TO_MAX_LIF)
        if maxLifValue <= postMaturityLif then
          lif := maxLifValue
        else
          lif := postMaturityLif
      else
        pure ()
      if outSeizedAssets > ZERO then
        outRepaidUnits :=
          mulDivUp (mulDivUp outSeizedAssets liquidatedCollatPrice ORACLE_PRICE_SCALE)
            WAD lif
      else
        outSeizedAssets :=
          mulDivDown (mulDivDown outRepaidUnits lif WAD)
            ORACLE_PRICE_SCALE liquidatedCollatPrice
      if postMaturityMode then
        pure ()
      else
        let lltv ← collateralLltvAt market.collateralParams collateralIndex
        let mut maxRepaidValue :=
          115792089237316195423570985008687907853269984665640564039457584007913129639935
        if lltv < WAD then
          maxRepaidValue :=
            mulDivUp (sub debt maxDebtValue)
              (mul WAD WAD)
              (sub (mul WAD WAD) (mul lif lltv))
        else
          pure ()
        let oldCollateralForRcf ← collateralAmount id borrower collateralIndex
        let collateralRepayCapacity :=
          mulDivDown
            (mulDivDown oldCollateralForRcf liquidatedCollatPrice ORACLE_PRICE_SCALE)
            WAD lif
        let mut capacityShortfall := ZERO
        if collateralRepayCapacity > maxRepaidValue then
          capacityShortfall := sub collateralRepayCapacity maxRepaidValue
        else
          pure ()
        requireError (outRepaidUnits <= maxRepaidValue ||
          capacityShortfall < market.rcfThreshold)
          RecoveryCloseFactorConditionsViolated()
      let oldCollateral ← collateralAmount id borrower collateralIndex
      let newCollateral := sub oldCollateral outSeizedAssets
      writeCollateralAmount id borrower collateralIndex newCollateral
      if newCollateral == ZERO then
        if outSeizedAssets > ZERO then
          let oldBitmap ← structMember2 "positionSlot" id borrower "collateralBitmap"
          let mask := shl collateralIndex ONE
          let newBitmap := bitAnd oldBitmap (bitNot mask)
          setStructMember2 "positionSlot" id borrower "collateralBitmap" newBitmap
        else
          pure ()
      else
        pure ()
      let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
      setStructMember "marketStateSlot" id "withdrawable"
        (add withdrawableAmount outRepaidUnits)
      setStructMember2 "positionSlot" id borrower "debt" (sub debt outRepaidUnits)
    else
      pure ()
    let collateralToken ← collateralTokenAt market.collateralParams collateralIndex
    safeTransfer collateralToken receiver outSeizedAssets
    let mut payer := sender
    if callback != 0 then
      payer := callback
      ecmDo (Compiler.Modules.Callbacks.callbackModule 0x6861b795 8 "data")
        [addressToWord callback, addressToWord sender, id, collateralIndex,
          outSeizedAssets, outRepaidUnits, addressToWord borrower,
          addressToWord receiver, badDebt]
    else
      pure ()
    let self ← contractAddress
    safeTransferFrom market.loanToken payer self outRepaidUnits
    return (outSeizedAssets, outRepaidUnits)

  function isHealthy (market : Market, id : Bytes32, borrower : Address) : Bool := do
    let debt ← structMember2 "positionSlot" id borrower "debt"
    if debt == ZERO then
      return true
    else
      pure ()
    let collateralValue ← collateralAmount id borrower ZERO
    let lltv ← collateralLltvAt market.collateralParams ZERO
    let maxDebt := mulDivDown collateralValue lltv WAD
    return debt <= maxDebt

  function allow_post_interaction_writes setCollateralAmount
      (id : Bytes32, user : Address, index : Uint256, value : Uint256) : Unit := do
    if index == 0 then
      setStructMember2 "collateralSlot" id user "amounts[0]" value
    else
      pure ()
    if index == 1 then
      setStructMember2 "collateralSlot" id user "amounts[1]" value
    else
      pure ()
    if index == 2 then
      setStructMember2 "collateralSlot" id user "amounts[2]" value
    else
      pure ()
    if index == 3 then
      setStructMember2 "collateralSlot" id user "amounts[3]" value
    else
      pure ()
    if index == 4 then
      setStructMember2 "collateralSlot" id user "amounts[4]" value
    else
      pure ()
    if index == 5 then
      setStructMember2 "collateralSlot" id user "amounts[5]" value
    else
      pure ()
    if index == 6 then
      setStructMember2 "collateralSlot" id user "amounts[6]" value
    else
      pure ()
    if index == 7 then
      setStructMember2 "collateralSlot" id user "amounts[7]" value
    else
      pure ()
    if index == 8 then
      setStructMember2 "collateralSlot" id user "amounts[8]" value
    else
      pure ()
    if index == 9 then
      setStructMember2 "collateralSlot" id user "amounts[9]" value
    else
      pure ()
    if index == 10 then
      setStructMember2 "collateralSlot" id user "amounts[10]" value
    else
      pure ()
    if index == 11 then
      setStructMember2 "collateralSlot" id user "amounts[11]" value
    else
      pure ()
    if index == 12 then
      setStructMember2 "collateralSlot" id user "amounts[12]" value
    else
      pure ()
    if index == 13 then
      setStructMember2 "collateralSlot" id user "amounts[13]" value
    else
      pure ()
    if index == 14 then
      setStructMember2 "collateralSlot" id user "amounts[14]" value
    else
      pure ()
    if index == 15 then
      setStructMember2 "collateralSlot" id user "amounts[15]" value
    else
      pure ()

  function allow_post_interaction_writes supplyCollateral
      (market : Market, collateralIndex : Uint256, assets : Uint256,
        onBehalf : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let oldCollateral ← collateralAmount id onBehalf collateralIndex
    let newCollateral := add oldCollateral assets
    requireError (newCollateral <= MAX_LOSS_FACTOR) CastOverflow()
    let oldBitmap ← structMember2 "positionSlot" id onBehalf "collateralBitmap"
    let mask := shl collateralIndex ONE
    if oldCollateral == ZERO then
      if assets > ZERO then
        let newBitmap := bitOr oldBitmap mask
        setStructMember2 "positionSlot" id onBehalf "collateralBitmap" newBitmap
        let activeCount ← countBits128 newBitmap
        requireError (activeCount <= MAX_COLLATERALS_PER_BORROWER) TooManyActivatedCollaterals()
      else
        pure ()
    else
      pure ()
    let collateralToken ← collateralTokenAt market.collateralParams collateralIndex
    let self ← contractAddress
    safeTransferFrom collateralToken sender self assets
    setCollateralAmount id onBehalf collateralIndex newCollateral

  function allow_post_interaction_writes withdrawCollateral
      (market : Market, collateralIndex : Uint256, assets : Uint256,
        onBehalf : Address, receiver : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let oldCollateral ← collateralAmount id onBehalf collateralIndex
    let newCollateral := sub oldCollateral assets
    let debt ← structMember2 "positionSlot" id onBehalf "debt"
    if debt > ZERO then
      let lltv ← collateralLltvAt market.collateralParams collateralIndex
      let requiredCollateral := mulDivUp debt 1000000000000000000 lltv
      requireError (newCollateral >= requiredCollateral) UnhealthyBorrower()
    else
      pure ()
    if newCollateral == ZERO then
      if assets > ZERO then
        let oldBitmap ← structMember2 "positionSlot" id onBehalf "collateralBitmap"
        let mask := shl collateralIndex ONE
        let newBitmap := bitAnd oldBitmap (bitNot mask)
        setStructMember2 "positionSlot" id onBehalf "collateralBitmap" newBitmap
      else
        pure ()
    else
      pure ()
    let collateralToken ← collateralTokenAt market.collateralParams collateralIndex
    safeTransfer collateralToken receiver assets
    setCollateralAmount id onBehalf collateralIndex newCollateral

  function allow_post_interaction_writes flashLoan
      (tokens : Array Address, assets : Array Uint256, callback : Address,
        data : Bytes) : Unit := do
    let tokenCount := arrayLength tokens
    let assetCount := arrayLength assets
    requireError (tokenCount == assetCount) InconsistentInput()
    forEach "i" tokenCount (do
      safeTransfer (arrayElement tokens i) callback (arrayElement assets i))
    ecmDo (Compiler.Modules.Callbacks.callbackModule 0xd1f260c3 1 "data")
      [addressToWord callback, ZERO]
    forEach "i" tokenCount (do
      let self ← contractAddress
      safeTransferFrom (arrayElement tokens i) callback self (arrayElement assets i))

  function collateral (id : Bytes32, user : Address, index : Uint256) : Uint256 := do
    let value ← collateralAmount id user index
    return value

  function pendingFee (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "pendingFee"
    return value

  function lastAccrual (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "lastAccrual"
    return value

  function lastLossFactor (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "lastLossFactor"
    return value

  function collateralBitmap (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "collateralBitmap"
    return value

end Midnight.Contract
