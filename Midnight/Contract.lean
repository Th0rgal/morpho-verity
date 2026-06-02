import Contracts.Common
import Verity.Core
import Verity.Stdlib.Math

namespace Midnight.Contract

open Verity hiding pure bind
open Verity.EVM.Uint256
open Verity.Stdlib.Math

def bitAnd (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def bitNot (a : Uint256) : Uint256 := Verity.Core.Uint256.not a
def shl (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shl shift value

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
      625-717,
    * a single normal-mode local `liquidate` sequence that combines the
      bad-debt market writes, max-repay branch, borrower/collateral writes,
      payer/callback branch, and return pair from lines 625-717,
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
      (liquidationLocked : Bool, postMaturityMode : Bool, timestamp : Uint256,
        maturity : Uint256, originalDebt : Uint256, maxDebt : Uint256) :
      Bool := do
    if postMaturityMode then
      return (liquidationLocked == false && timestamp > maturity)
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
      (postMaturityMode : Bool, maxLif : Uint256, timestamp : Uint256,
        maturity : Uint256) : Uint256 := do
    if postMaturityMode then
      let elapsed := sub timestamp maturity
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
        withdrawable : Uint256, rcfThreshold : Uint256, callback : Address,
        msgSender : Address, callbackReturn : Uint256) :
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
    let repayValue := mulDivDown maxRepaid lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
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
    let rcfAccepted := maxRepaid <= maxRepaid ||
      capacityShortfall < rcfThreshold
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable maxRepaid
    let finalDebt := sub currentDebt maxRepaid
    let mut payer := msgSender
    if callback != 0 then
      payer := callback
    else
      payer := payer
    let callbackAccepted := callback == 0 ||
      callbackReturn ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842
    return (currentDebt, maxRepaid, seizedAssets, newCollateral, postBitmap,
      postWithdrawable, finalDebt, payer, callbackAccepted && rcfAccepted,
      seizedAssets, maxRepaid)

  function normalModeLiquidateLocalSequence
      (originalDebt : Uint256, totalUnits : Uint256, oldLossFactor : Uint256,
        continuousFeeCredit : Uint256, badDebt : Uint256, maxDebt : Uint256,
        lltv : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256,
        collateral : Uint256, collateralBitmap : Uint256, collateralIndex : Uint256,
        withdrawable : Uint256, rcfThreshold : Uint256, callback : Address,
        msgSender : Address, callbackReturn : Uint256) :
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
    let repayValue := mulDivDown maxRepaid lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
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
    let rcfAccepted := maxRepaid <= maxRepaid ||
      capacityShortfall < rcfThreshold
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable maxRepaid
    let finalDebt := sub currentDebt maxRepaid
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
      postWithdrawable, finalDebt, payer, callbackAccepted && rcfAccepted,
      seizedAssets, maxRepaid)

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
        pendingFee : Uint256, lastAccrual : Uint256, timestamp : Uint256,
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
    if timestamp <= maturity then
      accrualEnd := timestamp
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

end Midnight.Contract
