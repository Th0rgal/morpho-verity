/-
  ContractShape - rfl checks for the focused Verity-facing Midnight contract.

  These examples pin the generated Verity model body for the arithmetic surface
  used by the RCF and accounting proofs. If `Midnight/Contract.lean` drifts away
  from the intended Solidity-shaped formulas, these checks fail at build time.
-/

import Midnight.Contract

namespace Midnight.Proofs.ContractShape

open Compiler.CompilationModel

example :
    Midnight.Contract.MidnightRCF.maxRepaid_modelBody =
      [Stmt.ite
        ((Expr.param "lltv").lt (Expr.literal 1000000000000000000))
        [Stmt.return
          (((Expr.param "debt").sub (Expr.param "maxDebt")).mulDivUp
            ((Expr.literal 1000000000000000000).mul
              (Expr.literal 1000000000000000000))
            (((Expr.literal 1000000000000000000).mul
                (Expr.literal 1000000000000000000)).sub
              ((Expr.param "lif").mul (Expr.param "lltv"))))]
        [Stmt.return
          (Expr.literal
            115792089237316195423570985008687907853269984665640564039457584007913129639935)]] := rfl

example :
    Midnight.Contract.MidnightRCF.seizedFromRepaid_modelBody =
      [Stmt.letVar "repayValue"
        ((Expr.param "repaidUnits").mulDivDown (Expr.param "lif")
          (Expr.literal 1000000000000000000)),
       Stmt.return
        ((Expr.localVar "repayValue").mulDivDown
          (Expr.literal 1000000000000000000000000000000000000)
          (Expr.param "liquidatedCollatPrice"))] := rfl

example :
    Midnight.Contract.MidnightRCF.repaidFromSeized_modelBody =
      [Stmt.letVar "seizedValue"
        ((Expr.param "seizedAssets").mulDivUp
          (Expr.param "liquidatedCollatPrice")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.return
        ((Expr.localVar "seizedValue").mulDivUp
          (Expr.literal 1000000000000000000)
          (Expr.param "lif"))] := rfl

example :
    Midnight.Contract.MidnightRCF.repaySeizeAmounts_modelBody =
      [Stmt.ite
        ((Expr.param "inputSeizedAssets").gt (Expr.literal 0))
        [Stmt.letVar "repaidUnits"
          (((Expr.param "inputSeizedAssets").mulDivUp
              (Expr.param "liquidatedCollatPrice")
              (Expr.literal 1000000000000000000000000000000000000)).mulDivUp
            (Expr.literal 1000000000000000000)
            (Expr.param "lif")),
         Stmt.returnValues
          [Expr.localVar "repaidUnits", Expr.param "inputSeizedAssets"]]
        [Stmt.letVar "seizedAssets"
          (((Expr.param "inputRepaidUnits").mulDivDown
              (Expr.param "lif")
              (Expr.literal 1000000000000000000)).mulDivDown
            (Expr.literal 1000000000000000000000000000000000000)
            (Expr.param "liquidatedCollatPrice")),
         Stmt.returnValues
          [Expr.param "inputRepaidUnits", Expr.localVar "seizedAssets"]]] := rfl

example :
    Midnight.Contract.MidnightRCF.collateralMaxDebt_modelBody =
      [Stmt.letVar "quoted"
        ((Expr.param "collateral").mulDivDown (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.return
        ((Expr.localVar "quoted").mulDivDown (Expr.param "lltv")
          (Expr.literal 1000000000000000000))] := rfl

example :
    Midnight.Contract.MidnightRCF.postDebt_modelBody =
      [Stmt.return
        ((Expr.param "debt").sub (Expr.param "repaidUnits"))] := rfl

example :
    Midnight.Contract.MidnightRCF.postCollateral_modelBody =
      [Stmt.return
        ((Expr.param "collateral").sub (Expr.param "seizedAssets"))] := rfl

example :
    Midnight.Contract.MidnightRCF.zeroFloorSub_modelBody =
      [Stmt.ite
        ((Expr.param "x").gt (Expr.param "y"))
        [Stmt.return ((Expr.param "x").sub (Expr.param "y"))]
        [Stmt.return (Expr.literal 0)]] := rfl

example :
    Midnight.Contract.MidnightRCF.withdrawableAfterRepay_modelBody =
      [Stmt.return
        ((Expr.param "withdrawable").add (Expr.param "repaidUnits"))] := rfl

example :
    Midnight.Contract.MidnightRCF.clearBitmapBit_modelBody =
      [Stmt.letVar "mask"
        (Expr.bitNot
          (Expr.shl (Expr.param "selectedIndex") (Expr.literal 1))),
       Stmt.return
        (Expr.bitAnd (Expr.param "bitmap") (Expr.localVar "mask"))] := rfl

example :
    Midnight.Contract.MidnightRCF.atMostOneNonZero_modelBody =
      [Stmt.return
        (Expr.logicalOr
          (Expr.eq (Expr.param "repaidUnits") (Expr.literal 0))
          (Expr.eq (Expr.param "seizedAssets") (Expr.literal 0)))] := rfl

example :
    Midnight.Contract.MidnightRCF.borrowerDebtPositive_modelBody =
      [Stmt.return
        (Expr.gt (Expr.param "debt") (Expr.literal 0))] := rfl

example :
    Midnight.Contract.MidnightRCF.liquidatorGateAllows_modelBody =
      [Stmt.return
        (Expr.logicalOr
          (Expr.eq (Expr.param "liquidatorGate") (Expr.literal 0))
          (Expr.param "canLiquidate"))] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeLiquidatable_modelBody =
      [Stmt.return
        (Expr.logicalAnd
          (Expr.param "liquidationUnlocked")
          (Expr.gt (Expr.param "originalDebt") (Expr.param "maxDebt")))] := rfl

example :
    Midnight.Contract.MidnightRCF.liquidatableGuard_modelBody =
      [Stmt.ite
        (Expr.param "postMaturityMode")
        [Stmt.return
          (Expr.logicalAnd
            (Expr.eq (Expr.param "liquidationLocked") (Expr.literal 0))
            (Expr.gt (Expr.param "blockTimestamp") (Expr.param "maturity")))]
        [Stmt.return
          (Expr.logicalAnd
            (Expr.eq (Expr.param "liquidationLocked") (Expr.literal 0))
            (Expr.gt (Expr.param "originalDebt") (Expr.param "maxDebt")))]] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeEntryGuards_modelBody =
      [Stmt.return
        (Expr.logicalAnd
          (Expr.logicalAnd
            (Expr.logicalAnd
              (Expr.logicalOr
                (Expr.eq (Expr.param "repaidUnits") (Expr.literal 0))
                (Expr.eq (Expr.param "seizedAssets") (Expr.literal 0)))
              (Expr.gt (Expr.param "debt") (Expr.literal 0)))
            (Expr.logicalOr
              (Expr.eq (Expr.param "liquidatorGate") (Expr.literal 0))
              (Expr.param "canLiquidate")))
          (Expr.logicalAnd
            (Expr.param "liquidationUnlocked")
            (Expr.gt (Expr.param "originalDebt") (Expr.param "maxDebt"))))] := rfl

example :
    Midnight.Contract.MidnightRCF.repayBranchActive_modelBody =
      [Stmt.return
        (Expr.logicalOr
          (Expr.gt (Expr.param "repaidUnits") (Expr.literal 0))
          (Expr.gt (Expr.param "seizedAssets") (Expr.literal 0)))] := rfl

example :
    Midnight.Contract.MidnightRCF.liquidationLif_modelBody =
      [Stmt.ite
        (Expr.param "postMaturityMode")
        [Stmt.letVar "elapsed"
          ((Expr.param "blockTimestamp").sub (Expr.param "maturity")),
         Stmt.letVar "postMaturityLif"
          ((Expr.literal 1000000000000000000).add
            (((Expr.param "maxLif").sub
                (Expr.literal 1000000000000000000)).mulDivDown
              (Expr.localVar "elapsed")
              (Expr.literal 900))),
         Stmt.ite
          ((Expr.param "maxLif").le (Expr.localVar "postMaturityLif"))
          [Stmt.return (Expr.param "maxLif")]
          [Stmt.return (Expr.localVar "postMaturityLif")]]
        [Stmt.return (Expr.param "maxLif")]] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtBranchActive_modelBody =
      [Stmt.return
        (Expr.gt (Expr.param "badDebt") (Expr.literal 0))] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtPostDebt_modelBody =
      [Stmt.return
        ((Expr.param "originalDebt").sub (Expr.param "badDebt"))] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtLocalSequence_modelBody =
      [Stmt.letVar "postDebt" (Expr.param "originalDebt"),
       Stmt.letVar "newTotalUnits" (Expr.param "totalUnits"),
       Stmt.letVar "newLossFactor" (Expr.param "oldLossFactor"),
       Stmt.letVar "newContinuousFeeCredit"
        (Expr.param "continuousFeeCredit"),
       Stmt.letVar "branchActive"
        (Expr.gt (Expr.param "badDebt") (Expr.literal 0)),
       Stmt.ite
        (Expr.localVar "branchActive")
        [Stmt.assignVar "postDebt"
          ((Expr.param "originalDebt").sub (Expr.param "badDebt")),
         Stmt.assignVar "newLossFactor"
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.param "oldLossFactor")).mulDivDown
              ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
              (Expr.param "totalUnits"))),
         Stmt.assignVar "newTotalUnits"
          ((Expr.param "totalUnits").sub (Expr.param "badDebt")),
         Stmt.ite
          ((Expr.param "oldLossFactor").lt
            (Expr.literal 340282366920938463463374607431768211455))
          [Stmt.assignVar "newContinuousFeeCredit"
            ((Expr.param "continuousFeeCredit").mulDivDown
              ((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.localVar "newLossFactor"))
              ((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.param "oldLossFactor")))]
          [Stmt.assignVar "newContinuousFeeCredit" (Expr.literal 0)]]
        [Stmt.assignVar "postDebt" (Expr.localVar "postDebt")],
       Stmt.returnValues
        [Expr.localVar "postDebt",
         Expr.localVar "newTotalUnits",
         Expr.localVar "newLossFactor",
         Expr.localVar "newContinuousFeeCredit",
         Expr.localVar "branchActive"]] := rfl

example :
    Midnight.Contract.MidnightRCF.callbackRequired_modelBody =
      [Stmt.return
        (Expr.logicalNot
          (Expr.eq (Expr.param "callback") (Expr.literal 0)))] := rfl

example :
    Midnight.Contract.MidnightRCF.liquidatePayer_modelBody =
      [Stmt.ite
        (Expr.logicalNot
          (Expr.eq (Expr.param "callback") (Expr.literal 0)))
        [Stmt.return (Expr.param "callback")]
        [Stmt.return (Expr.param "msgSender")]] := rfl

example :
    Midnight.Contract.MidnightRCF.callbackReturnAccepted_modelBody =
      [Stmt.return
        (Expr.logicalOr
          (Expr.eq (Expr.param "callback") (Expr.literal 0))
          (Expr.eq (Expr.param "returnValue")
            (Expr.literal
              57683088179238363159977504707935902064464440500167392520350201799917296135842)))] := rfl

example :
    Midnight.Contract.MidnightRCF.callbackSuccessValue_modelBody =
      [Stmt.return
        (Expr.literal
          57683088179238363159977504707935902064464440500167392520350201799917296135842)] := rfl

example :
    Midnight.Contract.MidnightRCF.liquidateReturn_modelBody =
      [Stmt.returnValues
        [Expr.param "seizedAssets", Expr.param "repaidUnits"]] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeRcfLocalSequence_modelBody =
      [Stmt.letVar "repaidUnits" (Expr.param "inputRepaidUnits"),
       Stmt.letVar "seizedAssets" (Expr.param "inputSeizedAssets"),
       Stmt.ite
        ((Expr.param "inputSeizedAssets").gt (Expr.literal 0))
        [Stmt.assignVar "repaidUnits"
          (((Expr.param "inputSeizedAssets").mulDivUp
              (Expr.param "liquidatedCollatPrice")
              (Expr.literal 1000000000000000000000000000000000000)).mulDivUp
            (Expr.literal 1000000000000000000)
            (Expr.param "lif"))]
        [Stmt.assignVar "seizedAssets"
          (((Expr.param "inputRepaidUnits").mulDivDown
              (Expr.param "lif")
              (Expr.literal 1000000000000000000)).mulDivDown
            (Expr.literal 1000000000000000000000000000000000000)
            (Expr.param "liquidatedCollatPrice"))],
       Stmt.letVar "maxRepaid"
        (Expr.literal
          115792089237316195423570985008687907853269984665640564039457584007913129639935),
       Stmt.ite
        ((Expr.param "lltv").lt (Expr.literal 1000000000000000000))
        [Stmt.assignVar "maxRepaid"
          (((Expr.param "currentDebt").sub (Expr.param "maxDebt")).mulDivUp
            ((Expr.literal 1000000000000000000).mul
              (Expr.literal 1000000000000000000))
            (((Expr.literal 1000000000000000000).mul
                (Expr.literal 1000000000000000000)).sub
              ((Expr.param "lif").mul (Expr.param "lltv"))))]
        [Stmt.assignVar "maxRepaid" (Expr.localVar "maxRepaid")],
       Stmt.letVar "collateralRepayCapacity"
        (((Expr.param "collateral").mulDivDown
            (Expr.param "liquidatedCollatPrice")
            (Expr.literal 1000000000000000000000000000000000000)).mulDivDown
          (Expr.literal 1000000000000000000)
          (Expr.param "lif")),
       Stmt.letVar "capacityShortfall" (Expr.literal 0),
       Stmt.ite
        ((Expr.localVar "collateralRepayCapacity").gt
          (Expr.localVar "maxRepaid"))
        [Stmt.assignVar "capacityShortfall"
          ((Expr.localVar "collateralRepayCapacity").sub
            (Expr.localVar "maxRepaid"))]
        [Stmt.assignVar "capacityShortfall"
          (Expr.localVar "capacityShortfall")],
       Stmt.letVar "rcfAccepted"
        (Expr.logicalOr
          ((Expr.localVar "repaidUnits").le (Expr.localVar "maxRepaid"))
          ((Expr.localVar "capacityShortfall").lt
            (Expr.param "rcfThreshold"))),
       Stmt.letVar "newCollateral"
        ((Expr.param "collateral").sub (Expr.localVar "seizedAssets")),
       Stmt.letVar "postBitmap" (Expr.param "collateralBitmap"),
       Stmt.ite
        (Expr.logicalAnd
          (Expr.eq (Expr.localVar "newCollateral") (Expr.literal 0))
          (Expr.gt (Expr.localVar "seizedAssets") (Expr.literal 0)))
        [Stmt.assignVar "postBitmap"
          (Expr.bitAnd
            (Expr.param "collateralBitmap")
            (Expr.bitNot
              (Expr.shl (Expr.param "collateralIndex") (Expr.literal 1))))]
        [Stmt.assignVar "postBitmap" (Expr.localVar "postBitmap")],
       Stmt.letVar "postWithdrawable"
        ((Expr.param "withdrawable").add (Expr.localVar "repaidUnits")),
       Stmt.letVar "finalDebt"
        ((Expr.param "currentDebt").sub (Expr.localVar "repaidUnits")),
       Stmt.returnValues
        [Expr.localVar "repaidUnits",
         Expr.localVar "seizedAssets",
         Expr.localVar "maxRepaid",
         Expr.localVar "newCollateral",
         Expr.localVar "postBitmap",
         Expr.localVar "postWithdrawable",
         Expr.localVar "finalDebt",
         Expr.localVar "rcfAccepted",
         Expr.localVar "capacityShortfall"]] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeRepaidInputLocalSequence_modelBody =
      [Stmt.letVar "currentDebt"
        ((Expr.param "originalDebt").sub (Expr.param "badDebt")),
       Stmt.letVar "maxRepaid"
        (Expr.literal
          115792089237316195423570985008687907853269984665640564039457584007913129639935),
       Stmt.ite
        ((Expr.param "lltv").lt (Expr.literal 1000000000000000000))
        [Stmt.assignVar "maxRepaid"
          (((Expr.localVar "currentDebt").sub (Expr.param "maxDebt")).mulDivUp
            ((Expr.literal 1000000000000000000).mul
              (Expr.literal 1000000000000000000))
            (((Expr.literal 1000000000000000000).mul
                (Expr.literal 1000000000000000000)).sub
              ((Expr.param "lif").mul (Expr.param "lltv"))))]
        [Stmt.assignVar "maxRepaid" (Expr.localVar "maxRepaid")],
       Stmt.letVar "repaidUnits" (Expr.localVar "maxRepaid"),
       Stmt.letVar "repayValue"
        ((Expr.localVar "repaidUnits").mulDivDown (Expr.param "lif")
          (Expr.literal 1000000000000000000)),
       Stmt.letVar "seizedAssets"
        ((Expr.localVar "repayValue").mulDivDown
          (Expr.literal 1000000000000000000000000000000000000)
          (Expr.param "liquidatedCollatPrice")),
       Stmt.letVar "newCollateral"
        ((Expr.param "collateral").sub (Expr.localVar "seizedAssets")),
       Stmt.letVar "postBitmap" (Expr.param "collateralBitmap"),
       Stmt.ite
        (Expr.logicalAnd
          (Expr.eq (Expr.localVar "newCollateral") (Expr.literal 0))
          (Expr.gt (Expr.localVar "seizedAssets") (Expr.literal 0)))
        [Stmt.assignVar "postBitmap"
          (Expr.bitAnd
            (Expr.param "collateralBitmap")
            (Expr.bitNot
              (Expr.shl (Expr.param "collateralIndex") (Expr.literal 1))))]
        [Stmt.assignVar "postBitmap" (Expr.localVar "postBitmap")],
       Stmt.letVar "postWithdrawable"
        ((Expr.param "withdrawable").add (Expr.localVar "repaidUnits")),
       Stmt.letVar "finalDebt"
        ((Expr.localVar "currentDebt").sub (Expr.localVar "repaidUnits")),
       Stmt.letVar "payer" (Expr.param "msgSender"),
       Stmt.ite
        (Expr.logicalNot
          (Expr.eq (Expr.param "callback") (Expr.literal 0)))
        [Stmt.assignVar "payer" (Expr.param "callback")]
        [Stmt.assignVar "payer" (Expr.localVar "payer")],
       Stmt.letVar "callbackAccepted"
        (Expr.logicalOr
          (Expr.eq (Expr.param "callback") (Expr.literal 0))
          (Expr.eq (Expr.param "callbackReturn")
            (Expr.literal
              57683088179238363159977504707935902064464440500167392520350201799917296135842))),
       Stmt.returnValues
        [Expr.localVar "currentDebt",
         Expr.localVar "maxRepaid",
         Expr.localVar "seizedAssets",
         Expr.localVar "newCollateral",
         Expr.localVar "postBitmap",
         Expr.localVar "postWithdrawable",
         Expr.localVar "finalDebt",
         Expr.localVar "payer",
         Expr.localVar "callbackAccepted",
         Expr.localVar "seizedAssets",
         Expr.localVar "repaidUnits"]] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeLiquidateLocalSequence_modelBody =
      [Stmt.letVar "postBadDebtDebt" (Expr.param "originalDebt"),
       Stmt.letVar "newTotalUnits" (Expr.param "totalUnits"),
       Stmt.letVar "newLossFactor" (Expr.param "oldLossFactor"),
       Stmt.letVar "newContinuousFeeCredit"
        (Expr.param "continuousFeeCredit"),
       Stmt.ite
        ((Expr.param "badDebt").gt (Expr.literal 0))
        [Stmt.assignVar "postBadDebtDebt"
          ((Expr.param "originalDebt").sub (Expr.param "badDebt")),
         Stmt.assignVar "newLossFactor"
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.param "oldLossFactor")).mulDivDown
              ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
              (Expr.param "totalUnits"))),
         Stmt.assignVar "newTotalUnits"
          ((Expr.param "totalUnits").sub (Expr.param "badDebt")),
         Stmt.ite
          ((Expr.param "oldLossFactor").lt
            (Expr.literal 340282366920938463463374607431768211455))
          [Stmt.assignVar "newContinuousFeeCredit"
            ((Expr.param "continuousFeeCredit").mulDivDown
              ((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.localVar "newLossFactor"))
              ((Expr.literal 340282366920938463463374607431768211455).sub
                (Expr.param "oldLossFactor")))]
          [Stmt.assignVar "newContinuousFeeCredit" (Expr.literal 0)]]
        [Stmt.assignVar "postBadDebtDebt"
          (Expr.localVar "postBadDebtDebt")],
       Stmt.letVar "currentDebt" (Expr.localVar "postBadDebtDebt"),
       Stmt.letVar "maxRepaid"
        (Expr.literal
          115792089237316195423570985008687907853269984665640564039457584007913129639935),
       Stmt.ite
        ((Expr.param "lltv").lt (Expr.literal 1000000000000000000))
        [Stmt.assignVar "maxRepaid"
          (((Expr.localVar "currentDebt").sub (Expr.param "maxDebt")).mulDivUp
            ((Expr.literal 1000000000000000000).mul
              (Expr.literal 1000000000000000000))
            (((Expr.literal 1000000000000000000).mul
                (Expr.literal 1000000000000000000)).sub
              ((Expr.param "lif").mul (Expr.param "lltv"))))]
        [Stmt.assignVar "maxRepaid" (Expr.localVar "maxRepaid")],
       Stmt.letVar "repaidUnits" (Expr.localVar "maxRepaid"),
       Stmt.letVar "repayValue"
        ((Expr.localVar "repaidUnits").mulDivDown (Expr.param "lif")
          (Expr.literal 1000000000000000000)),
       Stmt.letVar "seizedAssets"
        ((Expr.localVar "repayValue").mulDivDown
          (Expr.literal 1000000000000000000000000000000000000)
          (Expr.param "liquidatedCollatPrice")),
       Stmt.letVar "newCollateral"
        ((Expr.param "collateral").sub (Expr.localVar "seizedAssets")),
       Stmt.letVar "postBitmap" (Expr.param "collateralBitmap"),
       Stmt.ite
        (Expr.logicalAnd
          (Expr.eq (Expr.localVar "newCollateral") (Expr.literal 0))
          (Expr.gt (Expr.localVar "seizedAssets") (Expr.literal 0)))
        [Stmt.assignVar "postBitmap"
          (Expr.bitAnd
            (Expr.param "collateralBitmap")
            (Expr.bitNot
              (Expr.shl (Expr.param "collateralIndex") (Expr.literal 1))))]
        [Stmt.assignVar "postBitmap" (Expr.localVar "postBitmap")],
       Stmt.letVar "postWithdrawable"
        ((Expr.param "withdrawable").add (Expr.localVar "repaidUnits")),
       Stmt.letVar "finalDebt"
        ((Expr.localVar "currentDebt").sub (Expr.localVar "repaidUnits")),
       Stmt.letVar "payer" (Expr.param "msgSender"),
       Stmt.ite
        (Expr.logicalNot
          (Expr.eq (Expr.param "callback") (Expr.literal 0)))
        [Stmt.assignVar "payer" (Expr.param "callback")]
        [Stmt.assignVar "payer" (Expr.localVar "payer")],
       Stmt.letVar "callbackAccepted"
        (Expr.logicalOr
          (Expr.eq (Expr.param "callback") (Expr.literal 0))
          (Expr.eq (Expr.param "callbackReturn")
            (Expr.literal
              57683088179238363159977504707935902064464440500167392520350201799917296135842))),
       Stmt.returnValues
        [Expr.localVar "postBadDebtDebt",
         Expr.localVar "newTotalUnits",
         Expr.localVar "newLossFactor",
         Expr.localVar "newContinuousFeeCredit",
         Expr.localVar "currentDebt",
         Expr.localVar "maxRepaid",
         Expr.localVar "seizedAssets",
         Expr.localVar "newCollateral",
         Expr.localVar "postBitmap",
         Expr.localVar "postWithdrawable",
         Expr.localVar "finalDebt",
         Expr.localVar "payer",
         Expr.localVar "callbackAccepted",
         Expr.localVar "seizedAssets",
         Expr.localVar "repaidUnits"]] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtCollateralRepayable_modelBody =
      [Stmt.letVar "quoted"
        ((Expr.param "collateral").mulDivUp (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.return
        ((Expr.localVar "quoted").mulDivUp
          (Expr.literal 1000000000000000000)
          (Expr.param "maxLif"))] := rfl

example :
    Midnight.Contract.MidnightRCF.collateralLoopStep_modelBody =
      [Stmt.letVar "collateralQuote"
        ((Expr.param "collateral").mulDivDown (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.letVar "nextMaxDebt"
        ((Expr.param "maxDebt").add
          ((Expr.localVar "collateralQuote").mulDivDown
            (Expr.param "lltv")
            (Expr.literal 1000000000000000000))),
       Stmt.letVar "nextLiquidatedCollatPrice"
        (Expr.param "liquidatedCollatPrice"),
       Stmt.ite
        (Expr.eq (Expr.param "slotIndex") (Expr.param "selectedIndex"))
        [Stmt.assignVar "nextLiquidatedCollatPrice" (Expr.param "price")]
        [Stmt.assignVar "nextLiquidatedCollatPrice"
          (Expr.localVar "nextLiquidatedCollatPrice")],
       Stmt.letVar "repayableQuote"
        ((Expr.param "collateral").mulDivUp (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.letVar "repayable"
        ((Expr.localVar "repayableQuote").mulDivUp
          (Expr.literal 1000000000000000000)
          (Expr.param "maxLif")),
       Stmt.letVar "nextBadDebt" (Expr.literal 0),
       Stmt.ite
        ((Expr.param "badDebt").gt (Expr.localVar "repayable"))
        [Stmt.assignVar "nextBadDebt"
          ((Expr.param "badDebt").sub (Expr.localVar "repayable"))]
        [Stmt.assignVar "nextBadDebt" (Expr.localVar "nextBadDebt")],
       Stmt.letVar "nextBitmap"
        (Expr.bitAnd
          (Expr.param "collateralBitmap")
          (Expr.bitNot
            (Expr.shl (Expr.param "slotIndex") (Expr.literal 1)))),
       Stmt.returnValues
        [Expr.localVar "nextMaxDebt",
         Expr.localVar "nextLiquidatedCollatPrice",
         Expr.localVar "nextBadDebt",
         Expr.localVar "nextBitmap"]] := rfl

example :
    Midnight.Contract.MidnightRCF.rcfAllows_modelBody =
      [Stmt.letVar "collateralRepayCapacity"
        (((Expr.param "collateral").mulDivDown
            (Expr.param "liquidatedCollatPrice")
            (Expr.literal 1000000000000000000000000000000000000)).mulDivDown
          (Expr.literal 1000000000000000000)
          (Expr.param "lif")),
       Stmt.return
        (Expr.logicalOr
          ((Expr.param "repaidUnits").le (Expr.param "maxRepaid"))
          (((Expr.localVar "collateralRepayCapacity").sub
              (Expr.param "maxRepaid")).lt
            (Expr.param "rcfThreshold")))] := rfl

example :
    Midnight.Contract.MidnightRCF.rcfAllowsZeroFloor_modelBody =
      [Stmt.letVar "collateralRepayCapacity"
        (((Expr.param "collateral").mulDivDown
            (Expr.param "liquidatedCollatPrice")
            (Expr.literal 1000000000000000000000000000000000000)).mulDivDown
          (Expr.literal 1000000000000000000)
          (Expr.param "lif")),
       Stmt.letVar "capacityShortfall" (Expr.literal 0),
       Stmt.ite
        ((Expr.localVar "collateralRepayCapacity").gt
          (Expr.param "maxRepaid"))
        [Stmt.assignVar "capacityShortfall"
          ((Expr.localVar "collateralRepayCapacity").sub
            (Expr.param "maxRepaid"))]
        [Stmt.assignVar "capacityShortfall"
          (Expr.localVar "capacityShortfall")],
       Stmt.return
        (Expr.logicalOr
          ((Expr.param "repaidUnits").le (Expr.param "maxRepaid"))
          ((Expr.localVar "capacityShortfall").lt
            (Expr.param "rcfThreshold")))] := rfl

example :
    Midnight.Contract.MidnightRCF.normalModeMaxRepaidHealthyWithin3_modelBody =
      [Stmt.letVar "selectedQuote"
        ((Expr.param "collateral").mulDivDown (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.letVar "selectedMaxDebt"
        ((Expr.localVar "selectedQuote").mulDivDown (Expr.param "lltv")
          (Expr.literal 1000000000000000000)),
       Stmt.letVar "maxDebt"
        ((Expr.param "otherMaxDebt").add (Expr.localVar "selectedMaxDebt")),
       Stmt.letVar "repaid"
        (((Expr.param "debt").sub (Expr.localVar "maxDebt")).mulDivUp
          ((Expr.literal 1000000000000000000).mul
            (Expr.literal 1000000000000000000))
          (((Expr.literal 1000000000000000000).mul
              (Expr.literal 1000000000000000000)).sub
            ((Expr.param "lif").mul (Expr.param "lltv")))),
       Stmt.letVar "repayValue"
        ((Expr.localVar "repaid").mulDivDown (Expr.param "lif")
          (Expr.literal 1000000000000000000)),
       Stmt.letVar "seizedAssets"
        ((Expr.localVar "repayValue").mulDivDown
          (Expr.literal 1000000000000000000000000000000000000)
          (Expr.param "price")),
       Stmt.letVar "postDebt"
        ((Expr.param "debt").sub (Expr.localVar "repaid")),
       Stmt.letVar "postCollateral"
        ((Expr.param "collateral").sub (Expr.localVar "seizedAssets")),
       Stmt.letVar "postQuote"
        ((Expr.localVar "postCollateral").mulDivDown (Expr.param "price")
          (Expr.literal 1000000000000000000000000000000000000)),
       Stmt.letVar "postSelectedMaxDebt"
        ((Expr.localVar "postQuote").mulDivDown (Expr.param "lltv")
          (Expr.literal 1000000000000000000)),
       Stmt.letVar "postMaxDebt"
        ((Expr.param "otherMaxDebt").add (Expr.localVar "postSelectedMaxDebt")),
       Stmt.return
        ((Expr.localVar "postDebt").le
          ((Expr.localVar "postMaxDebt").add (Expr.literal 3)))] := rfl

example :
    Midnight.Contract.MidnightRCF.lossFactorAfterBadDebt_modelBody =
      [Stmt.return
        ((Expr.literal 340282366920938463463374607431768211455).sub
          (((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "oldLossFactor")).mulDivDown
            ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
            (Expr.param "totalUnits")))] := rfl

example :
    Midnight.Contract.MidnightRCF.totalUnitsAfterBadDebt_modelBody =
      [Stmt.return
        ((Expr.param "totalUnits").sub (Expr.param "badDebt"))] := rfl

example :
    Midnight.Contract.MidnightRCF.continuousFeeCreditAfterBadDebt_modelBody =
      [Stmt.ite
        ((Expr.param "oldLossFactor").lt
          (Expr.literal 340282366920938463463374607431768211455))
        [Stmt.return
          ((Expr.param "continuousFeeCredit").mulDivDown
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "newLossFactor"))
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "oldLossFactor")))]
        [Stmt.return (Expr.literal 0)]] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtCoversTwoPostSlashCredits_modelBody =
      [Stmt.letVar "newLossFactor"
        ((Expr.literal 340282366920938463463374607431768211455).sub
          (((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "oldLossFactor")).mulDivDown
            ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
            (Expr.param "totalUnits"))),
       Stmt.letVar "newTotalUnits"
        ((Expr.param "totalUnits").sub (Expr.param "badDebt")),
       Stmt.letVar "postSlashCredit0"
        ((Expr.param "credit0").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.letVar "postSlashCredit1"
        ((Expr.param "credit1").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.return
        (((Expr.localVar "postSlashCredit0").add
            (Expr.localVar "postSlashCredit1")).le
          (Expr.localVar "newTotalUnits"))] := rfl

example :
    Midnight.Contract.MidnightRCF.badDebtCoversTwoPostUpdateCredits_modelBody =
      [Stmt.letVar "newLossFactor"
        ((Expr.literal 340282366920938463463374607431768211455).sub
          (((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "oldLossFactor")).mulDivDown
            ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
            (Expr.param "totalUnits"))),
       Stmt.letVar "newTotalUnits"
        ((Expr.param "totalUnits").sub (Expr.param "badDebt")),
       Stmt.letVar "postSlashCredit0"
        ((Expr.param "credit0").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.letVar "postSlashCredit1"
        ((Expr.param "credit1").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.letVar "postUpdateCredit0"
        ((Expr.localVar "postSlashCredit0").sub (Expr.param "accruedFee0")),
       Stmt.letVar "postUpdateCredit1"
        ((Expr.localVar "postSlashCredit1").sub (Expr.param "accruedFee1")),
       Stmt.return
        (((Expr.localVar "postUpdateCredit0").add
            (Expr.localVar "postUpdateCredit1")).le
          (Expr.localVar "newTotalUnits"))] := rfl

/- This body intentionally mirrors `badDebtCoversTwoPostSlashCredits_modelBody`.
   The separate name pins the storage-after-update property surface. -/
example :
    Midnight.Contract.MidnightRCF.badDebtCoversTwoStoredCreditsAfterUpdates_modelBody =
      [Stmt.letVar "newLossFactor"
        ((Expr.literal 340282366920938463463374607431768211455).sub
          (((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "oldLossFactor")).mulDivDown
            ((Expr.param "totalUnits").sub (Expr.param "badDebt"))
            (Expr.param "totalUnits"))),
       Stmt.letVar "newTotalUnits"
        ((Expr.param "totalUnits").sub (Expr.param "badDebt")),
       Stmt.letVar "storedCredit0"
        ((Expr.param "credit0").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.letVar "storedCredit1"
        ((Expr.param "credit1").mulDivDown
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.localVar "newLossFactor"))
          ((Expr.literal 340282366920938463463374607431768211455).sub
            (Expr.param "oldLossFactor"))),
       Stmt.return
        (((Expr.localVar "storedCredit0").add
            (Expr.localVar "storedCredit1")).le
          (Expr.localVar "newTotalUnits"))] := rfl

example :
    Midnight.Contract.MidnightRCF.postSlashCredit_modelBody =
      [Stmt.ite
        ((Expr.param "lastLossFactor").lt
          (Expr.literal 340282366920938463463374607431768211455))
        [Stmt.return
          ((Expr.param "credit").mulDivDown
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "marketLossFactor"))
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "lastLossFactor")))]
        [Stmt.return (Expr.literal 0)]] := rfl

example :
    Midnight.Contract.MidnightRCF.postSlashPendingFee_modelBody =
      [Stmt.ite
        ((Expr.literal 0).lt (Expr.param "credit"))
        [Stmt.return
          ((Expr.param "pendingFee").sub
            ((Expr.param "pendingFee").mulDivUp
              ((Expr.param "credit").sub (Expr.param "postSlashCredit"))
              (Expr.param "credit")))]
        [Stmt.return (Expr.literal 0)]] := rfl

example :
    Midnight.Contract.MidnightRCF.accruedFee_modelBody =
      [Stmt.ite
        ((Expr.param "lastAccrual").lt (Expr.param "maturity"))
        [Stmt.return
          ((Expr.param "postSlashPendingFee").mulDivDown
            ((Expr.param "accrualEnd").sub (Expr.param "lastAccrual"))
            ((Expr.param "maturity").sub (Expr.param "lastAccrual")))]
        [Stmt.return (Expr.literal 0)]] := rfl

example :
    Midnight.Contract.MidnightRCF.postUpdateCreditAfterFee_modelBody =
      [Stmt.return
        ((Expr.param "postSlashCredit").sub (Expr.param "accruedFee"))] := rfl

example :
    Midnight.Contract.MidnightRCF.updatePositionViewSequence_modelBody =
      [Stmt.letVar "postSlashCredit" (Expr.literal 0),
       Stmt.ite
        ((Expr.param "lastLossFactor").lt
          (Expr.literal 340282366920938463463374607431768211455))
        [Stmt.assignVar "postSlashCredit"
          ((Expr.param "credit").mulDivDown
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "marketLossFactor"))
            ((Expr.literal 340282366920938463463374607431768211455).sub
              (Expr.param "lastLossFactor")))]
        [Stmt.assignVar "postSlashCredit"
          (Expr.localVar "postSlashCredit")],
       Stmt.letVar "postSlashPendingFee" (Expr.literal 0),
       Stmt.ite
        ((Expr.param "credit").gt (Expr.literal 0))
        [Stmt.assignVar "postSlashPendingFee"
          ((Expr.param "pendingFee").sub
            ((Expr.param "pendingFee").mulDivUp
              ((Expr.param "credit").sub
                (Expr.localVar "postSlashCredit"))
              (Expr.param "credit")))]
        [Stmt.assignVar "postSlashPendingFee"
          (Expr.localVar "postSlashPendingFee")],
       Stmt.letVar "accrualEnd" (Expr.param "maturity"),
       Stmt.ite
        ((Expr.param "blockTimestamp").le (Expr.param "maturity"))
        [Stmt.assignVar "accrualEnd" (Expr.param "blockTimestamp")]
        [Stmt.assignVar "accrualEnd" (Expr.localVar "accrualEnd")],
       Stmt.letVar "fee" (Expr.literal 0),
       Stmt.ite
        ((Expr.param "lastAccrual").lt (Expr.param "maturity"))
        [Stmt.assignVar "fee"
          ((Expr.localVar "postSlashPendingFee").mulDivDown
            ((Expr.localVar "accrualEnd").sub
              (Expr.param "lastAccrual"))
            ((Expr.param "maturity").sub
              (Expr.param "lastAccrual")))]
        [Stmt.assignVar "fee" (Expr.localVar "fee")],
       Stmt.returnValues
        [(Expr.localVar "postSlashCredit").sub (Expr.localVar "fee"),
         (Expr.localVar "postSlashPendingFee").sub (Expr.localVar "fee"),
         Expr.localVar "fee"]] := rfl

end Midnight.Proofs.ContractShape
