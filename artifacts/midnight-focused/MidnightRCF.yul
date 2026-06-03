object "MidnightRCF" {
    code {
        mstore(64, 128)
        if callvalue() {
            revert(0, 0)
        }
        function internal_internal_maxRepaid(debt, maxDebt, lif, lltv) -> __ret0 {
            {
                let __ite_cond := lt(lltv, 1000000000000000000)
                if __ite_cond {
                    __ret0 := div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                    leave
                }
            }
        }
        function internal_internal_seizedFromRepaid(repaidUnits, lif, liquidatedCollatPrice) -> __ret0 {
            let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
            __ret0 := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
            leave
        }
        function internal_internal_repaidFromSeized(seizedAssets, lif, liquidatedCollatPrice) -> __ret0 {
            let seizedValue := div(add(mul(seizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
            __ret0 := div(add(mul(seizedValue, 1000000000000000000), sub(lif, 1)), lif)
            leave
        }
        function internal_internal_repaySeizeAmounts(inputRepaidUnits, inputSeizedAssets, lif, liquidatedCollatPrice) -> __ret0, __ret1 {
            {
                let __ite_cond := gt(inputSeizedAssets, 0)
                if __ite_cond {
                    let repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                    __ret0 := repaidUnits
                    __ret1 := inputSeizedAssets
                    leave
                }
                if iszero(__ite_cond) {
                    let seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                    __ret0 := inputRepaidUnits
                    __ret1 := seizedAssets
                    leave
                }
            }
        }
        function internal_internal_collateralMaxDebt(collateral, price, lltv) -> __ret0 {
            let quoted := div(mul(collateral, price), 1000000000000000000000000000000000000)
            __ret0 := div(mul(quoted, lltv), 1000000000000000000)
            leave
        }
        function internal_internal_postDebt(debt, repaidUnits) -> __ret0 {
            __ret0 := sub(debt, repaidUnits)
            leave
        }
        function internal_internal_postCollateral(collateral, seizedAssets) -> __ret0 {
            __ret0 := sub(collateral, seizedAssets)
            leave
        }
        function internal_internal_zeroFloorSub(x, y) -> __ret0 {
            {
                let __ite_cond := gt(x, y)
                if __ite_cond {
                    __ret0 := sub(x, y)
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 0
                    leave
                }
            }
        }
        function internal_internal_withdrawableAfterRepay(withdrawable, repaidUnits) -> __ret0 {
            __ret0 := add(withdrawable, repaidUnits)
            leave
        }
        function internal_internal_clearBitmapBit(bitmap, selectedIndex) -> __ret0 {
            let mask := not(shl(selectedIndex, 1))
            __ret0 := and(bitmap, mask)
            leave
        }
        function internal_internal_atMostOneNonZero(repaidUnits, seizedAssets) -> __ret0 {
            __ret0 := or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0))))
            leave
        }
        function internal_internal_borrowerDebtPositive(debt) -> __ret0 {
            __ret0 := gt(debt, 0)
            leave
        }
        function internal_internal_liquidatorGateAllows(liquidatorGate, canLiquidate) -> __ret0 {
            __ret0 := or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate)))
            leave
        }
        function internal_internal_normalModeLiquidatable(liquidationUnlocked, originalDebt, maxDebt) -> __ret0 {
            __ret0 := and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt))))
            leave
        }
        function internal_internal_liquidatableGuard(liquidationLocked, postMaturityMode, blockTimestamp, maturity, originalDebt, maxDebt) -> __ret0 {
            {
                let __ite_cond := postMaturityMode
                if __ite_cond {
                    __ret0 := and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(blockTimestamp, maturity))))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(originalDebt, maxDebt))))
                    leave
                }
            }
        }
        function internal_internal_normalModeEntryGuards(repaidUnits, seizedAssets, debt, liquidatorGate, canLiquidate, liquidationUnlocked, originalDebt, maxDebt) -> __ret0 {
            __ret0 := and(iszero(iszero(and(iszero(iszero(and(iszero(iszero(or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0)))))), iszero(iszero(gt(debt, 0)))))), iszero(iszero(or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate)))))))), iszero(iszero(and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt)))))))
            leave
        }
        function internal_internal_repayBranchActive(repaidUnits, seizedAssets) -> __ret0 {
            __ret0 := or(iszero(iszero(gt(repaidUnits, 0))), iszero(iszero(gt(seizedAssets, 0))))
            leave
        }
        function internal_internal_liquidationLif(postMaturityMode, maxLif, blockTimestamp, maturity) -> __ret0 {
            {
                let __ite_cond := postMaturityMode
                if __ite_cond {
                    let elapsed := sub(blockTimestamp, maturity)
                    let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLif, 1000000000000000000), elapsed), 900))
                    {
                        let __ite_cond_1 := iszero(gt(maxLif, postMaturityLif))
                        if __ite_cond_1 {
                            __ret0 := maxLif
                            leave
                        }
                        if iszero(__ite_cond_1) {
                            __ret0 := postMaturityLif
                            leave
                        }
                    }
                }
                if iszero(__ite_cond) {
                    __ret0 := maxLif
                    leave
                }
            }
        }
        function internal_internal_badDebtBranchActive(badDebt) -> __ret0 {
            __ret0 := gt(badDebt, 0)
            leave
        }
        function internal_internal_badDebtPostDebt(originalDebt, badDebt) -> __ret0 {
            __ret0 := sub(originalDebt, badDebt)
            leave
        }
        function internal_internal_badDebtLocalSequence(originalDebt, totalUnits, oldLossFactor, continuousFeeCredit, badDebt) -> __ret0, __ret1, __ret2, __ret3, __ret4 {
            let postDebt := originalDebt
            let newTotalUnits := totalUnits
            let newLossFactor := oldLossFactor
            let newContinuousFeeCredit := continuousFeeCredit
            let branchActive := gt(badDebt, 0)
            {
                let __ite_cond := branchActive
                if __ite_cond {
                    postDebt := sub(originalDebt, badDebt)
                    newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                    newTotalUnits := sub(totalUnits, badDebt)
                    {
                        let __ite_cond_2 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                        if __ite_cond_2 {
                            newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        }
                        if iszero(__ite_cond_2) {
                            newContinuousFeeCredit := 0
                        }
                    }
                }
                if iszero(__ite_cond) {
                    postDebt := postDebt
                }
            }
            __ret0 := postDebt
            __ret1 := newTotalUnits
            __ret2 := newLossFactor
            __ret3 := newContinuousFeeCredit
            __ret4 := branchActive
            leave
        }
        function internal_internal_callbackRequired(callback) -> __ret0 {
            __ret0 := iszero(eq(callback, 0))
            leave
        }
        function internal_internal_liquidatePayer(callback, msgSender) -> __ret0 {
            {
                let __ite_cond := iszero(eq(callback, 0))
                if __ite_cond {
                    __ret0 := callback
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := msgSender
                    leave
                }
            }
        }
        function internal_internal_callbackSuccessValue() -> __ret0 {
            __ret0 := 57683088179238363159977504707935902064464440500167392520350201799917296135842
            leave
        }
        function internal_internal_callbackReturnAccepted(callback, returnValue) -> __ret0 {
            __ret0 := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(returnValue, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
            leave
        }
        function internal_internal_liquidateReturn(seizedAssets, repaidUnits) -> __ret0, __ret1 {
            __ret0 := seizedAssets
            __ret1 := repaidUnits
            leave
        }
        function internal_internal_normalModeRcfLocalSequence(currentDebt, maxDebt, inputRepaidUnits, inputSeizedAssets, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, rcfThreshold) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8 {
            let repaidUnits := inputRepaidUnits
            let seizedAssets := inputSeizedAssets
            {
                let __ite_cond := gt(inputSeizedAssets, 0)
                if __ite_cond {
                    repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                }
                if iszero(__ite_cond) {
                    seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                }
            }
            let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
            {
                let __ite_cond := lt(lltv, 1000000000000000000)
                if __ite_cond {
                    maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                }
                if iszero(__ite_cond) {
                    maxRepaid := maxRepaid
                }
            }
            let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
            let capacityShortfall := 0
            {
                let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                if __ite_cond {
                    capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                }
                if iszero(__ite_cond) {
                    capacityShortfall := capacityShortfall
                }
            }
            let rcfAccepted := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold))))
            let newCollateral := sub(collateral, seizedAssets)
            let postBitmap := collateralBitmap
            {
                let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                if __ite_cond {
                    postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                }
                if iszero(__ite_cond) {
                    postBitmap := postBitmap
                }
            }
            let postWithdrawable := add(withdrawable, repaidUnits)
            let finalDebt := sub(currentDebt, repaidUnits)
            __ret0 := repaidUnits
            __ret1 := seizedAssets
            __ret2 := maxRepaid
            __ret3 := newCollateral
            __ret4 := postBitmap
            __ret5 := postWithdrawable
            __ret6 := finalDebt
            __ret7 := rcfAccepted
            __ret8 := capacityShortfall
            leave
        }
        function internal_internal_normalModeRepaidInputLocalSequence(originalDebt, badDebt, maxDebt, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, callback, msgSender, callbackReturn) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10 {
            let currentDebt := sub(originalDebt, badDebt)
            let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
            {
                let __ite_cond := lt(lltv, 1000000000000000000)
                if __ite_cond {
                    maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                }
                if iszero(__ite_cond) {
                    maxRepaid := maxRepaid
                }
            }
            let repaidUnits := maxRepaid
            let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
            let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
            let newCollateral := sub(collateral, seizedAssets)
            let postBitmap := collateralBitmap
            {
                let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                if __ite_cond {
                    postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                }
                if iszero(__ite_cond) {
                    postBitmap := postBitmap
                }
            }
            let postWithdrawable := add(withdrawable, repaidUnits)
            let finalDebt := sub(currentDebt, repaidUnits)
            let payer := msgSender
            {
                let __ite_cond := iszero(eq(callback, 0))
                if __ite_cond {
                    payer := callback
                }
                if iszero(__ite_cond) {
                    payer := payer
                }
            }
            let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
            __ret0 := currentDebt
            __ret1 := maxRepaid
            __ret2 := seizedAssets
            __ret3 := newCollateral
            __ret4 := postBitmap
            __ret5 := postWithdrawable
            __ret6 := finalDebt
            __ret7 := payer
            __ret8 := callbackAccepted
            __ret9 := seizedAssets
            __ret10 := repaidUnits
            leave
        }
        function internal_internal_normalModeLiquidateLocalSequence(originalDebt, totalUnits, oldLossFactor, continuousFeeCredit, badDebt, maxDebt, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, callback, msgSender, callbackReturn) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10, __ret11, __ret12, __ret13, __ret14 {
            let postBadDebtDebt := originalDebt
            let newTotalUnits := totalUnits
            let newLossFactor := oldLossFactor
            let newContinuousFeeCredit := continuousFeeCredit
            {
                let __ite_cond := gt(badDebt, 0)
                if __ite_cond {
                    postBadDebtDebt := sub(originalDebt, badDebt)
                    newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                    newTotalUnits := sub(totalUnits, badDebt)
                    {
                        let __ite_cond_3 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                        if __ite_cond_3 {
                            newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        }
                        if iszero(__ite_cond_3) {
                            newContinuousFeeCredit := 0
                        }
                    }
                }
                if iszero(__ite_cond) {
                    postBadDebtDebt := postBadDebtDebt
                }
            }
            let currentDebt := postBadDebtDebt
            let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
            {
                let __ite_cond := lt(lltv, 1000000000000000000)
                if __ite_cond {
                    maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                }
                if iszero(__ite_cond) {
                    maxRepaid := maxRepaid
                }
            }
            let repaidUnits := maxRepaid
            let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
            let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
            let newCollateral := sub(collateral, seizedAssets)
            let postBitmap := collateralBitmap
            {
                let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                if __ite_cond {
                    postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                }
                if iszero(__ite_cond) {
                    postBitmap := postBitmap
                }
            }
            let postWithdrawable := add(withdrawable, repaidUnits)
            let finalDebt := sub(currentDebt, repaidUnits)
            let payer := msgSender
            {
                let __ite_cond := iszero(eq(callback, 0))
                if __ite_cond {
                    payer := callback
                }
                if iszero(__ite_cond) {
                    payer := payer
                }
            }
            let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
            __ret0 := postBadDebtDebt
            __ret1 := newTotalUnits
            __ret2 := newLossFactor
            __ret3 := newContinuousFeeCredit
            __ret4 := currentDebt
            __ret5 := maxRepaid
            __ret6 := seizedAssets
            __ret7 := newCollateral
            __ret8 := postBitmap
            __ret9 := postWithdrawable
            __ret10 := finalDebt
            __ret11 := payer
            __ret12 := callbackAccepted
            __ret13 := seizedAssets
            __ret14 := repaidUnits
            leave
        }
        function internal_internal_badDebtCollateralRepayable(collateral, price, maxLif) -> __ret0 {
            let quoted := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
            __ret0 := div(add(mul(quoted, 1000000000000000000), sub(maxLif, 1)), maxLif)
            leave
        }
        function internal_internal_collateralLoopStep(maxDebt, liquidatedCollatPrice, badDebt, collateralBitmap, slotIndex, selectedIndex, collateral, price, lltv, maxLif) -> __ret0, __ret1, __ret2, __ret3 {
            let collateralQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
            let nextMaxDebt := add(maxDebt, div(mul(collateralQuote, lltv), 1000000000000000000))
            let nextLiquidatedCollatPrice := liquidatedCollatPrice
            {
                let __ite_cond := eq(slotIndex, selectedIndex)
                if __ite_cond {
                    nextLiquidatedCollatPrice := price
                }
                if iszero(__ite_cond) {
                    nextLiquidatedCollatPrice := nextLiquidatedCollatPrice
                }
            }
            let repayableQuote := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
            let repayable := div(add(mul(repayableQuote, 1000000000000000000), sub(maxLif, 1)), maxLif)
            let nextBadDebt := 0
            {
                let __ite_cond := gt(badDebt, repayable)
                if __ite_cond {
                    nextBadDebt := sub(badDebt, repayable)
                }
                if iszero(__ite_cond) {
                    nextBadDebt := nextBadDebt
                }
            }
            let nextBitmap := and(collateralBitmap, not(shl(slotIndex, 1)))
            __ret0 := nextMaxDebt
            __ret1 := nextLiquidatedCollatPrice
            __ret2 := nextBadDebt
            __ret3 := nextBitmap
            leave
        }
        function internal_internal_rcfAllows(repaidUnits, maxRepaid, collateral, liquidatedCollatPrice, lif, rcfThreshold) -> __ret0 {
            let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
            __ret0 := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(sub(collateralRepayCapacity, maxRepaid), rcfThreshold))))
            leave
        }
        function internal_internal_rcfAllowsZeroFloor(repaidUnits, maxRepaid, collateral, liquidatedCollatPrice, lif, rcfThreshold) -> __ret0 {
            let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
            let capacityShortfall := 0
            {
                let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                if __ite_cond {
                    capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                }
                if iszero(__ite_cond) {
                    capacityShortfall := capacityShortfall
                }
            }
            __ret0 := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold))))
            leave
        }
        function internal_internal_normalModeMaxRepaidHealthyWithin3(debt, otherMaxDebt, collateral, price, lif, lltv) -> __ret0 {
            let selectedQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
            let selectedMaxDebt := div(mul(selectedQuote, lltv), 1000000000000000000)
            let maxDebt := add(otherMaxDebt, selectedMaxDebt)
            let repaid := div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
            let repayValue := div(mul(repaid, lif), 1000000000000000000)
            let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), price)
            let postDebt := sub(debt, repaid)
            let postCollateral := sub(collateral, seizedAssets)
            let postQuote := div(mul(postCollateral, price), 1000000000000000000000000000000000000)
            let postSelectedMaxDebt := div(mul(postQuote, lltv), 1000000000000000000)
            let postMaxDebt := add(otherMaxDebt, postSelectedMaxDebt)
            __ret0 := iszero(gt(postDebt, add(postMaxDebt, 3)))
            leave
        }
        function internal_internal_lossFactorAfterBadDebt(totalUnits, badDebt, oldLossFactor) -> __ret0 {
            __ret0 := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
            leave
        }
        function internal_internal_totalUnitsAfterBadDebt(totalUnits, badDebt) -> __ret0 {
            __ret0 := sub(totalUnits, badDebt)
            leave
        }
        function internal_internal_continuousFeeCreditAfterBadDebt(continuousFeeCredit, oldLossFactor, newLossFactor) -> __ret0 {
            {
                let __ite_cond := lt(oldLossFactor, 340282366920938463463374607431768211455)
                if __ite_cond {
                    __ret0 := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 0
                    leave
                }
            }
        }
        function internal_internal_badDebtCoversTwoPostSlashCredits(totalUnits, badDebt, oldLossFactor, credit0, credit1) -> __ret0 {
            let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
            let newTotalUnits := sub(totalUnits, badDebt)
            let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            __ret0 := iszero(gt(add(postSlashCredit0, postSlashCredit1), newTotalUnits))
            leave
        }
        function internal_internal_badDebtCoversTwoPostUpdateCredits(totalUnits, badDebt, oldLossFactor, credit0, accruedFee0, credit1, accruedFee1) -> __ret0 {
            let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
            let newTotalUnits := sub(totalUnits, badDebt)
            let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            let postUpdateCredit0 := sub(postSlashCredit0, accruedFee0)
            let postUpdateCredit1 := sub(postSlashCredit1, accruedFee1)
            __ret0 := iszero(gt(add(postUpdateCredit0, postUpdateCredit1), newTotalUnits))
            leave
        }
        function internal_internal_badDebtCoversTwoStoredCreditsAfterUpdates(totalUnits, badDebt, oldLossFactor, credit0, credit1) -> __ret0 {
            let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
            let newTotalUnits := sub(totalUnits, badDebt)
            let storedCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            let storedCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
            __ret0 := iszero(gt(add(storedCredit0, storedCredit1), newTotalUnits))
            leave
        }
        function internal_internal_postSlashCredit(credit, marketLossFactor, lastLossFactor) -> __ret0 {
            {
                let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                if __ite_cond {
                    __ret0 := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 0
                    leave
                }
            }
        }
        function internal_internal_postSlashPendingFee(pendingFee, credit, postSlashCredit) -> __ret0 {
            {
                let __ite_cond := lt(0, credit)
                if __ite_cond {
                    __ret0 := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 0
                    leave
                }
            }
        }
        function internal_internal_accruedFee(postSlashPendingFee, lastAccrual, accrualEnd, maturity) -> __ret0 {
            {
                let __ite_cond := lt(lastAccrual, maturity)
                if __ite_cond {
                    __ret0 := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual))
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := 0
                    leave
                }
            }
        }
        function internal_internal_postUpdateCreditAfterFee(postSlashCredit, accruedFee) -> __ret0 {
            __ret0 := sub(postSlashCredit, accruedFee)
            leave
        }
        function internal_internal_updatePositionViewSequence(credit, lastLossFactor, marketLossFactor, pendingFee, lastAccrual, blockTimestamp, maturity) -> __ret0, __ret1, __ret2 {
            let postSlashCredit := 0
            {
                let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                if __ite_cond {
                    postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                }
                if iszero(__ite_cond) {
                    postSlashCredit := postSlashCredit
                }
            }
            let postSlashPendingFee := 0
            {
                let __ite_cond := gt(credit, 0)
                if __ite_cond {
                    postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                }
                if iszero(__ite_cond) {
                    postSlashPendingFee := postSlashPendingFee
                }
            }
            let accrualEnd := maturity
            {
                let __ite_cond := iszero(gt(blockTimestamp, maturity))
                if __ite_cond {
                    accrualEnd := blockTimestamp
                }
                if iszero(__ite_cond) {
                    accrualEnd := accrualEnd
                }
            }
            let fee := 0
            {
                let __ite_cond := lt(lastAccrual, maturity)
                if __ite_cond {
                    fee := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual))
                }
                if iszero(__ite_cond) {
                    fee := fee
                }
            }
            __ret0 := sub(postSlashCredit, fee)
            __ret1 := sub(postSlashPendingFee, fee)
            __ret2 := fee
            leave
        }
        datacopy(0, dataoffset("runtime"), datasize("runtime"))
        return(0, datasize("runtime"))
    }
    object "runtime" {
        code {
            function internal_internal_maxRepaid(debt, maxDebt, lif, lltv) -> __ret0 {
                {
                    let __ite_cond := lt(lltv, 1000000000000000000)
                    if __ite_cond {
                        __ret0 := div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                        leave
                    }
                }
            }
            function internal_internal_seizedFromRepaid(repaidUnits, lif, liquidatedCollatPrice) -> __ret0 {
                let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                __ret0 := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
                leave
            }
            function internal_internal_repaidFromSeized(seizedAssets, lif, liquidatedCollatPrice) -> __ret0 {
                let seizedValue := div(add(mul(seizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                __ret0 := div(add(mul(seizedValue, 1000000000000000000), sub(lif, 1)), lif)
                leave
            }
            function internal_internal_repaySeizeAmounts(inputRepaidUnits, inputSeizedAssets, lif, liquidatedCollatPrice) -> __ret0, __ret1 {
                {
                    let __ite_cond := gt(inputSeizedAssets, 0)
                    if __ite_cond {
                        let repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                        __ret0 := repaidUnits
                        __ret1 := inputSeizedAssets
                        leave
                    }
                    if iszero(__ite_cond) {
                        let seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                        __ret0 := inputRepaidUnits
                        __ret1 := seizedAssets
                        leave
                    }
                }
            }
            function internal_internal_collateralMaxDebt(collateral, price, lltv) -> __ret0 {
                let quoted := div(mul(collateral, price), 1000000000000000000000000000000000000)
                __ret0 := div(mul(quoted, lltv), 1000000000000000000)
                leave
            }
            function internal_internal_postDebt(debt, repaidUnits) -> __ret0 {
                __ret0 := sub(debt, repaidUnits)
                leave
            }
            function internal_internal_postCollateral(collateral, seizedAssets) -> __ret0 {
                __ret0 := sub(collateral, seizedAssets)
                leave
            }
            function internal_internal_zeroFloorSub(x, y) -> __ret0 {
                {
                    let __ite_cond := gt(x, y)
                    if __ite_cond {
                        __ret0 := sub(x, y)
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 0
                        leave
                    }
                }
            }
            function internal_internal_withdrawableAfterRepay(withdrawable, repaidUnits) -> __ret0 {
                __ret0 := add(withdrawable, repaidUnits)
                leave
            }
            function internal_internal_clearBitmapBit(bitmap, selectedIndex) -> __ret0 {
                let mask := not(shl(selectedIndex, 1))
                __ret0 := and(bitmap, mask)
                leave
            }
            function internal_internal_atMostOneNonZero(repaidUnits, seizedAssets) -> __ret0 {
                __ret0 := or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0))))
                leave
            }
            function internal_internal_borrowerDebtPositive(debt) -> __ret0 {
                __ret0 := gt(debt, 0)
                leave
            }
            function internal_internal_liquidatorGateAllows(liquidatorGate, canLiquidate) -> __ret0 {
                __ret0 := or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate)))
                leave
            }
            function internal_internal_normalModeLiquidatable(liquidationUnlocked, originalDebt, maxDebt) -> __ret0 {
                __ret0 := and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt))))
                leave
            }
            function internal_internal_liquidatableGuard(liquidationLocked, postMaturityMode, blockTimestamp, maturity, originalDebt, maxDebt) -> __ret0 {
                {
                    let __ite_cond := postMaturityMode
                    if __ite_cond {
                        __ret0 := and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(blockTimestamp, maturity))))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(originalDebt, maxDebt))))
                        leave
                    }
                }
            }
            function internal_internal_normalModeEntryGuards(repaidUnits, seizedAssets, debt, liquidatorGate, canLiquidate, liquidationUnlocked, originalDebt, maxDebt) -> __ret0 {
                __ret0 := and(iszero(iszero(and(iszero(iszero(and(iszero(iszero(or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0)))))), iszero(iszero(gt(debt, 0)))))), iszero(iszero(or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate)))))))), iszero(iszero(and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt)))))))
                leave
            }
            function internal_internal_repayBranchActive(repaidUnits, seizedAssets) -> __ret0 {
                __ret0 := or(iszero(iszero(gt(repaidUnits, 0))), iszero(iszero(gt(seizedAssets, 0))))
                leave
            }
            function internal_internal_liquidationLif(postMaturityMode, maxLif, blockTimestamp, maturity) -> __ret0 {
                {
                    let __ite_cond := postMaturityMode
                    if __ite_cond {
                        let elapsed := sub(blockTimestamp, maturity)
                        let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLif, 1000000000000000000), elapsed), 900))
                        {
                            let __ite_cond_4 := iszero(gt(maxLif, postMaturityLif))
                            if __ite_cond_4 {
                                __ret0 := maxLif
                                leave
                            }
                            if iszero(__ite_cond_4) {
                                __ret0 := postMaturityLif
                                leave
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        __ret0 := maxLif
                        leave
                    }
                }
            }
            function internal_internal_badDebtBranchActive(badDebt) -> __ret0 {
                __ret0 := gt(badDebt, 0)
                leave
            }
            function internal_internal_badDebtPostDebt(originalDebt, badDebt) -> __ret0 {
                __ret0 := sub(originalDebt, badDebt)
                leave
            }
            function internal_internal_badDebtLocalSequence(originalDebt, totalUnits, oldLossFactor, continuousFeeCredit, badDebt) -> __ret0, __ret1, __ret2, __ret3, __ret4 {
                let postDebt := originalDebt
                let newTotalUnits := totalUnits
                let newLossFactor := oldLossFactor
                let newContinuousFeeCredit := continuousFeeCredit
                let branchActive := gt(badDebt, 0)
                {
                    let __ite_cond := branchActive
                    if __ite_cond {
                        postDebt := sub(originalDebt, badDebt)
                        newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                        newTotalUnits := sub(totalUnits, badDebt)
                        {
                            let __ite_cond_5 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                            if __ite_cond_5 {
                                newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                            }
                            if iszero(__ite_cond_5) {
                                newContinuousFeeCredit := 0
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        postDebt := postDebt
                    }
                }
                __ret0 := postDebt
                __ret1 := newTotalUnits
                __ret2 := newLossFactor
                __ret3 := newContinuousFeeCredit
                __ret4 := branchActive
                leave
            }
            function internal_internal_callbackRequired(callback) -> __ret0 {
                __ret0 := iszero(eq(callback, 0))
                leave
            }
            function internal_internal_liquidatePayer(callback, msgSender) -> __ret0 {
                {
                    let __ite_cond := iszero(eq(callback, 0))
                    if __ite_cond {
                        __ret0 := callback
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := msgSender
                        leave
                    }
                }
            }
            function internal_internal_callbackSuccessValue() -> __ret0 {
                __ret0 := 57683088179238363159977504707935902064464440500167392520350201799917296135842
                leave
            }
            function internal_internal_callbackReturnAccepted(callback, returnValue) -> __ret0 {
                __ret0 := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(returnValue, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
                leave
            }
            function internal_internal_liquidateReturn(seizedAssets, repaidUnits) -> __ret0, __ret1 {
                __ret0 := seizedAssets
                __ret1 := repaidUnits
                leave
            }
            function internal_internal_normalModeRcfLocalSequence(currentDebt, maxDebt, inputRepaidUnits, inputSeizedAssets, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, rcfThreshold) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8 {
                let repaidUnits := inputRepaidUnits
                let seizedAssets := inputSeizedAssets
                {
                    let __ite_cond := gt(inputSeizedAssets, 0)
                    if __ite_cond {
                        repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                    }
                    if iszero(__ite_cond) {
                        seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                    }
                }
                let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                {
                    let __ite_cond := lt(lltv, 1000000000000000000)
                    if __ite_cond {
                        maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                    }
                    if iszero(__ite_cond) {
                        maxRepaid := maxRepaid
                    }
                }
                let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                let capacityShortfall := 0
                {
                    let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                    if __ite_cond {
                        capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                    }
                    if iszero(__ite_cond) {
                        capacityShortfall := capacityShortfall
                    }
                }
                let rcfAccepted := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold))))
                let newCollateral := sub(collateral, seizedAssets)
                let postBitmap := collateralBitmap
                {
                    let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                    if __ite_cond {
                        postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                    }
                    if iszero(__ite_cond) {
                        postBitmap := postBitmap
                    }
                }
                let postWithdrawable := add(withdrawable, repaidUnits)
                let finalDebt := sub(currentDebt, repaidUnits)
                __ret0 := repaidUnits
                __ret1 := seizedAssets
                __ret2 := maxRepaid
                __ret3 := newCollateral
                __ret4 := postBitmap
                __ret5 := postWithdrawable
                __ret6 := finalDebt
                __ret7 := rcfAccepted
                __ret8 := capacityShortfall
                leave
            }
            function internal_internal_normalModeRepaidInputLocalSequence(originalDebt, badDebt, maxDebt, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, callback, msgSender, callbackReturn) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10 {
                let currentDebt := sub(originalDebt, badDebt)
                let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                {
                    let __ite_cond := lt(lltv, 1000000000000000000)
                    if __ite_cond {
                        maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                    }
                    if iszero(__ite_cond) {
                        maxRepaid := maxRepaid
                    }
                }
                let repaidUnits := maxRepaid
                let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
                let newCollateral := sub(collateral, seizedAssets)
                let postBitmap := collateralBitmap
                {
                    let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                    if __ite_cond {
                        postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                    }
                    if iszero(__ite_cond) {
                        postBitmap := postBitmap
                    }
                }
                let postWithdrawable := add(withdrawable, repaidUnits)
                let finalDebt := sub(currentDebt, repaidUnits)
                let payer := msgSender
                {
                    let __ite_cond := iszero(eq(callback, 0))
                    if __ite_cond {
                        payer := callback
                    }
                    if iszero(__ite_cond) {
                        payer := payer
                    }
                }
                let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
                __ret0 := currentDebt
                __ret1 := maxRepaid
                __ret2 := seizedAssets
                __ret3 := newCollateral
                __ret4 := postBitmap
                __ret5 := postWithdrawable
                __ret6 := finalDebt
                __ret7 := payer
                __ret8 := callbackAccepted
                __ret9 := seizedAssets
                __ret10 := repaidUnits
                leave
            }
            function internal_internal_normalModeLiquidateLocalSequence(originalDebt, totalUnits, oldLossFactor, continuousFeeCredit, badDebt, maxDebt, lltv, lif, liquidatedCollatPrice, collateral, collateralBitmap, collateralIndex, withdrawable, callback, msgSender, callbackReturn) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10, __ret11, __ret12, __ret13, __ret14 {
                let postBadDebtDebt := originalDebt
                let newTotalUnits := totalUnits
                let newLossFactor := oldLossFactor
                let newContinuousFeeCredit := continuousFeeCredit
                {
                    let __ite_cond := gt(badDebt, 0)
                    if __ite_cond {
                        postBadDebtDebt := sub(originalDebt, badDebt)
                        newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                        newTotalUnits := sub(totalUnits, badDebt)
                        {
                            let __ite_cond_6 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                            if __ite_cond_6 {
                                newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                            }
                            if iszero(__ite_cond_6) {
                                newContinuousFeeCredit := 0
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        postBadDebtDebt := postBadDebtDebt
                    }
                }
                let currentDebt := postBadDebtDebt
                let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                {
                    let __ite_cond := lt(lltv, 1000000000000000000)
                    if __ite_cond {
                        maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                    }
                    if iszero(__ite_cond) {
                        maxRepaid := maxRepaid
                    }
                }
                let repaidUnits := maxRepaid
                let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
                let newCollateral := sub(collateral, seizedAssets)
                let postBitmap := collateralBitmap
                {
                    let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                    if __ite_cond {
                        postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                    }
                    if iszero(__ite_cond) {
                        postBitmap := postBitmap
                    }
                }
                let postWithdrawable := add(withdrawable, repaidUnits)
                let finalDebt := sub(currentDebt, repaidUnits)
                let payer := msgSender
                {
                    let __ite_cond := iszero(eq(callback, 0))
                    if __ite_cond {
                        payer := callback
                    }
                    if iszero(__ite_cond) {
                        payer := payer
                    }
                }
                let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
                __ret0 := postBadDebtDebt
                __ret1 := newTotalUnits
                __ret2 := newLossFactor
                __ret3 := newContinuousFeeCredit
                __ret4 := currentDebt
                __ret5 := maxRepaid
                __ret6 := seizedAssets
                __ret7 := newCollateral
                __ret8 := postBitmap
                __ret9 := postWithdrawable
                __ret10 := finalDebt
                __ret11 := payer
                __ret12 := callbackAccepted
                __ret13 := seizedAssets
                __ret14 := repaidUnits
                leave
            }
            function internal_internal_badDebtCollateralRepayable(collateral, price, maxLif) -> __ret0 {
                let quoted := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                __ret0 := div(add(mul(quoted, 1000000000000000000), sub(maxLif, 1)), maxLif)
                leave
            }
            function internal_internal_collateralLoopStep(maxDebt, liquidatedCollatPrice, badDebt, collateralBitmap, slotIndex, selectedIndex, collateral, price, lltv, maxLif) -> __ret0, __ret1, __ret2, __ret3 {
                let collateralQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
                let nextMaxDebt := add(maxDebt, div(mul(collateralQuote, lltv), 1000000000000000000))
                let nextLiquidatedCollatPrice := liquidatedCollatPrice
                {
                    let __ite_cond := eq(slotIndex, selectedIndex)
                    if __ite_cond {
                        nextLiquidatedCollatPrice := price
                    }
                    if iszero(__ite_cond) {
                        nextLiquidatedCollatPrice := nextLiquidatedCollatPrice
                    }
                }
                let repayableQuote := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                let repayable := div(add(mul(repayableQuote, 1000000000000000000), sub(maxLif, 1)), maxLif)
                let nextBadDebt := 0
                {
                    let __ite_cond := gt(badDebt, repayable)
                    if __ite_cond {
                        nextBadDebt := sub(badDebt, repayable)
                    }
                    if iszero(__ite_cond) {
                        nextBadDebt := nextBadDebt
                    }
                }
                let nextBitmap := and(collateralBitmap, not(shl(slotIndex, 1)))
                __ret0 := nextMaxDebt
                __ret1 := nextLiquidatedCollatPrice
                __ret2 := nextBadDebt
                __ret3 := nextBitmap
                leave
            }
            function internal_internal_rcfAllows(repaidUnits, maxRepaid, collateral, liquidatedCollatPrice, lif, rcfThreshold) -> __ret0 {
                let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                __ret0 := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(sub(collateralRepayCapacity, maxRepaid), rcfThreshold))))
                leave
            }
            function internal_internal_rcfAllowsZeroFloor(repaidUnits, maxRepaid, collateral, liquidatedCollatPrice, lif, rcfThreshold) -> __ret0 {
                let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                let capacityShortfall := 0
                {
                    let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                    if __ite_cond {
                        capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                    }
                    if iszero(__ite_cond) {
                        capacityShortfall := capacityShortfall
                    }
                }
                __ret0 := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold))))
                leave
            }
            function internal_internal_normalModeMaxRepaidHealthyWithin3(debt, otherMaxDebt, collateral, price, lif, lltv) -> __ret0 {
                let selectedQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
                let selectedMaxDebt := div(mul(selectedQuote, lltv), 1000000000000000000)
                let maxDebt := add(otherMaxDebt, selectedMaxDebt)
                let repaid := div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                let repayValue := div(mul(repaid, lif), 1000000000000000000)
                let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), price)
                let postDebt := sub(debt, repaid)
                let postCollateral := sub(collateral, seizedAssets)
                let postQuote := div(mul(postCollateral, price), 1000000000000000000000000000000000000)
                let postSelectedMaxDebt := div(mul(postQuote, lltv), 1000000000000000000)
                let postMaxDebt := add(otherMaxDebt, postSelectedMaxDebt)
                __ret0 := iszero(gt(postDebt, add(postMaxDebt, 3)))
                leave
            }
            function internal_internal_lossFactorAfterBadDebt(totalUnits, badDebt, oldLossFactor) -> __ret0 {
                __ret0 := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                leave
            }
            function internal_internal_totalUnitsAfterBadDebt(totalUnits, badDebt) -> __ret0 {
                __ret0 := sub(totalUnits, badDebt)
                leave
            }
            function internal_internal_continuousFeeCreditAfterBadDebt(continuousFeeCredit, oldLossFactor, newLossFactor) -> __ret0 {
                {
                    let __ite_cond := lt(oldLossFactor, 340282366920938463463374607431768211455)
                    if __ite_cond {
                        __ret0 := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 0
                        leave
                    }
                }
            }
            function internal_internal_badDebtCoversTwoPostSlashCredits(totalUnits, badDebt, oldLossFactor, credit0, credit1) -> __ret0 {
                let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                let newTotalUnits := sub(totalUnits, badDebt)
                let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                __ret0 := iszero(gt(add(postSlashCredit0, postSlashCredit1), newTotalUnits))
                leave
            }
            function internal_internal_badDebtCoversTwoPostUpdateCredits(totalUnits, badDebt, oldLossFactor, credit0, accruedFee0, credit1, accruedFee1) -> __ret0 {
                let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                let newTotalUnits := sub(totalUnits, badDebt)
                let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                let postUpdateCredit0 := sub(postSlashCredit0, accruedFee0)
                let postUpdateCredit1 := sub(postSlashCredit1, accruedFee1)
                __ret0 := iszero(gt(add(postUpdateCredit0, postUpdateCredit1), newTotalUnits))
                leave
            }
            function internal_internal_badDebtCoversTwoStoredCreditsAfterUpdates(totalUnits, badDebt, oldLossFactor, credit0, credit1) -> __ret0 {
                let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                let newTotalUnits := sub(totalUnits, badDebt)
                let storedCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                let storedCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                __ret0 := iszero(gt(add(storedCredit0, storedCredit1), newTotalUnits))
                leave
            }
            function internal_internal_postSlashCredit(credit, marketLossFactor, lastLossFactor) -> __ret0 {
                {
                    let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                    if __ite_cond {
                        __ret0 := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 0
                        leave
                    }
                }
            }
            function internal_internal_postSlashPendingFee(pendingFee, credit, postSlashCredit) -> __ret0 {
                {
                    let __ite_cond := lt(0, credit)
                    if __ite_cond {
                        __ret0 := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 0
                        leave
                    }
                }
            }
            function internal_internal_accruedFee(postSlashPendingFee, lastAccrual, accrualEnd, maturity) -> __ret0 {
                {
                    let __ite_cond := lt(lastAccrual, maturity)
                    if __ite_cond {
                        __ret0 := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual))
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := 0
                        leave
                    }
                }
            }
            function internal_internal_postUpdateCreditAfterFee(postSlashCredit, accruedFee) -> __ret0 {
                __ret0 := sub(postSlashCredit, accruedFee)
                leave
            }
            function internal_internal_updatePositionViewSequence(credit, lastLossFactor, marketLossFactor, pendingFee, lastAccrual, blockTimestamp, maturity) -> __ret0, __ret1, __ret2 {
                let postSlashCredit := 0
                {
                    let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                    if __ite_cond {
                        postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                    }
                    if iszero(__ite_cond) {
                        postSlashCredit := postSlashCredit
                    }
                }
                let postSlashPendingFee := 0
                {
                    let __ite_cond := gt(credit, 0)
                    if __ite_cond {
                        postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                    }
                    if iszero(__ite_cond) {
                        postSlashPendingFee := postSlashPendingFee
                    }
                }
                let accrualEnd := maturity
                {
                    let __ite_cond := iszero(gt(blockTimestamp, maturity))
                    if __ite_cond {
                        accrualEnd := blockTimestamp
                    }
                    if iszero(__ite_cond) {
                        accrualEnd := accrualEnd
                    }
                }
                let fee := 0
                {
                    let __ite_cond := lt(lastAccrual, maturity)
                    if __ite_cond {
                        fee := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual))
                    }
                    if iszero(__ite_cond) {
                        fee := fee
                    }
                }
                __ret0 := sub(postSlashCredit, fee)
                __ret1 := sub(postSlashPendingFee, fee)
                __ret2 := fee
                leave
            }
            mstore(64, 128)
            {
                let __has_selector := iszero(lt(calldatasize(), 4))
                if iszero(__has_selector) {
                    revert(0, 0)
                }
                if __has_selector {
                    switch shr(224, calldataload(0))
                    case 0x7f29ea64 {
                        /* maxRepaid() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let debt := calldataload(4)
                        let maxDebt := calldataload(36)
                        let lif := calldataload(68)
                        let lltv := calldataload(100)
                        {
                            let __ite_cond := lt(lltv, 1000000000000000000)
                            if __ite_cond {
                                mstore(0, div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv))))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 115792089237316195423570985008687907853269984665640564039457584007913129639935)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x88baeb07 {
                        /* seizedFromRepaid() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let lif := calldataload(36)
                        let liquidatedCollatPrice := calldataload(68)
                        let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                        mstore(0, div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice))
                        return(0, 32)
                    }
                    case 0x15df7a5a {
                        /* repaidFromSeized() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let seizedAssets := calldataload(4)
                        let lif := calldataload(36)
                        let liquidatedCollatPrice := calldataload(68)
                        let seizedValue := div(add(mul(seizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                        mstore(0, div(add(mul(seizedValue, 1000000000000000000), sub(lif, 1)), lif))
                        return(0, 32)
                    }
                    case 0xe75de2a9 {
                        /* repaySeizeAmounts() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let inputRepaidUnits := calldataload(4)
                        let inputSeizedAssets := calldataload(36)
                        let lif := calldataload(68)
                        let liquidatedCollatPrice := calldataload(100)
                        {
                            let __ite_cond := gt(inputSeizedAssets, 0)
                            if __ite_cond {
                                let repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                                mstore(0, repaidUnits)
                                mstore(32, inputSeizedAssets)
                                return(0, 64)
                            }
                            if iszero(__ite_cond) {
                                let seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                                mstore(0, inputRepaidUnits)
                                mstore(32, seizedAssets)
                                return(0, 64)
                            }
                        }
                    }
                    case 0x0192fcae {
                        /* collateralMaxDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let collateral := calldataload(4)
                        let price := calldataload(36)
                        let lltv := calldataload(68)
                        let quoted := div(mul(collateral, price), 1000000000000000000000000000000000000)
                        mstore(0, div(mul(quoted, lltv), 1000000000000000000))
                        return(0, 32)
                    }
                    case 0xe5468261 {
                        /* postDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let debt := calldataload(4)
                        let repaidUnits := calldataload(36)
                        mstore(0, sub(debt, repaidUnits))
                        return(0, 32)
                    }
                    case 0x7507a345 {
                        /* postCollateral() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let collateral := calldataload(4)
                        let seizedAssets := calldataload(36)
                        mstore(0, sub(collateral, seizedAssets))
                        return(0, 32)
                    }
                    case 0xb56962c5 {
                        /* zeroFloorSub() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let x := calldataload(4)
                        let y := calldataload(36)
                        {
                            let __ite_cond := gt(x, y)
                            if __ite_cond {
                                mstore(0, sub(x, y))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 0)
                                return(0, 32)
                            }
                        }
                    }
                    case 0xb08884aa {
                        /* withdrawableAfterRepay() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let withdrawable := calldataload(4)
                        let repaidUnits := calldataload(36)
                        mstore(0, add(withdrawable, repaidUnits))
                        return(0, 32)
                    }
                    case 0xd895e254 {
                        /* clearBitmapBit() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let bitmap := calldataload(4)
                        let selectedIndex := calldataload(36)
                        let mask := not(shl(selectedIndex, 1))
                        mstore(0, and(bitmap, mask))
                        return(0, 32)
                    }
                    case 0x40509e2f {
                        /* atMostOneNonZero() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let seizedAssets := calldataload(36)
                        mstore(0, or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0)))))
                        return(0, 32)
                    }
                    case 0x7994f002 {
                        /* borrowerDebtPositive() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let debt := calldataload(4)
                        mstore(0, gt(debt, 0))
                        return(0, 32)
                    }
                    case 0x4ce59c6b {
                        /* liquidatorGateAllows() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let liquidatorGate := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let canLiquidate := iszero(iszero(calldataload(36)))
                        mstore(0, or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate))))
                        return(0, 32)
                    }
                    case 0xb3a08123 {
                        /* normalModeLiquidatable() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let liquidationUnlocked := iszero(iszero(calldataload(4)))
                        let originalDebt := calldataload(36)
                        let maxDebt := calldataload(68)
                        mstore(0, and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt)))))
                        return(0, 32)
                    }
                    case 0x9e6751a0 {
                        /* liquidatableGuard() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        let liquidationLocked := iszero(iszero(calldataload(4)))
                        let postMaturityMode := iszero(iszero(calldataload(36)))
                        let blockTimestamp := calldataload(68)
                        let maturity := calldataload(100)
                        let originalDebt := calldataload(132)
                        let maxDebt := calldataload(164)
                        {
                            let __ite_cond := postMaturityMode
                            if __ite_cond {
                                mstore(0, and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(blockTimestamp, maturity)))))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, and(iszero(iszero(eq(liquidationLocked, 0))), iszero(iszero(gt(originalDebt, maxDebt)))))
                                return(0, 32)
                            }
                        }
                    }
                    case 0x3a8e3191 {
                        /* normalModeEntryGuards() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 260) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 260) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let seizedAssets := calldataload(36)
                        let debt := calldataload(68)
                        let liquidatorGate := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let canLiquidate := iszero(iszero(calldataload(132)))
                        let liquidationUnlocked := iszero(iszero(calldataload(164)))
                        let originalDebt := calldataload(196)
                        let maxDebt := calldataload(228)
                        mstore(0, and(iszero(iszero(and(iszero(iszero(and(iszero(iszero(or(iszero(iszero(eq(repaidUnits, 0))), iszero(iszero(eq(seizedAssets, 0)))))), iszero(iszero(gt(debt, 0)))))), iszero(iszero(or(iszero(iszero(eq(liquidatorGate, 0))), iszero(iszero(canLiquidate)))))))), iszero(iszero(and(iszero(iszero(liquidationUnlocked)), iszero(iszero(gt(originalDebt, maxDebt))))))))
                        return(0, 32)
                    }
                    case 0x42614078 {
                        /* repayBranchActive() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let seizedAssets := calldataload(36)
                        mstore(0, or(iszero(iszero(gt(repaidUnits, 0))), iszero(iszero(gt(seizedAssets, 0)))))
                        return(0, 32)
                    }
                    case 0x97e9d034 {
                        /* liquidationLif() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let postMaturityMode := iszero(iszero(calldataload(4)))
                        let maxLif := calldataload(36)
                        let blockTimestamp := calldataload(68)
                        let maturity := calldataload(100)
                        {
                            let __ite_cond := postMaturityMode
                            if __ite_cond {
                                let elapsed := sub(blockTimestamp, maturity)
                                let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLif, 1000000000000000000), elapsed), 900))
                                {
                                    let __ite_cond_7 := iszero(gt(maxLif, postMaturityLif))
                                    if __ite_cond_7 {
                                        mstore(0, maxLif)
                                        return(0, 32)
                                    }
                                    if iszero(__ite_cond_7) {
                                        mstore(0, postMaturityLif)
                                        return(0, 32)
                                    }
                                }
                            }
                            if iszero(__ite_cond) {
                                mstore(0, maxLif)
                                return(0, 32)
                            }
                        }
                    }
                    case 0xd2d572e8 {
                        /* badDebtBranchActive() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let badDebt := calldataload(4)
                        mstore(0, gt(badDebt, 0))
                        return(0, 32)
                    }
                    case 0xfd028155 {
                        /* badDebtPostDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let originalDebt := calldataload(4)
                        let badDebt := calldataload(36)
                        mstore(0, sub(originalDebt, badDebt))
                        return(0, 32)
                    }
                    case 0x36c586c7 {
                        /* badDebtLocalSequence() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let originalDebt := calldataload(4)
                        let totalUnits := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        let continuousFeeCredit := calldataload(100)
                        let badDebt := calldataload(132)
                        let postDebt := originalDebt
                        let newTotalUnits := totalUnits
                        let newLossFactor := oldLossFactor
                        let newContinuousFeeCredit := continuousFeeCredit
                        let branchActive := gt(badDebt, 0)
                        {
                            let __ite_cond := branchActive
                            if __ite_cond {
                                postDebt := sub(originalDebt, badDebt)
                                newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                                newTotalUnits := sub(totalUnits, badDebt)
                                {
                                    let __ite_cond_8 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                                    if __ite_cond_8 {
                                        newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                                    }
                                    if iszero(__ite_cond_8) {
                                        newContinuousFeeCredit := 0
                                    }
                                }
                            }
                            if iszero(__ite_cond) {
                                postDebt := postDebt
                            }
                        }
                        mstore(0, postDebt)
                        mstore(32, newTotalUnits)
                        mstore(64, newLossFactor)
                        mstore(96, newContinuousFeeCredit)
                        mstore(128, branchActive)
                        return(0, 160)
                    }
                    case 0x6474182e {
                        /* callbackRequired() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let callback := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        mstore(0, iszero(eq(callback, 0)))
                        return(0, 32)
                    }
                    case 0xf0a99bb8 {
                        /* liquidatePayer() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let callback := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let msgSender := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        {
                            let __ite_cond := iszero(eq(callback, 0))
                            if __ite_cond {
                                mstore(0, callback)
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, msgSender)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x92f39bf7 {
                        /* callbackSuccessValue() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        mstore(0, 57683088179238363159977504707935902064464440500167392520350201799917296135842)
                        return(0, 32)
                    }
                    case 0x9c6f45df {
                        /* callbackReturnAccepted() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let callback := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let returnValue := calldataload(36)
                        mstore(0, or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(returnValue, 57683088179238363159977504707935902064464440500167392520350201799917296135842)))))
                        return(0, 32)
                    }
                    case 0x31bdeb06 {
                        /* liquidateReturn() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let seizedAssets := calldataload(4)
                        let repaidUnits := calldataload(36)
                        mstore(0, seizedAssets)
                        mstore(32, repaidUnits)
                        return(0, 64)
                    }
                    case 0x6e339814 {
                        /* normalModeRcfLocalSequence() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 388) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 388) {
                            revert(0, 0)
                        }
                        let currentDebt := calldataload(4)
                        let maxDebt := calldataload(36)
                        let inputRepaidUnits := calldataload(68)
                        let inputSeizedAssets := calldataload(100)
                        let lltv := calldataload(132)
                        let lif := calldataload(164)
                        let liquidatedCollatPrice := calldataload(196)
                        let collateral := calldataload(228)
                        let collateralBitmap := calldataload(260)
                        let collateralIndex := calldataload(292)
                        let withdrawable := calldataload(324)
                        let rcfThreshold := calldataload(356)
                        let repaidUnits := inputRepaidUnits
                        let seizedAssets := inputSeizedAssets
                        {
                            let __ite_cond := gt(inputSeizedAssets, 0)
                            if __ite_cond {
                                repaidUnits := div(add(mul(div(add(mul(inputSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                            }
                            if iszero(__ite_cond) {
                                seizedAssets := div(mul(div(mul(inputRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                            }
                        }
                        let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                        {
                            let __ite_cond := lt(lltv, 1000000000000000000)
                            if __ite_cond {
                                maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                            }
                            if iszero(__ite_cond) {
                                maxRepaid := maxRepaid
                            }
                        }
                        let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                        let capacityShortfall := 0
                        {
                            let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                            if __ite_cond {
                                capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                            }
                            if iszero(__ite_cond) {
                                capacityShortfall := capacityShortfall
                            }
                        }
                        let rcfAccepted := or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold))))
                        let newCollateral := sub(collateral, seizedAssets)
                        let postBitmap := collateralBitmap
                        {
                            let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                            if __ite_cond {
                                postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                            }
                            if iszero(__ite_cond) {
                                postBitmap := postBitmap
                            }
                        }
                        let postWithdrawable := add(withdrawable, repaidUnits)
                        let finalDebt := sub(currentDebt, repaidUnits)
                        mstore(0, repaidUnits)
                        mstore(32, seizedAssets)
                        mstore(64, maxRepaid)
                        mstore(96, newCollateral)
                        mstore(128, postBitmap)
                        mstore(160, postWithdrawable)
                        mstore(192, finalDebt)
                        mstore(224, rcfAccepted)
                        mstore(256, capacityShortfall)
                        return(0, 288)
                    }
                    case 0xb4167e78 {
                        /* normalModeRepaidInputLocalSequence() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 420) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 420) {
                            revert(0, 0)
                        }
                        let originalDebt := calldataload(4)
                        let badDebt := calldataload(36)
                        let maxDebt := calldataload(68)
                        let lltv := calldataload(100)
                        let lif := calldataload(132)
                        let liquidatedCollatPrice := calldataload(164)
                        let collateral := calldataload(196)
                        let collateralBitmap := calldataload(228)
                        let collateralIndex := calldataload(260)
                        let withdrawable := calldataload(292)
                        let callback := and(calldataload(324), 0xffffffffffffffffffffffffffffffffffffffff)
                        let msgSender := and(calldataload(356), 0xffffffffffffffffffffffffffffffffffffffff)
                        let callbackReturn := calldataload(388)
                        let currentDebt := sub(originalDebt, badDebt)
                        let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                        {
                            let __ite_cond := lt(lltv, 1000000000000000000)
                            if __ite_cond {
                                maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                            }
                            if iszero(__ite_cond) {
                                maxRepaid := maxRepaid
                            }
                        }
                        let repaidUnits := maxRepaid
                        let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                        let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
                        let newCollateral := sub(collateral, seizedAssets)
                        let postBitmap := collateralBitmap
                        {
                            let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                            if __ite_cond {
                                postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                            }
                            if iszero(__ite_cond) {
                                postBitmap := postBitmap
                            }
                        }
                        let postWithdrawable := add(withdrawable, repaidUnits)
                        let finalDebt := sub(currentDebt, repaidUnits)
                        let payer := msgSender
                        {
                            let __ite_cond := iszero(eq(callback, 0))
                            if __ite_cond {
                                payer := callback
                            }
                            if iszero(__ite_cond) {
                                payer := payer
                            }
                        }
                        let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
                        mstore(0, currentDebt)
                        mstore(32, maxRepaid)
                        mstore(64, seizedAssets)
                        mstore(96, newCollateral)
                        mstore(128, postBitmap)
                        mstore(160, postWithdrawable)
                        mstore(192, finalDebt)
                        mstore(224, payer)
                        mstore(256, callbackAccepted)
                        mstore(288, seizedAssets)
                        mstore(320, repaidUnits)
                        return(0, 352)
                    }
                    case 0x0675db6b {
                        /* normalModeLiquidateLocalSequence() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 516) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 516) {
                            revert(0, 0)
                        }
                        let originalDebt := calldataload(4)
                        let totalUnits := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        let continuousFeeCredit := calldataload(100)
                        let badDebt := calldataload(132)
                        let maxDebt := calldataload(164)
                        let lltv := calldataload(196)
                        let lif := calldataload(228)
                        let liquidatedCollatPrice := calldataload(260)
                        let collateral := calldataload(292)
                        let collateralBitmap := calldataload(324)
                        let collateralIndex := calldataload(356)
                        let withdrawable := calldataload(388)
                        let callback := and(calldataload(420), 0xffffffffffffffffffffffffffffffffffffffff)
                        let msgSender := and(calldataload(452), 0xffffffffffffffffffffffffffffffffffffffff)
                        let callbackReturn := calldataload(484)
                        let postBadDebtDebt := originalDebt
                        let newTotalUnits := totalUnits
                        let newLossFactor := oldLossFactor
                        let newContinuousFeeCredit := continuousFeeCredit
                        {
                            let __ite_cond := gt(badDebt, 0)
                            if __ite_cond {
                                postBadDebtDebt := sub(originalDebt, badDebt)
                                newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                                newTotalUnits := sub(totalUnits, badDebt)
                                {
                                    let __ite_cond_9 := lt(oldLossFactor, 340282366920938463463374607431768211455)
                                    if __ite_cond_9 {
                                        newContinuousFeeCredit := div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                                    }
                                    if iszero(__ite_cond_9) {
                                        newContinuousFeeCredit := 0
                                    }
                                }
                            }
                            if iszero(__ite_cond) {
                                postBadDebtDebt := postBadDebtDebt
                            }
                        }
                        let currentDebt := postBadDebtDebt
                        let maxRepaid := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                        {
                            let __ite_cond := lt(lltv, 1000000000000000000)
                            if __ite_cond {
                                maxRepaid := div(add(mul(sub(currentDebt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                            }
                            if iszero(__ite_cond) {
                                maxRepaid := maxRepaid
                            }
                        }
                        let repaidUnits := maxRepaid
                        let repayValue := div(mul(repaidUnits, lif), 1000000000000000000)
                        let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), liquidatedCollatPrice)
                        let newCollateral := sub(collateral, seizedAssets)
                        let postBitmap := collateralBitmap
                        {
                            let __ite_cond := and(iszero(iszero(eq(newCollateral, 0))), iszero(iszero(gt(seizedAssets, 0))))
                            if __ite_cond {
                                postBitmap := and(collateralBitmap, not(shl(collateralIndex, 1)))
                            }
                            if iszero(__ite_cond) {
                                postBitmap := postBitmap
                            }
                        }
                        let postWithdrawable := add(withdrawable, repaidUnits)
                        let finalDebt := sub(currentDebt, repaidUnits)
                        let payer := msgSender
                        {
                            let __ite_cond := iszero(eq(callback, 0))
                            if __ite_cond {
                                payer := callback
                            }
                            if iszero(__ite_cond) {
                                payer := payer
                            }
                        }
                        let callbackAccepted := or(iszero(iszero(eq(callback, 0))), iszero(iszero(eq(callbackReturn, 57683088179238363159977504707935902064464440500167392520350201799917296135842))))
                        mstore(0, postBadDebtDebt)
                        mstore(32, newTotalUnits)
                        mstore(64, newLossFactor)
                        mstore(96, newContinuousFeeCredit)
                        mstore(128, currentDebt)
                        mstore(160, maxRepaid)
                        mstore(192, seizedAssets)
                        mstore(224, newCollateral)
                        mstore(256, postBitmap)
                        mstore(288, postWithdrawable)
                        mstore(320, finalDebt)
                        mstore(352, payer)
                        mstore(384, callbackAccepted)
                        mstore(416, seizedAssets)
                        mstore(448, repaidUnits)
                        return(0, 480)
                    }
                    case 0xb8885584 {
                        /* badDebtCollateralRepayable() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let collateral := calldataload(4)
                        let price := calldataload(36)
                        let maxLif := calldataload(68)
                        let quoted := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                        mstore(0, div(add(mul(quoted, 1000000000000000000), sub(maxLif, 1)), maxLif))
                        return(0, 32)
                    }
                    case 0x0c4a386e {
                        /* collateralLoopStep() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 324) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 324) {
                            revert(0, 0)
                        }
                        let maxDebt := calldataload(4)
                        let liquidatedCollatPrice := calldataload(36)
                        let badDebt := calldataload(68)
                        let collateralBitmap := calldataload(100)
                        let slotIndex := calldataload(132)
                        let selectedIndex := calldataload(164)
                        let collateral := calldataload(196)
                        let price := calldataload(228)
                        let lltv := calldataload(260)
                        let maxLif := calldataload(292)
                        let collateralQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
                        let nextMaxDebt := add(maxDebt, div(mul(collateralQuote, lltv), 1000000000000000000))
                        let nextLiquidatedCollatPrice := liquidatedCollatPrice
                        {
                            let __ite_cond := eq(slotIndex, selectedIndex)
                            if __ite_cond {
                                nextLiquidatedCollatPrice := price
                            }
                            if iszero(__ite_cond) {
                                nextLiquidatedCollatPrice := nextLiquidatedCollatPrice
                            }
                        }
                        let repayableQuote := div(add(mul(collateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000)
                        let repayable := div(add(mul(repayableQuote, 1000000000000000000), sub(maxLif, 1)), maxLif)
                        let nextBadDebt := 0
                        {
                            let __ite_cond := gt(badDebt, repayable)
                            if __ite_cond {
                                nextBadDebt := sub(badDebt, repayable)
                            }
                            if iszero(__ite_cond) {
                                nextBadDebt := nextBadDebt
                            }
                        }
                        let nextBitmap := and(collateralBitmap, not(shl(slotIndex, 1)))
                        mstore(0, nextMaxDebt)
                        mstore(32, nextLiquidatedCollatPrice)
                        mstore(64, nextBadDebt)
                        mstore(96, nextBitmap)
                        return(0, 128)
                    }
                    case 0xdd587720 {
                        /* rcfAllows() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let maxRepaid := calldataload(36)
                        let collateral := calldataload(68)
                        let liquidatedCollatPrice := calldataload(100)
                        let lif := calldataload(132)
                        let rcfThreshold := calldataload(164)
                        let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                        mstore(0, or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(sub(collateralRepayCapacity, maxRepaid), rcfThreshold)))))
                        return(0, 32)
                    }
                    case 0x7c09f5c4 {
                        /* rcfAllowsZeroFloor() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        let repaidUnits := calldataload(4)
                        let maxRepaid := calldataload(36)
                        let collateral := calldataload(68)
                        let liquidatedCollatPrice := calldataload(100)
                        let lif := calldataload(132)
                        let rcfThreshold := calldataload(164)
                        let collateralRepayCapacity := div(mul(div(mul(collateral, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                        let capacityShortfall := 0
                        {
                            let __ite_cond := gt(collateralRepayCapacity, maxRepaid)
                            if __ite_cond {
                                capacityShortfall := sub(collateralRepayCapacity, maxRepaid)
                            }
                            if iszero(__ite_cond) {
                                capacityShortfall := capacityShortfall
                            }
                        }
                        mstore(0, or(iszero(iszero(iszero(gt(repaidUnits, maxRepaid)))), iszero(iszero(lt(capacityShortfall, rcfThreshold)))))
                        return(0, 32)
                    }
                    case 0x71164764 {
                        /* normalModeMaxRepaidHealthyWithin3() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        let debt := calldataload(4)
                        let otherMaxDebt := calldataload(36)
                        let collateral := calldataload(68)
                        let price := calldataload(100)
                        let lif := calldataload(132)
                        let lltv := calldataload(164)
                        let selectedQuote := div(mul(collateral, price), 1000000000000000000000000000000000000)
                        let selectedMaxDebt := div(mul(selectedQuote, lltv), 1000000000000000000)
                        let maxDebt := add(otherMaxDebt, selectedMaxDebt)
                        let repaid := div(add(mul(sub(debt, maxDebt), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                        let repayValue := div(mul(repaid, lif), 1000000000000000000)
                        let seizedAssets := div(mul(repayValue, 1000000000000000000000000000000000000), price)
                        let postDebt := sub(debt, repaid)
                        let postCollateral := sub(collateral, seizedAssets)
                        let postQuote := div(mul(postCollateral, price), 1000000000000000000000000000000000000)
                        let postSelectedMaxDebt := div(mul(postQuote, lltv), 1000000000000000000)
                        let postMaxDebt := add(otherMaxDebt, postSelectedMaxDebt)
                        mstore(0, iszero(gt(postDebt, add(postMaxDebt, 3))))
                        return(0, 32)
                    }
                    case 0xd1e28ef1 {
                        /* lossFactorAfterBadDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let totalUnits := calldataload(4)
                        let badDebt := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        mstore(0, sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits)))
                        return(0, 32)
                    }
                    case 0x7af68bd6 {
                        /* totalUnitsAfterBadDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let totalUnits := calldataload(4)
                        let badDebt := calldataload(36)
                        mstore(0, sub(totalUnits, badDebt))
                        return(0, 32)
                    }
                    case 0xe6d83825 {
                        /* continuousFeeCreditAfterBadDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let continuousFeeCredit := calldataload(4)
                        let oldLossFactor := calldataload(36)
                        let newLossFactor := calldataload(68)
                        {
                            let __ite_cond := lt(oldLossFactor, 340282366920938463463374607431768211455)
                            if __ite_cond {
                                mstore(0, div(mul(continuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor)))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 0)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x1d1c3454 {
                        /* badDebtCoversTwoPostSlashCredits() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let totalUnits := calldataload(4)
                        let badDebt := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        let credit0 := calldataload(100)
                        let credit1 := calldataload(132)
                        let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                        let newTotalUnits := sub(totalUnits, badDebt)
                        let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        mstore(0, iszero(gt(add(postSlashCredit0, postSlashCredit1), newTotalUnits)))
                        return(0, 32)
                    }
                    case 0xc36a988f {
                        /* badDebtCoversTwoPostUpdateCredits() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        let totalUnits := calldataload(4)
                        let badDebt := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        let credit0 := calldataload(100)
                        let accruedFee0 := calldataload(132)
                        let credit1 := calldataload(164)
                        let accruedFee1 := calldataload(196)
                        let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                        let newTotalUnits := sub(totalUnits, badDebt)
                        let postSlashCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        let postSlashCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        let postUpdateCredit0 := sub(postSlashCredit0, accruedFee0)
                        let postUpdateCredit1 := sub(postSlashCredit1, accruedFee1)
                        mstore(0, iszero(gt(add(postUpdateCredit0, postUpdateCredit1), newTotalUnits)))
                        return(0, 32)
                    }
                    case 0xe454cd7e {
                        /* badDebtCoversTwoStoredCreditsAfterUpdates() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let totalUnits := calldataload(4)
                        let badDebt := calldataload(36)
                        let oldLossFactor := calldataload(68)
                        let credit0 := calldataload(100)
                        let credit1 := calldataload(132)
                        let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnits, badDebt)), totalUnits))
                        let newTotalUnits := sub(totalUnits, badDebt)
                        let storedCredit0 := div(mul(credit0, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        let storedCredit1 := div(mul(credit1, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                        mstore(0, iszero(gt(add(storedCredit0, storedCredit1), newTotalUnits)))
                        return(0, 32)
                    }
                    case 0x56b76060 {
                        /* postSlashCredit() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let credit := calldataload(4)
                        let marketLossFactor := calldataload(36)
                        let lastLossFactor := calldataload(68)
                        {
                            let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                            if __ite_cond {
                                mstore(0, div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor)))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 0)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x89c6117c {
                        /* postSlashPendingFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let pendingFee := calldataload(4)
                        let credit := calldataload(36)
                        let postSlashCredit := calldataload(68)
                        {
                            let __ite_cond := lt(0, credit)
                            if __ite_cond {
                                mstore(0, sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit)))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 0)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x38fa740e {
                        /* accruedFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let postSlashPendingFee := calldataload(4)
                        let lastAccrual := calldataload(36)
                        let accrualEnd := calldataload(68)
                        let maturity := calldataload(100)
                        {
                            let __ite_cond := lt(lastAccrual, maturity)
                            if __ite_cond {
                                mstore(0, div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual)))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, 0)
                                return(0, 32)
                            }
                        }
                    }
                    case 0xd1fd457d {
                        /* postUpdateCreditAfterFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let postSlashCredit := calldataload(4)
                        let accruedFee := calldataload(36)
                        mstore(0, sub(postSlashCredit, accruedFee))
                        return(0, 32)
                    }
                    case 0x22b533f7 {
                        /* updatePositionViewSequence() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        let credit := calldataload(4)
                        let lastLossFactor := calldataload(36)
                        let marketLossFactor := calldataload(68)
                        let pendingFee := calldataload(100)
                        let lastAccrual := calldataload(132)
                        let blockTimestamp := calldataload(164)
                        let maturity := calldataload(196)
                        let postSlashCredit := 0
                        {
                            let __ite_cond := lt(lastLossFactor, 340282366920938463463374607431768211455)
                            if __ite_cond {
                                postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                            }
                            if iszero(__ite_cond) {
                                postSlashCredit := postSlashCredit
                            }
                        }
                        let postSlashPendingFee := 0
                        {
                            let __ite_cond := gt(credit, 0)
                            if __ite_cond {
                                postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                            }
                            if iszero(__ite_cond) {
                                postSlashPendingFee := postSlashPendingFee
                            }
                        }
                        let accrualEnd := maturity
                        {
                            let __ite_cond := iszero(gt(blockTimestamp, maturity))
                            if __ite_cond {
                                accrualEnd := blockTimestamp
                            }
                            if iszero(__ite_cond) {
                                accrualEnd := accrualEnd
                            }
                        }
                        let fee := 0
                        {
                            let __ite_cond := lt(lastAccrual, maturity)
                            if __ite_cond {
                                fee := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(maturity, lastAccrual))
                            }
                            if iszero(__ite_cond) {
                                fee := fee
                            }
                        }
                        mstore(0, sub(postSlashCredit, fee))
                        mstore(32, sub(postSlashPendingFee, fee))
                        mstore(64, fee)
                        return(0, 96)
                    }
                    default {
                        revert(0, 0)
                    }
                }
            }
        }
    }
}