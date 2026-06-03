#!/usr/bin/env python3
"""Patch generated Midnight Yul with code-backed Market storage.

The current Verity contract macro cannot express the dynamic Market tuple
encoding needed by IdLib/SSTORE2. This postprocess keeps that boundary explicit:
it patches only the generated full Midnight artifact so `toId`, `touchMarket`,
and `toMarket` use the same CREATE2/code-storage scheme as Solidity.
"""

from __future__ import annotations

import argparse
import re
import textwrap
from pathlib import Path


HELPERS = textwrap.dedent(r"""
function __midnight_market_abi_size(market_data_offset) -> size {
            let collateral_rel_offset := calldataload(add(market_data_offset, 32))
            let collateral_data_offset := add(market_data_offset, collateral_rel_offset)
            let collateral_len := calldataload(collateral_data_offset)
            size := add(collateral_rel_offset, add(32, mul(collateral_len, 128)))
        }
        function __midnight_market_initcode(ptr, market_data_offset) -> init_len {
            let collateral_rel_offset := calldataload(add(market_data_offset, 32))
            let collateral_data_offset := add(market_data_offset, collateral_rel_offset)
            let collateral_len := calldataload(collateral_data_offset)
            let market_size := add(collateral_rel_offset, add(32, mul(collateral_len, 128)))
            mstore8(ptr, 0x60)
            mstore8(add(ptr, 1), 0x0b)
            mstore8(add(ptr, 2), 0x38)
            mstore8(add(ptr, 3), 0x03)
            mstore8(add(ptr, 4), 0x80)
            mstore8(add(ptr, 5), 0x60)
            mstore8(add(ptr, 6), 0x0b)
            mstore8(add(ptr, 7), 0x5f)
            mstore8(add(ptr, 8), 0x39)
            mstore8(add(ptr, 9), 0x5f)
            mstore8(add(ptr, 10), 0xf3)
            mstore(add(ptr, 11), 32)
            calldatacopy(add(ptr, 43), market_data_offset, market_size)
            init_len := add(43, market_size)
            mstore(64, and(add(add(ptr, init_len), 31), not(31)))
        }
        function __midnight_market_id(market_data_offset) -> id {
            let ptr := mload(64)
            let init_len := __midnight_market_initcode(ptr, market_data_offset)
            let init_hash := keccak256(ptr, init_len)
            let scratch := and(add(add(ptr, init_len), 31), not(31))
            if lt(scratch, 128) {
                scratch := 128
            }
            mstore(scratch, shl(248, 0xff))
            mstore(add(scratch, 1), shl(96, address()))
            mstore(add(scratch, 21), sload(0))
            mstore(add(scratch, 53), init_hash)
            id := keccak256(scratch, 85)
            mstore(64, and(add(add(scratch, 85), 31), not(31)))
        }
        function __midnight_store_market_in_code(market_data_offset) -> deployed {
            let ptr := mload(64)
            let init_len := __midnight_market_initcode(ptr, market_data_offset)
            deployed := create2(0, ptr, init_len, sload(0))
            if iszero(deployed) {
                let id := __midnight_market_id(market_data_offset)
                deployed := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
                if iszero(gt(extcodesize(deployed), 0)) {
                    revert(0, 0)
                }
            }
        }
        function __midnight_return_market(id) {
            let create2Address := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
            let size := extcodesize(create2Address)
            if iszero(gt(size, 0)) {
                revert(0, 0)
            }
            extcodecopy(create2Address, 0, 0, size)
            return(0, size)
        }
        function __midnight_scaffold_id(id) -> scaffold_id {
            scaffold_id := id
            let create2Address := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
            if gt(extcodesize(create2Address), 127) {
                extcodecopy(create2Address, 0, 96, 32)
                scaffold_id := mload(0)
            }
        }
        function __midnight_call_repay_callback(callback, market_data_offset, units, onBehalf, data_abs_offset, data_length) {
            let callback_id := __midnight_market_id(market_data_offset)
            let market_size := __midnight_market_abi_size(market_data_offset)
            let data_padded := and(add(data_length, 31), not(31))
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0xfc56f72e00000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), callback_id)
            mstore(add(ptr, 36), 160)
            mstore(add(ptr, 68), units)
            mstore(add(ptr, 100), and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 132), add(160, market_size))
            calldatacopy(add(ptr, 164), market_data_offset, market_size)
            calldatacopy(add(add(ptr, 164), market_size), data_abs_offset, add(32, data_padded))
            let call_len := add(add(196, market_size), data_padded)
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := call(gas(), callback, 0, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0x40a13da200000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_call_buy_callback(callback, market_data_offset, id, buyerAssets, units, pendingFeeIncrease, buyer, data_abs_offset, data_length) {
            let market_size := __midnight_market_abi_size(market_data_offset)
            let data_padded := and(add(data_length, 31), not(31))
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0xf151bd5c00000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), id)
            mstore(add(ptr, 36), 224)
            mstore(add(ptr, 68), buyerAssets)
            mstore(add(ptr, 100), units)
            mstore(add(ptr, 132), pendingFeeIncrease)
            mstore(add(ptr, 164), and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 196), add(224, market_size))
            calldatacopy(add(ptr, 228), market_data_offset, market_size)
            calldatacopy(add(add(ptr, 228), market_size), data_abs_offset, add(32, data_padded))
            let call_len := add(add(260, market_size), data_padded)
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := call(gas(), callback, 0, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0xa8f3eb4400000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_call_sell_callback(callback, market_data_offset, id, sellerAssets, units, pendingFeeDecrease, seller, receiver, data_abs_offset, data_length) {
            let market_size := __midnight_market_abi_size(market_data_offset)
            let data_padded := and(add(data_length, 31), not(31))
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0x7f44a13a00000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), id)
            mstore(add(ptr, 36), 256)
            mstore(add(ptr, 68), sellerAssets)
            mstore(add(ptr, 100), units)
            mstore(add(ptr, 132), pendingFeeDecrease)
            mstore(add(ptr, 164), and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 196), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 228), add(256, market_size))
            calldatacopy(add(ptr, 260), market_data_offset, market_size)
            calldatacopy(add(add(ptr, 260), market_size), data_abs_offset, add(32, data_padded))
            let call_len := add(add(292, market_size), data_padded)
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := call(gas(), callback, 0, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0xa4fb788300000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_take_set_lock(frame) {
            mstore(add(frame, 576), mload(add(frame, 32)))
            mstore(add(frame, 608), and(mload(add(frame, 352)), 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(frame, 640), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
            let lock_slot := keccak256(add(frame, 576), 96)
            mstore(add(frame, 512), lock_slot)
            mstore(add(frame, 544), tload(lock_slot))
            tstore(lock_slot, 1)
        }
        function __midnight_take_clear_lock(frame) {
            if iszero(mload(add(frame, 544))) {
                tstore(mload(add(frame, 512)), 0)
            }
        }
        function __midnight_take_locked_or_healthy(frame) -> ok {
            ok := or(
                tload(mload(add(frame, 512))),
                internal_internal_isHealthy(mload(frame), mload(add(frame, 32)), mload(add(frame, 352)))
            )
        }
        function __midnight_take_call_buy_callback(frame) {
            let callback := mload(add(frame, 256))
            if iszero(eq(callback, 0)) {
                __midnight_call_buy_callback(
                    callback,
                    mload(frame),
                    mload(add(frame, 704)),
                    mload(add(frame, 64)),
                    mload(add(frame, 96)),
                    mload(add(frame, 128)),
                    mload(add(frame, 160)),
                    mload(add(frame, 192)),
                    mload(add(frame, 224))
                )
            }
        }
        function __midnight_take_call_sell_callback(frame) {
            let callback := mload(add(frame, 480))
            if iszero(eq(callback, 0)) {
                __midnight_call_sell_callback(
                    callback,
                    mload(frame),
                    mload(add(frame, 704)),
                    mload(add(frame, 288)),
                    mload(add(frame, 96)),
                    mload(add(frame, 320)),
                    mload(add(frame, 352)),
                    mload(add(frame, 384)),
                    mload(add(frame, 416)),
                    mload(add(frame, 448))
                )
            }
        }
        function __midnight_call_liquidate_callback(callback, caller_value, market_data_offset, id, collateralIndex, seizedAssets, repaidUnits, borrower, receiver, data_abs_offset, data_length, badDebt) {
            let market_size := __midnight_market_abi_size(market_data_offset)
            let data_padded := and(add(data_length, 31), not(31))
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0x6861b79500000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), and(caller_value, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 36), id)
            mstore(add(ptr, 68), 320)
            mstore(add(ptr, 100), collateralIndex)
            mstore(add(ptr, 132), seizedAssets)
            mstore(add(ptr, 164), repaidUnits)
            mstore(add(ptr, 196), and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 228), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 260), add(320, market_size))
            mstore(add(ptr, 292), badDebt)
            calldatacopy(add(ptr, 324), market_data_offset, market_size)
            calldatacopy(add(add(ptr, 324), market_size), data_abs_offset, add(32, data_padded))
            let call_len := add(add(356, market_size), data_padded)
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := call(gas(), callback, 0, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0x70b53d4b00000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_call_ratifier(ratifier, offer_data_offset, ratifier_data_offset) {
            let offer_len := sub(ratifier_data_offset, offer_data_offset)
            let ratifier_data_len := calldataload(ratifier_data_offset)
            let ratifier_data_padded := and(add(ratifier_data_len, 31), not(31))
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0x675ef8d300000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), 64)
            mstore(add(ptr, 36), add(64, offer_len))
            calldatacopy(add(ptr, 68), offer_data_offset, offer_len)
            calldatacopy(add(add(ptr, 68), offer_len), ratifier_data_offset, add(32, ratifier_data_padded))
            let call_len := add(add(100, offer_len), ratifier_data_padded)
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := staticcall(gas(), ratifier, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0x9e8ec67600000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_call_flashloan_callback(callback, caller_value, tokens_abs_offset, tokens_length, assets_abs_offset, assets_length, data_abs_offset, data_length) {
            let tokens_tail_size := add(32, mul(tokens_length, 32))
            let assets_tail_size := add(32, mul(assets_length, 32))
            let data_padded := and(add(data_length, 31), not(31))
            let data_tail_size := add(32, data_padded)
            let ptr := mload(64)
            if lt(ptr, 128) {
                ptr := 128
            }
            mstore(ptr, 0xd1f260c300000000000000000000000000000000000000000000000000000000)
            mstore(add(ptr, 4), and(caller_value, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 36), 128)
            mstore(add(ptr, 68), add(128, tokens_tail_size))
            mstore(add(ptr, 100), add(add(128, tokens_tail_size), assets_tail_size))
            calldatacopy(add(ptr, 132), tokens_abs_offset, tokens_tail_size)
            calldatacopy(add(add(ptr, 132), tokens_tail_size), assets_abs_offset, assets_tail_size)
            calldatacopy(add(add(add(ptr, 132), tokens_tail_size), assets_tail_size), data_abs_offset, data_tail_size)
            let call_len := add(132, add(add(tokens_tail_size, assets_tail_size), data_tail_size))
            mstore(64, and(add(add(ptr, call_len), 31), not(31)))
            let success := call(gas(), callback, 0, ptr, call_len, ptr, 32)
            if iszero(success) {
                let rds := returndatasize()
                returndatacopy(0, 0, rds)
                revert(0, rds)
            }
            if lt(returndatasize(), 32) {
                revert(0, 0)
            }
            returndatacopy(ptr, 0, 32)
            if iszero(eq(mload(ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                mstore(0, 0xa429c87000000000000000000000000000000000000000000000000000000000)
                revert(0, 4)
            }
        }
        function __midnight_emit_update_position(id, user, credit_before, credit_after, pending_before, pending_after, accrued) {
            let ptr := mload(64)
            mstore(ptr, sub(credit_before, credit_after))
            mstore(add(ptr, 32), sub(pending_before, pending_after))
            mstore(add(ptr, 64), accrued)
            log3(ptr, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
        }
        function __midnight_emit_take(caller_value, id, units, taker, maker, offer_is_buy, group, consumed, buyer_pending_fee_increase, seller_pending_fee_decrease, buyer_credit_increase, seller_credit_decrease, receiver, payer) {
            let ptr := mload(64)
            mstore(ptr, and(caller_value, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 32), units)
            mstore(add(ptr, 64), offer_is_buy)
            mstore(add(ptr, 96), group)
            mstore(add(ptr, 128), units)
            mstore(add(ptr, 160), units)
            mstore(add(ptr, 192), consumed)
            mstore(add(ptr, 224), buyer_pending_fee_increase)
            mstore(add(ptr, 256), seller_pending_fee_decrease)
            mstore(add(ptr, 288), buyer_credit_increase)
            mstore(add(ptr, 320), seller_credit_decrease)
            mstore(add(ptr, 352), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            mstore(add(ptr, 384), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
            log4(ptr, 416, 0x9e0c6d3ffe2895519e5543fe8da6e54858f4c06530d7557d808068b0ecdc9bc3, id, and(taker, 0xffffffffffffffffffffffffffffffffffffffff), and(maker, 0xffffffffffffffffffffffffffffffffffffffff))
        }
        function __midnight_mul_div_up(x, y, d) -> z {
            z := div(add(mul(x, y), sub(d, 1)), d)
        }
        function __midnight_div_half_down(x, d) -> z {
            z := div(add(x, div(sub(d, 1), 2)), d)
        }
        function __midnight_wexp_abs(x) -> z {
            let ln2 := 693147180559945309
            let offset := 322611214989459870
            let q := sdiv(add(x, offset), ln2)
            let r := sub(x, mul(q, ln2))
            let secondTerm := sdiv(mul(r, r), 2000000000000000000)
            let thirdTerm := sdiv(mul(secondTerm, r), 3000000000000000000)
            let expR := add(add(add(1000000000000000000, r), secondTerm), thirdTerm)
            z := shl(q, expR)
        }
        function __midnight_wexp(x) -> z {
            if slt(x, 0) {
                z := div(1000000000000000000000000000000000000, __midnight_wexp_abs(sub(0, x)))
            }
            if iszero(slt(x, 0)) {
                z := __midnight_wexp_abs(x)
            }
        }
        function __midnight_tick_to_price(tick) -> price {
            if gt(tick, 5820) {
                revert(0, 0)
            }
            let exponent := mul(4987541511039073, sub(2910, tick))
            let raw := __midnight_div_half_down(1000000000000000000000000000000000000, add(1000000000000000000, __midnight_wexp(exponent)))
            price := mul(__midnight_div_half_down(raw, 1000000000000), 1000000000000)
        }
""")

INLINE_VALIDATE_COLLATERAL_PARAMS = textwrap.dedent(r"""
{
    let collateralParams_rel_offset := calldataload(add(market_data_offset, 32))
    let collateralParams_data_offset := add(market_data_offset, collateralParams_rel_offset)
    let collateralParams_length := calldataload(collateralParams_data_offset)
    let collateralCount := collateralParams_length
    if iszero(gt(collateralCount, 0)) {
        {
            let __err_ptr := mload(64)
            mstore(add(__err_ptr, 0), 0x4e6f436f6c6c61746572616c506172616d732829000000000000000000000000)
            let __err_hash := keccak256(__err_ptr, 20)
            let __err_selector := shl(224, shr(224, __err_hash))
            mstore(0, __err_selector)
            let __err_tail := 0
            revert(0, add(4, __err_tail))
        }
    }
    if gt(collateralCount, 128) {
        {
            let __err_ptr := mload(64)
            mstore(add(__err_ptr, 0), 0x546f6f4d616e79436f6c6c61746572616c506172616d73282900000000000000)
            let __err_hash := keccak256(__err_ptr, 25)
            let __err_selector := shl(224, shr(224, __err_hash))
            mstore(0, __err_selector)
            let __err_tail := 0
            revert(0, add(4, __err_tail))
        }
    }
    let previousCollateralToken := 0
    for {
        let __forEach_idx := 0
        let __forEach_count := collateralCount
        let i := 0
    } lt(__forEach_idx, __forEach_count) {
        __forEach_idx := add(__forEach_idx, 1)
    } {
        i := __forEach_idx
        let collateralElement_offset := add(add(collateralParams_data_offset, 32), mul(i, 128))
        let collateralToken := calldataload(collateralElement_offset)
        if iszero(gt(collateralToken, previousCollateralToken)) {
            {
                let __err_ptr := mload(64)
                mstore(add(__err_ptr, 0), 0x436f6c6c61746572616c506172616d734e6f74536f7274656428290000000000)
                let __err_hash := keccak256(__err_ptr, 27)
                let __err_selector := shl(224, shr(224, __err_hash))
                mstore(0, __err_selector)
                let __err_tail := 0
                revert(0, add(4, __err_tail))
            }
        }
        let lltv := calldataload(add(collateralElement_offset, 32))
        let allowed := or(or(or(eq(lltv, 385000000000000000), eq(lltv, 625000000000000000)), or(eq(lltv, 770000000000000000), eq(lltv, 860000000000000000))), or(or(eq(lltv, 915000000000000000), eq(lltv, 945000000000000000)), or(or(eq(lltv, 965000000000000000), eq(lltv, 980000000000000000)), eq(lltv, 1000000000000000000))))
        if iszero(allowed) {
            {
                let __err_ptr := mload(64)
                mstore(add(__err_ptr, 0), 0x4c6c74764e6f74416c6c6f776564282900000000000000000000000000000000)
                let __err_hash := keccak256(__err_ptr, 16)
                let __err_selector := shl(224, shr(224, __err_hash))
                mstore(0, __err_selector)
                let __err_tail := 0
                revert(0, add(4, __err_tail))
            }
        }
        let lowMaxLif := div(1000000000000000000000000000000000000, sub(1000000000000000000, div(mul(250000000000000000, sub(1000000000000000000, lltv)), 1000000000000000000)))
        let highMaxLif := div(1000000000000000000000000000000000000, sub(1000000000000000000, div(mul(500000000000000000, sub(1000000000000000000, lltv)), 1000000000000000000)))
        let lif := calldataload(add(collateralElement_offset, 64))
        if iszero(or(iszero(iszero(eq(lif, lowMaxLif))), iszero(iszero(eq(lif, highMaxLif))))) {
            {
                let __err_ptr := mload(64)
                mstore(add(__err_ptr, 0), 0x496e76616c69644d61784c696628290000000000000000000000000000000000)
                let __err_hash := keccak256(__err_ptr, 15)
                let __err_selector := shl(224, shr(224, __err_hash))
                mstore(0, __err_selector)
                let __err_tail := 0
                revert(0, add(4, __err_tail))
            }
        }
        previousCollateralToken := collateralToken
    }
}
""")


def replace_once(text: str, old: str, new: str, label: str) -> str:
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"expected exactly one {label} match, found {count}")
    return text.replace(old, new, 1)


def replace_all(text: str, old: str, new: str, label: str, expected_min: int = 1) -> str:
    count = text.count(old)
    if count < expected_min:
        raise SystemExit(f"expected at least {expected_min} {label} matches, found {count}")
    return text.replace(old, new)


def patch(text: str) -> str:
    text = replace_once(
        text,
        """    object "runtime" {
        code {
            function mappingSlot(baseSlot, key) -> slot {
""",
        """    object "runtime" {
        code {
            mstore(64, memoryguard(32768))
            function mappingSlot(baseSlot, key) -> slot {
""",
        "runtime memoryguard",
    )

    def indent_block(block: str, indent: str) -> str:
        return "".join(
            (indent + line if line.strip() else line)
            for line in block.splitlines(keepends=True)
        )

    def replace_internal_unit_stop(text: str, name: str, expected_min: int) -> str:
        pattern = re.compile(
            rf"(?ms)^(?P<indent>\s*)function internal_internal_{name}"
            rf"\([^)]*\) \{{\n(?P<body>.*?)(?P=indent)    stop\(\)\n(?P=indent)\}}"
        )

        def repl(match: re.Match[str]) -> str:
            indent = match.group("indent")
            return (
                f"{indent}function internal_internal_{name}"
                + match.group(0).split(f"function internal_internal_{name}", 1)[1]
                .rsplit(f"{indent}    stop()\n{indent}}}", 1)[0]
                + f"{indent}    leave\n{indent}}}"
            )

        text, count = pattern.subn(repl, text)
        if count < expected_min:
            raise SystemExit(
                f"expected at least {expected_min} internal {name} stop matches, found {count}"
            )
        return text

    def replace_internal_is_healthy(text: str) -> str:
        pattern = re.compile(
            r"(?m)^(?P<indent>\s*)function internal_internal_isHealthy"
            r"\(market_data_offset, id, borrower\) -> __ret0 \{"
        )
        body = """function internal_internal_isHealthy(market_data_offset, id, borrower) -> __ret0 {
    let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(11, id), borrower), 2))), 340282366920938463463374607431768211455)
    if eq(debt, 0) {
        __ret0 := 1
        leave
    }
    let collateralParamsData := __verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1)
    let collateralParamsLength := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
    let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(11, id), borrower), 2))), 340282366920938463463374607431768211455)
    let maxDebt := 0
    for { let i := 0 } lt(i, collateralParamsLength) { i := add(i, 1) } {
        if gt(and(collateralBitmapValue, shl(i, 1)), 0) {
            let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
            let oracle := internal_internal_collateralOracleAt(collateralParamsData, collateralParamsLength, i)
            let price := internal_internal_oraclePrice(oracle)
            let lltv := internal_internal_collateralLltvAt(collateralParamsData, collateralParamsLength, i)
            let collateralDebt := div(mul(div(mul(activeCollateral, price), 1000000000000000000000000000000000000), lltv), 1000000000000000000)
            maxDebt := add(maxDebt, collateralDebt)
        }
    }
    __ret0 := iszero(gt(debt, maxDebt))
    leave
}
"""
        out: list[str] = []
        pos = 0
        count = 0
        while True:
            match = pattern.search(text, pos)
            if match is None:
                out.append(text[pos:])
                break
            brace = text.find("{", match.start())
            depth = 0
            end = brace
            while end < len(text):
                if text[end] == "{":
                    depth += 1
                elif text[end] == "}":
                    depth -= 1
                    if depth == 0:
                        end += 1
                        if end < len(text) and text[end] == "\n":
                            end += 1
                        break
                end += 1
            if depth != 0:
                raise SystemExit("unterminated internal isHealthy function")
            out.append(text[pos:match.start()])
            out.append(indent_block(body, match.group("indent")))
            pos = end
            count += 1
        text = "".join(out)
        if count < 2:
            raise SystemExit(f"expected at least 2 internal isHealthy matches, found {count}")
        return text

    validate_call_re = re.compile(
        r"(?m)^(?P<indent>\s*)internal_internal_validateCollateralParams"
        r"\(__verity_param_dynamic_member_data_offset_calldata_checked\(market_data_offset, 1\), "
        r"__verity_param_dynamic_member_length_calldata_checked\(market_data_offset, 1\)\)\n"
    )

    def validate_call_repl(match: re.Match[str]) -> str:
        return indent_block(INLINE_VALIDATE_COLLATERAL_PARAMS, match.group("indent"))

    text, validate_call_count = validate_call_re.subn(validate_call_repl, text)
    if validate_call_count < 3:
        raise SystemExit(f"expected at least 3 touch validation call matches, found {validate_call_count}")

    internal_toid_re = re.compile(
        r"(?m)^(?P<indent>\s*)function internal_internal_toId\(market_data_offset\) -> __ret0 \{\n"
        r"(?P=indent)    __ret0 := __verity_param_dynamic_head_word_calldata_checked\(market_data_offset, 2\)\n"
        r"(?P=indent)    leave\n"
        r"(?P=indent)\}\n"
    )

    def toid_repl(match: re.Match[str]) -> str:
        indent = match.group("indent")
        return indent_block(HELPERS, indent) + match.group(0)

    text, count = internal_toid_re.subn(toid_repl, text)
    if count < 2:
        raise SystemExit(f"expected at least 2 internal toId matches, found {count}")

    text = replace_internal_unit_stop(text, "writeCollateralAmount", expected_min=2)
    text = replace_internal_unit_stop(text, "setCollateralAmount", expected_min=2)
    text = replace_internal_is_healthy(text)

    for name, args in [
        ("creditOf", "id, user"),
        ("debtOf", "id, user"),
        ("totalUnits", "id"),
        ("lossFactor", "id"),
        ("withdrawable", "id"),
        ("continuousFee", "id"),
        ("collateral", "id, user, index"),
        ("pendingFee", "id, user"),
        ("lastAccrual", "id, user"),
        ("lastLossFactor", "id, user"),
        ("collateralBitmap", "id, user"),
    ]:
        signature = f"function internal_internal_{name}({args}) -> __ret0 {{\n"
        replacement = signature + "                id := __midnight_scaffold_id(id)\n"
        text = replace_all(text, signature, replacement, f"{name} scaffold id alias", expected_min=2)

    text = replace_all(
        text,
        "                        let id := calldataload(4)\n                        let user :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let user :=",
        "external user getter scaffold id alias",
        expected_min=6,
    )
    text = replace_all(
        text,
        "                        let id := calldataload(4)\n                        let value :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let value :=",
        "external value getter scaffold id alias",
        expected_min=6,
    )
    text = replace_all(
        text,
        "                        let id := calldataload(4)\n                        let settlementFeeCbp0 :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let settlementFeeCbp0 :=",
        "external settlement fees scaffold id alias",
        expected_min=1,
    )

    text = replace_once(
        text,
        "                        let id := calldataload(4)\n                        let totalUnits :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let totalUnits :=",
        "external marketState scaffold id alias",
    )
    text = replace_once(
        text,
        "                        let id := calldataload(4)\n                        let newTickSpacing :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let newTickSpacing :=",
        "external setMarketTickSpacing scaffold id alias",
    )
    text = replace_once(
        text,
        "                        let id := calldataload(4)\n                        let index := calldataload(36)\n                        let newSettlementFee :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let index := calldataload(36)\n                        let newSettlementFee :=",
        "external setMarketSettlementFee scaffold id alias",
    )
    text = replace_once(
        text,
        "                        let id := calldataload(4)\n                        let newContinuousFee :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let newContinuousFee :=",
        "external setMarketContinuousFee scaffold id alias",
    )
    text = replace_once(
        text,
        "                        let id := calldataload(4)\n                        let timeToMaturity :=",
        "                        let id := __midnight_scaffold_id(calldataload(4))\n                        let timeToMaturity :=",
        "external settlementFee scaffold id alias",
    )
    text = replace_once(
        text,
        "                        let id := calldataload(36)\n                        let borrower :=",
        "                        let id := __midnight_scaffold_id(calldataload(36))\n                        let borrower :=",
        "external isHealthy scaffold id alias",
    )

    external_toid = """                        mstore(0, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))
                        return(0, 32)
"""
    external_toid_new = """                        mstore(0, __midnight_market_id(market_data_offset))
                        return(0, 32)
"""
    text = replace_once(text, external_toid, external_toid_new, "external toId")

    external_tomarket = """                        stop()
                    }
                    case 0x93c52062 {
"""
    external_tomarket_new = """                        __midnight_return_market(id)
                    }
                    case 0x93c52062 {
"""
    text = replace_once(text, external_tomarket, external_tomarket_new, "external toMarket")

    touch_return = """            __ret0 := id
            leave
"""
    touch_return_new = """            if eq(currentTickSpacing, 0) {
                pop(__midnight_store_market_in_code(add(4, calldataload(4))))
            }
            __ret0 := id
            leave
"""
    text = replace_once(text, touch_return, touch_return_new, "internal touchMarket return")

    external_touch_return = """                        mstore(0, id)
                        return(0, 32)
                    }
                    case 0xa9bef809 {
"""
    external_touch_return_new = """                        if eq(currentTickSpacing, 0) {
                            pop(__midnight_store_market_in_code(add(4, calldataload(4))))
                        }
                        mstore(0, __midnight_market_id(market_data_offset))
                        return(0, 32)
                    }
                    case 0xa9bef809 {
"""
    text = replace_once(text, external_touch_return, external_touch_return_new, "external touchMarket return")

    text = replace_once(
        text,
        """                    case 0x1d8a2f16 {
""",
        """                    case 0x1b14c072 {
                        /* liquidationLocked() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        mstore(0x4800, calldataload(4))
                        mstore(0x4820, and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff))
                        mstore(0x4840, 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                        mstore(0, tload(keccak256(0x4800, 96)))
                        return(0, 32)
                    }
                    case 0x1d8a2f16 {
""",
        "external liquidationLocked view",
    )

    repay_stop = """                            }
                        }
                        stop()
                    }
                    case 0x9812a361 {
"""
    repay_stop_new = """                            }
                        }
                        if iszero(eq(callback, 0)) {
                            __midnight_call_repay_callback(callback, market_data_offset, units, onBehalf, data_abs_offset, data_length)
                        }
                        stop()
                    }
                    case 0x9812a361 {
"""
    text = replace_once(text, repay_stop, repay_stop_new, "repay callback")

    update_position_return = """                __ret0 := newCredit
                __ret1 := newPendingFee
                __ret2 := accrued
                leave
"""
    update_position_return_new = """                {
                    let __event_ptr := mload(64)
                    mstore(__event_ptr, sub(credit, newCredit))
                    mstore(add(__event_ptr, 32), sub(pendingFee, newPendingFee))
                    mstore(add(__event_ptr, 64), accrued)
                    log3(__event_ptr, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                __ret0 := newCredit
                __ret1 := newPendingFee
                __ret2 := accrued
                leave
"""
    text = replace_all(
        text,
        update_position_return,
        update_position_return_new,
        "UpdatePosition event",
        expected_min=1,
    )

    external_update_position_return = """                        mstore(0, newCredit)
                        mstore(32, newPendingFee)
                        mstore(64, accrued)
                        return(0, 96)
"""
    external_update_position_return_new = """                        {
                            let __event_ptr := mload(64)
                            mstore(__event_ptr, sub(credit, newCredit))
                            mstore(add(__event_ptr, 32), sub(pendingFee, newPendingFee))
                            mstore(add(__event_ptr, 64), accrued)
                            log3(__event_ptr, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        mstore(0, newCredit)
                        mstore(32, newPendingFee)
                        mstore(64, accrued)
                        return(0, 96)
"""
    text = replace_once(
        text,
        external_update_position_return,
        external_update_position_return_new,
        "external UpdatePosition event",
    )

    claim_continuous_transfer = """                let currentWithdrawable := and(shr(0, sload(add(mappingSlot(10, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(currentWithdrawable, amount)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(10, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(10, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __st_ptr := mload(64)
                    mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    claim_continuous_transfer_new = """                let currentWithdrawable := and(shr(0, sload(add(mappingSlot(10, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(currentWithdrawable, amount)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(10, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(10, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __event_ptr := mload(64)
                    mstore(__event_ptr, amount)
                    log4(__event_ptr, 32, 0xd64453ff5184816a23d670b5224717cd441c4a7cd5f4e7dbc30f8511d35f597b, and(sender, 0xffffffffffffffffffffffffffffffffffffffff), id, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    let __st_ptr := mload(64)
                    mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    text = replace_all(
        text,
        claim_continuous_transfer,
        claim_continuous_transfer_new,
        "ClaimContinuousFee event",
        expected_min=1,
    )

    external_claim_continuous_transfer = """                        let currentWithdrawable := and(shr(0, sload(add(mappingSlot(10, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(currentWithdrawable, amount)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(10, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(10, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __st_ptr := mload(64)
                            mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    external_claim_continuous_transfer_new = """                        let currentWithdrawable := and(shr(0, sload(add(mappingSlot(10, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(currentWithdrawable, amount)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(10, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(10, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __event_ptr := mload(64)
                            mstore(__event_ptr, amount)
                            log4(__event_ptr, 32, 0xd64453ff5184816a23d670b5224717cd441c4a7cd5f4e7dbc30f8511d35f597b, and(sender, 0xffffffffffffffffffffffffffffffffffffffff), id, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            let __st_ptr := mload(64)
                            mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    text = replace_once(
        text,
        external_claim_continuous_transfer,
        external_claim_continuous_transfer_new,
        "external ClaimContinuousFee event",
    )

    withdraw_transfer = """            let total := and(shr(0, sload(mappingSlot(10, id))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(total, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(10, id))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(10, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __st_ptr := mload(64)
                mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    withdraw_transfer_new = """            let total := and(shr(0, sload(mappingSlot(10, id))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(total, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(10, id))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(10, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __event_ptr := mload(64)
                mstore(__event_ptr, sub(creditBeforeUpdate, creditAfterUpdate))
                mstore(add(__event_ptr, 32), sub(pendingFeeBeforeUpdate, pendingFeeAfterUpdate))
                mstore(add(__event_ptr, 64), accrued)
                log3(__event_ptr, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                let __event_ptr := mload(64)
                mstore(__event_ptr, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__event_ptr, 32), units)
                mstore(add(__event_ptr, 64), pendingFeeDecrease)
                log4(__event_ptr, 96, 0x7a5e8e1731f88cf0c25f88fc7d5618e481e87be4d83f614e601850c2ff082fd7, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                let __st_ptr := mload(64)
                mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    text = replace_once(text, withdraw_transfer, withdraw_transfer_new, "Withdraw events")

    external_withdraw_transfer = """                        let total := and(shr(0, sload(mappingSlot(10, id))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(total, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(10, id))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(10, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __st_ptr := mload(64)
                            mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    external_withdraw_transfer_new = """                        let total := and(shr(0, sload(mappingSlot(10, id))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(total, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(10, id))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(10, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __event_ptr := mload(64)
                            mstore(__event_ptr, sub(creditBeforeUpdate, creditAfterUpdate))
                            mstore(add(__event_ptr, 32), sub(pendingFeeBeforeUpdate, pendingFeeAfterUpdate))
                            mstore(add(__event_ptr, 64), accrued)
                            log3(__event_ptr, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            let __event_ptr := mload(64)
                            mstore(__event_ptr, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__event_ptr, 32), units)
                            mstore(add(__event_ptr, 64), pendingFeeDecrease)
                            log4(__event_ptr, 96, 0x7a5e8e1731f88cf0c25f88fc7d5618e481e87be4d83f614e601850c2ff082fd7, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            let __st_ptr := mload(64)
                            mstore(__st_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
"""
    text = replace_once(
        text,
        external_withdraw_transfer,
        external_withdraw_transfer_new,
        "external Withdraw events",
    )

    external_take_decode_tail = """                        let takerCallbackData_data_offset := takerCallbackData_tail_head_end
                        let sender := caller()
"""
    external_take_decode_tail_new = """                        let takerCallbackData_data_offset := takerCallbackData_tail_head_end
                        mstore(0x4700, takerCallback)
                        mstore(0x4720, takerCallbackData_abs_offset)
                        mstore(0x4740, takerCallbackData_length)
                        let sender := caller()
"""
    text = replace_once(text, external_take_decode_tail, external_take_decode_tail_new, "external Take callback calldata staging")

    external_take_id = """                        let maturity := calldataload(add(marketBase, 64))
                        let id := maturity
                        let currentMarketTickSpacing := and(shr(144, sload(add(mappingSlot(10, id), 2))), 255)
"""
    external_take_id_new = """                        let maturity := calldataload(add(marketBase, 64))
                        let id := maturity
                        mstore(0x4000, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                        mstore(0x4020, units)
                        mstore(0x4040, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1))
                        mstore(0x4060, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6))
	                        mstore(0x4080, units)
	                        mstore(0x40a0, units)
	                        mstore(0x4420, and(calldataload(add(marketBase, 128)), 0xffffffffffffffffffffffffffffffffffffffff))
	                        let currentMarketTickSpacing := and(shr(144, sload(add(mappingSlot(10, id), 2))), 255)
	                        if eq(currentMarketTickSpacing, 0) {
	                            pop(__midnight_store_market_in_code(marketBase))
	                        }
	"""
    text = replace_once(text, external_take_id, external_take_id_new, "external Take event staging header")

    external_take_before_consumed = """                        let newConsumed := 0
                        {
                            let __ite_cond := gt(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0)
"""
    external_take_before_consumed_new = """                        __midnight_call_ratifier(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10), offer_data_offset, ratifierData_abs_offset)
                        let offerPrice := __midnight_tick_to_price(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5))
                        let takeTimeToMaturity := 0
                        if gt(maturity, now) {
                            takeTimeToMaturity := sub(maturity, now)
                        }
                        let settlementFeeValue := internal_internal_settlementFee(id, takeTimeToMaturity)
                        let sellerPrice := offerPrice
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            if lt(offerPrice, settlementFeeValue) {
                                revert(0, 0)
                            }
                            sellerPrice := sub(offerPrice, settlementFeeValue)
                        }
                        let buyerPrice := add(sellerPrice, settlementFeeValue)
                        let buyerAssets := __midnight_mul_div_up(units, buyerPrice, 1000000000000000000)
                        let sellerAssets := __midnight_mul_div_up(units, sellerPrice, 1000000000000000000)
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            buyerAssets := div(mul(units, buyerPrice), 1000000000000000000)
                            sellerAssets := div(mul(units, sellerPrice), 1000000000000000000)
                        }
                        mstore(0x4080, buyerAssets)
                        mstore(0x40a0, sellerAssets)
                        let newConsumed := 0
                        {
                            let __ite_cond := gt(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0)
"""
    text = replace_once(text, external_take_before_consumed, external_take_before_consumed_new, "external Take asset math")

    external_take_assets_consumed = """                                let currentConsumed := sload(mappingSlot(mappingSlot(5, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                                newConsumed := add(currentConsumed, units)
                                if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13)) {
"""
    external_take_assets_consumed_new = """                                let currentConsumed := sload(mappingSlot(mappingSlot(5, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                                let consumedAssetsIncrease := sellerAssets
                                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                                    consumedAssetsIncrease := buyerAssets
                                }
                                newConsumed := add(currentConsumed, consumedAssetsIncrease)
                                if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13)) {
"""
    text = replace_once(text, external_take_assets_consumed, external_take_assets_consumed_new, "external Take maxAssets consumed")

    external_take_consumed = """                        let buyer := taker
                        let seller := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
"""
    external_take_consumed_new = """                        mstore(0x40c0, newConsumed)
                        mstore(0x41a0, id)
                        mstore(0x41c0, marketBase)
                        let buyer := taker
                        let seller := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
"""
    text = replace_once(text, external_take_consumed, external_take_consumed_new, "external Take consumed staging")

    external_take_seller_staging = """                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            buyer := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                            seller := taker
                        }
"""
    external_take_seller_staging_new = """                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            buyer := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                            seller := taker
                        }
                        mstore(0x41e0, seller)
"""
    text = replace_once(text, external_take_seller_staging, external_take_seller_staging_new, "external Take seller staging")

    external_take_buyer_fee = """                        let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValue, timeToMaturity)), 1000000000000000000)
                        let buyerCredit := and(shr(0, sload(mappingSlot(mappingSlot(11, id), buyer))), 340282366920938463463374607431768211455)
"""
    external_take_buyer_fee_new = """                        let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValue, timeToMaturity)), 1000000000000000000)
                        mstore(0x40e0, buyerPendingFeeIncrease)
                        mstore(0x4120, buyerCreditIncrease)
                        if gt(buyerCreditIncrease, 0) {
                            mstore(0x4200, 0)
                            mstore(0x4220, 0)
                            mstore(0x4240, 0)
                            log3(0x4200, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        let buyerCredit := and(shr(0, sload(mappingSlot(mappingSlot(11, id), buyer))), 340282366920938463463374607431768211455)
"""
    text = replace_once(text, external_take_buyer_fee, external_take_buyer_fee_new, "external Take buyer fee staging")

    external_take_before_seller = """                        let sellerCredit := and(shr(0, sload(mappingSlot(mappingSlot(11, id), seller))), 340282366920938463463374607431768211455)
"""
    external_take_before_seller_new = """                        let sellerCredit := and(shr(0, sload(mappingSlot(mappingSlot(11, id), seller))), 340282366920938463463374607431768211455)
"""
    text = replace_once(text, external_take_before_seller, external_take_before_seller_new, "external Take buyer UpdatePosition event")

    external_take_seller_decrease = """                        let sellerPendingFeeDecrease := 0
                        if gt(sellerCreditAfterUpdate, 0) {
                            sellerPendingFeeDecrease := div(add(mul(sellerPendingFeeAfterUpdate, sellerCreditDecrease), sub(sellerCreditAfterUpdate, 1)), sellerCreditAfterUpdate)
                        }
                        if iszero(or(iszero(iszero(iszero(gt(now, maturity)))), iszero(iszero(eq(sellerDebtIncrease, 0))))) {
"""
    external_take_seller_decrease_new = """                        let sellerPendingFeeDecrease := 0
	                        if gt(sellerCreditAfterUpdate, 0) {
	                            sellerPendingFeeDecrease := div(add(mul(sellerPendingFeeAfterUpdate, sellerCreditDecrease), sub(sellerCreditAfterUpdate, 1)), sellerCreditAfterUpdate)
	                        }
	                        mstore(0x4100, sellerPendingFeeDecrease)
	                        mstore(0x4140, sellerCreditDecrease)
	                        if and(iszero(eq(mload(0x4420), 0)), gt(buyerCreditIncrease, 0)) {
	                            mstore(0, 0x58ac9f9e00000000000000000000000000000000000000000000000000000000)
	                            mstore(4, and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
	                            let __gate_success := staticcall(gas(), mload(0x4420), 0, 36, 0, 32)
	                            if iszero(__gate_success) {
	                                mstore(0, 0x71a4e09b00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                            if lt(returndatasize(), 32) {
	                                mstore(0, 0x71a4e09b00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                            returndatacopy(0, 0, 32)
	                            if iszero(mload(0)) {
	                                mstore(0, 0x71a4e09b00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                        }
	                        if and(iszero(eq(mload(0x4420), 0)), gt(sellerDebtIncrease, 0)) {
	                            mstore(0, 0xfe9bf95600000000000000000000000000000000000000000000000000000000)
	                            mstore(4, and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
	                            let __gate_success := staticcall(gas(), mload(0x4420), 0, 36, 0, 32)
	                            if iszero(__gate_success) {
	                                mstore(0, 0xe2d4c76c00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                            if lt(returndatasize(), 32) {
	                                mstore(0, 0xe2d4c76c00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                            returndatacopy(0, 0, 32)
	                            if iszero(mload(0)) {
	                                mstore(0, 0xe2d4c76c00000000000000000000000000000000000000000000000000000000)
	                                revert(0, 4)
	                            }
	                        }
	                        if iszero(or(iszero(iszero(iszero(gt(now, maturity)))), iszero(iszero(eq(sellerDebtIncrease, 0))))) {
	"""
    text = replace_once(text, external_take_seller_decrease, external_take_seller_decrease_new, "external Take seller fee staging")

    external_take_after_seller_update = """                        let sellerCreditDecrease := internal_internal_min(units, sellerCreditAfterUpdate)
"""
    external_take_after_seller_update_new = """                        if gt(sellerCredit, 0) {
                            mstore(0x4200, sub(sellerCredit, sellerCreditAfterUpdate))
                            mstore(0x4220, sub(sellerPendingFee, sellerPendingFeeAfterUpdate))
                            mstore(0x4240, sellerAccrued)
                            log3(0x4200, 96, 0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c, id, and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        let sellerCreditDecrease := internal_internal_min(units, sellerCreditAfterUpdate)
"""
    text = replace_once(text, external_take_after_seller_update, external_take_after_seller_update_new, "external Take seller UpdatePosition event")

    external_take_before_transfer = """                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            transferAssets := 0
                        }
                        {
                            let __stf_ptr := mload(64)
"""
    external_take_before_transfer_new = """                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            transferAssets := 0
                        }
                        {
                            let currentClaimableSettlementFee := sload(mappingSlot(9, loanToken))
                            sstore(mappingSlot(9, loanToken), add(currentClaimableSettlementFee, sub(buyerAssets, sellerAssets)))
                        }
                        mstore(0x4500, marketBase)
                        mstore(0x4520, id)
                        mstore(0x47c0, __midnight_market_id(marketBase))
                        mstore(0x4540, buyerAssets)
                        mstore(0x4560, units)
                        mstore(0x4580, buyerPendingFeeIncrease)
                        mstore(0x45a0, buyer)
                        mstore(0x45c0, mload(0x4720))
                        mstore(0x45e0, mload(0x4740))
                        mstore(0x4600, mload(0x4700))
                        mstore(0x4620, sellerAssets)
                        mstore(0x4640, sellerPendingFeeDecrease)
                        mstore(0x4660, seller)
                        mstore(0x4680, receiver)
                        mstore(0x46a0, sub(__verity_param_dynamic_member_data_offset_calldata_checked(offer_data_offset, 8), 32))
                        mstore(0x46c0, __verity_param_dynamic_member_length_calldata_checked(offer_data_offset, 8))
                        mstore(0x46e0, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7))
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            mstore(0x45c0, sub(__verity_param_dynamic_member_data_offset_calldata_checked(offer_data_offset, 8), 32))
                            mstore(0x45e0, __verity_param_dynamic_member_length_calldata_checked(offer_data_offset, 8))
                            mstore(0x4600, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7))
                            mstore(0x46a0, mload(0x4720))
                            mstore(0x46c0, mload(0x4740))
                            mstore(0x46e0, mload(0x4700))
                        }
                        if iszero(eq(mload(0x4600), 0)) {
                            payer := mload(0x4600)
                        }
                        mstore(0x4160, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                        mstore(0x4180, and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                        log4(0x4000, 416, 0x9e0c6d3ffe2895519e5543fe8da6e54858f4c06530d7557d808068b0ecdc9bc3, id, and(taker, 0xffffffffffffffffffffffffffffffffffffffff), and(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), 0xffffffffffffffffffffffffffffffffffffffff))
                        __midnight_take_set_lock(0x4500)
                        __midnight_take_call_buy_callback(0x4500)
                        if gt(buyerAssets, sellerAssets) {
                            let __stf_ptr := mload(64)
                            mstore(__stf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__stf_ptr, 4), payer)
                            mstore(add(__stf_ptr, 36), address())
                            mstore(add(__stf_ptr, 68), sub(buyerAssets, sellerAssets))
                            mstore(64, and(add(add(__stf_ptr, 100), 31), not(31)))
                            let __stf_success := call(gas(), loanToken, 0, __stf_ptr, 100, __stf_ptr, 32)
                            if iszero(__stf_success) {
                                let __stf_rds := returndatasize()
                                returndatacopy(0, 0, __stf_rds)
                                revert(0, __stf_rds)
                            }
                            let __erc20_rds := returndatasize()
                            if iszero(__erc20_rds) {
                                if iszero(gt(extcodesize(loanToken), 0)) {
                                    mstore(0, 0x5274afe700000000000000000000000000000000000000000000000000000000)
                                    mstore(4, and(loanToken, 1461501637330902918203684832716283019655932542975))
                                    revert(0, 36)
                                }
                            }
                            if __erc20_rds {
                                if iszero(eq(__erc20_rds, 32)) {
                                    mstore(0, 0x5274afe700000000000000000000000000000000000000000000000000000000)
                                    mstore(4, and(loanToken, 1461501637330902918203684832716283019655932542975))
                                    revert(0, 36)
                                }
                                if iszero(eq(mload(__stf_ptr), 1)) {
                                    mstore(0, 0x5274afe700000000000000000000000000000000000000000000000000000000)
                                    mstore(4, and(loanToken, 1461501637330902918203684832716283019655932542975))
                                    revert(0, 36)
                                }
                            }
                        }
                        {
                            let __stf_ptr := mload(64)
"""
    text = replace_once(text, external_take_before_transfer, external_take_before_transfer_new, "external Take event")

    text = replace_once(
        text,
        "                            mstore(add(__stf_ptr, 68), transferAssets)\n",
        "                            mstore(add(__stf_ptr, 68), sellerAssets)\n",
        "external Take seller transfer amount",
    )

    text = replace_once(
        text,
        """                        mstore(0, units)
                        mstore(32, units)
                        return(0, 64)
""",
        """                        mstore(0, buyerAssets)
                        mstore(32, sellerAssets)
                        return(0, 64)
""",
        "external Take return assets",
    )
    external_take_before_return = """                        mstore(0, buyerAssets)
                        mstore(32, sellerAssets)
                        return(0, 64)
"""
    external_take_before_return_new = """                        __midnight_take_call_sell_callback(0x4500)
                        __midnight_take_clear_lock(0x4500)
                        if iszero(__midnight_take_locked_or_healthy(0x4500)) {
                            mstore(0, 0x4aa9880900000000000000000000000000000000000000000000000000000000)
                            revert(0, 4)
                        }
                        mstore(0, buyerAssets)
                        mstore(32, sellerAssets)
                        return(0, 64)
"""
    text = replace_once(text, external_take_before_return, external_take_before_return_new, "external Take seller health check")

    # Callback injection for Take needs a lower-stack implementation. The broad
    # inline patch currently makes solc's strict-assembly backend fail with
    # stack-too-deep, so keep the artifact buildable and leave callback parity as
    # an explicit remaining slice.

    external_liquidate_before_payer = """                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                        }
                        let self := address()
"""
    external_liquidate_before_payer_new = """                        if iszero(eq(callback, 0)) {
                            __midnight_call_liquidate_callback(callback, sender, market_data_offset, __midnight_market_id(market_data_offset), collateralIndex, outSeizedAssets, outRepaidUnits, borrower, receiver, data_abs_offset, data_length, badDebt)
                        }
                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                        }
                        let self := address()
"""
    liquidate_callback_count = 0
    cursor = 0
    while True:
        idx = text.find(external_liquidate_before_payer, cursor)
        if idx == -1:
            break
        context = text[max(0, idx - 2500):idx]
        if (
            "let collateralToken := internal_internal_collateralTokenAt" in context
            and "mstore(add(__st_ptr, 36), outSeizedAssets)" in context
        ):
            text = (
                text[:idx]
                + external_liquidate_before_payer_new
                + text[idx + len(external_liquidate_before_payer):]
            )
            cursor = idx + len(external_liquidate_before_payer_new)
            liquidate_callback_count += 1
        else:
            cursor = idx + len(external_liquidate_before_payer)
    if liquidate_callback_count < 1:
        raise SystemExit(
            f"expected at least 1 external Liquidate callback match, found {liquidate_callback_count}"
        )

    external_liquidate_lock_check = """                        let data_data_offset := data_tail_head_end
                        let sender := caller()
                        let id := internal_internal_toId(market_data_offset)
                        let debtLoaded := and(shr(0, sload(add(mappingSlot(mappingSlot(11, id), borrower), 2))), 340282366920938463463374607431768211455)
"""
    external_liquidate_lock_check_new = """                        let data_data_offset := data_tail_head_end
                        let sender := caller()
                        let id := internal_internal_toId(market_data_offset)
                        mstore(0x4800, id)
                        mstore(0x4820, and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
                        mstore(0x4840, 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                        if tload(keccak256(0x4800, 96)) {
                            mstore(0, 0xddeb79ba00000000000000000000000000000000000000000000000000000000)
                            revert(0, 4)
                        }
                        let debtLoaded := and(shr(0, sload(add(mappingSlot(mappingSlot(11, id), borrower), 2))), 340282366920938463463374607431768211455)
"""
    text = replace_once(text, external_liquidate_lock_check, external_liquidate_lock_check_new, "external Liquidate lock check")

    liquidate_event_before_self = """                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                        }
                        let self := address()
"""
    liquidate_event_before_self_new = """                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                        }
                        {
                            let __liq_ptr := mload(64)
                            mstore(__liq_ptr, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__liq_ptr, 32), outSeizedAssets)
                            mstore(add(__liq_ptr, 64), outRepaidUnits)
                            mstore(add(__liq_ptr, 96), postMaturityMode)
                            mstore(add(__liq_ptr, 128), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__liq_ptr, 160), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__liq_ptr, 192), badDebt)
                            mstore(add(__liq_ptr, 224), and(shr(128, sload(mappingSlot(10, id))), 340282366920938463463374607431768211455))
                            mstore(add(__liq_ptr, 256), and(shr(128, sload(add(mappingSlot(10, id), 1))), 340282366920938463463374607431768211455))
                            log4(__liq_ptr, 288, 0xb137b989b9fd54b984273db8f16364f52f383aaca56076a320c1896e9fc2dad9, id, and(collateralToken, 0xffffffffffffffffffffffffffffffffffffffff), and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        let self := address()
"""
    liquidate_event_count = 0
    cursor = 0
    while True:
        idx = text.find(liquidate_event_before_self, cursor)
        if idx == -1:
            break
        context = text[max(0, idx - 2800):idx]
        if (
            "let collateralToken := internal_internal_collateralTokenAt" in context
            and "mstore(add(__st_ptr, 36), outSeizedAssets)" in context
        ):
            text = (
                text[:idx]
                + liquidate_event_before_self_new
                + text[idx + len(liquidate_event_before_self):]
            )
            cursor = idx + len(liquidate_event_before_self_new)
            liquidate_event_count += 1
        else:
            cursor = idx + len(liquidate_event_before_self)
    if liquidate_event_count < 1:
        raise SystemExit(f"expected at least 1 Liquidate event match, found {liquidate_event_count}")

    liquidate_collateral_index_revert = """                        if iszero(lt(collateralIndex, collateralCount)) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 30)
                            mstore(68, 0x636f6c6c61746572616c20696e646578206f7574206f6620626f756e64730000)
                            revert(0, 100)
                        }
"""
    liquidate_collateral_index_revert_new = """                        if iszero(lt(collateralIndex, collateralCount)) {
                            mstore(0, 0x4e487b7100000000000000000000000000000000000000000000000000000000)
                            mstore(4, 0x32)
                            revert(0, 36)
                        }
"""
    text = replace_all(
        text,
        liquidate_collateral_index_revert,
        liquidate_collateral_index_revert_new,
        "Liquidate collateral index OOB panic",
        expected_min=1,
    )

    liquidate_collateral_underflow = """                            let oldCollateral := internal_internal_collateralAmount(id, borrower, collateralIndex)
                            let newCollateral := sub(oldCollateral, outSeizedAssets)
"""
    liquidate_collateral_underflow_new = """                            let oldCollateral := internal_internal_collateralAmount(id, borrower, collateralIndex)
                            if gt(outSeizedAssets, oldCollateral) {
                                mstore(0, 0x4e487b7100000000000000000000000000000000000000000000000000000000)
                                mstore(4, 0x11)
                                revert(0, 36)
                            }
                            let newCollateral := sub(oldCollateral, outSeizedAssets)
"""
    text = replace_all(
        text,
        liquidate_collateral_underflow,
        liquidate_collateral_underflow_new,
        "Liquidate collateral underflow panic",
        expected_min=1,
    )

    liquidate_debt_underflow = """                            {
                                let __compat_value := sub(debt, outRepaidUnits)
"""
    liquidate_debt_underflow_new = """                            {
                                if gt(outRepaidUnits, debt) {
                                    mstore(0, 0x4e487b7100000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 0x11)
                                    revert(0, 36)
                                }
                                let __compat_value := sub(debt, outRepaidUnits)
"""
    text = replace_all(
        text,
        liquidate_debt_underflow,
        liquidate_debt_underflow_new,
        "Liquidate debt underflow panic",
        expected_min=1,
    )

    external_flashloan_callback = """                        }
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := tokenCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
"""
    external_flashloan_callback_new = """                        }
                        __midnight_call_flashloan_callback(callback, caller(), tokens_abs_offset, tokens_length, assets_abs_offset, assets_length, data_abs_offset, data_length)
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := tokenCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
"""
    flashloan_matches = text.count(external_flashloan_callback)
    if flashloan_matches < 1:
        raise SystemExit("expected flashLoan callback insertion match")
    text = text.replace(external_flashloan_callback, external_flashloan_callback_new, 1)

    dispatcher_default = """                    default {
                        revert(0, 0)
                    }
"""
    multicall_case = """                    case 0xac9650d8 {
                        /* multicall() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let calls_offset := calldataload(4)
                        if lt(calls_offset, 32) {
                            revert(0, 0)
                        }
                        let calls_abs_offset := add(4, calls_offset)
                        if gt(calls_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let calls_length := calldataload(calls_abs_offset)
                        let calls_heads := add(calls_abs_offset, 32)
                        if gt(calls_length, div(sub(calldatasize(), calls_heads), 32)) {
                            revert(0, 0)
                        }
                        for {
                            let i := 0
                        } lt(i, calls_length) {
                            i := add(i, 1)
                        } {
                            let call_rel_offset := calldataload(add(calls_heads, mul(i, 32)))
                            let call_abs_offset := add(calls_heads, call_rel_offset)
                            if gt(call_abs_offset, sub(calldatasize(), 32)) {
                                revert(0, 0)
                            }
                            let call_length := calldataload(call_abs_offset)
                            let call_data_offset := add(call_abs_offset, 32)
                            if gt(call_length, sub(calldatasize(), call_data_offset)) {
                                revert(0, 0)
                            }
                            let ptr := mload(64)
                            if lt(ptr, 128) {
                                ptr := 128
                            }
                            calldatacopy(ptr, call_data_offset, call_length)
                            mstore(64, and(add(add(ptr, call_length), 31), not(31)))
                            let success := delegatecall(gas(), address(), ptr, call_length, 0, 0)
                            if iszero(success) {
                                let rds := returndatasize()
                                returndatacopy(0, 0, rds)
                                revert(0, rds)
                            }
                        }
                        stop()
                    }
                    default {
                        revert(0, 0)
                    }
"""
    text = replace_once(text, dispatcher_default, multicall_case, "multicall dispatcher")

    return text


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    path = Path(args.input)
    text = path.read_text(encoding="utf-8")
    patched = patch(text)
    Path(args.output).write_text(patched, encoding="utf-8")


if __name__ == "__main__":
    main()
