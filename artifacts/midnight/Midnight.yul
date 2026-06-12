object "Midnight" {
    code {
        mstore(64, 128)
        if callvalue() {
            revert(0, 0)
        }
        function mappingSlot(baseSlot, key) -> slot {
            mstore(0, key)
            mstore(32, baseSlot)
            slot := keccak256(0, 64)
        }
        function __verity_array_element_calldata_checked(data_offset, length, index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            word := calldataload(add(data_offset, mul(index, 32)))
        }
        function __verity_array_element_memory_checked(data_offset, length, index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            word := mload(add(data_offset, mul(index, 32)))
        }
        function __verity_array_element_word_calldata_checked(data_offset, length, index, element_words, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            word := calldataload(add(data_offset, mul(add(mul(index, element_words), word_offset), 32)))
        }
        function __verity_array_element_word_memory_checked(data_offset, length, index, element_words, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            word := mload(add(data_offset, mul(add(mul(index, element_words), word_offset), 32)))
        }
        function __verity_array_element_dynamic_word_calldata_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_word_pos := add(add(data_offset, __element_rel_offset), mul(word_offset, 32))
            if gt(__element_word_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__element_word_pos)
        }
        function __verity_array_element_dynamic_word_memory_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_word_pos := add(add(data_offset, __element_rel_offset), mul(word_offset, 32))
            word := mload(__element_word_pos)
        }
        function __verity_array_element_dynamic_data_offset_calldata_checked(data_offset, length, index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            if gt(__element_head_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := __element_head_pos
        }
        function __verity_array_element_dynamic_data_offset_memory_checked(data_offset, length, index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            word := __element_head_pos
        }
        function __verity_array_element_dynamic_member_length_calldata_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            if gt(__member_data_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__member_data_pos)
        }
        function __verity_array_element_dynamic_member_length_memory_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            word := mload(__member_data_pos)
        }
        function __verity_array_element_dynamic_member_data_offset_calldata_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            if gt(__member_data_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := add(__member_data_pos, 32)
        }
        function __verity_array_element_dynamic_member_data_offset_memory_checked(data_offset, length, index, word_offset) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            word := add(__member_data_pos, 32)
        }
        function __verity_array_element_dynamic_member_element_calldata_checked(data_offset, length, index, word_offset, inner_index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            let __member_length := calldataload(__member_data_pos)
            if iszero(lt(inner_index, __member_length)) {
                revert(0, 0)
            }
            let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
            if gt(__word_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__word_pos)
        }
        function __verity_array_element_dynamic_member_element_memory_checked(data_offset, length, index, word_offset, inner_index) -> word {
            if iszero(lt(index, length)) {
                revert(0, 0)
            }
            let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
            if lt(__element_rel_offset, mul(length, 32)) {
                revert(0, 0)
            }
            let __element_head_pos := add(data_offset, __element_rel_offset)
            let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
            let __member_data_pos := add(__element_head_pos, __member_rel_offset)
            let __member_length := mload(__member_data_pos)
            if iszero(lt(inner_index, __member_length)) {
                revert(0, 0)
            }
            let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
            word := mload(__word_pos)
        }
        function __verity_param_dynamic_head_word_calldata_checked(data_offset, word_offset) -> word {
            let __head_word_pos := add(data_offset, mul(word_offset, 32))
            if gt(__head_word_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__head_word_pos)
        }
        function __verity_param_dynamic_head_word_memory_checked(data_offset, word_offset) -> word {
            let __head_word_pos := add(data_offset, mul(word_offset, 32))
            word := mload(__head_word_pos)
        }
        function __verity_param_dynamic_member_length_calldata_checked(data_offset, word_offset) -> word {
            let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            if gt(__member_data_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__member_data_pos)
        }
        function __verity_param_dynamic_member_length_memory_checked(data_offset, word_offset) -> word {
            let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            word := mload(__member_data_pos)
        }
        function __verity_param_dynamic_member_data_offset_calldata_checked(data_offset, word_offset) -> word {
            let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            if gt(__member_data_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := add(__member_data_pos, 32)
        }
        function __verity_param_dynamic_member_data_offset_memory_checked(data_offset, word_offset) -> word {
            let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            word := add(__member_data_pos, 32)
        }
        function __verity_param_dynamic_member_element_calldata_checked(data_offset, word_offset, inner_index) -> word {
            let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            let __member_length := calldataload(__member_data_pos)
            if iszero(lt(inner_index, __member_length)) {
                revert(0, 0)
            }
            let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
            if gt(__word_pos, sub(calldatasize(), 32)) {
                revert(0, 0)
            }
            word := calldataload(__word_pos)
        }
        function __verity_param_dynamic_member_element_memory_checked(data_offset, word_offset, inner_index) -> word {
            let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
            let __member_data_pos := add(data_offset, __member_rel_offset)
            let __member_length := mload(__member_data_pos)
            if iszero(lt(inner_index, __member_length)) {
                revert(0, 0)
            }
            let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
            word := mload(__word_pos)
        }
        function internal_internal_multicall(calls_data_offset, calls_length) {
            {
                let __mc_head := calldataload(4)
                let __mc_base := add(4, __mc_head)
                let __mc_len := calldataload(__mc_base)
                let __mc_i := 0
                for {
                } lt(__mc_i, __mc_len) {
                    __mc_i := add(__mc_i, 1)
                } {
                    let __mc_elem_head := calldataload(add(add(__mc_base, 32), mul(__mc_i, 32)))
                    let __mc_elem := add(add(__mc_base, 32), __mc_elem_head)
                    let __mc_size := calldataload(__mc_elem)
                    let __mc_data := add(__mc_elem, 32)
                    let __mc_ptr := mload(64)
                    calldatacopy(__mc_ptr, __mc_data, __mc_size)
                    mstore(64, add(__mc_ptr, and(add(__mc_size, 31), not(31))))
                    let __mc_success := delegatecall(gas(), address(), __mc_ptr, __mc_size, 0, 0)
                    if iszero(__mc_success) {
                        let __mc_rds := returndatasize()
                        returndatacopy(0, 0, __mc_rds)
                        revert(0, __mc_rds)
                    }
                }
            }
            stop()
        }
        function internal_internal_INITIAL_CHAIN_ID() -> __ret0 {
            let cid := sload(1024)
            __ret0 := cid
            leave
        }
        function internal_internal_roleSetter() -> __ret0 {
            let value := sload(7)
            __ret0 := value
            leave
        }
        function internal_internal_feeSetter() -> __ret0 {
            let value := sload(8)
            __ret0 := value
            leave
        }
        function internal_internal_feeClaimer() -> __ret0 {
            let value := sload(9)
            __ret0 := value
            leave
        }
        function internal_internal_tickSpacingSetter() -> __ret0 {
            let value := sload(10)
            __ret0 := value
            leave
        }
        function internal_internal_consumed(user, group) -> __ret0 {
            let value := sload(mappingSlot(mappingSlot(2, user), group))
            __ret0 := value
            leave
        }
        function internal_internal_isAuthorized(authorizer, authorized) -> __ret0 {
            let value := sload(mappingSlot(mappingSlot(3, authorizer), authorized))
            __ret0 := iszero(eq(value, 0))
            leave
        }
        function internal_internal_defaultSettlementFeeCbp(loanToken, index) -> __ret0 {
            let value := sload(mappingSlot(mappingSlot(4, loanToken), index))
            __ret0 := value
            leave
        }
        function internal_internal_defaultContinuousFee(loanToken) -> __ret0 {
            let value := sload(mappingSlot(5, loanToken))
            __ret0 := value
            leave
        }
        function internal_internal_claimableSettlementFee(token) -> __ret0 {
            let value := sload(mappingSlot(6, token))
            __ret0 := value
            leave
        }
        function internal_internal_maxSettlementFee(index) -> __ret0 {
            let value := 5000000000000000
            if eq(index, 0) {
                value := 14000000000000
            }
            if eq(index, 1) {
                value := 14000000000000
            }
            if eq(index, 2) {
                value := 98000000000000
            }
            if eq(index, 3) {
                value := 417000000000000
            }
            if eq(index, 4) {
                value := 1250000000000000
            }
            if eq(index, 5) {
                value := 2500000000000000
            }
            __ret0 := value
            leave
        }
        function internal_internal_isLltvAllowed(lltv) -> __ret0 {
            __ret0 := or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(eq(lltv, 385000000000000000))), iszero(iszero(eq(lltv, 625000000000000000)))))), iszero(iszero(eq(lltv, 770000000000000000)))))), iszero(iszero(eq(lltv, 860000000000000000)))))), iszero(iszero(eq(lltv, 915000000000000000)))))), iszero(iszero(eq(lltv, 945000000000000000)))))), iszero(iszero(eq(lltv, 965000000000000000)))))), iszero(iszero(eq(lltv, 980000000000000000)))))), iszero(iszero(eq(lltv, 1000000000000000000))))
            leave
        }
        function internal_internal_maxLif(lltv, cursor) -> __ret0 {
            __ret0 := div(mul(1000000000000000000, 1000000000000000000), sub(1000000000000000000, div(mul(cursor, sub(1000000000000000000, lltv)), 1000000000000000000)))
            leave
        }
        function internal_internal_min(a, b) -> __ret0 {
            {
                let __ite_cond := lt(a, b)
                if __ite_cond {
                    __ret0 := a
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := b
                    leave
                }
            }
        }
        function internal_internal_validateCollateralParams(collateralParams_data_offset, collateralParams_length) -> __ret0 {
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
                let collateralToken := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 0)
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
                let lltv := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 1)
                let allowed := internal_internal_isLltvAllowed(lltv)
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
                let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                let lif := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 2)
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
            __ret0 := 1
            leave
        }
        function internal_internal_countBits128(bitmap) -> __ret0 {
            let count := 0
            for {
                let __forEach_idx := 0
                let __forEach_count := 128
                let i := 0
            } lt(__forEach_idx, __forEach_count) {
                __forEach_idx := add(__forEach_idx, 1)
            } {
                i := __forEach_idx
                let mask := shl(i, 1)
                if iszero(eq(and(bitmap, mask), 0)) {
                    count := add(count, 1)
                }
            }
            __ret0 := count
            leave
        }
        function internal_internal_collateralBitMask(index) -> __ret0 {
            __ret0 := shl(index, 1)
            leave
        }
        function internal_internal_collateralBitmapSetBit(bitmap, index) -> __ret0 {
            let mask := internal_internal_collateralBitMask(index)
            __ret0 := or(bitmap, mask)
            leave
        }
        function internal_internal_collateralBitmapClearBit(bitmap, index) -> __ret0 {
            let mask := internal_internal_collateralBitMask(index)
            __ret0 := and(bitmap, not(mask))
            leave
        }
        function internal_internal_collateralBitmapIsSet(bitmap, index) -> __ret0 {
            let mask := internal_internal_collateralBitMask(index)
            __ret0 := gt(and(bitmap, mask), 0)
            leave
        }
        function internal_internal_codeDataMarketId(market_data_offset) -> __ret0 {
            let initialChainId := sload(1024)
            let self := address()
            let id := 0
            {
                let __midnight_id_ptr := mload(64)
                mstore(__midnight_id_ptr, shl(168, 0x600b380380600b5f395ff3))
                mstore(add(__midnight_id_ptr, 11), 32)
                let __midnight_id_tuple_ptr := add(__midnight_id_ptr, 43)
                mstore(__midnight_id_tuple_ptr, calldataload(market_data_offset))
                mstore(add(__midnight_id_tuple_ptr, 32), 192)
                mstore(add(__midnight_id_tuple_ptr, 64), calldataload(add(market_data_offset, 64)))
                mstore(add(__midnight_id_tuple_ptr, 96), calldataload(add(market_data_offset, 96)))
                mstore(add(__midnight_id_tuple_ptr, 128), calldataload(add(market_data_offset, 128)))
                mstore(add(__midnight_id_tuple_ptr, 160), calldataload(add(market_data_offset, 160)))
                let __midnight_id_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                let __midnight_id_collateral_length := calldataload(__midnight_id_collateral_offset)
                let __midnight_id_collateral_bytes := mul(__midnight_id_collateral_length, 128)
                mstore(add(__midnight_id_tuple_ptr, 192), __midnight_id_collateral_length)
                calldatacopy(add(__midnight_id_tuple_ptr, 224), add(__midnight_id_collateral_offset, 32), __midnight_id_collateral_bytes)
                let __midnight_id_abi_length := add(256, __midnight_id_collateral_bytes)
                let __midnight_id_initcode_length := add(11, __midnight_id_abi_length)
                let __midnight_id_inner_hash := keccak256(__midnight_id_ptr, __midnight_id_initcode_length)
                let __midnight_id_outer_ptr := add(__midnight_id_ptr, and(add(__midnight_id_initcode_length, 31), not(31)))
                mstore(__midnight_id_outer_ptr, shl(248, 255))
                mstore(add(__midnight_id_outer_ptr, 1), shl(96, self))
                mstore(add(__midnight_id_outer_ptr, 21), initialChainId)
                mstore(add(__midnight_id_outer_ptr, 53), __midnight_id_inner_hash)
                id := keccak256(__midnight_id_outer_ptr, 85)
                mstore(64, add(__midnight_id_outer_ptr, 96))
            }
            __ret0 := id
            leave
        }
        function internal_internal_codeDataStoreMarket(market_data_offset, salt) -> __ret0 {
            let pointer := 0
            {
                let __midnight_store_ptr := mload(64)
                mstore(__midnight_store_ptr, shl(168, 0x600b380380600b5f395ff3))
                mstore(add(__midnight_store_ptr, 11), 32)
                let __midnight_store_tuple_ptr := add(__midnight_store_ptr, 43)
                mstore(__midnight_store_tuple_ptr, calldataload(market_data_offset))
                mstore(add(__midnight_store_tuple_ptr, 32), 192)
                mstore(add(__midnight_store_tuple_ptr, 64), calldataload(add(market_data_offset, 64)))
                mstore(add(__midnight_store_tuple_ptr, 96), calldataload(add(market_data_offset, 96)))
                mstore(add(__midnight_store_tuple_ptr, 128), calldataload(add(market_data_offset, 128)))
                mstore(add(__midnight_store_tuple_ptr, 160), calldataload(add(market_data_offset, 160)))
                let __midnight_store_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                let __midnight_store_collateral_length := calldataload(__midnight_store_collateral_offset)
                let __midnight_store_collateral_bytes := mul(__midnight_store_collateral_length, 128)
                mstore(add(__midnight_store_tuple_ptr, 192), __midnight_store_collateral_length)
                calldatacopy(add(__midnight_store_tuple_ptr, 224), add(__midnight_store_collateral_offset, 32), __midnight_store_collateral_bytes)
                let __midnight_store_abi_length := add(256, __midnight_store_collateral_bytes)
                let __midnight_store_initcode_length := add(11, __midnight_store_abi_length)
                pointer := create2(0, __midnight_store_ptr, __midnight_store_initcode_length, salt)
                if iszero(pointer) {
                    mstore(0, shl(224, 0x4e487b71))
                    mstore(4, 81)
                    revert(0, 36)
                }
                mstore(64, add(__midnight_store_ptr, and(add(__midnight_store_initcode_length, 31), not(31))))
            }
            __ret0 := pointer
            leave
        }
        function internal_internal_toId(market_data_offset) -> __ret0 {
            let id := internal_internal_codeDataMarketId(market_data_offset)
            __ret0 := id
            leave
        }
        function internal_internal_toMarket(id) {
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            {
                let __midnight_market_return_pointer := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
                let __midnight_market_return_length := extcodesize(__midnight_market_return_pointer)
                let __midnight_market_return_ptr := mload(64)
                extcodecopy(__midnight_market_return_pointer, __midnight_market_return_ptr, 0, __midnight_market_return_length)
                return(__midnight_market_return_ptr, __midnight_market_return_length)
            }
            stop()
        }
        function internal_internal_position(id, user) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5 {
            let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
            let collateralBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
            __ret0 := credit
            __ret1 := pendingFee
            __ret2 := lastLossFactor
            __ret3 := lastAccrual
            __ret4 := debt
            __ret5 := collateralBitmap
            leave
        }
        function internal_internal_marketState(id) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10, __ret11, __ret12 {
            let totalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let lossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let withdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            let continuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
            let continuousFee := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
            let tickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            __ret0 := totalUnits
            __ret1 := lossFactor
            __ret2 := withdrawable
            __ret3 := continuousFeeCredit
            __ret4 := settlementFeeCbp0
            __ret5 := settlementFeeCbp1
            __ret6 := settlementFeeCbp2
            __ret7 := settlementFeeCbp3
            __ret8 := settlementFeeCbp4
            __ret9 := settlementFeeCbp5
            __ret10 := settlementFeeCbp6
            __ret11 := continuousFee
            __ret12 := tickSpacing
            leave
        }
        function internal_internal_updatePositionView(market_data_offset, id, user) -> __ret0, __ret1, __ret2 {
            let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let now := timestamp()
            let postSlashCredit := 0
            if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
            }
            let postSlashPendingFee := 0
            if gt(credit, 0) {
                postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
            }
            let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
            if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                accrualEnd := now
            }
            let accrued := 0
            if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
            }
            __ret0 := sub(postSlashCredit, accrued)
            __ret1 := sub(postSlashPendingFee, accrued)
            __ret2 := accrued
            leave
        }
        function internal_internal_updatePosition(market_data_offset, user) -> __ret0, __ret1, __ret2 {
            let id := internal_internal_toId(market_data_offset)
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let now := timestamp()
            let postSlashCredit := 0
            if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
            }
            let postSlashPendingFee := 0
            if gt(credit, 0) {
                postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
            }
            let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
            if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                accrualEnd := now
            }
            let accrued := 0
            if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
            }
            let newCredit := sub(postSlashCredit, accrued)
            let newPendingFee := sub(postSlashPendingFee, accrued)
            let creditDecrease := sub(credit, newCredit)
            let pendingFeeDecrease := sub(pendingFee, newPendingFee)
            {
                let __compat_value := newCredit
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := marketLossFactor
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := newPendingFee
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            {
                let __compat_value := now
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := add(currentContinuousFeeCredit, accrued)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 55)
                mstore(add(__evt_ptr, 0), creditDecrease)
                mstore(add(__evt_ptr, 32), pendingFeeDecrease)
                mstore(add(__evt_ptr, 64), accrued)
                log3(__evt_ptr, 96, __evt_topic0, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            __ret0 := newCredit
            __ret1 := newPendingFee
            __ret2 := accrued
            leave
        }
        function internal_internal_setRoleSetter(newRoleSetter) {
            let sender := caller()
            let currentRoleSetter := sload(7)
            if iszero(eq(sender, currentRoleSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(7, and(newRoleSetter, 0xffffffffffffffffffffffffffffffffffffffff))
            stop()
        }
        function internal_internal_setFeeSetter(newFeeSetter) {
            let sender := caller()
            let currentRoleSetter := sload(7)
            if iszero(eq(sender, currentRoleSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(8, and(newFeeSetter, 0xffffffffffffffffffffffffffffffffffffffff))
            stop()
        }
        function internal_internal_setFeeClaimer(newFeeClaimer) {
            let sender := caller()
            let currentRoleSetter := sload(7)
            if iszero(eq(sender, currentRoleSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(9, and(newFeeClaimer, 0xffffffffffffffffffffffffffffffffffffffff))
            stop()
        }
        function internal_internal_setTickSpacingSetter(newTickSpacingSetter) {
            let sender := caller()
            let currentRoleSetter := sload(7)
            if iszero(eq(sender, currentRoleSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(10, and(newTickSpacingSetter, 0xffffffffffffffffffffffffffffffffffffffff))
            stop()
        }
        function internal_internal_setIsAuthorized(authorized, newIsAuthorized, onBehalf) {
            let sender := caller()
            let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
            if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let flag := 0
            {
                let __ite_cond := newIsAuthorized
                if __ite_cond {
                    flag := 1
                }
                if iszero(__ite_cond) {
                    flag := 0
                }
            }
            sstore(mappingSlot(mappingSlot(3, onBehalf), authorized), flag)
            stop()
        }
        function internal_internal_setConsumed(group, amount, onBehalf) {
            let sender := caller()
            let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
            if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let current := sload(mappingSlot(mappingSlot(2, onBehalf), group))
            if lt(amount, current) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x416c7265616479436f6e73756d65642829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(mappingSlot(mappingSlot(2, onBehalf), group), amount)
            stop()
        }
        function internal_internal_setDefaultSettlementFee(loanToken, index, newSettlementFee) {
            let sender := caller()
            let currentFeeSetter := sload(8)
            if iszero(eq(sender, currentFeeSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 15)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(lt(index, 7)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let maxFee := 5000000000000000
            if eq(index, 0) {
                maxFee := 14000000000000
            }
            if eq(index, 1) {
                maxFee := 14000000000000
            }
            if eq(index, 2) {
                maxFee := 98000000000000
            }
            if eq(index, 3) {
                maxFee := 417000000000000
            }
            if eq(index, 4) {
                maxFee := 1250000000000000
            }
            if eq(index, 5) {
                maxFee := 2500000000000000
            }
            if gt(newSettlementFee, maxFee) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 22)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                    let __err_hash := keccak256(__err_ptr, 24)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(mappingSlot(mappingSlot(4, loanToken), index), div(newSettlementFee, 1000000000000))
            stop()
        }
        function internal_internal_setDefaultContinuousFee(loanToken, newContinuousFee) {
            let sender := caller()
            let currentFeeSetter := sload(8)
            if iszero(eq(sender, currentFeeSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 15)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if gt(newContinuousFee, 317097919) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 22)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(mappingSlot(5, loanToken), newContinuousFee)
            stop()
        }
        function internal_internal_claimSettlementFee(token, amount, receiver) {
            let sender := caller()
            let currentFeeClaimer := sload(9)
            if iszero(eq(sender, currentFeeClaimer)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let claimable := sload(mappingSlot(6, token))
            if gt(amount, claimable) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            sstore(mappingSlot(6, token), sub(claimable, amount))
            {
                if iszero(gt(extcodesize(token), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_ptr := mload(64)
                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lst_ptr, 4), receiver)
                mstore(add(__lst_ptr, 36), amount)
                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                let __lst_success := call(gas(), token, 0, __lst_ptr, 68, __lst_ptr, 32)
                if iszero(__lst_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 17)
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 23)
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                        revert(0, 100)
                    }
                }
            }
            stop()
        }
        function internal_internal_claimContinuousFee(market_data_offset, amount, receiver) {
            let id := internal_internal_toId(market_data_offset)
            let sender := caller()
            let currentFeeClaimer := sload(9)
            if iszero(eq(sender, currentFeeClaimer)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(currentContinuousFeeCredit, amount)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(currentTotalUnits, amount)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(1, id))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let currentWithdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(currentWithdrawable, amount)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x436c61696d436f6e74696e756f757346656528616464726573732c6279746573)
                mstore(add(__evt_ptr, 32), 0x33322c75696e743235362c616464726573732900000000000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 51)
                mstore(add(__evt_ptr, 0), amount)
                log4(__evt_ptr, 32, __evt_topic0, and(sender, 0xffffffffffffffffffffffffffffffffffffffff), id, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_ptr := mload(64)
                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lst_ptr, 4), receiver)
                mstore(add(__lst_ptr, 36), amount)
                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                if iszero(__lst_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 17)
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 23)
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                        revert(0, 100)
                    }
                }
            }
            stop()
        }
        function internal_internal_setMarketTickSpacing(id, newTickSpacing) {
            let sender := caller()
            let currentTickSpacingSetter := sload(10)
            if iszero(eq(sender, currentTickSpacingSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c795469636b53706163696e675365747465722829000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 23)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(gt(newTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 20)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(eq(mod(currentTickSpacing, newTickSpacing), 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 20)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            {
                let __compat_value := newTickSpacing
                let __compat_packed := and(__compat_value, 255)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
            }
            stop()
        }
        function internal_internal_setMarketSettlementFee(id, index, newSettlementFee) {
            let sender := caller()
            let currentFeeSetter := sload(8)
            if iszero(eq(sender, currentFeeSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 15)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(lt(index, 7)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let maxFee := 5000000000000000
            if eq(index, 0) {
                maxFee := 14000000000000
            }
            if eq(index, 1) {
                maxFee := 14000000000000
            }
            if eq(index, 2) {
                maxFee := 98000000000000
            }
            if eq(index, 3) {
                maxFee := 417000000000000
            }
            if eq(index, 4) {
                maxFee := 1250000000000000
            }
            if eq(index, 5) {
                maxFee := 2500000000000000
            }
            if gt(newSettlementFee, maxFee) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 22)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                    let __err_hash := keccak256(__err_ptr, 24)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let newSettlementFeeCbp := div(newSettlementFee, 1000000000000)
            if eq(index, 0) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
            }
            if eq(index, 1) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                }
            }
            if eq(index, 2) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                }
            }
            if eq(index, 3) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                }
            }
            if eq(index, 4) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                }
            }
            if eq(index, 5) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                }
            }
            if eq(index, 6) {
                {
                    let __compat_value := newSettlementFeeCbp
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                }
            }
            stop()
        }
        function internal_internal_setMarketContinuousFee(id, newContinuousFee) {
            let sender := caller()
            let currentFeeSetter := sload(8)
            if iszero(eq(sender, currentFeeSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 15)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if gt(newContinuousFee, 317097919) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 22)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            {
                let __compat_value := newContinuousFee
                let __compat_packed := and(__compat_value, 4294967295)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
            }
            stop()
        }
        function internal_internal_touchMarket(market_data_offset) -> __ret0 {
            let id := internal_internal_toId(market_data_offset)
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if eq(currentTickSpacing, 0) {
                let now := timestamp()
                if gt(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), add(now, 3153600000)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let _collateralParamsValid := internal_internal_validateCollateralParams(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1))
                let salt := sload(1024)
                let _marketPointer := internal_internal_codeDataStoreMarket(market_data_offset, salt)
                {
                    let __compat_value := 4
                    let __compat_packed := and(__compat_value, 255)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                }
                let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0)
                let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 1)
                let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 2)
                let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 3)
                let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 4)
                let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 5)
                let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 6)
                let continuous := sload(mappingSlot(5, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)))
                {
                    let __compat_value := settlementFeeCbp0
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp1
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp2
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp3
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp4
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp5
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp6
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                }
                {
                    let __compat_value := continuous
                    let __compat_packed := and(__compat_value, 4294967295)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                }
            }
            __ret0 := id
            leave
        }
        function internal_internal_settlementFee(id, timeToMaturity) -> __ret0 {
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(gt(currentTickSpacing, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 18)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
            let start := 15552000
            let finish := 31104000
            let feeLower := mul(settlementFeeCbp5, 1000000000000)
            let feeUpper := mul(settlementFeeCbp6, 1000000000000)
            if lt(timeToMaturity, 86400) {
                start := 0
                finish := 86400
                feeLower := mul(settlementFeeCbp0, 1000000000000)
                feeUpper := mul(settlementFeeCbp1, 1000000000000)
            }
            if and(iszero(iszero(iszero(lt(timeToMaturity, 86400)))), iszero(iszero(lt(timeToMaturity, 604800)))) {
                start := 86400
                finish := 604800
                feeLower := mul(settlementFeeCbp1, 1000000000000)
                feeUpper := mul(settlementFeeCbp2, 1000000000000)
            }
            if and(iszero(iszero(iszero(lt(timeToMaturity, 604800)))), iszero(iszero(lt(timeToMaturity, 2592000)))) {
                start := 604800
                finish := 2592000
                feeLower := mul(settlementFeeCbp2, 1000000000000)
                feeUpper := mul(settlementFeeCbp3, 1000000000000)
            }
            if and(iszero(iszero(iszero(lt(timeToMaturity, 2592000)))), iszero(iszero(lt(timeToMaturity, 7776000)))) {
                start := 2592000
                finish := 7776000
                feeLower := mul(settlementFeeCbp3, 1000000000000)
                feeUpper := mul(settlementFeeCbp4, 1000000000000)
            }
            if and(iszero(iszero(iszero(lt(timeToMaturity, 7776000)))), iszero(iszero(lt(timeToMaturity, 15552000)))) {
                start := 7776000
                finish := 15552000
                feeLower := mul(settlementFeeCbp4, 1000000000000)
                feeUpper := mul(settlementFeeCbp5, 1000000000000)
            }
            {
                let __ite_cond := iszero(lt(timeToMaturity, 31104000))
                if __ite_cond {
                    __ret0 := mul(settlementFeeCbp6, 1000000000000)
                    leave
                }
                if iszero(__ite_cond) {
                    __ret0 := div(add(mul(feeLower, sub(finish, timeToMaturity)), mul(feeUpper, sub(timeToMaturity, start))), sub(finish, start))
                    leave
                }
            }
        }
        function internal_internal_tickToPrice(tick) -> __ret0 {
            if gt(tick, 5820) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x5469636b4f75744f6652616e6765282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let price := 0
            {
                let __tp_x := mul(4987541511039073, sub(2910, tick))
                let __tp_abs := __tp_x
                let __tp_negative := slt(__tp_x, 0)
                if __tp_negative {
                    __tp_abs := sub(0, __tp_x)
                }
                let __tp_q := div(add(__tp_abs, 322611214989459870), 693147180559945309)
                let __tp_r := sub(__tp_abs, mul(__tp_q, 693147180559945309))
                let __tp_second := sdiv(mul(__tp_r, __tp_r), 2000000000000000000)
                let __tp_third := sdiv(mul(__tp_second, __tp_r), 3000000000000000000)
                let __tp_expR := add(1000000000000000000, add(__tp_r, add(__tp_second, __tp_third)))
                let __tp_wexp := shl(__tp_q, __tp_expR)
                if __tp_negative {
                    __tp_wexp := div(1000000000000000000000000000000000000, __tp_wexp)
                }
                let __tp_den := add(1000000000000000000, __tp_wexp)
                let __tp_raw := div(add(1000000000000000000000000000000000000, div(sub(__tp_den, 1), 2)), __tp_den)
                price := mul(div(add(__tp_raw, div(sub(1000000000000, 1), 2)), 1000000000000), 1000000000000)
            }
            __ret0 := price
            leave
        }
        function internal_internal_enterGateCanIncreaseCredit(gate, account) -> __ret0 {
            let allowed := 0
            {
                let __ecwr_ptr := mload(64)
                mstore(__ecwr_ptr, shl(224, 0x58ac9f9e))
                mstore(add(__ecwr_ptr, 4), account)
                mstore(64, add(__ecwr_ptr, 64))
                let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                if iszero(__ecwr_success) {
                    let __ecwr_rds := returndatasize()
                    returndatacopy(0, 0, __ecwr_rds)
                    revert(0, __ecwr_rds)
                }
                if lt(returndatasize(), 32) {
                    revert(0, 0)
                }
                allowed := mload(__ecwr_ptr)
            }
            __ret0 := allowed
            leave
        }
        function internal_internal_enterGateCanIncreaseDebt(gate, account) -> __ret0 {
            let allowed := 0
            {
                let __ecwr_ptr := mload(64)
                mstore(__ecwr_ptr, shl(224, 0xfe9bf956))
                mstore(add(__ecwr_ptr, 4), account)
                mstore(64, add(__ecwr_ptr, 64))
                let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                if iszero(__ecwr_success) {
                    let __ecwr_rds := returndatasize()
                    returndatacopy(0, 0, __ecwr_rds)
                    revert(0, __ecwr_rds)
                }
                if lt(returndatasize(), 32) {
                    revert(0, 0)
                }
                allowed := mload(__ecwr_ptr)
            }
            __ret0 := allowed
            leave
        }
        function internal_internal_updateBuyerForTake(id, buyer, units, maturity, now) -> __ret0 {
            let buyerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))), 340282366920938463463374607431768211455)
            let buyerDebtDecrease := internal_internal_min(units, buyerDebt)
            let buyerCreditIncrease := sub(units, buyerDebtDecrease)
            let continuousFeeValue := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
            let timeToMaturity := 0
            if gt(maturity, now) {
                timeToMaturity := sub(maturity, now)
            }
            let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValue, timeToMaturity)), 1000000000000000000)
            let buyerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
            let buyerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
            let buyerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
            let buyerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
            let buyerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let buyerPostSlashCredit := 0
            if lt(buyerLastLossFactor, 340282366920938463463374607431768211455) {
                buyerPostSlashCredit := div(mul(buyerCredit, sub(340282366920938463463374607431768211455, buyerMarketLossFactor)), sub(340282366920938463463374607431768211455, buyerLastLossFactor))
            }
            let buyerPostSlashPendingFee := 0
            if gt(buyerCredit, 0) {
                buyerPostSlashPendingFee := sub(buyerPendingFee, div(add(mul(buyerPendingFee, sub(buyerCredit, buyerPostSlashCredit)), sub(buyerCredit, 1)), buyerCredit))
            }
            let buyerAccrualEnd := maturity
            if iszero(gt(now, maturity)) {
                buyerAccrualEnd := now
            }
            let buyerAccrued := 0
            if lt(buyerLastAccrual, maturity) {
                buyerAccrued := div(mul(buyerPostSlashPendingFee, sub(buyerAccrualEnd, buyerLastAccrual)), sub(maturity, buyerLastAccrual))
            }
            let buyerCreditAfterUpdate := sub(buyerPostSlashCredit, buyerAccrued)
            let buyerPendingFeeAfterUpdate := sub(buyerPostSlashPendingFee, buyerAccrued)
            let buyerCreditDecrease := sub(buyerCredit, buyerCreditAfterUpdate)
            let buyerPendingFeeDecreaseForUpdate := sub(buyerPendingFee, buyerPendingFeeAfterUpdate)
            if or(iszero(iszero(gt(buyerCredit, 0))), iszero(iszero(gt(buyerCreditIncrease, 0)))) {
                {
                    let __compat_value := buyerCreditAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := buyerMarketLossFactor
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := buyerPendingFeeAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := now
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(currentContinuousFeeCredit, buyerAccrued)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 55)
                mstore(add(__evt_ptr, 0), buyerCreditDecrease)
                mstore(add(__evt_ptr, 32), buyerPendingFeeDecreaseForUpdate)
                mstore(add(__evt_ptr, 64), buyerAccrued)
                log3(__evt_ptr, 96, __evt_topic0, id, and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                let __compat_value := sub(buyerDebt, buyerDebtDecrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(mappingSlot(0, id), buyer), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := add(buyerPendingFeeAfterUpdate, buyerPendingFeeIncrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            {
                let __compat_value := add(buyerCreditAfterUpdate, buyerCreditIncrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            __ret0 := buyerCreditIncrease
            leave
        }
        function internal_internal_updateSellerForTake(id, seller, units, maturity, now) -> __ret0, __ret1, __ret2 {
            let sellerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
            let sellerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
            let sellerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
            let sellerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
            let sellerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let sellerPostSlashCredit := 0
            if lt(sellerLastLossFactor, 340282366920938463463374607431768211455) {
                sellerPostSlashCredit := div(mul(sellerCredit, sub(340282366920938463463374607431768211455, sellerMarketLossFactor)), sub(340282366920938463463374607431768211455, sellerLastLossFactor))
            }
            let sellerPostSlashPendingFee := 0
            if gt(sellerCredit, 0) {
                sellerPostSlashPendingFee := sub(sellerPendingFee, div(add(mul(sellerPendingFee, sub(sellerCredit, sellerPostSlashCredit)), sub(sellerCredit, 1)), sellerCredit))
            }
            let sellerAccrualEnd := maturity
            if iszero(gt(now, maturity)) {
                sellerAccrualEnd := now
            }
            let sellerAccrued := 0
            if lt(sellerLastAccrual, maturity) {
                sellerAccrued := div(mul(sellerPostSlashPendingFee, sub(sellerAccrualEnd, sellerLastAccrual)), sub(maturity, sellerLastAccrual))
            }
            let sellerCreditAfterUpdate := sub(sellerPostSlashCredit, sellerAccrued)
            let sellerPendingFeeAfterUpdate := sub(sellerPostSlashPendingFee, sellerAccrued)
            let sellerCreditDecreaseForUpdate := sub(sellerCredit, sellerCreditAfterUpdate)
            let sellerPendingFeeDecreaseForUpdate := sub(sellerPendingFee, sellerPendingFeeAfterUpdate)
            if gt(sellerCredit, 0) {
                {
                    let __compat_value := sellerCreditAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := sellerMarketLossFactor
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := sellerPendingFeeAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := now
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(currentContinuousFeeCredit, sellerAccrued)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 55)
                mstore(add(__evt_ptr, 0), sellerCreditDecreaseForUpdate)
                mstore(add(__evt_ptr, 32), sellerPendingFeeDecreaseForUpdate)
                mstore(add(__evt_ptr, 64), sellerAccrued)
                log3(__evt_ptr, 96, __evt_topic0, id, and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            let sellerCreditDecrease := internal_internal_min(units, sellerCreditAfterUpdate)
            let sellerDebtIncrease := sub(units, sellerCreditDecrease)
            let sellerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 2))), 340282366920938463463374607431768211455)
            let sellerPendingFeeDecrease := 0
            if gt(sellerCreditAfterUpdate, 0) {
                sellerPendingFeeDecrease := div(add(mul(sellerPendingFeeAfterUpdate, sellerCreditDecrease), sub(sellerCreditAfterUpdate, 1)), sellerCreditAfterUpdate)
            }
            {
                let __compat_value := sub(sellerPendingFeeAfterUpdate, sellerPendingFeeDecrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            {
                let __compat_value := sub(sellerCreditAfterUpdate, sellerCreditDecrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := add(sellerDebt, sellerDebtIncrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 2))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(mappingSlot(0, id), seller), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            __ret0 := sellerCreditDecrease
            __ret1 := sellerDebtIncrease
            __ret2 := sellerPendingFeeDecrease
            leave
        }
        function internal_internal_takeAssetsForPrice(id, units, tick, offerIsBuy, maturity, now, buyerCreditIncrease) -> __ret0, __ret1, __ret2 {
            let offerPrice := internal_internal_tickToPrice(tick)
            let timeToMaturityForFee := 0
            if gt(maturity, now) {
                timeToMaturityForFee := sub(maturity, now)
            }
            let settlementFeeValue := internal_internal_settlementFee(id, timeToMaturityForFee)
            let continuousFeeValueForCallback := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
            let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValueForCallback, timeToMaturityForFee)), 1000000000000000000)
            let sellerPrice := offerPrice
            if offerIsBuy {
                sellerPrice := sub(offerPrice, settlementFeeValue)
            }
            let buyerPrice := add(sellerPrice, settlementFeeValue)
            let buyerAssets := 0
            let sellerAssets := 0
            {
                let __ite_cond := offerIsBuy
                if __ite_cond {
                    buyerAssets := div(mul(units, buyerPrice), 1000000000000000000)
                    sellerAssets := div(mul(units, sellerPrice), 1000000000000000000)
                }
                if iszero(__ite_cond) {
                    buyerAssets := div(add(mul(units, buyerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                    sellerAssets := div(add(mul(units, sellerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                }
            }
            __ret0 := buyerAssets
            __ret1 := sellerAssets
            __ret2 := buyerPendingFeeIncrease
            leave
        }
        function internal_internal_take(offer_data_offset, ratifierData_data_offset, ratifierData_length, units, taker, receiverIfTakerIsSeller, takerCallback, takerCallbackData_data_offset, takerCallbackData_length) -> __ret0, __ret1 {
            let sender := caller()
            let authorized := internal_internal_isAuthorized(taker, sender)
            if iszero(or(iszero(iszero(eq(taker, sender))), iszero(iszero(authorized)))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x54616b6572556e617574686f72697a6564282900000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 19)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let offerBase := add(calldataload(4), 4)
            let marketBase := add(offerBase, calldataload(offerBase))
            let loanToken := calldataload(marketBase)
            let maturity := calldataload(add(marketBase, 64))
            let enterGate := calldataload(add(marketBase, 128))
            let initialChainId := sload(1024)
            let contractSelf := address()
            let id := 0
            {
                let __midnight_id_ptr := mload(64)
                mstore(__midnight_id_ptr, shl(168, 0x600b380380600b5f395ff3))
                mstore(add(__midnight_id_ptr, 11), 32)
                let __midnight_id_tuple_ptr := add(__midnight_id_ptr, 43)
                mstore(__midnight_id_tuple_ptr, calldataload(marketBase))
                mstore(add(__midnight_id_tuple_ptr, 32), 192)
                mstore(add(__midnight_id_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                mstore(add(__midnight_id_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                mstore(add(__midnight_id_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                mstore(add(__midnight_id_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                let __midnight_id_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                let __midnight_id_collateral_length := calldataload(__midnight_id_collateral_offset)
                let __midnight_id_collateral_bytes := mul(__midnight_id_collateral_length, 128)
                mstore(add(__midnight_id_tuple_ptr, 192), __midnight_id_collateral_length)
                calldatacopy(add(__midnight_id_tuple_ptr, 224), add(__midnight_id_collateral_offset, 32), __midnight_id_collateral_bytes)
                let __midnight_id_abi_length := add(256, __midnight_id_collateral_bytes)
                let __midnight_id_initcode_length := add(11, __midnight_id_abi_length)
                let __midnight_id_inner_hash := keccak256(__midnight_id_ptr, __midnight_id_initcode_length)
                let __midnight_id_outer_ptr := add(__midnight_id_ptr, and(add(__midnight_id_initcode_length, 31), not(31)))
                mstore(__midnight_id_outer_ptr, shl(248, 255))
                mstore(add(__midnight_id_outer_ptr, 1), shl(96, contractSelf))
                mstore(add(__midnight_id_outer_ptr, 21), initialChainId)
                mstore(add(__midnight_id_outer_ptr, 53), __midnight_id_inner_hash)
                id := keccak256(__midnight_id_outer_ptr, 85)
                mstore(64, add(__midnight_id_outer_ptr, 96))
            }
            let currentMarketTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if eq(currentMarketTickSpacing, 0) {
                let now := timestamp()
                if gt(maturity, add(now, 3153600000)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let takeCollateralParamsOffset := add(marketBase, calldataload(add(marketBase, 32)))
                let takeCollateralCount := calldataload(takeCollateralParamsOffset)
                if iszero(gt(takeCollateralCount, 0)) {
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
                if gt(takeCollateralCount, 128) {
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
                    let __forEach_count := takeCollateralCount
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    let collateralParamOffset := add(add(takeCollateralParamsOffset, 32), mul(i, 128))
                    let collateralToken := calldataload(collateralParamOffset)
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
                    let lltv := calldataload(add(collateralParamOffset, 32))
                    let allowed := internal_internal_isLltvAllowed(lltv)
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
                    let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                    let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                    let lif := calldataload(add(collateralParamOffset, 64))
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
                let _marketPointer := 0
                {
                    let __midnight_store_ptr := mload(64)
                    mstore(__midnight_store_ptr, shl(168, 0x600b380380600b5f395ff3))
                    mstore(add(__midnight_store_ptr, 11), 32)
                    let __midnight_store_tuple_ptr := add(__midnight_store_ptr, 43)
                    mstore(__midnight_store_tuple_ptr, calldataload(marketBase))
                    mstore(add(__midnight_store_tuple_ptr, 32), 192)
                    mstore(add(__midnight_store_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                    mstore(add(__midnight_store_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                    mstore(add(__midnight_store_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                    mstore(add(__midnight_store_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                    let __midnight_store_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                    let __midnight_store_collateral_length := calldataload(__midnight_store_collateral_offset)
                    let __midnight_store_collateral_bytes := mul(__midnight_store_collateral_length, 128)
                    mstore(add(__midnight_store_tuple_ptr, 192), __midnight_store_collateral_length)
                    calldatacopy(add(__midnight_store_tuple_ptr, 224), add(__midnight_store_collateral_offset, 32), __midnight_store_collateral_bytes)
                    let __midnight_store_abi_length := add(256, __midnight_store_collateral_bytes)
                    let __midnight_store_initcode_length := add(11, __midnight_store_abi_length)
                    _marketPointer := create2(0, __midnight_store_ptr, __midnight_store_initcode_length, initialChainId)
                    if iszero(_marketPointer) {
                        mstore(0, shl(224, 0x4e487b71))
                        mstore(4, 81)
                        revert(0, 36)
                    }
                    mstore(64, add(__midnight_store_ptr, and(add(__midnight_store_initcode_length, 31), not(31))))
                }
                {
                    let __compat_value := 4
                    let __compat_packed := and(__compat_value, 255)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                }
                let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(loanToken, 0)
                let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(loanToken, 1)
                let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(loanToken, 2)
                let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(loanToken, 3)
                let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(loanToken, 4)
                let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(loanToken, 5)
                let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(loanToken, 6)
                let continuous := sload(mappingSlot(5, loanToken))
                {
                    let __compat_value := settlementFeeCbp0
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp1
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp2
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp3
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp4
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp5
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                }
                {
                    let __compat_value := settlementFeeCbp6
                    let __compat_packed := and(__compat_value, 65535)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                }
                {
                    let __compat_value := continuous
                    let __compat_packed := and(__compat_value, 4294967295)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                }
            }
            let lossFactorForGuard := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            if iszero(lt(lossFactorForGuard, 340282366920938463463374607431768211455)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d61726b65744c6f7373466163746f724d617865644f75742829000000000000)
                    let __err_hash := keccak256(__err_ptr, 26)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0))), iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12), 0))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d756c7469706c654e6f6e5a65726f2829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            if iszero(eq(mod(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), currentTickSpacing), 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x5469636b4e6f7441636365737369626c65282900000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 19)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let now := timestamp()
            if lt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 3)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f666665724e6f74537461727465642829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if gt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 4)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f66666572457870697265642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), taker))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x53656c6654616b65282900000000000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 10)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let ratifierAuthorized := internal_internal_isAuthorized(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10))
            if iszero(ratifierAuthorized) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x5261746966696572556e617574686f72697a6564282900000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 22)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            {
                let __rat_ptr := mload(64)
                let __rat_offer_ptr := add(__rat_ptr, 68)
                let __rat_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                let __rat_collateral_offset := add(__rat_market_offset, calldataload(add(__rat_market_offset, 32)))
                let __rat_collateral_length := calldataload(__rat_collateral_offset)
                let __rat_collateral_bytes := mul(__rat_collateral_length, 128)
                let __rat_market_size := add(224, __rat_collateral_bytes)
                let __rat_padded_market_size := and(add(__rat_market_size, 31), not(31))
                let __rat_callback_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                let __rat_callback_data_length := calldataload(__rat_callback_data_offset)
                let __rat_padded_callback_data_length := and(add(__rat_callback_data_length, 31), not(31))
                let __rat_offer_size := add(448, add(__rat_padded_market_size, add(32, __rat_padded_callback_data_length)))
                let __rat_padded_offer_size := and(add(__rat_offer_size, 31), not(31))
                let __rat_data_ptr := add(__rat_offer_ptr, __rat_padded_offer_size)
                let __rat_padded_data_length := and(add(ratifierData_length, 31), not(31))
                let __rat_total := add(68, add(__rat_padded_offer_size, add(32, __rat_padded_data_length)))
                mstore(__rat_ptr, shl(224, 0x675ef8d3))
                mstore(add(__rat_ptr, 4), 64)
                mstore(add(__rat_ptr, 36), add(64, __rat_padded_offer_size))
                mstore(__rat_offer_ptr, 448)
                for {
                    let __rat_i := 1
                } lt(__rat_i, 8) {
                    __rat_i := add(__rat_i, 1)
                } {
                    mstore(add(__rat_offer_ptr, mul(__rat_i, 32)), calldataload(add(offer_data_offset, mul(__rat_i, 32))))
                }
                mstore(add(__rat_offer_ptr, 256), add(448, __rat_padded_market_size))
                for {
                    let __rat_j := 9
                } lt(__rat_j, 14) {
                    __rat_j := add(__rat_j, 1)
                } {
                    mstore(add(__rat_offer_ptr, mul(__rat_j, 32)), calldataload(add(offer_data_offset, mul(__rat_j, 32))))
                }
                let __rat_market_ptr := add(__rat_offer_ptr, 448)
                mstore(__rat_market_ptr, calldataload(__rat_market_offset))
                mstore(add(__rat_market_ptr, 32), 192)
                for {
                    let __rat_m := 2
                } lt(__rat_m, 6) {
                    __rat_m := add(__rat_m, 1)
                } {
                    mstore(add(__rat_market_ptr, mul(__rat_m, 32)), calldataload(add(__rat_market_offset, mul(__rat_m, 32))))
                }
                mstore(add(__rat_market_ptr, 192), __rat_collateral_length)
                calldatacopy(add(__rat_market_ptr, 224), add(__rat_collateral_offset, 32), __rat_collateral_bytes)
                mstore(add(__rat_offer_ptr, add(448, __rat_padded_market_size)), __rat_callback_data_length)
                calldatacopy(add(__rat_offer_ptr, add(480, __rat_padded_market_size)), add(__rat_callback_data_offset, 32), __rat_callback_data_length)
                mstore(add(__rat_offer_ptr, add(480, add(__rat_padded_market_size, __rat_callback_data_length))), 0)
                mstore(__rat_data_ptr, ratifierData_length)
                calldatacopy(add(__rat_data_ptr, 32), ratifierData_data_offset, ratifierData_length)
                mstore(add(__rat_data_ptr, add(32, ratifierData_length)), 0)
                mstore(64, add(__rat_ptr, and(add(__rat_total, 31), not(31))))
                let __rat_success := staticcall(gas(), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10), __rat_ptr, __rat_total, __rat_ptr, 32)
                if iszero(__rat_success) {
                    let __rat_rds := returndatasize()
                    returndatacopy(0, 0, __rat_rds)
                    revert(0, __rat_rds)
                }
                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__rat_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                    mstore(0, shl(224, 0x9e8ec676))
                    revert(0, 4)
                }
            }
            if gt(units, 340282366920938463463374607431768211455) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let newConsumed := 0
            {
                let __ite_cond := gt(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0)
                if __ite_cond {
                    let offerPriceConsumed := internal_internal_tickToPrice(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5))
                    let timeToMaturityConsumed := 0
                    if gt(maturity, now) {
                        timeToMaturityConsumed := sub(maturity, now)
                    }
                    let settlementFeeConsumed := internal_internal_settlementFee(id, timeToMaturityConsumed)
                    let sellerPriceConsumed := offerPriceConsumed
                    if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                        sellerPriceConsumed := sub(offerPriceConsumed, settlementFeeConsumed)
                    }
                    let buyerPriceConsumed := add(sellerPriceConsumed, settlementFeeConsumed)
                    let buyerAssetsConsumed := 0
                    let sellerAssetsConsumed := 0
                    {
                        let __ite_cond_1 := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                        if __ite_cond_1 {
                            buyerAssetsConsumed := div(mul(units, buyerPriceConsumed), 1000000000000000000)
                            sellerAssetsConsumed := div(mul(units, sellerPriceConsumed), 1000000000000000000)
                        }
                        if iszero(__ite_cond_1) {
                            buyerAssetsConsumed := div(add(mul(units, buyerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                            sellerAssetsConsumed := div(add(mul(units, sellerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                        }
                    }
                    if lt(buyerAssetsConsumed, sellerAssetsConsumed) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 19)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                    let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                    let consumedIncrease := sellerAssetsConsumed
                    if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                        consumedIncrease := buyerAssetsConsumed
                    }
                    newConsumed := add(currentConsumed, consumedIncrease)
                    if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13)) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 16)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                    sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                }
                if iszero(__ite_cond) {
                    let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                    newConsumed := add(currentConsumed, units)
                    if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12)) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x436f6e73756d6564556e69747328290000000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 15)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                    sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                }
            }
            let buyer := taker
            let seller := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
            if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                buyer := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                seller := taker
            }
            let buyerCreditIncrease := internal_internal_updateBuyerForTake(id, buyer, units, maturity, now)
            let sellerCreditDecrease, sellerDebtIncrease, sellerPendingFeeDecrease := internal_internal_updateSellerForTake(id, seller, units, maturity, now)
            if iszero(or(iszero(iszero(iszero(gt(now, maturity)))), iszero(iszero(eq(sellerDebtIncrease, 0))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x43616e6e6f74496e63726561736544656274506f73744d617475726974792829)
                    let __err_hash := keccak256(__err_ptr, 32)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let reduceOnlyAllowed := 1
            if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 11) {
                {
                    let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                    if __ite_cond {
                        reduceOnlyAllowed := eq(buyerCreditIncrease, 0)
                    }
                    if iszero(__ite_cond) {
                        reduceOnlyAllowed := eq(sellerDebtIncrease, 0)
                    }
                }
            }
            if iszero(reduceOnlyAllowed) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4d616b65724372656469744f7244656274496e63726561736564282900000000)
                    let __err_hash := keccak256(__err_ptr, 28)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let buyerGateAllowed := 1
            if iszero(eq(enterGate, 0)) {
                if gt(buyerCreditIncrease, 0) {
                    let canIncreaseCredit := internal_internal_enterGateCanIncreaseCredit(enterGate, buyer)
                    buyerGateAllowed := iszero(eq(canIncreaseCredit, 0))
                }
            }
            if iszero(buyerGateAllowed) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4275796572476174656446726f6d496e6372656173696e674372656469742829)
                    let __err_hash := keccak256(__err_ptr, 32)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let sellerGateAllowed := 1
            if iszero(eq(enterGate, 0)) {
                if gt(sellerDebtIncrease, 0) {
                    let canIncreaseDebt := internal_internal_enterGateCanIncreaseDebt(enterGate, seller)
                    sellerGateAllowed := iszero(eq(canIncreaseDebt, 0))
                }
            }
            if iszero(sellerGateAllowed) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x53656c6c6572476174656446726f6d496e6372656173696e6744656274282900)
                    let __err_hash := keccak256(__err_ptr, 31)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let newTotalUnits := add(currentTotalUnits, buyerCreditIncrease)
            newTotalUnits := sub(newTotalUnits, sellerCreditDecrease)
            {
                let __compat_value := newTotalUnits
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(1, id))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let buyerAssets, sellerAssets, buyerPendingFeeIncrease := internal_internal_takeAssetsForPrice(id, units, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1), maturity, now, buyerCreditIncrease)
            if lt(buyerAssets, sellerAssets) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 19)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let claimableBefore := sload(mappingSlot(6, loanToken))
            sstore(mappingSlot(6, loanToken), add(claimableBefore, sub(buyerAssets, sellerAssets)))
            let receiver := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 9)
            if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                receiver := receiverIfTakerIsSeller
            }
            let payer := buyer
            {
                let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                if __ite_cond {
                }
                if iszero(__ite_cond) {
                    payer := sender
                }
            }
            let buyerCallback := takerCallback
            if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                buyerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x54616b6528616464726573732c627974657333322c75696e743235362c616464)
                mstore(add(__evt_ptr, 32), 0x726573732c616464726573732c626f6f6c2c627974657333322c75696e743235)
                mstore(add(__evt_ptr, 64), 0x362c75696e743235362c75696e743235362c75696e743235362c75696e743235)
                mstore(add(__evt_ptr, 96), 0x362c75696e743235362c75696e743235362c616464726573732c616464726573)
                mstore(add(__evt_ptr, 128), 0x7329000000000000000000000000000000000000000000000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 130)
                mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 32), units)
                mstore(add(__evt_ptr, 64), iszero(iszero(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1))))
                mstore(add(__evt_ptr, 96), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6))
                mstore(add(__evt_ptr, 128), buyerAssets)
                mstore(add(__evt_ptr, 160), sellerAssets)
                mstore(add(__evt_ptr, 192), newConsumed)
                mstore(add(__evt_ptr, 224), buyerPendingFeeIncrease)
                mstore(add(__evt_ptr, 256), sellerPendingFeeDecrease)
                mstore(add(__evt_ptr, 288), buyerCreditIncrease)
                mstore(add(__evt_ptr, 320), sellerCreditDecrease)
                mstore(add(__evt_ptr, 352), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 384), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                log4(__evt_ptr, 416, __evt_topic0, id, and(taker, 0xffffffffffffffffffffffffffffffffffffffff), and(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                let __liq_lock_ptr := mload(64)
                mstore(__liq_lock_ptr, id)
                mstore(add(__liq_lock_ptr, 32), seller)
                mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                tstore(__liq_lock_slot, add(tload(__liq_lock_slot), 1))
            }
            if iszero(eq(buyerCallback, 0)) {
                payer := buyerCallback
                {
                    let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                    if __ite_cond {
                        {
                            let __buycb_ptr := mload(64)
                            let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                            let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                            let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                            let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                            let __buycb_market_size := add(224, __buycb_collateral_bytes)
                            let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                            let __buycb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                            let __buycb_data_length := calldataload(__buycb_data_offset)
                            let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                            let __buycb_market_ptr := add(__buycb_ptr, 228)
                            let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                            let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                            mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                            mstore(add(__buycb_ptr, 4), id)
                            mstore(add(__buycb_ptr, 36), 224)
                            mstore(add(__buycb_ptr, 68), buyerAssets)
                            mstore(add(__buycb_ptr, 100), units)
                            mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                            mstore(add(__buycb_ptr, 164), buyer)
                            mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                            mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                            mstore(add(__buycb_market_ptr, 32), 192)
                            for {
                                let __buycb_m := 2
                            } lt(__buycb_m, 6) {
                                __buycb_m := add(__buycb_m, 1)
                            } {
                                mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                            }
                            mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                            calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                            mstore(__buycb_data_ptr, __buycb_data_length)
                            calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                            mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                            mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                            let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                            if iszero(__buycb_success) {
                                let __buycb_rds := returndatasize()
                                returndatacopy(0, 0, __buycb_rds)
                                revert(0, __buycb_rds)
                            }
                            if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                mstore(0, shl(224, 0xa8f3eb44))
                                revert(0, 4)
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        {
                            let __buycb_ptr := mload(64)
                            let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                            let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                            let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                            let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                            let __buycb_market_size := add(224, __buycb_collateral_bytes)
                            let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                            let __buycb_data_offset := sub(takerCallbackData_data_offset, 32)
                            let __buycb_data_length := calldataload(__buycb_data_offset)
                            let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                            let __buycb_market_ptr := add(__buycb_ptr, 228)
                            let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                            let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                            mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                            mstore(add(__buycb_ptr, 4), id)
                            mstore(add(__buycb_ptr, 36), 224)
                            mstore(add(__buycb_ptr, 68), buyerAssets)
                            mstore(add(__buycb_ptr, 100), units)
                            mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                            mstore(add(__buycb_ptr, 164), buyer)
                            mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                            mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                            mstore(add(__buycb_market_ptr, 32), 192)
                            for {
                                let __buycb_m := 2
                            } lt(__buycb_m, 6) {
                                __buycb_m := add(__buycb_m, 1)
                            } {
                                mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                            }
                            mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                            calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                            mstore(__buycb_data_ptr, __buycb_data_length)
                            calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                            mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                            mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                            let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                            if iszero(__buycb_success) {
                                let __buycb_rds := returndatasize()
                                returndatacopy(0, 0, __buycb_rds)
                                revert(0, __buycb_rds)
                            }
                            if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                mstore(0, shl(224, 0xa8f3eb44))
                                revert(0, 4)
                            }
                        }
                    }
                }
            }
            let feeAssets := sub(buyerAssets, sellerAssets)
            if gt(feeAssets, 0) {
                let self := address()
                {
                    if iszero(gt(extcodesize(loanToken), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), payer)
                    mstore(add(__lstf_ptr, 36), self)
                    mstore(add(__lstf_ptr, 68), feeAssets)
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
            }
            if gt(sellerAssets, 0) {
                {
                    if iszero(gt(extcodesize(loanToken), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), payer)
                    mstore(add(__lstf_ptr, 36), receiver)
                    mstore(add(__lstf_ptr, 68), sellerAssets)
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
            }
            let sellerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
            if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                sellerCallback := takerCallback
            }
            if iszero(eq(sellerCallback, 0)) {
                {
                    let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                    if __ite_cond {
                        {
                            let __sellcb_ptr := mload(64)
                            let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                            let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                            let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                            let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                            let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                            let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                            let __sellcb_data_offset := sub(takerCallbackData_data_offset, 32)
                            let __sellcb_data_length := calldataload(__sellcb_data_offset)
                            let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                            let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                            let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                            let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                            mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                            mstore(add(__sellcb_ptr, 4), id)
                            mstore(add(__sellcb_ptr, 36), 256)
                            mstore(add(__sellcb_ptr, 68), sellerAssets)
                            mstore(add(__sellcb_ptr, 100), units)
                            mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                            mstore(add(__sellcb_ptr, 164), seller)
                            mstore(add(__sellcb_ptr, 196), receiver)
                            mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                            mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                            mstore(add(__sellcb_market_ptr, 32), 192)
                            for {
                                let __sellcb_m := 2
                            } lt(__sellcb_m, 6) {
                                __sellcb_m := add(__sellcb_m, 1)
                            } {
                                mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                            }
                            mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                            calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                            mstore(__sellcb_data_ptr, __sellcb_data_length)
                            calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                            mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                            mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                            let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                            if iszero(__sellcb_success) {
                                let __sellcb_rds := returndatasize()
                                returndatacopy(0, 0, __sellcb_rds)
                                revert(0, __sellcb_rds)
                            }
                            if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                mstore(0, shl(224, 0xa4fb7883))
                                revert(0, 4)
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        {
                            let __sellcb_ptr := mload(64)
                            let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                            let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                            let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                            let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                            let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                            let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                            let __sellcb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                            let __sellcb_data_length := calldataload(__sellcb_data_offset)
                            let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                            let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                            let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                            let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                            mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                            mstore(add(__sellcb_ptr, 4), id)
                            mstore(add(__sellcb_ptr, 36), 256)
                            mstore(add(__sellcb_ptr, 68), sellerAssets)
                            mstore(add(__sellcb_ptr, 100), units)
                            mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                            mstore(add(__sellcb_ptr, 164), seller)
                            mstore(add(__sellcb_ptr, 196), receiver)
                            mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                            mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                            mstore(add(__sellcb_market_ptr, 32), 192)
                            for {
                                let __sellcb_m := 2
                            } lt(__sellcb_m, 6) {
                                __sellcb_m := add(__sellcb_m, 1)
                            } {
                                mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                            }
                            mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                            calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                            mstore(__sellcb_data_ptr, __sellcb_data_length)
                            calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                            mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                            mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                            let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                            if iszero(__sellcb_success) {
                                let __sellcb_rds := returndatasize()
                                returndatacopy(0, 0, __sellcb_rds)
                                revert(0, __sellcb_rds)
                            }
                            if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                mstore(0, shl(224, 0xa4fb7883))
                                revert(0, 4)
                            }
                        }
                    }
                }
            }
            let lockedAfterCallbacks := 0
            {
                let __liq_lock_ptr := mload(64)
                mstore(__liq_lock_ptr, id)
                mstore(add(__liq_lock_ptr, 32), seller)
                mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                let __liq_lock_depth := tload(__liq_lock_slot)
                if gt(__liq_lock_depth, 0) {
                    __liq_lock_depth := sub(__liq_lock_depth, 1)
                    tstore(__liq_lock_slot, __liq_lock_depth)
                }
                lockedAfterCallbacks := __liq_lock_depth
            }
            {
                let __ite_cond := iszero(eq(lockedAfterCallbacks, 0))
                if __ite_cond {
                }
                if iszero(__ite_cond) {
                    let sellerHealthy := internal_internal_isHealthy(add(offer_data_offset, calldataload(offer_data_offset)), id, seller)
                    if iszero(iszero(eq(sellerHealthy, 0))) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x53656c6c657249734c6971756964617461626c65282900000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 22)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                }
            }
            __ret0 := buyerAssets
            __ret1 := sellerAssets
            leave
        }
        function internal_internal_creditOf(id, user) -> __ret0 {
            let value := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_debtOf(id, user) -> __ret0 {
            let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_totalUnits(id) -> __ret0 {
            let value := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_lossFactor(id) -> __ret0 {
            let value := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_tickSpacing(id) -> __ret0 {
            let value := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
            __ret0 := value
            leave
        }
        function internal_internal_settlementFeeCbps(id) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6 {
            let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
            let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
            __ret0 := settlementFeeCbp0
            __ret1 := settlementFeeCbp1
            __ret2 := settlementFeeCbp2
            __ret3 := settlementFeeCbp3
            __ret4 := settlementFeeCbp4
            __ret5 := settlementFeeCbp5
            __ret6 := settlementFeeCbp6
            leave
        }
        function internal_internal_withdrawable(id) -> __ret0 {
            let value := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_continuousFee(id) -> __ret0 {
            let value := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
            __ret0 := value
            leave
        }
        function internal_internal_continuousFeeCredit(id) -> __ret0 {
            let value := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_withdraw(market_data_offset, units, onBehalf, receiver) {
            let sender := caller()
            let authorized := internal_internal_isAuthorized(onBehalf, sender)
            if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let id := internal_internal_toId(market_data_offset)
            let creditBeforeUpdate := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
            let pendingFeeBeforeUpdate := and(shr(128, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
            let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
            let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
            let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let now := timestamp()
            let postSlashCredit := 0
            if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                postSlashCredit := div(mul(creditBeforeUpdate, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
            }
            let postSlashPendingFee := 0
            if gt(creditBeforeUpdate, 0) {
                postSlashPendingFee := sub(pendingFeeBeforeUpdate, div(add(mul(pendingFeeBeforeUpdate, sub(creditBeforeUpdate, postSlashCredit)), sub(creditBeforeUpdate, 1)), creditBeforeUpdate))
            }
            let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
            if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                accrualEnd := now
            }
            let accrued := 0
            if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
            }
            let creditAfterUpdate := sub(postSlashCredit, accrued)
            let pendingFeeAfterUpdate := sub(postSlashPendingFee, accrued)
            {
                let __compat_value := creditAfterUpdate
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := marketLossFactor
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __compat_value := pendingFeeAfterUpdate
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            {
                let __compat_value := now
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := add(currentContinuousFeeCredit, accrued)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            let creditDecrease := sub(creditBeforeUpdate, creditAfterUpdate)
            let updatePendingFeeDecrease := sub(pendingFeeBeforeUpdate, pendingFeeAfterUpdate)
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 55)
                mstore(add(__evt_ptr, 0), creditDecrease)
                mstore(add(__evt_ptr, 32), updatePendingFeeDecrease)
                mstore(add(__evt_ptr, 64), accrued)
                log3(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            let pendingFeeDecrease := 0
            if gt(creditAfterUpdate, 0) {
                pendingFeeDecrease := div(add(mul(pendingFeeAfterUpdate, units), sub(creditAfterUpdate, 1)), creditAfterUpdate)
            }
            {
                let __compat_value := sub(pendingFeeAfterUpdate, pendingFeeDecrease)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
            }
            let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(credit, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(withdrawableAmount, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let total := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(total, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(mappingSlot(1, id))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x576974686472617728616464726573732c627974657333322c75696e74323536)
                mstore(add(__evt_ptr, 32), 0x2c616464726573732c616464726573732c75696e743235362900000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 57)
                mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 32), units)
                mstore(add(__evt_ptr, 64), pendingFeeDecrease)
                log4(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_ptr := mload(64)
                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lst_ptr, 4), receiver)
                mstore(add(__lst_ptr, 36), units)
                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                if iszero(__lst_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 17)
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 23)
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                        revert(0, 100)
                    }
                }
            }
            stop()
        }
        function internal_internal_repay(market_data_offset, units, onBehalf, callback, data_data_offset, data_length) {
            let sender := caller()
            let authorized := internal_internal_isAuthorized(onBehalf, sender)
            if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let id := internal_internal_toId(market_data_offset)
            let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
            {
                let __compat_value := sub(debt, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __compat_value := add(withdrawableAmount, units)
                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
            }
            let payer := sender
            if iszero(eq(callback, 0)) {
                payer := callback
                {
                    let __repaycb_ptr := mload(64)
                    let __repaycb_market_offset := add(4, calldataload(4))
                    let __repaycb_collateral_offset := add(__repaycb_market_offset, calldataload(add(__repaycb_market_offset, 32)))
                    let __repaycb_collateral_length := calldataload(__repaycb_collateral_offset)
                    let __repaycb_collateral_bytes := mul(__repaycb_collateral_length, 128)
                    let __repaycb_market_size := add(224, __repaycb_collateral_bytes)
                    let __repaycb_padded_market_size := and(add(__repaycb_market_size, 31), not(31))
                    let __repaycb_data_offset := sub(data_data_offset, 32)
                    let __repaycb_data_length := calldataload(__repaycb_data_offset)
                    let __repaycb_padded_data_len := and(add(__repaycb_data_length, 31), not(31))
                    let __repaycb_market_ptr := add(__repaycb_ptr, 164)
                    let __repaycb_data_ptr := add(__repaycb_market_ptr, __repaycb_padded_market_size)
                    let __repaycb_total := add(164, add(__repaycb_padded_market_size, add(32, __repaycb_padded_data_len)))
                    mstore(__repaycb_ptr, shl(224, 0xfc56f72e))
                    mstore(add(__repaycb_ptr, 4), id)
                    mstore(add(__repaycb_ptr, 36), 160)
                    mstore(add(__repaycb_ptr, 68), units)
                    mstore(add(__repaycb_ptr, 100), onBehalf)
                    mstore(add(__repaycb_ptr, 132), add(160, __repaycb_padded_market_size))
                    mstore(__repaycb_market_ptr, calldataload(__repaycb_market_offset))
                    mstore(add(__repaycb_market_ptr, 32), 192)
                    for {
                        let __repaycb_m := 2
                    } lt(__repaycb_m, 6) {
                        __repaycb_m := add(__repaycb_m, 1)
                    } {
                        mstore(add(__repaycb_market_ptr, mul(__repaycb_m, 32)), calldataload(add(__repaycb_market_offset, mul(__repaycb_m, 32))))
                    }
                    mstore(add(__repaycb_market_ptr, 192), __repaycb_collateral_length)
                    calldatacopy(add(__repaycb_market_ptr, 224), add(__repaycb_collateral_offset, 32), __repaycb_collateral_bytes)
                    mstore(__repaycb_data_ptr, __repaycb_data_length)
                    calldatacopy(add(__repaycb_data_ptr, 32), add(__repaycb_data_offset, 32), __repaycb_data_length)
                    mstore(add(__repaycb_data_ptr, add(32, __repaycb_data_length)), 0)
                    mstore(64, add(__repaycb_ptr, and(add(__repaycb_total, 31), not(31))))
                    let __repaycb_success := call(gas(), callback, 0, __repaycb_ptr, __repaycb_total, __repaycb_ptr, 32)
                    if iszero(__repaycb_success) {
                        let __repaycb_rds := returndatasize()
                        returndatacopy(0, 0, __repaycb_rds)
                        revert(0, __repaycb_rds)
                    }
                    if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__repaycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                        mstore(0, shl(224, 0x40a13da2))
                        revert(0, 4)
                    }
                }
            }
            let self := address()
            {
                if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lstf_ptr := mload(64)
                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lstf_ptr, 4), payer)
                mstore(add(__lstf_ptr, 36), self)
                mstore(add(__lstf_ptr, 68), units)
                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                if iszero(__lstf_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 21)
                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 27)
                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                        revert(0, 100)
                    }
                }
            }
            stop()
        }
        function internal_internal_collateralTokenAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
            __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 0)
            leave
        }
        function internal_internal_collateralLltvAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
            __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 1)
            leave
        }
        function internal_internal_collateralMaxLifAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
            __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 2)
            leave
        }
        function internal_internal_collateralOracleAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
            __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 3)
            leave
        }
        function internal_internal_oraclePrice(oracle) -> __ret0 {
            let price := 0
            {
                let __oracle_ptr := mload(64)
                mstore(__oracle_ptr, shl(224, 0xa035b1fe))
                mstore(64, add(__oracle_ptr, 32))
                let __oracle_success := staticcall(gas(), oracle, __oracle_ptr, 4, __oracle_ptr, 32)
                if iszero(__oracle_success) {
                    let __oracle_rds := returndatasize()
                    returndatacopy(0, 0, __oracle_rds)
                    revert(0, __oracle_rds)
                }
                if iszero(eq(returndatasize(), 32)) {
                    revert(0, 0)
                }
                price := mload(__oracle_ptr)
            }
            __ret0 := price
            leave
        }
        function internal_internal_liquidatorGateCanLiquidate(gate, account) -> __ret0 {
            let allowed := 0
            {
                let __ecwr_ptr := mload(64)
                mstore(__ecwr_ptr, shl(224, 0xb9f4ff55))
                mstore(add(__ecwr_ptr, 4), account)
                mstore(64, add(__ecwr_ptr, 64))
                let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                if iszero(__ecwr_success) {
                    let __ecwr_rds := returndatasize()
                    returndatacopy(0, 0, __ecwr_rds)
                    revert(0, __ecwr_rds)
                }
                if lt(returndatasize(), 32) {
                    revert(0, 0)
                }
                allowed := mload(__ecwr_ptr)
            }
            __ret0 := allowed
            leave
        }
        function internal_internal_liquidatorGateCanLiquidateOrDefault(gate, account) -> __ret0 {
            let allowed := 1
            if iszero(eq(gate, 0)) {
                let loaded := internal_internal_liquidatorGateCanLiquidate(gate, account)
                allowed := loaded
            }
            __ret0 := allowed
            leave
        }
        function internal_internal_liquidationLockedValue(id, user) -> __ret0 {
            let locked := 0
            {
                let __liq_lock_ptr := mload(64)
                mstore(__liq_lock_ptr, id)
                mstore(add(__liq_lock_ptr, 32), user)
                mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                locked := tload(keccak256(__liq_lock_ptr, 96))
            }
            __ret0 := locked
            leave
        }
        function internal_internal_liquidationLockExchange(id, user, value) -> __ret0 {
            let previous := 0
            {
                let __liq_lock_ptr := mload(64)
                mstore(__liq_lock_ptr, id)
                mstore(add(__liq_lock_ptr, 32), user)
                mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                previous := tload(__liq_lock_slot)
                tstore(__liq_lock_slot, value)
            }
            __ret0 := previous
            leave
        }
        function internal_internal_liquidationLocked(id, user) -> __ret0 {
            let locked := internal_internal_liquidationLockedValue(id, user)
            __ret0 := iszero(eq(locked, 0))
            leave
        }
        function internal_internal_collateralAmount(id, user, index) -> __ret0 {
            let value := 0
            if eq(index, 0) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 3))
                value := loaded
            }
            if eq(index, 1) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 4))
                value := loaded
            }
            if eq(index, 2) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 5))
                value := loaded
            }
            if eq(index, 3) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 6))
                value := loaded
            }
            if eq(index, 4) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 7))
                value := loaded
            }
            if eq(index, 5) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 8))
                value := loaded
            }
            if eq(index, 6) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 9))
                value := loaded
            }
            if eq(index, 7) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 10))
                value := loaded
            }
            if eq(index, 8) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 11))
                value := loaded
            }
            if eq(index, 9) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 12))
                value := loaded
            }
            if eq(index, 10) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 13))
                value := loaded
            }
            if eq(index, 11) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 14))
                value := loaded
            }
            if eq(index, 12) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 15))
                value := loaded
            }
            if eq(index, 13) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 16))
                value := loaded
            }
            if eq(index, 14) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 17))
                value := loaded
            }
            if eq(index, 15) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 18))
                value := loaded
            }
            if eq(index, 16) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 19))
                value := loaded
            }
            if eq(index, 17) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 20))
                value := loaded
            }
            if eq(index, 18) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 21))
                value := loaded
            }
            if eq(index, 19) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 22))
                value := loaded
            }
            if eq(index, 20) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 23))
                value := loaded
            }
            if eq(index, 21) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 24))
                value := loaded
            }
            if eq(index, 22) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 25))
                value := loaded
            }
            if eq(index, 23) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 26))
                value := loaded
            }
            if eq(index, 24) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 27))
                value := loaded
            }
            if eq(index, 25) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 28))
                value := loaded
            }
            if eq(index, 26) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 29))
                value := loaded
            }
            if eq(index, 27) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 30))
                value := loaded
            }
            if eq(index, 28) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 31))
                value := loaded
            }
            if eq(index, 29) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 32))
                value := loaded
            }
            if eq(index, 30) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 33))
                value := loaded
            }
            if eq(index, 31) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 34))
                value := loaded
            }
            if eq(index, 32) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 35))
                value := loaded
            }
            if eq(index, 33) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 36))
                value := loaded
            }
            if eq(index, 34) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 37))
                value := loaded
            }
            if eq(index, 35) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 38))
                value := loaded
            }
            if eq(index, 36) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 39))
                value := loaded
            }
            if eq(index, 37) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 40))
                value := loaded
            }
            if eq(index, 38) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 41))
                value := loaded
            }
            if eq(index, 39) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 42))
                value := loaded
            }
            if eq(index, 40) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 43))
                value := loaded
            }
            if eq(index, 41) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 44))
                value := loaded
            }
            if eq(index, 42) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 45))
                value := loaded
            }
            if eq(index, 43) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 46))
                value := loaded
            }
            if eq(index, 44) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 47))
                value := loaded
            }
            if eq(index, 45) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 48))
                value := loaded
            }
            if eq(index, 46) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 49))
                value := loaded
            }
            if eq(index, 47) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 50))
                value := loaded
            }
            if eq(index, 48) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 51))
                value := loaded
            }
            if eq(index, 49) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 52))
                value := loaded
            }
            if eq(index, 50) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 53))
                value := loaded
            }
            if eq(index, 51) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 54))
                value := loaded
            }
            if eq(index, 52) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 55))
                value := loaded
            }
            if eq(index, 53) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 56))
                value := loaded
            }
            if eq(index, 54) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 57))
                value := loaded
            }
            if eq(index, 55) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 58))
                value := loaded
            }
            if eq(index, 56) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 59))
                value := loaded
            }
            if eq(index, 57) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 60))
                value := loaded
            }
            if eq(index, 58) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 61))
                value := loaded
            }
            if eq(index, 59) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 62))
                value := loaded
            }
            if eq(index, 60) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 63))
                value := loaded
            }
            if eq(index, 61) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 64))
                value := loaded
            }
            if eq(index, 62) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 65))
                value := loaded
            }
            if eq(index, 63) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 66))
                value := loaded
            }
            if eq(index, 64) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 67))
                value := loaded
            }
            if eq(index, 65) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 68))
                value := loaded
            }
            if eq(index, 66) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 69))
                value := loaded
            }
            if eq(index, 67) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 70))
                value := loaded
            }
            if eq(index, 68) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 71))
                value := loaded
            }
            if eq(index, 69) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 72))
                value := loaded
            }
            if eq(index, 70) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 73))
                value := loaded
            }
            if eq(index, 71) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 74))
                value := loaded
            }
            if eq(index, 72) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 75))
                value := loaded
            }
            if eq(index, 73) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 76))
                value := loaded
            }
            if eq(index, 74) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 77))
                value := loaded
            }
            if eq(index, 75) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 78))
                value := loaded
            }
            if eq(index, 76) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 79))
                value := loaded
            }
            if eq(index, 77) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 80))
                value := loaded
            }
            if eq(index, 78) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 81))
                value := loaded
            }
            if eq(index, 79) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 82))
                value := loaded
            }
            if eq(index, 80) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 83))
                value := loaded
            }
            if eq(index, 81) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 84))
                value := loaded
            }
            if eq(index, 82) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 85))
                value := loaded
            }
            if eq(index, 83) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 86))
                value := loaded
            }
            if eq(index, 84) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 87))
                value := loaded
            }
            if eq(index, 85) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 88))
                value := loaded
            }
            if eq(index, 86) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 89))
                value := loaded
            }
            if eq(index, 87) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 90))
                value := loaded
            }
            if eq(index, 88) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 91))
                value := loaded
            }
            if eq(index, 89) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 92))
                value := loaded
            }
            if eq(index, 90) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 93))
                value := loaded
            }
            if eq(index, 91) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 94))
                value := loaded
            }
            if eq(index, 92) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 95))
                value := loaded
            }
            if eq(index, 93) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 96))
                value := loaded
            }
            if eq(index, 94) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 97))
                value := loaded
            }
            if eq(index, 95) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 98))
                value := loaded
            }
            if eq(index, 96) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 99))
                value := loaded
            }
            if eq(index, 97) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 100))
                value := loaded
            }
            if eq(index, 98) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 101))
                value := loaded
            }
            if eq(index, 99) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 102))
                value := loaded
            }
            if eq(index, 100) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 103))
                value := loaded
            }
            if eq(index, 101) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 104))
                value := loaded
            }
            if eq(index, 102) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 105))
                value := loaded
            }
            if eq(index, 103) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 106))
                value := loaded
            }
            if eq(index, 104) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 107))
                value := loaded
            }
            if eq(index, 105) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 108))
                value := loaded
            }
            if eq(index, 106) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 109))
                value := loaded
            }
            if eq(index, 107) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 110))
                value := loaded
            }
            if eq(index, 108) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 111))
                value := loaded
            }
            if eq(index, 109) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 112))
                value := loaded
            }
            if eq(index, 110) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 113))
                value := loaded
            }
            if eq(index, 111) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 114))
                value := loaded
            }
            if eq(index, 112) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 115))
                value := loaded
            }
            if eq(index, 113) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 116))
                value := loaded
            }
            if eq(index, 114) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 117))
                value := loaded
            }
            if eq(index, 115) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 118))
                value := loaded
            }
            if eq(index, 116) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 119))
                value := loaded
            }
            if eq(index, 117) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 120))
                value := loaded
            }
            if eq(index, 118) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 121))
                value := loaded
            }
            if eq(index, 119) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 122))
                value := loaded
            }
            if eq(index, 120) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 123))
                value := loaded
            }
            if eq(index, 121) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 124))
                value := loaded
            }
            if eq(index, 122) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 125))
                value := loaded
            }
            if eq(index, 123) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 126))
                value := loaded
            }
            if eq(index, 124) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 127))
                value := loaded
            }
            if eq(index, 125) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 128))
                value := loaded
            }
            if eq(index, 126) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 129))
                value := loaded
            }
            if eq(index, 127) {
                let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 130))
                value := loaded
            }
            __ret0 := value
            leave
        }
        function internal_internal_liquidationDebtSnapshot(id, borrower, collateralCount, collateralBitmapValue, collateralParamsOffset, originalDebt) -> __ret0, __ret1 {
            let maxDebtValue := 0
            let badDebt := originalDebt
            for {
                let __forEach_idx := 0
                let __forEach_count := collateralCount
                let i := 0
            } lt(__forEach_idx, __forEach_count) {
                __forEach_idx := add(__forEach_idx, 1)
            } {
                i := __forEach_idx
                let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                if active {
                    let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                    let collateralParamOffset := add(add(collateralParamsOffset, 32), mul(i, 128))
                    let oracle := calldataload(add(collateralParamOffset, 96))
                    let price := internal_internal_oraclePrice(oracle)
                    let lltv := calldataload(add(collateralParamOffset, 32))
                    let maxLifValue := calldataload(add(collateralParamOffset, 64))
                    let collateralDebtValue := div(mul(div(mul(activeCollateral, price), 1000000000000000000000000000000000000), lltv), 1000000000000000000)
                    maxDebtValue := add(maxDebtValue, collateralDebtValue)
                    let repayable := div(add(mul(div(add(mul(activeCollateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(maxLifValue, 1)), maxLifValue)
                    {
                        let __ite_cond := gt(badDebt, repayable)
                        if __ite_cond {
                            badDebt := sub(badDebt, repayable)
                        }
                        if iszero(__ite_cond) {
                            badDebt := 0
                        }
                    }
                }
            }
            __ret0 := maxDebtValue
            __ret1 := badDebt
            leave
        }
        function internal_internal_writeCollateralAmount(id, user, index, value) -> __ret0 {
            if eq(index, 0) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 3), value)
            }
            if eq(index, 1) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 4), value)
            }
            if eq(index, 2) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 5), value)
            }
            if eq(index, 3) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 6), value)
            }
            if eq(index, 4) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 7), value)
            }
            if eq(index, 5) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 8), value)
            }
            if eq(index, 6) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 9), value)
            }
            if eq(index, 7) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 10), value)
            }
            if eq(index, 8) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 11), value)
            }
            if eq(index, 9) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 12), value)
            }
            if eq(index, 10) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 13), value)
            }
            if eq(index, 11) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 14), value)
            }
            if eq(index, 12) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 15), value)
            }
            if eq(index, 13) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 16), value)
            }
            if eq(index, 14) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 17), value)
            }
            if eq(index, 15) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 18), value)
            }
            if eq(index, 16) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 19), value)
            }
            if eq(index, 17) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 20), value)
            }
            if eq(index, 18) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 21), value)
            }
            if eq(index, 19) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 22), value)
            }
            if eq(index, 20) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 23), value)
            }
            if eq(index, 21) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 24), value)
            }
            if eq(index, 22) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 25), value)
            }
            if eq(index, 23) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 26), value)
            }
            if eq(index, 24) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 27), value)
            }
            if eq(index, 25) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 28), value)
            }
            if eq(index, 26) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 29), value)
            }
            if eq(index, 27) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 30), value)
            }
            if eq(index, 28) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 31), value)
            }
            if eq(index, 29) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 32), value)
            }
            if eq(index, 30) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 33), value)
            }
            if eq(index, 31) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 34), value)
            }
            if eq(index, 32) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 35), value)
            }
            if eq(index, 33) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 36), value)
            }
            if eq(index, 34) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 37), value)
            }
            if eq(index, 35) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 38), value)
            }
            if eq(index, 36) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 39), value)
            }
            if eq(index, 37) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 40), value)
            }
            if eq(index, 38) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 41), value)
            }
            if eq(index, 39) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 42), value)
            }
            if eq(index, 40) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 43), value)
            }
            if eq(index, 41) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 44), value)
            }
            if eq(index, 42) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 45), value)
            }
            if eq(index, 43) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 46), value)
            }
            if eq(index, 44) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 47), value)
            }
            if eq(index, 45) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 48), value)
            }
            if eq(index, 46) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 49), value)
            }
            if eq(index, 47) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 50), value)
            }
            if eq(index, 48) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 51), value)
            }
            if eq(index, 49) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 52), value)
            }
            if eq(index, 50) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 53), value)
            }
            if eq(index, 51) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 54), value)
            }
            if eq(index, 52) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 55), value)
            }
            if eq(index, 53) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 56), value)
            }
            if eq(index, 54) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 57), value)
            }
            if eq(index, 55) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 58), value)
            }
            if eq(index, 56) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 59), value)
            }
            if eq(index, 57) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 60), value)
            }
            if eq(index, 58) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 61), value)
            }
            if eq(index, 59) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 62), value)
            }
            if eq(index, 60) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 63), value)
            }
            if eq(index, 61) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 64), value)
            }
            if eq(index, 62) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 65), value)
            }
            if eq(index, 63) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 66), value)
            }
            if eq(index, 64) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 67), value)
            }
            if eq(index, 65) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 68), value)
            }
            if eq(index, 66) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 69), value)
            }
            if eq(index, 67) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 70), value)
            }
            if eq(index, 68) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 71), value)
            }
            if eq(index, 69) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 72), value)
            }
            if eq(index, 70) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 73), value)
            }
            if eq(index, 71) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 74), value)
            }
            if eq(index, 72) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 75), value)
            }
            if eq(index, 73) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 76), value)
            }
            if eq(index, 74) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 77), value)
            }
            if eq(index, 75) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 78), value)
            }
            if eq(index, 76) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 79), value)
            }
            if eq(index, 77) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 80), value)
            }
            if eq(index, 78) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 81), value)
            }
            if eq(index, 79) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 82), value)
            }
            if eq(index, 80) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 83), value)
            }
            if eq(index, 81) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 84), value)
            }
            if eq(index, 82) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 85), value)
            }
            if eq(index, 83) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 86), value)
            }
            if eq(index, 84) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 87), value)
            }
            if eq(index, 85) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 88), value)
            }
            if eq(index, 86) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 89), value)
            }
            if eq(index, 87) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 90), value)
            }
            if eq(index, 88) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 91), value)
            }
            if eq(index, 89) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 92), value)
            }
            if eq(index, 90) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 93), value)
            }
            if eq(index, 91) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 94), value)
            }
            if eq(index, 92) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 95), value)
            }
            if eq(index, 93) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 96), value)
            }
            if eq(index, 94) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 97), value)
            }
            if eq(index, 95) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 98), value)
            }
            if eq(index, 96) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 99), value)
            }
            if eq(index, 97) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 100), value)
            }
            if eq(index, 98) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 101), value)
            }
            if eq(index, 99) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 102), value)
            }
            if eq(index, 100) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 103), value)
            }
            if eq(index, 101) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 104), value)
            }
            if eq(index, 102) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 105), value)
            }
            if eq(index, 103) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 106), value)
            }
            if eq(index, 104) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 107), value)
            }
            if eq(index, 105) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 108), value)
            }
            if eq(index, 106) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 109), value)
            }
            if eq(index, 107) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 110), value)
            }
            if eq(index, 108) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 111), value)
            }
            if eq(index, 109) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 112), value)
            }
            if eq(index, 110) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 113), value)
            }
            if eq(index, 111) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 114), value)
            }
            if eq(index, 112) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 115), value)
            }
            if eq(index, 113) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 116), value)
            }
            if eq(index, 114) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 117), value)
            }
            if eq(index, 115) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 118), value)
            }
            if eq(index, 116) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 119), value)
            }
            if eq(index, 117) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 120), value)
            }
            if eq(index, 118) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 121), value)
            }
            if eq(index, 119) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 122), value)
            }
            if eq(index, 120) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 123), value)
            }
            if eq(index, 121) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 124), value)
            }
            if eq(index, 122) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 125), value)
            }
            if eq(index, 123) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 126), value)
            }
            if eq(index, 124) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 127), value)
            }
            if eq(index, 125) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 128), value)
            }
            if eq(index, 126) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 129), value)
            }
            if eq(index, 127) {
                sstore(add(mappingSlot(mappingSlot(0, id), user), 130), value)
            }
            __ret0 := 0
            leave
        }
        function internal_internal_liquidate(market_data_offset, collateralIndex, seizedAssets, repaidUnits, borrower, postMaturityMode, receiver, callback, data_data_offset, data_length) -> __ret0, __ret1 {
            let sender := caller()
            let id := internal_internal_toId(market_data_offset)
            let debtLoaded := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
            let debt := debtLoaded
            let totalUnitsValue := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            if iszero(or(iszero(iszero(eq(seizedAssets, 0))), iszero(iszero(eq(repaidUnits, 0))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 19)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            if iszero(gt(debt, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4e6f74426f72726f776572282900000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 13)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let canLiquidate := internal_internal_liquidatorGateCanLiquidateOrDefault(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), sender)
            if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), 0))), iszero(iszero(iszero(eq(canLiquidate, 0)))))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4c697175696461746f72476174656446726f6d4c69717569646174696e672829)
                    let __err_hash := keccak256(__err_ptr, 32)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
            {
                let __ite_cond := lt(collateralIndex, collateralCount)
                if __ite_cond {
                }
                if iszero(__ite_cond) {
                    mstore(0, shl(224, 1313373041))
                    mstore(4, 50)
                    revert(0, 36)
                }
            }
            let _collateralIndexBoundsCheck := internal_internal_collateralMaxLifAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
            let marketDataOffset := add(calldataload(4), 4)
            let collateralParamsOffset := add(marketDataOffset, calldataload(add(marketDataOffset, 32)))
            let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
            let collateralActive := internal_internal_collateralBitmapIsSet(collateralBitmapValue, collateralIndex)
            if or(iszero(iszero(gt(seizedAssets, 0))), iszero(iszero(gt(repaidUnits, 0)))) {
                if iszero(collateralActive) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 19)
                    mstore(68, 0x696e61637469766520636f6c6c61746572616c00000000000000000000000000)
                    revert(0, 100)
                }
            }
            let originalDebt := debt
            let maxDebtValue, badDebt := internal_internal_liquidationDebtSnapshot(id, borrower, collateralCount, collateralBitmapValue, collateralParamsOffset, originalDebt)
            let selectedCollateralParamOffsetForPrice := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
            let selectedOracle := calldataload(add(selectedCollateralParamOffsetForPrice, 96))
            let liquidatedCollatPrice := internal_internal_oraclePrice(selectedOracle)
            let now := timestamp()
            let lockedBeforeLiquidation := internal_internal_liquidationLockedValue(id, borrower)
            if iszero(eq(lockedBeforeLiquidation, 0)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 17)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            {
                let __ite_cond := postMaturityMode
                if __ite_cond {
                    if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 17)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                }
                if iszero(__ite_cond) {
                    if iszero(gt(originalDebt, maxDebtValue)) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 17)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                }
            }
            if gt(badDebt, 0) {
                {
                    let __compat_value := sub(debt, badDebt)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                debt := sub(debt, badDebt)
                let oldLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnitsValue, badDebt)), totalUnitsValue))
                {
                    let __compat_value := newLossFactor
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(1, id))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := sub(totalUnitsValue, badDebt)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(1, id))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let oldContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                let newContinuousFeeCredit := 0
                if lt(oldLossFactor, 340282366920938463463374607431768211455) {
                    newContinuousFeeCredit := div(mul(oldContinuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                }
                {
                    let __compat_value := newContinuousFeeCredit
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
            }
            let outSeizedAssets := seizedAssets
            let outRepaidUnits := repaidUnits
            if or(iszero(iszero(gt(outRepaidUnits, 0))), iszero(iszero(gt(outSeizedAssets, 0)))) {
                let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                let maxLifValue := calldataload(add(selectedCollateralParamOffset, 64))
                let lif := maxLifValue
                if postMaturityMode {
                    let elapsed := sub(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))
                    let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLifValue, 1000000000000000000), elapsed), 900))
                    {
                        let __ite_cond := iszero(gt(maxLifValue, postMaturityLif))
                        if __ite_cond {
                            lif := maxLifValue
                        }
                        if iszero(__ite_cond) {
                            lif := postMaturityLif
                        }
                    }
                }
                {
                    let __ite_cond := gt(outSeizedAssets, 0)
                    if __ite_cond {
                        outRepaidUnits := div(add(mul(div(add(mul(outSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                    }
                    if iszero(__ite_cond) {
                        outSeizedAssets := div(mul(div(mul(outRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                    }
                }
                {
                    let __ite_cond := postMaturityMode
                    if __ite_cond {
                    }
                    if iszero(__ite_cond) {
                        let lltv := calldataload(add(selectedCollateralParamOffset, 32))
                        let maxRepaidValue := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                        if lt(lltv, 1000000000000000000) {
                            maxRepaidValue := div(add(mul(sub(debt, maxDebtValue), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                        }
                        let oldCollateralForRcf := internal_internal_collateralAmount(id, borrower, collateralIndex)
                        let collateralRepayCapacity := div(mul(div(mul(oldCollateralForRcf, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                        let capacityShortfall := 0
                        if gt(collateralRepayCapacity, maxRepaidValue) {
                            capacityShortfall := sub(collateralRepayCapacity, maxRepaidValue)
                        }
                        if iszero(or(iszero(iszero(iszero(gt(outRepaidUnits, maxRepaidValue)))), iszero(iszero(lt(capacityShortfall, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 3)))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x5265636f76657279436c6f7365466163746f72436f6e646974696f6e7356696f)
                                mstore(add(__err_ptr, 32), 0x6c61746564282900000000000000000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 39)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                    }
                }
                let oldCollateral := internal_internal_collateralAmount(id, borrower, collateralIndex)
                if lt(oldCollateral, outSeizedAssets) {
                    mstore(0, shl(224, 1313373041))
                    mstore(4, 17)
                    revert(0, 36)
                }
                let newCollateral := sub(oldCollateral, outSeizedAssets)
                let _writeOk := internal_internal_writeCollateralAmount(id, borrower, collateralIndex, newCollateral)
                if eq(newCollateral, 0) {
                    if gt(outSeizedAssets, 0) {
                        let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                        let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                        {
                            let __compat_value := newBitmap
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                    }
                }
                let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(withdrawableAmount, outRepaidUnits)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                if lt(debt, outRepaidUnits) {
                    mstore(0, shl(224, 1313373041))
                    mstore(4, 17)
                    revert(0, 36)
                }
                let newDebtAfterRepay := sub(debt, outRepaidUnits)
                {
                    let __compat_value := newDebtAfterRepay
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
            }
            let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
            let collateralToken := calldataload(selectedCollateralParamOffset)
            let payer := sender
            if iszero(eq(callback, 0)) {
                payer := callback
            }
            let latestLossFactorLoaded := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
            let latestContinuousFeeCreditLoaded := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
            {
                let __evt_ptr := mload(64)
                mstore(add(__evt_ptr, 0), 0x4c697175696461746528616464726573732c627974657333322c616464726573)
                mstore(add(__evt_ptr, 32), 0x732c75696e743235362c75696e743235362c616464726573732c626f6f6c2c61)
                mstore(add(__evt_ptr, 64), 0x6464726573732c616464726573732c75696e743235362c75696e743235362c75)
                mstore(add(__evt_ptr, 96), 0x696e743235362900000000000000000000000000000000000000000000000000)
                let __evt_topic0 := keccak256(__evt_ptr, 103)
                mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 32), outSeizedAssets)
                mstore(add(__evt_ptr, 64), outRepaidUnits)
                mstore(add(__evt_ptr, 96), iszero(iszero(postMaturityMode)))
                mstore(add(__evt_ptr, 128), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 160), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                mstore(add(__evt_ptr, 192), badDebt)
                mstore(add(__evt_ptr, 224), add(latestLossFactorLoaded, 0))
                mstore(add(__evt_ptr, 256), add(latestContinuousFeeCreditLoaded, 0))
                log4(__evt_ptr, 288, __evt_topic0, id, and(collateralToken, 0xffffffffffffffffffffffffffffffffffffffff), and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
            }
            {
                if iszero(gt(extcodesize(collateralToken), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_ptr := mload(64)
                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lst_ptr, 4), receiver)
                mstore(add(__lst_ptr, 36), outSeizedAssets)
                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                if iszero(__lst_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 17)
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 23)
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                        revert(0, 100)
                    }
                }
            }
            if iszero(eq(callback, 0)) {
                {
                    let __liqcb_ptr := mload(64)
                    let __liqcb_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                    let __liqcb_collateral_length := calldataload(__liqcb_collateral_offset)
                    let __liqcb_collateral_bytes := mul(__liqcb_collateral_length, 128)
                    let __liqcb_market_size := add(224, __liqcb_collateral_bytes)
                    let __liqcb_padded_market_size := and(add(__liqcb_market_size, 31), not(31))
                    let __liqcb_padded_data_len := and(add(data_length, 31), not(31))
                    let __liqcb_market_ptr := add(__liqcb_ptr, 324)
                    let __liqcb_data_ptr := add(__liqcb_market_ptr, __liqcb_padded_market_size)
                    let __liqcb_total := add(324, add(__liqcb_padded_market_size, add(32, __liqcb_padded_data_len)))
                    mstore(__liqcb_ptr, shl(224, 0x6861b795))
                    mstore(add(__liqcb_ptr, 4), sender)
                    mstore(add(__liqcb_ptr, 36), id)
                    mstore(add(__liqcb_ptr, 68), 320)
                    mstore(add(__liqcb_ptr, 100), collateralIndex)
                    mstore(add(__liqcb_ptr, 132), outSeizedAssets)
                    mstore(add(__liqcb_ptr, 164), outRepaidUnits)
                    mstore(add(__liqcb_ptr, 196), borrower)
                    mstore(add(__liqcb_ptr, 228), receiver)
                    mstore(add(__liqcb_ptr, 260), add(320, __liqcb_padded_market_size))
                    mstore(add(__liqcb_ptr, 292), badDebt)
                    mstore(__liqcb_market_ptr, calldataload(market_data_offset))
                    mstore(add(__liqcb_market_ptr, 32), 192)
                    mstore(add(__liqcb_market_ptr, 64), calldataload(add(market_data_offset, 64)))
                    mstore(add(__liqcb_market_ptr, 96), calldataload(add(market_data_offset, 96)))
                    mstore(add(__liqcb_market_ptr, 128), calldataload(add(market_data_offset, 128)))
                    mstore(add(__liqcb_market_ptr, 160), calldataload(add(market_data_offset, 160)))
                    mstore(add(__liqcb_market_ptr, 192), __liqcb_collateral_length)
                    calldatacopy(add(__liqcb_market_ptr, 224), add(__liqcb_collateral_offset, 32), __liqcb_collateral_bytes)
                    mstore(__liqcb_data_ptr, data_length)
                    calldatacopy(add(__liqcb_data_ptr, 32), data_data_offset, data_length)
                    mstore(64, add(__liqcb_ptr, and(add(__liqcb_total, 31), not(31))))
                    let __liqcb_success := call(gas(), callback, 0, __liqcb_ptr, __liqcb_total, __liqcb_ptr, 32)
                    if iszero(__liqcb_success) {
                        let __liqcb_rds := returndatasize()
                        returndatacopy(0, 0, __liqcb_rds)
                        revert(0, __liqcb_rds)
                    }
                    if lt(returndatasize(), 32) {
                        mstore(0, shl(224, 0x70b53d4b))
                        revert(0, 4)
                    }
                    if iszero(eq(mload(__liqcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                        mstore(0, shl(224, 0x70b53d4b))
                        revert(0, 4)
                    }
                }
            }
            let self := address()
            {
                if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lstf_ptr := mload(64)
                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lstf_ptr, 4), payer)
                mstore(add(__lstf_ptr, 36), self)
                mstore(add(__lstf_ptr, 68), outRepaidUnits)
                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                if iszero(__lstf_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 21)
                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 27)
                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                        revert(0, 100)
                    }
                }
            }
            __ret0 := outSeizedAssets
            __ret1 := outRepaidUnits
            leave
        }
        function internal_internal_isHealthy(market_data_offset, id, borrower) -> __ret0 {
            let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
            if eq(debt, 0) {
                __ret0 := 1
                leave
            }
            let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
            let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
            let maxDebt := 0
            for {
                let __forEach_idx := 0
                let __forEach_count := collateralCount
                let i := 0
            } lt(__forEach_idx, __forEach_count) {
                __forEach_idx := add(__forEach_idx, 1)
            } {
                i := __forEach_idx
                let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                if active {
                    let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                    let oracle := internal_internal_collateralOracleAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                    let price := internal_internal_oraclePrice(oracle)
                    let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                    let collateralValue := div(mul(activeCollateral, price), 1000000000000000000000000000000000000)
                    maxDebt := add(maxDebt, div(mul(collateralValue, lltv), 1000000000000000000))
                }
            }
            __ret0 := iszero(gt(debt, maxDebt))
            leave
        }
        function internal_internal_setCollateralAmount(id, user, index, value) {
            let _writeOk := internal_internal_writeCollateralAmount(id, user, index, value)
            stop()
        }
        function internal_internal_supplyCollateral(market_data_offset, collateralIndex, assets, onBehalf) {
            let sender := caller()
            let authorized := internal_internal_isAuthorized(onBehalf, sender)
            if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let id := internal_internal_toId(market_data_offset)
            let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
            let newCollateral := add(oldCollateral, assets)
            if gt(newCollateral, 340282366920938463463374607431768211455) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
            if eq(oldCollateral, 0) {
                if gt(assets, 0) {
                    let newBitmap := internal_internal_collateralBitmapSetBit(oldBitmap, collateralIndex)
                    {
                        let __compat_value := newBitmap
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    let activeCount := internal_internal_countBits128(newBitmap)
                    if gt(activeCount, 16) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x546f6f4d616e79416374697661746564436f6c6c61746572616c732829000000)
                            let __err_hash := keccak256(__err_ptr, 29)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                }
            }
            let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
            let self := address()
            {
                if iszero(gt(extcodesize(collateralToken), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lstf_ptr := mload(64)
                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lstf_ptr, 4), sender)
                mstore(add(__lstf_ptr, 36), self)
                mstore(add(__lstf_ptr, 68), assets)
                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                let __lstf_success := call(gas(), collateralToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                if iszero(__lstf_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 21)
                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 27)
                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                        revert(0, 100)
                    }
                }
            }
            internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
            stop()
        }
        function internal_internal_withdrawCollateral(market_data_offset, collateralIndex, assets, onBehalf, receiver) {
            let sender := caller()
            let authorized := internal_internal_isAuthorized(onBehalf, sender)
            if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 14)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            let id := internal_internal_toId(market_data_offset)
            let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
            let newCollateral := sub(oldCollateral, assets)
            let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
            if gt(debt, 0) {
                let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                let requiredCollateral := div(add(mul(debt, 1000000000000000000), sub(lltv, 1)), lltv)
                if lt(newCollateral, requiredCollateral) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e6865616c746879426f72726f776572282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
            }
            if eq(newCollateral, 0) {
                if gt(assets, 0) {
                    let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                    let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                    {
                        let __compat_value := newBitmap
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                }
            }
            let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
            {
                if iszero(gt(extcodesize(collateralToken), 0)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 7)
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_ptr := mload(64)
                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                mstore(add(__lst_ptr, 4), receiver)
                mstore(add(__lst_ptr, 36), assets)
                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                if iszero(__lst_success) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 17)
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                    revert(0, 100)
                }
                let __lst_rds := returndatasize()
                if __lst_rds {
                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 23)
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                        revert(0, 100)
                    }
                }
            }
            internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
            stop()
        }
        function internal_internal_flashLoan(tokens_data_offset, tokens_length, assets_data_offset, assets_length, callback, data_data_offset, data_length) {
            let tokenCount := tokens_length
            let assetCount := assets_length
            if iszero(eq(tokenCount, assetCount)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 19)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            for {
                let __forEach_idx := 0
                let __forEach_count := tokenCount
                let i := 0
            } lt(__forEach_idx, __forEach_count) {
                __forEach_idx := add(__forEach_idx, 1)
            } {
                i := __forEach_idx
                {
                    if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), callback)
                    mstore(add(__lst_ptr, 36), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
            }
            let sender := caller()
            {
                let __flcb_ptr := mload(64)
                let __flcb_tokens_bytes := mul(tokens_length, 32)
                let __flcb_assets_bytes := mul(assets_length, 32)
                let __flcb_tokens_seg := add(32, __flcb_tokens_bytes)
                let __flcb_assets_seg := add(32, __flcb_assets_bytes)
                let __flcb_data_off := add(128, add(__flcb_tokens_seg, __flcb_assets_seg))
                let __flcb_tokens_pos := add(__flcb_ptr, 132)
                let __flcb_assets_pos := add(__flcb_ptr, add(132, __flcb_tokens_seg))
                let __flcb_data_pos := add(__flcb_ptr, add(4, __flcb_data_off))
                let __flcb_padded_data_len := and(add(data_length, 31), not(31))
                let __flcb_total := add(add(__flcb_data_pos, 32), __flcb_padded_data_len)
                __flcb_total := sub(__flcb_total, __flcb_ptr)
                mstore(__flcb_ptr, shl(224, 0xd1f260c3))
                mstore(add(__flcb_ptr, 4), sender)
                mstore(add(__flcb_ptr, 36), 128)
                mstore(add(__flcb_ptr, 68), add(128, __flcb_tokens_seg))
                mstore(add(__flcb_ptr, 100), __flcb_data_off)
                mstore(__flcb_tokens_pos, tokens_length)
                calldatacopy(add(__flcb_tokens_pos, 32), tokens_data_offset, __flcb_tokens_bytes)
                mstore(__flcb_assets_pos, assets_length)
                calldatacopy(add(__flcb_assets_pos, 32), assets_data_offset, __flcb_assets_bytes)
                mstore(__flcb_data_pos, data_length)
                calldatacopy(add(__flcb_data_pos, 32), data_data_offset, data_length)
                mstore(64, add(__flcb_ptr, and(add(__flcb_total, 31), not(31))))
                let __flcb_success := call(gas(), callback, 0, __flcb_ptr, __flcb_total, __flcb_ptr, 32)
                if iszero(__flcb_success) {
                    let __flcb_rds := returndatasize()
                    returndatacopy(0, 0, __flcb_rds)
                    revert(0, __flcb_rds)
                }
                if lt(returndatasize(), 32) {
                    revert(0, 0)
                }
                if iszero(eq(mload(__flcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                    revert(0, 0)
                }
            }
            for {
                let __forEach_idx := 0
                let __forEach_count := tokenCount
                let i := 0
            } lt(__forEach_idx, __forEach_count) {
                __forEach_idx := add(__forEach_idx, 1)
            } {
                i := __forEach_idx
                let self := address()
                {
                    if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), callback)
                    mstore(add(__lstf_ptr, 36), self)
                    mstore(add(__lstf_ptr, 68), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
            }
            stop()
        }
        function internal_internal_collateral(id, user, index) -> __ret0 {
            let value := internal_internal_collateralAmount(id, user, index)
            __ret0 := value
            leave
        }
        function internal_internal_pendingFee(id, user) -> __ret0 {
            let value := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_lastAccrual(id, user) -> __ret0 {
            let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_lastLossFactor(id, user) -> __ret0 {
            let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal_internal_collateralBitmap(id, user) -> __ret0 {
            let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
            __ret0 := value
            leave
        }
        function internal___modifier_onlyRoleSetter() {
            let sender := caller()
            let currentRoleSetter := sload(7)
            if iszero(eq(sender, currentRoleSetter)) {
                {
                    let __err_ptr := mload(64)
                    mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                    let __err_hash := keccak256(__err_ptr, 16)
                    let __err_selector := shl(224, shr(224, __err_hash))
                    mstore(0, __err_selector)
                    let __err_tail := 0
                    revert(0, add(4, __err_tail))
                }
            }
            stop()
        }
        function internal___modifier_onlyFeeSetter() {
            let sender := caller()
            let currentFeeSetter := sload(8)
            if iszero(eq(sender, currentFeeSetter)) {
                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                mstore(4, 32)
                mstore(36, 13)
                mstore(68, 0x4f6e6c7946656553657474657200000000000000000000000000000000000000)
                revert(0, 100)
            }
            stop()
        }
        function internal___modifier_onlyFeeClaimer() {
            let sender := caller()
            let currentFeeClaimer := sload(9)
            if iszero(eq(sender, currentFeeClaimer)) {
                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                mstore(4, 32)
                mstore(36, 14)
                mstore(68, 0x4f6e6c79466565436c61696d6572000000000000000000000000000000000000)
                revert(0, 100)
            }
            stop()
        }
        function internal___modifier_onlyTickSpacingSetter() {
            let sender := caller()
            let currentTickSpacingSetter := sload(10)
            if iszero(eq(sender, currentTickSpacingSetter)) {
                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                mstore(4, 32)
                mstore(36, 21)
                mstore(68, 0x4f6e6c795469636b53706163696e675365747465720000000000000000000000)
                revert(0, 100)
            }
            stop()
        }
        let cid := chainid()
        let sender := caller()
        sstore(1024, cid)
        sstore(7, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
        sstore(8, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
        sstore(9, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
        sstore(10, and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
        datacopy(0, dataoffset("runtime"), datasize("runtime"))
        return(0, datasize("runtime"))
    }
    object "runtime" {
        code {
            function mappingSlot(baseSlot, key) -> slot {
                mstore(0, key)
                mstore(32, baseSlot)
                slot := keccak256(0, 64)
            }
            function __verity_array_element_calldata_checked(data_offset, length, index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                word := calldataload(add(data_offset, mul(index, 32)))
            }
            function __verity_array_element_memory_checked(data_offset, length, index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                word := mload(add(data_offset, mul(index, 32)))
            }
            function __verity_array_element_word_calldata_checked(data_offset, length, index, element_words, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                word := calldataload(add(data_offset, mul(add(mul(index, element_words), word_offset), 32)))
            }
            function __verity_array_element_word_memory_checked(data_offset, length, index, element_words, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                word := mload(add(data_offset, mul(add(mul(index, element_words), word_offset), 32)))
            }
            function __verity_array_element_dynamic_word_calldata_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_word_pos := add(add(data_offset, __element_rel_offset), mul(word_offset, 32))
                if gt(__element_word_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__element_word_pos)
            }
            function __verity_array_element_dynamic_word_memory_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_word_pos := add(add(data_offset, __element_rel_offset), mul(word_offset, 32))
                word := mload(__element_word_pos)
            }
            function __verity_array_element_dynamic_data_offset_calldata_checked(data_offset, length, index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                if gt(__element_head_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := __element_head_pos
            }
            function __verity_array_element_dynamic_data_offset_memory_checked(data_offset, length, index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                word := __element_head_pos
            }
            function __verity_array_element_dynamic_member_length_calldata_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                if gt(__member_data_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__member_data_pos)
            }
            function __verity_array_element_dynamic_member_length_memory_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                word := mload(__member_data_pos)
            }
            function __verity_array_element_dynamic_member_data_offset_calldata_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                if gt(__member_data_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := add(__member_data_pos, 32)
            }
            function __verity_array_element_dynamic_member_data_offset_memory_checked(data_offset, length, index, word_offset) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                word := add(__member_data_pos, 32)
            }
            function __verity_array_element_dynamic_member_element_calldata_checked(data_offset, length, index, word_offset, inner_index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := calldataload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := calldataload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                let __member_length := calldataload(__member_data_pos)
                if iszero(lt(inner_index, __member_length)) {
                    revert(0, 0)
                }
                let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
                if gt(__word_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__word_pos)
            }
            function __verity_array_element_dynamic_member_element_memory_checked(data_offset, length, index, word_offset, inner_index) -> word {
                if iszero(lt(index, length)) {
                    revert(0, 0)
                }
                let __element_rel_offset := mload(add(data_offset, mul(index, 32)))
                if lt(__element_rel_offset, mul(length, 32)) {
                    revert(0, 0)
                }
                let __element_head_pos := add(data_offset, __element_rel_offset)
                let __member_rel_offset := mload(add(__element_head_pos, mul(word_offset, 32)))
                let __member_data_pos := add(__element_head_pos, __member_rel_offset)
                let __member_length := mload(__member_data_pos)
                if iszero(lt(inner_index, __member_length)) {
                    revert(0, 0)
                }
                let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
                word := mload(__word_pos)
            }
            function __verity_param_dynamic_head_word_calldata_checked(data_offset, word_offset) -> word {
                let __head_word_pos := add(data_offset, mul(word_offset, 32))
                if gt(__head_word_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__head_word_pos)
            }
            function __verity_param_dynamic_head_word_memory_checked(data_offset, word_offset) -> word {
                let __head_word_pos := add(data_offset, mul(word_offset, 32))
                word := mload(__head_word_pos)
            }
            function __verity_param_dynamic_member_length_calldata_checked(data_offset, word_offset) -> word {
                let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                if gt(__member_data_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__member_data_pos)
            }
            function __verity_param_dynamic_member_length_memory_checked(data_offset, word_offset) -> word {
                let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                word := mload(__member_data_pos)
            }
            function __verity_param_dynamic_member_data_offset_calldata_checked(data_offset, word_offset) -> word {
                let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                if gt(__member_data_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := add(__member_data_pos, 32)
            }
            function __verity_param_dynamic_member_data_offset_memory_checked(data_offset, word_offset) -> word {
                let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                word := add(__member_data_pos, 32)
            }
            function __verity_param_dynamic_member_element_calldata_checked(data_offset, word_offset, inner_index) -> word {
                let __member_rel_offset := calldataload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                let __member_length := calldataload(__member_data_pos)
                if iszero(lt(inner_index, __member_length)) {
                    revert(0, 0)
                }
                let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
                if gt(__word_pos, sub(calldatasize(), 32)) {
                    revert(0, 0)
                }
                word := calldataload(__word_pos)
            }
            function __verity_param_dynamic_member_element_memory_checked(data_offset, word_offset, inner_index) -> word {
                let __member_rel_offset := mload(add(data_offset, mul(word_offset, 32)))
                let __member_data_pos := add(data_offset, __member_rel_offset)
                let __member_length := mload(__member_data_pos)
                if iszero(lt(inner_index, __member_length)) {
                    revert(0, 0)
                }
                let __word_pos := add(__member_data_pos, add(32, mul(inner_index, 32)))
                word := mload(__word_pos)
            }
            function internal_internal_multicall(calls_data_offset, calls_length) {
                {
                    let __mc_head := calldataload(4)
                    let __mc_base := add(4, __mc_head)
                    let __mc_len := calldataload(__mc_base)
                    let __mc_i := 0
                    for {
                    } lt(__mc_i, __mc_len) {
                        __mc_i := add(__mc_i, 1)
                    } {
                        let __mc_elem_head := calldataload(add(add(__mc_base, 32), mul(__mc_i, 32)))
                        let __mc_elem := add(add(__mc_base, 32), __mc_elem_head)
                        let __mc_size := calldataload(__mc_elem)
                        let __mc_data := add(__mc_elem, 32)
                        let __mc_ptr := mload(64)
                        calldatacopy(__mc_ptr, __mc_data, __mc_size)
                        mstore(64, add(__mc_ptr, and(add(__mc_size, 31), not(31))))
                        let __mc_success := delegatecall(gas(), address(), __mc_ptr, __mc_size, 0, 0)
                        if iszero(__mc_success) {
                            let __mc_rds := returndatasize()
                            returndatacopy(0, 0, __mc_rds)
                            revert(0, __mc_rds)
                        }
                    }
                }
                stop()
            }
            function internal_internal_INITIAL_CHAIN_ID() -> __ret0 {
                let cid := sload(1024)
                __ret0 := cid
                leave
            }
            function internal_internal_roleSetter() -> __ret0 {
                let value := sload(7)
                __ret0 := value
                leave
            }
            function internal_internal_feeSetter() -> __ret0 {
                let value := sload(8)
                __ret0 := value
                leave
            }
            function internal_internal_feeClaimer() -> __ret0 {
                let value := sload(9)
                __ret0 := value
                leave
            }
            function internal_internal_tickSpacingSetter() -> __ret0 {
                let value := sload(10)
                __ret0 := value
                leave
            }
            function internal_internal_consumed(user, group) -> __ret0 {
                let value := sload(mappingSlot(mappingSlot(2, user), group))
                __ret0 := value
                leave
            }
            function internal_internal_isAuthorized(authorizer, authorized) -> __ret0 {
                let value := sload(mappingSlot(mappingSlot(3, authorizer), authorized))
                __ret0 := iszero(eq(value, 0))
                leave
            }
            function internal_internal_defaultSettlementFeeCbp(loanToken, index) -> __ret0 {
                let value := sload(mappingSlot(mappingSlot(4, loanToken), index))
                __ret0 := value
                leave
            }
            function internal_internal_defaultContinuousFee(loanToken) -> __ret0 {
                let value := sload(mappingSlot(5, loanToken))
                __ret0 := value
                leave
            }
            function internal_internal_claimableSettlementFee(token) -> __ret0 {
                let value := sload(mappingSlot(6, token))
                __ret0 := value
                leave
            }
            function internal_internal_maxSettlementFee(index) -> __ret0 {
                let value := 5000000000000000
                if eq(index, 0) {
                    value := 14000000000000
                }
                if eq(index, 1) {
                    value := 14000000000000
                }
                if eq(index, 2) {
                    value := 98000000000000
                }
                if eq(index, 3) {
                    value := 417000000000000
                }
                if eq(index, 4) {
                    value := 1250000000000000
                }
                if eq(index, 5) {
                    value := 2500000000000000
                }
                __ret0 := value
                leave
            }
            function internal_internal_isLltvAllowed(lltv) -> __ret0 {
                __ret0 := or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(eq(lltv, 385000000000000000))), iszero(iszero(eq(lltv, 625000000000000000)))))), iszero(iszero(eq(lltv, 770000000000000000)))))), iszero(iszero(eq(lltv, 860000000000000000)))))), iszero(iszero(eq(lltv, 915000000000000000)))))), iszero(iszero(eq(lltv, 945000000000000000)))))), iszero(iszero(eq(lltv, 965000000000000000)))))), iszero(iszero(eq(lltv, 980000000000000000)))))), iszero(iszero(eq(lltv, 1000000000000000000))))
                leave
            }
            function internal_internal_maxLif(lltv, cursor) -> __ret0 {
                __ret0 := div(mul(1000000000000000000, 1000000000000000000), sub(1000000000000000000, div(mul(cursor, sub(1000000000000000000, lltv)), 1000000000000000000)))
                leave
            }
            function internal_internal_min(a, b) -> __ret0 {
                {
                    let __ite_cond := lt(a, b)
                    if __ite_cond {
                        __ret0 := a
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := b
                        leave
                    }
                }
            }
            function internal_internal_validateCollateralParams(collateralParams_data_offset, collateralParams_length) -> __ret0 {
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
                    let collateralToken := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 0)
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
                    let lltv := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 1)
                    let allowed := internal_internal_isLltvAllowed(lltv)
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
                    let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                    let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                    let lif := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 2)
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
                __ret0 := 1
                leave
            }
            function internal_internal_countBits128(bitmap) -> __ret0 {
                let count := 0
                for {
                    let __forEach_idx := 0
                    let __forEach_count := 128
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    let mask := shl(i, 1)
                    if iszero(eq(and(bitmap, mask), 0)) {
                        count := add(count, 1)
                    }
                }
                __ret0 := count
                leave
            }
            function internal_internal_collateralBitMask(index) -> __ret0 {
                __ret0 := shl(index, 1)
                leave
            }
            function internal_internal_collateralBitmapSetBit(bitmap, index) -> __ret0 {
                let mask := internal_internal_collateralBitMask(index)
                __ret0 := or(bitmap, mask)
                leave
            }
            function internal_internal_collateralBitmapClearBit(bitmap, index) -> __ret0 {
                let mask := internal_internal_collateralBitMask(index)
                __ret0 := and(bitmap, not(mask))
                leave
            }
            function internal_internal_collateralBitmapIsSet(bitmap, index) -> __ret0 {
                let mask := internal_internal_collateralBitMask(index)
                __ret0 := gt(and(bitmap, mask), 0)
                leave
            }
            function internal_internal_codeDataMarketId(market_data_offset) -> __ret0 {
                let initialChainId := sload(1024)
                let self := address()
                let id := 0
                {
                    let __midnight_id_ptr := mload(64)
                    mstore(__midnight_id_ptr, shl(168, 0x600b380380600b5f395ff3))
                    mstore(add(__midnight_id_ptr, 11), 32)
                    let __midnight_id_tuple_ptr := add(__midnight_id_ptr, 43)
                    mstore(__midnight_id_tuple_ptr, calldataload(market_data_offset))
                    mstore(add(__midnight_id_tuple_ptr, 32), 192)
                    mstore(add(__midnight_id_tuple_ptr, 64), calldataload(add(market_data_offset, 64)))
                    mstore(add(__midnight_id_tuple_ptr, 96), calldataload(add(market_data_offset, 96)))
                    mstore(add(__midnight_id_tuple_ptr, 128), calldataload(add(market_data_offset, 128)))
                    mstore(add(__midnight_id_tuple_ptr, 160), calldataload(add(market_data_offset, 160)))
                    let __midnight_id_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                    let __midnight_id_collateral_length := calldataload(__midnight_id_collateral_offset)
                    let __midnight_id_collateral_bytes := mul(__midnight_id_collateral_length, 128)
                    mstore(add(__midnight_id_tuple_ptr, 192), __midnight_id_collateral_length)
                    calldatacopy(add(__midnight_id_tuple_ptr, 224), add(__midnight_id_collateral_offset, 32), __midnight_id_collateral_bytes)
                    let __midnight_id_abi_length := add(256, __midnight_id_collateral_bytes)
                    let __midnight_id_initcode_length := add(11, __midnight_id_abi_length)
                    let __midnight_id_inner_hash := keccak256(__midnight_id_ptr, __midnight_id_initcode_length)
                    let __midnight_id_outer_ptr := add(__midnight_id_ptr, and(add(__midnight_id_initcode_length, 31), not(31)))
                    mstore(__midnight_id_outer_ptr, shl(248, 255))
                    mstore(add(__midnight_id_outer_ptr, 1), shl(96, self))
                    mstore(add(__midnight_id_outer_ptr, 21), initialChainId)
                    mstore(add(__midnight_id_outer_ptr, 53), __midnight_id_inner_hash)
                    id := keccak256(__midnight_id_outer_ptr, 85)
                    mstore(64, add(__midnight_id_outer_ptr, 96))
                }
                __ret0 := id
                leave
            }
            function internal_internal_codeDataStoreMarket(market_data_offset, salt) -> __ret0 {
                let pointer := 0
                {
                    let __midnight_store_ptr := mload(64)
                    mstore(__midnight_store_ptr, shl(168, 0x600b380380600b5f395ff3))
                    mstore(add(__midnight_store_ptr, 11), 32)
                    let __midnight_store_tuple_ptr := add(__midnight_store_ptr, 43)
                    mstore(__midnight_store_tuple_ptr, calldataload(market_data_offset))
                    mstore(add(__midnight_store_tuple_ptr, 32), 192)
                    mstore(add(__midnight_store_tuple_ptr, 64), calldataload(add(market_data_offset, 64)))
                    mstore(add(__midnight_store_tuple_ptr, 96), calldataload(add(market_data_offset, 96)))
                    mstore(add(__midnight_store_tuple_ptr, 128), calldataload(add(market_data_offset, 128)))
                    mstore(add(__midnight_store_tuple_ptr, 160), calldataload(add(market_data_offset, 160)))
                    let __midnight_store_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                    let __midnight_store_collateral_length := calldataload(__midnight_store_collateral_offset)
                    let __midnight_store_collateral_bytes := mul(__midnight_store_collateral_length, 128)
                    mstore(add(__midnight_store_tuple_ptr, 192), __midnight_store_collateral_length)
                    calldatacopy(add(__midnight_store_tuple_ptr, 224), add(__midnight_store_collateral_offset, 32), __midnight_store_collateral_bytes)
                    let __midnight_store_abi_length := add(256, __midnight_store_collateral_bytes)
                    let __midnight_store_initcode_length := add(11, __midnight_store_abi_length)
                    pointer := create2(0, __midnight_store_ptr, __midnight_store_initcode_length, salt)
                    if iszero(pointer) {
                        mstore(0, shl(224, 0x4e487b71))
                        mstore(4, 81)
                        revert(0, 36)
                    }
                    mstore(64, add(__midnight_store_ptr, and(add(__midnight_store_initcode_length, 31), not(31))))
                }
                __ret0 := pointer
                leave
            }
            function internal_internal_toId(market_data_offset) -> __ret0 {
                let id := internal_internal_codeDataMarketId(market_data_offset)
                __ret0 := id
                leave
            }
            function internal_internal_toMarket(id) {
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                {
                    let __midnight_market_return_pointer := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
                    let __midnight_market_return_length := extcodesize(__midnight_market_return_pointer)
                    let __midnight_market_return_ptr := mload(64)
                    extcodecopy(__midnight_market_return_pointer, __midnight_market_return_ptr, 0, __midnight_market_return_length)
                    return(__midnight_market_return_ptr, __midnight_market_return_length)
                }
                stop()
            }
            function internal_internal_position(id, user) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5 {
                let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                let collateralBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                __ret0 := credit
                __ret1 := pendingFee
                __ret2 := lastLossFactor
                __ret3 := lastAccrual
                __ret4 := debt
                __ret5 := collateralBitmap
                leave
            }
            function internal_internal_marketState(id) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6, __ret7, __ret8, __ret9, __ret10, __ret11, __ret12 {
                let totalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let lossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let withdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                let continuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                let continuousFee := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                let tickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                __ret0 := totalUnits
                __ret1 := lossFactor
                __ret2 := withdrawable
                __ret3 := continuousFeeCredit
                __ret4 := settlementFeeCbp0
                __ret5 := settlementFeeCbp1
                __ret6 := settlementFeeCbp2
                __ret7 := settlementFeeCbp3
                __ret8 := settlementFeeCbp4
                __ret9 := settlementFeeCbp5
                __ret10 := settlementFeeCbp6
                __ret11 := continuousFee
                __ret12 := tickSpacing
                leave
            }
            function internal_internal_updatePositionView(market_data_offset, id, user) -> __ret0, __ret1, __ret2 {
                let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let now := timestamp()
                let postSlashCredit := 0
                if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                    postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                }
                let postSlashPendingFee := 0
                if gt(credit, 0) {
                    postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                }
                let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                    accrualEnd := now
                }
                let accrued := 0
                if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                    accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                }
                __ret0 := sub(postSlashCredit, accrued)
                __ret1 := sub(postSlashPendingFee, accrued)
                __ret2 := accrued
                leave
            }
            function internal_internal_updatePosition(market_data_offset, user) -> __ret0, __ret1, __ret2 {
                let id := internal_internal_toId(market_data_offset)
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let now := timestamp()
                let postSlashCredit := 0
                if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                    postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                }
                let postSlashPendingFee := 0
                if gt(credit, 0) {
                    postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                }
                let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                    accrualEnd := now
                }
                let accrued := 0
                if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                    accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                }
                let newCredit := sub(postSlashCredit, accrued)
                let newPendingFee := sub(postSlashPendingFee, accrued)
                let creditDecrease := sub(credit, newCredit)
                let pendingFeeDecrease := sub(pendingFee, newPendingFee)
                {
                    let __compat_value := newCredit
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := marketLossFactor
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := newPendingFee
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := now
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(currentContinuousFeeCredit, accrued)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                    mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 55)
                    mstore(add(__evt_ptr, 0), creditDecrease)
                    mstore(add(__evt_ptr, 32), pendingFeeDecrease)
                    mstore(add(__evt_ptr, 64), accrued)
                    log3(__evt_ptr, 96, __evt_topic0, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                __ret0 := newCredit
                __ret1 := newPendingFee
                __ret2 := accrued
                leave
            }
            function internal_internal_setRoleSetter(newRoleSetter) {
                let sender := caller()
                let currentRoleSetter := sload(7)
                if iszero(eq(sender, currentRoleSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(7, and(newRoleSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                stop()
            }
            function internal_internal_setFeeSetter(newFeeSetter) {
                let sender := caller()
                let currentRoleSetter := sload(7)
                if iszero(eq(sender, currentRoleSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(8, and(newFeeSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                stop()
            }
            function internal_internal_setFeeClaimer(newFeeClaimer) {
                let sender := caller()
                let currentRoleSetter := sload(7)
                if iszero(eq(sender, currentRoleSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(9, and(newFeeClaimer, 0xffffffffffffffffffffffffffffffffffffffff))
                stop()
            }
            function internal_internal_setTickSpacingSetter(newTickSpacingSetter) {
                let sender := caller()
                let currentRoleSetter := sload(7)
                if iszero(eq(sender, currentRoleSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(10, and(newTickSpacingSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                stop()
            }
            function internal_internal_setIsAuthorized(authorized, newIsAuthorized, onBehalf) {
                let sender := caller()
                let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
                if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let flag := 0
                {
                    let __ite_cond := newIsAuthorized
                    if __ite_cond {
                        flag := 1
                    }
                    if iszero(__ite_cond) {
                        flag := 0
                    }
                }
                sstore(mappingSlot(mappingSlot(3, onBehalf), authorized), flag)
                stop()
            }
            function internal_internal_setConsumed(group, amount, onBehalf) {
                let sender := caller()
                let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
                if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let current := sload(mappingSlot(mappingSlot(2, onBehalf), group))
                if lt(amount, current) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x416c7265616479436f6e73756d65642829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(mappingSlot(mappingSlot(2, onBehalf), group), amount)
                stop()
            }
            function internal_internal_setDefaultSettlementFee(loanToken, index, newSettlementFee) {
                let sender := caller()
                let currentFeeSetter := sload(8)
                if iszero(eq(sender, currentFeeSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 15)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(lt(index, 7)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let maxFee := 5000000000000000
                if eq(index, 0) {
                    maxFee := 14000000000000
                }
                if eq(index, 1) {
                    maxFee := 14000000000000
                }
                if eq(index, 2) {
                    maxFee := 98000000000000
                }
                if eq(index, 3) {
                    maxFee := 417000000000000
                }
                if eq(index, 4) {
                    maxFee := 1250000000000000
                }
                if eq(index, 5) {
                    maxFee := 2500000000000000
                }
                if gt(newSettlementFee, maxFee) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 22)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                        let __err_hash := keccak256(__err_ptr, 24)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(mappingSlot(mappingSlot(4, loanToken), index), div(newSettlementFee, 1000000000000))
                stop()
            }
            function internal_internal_setDefaultContinuousFee(loanToken, newContinuousFee) {
                let sender := caller()
                let currentFeeSetter := sload(8)
                if iszero(eq(sender, currentFeeSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 15)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if gt(newContinuousFee, 317097919) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 22)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(mappingSlot(5, loanToken), newContinuousFee)
                stop()
            }
            function internal_internal_claimSettlementFee(token, amount, receiver) {
                let sender := caller()
                let currentFeeClaimer := sload(9)
                if iszero(eq(sender, currentFeeClaimer)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let claimable := sload(mappingSlot(6, token))
                if gt(amount, claimable) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                sstore(mappingSlot(6, token), sub(claimable, amount))
                {
                    if iszero(gt(extcodesize(token), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), receiver)
                    mstore(add(__lst_ptr, 36), amount)
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), token, 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
                stop()
            }
            function internal_internal_claimContinuousFee(market_data_offset, amount, receiver) {
                let id := internal_internal_toId(market_data_offset)
                let sender := caller()
                let currentFeeClaimer := sload(9)
                if iszero(eq(sender, currentFeeClaimer)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(currentContinuousFeeCredit, amount)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(currentTotalUnits, amount)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(1, id))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let currentWithdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(currentWithdrawable, amount)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x436c61696d436f6e74696e756f757346656528616464726573732c6279746573)
                    mstore(add(__evt_ptr, 32), 0x33322c75696e743235362c616464726573732900000000000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 51)
                    mstore(add(__evt_ptr, 0), amount)
                    log4(__evt_ptr, 32, __evt_topic0, and(sender, 0xffffffffffffffffffffffffffffffffffffffff), id, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), receiver)
                    mstore(add(__lst_ptr, 36), amount)
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
                stop()
            }
            function internal_internal_setMarketTickSpacing(id, newTickSpacing) {
                let sender := caller()
                let currentTickSpacingSetter := sload(10)
                if iszero(eq(sender, currentTickSpacingSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c795469636b53706163696e675365747465722829000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 23)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(gt(newTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 20)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(eq(mod(currentTickSpacing, newTickSpacing), 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 20)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                {
                    let __compat_value := newTickSpacing
                    let __compat_packed := and(__compat_value, 255)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                }
                stop()
            }
            function internal_internal_setMarketSettlementFee(id, index, newSettlementFee) {
                let sender := caller()
                let currentFeeSetter := sload(8)
                if iszero(eq(sender, currentFeeSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 15)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(lt(index, 7)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let maxFee := 5000000000000000
                if eq(index, 0) {
                    maxFee := 14000000000000
                }
                if eq(index, 1) {
                    maxFee := 14000000000000
                }
                if eq(index, 2) {
                    maxFee := 98000000000000
                }
                if eq(index, 3) {
                    maxFee := 417000000000000
                }
                if eq(index, 4) {
                    maxFee := 1250000000000000
                }
                if eq(index, 5) {
                    maxFee := 2500000000000000
                }
                if gt(newSettlementFee, maxFee) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 22)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                        let __err_hash := keccak256(__err_ptr, 24)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let newSettlementFeeCbp := div(newSettlementFee, 1000000000000)
                if eq(index, 0) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                }
                if eq(index, 1) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                    }
                }
                if eq(index, 2) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                    }
                }
                if eq(index, 3) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                    }
                }
                if eq(index, 4) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                    }
                }
                if eq(index, 5) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                    }
                }
                if eq(index, 6) {
                    {
                        let __compat_value := newSettlementFeeCbp
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                    }
                }
                stop()
            }
            function internal_internal_setMarketContinuousFee(id, newContinuousFee) {
                let sender := caller()
                let currentFeeSetter := sload(8)
                if iszero(eq(sender, currentFeeSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 15)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if gt(newContinuousFee, 317097919) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 22)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                {
                    let __compat_value := newContinuousFee
                    let __compat_packed := and(__compat_value, 4294967295)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                    sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                }
                stop()
            }
            function internal_internal_touchMarket(market_data_offset) -> __ret0 {
                let id := internal_internal_toId(market_data_offset)
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if eq(currentTickSpacing, 0) {
                    let now := timestamp()
                    if gt(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), add(now, 3153600000)) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 16)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                    let _collateralParamsValid := internal_internal_validateCollateralParams(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1))
                    let salt := sload(1024)
                    let _marketPointer := internal_internal_codeDataStoreMarket(market_data_offset, salt)
                    {
                        let __compat_value := 4
                        let __compat_packed := and(__compat_value, 255)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                    }
                    let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0)
                    let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 1)
                    let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 2)
                    let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 3)
                    let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 4)
                    let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 5)
                    let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 6)
                    let continuous := sload(mappingSlot(5, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)))
                    {
                        let __compat_value := settlementFeeCbp0
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp1
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp2
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp3
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp4
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp5
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp6
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                    }
                    {
                        let __compat_value := continuous
                        let __compat_packed := and(__compat_value, 4294967295)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                    }
                }
                __ret0 := id
                leave
            }
            function internal_internal_settlementFee(id, timeToMaturity) -> __ret0 {
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(gt(currentTickSpacing, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 18)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                let start := 15552000
                let finish := 31104000
                let feeLower := mul(settlementFeeCbp5, 1000000000000)
                let feeUpper := mul(settlementFeeCbp6, 1000000000000)
                if lt(timeToMaturity, 86400) {
                    start := 0
                    finish := 86400
                    feeLower := mul(settlementFeeCbp0, 1000000000000)
                    feeUpper := mul(settlementFeeCbp1, 1000000000000)
                }
                if and(iszero(iszero(iszero(lt(timeToMaturity, 86400)))), iszero(iszero(lt(timeToMaturity, 604800)))) {
                    start := 86400
                    finish := 604800
                    feeLower := mul(settlementFeeCbp1, 1000000000000)
                    feeUpper := mul(settlementFeeCbp2, 1000000000000)
                }
                if and(iszero(iszero(iszero(lt(timeToMaturity, 604800)))), iszero(iszero(lt(timeToMaturity, 2592000)))) {
                    start := 604800
                    finish := 2592000
                    feeLower := mul(settlementFeeCbp2, 1000000000000)
                    feeUpper := mul(settlementFeeCbp3, 1000000000000)
                }
                if and(iszero(iszero(iszero(lt(timeToMaturity, 2592000)))), iszero(iszero(lt(timeToMaturity, 7776000)))) {
                    start := 2592000
                    finish := 7776000
                    feeLower := mul(settlementFeeCbp3, 1000000000000)
                    feeUpper := mul(settlementFeeCbp4, 1000000000000)
                }
                if and(iszero(iszero(iszero(lt(timeToMaturity, 7776000)))), iszero(iszero(lt(timeToMaturity, 15552000)))) {
                    start := 7776000
                    finish := 15552000
                    feeLower := mul(settlementFeeCbp4, 1000000000000)
                    feeUpper := mul(settlementFeeCbp5, 1000000000000)
                }
                {
                    let __ite_cond := iszero(lt(timeToMaturity, 31104000))
                    if __ite_cond {
                        __ret0 := mul(settlementFeeCbp6, 1000000000000)
                        leave
                    }
                    if iszero(__ite_cond) {
                        __ret0 := div(add(mul(feeLower, sub(finish, timeToMaturity)), mul(feeUpper, sub(timeToMaturity, start))), sub(finish, start))
                        leave
                    }
                }
            }
            function internal_internal_tickToPrice(tick) -> __ret0 {
                if gt(tick, 5820) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x5469636b4f75744f6652616e6765282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let price := 0
                {
                    let __tp_x := mul(4987541511039073, sub(2910, tick))
                    let __tp_abs := __tp_x
                    let __tp_negative := slt(__tp_x, 0)
                    if __tp_negative {
                        __tp_abs := sub(0, __tp_x)
                    }
                    let __tp_q := div(add(__tp_abs, 322611214989459870), 693147180559945309)
                    let __tp_r := sub(__tp_abs, mul(__tp_q, 693147180559945309))
                    let __tp_second := sdiv(mul(__tp_r, __tp_r), 2000000000000000000)
                    let __tp_third := sdiv(mul(__tp_second, __tp_r), 3000000000000000000)
                    let __tp_expR := add(1000000000000000000, add(__tp_r, add(__tp_second, __tp_third)))
                    let __tp_wexp := shl(__tp_q, __tp_expR)
                    if __tp_negative {
                        __tp_wexp := div(1000000000000000000000000000000000000, __tp_wexp)
                    }
                    let __tp_den := add(1000000000000000000, __tp_wexp)
                    let __tp_raw := div(add(1000000000000000000000000000000000000, div(sub(__tp_den, 1), 2)), __tp_den)
                    price := mul(div(add(__tp_raw, div(sub(1000000000000, 1), 2)), 1000000000000), 1000000000000)
                }
                __ret0 := price
                leave
            }
            function internal_internal_enterGateCanIncreaseCredit(gate, account) -> __ret0 {
                let allowed := 0
                {
                    let __ecwr_ptr := mload(64)
                    mstore(__ecwr_ptr, shl(224, 0x58ac9f9e))
                    mstore(add(__ecwr_ptr, 4), account)
                    mstore(64, add(__ecwr_ptr, 64))
                    let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                    if iszero(__ecwr_success) {
                        let __ecwr_rds := returndatasize()
                        returndatacopy(0, 0, __ecwr_rds)
                        revert(0, __ecwr_rds)
                    }
                    if lt(returndatasize(), 32) {
                        revert(0, 0)
                    }
                    allowed := mload(__ecwr_ptr)
                }
                __ret0 := allowed
                leave
            }
            function internal_internal_enterGateCanIncreaseDebt(gate, account) -> __ret0 {
                let allowed := 0
                {
                    let __ecwr_ptr := mload(64)
                    mstore(__ecwr_ptr, shl(224, 0xfe9bf956))
                    mstore(add(__ecwr_ptr, 4), account)
                    mstore(64, add(__ecwr_ptr, 64))
                    let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                    if iszero(__ecwr_success) {
                        let __ecwr_rds := returndatasize()
                        returndatacopy(0, 0, __ecwr_rds)
                        revert(0, __ecwr_rds)
                    }
                    if lt(returndatasize(), 32) {
                        revert(0, 0)
                    }
                    allowed := mload(__ecwr_ptr)
                }
                __ret0 := allowed
                leave
            }
            function internal_internal_updateBuyerForTake(id, buyer, units, maturity, now) -> __ret0 {
                let buyerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))), 340282366920938463463374607431768211455)
                let buyerDebtDecrease := internal_internal_min(units, buyerDebt)
                let buyerCreditIncrease := sub(units, buyerDebtDecrease)
                let continuousFeeValue := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                let timeToMaturity := 0
                if gt(maturity, now) {
                    timeToMaturity := sub(maturity, now)
                }
                let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValue, timeToMaturity)), 1000000000000000000)
                let buyerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
                let buyerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
                let buyerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
                let buyerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
                let buyerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let buyerPostSlashCredit := 0
                if lt(buyerLastLossFactor, 340282366920938463463374607431768211455) {
                    buyerPostSlashCredit := div(mul(buyerCredit, sub(340282366920938463463374607431768211455, buyerMarketLossFactor)), sub(340282366920938463463374607431768211455, buyerLastLossFactor))
                }
                let buyerPostSlashPendingFee := 0
                if gt(buyerCredit, 0) {
                    buyerPostSlashPendingFee := sub(buyerPendingFee, div(add(mul(buyerPendingFee, sub(buyerCredit, buyerPostSlashCredit)), sub(buyerCredit, 1)), buyerCredit))
                }
                let buyerAccrualEnd := maturity
                if iszero(gt(now, maturity)) {
                    buyerAccrualEnd := now
                }
                let buyerAccrued := 0
                if lt(buyerLastAccrual, maturity) {
                    buyerAccrued := div(mul(buyerPostSlashPendingFee, sub(buyerAccrualEnd, buyerLastAccrual)), sub(maturity, buyerLastAccrual))
                }
                let buyerCreditAfterUpdate := sub(buyerPostSlashCredit, buyerAccrued)
                let buyerPendingFeeAfterUpdate := sub(buyerPostSlashPendingFee, buyerAccrued)
                let buyerCreditDecrease := sub(buyerCredit, buyerCreditAfterUpdate)
                let buyerPendingFeeDecreaseForUpdate := sub(buyerPendingFee, buyerPendingFeeAfterUpdate)
                if or(iszero(iszero(gt(buyerCredit, 0))), iszero(iszero(gt(buyerCreditIncrease, 0)))) {
                    {
                        let __compat_value := buyerCreditAfterUpdate
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := buyerMarketLossFactor
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := buyerPendingFeeAfterUpdate
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    {
                        let __compat_value := now
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                    {
                        let __compat_value := add(currentContinuousFeeCredit, buyerAccrued)
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                    mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 55)
                    mstore(add(__evt_ptr, 0), buyerCreditDecrease)
                    mstore(add(__evt_ptr, 32), buyerPendingFeeDecreaseForUpdate)
                    mstore(add(__evt_ptr, 64), buyerAccrued)
                    log3(__evt_ptr, 96, __evt_topic0, id, and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    let __compat_value := sub(buyerDebt, buyerDebtDecrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), buyer), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := add(buyerPendingFeeAfterUpdate, buyerPendingFeeIncrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := add(buyerCreditAfterUpdate, buyerCreditIncrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                __ret0 := buyerCreditIncrease
                leave
            }
            function internal_internal_updateSellerForTake(id, seller, units, maturity, now) -> __ret0, __ret1, __ret2 {
                let sellerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
                let sellerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
                let sellerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
                let sellerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
                let sellerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let sellerPostSlashCredit := 0
                if lt(sellerLastLossFactor, 340282366920938463463374607431768211455) {
                    sellerPostSlashCredit := div(mul(sellerCredit, sub(340282366920938463463374607431768211455, sellerMarketLossFactor)), sub(340282366920938463463374607431768211455, sellerLastLossFactor))
                }
                let sellerPostSlashPendingFee := 0
                if gt(sellerCredit, 0) {
                    sellerPostSlashPendingFee := sub(sellerPendingFee, div(add(mul(sellerPendingFee, sub(sellerCredit, sellerPostSlashCredit)), sub(sellerCredit, 1)), sellerCredit))
                }
                let sellerAccrualEnd := maturity
                if iszero(gt(now, maturity)) {
                    sellerAccrualEnd := now
                }
                let sellerAccrued := 0
                if lt(sellerLastAccrual, maturity) {
                    sellerAccrued := div(mul(sellerPostSlashPendingFee, sub(sellerAccrualEnd, sellerLastAccrual)), sub(maturity, sellerLastAccrual))
                }
                let sellerCreditAfterUpdate := sub(sellerPostSlashCredit, sellerAccrued)
                let sellerPendingFeeAfterUpdate := sub(sellerPostSlashPendingFee, sellerAccrued)
                let sellerCreditDecreaseForUpdate := sub(sellerCredit, sellerCreditAfterUpdate)
                let sellerPendingFeeDecreaseForUpdate := sub(sellerPendingFee, sellerPendingFeeAfterUpdate)
                if gt(sellerCredit, 0) {
                    {
                        let __compat_value := sellerCreditAfterUpdate
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := sellerMarketLossFactor
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := sellerPendingFeeAfterUpdate
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    {
                        let __compat_value := now
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                    {
                        let __compat_value := add(currentContinuousFeeCredit, sellerAccrued)
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                    mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 55)
                    mstore(add(__evt_ptr, 0), sellerCreditDecreaseForUpdate)
                    mstore(add(__evt_ptr, 32), sellerPendingFeeDecreaseForUpdate)
                    mstore(add(__evt_ptr, 64), sellerAccrued)
                    log3(__evt_ptr, 96, __evt_topic0, id, and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                let sellerCreditDecrease := internal_internal_min(units, sellerCreditAfterUpdate)
                let sellerDebtIncrease := sub(units, sellerCreditDecrease)
                let sellerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 2))), 340282366920938463463374607431768211455)
                let sellerPendingFeeDecrease := 0
                if gt(sellerCreditAfterUpdate, 0) {
                    sellerPendingFeeDecrease := div(add(mul(sellerPendingFeeAfterUpdate, sellerCreditDecrease), sub(sellerCreditAfterUpdate, 1)), sellerCreditAfterUpdate)
                }
                {
                    let __compat_value := sub(sellerPendingFeeAfterUpdate, sellerPendingFeeDecrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := sub(sellerCreditAfterUpdate, sellerCreditDecrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := add(sellerDebt, sellerDebtIncrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), seller), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                __ret0 := sellerCreditDecrease
                __ret1 := sellerDebtIncrease
                __ret2 := sellerPendingFeeDecrease
                leave
            }
            function internal_internal_takeAssetsForPrice(id, units, tick, offerIsBuy, maturity, now, buyerCreditIncrease) -> __ret0, __ret1, __ret2 {
                let offerPrice := internal_internal_tickToPrice(tick)
                let timeToMaturityForFee := 0
                if gt(maturity, now) {
                    timeToMaturityForFee := sub(maturity, now)
                }
                let settlementFeeValue := internal_internal_settlementFee(id, timeToMaturityForFee)
                let continuousFeeValueForCallback := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValueForCallback, timeToMaturityForFee)), 1000000000000000000)
                let sellerPrice := offerPrice
                if offerIsBuy {
                    sellerPrice := sub(offerPrice, settlementFeeValue)
                }
                let buyerPrice := add(sellerPrice, settlementFeeValue)
                let buyerAssets := 0
                let sellerAssets := 0
                {
                    let __ite_cond := offerIsBuy
                    if __ite_cond {
                        buyerAssets := div(mul(units, buyerPrice), 1000000000000000000)
                        sellerAssets := div(mul(units, sellerPrice), 1000000000000000000)
                    }
                    if iszero(__ite_cond) {
                        buyerAssets := div(add(mul(units, buyerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                        sellerAssets := div(add(mul(units, sellerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                    }
                }
                __ret0 := buyerAssets
                __ret1 := sellerAssets
                __ret2 := buyerPendingFeeIncrease
                leave
            }
            function internal_internal_take(offer_data_offset, ratifierData_data_offset, ratifierData_length, units, taker, receiverIfTakerIsSeller, takerCallback, takerCallbackData_data_offset, takerCallbackData_length) -> __ret0, __ret1 {
                let sender := caller()
                let authorized := internal_internal_isAuthorized(taker, sender)
                if iszero(or(iszero(iszero(eq(taker, sender))), iszero(iszero(authorized)))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x54616b6572556e617574686f72697a6564282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let offerBase := add(calldataload(4), 4)
                let marketBase := add(offerBase, calldataload(offerBase))
                let loanToken := calldataload(marketBase)
                let maturity := calldataload(add(marketBase, 64))
                let enterGate := calldataload(add(marketBase, 128))
                let initialChainId := sload(1024)
                let contractSelf := address()
                let id := 0
                {
                    let __midnight_id_ptr := mload(64)
                    mstore(__midnight_id_ptr, shl(168, 0x600b380380600b5f395ff3))
                    mstore(add(__midnight_id_ptr, 11), 32)
                    let __midnight_id_tuple_ptr := add(__midnight_id_ptr, 43)
                    mstore(__midnight_id_tuple_ptr, calldataload(marketBase))
                    mstore(add(__midnight_id_tuple_ptr, 32), 192)
                    mstore(add(__midnight_id_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                    mstore(add(__midnight_id_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                    mstore(add(__midnight_id_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                    mstore(add(__midnight_id_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                    let __midnight_id_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                    let __midnight_id_collateral_length := calldataload(__midnight_id_collateral_offset)
                    let __midnight_id_collateral_bytes := mul(__midnight_id_collateral_length, 128)
                    mstore(add(__midnight_id_tuple_ptr, 192), __midnight_id_collateral_length)
                    calldatacopy(add(__midnight_id_tuple_ptr, 224), add(__midnight_id_collateral_offset, 32), __midnight_id_collateral_bytes)
                    let __midnight_id_abi_length := add(256, __midnight_id_collateral_bytes)
                    let __midnight_id_initcode_length := add(11, __midnight_id_abi_length)
                    let __midnight_id_inner_hash := keccak256(__midnight_id_ptr, __midnight_id_initcode_length)
                    let __midnight_id_outer_ptr := add(__midnight_id_ptr, and(add(__midnight_id_initcode_length, 31), not(31)))
                    mstore(__midnight_id_outer_ptr, shl(248, 255))
                    mstore(add(__midnight_id_outer_ptr, 1), shl(96, contractSelf))
                    mstore(add(__midnight_id_outer_ptr, 21), initialChainId)
                    mstore(add(__midnight_id_outer_ptr, 53), __midnight_id_inner_hash)
                    id := keccak256(__midnight_id_outer_ptr, 85)
                    mstore(64, add(__midnight_id_outer_ptr, 96))
                }
                let currentMarketTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if eq(currentMarketTickSpacing, 0) {
                    let now := timestamp()
                    if gt(maturity, add(now, 3153600000)) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 16)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                    let takeCollateralParamsOffset := add(marketBase, calldataload(add(marketBase, 32)))
                    let takeCollateralCount := calldataload(takeCollateralParamsOffset)
                    if iszero(gt(takeCollateralCount, 0)) {
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
                    if gt(takeCollateralCount, 128) {
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
                        let __forEach_count := takeCollateralCount
                        let i := 0
                    } lt(__forEach_idx, __forEach_count) {
                        __forEach_idx := add(__forEach_idx, 1)
                    } {
                        i := __forEach_idx
                        let collateralParamOffset := add(add(takeCollateralParamsOffset, 32), mul(i, 128))
                        let collateralToken := calldataload(collateralParamOffset)
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
                        let lltv := calldataload(add(collateralParamOffset, 32))
                        let allowed := internal_internal_isLltvAllowed(lltv)
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
                        let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                        let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                        let lif := calldataload(add(collateralParamOffset, 64))
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
                    let _marketPointer := 0
                    {
                        let __midnight_store_ptr := mload(64)
                        mstore(__midnight_store_ptr, shl(168, 0x600b380380600b5f395ff3))
                        mstore(add(__midnight_store_ptr, 11), 32)
                        let __midnight_store_tuple_ptr := add(__midnight_store_ptr, 43)
                        mstore(__midnight_store_tuple_ptr, calldataload(marketBase))
                        mstore(add(__midnight_store_tuple_ptr, 32), 192)
                        mstore(add(__midnight_store_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                        mstore(add(__midnight_store_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                        mstore(add(__midnight_store_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                        mstore(add(__midnight_store_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                        let __midnight_store_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                        let __midnight_store_collateral_length := calldataload(__midnight_store_collateral_offset)
                        let __midnight_store_collateral_bytes := mul(__midnight_store_collateral_length, 128)
                        mstore(add(__midnight_store_tuple_ptr, 192), __midnight_store_collateral_length)
                        calldatacopy(add(__midnight_store_tuple_ptr, 224), add(__midnight_store_collateral_offset, 32), __midnight_store_collateral_bytes)
                        let __midnight_store_abi_length := add(256, __midnight_store_collateral_bytes)
                        let __midnight_store_initcode_length := add(11, __midnight_store_abi_length)
                        _marketPointer := create2(0, __midnight_store_ptr, __midnight_store_initcode_length, initialChainId)
                        if iszero(_marketPointer) {
                            mstore(0, shl(224, 0x4e487b71))
                            mstore(4, 81)
                            revert(0, 36)
                        }
                        mstore(64, add(__midnight_store_ptr, and(add(__midnight_store_initcode_length, 31), not(31))))
                    }
                    {
                        let __compat_value := 4
                        let __compat_packed := and(__compat_value, 255)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                    }
                    let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(loanToken, 0)
                    let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(loanToken, 1)
                    let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(loanToken, 2)
                    let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(loanToken, 3)
                    let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(loanToken, 4)
                    let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(loanToken, 5)
                    let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(loanToken, 6)
                    let continuous := sload(mappingSlot(5, loanToken))
                    {
                        let __compat_value := settlementFeeCbp0
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp1
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp2
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp3
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp4
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp5
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                    }
                    {
                        let __compat_value := settlementFeeCbp6
                        let __compat_packed := and(__compat_value, 65535)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                    }
                    {
                        let __compat_value := continuous
                        let __compat_packed := and(__compat_value, 4294967295)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                        sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                    }
                }
                let lossFactorForGuard := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                if iszero(lt(lossFactorForGuard, 340282366920938463463374607431768211455)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d61726b65744c6f7373466163746f724d617865644f75742829000000000000)
                        let __err_hash := keccak256(__err_ptr, 26)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0))), iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12), 0))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d756c7469706c654e6f6e5a65726f2829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                if iszero(eq(mod(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), currentTickSpacing), 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x5469636b4e6f7441636365737369626c65282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let now := timestamp()
                if lt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 3)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f666665724e6f74537461727465642829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if gt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 4)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f66666572457870697265642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), taker))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x53656c6654616b65282900000000000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 10)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let ratifierAuthorized := internal_internal_isAuthorized(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10))
                if iszero(ratifierAuthorized) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x5261746966696572556e617574686f72697a6564282900000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 22)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                {
                    let __rat_ptr := mload(64)
                    let __rat_offer_ptr := add(__rat_ptr, 68)
                    let __rat_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                    let __rat_collateral_offset := add(__rat_market_offset, calldataload(add(__rat_market_offset, 32)))
                    let __rat_collateral_length := calldataload(__rat_collateral_offset)
                    let __rat_collateral_bytes := mul(__rat_collateral_length, 128)
                    let __rat_market_size := add(224, __rat_collateral_bytes)
                    let __rat_padded_market_size := and(add(__rat_market_size, 31), not(31))
                    let __rat_callback_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                    let __rat_callback_data_length := calldataload(__rat_callback_data_offset)
                    let __rat_padded_callback_data_length := and(add(__rat_callback_data_length, 31), not(31))
                    let __rat_offer_size := add(448, add(__rat_padded_market_size, add(32, __rat_padded_callback_data_length)))
                    let __rat_padded_offer_size := and(add(__rat_offer_size, 31), not(31))
                    let __rat_data_ptr := add(__rat_offer_ptr, __rat_padded_offer_size)
                    let __rat_padded_data_length := and(add(ratifierData_length, 31), not(31))
                    let __rat_total := add(68, add(__rat_padded_offer_size, add(32, __rat_padded_data_length)))
                    mstore(__rat_ptr, shl(224, 0x675ef8d3))
                    mstore(add(__rat_ptr, 4), 64)
                    mstore(add(__rat_ptr, 36), add(64, __rat_padded_offer_size))
                    mstore(__rat_offer_ptr, 448)
                    for {
                        let __rat_i := 1
                    } lt(__rat_i, 8) {
                        __rat_i := add(__rat_i, 1)
                    } {
                        mstore(add(__rat_offer_ptr, mul(__rat_i, 32)), calldataload(add(offer_data_offset, mul(__rat_i, 32))))
                    }
                    mstore(add(__rat_offer_ptr, 256), add(448, __rat_padded_market_size))
                    for {
                        let __rat_j := 9
                    } lt(__rat_j, 14) {
                        __rat_j := add(__rat_j, 1)
                    } {
                        mstore(add(__rat_offer_ptr, mul(__rat_j, 32)), calldataload(add(offer_data_offset, mul(__rat_j, 32))))
                    }
                    let __rat_market_ptr := add(__rat_offer_ptr, 448)
                    mstore(__rat_market_ptr, calldataload(__rat_market_offset))
                    mstore(add(__rat_market_ptr, 32), 192)
                    for {
                        let __rat_m := 2
                    } lt(__rat_m, 6) {
                        __rat_m := add(__rat_m, 1)
                    } {
                        mstore(add(__rat_market_ptr, mul(__rat_m, 32)), calldataload(add(__rat_market_offset, mul(__rat_m, 32))))
                    }
                    mstore(add(__rat_market_ptr, 192), __rat_collateral_length)
                    calldatacopy(add(__rat_market_ptr, 224), add(__rat_collateral_offset, 32), __rat_collateral_bytes)
                    mstore(add(__rat_offer_ptr, add(448, __rat_padded_market_size)), __rat_callback_data_length)
                    calldatacopy(add(__rat_offer_ptr, add(480, __rat_padded_market_size)), add(__rat_callback_data_offset, 32), __rat_callback_data_length)
                    mstore(add(__rat_offer_ptr, add(480, add(__rat_padded_market_size, __rat_callback_data_length))), 0)
                    mstore(__rat_data_ptr, ratifierData_length)
                    calldatacopy(add(__rat_data_ptr, 32), ratifierData_data_offset, ratifierData_length)
                    mstore(add(__rat_data_ptr, add(32, ratifierData_length)), 0)
                    mstore(64, add(__rat_ptr, and(add(__rat_total, 31), not(31))))
                    let __rat_success := staticcall(gas(), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10), __rat_ptr, __rat_total, __rat_ptr, 32)
                    if iszero(__rat_success) {
                        let __rat_rds := returndatasize()
                        returndatacopy(0, 0, __rat_rds)
                        revert(0, __rat_rds)
                    }
                    if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__rat_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                        mstore(0, shl(224, 0x9e8ec676))
                        revert(0, 4)
                    }
                }
                if gt(units, 340282366920938463463374607431768211455) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let newConsumed := 0
                {
                    let __ite_cond := gt(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0)
                    if __ite_cond {
                        let offerPriceConsumed := internal_internal_tickToPrice(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5))
                        let timeToMaturityConsumed := 0
                        if gt(maturity, now) {
                            timeToMaturityConsumed := sub(maturity, now)
                        }
                        let settlementFeeConsumed := internal_internal_settlementFee(id, timeToMaturityConsumed)
                        let sellerPriceConsumed := offerPriceConsumed
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            sellerPriceConsumed := sub(offerPriceConsumed, settlementFeeConsumed)
                        }
                        let buyerPriceConsumed := add(sellerPriceConsumed, settlementFeeConsumed)
                        let buyerAssetsConsumed := 0
                        let sellerAssetsConsumed := 0
                        {
                            let __ite_cond_2 := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                            if __ite_cond_2 {
                                buyerAssetsConsumed := div(mul(units, buyerPriceConsumed), 1000000000000000000)
                                sellerAssetsConsumed := div(mul(units, sellerPriceConsumed), 1000000000000000000)
                            }
                            if iszero(__ite_cond_2) {
                                buyerAssetsConsumed := div(add(mul(units, buyerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                                sellerAssetsConsumed := div(add(mul(units, sellerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                            }
                        }
                        if lt(buyerAssetsConsumed, sellerAssetsConsumed) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                        let consumedIncrease := sellerAssetsConsumed
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            consumedIncrease := buyerAssetsConsumed
                        }
                        newConsumed := add(currentConsumed, consumedIncrease)
                        if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                    }
                    if iszero(__ite_cond) {
                        let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                        newConsumed := add(currentConsumed, units)
                        if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436f6e73756d6564556e69747328290000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 15)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                    }
                }
                let buyer := taker
                let seller := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                    buyer := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                    seller := taker
                }
                let buyerCreditIncrease := internal_internal_updateBuyerForTake(id, buyer, units, maturity, now)
                let sellerCreditDecrease, sellerDebtIncrease, sellerPendingFeeDecrease := internal_internal_updateSellerForTake(id, seller, units, maturity, now)
                if iszero(or(iszero(iszero(iszero(gt(now, maturity)))), iszero(iszero(eq(sellerDebtIncrease, 0))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x43616e6e6f74496e63726561736544656274506f73744d617475726974792829)
                        let __err_hash := keccak256(__err_ptr, 32)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let reduceOnlyAllowed := 1
                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 11) {
                    {
                        let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                        if __ite_cond {
                            reduceOnlyAllowed := eq(buyerCreditIncrease, 0)
                        }
                        if iszero(__ite_cond) {
                            reduceOnlyAllowed := eq(sellerDebtIncrease, 0)
                        }
                    }
                }
                if iszero(reduceOnlyAllowed) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4d616b65724372656469744f7244656274496e63726561736564282900000000)
                        let __err_hash := keccak256(__err_ptr, 28)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let buyerGateAllowed := 1
                if iszero(eq(enterGate, 0)) {
                    if gt(buyerCreditIncrease, 0) {
                        let canIncreaseCredit := internal_internal_enterGateCanIncreaseCredit(enterGate, buyer)
                        buyerGateAllowed := iszero(eq(canIncreaseCredit, 0))
                    }
                }
                if iszero(buyerGateAllowed) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4275796572476174656446726f6d496e6372656173696e674372656469742829)
                        let __err_hash := keccak256(__err_ptr, 32)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let sellerGateAllowed := 1
                if iszero(eq(enterGate, 0)) {
                    if gt(sellerDebtIncrease, 0) {
                        let canIncreaseDebt := internal_internal_enterGateCanIncreaseDebt(enterGate, seller)
                        sellerGateAllowed := iszero(eq(canIncreaseDebt, 0))
                    }
                }
                if iszero(sellerGateAllowed) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x53656c6c6572476174656446726f6d496e6372656173696e6744656274282900)
                        let __err_hash := keccak256(__err_ptr, 31)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let newTotalUnits := add(currentTotalUnits, buyerCreditIncrease)
                newTotalUnits := sub(newTotalUnits, sellerCreditDecrease)
                {
                    let __compat_value := newTotalUnits
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(1, id))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let buyerAssets, sellerAssets, buyerPendingFeeIncrease := internal_internal_takeAssetsForPrice(id, units, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1), maturity, now, buyerCreditIncrease)
                if lt(buyerAssets, sellerAssets) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let claimableBefore := sload(mappingSlot(6, loanToken))
                sstore(mappingSlot(6, loanToken), add(claimableBefore, sub(buyerAssets, sellerAssets)))
                let receiver := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 9)
                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                    receiver := receiverIfTakerIsSeller
                }
                let payer := buyer
                {
                    let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                    if __ite_cond {
                    }
                    if iszero(__ite_cond) {
                        payer := sender
                    }
                }
                let buyerCallback := takerCallback
                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                    buyerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x54616b6528616464726573732c627974657333322c75696e743235362c616464)
                    mstore(add(__evt_ptr, 32), 0x726573732c616464726573732c626f6f6c2c627974657333322c75696e743235)
                    mstore(add(__evt_ptr, 64), 0x362c75696e743235362c75696e743235362c75696e743235362c75696e743235)
                    mstore(add(__evt_ptr, 96), 0x362c75696e743235362c75696e743235362c616464726573732c616464726573)
                    mstore(add(__evt_ptr, 128), 0x7329000000000000000000000000000000000000000000000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 130)
                    mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 32), units)
                    mstore(add(__evt_ptr, 64), iszero(iszero(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1))))
                    mstore(add(__evt_ptr, 96), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6))
                    mstore(add(__evt_ptr, 128), buyerAssets)
                    mstore(add(__evt_ptr, 160), sellerAssets)
                    mstore(add(__evt_ptr, 192), newConsumed)
                    mstore(add(__evt_ptr, 224), buyerPendingFeeIncrease)
                    mstore(add(__evt_ptr, 256), sellerPendingFeeDecrease)
                    mstore(add(__evt_ptr, 288), buyerCreditIncrease)
                    mstore(add(__evt_ptr, 320), sellerCreditDecrease)
                    mstore(add(__evt_ptr, 352), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 384), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                    log4(__evt_ptr, 416, __evt_topic0, id, and(taker, 0xffffffffffffffffffffffffffffffffffffffff), and(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    let __liq_lock_ptr := mload(64)
                    mstore(__liq_lock_ptr, id)
                    mstore(add(__liq_lock_ptr, 32), seller)
                    mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                    let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                    tstore(__liq_lock_slot, add(tload(__liq_lock_slot), 1))
                }
                if iszero(eq(buyerCallback, 0)) {
                    payer := buyerCallback
                    {
                        let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                        if __ite_cond {
                            {
                                let __buycb_ptr := mload(64)
                                let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                                let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                                let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                                let __buycb_market_size := add(224, __buycb_collateral_bytes)
                                let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                                let __buycb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                                let __buycb_data_length := calldataload(__buycb_data_offset)
                                let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                                let __buycb_market_ptr := add(__buycb_ptr, 228)
                                let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                                let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                                mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                                mstore(add(__buycb_ptr, 4), id)
                                mstore(add(__buycb_ptr, 36), 224)
                                mstore(add(__buycb_ptr, 68), buyerAssets)
                                mstore(add(__buycb_ptr, 100), units)
                                mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                                mstore(add(__buycb_ptr, 164), buyer)
                                mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                                mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                                mstore(add(__buycb_market_ptr, 32), 192)
                                for {
                                    let __buycb_m := 2
                                } lt(__buycb_m, 6) {
                                    __buycb_m := add(__buycb_m, 1)
                                } {
                                    mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                                }
                                mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                                calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                                mstore(__buycb_data_ptr, __buycb_data_length)
                                calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                                mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                                mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                                let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                                if iszero(__buycb_success) {
                                    let __buycb_rds := returndatasize()
                                    returndatacopy(0, 0, __buycb_rds)
                                    revert(0, __buycb_rds)
                                }
                                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                    mstore(0, shl(224, 0xa8f3eb44))
                                    revert(0, 4)
                                }
                            }
                        }
                        if iszero(__ite_cond) {
                            {
                                let __buycb_ptr := mload(64)
                                let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                                let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                                let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                                let __buycb_market_size := add(224, __buycb_collateral_bytes)
                                let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                                let __buycb_data_offset := sub(takerCallbackData_data_offset, 32)
                                let __buycb_data_length := calldataload(__buycb_data_offset)
                                let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                                let __buycb_market_ptr := add(__buycb_ptr, 228)
                                let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                                let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                                mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                                mstore(add(__buycb_ptr, 4), id)
                                mstore(add(__buycb_ptr, 36), 224)
                                mstore(add(__buycb_ptr, 68), buyerAssets)
                                mstore(add(__buycb_ptr, 100), units)
                                mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                                mstore(add(__buycb_ptr, 164), buyer)
                                mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                                mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                                mstore(add(__buycb_market_ptr, 32), 192)
                                for {
                                    let __buycb_m := 2
                                } lt(__buycb_m, 6) {
                                    __buycb_m := add(__buycb_m, 1)
                                } {
                                    mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                                }
                                mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                                calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                                mstore(__buycb_data_ptr, __buycb_data_length)
                                calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                                mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                                mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                                let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                                if iszero(__buycb_success) {
                                    let __buycb_rds := returndatasize()
                                    returndatacopy(0, 0, __buycb_rds)
                                    revert(0, __buycb_rds)
                                }
                                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                    mstore(0, shl(224, 0xa8f3eb44))
                                    revert(0, 4)
                                }
                            }
                        }
                    }
                }
                let feeAssets := sub(buyerAssets, sellerAssets)
                if gt(feeAssets, 0) {
                    let self := address()
                    {
                        if iszero(gt(extcodesize(loanToken), 0)) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 7)
                            mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                            revert(0, 100)
                        }
                        let __lstf_ptr := mload(64)
                        mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                        mstore(add(__lstf_ptr, 4), payer)
                        mstore(add(__lstf_ptr, 36), self)
                        mstore(add(__lstf_ptr, 68), feeAssets)
                        mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                        let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                        if iszero(__lstf_success) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 21)
                            mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                            revert(0, 100)
                        }
                        let __lst_rds := returndatasize()
                        if __lst_rds {
                            if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 27)
                                mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                revert(0, 100)
                            }
                        }
                    }
                }
                if gt(sellerAssets, 0) {
                    {
                        if iszero(gt(extcodesize(loanToken), 0)) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 7)
                            mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                            revert(0, 100)
                        }
                        let __lstf_ptr := mload(64)
                        mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                        mstore(add(__lstf_ptr, 4), payer)
                        mstore(add(__lstf_ptr, 36), receiver)
                        mstore(add(__lstf_ptr, 68), sellerAssets)
                        mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                        let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                        if iszero(__lstf_success) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 21)
                            mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                            revert(0, 100)
                        }
                        let __lst_rds := returndatasize()
                        if __lst_rds {
                            if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 27)
                                mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                revert(0, 100)
                            }
                        }
                    }
                }
                let sellerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                    sellerCallback := takerCallback
                }
                if iszero(eq(sellerCallback, 0)) {
                    {
                        let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                        if __ite_cond {
                            {
                                let __sellcb_ptr := mload(64)
                                let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                                let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                                let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                                let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                                let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                                let __sellcb_data_offset := sub(takerCallbackData_data_offset, 32)
                                let __sellcb_data_length := calldataload(__sellcb_data_offset)
                                let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                                let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                                let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                                let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                                mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                                mstore(add(__sellcb_ptr, 4), id)
                                mstore(add(__sellcb_ptr, 36), 256)
                                mstore(add(__sellcb_ptr, 68), sellerAssets)
                                mstore(add(__sellcb_ptr, 100), units)
                                mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                                mstore(add(__sellcb_ptr, 164), seller)
                                mstore(add(__sellcb_ptr, 196), receiver)
                                mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                                mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                                mstore(add(__sellcb_market_ptr, 32), 192)
                                for {
                                    let __sellcb_m := 2
                                } lt(__sellcb_m, 6) {
                                    __sellcb_m := add(__sellcb_m, 1)
                                } {
                                    mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                                }
                                mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                                calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                                mstore(__sellcb_data_ptr, __sellcb_data_length)
                                calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                                mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                                mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                                let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                                if iszero(__sellcb_success) {
                                    let __sellcb_rds := returndatasize()
                                    returndatacopy(0, 0, __sellcb_rds)
                                    revert(0, __sellcb_rds)
                                }
                                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                    mstore(0, shl(224, 0xa4fb7883))
                                    revert(0, 4)
                                }
                            }
                        }
                        if iszero(__ite_cond) {
                            {
                                let __sellcb_ptr := mload(64)
                                let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                                let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                                let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                                let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                                let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                                let __sellcb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                                let __sellcb_data_length := calldataload(__sellcb_data_offset)
                                let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                                let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                                let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                                let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                                mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                                mstore(add(__sellcb_ptr, 4), id)
                                mstore(add(__sellcb_ptr, 36), 256)
                                mstore(add(__sellcb_ptr, 68), sellerAssets)
                                mstore(add(__sellcb_ptr, 100), units)
                                mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                                mstore(add(__sellcb_ptr, 164), seller)
                                mstore(add(__sellcb_ptr, 196), receiver)
                                mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                                mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                                mstore(add(__sellcb_market_ptr, 32), 192)
                                for {
                                    let __sellcb_m := 2
                                } lt(__sellcb_m, 6) {
                                    __sellcb_m := add(__sellcb_m, 1)
                                } {
                                    mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                                }
                                mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                                calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                                mstore(__sellcb_data_ptr, __sellcb_data_length)
                                calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                                mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                                mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                                let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                                if iszero(__sellcb_success) {
                                    let __sellcb_rds := returndatasize()
                                    returndatacopy(0, 0, __sellcb_rds)
                                    revert(0, __sellcb_rds)
                                }
                                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                    mstore(0, shl(224, 0xa4fb7883))
                                    revert(0, 4)
                                }
                            }
                        }
                    }
                }
                let lockedAfterCallbacks := 0
                {
                    let __liq_lock_ptr := mload(64)
                    mstore(__liq_lock_ptr, id)
                    mstore(add(__liq_lock_ptr, 32), seller)
                    mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                    let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                    let __liq_lock_depth := tload(__liq_lock_slot)
                    if gt(__liq_lock_depth, 0) {
                        __liq_lock_depth := sub(__liq_lock_depth, 1)
                        tstore(__liq_lock_slot, __liq_lock_depth)
                    }
                    lockedAfterCallbacks := __liq_lock_depth
                }
                {
                    let __ite_cond := iszero(eq(lockedAfterCallbacks, 0))
                    if __ite_cond {
                    }
                    if iszero(__ite_cond) {
                        let sellerHealthy := internal_internal_isHealthy(add(offer_data_offset, calldataload(offer_data_offset)), id, seller)
                        if iszero(iszero(eq(sellerHealthy, 0))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x53656c6c657249734c6971756964617461626c65282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                    }
                }
                __ret0 := buyerAssets
                __ret1 := sellerAssets
                leave
            }
            function internal_internal_creditOf(id, user) -> __ret0 {
                let value := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_debtOf(id, user) -> __ret0 {
                let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_totalUnits(id) -> __ret0 {
                let value := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_lossFactor(id) -> __ret0 {
                let value := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_tickSpacing(id) -> __ret0 {
                let value := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                __ret0 := value
                leave
            }
            function internal_internal_settlementFeeCbps(id) -> __ret0, __ret1, __ret2, __ret3, __ret4, __ret5, __ret6 {
                let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                __ret0 := settlementFeeCbp0
                __ret1 := settlementFeeCbp1
                __ret2 := settlementFeeCbp2
                __ret3 := settlementFeeCbp3
                __ret4 := settlementFeeCbp4
                __ret5 := settlementFeeCbp5
                __ret6 := settlementFeeCbp6
                leave
            }
            function internal_internal_withdrawable(id) -> __ret0 {
                let value := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_continuousFee(id) -> __ret0 {
                let value := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                __ret0 := value
                leave
            }
            function internal_internal_continuousFeeCredit(id) -> __ret0 {
                let value := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_withdraw(market_data_offset, units, onBehalf, receiver) {
                let sender := caller()
                let authorized := internal_internal_isAuthorized(onBehalf, sender)
                if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let id := internal_internal_toId(market_data_offset)
                let creditBeforeUpdate := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                let pendingFeeBeforeUpdate := and(shr(128, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
                let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
                let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let now := timestamp()
                let postSlashCredit := 0
                if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                    postSlashCredit := div(mul(creditBeforeUpdate, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                }
                let postSlashPendingFee := 0
                if gt(creditBeforeUpdate, 0) {
                    postSlashPendingFee := sub(pendingFeeBeforeUpdate, div(add(mul(pendingFeeBeforeUpdate, sub(creditBeforeUpdate, postSlashCredit)), sub(creditBeforeUpdate, 1)), creditBeforeUpdate))
                }
                let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                    accrualEnd := now
                }
                let accrued := 0
                if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                    accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                }
                let creditAfterUpdate := sub(postSlashCredit, accrued)
                let pendingFeeAfterUpdate := sub(postSlashPendingFee, accrued)
                {
                    let __compat_value := creditAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := marketLossFactor
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __compat_value := pendingFeeAfterUpdate
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                {
                    let __compat_value := now
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(currentContinuousFeeCredit, accrued)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let creditDecrease := sub(creditBeforeUpdate, creditAfterUpdate)
                let updatePendingFeeDecrease := sub(pendingFeeBeforeUpdate, pendingFeeAfterUpdate)
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                    mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 55)
                    mstore(add(__evt_ptr, 0), creditDecrease)
                    mstore(add(__evt_ptr, 32), updatePendingFeeDecrease)
                    mstore(add(__evt_ptr, 64), accrued)
                    log3(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                let pendingFeeDecrease := 0
                if gt(creditAfterUpdate, 0) {
                    pendingFeeDecrease := div(add(mul(pendingFeeAfterUpdate, units), sub(creditAfterUpdate, 1)), creditAfterUpdate)
                }
                {
                    let __compat_value := sub(pendingFeeAfterUpdate, pendingFeeDecrease)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                    sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
                }
                let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(credit, units)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(withdrawableAmount, units)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let total := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(total, units)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(mappingSlot(1, id))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x576974686472617728616464726573732c627974657333322c75696e74323536)
                    mstore(add(__evt_ptr, 32), 0x2c616464726573732c616464726573732c75696e743235362900000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 57)
                    mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 32), units)
                    mstore(add(__evt_ptr, 64), pendingFeeDecrease)
                    log4(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), receiver)
                    mstore(add(__lst_ptr, 36), units)
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
                stop()
            }
            function internal_internal_repay(market_data_offset, units, onBehalf, callback, data_data_offset, data_length) {
                let sender := caller()
                let authorized := internal_internal_isAuthorized(onBehalf, sender)
                if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let id := internal_internal_toId(market_data_offset)
                let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := sub(debt, units)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __compat_value := add(withdrawableAmount, units)
                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                    let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                    let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                    sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                }
                let payer := sender
                if iszero(eq(callback, 0)) {
                    payer := callback
                    {
                        let __repaycb_ptr := mload(64)
                        let __repaycb_market_offset := add(4, calldataload(4))
                        let __repaycb_collateral_offset := add(__repaycb_market_offset, calldataload(add(__repaycb_market_offset, 32)))
                        let __repaycb_collateral_length := calldataload(__repaycb_collateral_offset)
                        let __repaycb_collateral_bytes := mul(__repaycb_collateral_length, 128)
                        let __repaycb_market_size := add(224, __repaycb_collateral_bytes)
                        let __repaycb_padded_market_size := and(add(__repaycb_market_size, 31), not(31))
                        let __repaycb_data_offset := sub(data_data_offset, 32)
                        let __repaycb_data_length := calldataload(__repaycb_data_offset)
                        let __repaycb_padded_data_len := and(add(__repaycb_data_length, 31), not(31))
                        let __repaycb_market_ptr := add(__repaycb_ptr, 164)
                        let __repaycb_data_ptr := add(__repaycb_market_ptr, __repaycb_padded_market_size)
                        let __repaycb_total := add(164, add(__repaycb_padded_market_size, add(32, __repaycb_padded_data_len)))
                        mstore(__repaycb_ptr, shl(224, 0xfc56f72e))
                        mstore(add(__repaycb_ptr, 4), id)
                        mstore(add(__repaycb_ptr, 36), 160)
                        mstore(add(__repaycb_ptr, 68), units)
                        mstore(add(__repaycb_ptr, 100), onBehalf)
                        mstore(add(__repaycb_ptr, 132), add(160, __repaycb_padded_market_size))
                        mstore(__repaycb_market_ptr, calldataload(__repaycb_market_offset))
                        mstore(add(__repaycb_market_ptr, 32), 192)
                        for {
                            let __repaycb_m := 2
                        } lt(__repaycb_m, 6) {
                            __repaycb_m := add(__repaycb_m, 1)
                        } {
                            mstore(add(__repaycb_market_ptr, mul(__repaycb_m, 32)), calldataload(add(__repaycb_market_offset, mul(__repaycb_m, 32))))
                        }
                        mstore(add(__repaycb_market_ptr, 192), __repaycb_collateral_length)
                        calldatacopy(add(__repaycb_market_ptr, 224), add(__repaycb_collateral_offset, 32), __repaycb_collateral_bytes)
                        mstore(__repaycb_data_ptr, __repaycb_data_length)
                        calldatacopy(add(__repaycb_data_ptr, 32), add(__repaycb_data_offset, 32), __repaycb_data_length)
                        mstore(add(__repaycb_data_ptr, add(32, __repaycb_data_length)), 0)
                        mstore(64, add(__repaycb_ptr, and(add(__repaycb_total, 31), not(31))))
                        let __repaycb_success := call(gas(), callback, 0, __repaycb_ptr, __repaycb_total, __repaycb_ptr, 32)
                        if iszero(__repaycb_success) {
                            let __repaycb_rds := returndatasize()
                            returndatacopy(0, 0, __repaycb_rds)
                            revert(0, __repaycb_rds)
                        }
                        if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__repaycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                            mstore(0, shl(224, 0x40a13da2))
                            revert(0, 4)
                        }
                    }
                }
                let self := address()
                {
                    if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), payer)
                    mstore(add(__lstf_ptr, 36), self)
                    mstore(add(__lstf_ptr, 68), units)
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
                stop()
            }
            function internal_internal_collateralTokenAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
                __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 0)
                leave
            }
            function internal_internal_collateralLltvAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
                __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 1)
                leave
            }
            function internal_internal_collateralMaxLifAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
                __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 2)
                leave
            }
            function internal_internal_collateralOracleAt(collateralParams_data_offset, collateralParams_length, index) -> __ret0 {
                __ret0 := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 3)
                leave
            }
            function internal_internal_oraclePrice(oracle) -> __ret0 {
                let price := 0
                {
                    let __oracle_ptr := mload(64)
                    mstore(__oracle_ptr, shl(224, 0xa035b1fe))
                    mstore(64, add(__oracle_ptr, 32))
                    let __oracle_success := staticcall(gas(), oracle, __oracle_ptr, 4, __oracle_ptr, 32)
                    if iszero(__oracle_success) {
                        let __oracle_rds := returndatasize()
                        returndatacopy(0, 0, __oracle_rds)
                        revert(0, __oracle_rds)
                    }
                    if iszero(eq(returndatasize(), 32)) {
                        revert(0, 0)
                    }
                    price := mload(__oracle_ptr)
                }
                __ret0 := price
                leave
            }
            function internal_internal_liquidatorGateCanLiquidate(gate, account) -> __ret0 {
                let allowed := 0
                {
                    let __ecwr_ptr := mload(64)
                    mstore(__ecwr_ptr, shl(224, 0xb9f4ff55))
                    mstore(add(__ecwr_ptr, 4), account)
                    mstore(64, add(__ecwr_ptr, 64))
                    let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                    if iszero(__ecwr_success) {
                        let __ecwr_rds := returndatasize()
                        returndatacopy(0, 0, __ecwr_rds)
                        revert(0, __ecwr_rds)
                    }
                    if lt(returndatasize(), 32) {
                        revert(0, 0)
                    }
                    allowed := mload(__ecwr_ptr)
                }
                __ret0 := allowed
                leave
            }
            function internal_internal_liquidatorGateCanLiquidateOrDefault(gate, account) -> __ret0 {
                let allowed := 1
                if iszero(eq(gate, 0)) {
                    let loaded := internal_internal_liquidatorGateCanLiquidate(gate, account)
                    allowed := loaded
                }
                __ret0 := allowed
                leave
            }
            function internal_internal_liquidationLockedValue(id, user) -> __ret0 {
                let locked := 0
                {
                    let __liq_lock_ptr := mload(64)
                    mstore(__liq_lock_ptr, id)
                    mstore(add(__liq_lock_ptr, 32), user)
                    mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                    locked := tload(keccak256(__liq_lock_ptr, 96))
                }
                __ret0 := locked
                leave
            }
            function internal_internal_liquidationLockExchange(id, user, value) -> __ret0 {
                let previous := 0
                {
                    let __liq_lock_ptr := mload(64)
                    mstore(__liq_lock_ptr, id)
                    mstore(add(__liq_lock_ptr, 32), user)
                    mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                    let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                    previous := tload(__liq_lock_slot)
                    tstore(__liq_lock_slot, value)
                }
                __ret0 := previous
                leave
            }
            function internal_internal_liquidationLocked(id, user) -> __ret0 {
                let locked := internal_internal_liquidationLockedValue(id, user)
                __ret0 := iszero(eq(locked, 0))
                leave
            }
            function internal_internal_collateralAmount(id, user, index) -> __ret0 {
                let value := 0
                if eq(index, 0) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 3))
                    value := loaded
                }
                if eq(index, 1) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 4))
                    value := loaded
                }
                if eq(index, 2) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 5))
                    value := loaded
                }
                if eq(index, 3) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 6))
                    value := loaded
                }
                if eq(index, 4) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 7))
                    value := loaded
                }
                if eq(index, 5) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 8))
                    value := loaded
                }
                if eq(index, 6) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 9))
                    value := loaded
                }
                if eq(index, 7) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 10))
                    value := loaded
                }
                if eq(index, 8) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 11))
                    value := loaded
                }
                if eq(index, 9) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 12))
                    value := loaded
                }
                if eq(index, 10) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 13))
                    value := loaded
                }
                if eq(index, 11) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 14))
                    value := loaded
                }
                if eq(index, 12) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 15))
                    value := loaded
                }
                if eq(index, 13) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 16))
                    value := loaded
                }
                if eq(index, 14) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 17))
                    value := loaded
                }
                if eq(index, 15) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 18))
                    value := loaded
                }
                if eq(index, 16) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 19))
                    value := loaded
                }
                if eq(index, 17) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 20))
                    value := loaded
                }
                if eq(index, 18) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 21))
                    value := loaded
                }
                if eq(index, 19) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 22))
                    value := loaded
                }
                if eq(index, 20) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 23))
                    value := loaded
                }
                if eq(index, 21) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 24))
                    value := loaded
                }
                if eq(index, 22) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 25))
                    value := loaded
                }
                if eq(index, 23) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 26))
                    value := loaded
                }
                if eq(index, 24) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 27))
                    value := loaded
                }
                if eq(index, 25) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 28))
                    value := loaded
                }
                if eq(index, 26) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 29))
                    value := loaded
                }
                if eq(index, 27) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 30))
                    value := loaded
                }
                if eq(index, 28) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 31))
                    value := loaded
                }
                if eq(index, 29) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 32))
                    value := loaded
                }
                if eq(index, 30) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 33))
                    value := loaded
                }
                if eq(index, 31) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 34))
                    value := loaded
                }
                if eq(index, 32) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 35))
                    value := loaded
                }
                if eq(index, 33) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 36))
                    value := loaded
                }
                if eq(index, 34) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 37))
                    value := loaded
                }
                if eq(index, 35) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 38))
                    value := loaded
                }
                if eq(index, 36) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 39))
                    value := loaded
                }
                if eq(index, 37) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 40))
                    value := loaded
                }
                if eq(index, 38) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 41))
                    value := loaded
                }
                if eq(index, 39) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 42))
                    value := loaded
                }
                if eq(index, 40) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 43))
                    value := loaded
                }
                if eq(index, 41) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 44))
                    value := loaded
                }
                if eq(index, 42) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 45))
                    value := loaded
                }
                if eq(index, 43) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 46))
                    value := loaded
                }
                if eq(index, 44) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 47))
                    value := loaded
                }
                if eq(index, 45) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 48))
                    value := loaded
                }
                if eq(index, 46) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 49))
                    value := loaded
                }
                if eq(index, 47) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 50))
                    value := loaded
                }
                if eq(index, 48) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 51))
                    value := loaded
                }
                if eq(index, 49) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 52))
                    value := loaded
                }
                if eq(index, 50) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 53))
                    value := loaded
                }
                if eq(index, 51) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 54))
                    value := loaded
                }
                if eq(index, 52) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 55))
                    value := loaded
                }
                if eq(index, 53) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 56))
                    value := loaded
                }
                if eq(index, 54) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 57))
                    value := loaded
                }
                if eq(index, 55) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 58))
                    value := loaded
                }
                if eq(index, 56) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 59))
                    value := loaded
                }
                if eq(index, 57) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 60))
                    value := loaded
                }
                if eq(index, 58) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 61))
                    value := loaded
                }
                if eq(index, 59) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 62))
                    value := loaded
                }
                if eq(index, 60) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 63))
                    value := loaded
                }
                if eq(index, 61) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 64))
                    value := loaded
                }
                if eq(index, 62) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 65))
                    value := loaded
                }
                if eq(index, 63) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 66))
                    value := loaded
                }
                if eq(index, 64) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 67))
                    value := loaded
                }
                if eq(index, 65) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 68))
                    value := loaded
                }
                if eq(index, 66) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 69))
                    value := loaded
                }
                if eq(index, 67) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 70))
                    value := loaded
                }
                if eq(index, 68) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 71))
                    value := loaded
                }
                if eq(index, 69) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 72))
                    value := loaded
                }
                if eq(index, 70) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 73))
                    value := loaded
                }
                if eq(index, 71) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 74))
                    value := loaded
                }
                if eq(index, 72) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 75))
                    value := loaded
                }
                if eq(index, 73) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 76))
                    value := loaded
                }
                if eq(index, 74) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 77))
                    value := loaded
                }
                if eq(index, 75) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 78))
                    value := loaded
                }
                if eq(index, 76) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 79))
                    value := loaded
                }
                if eq(index, 77) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 80))
                    value := loaded
                }
                if eq(index, 78) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 81))
                    value := loaded
                }
                if eq(index, 79) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 82))
                    value := loaded
                }
                if eq(index, 80) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 83))
                    value := loaded
                }
                if eq(index, 81) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 84))
                    value := loaded
                }
                if eq(index, 82) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 85))
                    value := loaded
                }
                if eq(index, 83) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 86))
                    value := loaded
                }
                if eq(index, 84) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 87))
                    value := loaded
                }
                if eq(index, 85) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 88))
                    value := loaded
                }
                if eq(index, 86) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 89))
                    value := loaded
                }
                if eq(index, 87) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 90))
                    value := loaded
                }
                if eq(index, 88) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 91))
                    value := loaded
                }
                if eq(index, 89) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 92))
                    value := loaded
                }
                if eq(index, 90) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 93))
                    value := loaded
                }
                if eq(index, 91) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 94))
                    value := loaded
                }
                if eq(index, 92) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 95))
                    value := loaded
                }
                if eq(index, 93) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 96))
                    value := loaded
                }
                if eq(index, 94) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 97))
                    value := loaded
                }
                if eq(index, 95) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 98))
                    value := loaded
                }
                if eq(index, 96) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 99))
                    value := loaded
                }
                if eq(index, 97) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 100))
                    value := loaded
                }
                if eq(index, 98) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 101))
                    value := loaded
                }
                if eq(index, 99) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 102))
                    value := loaded
                }
                if eq(index, 100) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 103))
                    value := loaded
                }
                if eq(index, 101) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 104))
                    value := loaded
                }
                if eq(index, 102) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 105))
                    value := loaded
                }
                if eq(index, 103) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 106))
                    value := loaded
                }
                if eq(index, 104) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 107))
                    value := loaded
                }
                if eq(index, 105) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 108))
                    value := loaded
                }
                if eq(index, 106) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 109))
                    value := loaded
                }
                if eq(index, 107) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 110))
                    value := loaded
                }
                if eq(index, 108) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 111))
                    value := loaded
                }
                if eq(index, 109) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 112))
                    value := loaded
                }
                if eq(index, 110) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 113))
                    value := loaded
                }
                if eq(index, 111) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 114))
                    value := loaded
                }
                if eq(index, 112) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 115))
                    value := loaded
                }
                if eq(index, 113) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 116))
                    value := loaded
                }
                if eq(index, 114) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 117))
                    value := loaded
                }
                if eq(index, 115) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 118))
                    value := loaded
                }
                if eq(index, 116) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 119))
                    value := loaded
                }
                if eq(index, 117) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 120))
                    value := loaded
                }
                if eq(index, 118) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 121))
                    value := loaded
                }
                if eq(index, 119) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 122))
                    value := loaded
                }
                if eq(index, 120) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 123))
                    value := loaded
                }
                if eq(index, 121) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 124))
                    value := loaded
                }
                if eq(index, 122) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 125))
                    value := loaded
                }
                if eq(index, 123) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 126))
                    value := loaded
                }
                if eq(index, 124) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 127))
                    value := loaded
                }
                if eq(index, 125) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 128))
                    value := loaded
                }
                if eq(index, 126) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 129))
                    value := loaded
                }
                if eq(index, 127) {
                    let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 130))
                    value := loaded
                }
                __ret0 := value
                leave
            }
            function internal_internal_liquidationDebtSnapshot(id, borrower, collateralCount, collateralBitmapValue, collateralParamsOffset, originalDebt) -> __ret0, __ret1 {
                let maxDebtValue := 0
                let badDebt := originalDebt
                for {
                    let __forEach_idx := 0
                    let __forEach_count := collateralCount
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                    if active {
                        let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                        let collateralParamOffset := add(add(collateralParamsOffset, 32), mul(i, 128))
                        let oracle := calldataload(add(collateralParamOffset, 96))
                        let price := internal_internal_oraclePrice(oracle)
                        let lltv := calldataload(add(collateralParamOffset, 32))
                        let maxLifValue := calldataload(add(collateralParamOffset, 64))
                        let collateralDebtValue := div(mul(div(mul(activeCollateral, price), 1000000000000000000000000000000000000), lltv), 1000000000000000000)
                        maxDebtValue := add(maxDebtValue, collateralDebtValue)
                        let repayable := div(add(mul(div(add(mul(activeCollateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(maxLifValue, 1)), maxLifValue)
                        {
                            let __ite_cond := gt(badDebt, repayable)
                            if __ite_cond {
                                badDebt := sub(badDebt, repayable)
                            }
                            if iszero(__ite_cond) {
                                badDebt := 0
                            }
                        }
                    }
                }
                __ret0 := maxDebtValue
                __ret1 := badDebt
                leave
            }
            function internal_internal_writeCollateralAmount(id, user, index, value) -> __ret0 {
                if eq(index, 0) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 3), value)
                }
                if eq(index, 1) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 4), value)
                }
                if eq(index, 2) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 5), value)
                }
                if eq(index, 3) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 6), value)
                }
                if eq(index, 4) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 7), value)
                }
                if eq(index, 5) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 8), value)
                }
                if eq(index, 6) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 9), value)
                }
                if eq(index, 7) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 10), value)
                }
                if eq(index, 8) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 11), value)
                }
                if eq(index, 9) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 12), value)
                }
                if eq(index, 10) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 13), value)
                }
                if eq(index, 11) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 14), value)
                }
                if eq(index, 12) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 15), value)
                }
                if eq(index, 13) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 16), value)
                }
                if eq(index, 14) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 17), value)
                }
                if eq(index, 15) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 18), value)
                }
                if eq(index, 16) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 19), value)
                }
                if eq(index, 17) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 20), value)
                }
                if eq(index, 18) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 21), value)
                }
                if eq(index, 19) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 22), value)
                }
                if eq(index, 20) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 23), value)
                }
                if eq(index, 21) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 24), value)
                }
                if eq(index, 22) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 25), value)
                }
                if eq(index, 23) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 26), value)
                }
                if eq(index, 24) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 27), value)
                }
                if eq(index, 25) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 28), value)
                }
                if eq(index, 26) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 29), value)
                }
                if eq(index, 27) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 30), value)
                }
                if eq(index, 28) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 31), value)
                }
                if eq(index, 29) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 32), value)
                }
                if eq(index, 30) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 33), value)
                }
                if eq(index, 31) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 34), value)
                }
                if eq(index, 32) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 35), value)
                }
                if eq(index, 33) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 36), value)
                }
                if eq(index, 34) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 37), value)
                }
                if eq(index, 35) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 38), value)
                }
                if eq(index, 36) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 39), value)
                }
                if eq(index, 37) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 40), value)
                }
                if eq(index, 38) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 41), value)
                }
                if eq(index, 39) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 42), value)
                }
                if eq(index, 40) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 43), value)
                }
                if eq(index, 41) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 44), value)
                }
                if eq(index, 42) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 45), value)
                }
                if eq(index, 43) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 46), value)
                }
                if eq(index, 44) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 47), value)
                }
                if eq(index, 45) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 48), value)
                }
                if eq(index, 46) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 49), value)
                }
                if eq(index, 47) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 50), value)
                }
                if eq(index, 48) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 51), value)
                }
                if eq(index, 49) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 52), value)
                }
                if eq(index, 50) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 53), value)
                }
                if eq(index, 51) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 54), value)
                }
                if eq(index, 52) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 55), value)
                }
                if eq(index, 53) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 56), value)
                }
                if eq(index, 54) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 57), value)
                }
                if eq(index, 55) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 58), value)
                }
                if eq(index, 56) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 59), value)
                }
                if eq(index, 57) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 60), value)
                }
                if eq(index, 58) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 61), value)
                }
                if eq(index, 59) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 62), value)
                }
                if eq(index, 60) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 63), value)
                }
                if eq(index, 61) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 64), value)
                }
                if eq(index, 62) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 65), value)
                }
                if eq(index, 63) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 66), value)
                }
                if eq(index, 64) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 67), value)
                }
                if eq(index, 65) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 68), value)
                }
                if eq(index, 66) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 69), value)
                }
                if eq(index, 67) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 70), value)
                }
                if eq(index, 68) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 71), value)
                }
                if eq(index, 69) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 72), value)
                }
                if eq(index, 70) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 73), value)
                }
                if eq(index, 71) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 74), value)
                }
                if eq(index, 72) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 75), value)
                }
                if eq(index, 73) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 76), value)
                }
                if eq(index, 74) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 77), value)
                }
                if eq(index, 75) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 78), value)
                }
                if eq(index, 76) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 79), value)
                }
                if eq(index, 77) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 80), value)
                }
                if eq(index, 78) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 81), value)
                }
                if eq(index, 79) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 82), value)
                }
                if eq(index, 80) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 83), value)
                }
                if eq(index, 81) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 84), value)
                }
                if eq(index, 82) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 85), value)
                }
                if eq(index, 83) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 86), value)
                }
                if eq(index, 84) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 87), value)
                }
                if eq(index, 85) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 88), value)
                }
                if eq(index, 86) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 89), value)
                }
                if eq(index, 87) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 90), value)
                }
                if eq(index, 88) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 91), value)
                }
                if eq(index, 89) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 92), value)
                }
                if eq(index, 90) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 93), value)
                }
                if eq(index, 91) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 94), value)
                }
                if eq(index, 92) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 95), value)
                }
                if eq(index, 93) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 96), value)
                }
                if eq(index, 94) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 97), value)
                }
                if eq(index, 95) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 98), value)
                }
                if eq(index, 96) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 99), value)
                }
                if eq(index, 97) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 100), value)
                }
                if eq(index, 98) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 101), value)
                }
                if eq(index, 99) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 102), value)
                }
                if eq(index, 100) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 103), value)
                }
                if eq(index, 101) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 104), value)
                }
                if eq(index, 102) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 105), value)
                }
                if eq(index, 103) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 106), value)
                }
                if eq(index, 104) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 107), value)
                }
                if eq(index, 105) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 108), value)
                }
                if eq(index, 106) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 109), value)
                }
                if eq(index, 107) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 110), value)
                }
                if eq(index, 108) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 111), value)
                }
                if eq(index, 109) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 112), value)
                }
                if eq(index, 110) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 113), value)
                }
                if eq(index, 111) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 114), value)
                }
                if eq(index, 112) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 115), value)
                }
                if eq(index, 113) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 116), value)
                }
                if eq(index, 114) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 117), value)
                }
                if eq(index, 115) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 118), value)
                }
                if eq(index, 116) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 119), value)
                }
                if eq(index, 117) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 120), value)
                }
                if eq(index, 118) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 121), value)
                }
                if eq(index, 119) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 122), value)
                }
                if eq(index, 120) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 123), value)
                }
                if eq(index, 121) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 124), value)
                }
                if eq(index, 122) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 125), value)
                }
                if eq(index, 123) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 126), value)
                }
                if eq(index, 124) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 127), value)
                }
                if eq(index, 125) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 128), value)
                }
                if eq(index, 126) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 129), value)
                }
                if eq(index, 127) {
                    sstore(add(mappingSlot(mappingSlot(0, id), user), 130), value)
                }
                __ret0 := 0
                leave
            }
            function internal_internal_liquidate(market_data_offset, collateralIndex, seizedAssets, repaidUnits, borrower, postMaturityMode, receiver, callback, data_data_offset, data_length) -> __ret0, __ret1 {
                let sender := caller()
                let id := internal_internal_toId(market_data_offset)
                let debtLoaded := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                let debt := debtLoaded
                let totalUnitsValue := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                if iszero(or(iszero(iszero(eq(seizedAssets, 0))), iszero(iszero(eq(repaidUnits, 0))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                if iszero(gt(debt, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4e6f74426f72726f776572282900000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 13)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let canLiquidate := internal_internal_liquidatorGateCanLiquidateOrDefault(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), sender)
                if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), 0))), iszero(iszero(iszero(eq(canLiquidate, 0)))))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4c697175696461746f72476174656446726f6d4c69717569646174696e672829)
                        let __err_hash := keccak256(__err_ptr, 32)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
                {
                    let __ite_cond := lt(collateralIndex, collateralCount)
                    if __ite_cond {
                    }
                    if iszero(__ite_cond) {
                        mstore(0, shl(224, 1313373041))
                        mstore(4, 50)
                        revert(0, 36)
                    }
                }
                let _collateralIndexBoundsCheck := internal_internal_collateralMaxLifAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                let marketDataOffset := add(calldataload(4), 4)
                let collateralParamsOffset := add(marketDataOffset, calldataload(add(marketDataOffset, 32)))
                let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                let collateralActive := internal_internal_collateralBitmapIsSet(collateralBitmapValue, collateralIndex)
                if or(iszero(iszero(gt(seizedAssets, 0))), iszero(iszero(gt(repaidUnits, 0)))) {
                    if iszero(collateralActive) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 19)
                        mstore(68, 0x696e61637469766520636f6c6c61746572616c00000000000000000000000000)
                        revert(0, 100)
                    }
                }
                let originalDebt := debt
                let maxDebtValue, badDebt := internal_internal_liquidationDebtSnapshot(id, borrower, collateralCount, collateralBitmapValue, collateralParamsOffset, originalDebt)
                let selectedCollateralParamOffsetForPrice := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                let selectedOracle := calldataload(add(selectedCollateralParamOffsetForPrice, 96))
                let liquidatedCollatPrice := internal_internal_oraclePrice(selectedOracle)
                let now := timestamp()
                let lockedBeforeLiquidation := internal_internal_liquidationLockedValue(id, borrower)
                if iszero(eq(lockedBeforeLiquidation, 0)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 17)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                {
                    let __ite_cond := postMaturityMode
                    if __ite_cond {
                        if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                    }
                    if iszero(__ite_cond) {
                        if iszero(gt(originalDebt, maxDebtValue)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                    }
                }
                if gt(badDebt, 0) {
                    {
                        let __compat_value := sub(debt, badDebt)
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    debt := sub(debt, badDebt)
                    let oldLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                    let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnitsValue, badDebt)), totalUnitsValue))
                    {
                        let __compat_value := newLossFactor
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(1, id))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                    {
                        let __compat_value := sub(totalUnitsValue, badDebt)
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(mappingSlot(1, id))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    let oldContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                    let newContinuousFeeCredit := 0
                    if lt(oldLossFactor, 340282366920938463463374607431768211455) {
                        newContinuousFeeCredit := div(mul(oldContinuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                    }
                    {
                        let __compat_value := newContinuousFeeCredit
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                        sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                    }
                }
                let outSeizedAssets := seizedAssets
                let outRepaidUnits := repaidUnits
                if or(iszero(iszero(gt(outRepaidUnits, 0))), iszero(iszero(gt(outSeizedAssets, 0)))) {
                    let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                    let maxLifValue := calldataload(add(selectedCollateralParamOffset, 64))
                    let lif := maxLifValue
                    if postMaturityMode {
                        let elapsed := sub(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))
                        let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLifValue, 1000000000000000000), elapsed), 900))
                        {
                            let __ite_cond := iszero(gt(maxLifValue, postMaturityLif))
                            if __ite_cond {
                                lif := maxLifValue
                            }
                            if iszero(__ite_cond) {
                                lif := postMaturityLif
                            }
                        }
                    }
                    {
                        let __ite_cond := gt(outSeizedAssets, 0)
                        if __ite_cond {
                            outRepaidUnits := div(add(mul(div(add(mul(outSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                        }
                        if iszero(__ite_cond) {
                            outSeizedAssets := div(mul(div(mul(outRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                        }
                    }
                    {
                        let __ite_cond := postMaturityMode
                        if __ite_cond {
                        }
                        if iszero(__ite_cond) {
                            let lltv := calldataload(add(selectedCollateralParamOffset, 32))
                            let maxRepaidValue := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                            if lt(lltv, 1000000000000000000) {
                                maxRepaidValue := div(add(mul(sub(debt, maxDebtValue), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                            }
                            let oldCollateralForRcf := internal_internal_collateralAmount(id, borrower, collateralIndex)
                            let collateralRepayCapacity := div(mul(div(mul(oldCollateralForRcf, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                            let capacityShortfall := 0
                            if gt(collateralRepayCapacity, maxRepaidValue) {
                                capacityShortfall := sub(collateralRepayCapacity, maxRepaidValue)
                            }
                            if iszero(or(iszero(iszero(iszero(gt(outRepaidUnits, maxRepaidValue)))), iszero(iszero(lt(capacityShortfall, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 3)))))) {
                                {
                                    let __err_ptr := mload(64)
                                    mstore(add(__err_ptr, 0), 0x5265636f76657279436c6f7365466163746f72436f6e646974696f6e7356696f)
                                    mstore(add(__err_ptr, 32), 0x6c61746564282900000000000000000000000000000000000000000000000000)
                                    let __err_hash := keccak256(__err_ptr, 39)
                                    let __err_selector := shl(224, shr(224, __err_hash))
                                    mstore(0, __err_selector)
                                    let __err_tail := 0
                                    revert(0, add(4, __err_tail))
                                }
                            }
                        }
                    }
                    let oldCollateral := internal_internal_collateralAmount(id, borrower, collateralIndex)
                    if lt(oldCollateral, outSeizedAssets) {
                        mstore(0, shl(224, 1313373041))
                        mstore(4, 17)
                        revert(0, 36)
                    }
                    let newCollateral := sub(oldCollateral, outSeizedAssets)
                    let _writeOk := internal_internal_writeCollateralAmount(id, borrower, collateralIndex, newCollateral)
                    if eq(newCollateral, 0) {
                        if gt(outSeizedAssets, 0) {
                            let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                            let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                            {
                                let __compat_value := newBitmap
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                        }
                    }
                    let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                    {
                        let __compat_value := add(withdrawableAmount, outRepaidUnits)
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                    if lt(debt, outRepaidUnits) {
                        mstore(0, shl(224, 1313373041))
                        mstore(4, 17)
                        revert(0, 36)
                    }
                    let newDebtAfterRepay := sub(debt, outRepaidUnits)
                    {
                        let __compat_value := newDebtAfterRepay
                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                        let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                        sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                    }
                }
                let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                let collateralToken := calldataload(selectedCollateralParamOffset)
                let payer := sender
                if iszero(eq(callback, 0)) {
                    payer := callback
                }
                let latestLossFactorLoaded := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                let latestContinuousFeeCreditLoaded := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                {
                    let __evt_ptr := mload(64)
                    mstore(add(__evt_ptr, 0), 0x4c697175696461746528616464726573732c627974657333322c616464726573)
                    mstore(add(__evt_ptr, 32), 0x732c75696e743235362c75696e743235362c616464726573732c626f6f6c2c61)
                    mstore(add(__evt_ptr, 64), 0x6464726573732c616464726573732c75696e743235362c75696e743235362c75)
                    mstore(add(__evt_ptr, 96), 0x696e743235362900000000000000000000000000000000000000000000000000)
                    let __evt_topic0 := keccak256(__evt_ptr, 103)
                    mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 32), outSeizedAssets)
                    mstore(add(__evt_ptr, 64), outRepaidUnits)
                    mstore(add(__evt_ptr, 96), iszero(iszero(postMaturityMode)))
                    mstore(add(__evt_ptr, 128), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 160), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                    mstore(add(__evt_ptr, 192), badDebt)
                    mstore(add(__evt_ptr, 224), add(latestLossFactorLoaded, 0))
                    mstore(add(__evt_ptr, 256), add(latestContinuousFeeCreditLoaded, 0))
                    log4(__evt_ptr, 288, __evt_topic0, id, and(collateralToken, 0xffffffffffffffffffffffffffffffffffffffff), and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
                }
                {
                    if iszero(gt(extcodesize(collateralToken), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), receiver)
                    mstore(add(__lst_ptr, 36), outSeizedAssets)
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
                if iszero(eq(callback, 0)) {
                    {
                        let __liqcb_ptr := mload(64)
                        let __liqcb_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                        let __liqcb_collateral_length := calldataload(__liqcb_collateral_offset)
                        let __liqcb_collateral_bytes := mul(__liqcb_collateral_length, 128)
                        let __liqcb_market_size := add(224, __liqcb_collateral_bytes)
                        let __liqcb_padded_market_size := and(add(__liqcb_market_size, 31), not(31))
                        let __liqcb_padded_data_len := and(add(data_length, 31), not(31))
                        let __liqcb_market_ptr := add(__liqcb_ptr, 324)
                        let __liqcb_data_ptr := add(__liqcb_market_ptr, __liqcb_padded_market_size)
                        let __liqcb_total := add(324, add(__liqcb_padded_market_size, add(32, __liqcb_padded_data_len)))
                        mstore(__liqcb_ptr, shl(224, 0x6861b795))
                        mstore(add(__liqcb_ptr, 4), sender)
                        mstore(add(__liqcb_ptr, 36), id)
                        mstore(add(__liqcb_ptr, 68), 320)
                        mstore(add(__liqcb_ptr, 100), collateralIndex)
                        mstore(add(__liqcb_ptr, 132), outSeizedAssets)
                        mstore(add(__liqcb_ptr, 164), outRepaidUnits)
                        mstore(add(__liqcb_ptr, 196), borrower)
                        mstore(add(__liqcb_ptr, 228), receiver)
                        mstore(add(__liqcb_ptr, 260), add(320, __liqcb_padded_market_size))
                        mstore(add(__liqcb_ptr, 292), badDebt)
                        mstore(__liqcb_market_ptr, calldataload(market_data_offset))
                        mstore(add(__liqcb_market_ptr, 32), 192)
                        mstore(add(__liqcb_market_ptr, 64), calldataload(add(market_data_offset, 64)))
                        mstore(add(__liqcb_market_ptr, 96), calldataload(add(market_data_offset, 96)))
                        mstore(add(__liqcb_market_ptr, 128), calldataload(add(market_data_offset, 128)))
                        mstore(add(__liqcb_market_ptr, 160), calldataload(add(market_data_offset, 160)))
                        mstore(add(__liqcb_market_ptr, 192), __liqcb_collateral_length)
                        calldatacopy(add(__liqcb_market_ptr, 224), add(__liqcb_collateral_offset, 32), __liqcb_collateral_bytes)
                        mstore(__liqcb_data_ptr, data_length)
                        calldatacopy(add(__liqcb_data_ptr, 32), data_data_offset, data_length)
                        mstore(64, add(__liqcb_ptr, and(add(__liqcb_total, 31), not(31))))
                        let __liqcb_success := call(gas(), callback, 0, __liqcb_ptr, __liqcb_total, __liqcb_ptr, 32)
                        if iszero(__liqcb_success) {
                            let __liqcb_rds := returndatasize()
                            returndatacopy(0, 0, __liqcb_rds)
                            revert(0, __liqcb_rds)
                        }
                        if lt(returndatasize(), 32) {
                            mstore(0, shl(224, 0x70b53d4b))
                            revert(0, 4)
                        }
                        if iszero(eq(mload(__liqcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                            mstore(0, shl(224, 0x70b53d4b))
                            revert(0, 4)
                        }
                    }
                }
                let self := address()
                {
                    if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), payer)
                    mstore(add(__lstf_ptr, 36), self)
                    mstore(add(__lstf_ptr, 68), outRepaidUnits)
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
                __ret0 := outSeizedAssets
                __ret1 := outRepaidUnits
                leave
            }
            function internal_internal_isHealthy(market_data_offset, id, borrower) -> __ret0 {
                let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                if eq(debt, 0) {
                    __ret0 := 1
                    leave
                }
                let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
                let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                let maxDebt := 0
                for {
                    let __forEach_idx := 0
                    let __forEach_count := collateralCount
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                    if active {
                        let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                        let oracle := internal_internal_collateralOracleAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                        let price := internal_internal_oraclePrice(oracle)
                        let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                        let collateralValue := div(mul(activeCollateral, price), 1000000000000000000000000000000000000)
                        maxDebt := add(maxDebt, div(mul(collateralValue, lltv), 1000000000000000000))
                    }
                }
                __ret0 := iszero(gt(debt, maxDebt))
                leave
            }
            function internal_internal_setCollateralAmount(id, user, index, value) {
                let _writeOk := internal_internal_writeCollateralAmount(id, user, index, value)
                stop()
            }
            function internal_internal_supplyCollateral(market_data_offset, collateralIndex, assets, onBehalf) {
                let sender := caller()
                let authorized := internal_internal_isAuthorized(onBehalf, sender)
                if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let id := internal_internal_toId(market_data_offset)
                let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
                let newCollateral := add(oldCollateral, assets)
                if gt(newCollateral, 340282366920938463463374607431768211455) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                if eq(oldCollateral, 0) {
                    if gt(assets, 0) {
                        let newBitmap := internal_internal_collateralBitmapSetBit(oldBitmap, collateralIndex)
                        {
                            let __compat_value := newBitmap
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let activeCount := internal_internal_countBits128(newBitmap)
                        if gt(activeCount, 16) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x546f6f4d616e79416374697661746564436f6c6c61746572616c732829000000)
                                let __err_hash := keccak256(__err_ptr, 29)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                    }
                }
                let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                let self := address()
                {
                    if iszero(gt(extcodesize(collateralToken), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lstf_ptr := mload(64)
                    mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lstf_ptr, 4), sender)
                    mstore(add(__lstf_ptr, 36), self)
                    mstore(add(__lstf_ptr, 68), assets)
                    mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                    let __lstf_success := call(gas(), collateralToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                    if iszero(__lstf_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 21)
                        mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 27)
                            mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                            revert(0, 100)
                        }
                    }
                }
                internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
                stop()
            }
            function internal_internal_withdrawCollateral(market_data_offset, collateralIndex, assets, onBehalf, receiver) {
                let sender := caller()
                let authorized := internal_internal_isAuthorized(onBehalf, sender)
                if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 14)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                let id := internal_internal_toId(market_data_offset)
                let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
                let newCollateral := sub(oldCollateral, assets)
                let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                if gt(debt, 0) {
                    let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                    let requiredCollateral := div(add(mul(debt, 1000000000000000000), sub(lltv, 1)), lltv)
                    if lt(newCollateral, requiredCollateral) {
                        {
                            let __err_ptr := mload(64)
                            mstore(add(__err_ptr, 0), 0x556e6865616c746879426f72726f776572282900000000000000000000000000)
                            let __err_hash := keccak256(__err_ptr, 19)
                            let __err_selector := shl(224, shr(224, __err_hash))
                            mstore(0, __err_selector)
                            let __err_tail := 0
                            revert(0, add(4, __err_tail))
                        }
                    }
                }
                if eq(newCollateral, 0) {
                    if gt(assets, 0) {
                        let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                        let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                        {
                            let __compat_value := newBitmap
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                    }
                }
                let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                {
                    if iszero(gt(extcodesize(collateralToken), 0)) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 7)
                        mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_ptr := mload(64)
                    mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                    mstore(add(__lst_ptr, 4), receiver)
                    mstore(add(__lst_ptr, 36), assets)
                    mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                    let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                    if iszero(__lst_success) {
                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                        mstore(4, 32)
                        mstore(36, 17)
                        mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                        revert(0, 100)
                    }
                    let __lst_rds := returndatasize()
                    if __lst_rds {
                        if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 23)
                            mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                            revert(0, 100)
                        }
                    }
                }
                internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
                stop()
            }
            function internal_internal_flashLoan(tokens_data_offset, tokens_length, assets_data_offset, assets_length, callback, data_data_offset, data_length) {
                let tokenCount := tokens_length
                let assetCount := assets_length
                if iszero(eq(tokenCount, assetCount)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 19)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                for {
                    let __forEach_idx := 0
                    let __forEach_count := tokenCount
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    {
                        if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 7)
                            mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                            revert(0, 100)
                        }
                        let __lst_ptr := mload(64)
                        mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                        mstore(add(__lst_ptr, 4), callback)
                        mstore(add(__lst_ptr, 36), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                        mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                        let __lst_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lst_ptr, 68, __lst_ptr, 32)
                        if iszero(__lst_success) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 17)
                            mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                            revert(0, 100)
                        }
                        let __lst_rds := returndatasize()
                        if __lst_rds {
                            if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 23)
                                mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                revert(0, 100)
                            }
                        }
                    }
                }
                let sender := caller()
                {
                    let __flcb_ptr := mload(64)
                    let __flcb_tokens_bytes := mul(tokens_length, 32)
                    let __flcb_assets_bytes := mul(assets_length, 32)
                    let __flcb_tokens_seg := add(32, __flcb_tokens_bytes)
                    let __flcb_assets_seg := add(32, __flcb_assets_bytes)
                    let __flcb_data_off := add(128, add(__flcb_tokens_seg, __flcb_assets_seg))
                    let __flcb_tokens_pos := add(__flcb_ptr, 132)
                    let __flcb_assets_pos := add(__flcb_ptr, add(132, __flcb_tokens_seg))
                    let __flcb_data_pos := add(__flcb_ptr, add(4, __flcb_data_off))
                    let __flcb_padded_data_len := and(add(data_length, 31), not(31))
                    let __flcb_total := add(add(__flcb_data_pos, 32), __flcb_padded_data_len)
                    __flcb_total := sub(__flcb_total, __flcb_ptr)
                    mstore(__flcb_ptr, shl(224, 0xd1f260c3))
                    mstore(add(__flcb_ptr, 4), sender)
                    mstore(add(__flcb_ptr, 36), 128)
                    mstore(add(__flcb_ptr, 68), add(128, __flcb_tokens_seg))
                    mstore(add(__flcb_ptr, 100), __flcb_data_off)
                    mstore(__flcb_tokens_pos, tokens_length)
                    calldatacopy(add(__flcb_tokens_pos, 32), tokens_data_offset, __flcb_tokens_bytes)
                    mstore(__flcb_assets_pos, assets_length)
                    calldatacopy(add(__flcb_assets_pos, 32), assets_data_offset, __flcb_assets_bytes)
                    mstore(__flcb_data_pos, data_length)
                    calldatacopy(add(__flcb_data_pos, 32), data_data_offset, data_length)
                    mstore(64, add(__flcb_ptr, and(add(__flcb_total, 31), not(31))))
                    let __flcb_success := call(gas(), callback, 0, __flcb_ptr, __flcb_total, __flcb_ptr, 32)
                    if iszero(__flcb_success) {
                        let __flcb_rds := returndatasize()
                        returndatacopy(0, 0, __flcb_rds)
                        revert(0, __flcb_rds)
                    }
                    if lt(returndatasize(), 32) {
                        revert(0, 0)
                    }
                    if iszero(eq(mload(__flcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                        revert(0, 0)
                    }
                }
                for {
                    let __forEach_idx := 0
                    let __forEach_count := tokenCount
                    let i := 0
                } lt(__forEach_idx, __forEach_count) {
                    __forEach_idx := add(__forEach_idx, 1)
                } {
                    i := __forEach_idx
                    let self := address()
                    {
                        if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 7)
                            mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                            revert(0, 100)
                        }
                        let __lstf_ptr := mload(64)
                        mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                        mstore(add(__lstf_ptr, 4), callback)
                        mstore(add(__lstf_ptr, 36), self)
                        mstore(add(__lstf_ptr, 68), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                        mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                        let __lstf_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                        if iszero(__lstf_success) {
                            mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                            mstore(4, 32)
                            mstore(36, 21)
                            mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                            revert(0, 100)
                        }
                        let __lst_rds := returndatasize()
                        if __lst_rds {
                            if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 27)
                                mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                revert(0, 100)
                            }
                        }
                    }
                }
                stop()
            }
            function internal_internal_collateral(id, user, index) -> __ret0 {
                let value := internal_internal_collateralAmount(id, user, index)
                __ret0 := value
                leave
            }
            function internal_internal_pendingFee(id, user) -> __ret0 {
                let value := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_lastAccrual(id, user) -> __ret0 {
                let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_lastLossFactor(id, user) -> __ret0 {
                let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal_internal_collateralBitmap(id, user) -> __ret0 {
                let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                __ret0 := value
                leave
            }
            function internal___modifier_onlyRoleSetter() {
                let sender := caller()
                let currentRoleSetter := sload(7)
                if iszero(eq(sender, currentRoleSetter)) {
                    {
                        let __err_ptr := mload(64)
                        mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                        let __err_hash := keccak256(__err_ptr, 16)
                        let __err_selector := shl(224, shr(224, __err_hash))
                        mstore(0, __err_selector)
                        let __err_tail := 0
                        revert(0, add(4, __err_tail))
                    }
                }
                stop()
            }
            function internal___modifier_onlyFeeSetter() {
                let sender := caller()
                let currentFeeSetter := sload(8)
                if iszero(eq(sender, currentFeeSetter)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 13)
                    mstore(68, 0x4f6e6c7946656553657474657200000000000000000000000000000000000000)
                    revert(0, 100)
                }
                stop()
            }
            function internal___modifier_onlyFeeClaimer() {
                let sender := caller()
                let currentFeeClaimer := sload(9)
                if iszero(eq(sender, currentFeeClaimer)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 14)
                    mstore(68, 0x4f6e6c79466565436c61696d6572000000000000000000000000000000000000)
                    revert(0, 100)
                }
                stop()
            }
            function internal___modifier_onlyTickSpacingSetter() {
                let sender := caller()
                let currentTickSpacingSetter := sload(10)
                if iszero(eq(sender, currentTickSpacingSetter)) {
                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                    mstore(4, 32)
                    mstore(36, 21)
                    mstore(68, 0x4f6e6c795469636b53706163696e675365747465720000000000000000000000)
                    revert(0, 100)
                }
                stop()
            }
            mstore(64, 128)
            {
                let __has_selector := iszero(lt(calldatasize(), 4))
                if iszero(__has_selector) {
                    revert(0, 0)
                }
                if __has_selector {
                    switch shr(224, calldataload(0))
                    case 0xac9650d8 {
                        /* multicall() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
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
                        let calls_tail_head_end := add(calls_abs_offset, 32)
                        let calls_tail_remaining := sub(calldatasize(), calls_tail_head_end)
                        if gt(calls_length, div(calls_tail_remaining, 32)) {
                            revert(0, 0)
                        }
                        let calls_data_offset := calls_tail_head_end
                        {
                            let __mc_head := calldataload(4)
                            let __mc_base := add(4, __mc_head)
                            let __mc_len := calldataload(__mc_base)
                            let __mc_i := 0
                            for {
                            } lt(__mc_i, __mc_len) {
                                __mc_i := add(__mc_i, 1)
                            } {
                                let __mc_elem_head := calldataload(add(add(__mc_base, 32), mul(__mc_i, 32)))
                                let __mc_elem := add(add(__mc_base, 32), __mc_elem_head)
                                let __mc_size := calldataload(__mc_elem)
                                let __mc_data := add(__mc_elem, 32)
                                let __mc_ptr := mload(64)
                                calldatacopy(__mc_ptr, __mc_data, __mc_size)
                                mstore(64, add(__mc_ptr, and(add(__mc_size, 31), not(31))))
                                let __mc_success := delegatecall(gas(), address(), __mc_ptr, __mc_size, 0, 0)
                                if iszero(__mc_success) {
                                    let __mc_rds := returndatasize()
                                    returndatacopy(0, 0, __mc_rds)
                                    revert(0, __mc_rds)
                                }
                            }
                        }
                        stop()
                    }
                    case 0x54f33522 {
                        /* INITIAL_CHAIN_ID() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        let cid := sload(1024)
                        mstore(0, cid)
                        return(0, 32)
                    }
                    case 0x3f6481ad {
                        /* roleSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        let value := sload(7)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x87cf3ef4 {
                        /* feeSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        let value := sload(8)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x81cbd3ea {
                        /* feeClaimer() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        let value := sload(9)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x98285e82 {
                        /* tickSpacingSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 4) {
                            revert(0, 0)
                        }
                        let value := sload(10)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xe29dfba8 {
                        /* consumed() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let user := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let group := calldataload(36)
                        let value := sload(mappingSlot(mappingSlot(2, user), group))
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x65e4ad9e {
                        /* isAuthorized() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let authorizer := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let authorized := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := sload(mappingSlot(mappingSlot(3, authorizer), authorized))
                        mstore(0, iszero(eq(value, 0)))
                        return(0, 32)
                    }
                    case 0xc18f47da {
                        /* defaultSettlementFeeCbp() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(36)
                        let value := sload(mappingSlot(mappingSlot(4, loanToken), index))
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x265d32c1 {
                        /* defaultContinuousFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := sload(mappingSlot(5, loanToken))
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x5308a18f {
                        /* claimableSettlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let token := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := sload(mappingSlot(6, token))
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x8f360502 {
                        /* maxSettlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let index := calldataload(4)
                        let value := 5000000000000000
                        if eq(index, 0) {
                            value := 14000000000000
                        }
                        if eq(index, 1) {
                            value := 14000000000000
                        }
                        if eq(index, 2) {
                            value := 98000000000000
                        }
                        if eq(index, 3) {
                            value := 417000000000000
                        }
                        if eq(index, 4) {
                            value := 1250000000000000
                        }
                        if eq(index, 5) {
                            value := 2500000000000000
                        }
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x1d8a2f16 {
                        /* isLltvAllowed() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let lltv := calldataload(4)
                        mstore(0, or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(or(iszero(iszero(eq(lltv, 385000000000000000))), iszero(iszero(eq(lltv, 625000000000000000)))))), iszero(iszero(eq(lltv, 770000000000000000)))))), iszero(iszero(eq(lltv, 860000000000000000)))))), iszero(iszero(eq(lltv, 915000000000000000)))))), iszero(iszero(eq(lltv, 945000000000000000)))))), iszero(iszero(eq(lltv, 965000000000000000)))))), iszero(iszero(eq(lltv, 980000000000000000)))))), iszero(iszero(eq(lltv, 1000000000000000000)))))
                        return(0, 32)
                    }
                    case 0xb71bd73c {
                        /* maxLif() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let lltv := calldataload(4)
                        let cursor := calldataload(36)
                        mstore(0, div(mul(1000000000000000000, 1000000000000000000), sub(1000000000000000000, div(mul(cursor, sub(1000000000000000000, lltv)), 1000000000000000000))))
                        return(0, 32)
                    }
                    case 0x7ae2b5c7 {
                        /* min() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let a := calldataload(4)
                        let b := calldataload(36)
                        {
                            let __ite_cond := lt(a, b)
                            if __ite_cond {
                                mstore(0, a)
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, b)
                                return(0, 32)
                            }
                        }
                    }
                    case 0x321b0d7f {
                        /* validateCollateralParams() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let collateralParams_offset := calldataload(4)
                        if lt(collateralParams_offset, 32) {
                            revert(0, 0)
                        }
                        let collateralParams_abs_offset := add(4, collateralParams_offset)
                        if gt(collateralParams_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let collateralParams_length := calldataload(collateralParams_abs_offset)
                        let collateralParams_tail_head_end := add(collateralParams_abs_offset, 32)
                        let collateralParams_tail_remaining := sub(calldatasize(), collateralParams_tail_head_end)
                        if gt(collateralParams_length, div(collateralParams_tail_remaining, 128)) {
                            revert(0, 0)
                        }
                        let collateralParams_data_offset := collateralParams_tail_head_end
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
                            let collateralToken := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 0)
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
                            let lltv := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 1)
                            let allowed := internal_internal_isLltvAllowed(lltv)
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
                            let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                            let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                            let lif := __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, i, 4, 2)
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
                        mstore(0, 1)
                        return(0, 32)
                    }
                    case 0x2d91f55a {
                        /* countBits128() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let bitmap := calldataload(4)
                        let count := 0
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := 128
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
                            __forEach_idx := add(__forEach_idx, 1)
                        } {
                            i := __forEach_idx
                            let mask := shl(i, 1)
                            if iszero(eq(and(bitmap, mask), 0)) {
                                count := add(count, 1)
                            }
                        }
                        mstore(0, count)
                        return(0, 32)
                    }
                    case 0x355986fa {
                        /* toId() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 32) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let id := internal_internal_codeDataMarketId(market_data_offset)
                        mstore(0, id)
                        return(0, 32)
                    }
                    case 0xfc20106c {
                        /* toMarket() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        {
                            let __midnight_market_return_pointer := and(id, 0xffffffffffffffffffffffffffffffffffffffff)
                            let __midnight_market_return_length := extcodesize(__midnight_market_return_pointer)
                            let __midnight_market_return_ptr := mload(64)
                            extcodecopy(__midnight_market_return_pointer, __midnight_market_return_ptr, 0, __midnight_market_return_length)
                            return(__midnight_market_return_ptr, __midnight_market_return_length)
                        }
                        stop()
                    }
                    case 0x93c52062 {
                        /* position() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                        let collateralBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                        mstore(0, credit)
                        mstore(32, pendingFee)
                        mstore(64, lastLossFactor)
                        mstore(96, lastAccrual)
                        mstore(128, debt)
                        mstore(160, collateralBitmap)
                        return(0, 192)
                    }
                    case 0xead29b0b {
                        /* marketState() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let totalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let lossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let withdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        let continuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                        let continuousFee := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                        let tickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        mstore(0, totalUnits)
                        mstore(32, lossFactor)
                        mstore(64, withdrawable)
                        mstore(96, continuousFeeCredit)
                        mstore(128, settlementFeeCbp0)
                        mstore(160, settlementFeeCbp1)
                        mstore(192, settlementFeeCbp2)
                        mstore(224, settlementFeeCbp3)
                        mstore(256, settlementFeeCbp4)
                        mstore(288, settlementFeeCbp5)
                        mstore(320, settlementFeeCbp6)
                        mstore(352, continuousFee)
                        mstore(384, tickSpacing)
                        return(0, 416)
                    }
                    case 0x4bfde00e {
                        /* updatePositionView() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 96) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let id := calldataload(36)
                        let user := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let now := timestamp()
                        let postSlashCredit := 0
                        if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                            postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                        }
                        let postSlashPendingFee := 0
                        if gt(credit, 0) {
                            postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                        }
                        let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                        if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                            accrualEnd := now
                        }
                        let accrued := 0
                        if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                            accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                        }
                        mstore(0, sub(postSlashCredit, accrued))
                        mstore(32, sub(postSlashPendingFee, accrued))
                        mstore(64, accrued)
                        return(0, 96)
                    }
                    case 0x03d91692 {
                        /* updatePosition() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 64) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let id := internal_internal_toId(market_data_offset)
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let pendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let now := timestamp()
                        let postSlashCredit := 0
                        if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                            postSlashCredit := div(mul(credit, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                        }
                        let postSlashPendingFee := 0
                        if gt(credit, 0) {
                            postSlashPendingFee := sub(pendingFee, div(add(mul(pendingFee, sub(credit, postSlashCredit)), sub(credit, 1)), credit))
                        }
                        let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                        if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                            accrualEnd := now
                        }
                        let accrued := 0
                        if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                            accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                        }
                        let newCredit := sub(postSlashCredit, accrued)
                        let newPendingFee := sub(postSlashPendingFee, accrued)
                        let creditDecrease := sub(credit, newCredit)
                        let pendingFeeDecrease := sub(pendingFee, newPendingFee)
                        {
                            let __compat_value := newCredit
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := marketLossFactor
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := newPendingFee
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), user))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(mappingSlot(mappingSlot(0, id), user), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        {
                            let __compat_value := now
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), user), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := add(currentContinuousFeeCredit, accrued)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                            mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 55)
                            mstore(add(__evt_ptr, 0), creditDecrease)
                            mstore(add(__evt_ptr, 32), pendingFeeDecrease)
                            mstore(add(__evt_ptr, 64), accrued)
                            log3(__evt_ptr, 96, __evt_topic0, id, and(user, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        mstore(0, newCredit)
                        mstore(32, newPendingFee)
                        mstore(64, accrued)
                        return(0, 96)
                    }
                    case 0xc2adf607 {
                        /* setRoleSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let newRoleSetter := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentRoleSetter := sload(7)
                        if iszero(eq(sender, currentRoleSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(7, and(newRoleSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                        stop()
                    }
                    case 0xb19805af {
                        /* setFeeSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let newFeeSetter := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentRoleSetter := sload(7)
                        if iszero(eq(sender, currentRoleSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(8, and(newFeeSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                        stop()
                    }
                    case 0xc3accd48 {
                        /* setFeeClaimer() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let newFeeClaimer := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentRoleSetter := sload(7)
                        if iszero(eq(sender, currentRoleSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(9, and(newFeeClaimer, 0xffffffffffffffffffffffffffffffffffffffff))
                        stop()
                    }
                    case 0xf3ad575e {
                        /* setTickSpacingSetter() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let newTickSpacingSetter := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentRoleSetter := sload(7)
                        if iszero(eq(sender, currentRoleSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79526f6c65536574746572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(10, and(newTickSpacingSetter, 0xffffffffffffffffffffffffffffffffffffffff))
                        stop()
                    }
                    case 0xb78212ab {
                        /* setIsAuthorized() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let authorized := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let newIsAuthorized := iszero(iszero(calldataload(36)))
                        let onBehalf := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
                        if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let flag := 0
                        {
                            let __ite_cond := newIsAuthorized
                            if __ite_cond {
                                flag := 1
                            }
                            if iszero(__ite_cond) {
                                flag := 0
                            }
                        }
                        sstore(mappingSlot(mappingSlot(3, onBehalf), authorized), flag)
                        stop()
                    }
                    case 0xd6359bf4 {
                        /* setConsumed() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let group := calldataload(4)
                        let amount := calldataload(36)
                        let onBehalf := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentAuth := sload(mappingSlot(mappingSlot(3, onBehalf), sender))
                        if iszero(or(iszero(iszero(eq(sender, onBehalf))), iszero(iszero(iszero(eq(currentAuth, 0)))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let current := sload(mappingSlot(mappingSlot(2, onBehalf), group))
                        if lt(amount, current) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x416c7265616479436f6e73756d65642829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(mappingSlot(2, onBehalf), group), amount)
                        stop()
                    }
                    case 0x5f59ebd8 {
                        /* setDefaultSettlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(36)
                        let newSettlementFee := calldataload(68)
                        let sender := caller()
                        let currentFeeSetter := sload(8)
                        if iszero(eq(sender, currentFeeSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 15)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(lt(index, 7)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let maxFee := 5000000000000000
                        if eq(index, 0) {
                            maxFee := 14000000000000
                        }
                        if eq(index, 1) {
                            maxFee := 14000000000000
                        }
                        if eq(index, 2) {
                            maxFee := 98000000000000
                        }
                        if eq(index, 3) {
                            maxFee := 417000000000000
                        }
                        if eq(index, 4) {
                            maxFee := 1250000000000000
                        }
                        if eq(index, 5) {
                            maxFee := 2500000000000000
                        }
                        if gt(newSettlementFee, maxFee) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                                let __err_hash := keccak256(__err_ptr, 24)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(mappingSlot(4, loanToken), index), div(newSettlementFee, 1000000000000))
                        stop()
                    }
                    case 0x21559e6b {
                        /* setDefaultContinuousFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let newContinuousFee := calldataload(36)
                        let sender := caller()
                        let currentFeeSetter := sload(8)
                        if iszero(eq(sender, currentFeeSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 15)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if gt(newContinuousFee, 317097919) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(5, loanToken), newContinuousFee)
                        stop()
                    }
                    case 0x2abdc15e {
                        /* claimSettlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let token := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let amount := calldataload(36)
                        let receiver := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let currentFeeClaimer := sload(9)
                        if iszero(eq(sender, currentFeeClaimer)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let claimable := sload(mappingSlot(6, token))
                        if gt(amount, claimable) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        sstore(mappingSlot(6, token), sub(claimable, amount))
                        {
                            if iszero(gt(extcodesize(token), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_ptr := mload(64)
                            mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lst_ptr, 4), receiver)
                            mstore(add(__lst_ptr, 36), amount)
                            mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                            let __lst_success := call(gas(), token, 0, __lst_ptr, 68, __lst_ptr, 32)
                            if iszero(__lst_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 17)
                                mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 23)
                                    mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        stop()
                    }
                    case 0x525e8201 {
                        /* claimContinuousFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 96) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let amount := calldataload(36)
                        let receiver := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let id := internal_internal_toId(market_data_offset)
                        let sender := caller()
                        let currentFeeClaimer := sload(9)
                        if iszero(eq(sender, currentFeeClaimer)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c79466565436c61696d6572282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(currentContinuousFeeCredit, amount)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(currentTotalUnits, amount)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(1, id))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let currentWithdrawable := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(currentWithdrawable, amount)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x436c61696d436f6e74696e756f757346656528616464726573732c6279746573)
                            mstore(add(__evt_ptr, 32), 0x33322c75696e743235362c616464726573732900000000000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 51)
                            mstore(add(__evt_ptr, 0), amount)
                            log4(__evt_ptr, 32, __evt_topic0, and(sender, 0xffffffffffffffffffffffffffffffffffffffff), id, and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_ptr := mload(64)
                            mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lst_ptr, 4), receiver)
                            mstore(add(__lst_ptr, 36), amount)
                            mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                            let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                            if iszero(__lst_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 17)
                                mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 23)
                                    mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        stop()
                    }
                    case 0xf37e9ba4 {
                        /* setMarketTickSpacing() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let newTickSpacing := calldataload(36)
                        let sender := caller()
                        let currentTickSpacingSetter := sload(10)
                        if iszero(eq(sender, currentTickSpacingSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c795469636b53706163696e675365747465722829000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 23)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(gt(newTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 20)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(eq(mod(currentTickSpacing, newTickSpacing), 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e76616c69645469636b53706163696e672829000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 20)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        {
                            let __compat_value := newTickSpacing
                            let __compat_packed := and(__compat_value, 255)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                            sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                        }
                        stop()
                    }
                    case 0x9e3ff40a {
                        /* setMarketSettlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let index := calldataload(36)
                        let newSettlementFee := calldataload(68)
                        let sender := caller()
                        let currentFeeSetter := sload(8)
                        if iszero(eq(sender, currentFeeSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 15)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(lt(index, 7)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e76616c6964466565496e6465782829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let maxFee := 5000000000000000
                        if eq(index, 0) {
                            maxFee := 14000000000000
                        }
                        if eq(index, 1) {
                            maxFee := 14000000000000
                        }
                        if eq(index, 2) {
                            maxFee := 98000000000000
                        }
                        if eq(index, 3) {
                            maxFee := 417000000000000
                        }
                        if eq(index, 4) {
                            maxFee := 1250000000000000
                        }
                        if eq(index, 5) {
                            maxFee := 2500000000000000
                        }
                        if gt(newSettlementFee, maxFee) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x536574746c656d656e74466565546f6f48696768282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(eq(mod(newSettlementFee, 1000000000000), 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4665654e6f744d756c7469706c654f6646656543627028290000000000000000)
                                let __err_hash := keccak256(__err_ptr, 24)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let newSettlementFeeCbp := div(newSettlementFee, 1000000000000)
                        if eq(index, 0) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                        }
                        if eq(index, 1) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                            }
                        }
                        if eq(index, 2) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                            }
                        }
                        if eq(index, 3) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                            }
                        }
                        if eq(index, 4) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                            }
                        }
                        if eq(index, 5) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                            }
                        }
                        if eq(index, 6) {
                            {
                                let __compat_value := newSettlementFeeCbp
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                            }
                        }
                        stop()
                    }
                    case 0x462e59b4 {
                        /* setMarketContinuousFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let newContinuousFee := calldataload(36)
                        let sender := caller()
                        let currentFeeSetter := sload(8)
                        if iszero(eq(sender, currentFeeSetter)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f6e6c7946656553657474657228290000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 15)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if gt(newContinuousFee, 317097919) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436f6e74696e756f7573466565546f6f48696768282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        {
                            let __compat_value := newContinuousFee
                            let __compat_packed := and(__compat_value, 4294967295)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                            sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                        }
                        stop()
                    }
                    case 0x2bfb98ac {
                        /* touchMarket() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 32) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let id := internal_internal_toId(market_data_offset)
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if eq(currentTickSpacing, 0) {
                            let now := timestamp()
                            if gt(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), add(now, 3153600000)) {
                                {
                                    let __err_ptr := mload(64)
                                    mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                                    let __err_hash := keccak256(__err_ptr, 16)
                                    let __err_selector := shl(224, shr(224, __err_hash))
                                    mstore(0, __err_selector)
                                    let __err_tail := 0
                                    revert(0, add(4, __err_tail))
                                }
                            }
                            let _collateralParamsValid := internal_internal_validateCollateralParams(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1))
                            let salt := sload(1024)
                            let _marketPointer := internal_internal_codeDataStoreMarket(market_data_offset, salt)
                            {
                                let __compat_value := 4
                                let __compat_packed := and(__compat_value, 255)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                            }
                            let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0)
                            let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 1)
                            let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 2)
                            let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 3)
                            let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 4)
                            let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 5)
                            let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 6)
                            let continuous := sload(mappingSlot(5, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)))
                            {
                                let __compat_value := settlementFeeCbp0
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp1
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp2
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp3
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp4
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp5
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp6
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                            }
                            {
                                let __compat_value := continuous
                                let __compat_packed := and(__compat_value, 4294967295)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                            }
                        }
                        mstore(0, id)
                        return(0, 32)
                    }
                    case 0xa9bef809 {
                        /* settlementFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let timeToMaturity := calldataload(36)
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(gt(currentTickSpacing, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744e6f744372656174656428290000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 18)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                        let start := 15552000
                        let finish := 31104000
                        let feeLower := mul(settlementFeeCbp5, 1000000000000)
                        let feeUpper := mul(settlementFeeCbp6, 1000000000000)
                        if lt(timeToMaturity, 86400) {
                            start := 0
                            finish := 86400
                            feeLower := mul(settlementFeeCbp0, 1000000000000)
                            feeUpper := mul(settlementFeeCbp1, 1000000000000)
                        }
                        if and(iszero(iszero(iszero(lt(timeToMaturity, 86400)))), iszero(iszero(lt(timeToMaturity, 604800)))) {
                            start := 86400
                            finish := 604800
                            feeLower := mul(settlementFeeCbp1, 1000000000000)
                            feeUpper := mul(settlementFeeCbp2, 1000000000000)
                        }
                        if and(iszero(iszero(iszero(lt(timeToMaturity, 604800)))), iszero(iszero(lt(timeToMaturity, 2592000)))) {
                            start := 604800
                            finish := 2592000
                            feeLower := mul(settlementFeeCbp2, 1000000000000)
                            feeUpper := mul(settlementFeeCbp3, 1000000000000)
                        }
                        if and(iszero(iszero(iszero(lt(timeToMaturity, 2592000)))), iszero(iszero(lt(timeToMaturity, 7776000)))) {
                            start := 2592000
                            finish := 7776000
                            feeLower := mul(settlementFeeCbp3, 1000000000000)
                            feeUpper := mul(settlementFeeCbp4, 1000000000000)
                        }
                        if and(iszero(iszero(iszero(lt(timeToMaturity, 7776000)))), iszero(iszero(lt(timeToMaturity, 15552000)))) {
                            start := 7776000
                            finish := 15552000
                            feeLower := mul(settlementFeeCbp4, 1000000000000)
                            feeUpper := mul(settlementFeeCbp5, 1000000000000)
                        }
                        {
                            let __ite_cond := iszero(lt(timeToMaturity, 31104000))
                            if __ite_cond {
                                mstore(0, mul(settlementFeeCbp6, 1000000000000))
                                return(0, 32)
                            }
                            if iszero(__ite_cond) {
                                mstore(0, div(add(mul(feeLower, sub(finish, timeToMaturity)), mul(feeUpper, sub(timeToMaturity, start))), sub(finish, start)))
                                return(0, 32)
                            }
                        }
                    }
                    case 0x2839949b {
                        /* tickToPrice() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let tick := calldataload(4)
                        if gt(tick, 5820) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x5469636b4f75744f6652616e6765282900000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 16)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let price := 0
                        {
                            let __tp_x := mul(4987541511039073, sub(2910, tick))
                            let __tp_abs := __tp_x
                            let __tp_negative := slt(__tp_x, 0)
                            if __tp_negative {
                                __tp_abs := sub(0, __tp_x)
                            }
                            let __tp_q := div(add(__tp_abs, 322611214989459870), 693147180559945309)
                            let __tp_r := sub(__tp_abs, mul(__tp_q, 693147180559945309))
                            let __tp_second := sdiv(mul(__tp_r, __tp_r), 2000000000000000000)
                            let __tp_third := sdiv(mul(__tp_second, __tp_r), 3000000000000000000)
                            let __tp_expR := add(1000000000000000000, add(__tp_r, add(__tp_second, __tp_third)))
                            let __tp_wexp := shl(__tp_q, __tp_expR)
                            if __tp_negative {
                                __tp_wexp := div(1000000000000000000000000000000000000, __tp_wexp)
                            }
                            let __tp_den := add(1000000000000000000, __tp_wexp)
                            let __tp_raw := div(add(1000000000000000000000000000000000000, div(sub(__tp_den, 1), 2)), __tp_den)
                            price := mul(div(add(__tp_raw, div(sub(1000000000000, 1), 2)), 1000000000000), 1000000000000)
                        }
                        mstore(0, price)
                        return(0, 32)
                    }
                    case 0x2e115583 {
                        /* enterGateCanIncreaseCredit() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let gate := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let account := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let allowed := 0
                        {
                            let __ecwr_ptr := mload(64)
                            mstore(__ecwr_ptr, shl(224, 0x58ac9f9e))
                            mstore(add(__ecwr_ptr, 4), account)
                            mstore(64, add(__ecwr_ptr, 64))
                            let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                            if iszero(__ecwr_success) {
                                let __ecwr_rds := returndatasize()
                                returndatacopy(0, 0, __ecwr_rds)
                                revert(0, __ecwr_rds)
                            }
                            if lt(returndatasize(), 32) {
                                revert(0, 0)
                            }
                            allowed := mload(__ecwr_ptr)
                        }
                        mstore(0, allowed)
                        return(0, 32)
                    }
                    case 0x8e985fde {
                        /* enterGateCanIncreaseDebt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let gate := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let account := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let allowed := 0
                        {
                            let __ecwr_ptr := mload(64)
                            mstore(__ecwr_ptr, shl(224, 0xfe9bf956))
                            mstore(add(__ecwr_ptr, 4), account)
                            mstore(64, add(__ecwr_ptr, 64))
                            let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                            if iszero(__ecwr_success) {
                                let __ecwr_rds := returndatasize()
                                returndatacopy(0, 0, __ecwr_rds)
                                revert(0, __ecwr_rds)
                            }
                            if lt(returndatasize(), 32) {
                                revert(0, 0)
                            }
                            allowed := mload(__ecwr_ptr)
                        }
                        mstore(0, allowed)
                        return(0, 32)
                    }
                    case 0x22402ddf {
                        /* updateBuyerForTake() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let buyer := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let units := calldataload(68)
                        let maturity := calldataload(100)
                        let now := calldataload(132)
                        let buyerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))), 340282366920938463463374607431768211455)
                        let buyerDebtDecrease := internal_internal_min(units, buyerDebt)
                        let buyerCreditIncrease := sub(units, buyerDebtDecrease)
                        let continuousFeeValue := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                        let timeToMaturity := 0
                        if gt(maturity, now) {
                            timeToMaturity := sub(maturity, now)
                        }
                        let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValue, timeToMaturity)), 1000000000000000000)
                        let buyerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
                        let buyerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), buyer))), 340282366920938463463374607431768211455)
                        let buyerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
                        let buyerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))), 340282366920938463463374607431768211455)
                        let buyerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let buyerPostSlashCredit := 0
                        if lt(buyerLastLossFactor, 340282366920938463463374607431768211455) {
                            buyerPostSlashCredit := div(mul(buyerCredit, sub(340282366920938463463374607431768211455, buyerMarketLossFactor)), sub(340282366920938463463374607431768211455, buyerLastLossFactor))
                        }
                        let buyerPostSlashPendingFee := 0
                        if gt(buyerCredit, 0) {
                            buyerPostSlashPendingFee := sub(buyerPendingFee, div(add(mul(buyerPendingFee, sub(buyerCredit, buyerPostSlashCredit)), sub(buyerCredit, 1)), buyerCredit))
                        }
                        let buyerAccrualEnd := maturity
                        if iszero(gt(now, maturity)) {
                            buyerAccrualEnd := now
                        }
                        let buyerAccrued := 0
                        if lt(buyerLastAccrual, maturity) {
                            buyerAccrued := div(mul(buyerPostSlashPendingFee, sub(buyerAccrualEnd, buyerLastAccrual)), sub(maturity, buyerLastAccrual))
                        }
                        let buyerCreditAfterUpdate := sub(buyerPostSlashCredit, buyerAccrued)
                        let buyerPendingFeeAfterUpdate := sub(buyerPostSlashPendingFee, buyerAccrued)
                        let buyerCreditDecrease := sub(buyerCredit, buyerCreditAfterUpdate)
                        let buyerPendingFeeDecreaseForUpdate := sub(buyerPendingFee, buyerPendingFeeAfterUpdate)
                        if or(iszero(iszero(gt(buyerCredit, 0))), iszero(iszero(gt(buyerCreditIncrease, 0)))) {
                            {
                                let __compat_value := buyerCreditAfterUpdate
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := buyerMarketLossFactor
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := buyerPendingFeeAfterUpdate
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                            {
                                let __compat_value := now
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(mappingSlot(0, id), buyer), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                            let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                            {
                                let __compat_value := add(currentContinuousFeeCredit, buyerAccrued)
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                            mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 55)
                            mstore(add(__evt_ptr, 0), buyerCreditDecrease)
                            mstore(add(__evt_ptr, 32), buyerPendingFeeDecreaseForUpdate)
                            mstore(add(__evt_ptr, 64), buyerAccrued)
                            log3(__evt_ptr, 96, __evt_topic0, id, and(buyer, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            let __compat_value := sub(buyerDebt, buyerDebtDecrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), buyer), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(mappingSlot(0, id), buyer), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := add(buyerPendingFeeAfterUpdate, buyerPendingFeeIncrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        {
                            let __compat_value := add(buyerCreditAfterUpdate, buyerCreditIncrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), buyer))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(mappingSlot(0, id), buyer), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        mstore(0, buyerCreditIncrease)
                        return(0, 32)
                    }
                    case 0xa37c87cb {
                        /* updateSellerForTake() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let seller := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let units := calldataload(68)
                        let maturity := calldataload(100)
                        let now := calldataload(132)
                        let sellerCredit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
                        let sellerPendingFee := and(shr(128, sload(mappingSlot(mappingSlot(0, id), seller))), 340282366920938463463374607431768211455)
                        let sellerLastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
                        let sellerLastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), seller), 1))), 340282366920938463463374607431768211455)
                        let sellerMarketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let sellerPostSlashCredit := 0
                        if lt(sellerLastLossFactor, 340282366920938463463374607431768211455) {
                            sellerPostSlashCredit := div(mul(sellerCredit, sub(340282366920938463463374607431768211455, sellerMarketLossFactor)), sub(340282366920938463463374607431768211455, sellerLastLossFactor))
                        }
                        let sellerPostSlashPendingFee := 0
                        if gt(sellerCredit, 0) {
                            sellerPostSlashPendingFee := sub(sellerPendingFee, div(add(mul(sellerPendingFee, sub(sellerCredit, sellerPostSlashCredit)), sub(sellerCredit, 1)), sellerCredit))
                        }
                        let sellerAccrualEnd := maturity
                        if iszero(gt(now, maturity)) {
                            sellerAccrualEnd := now
                        }
                        let sellerAccrued := 0
                        if lt(sellerLastAccrual, maturity) {
                            sellerAccrued := div(mul(sellerPostSlashPendingFee, sub(sellerAccrualEnd, sellerLastAccrual)), sub(maturity, sellerLastAccrual))
                        }
                        let sellerCreditAfterUpdate := sub(sellerPostSlashCredit, sellerAccrued)
                        let sellerPendingFeeAfterUpdate := sub(sellerPostSlashPendingFee, sellerAccrued)
                        let sellerCreditDecreaseForUpdate := sub(sellerCredit, sellerCreditAfterUpdate)
                        let sellerPendingFeeDecreaseForUpdate := sub(sellerPendingFee, sellerPendingFeeAfterUpdate)
                        if gt(sellerCredit, 0) {
                            {
                                let __compat_value := sellerCreditAfterUpdate
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := sellerMarketLossFactor
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := sellerPendingFeeAfterUpdate
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                            {
                                let __compat_value := now
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(mappingSlot(0, id), seller), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                            let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                            {
                                let __compat_value := add(currentContinuousFeeCredit, sellerAccrued)
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                            mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 55)
                            mstore(add(__evt_ptr, 0), sellerCreditDecreaseForUpdate)
                            mstore(add(__evt_ptr, 32), sellerPendingFeeDecreaseForUpdate)
                            mstore(add(__evt_ptr, 64), sellerAccrued)
                            log3(__evt_ptr, 96, __evt_topic0, id, and(seller, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        let sellerCreditDecrease := internal_internal_min(units, sellerCreditAfterUpdate)
                        let sellerDebtIncrease := sub(units, sellerCreditDecrease)
                        let sellerDebt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), seller), 2))), 340282366920938463463374607431768211455)
                        let sellerPendingFeeDecrease := 0
                        if gt(sellerCreditAfterUpdate, 0) {
                            sellerPendingFeeDecrease := div(add(mul(sellerPendingFeeAfterUpdate, sellerCreditDecrease), sub(sellerCreditAfterUpdate, 1)), sellerCreditAfterUpdate)
                        }
                        {
                            let __compat_value := sub(sellerPendingFeeAfterUpdate, sellerPendingFeeDecrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        {
                            let __compat_value := sub(sellerCreditAfterUpdate, sellerCreditDecrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), seller))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(mappingSlot(0, id), seller), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := add(sellerDebt, sellerDebtIncrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), seller), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(mappingSlot(0, id), seller), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        mstore(0, sellerCreditDecrease)
                        mstore(32, sellerDebtIncrease)
                        mstore(64, sellerPendingFeeDecrease)
                        return(0, 96)
                    }
                    case 0x89e05787 {
                        /* takeAssetsForPrice() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let units := calldataload(36)
                        let tick := calldataload(68)
                        let offerIsBuy := iszero(iszero(calldataload(100)))
                        let maturity := calldataload(132)
                        let now := calldataload(164)
                        let buyerCreditIncrease := calldataload(196)
                        let offerPrice := internal_internal_tickToPrice(tick)
                        let timeToMaturityForFee := 0
                        if gt(maturity, now) {
                            timeToMaturityForFee := sub(maturity, now)
                        }
                        let settlementFeeValue := internal_internal_settlementFee(id, timeToMaturityForFee)
                        let continuousFeeValueForCallback := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                        let buyerPendingFeeIncrease := div(mul(buyerCreditIncrease, mul(continuousFeeValueForCallback, timeToMaturityForFee)), 1000000000000000000)
                        let sellerPrice := offerPrice
                        if offerIsBuy {
                            sellerPrice := sub(offerPrice, settlementFeeValue)
                        }
                        let buyerPrice := add(sellerPrice, settlementFeeValue)
                        let buyerAssets := 0
                        let sellerAssets := 0
                        {
                            let __ite_cond := offerIsBuy
                            if __ite_cond {
                                buyerAssets := div(mul(units, buyerPrice), 1000000000000000000)
                                sellerAssets := div(mul(units, sellerPrice), 1000000000000000000)
                            }
                            if iszero(__ite_cond) {
                                buyerAssets := div(add(mul(units, buyerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                                sellerAssets := div(add(mul(units, sellerPrice), sub(1000000000000000000, 1)), 1000000000000000000)
                            }
                        }
                        mstore(0, buyerAssets)
                        mstore(32, sellerAssets)
                        mstore(64, buyerPendingFeeIncrease)
                        return(0, 96)
                    }
                    case 0x81a01d33 {
                        /* take() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 228) {
                            revert(0, 0)
                        }
                        let offer_offset := calldataload(4)
                        if lt(offer_offset, 224) {
                            revert(0, 0)
                        }
                        let offer_abs_offset := add(4, offer_offset)
                        if gt(offer_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let offer_data_offset := offer_abs_offset
                        let ratifierData_offset := calldataload(36)
                        if lt(ratifierData_offset, 224) {
                            revert(0, 0)
                        }
                        let ratifierData_abs_offset := add(4, ratifierData_offset)
                        if gt(ratifierData_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let ratifierData_length := calldataload(ratifierData_abs_offset)
                        let ratifierData_tail_head_end := add(ratifierData_abs_offset, 32)
                        let ratifierData_tail_remaining := sub(calldatasize(), ratifierData_tail_head_end)
                        if gt(ratifierData_length, ratifierData_tail_remaining) {
                            revert(0, 0)
                        }
                        let ratifierData_data_offset := ratifierData_tail_head_end
                        let units := calldataload(68)
                        let taker := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let receiverIfTakerIsSeller := and(calldataload(132), 0xffffffffffffffffffffffffffffffffffffffff)
                        let takerCallback := and(calldataload(164), 0xffffffffffffffffffffffffffffffffffffffff)
                        let takerCallbackData_offset := calldataload(196)
                        if lt(takerCallbackData_offset, 224) {
                            revert(0, 0)
                        }
                        let takerCallbackData_abs_offset := add(4, takerCallbackData_offset)
                        if gt(takerCallbackData_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let takerCallbackData_length := calldataload(takerCallbackData_abs_offset)
                        let takerCallbackData_tail_head_end := add(takerCallbackData_abs_offset, 32)
                        let takerCallbackData_tail_remaining := sub(calldatasize(), takerCallbackData_tail_head_end)
                        if gt(takerCallbackData_length, takerCallbackData_tail_remaining) {
                            revert(0, 0)
                        }
                        let takerCallbackData_data_offset := takerCallbackData_tail_head_end
                        let sender := caller()
                        let authorized := internal_internal_isAuthorized(taker, sender)
                        if iszero(or(iszero(iszero(eq(taker, sender))), iszero(iszero(authorized)))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x54616b6572556e617574686f72697a6564282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let offerBase := add(calldataload(4), 4)
                        let marketBase := add(offerBase, calldataload(offerBase))
                        let loanToken := calldataload(marketBase)
                        let maturity := calldataload(add(marketBase, 64))
                        let enterGate := calldataload(add(marketBase, 128))
                        let initialChainId := sload(1024)
                        let contractSelf := address()
                        let id := 0
                        {
                            let __midnight_id_ptr := mload(64)
                            mstore(__midnight_id_ptr, shl(168, 0x600b380380600b5f395ff3))
                            mstore(add(__midnight_id_ptr, 11), 32)
                            let __midnight_id_tuple_ptr := add(__midnight_id_ptr, 43)
                            mstore(__midnight_id_tuple_ptr, calldataload(marketBase))
                            mstore(add(__midnight_id_tuple_ptr, 32), 192)
                            mstore(add(__midnight_id_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                            mstore(add(__midnight_id_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                            mstore(add(__midnight_id_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                            mstore(add(__midnight_id_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                            let __midnight_id_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                            let __midnight_id_collateral_length := calldataload(__midnight_id_collateral_offset)
                            let __midnight_id_collateral_bytes := mul(__midnight_id_collateral_length, 128)
                            mstore(add(__midnight_id_tuple_ptr, 192), __midnight_id_collateral_length)
                            calldatacopy(add(__midnight_id_tuple_ptr, 224), add(__midnight_id_collateral_offset, 32), __midnight_id_collateral_bytes)
                            let __midnight_id_abi_length := add(256, __midnight_id_collateral_bytes)
                            let __midnight_id_initcode_length := add(11, __midnight_id_abi_length)
                            let __midnight_id_inner_hash := keccak256(__midnight_id_ptr, __midnight_id_initcode_length)
                            let __midnight_id_outer_ptr := add(__midnight_id_ptr, and(add(__midnight_id_initcode_length, 31), not(31)))
                            mstore(__midnight_id_outer_ptr, shl(248, 255))
                            mstore(add(__midnight_id_outer_ptr, 1), shl(96, contractSelf))
                            mstore(add(__midnight_id_outer_ptr, 21), initialChainId)
                            mstore(add(__midnight_id_outer_ptr, 53), __midnight_id_inner_hash)
                            id := keccak256(__midnight_id_outer_ptr, 85)
                            mstore(64, add(__midnight_id_outer_ptr, 96))
                        }
                        let currentMarketTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if eq(currentMarketTickSpacing, 0) {
                            let now := timestamp()
                            if gt(maturity, add(now, 3153600000)) {
                                {
                                    let __err_ptr := mload(64)
                                    mstore(add(__err_ptr, 0), 0x4d61747572697479546f6f466172282900000000000000000000000000000000)
                                    let __err_hash := keccak256(__err_ptr, 16)
                                    let __err_selector := shl(224, shr(224, __err_hash))
                                    mstore(0, __err_selector)
                                    let __err_tail := 0
                                    revert(0, add(4, __err_tail))
                                }
                            }
                            let takeCollateralParamsOffset := add(marketBase, calldataload(add(marketBase, 32)))
                            let takeCollateralCount := calldataload(takeCollateralParamsOffset)
                            if iszero(gt(takeCollateralCount, 0)) {
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
                            if gt(takeCollateralCount, 128) {
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
                                let __forEach_count := takeCollateralCount
                                let i := 0
                            } lt(__forEach_idx, __forEach_count) {
                                __forEach_idx := add(__forEach_idx, 1)
                            } {
                                i := __forEach_idx
                                let collateralParamOffset := add(add(takeCollateralParamsOffset, 32), mul(i, 128))
                                let collateralToken := calldataload(collateralParamOffset)
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
                                let lltv := calldataload(add(collateralParamOffset, 32))
                                let allowed := internal_internal_isLltvAllowed(lltv)
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
                                let lowMaxLif := internal_internal_maxLif(lltv, 250000000000000000)
                                let highMaxLif := internal_internal_maxLif(lltv, 500000000000000000)
                                let lif := calldataload(add(collateralParamOffset, 64))
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
                            let _marketPointer := 0
                            {
                                let __midnight_store_ptr := mload(64)
                                mstore(__midnight_store_ptr, shl(168, 0x600b380380600b5f395ff3))
                                mstore(add(__midnight_store_ptr, 11), 32)
                                let __midnight_store_tuple_ptr := add(__midnight_store_ptr, 43)
                                mstore(__midnight_store_tuple_ptr, calldataload(marketBase))
                                mstore(add(__midnight_store_tuple_ptr, 32), 192)
                                mstore(add(__midnight_store_tuple_ptr, 64), calldataload(add(marketBase, 64)))
                                mstore(add(__midnight_store_tuple_ptr, 96), calldataload(add(marketBase, 96)))
                                mstore(add(__midnight_store_tuple_ptr, 128), calldataload(add(marketBase, 128)))
                                mstore(add(__midnight_store_tuple_ptr, 160), calldataload(add(marketBase, 160)))
                                let __midnight_store_collateral_offset := add(marketBase, calldataload(add(marketBase, 32)))
                                let __midnight_store_collateral_length := calldataload(__midnight_store_collateral_offset)
                                let __midnight_store_collateral_bytes := mul(__midnight_store_collateral_length, 128)
                                mstore(add(__midnight_store_tuple_ptr, 192), __midnight_store_collateral_length)
                                calldatacopy(add(__midnight_store_tuple_ptr, 224), add(__midnight_store_collateral_offset, 32), __midnight_store_collateral_bytes)
                                let __midnight_store_abi_length := add(256, __midnight_store_collateral_bytes)
                                let __midnight_store_initcode_length := add(11, __midnight_store_abi_length)
                                _marketPointer := create2(0, __midnight_store_ptr, __midnight_store_initcode_length, initialChainId)
                                if iszero(_marketPointer) {
                                    mstore(0, shl(224, 0x4e487b71))
                                    mstore(4, 81)
                                    revert(0, 36)
                                }
                                mstore(64, add(__midnight_store_ptr, and(add(__midnight_store_initcode_length, 31), not(31))))
                            }
                            {
                                let __compat_value := 4
                                let __compat_packed := and(__compat_value, 255)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(5686690025625308901091608159525332184025006080))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(144, __compat_packed)))
                            }
                            let settlementFeeCbp0 := internal_internal_defaultSettlementFeeCbp(loanToken, 0)
                            let settlementFeeCbp1 := internal_internal_defaultSettlementFeeCbp(loanToken, 1)
                            let settlementFeeCbp2 := internal_internal_defaultSettlementFeeCbp(loanToken, 2)
                            let settlementFeeCbp3 := internal_internal_defaultSettlementFeeCbp(loanToken, 3)
                            let settlementFeeCbp4 := internal_internal_defaultSettlementFeeCbp(loanToken, 4)
                            let settlementFeeCbp5 := internal_internal_defaultSettlementFeeCbp(loanToken, 5)
                            let settlementFeeCbp6 := internal_internal_defaultSettlementFeeCbp(loanToken, 6)
                            let continuous := sload(mappingSlot(5, loanToken))
                            {
                                let __compat_value := settlementFeeCbp0
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(65535))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp1
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(4294901760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(16, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp2
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(281470681743360))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(32, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp3
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(18446462598732840960))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(48, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp4
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(1208907372870555465154560))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(64, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp5
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(79226953588444722964369244160))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(80, __compat_packed)))
                            }
                            {
                                let __compat_value := settlementFeeCbp6
                                let __compat_packed := and(__compat_value, 65535)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(5192217630372313364192902785269760))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(96, __compat_packed)))
                            }
                            {
                                let __compat_value := continuous
                                let __compat_packed := and(__compat_value, 4294967295)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(22300745193338326283000890644117865176760320))
                                sstore(add(mappingSlot(1, id), 2), or(__compat_slot_cleared, shl(112, __compat_packed)))
                            }
                        }
                        let lossFactorForGuard := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        if iszero(lt(lossFactorForGuard, 340282366920938463463374607431768211455)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d61726b65744c6f7373466163746f724d617865644f75742829000000000000)
                                let __err_hash := keccak256(__err_ptr, 26)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0))), iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12), 0))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d756c7469706c654e6f6e5a65726f2829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTickSpacing := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        if iszero(eq(mod(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), currentTickSpacing), 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x5469636b4e6f7441636365737369626c65282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let now := timestamp()
                        if lt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 3)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f666665724e6f74537461727465642829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if gt(now, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 4)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4f66666572457870697265642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), taker))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x53656c6654616b65282900000000000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 10)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let ratifierAuthorized := internal_internal_isAuthorized(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10))
                        if iszero(ratifierAuthorized) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x5261746966696572556e617574686f72697a6564282900000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 22)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        {
                            let __rat_ptr := mload(64)
                            let __rat_offer_ptr := add(__rat_ptr, 68)
                            let __rat_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                            let __rat_collateral_offset := add(__rat_market_offset, calldataload(add(__rat_market_offset, 32)))
                            let __rat_collateral_length := calldataload(__rat_collateral_offset)
                            let __rat_collateral_bytes := mul(__rat_collateral_length, 128)
                            let __rat_market_size := add(224, __rat_collateral_bytes)
                            let __rat_padded_market_size := and(add(__rat_market_size, 31), not(31))
                            let __rat_callback_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                            let __rat_callback_data_length := calldataload(__rat_callback_data_offset)
                            let __rat_padded_callback_data_length := and(add(__rat_callback_data_length, 31), not(31))
                            let __rat_offer_size := add(448, add(__rat_padded_market_size, add(32, __rat_padded_callback_data_length)))
                            let __rat_padded_offer_size := and(add(__rat_offer_size, 31), not(31))
                            let __rat_data_ptr := add(__rat_offer_ptr, __rat_padded_offer_size)
                            let __rat_padded_data_length := and(add(ratifierData_length, 31), not(31))
                            let __rat_total := add(68, add(__rat_padded_offer_size, add(32, __rat_padded_data_length)))
                            mstore(__rat_ptr, shl(224, 0x675ef8d3))
                            mstore(add(__rat_ptr, 4), 64)
                            mstore(add(__rat_ptr, 36), add(64, __rat_padded_offer_size))
                            mstore(__rat_offer_ptr, 448)
                            for {
                                let __rat_i := 1
                            } lt(__rat_i, 8) {
                                __rat_i := add(__rat_i, 1)
                            } {
                                mstore(add(__rat_offer_ptr, mul(__rat_i, 32)), calldataload(add(offer_data_offset, mul(__rat_i, 32))))
                            }
                            mstore(add(__rat_offer_ptr, 256), add(448, __rat_padded_market_size))
                            for {
                                let __rat_j := 9
                            } lt(__rat_j, 14) {
                                __rat_j := add(__rat_j, 1)
                            } {
                                mstore(add(__rat_offer_ptr, mul(__rat_j, 32)), calldataload(add(offer_data_offset, mul(__rat_j, 32))))
                            }
                            let __rat_market_ptr := add(__rat_offer_ptr, 448)
                            mstore(__rat_market_ptr, calldataload(__rat_market_offset))
                            mstore(add(__rat_market_ptr, 32), 192)
                            for {
                                let __rat_m := 2
                            } lt(__rat_m, 6) {
                                __rat_m := add(__rat_m, 1)
                            } {
                                mstore(add(__rat_market_ptr, mul(__rat_m, 32)), calldataload(add(__rat_market_offset, mul(__rat_m, 32))))
                            }
                            mstore(add(__rat_market_ptr, 192), __rat_collateral_length)
                            calldatacopy(add(__rat_market_ptr, 224), add(__rat_collateral_offset, 32), __rat_collateral_bytes)
                            mstore(add(__rat_offer_ptr, add(448, __rat_padded_market_size)), __rat_callback_data_length)
                            calldatacopy(add(__rat_offer_ptr, add(480, __rat_padded_market_size)), add(__rat_callback_data_offset, 32), __rat_callback_data_length)
                            mstore(add(__rat_offer_ptr, add(480, add(__rat_padded_market_size, __rat_callback_data_length))), 0)
                            mstore(__rat_data_ptr, ratifierData_length)
                            calldatacopy(add(__rat_data_ptr, 32), ratifierData_data_offset, ratifierData_length)
                            mstore(add(__rat_data_ptr, add(32, ratifierData_length)), 0)
                            mstore(64, add(__rat_ptr, and(add(__rat_total, 31), not(31))))
                            let __rat_success := staticcall(gas(), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 10), __rat_ptr, __rat_total, __rat_ptr, 32)
                            if iszero(__rat_success) {
                                let __rat_rds := returndatasize()
                                returndatacopy(0, 0, __rat_rds)
                                revert(0, __rat_rds)
                            }
                            if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__rat_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                mstore(0, shl(224, 0x9e8ec676))
                                revert(0, 4)
                            }
                        }
                        if gt(units, 340282366920938463463374607431768211455) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let newConsumed := 0
                        {
                            let __ite_cond := gt(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13), 0)
                            if __ite_cond {
                                let offerPriceConsumed := internal_internal_tickToPrice(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5))
                                let timeToMaturityConsumed := 0
                                if gt(maturity, now) {
                                    timeToMaturityConsumed := sub(maturity, now)
                                }
                                let settlementFeeConsumed := internal_internal_settlementFee(id, timeToMaturityConsumed)
                                let sellerPriceConsumed := offerPriceConsumed
                                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                                    sellerPriceConsumed := sub(offerPriceConsumed, settlementFeeConsumed)
                                }
                                let buyerPriceConsumed := add(sellerPriceConsumed, settlementFeeConsumed)
                                let buyerAssetsConsumed := 0
                                let sellerAssetsConsumed := 0
                                {
                                    let __ite_cond_3 := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                                    if __ite_cond_3 {
                                        buyerAssetsConsumed := div(mul(units, buyerPriceConsumed), 1000000000000000000)
                                        sellerAssetsConsumed := div(mul(units, sellerPriceConsumed), 1000000000000000000)
                                    }
                                    if iszero(__ite_cond_3) {
                                        buyerAssetsConsumed := div(add(mul(units, buyerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                                        sellerAssetsConsumed := div(add(mul(units, sellerPriceConsumed), sub(1000000000000000000, 1)), 1000000000000000000)
                                    }
                                }
                                if lt(buyerAssetsConsumed, sellerAssetsConsumed) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 19)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                                let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                                let consumedIncrease := sellerAssetsConsumed
                                if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                                    consumedIncrease := buyerAssetsConsumed
                                }
                                newConsumed := add(currentConsumed, consumedIncrease)
                                if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 13)) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x436f6e73756d6564417373657473282900000000000000000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 16)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                                sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                            }
                            if iszero(__ite_cond) {
                                let currentConsumed := sload(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)))
                                newConsumed := add(currentConsumed, units)
                                if gt(newConsumed, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 12)) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x436f6e73756d6564556e69747328290000000000000000000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 15)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                                sstore(mappingSlot(mappingSlot(2, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6)), newConsumed)
                            }
                        }
                        let buyer := taker
                        let seller := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            buyer := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2)
                            seller := taker
                        }
                        let buyerCreditIncrease := internal_internal_updateBuyerForTake(id, buyer, units, maturity, now)
                        let sellerCreditDecrease, sellerDebtIncrease, sellerPendingFeeDecrease := internal_internal_updateSellerForTake(id, seller, units, maturity, now)
                        if iszero(or(iszero(iszero(iszero(gt(now, maturity)))), iszero(iszero(eq(sellerDebtIncrease, 0))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x43616e6e6f74496e63726561736544656274506f73744d617475726974792829)
                                let __err_hash := keccak256(__err_ptr, 32)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let reduceOnlyAllowed := 1
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 11) {
                            {
                                let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                                if __ite_cond {
                                    reduceOnlyAllowed := eq(buyerCreditIncrease, 0)
                                }
                                if iszero(__ite_cond) {
                                    reduceOnlyAllowed := eq(sellerDebtIncrease, 0)
                                }
                            }
                        }
                        if iszero(reduceOnlyAllowed) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4d616b65724372656469744f7244656274496e63726561736564282900000000)
                                let __err_hash := keccak256(__err_ptr, 28)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let buyerGateAllowed := 1
                        if iszero(eq(enterGate, 0)) {
                            if gt(buyerCreditIncrease, 0) {
                                let canIncreaseCredit := internal_internal_enterGateCanIncreaseCredit(enterGate, buyer)
                                buyerGateAllowed := iszero(eq(canIncreaseCredit, 0))
                            }
                        }
                        if iszero(buyerGateAllowed) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4275796572476174656446726f6d496e6372656173696e674372656469742829)
                                let __err_hash := keccak256(__err_ptr, 32)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let sellerGateAllowed := 1
                        if iszero(eq(enterGate, 0)) {
                            if gt(sellerDebtIncrease, 0) {
                                let canIncreaseDebt := internal_internal_enterGateCanIncreaseDebt(enterGate, seller)
                                sellerGateAllowed := iszero(eq(canIncreaseDebt, 0))
                            }
                        }
                        if iszero(sellerGateAllowed) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x53656c6c6572476174656446726f6d496e6372656173696e6744656274282900)
                                let __err_hash := keccak256(__err_ptr, 31)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let currentTotalUnits := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let newTotalUnits := add(currentTotalUnits, buyerCreditIncrease)
                        newTotalUnits := sub(newTotalUnits, sellerCreditDecrease)
                        {
                            let __compat_value := newTotalUnits
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(1, id))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let buyerAssets, sellerAssets, buyerPendingFeeIncrease := internal_internal_takeAssetsForPrice(id, units, __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 5), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1), maturity, now, buyerCreditIncrease)
                        if lt(buyerAssets, sellerAssets) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let claimableBefore := sload(mappingSlot(6, loanToken))
                        sstore(mappingSlot(6, loanToken), add(claimableBefore, sub(buyerAssets, sellerAssets)))
                        let receiver := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 9)
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            receiver := receiverIfTakerIsSeller
                        }
                        let payer := buyer
                        {
                            let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                            if __ite_cond {
                            }
                            if iszero(__ite_cond) {
                                payer := sender
                            }
                        }
                        let buyerCallback := takerCallback
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            buyerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x54616b6528616464726573732c627974657333322c75696e743235362c616464)
                            mstore(add(__evt_ptr, 32), 0x726573732c616464726573732c626f6f6c2c627974657333322c75696e743235)
                            mstore(add(__evt_ptr, 64), 0x362c75696e743235362c75696e743235362c75696e743235362c75696e743235)
                            mstore(add(__evt_ptr, 96), 0x362c75696e743235362c75696e743235362c616464726573732c616464726573)
                            mstore(add(__evt_ptr, 128), 0x7329000000000000000000000000000000000000000000000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 130)
                            mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 32), units)
                            mstore(add(__evt_ptr, 64), iszero(iszero(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1))))
                            mstore(add(__evt_ptr, 96), __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 6))
                            mstore(add(__evt_ptr, 128), buyerAssets)
                            mstore(add(__evt_ptr, 160), sellerAssets)
                            mstore(add(__evt_ptr, 192), newConsumed)
                            mstore(add(__evt_ptr, 224), buyerPendingFeeIncrease)
                            mstore(add(__evt_ptr, 256), sellerPendingFeeDecrease)
                            mstore(add(__evt_ptr, 288), buyerCreditIncrease)
                            mstore(add(__evt_ptr, 320), sellerCreditDecrease)
                            mstore(add(__evt_ptr, 352), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 384), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                            log4(__evt_ptr, 416, __evt_topic0, id, and(taker, 0xffffffffffffffffffffffffffffffffffffffff), and(__verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 2), 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            let __liq_lock_ptr := mload(64)
                            mstore(__liq_lock_ptr, id)
                            mstore(add(__liq_lock_ptr, 32), seller)
                            mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                            let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                            tstore(__liq_lock_slot, add(tload(__liq_lock_slot), 1))
                        }
                        if iszero(eq(buyerCallback, 0)) {
                            payer := buyerCallback
                            {
                                let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                                if __ite_cond {
                                    {
                                        let __buycb_ptr := mload(64)
                                        let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                        let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                                        let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                                        let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                                        let __buycb_market_size := add(224, __buycb_collateral_bytes)
                                        let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                                        let __buycb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                                        let __buycb_data_length := calldataload(__buycb_data_offset)
                                        let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                                        let __buycb_market_ptr := add(__buycb_ptr, 228)
                                        let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                                        let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                                        mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                                        mstore(add(__buycb_ptr, 4), id)
                                        mstore(add(__buycb_ptr, 36), 224)
                                        mstore(add(__buycb_ptr, 68), buyerAssets)
                                        mstore(add(__buycb_ptr, 100), units)
                                        mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                                        mstore(add(__buycb_ptr, 164), buyer)
                                        mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                                        mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                                        mstore(add(__buycb_market_ptr, 32), 192)
                                        for {
                                            let __buycb_m := 2
                                        } lt(__buycb_m, 6) {
                                            __buycb_m := add(__buycb_m, 1)
                                        } {
                                            mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                                        }
                                        mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                                        calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                                        mstore(__buycb_data_ptr, __buycb_data_length)
                                        calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                                        mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                                        mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                                        let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                                        if iszero(__buycb_success) {
                                            let __buycb_rds := returndatasize()
                                            returndatacopy(0, 0, __buycb_rds)
                                            revert(0, __buycb_rds)
                                        }
                                        if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                            mstore(0, shl(224, 0xa8f3eb44))
                                            revert(0, 4)
                                        }
                                    }
                                }
                                if iszero(__ite_cond) {
                                    {
                                        let __buycb_ptr := mload(64)
                                        let __buycb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                        let __buycb_collateral_offset := add(__buycb_market_offset, calldataload(add(__buycb_market_offset, 32)))
                                        let __buycb_collateral_length := calldataload(__buycb_collateral_offset)
                                        let __buycb_collateral_bytes := mul(__buycb_collateral_length, 128)
                                        let __buycb_market_size := add(224, __buycb_collateral_bytes)
                                        let __buycb_padded_market_size := and(add(__buycb_market_size, 31), not(31))
                                        let __buycb_data_offset := sub(takerCallbackData_data_offset, 32)
                                        let __buycb_data_length := calldataload(__buycb_data_offset)
                                        let __buycb_padded_data_len := and(add(__buycb_data_length, 31), not(31))
                                        let __buycb_market_ptr := add(__buycb_ptr, 228)
                                        let __buycb_data_ptr := add(__buycb_market_ptr, __buycb_padded_market_size)
                                        let __buycb_total := add(228, add(__buycb_padded_market_size, add(32, __buycb_padded_data_len)))
                                        mstore(__buycb_ptr, shl(224, 0xf151bd5c))
                                        mstore(add(__buycb_ptr, 4), id)
                                        mstore(add(__buycb_ptr, 36), 224)
                                        mstore(add(__buycb_ptr, 68), buyerAssets)
                                        mstore(add(__buycb_ptr, 100), units)
                                        mstore(add(__buycb_ptr, 132), buyerPendingFeeIncrease)
                                        mstore(add(__buycb_ptr, 164), buyer)
                                        mstore(add(__buycb_ptr, 196), add(224, __buycb_padded_market_size))
                                        mstore(__buycb_market_ptr, calldataload(__buycb_market_offset))
                                        mstore(add(__buycb_market_ptr, 32), 192)
                                        for {
                                            let __buycb_m := 2
                                        } lt(__buycb_m, 6) {
                                            __buycb_m := add(__buycb_m, 1)
                                        } {
                                            mstore(add(__buycb_market_ptr, mul(__buycb_m, 32)), calldataload(add(__buycb_market_offset, mul(__buycb_m, 32))))
                                        }
                                        mstore(add(__buycb_market_ptr, 192), __buycb_collateral_length)
                                        calldatacopy(add(__buycb_market_ptr, 224), add(__buycb_collateral_offset, 32), __buycb_collateral_bytes)
                                        mstore(__buycb_data_ptr, __buycb_data_length)
                                        calldatacopy(add(__buycb_data_ptr, 32), add(__buycb_data_offset, 32), __buycb_data_length)
                                        mstore(add(__buycb_data_ptr, add(32, __buycb_data_length)), 0)
                                        mstore(64, add(__buycb_ptr, and(add(__buycb_total, 31), not(31))))
                                        let __buycb_success := call(gas(), buyerCallback, 0, __buycb_ptr, __buycb_total, __buycb_ptr, 32)
                                        if iszero(__buycb_success) {
                                            let __buycb_rds := returndatasize()
                                            returndatacopy(0, 0, __buycb_rds)
                                            revert(0, __buycb_rds)
                                        }
                                        if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__buycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                            mstore(0, shl(224, 0xa8f3eb44))
                                            revert(0, 4)
                                        }
                                    }
                                }
                            }
                        }
                        let feeAssets := sub(buyerAssets, sellerAssets)
                        if gt(feeAssets, 0) {
                            let self := address()
                            {
                                if iszero(gt(extcodesize(loanToken), 0)) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 7)
                                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lstf_ptr := mload(64)
                                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                                mstore(add(__lstf_ptr, 4), payer)
                                mstore(add(__lstf_ptr, 36), self)
                                mstore(add(__lstf_ptr, 68), feeAssets)
                                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                                let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                                if iszero(__lstf_success) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 21)
                                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lst_rds := returndatasize()
                                if __lst_rds {
                                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                        mstore(4, 32)
                                        mstore(36, 27)
                                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                        revert(0, 100)
                                    }
                                }
                            }
                        }
                        if gt(sellerAssets, 0) {
                            {
                                if iszero(gt(extcodesize(loanToken), 0)) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 7)
                                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lstf_ptr := mload(64)
                                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                                mstore(add(__lstf_ptr, 4), payer)
                                mstore(add(__lstf_ptr, 36), receiver)
                                mstore(add(__lstf_ptr, 68), sellerAssets)
                                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                                let __lstf_success := call(gas(), loanToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                                if iszero(__lstf_success) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 21)
                                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lst_rds := returndatasize()
                                if __lst_rds {
                                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                        mstore(4, 32)
                                        mstore(36, 27)
                                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                        revert(0, 100)
                                    }
                                }
                            }
                        }
                        let sellerCallback := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 7)
                        if __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1) {
                            sellerCallback := takerCallback
                        }
                        if iszero(eq(sellerCallback, 0)) {
                            {
                                let __ite_cond := __verity_param_dynamic_head_word_calldata_checked(offer_data_offset, 1)
                                if __ite_cond {
                                    {
                                        let __sellcb_ptr := mload(64)
                                        let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                        let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                                        let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                                        let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                                        let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                                        let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                                        let __sellcb_data_offset := sub(takerCallbackData_data_offset, 32)
                                        let __sellcb_data_length := calldataload(__sellcb_data_offset)
                                        let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                                        let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                                        let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                                        let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                                        mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                                        mstore(add(__sellcb_ptr, 4), id)
                                        mstore(add(__sellcb_ptr, 36), 256)
                                        mstore(add(__sellcb_ptr, 68), sellerAssets)
                                        mstore(add(__sellcb_ptr, 100), units)
                                        mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                                        mstore(add(__sellcb_ptr, 164), seller)
                                        mstore(add(__sellcb_ptr, 196), receiver)
                                        mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                                        mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                                        mstore(add(__sellcb_market_ptr, 32), 192)
                                        for {
                                            let __sellcb_m := 2
                                        } lt(__sellcb_m, 6) {
                                            __sellcb_m := add(__sellcb_m, 1)
                                        } {
                                            mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                                        }
                                        mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                                        calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                                        mstore(__sellcb_data_ptr, __sellcb_data_length)
                                        calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                                        mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                                        mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                                        let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                                        if iszero(__sellcb_success) {
                                            let __sellcb_rds := returndatasize()
                                            returndatacopy(0, 0, __sellcb_rds)
                                            revert(0, __sellcb_rds)
                                        }
                                        if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                            mstore(0, shl(224, 0xa4fb7883))
                                            revert(0, 4)
                                        }
                                    }
                                }
                                if iszero(__ite_cond) {
                                    {
                                        let __sellcb_ptr := mload(64)
                                        let __sellcb_market_offset := add(offer_data_offset, calldataload(offer_data_offset))
                                        let __sellcb_collateral_offset := add(__sellcb_market_offset, calldataload(add(__sellcb_market_offset, 32)))
                                        let __sellcb_collateral_length := calldataload(__sellcb_collateral_offset)
                                        let __sellcb_collateral_bytes := mul(__sellcb_collateral_length, 128)
                                        let __sellcb_market_size := add(224, __sellcb_collateral_bytes)
                                        let __sellcb_padded_market_size := and(add(__sellcb_market_size, 31), not(31))
                                        let __sellcb_data_offset := add(offer_data_offset, calldataload(add(offer_data_offset, 256)))
                                        let __sellcb_data_length := calldataload(__sellcb_data_offset)
                                        let __sellcb_padded_data_len := and(add(__sellcb_data_length, 31), not(31))
                                        let __sellcb_market_ptr := add(__sellcb_ptr, 260)
                                        let __sellcb_data_ptr := add(__sellcb_market_ptr, __sellcb_padded_market_size)
                                        let __sellcb_total := add(260, add(__sellcb_padded_market_size, add(32, __sellcb_padded_data_len)))
                                        mstore(__sellcb_ptr, shl(224, 0x7f44a13a))
                                        mstore(add(__sellcb_ptr, 4), id)
                                        mstore(add(__sellcb_ptr, 36), 256)
                                        mstore(add(__sellcb_ptr, 68), sellerAssets)
                                        mstore(add(__sellcb_ptr, 100), units)
                                        mstore(add(__sellcb_ptr, 132), sellerPendingFeeDecrease)
                                        mstore(add(__sellcb_ptr, 164), seller)
                                        mstore(add(__sellcb_ptr, 196), receiver)
                                        mstore(add(__sellcb_ptr, 228), add(256, __sellcb_padded_market_size))
                                        mstore(__sellcb_market_ptr, calldataload(__sellcb_market_offset))
                                        mstore(add(__sellcb_market_ptr, 32), 192)
                                        for {
                                            let __sellcb_m := 2
                                        } lt(__sellcb_m, 6) {
                                            __sellcb_m := add(__sellcb_m, 1)
                                        } {
                                            mstore(add(__sellcb_market_ptr, mul(__sellcb_m, 32)), calldataload(add(__sellcb_market_offset, mul(__sellcb_m, 32))))
                                        }
                                        mstore(add(__sellcb_market_ptr, 192), __sellcb_collateral_length)
                                        calldatacopy(add(__sellcb_market_ptr, 224), add(__sellcb_collateral_offset, 32), __sellcb_collateral_bytes)
                                        mstore(__sellcb_data_ptr, __sellcb_data_length)
                                        calldatacopy(add(__sellcb_data_ptr, 32), add(__sellcb_data_offset, 32), __sellcb_data_length)
                                        mstore(add(__sellcb_data_ptr, add(32, __sellcb_data_length)), 0)
                                        mstore(64, add(__sellcb_ptr, and(add(__sellcb_total, 31), not(31))))
                                        let __sellcb_success := call(gas(), sellerCallback, 0, __sellcb_ptr, __sellcb_total, __sellcb_ptr, 32)
                                        if iszero(__sellcb_success) {
                                            let __sellcb_rds := returndatasize()
                                            returndatacopy(0, 0, __sellcb_rds)
                                            revert(0, __sellcb_rds)
                                        }
                                        if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__sellcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                            mstore(0, shl(224, 0xa4fb7883))
                                            revert(0, 4)
                                        }
                                    }
                                }
                            }
                        }
                        let lockedAfterCallbacks := 0
                        {
                            let __liq_lock_ptr := mload(64)
                            mstore(__liq_lock_ptr, id)
                            mstore(add(__liq_lock_ptr, 32), seller)
                            mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                            let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                            let __liq_lock_depth := tload(__liq_lock_slot)
                            if gt(__liq_lock_depth, 0) {
                                __liq_lock_depth := sub(__liq_lock_depth, 1)
                                tstore(__liq_lock_slot, __liq_lock_depth)
                            }
                            lockedAfterCallbacks := __liq_lock_depth
                        }
                        {
                            let __ite_cond := iszero(eq(lockedAfterCallbacks, 0))
                            if __ite_cond {
                            }
                            if iszero(__ite_cond) {
                                let sellerHealthy := internal_internal_isHealthy(add(offer_data_offset, calldataload(offer_data_offset)), id, seller)
                                if iszero(iszero(eq(sellerHealthy, 0))) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x53656c6c657249734c6971756964617461626c65282900000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 22)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                            }
                        }
                        mstore(0, buyerAssets)
                        mstore(32, sellerAssets)
                        return(0, 64)
                    }
                    case 0xb6a37a3b {
                        /* creditOf() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(0, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xd881a443 {
                        /* debtOf() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x53a6332b {
                        /* totalUnits() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x570b935f {
                        /* lossFactor() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xdb94c029 {
                        /* tickSpacing() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(144, sload(add(mappingSlot(1, id), 2))), 255)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x2a7c7c09 {
                        /* settlementFeeCbps() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let settlementFeeCbp0 := and(shr(0, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp1 := and(shr(16, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp2 := and(shr(32, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp3 := and(shr(48, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp4 := and(shr(64, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp5 := and(shr(80, sload(add(mappingSlot(1, id), 2))), 65535)
                        let settlementFeeCbp6 := and(shr(96, sload(add(mappingSlot(1, id), 2))), 65535)
                        mstore(0, settlementFeeCbp0)
                        mstore(32, settlementFeeCbp1)
                        mstore(64, settlementFeeCbp2)
                        mstore(96, settlementFeeCbp3)
                        mstore(128, settlementFeeCbp4)
                        mstore(160, settlementFeeCbp5)
                        mstore(192, settlementFeeCbp6)
                        return(0, 224)
                    }
                    case 0x7cf43d8c {
                        /* withdrawable() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xc04e7898 {
                        /* continuousFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(112, sload(add(mappingSlot(1, id), 2))), 4294967295)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xfd2ff327 {
                        /* continuousFeeCredit() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let value := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xb6b271c7 {
                        /* withdraw() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 128) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let units := calldataload(36)
                        let onBehalf := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let receiver := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let authorized := internal_internal_isAuthorized(onBehalf, sender)
                        if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let id := internal_internal_toId(market_data_offset)
                        let creditBeforeUpdate := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                        let pendingFeeBeforeUpdate := and(shr(128, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                        let lastLossFactor := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
                        let lastAccrual := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))), 340282366920938463463374607431768211455)
                        let marketLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let now := timestamp()
                        let postSlashCredit := 0
                        if lt(lastLossFactor, 340282366920938463463374607431768211455) {
                            postSlashCredit := div(mul(creditBeforeUpdate, sub(340282366920938463463374607431768211455, marketLossFactor)), sub(340282366920938463463374607431768211455, lastLossFactor))
                        }
                        let postSlashPendingFee := 0
                        if gt(creditBeforeUpdate, 0) {
                            postSlashPendingFee := sub(pendingFeeBeforeUpdate, div(add(mul(pendingFeeBeforeUpdate, sub(creditBeforeUpdate, postSlashCredit)), sub(creditBeforeUpdate, 1)), creditBeforeUpdate))
                        }
                        let accrualEnd := __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)
                        if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                            accrualEnd := now
                        }
                        let accrued := 0
                        if lt(lastAccrual, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2)) {
                            accrued := div(mul(postSlashPendingFee, sub(accrualEnd, lastAccrual)), sub(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2), lastAccrual))
                        }
                        let creditAfterUpdate := sub(postSlashCredit, accrued)
                        let pendingFeeAfterUpdate := sub(postSlashPendingFee, accrued)
                        {
                            let __compat_value := creditAfterUpdate
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := marketLossFactor
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __compat_value := pendingFeeAfterUpdate
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        {
                            let __compat_value := now
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let currentContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := add(currentContinuousFeeCredit, accrued)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let creditDecrease := sub(creditBeforeUpdate, creditAfterUpdate)
                        let updatePendingFeeDecrease := sub(pendingFeeBeforeUpdate, pendingFeeAfterUpdate)
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x557064617465506f736974696f6e28627974657333322c616464726573732c75)
                            mstore(add(__evt_ptr, 32), 0x696e743235362c75696e743235362c75696e7432353629000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 55)
                            mstore(add(__evt_ptr, 0), creditDecrease)
                            mstore(add(__evt_ptr, 32), updatePendingFeeDecrease)
                            mstore(add(__evt_ptr, 64), accrued)
                            log3(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        let pendingFeeDecrease := 0
                        if gt(creditAfterUpdate, 0) {
                            pendingFeeDecrease := div(add(mul(pendingFeeAfterUpdate, units), sub(creditAfterUpdate, 1)), creditAfterUpdate)
                        }
                        {
                            let __compat_value := sub(pendingFeeAfterUpdate, pendingFeeDecrease)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                            let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                            sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(128, __compat_packed)))
                        }
                        let credit := and(shr(0, sload(mappingSlot(mappingSlot(0, id), onBehalf))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(credit, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(mappingSlot(0, id), onBehalf))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(mappingSlot(0, id), onBehalf), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(withdrawableAmount, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let total := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(total, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(mappingSlot(1, id))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x576974686472617728616464726573732c627974657333322c75696e74323536)
                            mstore(add(__evt_ptr, 32), 0x2c616464726573732c616464726573732c75696e743235362900000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 57)
                            mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 32), units)
                            mstore(add(__evt_ptr, 64), pendingFeeDecrease)
                            log4(__evt_ptr, 96, __evt_topic0, id, and(onBehalf, 0xffffffffffffffffffffffffffffffffffffffff), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_ptr := mload(64)
                            mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lst_ptr, 4), receiver)
                            mstore(add(__lst_ptr, 36), units)
                            mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                            let __lst_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lst_ptr, 68, __lst_ptr, 32)
                            if iszero(__lst_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 17)
                                mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 23)
                                    mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        stop()
                    }
                    case 0x4ae30a91 {
                        /* repay() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 160) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let units := calldataload(36)
                        let onBehalf := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let callback := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let data_offset := calldataload(132)
                        if lt(data_offset, 160) {
                            revert(0, 0)
                        }
                        let data_abs_offset := add(4, data_offset)
                        if gt(data_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let data_length := calldataload(data_abs_offset)
                        let data_tail_head_end := add(data_abs_offset, 32)
                        let data_tail_remaining := sub(calldatasize(), data_tail_head_end)
                        if gt(data_length, data_tail_remaining) {
                            revert(0, 0)
                        }
                        let data_data_offset := data_tail_head_end
                        let sender := caller()
                        let authorized := internal_internal_isAuthorized(onBehalf, sender)
                        if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let id := internal_internal_toId(market_data_offset)
                        let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := sub(debt, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __compat_value := add(withdrawableAmount, units)
                            let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                            let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                            let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                            sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                        }
                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                            {
                                let __repaycb_ptr := mload(64)
                                let __repaycb_market_offset := add(4, calldataload(4))
                                let __repaycb_collateral_offset := add(__repaycb_market_offset, calldataload(add(__repaycb_market_offset, 32)))
                                let __repaycb_collateral_length := calldataload(__repaycb_collateral_offset)
                                let __repaycb_collateral_bytes := mul(__repaycb_collateral_length, 128)
                                let __repaycb_market_size := add(224, __repaycb_collateral_bytes)
                                let __repaycb_padded_market_size := and(add(__repaycb_market_size, 31), not(31))
                                let __repaycb_data_offset := sub(data_data_offset, 32)
                                let __repaycb_data_length := calldataload(__repaycb_data_offset)
                                let __repaycb_padded_data_len := and(add(__repaycb_data_length, 31), not(31))
                                let __repaycb_market_ptr := add(__repaycb_ptr, 164)
                                let __repaycb_data_ptr := add(__repaycb_market_ptr, __repaycb_padded_market_size)
                                let __repaycb_total := add(164, add(__repaycb_padded_market_size, add(32, __repaycb_padded_data_len)))
                                mstore(__repaycb_ptr, shl(224, 0xfc56f72e))
                                mstore(add(__repaycb_ptr, 4), id)
                                mstore(add(__repaycb_ptr, 36), 160)
                                mstore(add(__repaycb_ptr, 68), units)
                                mstore(add(__repaycb_ptr, 100), onBehalf)
                                mstore(add(__repaycb_ptr, 132), add(160, __repaycb_padded_market_size))
                                mstore(__repaycb_market_ptr, calldataload(__repaycb_market_offset))
                                mstore(add(__repaycb_market_ptr, 32), 192)
                                for {
                                    let __repaycb_m := 2
                                } lt(__repaycb_m, 6) {
                                    __repaycb_m := add(__repaycb_m, 1)
                                } {
                                    mstore(add(__repaycb_market_ptr, mul(__repaycb_m, 32)), calldataload(add(__repaycb_market_offset, mul(__repaycb_m, 32))))
                                }
                                mstore(add(__repaycb_market_ptr, 192), __repaycb_collateral_length)
                                calldatacopy(add(__repaycb_market_ptr, 224), add(__repaycb_collateral_offset, 32), __repaycb_collateral_bytes)
                                mstore(__repaycb_data_ptr, __repaycb_data_length)
                                calldatacopy(add(__repaycb_data_ptr, 32), add(__repaycb_data_offset, 32), __repaycb_data_length)
                                mstore(add(__repaycb_data_ptr, add(32, __repaycb_data_length)), 0)
                                mstore(64, add(__repaycb_ptr, and(add(__repaycb_total, 31), not(31))))
                                let __repaycb_success := call(gas(), callback, 0, __repaycb_ptr, __repaycb_total, __repaycb_ptr, 32)
                                if iszero(__repaycb_success) {
                                    let __repaycb_rds := returndatasize()
                                    returndatacopy(0, 0, __repaycb_rds)
                                    revert(0, __repaycb_rds)
                                }
                                if iszero(and(iszero(lt(returndatasize(), 32)), eq(mload(__repaycb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2))) {
                                    mstore(0, shl(224, 0x40a13da2))
                                    revert(0, 4)
                                }
                            }
                        }
                        let self := address()
                        {
                            if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lstf_ptr := mload(64)
                            mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lstf_ptr, 4), payer)
                            mstore(add(__lstf_ptr, 36), self)
                            mstore(add(__lstf_ptr, 68), units)
                            mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                            let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                            if iszero(__lstf_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 21)
                                mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 27)
                                    mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        stop()
                    }
                    case 0x9812a361 {
                        /* collateralTokenAt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let collateralParams_offset := calldataload(4)
                        if lt(collateralParams_offset, 64) {
                            revert(0, 0)
                        }
                        let collateralParams_abs_offset := add(4, collateralParams_offset)
                        if gt(collateralParams_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let collateralParams_length := calldataload(collateralParams_abs_offset)
                        let collateralParams_tail_head_end := add(collateralParams_abs_offset, 32)
                        let collateralParams_tail_remaining := sub(calldatasize(), collateralParams_tail_head_end)
                        if gt(collateralParams_length, div(collateralParams_tail_remaining, 128)) {
                            revert(0, 0)
                        }
                        let collateralParams_data_offset := collateralParams_tail_head_end
                        let index := calldataload(36)
                        mstore(0, __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 0))
                        return(0, 32)
                    }
                    case 0xda8af457 {
                        /* collateralLltvAt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let collateralParams_offset := calldataload(4)
                        if lt(collateralParams_offset, 64) {
                            revert(0, 0)
                        }
                        let collateralParams_abs_offset := add(4, collateralParams_offset)
                        if gt(collateralParams_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let collateralParams_length := calldataload(collateralParams_abs_offset)
                        let collateralParams_tail_head_end := add(collateralParams_abs_offset, 32)
                        let collateralParams_tail_remaining := sub(calldatasize(), collateralParams_tail_head_end)
                        if gt(collateralParams_length, div(collateralParams_tail_remaining, 128)) {
                            revert(0, 0)
                        }
                        let collateralParams_data_offset := collateralParams_tail_head_end
                        let index := calldataload(36)
                        mstore(0, __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 1))
                        return(0, 32)
                    }
                    case 0x2082d1fc {
                        /* collateralMaxLifAt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let collateralParams_offset := calldataload(4)
                        if lt(collateralParams_offset, 64) {
                            revert(0, 0)
                        }
                        let collateralParams_abs_offset := add(4, collateralParams_offset)
                        if gt(collateralParams_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let collateralParams_length := calldataload(collateralParams_abs_offset)
                        let collateralParams_tail_head_end := add(collateralParams_abs_offset, 32)
                        let collateralParams_tail_remaining := sub(calldatasize(), collateralParams_tail_head_end)
                        if gt(collateralParams_length, div(collateralParams_tail_remaining, 128)) {
                            revert(0, 0)
                        }
                        let collateralParams_data_offset := collateralParams_tail_head_end
                        let index := calldataload(36)
                        mstore(0, __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 2))
                        return(0, 32)
                    }
                    case 0x8bf840f7 {
                        /* collateralOracleAt() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let collateralParams_offset := calldataload(4)
                        if lt(collateralParams_offset, 64) {
                            revert(0, 0)
                        }
                        let collateralParams_abs_offset := add(4, collateralParams_offset)
                        if gt(collateralParams_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let collateralParams_length := calldataload(collateralParams_abs_offset)
                        let collateralParams_tail_head_end := add(collateralParams_abs_offset, 32)
                        let collateralParams_tail_remaining := sub(calldatasize(), collateralParams_tail_head_end)
                        if gt(collateralParams_length, div(collateralParams_tail_remaining, 128)) {
                            revert(0, 0)
                        }
                        let collateralParams_data_offset := collateralParams_tail_head_end
                        let index := calldataload(36)
                        mstore(0, __verity_array_element_word_calldata_checked(collateralParams_data_offset, collateralParams_length, index, 4, 3))
                        return(0, 32)
                    }
                    case 0x4393876e {
                        /* oraclePrice() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 36) {
                            revert(0, 0)
                        }
                        let oracle := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let price := 0
                        {
                            let __oracle_ptr := mload(64)
                            mstore(__oracle_ptr, shl(224, 0xa035b1fe))
                            mstore(64, add(__oracle_ptr, 32))
                            let __oracle_success := staticcall(gas(), oracle, __oracle_ptr, 4, __oracle_ptr, 32)
                            if iszero(__oracle_success) {
                                let __oracle_rds := returndatasize()
                                returndatacopy(0, 0, __oracle_rds)
                                revert(0, __oracle_rds)
                            }
                            if iszero(eq(returndatasize(), 32)) {
                                revert(0, 0)
                            }
                            price := mload(__oracle_ptr)
                        }
                        mstore(0, price)
                        return(0, 32)
                    }
                    case 0x582577b6 {
                        /* liquidatorGateCanLiquidate() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let gate := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let account := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let allowed := 0
                        {
                            let __ecwr_ptr := mload(64)
                            mstore(__ecwr_ptr, shl(224, 0xb9f4ff55))
                            mstore(add(__ecwr_ptr, 4), account)
                            mstore(64, add(__ecwr_ptr, 64))
                            let __ecwr_success := staticcall(gas(), gate, __ecwr_ptr, 36, __ecwr_ptr, 32)
                            if iszero(__ecwr_success) {
                                let __ecwr_rds := returndatasize()
                                returndatacopy(0, 0, __ecwr_rds)
                                revert(0, __ecwr_rds)
                            }
                            if lt(returndatasize(), 32) {
                                revert(0, 0)
                            }
                            allowed := mload(__ecwr_ptr)
                        }
                        mstore(0, allowed)
                        return(0, 32)
                    }
                    case 0x0f66e87e {
                        /* liquidatorGateCanLiquidateOrDefault() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let gate := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)
                        let account := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let allowed := 1
                        if iszero(eq(gate, 0)) {
                            let loaded := internal_internal_liquidatorGateCanLiquidate(gate, account)
                            allowed := loaded
                        }
                        mstore(0, allowed)
                        return(0, 32)
                    }
                    case 0x610dc3ac {
                        /* liquidationLockedValue() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let locked := 0
                        {
                            let __liq_lock_ptr := mload(64)
                            mstore(__liq_lock_ptr, id)
                            mstore(add(__liq_lock_ptr, 32), user)
                            mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                            locked := tload(keccak256(__liq_lock_ptr, 96))
                        }
                        mstore(0, locked)
                        return(0, 32)
                    }
                    case 0xd69a5d2d {
                        /* liquidationLockExchange() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := calldataload(68)
                        let previous := 0
                        {
                            let __liq_lock_ptr := mload(64)
                            mstore(__liq_lock_ptr, id)
                            mstore(add(__liq_lock_ptr, 32), user)
                            mstore(add(__liq_lock_ptr, 64), 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4)
                            let __liq_lock_slot := keccak256(__liq_lock_ptr, 96)
                            previous := tload(__liq_lock_slot)
                            tstore(__liq_lock_slot, value)
                        }
                        mstore(0, previous)
                        return(0, 32)
                    }
                    case 0x1b14c072 {
                        /* liquidationLocked() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let locked := internal_internal_liquidationLockedValue(id, user)
                        mstore(0, iszero(eq(locked, 0)))
                        return(0, 32)
                    }
                    case 0x988fe2e6 {
                        /* collateralAmount() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(68)
                        let value := 0
                        if eq(index, 0) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 3))
                            value := loaded
                        }
                        if eq(index, 1) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 4))
                            value := loaded
                        }
                        if eq(index, 2) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 5))
                            value := loaded
                        }
                        if eq(index, 3) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 6))
                            value := loaded
                        }
                        if eq(index, 4) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 7))
                            value := loaded
                        }
                        if eq(index, 5) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 8))
                            value := loaded
                        }
                        if eq(index, 6) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 9))
                            value := loaded
                        }
                        if eq(index, 7) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 10))
                            value := loaded
                        }
                        if eq(index, 8) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 11))
                            value := loaded
                        }
                        if eq(index, 9) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 12))
                            value := loaded
                        }
                        if eq(index, 10) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 13))
                            value := loaded
                        }
                        if eq(index, 11) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 14))
                            value := loaded
                        }
                        if eq(index, 12) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 15))
                            value := loaded
                        }
                        if eq(index, 13) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 16))
                            value := loaded
                        }
                        if eq(index, 14) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 17))
                            value := loaded
                        }
                        if eq(index, 15) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 18))
                            value := loaded
                        }
                        if eq(index, 16) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 19))
                            value := loaded
                        }
                        if eq(index, 17) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 20))
                            value := loaded
                        }
                        if eq(index, 18) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 21))
                            value := loaded
                        }
                        if eq(index, 19) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 22))
                            value := loaded
                        }
                        if eq(index, 20) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 23))
                            value := loaded
                        }
                        if eq(index, 21) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 24))
                            value := loaded
                        }
                        if eq(index, 22) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 25))
                            value := loaded
                        }
                        if eq(index, 23) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 26))
                            value := loaded
                        }
                        if eq(index, 24) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 27))
                            value := loaded
                        }
                        if eq(index, 25) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 28))
                            value := loaded
                        }
                        if eq(index, 26) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 29))
                            value := loaded
                        }
                        if eq(index, 27) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 30))
                            value := loaded
                        }
                        if eq(index, 28) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 31))
                            value := loaded
                        }
                        if eq(index, 29) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 32))
                            value := loaded
                        }
                        if eq(index, 30) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 33))
                            value := loaded
                        }
                        if eq(index, 31) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 34))
                            value := loaded
                        }
                        if eq(index, 32) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 35))
                            value := loaded
                        }
                        if eq(index, 33) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 36))
                            value := loaded
                        }
                        if eq(index, 34) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 37))
                            value := loaded
                        }
                        if eq(index, 35) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 38))
                            value := loaded
                        }
                        if eq(index, 36) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 39))
                            value := loaded
                        }
                        if eq(index, 37) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 40))
                            value := loaded
                        }
                        if eq(index, 38) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 41))
                            value := loaded
                        }
                        if eq(index, 39) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 42))
                            value := loaded
                        }
                        if eq(index, 40) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 43))
                            value := loaded
                        }
                        if eq(index, 41) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 44))
                            value := loaded
                        }
                        if eq(index, 42) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 45))
                            value := loaded
                        }
                        if eq(index, 43) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 46))
                            value := loaded
                        }
                        if eq(index, 44) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 47))
                            value := loaded
                        }
                        if eq(index, 45) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 48))
                            value := loaded
                        }
                        if eq(index, 46) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 49))
                            value := loaded
                        }
                        if eq(index, 47) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 50))
                            value := loaded
                        }
                        if eq(index, 48) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 51))
                            value := loaded
                        }
                        if eq(index, 49) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 52))
                            value := loaded
                        }
                        if eq(index, 50) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 53))
                            value := loaded
                        }
                        if eq(index, 51) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 54))
                            value := loaded
                        }
                        if eq(index, 52) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 55))
                            value := loaded
                        }
                        if eq(index, 53) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 56))
                            value := loaded
                        }
                        if eq(index, 54) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 57))
                            value := loaded
                        }
                        if eq(index, 55) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 58))
                            value := loaded
                        }
                        if eq(index, 56) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 59))
                            value := loaded
                        }
                        if eq(index, 57) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 60))
                            value := loaded
                        }
                        if eq(index, 58) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 61))
                            value := loaded
                        }
                        if eq(index, 59) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 62))
                            value := loaded
                        }
                        if eq(index, 60) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 63))
                            value := loaded
                        }
                        if eq(index, 61) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 64))
                            value := loaded
                        }
                        if eq(index, 62) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 65))
                            value := loaded
                        }
                        if eq(index, 63) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 66))
                            value := loaded
                        }
                        if eq(index, 64) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 67))
                            value := loaded
                        }
                        if eq(index, 65) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 68))
                            value := loaded
                        }
                        if eq(index, 66) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 69))
                            value := loaded
                        }
                        if eq(index, 67) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 70))
                            value := loaded
                        }
                        if eq(index, 68) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 71))
                            value := loaded
                        }
                        if eq(index, 69) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 72))
                            value := loaded
                        }
                        if eq(index, 70) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 73))
                            value := loaded
                        }
                        if eq(index, 71) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 74))
                            value := loaded
                        }
                        if eq(index, 72) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 75))
                            value := loaded
                        }
                        if eq(index, 73) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 76))
                            value := loaded
                        }
                        if eq(index, 74) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 77))
                            value := loaded
                        }
                        if eq(index, 75) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 78))
                            value := loaded
                        }
                        if eq(index, 76) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 79))
                            value := loaded
                        }
                        if eq(index, 77) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 80))
                            value := loaded
                        }
                        if eq(index, 78) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 81))
                            value := loaded
                        }
                        if eq(index, 79) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 82))
                            value := loaded
                        }
                        if eq(index, 80) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 83))
                            value := loaded
                        }
                        if eq(index, 81) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 84))
                            value := loaded
                        }
                        if eq(index, 82) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 85))
                            value := loaded
                        }
                        if eq(index, 83) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 86))
                            value := loaded
                        }
                        if eq(index, 84) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 87))
                            value := loaded
                        }
                        if eq(index, 85) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 88))
                            value := loaded
                        }
                        if eq(index, 86) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 89))
                            value := loaded
                        }
                        if eq(index, 87) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 90))
                            value := loaded
                        }
                        if eq(index, 88) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 91))
                            value := loaded
                        }
                        if eq(index, 89) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 92))
                            value := loaded
                        }
                        if eq(index, 90) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 93))
                            value := loaded
                        }
                        if eq(index, 91) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 94))
                            value := loaded
                        }
                        if eq(index, 92) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 95))
                            value := loaded
                        }
                        if eq(index, 93) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 96))
                            value := loaded
                        }
                        if eq(index, 94) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 97))
                            value := loaded
                        }
                        if eq(index, 95) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 98))
                            value := loaded
                        }
                        if eq(index, 96) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 99))
                            value := loaded
                        }
                        if eq(index, 97) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 100))
                            value := loaded
                        }
                        if eq(index, 98) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 101))
                            value := loaded
                        }
                        if eq(index, 99) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 102))
                            value := loaded
                        }
                        if eq(index, 100) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 103))
                            value := loaded
                        }
                        if eq(index, 101) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 104))
                            value := loaded
                        }
                        if eq(index, 102) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 105))
                            value := loaded
                        }
                        if eq(index, 103) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 106))
                            value := loaded
                        }
                        if eq(index, 104) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 107))
                            value := loaded
                        }
                        if eq(index, 105) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 108))
                            value := loaded
                        }
                        if eq(index, 106) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 109))
                            value := loaded
                        }
                        if eq(index, 107) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 110))
                            value := loaded
                        }
                        if eq(index, 108) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 111))
                            value := loaded
                        }
                        if eq(index, 109) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 112))
                            value := loaded
                        }
                        if eq(index, 110) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 113))
                            value := loaded
                        }
                        if eq(index, 111) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 114))
                            value := loaded
                        }
                        if eq(index, 112) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 115))
                            value := loaded
                        }
                        if eq(index, 113) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 116))
                            value := loaded
                        }
                        if eq(index, 114) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 117))
                            value := loaded
                        }
                        if eq(index, 115) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 118))
                            value := loaded
                        }
                        if eq(index, 116) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 119))
                            value := loaded
                        }
                        if eq(index, 117) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 120))
                            value := loaded
                        }
                        if eq(index, 118) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 121))
                            value := loaded
                        }
                        if eq(index, 119) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 122))
                            value := loaded
                        }
                        if eq(index, 120) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 123))
                            value := loaded
                        }
                        if eq(index, 121) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 124))
                            value := loaded
                        }
                        if eq(index, 122) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 125))
                            value := loaded
                        }
                        if eq(index, 123) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 126))
                            value := loaded
                        }
                        if eq(index, 124) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 127))
                            value := loaded
                        }
                        if eq(index, 125) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 128))
                            value := loaded
                        }
                        if eq(index, 126) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 129))
                            value := loaded
                        }
                        if eq(index, 127) {
                            let loaded := sload(add(mappingSlot(mappingSlot(0, id), user), 130))
                            value := loaded
                        }
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xd7188aee {
                        /* liquidationDebtSnapshot() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 196) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let borrower := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let collateralCount := calldataload(68)
                        let collateralBitmapValue := calldataload(100)
                        let collateralParamsOffset := calldataload(132)
                        let originalDebt := calldataload(164)
                        let maxDebtValue := 0
                        let badDebt := originalDebt
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := collateralCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
                            __forEach_idx := add(__forEach_idx, 1)
                        } {
                            i := __forEach_idx
                            let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                            if active {
                                let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                                let collateralParamOffset := add(add(collateralParamsOffset, 32), mul(i, 128))
                                let oracle := calldataload(add(collateralParamOffset, 96))
                                let price := internal_internal_oraclePrice(oracle)
                                let lltv := calldataload(add(collateralParamOffset, 32))
                                let maxLifValue := calldataload(add(collateralParamOffset, 64))
                                let collateralDebtValue := div(mul(div(mul(activeCollateral, price), 1000000000000000000000000000000000000), lltv), 1000000000000000000)
                                maxDebtValue := add(maxDebtValue, collateralDebtValue)
                                let repayable := div(add(mul(div(add(mul(activeCollateral, price), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(maxLifValue, 1)), maxLifValue)
                                {
                                    let __ite_cond := gt(badDebt, repayable)
                                    if __ite_cond {
                                        badDebt := sub(badDebt, repayable)
                                    }
                                    if iszero(__ite_cond) {
                                        badDebt := 0
                                    }
                                }
                            }
                        }
                        mstore(0, maxDebtValue)
                        mstore(32, badDebt)
                        return(0, 64)
                    }
                    case 0x02564eae {
                        /* writeCollateralAmount() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(68)
                        let value := calldataload(100)
                        if eq(index, 0) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 3), value)
                        }
                        if eq(index, 1) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 4), value)
                        }
                        if eq(index, 2) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 5), value)
                        }
                        if eq(index, 3) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 6), value)
                        }
                        if eq(index, 4) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 7), value)
                        }
                        if eq(index, 5) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 8), value)
                        }
                        if eq(index, 6) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 9), value)
                        }
                        if eq(index, 7) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 10), value)
                        }
                        if eq(index, 8) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 11), value)
                        }
                        if eq(index, 9) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 12), value)
                        }
                        if eq(index, 10) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 13), value)
                        }
                        if eq(index, 11) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 14), value)
                        }
                        if eq(index, 12) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 15), value)
                        }
                        if eq(index, 13) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 16), value)
                        }
                        if eq(index, 14) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 17), value)
                        }
                        if eq(index, 15) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 18), value)
                        }
                        if eq(index, 16) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 19), value)
                        }
                        if eq(index, 17) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 20), value)
                        }
                        if eq(index, 18) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 21), value)
                        }
                        if eq(index, 19) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 22), value)
                        }
                        if eq(index, 20) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 23), value)
                        }
                        if eq(index, 21) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 24), value)
                        }
                        if eq(index, 22) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 25), value)
                        }
                        if eq(index, 23) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 26), value)
                        }
                        if eq(index, 24) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 27), value)
                        }
                        if eq(index, 25) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 28), value)
                        }
                        if eq(index, 26) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 29), value)
                        }
                        if eq(index, 27) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 30), value)
                        }
                        if eq(index, 28) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 31), value)
                        }
                        if eq(index, 29) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 32), value)
                        }
                        if eq(index, 30) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 33), value)
                        }
                        if eq(index, 31) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 34), value)
                        }
                        if eq(index, 32) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 35), value)
                        }
                        if eq(index, 33) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 36), value)
                        }
                        if eq(index, 34) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 37), value)
                        }
                        if eq(index, 35) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 38), value)
                        }
                        if eq(index, 36) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 39), value)
                        }
                        if eq(index, 37) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 40), value)
                        }
                        if eq(index, 38) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 41), value)
                        }
                        if eq(index, 39) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 42), value)
                        }
                        if eq(index, 40) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 43), value)
                        }
                        if eq(index, 41) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 44), value)
                        }
                        if eq(index, 42) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 45), value)
                        }
                        if eq(index, 43) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 46), value)
                        }
                        if eq(index, 44) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 47), value)
                        }
                        if eq(index, 45) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 48), value)
                        }
                        if eq(index, 46) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 49), value)
                        }
                        if eq(index, 47) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 50), value)
                        }
                        if eq(index, 48) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 51), value)
                        }
                        if eq(index, 49) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 52), value)
                        }
                        if eq(index, 50) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 53), value)
                        }
                        if eq(index, 51) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 54), value)
                        }
                        if eq(index, 52) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 55), value)
                        }
                        if eq(index, 53) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 56), value)
                        }
                        if eq(index, 54) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 57), value)
                        }
                        if eq(index, 55) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 58), value)
                        }
                        if eq(index, 56) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 59), value)
                        }
                        if eq(index, 57) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 60), value)
                        }
                        if eq(index, 58) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 61), value)
                        }
                        if eq(index, 59) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 62), value)
                        }
                        if eq(index, 60) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 63), value)
                        }
                        if eq(index, 61) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 64), value)
                        }
                        if eq(index, 62) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 65), value)
                        }
                        if eq(index, 63) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 66), value)
                        }
                        if eq(index, 64) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 67), value)
                        }
                        if eq(index, 65) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 68), value)
                        }
                        if eq(index, 66) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 69), value)
                        }
                        if eq(index, 67) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 70), value)
                        }
                        if eq(index, 68) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 71), value)
                        }
                        if eq(index, 69) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 72), value)
                        }
                        if eq(index, 70) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 73), value)
                        }
                        if eq(index, 71) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 74), value)
                        }
                        if eq(index, 72) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 75), value)
                        }
                        if eq(index, 73) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 76), value)
                        }
                        if eq(index, 74) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 77), value)
                        }
                        if eq(index, 75) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 78), value)
                        }
                        if eq(index, 76) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 79), value)
                        }
                        if eq(index, 77) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 80), value)
                        }
                        if eq(index, 78) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 81), value)
                        }
                        if eq(index, 79) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 82), value)
                        }
                        if eq(index, 80) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 83), value)
                        }
                        if eq(index, 81) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 84), value)
                        }
                        if eq(index, 82) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 85), value)
                        }
                        if eq(index, 83) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 86), value)
                        }
                        if eq(index, 84) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 87), value)
                        }
                        if eq(index, 85) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 88), value)
                        }
                        if eq(index, 86) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 89), value)
                        }
                        if eq(index, 87) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 90), value)
                        }
                        if eq(index, 88) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 91), value)
                        }
                        if eq(index, 89) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 92), value)
                        }
                        if eq(index, 90) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 93), value)
                        }
                        if eq(index, 91) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 94), value)
                        }
                        if eq(index, 92) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 95), value)
                        }
                        if eq(index, 93) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 96), value)
                        }
                        if eq(index, 94) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 97), value)
                        }
                        if eq(index, 95) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 98), value)
                        }
                        if eq(index, 96) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 99), value)
                        }
                        if eq(index, 97) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 100), value)
                        }
                        if eq(index, 98) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 101), value)
                        }
                        if eq(index, 99) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 102), value)
                        }
                        if eq(index, 100) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 103), value)
                        }
                        if eq(index, 101) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 104), value)
                        }
                        if eq(index, 102) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 105), value)
                        }
                        if eq(index, 103) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 106), value)
                        }
                        if eq(index, 104) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 107), value)
                        }
                        if eq(index, 105) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 108), value)
                        }
                        if eq(index, 106) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 109), value)
                        }
                        if eq(index, 107) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 110), value)
                        }
                        if eq(index, 108) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 111), value)
                        }
                        if eq(index, 109) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 112), value)
                        }
                        if eq(index, 110) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 113), value)
                        }
                        if eq(index, 111) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 114), value)
                        }
                        if eq(index, 112) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 115), value)
                        }
                        if eq(index, 113) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 116), value)
                        }
                        if eq(index, 114) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 117), value)
                        }
                        if eq(index, 115) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 118), value)
                        }
                        if eq(index, 116) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 119), value)
                        }
                        if eq(index, 117) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 120), value)
                        }
                        if eq(index, 118) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 121), value)
                        }
                        if eq(index, 119) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 122), value)
                        }
                        if eq(index, 120) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 123), value)
                        }
                        if eq(index, 121) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 124), value)
                        }
                        if eq(index, 122) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 125), value)
                        }
                        if eq(index, 123) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 126), value)
                        }
                        if eq(index, 124) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 127), value)
                        }
                        if eq(index, 125) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 128), value)
                        }
                        if eq(index, 126) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 129), value)
                        }
                        if eq(index, 127) {
                            sstore(add(mappingSlot(mappingSlot(0, id), user), 130), value)
                        }
                        mstore(0, 0)
                        return(0, 32)
                    }
                    case 0xa15bdf2f {
                        /* liquidate() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 292) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 292) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 288) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let collateralIndex := calldataload(36)
                        let seizedAssets := calldataload(68)
                        let repaidUnits := calldataload(100)
                        let borrower := and(calldataload(132), 0xffffffffffffffffffffffffffffffffffffffff)
                        let postMaturityMode := iszero(iszero(calldataload(164)))
                        let receiver := and(calldataload(196), 0xffffffffffffffffffffffffffffffffffffffff)
                        let callback := and(calldataload(228), 0xffffffffffffffffffffffffffffffffffffffff)
                        let data_offset := calldataload(260)
                        if lt(data_offset, 288) {
                            revert(0, 0)
                        }
                        let data_abs_offset := add(4, data_offset)
                        if gt(data_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let data_length := calldataload(data_abs_offset)
                        let data_tail_head_end := add(data_abs_offset, 32)
                        let data_tail_remaining := sub(calldatasize(), data_tail_head_end)
                        if gt(data_length, data_tail_remaining) {
                            revert(0, 0)
                        }
                        let data_data_offset := data_tail_head_end
                        let sender := caller()
                        let id := internal_internal_toId(market_data_offset)
                        let debtLoaded := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                        let debt := debtLoaded
                        let totalUnitsValue := and(shr(0, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        if iszero(or(iszero(iszero(eq(seizedAssets, 0))), iszero(iszero(eq(repaidUnits, 0))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        if iszero(gt(debt, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4e6f74426f72726f776572282900000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 13)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let canLiquidate := internal_internal_liquidatorGateCanLiquidateOrDefault(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), sender)
                        if iszero(or(iszero(iszero(eq(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 5), 0))), iszero(iszero(iszero(eq(canLiquidate, 0)))))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4c697175696461746f72476174656446726f6d4c69717569646174696e672829)
                                let __err_hash := keccak256(__err_ptr, 32)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
                        {
                            let __ite_cond := lt(collateralIndex, collateralCount)
                            if __ite_cond {
                            }
                            if iszero(__ite_cond) {
                                mstore(0, shl(224, 1313373041))
                                mstore(4, 50)
                                revert(0, 36)
                            }
                        }
                        let _collateralIndexBoundsCheck := internal_internal_collateralMaxLifAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                        let marketDataOffset := add(calldataload(4), 4)
                        let collateralParamsOffset := add(marketDataOffset, calldataload(add(marketDataOffset, 32)))
                        let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                        let collateralActive := internal_internal_collateralBitmapIsSet(collateralBitmapValue, collateralIndex)
                        if or(iszero(iszero(gt(seizedAssets, 0))), iszero(iszero(gt(repaidUnits, 0)))) {
                            if iszero(collateralActive) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 19)
                                mstore(68, 0x696e61637469766520636f6c6c61746572616c00000000000000000000000000)
                                revert(0, 100)
                            }
                        }
                        let originalDebt := debt
                        let maxDebtValue, badDebt := internal_internal_liquidationDebtSnapshot(id, borrower, collateralCount, collateralBitmapValue, collateralParamsOffset, originalDebt)
                        let selectedCollateralParamOffsetForPrice := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                        let selectedOracle := calldataload(add(selectedCollateralParamOffsetForPrice, 96))
                        let liquidatedCollatPrice := internal_internal_oraclePrice(selectedOracle)
                        let now := timestamp()
                        let lockedBeforeLiquidation := internal_internal_liquidationLockedValue(id, borrower)
                        if iszero(eq(lockedBeforeLiquidation, 0)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 17)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        {
                            let __ite_cond := postMaturityMode
                            if __ite_cond {
                                if iszero(gt(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 17)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                            }
                            if iszero(__ite_cond) {
                                if iszero(gt(originalDebt, maxDebtValue)) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x4e6f744c6971756964617461626c652829000000000000000000000000000000)
                                        let __err_hash := keccak256(__err_ptr, 17)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                            }
                        }
                        if gt(badDebt, 0) {
                            {
                                let __compat_value := sub(debt, badDebt)
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            debt := sub(debt, badDebt)
                            let oldLossFactor := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                            let newLossFactor := sub(340282366920938463463374607431768211455, div(mul(sub(340282366920938463463374607431768211455, oldLossFactor), sub(totalUnitsValue, badDebt)), totalUnitsValue))
                            {
                                let __compat_value := newLossFactor
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(1, id))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                            {
                                let __compat_value := sub(totalUnitsValue, badDebt)
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(mappingSlot(1, id))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(mappingSlot(1, id), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            let oldContinuousFeeCredit := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                            let newContinuousFeeCredit := 0
                            if lt(oldLossFactor, 340282366920938463463374607431768211455) {
                                newContinuousFeeCredit := div(mul(oldContinuousFeeCredit, sub(340282366920938463463374607431768211455, newLossFactor)), sub(340282366920938463463374607431768211455, oldLossFactor))
                            }
                            {
                                let __compat_value := newContinuousFeeCredit
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(128, __compat_packed)))
                            }
                        }
                        let outSeizedAssets := seizedAssets
                        let outRepaidUnits := repaidUnits
                        if or(iszero(iszero(gt(outRepaidUnits, 0))), iszero(iszero(gt(outSeizedAssets, 0)))) {
                            let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                            let maxLifValue := calldataload(add(selectedCollateralParamOffset, 64))
                            let lif := maxLifValue
                            if postMaturityMode {
                                let elapsed := sub(now, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 2))
                                let postMaturityLif := add(1000000000000000000, div(mul(sub(maxLifValue, 1000000000000000000), elapsed), 900))
                                {
                                    let __ite_cond := iszero(gt(maxLifValue, postMaturityLif))
                                    if __ite_cond {
                                        lif := maxLifValue
                                    }
                                    if iszero(__ite_cond) {
                                        lif := postMaturityLif
                                    }
                                }
                            }
                            {
                                let __ite_cond := gt(outSeizedAssets, 0)
                                if __ite_cond {
                                    outRepaidUnits := div(add(mul(div(add(mul(outSeizedAssets, liquidatedCollatPrice), sub(1000000000000000000000000000000000000, 1)), 1000000000000000000000000000000000000), 1000000000000000000), sub(lif, 1)), lif)
                                }
                                if iszero(__ite_cond) {
                                    outSeizedAssets := div(mul(div(mul(outRepaidUnits, lif), 1000000000000000000), 1000000000000000000000000000000000000), liquidatedCollatPrice)
                                }
                            }
                            {
                                let __ite_cond := postMaturityMode
                                if __ite_cond {
                                }
                                if iszero(__ite_cond) {
                                    let lltv := calldataload(add(selectedCollateralParamOffset, 32))
                                    let maxRepaidValue := 115792089237316195423570985008687907853269984665640564039457584007913129639935
                                    if lt(lltv, 1000000000000000000) {
                                        maxRepaidValue := div(add(mul(sub(debt, maxDebtValue), mul(1000000000000000000, 1000000000000000000)), sub(sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)), 1)), sub(mul(1000000000000000000, 1000000000000000000), mul(lif, lltv)))
                                    }
                                    let oldCollateralForRcf := internal_internal_collateralAmount(id, borrower, collateralIndex)
                                    let collateralRepayCapacity := div(mul(div(mul(oldCollateralForRcf, liquidatedCollatPrice), 1000000000000000000000000000000000000), 1000000000000000000), lif)
                                    let capacityShortfall := 0
                                    if gt(collateralRepayCapacity, maxRepaidValue) {
                                        capacityShortfall := sub(collateralRepayCapacity, maxRepaidValue)
                                    }
                                    if iszero(or(iszero(iszero(iszero(gt(outRepaidUnits, maxRepaidValue)))), iszero(iszero(lt(capacityShortfall, __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 3)))))) {
                                        {
                                            let __err_ptr := mload(64)
                                            mstore(add(__err_ptr, 0), 0x5265636f76657279436c6f7365466163746f72436f6e646974696f6e7356696f)
                                            mstore(add(__err_ptr, 32), 0x6c61746564282900000000000000000000000000000000000000000000000000)
                                            let __err_hash := keccak256(__err_ptr, 39)
                                            let __err_selector := shl(224, shr(224, __err_hash))
                                            mstore(0, __err_selector)
                                            let __err_tail := 0
                                            revert(0, add(4, __err_tail))
                                        }
                                    }
                                }
                            }
                            let oldCollateral := internal_internal_collateralAmount(id, borrower, collateralIndex)
                            if lt(oldCollateral, outSeizedAssets) {
                                mstore(0, shl(224, 1313373041))
                                mstore(4, 17)
                                revert(0, 36)
                            }
                            let newCollateral := sub(oldCollateral, outSeizedAssets)
                            let _writeOk := internal_internal_writeCollateralAmount(id, borrower, collateralIndex, newCollateral)
                            if eq(newCollateral, 0) {
                                if gt(outSeizedAssets, 0) {
                                    let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                                    let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                                    {
                                        let __compat_value := newBitmap
                                        let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                        let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                                        let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                        sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                                    }
                                }
                            }
                            let withdrawableAmount := and(shr(0, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                            {
                                let __compat_value := add(withdrawableAmount, outRepaidUnits)
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(1, id), 1))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(add(mappingSlot(1, id), 1), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                            if lt(debt, outRepaidUnits) {
                                mstore(0, shl(224, 1313373041))
                                mstore(4, 17)
                                revert(0, 36)
                            }
                            let newDebtAfterRepay := sub(debt, outRepaidUnits)
                            {
                                let __compat_value := newDebtAfterRepay
                                let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))
                                let __compat_slot_cleared := and(__compat_slot_word, not(340282366920938463463374607431768211455))
                                sstore(add(mappingSlot(mappingSlot(0, id), borrower), 2), or(__compat_slot_cleared, shl(0, __compat_packed)))
                            }
                        }
                        let selectedCollateralParamOffset := add(add(collateralParamsOffset, 32), mul(collateralIndex, 128))
                        let collateralToken := calldataload(selectedCollateralParamOffset)
                        let payer := sender
                        if iszero(eq(callback, 0)) {
                            payer := callback
                        }
                        let latestLossFactorLoaded := and(shr(128, sload(mappingSlot(1, id))), 340282366920938463463374607431768211455)
                        let latestContinuousFeeCreditLoaded := and(shr(128, sload(add(mappingSlot(1, id), 1))), 340282366920938463463374607431768211455)
                        {
                            let __evt_ptr := mload(64)
                            mstore(add(__evt_ptr, 0), 0x4c697175696461746528616464726573732c627974657333322c616464726573)
                            mstore(add(__evt_ptr, 32), 0x732c75696e743235362c75696e743235362c616464726573732c626f6f6c2c61)
                            mstore(add(__evt_ptr, 64), 0x6464726573732c616464726573732c75696e743235362c75696e743235362c75)
                            mstore(add(__evt_ptr, 96), 0x696e743235362900000000000000000000000000000000000000000000000000)
                            let __evt_topic0 := keccak256(__evt_ptr, 103)
                            mstore(add(__evt_ptr, 0), and(sender, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 32), outSeizedAssets)
                            mstore(add(__evt_ptr, 64), outRepaidUnits)
                            mstore(add(__evt_ptr, 96), iszero(iszero(postMaturityMode)))
                            mstore(add(__evt_ptr, 128), and(receiver, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 160), and(payer, 0xffffffffffffffffffffffffffffffffffffffff))
                            mstore(add(__evt_ptr, 192), badDebt)
                            mstore(add(__evt_ptr, 224), add(latestLossFactorLoaded, 0))
                            mstore(add(__evt_ptr, 256), add(latestContinuousFeeCreditLoaded, 0))
                            log4(__evt_ptr, 288, __evt_topic0, id, and(collateralToken, 0xffffffffffffffffffffffffffffffffffffffff), and(borrower, 0xffffffffffffffffffffffffffffffffffffffff))
                        }
                        {
                            if iszero(gt(extcodesize(collateralToken), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_ptr := mload(64)
                            mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lst_ptr, 4), receiver)
                            mstore(add(__lst_ptr, 36), outSeizedAssets)
                            mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                            let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                            if iszero(__lst_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 17)
                                mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 23)
                                    mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        if iszero(eq(callback, 0)) {
                            {
                                let __liqcb_ptr := mload(64)
                                let __liqcb_collateral_offset := add(market_data_offset, calldataload(add(market_data_offset, 32)))
                                let __liqcb_collateral_length := calldataload(__liqcb_collateral_offset)
                                let __liqcb_collateral_bytes := mul(__liqcb_collateral_length, 128)
                                let __liqcb_market_size := add(224, __liqcb_collateral_bytes)
                                let __liqcb_padded_market_size := and(add(__liqcb_market_size, 31), not(31))
                                let __liqcb_padded_data_len := and(add(data_length, 31), not(31))
                                let __liqcb_market_ptr := add(__liqcb_ptr, 324)
                                let __liqcb_data_ptr := add(__liqcb_market_ptr, __liqcb_padded_market_size)
                                let __liqcb_total := add(324, add(__liqcb_padded_market_size, add(32, __liqcb_padded_data_len)))
                                mstore(__liqcb_ptr, shl(224, 0x6861b795))
                                mstore(add(__liqcb_ptr, 4), sender)
                                mstore(add(__liqcb_ptr, 36), id)
                                mstore(add(__liqcb_ptr, 68), 320)
                                mstore(add(__liqcb_ptr, 100), collateralIndex)
                                mstore(add(__liqcb_ptr, 132), outSeizedAssets)
                                mstore(add(__liqcb_ptr, 164), outRepaidUnits)
                                mstore(add(__liqcb_ptr, 196), borrower)
                                mstore(add(__liqcb_ptr, 228), receiver)
                                mstore(add(__liqcb_ptr, 260), add(320, __liqcb_padded_market_size))
                                mstore(add(__liqcb_ptr, 292), badDebt)
                                mstore(__liqcb_market_ptr, calldataload(market_data_offset))
                                mstore(add(__liqcb_market_ptr, 32), 192)
                                mstore(add(__liqcb_market_ptr, 64), calldataload(add(market_data_offset, 64)))
                                mstore(add(__liqcb_market_ptr, 96), calldataload(add(market_data_offset, 96)))
                                mstore(add(__liqcb_market_ptr, 128), calldataload(add(market_data_offset, 128)))
                                mstore(add(__liqcb_market_ptr, 160), calldataload(add(market_data_offset, 160)))
                                mstore(add(__liqcb_market_ptr, 192), __liqcb_collateral_length)
                                calldatacopy(add(__liqcb_market_ptr, 224), add(__liqcb_collateral_offset, 32), __liqcb_collateral_bytes)
                                mstore(__liqcb_data_ptr, data_length)
                                calldatacopy(add(__liqcb_data_ptr, 32), data_data_offset, data_length)
                                mstore(64, add(__liqcb_ptr, and(add(__liqcb_total, 31), not(31))))
                                let __liqcb_success := call(gas(), callback, 0, __liqcb_ptr, __liqcb_total, __liqcb_ptr, 32)
                                if iszero(__liqcb_success) {
                                    let __liqcb_rds := returndatasize()
                                    returndatacopy(0, 0, __liqcb_rds)
                                    revert(0, __liqcb_rds)
                                }
                                if lt(returndatasize(), 32) {
                                    mstore(0, shl(224, 0x70b53d4b))
                                    revert(0, 4)
                                }
                                if iszero(eq(mload(__liqcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                                    mstore(0, shl(224, 0x70b53d4b))
                                    revert(0, 4)
                                }
                            }
                        }
                        let self := address()
                        {
                            if iszero(gt(extcodesize(__verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0)), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lstf_ptr := mload(64)
                            mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lstf_ptr, 4), payer)
                            mstore(add(__lstf_ptr, 36), self)
                            mstore(add(__lstf_ptr, 68), outRepaidUnits)
                            mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                            let __lstf_success := call(gas(), __verity_param_dynamic_head_word_calldata_checked(market_data_offset, 0), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                            if iszero(__lstf_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 21)
                                mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 27)
                                    mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        mstore(0, outSeizedAssets)
                        mstore(32, outRepaidUnits)
                        return(0, 64)
                    }
                    case 0x06b4290c {
                        /* isHealthy() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 96) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let id := calldataload(36)
                        let borrower := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                        if eq(debt, 0) {
                            mstore(0, 1)
                            return(0, 32)
                        }
                        let collateralCount := __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1)
                        let collateralBitmapValue := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), borrower), 2))), 340282366920938463463374607431768211455)
                        let maxDebt := 0
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := collateralCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
                            __forEach_idx := add(__forEach_idx, 1)
                        } {
                            i := __forEach_idx
                            let active := internal_internal_collateralBitmapIsSet(collateralBitmapValue, i)
                            if active {
                                let activeCollateral := internal_internal_collateralAmount(id, borrower, i)
                                let oracle := internal_internal_collateralOracleAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                                let price := internal_internal_oraclePrice(oracle)
                                let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), i)
                                let collateralValue := div(mul(activeCollateral, price), 1000000000000000000000000000000000000)
                                maxDebt := add(maxDebt, div(mul(collateralValue, lltv), 1000000000000000000))
                            }
                        }
                        mstore(0, iszero(gt(debt, maxDebt)))
                        return(0, 32)
                    }
                    case 0x3f8ccc0a {
                        /* setCollateralAmount() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(68)
                        let value := calldataload(100)
                        let _writeOk := internal_internal_writeCollateralAmount(id, user, index, value)
                        stop()
                    }
                    case 0xfd1b2036 {
                        /* supplyCollateral() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 128) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let collateralIndex := calldataload(36)
                        let assets := calldataload(68)
                        let onBehalf := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let authorized := internal_internal_isAuthorized(onBehalf, sender)
                        if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let id := internal_internal_toId(market_data_offset)
                        let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
                        let newCollateral := add(oldCollateral, assets)
                        if gt(newCollateral, 340282366920938463463374607431768211455) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x436173744f766572666c6f772829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                        if eq(oldCollateral, 0) {
                            if gt(assets, 0) {
                                let newBitmap := internal_internal_collateralBitmapSetBit(oldBitmap, collateralIndex)
                                {
                                    let __compat_value := newBitmap
                                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                    sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                                }
                                let activeCount := internal_internal_countBits128(newBitmap)
                                if gt(activeCount, 16) {
                                    {
                                        let __err_ptr := mload(64)
                                        mstore(add(__err_ptr, 0), 0x546f6f4d616e79416374697661746564436f6c6c61746572616c732829000000)
                                        let __err_hash := keccak256(__err_ptr, 29)
                                        let __err_selector := shl(224, shr(224, __err_hash))
                                        mstore(0, __err_selector)
                                        let __err_tail := 0
                                        revert(0, add(4, __err_tail))
                                    }
                                }
                            }
                        }
                        let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                        let self := address()
                        {
                            if iszero(gt(extcodesize(collateralToken), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lstf_ptr := mload(64)
                            mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lstf_ptr, 4), sender)
                            mstore(add(__lstf_ptr, 36), self)
                            mstore(add(__lstf_ptr, 68), assets)
                            mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                            let __lstf_success := call(gas(), collateralToken, 0, __lstf_ptr, 100, __lstf_ptr, 32)
                            if iszero(__lstf_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 21)
                                mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 27)
                                    mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
                        stop()
                    }
                    case 0x564b55d9 {
                        /* withdrawCollateral() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 164) {
                            revert(0, 0)
                        }
                        let market_offset := calldataload(4)
                        if lt(market_offset, 160) {
                            revert(0, 0)
                        }
                        let market_abs_offset := add(4, market_offset)
                        if gt(market_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let market_data_offset := market_abs_offset
                        let collateralIndex := calldataload(36)
                        let assets := calldataload(68)
                        let onBehalf := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)
                        let receiver := and(calldataload(132), 0xffffffffffffffffffffffffffffffffffffffff)
                        let sender := caller()
                        let authorized := internal_internal_isAuthorized(onBehalf, sender)
                        if iszero(or(iszero(iszero(eq(onBehalf, sender))), iszero(iszero(authorized)))) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x556e617574686f72697a65642829000000000000000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 14)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        let id := internal_internal_toId(market_data_offset)
                        let oldCollateral := internal_internal_collateralAmount(id, onBehalf, collateralIndex)
                        let newCollateral := sub(oldCollateral, assets)
                        let debt := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                        if gt(debt, 0) {
                            let lltv := internal_internal_collateralLltvAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                            let requiredCollateral := div(add(mul(debt, 1000000000000000000), sub(lltv, 1)), lltv)
                            if lt(newCollateral, requiredCollateral) {
                                {
                                    let __err_ptr := mload(64)
                                    mstore(add(__err_ptr, 0), 0x556e6865616c746879426f72726f776572282900000000000000000000000000)
                                    let __err_hash := keccak256(__err_ptr, 19)
                                    let __err_selector := shl(224, shr(224, __err_hash))
                                    mstore(0, __err_selector)
                                    let __err_tail := 0
                                    revert(0, add(4, __err_tail))
                                }
                            }
                        }
                        if eq(newCollateral, 0) {
                            if gt(assets, 0) {
                                let oldBitmap := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))), 340282366920938463463374607431768211455)
                                let newBitmap := internal_internal_collateralBitmapClearBit(oldBitmap, collateralIndex)
                                {
                                    let __compat_value := newBitmap
                                    let __compat_packed := and(__compat_value, 340282366920938463463374607431768211455)
                                    let __compat_slot_word := sload(add(mappingSlot(mappingSlot(0, id), onBehalf), 2))
                                    let __compat_slot_cleared := and(__compat_slot_word, not(115792089237316195423570985008687907852929702298719625575994209400481361428480))
                                    sstore(add(mappingSlot(mappingSlot(0, id), onBehalf), 2), or(__compat_slot_cleared, shl(128, __compat_packed)))
                                }
                            }
                        }
                        let collateralToken := internal_internal_collateralTokenAt(__verity_param_dynamic_member_data_offset_calldata_checked(market_data_offset, 1), __verity_param_dynamic_member_length_calldata_checked(market_data_offset, 1), collateralIndex)
                        {
                            if iszero(gt(extcodesize(collateralToken), 0)) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 7)
                                mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_ptr := mload(64)
                            mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                            mstore(add(__lst_ptr, 4), receiver)
                            mstore(add(__lst_ptr, 36), assets)
                            mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                            let __lst_success := call(gas(), collateralToken, 0, __lst_ptr, 68, __lst_ptr, 32)
                            if iszero(__lst_success) {
                                mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                mstore(4, 32)
                                mstore(36, 17)
                                mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                revert(0, 100)
                            }
                            let __lst_rds := returndatasize()
                            if __lst_rds {
                                if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 23)
                                    mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                    revert(0, 100)
                                }
                            }
                        }
                        internal_internal_setCollateralAmount(id, onBehalf, collateralIndex, newCollateral)
                        stop()
                    }
                    case 0x4f80fe10 {
                        /* flashLoan() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 132) {
                            revert(0, 0)
                        }
                        let tokens_offset := calldataload(4)
                        if lt(tokens_offset, 128) {
                            revert(0, 0)
                        }
                        let tokens_abs_offset := add(4, tokens_offset)
                        if gt(tokens_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let tokens_length := calldataload(tokens_abs_offset)
                        let tokens_tail_head_end := add(tokens_abs_offset, 32)
                        let tokens_tail_remaining := sub(calldatasize(), tokens_tail_head_end)
                        if gt(tokens_length, div(tokens_tail_remaining, 32)) {
                            revert(0, 0)
                        }
                        let tokens_data_offset := tokens_tail_head_end
                        let assets_offset := calldataload(36)
                        if lt(assets_offset, 128) {
                            revert(0, 0)
                        }
                        let assets_abs_offset := add(4, assets_offset)
                        if gt(assets_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let assets_length := calldataload(assets_abs_offset)
                        let assets_tail_head_end := add(assets_abs_offset, 32)
                        let assets_tail_remaining := sub(calldatasize(), assets_tail_head_end)
                        if gt(assets_length, div(assets_tail_remaining, 32)) {
                            revert(0, 0)
                        }
                        let assets_data_offset := assets_tail_head_end
                        let callback := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)
                        let data_offset := calldataload(100)
                        if lt(data_offset, 128) {
                            revert(0, 0)
                        }
                        let data_abs_offset := add(4, data_offset)
                        if gt(data_abs_offset, sub(calldatasize(), 32)) {
                            revert(0, 0)
                        }
                        let data_length := calldataload(data_abs_offset)
                        let data_tail_head_end := add(data_abs_offset, 32)
                        let data_tail_remaining := sub(calldatasize(), data_tail_head_end)
                        if gt(data_length, data_tail_remaining) {
                            revert(0, 0)
                        }
                        let data_data_offset := data_tail_head_end
                        let tokenCount := tokens_length
                        let assetCount := assets_length
                        if iszero(eq(tokenCount, assetCount)) {
                            {
                                let __err_ptr := mload(64)
                                mstore(add(__err_ptr, 0), 0x496e636f6e73697374656e74496e707574282900000000000000000000000000)
                                let __err_hash := keccak256(__err_ptr, 19)
                                let __err_selector := shl(224, shr(224, __err_hash))
                                mstore(0, __err_selector)
                                let __err_tail := 0
                                revert(0, add(4, __err_tail))
                            }
                        }
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := tokenCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
                            __forEach_idx := add(__forEach_idx, 1)
                        } {
                            i := __forEach_idx
                            {
                                if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 7)
                                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lst_ptr := mload(64)
                                mstore(__lst_ptr, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)
                                mstore(add(__lst_ptr, 4), callback)
                                mstore(add(__lst_ptr, 36), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                                mstore(64, and(add(add(__lst_ptr, 68), 31), not(31)))
                                let __lst_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lst_ptr, 68, __lst_ptr, 32)
                                if iszero(__lst_success) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 17)
                                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lst_rds := returndatasize()
                                if __lst_rds {
                                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lst_ptr), 1))) {
                                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                        mstore(4, 32)
                                        mstore(36, 23)
                                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)
                                        revert(0, 100)
                                    }
                                }
                            }
                        }
                        let sender := caller()
                        {
                            let __flcb_ptr := mload(64)
                            let __flcb_tokens_bytes := mul(tokens_length, 32)
                            let __flcb_assets_bytes := mul(assets_length, 32)
                            let __flcb_tokens_seg := add(32, __flcb_tokens_bytes)
                            let __flcb_assets_seg := add(32, __flcb_assets_bytes)
                            let __flcb_data_off := add(128, add(__flcb_tokens_seg, __flcb_assets_seg))
                            let __flcb_tokens_pos := add(__flcb_ptr, 132)
                            let __flcb_assets_pos := add(__flcb_ptr, add(132, __flcb_tokens_seg))
                            let __flcb_data_pos := add(__flcb_ptr, add(4, __flcb_data_off))
                            let __flcb_padded_data_len := and(add(data_length, 31), not(31))
                            let __flcb_total := add(add(__flcb_data_pos, 32), __flcb_padded_data_len)
                            __flcb_total := sub(__flcb_total, __flcb_ptr)
                            mstore(__flcb_ptr, shl(224, 0xd1f260c3))
                            mstore(add(__flcb_ptr, 4), sender)
                            mstore(add(__flcb_ptr, 36), 128)
                            mstore(add(__flcb_ptr, 68), add(128, __flcb_tokens_seg))
                            mstore(add(__flcb_ptr, 100), __flcb_data_off)
                            mstore(__flcb_tokens_pos, tokens_length)
                            calldatacopy(add(__flcb_tokens_pos, 32), tokens_data_offset, __flcb_tokens_bytes)
                            mstore(__flcb_assets_pos, assets_length)
                            calldatacopy(add(__flcb_assets_pos, 32), assets_data_offset, __flcb_assets_bytes)
                            mstore(__flcb_data_pos, data_length)
                            calldatacopy(add(__flcb_data_pos, 32), data_data_offset, data_length)
                            mstore(64, add(__flcb_ptr, and(add(__flcb_total, 31), not(31))))
                            let __flcb_success := call(gas(), callback, 0, __flcb_ptr, __flcb_total, __flcb_ptr, 32)
                            if iszero(__flcb_success) {
                                let __flcb_rds := returndatasize()
                                returndatacopy(0, 0, __flcb_rds)
                                revert(0, __flcb_rds)
                            }
                            if lt(returndatasize(), 32) {
                                revert(0, 0)
                            }
                            if iszero(eq(mload(__flcb_ptr), 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2)) {
                                revert(0, 0)
                            }
                        }
                        for {
                            let __forEach_idx := 0
                            let __forEach_count := tokenCount
                            let i := 0
                        } lt(__forEach_idx, __forEach_count) {
                            __forEach_idx := add(__forEach_idx, 1)
                        } {
                            i := __forEach_idx
                            let self := address()
                            {
                                if iszero(gt(extcodesize(__verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i)), 0)) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 7)
                                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lstf_ptr := mload(64)
                                mstore(__lstf_ptr, 0x23b872dd00000000000000000000000000000000000000000000000000000000)
                                mstore(add(__lstf_ptr, 4), callback)
                                mstore(add(__lstf_ptr, 36), self)
                                mstore(add(__lstf_ptr, 68), __verity_array_element_calldata_checked(assets_data_offset, assets_length, i))
                                mstore(64, and(add(add(__lstf_ptr, 100), 31), not(31)))
                                let __lstf_success := call(gas(), __verity_array_element_calldata_checked(tokens_data_offset, tokens_length, i), 0, __lstf_ptr, 100, __lstf_ptr, 32)
                                if iszero(__lstf_success) {
                                    mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                    mstore(4, 32)
                                    mstore(36, 21)
                                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)
                                    revert(0, 100)
                                }
                                let __lst_rds := returndatasize()
                                if __lst_rds {
                                    if iszero(and(gt(__lst_rds, 31), eq(mload(__lstf_ptr), 1))) {
                                        mstore(0, 0x08c379a000000000000000000000000000000000000000000000000000000000)
                                        mstore(4, 32)
                                        mstore(36, 27)
                                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)
                                        revert(0, 100)
                                    }
                                }
                            }
                        }
                        stop()
                    }
                    case 0xecdcc72d {
                        /* collateral() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 100) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let index := calldataload(68)
                        let value := internal_internal_collateralAmount(id, user, index)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xc60c6271 {
                        /* pendingFee() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(128, sload(mappingSlot(mappingSlot(0, id), user))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xe20e9a64 {
                        /* lastAccrual() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0x8c366a0a {
                        /* lastLossFactor() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(0, sload(add(mappingSlot(mappingSlot(0, id), user), 1))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    case 0xb502e1f9 {
                        /* collateralBitmap() */
                        if callvalue() {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        if lt(calldatasize(), 68) {
                            revert(0, 0)
                        }
                        let id := calldataload(4)
                        let user := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)
                        let value := and(shr(128, sload(add(mappingSlot(mappingSlot(0, id), user), 2))), 340282366920938463463374607431768211455)
                        mstore(0, value)
                        return(0, 32)
                    }
                    default {
                        revert(0, 0)
                    }
                }
            }
        }
    }
}