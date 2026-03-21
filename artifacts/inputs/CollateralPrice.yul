{
  function collateralPrice(oracle) -> price {
    // staticcall oracle.price() — selector 0xa035b1fe
    // Use scratch space at 0x300 to avoid clobbering memory 0-0x200
    mstore(0x300, 0xa035b1fe00000000000000000000000000000000000000000000000000000000)
    let success := staticcall(gas(), oracle, 0x300, 4, 0x300, 32)
    if iszero(success) { revert(0, 0) }
    price := mload(0x300)
  }
}
