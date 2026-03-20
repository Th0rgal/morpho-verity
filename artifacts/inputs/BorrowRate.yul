{
  function borrowRate(irm, totalBorrowAssets) -> rate {
    // staticcall to IRM contract fallback — returns borrow rate
    // Use scratch space at 0x300 to avoid clobbering memory 0-0x200
    mstore(0x300, totalBorrowAssets)
    let success := staticcall(gas(), irm, 0x300, 32, 0x300, 32)
    if iszero(success) { revert(0, 0) }
    rate := mload(0x300)
  }
}
