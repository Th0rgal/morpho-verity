{
  function flashLoanCallback(borrower, assets) -> ret {
    // Call onMorphoFlashLoan(uint256,bytes) on borrower
    // selector: 0x31f57072
    // For simplicity, pass (assets, empty bytes)
    mstore(0x300, 0x31f5707200000000000000000000000000000000000000000000000000000000)
    mstore(0x304, assets)
    mstore(0x324, 64)  // offset to bytes
    mstore(0x344, 0)   // bytes length = 0
    let success := call(gas(), borrower, 0, 0x300, 100, 0x300, 0)
    if iszero(success) { revert(0, 0) }
    ret := 0
  }
}
