{
  function keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv) -> id {
    // Keep the Solidity free-memory pointer slot at 0x40 untouched.
    // Event encoding paths in generated Yul read mload(0x40), so clobbering
    // that slot can cause huge memory expansion and OOG/invalid-operand errors.
    mstore(0x240, loanToken)
    mstore(0x260, collateralToken)
    mstore(0x280, oracle)
    mstore(0x2a0, irm)
    mstore(0x2c0, lltv)
    id := keccak256(0x240, 0xa0)
  }
}
