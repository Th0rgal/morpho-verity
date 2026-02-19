{
  function keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv) -> id {
    mstore(0x00, loanToken)
    mstore(0x20, collateralToken)
    mstore(0x40, oracle)
    mstore(0x60, irm)
    mstore(0x80, lltv)
    id := keccak256(0x00, 0xa0)
  }
}
