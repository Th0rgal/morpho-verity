#!/usr/bin/env python3
"""Regression checks for the Midnight SSTORE2 Yul postprocessor."""

from __future__ import annotations

import pathlib
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "patch_midnight_sstore2_yul.py"


class PatchMidnightSstore2YulTest(unittest.TestCase):
  def test_callback_patch_and_no_diagnostic_stores(self) -> None:
    text = SCRIPT.read_text(encoding="utf-8")

    self.assertIn("__midnight_call_repay_callback", text)
    self.assertIn("__midnight_call_ratifier", text)
    self.assertIn("__midnight_call_flashloan_callback", text)
    self.assertIn("0x675ef8d3", text)
    self.assertIn("0xd1f260c3", text)
    self.assertIn("0xac9650d8", text)
    self.assertIn("delegatecall(gas(), address()", text)
    self.assertIn("mstore(0, __midnight_market_id(market_data_offset))", text)
    self.assertIn("external marketState scaffold id alias", text)
    self.assertIn("external settlementFee scaffold id alias", text)
    self.assertIn("external isHealthy scaffold id alias", text)
    self.assertIn("0xfc56f72e", text)
    self.assertIn("0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2", text)
    self.assertIn("0x8fd212bc8fa18d807a9b47aa2de07104bf036cfcef8ea259157085a1b618a77c", text)
    self.assertIn("0xd64453ff5184816a23d670b5224717cd441c4a7cd5f4e7dbc30f8511d35f597b", text)
    self.assertIn("0x7a5e8e1731f88cf0c25f88fc7d5618e481e87be4d83f614e601850c2ff082fd7", text)
    self.assertIn("0x9e0c6d3ffe2895519e5543fe8da6e54858f4c06530d7557d808068b0ecdc9bc3", text)
    self.assertIn("mstore(0x40e0, buyerPendingFeeIncrease)", text)
    self.assertIn("mstore(0x4100, sellerPendingFeeDecrease)", text)
    self.assertIn("__midnight_tick_to_price", text)
    self.assertIn("sstore(mappingSlot(9, loanToken)", text)
    self.assertIn("mstore(0, buyerAssets)", text)
    self.assertIn("log4(0x4000, 416", text)
    self.assertIn("log3(__event_ptr, 96", text)
    self.assertIn("log4(__event_ptr, 32", text)
    self.assertIn("log4(__event_ptr, 96", text)
    self.assertIn("deployed := create2(0, ptr, init_len, sload(0))", text)
    self.assertIn("pop(__midnight_store_market_in_code(marketBase))", text)
    self.assertNotIn("sstore(999", text)
    self.assertNotIn("sstore(1000", text)
    self.assertNotIn("sstore(1001", text)
    self.assertNotIn("deployed := 1", text)


if __name__ == "__main__":
  unittest.main()
