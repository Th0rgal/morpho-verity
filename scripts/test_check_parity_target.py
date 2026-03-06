#!/usr/bin/env python3
"""Regression tests for parity-target config parsing."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_parity_target import parse_foundry_default, parse_yul_identity_gate_mode  # noqa: E402


class ParseFoundryDefaultTests(unittest.TestCase):
  def test_inline_comments_are_accepted(self) -> None:
    text = """
[profile.default]
optimizer = true # enabled
optimizer_runs = 200 # production parity
via_ir = false # not via-ir
evm_version = "shanghai" # chain target
bytecode_hash = "none" # deterministic bytecode
"""
    got = parse_foundry_default(text)
    self.assertEqual(
      got,
      {
        "optimizer": True,
        "optimizerRuns": 200,
        "viaIR": False,
        "evmVersion": "shanghai",
        "bytecodeHash": "none",
      },
    )

  def test_optimizer_runs_with_underscores_are_accepted(self) -> None:
    text = """
[profile.default]
optimizer = true
optimizer_runs = 999_999
via_ir = true
evm_version = "paris"
bytecode_hash = "none"
"""
    got = parse_foundry_default(text)
    self.assertEqual(got["optimizerRuns"], 999999)


class ParseYulIdentityGateModeTests(unittest.TestCase):
  def test_accepts_supported_gate_modes(self) -> None:
    self.assertEqual(
      parse_yul_identity_gate_mode({"yulIdentity": {"gateMode": "unsupported-manifest"}}),
      "unsupported-manifest",
    )
    self.assertEqual(
      parse_yul_identity_gate_mode({"yulIdentity": {"gateMode": "exact"}}),
      "exact",
    )

  def test_rejects_missing_gate_mode(self) -> None:
    with self.assertRaisesRegex(RuntimeError, "missing required config `yulIdentity.gateMode`"):
      parse_yul_identity_gate_mode({})

  def test_rejects_unknown_gate_mode(self) -> None:
    with self.assertRaisesRegex(RuntimeError, "invalid config `yulIdentity.gateMode`"):
      parse_yul_identity_gate_mode({"yulIdentity": {"gateMode": "strict"}})


if __name__ == "__main__":
  unittest.main()
