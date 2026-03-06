#!/usr/bin/env python3
"""Unit tests for shared parity-target config helpers."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from parity_target_config import parse_yul_identity_gate_mode  # noqa: E402


class ParseYulIdentityGateModeTests(unittest.TestCase):
  def test_accepts_supported_gate_modes(self) -> None:
    self.assertEqual(
      parse_yul_identity_gate_mode(
        {"yulIdentity": {"gateMode": "unsupported-manifest"}},
        missing_message="missing gate mode",
        invalid_message_prefix="invalid gate mode",
      ),
      "unsupported-manifest",
    )
    self.assertEqual(
      parse_yul_identity_gate_mode(
        {"yulIdentity": {"gateMode": "exact"}},
        missing_message="missing gate mode",
        invalid_message_prefix="invalid gate mode",
      ),
      "exact",
    )

  def test_rejects_missing_gate_mode(self) -> None:
    with self.assertRaisesRegex(RuntimeError, "missing gate mode"):
      parse_yul_identity_gate_mode(
        {},
        missing_message="missing gate mode",
        invalid_message_prefix="invalid gate mode",
      )

  def test_rejects_unknown_gate_mode_with_suffix(self) -> None:
    with self.assertRaisesRegex(
      RuntimeError,
      r"invalid gate mode \(expected one of: exact, unsupported-manifest\)\.",
    ):
      parse_yul_identity_gate_mode(
        {"yulIdentity": {"gateMode": "strict"}},
        missing_message="missing gate mode",
        invalid_message_prefix="invalid gate mode",
        invalid_message_suffix=".",
      )


if __name__ == "__main__":
  unittest.main()
