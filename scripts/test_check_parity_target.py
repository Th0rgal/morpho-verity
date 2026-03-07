#!/usr/bin/env python3
"""Regression tests for parity-target config parsing."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_parity_target import (  # noqa: E402
  ParityTargetError,
  load_target,
  parse_foundry_default,
  parse_yul_identity_gate_mode,
)


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


class LoadTargetTests(unittest.TestCase):
  def write_target(self, payload: object) -> pathlib.Path:
    temp_dir = tempfile.TemporaryDirectory()
    self.addCleanup(temp_dir.cleanup)
    path = pathlib.Path(temp_dir.name) / "parity-target.json"
    path.write_text(json.dumps(payload), encoding="utf-8")
    return path

  def test_rejects_invalid_json(self) -> None:
    temp_dir = tempfile.TemporaryDirectory()
    self.addCleanup(temp_dir.cleanup)
    path = pathlib.Path(temp_dir.name) / "parity-target.json"
    path.write_text("{", encoding="utf-8")

    with self.assertRaisesRegex(ParityTargetError, "is not valid JSON"):
      load_target(path)

  def test_rejects_non_object_root(self) -> None:
    path = self.write_target([])

    with self.assertRaisesRegex(ParityTargetError, "root must be a JSON object"):
      load_target(path)

  def test_rejects_missing_foundry_profile_fields(self) -> None:
    path = self.write_target(
      {
        "id": "parity-target",
        "solc": {"version": "0.8.24", "commit": "abcd1234"},
        "foundryDefaultProfile": {
          "optimizer": True,
          "optimizerRuns": 200,
          "viaIR": True,
          "evmVersion": "shanghai",
        },
        "verity": {"parityPackId": "pack-1"},
      }
    )

    with self.assertRaisesRegex(
      ParityTargetError, "missing non-empty string `foundryDefaultProfile.bytecodeHash`"
    ):
      load_target(path)

  def test_rejects_non_boolean_optimizer(self) -> None:
    path = self.write_target(
      {
        "id": "parity-target",
        "solc": {"version": "0.8.24"},
        "foundryDefaultProfile": {
          "optimizer": "true",
          "optimizerRuns": 200,
          "viaIR": True,
          "evmVersion": "shanghai",
          "bytecodeHash": "none",
        },
        "verity": {"parityPackId": "pack-1"},
      }
    )

    with self.assertRaisesRegex(
      ParityTargetError, "missing boolean `foundryDefaultProfile.optimizer`"
    ):
      load_target(path)

  def test_rejects_empty_verity_pack_id(self) -> None:
    path = self.write_target(
      {
        "id": "parity-target",
        "solc": {"version": "0.8.24"},
        "foundryDefaultProfile": {
          "optimizer": True,
          "optimizerRuns": 200,
          "viaIR": True,
          "evmVersion": "shanghai",
          "bytecodeHash": "none",
        },
        "verity": {"parityPackId": ""},
      }
    )

    with self.assertRaisesRegex(ParityTargetError, "missing non-empty string `verity.parityPackId`"):
      load_target(path)


if __name__ == "__main__":
  unittest.main()
