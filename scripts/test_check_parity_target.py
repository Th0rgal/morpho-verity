#!/usr/bin/env python3
"""Regression tests for parity-target config parsing."""

from __future__ import annotations

import json
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_parity_target import (  # noqa: E402
  ParityTargetError,
  load_target,
  parse_solc_version,
  parse_foundry_default,
  parse_yul_identity_gate_mode,
  read_text,
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

  def test_rejects_invalid_utf8(self) -> None:
    temp_dir = tempfile.TemporaryDirectory()
    self.addCleanup(temp_dir.cleanup)
    path = pathlib.Path(temp_dir.name) / "parity-target.json"
    path.write_bytes(b"\xff")

    with self.assertRaisesRegex(ParityTargetError, "is not valid UTF-8"):
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


class ReadTextTests(unittest.TestCase):
  def test_rejects_invalid_utf8(self) -> None:
    temp_dir = tempfile.TemporaryDirectory()
    self.addCleanup(temp_dir.cleanup)
    path = pathlib.Path(temp_dir.name) / "foundry.toml"
    path.write_bytes(b"\xff")

    with self.assertRaisesRegex(ParityTargetError, "failed to decode text file as UTF-8"):
      read_text(path)


class ParseSolcVersionTests(unittest.TestCase):
  def test_wraps_unparseable_output(self) -> None:
    with mock.patch("check_parity_target.subprocess.check_output", return_value="solc ???"):
      with self.assertRaisesRegex(ParityTargetError, "unable to parse `solc --version` output"):
        parse_solc_version()


class CliTests(unittest.TestCase):
  def test_cli_reports_invalid_utf8_foundry_without_traceback(self) -> None:
    temp_dir = tempfile.TemporaryDirectory()
    self.addCleanup(temp_dir.cleanup)
    foundry_path = pathlib.Path(temp_dir.name) / "foundry.toml"
    foundry_path.write_bytes(b"\xff")

    proc = subprocess.run(
      [
        sys.executable,
        "-c",
        "\n".join([
          "import pathlib, sys",
          f"sys.path.insert(0, {str(pathlib.Path(__file__).resolve().parent)!r})",
          "import check_parity_target",
          f"check_parity_target.FOUNDRY_PATH = pathlib.Path({str(foundry_path)!r})",
          "check_parity_target.parse_solc_version = lambda: ('0.8.24', 'deadbeef')",
          "check_parity_target.main()",
        ]),
      ],
      capture_output=True,
      text=True,
      check=False,
    )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("parity-target check failed: failed to decode text file as UTF-8", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
