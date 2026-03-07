#!/usr/bin/env python3
"""Unit tests for macro migration selector-slice checker."""

from __future__ import annotations

import json
import pathlib
import re
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_slice import (  # noqa: E402
  MigrationSliceError,
  ROOT,
  extract_macro_signatures,
  load_baseline,
  validate_baseline_metadata,
  run_check,
  validate_blocked_against_baseline,
  validate_against_baseline,
)


class ParseMacroSliceTests(unittest.TestCase):
  def test_extract_macro_signatures(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function owner () : Address := do
    return 0

  function nonce (authorizer : Address) : Uint256 := do
    return 0
"""
    self.assertEqual(
      extract_macro_signatures(text),
      {"owner()", "nonce(address)"},
    )

  def test_extract_macro_signatures_unsupported_type_fails(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function bad (id : Id) : Uint256 := do
    return 0
"""
    with self.assertRaises(MigrationSliceError):
      extract_macro_signatures(text)

  def test_extract_macro_signatures_tuple_params(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function setFee (marketParams : Tuple [Address, Address, Address, Address, Uint256], newFee : Uint256) : Unit := do
    pure ()
"""
    self.assertEqual(
      extract_macro_signatures(text),
      {"setFee((address,address,address,address,uint256),uint256)"},
    )

  def test_extract_macro_signatures_tuple_uint8_param(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function setAuthorizationWithSig (authorization : Tuple [Address, Address, Bool, Uint256, Uint256], signature : Tuple [Uint8, Bytes32, Bytes32]) : Unit := do
    pure ()
"""
    self.assertEqual(
      extract_macro_signatures(text),
      {"setAuthorizationWithSig((address,address,bool,uint256,uint256),(uint8,bytes32,bytes32))"},
    )


class BaselineValidationTests(unittest.TestCase):
  def test_validate_against_baseline_matches(self) -> None:
    validate_against_baseline(
      {"owner()"},
      {"expectedMigrated": ["owner()"]},
    )

  def test_validate_against_baseline_detects_drift(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_against_baseline(
        {"owner()", "nonce(address)"},
        {"expectedMigrated": ["owner()"]},
      )

  def test_validate_against_baseline_rejects_non_list_expected_migrated(self) -> None:
    with self.assertRaisesRegex(MigrationSliceError, "expectedMigrated must be a list"):
      validate_against_baseline(
        {"owner()"},
        {"expectedMigrated": "owner()"},
      )

  def test_validate_against_baseline_rejects_invalid_signature_entry(self) -> None:
    with self.assertRaisesRegex(MigrationSliceError, "contain only non-empty signatures"):
      validate_against_baseline(
        {"owner()"},
        {"expectedMigrated": ["owner()", ""]},
      )

  def test_validate_against_baseline_rejects_duplicate_signatures(self) -> None:
    with self.assertRaisesRegex(MigrationSliceError, "duplicate signatures"):
      validate_against_baseline(
        {"owner()"},
        {"expectedMigrated": ["owner()", "owner()"]},
      )

  def test_validate_blocked_against_baseline_matches(self) -> None:
    out = validate_blocked_against_baseline(
      spec_signatures={"owner()", "nonce(address)"},
      migrated_signatures={"owner()"},
      baseline={"expectedBlocked": {"nonce(address)": "pending migration"}},
    )
    self.assertEqual(out, {"nonce(address)": "pending migration"})

  def test_validate_blocked_against_baseline_detects_unclassified(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_blocked_against_baseline(
        spec_signatures={"owner()", "nonce(address)"},
        migrated_signatures={"owner()"},
        baseline={"expectedBlocked": {}},
      )

  def test_validate_blocked_against_baseline_detects_overlap(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_blocked_against_baseline(
        spec_signatures={"owner()"},
        migrated_signatures={"owner()"},
        baseline={"expectedBlocked": {"owner()": "invalid"}},
      )

  def test_validate_blocked_against_baseline_rejects_invalid_signature_key(self) -> None:
    with self.assertRaisesRegex(MigrationSliceError, "invalid signature key"):
      validate_blocked_against_baseline(
        spec_signatures={"owner()", "nonce(address)"},
        migrated_signatures={"owner()"},
        baseline={"expectedBlocked": {"": "tracked"}},
      )

  def test_validate_baseline_metadata_matches(self) -> None:
    validate_baseline_metadata(
      baseline={
        "contract": "MorphoViewSlice",
        "source": "Morpho/Compiler/MacroSlice.lean",
      },
      macro_path=ROOT / "Morpho" / "Compiler" / "MacroSlice.lean",
      contract_name="MorphoViewSlice",
    )

  def test_validate_baseline_metadata_detects_contract_mismatch(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_baseline_metadata(
        baseline={
          "contract": "OtherSlice",
          "source": "Morpho/Compiler/MacroSlice.lean",
        },
        macro_path=ROOT / "Morpho" / "Compiler" / "MacroSlice.lean",
        contract_name="MorphoViewSlice",
      )

  def test_validate_baseline_metadata_detects_source_mismatch(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_baseline_metadata(
        baseline={
          "contract": "MorphoViewSlice",
          "source": "Morpho/Compiler/NotMacroSlice.lean",
        },
        macro_path=ROOT / "Morpho" / "Compiler" / "MacroSlice.lean",
        contract_name="MorphoViewSlice",
      )

  def test_validate_blocked_against_baseline_rejects_placeholder_reason(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_blocked_against_baseline(
        spec_signatures={"owner()", "nonce(address)"},
        migrated_signatures={"owner()"},
        baseline={
          "expectedBlocked": {
            "nonce(address)": "pending macro migration (tracked blocker)",
          }
        },
      )

  def test_load_baseline_rejects_malformed_json(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "baseline.json"
      path.write_text("{invalid\n", encoding="utf-8")

      with self.assertRaisesRegex(MigrationSliceError, "not valid JSON"):
        load_baseline(path)

  def test_load_baseline_rejects_non_object_root(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "baseline.json"
      path.write_text('["owner()"]\n', encoding="utf-8")

      with self.assertRaisesRegex(MigrationSliceError, "must contain a JSON object"):
        load_baseline(path)

  def test_load_baseline_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "baseline.json"
      path.write_bytes(b"\x80")

      with self.assertRaisesRegex(MigrationSliceError, "baseline file is not valid UTF-8"):
        load_baseline(path)

  def test_write_mode_and_report_support_external_absolute_paths(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      spec_path = root / "Spec.lean"
      macro_path = root / "MacroSlice.lean"
      baseline_path = root / "baseline.json"
      repo_report = run_check()
      spec_path.write_text((ROOT / "Morpho" / "Compiler" / "Spec.lean").read_text(encoding="utf-8"), encoding="utf-8")
      macro_path.write_text((ROOT / "Morpho" / "Compiler" / "MacroSlice.lean").read_text(encoding="utf-8"), encoding="utf-8")
      baseline_path.write_text(
        json.dumps(
          {
            "source": str(macro_path),
            "contract": "MorphoViewSlice",
            "expectedMigrated": repo_report["migratedSignatures"],
            "expectedBlocked": repo_report["blockedSignatures"],
          }
        ),
        encoding="utf-8",
      )

      report = run_check(spec_path=spec_path, macro_path=macro_path, baseline_path=baseline_path)

    self.assertEqual(report["specPath"], str(spec_path))
    self.assertEqual(report["macroPath"], str(macro_path))


class RepoCheckTests(unittest.TestCase):
  def test_flash_loan_event_topic_matches_spec(self) -> None:
    macro_text = (ROOT / "Morpho" / "Compiler" / "MacroSlice.lean").read_text()
    spec_text = (ROOT / "Morpho" / "Compiler" / "Spec.lean").read_text()
    spec_match = re.search(
      r"flashLoanEventTopic\s*:\s*Nat\s*:=\s*(0x[0-9a-f]+)",
      spec_text,
    )
    self.assertIsNotNone(spec_match)
    expected_topic = str(int(spec_match.group(1), 16))
    self.assertIn(
      f"rawLog [{expected_topic}, sender, token] 0 32",
      macro_text,
    )

  def test_current_repo_slice_matches(self) -> None:
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["contract"], "MorphoViewSlice")
    self.assertTrue((ROOT / report["macroPath"]).exists())
    self.assertGreaterEqual(report["migratedCount"], 1)


class CliTests(unittest.TestCase):
  def test_cli_reports_invalid_utf8_macro_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      spec_path = pathlib.Path(d) / "Spec.lean"
      macro_path = pathlib.Path(d) / "MacroSlice.lean"
      baseline_path = pathlib.Path(d) / "baseline.json"
      spec_path.write_text((ROOT / "Morpho" / "Compiler" / "Spec.lean").read_text(encoding="utf-8"), encoding="utf-8")
      macro_path.write_bytes(b"\x80")
      baseline_path.write_text((ROOT / "config" / "macro-migration-slice.json").read_text(encoding="utf-8"), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_slice.py"),
          "--spec",
          str(spec_path),
          "--macro",
          str(macro_path),
          "--baseline",
          str(baseline_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("macro-migration-slice check failed:", proc.stderr)
    self.assertIn("failed to decode UTF-8 text file", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_json_out_failure_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      json_out = pathlib.Path(d) / "occupied"
      json_out.write_text("not a directory", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          "-c",
          "\n".join([
            "import sys",
            f"sys.path.insert(0, {str(ROOT / 'scripts')!r})",
            "import check_macro_migration_slice as mod",
            "mod.run_check = lambda **kwargs: {'status': 'ok', 'specPath': 'Spec.lean', 'macroPath': 'MacroSlice.lean', 'contract': 'MorphoViewSlice', 'migratedCount': 1, 'specSignatureCount': 1, 'coveragePct': 100.0, 'migratedSignatures': ['owner()'], 'blockedCount': 0, 'blockedSignatures': {}}",
            f"sys.argv = ['check_macro_migration_slice.py', '--json-out', {str(json_out / 'report.json')!r}]",
            "try:",
            "  mod.main()",
            "except mod.MigrationSliceError as exc:",
            "  print(f'macro-migration-slice check failed: {exc}', file=sys.stderr)",
            "  raise SystemExit(1)",
          ]),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("macro-migration-slice check failed: failed to write JSON report", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
