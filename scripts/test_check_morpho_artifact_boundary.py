#!/usr/bin/env python3
"""Unit tests for Morpho artifact-boundary checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_morpho_artifact_boundary.py"
MAIN_PATH = ROOT / "Morpho" / "Compiler" / "Main.lean"
ARTIFACT_CONFIG_PATH = ROOT / "Morpho" / "Compiler" / "ArtifactConfig.lean"
TRUST_DOC_PATH = ROOT / "docs" / "TRUST_BOUNDARIES.md"


class CheckMorphoArtifactBoundaryTests(unittest.TestCase):
  def test_repo_boundary_is_clean(self) -> None:
    proc = subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_detects_direct_manual_surface_usage(self) -> None:
    original = MAIN_PATH.read_text(encoding="utf-8")
    try:
      MAIN_PATH.write_text(
        original + "\n-- test sentinel\n#eval morphoSelectors.length\n",
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("Main.lean", proc.stderr)
    finally:
      MAIN_PATH.write_text(original, encoding="utf-8")

  def test_detects_legacy_compiler_model_file(self) -> None:
    legacy_path = ROOT / "Morpho" / "Compiler" / "Spec.lean"
    self.assertFalse(legacy_path.exists(), "repo must not ship the legacy Spec.lean")
    try:
      legacy_path.write_text("def morphoSpec := 0\n", encoding="utf-8")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("legacy Morpho compiler model files must not exist", proc.stderr)
      self.assertIn("Morpho/Compiler/Spec.lean", proc.stderr)
    finally:
      legacy_path.unlink(missing_ok=True)

  def test_detects_legacy_protocol_projection_file(self) -> None:
    legacy_path = ROOT / "Morpho" / "Types.lean"
    self.assertFalse(legacy_path.exists(), "repo must not ship the legacy Types.lean projection model")
    try:
      legacy_path.write_text("structure MorphoState where\n  owner : Nat\n", encoding="utf-8")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("legacy Morpho compiler model files must not exist", proc.stderr)
      self.assertIn("Morpho/Types.lean", proc.stderr)
    finally:
      legacy_path.unlink(missing_ok=True)

  def test_detects_second_model_marker_outside_contract(self) -> None:
    probe_path = ROOT / "Morpho" / "Compiler" / "SecondModelProbe.lean"
    try:
      probe_path.write_text("def marketId := 0\n", encoding="utf-8")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("second protocol model marker", proc.stderr)
      self.assertIn("Morpho/Compiler/SecondModelProbe.lean", proc.stderr)
    finally:
      probe_path.unlink(missing_ok=True)

  def test_detects_unexpected_linked_external(self) -> None:
    original = ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8")
    try:
      ARTIFACT_CONFIG_PATH.write_text(
        original.replace(
          "externals := []",
          'externals := [{ name := "borrowRate", params := [.uint256], returnType := some .uint256, returns := [.uint256], axiomNames := ["irm_borrow_rate_boundary"] }]',
          1,
        ),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("must not declare linked externals", proc.stderr)
    finally:
      ARTIFACT_CONFIG_PATH.write_text(original, encoding="utf-8")

  def test_detects_artifact_config_macro_slice_import(self) -> None:
    original = ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8")
    try:
      ARTIFACT_CONFIG_PATH.write_text(
        original.replace("import Morpho.Contract", "import Morpho.Compiler.MacroSlice"),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("unexpected imports", proc.stderr)
      self.assertIn("Morpho.Compiler.MacroSlice", proc.stderr)
    finally:
      ARTIFACT_CONFIG_PATH.write_text(original, encoding="utf-8")

  def test_detects_artifact_config_extra_import(self) -> None:
    original = ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8")
    try:
      ARTIFACT_CONFIG_PATH.write_text(
        original + "\nimport Morpho.Libraries.MathLib\n",
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("unexpected imports", proc.stderr)
      self.assertIn("Morpho.Libraries.MathLib", proc.stderr)
    finally:
      ARTIFACT_CONFIG_PATH.write_text(original, encoding="utf-8")

  def test_detects_artifact_config_protocol_declaration(self) -> None:
    original = ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8")
    try:
      ARTIFACT_CONFIG_PATH.write_text(
        original + "\nstructure ArtifactProtocolState where\n  owner : Nat\n",
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("unexpected declarations", proc.stderr)
      self.assertIn("ArtifactProtocolState", proc.stderr)
    finally:
      ARTIFACT_CONFIG_PATH.write_text(original, encoding="utf-8")

  def test_detects_artifact_config_not_wrapping_contract_spec(self) -> None:
    original = ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8")
    try:
      ARTIFACT_CONFIG_PATH.write_text(
        original.replace("_root_.Morpho.Contract.Morpho.spec", "dummySpec"),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("artifactSpec must wrap _root_.Morpho.Contract.Morpho.spec", proc.stderr)
    finally:
      ARTIFACT_CONFIG_PATH.write_text(original, encoding="utf-8")

  def test_external_axiom_validator_accepts_required_boundaries(self) -> None:
    from check_morpho_artifact_boundary import validate_artifact_config_external_axioms

    validate_artifact_config_external_axioms(ARTIFACT_CONFIG_PATH.read_text(encoding="utf-8"))

  def test_detects_missing_trust_boundary_doc_entry(self) -> None:
    original = TRUST_DOC_PATH.read_text(encoding="utf-8")
    try:
      TRUST_DOC_PATH.write_text(
        original.replace("`flash_loan_transfers`", "`flash_loan_transfers_removed`"),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("TRUST_BOUNDARIES.md drift", proc.stderr)
      self.assertIn("flash_loan_transfers", proc.stderr)
    finally:
      TRUST_DOC_PATH.write_text(original, encoding="utf-8")

  def test_reports_invalid_utf8_without_traceback(self) -> None:
    original = MAIN_PATH.read_bytes()
    try:
      MAIN_PATH.write_bytes(b"\xff")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertEqual(proc.returncode, 1)
      self.assertIn("morpho-artifact-boundary check failed:", proc.stderr)
      self.assertIn("Main.lean is not valid UTF-8", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)
    finally:
      MAIN_PATH.write_bytes(original)


if __name__ == "__main__":
  unittest.main()
