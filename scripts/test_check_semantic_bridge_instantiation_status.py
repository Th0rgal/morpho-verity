#!/usr/bin/env python3
"""Unit tests for SemanticBridgeInstantiation upstream-status sync guard."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import textwrap
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_semantic_bridge_instantiation_status import (  # noqa: E402
  EXPECTED_INTRO_STATUS,
  EXPECTED_SUMMARY_COMPOSITION,
  EXPECTED_SUMMARY_HEADING,
  EXPECTED_SUMMARY_STATUS,
  FORBIDDEN_SNIPPETS,
  SemanticBridgeInstantiationStatusError,
  main,
  validate_status,
)


def make_text() -> str:
  return textwrap.dedent(
    f"""\
    /-!
    # Semantic Bridge Instantiation

    {EXPECTED_INTRO_STATUS}
    -/

    namespace Morpho.Proofs.SemanticBridgeInstantiation

    /-! ## Summary

    {EXPECTED_SUMMARY_HEADING}

    These theorems prove that the EDSL execution preserves Morpho invariants.
    {EXPECTED_SUMMARY_STATUS}

    - **Link 2**: EDSL ≡ compiled CompilationModel IR
    - **Link 3**: compiled IR ≡ EVMYulLean(Yul)

    {EXPECTED_SUMMARY_COMPOSITION}
    -/
    """
  )


class SemanticBridgeInstantiationStatusTests(unittest.TestCase):
  def test_validate_status_accepts_matching_text(self) -> None:
    validate_status(make_text())

  def test_validate_status_rejects_missing_intro_status(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing expected text",
    ):
      validate_status(make_text().replace(EXPECTED_INTRO_STATUS, "old intro"))

  def test_validate_status_rejects_missing_summary_heading(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing expected text",
    ):
      validate_status(make_text().replace(EXPECTED_SUMMARY_HEADING, "### Remaining gap: Links 2+3"))

  def test_validate_status_rejects_stale_future_tense_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "stale future-tense bridge text",
    ):
      validate_status(make_text() + "\n" + FORBIDDEN_SNIPPETS[0])

  def test_main_passes_for_synced_file(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      instantiation_path = root / "SemanticBridgeInstantiation.lean"
      instantiation_path.write_text(make_text(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_semantic_bridge_instantiation_status.py",
          "--instantiation",
          str(instantiation_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
