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
  NAMESPACE_HEADER,
  SemanticBridgeInstantiationStatusError,
  extract_summary_section,
  extract_validates_section,
  main,
  normalize_text,
  validate_status,
)


def make_text() -> str:
  return textwrap.dedent(
    f"""\
    /-!
    # Semantic Bridge Instantiation

    ## What this validates

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
  def test_normalize_text_collapses_whitespace(self) -> None:
    self.assertEqual(normalize_text("alpha\n\n beta\tgamma"), "alpha beta gamma")

  def test_extract_validates_section_returns_target_block(self) -> None:
    self.assertIn(EXPECTED_INTRO_STATUS, extract_validates_section(make_text()))

  def test_extract_summary_section_returns_target_block(self) -> None:
    self.assertIn(EXPECTED_SUMMARY_COMPOSITION, extract_summary_section(make_text()))

  def test_extract_validates_section_rejects_missing_header(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing `## What this validates` section",
    ):
      extract_validates_section(make_text().replace("## What this validates", "## Validation"))

  def test_extract_summary_section_rejects_missing_header(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing `## Summary` section",
    ):
      extract_summary_section(make_text().replace("/-! ## Summary", "/-! ## Closing"))

  def test_validate_status_accepts_matching_text(self) -> None:
    validate_status(make_text())

  def test_validate_status_accepts_wrapped_intro_and_summary_text(self) -> None:
    validate_status(
      make_text()
      .replace(
        "supported fragment, so for the\noperations instantiated here",
        "supported fragment, so for the\n\noperations instantiated here",
      )
      .replace(
        "the operations above. The\nremaining repo-local work",
        "the operations above.\nThe\nremaining repo-local work",
      )
    )

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
      validate_status(
        make_text().replace(
          EXPECTED_SUMMARY_STATUS,
          EXPECTED_SUMMARY_STATUS + "\n\n" + FORBIDDEN_SNIPPETS[0],
          1,
        )
      )

  def test_validate_status_rejects_intro_drift_hidden_by_duplicate_elsewhere(self) -> None:
    stale_text = make_text().replace(EXPECTED_INTRO_STATUS, "old intro", 1)
    masked_text = stale_text + "\n" + EXPECTED_INTRO_STATUS
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing expected text",
    ):
      validate_status(masked_text)

  def test_validate_status_rejects_summary_drift_hidden_by_duplicate_elsewhere(self) -> None:
    stale_text = make_text().replace(EXPECTED_SUMMARY_COMPOSITION, "old summary", 1)
    masked_text = stale_text.replace(
      NAMESPACE_HEADER,
      NAMESPACE_HEADER + "\n\n" + EXPECTED_SUMMARY_COMPOSITION,
      1,
    )
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing expected text",
    ):
      validate_status(masked_text)

  def test_validate_status_rejects_missing_intro_docblock_boundary(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeInstantiationStatusError,
      "missing closing `-/` for `## What this validates` section",
    ):
      validate_status(make_text().replace("-/", "", 1))

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
