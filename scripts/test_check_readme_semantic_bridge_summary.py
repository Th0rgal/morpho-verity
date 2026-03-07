#!/usr/bin/env python3
"""Unit tests for the README semantic-bridge summary sync guard."""

from __future__ import annotations

import json
import pathlib
import subprocess
import sys
import tempfile
import textwrap
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_readme_semantic_bridge_summary import (  # noqa: E402
  UPSTREAM_STATUS_PREFIX,
  ReadmeSemanticBridgeSummaryError,
  extract_semantic_bridge_section,
  extract_link1_operations,
  main,
  normalize_text,
  validate_summary,
)
from check_semantic_bridge_readiness_summary import derive_summary  # noqa: E402


def make_config() -> dict:
  return {
    "obligations": [
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "supplySemEq",
        "operation": "supply",
        "status": "assumed",
        "macroMigrated": False,
      },
      {
        "id": "OBL-SET-OWNER-SEM-EQ",
        "hypothesis": "setOwnerSemEq",
        "operation": "setOwner",
        "status": "in_progress",
        "macroMigrated": True,
      },
      {
        "id": "OBL-FLASH-LOAN-SEM-EQ",
        "hypothesis": "flashLoanSemEq",
        "operation": "flashLoan",
        "status": "discharged",
        "macroMigrated": True,
      },
    ]
  }


def make_readme(
  proven_count: int = 2,
  total: int = 3,
  assumed_count: int = 1,
  operations: str = "`setOwner`, `flashLoan`",
) -> str:
  return textwrap.dedent(
    f"""\
    # Morpho Verity

    ### Upstream: Verity hybrid canonical-semantics migration (verity#1060 / #1065)

    {UPSTREAM_STATUS_PREFIX}
    This removes the hand-rolled `interpretSpec` interpreter from the target trust
    story and enables auto-generated semantic preservation proofs in the
    `verity_contract` macro where the frontend can lower the contract successfully.

    **Link 1 proofs (stable `Morpho.*` wrapper API ↔ EDSL) are now proven for {proven_count}/{total} operations:**
    {operations}
    The remaining {assumed_count}/{total} operations still have assumed Link 1 status in
    `config/semantic-bridge-obligations.json`.
    See `Morpho/Proofs/SemanticBridgeDischarge.lean`.

    Machine-readable parity target artifacts:
    - `config/parity-target.json`
    """
  )


class ReadmeSemanticBridgeSummaryTests(unittest.TestCase):
  def test_normalize_text_collapses_whitespace(self) -> None:
    self.assertEqual(normalize_text("alpha\n\n beta\tgamma"), "alpha beta gamma")

  def test_extract_semantic_bridge_section(self) -> None:
    section = extract_semantic_bridge_section(make_readme())
    self.assertIn(UPSTREAM_STATUS_PREFIX, section)
    self.assertIn("Link 1 proofs", section)

  def test_extract_link1_operations(self) -> None:
    self.assertEqual(
      extract_link1_operations(extract_semantic_bridge_section(make_readme())),
      ["setOwner", "flashLoan"],
    )

  def test_extract_link1_operations_allows_trailing_period_on_last_item(self) -> None:
    self.assertEqual(
      extract_link1_operations(
        extract_semantic_bridge_section(
          make_readme(operations="`setOwner`, `flashLoan`.")
        )
      ),
      ["setOwner", "flashLoan"],
    )

  def test_extract_link1_operations_allows_none_sentinel(self) -> None:
    self.assertEqual(
      extract_link1_operations(
        extract_semantic_bridge_section(
          make_readme(proven_count=0, assumed_count=3, operations="none")
        )
      ),
      [],
    )

  def test_validate_summary_accepts_matching_readme(self) -> None:
    validate_summary(make_readme(), derive_summary(make_config()))

  def test_validate_summary_accepts_zero_link1_operations(self) -> None:
    summary = {
      "total": 3,
      "link1_count": 0,
      "link1_operations": [],
      "assumed_count": 3,
    }
    validate_summary(make_readme(proven_count=0, assumed_count=3, operations="none"), summary)

  def test_extract_link1_operations_rejects_mixed_none_sentinel(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "mixes the none sentinel",
    ):
      extract_link1_operations(
        make_readme(proven_count=0, assumed_count=3, operations="none, `setOwner`")
      )

  def test_extract_link1_operations_rejects_repeated_none_sentinel(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "repeats the none sentinel",
    ):
      extract_link1_operations(
        extract_semantic_bridge_section(
          make_readme(proven_count=0, assumed_count=3, operations="none, none")
        )
      )

  def test_extract_link1_operations_rejects_unquoted_named_operation(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "contains malformed entries",
    ):
      extract_link1_operations(
        extract_semantic_bridge_section(
          make_readme(proven_count=0, assumed_count=3, operations="none, setOwner")
        )
      )

  def test_validate_summary_rejects_proven_count_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 proof summary drift",
    ):
      validate_summary(make_readme(proven_count=1), derive_summary(make_config()))

  def test_validate_summary_rejects_assumed_count_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README assumed Link 1 summary drift",
    ):
      validate_summary(make_readme(assumed_count=2), derive_summary(make_config()))

  def test_validate_summary_rejects_operation_list_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 operation list drift",
    ):
      validate_summary(make_readme(operations="`setOwner`, `enableIrm`"), derive_summary(make_config()))

  def test_validate_summary_rejects_duplicate_operation_list_entries(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "duplicate operations",
    ):
      validate_summary(make_readme(operations="`setOwner`, `setOwner`"), derive_summary(make_config()))

  def test_validate_summary_rejects_upstream_status_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "upstream semantic-bridge status drift",
    ):
      validate_summary(
        make_readme().replace(
          UPSTREAM_STATUS_PREFIX,
          "The Verity framework is working toward a single machine-checked theorem per\n"
          "contract function: `EDSL execution ≡ EVMYulLean(compile(CompilationModel))`.",
        ),
        derive_summary(make_config()),
      )

  def test_validate_summary_rejects_upstream_status_drift_hidden_outside_section(self) -> None:
    readme_text = make_readme().replace(UPSTREAM_STATUS_PREFIX, "old prefix", 1)
    readme_text += f"\n## Notes\n\n{UPSTREAM_STATUS_PREFIX}\n"
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "upstream semantic-bridge status drift",
    ):
      validate_summary(readme_text, derive_summary(make_config()))

  def test_validate_summary_accepts_wrapped_upstream_status_prefix(self) -> None:
    readme_text = make_readme().replace(
      UPSTREAM_STATUS_PREFIX,
      "The Verity framework now has the upstream typed-IR / canonical-semantics bridge "
      "for supported `verity_contract` functions:\n"
      "`EDSL execution ≡ EVMYulLean(compile(CompilationModel))`.",
      1,
    )
    validate_summary(readme_text, derive_summary(make_config()))

  def test_validate_summary_rejects_link1_summary_drift_hidden_outside_section(self) -> None:
    readme_text = make_readme(proven_count=1)
    readme_text += (
      "\n## Notes\n\n"
      "**Link 1 proofs (stable `Morpho.*` wrapper API ↔ EDSL) are now proven for 2/3 operations:**\n"
      "`setOwner`, `flashLoan`\n"
      "The remaining 1/3 operations still have assumed Link 1 status in\n"
      "`config/semantic-bridge-obligations.json`.\n"
    )
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 proof summary drift",
    ):
      validate_summary(readme_text, derive_summary(make_config()))

  def test_validate_summary_rejects_operation_drift_hidden_outside_section(self) -> None:
    readme_text = make_readme(operations="`setOwner`, `enableIrm`")
    readme_text += "\n## Notes\n\n`setOwner`, `flashLoan`\n"
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 operation list drift",
    ):
      validate_summary(readme_text, derive_summary(make_config()))

  def test_validate_summary_rejects_missing_semantic_bridge_end_boundary(self) -> None:
    readme_text = make_readme().replace(
      "Machine-readable parity target artifacts:\n    - `config/parity-target.json`\n",
      "\n",
      1,
    )
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "semantic-bridge section end boundary",
    ):
      validate_summary(readme_text, derive_summary(make_config()))

  def test_main_passes_for_synced_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readme_path = root / "README.md"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readme_path.write_text(make_readme(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_readme_semantic_bridge_summary.py",
          "--config",
          str(config_path),
          "--readme",
          str(readme_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_cli_reports_invalid_json_config_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readme_path = root / "README.md"
      config_path.write_text("{not json", encoding="utf-8")
      readme_path.write_text(make_readme(), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_readme_semantic_bridge_summary.py"),
          "--config",
          str(config_path),
          "--readme",
          str(readme_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("readme-semantic-bridge-summary check failed:", proc.stderr)
    self.assertIn("failed to parse JSON config", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_invalid_utf8_readme_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readme_path = root / "README.md"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readme_path.write_bytes(b"\xff")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_readme_semantic_bridge_summary.py"),
          "--config",
          str(config_path),
          "--readme",
          str(readme_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("readme-semantic-bridge-summary check failed:", proc.stderr)
    self.assertIn("failed to decode README file", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_missing_readme_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readme_path = root / "README.md"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_readme_semantic_bridge_summary.py"),
          "--config",
          str(config_path),
          "--readme",
          str(readme_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("readme-semantic-bridge-summary check failed:", proc.stderr)
    self.assertIn("failed to read README file", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
