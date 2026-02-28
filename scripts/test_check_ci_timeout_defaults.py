#!/usr/bin/env python3
"""Unit tests for CI timeout defaults sync check."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_timeout_defaults import (  # noqa: E402
  collect_run_timeout_defaults,
  collect_timeout_env_literals,
  main,
  parse_timeout_env_file,
)


class CheckCiTimeoutDefaultsTests(unittest.TestCase):
  def test_parse_timeout_env_file(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      defaults.write_text("A_TIMEOUT=10\nB_TIMEOUT=20\n", encoding="utf-8")
      self.assertEqual(parse_timeout_env_file(defaults), {"A_TIMEOUT": 10, "B_TIMEOUT": 20})

  def test_collect_run_timeout_defaults(self) -> None:
    workflow = (
      "run: ./scripts/run_with_timeout.sh A_TIMEOUT 10 \"A\" -- cmd\n"
      "run: ./scripts/run_with_timeout.sh A_TIMEOUT 10 \"A2\" -- cmd\n"
      "run: ./scripts/run_with_timeout.sh B_TIMEOUT 20 \"B\" -- cmd\n"
    )
    self.assertEqual(collect_run_timeout_defaults(workflow), {"A_TIMEOUT": {10}, "B_TIMEOUT": {20}})

  def test_collect_timeout_env_literals(self) -> None:
    workflow = (
      "env:\n"
      "  A_TIMEOUT: \"10\"\n"
      "  B_TIMEOUT_SEC: \"20\"\n"
    )
    self.assertEqual(collect_timeout_env_literals(workflow), {"A_TIMEOUT": {10}, "B_TIMEOUT_SEC": {20}})

  def test_repo_files_are_in_sync(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      workflow.write_text(
        "run: ./scripts/run_with_timeout.sh MORPHO_A_TIMEOUT_SEC 10 \"A\" -- cmd\n"
        "env:\n"
        "  MORPHO_A_TIMEOUT_SEC: \"10\"\n",
        encoding="utf-8",
      )
      defaults.write_text("MORPHO_A_TIMEOUT_SEC=10\n", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_timeout_defaults.py",
          "--workflow",
          str(workflow),
          "--defaults",
          str(defaults),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
