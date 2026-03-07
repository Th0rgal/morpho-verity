#!/usr/bin/env python3
"""Unit tests for workflow cache restore-key checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_cache_restore_keys import (  # noqa: E402
  CiWorkflowCacheRestoreKeysError,
  collect_cache_key_restore_keys,
  main,
  validate_cache_restore_keys,
)


class CollectCacheKeyRestoreKeysTests(unittest.TestCase):
  def test_collects_cache_keys_and_restore_keys(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - name: Restore Lean cache",
        "        uses: actions/cache@v4",
        "        with:",
        "          key: ${{ runner.os }}-lean-${{ hashFiles('lean-toolchain') }}",
        "          restore-keys: |",
        "            ${{ runner.os }}-lean-",
        "            ${{ runner.os }}-fallback-",
      ]
    )

    self.assertEqual(
      collect_cache_key_restore_keys(workflow_text),
      [
        (
          "Restore Lean cache",
          "${{ runner.os }}-lean-${{ hashFiles('lean-toolchain') }}",
          ["${{ runner.os }}-lean-", "${{ runner.os }}-fallback-"],
        )
      ],
    )

  def test_collects_inline_restore_keys(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/cache@v4",
        "        with:",
        "          key: cache-key",
        "          restore-keys: cache-",
      ]
    )

    self.assertEqual(
      collect_cache_key_restore_keys(workflow_text),
      [(None, "cache-key", ["cache-"])],
    )


class ValidateCacheRestoreKeysTests(unittest.TestCase):
  def test_rejects_missing_key(self) -> None:
    with self.assertRaisesRegex(CiWorkflowCacheRestoreKeysError, r"must define with\.key"):
      validate_cache_restore_keys("Restore cache", None, ["cache-"])

  def test_rejects_missing_restore_keys(self) -> None:
    with self.assertRaisesRegex(CiWorkflowCacheRestoreKeysError, r"must define at least one"):
      validate_cache_restore_keys("Restore cache", "cache-key", [])

  def test_rejects_non_prefix_restore_key(self) -> None:
    with self.assertRaisesRegex(CiWorkflowCacheRestoreKeysError, r"is not a prefix of key"):
      validate_cache_restore_keys("Restore cache", "linux-lean-abc", ["linux-foundry-"])

  def test_allows_prefix_restore_keys(self) -> None:
    validate_cache_restore_keys("Restore cache", "${{ runner.os }}-lean-${{ hashFiles('x') }}", ["${{ runner.os }}-lean-"])


class CliTests(unittest.TestCase):
  def test_main_reports_missing_restore_keys(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - name: Restore Lean cache",
            "        uses: actions/cache@v4",
            "        with:",
            "          key: lean-cache",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_cache_restore_keys.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-cache-restore-keys check failed: "
        "cache step Restore Lean cache must define at least one with.restore-keys entry",
        proc.stderr,
      )

  def test_main_reports_non_prefix_restore_key(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - name: Restore Lean cache",
            "        uses: actions/cache@v4",
            "        with:",
            "          key: linux-lean-${{ hashFiles('lean-toolchain') }}",
            "          restore-keys: |",
            "            linux-foundry-",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_cache_restore_keys.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("ci-workflow-cache-restore-keys check failed:", proc.stderr)
      self.assertIn("is not a prefix of key", proc.stderr)

  def test_main_accepts_relative_workflow_path_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.parent.mkdir(parents=True)
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/cache@v4",
            "        with:",
            "          key: cache-key",
            "          restore-keys: cache-",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_cache_restore_keys.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-cache-restore-keys check: OK", proc.stdout)

  def test_main_function_passes_for_prefix_restore_key(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/cache@v4",
            "        with:",
            "          key: cache-key",
            "          restore-keys: cache-",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_cache_restore_keys.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
