#!/usr/bin/env python3
"""Unit tests for workflow hashFiles() reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_hashfiles_refs import (  # noqa: E402
  CiWorkflowHashFilesRefsError,
  collect_workflow_hashfiles_patterns,
  expand_hashfiles_pattern,
  iter_hashfiles_call_args,
  main,
  parse_hashfiles_call_patterns,
)


class CollectWorkflowHashFilesPatternsTests(unittest.TestCase):
  def test_collects_patterns_from_multiple_hashfiles_calls(self) -> None:
    workflow_text = "\n".join(
      [
        "key: ${{ runner.os }}-${{ hashFiles('lean-toolchain', 'lakefile.lean') }}",
        "key: ${{ runner.os }}-${{ hashFiles('config/*.json', '!config/private/*.json') }}",
      ]
    )

    self.assertEqual(
      collect_workflow_hashfiles_patterns(workflow_text),
      ["lean-toolchain", "lakefile.lean", "config/*.json", "!config/private/*.json"],
    )

  def test_collect_ignores_non_expression_hashfiles_text(self) -> None:
    workflow_text = "\n".join(
      [
        "- name: Validate CI workflow hashFiles() references",
        "  run: echo \"hashFiles('config/*.json')\"",
      ]
    )

    self.assertEqual(collect_workflow_hashfiles_patterns(workflow_text), [])

  def test_collect_rejects_non_literal_hashfiles_call(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowHashFilesRefsError,
      r"hashFiles\(\) call must use quoted literal patterns",
    ):
      collect_workflow_hashfiles_patterns("key: ${{ hashFiles(matrix.cacheKey) }}")

  def test_collect_preserves_parentheses_inside_quoted_pattern(self) -> None:
    workflow_text = (
      "key: ${{ runner.os }}-${{ hashFiles('config/cache(1).json', 'config/(extra).json') }}"
    )

    self.assertEqual(
      collect_workflow_hashfiles_patterns(workflow_text),
      ["config/cache(1).json", "config/(extra).json"],
    )

  def test_collect_rejects_unterminated_hashfiles_call(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowHashFilesRefsError,
      r"hashFiles\(\) call is missing a closing parenthesis",
    ):
      collect_workflow_hashfiles_patterns("key: ${{ hashFiles('config/cache.json' }}")


class ParseHashFilesCallPatternsTests(unittest.TestCase):
  def test_accepts_parentheses_and_commas_inside_quoted_patterns(self) -> None:
    self.assertEqual(
      parse_hashfiles_call_patterns("'config/cache(1).json', \"config/part,2.json\""),
      ["config/cache(1).json", "config/part,2.json"],
    )

  def test_rejects_mixed_literal_and_non_literal_arguments(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowHashFilesRefsError,
      r"hashFiles\(\) call must use quoted literal patterns",
    ):
      parse_hashfiles_call_patterns("'config/cache.json', matrix.extra")


class IterHashFilesCallArgsTests(unittest.TestCase):
  def test_returns_complete_args_when_pattern_contains_parenthesis(self) -> None:
    self.assertEqual(
      iter_hashfiles_call_args("runner.os && hashFiles('config/cache(1).json', 'config/a.json')"),
      ["'config/cache(1).json', 'config/a.json'"],
    )


class ExpandHashFilesPatternTests(unittest.TestCase):
  def test_expand_matches_repo_files(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      (root / "config").mkdir()
      expected = root / "config" / "parity-target.json"
      expected.write_text("{}", encoding="utf-8")

      self.assertEqual(expand_hashfiles_pattern(root, "config/*.json"), [expected])


class CliTests(unittest.TestCase):
  def test_main_reports_missing_hashfiles_match(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "key: ${{ runner.os }}-${{ hashFiles('config/missing.json') }}\n",
        encoding="utf-8",
      )
      (root / "config").mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_hashfiles_refs.py"),
          "--workflow",
          str(workflow_path),
          "--repo-root",
          str(root),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-hashfiles-refs check failed: "
        "workflow hashFiles() patterns matched no files: config/missing.json",
        proc.stderr,
      )

  def test_main_accepts_relative_workflow_and_repo_root_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.parent.mkdir(parents=True)
      workflow_path.write_text(
        "key: ${{ runner.os }}-${{ hashFiles('config/*.json') }}\n",
        encoding="utf-8",
      )
      config_dir = root / "repo" / "config"
      config_dir.mkdir(parents=True)
      (config_dir / "parity-target.json").write_text("{}", encoding="utf-8")
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_hashfiles_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
          "--repo-root",
          str(pathlib.Path("..") / "repo"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-hashfiles-refs check: OK", proc.stdout)

  def test_main_function_passes_when_all_hashfiles_match(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      (root / "config").mkdir()
      (root / "config" / "parity-target.json").write_text("{}", encoding="utf-8")
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "key: ${{ runner.os }}-${{ hashFiles('config/parity-target.json') }}\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_hashfiles_refs.py",
          "--workflow",
          str(workflow_path),
          "--repo-root",
          str(root),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
