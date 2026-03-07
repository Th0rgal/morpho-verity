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
  collect_script_timeout_refs,
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

  def test_collect_run_timeout_defaults_supports_line_continuation(self) -> None:
    workflow = (
      "run: ./scripts/run_with_timeout.sh \\\n"
      "  A_TIMEOUT 10 \"A\" -- cmd\n"
    )
    self.assertEqual(collect_run_timeout_defaults(workflow), {"A_TIMEOUT": {10}})

  def test_collect_run_timeout_defaults_ignores_nested_non_step_run_mapping(self) -> None:
    workflow = (
      "jobs:\n"
      "  test:\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        with:\n"
      "          run: ./scripts/run_with_timeout.sh FAKE_TIMEOUT 99 \"fake\" -- cmd\n"
      "        run: ./scripts/run_with_timeout.sh REAL_TIMEOUT 10 \"real\" -- cmd\n"
    )
    self.assertEqual(collect_run_timeout_defaults(workflow), {"REAL_TIMEOUT": {10}})

  def test_collect_timeout_env_literals(self) -> None:
    workflow = (
      "env:\n"
      "  A_TIMEOUT: \"10\"\n"
      "  B_TIMEOUT_SEC: \"20\"\n"
    )
    self.assertEqual(collect_timeout_env_literals(workflow), {"A_TIMEOUT": {10}, "B_TIMEOUT_SEC": {20}})

  def test_collect_timeout_env_literals_ignores_nested_non_env_metadata(self) -> None:
    workflow = (
      "jobs:\n"
      "  test:\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        with:\n"
      "          env:\n"
      "            FAKE_TIMEOUT_SEC: \"99\"\n"
      "        env:\n"
      "          REAL_TIMEOUT_SEC: \"10\"\n"
      "        run: ./scripts/run_with_timeout.sh REAL_TIMEOUT_SEC 10 \"real\" -- cmd\n"
    )
    self.assertEqual(collect_timeout_env_literals(workflow), {"REAL_TIMEOUT_SEC": {10}})

  def test_collect_timeout_env_literals_handles_inline_flow_mapping(self) -> None:
    workflow = (
      "env: {MORPHO_TOP_TIMEOUT_SEC: \"10\", NON_TIMEOUT: foo}\n"
      "jobs:\n"
      "  test:\n"
      "    env: {MORPHO_JOB_TIMEOUT_SEC: '20'}\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: {MORPHO_STEP_TIMEOUT_SEC: 30}\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_JOB_TIMEOUT_SEC": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_handles_inline_flow_mapping_comment(self) -> None:
    workflow = (
      "env: {MORPHO_TOP_TIMEOUT_SEC: \"10\"} # workflow timeout\n"
      "jobs:\n"
      "  test:\n"
      "    env: {MORPHO_JOB_TIMEOUT_SEC: '20'} # job timeout\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: {MORPHO_STEP_TIMEOUT_SEC: 30} # step timeout\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_JOB_TIMEOUT_SEC": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_handles_multiline_inline_flow_mapping(self) -> None:
    workflow = (
      "env: {MORPHO_TOP_TIMEOUT_SEC: \"1\\\n"
      "    0\", MORPHO_TOP_TIMEOUT_COPY: &top_timeout \"2\\\n"
      "    0\", MORPHO_TOP_TIMEOUT_ALIAS: *top_timeout}\n"
      "jobs:\n"
      "  test:\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: {MORPHO_STEP_TIMEOUT_SEC: \"3\\\n"
      "            0\"}\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_TOP_TIMEOUT_COPY": {20},
        "MORPHO_TOP_TIMEOUT_ALIAS": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_supports_env_mapping_properties(self) -> None:
    workflow = (
      "env: &workflow_env\n"
      "  MORPHO_TOP_TIMEOUT_SEC: \"10\"\n"
      "jobs:\n"
      "  test:\n"
      "    env: !!map {MORPHO_JOB_TIMEOUT_SEC: '20'}\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: &step_env\n"
      "          MORPHO_STEP_TIMEOUT_SEC: 30\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_JOB_TIMEOUT_SEC": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_handles_block_env_value_comments(self) -> None:
    workflow = (
      "env:\n"
      "  MORPHO_TOP_TIMEOUT_SEC: \"10\" # workflow timeout\n"
      "jobs:\n"
      "  test:\n"
      "    env:\n"
      "      MORPHO_JOB_TIMEOUT_SEC: 20 # job timeout\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env:\n"
      "          MORPHO_STEP_TIMEOUT_SEC: 30 # step timeout\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_JOB_TIMEOUT_SEC": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_handles_inline_step_item_env_mapping(self) -> None:
    workflow = (
      "jobs:\n"
      "  test:\n"
      "    steps:\n"
      "      - env: {MORPHO_STEP_TIMEOUT_SEC: 30}\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(collect_timeout_env_literals(workflow), {"MORPHO_STEP_TIMEOUT_SEC": {30}})

  def test_collect_timeout_env_literals_keeps_following_step_after_unparseable_inline_env_alias(self) -> None:
    workflow = (
      "jobs:\n"
      "  test:\n"
      "    env: *shared_defaults\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env:\n"
      '          MORPHO_STEP_TIMEOUT_SEC: "30"\n'
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(collect_timeout_env_literals(workflow), {"MORPHO_STEP_TIMEOUT_SEC": {30}})

  def test_collect_timeout_env_literals_accepts_inline_flow_mapping_trailing_comma(self) -> None:
    workflow = (
      'env: {MORPHO_TOP_TIMEOUT_SEC: "10",}\n'
      "jobs:\n"
      "  test:\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: {MORPHO_STEP_TIMEOUT_SEC: 30,}\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_timeout_env_literals_supports_yaml_scalar_tags_and_anchors(self) -> None:
    workflow = (
      "env:\n"
      "  MORPHO_TOP_TIMEOUT_SEC: !!str \"10\"\n"
      "jobs:\n"
      "  test:\n"
      "    env:\n"
      "      MORPHO_JOB_TIMEOUT_SEC: &job_timeout '20'\n"
      "    steps:\n"
      "      - name: Validate timeout defaults\n"
      "        env: {MORPHO_STEP_TIMEOUT_SEC: !!str &step_timeout \"30\"}\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_STEP_TIMEOUT_SEC 30 \"real\" -- cmd\n"
    )
    self.assertEqual(
      collect_timeout_env_literals(workflow),
      {
        "MORPHO_TOP_TIMEOUT_SEC": {10},
        "MORPHO_JOB_TIMEOUT_SEC": {20},
        "MORPHO_STEP_TIMEOUT_SEC": {30},
      },
    )

  def test_collect_script_timeout_refs(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "run_alpha.sh").write_text(
        "echo ${ALPHA_TIMEOUT_SEC:-30}\n",
        encoding="utf-8",
      )
      (scripts_dir / "check_beta.py").write_text(
        "value = \"BETA_TIMEOUT_SEC\"\n",
        encoding="utf-8",
      )
      (scripts_dir / "test_ignore.sh").write_text(
        "echo ${SHOULD_NOT_COUNT_TIMEOUT_SEC:-0}\n",
        encoding="utf-8",
      )
      self.assertEqual(
        collect_script_timeout_refs(scripts_dir),
        {"ALPHA_TIMEOUT_SEC", "BETA_TIMEOUT_SEC"},
      )

  def test_repo_files_are_in_sync(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      workflow.write_text(
        "run: ./scripts/run_with_timeout.sh MORPHO_A_TIMEOUT_SEC 10 \"A\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PREP_TIMEOUT_SEC 10 \"prep\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_YUL_IDENTITY_TIMEOUT_SEC 10 \"outer1\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC 10 \"outer2\" -- cmd\n"
        "env:\n"
        "  MORPHO_A_TIMEOUT_SEC: \"10\"\n",
        encoding="utf-8",
      )
      defaults.write_text(
        "MORPHO_A_TIMEOUT_SEC=10\n"
        "MORPHO_VERITY_PREP_TIMEOUT_SEC=10\n"
        "MORPHO_YUL_IDENTITY_TIMEOUT_SEC=10\n"
        "MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=10\n",
        encoding="utf-8",
      )

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

  def test_nested_timeout_invariant_violation_fails(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      workflow.write_text(
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PREP_TIMEOUT_SEC 4500 \"prep\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_YUL_IDENTITY_TIMEOUT_SEC 4200 \"outer\" -- cmd\n",
        encoding="utf-8",
      )
      defaults.write_text(
        "MORPHO_VERITY_PREP_TIMEOUT_SEC=4500\n"
        "MORPHO_YUL_IDENTITY_TIMEOUT_SEC=4200\n"
        "MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=5100\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_timeout_defaults.py",
          "--workflow",
          str(workflow),
          "--defaults",
          str(defaults),
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

  def test_nested_timeout_invariant_valid_passes(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      workflow.write_text(
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PREP_TIMEOUT_SEC 4500 \"prep\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_YUL_IDENTITY_TIMEOUT_SEC 5100 \"outer1\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC 5100 \"outer2\" -- cmd\n",
        encoding="utf-8",
      )
      defaults.write_text(
        "MORPHO_VERITY_PREP_TIMEOUT_SEC=4500\n"
        "MORPHO_YUL_IDENTITY_TIMEOUT_SEC=5100\n"
        "MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=5100\n",
        encoding="utf-8",
      )

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

  def test_stale_defaults_fail(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      defaults = pathlib.Path(tmp_dir) / "defaults.env"
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      workflow.write_text(
        "run: ./scripts/run_with_timeout.sh MORPHO_ACTIVE_TIMEOUT_SEC 10 \"active\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PREP_TIMEOUT_SEC 10 \"prep\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_YUL_IDENTITY_TIMEOUT_SEC 10 \"outer1\" -- cmd\n"
        "run: ./scripts/run_with_timeout.sh MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC 10 \"outer2\" -- cmd\n",
        encoding="utf-8",
      )
      defaults.write_text(
        "MORPHO_ACTIVE_TIMEOUT_SEC=10\n"
        "MORPHO_VERITY_PREP_TIMEOUT_SEC=10\n"
        "MORPHO_YUL_IDENTITY_TIMEOUT_SEC=10\n"
        "MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC=10\n"
        "MORPHO_STALE_TIMEOUT_SEC=42\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_timeout_defaults.py",
          "--workflow",
          str(workflow),
          "--defaults",
          str(defaults),
          "--scripts-dir",
          str(scripts_dir),
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
