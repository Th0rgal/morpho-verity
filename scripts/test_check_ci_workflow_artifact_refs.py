#!/usr/bin/env python3
"""Unit tests for workflow artifact reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_artifact_refs import (  # noqa: E402
  collect_workflow_artifact_references,
  main,
)


class CollectWorkflowArtifactReferencesTests(unittest.TestCase):
  def test_collects_upload_and_download_artifact_names(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - name: Upload built bundle",
        "        uses: actions/upload-artifact@v4",
        "        with:",
        "          name: verity-edsl-artifacts",
        "          path: out/parity-shared",
        "      - name: Download built bundle",
        "        uses: actions/download-artifact@v4",
        "        with:",
        "          name: verity-edsl-artifacts",
        "          path: out/parity-shared",
      ]
    )

    self.assertEqual(
      collect_workflow_artifact_references(workflow_text),
      (
        ["verity-edsl-artifacts"],
        ["verity-edsl-artifacts"],
        {"verify": ["verity-edsl-artifacts"]},
        {"verify": ["verity-edsl-artifacts"]},
      ),
    )

  def test_preserves_quoted_hash_and_strips_inline_comments(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/upload-artifact@v4 # keep",
        "        with:",
        '          name: "verity#bundle"',
        "      - uses: actions/download-artifact@v4",
        "        with:",
        '          name: "verity#bundle" # still same artifact',
      ]
    )

    self.assertEqual(
      collect_workflow_artifact_references(workflow_text),
      (
        ["verity#bundle"],
        ["verity#bundle"],
        {"verify": ["verity#bundle"]},
        {"verify": ["verity#bundle"]},
      ),
    )

  def test_preserves_plain_hash_without_treating_it_as_comment(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/upload-artifact@v4",
        "        with:",
        "          name: verity#bundle",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        "          name: verity#bundle",
      ]
    )

    self.assertEqual(
      collect_workflow_artifact_references(workflow_text),
      (
        ["verity#bundle"],
        ["verity#bundle"],
        {"verify": ["verity#bundle"]},
        {"verify": ["verity#bundle"]},
      ),
    )

  def test_unescapes_quoted_artifact_names(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/upload-artifact@v4",
        "        with:",
        "          name: 'Don''t drift'",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        '          name: "Say \\"hi\\""',
      ]
    )

    self.assertEqual(
      collect_workflow_artifact_references(workflow_text),
      (
        ["Don't drift"],
        ['Say "hi"'],
        {"verify": ["Don't drift"]},
        {"verify": ['Say "hi"']},
      ),
    )

  def test_rejects_download_steps_without_explicit_name(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        "          path: out/all-artifacts",
      ]
    )

    with self.assertRaisesRegex(
      RuntimeError,
      "download-artifact step missing non-empty with.name",
    ):
      collect_workflow_artifact_references(workflow_text)

  def test_rejects_download_steps_with_empty_name(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        '          name: ""',
      ]
    )

    with self.assertRaisesRegex(
      RuntimeError,
      "download-artifact step missing non-empty with.name",
    ):
      collect_workflow_artifact_references(workflow_text)


class CliTests(unittest.TestCase):
  def test_main_reports_download_without_matching_upload(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: missing-artifact",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
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
        "ci-workflow-artifact-refs check failed: "
        "workflow downloads artifacts with no matching upload step: missing-artifact",
        proc.stderr,
      )

  def test_main_reports_download_without_explicit_name(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          path: out/all-artifacts",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
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
        "ci-workflow-artifact-refs check failed: download-artifact step missing non-empty with.name",
        proc.stderr,
      )

  def test_main_reports_duplicate_upload_artifact_names(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: duplicate-artifact",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: duplicate-artifact",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
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
        "ci-workflow-artifact-refs check failed: workflow uploads duplicate artifact names: duplicate-artifact",
        proc.stderr,
      )

  def test_main_reports_download_without_dependency_on_producer_job(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: bundle",
            "  test:",
            "    steps:",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: bundle",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
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
        "ci-workflow-artifact-refs check failed: "
        "workflow downloads artifacts without depending on producer jobs: bundle (test needs build)",
        proc.stderr,
      )

  def test_main_allows_transitive_dependency_on_producer_job(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: bundle",
            "  verify:",
            "    needs: build",
            "    steps:",
            "      - run: echo verify",
            "  report:",
            "    needs: verify",
            "    steps:",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: bundle",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-artifact-refs check: OK", proc.stdout)

  def test_main_allows_same_job_upload_and_download(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: bundle",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: bundle",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-artifact-refs check: OK", proc.stdout)

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
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: verity-edsl-artifacts",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: verity-edsl-artifacts",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_artifact_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-artifact-refs check: OK", proc.stdout)

  def test_main_function_passes_for_matching_uploads_and_downloads(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: verity-edsl-artifacts",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: verity-edsl-artifacts",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_artifact_refs.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
