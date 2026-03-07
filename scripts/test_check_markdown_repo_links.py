#!/usr/bin/env python3
"""Unit tests for markdown repo-link integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_markdown_repo_links import (  # noqa: E402
  collect_default_docs,
  collect_missing_repo_links,
  main,
)


class CollectDefaultDocsTests(unittest.TestCase):
  def test_collect_default_docs_includes_readme_and_docs_markdown(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      (root / "README.md").write_text("# README\n", encoding="utf-8")
      docs_dir = root / "docs"
      docs_dir.mkdir()
      (docs_dir / "guide.md").write_text("# Guide\n", encoding="utf-8")
      (docs_dir / "notes.txt").write_text("not markdown\n", encoding="utf-8")

      self.assertEqual(
        [path.relative_to(root).as_posix() for path in collect_default_docs(root)],
        ["README.md", "docs/guide.md"],
      )


class CollectMissingRepoLinksTests(unittest.TestCase):
  def test_ignores_external_and_anchor_links(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      docs_dir = root / "docs"
      docs_dir.mkdir()
      (root / "README.md").write_text("# README\n", encoding="utf-8")
      (root / "config.json").write_text("{}\n", encoding="utf-8")
      doc_path = docs_dir / "guide.md"
      doc_path.write_text(
        "\n".join(
          [
            "# Guide",
            "[Config](../config.json)",
            "[Section](#local-anchor)",
            "[Website](https://example.com/docs)",
            "[Mail](mailto:test@example.com)",
          ]
        ),
        encoding="utf-8",
      )

      self.assertEqual(collect_missing_repo_links(doc_path, root=root), [])

  def test_reports_missing_repo_local_targets(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      docs_dir = root / "docs"
      docs_dir.mkdir()
      doc_path = docs_dir / "guide.md"
      doc_path.write_text("[Missing](missing.md)\n", encoding="utf-8")

      self.assertEqual(collect_missing_repo_links(doc_path, root=root), ["missing.md"])

  def test_rejects_links_that_escape_repo_root(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      docs_dir = root / "docs"
      docs_dir.mkdir()
      doc_path = docs_dir / "guide.md"
      doc_path.write_text("[Outside](../../outside.md)\n", encoding="utf-8")

      with self.assertRaisesRegex(RuntimeError, "links outside repo root"):
        collect_missing_repo_links(doc_path, root=root)


class CliTests(unittest.TestCase):
  def make_repo(self, root: pathlib.Path) -> None:
    docs_dir = root / "docs"
    docs_dir.mkdir(parents=True)
    config_dir = root / "config"
    config_dir.mkdir()
    (root / "README.md").write_text("[Guide](docs/guide.md#overview)\n", encoding="utf-8")
    (docs_dir / "guide.md").write_text("[Config](../config/data.json)\n", encoding="utf-8")
    (config_dir / "data.json").write_text("{}\n", encoding="utf-8")

  def test_main_reports_missing_link(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      docs_dir = root / "docs"
      docs_dir.mkdir()
      doc_path = docs_dir / "guide.md"
      doc_path.write_text("[Missing](missing.md)\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_markdown_repo_links.py"),
          "--root",
          str(root),
          "--doc",
          str(doc_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "markdown-repo-links check failed: missing repo-local markdown links: docs/guide.md -> missing.md",
        proc.stderr,
      )

  def test_main_accepts_relative_root_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      repo_root = root / "repo"
      repo_root.mkdir()
      self.make_repo(repo_root)
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_markdown_repo_links.py"),
          "--root",
          str(pathlib.Path("..") / "repo"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("markdown-repo-links check: OK", proc.stdout)

  def test_main_rejects_explicit_doc_outside_root(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      repo_root = root / "repo"
      repo_root.mkdir()
      self.make_repo(repo_root)
      external_doc = root / "external.md"
      external_doc.write_text("# External\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_markdown_repo_links.py"),
          "--root",
          str(repo_root),
          "--doc",
          str(external_doc),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("markdown file is outside repo root", proc.stderr)

  def test_main_no_args_succeeds_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      runner = pathlib.Path(temp_dir) / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_markdown_repo_links.py")],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("markdown-repo-links check: OK", proc.stdout)

  def test_main_function_passes_for_valid_repo(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      self.make_repo(root)

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_markdown_repo_links.py",
          "--root",
          str(root),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
