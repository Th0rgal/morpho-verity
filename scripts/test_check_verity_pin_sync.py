#!/usr/bin/env python3
"""Unit tests for Verity dependency pin sync checking."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import textwrap
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_verity_pin_sync import main as check_main  # noqa: E402


class CheckVerityPinSyncTests(unittest.TestCase):
  def run_check(self, lakefile_text: str, manifest_text: str) -> int:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      lakefile = root / "lakefile.lean"
      manifest = root / "lake-manifest.json"
      lakefile.write_text(textwrap.dedent(lakefile_text), encoding="utf-8")
      manifest.write_text(textwrap.dedent(manifest_text), encoding="utf-8")
      argv = sys.argv[:]
      try:
        sys.argv = [
          "check_verity_pin_sync.py",
          "--lakefile",
          str(lakefile),
          "--manifest",
          str(manifest),
        ]
        return check_main()
      finally:
        sys.argv = argv

  def test_accepts_synced_pin(self) -> None:
    rc = self.run_check(
      """
      require verity from git
        "https://github.com/Th0rgal/verity.git" @ "4e862c54"
      """,
      """
      {
        "packages": [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "4e862c54111e0f8c4dcaba61ec0a190cbe2afda8",
            "inputRev": "4e862c54"
          }
        ]
      }
      """,
    )
    self.assertEqual(rc, 0)

  def test_rejects_input_rev_mismatch(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "4e862c54111e0f8c4dcaba61ec0a190cbe2afda8",
              "inputRev": "ffffffff"
            }
          ]
        }
        """,
      )

  def test_rejects_full_rev_mismatch(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
              "inputRev": "4e862c54"
            }
          ]
        }
        """,
      )

  def test_rejects_invalid_manifest_json(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """{"packages": [}""",
      )

  def test_rejects_non_object_manifest_root(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """
        [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "4e862c54111e0f8c4dcaba61ec0a190cbe2afda8",
            "inputRev": "4e862c54"
          }
        ]
        """,
      )

  def test_rejects_non_object_package_entries(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """
        {
          "packages": [
            "verity"
          ]
        }
        """,
      )

  def test_rejects_incomplete_verity_metadata(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "4e862c54"
        """,
        """
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "4e862c54111e0f8c4dcaba61ec0a190cbe2afda8",
              "inputRev": null
            }
          ]
        }
        """,
      )

  def test_cli_reports_invalid_json_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      lakefile = root / "lakefile.lean"
      manifest = root / "lake-manifest.json"
      lakefile.write_text(
        textwrap.dedent(
          """
          require verity from git
            "https://github.com/Th0rgal/verity.git" @ "4e862c54"
          """
        ),
        encoding="utf-8",
      )
      manifest.write_text("""{"packages": [}""", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_verity_pin_sync.py"),
          "--lakefile",
          str(lakefile),
          "--manifest",
          str(manifest),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("verity-pin-sync check failed:", proc.stderr)
    self.assertIn("failed to parse JSON manifest", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_invalid_utf8_manifest_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      lakefile = root / "lakefile.lean"
      manifest = root / "lake-manifest.json"
      lakefile.write_text(
        textwrap.dedent(
          """
          require verity from git
            "https://github.com/Th0rgal/verity.git" @ "4e862c54"
          """
        ),
        encoding="utf-8",
      )
      manifest.write_bytes(b"\xff")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_verity_pin_sync.py"),
          "--lakefile",
          str(lakefile),
          "--manifest",
          str(manifest),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("verity-pin-sync check failed:", proc.stderr)
    self.assertIn("failed to read manifest", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_missing_lakefile_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      lakefile = root / "lakefile.lean"
      manifest = root / "lake-manifest.json"
      manifest.write_text(
        textwrap.dedent(
          """
          {
            "packages": [
              {
                "name": "verity",
                "url": "https://github.com/Th0rgal/verity.git",
                "rev": "4e862c54111e0f8c4dcaba61ec0a190cbe2afda8",
                "inputRev": "4e862c54"
              }
            ]
          }
          """
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_verity_pin_sync.py"),
          "--lakefile",
          str(lakefile),
          "--manifest",
          str(manifest),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("verity-pin-sync check failed:", proc.stderr)
    self.assertIn("failed to read lakefile", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_uses_repo_relative_defaults_from_another_working_directory(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_verity_pin_sync.py"),
        ],
        cwd=d,
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 0)
    self.assertIn("verity-pin-sync check: OK", proc.stdout)
    self.assertNotIn("Traceback", proc.stderr)
