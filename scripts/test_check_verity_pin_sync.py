#!/usr/bin/env python3
"""Unit tests for Verity dependency pin sync checking."""

from __future__ import annotations

import pathlib
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
        "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
      """,
      """
      {
        "packages": [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
            "inputRev": "ad03fc64"
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        """
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        """
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
              "inputRev": "ad03fc64"
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        """{"packages": [}""",
      )

  def test_rejects_non_object_manifest_root(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        """
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        """
        [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
            "inputRev": "ad03fc64"
          }
        ]
        """,
      )
