#!/usr/bin/env python3
"""Unit tests for keccak256 selector-helper forwarder."""

from __future__ import annotations

import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
SOURCE_FORWARDER = ROOT / "scripts" / "keccak256.py"


class KeccakForwarderTests(unittest.TestCase):
  def test_fails_closed_when_delegate_is_missing(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      workspace = pathlib.Path(tmp)
      scripts_dir = workspace / "scripts"
      scripts_dir.mkdir(parents=True, exist_ok=True)
      shutil.copy2(SOURCE_FORWARDER, scripts_dir / "keccak256.py")

      proc = subprocess.run(
        [sys.executable, str(scripts_dir / "keccak256.py"), "transfer(address,uint256)"],
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("missing delegated selector helper", proc.stderr)

  def test_delegates_to_verity_helper_and_preserves_args(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      workspace = pathlib.Path(tmp)
      scripts_dir = workspace / "scripts"
      delegate = workspace / ".lake" / "packages" / "verity" / "scripts" / "keccak256.py"
      scripts_dir.mkdir(parents=True, exist_ok=True)
      delegate.parent.mkdir(parents=True, exist_ok=True)
      shutil.copy2(SOURCE_FORWARDER, scripts_dir / "keccak256.py")
      delegate.write_text(
        "import json\n"
        "import sys\n"
        "print(json.dumps({'argv0': sys.argv[0], 'args': sys.argv[1:]}))\n",
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(scripts_dir / "keccak256.py"),
          "transfer(address,uint256)",
          "approve(address,uint256)",
        ],
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0)
      payload = json.loads(proc.stdout.strip())
      self.assertEqual(payload["argv0"], str(delegate))
      self.assertEqual(
        payload["args"],
        ["transfer(address,uint256)", "approve(address,uint256)"],
      )


if __name__ == "__main__":
  unittest.main()
