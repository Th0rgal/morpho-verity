#!/usr/bin/env python3
"""Unit tests for no-arg check-script cwd coverage guard."""

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

from check_no_arg_check_scripts import (  # noqa: E402
  collect_no_arg_check_scripts,
  main,
)


class CollectNoArgCheckScriptsTests(unittest.TestCase):
  def test_collect_excludes_skipped_entries(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      scripts_dir = pathlib.Path(temp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_check_alpha.py").write_text("", encoding="utf-8")

      got = collect_no_arg_check_scripts(
        scripts_dir,
        skip_scripts={"check_beta.py"},
      )

      self.assertEqual([path.name for path in got], ["check_alpha.py"])


class CliTests(unittest.TestCase):
  def write_executable(self, path: pathlib.Path, body: str) -> None:
    path.write_text(textwrap.dedent(body), encoding="utf-8")
    path.chmod(0o755)

  def write_parity_target(self, path: pathlib.Path) -> None:
    path.write_text(
      json.dumps(
        {
          "id": "test-parity-target",
          "solc": {"version": "0.8.28", "commit": "7893614a"},
          "foundryDefaultProfile": {
            "optimizer": True,
            "optimizerRuns": 999999,
            "viaIR": True,
            "evmVersion": "paris",
            "bytecodeHash": "none",
          },
          "verity": {"parityPackId": "solc-0.8.28-o999999-viair-true-evm-paris"},
          "yulIdentity": {"gateMode": "unsupported-manifest"},
        }
      ),
      encoding="utf-8",
    )

  def test_main_passes_when_scripts_succeed_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      parity_target = root / "parity-target.json"
      self.write_parity_target(parity_target)
      self.write_executable(
        scripts_dir / "check_alpha.py",
        """\
        #!/usr/bin/env python3
        import pathlib
        import sys
        if pathlib.Path.cwd().name == 'scripts':
          print('ran from scripts dir', file=sys.stderr)
          raise SystemExit(1)
        print('alpha ok')
        """,
      )
      self.write_executable(
        scripts_dir / "check_parity_target.py",
        """\
        #!/usr/bin/env python3
        import subprocess
        import sys
        out = subprocess.check_output(['solc', '--version'], text=True)
        if '0.8.28+commit.7893614a' not in out:
          print(out, file=sys.stderr)
          raise SystemExit(1)
        print('parity ok')
        """,
      )
      (root / "elsewhere").mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_no_arg_check_scripts.py"),
          "--scripts-dir",
          str(scripts_dir),
          "--parity-target",
          str(parity_target),
        ],
        cwd=root / "elsewhere",
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("no-arg-check-scripts check: OK", proc.stdout)

  def test_main_reports_failing_script(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      parity_target = root / "parity-target.json"
      self.write_parity_target(parity_target)
      self.write_executable(
        scripts_dir / "check_alpha.py",
        """\
        #!/usr/bin/env python3
        import sys
        print('alpha failed', file=sys.stderr)
        raise SystemExit(7)
        """,
      )
      (root / "elsewhere").mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_no_arg_check_scripts.py"),
          "--scripts-dir",
          str(scripts_dir),
          "--parity-target",
          str(parity_target),
          "--skip-script",
          "check_parity_target.py",
        ],
        cwd=root / "elsewhere",
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("check_alpha.py exited with status 7", proc.stderr)
      self.assertIn("stderr=alpha failed", proc.stderr)

  def test_main_allows_explicit_skip_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      parity_target = root / "parity-target.json"
      self.write_parity_target(parity_target)
      self.write_executable(
        scripts_dir / "check_alpha.py",
        """\
        #!/usr/bin/env python3
        raise SystemExit(9)
        """,
      )
      self.write_executable(
        scripts_dir / "check_beta.py",
        """\
        #!/usr/bin/env python3
        print('beta ok')
        """,
      )
      (root / "elsewhere").mkdir()

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_no_arg_check_scripts.py",
          "--scripts-dir",
          str(scripts_dir),
          "--parity-target",
          str(parity_target),
          "--skip-script",
          "check_alpha.py",
          "--skip-script",
          "check_parity_target.py",
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
