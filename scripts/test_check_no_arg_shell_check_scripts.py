#!/usr/bin/env python3
"""Unit tests for no-arg shell check-script cwd coverage guard."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import textwrap
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_no_arg_shell_check_scripts import (  # noqa: E402
  collect_no_arg_shell_check_scripts,
  main,
)


class CollectNoArgShellCheckScriptsTests(unittest.TestCase):
  def test_collect_excludes_skipped_entries(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      scripts_dir = pathlib.Path(temp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.sh").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "test_check_alpha.sh").write_text("", encoding="utf-8")

      got = collect_no_arg_shell_check_scripts(
        scripts_dir,
        skip_scripts={"check_beta.sh"},
      )

      self.assertEqual([path.name for path in got], ["check_alpha.sh"])


class CliTests(unittest.TestCase):
  def write_executable(self, path: pathlib.Path, body: str) -> None:
    path.write_text(textwrap.dedent(body), encoding="utf-8")
    path.chmod(0o755)

  def test_main_passes_when_scripts_succeed_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      self.write_executable(
        scripts_dir / "check_alpha.sh",
        """\
        #!/usr/bin/env bash
        set -euo pipefail
        if [[ "$(basename "$PWD")" == "scripts" ]]; then
          echo "ran from scripts dir" >&2
          exit 1
        fi
        echo "alpha ok"
        """,
      )
      self.write_executable(
        scripts_dir / "check_toolchain_readiness.sh",
        """\
        #!/usr/bin/env bash
        set -euo pipefail
        for bin in lean lake forge anvil solc solc-select; do
          command -v "${bin}" >/dev/null 2>&1
        done
        solc --version | grep -F "0.8.28" >/dev/null
        echo "toolchain ok"
        """,
      )
      (root / "elsewhere").mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_no_arg_shell_check_scripts.py"),
          "--scripts-dir",
          str(scripts_dir),
        ],
        cwd=root / "elsewhere",
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("no-arg-shell-check-scripts check: OK", proc.stdout)

  def test_main_reports_failing_script(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      self.write_executable(
        scripts_dir / "check_alpha.sh",
        """\
        #!/usr/bin/env bash
        set -euo pipefail
        echo "alpha failed" >&2
        exit 7
        """,
      )
      (root / "elsewhere").mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_no_arg_shell_check_scripts.py"),
          "--scripts-dir",
          str(scripts_dir),
          "--skip-script",
          "check_input_mode_parity.sh",
          "--skip-script",
          "check_toolchain_readiness.sh",
        ],
        cwd=root / "elsewhere",
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("check_alpha.sh exited with status 7", proc.stderr)
      self.assertIn("stderr=alpha failed", proc.stderr)

  def test_main_allows_explicit_skip_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      self.write_executable(
        scripts_dir / "check_alpha.sh",
        """\
        #!/usr/bin/env bash
        set -euo pipefail
        exit 9
        """,
      )
      self.write_executable(
        scripts_dir / "check_beta.sh",
        """\
        #!/usr/bin/env bash
        set -euo pipefail
        echo "beta ok"
        """,
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_no_arg_shell_check_scripts.py",
          "--scripts-dir",
          str(scripts_dir),
          "--skip-script",
          "check_alpha.sh",
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
