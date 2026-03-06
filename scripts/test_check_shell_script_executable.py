#!/usr/bin/env python3
"""Unit tests for shell script executable-bit guard."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_shell_script_executable import (  # noqa: E402
  collect_shell_scripts,
  find_non_executable_scripts,
  main,
)


class CheckShellScriptExecutableTests(unittest.TestCase):
  def test_collect_shell_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.sh").write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      (scripts_dir / "beta.py").write_text("#!/usr/bin/env python3\n", encoding="utf-8")
      self.assertEqual([path.name for path in collect_shell_scripts(scripts_dir)], ["alpha.sh"])

  def test_find_non_executable_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      executable = scripts_dir / "alpha.sh"
      non_executable = scripts_dir / "beta.sh"
      executable.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      non_executable.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      executable.chmod(0o755)
      non_executable.chmod(0o644)

      self.assertEqual(
        [path.name for path in find_non_executable_scripts([executable, non_executable])],
        ["beta.sh"],
      )

  def test_main_passes_when_all_shell_scripts_are_executable(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      alpha = scripts_dir / "alpha.sh"
      beta = scripts_dir / "beta.sh"
      alpha.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      beta.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      alpha.chmod(0o755)
      beta.chmod(0o755)

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_shell_script_executable.py",
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_shell_script_is_not_executable(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      alpha = scripts_dir / "alpha.sh"
      beta = scripts_dir / "beta.sh"
      alpha.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      beta.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
      alpha.chmod(0o755)
      beta.chmod(0o644)

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_shell_script_executable.py",
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
