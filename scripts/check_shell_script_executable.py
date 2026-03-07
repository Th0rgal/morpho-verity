#!/usr/bin/env python3
"""Fail-closed check that all repository shell scripts are executable."""

from __future__ import annotations

import argparse
import pathlib
import stat
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent


# Require owner execute bit because Git tracks executable files as mode 100755.
REQUIRED_EXEC_BIT = stat.S_IXUSR


def fail(msg: str) -> None:
  print(f"shell-script-executable check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def collect_shell_scripts(scripts_dir: pathlib.Path) -> list[pathlib.Path]:
  return sorted(path for path in scripts_dir.glob("*.sh") if path.is_file())


def find_non_executable_scripts(shell_scripts: list[pathlib.Path]) -> list[pathlib.Path]:
  non_executable: list[pathlib.Path] = []
  for path in shell_scripts:
    mode = path.stat().st_mode
    if mode & REQUIRED_EXEC_BIT == 0:
      non_executable.append(path)
  return non_executable


def main() -> int:
  parser = argparse.ArgumentParser(description="Validate scripts/*.sh files are executable")
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=ROOT / "scripts",
    help="Path to scripts directory",
  )
  args = parser.parse_args()
  scripts_dir = args.scripts_dir.resolve()

  shell_scripts = collect_shell_scripts(scripts_dir)
  if not shell_scripts:
    fail(f"{scripts_dir}: no shell scripts found")

  non_executable = find_non_executable_scripts(shell_scripts)
  if non_executable:
    fail("non-executable shell scripts: " + ", ".join(path.name for path in non_executable))

  print(f"shell-script-executable: scripts={len(shell_scripts)}")
  print("shell-script-executable check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
