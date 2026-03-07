#!/usr/bin/env python3
"""Fail-closed check for duplicate top-level Python definitions in scripts/."""

from __future__ import annotations

import argparse
import ast
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPTS_DIR = ROOT / "scripts"


class DuplicateTopLevelDefsError(RuntimeError):
  """Raised when duplicate top-level Python definitions are detected."""


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise DuplicateTopLevelDefsError(f"failed to read Python source {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise DuplicateTopLevelDefsError(f"failed to decode Python source {path} as UTF-8: {exc}") from exc


def collect_python_scripts(scripts_dir: pathlib.Path) -> list[pathlib.Path]:
  return sorted(
    path for path in scripts_dir.glob("*.py")
    if path.is_file()
  )


def duplicate_top_level_defs(path: pathlib.Path) -> dict[str, list[int]]:
  try:
    module = ast.parse(read_text(path), filename=str(path))
  except SyntaxError as exc:
    raise DuplicateTopLevelDefsError(
      f"failed to parse Python source {path}: {exc.msg} (line {exc.lineno})"
    ) from exc

  seen: dict[str, list[int]] = {}
  for node in module.body:
    if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
      seen.setdefault(node.name, []).append(node.lineno)
  return {
    name: lines
    for name, lines in seen.items()
    if len(lines) > 1
  }


def find_duplicate_top_level_defs(scripts_dir: pathlib.Path) -> dict[pathlib.Path, dict[str, list[int]]]:
  duplicates: dict[pathlib.Path, dict[str, list[int]]] = {}
  for path in collect_python_scripts(scripts_dir):
    file_duplicates = duplicate_top_level_defs(path)
    if file_duplicates:
      duplicates[path] = file_duplicates
  return duplicates


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def fail(message: str) -> None:
  print(f"duplicate-top-level-defs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate scripts/*.py do not shadow top-level functions or classes"
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=SCRIPTS_DIR,
    help="Path to scripts directory",
  )
  args = parser.parse_args()
  scripts_dir = args.scripts_dir.resolve()

  try:
    duplicates = find_duplicate_top_level_defs(scripts_dir)
  except DuplicateTopLevelDefsError as exc:
    fail(str(exc))

  if duplicates:
    details = []
    for path, file_duplicates in sorted(duplicates.items()):
      formatted = ", ".join(
        f"{name} lines {','.join(str(line) for line in lines)}"
        for name, lines in sorted(file_duplicates.items())
      )
      details.append(f"{display_path(path)} ({formatted})")
    fail("duplicate top-level definitions found in: " + "; ".join(details))

  print(f"duplicate-top-level-defs: scanned={len(collect_python_scripts(scripts_dir))}")
  print("duplicate-top-level-defs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
