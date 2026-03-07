#!/usr/bin/env python3
"""Fail-closed check that workflow working-directory paths exist in the repo."""

from __future__ import annotations

import argparse
import pathlib
import sys

from ci_workflow_helpers import ROOT, WORKFLOW_PATH, read_text, strip_yaml_scalar


class CiWorkflowWorkingDirectoryRefsError(RuntimeError):
  """Raised when workflow working-directory validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-working-directory-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)

def collect_workflow_working_directories(workflow_text: str) -> list[str]:
  working_directories: list[str] = []
  for raw_line in workflow_text.splitlines():
    stripped = raw_line.lstrip()
    if not stripped.startswith("working-directory:"):
      continue
    raw_value = stripped.split(":", 1)[1]
    working_directory = strip_yaml_scalar(raw_value)
    if not working_directory:
      raise CiWorkflowWorkingDirectoryRefsError("working-directory must be a non-empty literal path")
    if "${{" in working_directory:
      raise CiWorkflowWorkingDirectoryRefsError(
        f"working-directory must be a literal repo-relative path: {working_directory}"
      )
    path = pathlib.PurePosixPath(working_directory)
    if path.is_absolute():
      raise CiWorkflowWorkingDirectoryRefsError(
        f"working-directory must be repo-relative, not absolute: {working_directory}"
      )
    if any(part == ".." for part in path.parts):
      raise CiWorkflowWorkingDirectoryRefsError(
        f"working-directory must stay inside the repository: {working_directory}"
      )
    working_directories.append(working_directory)
  return working_directories


def resolve_working_directory(repo_root: pathlib.Path, working_directory: str) -> pathlib.Path:
  resolved = (repo_root / pathlib.PurePosixPath(working_directory)).resolve()
  try:
    resolved.relative_to(repo_root.resolve())
  except ValueError as exc:
    raise CiWorkflowWorkingDirectoryRefsError(
      f"working-directory escapes repository root: {working_directory}"
    ) from exc
  return resolved


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml working-directory paths are literal repo-relative directories"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--repo-root",
    type=pathlib.Path,
    default=ROOT,
    help="Path to repository root used to resolve working-directory paths",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()
  repo_root = args.repo_root.resolve()

  if not repo_root.is_dir():
    fail(f"repository root does not exist: {repo_root}")

  try:
    workflow_text = read_text(workflow_path, CiWorkflowWorkingDirectoryRefsError)
    working_directories = collect_workflow_working_directories(workflow_text)
  except CiWorkflowWorkingDirectoryRefsError as exc:
    fail(str(exc))

  missing_directories: list[str] = []
  for working_directory in working_directories:
    try:
      resolved = resolve_working_directory(repo_root, working_directory)
    except CiWorkflowWorkingDirectoryRefsError as exc:
      fail(str(exc))
    if not resolved.is_dir():
      missing_directories.append(working_directory)

  if missing_directories:
    fail(
      "workflow references missing working-directory paths: "
      + ", ".join(sorted(set(missing_directories)))
    )

  print(
    "ci-workflow-working-directory-refs: "
    f"working_directories={len(working_directories)}"
  )
  print("ci-workflow-working-directory-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
