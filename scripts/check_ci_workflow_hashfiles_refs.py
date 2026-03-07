#!/usr/bin/env python3
"""Fail-closed check that workflow hashFiles() inputs match repo files."""

from __future__ import annotations

import argparse
import glob
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
EXPRESSION_RE = re.compile(r"\$\{\{(?P<expr>.*?)\}\}", re.DOTALL)
HASHFILES_CALL_RE = re.compile(r"hashFiles\((?P<args>[^)]*)\)")
HASHFILES_ARG_RE = re.compile(r"""(['"])(.*?)\1""")


class CiWorkflowHashFilesRefsError(RuntimeError):
  """Raised when workflow hashFiles() reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-hashfiles-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowHashFilesRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowHashFilesRefsError(f"failed to read workflow {path}: {exc}") from exc


def collect_workflow_hashfiles_patterns(workflow_text: str) -> list[str]:
  patterns: list[str] = []
  for expression_match in EXPRESSION_RE.finditer(workflow_text):
    expression_text = expression_match.group("expr")
    for match in HASHFILES_CALL_RE.finditer(expression_text):
      args_text = match.group("args")
      call_patterns = [arg_match.group(2).strip() for arg_match in HASHFILES_ARG_RE.finditer(args_text)]
      if not call_patterns:
        raise CiWorkflowHashFilesRefsError(
          f"hashFiles() call must use quoted literal patterns: {match.group(0)}"
        )
      empty_patterns = [pattern for pattern in call_patterns if not pattern]
      if empty_patterns:
        raise CiWorkflowHashFilesRefsError("hashFiles() call contains an empty pattern")
      patterns.extend(call_patterns)
  return patterns


def expand_hashfiles_pattern(repo_root: pathlib.Path, pattern: str) -> list[pathlib.Path]:
  normalized = pattern[1:] if pattern.startswith("!") else pattern
  if pathlib.PurePosixPath(normalized).is_absolute():
    raise CiWorkflowHashFilesRefsError(f"hashFiles() pattern must be repo-relative: {pattern}")

  matches = [
    pathlib.Path(path)
    for path in glob.glob(str(repo_root / normalized), recursive=True)
    if pathlib.Path(path).is_file()
  ]
  return sorted(matches)


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml hashFiles() expressions only reference existing repo files"
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
    help="Path to repository root used to resolve hashFiles() patterns",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()
  repo_root = args.repo_root.resolve()

  if not repo_root.is_dir():
    fail(f"repo root does not exist: {repo_root}")

  try:
    workflow_text = read_text(workflow_path)
    patterns = collect_workflow_hashfiles_patterns(workflow_text)
  except CiWorkflowHashFilesRefsError as exc:
    fail(str(exc))

  missing_patterns: list[str] = []
  for pattern in patterns:
    if pattern.startswith("!"):
      continue
    if not expand_hashfiles_pattern(repo_root, pattern):
      missing_patterns.append(pattern)

  if missing_patterns:
    fail("workflow hashFiles() patterns matched no files: " + ", ".join(sorted(set(missing_patterns))))

  print(
    "ci-workflow-hashfiles-refs: "
    f"patterns={len(patterns)} positive_patterns={sum(not pattern.startswith('!') for pattern in patterns)}"
  )
  print("ci-workflow-hashfiles-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
