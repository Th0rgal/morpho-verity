#!/usr/bin/env python3
"""Fail-closed check that workflow hashFiles() inputs match repo files."""

from __future__ import annotations

import argparse
import glob
import pathlib
import re
import sys

from ci_workflow_helpers import ROOT, WORKFLOW_PATH, read_text
EXPRESSION_RE = re.compile(r"\$\{\{(?P<expr>.*?)\}\}", re.DOTALL)


class CiWorkflowHashFilesRefsError(RuntimeError):
  """Raised when workflow hashFiles() reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-hashfiles-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def _consume_quoted_literal(text: str, start: int) -> tuple[str, int]:
  quote = text[start]
  index = start + 1
  chars: list[str] = []
  while index < len(text):
    char = text[index]
    if char == quote:
      return "".join(chars), index + 1
    if quote == '"' and char == "\\" and index + 1 < len(text):
      chars.append(text[index + 1])
      index += 2
      continue
    if quote == "'" and char == "'" and index + 1 < len(text) and text[index + 1] == "'":
      chars.append("'")
      index += 2
      continue
    chars.append(char)
    index += 1
  raise CiWorkflowHashFilesRefsError("hashFiles() call contains an unterminated quoted pattern")


def parse_hashfiles_call_patterns(args_text: str) -> list[str]:
  patterns: list[str] = []
  index = 0
  while index < len(args_text):
    while index < len(args_text) and args_text[index].isspace():
      index += 1
    if index >= len(args_text):
      break
    if args_text[index] not in {"'", '"'}:
      raise CiWorkflowHashFilesRefsError(
        f"hashFiles() call must use quoted literal patterns: hashFiles({args_text})"
      )
    pattern, index = _consume_quoted_literal(args_text, index)
    pattern = pattern.strip()
    if not pattern:
      raise CiWorkflowHashFilesRefsError("hashFiles() call contains an empty pattern")
    patterns.append(pattern)
    while index < len(args_text) and args_text[index].isspace():
      index += 1
    if index >= len(args_text):
      break
    if args_text[index] != ",":
      raise CiWorkflowHashFilesRefsError(
        f"hashFiles() call must use quoted literal patterns: hashFiles({args_text})"
      )
    index += 1
  return patterns


def iter_hashfiles_call_args(expression_text: str) -> list[str]:
  args_blocks: list[str] = []
  search_from = 0
  marker = "hashFiles("
  while True:
    start = expression_text.find(marker, search_from)
    if start == -1:
      return args_blocks
    index = start + len(marker)
    depth = 1
    in_single_quote = False
    in_double_quote = False
    while index < len(expression_text):
      char = expression_text[index]
      if char == "'" and not in_double_quote:
        if in_single_quote and index + 1 < len(expression_text) and expression_text[index + 1] == "'":
          index += 2
          continue
        in_single_quote = not in_single_quote
      elif char == '"' and not in_single_quote:
        backslash_count = 0
        cursor = index - 1
        while cursor >= start and expression_text[cursor] == "\\":
          backslash_count += 1
          cursor -= 1
        if backslash_count % 2 == 0:
          in_double_quote = not in_double_quote
      elif not in_single_quote and not in_double_quote:
        if char == "(":
          depth += 1
        elif char == ")":
          depth -= 1
          if depth == 0:
            args_blocks.append(expression_text[start + len(marker):index])
            search_from = index + 1
            break
      index += 1
    else:
      raise CiWorkflowHashFilesRefsError("hashFiles() call is missing a closing parenthesis")


def collect_workflow_hashfiles_patterns(workflow_text: str) -> list[str]:
  patterns: list[str] = []
  for expression_match in EXPRESSION_RE.finditer(workflow_text):
    expression_text = expression_match.group("expr")
    for args_text in iter_hashfiles_call_args(expression_text):
      patterns.extend(parse_hashfiles_call_patterns(args_text))
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
    workflow_text = read_text(workflow_path, CiWorkflowHashFilesRefsError)
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
