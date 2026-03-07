#!/usr/bin/env python3
"""Fail-closed check that workflow cache/artifact paths stay well-formed."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"

SUPPORTED_ACTION_PREFIXES = {
  "actions/cache@": "cache",
  "actions/upload-artifact@": "upload-artifact",
  "actions/download-artifact@": "download-artifact",
}


class CiWorkflowPathRefsError(RuntimeError):
  """Raised when workflow path reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-path-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowPathRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowPathRefsError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def _collect_job_blocks(lines: list[str]) -> list[list[str]]:
  jobs_index: int | None = None
  for index, line in enumerate(lines):
    if line.strip() == "jobs:" and len(line) - len(line.lstrip()) == 0:
      jobs_index = index
      break
  if jobs_index is None:
    raise CiWorkflowPathRefsError("workflow is missing top-level jobs: section")

  blocks: list[list[str]] = []
  current_block: list[str] | None = None

  for line in lines[jobs_index + 1:]:
    stripped = line.strip()
    indent = len(line) - len(line.lstrip())
    if indent == 0 and stripped and not stripped.startswith("#"):
      break
    if indent == 2 and stripped.endswith(":"):
      if current_block is not None:
        blocks.append(current_block)
      current_block = []
      continue
    if current_block is not None:
      current_block.append(line)

  if current_block is not None:
    blocks.append(current_block)
  if not blocks:
    raise CiWorkflowPathRefsError("workflow jobs: section defines no jobs")
  return blocks


def _parse_path_entries(
  step_lines: list[tuple[int, str]],
  path_index: int,
) -> tuple[list[str], int]:
  path_indent, stripped = step_lines[path_index]
  raw_value = stripped.split(":", 1)[1].strip()
  if raw_value in {"|", ">"}:
    values: list[str] = []
    next_index = path_index + 1
    while next_index < len(step_lines):
      indent, candidate = step_lines[next_index]
      if indent <= path_indent:
        break
      entry = _strip_yaml_scalar(candidate)
      if entry:
        values.append(entry)
      next_index += 1
    if not values:
      raise CiWorkflowPathRefsError("workflow path block scalar must include at least one entry")
    return values, next_index

  value = _strip_yaml_scalar(raw_value)
  if not value:
    raise CiWorkflowPathRefsError("workflow path must be a non-empty literal")
  return [value], path_index + 1


def collect_workflow_action_paths(workflow_text: str) -> list[tuple[str, str]]:
  collected: list[tuple[str, str]] = []

  for block_lines in _collect_job_blocks(workflow_text.splitlines()):
    index = 0
    while index < len(block_lines):
      line = block_lines[index]
      stripped = line.strip()
      step_indent = len(line) - len(line.lstrip())
      if not stripped.startswith("- "):
        index += 1
        continue

      step_lines: list[tuple[int, str]] = [(step_indent, stripped[2:].strip())]
      index += 1
      while index < len(block_lines):
        candidate = block_lines[index]
        candidate_stripped = candidate.strip()
        candidate_indent = len(candidate) - len(candidate.lstrip())
        if candidate_indent <= step_indent and candidate_stripped.startswith("- "):
          break
        step_lines.append((candidate_indent, candidate_stripped))
        index += 1

      action_kind: str | None = None
      with_indent: int | None = None
      cursor = 0
      while cursor < len(step_lines):
        indent, candidate = step_lines[cursor]
        if candidate.startswith("uses:"):
          action_ref = _strip_yaml_scalar(candidate.split(":", 1)[1])
          for prefix, kind in SUPPORTED_ACTION_PREFIXES.items():
            if action_ref.startswith(prefix):
              action_kind = kind
              break
        elif candidate == "with:":
          with_indent = indent
        elif with_indent is not None and indent <= with_indent:
          with_indent = None
          continue
        elif with_indent is not None and candidate.startswith("path:") and action_kind is not None:
          paths, next_cursor = _parse_path_entries(step_lines, cursor)
          collected.extend((action_kind, path) for path in paths)
          cursor = next_cursor
          continue
        cursor += 1

  return collected


def validate_path_ref(path_ref: str) -> None:
  if "${{" in path_ref:
    raise CiWorkflowPathRefsError(
      f"workflow path references must be literal values, got expression: {path_ref}"
    )

  path = pathlib.PurePosixPath(path_ref)
  if path.is_absolute() or path_ref.startswith("~"):
    return
  if any(part == ".." for part in path.parts):
    raise CiWorkflowPathRefsError(
      f"workflow path references must stay inside the repository: {path_ref}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml cache/artifact paths are literal and repo-contained"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()

  try:
    workflow_text = read_text(workflow_path)
    action_paths = collect_workflow_action_paths(workflow_text)
    for _, path_ref in action_paths:
      validate_path_ref(path_ref)
  except CiWorkflowPathRefsError as exc:
    fail(str(exc))

  print(
    "ci-workflow-path-refs: "
    f"path_refs={len(action_paths)} supported_actions={len(SUPPORTED_ACTION_PREFIXES)}"
  )
  print("ci-workflow-path-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
