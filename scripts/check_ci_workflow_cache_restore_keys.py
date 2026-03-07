#!/usr/bin/env python3
"""Fail-closed check that workflow cache restore-keys stay aligned with cache keys."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"


class CiWorkflowCacheRestoreKeysError(RuntimeError):
  """Raised when workflow cache restore-key validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-cache-restore-keys check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowCacheRestoreKeysError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowCacheRestoreKeysError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_cache_key_restore_keys(workflow_text: str) -> list[tuple[str | None, str | None, list[str]]]:
  cache_steps: list[tuple[str | None, str | None, list[str]]] = []
  lines = workflow_text.splitlines()
  index = 0
  while index < len(lines):
    line = lines[index]
    stripped = line.lstrip()
    if not stripped.startswith("- "):
      index += 1
      continue
    indent = len(line) - len(stripped)
    step_name: str | None = None
    uses_ref: str | None = None
    cache_key: str | None = None
    restore_keys: list[str] = []
    normalized = stripped[2:].lstrip()
    if normalized.startswith("name:"):
      step_name = _strip_yaml_scalar(normalized.split(":", 1)[1])
    elif normalized.startswith("uses:"):
      uses_ref = _strip_yaml_scalar(normalized.split(":", 1)[1])
    index += 1

    while index < len(lines):
      candidate = lines[index]
      candidate_stripped = candidate.lstrip()
      if candidate_stripped:
        candidate_indent = len(candidate) - len(candidate_stripped)
        if candidate_indent <= indent:
          break
      else:
        index += 1
        continue

      normalized_candidate = (
        candidate_stripped[2:].lstrip() if candidate_stripped.startswith("- ") else candidate_stripped
      )
      if normalized_candidate.startswith("name:"):
        step_name = _strip_yaml_scalar(normalized_candidate.split(":", 1)[1])
        index += 1
        continue
      if normalized_candidate.startswith("uses:"):
        uses_ref = _strip_yaml_scalar(normalized_candidate.split(":", 1)[1])
        index += 1
        continue
      if not normalized_candidate.startswith("with:"):
        index += 1
        continue

      with_indent = candidate_indent
      index += 1
      while index < len(lines):
        with_line = lines[index]
        with_stripped = with_line.lstrip()
        if with_stripped:
          with_line_indent = len(with_line) - len(with_stripped)
          if with_line_indent <= with_indent:
            break
        else:
          index += 1
          continue

        if with_stripped.startswith("key:"):
          cache_key = _strip_yaml_scalar(with_stripped.split(":", 1)[1])
          index += 1
          continue

        if with_stripped.startswith("restore-keys:"):
          tail = _strip_yaml_comment(with_stripped.split(":", 1)[1]).strip()
          if tail and tail != "|":
            restore_keys.append(_strip_yaml_scalar(tail))
            index += 1
            continue
          index += 1
          while index < len(lines):
            restore_line = lines[index]
            restore_stripped = restore_line.lstrip()
            if restore_stripped:
              restore_indent = len(restore_line) - len(restore_stripped)
              if restore_indent <= with_line_indent:
                break
              restore_keys.append(_strip_yaml_scalar(restore_stripped))
            index += 1
          continue

        index += 1

    if uses_ref is not None and uses_ref.startswith("actions/cache@"):
      cache_steps.append((step_name, cache_key, restore_keys))

  return cache_steps


def validate_cache_restore_keys(step_name: str | None, cache_key: str | None, restore_keys: list[str]) -> None:
  label = step_name or "<unnamed cache step>"
  if cache_key is None or not cache_key.strip():
    raise CiWorkflowCacheRestoreKeysError(f"cache step {label} must define with.key")
  if not restore_keys:
    raise CiWorkflowCacheRestoreKeysError(f"cache step {label} must define at least one with.restore-keys entry")
  for restore_key in restore_keys:
    if not restore_key.strip():
      raise CiWorkflowCacheRestoreKeysError(f"cache step {label} has an empty restore-keys entry")
    if not cache_key.startswith(restore_key):
      raise CiWorkflowCacheRestoreKeysError(
        f"cache step {label} has restore-keys entry {restore_key!r} which is not a prefix of key {cache_key!r}"
      )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml cache restore-keys are present and aligned with cache keys"
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
    cache_steps = collect_cache_key_restore_keys(workflow_text)
    for step_name, cache_key, restore_keys in cache_steps:
      validate_cache_restore_keys(step_name, cache_key, restore_keys)
  except CiWorkflowCacheRestoreKeysError as exc:
    fail(str(exc))

  print(f"ci-workflow-cache-restore-keys: cache_steps={len(cache_steps)}")
  print("ci-workflow-cache-restore-keys check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
