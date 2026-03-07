#!/usr/bin/env python3
"""Fail-closed check that the workflow keeps the expected top-level trigger policy."""

from __future__ import annotations

import argparse
import pathlib
import sys

from ci_workflow_helpers import read_text, strip_yaml_comment, strip_yaml_scalar


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
EXPECTED_TRIGGERS = {
  "push": {"branches": ["master"]},
  "pull_request": {},
  "workflow_dispatch": {},
}


class CiWorkflowTriggersError(RuntimeError):
  """Raised when workflow trigger validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-triggers check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def _indent_of(line: str) -> int:
  return len(line) - len(line.lstrip())


def _format_triggers(triggers: dict[str, dict[str, list[str]]]) -> str:
  parts: list[str] = []
  for trigger, config in sorted(triggers.items()):
    if not config:
      parts.append(trigger)
      continue
    detail = ", ".join(
      f"{key}=[{', '.join(values)}]" for key, values in sorted(config.items())
    )
    parts.append(f"{trigger}({detail})")
  return "; ".join(parts)


def collect_top_level_triggers(workflow_text: str) -> dict[str, dict[str, list[str]]]:
  lines = workflow_text.splitlines()
  triggers: dict[str, dict[str, list[str]]] | None = None

  for index, line in enumerate(lines):
    stripped = line.strip()
    indent = _indent_of(line)
    if indent != 0 or not stripped or stripped.startswith("#"):
      continue
    if not stripped.startswith("on:"):
      continue
    if triggers is not None:
      raise CiWorkflowTriggersError("workflow defines multiple top-level on: entries")

    raw_value = strip_yaml_comment(stripped.split(":", 1)[1]).strip()
    if raw_value:
      raise CiWorkflowTriggersError("workflow top-level on: must use a mapping, not an inline value")

    triggers = {}
    cursor = index + 1
    while cursor < len(lines):
      candidate = lines[cursor]
      candidate_stripped = candidate.strip()
      candidate_indent = _indent_of(candidate)
      if candidate_indent == 0 and candidate_stripped and not candidate_stripped.startswith("#"):
        break
      if not candidate_stripped or candidate_stripped.startswith("#"):
        cursor += 1
        continue
      if candidate_indent <= indent:
        break
      if candidate_indent != indent + 2 or ":" not in candidate_stripped:
        raise CiWorkflowTriggersError("workflow top-level on: must contain simple trigger mappings")

      trigger_name, raw_trigger_value = candidate_stripped.split(":", 1)
      trigger = strip_yaml_scalar(trigger_name)
      raw_trigger_value = strip_yaml_comment(raw_trigger_value).strip()
      if raw_trigger_value:
        raise CiWorkflowTriggersError(
          f"workflow trigger {trigger} must use a nested mapping, not an inline value"
        )

      config: dict[str, list[str]] = {}
      cursor += 1
      while cursor < len(lines):
        detail_line = lines[cursor]
        detail_stripped = detail_line.strip()
        detail_indent = _indent_of(detail_line)
        if detail_indent <= indent + 2 and detail_stripped and not detail_stripped.startswith("#"):
          break
        if not detail_stripped or detail_stripped.startswith("#"):
          cursor += 1
          continue
        if detail_indent == indent + 4:
          if ":" not in detail_stripped:
            raise CiWorkflowTriggersError(
              f"workflow trigger {trigger} must contain simple key: value mappings"
            )
          key_name, raw_key_value = detail_stripped.split(":", 1)
          key = strip_yaml_scalar(key_name)
          raw_key_value = strip_yaml_comment(raw_key_value).strip()
          if raw_key_value:
            raise CiWorkflowTriggersError(
              f"workflow trigger {trigger}.{key} must use a block list, not an inline value"
            )
          values: list[str] = []
          cursor += 1
          while cursor < len(lines):
            list_line = lines[cursor]
            list_stripped = list_line.strip()
            list_indent = _indent_of(list_line)
            if list_indent <= indent + 4 and list_stripped and not list_stripped.startswith("#"):
              break
            if not list_stripped or list_stripped.startswith("#"):
              cursor += 1
              continue
            if list_indent != indent + 6 or not list_stripped.startswith("- "):
              raise CiWorkflowTriggersError(
                f"workflow trigger {trigger}.{key} must contain only literal list items"
              )
            values.append(strip_yaml_scalar(list_stripped[2:]))
            cursor += 1
          if not values:
            raise CiWorkflowTriggersError(
              f"workflow trigger {trigger}.{key} block must include at least one entry"
            )
          if any(not value for value in values):
            raise CiWorkflowTriggersError(
              f"workflow trigger {trigger}.{key} contains an empty literal entry"
            )
          config[key] = values
          continue
        raise CiWorkflowTriggersError(
          f"workflow trigger {trigger} contains unsupported structure at line: {detail_stripped}"
        )
      triggers[trigger] = config
      continue

  if triggers is None:
    raise CiWorkflowTriggersError("workflow is missing a top-level on: block")
  if not triggers:
    raise CiWorkflowTriggersError("workflow top-level on: block must not be empty")
  return triggers


def validate_triggers(triggers: dict[str, dict[str, list[str]]]) -> list[str]:
  if triggers == EXPECTED_TRIGGERS:
    return []
  return [
    "workflow top-level on: must be exactly "
    f"{_format_triggers(EXPECTED_TRIGGERS)}, found: {_format_triggers(triggers)}"
  ]


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml keeps the expected top-level workflow trigger policy"
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
    workflow_text = read_text(workflow_path, CiWorkflowTriggersError)
    triggers = collect_top_level_triggers(workflow_text)
  except CiWorkflowTriggersError as exc:
    fail(str(exc))

  errors = validate_triggers(triggers)
  if errors:
    fail("; ".join(errors))

  print(f"ci-workflow-triggers: triggers={len(triggers)}")
  print("ci-workflow-triggers check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
