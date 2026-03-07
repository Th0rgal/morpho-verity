#!/usr/bin/env python3
"""Fail-closed check that workflow checkout steps declare explicit safe defaults."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
EXPECTED_FETCH_DEPTH = "1"
EXPECTED_PERSIST_CREDENTIALS = "false"


class CiWorkflowCheckoutPolicyError(RuntimeError):
  """Raised when workflow checkout policy validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-checkout-policy check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowCheckoutPolicyError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowCheckoutPolicyError(f"failed to read workflow {path}: {exc}") from exc


def _indent_of(line: str) -> int:
  return len(line) - len(line.lstrip())


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_checkout_steps(workflow_text: str) -> list[dict[str, object]]:
  lines = workflow_text.splitlines()
  checkout_steps: list[dict[str, object]] = []

  for index, line in enumerate(lines):
    stripped = line.strip()
    if not stripped.startswith("uses:"):
      continue
    uses_ref = _strip_yaml_scalar(stripped.split(":", 1)[1])
    if not uses_ref.startswith("actions/checkout@"):
      continue

    uses_indent = _indent_of(line)
    step_indent = uses_indent - 2
    name = uses_ref

    search = index - 1
    while search >= 0:
      candidate = lines[search]
      candidate_stripped = candidate.strip()
      candidate_indent = _indent_of(candidate)
      if not candidate_stripped or candidate_stripped.startswith("#"):
        search -= 1
        continue
      if candidate_indent < step_indent:
        break
      if candidate_indent == step_indent and candidate_stripped.startswith("- name:"):
        name = _strip_yaml_scalar(candidate_stripped.split(":", 1)[1]) or name
        break
      search -= 1

    with_values: dict[str, str] = {}
    cursor = index + 1
    while cursor < len(lines):
      candidate = lines[cursor]
      candidate_stripped = candidate.strip()
      candidate_indent = _indent_of(candidate)
      if candidate_indent == uses_indent and candidate_stripped.startswith("with:"):
        cursor += 1
        while cursor < len(lines):
          entry = lines[cursor]
          entry_stripped = entry.strip()
          entry_indent = _indent_of(entry)
          if entry_indent <= uses_indent and entry_stripped and not entry_stripped.startswith("#"):
            cursor -= 1
            break
          if not entry_stripped or entry_stripped.startswith("#"):
            cursor += 1
            continue
          if entry_indent != uses_indent + 2 or ":" not in entry_stripped:
            raise CiWorkflowCheckoutPolicyError(
              f"checkout step {name} must use simple with: key: value entries"
            )
          key, raw_value = entry_stripped.split(":", 1)
          value = _strip_yaml_scalar(raw_value)
          if not value:
            raise CiWorkflowCheckoutPolicyError(
              f"checkout step {name} key {key.strip()} must have a non-empty literal value"
            )
          with_values[_strip_yaml_scalar(key)] = value
          cursor += 1
        continue
      if candidate_indent <= uses_indent and candidate_stripped and not candidate_stripped.startswith("#"):
        break
      cursor += 1

    checkout_steps.append({"name": name, "uses": uses_ref, "with": with_values})

  return checkout_steps


def validate_checkout_steps(checkout_steps: list[dict[str, object]]) -> list[str]:
  errors: list[str] = []

  if not checkout_steps:
    errors.append("workflow does not define any actions/checkout steps")
    return errors

  for step in checkout_steps:
    name = str(step["name"])
    with_values = dict(step["with"])
    fetch_depth = with_values.get("fetch-depth")
    persist_credentials = with_values.get("persist-credentials")

    if fetch_depth is None:
      errors.append(f"checkout step {name} must set with.fetch-depth: {EXPECTED_FETCH_DEPTH}")
    elif "${{" in fetch_depth or fetch_depth != EXPECTED_FETCH_DEPTH:
      errors.append(
        f"checkout step {name} must set with.fetch-depth: {EXPECTED_FETCH_DEPTH}, "
        f"found: {fetch_depth}"
      )

    if persist_credentials is None:
      errors.append(
        "checkout step "
        f"{name} must set with.persist-credentials: {EXPECTED_PERSIST_CREDENTIALS}"
      )
    elif (
      "${{" in persist_credentials
      or persist_credentials.lower() != EXPECTED_PERSIST_CREDENTIALS
    ):
      errors.append(
        "checkout step "
        f"{name} must set with.persist-credentials: {EXPECTED_PERSIST_CREDENTIALS}, "
        f"found: {persist_credentials}"
      )

  return errors


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml checkout steps declare explicit safe defaults"
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
    checkout_steps = collect_checkout_steps(workflow_text)
  except CiWorkflowCheckoutPolicyError as exc:
    fail(str(exc))

  errors = validate_checkout_steps(checkout_steps)
  if errors:
    fail("; ".join(errors))

  print(f"ci-workflow-checkout-policy: checkout_steps={len(checkout_steps)}")
  print("ci-workflow-checkout-policy check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
