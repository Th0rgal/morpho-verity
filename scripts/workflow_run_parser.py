#!/usr/bin/env python3
"""Utilities for extracting executable `run:` command text from CI workflow YAML."""

from __future__ import annotations

import re

STEPS_FIELD_RE = re.compile(r"^(\s*)steps:\s*$")
STEP_ITEM_RE = re.compile(r"^(\s*)-\s*(.*)$")
NAME_FIELD_RE = re.compile(r"^(\s*)name:\s*(.+?)\s*$")
RUN_FIELD_RE = re.compile(r"^(\s*)run:\s*(.*)$")
RUN_BLOCK_SCALAR_RE = re.compile(r"^[|>][-+]?$")


def extract_workflow_run_text(workflow_text: str) -> str:
  """Return concatenated command text from workflow `run:` steps only."""
  commands: list[str] = []
  lines = workflow_text.splitlines()
  i = 0
  steps_indent: int | None = None
  current_step_indent: int | None = None
  while i < len(lines):
    line = lines[i]
    steps_match = STEPS_FIELD_RE.match(line)
    if steps_match is not None:
      steps_indent = len(steps_match.group(1))
      current_step_indent = None
      i += 1
      continue

    stripped = line.lstrip(" ")
    if stripped:
      indent = len(line) - len(stripped)
      if steps_indent is not None and indent <= steps_indent:
        steps_indent = None
        current_step_indent = None
      elif current_step_indent is not None and indent <= current_step_indent:
        current_step_indent = None

    step_match = STEP_ITEM_RE.match(line)
    if step_match is not None and steps_indent is not None:
      step_indent = len(step_match.group(1))
      if step_indent > steps_indent and (current_step_indent is None or step_indent <= current_step_indent):
        current_step_indent = step_indent
        line = f"{step_match.group(1)}  {step_match.group(2)}"

    match = RUN_FIELD_RE.match(line)
    # Keep support for simple top-level `run:` fixtures used by the repo's unit
    # tests while still rejecting nested non-step mappings like `defaults.run`.
    if match is None:
      i += 1
      continue

    run_indent = len(match.group(1))
    if current_step_indent is None:
      if run_indent != 0:
        i += 1
        continue
    elif run_indent != current_step_indent + 2:
      i += 1
      continue

    tail = match.group(2).strip()
    if tail and not RUN_BLOCK_SCALAR_RE.fullmatch(tail):
      line_parts = [tail]
      i += 1
      while i < len(lines):
        candidate = lines[i]
        stripped = candidate.lstrip(" ")
        if not stripped:
          break
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        line_parts.append(stripped)
        i += 1
      commands.append("\n".join(line_parts))
      continue

    i += 1
    block_lines: list[str] = []
    while i < len(lines):
      candidate = lines[i]
      stripped = candidate.lstrip(" ")
      if stripped:
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        block_lines.append(stripped)
      else:
        block_lines.append("")
      i += 1
    commands.append("\n".join(block_lines))
  return "\n".join(commands)


def extract_named_step_runs(workflow_text: str) -> tuple[dict[str, int], dict[str, list[str]]]:
  """Return named workflow step counts and their step-level inline `run:` lines."""
  step_counts: dict[str, int] = {}
  step_runs: dict[str, list[str]] = {}
  lines = workflow_text.splitlines()
  steps_indent: int | None = None
  current_step_indent: int | None = None
  current_step_name: str | None = None
  for line in lines:
    steps_match = STEPS_FIELD_RE.match(line)
    if steps_match is not None:
      steps_indent = len(steps_match.group(1))
      current_step_indent = None
      current_step_name = None
      continue

    stripped = line.lstrip(" ")
    if stripped:
      indent = len(line) - len(stripped)
      if steps_indent is not None and indent <= steps_indent:
        steps_indent = None
        current_step_indent = None
        current_step_name = None
      elif current_step_indent is not None and indent <= current_step_indent:
        current_step_indent = None
        current_step_name = None

    step_match = STEP_ITEM_RE.match(line)
    if step_match is not None and steps_indent is not None:
      step_indent = len(step_match.group(1))
      if step_indent > steps_indent and (current_step_indent is None or step_indent <= current_step_indent):
        current_step_indent = step_indent
        current_step_name = None
        line = f"{step_match.group(1)}  {step_match.group(2)}"

    if current_step_indent is None:
      continue

    name_match = NAME_FIELD_RE.match(line)
    if name_match is not None and len(name_match.group(1)) == current_step_indent + 2:
      current_step_name = name_match.group(2)
      step_counts[current_step_name] = step_counts.get(current_step_name, 0) + 1
      step_runs.setdefault(current_step_name, [])
      continue

    run_match = RUN_FIELD_RE.match(line)
    if (
      run_match is not None
      and current_step_name is not None
      and len(run_match.group(1)) == current_step_indent + 2
    ):
      step_runs[current_step_name].append(run_match.group(2).strip())

  return step_counts, step_runs
