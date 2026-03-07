#!/usr/bin/env python3
"""Utilities for extracting executable `run:` command text from CI workflow YAML."""

from __future__ import annotations

import re

STEPS_FIELD_RE = re.compile(r"^(\s*)steps:\s*$")
STEP_ITEM_RE = re.compile(r"^(\s*)-\s*(.*)$")
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
    if match is None or (current_step_indent is None and len(match.group(1)) != 0):
      i += 1
      continue

    run_indent = len(match.group(1))
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
