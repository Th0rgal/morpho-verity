#!/usr/bin/env python3
"""Utilities for extracting executable `run:` command text from CI workflow YAML."""

from __future__ import annotations

import re

JOBS_FIELD_RE = re.compile(r"^(\s*)jobs:\s*$")
MAPPING_FIELD_RE = re.compile(r"^(\s*)([A-Za-z0-9_-]+):\s*(.*)$")
STEPS_FIELD_RE = re.compile(r"^(\s*)steps:\s*$")
STEP_ITEM_RE = re.compile(r"^(\s*)-\s*(.*)$")
NAME_FIELD_RE = re.compile(r"^(\s*)name:\s*(.+?)\s*$")
ENV_LINE_RE = re.compile(r"^(\s*)env:\s*(.*?)\s*$")
ENV_ENTRY_RE = re.compile(r"^(\s*)([A-Z_][A-Z0-9_]*):\s*(.+?)\s*$")
RUN_FIELD_RE = re.compile(r"^(\s*)run:\s*(.*)$")
RUN_BLOCK_SCALAR_RE = re.compile(r"^[|>](?:[1-9][-+]?|[-+]?[1-9]|[-+])?(?:\s+#.*)?$")
INLINE_ENV_ENTRY_RE = re.compile(r"""
  \s*
  ([A-Z_][A-Z0-9_]*)
  \s*:\s*
  (
    "(?:[^"\\]|\\.)*"
    |
    '(?:[^'\\]|\\.)*'
    |
    [^,{}]+
  )
  \s*
  (?:,|$)
""", re.VERBOSE)


def _consume_run_command(
  lines: list[str],
  start_index: int,
  run_indent: int,
  *,
  first_line: str | None = None,
) -> tuple[str, int]:
  """Return the full command text for a matched `run:` line and the next line index."""
  match = RUN_FIELD_RE.match(lines[start_index] if first_line is None else first_line)
  if match is None:
    raise ValueError("expected run field at start_index")

  tail = match.group(2).strip()
  if tail and not RUN_BLOCK_SCALAR_RE.fullmatch(tail):
    line_parts = [tail]
    next_index = start_index + 1
    while next_index < len(lines):
      candidate = lines[next_index]
      stripped = candidate.lstrip(" ")
      if not stripped:
        break
      indent = len(candidate) - len(stripped)
      if indent <= run_indent:
        break
      line_parts.append(stripped)
      next_index += 1
    return "\n".join(line_parts), next_index

  next_index = start_index + 1
  block_lines: list[tuple[str, int | None]] = []
  while next_index < len(lines):
    candidate = lines[next_index]
    stripped = candidate.lstrip(" ")
    if stripped:
      indent = len(candidate) - len(stripped)
      if indent <= run_indent:
        break
      block_lines.append((candidate, indent))
    else:
      block_lines.append(("", None))
    next_index += 1
  if not any(indent is not None for _, indent in block_lines):
    return "", next_index
  content_indent = min(
    indent for _, indent in block_lines if indent is not None
  )
  normalized_lines = [
    line[content_indent:] if indent is not None else ""
    for line, indent in block_lines
  ]
  if tail.startswith(">"):
    return _fold_block_scalar_lines(normalized_lines), next_index
  return "\n".join(normalized_lines), next_index


def _fold_block_scalar_lines(lines: list[str]) -> str:
  """Approximate YAML folded scalar semantics for workflow `run: >` bodies."""
  folded: list[str] = []
  for line in lines:
    if not folded:
      folded.append(line)
      continue
    previous = folded[-1]
    if (
      not previous
      or not line
      or previous.startswith(" ")
      or line.startswith(" ")
    ):
      folded.append("\n")
    else:
      folded.append(" ")
    folded.append(line)
  return "".join(folded)


def _parse_scalar_env_value(raw: str) -> str | None:
  value = raw.strip()
  if not value:
    return None
  if value[0] in {'"', "'"}:
    if len(value) < 2 or value[-1] != value[0]:
      return None
    return value[1:-1]
  return value


def _parse_inline_env_mapping(raw: str) -> dict[str, list[str]] | None:
  value = raw.strip()
  if not value.startswith("{") or not value.endswith("}"):
    return None
  body = value[1:-1]
  if not body.strip():
    return {}
  parsed: dict[str, list[str]] = {}
  position = 0
  while position < len(body):
    match = INLINE_ENV_ENTRY_RE.match(body, position)
    if match is None:
      return None
    scalar = _parse_scalar_env_value(match.group(2))
    if scalar is not None:
      parsed.setdefault(match.group(1), []).append(scalar)
    position = match.end()
  return parsed


def _consume_env_mapping(lines: list[str], start_index: int, env_indent: int) -> tuple[dict[str, list[str]], int]:
  values: dict[str, list[str]] = {}
  next_index = start_index + 1
  while next_index < len(lines):
    candidate = lines[next_index]
    stripped = candidate.lstrip(" ")
    if not stripped:
      next_index += 1
      continue
    indent = len(candidate) - len(stripped)
    if indent <= env_indent:
      break
    entry_match = ENV_ENTRY_RE.match(candidate)
    if entry_match is None or len(entry_match.group(1)) != env_indent + 2:
      next_index += 1
      continue
    value = _parse_scalar_env_value(entry_match.group(3))
    if value is not None:
      values.setdefault(entry_match.group(2), []).append(value)
    next_index += 1
  return values, next_index


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

    command, i = _consume_run_command(lines, i, run_indent, first_line=line)
    commands.append(command)
  return "\n".join(commands)


def extract_workflow_env_literals(workflow_text: str) -> dict[str, list[str]]:
  """Return scalar env literals from workflow, job, and step `env:` mappings only."""
  values: dict[str, list[str]] = {}
  lines = workflow_text.splitlines()
  i = 0
  jobs_indent: int | None = None
  current_job_indent: int | None = None
  steps_indent: int | None = None
  current_step_indent: int | None = None
  while i < len(lines):
    line = lines[i]
    stripped = line.lstrip(" ")
    indent = len(line) - len(stripped)
    if stripped:
      if jobs_indent is not None and indent <= jobs_indent:
        jobs_indent = None
        current_job_indent = None
        steps_indent = None
        current_step_indent = None
      elif current_job_indent is not None and indent <= current_job_indent:
        current_job_indent = None
        steps_indent = None
        current_step_indent = None
      elif steps_indent is not None and indent <= steps_indent:
        steps_indent = None
        current_step_indent = None
      elif current_step_indent is not None and indent <= current_step_indent:
        current_step_indent = None

    jobs_match = JOBS_FIELD_RE.match(line)
    if jobs_match is not None:
      jobs_indent = len(jobs_match.group(1))
      current_job_indent = None
      steps_indent = None
      current_step_indent = None
      i += 1
      continue

    if jobs_indent is not None and current_step_indent is None:
      mapping_match = MAPPING_FIELD_RE.match(line)
      if mapping_match is not None and len(mapping_match.group(1)) == jobs_indent + 2:
        current_job_indent = len(mapping_match.group(1))
        steps_indent = None
        current_step_indent = None

    steps_match = STEPS_FIELD_RE.match(line)
    if steps_match is not None:
      steps_indent = len(steps_match.group(1))
      current_step_indent = None
      i += 1
      continue

    step_match = STEP_ITEM_RE.match(line)
    if step_match is not None and steps_indent is not None:
      step_indent = len(step_match.group(1))
      if step_indent > steps_indent and (current_step_indent is None or step_indent <= current_step_indent):
        current_step_indent = step_indent
        line = f"{step_match.group(1)}  {step_match.group(2)}"

    env_match = ENV_LINE_RE.match(line)
    if env_match is None:
      i += 1
      continue

    env_indent = len(env_match.group(1))
    env_tail = env_match.group(2).strip()
    is_workflow_env = env_indent == 0
    is_job_env = current_job_indent is not None and current_step_indent is None and env_indent == current_job_indent + 2
    is_step_env = current_step_indent is not None and env_indent == current_step_indent + 2
    if not (is_workflow_env or is_job_env or is_step_env):
      i += 1
      continue

    if env_tail:
      env_values = _parse_inline_env_mapping(env_tail)
      if env_values is None:
        i += 1
        continue
      i += 1
    else:
      env_values, i = _consume_env_mapping(lines, i, env_indent)
    for key, extracted in env_values.items():
      values.setdefault(key, []).extend(extracted)
  return values


def extract_named_step_runs(workflow_text: str) -> tuple[dict[str, int], dict[str, list[str]]]:
  """Return named workflow step counts and their step-level inline `run:` lines."""
  step_counts: dict[str, int] = {}
  step_runs: dict[str, list[str]] = {}
  lines = workflow_text.splitlines()
  steps_indent: int | None = None
  current_step_indent: int | None = None
  current_step_name: str | None = None
  i = 0
  while i < len(lines):
    line = lines[i]
    steps_match = STEPS_FIELD_RE.match(line)
    if steps_match is not None:
      steps_indent = len(steps_match.group(1))
      current_step_indent = None
      current_step_name = None
      i += 1
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
      i += 1
      continue

    name_match = NAME_FIELD_RE.match(line)
    if name_match is not None and len(name_match.group(1)) == current_step_indent + 2:
      current_step_name = name_match.group(2)
      step_counts[current_step_name] = step_counts.get(current_step_name, 0) + 1
      step_runs.setdefault(current_step_name, [])
      i += 1
      continue

    run_match = RUN_FIELD_RE.match(line)
    if (
      run_match is not None
      and current_step_name is not None
      and len(run_match.group(1)) == current_step_indent + 2
    ):
      command, i = _consume_run_command(lines, i, len(run_match.group(1)))
      step_runs[current_step_name].append(command)
      continue

    i += 1

  return step_counts, step_runs
