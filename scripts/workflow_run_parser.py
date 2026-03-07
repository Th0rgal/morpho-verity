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
INLINE_ENV_KEY_RE = re.compile(r"^[A-Z_][A-Z0-9_]*$")
TAG_PROPERTY_RE = re.compile(r"^!(?:<[^>]+>|[^\s]+)?(?:\s+(.*))?$", re.DOTALL)
ANCHOR_PROPERTY_RE = re.compile(r"^&([^\s]+)(?:\s+(.*))?$", re.DOTALL)
ALIAS_SCALAR_RE = re.compile(r"^\*([^\s]+)$")
SINGLE_QUOTED_SCALAR_RE = re.compile(r"^'(?:[^']|'')*'(?:\s+#.*)?\s*$", re.DOTALL)
DOUBLE_QUOTED_SCALAR_RE = re.compile(r'^"(?:[^"\\]|\\.)*"(?:\s+#.*)?\s*$', re.DOTALL)

DOUBLE_QUOTED_ESCAPES = {
  "0": "\0",
  "a": "\a",
  "b": "\b",
  "t": "\t",
  "n": "\n",
  "v": "\v",
  "f": "\f",
  "r": "\r",
  "e": "\x1b",
  " ": " ",
  '"': '"',
  "/": "/",
  "\\": "\\",
  "N": "\x85",
  "_": "\xa0",
  "L": "\u2028",
  "P": "\u2029",
}


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

  block_scalar = _consume_block_scalar(lines, start_index, run_indent, tail)
  if block_scalar is None:
    raise ValueError("expected block scalar run field")
  command, next_index, _ = block_scalar
  return command, next_index


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


def _extract_block_scalar_indent(tail: str) -> int | None:
  header = tail.split("#", 1)[0].strip()
  indicator = header[1:]
  if not indicator:
    return None
  for char in indicator:
    if char.isdigit():
      return int(char)
  return None


def _strip_yaml_node_properties(value: str) -> tuple[str, list[str]]:
  anchors: list[str] = []
  previous = None
  while value and value != previous:
    previous = value
    tag_match = TAG_PROPERTY_RE.match(value)
    if tag_match is not None:
      value = (tag_match.group(1) or "").lstrip()
      continue
    anchor_match = ANCHOR_PROPERTY_RE.match(value)
    if anchor_match is not None:
      anchors.append(anchor_match.group(1))
      value = (anchor_match.group(2) or "").lstrip()
  return value, anchors


def _starts_quoted_yaml_scalar(raw: str) -> str | None:
  value, _ = _strip_yaml_node_properties(raw.strip())
  if value.startswith("'"):
    return "'"
  if value.startswith('"'):
    return '"'
  return None


def _is_complete_quoted_yaml_scalar(raw: str) -> bool:
  value, _ = _strip_yaml_node_properties(raw.strip())
  if value.startswith("'"):
    return SINGLE_QUOTED_SCALAR_RE.fullmatch(value) is not None
  if value.startswith('"'):
    return DOUBLE_QUOTED_SCALAR_RE.fullmatch(value) is not None
  return False


def _consume_multiline_quoted_scalar(
  lines: list[str],
  start_index: int,
  raw: str,
  field_indent: int,
) -> tuple[str, int]:
  if _starts_quoted_yaml_scalar(raw) is None or _is_complete_quoted_yaml_scalar(raw):
    return raw, start_index + 1

  parts = [raw.rstrip()]
  next_index = start_index + 1
  while next_index < len(lines):
    candidate = lines[next_index]
    stripped = candidate.lstrip(" ")
    if stripped:
      indent = len(candidate) - len(stripped)
      if indent <= field_indent:
        break
      parts.append(stripped)
    else:
      parts.append("")
    combined = "\n".join(parts)
    next_index += 1
    if _is_complete_quoted_yaml_scalar(combined):
      return combined, next_index
  return raw, start_index + 1


def _consume_multiline_inline_env_mapping(
  lines: list[str],
  start_index: int,
  raw: str,
  field_indent: int,
) -> tuple[str, int]:
  if not raw.strip().startswith("{") or _extract_inline_env_mapping_body(raw) is not None:
    return raw, start_index + 1

  parts = [raw.rstrip()]
  next_index = start_index + 1
  while next_index < len(lines):
    candidate = lines[next_index]
    stripped = candidate.lstrip(" ")
    if stripped:
      indent = len(candidate) - len(stripped)
      if indent <= field_indent:
        break
      parts.append(stripped)
    else:
      parts.append("")
    combined = "\n".join(parts)
    next_index += 1
    if _extract_inline_env_mapping_body(combined) is not None:
      return combined, next_index
  return raw, start_index + 1


def _consume_block_scalar(
  lines: list[str],
  start_index: int,
  field_indent: int,
  tail: str,
) -> tuple[str, int, list[str]] | None:
  normalized_tail, declared_anchors = _strip_yaml_node_properties(tail.strip())
  if RUN_BLOCK_SCALAR_RE.fullmatch(normalized_tail) is None:
    return None

  next_index = start_index + 1
  block_lines: list[tuple[str, int | None]] = []
  while next_index < len(lines):
    candidate = lines[next_index]
    stripped = candidate.lstrip(" ")
    if stripped:
      indent = len(candidate) - len(stripped)
      if indent <= field_indent:
        break
      block_lines.append((candidate, indent))
    else:
      block_lines.append(("", None))
    next_index += 1
  if not any(indent is not None for _, indent in block_lines):
    return "", next_index, declared_anchors
  explicit_indent = _extract_block_scalar_indent(normalized_tail)
  content_indent = (
    field_indent + explicit_indent
    if explicit_indent is not None
    else min(indent for _, indent in block_lines if indent is not None)
  )
  normalized_lines = [
    line[content_indent:] if indent is not None else ""
    for line, indent in block_lines
  ]
  if normalized_tail.startswith(">"):
    return _fold_block_scalar_lines(normalized_lines), next_index, declared_anchors
  return "\n".join(normalized_lines), next_index, declared_anchors


def _fold_quoted_yaml_scalar_lines(inner: str, quote: str) -> str:
  if "\n" not in inner:
    return inner
  if quote == '"':
    inner = re.sub(r"\\\n[ \t]*", "", inner)
  return re.sub(r"\n[ \t]*", " ", inner)


def _parse_scalar_env_value(raw: str, anchors: dict[str, str] | None = None) -> str | None:
  value = _strip_yaml_comment(raw).strip()
  if not value:
    return None
  value, declared_anchors = _strip_yaml_node_properties(value)
  if not value:
    return None
  alias_match = ALIAS_SCALAR_RE.fullmatch(value)
  if alias_match is not None:
    return anchors.get(alias_match.group(1)) if anchors is not None else None

  parsed: str | None
  if value[0] in {'"', "'"}:
    if len(value) < 2 or value[-1] != value[0]:
      return None
    inner = _fold_quoted_yaml_scalar_lines(value[1:-1], value[0])
    if value[0] == "'":
      parsed = inner.replace("''", "'")
    else:
      parsed = _parse_double_quoted_yaml_scalar(inner)
  else:
    parsed = value

  if parsed is not None and anchors is not None:
    for anchor_name in declared_anchors:
      anchors[anchor_name] = parsed
  return parsed


def _parse_double_quoted_yaml_scalar(inner: str) -> str | None:
  parsed: list[str] = []
  i = 0
  while i < len(inner):
    char = inner[i]
    if char != "\\":
      parsed.append(char)
      i += 1
      continue
    i += 1
    if i >= len(inner):
      return None
    escape = inner[i]
    if escape in DOUBLE_QUOTED_ESCAPES:
      parsed.append(DOUBLE_QUOTED_ESCAPES[escape])
      i += 1
      continue
    if escape in {"x", "u", "U"}:
      width = {"x": 2, "u": 4, "U": 8}[escape]
      digits = inner[i + 1:i + 1 + width]
      if len(digits) != width or re.fullmatch(rf"[0-9A-Fa-f]{{{width}}}", digits) is None:
        return None
      parsed.append(chr(int(digits, 16)))
      i += 1 + width
      continue
    return None
  return "".join(parsed)


def _strip_yaml_comment(raw: str) -> str:
  in_single_quote = False
  in_double_quote = False
  escape = False
  for index, char in enumerate(raw):
    if in_single_quote:
      if char == "'":
        in_single_quote = False
      continue
    if in_double_quote:
      if escape:
        escape = False
        continue
      if char == "\\":
        escape = True
        continue
      if char == '"':
        in_double_quote = False
      continue
    if char == "'":
      in_single_quote = True
      continue
    if char == '"':
      in_double_quote = True
      continue
    if char == "#" and (index == 0 or raw[index - 1].isspace()):
      return raw[:index].rstrip()
  return raw


def _extract_inline_env_mapping_body(raw: str) -> str | None:
  value = raw.strip()
  if not value.startswith("{"):
    return None

  depth = 0
  in_single_quote = False
  in_double_quote = False
  escape = False
  index = 0
  while index < len(value):
    char = value[index]
    if in_single_quote:
      if char == "'":
        if index + 1 < len(value) and value[index + 1] == "'":
          index += 2
          continue
        in_single_quote = False
      index += 1
      continue
    if in_double_quote:
      if escape:
        escape = False
        index += 1
        continue
      if char == "\\":
        escape = True
        index += 1
        continue
      if char == '"':
        in_double_quote = False
      index += 1
      continue

    if char == "'":
      in_single_quote = True
      index += 1
      continue
    if char == '"':
      in_double_quote = True
      index += 1
      continue
    if char == "{":
      depth += 1
      index += 1
      continue
    if char != "}":
      index += 1
      continue

    depth -= 1
    if depth != 0:
      index += 1
      continue
    remainder = value[index + 1:].strip()
    if remainder and not remainder.startswith("#"):
      return None
    return value[1:index]
  return None


def _split_inline_env_mapping_entries(body: str) -> list[str] | None:
  entries: list[str] = []
  start = 0
  in_single_quote = False
  in_double_quote = False
  escape = False
  index = 0
  while index < len(body):
    char = body[index]
    if in_single_quote:
      if char == "'":
        if index + 1 < len(body) and body[index + 1] == "'":
          index += 2
          continue
        in_single_quote = False
      index += 1
      continue
    if in_double_quote:
      if escape:
        escape = False
        index += 1
        continue
      if char == "\\":
        escape = True
        index += 1
        continue
      if char == '"':
        in_double_quote = False
      index += 1
      continue

    if char == "'":
      in_single_quote = True
      index += 1
      continue
    if char == '"':
      in_double_quote = True
      index += 1
      continue
    if char == ",":
      entry = body[start:index].strip()
      if not entry:
        if entries:
          start = index + 1
          index += 1
          continue
        return None
      entries.append(entry)
      start = index + 1
    index += 1

  if in_single_quote or in_double_quote:
    return None
  entry = body[start:].strip()
  if not entry:
    return entries if entries else None
  entries.append(entry)
  return entries


def _split_inline_env_entry(entry: str) -> tuple[str, str] | None:
  in_single_quote = False
  in_double_quote = False
  escape = False
  index = 0
  while index < len(entry):
    char = entry[index]
    if in_single_quote:
      if char == "'":
        if index + 1 < len(entry) and entry[index + 1] == "'":
          index += 2
          continue
        in_single_quote = False
      index += 1
      continue
    if in_double_quote:
      if escape:
        escape = False
        index += 1
        continue
      if char == "\\":
        escape = True
        index += 1
        continue
      if char == '"':
        in_double_quote = False
      index += 1
      continue

    if char == "'":
      in_single_quote = True
      index += 1
      continue
    if char == '"':
      in_double_quote = True
      index += 1
      continue
    if char == ":":
      key = entry[:index].strip()
      value = entry[index + 1:].strip()
      if INLINE_ENV_KEY_RE.fullmatch(key) is None or not value:
        return None
      return key, value
    index += 1
  return None


def _parse_inline_env_mapping(raw: str, anchors: dict[str, str] | None = None) -> dict[str, list[str]] | None:
  body = _extract_inline_env_mapping_body(raw)
  if body is None:
    return None
  if not body.strip():
    return {}
  parsed: dict[str, list[str]] = {}
  entries = _split_inline_env_mapping_entries(body)
  if entries is None:
    return None
  for entry in entries:
    split_entry = _split_inline_env_entry(entry)
    if split_entry is None:
      return None
    key, raw_value = split_entry
    scalar = _parse_scalar_env_value(raw_value, anchors)
    if scalar is not None:
      parsed.setdefault(key, []).append(scalar)
  return parsed


def _consume_env_mapping(
  lines: list[str],
  start_index: int,
  env_indent: int,
  anchors: dict[str, str] | None = None,
) -> tuple[dict[str, list[str]], int]:
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
    raw_value, next_index = _consume_multiline_quoted_scalar(
      lines,
      next_index,
      entry_match.group(3),
      env_indent + 2,
    )
    block_scalar = _consume_block_scalar(lines, next_index - 1, env_indent + 2, raw_value)
    if block_scalar is not None:
      raw_value, next_index, declared_anchors = block_scalar
      if anchors is not None:
        for anchor_name in declared_anchors:
          anchors[anchor_name] = raw_value
    value = _parse_scalar_env_value(raw_value, anchors)
    if value is not None:
      values.setdefault(entry_match.group(2), []).append(value)
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
  anchors: dict[str, str] = {}
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

    env_value, _ = _strip_yaml_node_properties(env_tail.strip())
    if env_tail and env_value:
      raw_inline_env, i = _consume_multiline_inline_env_mapping(lines, i, env_value, env_indent)
      env_values = _parse_inline_env_mapping(raw_inline_env, anchors)
      if env_values is None:
        if i == len(lines):
          i += 1
        continue
    else:
      env_values, i = _consume_env_mapping(lines, i, env_indent, anchors)
    for key, extracted in env_values.items():
      values.setdefault(key, []).extend(extracted)
  return values


def extract_named_step_runs(workflow_text: str) -> tuple[dict[str, int], dict[str, list[str]]]:
  """Return named workflow step counts and their step-level inline `run:` lines."""
  step_counts: dict[str, int] = {}
  step_runs: dict[str, list[str]] = {}
  anchors: dict[str, str] = {}
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
      raw_name, i = _consume_multiline_quoted_scalar(
        lines,
        i,
        name_match.group(2),
        current_step_indent + 2,
      )
      parsed_name = _parse_scalar_env_value(raw_name, anchors)
      current_step_name = parsed_name
      if current_step_name is not None:
        step_counts[current_step_name] = step_counts.get(current_step_name, 0) + 1
        step_runs.setdefault(current_step_name, [])
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
