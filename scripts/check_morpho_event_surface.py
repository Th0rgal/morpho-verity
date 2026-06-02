#!/usr/bin/env python3
"""Fail-closed check for Morpho contract event metadata.

The canonical compiler boundary is the macro contract in morpho-blue-verity/Morpho/Contract.lean.
Keep that event_defs surface synchronized with Morpho Blue's EventsLib so
missing event payloads cannot silently pass.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from dataclasses import dataclass


ROOT = pathlib.Path(__file__).resolve().parent.parent
EVENTS_LIB_PATH = ROOT / "morpho-blue" / "src" / "libraries" / "EventsLib.sol"
CONTRACT_PATH = ROOT / "morpho-blue-verity" / "Morpho" / "Contract.lean"

SOL_EVENT_RE = re.compile(r"\bevent\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)\s*;", re.DOTALL)


@dataclass(frozen=True)
class EventParam:
  name: str
  ty: str
  indexed: bool


EventSurface = dict[str, list[EventParam]]


class MorphoEventSurfaceError(RuntimeError):
  pass


def fail(msg: str) -> None:
  print(f"morpho-event-surface check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise MorphoEventSurfaceError(f"failed to read {context} {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise MorphoEventSurfaceError(f"failed to decode {context} {path} as UTF-8: {exc}") from exc


def split_top_level(src: str) -> list[str]:
  out: list[str] = []
  current: list[str] = []
  bracket_depth = 0
  paren_depth = 0
  for ch in src:
    if ch == "[":
      bracket_depth += 1
      current.append(ch)
    elif ch == "]":
      bracket_depth -= 1
      current.append(ch)
    elif ch == "(":
      paren_depth += 1
      current.append(ch)
    elif ch == ")":
      paren_depth -= 1
      current.append(ch)
    elif ch == "," and bracket_depth == 0 and paren_depth == 0:
      part = "".join(current).strip()
      if part:
        out.append(part)
      current = []
    else:
      current.append(ch)
  tail = "".join(current).strip()
  if tail:
    out.append(tail)
  return out


def normalize_solidity_type(ty: str) -> str:
  ty = " ".join(ty.split())
  if ty == "Id":
    return "bytes32"
  if ty == "MarketParams":
    return "tuple(address,address,address,address,uint256)"
  return ty


def normalize_lean_type(ty: str) -> str:
  ty = " ".join(ty.strip().split())
  if ty.startswith("."):
    ty = ty[1:]
  if ty.startswith("tuple [") and ty.endswith("]"):
    inner = ty[len("tuple ["):-1]
    elems = [normalize_lean_type(part) for part in split_top_level(inner)]
    return "tuple(" + ",".join(elems) + ")"
  if ty == "MarketParams":
    return "tuple(address,address,address,address,uint256)"
  if ty == "Address":
    return "address"
  if ty == "Bytes32":
    return "bytes32"
  if ty == "Uint256":
    return "uint256"
  if ty == "Bool":
    return "bool"
  return ty.replace(".", "")


def extract_solidity_events(text: str) -> EventSurface:
  events: EventSurface = {}
  for match in SOL_EVENT_RE.finditer(text):
    name = match.group(1)
    params: list[EventParam] = []
    for raw_param in split_top_level(match.group(2)):
      tokens = raw_param.split()
      if len(tokens) < 2:
        raise MorphoEventSurfaceError(f"unable to parse Solidity event parameter: {raw_param!r}")
      indexed = "indexed" in tokens
      filtered = [token for token in tokens if token != "indexed"]
      if len(filtered) != 2:
        raise MorphoEventSurfaceError(f"unsupported Solidity event parameter: {raw_param!r}")
      params.append(EventParam(name=filtered[1], ty=normalize_solidity_type(filtered[0]), indexed=indexed))
    if name in events:
      raise MorphoEventSurfaceError(f"duplicate Solidity event declaration: {name}")
    events[name] = params
  if not events:
    raise MorphoEventSurfaceError("no Solidity events found")
  return events


def find_balanced_slice(text: str, start: int, open_ch: str, close_ch: str) -> tuple[str, int]:
  depth = 0
  for idx in range(start, len(text)):
    ch = text[idx]
    if ch == open_ch:
      depth += 1
    elif ch == close_ch:
      depth -= 1
      if depth == 0:
        return text[start + 1:idx], idx + 1
  raise MorphoEventSurfaceError(f"unclosed {open_ch}{close_ch} block")


def extract_contract_events(text: str) -> EventSurface:
  if "event_defs" in text:
    return extract_contract_event_defs(text)

  marker = "private def morphoEvents"
  marker_idx = text.find(marker)
  if marker_idx == -1:
    raise MorphoEventSurfaceError("unable to find private def morphoEvents")
  list_start = text.find("[", marker_idx)
  if list_start == -1:
    raise MorphoEventSurfaceError("unable to find morphoEvents list")
  list_body, _ = find_balanced_slice(text, list_start, "[", "]")

  events: EventSurface = {}
  offset = 0
  while True:
    event_start = list_body.find("{", offset)
    if event_start == -1:
      break
    event_body, next_offset = find_balanced_slice(list_body, event_start, "{", "}")
    offset = next_offset

    name_match = re.search(r'name\s*:=\s*"([^"]+)"', event_body)
    params_idx = event_body.find("params := [")
    if not name_match or params_idx == -1:
      continue
    params_start = event_body.find("[", params_idx)
    params_body, _ = find_balanced_slice(event_body, params_start, "[", "]")
    params: list[EventParam] = []
    record_offset = 0
    while True:
      record_start = params_body.find("⟨", record_offset)
      if record_start == -1:
        break
      record_end = params_body.find("⟩", record_start)
      if record_end == -1:
        raise MorphoEventSurfaceError("unterminated contract event parameter record")
      fields = split_top_level(params_body[record_start + 1:record_end])
      if len(fields) != 3:
        raise MorphoEventSurfaceError(f"contract event parameter must have 3 fields: {fields!r}")
      param_name = fields[0].strip().strip('"')
      param_ty = normalize_lean_type(fields[1])
      kind = fields[2].strip()
      if kind not in {".indexed", ".unindexed"}:
        raise MorphoEventSurfaceError(f"unsupported contract event parameter kind: {kind!r}")
      params.append(EventParam(name=param_name, ty=param_ty, indexed=kind == ".indexed"))
      record_offset = record_end + 1

    event_name = name_match.group(1)
    if event_name in events:
      raise MorphoEventSurfaceError(f"duplicate contract event declaration: {event_name}")
    events[event_name] = params

  if not events:
    raise MorphoEventSurfaceError("no contract events found")
  return events


def extract_contract_event_defs(text: str) -> EventSurface:
  marker_idx = text.find("event_defs")
  if marker_idx == -1:
    raise MorphoEventSurfaceError("unable to find event_defs")

  events: EventSurface = {}
  for line in text[marker_idx:].splitlines()[1:]:
    stripped = line.strip()
    if not stripped:
      continue
    if not stripped.startswith("event "):
      if events:
        break
      continue
    match = re.match(r"event\s+([A-Za-z_][A-Za-z0-9_]*)\((.*)\)$", stripped)
    if not match:
      raise MorphoEventSurfaceError(f"unable to parse contract event declaration: {stripped!r}")
    name = match.group(1)
    params: list[EventParam] = []
    for raw_param in split_top_level(match.group(2)):
      raw_param = raw_param.strip()
      indexed = raw_param.startswith("@indexed ")
      if indexed:
        raw_param = raw_param[len("@indexed "):].strip()
      if ":" not in raw_param:
        raise MorphoEventSurfaceError(f"unable to parse event parameter: {raw_param!r}")
      param_name, param_ty = [part.strip() for part in raw_param.split(":", 1)]
      params.append(EventParam(name=param_name, ty=normalize_lean_type(param_ty), indexed=indexed))
    if name in events:
      raise MorphoEventSurfaceError(f"duplicate contract event declaration: {name}")
    events[name] = params
  if not events:
    raise MorphoEventSurfaceError("no contract events found")
  return events


def format_param(param: EventParam) -> str:
  indexed = " indexed" if param.indexed else ""
  return f"{param.ty}{indexed} {param.name}"


def format_event(name: str, params: list[EventParam]) -> str:
  return f"{name}(" + ", ".join(format_param(param) for param in params) + ")"


def validate_event_surface(solidity: EventSurface, generated: EventSurface) -> None:
  solidity_order = list(solidity)
  generated_order = list(generated)
  missing = sorted(set(solidity) - set(generated))
  extra = sorted(set(generated) - set(solidity))
  mismatched: list[str] = []
  for name in sorted(set(solidity) & set(generated)):
    if generated[name] != solidity[name]:
      mismatched.append(
        f"{name}: expected {format_event(name, solidity[name])}; "
        f"got {format_event(name, generated[name])}"
      )
  problems: list[str] = []
  if missing:
    problems.append("missing contract events: " + ", ".join(missing))
  if extra:
    problems.append("extra contract events: " + ", ".join(extra))
  if not missing and not extra and generated_order != solidity_order:
    problems.append(
      "contract event order drift: expected "
      + ", ".join(solidity_order)
      + "; got "
      + ", ".join(generated_order)
    )
  if mismatched:
    problems.append("event parameter mismatches: " + "; ".join(mismatched))
  if problems:
    raise MorphoEventSurfaceError(" | ".join(problems))


def main() -> int:
  parser = argparse.ArgumentParser(description="Validate Morpho contract events against EventsLib.sol")
  parser.add_argument("--events-lib", type=pathlib.Path, default=EVENTS_LIB_PATH)
  parser.add_argument("--contract", type=pathlib.Path, default=CONTRACT_PATH)
  args = parser.parse_args()

  try:
    solidity = extract_solidity_events(read_text(args.events_lib, context="EventsLib.sol"))
    generated = extract_contract_events(read_text(args.contract, context=str(args.contract)))
    validate_event_surface(solidity, generated)
  except MorphoEventSurfaceError as exc:
    fail(str(exc))

  print(f"morpho-event-surface: events={len(solidity)}")
  print("morpho-event-surface check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
