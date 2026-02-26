#!/usr/bin/env python3
"""Validate pinned parity tuple against local toolchain and foundry config."""

from __future__ import annotations

import json
import pathlib
import re
import subprocess
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
TARGET_PATH = ROOT / "config" / "parity-target.json"
FOUNDRY_PATH = ROOT / "morpho-blue" / "foundry.toml"


def load_json(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


def parse_solc_version() -> tuple[str, str]:
  out = subprocess.check_output(["solc", "--version"], text=True)
  match = re.search(r"Version:\s+(\d+\.\d+\.\d+)\+commit\.([0-9a-fA-F]+)", out)
  if not match:
    raise RuntimeError(f"Unable to parse solc version output:\n{out}")
  return match.group(1), match.group(2).lower()


def parse_foundry_default(text: str) -> dict[str, Any]:
  header = re.search(r"(?m)^\[profile\.default\]\s*$", text)
  if not header:
    raise RuntimeError("Missing [profile.default] section in morpho-blue/foundry.toml")
  start = header.end()
  next_section = re.search(r"(?m)^\[", text[start:])
  end = start + next_section.start() if next_section else len(text)
  section = text[start:end]

  def get_bool(name: str) -> bool:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*(true|false)\s*$", section)
    if not m:
      raise RuntimeError(f"Missing boolean key `{name}` in [profile.default]")
    return m.group(1) == "true"

  def get_int(name: str) -> int:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*(\d+)\s*$", section)
    if not m:
      raise RuntimeError(f"Missing integer key `{name}` in [profile.default]")
    return int(m.group(1))

  def get_str(name: str) -> str:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*\"([^\"]+)\"\s*$", section)
    if not m:
      raise RuntimeError(f"Missing string key `{name}` in [profile.default]")
    return m.group(1)

  return {
    "optimizer": get_bool("optimizer"),
    "optimizerRuns": get_int("optimizer_runs"),
    "viaIR": get_bool("via_ir"),
    "evmVersion": get_str("evm_version"),
    "bytecodeHash": get_str("bytecode_hash"),
  }


def fail(msg: str) -> None:
  print(f"parity-target check failed: {msg}", file=sys.stderr)
  sys.exit(1)


def main() -> None:
  target = load_json(TARGET_PATH)
  foundry = parse_foundry_default(read_text(FOUNDRY_PATH))

  version, commit = parse_solc_version()
  expected_solc = target["solc"]
  if version != expected_solc["version"]:
    fail(f"solc version mismatch: expected {expected_solc['version']}, got {version}")
  expected_commit = expected_solc.get("commit")
  if expected_commit and commit != expected_commit.lower():
    fail(f"solc commit mismatch: expected {expected_commit}, got {commit}")

  expected_foundry = target["foundryDefaultProfile"]
  for key in ("optimizer", "optimizerRuns", "viaIR", "evmVersion", "bytecodeHash"):
    if foundry[key] != expected_foundry[key]:
      fail(f"foundry default `{key}` mismatch: expected {expected_foundry[key]!r}, got {foundry[key]!r}")

  print(f"parity-target id: {target['id']}")
  print(f"solc: {version}+commit.{commit}")
  print(
    "foundry.default: "
    f"optimizer={foundry['optimizer']} "
    f"runs={foundry['optimizerRuns']} "
    f"viaIR={foundry['viaIR']} "
    f"evmVersion={foundry['evmVersion']} "
    f"bytecodeHash={foundry['bytecodeHash']}"
  )
  print("parity-target check: OK")


if __name__ == "__main__":
  main()
