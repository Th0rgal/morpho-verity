#!/usr/bin/env python3
"""Validate pinned parity tuple against local toolchain and foundry config."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import sys
from typing import Any

from parity_target_config import parse_yul_identity_gate_mode as parse_parity_target_yul_identity_gate_mode


ROOT = pathlib.Path(__file__).resolve().parent.parent
TARGET_PATH = ROOT / "config" / "parity-target.json"
FOUNDRY_PATH = ROOT / "morpho-blue" / "foundry.toml"


class ParityTargetError(RuntimeError):
  """Raised when config/parity-target.json is malformed."""


def load_json(path: pathlib.Path) -> Any:
  try:
    with path.open("r", encoding="utf-8") as f:
      try:
        return json.load(f)
      except json.JSONDecodeError as exc:
        raise ParityTargetError(f"config/parity-target.json is not valid JSON: {path}") from exc
  except UnicodeDecodeError as exc:
    raise ParityTargetError(f"config/parity-target.json is not valid UTF-8: {path}") from exc
  except OSError as exc:
    raise ParityTargetError(f"failed to read config/parity-target.json: {path}: {exc}") from exc


def require_non_empty_string(value: Any, field: str) -> str:
  if not isinstance(value, str) or not value:
    raise ParityTargetError(f"config/parity-target.json missing non-empty string `{field}`")
  return value


def require_bool(value: Any, field: str) -> bool:
  if not isinstance(value, bool):
    raise ParityTargetError(f"config/parity-target.json missing boolean `{field}`")
  return value


def require_int(value: Any, field: str) -> int:
  if not isinstance(value, int) or isinstance(value, bool):
    raise ParityTargetError(f"config/parity-target.json missing integer `{field}`")
  return value


def require_object(value: Any, field: str) -> dict[str, Any]:
  if not isinstance(value, dict):
    raise ParityTargetError(f"config/parity-target.json missing object `{field}`")
  return value


def load_target(path: pathlib.Path) -> dict[str, Any]:
  data = load_json(path)
  if not isinstance(data, dict):
    raise ParityTargetError(f"config/parity-target.json root must be a JSON object: {path}")

  require_non_empty_string(data.get("id"), "id")

  solc = require_object(data.get("solc"), "solc")
  require_non_empty_string(solc.get("version"), "solc.version")
  commit = solc.get("commit")
  if commit is not None:
    require_non_empty_string(commit, "solc.commit")

  foundry = require_object(data.get("foundryDefaultProfile"), "foundryDefaultProfile")
  require_bool(foundry.get("optimizer"), "foundryDefaultProfile.optimizer")
  require_int(foundry.get("optimizerRuns"), "foundryDefaultProfile.optimizerRuns")
  require_bool(foundry.get("viaIR"), "foundryDefaultProfile.viaIR")
  require_non_empty_string(foundry.get("evmVersion"), "foundryDefaultProfile.evmVersion")
  require_non_empty_string(foundry.get("bytecodeHash"), "foundryDefaultProfile.bytecodeHash")

  verity = require_object(data.get("verity"), "verity")
  require_non_empty_string(verity.get("parityPackId"), "verity.parityPackId")

  return data


def read_text(path: pathlib.Path) -> str:
  try:
    with path.open("r", encoding="utf-8") as f:
      return f.read()
  except UnicodeDecodeError as exc:
    raise ParityTargetError(f"failed to decode text file as UTF-8: {path}") from exc
  except OSError as exc:
    raise ParityTargetError(f"failed to read text file: {path}: {exc}") from exc


def parse_solc_version() -> tuple[str, str]:
  try:
    out = subprocess.check_output(["solc", "--version"], text=True)
  except OSError as exc:
    raise ParityTargetError(f"failed to execute `solc --version`: {exc}") from exc
  except subprocess.CalledProcessError as exc:
    raise ParityTargetError(f"`solc --version` exited with status {exc.returncode}") from exc
  match = re.search(r"Version:\s+(\d+\.\d+\.\d+)\+commit\.([0-9a-fA-F]+)", out)
  if not match:
    raise ParityTargetError(f"unable to parse `solc --version` output:\n{out}")
  return match.group(1), match.group(2).lower()


def parse_foundry_default(text: str) -> dict[str, Any]:
  header = re.search(r"(?m)^\[profile\.default\]\s*$", text)
  if not header:
    raise RuntimeError("Missing [profile.default] section in morpho-blue/foundry.toml")
  start = header.end()
  next_section = re.search(r"(?m)^\[", text[start:])
  end = start + next_section.start() if next_section else len(text)
  section = text[start:end]

  trailing_comment = r"(?:\s+#.*)?"

  def get_bool(name: str) -> bool:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*(true|false)\s*{trailing_comment}$", section)
    if not m:
      raise RuntimeError(f"Missing boolean key `{name}` in [profile.default]")
    return m.group(1) == "true"

  def get_int(name: str) -> int:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*([\d_]+)\s*{trailing_comment}$", section)
    if not m:
      raise RuntimeError(f"Missing integer key `{name}` in [profile.default]")
    return int(m.group(1).replace("_", ""))

  def get_str(name: str) -> str:
    m = re.search(rf"(?m)^\s*{re.escape(name)}\s*=\s*\"([^\"]+)\"\s*{trailing_comment}$", section)
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


def parse_yul_identity_gate_mode(target: dict[str, Any]) -> str:
  return parse_parity_target_yul_identity_gate_mode(
    target,
    missing_message="missing required config `yulIdentity.gateMode` in config/parity-target.json",
    invalid_message_prefix="invalid config `yulIdentity.gateMode` in config/parity-target.json",
  )


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--target",
    type=pathlib.Path,
    default=TARGET_PATH,
    help="Path to config/parity-target.json",
  )
  parser.add_argument(
    "--foundry",
    type=pathlib.Path,
    default=FOUNDRY_PATH,
    help="Path to morpho-blue/foundry.toml",
  )
  return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
  args = parse_args(argv)
  target_path = args.target.resolve()
  foundry_path = args.foundry.resolve()
  try:
    target = load_target(target_path)
    foundry = parse_foundry_default(read_text(foundry_path))
    version, commit = parse_solc_version()
  except (ParityTargetError, RuntimeError) as exc:
    fail(str(exc))
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

  verity_pack = target["verity"]["parityPackId"]
  try:
    yul_gate_mode = parse_yul_identity_gate_mode(target)
  except (ParityTargetError, RuntimeError) as exc:
    fail(str(exc))

  print(f"parity-target id: {target['id']}")
  print(f"solc: {version}+commit.{commit}")
  print(f"verity parity-pack: {verity_pack}")
  print(f"yul identity gate: {yul_gate_mode}")
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
