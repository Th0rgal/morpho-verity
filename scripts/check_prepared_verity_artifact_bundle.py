#!/usr/bin/env python3
"""Fail-closed validation for reusable prepared Verity artifact bundles."""

from __future__ import annotations

import argparse
import json
import pathlib
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
DEFAULT_PARITY_TARGET = ROOT / "config" / "parity-target.json"
DEFAULT_PIPELINE_MANIFEST = ROOT / "config" / "yul-rewrite-pipeline.json"
DEFAULT_PROOF_MANIFEST = ROOT / "config" / "yul-rewrite-proof-obligations.json"


def _display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def resolve_artifact_dir(path: pathlib.Path) -> pathlib.Path:
  candidate = path / "edsl"
  if (candidate / "Morpho.yul").is_file():
    return candidate
  return path


def _read_json(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as handle:
    data = json.load(handle)
  if not isinstance(data, dict):
    raise RuntimeError(f"Expected JSON object in {_display_path(path)}")
  return data


def _require_nonempty_file(path: pathlib.Path, *, label: str) -> None:
  if not path.is_file() or path.stat().st_size <= 0:
    raise RuntimeError(f"Missing required non-empty {label}: {_display_path(path)}")


def _parse_manifest(path: pathlib.Path) -> dict[str, str]:
  _require_nonempty_file(path, label="artifact manifest")
  parsed: dict[str, str] = {}
  for raw_line in path.read_text(encoding="utf-8").splitlines():
    line = raw_line.strip()
    if not line:
      continue
    if "=" not in line:
      raise RuntimeError(f"Malformed artifact manifest line in {_display_path(path)}: {raw_line!r}")
    key, value = line.split("=", 1)
    parsed[key] = value
  return parsed


def _required_parity_pack(path: pathlib.Path) -> str:
  data = _read_json(path)
  verity = data.get("verity")
  if not isinstance(verity, dict):
    raise RuntimeError(f"Missing object key `verity` in {_display_path(path)}")
  parity_pack = verity.get("parityPackId")
  if not isinstance(parity_pack, str) or not parity_pack.strip():
    raise RuntimeError(f"Missing required `verity.parityPackId` in {_display_path(path)}")
  return parity_pack


def _expected_proof_manifest(proof_manifest_path: pathlib.Path) -> str | None:
  if not proof_manifest_path.exists():
    return None
  return _display_path(proof_manifest_path)


def validate_prepared_verity_artifact_bundle(
    artifact_dir: pathlib.Path,
    *,
    require_bin: bool,
    require_rewrite: bool,
    parity_target_path: pathlib.Path = DEFAULT_PARITY_TARGET,
    pipeline_manifest_path: pathlib.Path = DEFAULT_PIPELINE_MANIFEST,
    proof_manifest_path: pathlib.Path = DEFAULT_PROOF_MANIFEST,
) -> pathlib.Path:
  resolved_dir = resolve_artifact_dir(artifact_dir)
  if not resolved_dir.is_dir():
    raise RuntimeError(f"Prepared artifact directory does not exist: {_display_path(resolved_dir)}")

  _require_nonempty_file(resolved_dir / "Morpho.yul", label="artifact")
  _require_nonempty_file(resolved_dir / "Morpho.abi.json", label="artifact")
  _require_nonempty_file(resolved_dir / "Morpho.stage-times.log", label="stage timing log")

  manifest = _parse_manifest(resolved_dir / "Morpho.artifact-manifest.env")
  input_digest = manifest.get("input_digest", "").strip()
  artifact_mode = manifest.get("artifact_mode", "").strip()
  skip_solc = manifest.get("skip_solc", "").strip()
  parity_pack = manifest.get("parity_pack", "").strip()

  if not input_digest:
    raise RuntimeError(
      f"Prepared artifact manifest is missing non-empty `input_digest`: "
      f"{_display_path(resolved_dir / 'Morpho.artifact-manifest.env')}"
    )
  if artifact_mode != "edsl":
    raise RuntimeError(
      f"Prepared artifact manifest must pin `artifact_mode=edsl` "
      f"(got {artifact_mode or '<missing>'})"
    )
  if skip_solc not in {"0", "1"}:
    raise RuntimeError(
      f"Prepared artifact manifest must pin `skip_solc` to 0 or 1 "
      f"(got {skip_solc or '<missing>'})"
    )

  expected_parity_pack = _required_parity_pack(parity_target_path)
  if parity_pack != expected_parity_pack:
    raise RuntimeError(
      f"Prepared artifact manifest parity pack mismatch: expected {expected_parity_pack}, got "
      f"{parity_pack or '<missing>'}"
    )

  bin_path = resolved_dir / "Morpho.bin"
  if require_bin:
    _require_nonempty_file(bin_path, label="artifact")
    if skip_solc != "0":
      raise RuntimeError("Prepared artifact bundle requires Morpho.bin, so manifest must record `skip_solc=0`")
  else:
    if bin_path.exists() and bin_path.stat().st_size <= 0:
      raise RuntimeError(f"Prepared artifact file is empty: {_display_path(bin_path)}")
    if bin_path.exists() and skip_solc == "1":
      raise RuntimeError("Prepared artifact manifest recorded `skip_solc=1`, but Morpho.bin is present")
    if not bin_path.exists() and skip_solc == "0":
      raise RuntimeError("Prepared artifact manifest recorded `skip_solc=0`, but Morpho.bin is missing")

  rewritten_path = resolved_dir / "Morpho.rewritten.yul"
  rewrite_report_path = resolved_dir / "Morpho.rewrite-report.json"
  if require_rewrite:
    _require_nonempty_file(rewritten_path, label="rewritten artifact")
    _require_nonempty_file(rewrite_report_path, label="rewrite pipeline report")

  if rewrite_report_path.exists():
    report = _read_json(rewrite_report_path)
    expected_pipeline_manifest = _display_path(pipeline_manifest_path)
    if report.get("pipelineManifest") != expected_pipeline_manifest:
      raise RuntimeError(
        "Prepared rewrite report pipeline manifest mismatch: expected "
        f"{expected_pipeline_manifest}, got {report.get('pipelineManifest')!r}"
      )
    expected_proof_manifest = _expected_proof_manifest(proof_manifest_path)
    if report.get("proofManifest") != expected_proof_manifest:
      raise RuntimeError(
        "Prepared rewrite report proof manifest mismatch: expected "
        f"{expected_proof_manifest!r}, got {report.get('proofManifest')!r}"
      )
    if rewritten_path.exists() and rewritten_path.stat().st_size <= 0:
      raise RuntimeError(f"Prepared rewritten artifact is empty: {_display_path(rewritten_path)}")
  elif rewritten_path.exists():
    raise RuntimeError(
      "Prepared rewritten artifact is present without matching Morpho.rewrite-report.json: "
      f"{_display_path(rewritten_path)}"
    )

  return resolved_dir


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--artifact-dir",
    required=True,
    help="Prepared artifact directory or parent directory containing edsl/.",
  )
  parser.add_argument(
    "--parity-target",
    default=str(DEFAULT_PARITY_TARGET),
    help="Pinned parity target JSON used to validate the prepared bundle.",
  )
  parser.add_argument(
    "--rewrite-pipeline-manifest",
    default=str(DEFAULT_PIPELINE_MANIFEST),
    help="Rewrite pipeline manifest expected by the prepared rewrite report.",
  )
  parser.add_argument(
    "--rewrite-proof-manifest",
    default=str(DEFAULT_PROOF_MANIFEST),
    help="Rewrite proof manifest expected by the prepared rewrite report.",
  )
  parser.add_argument(
    "--allow-missing-bin",
    action="store_true",
    help="Allow Morpho.bin to be absent when the prepared bundle was built with skip_solc=1.",
  )
  parser.add_argument(
    "--require-rewrite",
    action="store_true",
    help="Require Morpho.rewritten.yul and Morpho.rewrite-report.json to be present and in sync.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  resolved_dir = validate_prepared_verity_artifact_bundle(
    pathlib.Path(args.artifact_dir).resolve(),
    require_bin=not args.allow_missing_bin,
    require_rewrite=args.require_rewrite,
    parity_target_path=pathlib.Path(args.parity_target).resolve(),
    pipeline_manifest_path=pathlib.Path(args.rewrite_pipeline_manifest).resolve(),
    proof_manifest_path=pathlib.Path(args.rewrite_proof_manifest).resolve(),
  )
  print(f"validatedPreparedArtifactDir: {_display_path(resolved_dir)}")
  print(f"requireBin: {not args.allow_missing_bin}")
  print(f"requireRewrite: {args.require_rewrite}")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
