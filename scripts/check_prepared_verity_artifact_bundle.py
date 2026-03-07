#!/usr/bin/env python3
"""Fail-closed validation for reusable prepared Verity artifact bundles."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
DEFAULT_PARITY_TARGET = ROOT / "config" / "parity-target.json"
DEFAULT_PIPELINE_MANIFEST = ROOT / "config" / "yul-rewrite-pipeline.json"
DEFAULT_PROOF_MANIFEST = ROOT / "config" / "yul-rewrite-proof-obligations.json"


class PreparedArtifactBundleError(RuntimeError):
  """Raised when a prepared Verity artifact bundle is malformed or out of sync."""


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
  try:
    with path.open("r", encoding="utf-8") as handle:
      data = json.load(handle)
  except json.JSONDecodeError as exc:
    raise PreparedArtifactBundleError(
      f"Invalid JSON in {_display_path(path)}: {exc.msg}"
    ) from exc
  except UnicodeDecodeError as exc:
    raise PreparedArtifactBundleError(
      f"Failed to decode JSON file {_display_path(path)} as UTF-8"
    ) from exc
  except OSError as exc:
    raise PreparedArtifactBundleError(
      f"Failed to read JSON file {_display_path(path)}: {exc}"
    ) from exc
  if not isinstance(data, dict):
    raise PreparedArtifactBundleError(f"Expected JSON object in {_display_path(path)}")
  return data


def _sha256_text(text: str) -> str:
  return hashlib.sha256(text.encode("utf-8")).hexdigest()


def _sha256_file(path: pathlib.Path) -> str:
  try:
    return _sha256_text(path.read_text(encoding="utf-8"))
  except UnicodeDecodeError as exc:
    raise PreparedArtifactBundleError(
      f"Failed to decode file {_display_path(path)} as UTF-8"
    ) from exc
  except OSError as exc:
    raise PreparedArtifactBundleError(f"Failed to read file {_display_path(path)}: {exc}") from exc


def _require_nonempty_file(path: pathlib.Path, *, label: str) -> None:
  if not path.is_file() or path.stat().st_size <= 0:
    raise PreparedArtifactBundleError(f"Missing required non-empty {label}: {_display_path(path)}")


def _parse_manifest(path: pathlib.Path) -> dict[str, str]:
  _require_nonempty_file(path, label="artifact manifest")
  parsed: dict[str, str] = {}
  try:
    manifest_text = path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise PreparedArtifactBundleError(
      f"Failed to decode artifact manifest {_display_path(path)} as UTF-8"
    ) from exc
  except OSError as exc:
    raise PreparedArtifactBundleError(
      f"Failed to read artifact manifest {_display_path(path)}: {exc}"
    ) from exc
  for raw_line in manifest_text.splitlines():
    line = raw_line.strip()
    if not line:
      continue
    if "=" not in line:
      raise PreparedArtifactBundleError(
        f"Malformed artifact manifest line in {_display_path(path)}: {raw_line!r}"
      )
    key, value = line.split("=", 1)
    if not key:
      raise PreparedArtifactBundleError(
        f"Malformed artifact manifest line in {_display_path(path)}: {raw_line!r}"
      )
    if key in parsed:
      raise PreparedArtifactBundleError(
        f"Duplicate artifact manifest key `{key}` in {_display_path(path)}"
      )
    parsed[key] = value
  return parsed


def _require_string_field(report: dict[str, Any], path: pathlib.Path, key: str) -> str:
  value = report.get(key)
  if not isinstance(value, str) or not value.strip():
    raise PreparedArtifactBundleError(
      f"Prepared rewrite report requires non-empty string `{key}` in {_display_path(path)}"
    )
  return value


def _require_optional_string_field(report: dict[str, Any], path: pathlib.Path, key: str) -> str | None:
  value = report.get(key)
  if value is None:
    return None
  if not isinstance(value, str) or not value.strip():
    raise PreparedArtifactBundleError(
      f"Prepared rewrite report field `{key}` must be a non-empty string or null in {_display_path(path)}"
    )
  return value


def _required_parity_pack(path: pathlib.Path) -> str:
  data = _read_json(path)
  verity = data.get("verity")
  if not isinstance(verity, dict):
    raise PreparedArtifactBundleError(f"Missing object key `verity` in {_display_path(path)}")
  parity_pack = verity.get("parityPackId")
  if not isinstance(parity_pack, str) or not parity_pack.strip():
    raise PreparedArtifactBundleError(f"Missing required `verity.parityPackId` in {_display_path(path)}")
  return parity_pack


def _expected_proof_manifest(proof_manifest_path: pathlib.Path) -> str | None:
  if not proof_manifest_path.exists():
    return None
  return _display_path(proof_manifest_path)


def _expected_manifest_sha256(path: pathlib.Path) -> str | None:
  if not path.exists():
    return None
  return _sha256_file(path)


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
    raise PreparedArtifactBundleError(
      f"Prepared artifact directory does not exist: {_display_path(resolved_dir)}"
    )

  _require_nonempty_file(resolved_dir / "Morpho.yul", label="artifact")
  _require_nonempty_file(resolved_dir / "Morpho.abi.json", label="artifact")
  _require_nonempty_file(resolved_dir / "Morpho.stage-times.log", label="stage timing log")

  manifest = _parse_manifest(resolved_dir / "Morpho.artifact-manifest.env")
  input_digest = manifest.get("input_digest", "").strip()
  artifact_mode = manifest.get("artifact_mode", "").strip()
  skip_solc = manifest.get("skip_solc", "").strip()
  parity_pack = manifest.get("parity_pack", "").strip()

  if not input_digest:
    raise PreparedArtifactBundleError(
      f"Prepared artifact manifest is missing non-empty `input_digest`: "
      f"{_display_path(resolved_dir / 'Morpho.artifact-manifest.env')}"
    )
  if artifact_mode != "edsl":
    raise PreparedArtifactBundleError(
      f"Prepared artifact manifest must pin `artifact_mode=edsl` "
      f"(got {artifact_mode or '<missing>'})"
    )
  if skip_solc not in {"0", "1"}:
    raise PreparedArtifactBundleError(
      f"Prepared artifact manifest must pin `skip_solc` to 0 or 1 "
      f"(got {skip_solc or '<missing>'})"
    )

  expected_parity_pack = _required_parity_pack(parity_target_path)
  if parity_pack != expected_parity_pack:
    raise PreparedArtifactBundleError(
      f"Prepared artifact manifest parity pack mismatch: expected {expected_parity_pack}, got "
      f"{parity_pack or '<missing>'}"
    )

  bin_path = resolved_dir / "Morpho.bin"
  if require_bin:
    _require_nonempty_file(bin_path, label="artifact")
    if skip_solc != "0":
      raise PreparedArtifactBundleError(
        "Prepared artifact bundle requires Morpho.bin, so manifest must record `skip_solc=0`"
      )
  else:
    if bin_path.exists() and bin_path.stat().st_size <= 0:
      raise PreparedArtifactBundleError(f"Prepared artifact file is empty: {_display_path(bin_path)}")
    if bin_path.exists() and skip_solc == "1":
      raise PreparedArtifactBundleError(
        "Prepared artifact manifest recorded `skip_solc=1`, but Morpho.bin is present"
      )
    if not bin_path.exists() and skip_solc == "0":
      raise PreparedArtifactBundleError(
        "Prepared artifact manifest recorded `skip_solc=0`, but Morpho.bin is missing"
      )

  rewritten_path = resolved_dir / "Morpho.rewritten.yul"
  rewrite_report_path = resolved_dir / "Morpho.rewrite-report.json"
  if require_rewrite:
    _require_nonempty_file(rewritten_path, label="rewritten artifact")
    _require_nonempty_file(rewrite_report_path, label="rewrite pipeline report")

  if rewrite_report_path.exists():
    report = _read_json(rewrite_report_path)
    expected_pipeline_manifest = _display_path(pipeline_manifest_path)
    pipeline_manifest = _require_string_field(report, rewrite_report_path, "pipelineManifest")
    if pipeline_manifest != expected_pipeline_manifest:
      raise PreparedArtifactBundleError(
        "Prepared rewrite report pipeline manifest mismatch: expected "
        f"{expected_pipeline_manifest}, got {pipeline_manifest!r}"
      )
    expected_pipeline_sha256 = _expected_manifest_sha256(pipeline_manifest_path)
    pipeline_manifest_sha256 = _require_optional_string_field(
      report, rewrite_report_path, "pipelineManifestSha256"
    )
    if pipeline_manifest_sha256 != expected_pipeline_sha256:
      raise PreparedArtifactBundleError(
        "Prepared rewrite report pipeline manifest digest mismatch: expected "
        f"{expected_pipeline_sha256!r}, got {pipeline_manifest_sha256!r}"
      )
    expected_proof_manifest = _expected_proof_manifest(proof_manifest_path)
    proof_manifest = _require_optional_string_field(report, rewrite_report_path, "proofManifest")
    if proof_manifest != expected_proof_manifest:
      raise PreparedArtifactBundleError(
        "Prepared rewrite report proof manifest mismatch: expected "
        f"{expected_proof_manifest!r}, got {proof_manifest!r}"
      )
    expected_proof_sha256 = _expected_manifest_sha256(proof_manifest_path)
    proof_manifest_sha256 = _require_optional_string_field(
      report, rewrite_report_path, "proofManifestSha256"
    )
    if proof_manifest_sha256 != expected_proof_sha256:
      raise PreparedArtifactBundleError(
        "Prepared rewrite report proof manifest digest mismatch: expected "
        f"{expected_proof_sha256!r}, got {proof_manifest_sha256!r}"
      )
    if rewritten_path.exists() and rewritten_path.stat().st_size <= 0:
      raise PreparedArtifactBundleError(
        f"Prepared rewritten artifact is empty: {_display_path(rewritten_path)}"
      )
  elif rewritten_path.exists():
    raise PreparedArtifactBundleError(
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
  try:
    raise SystemExit(main())
  except PreparedArtifactBundleError as exc:
    print(f"prepared-verity-artifact-bundle check failed: {exc}", file=sys.stderr)
    raise SystemExit(1)
