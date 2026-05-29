#!/usr/bin/env python3
"""Apply the configured Yul rewrite pipeline to a Verity Yul artifact."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import sys
from typing import Any

ROOT = pathlib.Path(__file__).resolve().parent.parent
DEFAULT_PIPELINE_MANIFEST = ROOT / "config" / "yul-rewrite-pipeline.json"
DEFAULT_PROOF_MANIFEST = ROOT / "config" / "yul-rewrite-proof-obligations.json"


class RewritePipelineError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    with path.open("r", encoding="utf-8") as f:
      return f.read()
  except UnicodeDecodeError as exc:
    raise RewritePipelineError(f"failed to decode UTF-8 text file {display_path(path)}") from exc
  except OSError as exc:
    raise RewritePipelineError(f"failed to read text file {display_path(path)}: {exc}") from exc


def read_json(path: pathlib.Path) -> Any:
  try:
    with path.open("r", encoding="utf-8") as f:
      return json.load(f)
  except json.JSONDecodeError as exc:
    raise RewritePipelineError(f"invalid JSON in {display_path(path)}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise RewritePipelineError(f"failed to decode UTF-8 JSON file {display_path(path)}") from exc
  except OSError as exc:
    raise RewritePipelineError(f"failed to read JSON file {display_path(path)}: {exc}") from exc


def write_text(path: pathlib.Path, content: str) -> None:
  try:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
      f.write(content)
  except OSError as exc:
    raise RewritePipelineError(f"failed to write text file {display_path(path)}: {exc}") from exc


def sha256_text(text: str) -> str:
  return hashlib.sha256(text.encode("utf-8")).hexdigest()


def sha256_file(path: pathlib.Path) -> str:
  return sha256_text(read_text(path))


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def load_rewrite_pipeline_manifest(path: pathlib.Path) -> dict[str, Any]:
  data = read_json(path)
  if not isinstance(data, dict):
    raise RewritePipelineError(
      f"rewrite pipeline manifest must be a JSON object: {display_path(path)}"
    )

  version = data.get("version")
  if not isinstance(version, str) or not version.strip():
      raise RewritePipelineError(
        f"rewrite pipeline manifest key `version` must be a non-empty string: {display_path(path)}"
      )

  stages = data.get("stages")
  if not isinstance(stages, list) or not stages:
      raise RewritePipelineError(
        f"rewrite pipeline manifest key `stages` must be a non-empty list: {display_path(path)}"
      )

  normalized_stages: list[dict[str, Any]] = []
  seen_passes: set[str] = set()
  for index, stage in enumerate(stages):
    where = f"stages[{index}]"
    if not isinstance(stage, dict):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must be an object: {display_path(path)}"
        )
    rewrite_pass = stage.get("rewritePass")
    if not isinstance(rewrite_pass, str) or not rewrite_pass.strip():
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must include non-empty `rewritePass`: {display_path(path)}"
        )
    if rewrite_pass in seen_passes:
        raise RewritePipelineError(
          f"rewrite pipeline manifest defines duplicate rewritePass `{rewrite_pass}`: {display_path(path)}"
        )
    seen_passes.add(rewrite_pass)

    families = stage.get("families")
    if not isinstance(families, list) or not families:
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must include non-empty `families`: {display_path(path)}"
        )
    if not all(isinstance(family, str) and family.strip() for family in families):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must use non-empty strings in `families`: {display_path(path)}"
        )
    if len(families) != len(set(families)):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` contains duplicate `families`: {display_path(path)}"
        )

    proof_refs = stage.get("proofRefs")
    if not isinstance(proof_refs, list) or not proof_refs:
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must include non-empty `proofRefs`: {display_path(path)}"
        )
    if not all(isinstance(proof_ref, str) and proof_ref.strip() for proof_ref in proof_refs):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must use non-empty strings in `proofRefs`: {display_path(path)}"
        )
    if len(proof_refs) != len(set(proof_refs)):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` contains duplicate `proofRefs`: {display_path(path)}"
        )

    implemented = stage.get("implemented", False)
    if not isinstance(implemented, bool):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` must use boolean `implemented`: {display_path(path)}"
        )

    notes = stage.get("notes")
    if notes is not None and not isinstance(notes, str):
        raise RewritePipelineError(
          f"rewrite pipeline entry `{where}` has non-string `notes`: {display_path(path)}"
        )

    normalized = {
      "rewritePass": rewrite_pass,
      "families": families,
      "proofRefs": proof_refs,
      "implemented": implemented,
    }
    if notes is not None:
      normalized["notes"] = notes
    normalized_stages.append(normalized)

  return {"version": version, "stages": normalized_stages}


def _extract_manifest_proof_plans(manifest: dict[str, Any]) -> dict[str, dict[str, str]]:
  defaults = manifest.get("defaults", {})
  if defaults is not None and not isinstance(defaults, dict):
    raise RewritePipelineError("rewrite proof manifest key `defaults` must be an object")
  families = manifest.get("families", [])
  if not isinstance(families, list):
    raise RewritePipelineError("rewrite proof manifest key `families` must be a list")

  plans: dict[str, dict[str, str]] = {}

  def add_plan(family: str, entry: dict[str, Any], where: str) -> None:
    rewrite_pass = entry.get("rewritePass")
    proof_refs = entry.get("proofRefs", [])
    if not isinstance(rewrite_pass, str) or not rewrite_pass.strip():
      raise RewritePipelineError(f"rewrite proof manifest entry `{where}` must include non-empty `rewritePass`")
    if not isinstance(proof_refs, list) or not all(isinstance(ref, str) and ref.strip() for ref in proof_refs):
      raise RewritePipelineError(f"rewrite proof manifest entry `{where}` must include string `proofRefs`")
    for proof_ref in proof_refs:
      if proof_ref in plans:
        raise RewritePipelineError(f"rewrite proof manifest proof ref `{proof_ref}` is declared more than once")
      plans[proof_ref] = {"family": family, "rewritePass": rewrite_pass}

  for family, entry in defaults.items():
    if not isinstance(family, str) or not family.strip():
      raise RewritePipelineError("rewrite proof manifest default family names must be non-empty strings")
    if not isinstance(entry, dict):
      raise RewritePipelineError(f"rewrite proof manifest default `{family}` must be an object")
    add_plan(family, entry, f"defaults.{family}")

  seen_families: set[str] = set()
  for index, entry in enumerate(families):
    where = f"families[{index}]"
    if not isinstance(entry, dict):
      raise RewritePipelineError(f"rewrite proof manifest entry `{where}` must be an object")
    family = entry.get("family")
    if not isinstance(family, str) or not family.strip():
      raise RewritePipelineError(f"rewrite proof manifest entry `{where}` must include non-empty `family`")
    if family in seen_families:
      raise RewritePipelineError(f"rewrite proof manifest defines duplicate family `{family}`")
    seen_families.add(family)
    add_plan(family, entry, where)

  return plans


def load_rewrite_proof_manifest(path: pathlib.Path) -> dict[str, Any]:
  data = read_json(path)
  if not isinstance(data, dict):
    raise RewritePipelineError(
      f"rewrite proof manifest must be a JSON object: {display_path(path)}"
    )
  try:
    _extract_manifest_proof_plans(data)
  except RewritePipelineError as exc:
    raise RewritePipelineError(f"invalid rewrite proof manifest {display_path(path)}: {exc}") from exc
  return data


def validate_pipeline_against_proof_manifest(
    pipeline_manifest: dict[str, Any], proof_manifest: dict[str, Any]
) -> dict[str, dict[str, str]]:
  manifest_plans = _extract_manifest_proof_plans(proof_manifest)
  proof_passes = {plan["rewritePass"] for plan in manifest_plans.values()}
  pipeline_passes = {stage["rewritePass"] for stage in pipeline_manifest["stages"]}

  missing_passes = sorted(proof_passes - pipeline_passes)
  extra_passes = sorted(pipeline_passes - proof_passes)
  if missing_passes:
    raise RewritePipelineError(
      "rewrite pipeline is missing passes tracked by the proof manifest: "
      + ", ".join(missing_passes)
    )
  if extra_passes:
    raise RewritePipelineError(
      "rewrite pipeline declares passes not tracked by the proof manifest: "
      + ", ".join(extra_passes)
    )

  for stage in pipeline_manifest["stages"]:
    stage_pass = stage["rewritePass"]
    stage_families = set(stage["families"])
    for proof_ref in stage["proofRefs"]:
      if proof_ref not in manifest_plans:
        raise RewritePipelineError(
          f"rewrite pipeline proof ref `{proof_ref}` is not tracked by the proof manifest"
        )
      plan = manifest_plans[proof_ref]
      if plan["rewritePass"] != stage_pass:
        raise RewritePipelineError(
          f"rewrite pipeline stage `{stage_pass}` references proof ref `{proof_ref}` "
          f"with manifest rewritePass `{plan['rewritePass']}`"
        )
      if plan["family"] not in stage_families:
        raise RewritePipelineError(
          f"rewrite pipeline stage `{stage_pass}` omits family `{plan['family']}` "
          f"required by proof ref `{proof_ref}`"
        )

  return manifest_plans


def apply_rewrite_pipeline_text(
    input_text: str, pipeline_manifest: dict[str, Any]
) -> tuple[str, dict[str, Any]]:
  current_text = input_text
  stage_reports: list[dict[str, Any]] = []

  for stage in pipeline_manifest["stages"]:
    input_sha = sha256_text(current_text)
    output_text = current_text
    output_sha = sha256_text(output_text)
    stage_reports.append(
      {
        "rewritePass": stage["rewritePass"],
        "families": list(stage["families"]),
        "proofRefs": list(stage["proofRefs"]),
        "implemented": stage["implemented"],
        "changed": input_sha != output_sha,
        "inputSha256": input_sha,
        "outputSha256": output_sha,
        **({"notes": stage["notes"]} if "notes" in stage else {}),
      }
    )
    current_text = output_text

  report = {
    "version": pipeline_manifest["version"],
    "stageCount": len(stage_reports),
    "implementedStageCount": sum(1 for stage in stage_reports if stage["implemented"]),
    "changedStageCount": sum(1 for stage in stage_reports if stage["changed"]),
    "stages": stage_reports,
  }
  return current_text, report


def apply_rewrite_pipeline_to_file(
    input_path: pathlib.Path,
    output_path: pathlib.Path,
    *,
    pipeline_manifest_path: pathlib.Path = DEFAULT_PIPELINE_MANIFEST,
    proof_manifest_path: pathlib.Path | None = DEFAULT_PROOF_MANIFEST,
    json_out: pathlib.Path | None = None,
) -> dict[str, Any]:
  input_path = input_path.resolve()
  output_path = output_path.resolve()
  pipeline_manifest_path = pipeline_manifest_path.resolve()
  if proof_manifest_path is not None:
    proof_manifest_path = proof_manifest_path.resolve()
  if json_out is not None:
    json_out = json_out.resolve()

  pipeline_manifest = load_rewrite_pipeline_manifest(pipeline_manifest_path)
  if proof_manifest_path is not None:
    proof_manifest = load_rewrite_proof_manifest(proof_manifest_path)
    validate_pipeline_against_proof_manifest(pipeline_manifest, proof_manifest)
  input_text = read_text(input_path)
  rewritten_text, report = apply_rewrite_pipeline_text(input_text, pipeline_manifest)
  write_text(output_path, rewritten_text)

  full_report = {
    "pipelineManifestPath": display_path(pipeline_manifest_path),
    "pipelineManifestSha256": sha256_file(pipeline_manifest_path),
    "proofManifestPath": display_path(proof_manifest_path) if proof_manifest_path is not None else None,
    "proofManifestSha256": sha256_file(proof_manifest_path) if proof_manifest_path is not None else None,
    "inputPath": display_path(input_path),
    "outputPath": display_path(output_path),
    "inputSha256": sha256_text(input_text),
    "outputSha256": sha256_text(rewritten_text),
    **report,
  }
  if json_out is not None:
    write_text(json_out, json.dumps(full_report, indent=2) + "\n")
  return full_report


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--input", required=True, help="Path to raw Verity Yul input")
  parser.add_argument("--output", required=True, help="Path to rewritten Verity Yul output")
  parser.add_argument(
    "--pipeline-manifest",
    default=str(DEFAULT_PIPELINE_MANIFEST),
    help="JSON manifest defining the rewrite stage order.",
  )
  parser.add_argument(
    "--proof-manifest",
    default=str(DEFAULT_PROOF_MANIFEST),
    help="Legacy rewrite metadata manifest. This no longer corresponds to Lean proof files.",
  )
  parser.add_argument(
    "--json-out",
    help="Optional JSON report path describing the applied rewrite pipeline.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  input_path = pathlib.Path(args.input).resolve()
  output_path = pathlib.Path(args.output).resolve()
  pipeline_manifest_path = pathlib.Path(args.pipeline_manifest).resolve()
  proof_manifest_path = pathlib.Path(args.proof_manifest).resolve()
  json_out = pathlib.Path(args.json_out).resolve() if args.json_out else None

  try:
    report = apply_rewrite_pipeline_to_file(
      input_path,
      output_path,
      pipeline_manifest_path=pipeline_manifest_path,
      proof_manifest_path=proof_manifest_path,
      json_out=json_out,
    )
  except RewritePipelineError as exc:
    print(f"yul-rewrite-pipeline failed: {exc}", file=sys.stderr)
    return 1

  print(f"rewritePipeline: {report['pipelineManifestPath']}")
  print(f"rewriteProofManifest: {report['proofManifestPath']}")
  print(f"rewritePipelineStages: {report['stageCount']}")
  print(f"rewritePipelineImplementedStages: {report['implementedStageCount']}")
  print(f"rewritePipelineChangedStages: {report['changedStageCount']}")
  print(f"rewrittenYul: {report['outputPath']}")
  if json_out is not None:
    print(f"rewritePipelineReport: {display_path(json_out)}")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
