#!/usr/bin/env python3
"""Apply the configured Yul rewrite pipeline to a Verity Yul artifact."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import sys
from typing import Any

from check_yul_rewrite_proof_obligations import (
  RewriteProofError,
  extract_manifest_proof_plans,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
DEFAULT_PIPELINE_MANIFEST = ROOT / "config" / "yul-rewrite-pipeline.json"
DEFAULT_PROOF_MANIFEST = ROOT / "config" / "yul-rewrite-proof-obligations.json"


class RewritePipelineError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


def read_json(path: pathlib.Path) -> Any:
  try:
    with path.open("r", encoding="utf-8") as f:
      return json.load(f)
  except json.JSONDecodeError as exc:
    raise RewritePipelineError(f"invalid JSON in {path}: {exc}") from exc


def write_text(path: pathlib.Path, content: str) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  with path.open("w", encoding="utf-8") as f:
    f.write(content)


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
    raise RewritePipelineError(f"rewrite pipeline manifest must be a JSON object: {path}")

  version = data.get("version")
  if not isinstance(version, str) or not version.strip():
    raise RewritePipelineError(
      f"rewrite pipeline manifest key `version` must be a non-empty string: {path}"
    )

  stages = data.get("stages")
  if not isinstance(stages, list) or not stages:
    raise RewritePipelineError(
      f"rewrite pipeline manifest key `stages` must be a non-empty list: {path}"
    )

  normalized_stages: list[dict[str, Any]] = []
  seen_passes: set[str] = set()
  for index, stage in enumerate(stages):
    where = f"stages[{index}]"
    if not isinstance(stage, dict):
      raise RewritePipelineError(f"rewrite pipeline entry `{where}` must be an object: {path}")
    rewrite_pass = stage.get("rewritePass")
    if not isinstance(rewrite_pass, str) or not rewrite_pass.strip():
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must include non-empty `rewritePass`: {path}"
      )
    if rewrite_pass in seen_passes:
      raise RewritePipelineError(
        f"rewrite pipeline manifest defines duplicate rewritePass `{rewrite_pass}`: {path}"
      )
    seen_passes.add(rewrite_pass)

    families = stage.get("families")
    if not isinstance(families, list) or not families:
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must include non-empty `families`: {path}"
      )
    if not all(isinstance(family, str) and family.strip() for family in families):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must use non-empty strings in `families`: {path}"
      )
    if len(families) != len(set(families)):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` contains duplicate `families`: {path}"
      )

    proof_refs = stage.get("proofRefs")
    if not isinstance(proof_refs, list) or not proof_refs:
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must include non-empty `proofRefs`: {path}"
      )
    if not all(isinstance(proof_ref, str) and proof_ref.strip() for proof_ref in proof_refs):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must use non-empty strings in `proofRefs`: {path}"
      )
    if len(proof_refs) != len(set(proof_refs)):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` contains duplicate `proofRefs`: {path}"
      )

    implemented = stage.get("implemented", False)
    if not isinstance(implemented, bool):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` must use boolean `implemented`: {path}"
      )

    notes = stage.get("notes")
    if notes is not None and not isinstance(notes, str):
      raise RewritePipelineError(
        f"rewrite pipeline entry `{where}` has non-string `notes`: {path}"
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


def load_rewrite_proof_manifest(path: pathlib.Path) -> dict[str, Any]:
  data = read_json(path)
  if not isinstance(data, dict):
    raise RewritePipelineError(f"rewrite proof manifest must be a JSON object: {path}")
  try:
    extract_manifest_proof_plans(data)
  except RewriteProofError as exc:
    raise RewritePipelineError(f"invalid rewrite proof manifest {path}: {exc}") from exc
  return data


def validate_pipeline_against_proof_manifest(
    pipeline_manifest: dict[str, Any], proof_manifest: dict[str, Any]
) -> dict[str, dict[str, str]]:
  manifest_plans = extract_manifest_proof_plans(proof_manifest)
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

    unmatched_families = [
      family
      for family in stage["families"]
      if not any(
        proof_ref in manifest_plans
        and manifest_plans[proof_ref]["family"] == family
        and manifest_plans[proof_ref]["rewritePass"] == stage_pass
        for proof_ref in stage["proofRefs"]
      )
    ]
    if unmatched_families:
      raise RewritePipelineError(
        f"rewrite pipeline stage `{stage_pass}` lists families without matching proof refs: "
        + ", ".join(sorted(unmatched_families))
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
  pipeline_manifest = load_rewrite_pipeline_manifest(pipeline_manifest_path)
  if proof_manifest_path is not None:
    proof_manifest = load_rewrite_proof_manifest(proof_manifest_path)
    validate_pipeline_against_proof_manifest(pipeline_manifest, proof_manifest)

  input_text = read_text(input_path)
  rewritten_text, report = apply_rewrite_pipeline_text(input_text, pipeline_manifest)
  write_text(output_path, rewritten_text)

  full_report = {
    "pipelineManifest": display_path(pipeline_manifest_path),
    "pipelineManifestSha256": sha256_file(pipeline_manifest_path),
    "proofManifest": display_path(proof_manifest_path) if proof_manifest_path is not None else None,
    "proofManifestSha256": sha256_file(proof_manifest_path) if proof_manifest_path is not None else None,
    "input": display_path(input_path),
    "output": display_path(output_path),
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
    help="JSON manifest defining rewrite-proof obligations.",
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

  print(f"rewritePipeline: {report['pipelineManifest']}")
  print(f"rewriteProofManifest: {report['proofManifest']}")
  print(f"rewritePipelineStages: {report['stageCount']}")
  print(f"rewritePipelineImplementedStages: {report['implementedStageCount']}")
  print(f"rewritePipelineChangedStages: {report['changedStageCount']}")
  print(f"rewrittenYul: {report['output']}")
  if json_out is not None:
    print(f"rewritePipelineReport: {display_path(json_out)}")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
