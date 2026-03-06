#!/usr/bin/env python3
"""Unit tests for prepared Verity artifact bundle validation."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_prepared_verity_artifact_bundle import (  # noqa: E402
  validate_prepared_verity_artifact_bundle,
)


def write_bundle(
    root: pathlib.Path,
    *,
    skip_solc: str = "0",
    parity_pack: str = "test-pack",
    include_bin: bool = True,
    include_rewrite: bool = True,
    pipeline_manifest: str = "config/yul-rewrite-pipeline.json",
    proof_manifest: str | None = "config/yul-rewrite-proof-obligations.json",
) -> pathlib.Path:
  bundle = root / "edsl"
  bundle.mkdir(parents=True, exist_ok=True)
  (bundle / "Morpho.yul").write_text("raw-yul\n", encoding="utf-8")
  (bundle / "Morpho.abi.json").write_text("[]\n", encoding="utf-8")
  (bundle / "Morpho.stage-times.log").write_text("stage=rewrite-yul status=ok elapsed_sec=1\n", encoding="utf-8")
  (bundle / "Morpho.artifact-manifest.env").write_text(
    "\n".join(
      [
        "input_digest=abc123",
        "artifact_mode=edsl",
        f"skip_solc={skip_solc}",
        f"parity_pack={parity_pack}",
      ]
    )
    + "\n",
    encoding="utf-8",
  )
  if include_bin:
    (bundle / "Morpho.bin").write_text("6000\n", encoding="utf-8")
  if include_rewrite:
    (bundle / "Morpho.rewritten.yul").write_text("rewritten-yul\n", encoding="utf-8")
    (bundle / "Morpho.rewrite-report.json").write_text(
      json.dumps(
        {
          "pipelineManifest": pipeline_manifest,
          "proofManifest": proof_manifest,
          "stageCount": 1,
          "implementedStageCount": 1,
          "changedStageCount": 1,
        }
      )
      + "\n",
      encoding="utf-8",
    )
  return bundle


class CheckPreparedVerityArtifactBundleTests(unittest.TestCase):
  def test_accepts_nested_edsl_bundle(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      parity_target = root / "parity-target.json"
      parity_target.write_text('{"verity":{"parityPackId":"test-pack"}}\n', encoding="utf-8")
      pipeline_manifest = root / "pipeline.json"
      pipeline_manifest.write_text("{}\n", encoding="utf-8")
      proof_manifest = root / "proof.json"
      proof_manifest.write_text("{}\n", encoding="utf-8")
      write_bundle(
        root,
        pipeline_manifest=str(pipeline_manifest.resolve()),
        proof_manifest=str(proof_manifest.resolve()),
      )

      resolved = validate_prepared_verity_artifact_bundle(
        root,
        require_bin=True,
        require_rewrite=True,
        parity_target_path=parity_target,
        pipeline_manifest_path=pipeline_manifest,
        proof_manifest_path=proof_manifest,
      )

      self.assertEqual(resolved, root / "edsl")

  def test_rejects_parity_pack_mismatch(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      parity_target = root / "parity-target.json"
      parity_target.write_text('{"verity":{"parityPackId":"test-pack"}}\n', encoding="utf-8")
      pipeline_manifest = root / "pipeline.json"
      pipeline_manifest.write_text("{}\n", encoding="utf-8")
      proof_manifest = root / "proof.json"
      proof_manifest.write_text("{}\n", encoding="utf-8")
      write_bundle(
        root,
        parity_pack="wrong-pack",
        pipeline_manifest=str(pipeline_manifest.resolve()),
        proof_manifest=str(proof_manifest.resolve()),
      )

      with self.assertRaisesRegex(RuntimeError, "parity pack mismatch"):
        validate_prepared_verity_artifact_bundle(
          root,
          require_bin=True,
          require_rewrite=True,
          parity_target_path=parity_target,
          pipeline_manifest_path=pipeline_manifest,
          proof_manifest_path=proof_manifest,
        )

  def test_rejects_missing_bin_when_manifest_requires_solc(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      parity_target = root / "parity-target.json"
      parity_target.write_text('{"verity":{"parityPackId":"test-pack"}}\n', encoding="utf-8")
      pipeline_manifest = root / "pipeline.json"
      pipeline_manifest.write_text("{}\n", encoding="utf-8")
      proof_manifest = root / "proof.json"
      proof_manifest.write_text("{}\n", encoding="utf-8")
      write_bundle(
        root,
        include_bin=False,
        skip_solc="0",
        pipeline_manifest=str(pipeline_manifest.resolve()),
        proof_manifest=str(proof_manifest.resolve()),
      )

      with self.assertRaisesRegex(RuntimeError, "Morpho.bin"):
        validate_prepared_verity_artifact_bundle(
          root,
          require_bin=False,
          require_rewrite=True,
          parity_target_path=parity_target,
          pipeline_manifest_path=pipeline_manifest,
          proof_manifest_path=proof_manifest,
        )

  def test_rejects_rewrite_manifest_mismatch(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      parity_target = root / "parity-target.json"
      parity_target.write_text('{"verity":{"parityPackId":"test-pack"}}\n', encoding="utf-8")
      pipeline_manifest = root / "pipeline.json"
      pipeline_manifest.write_text("{}\n", encoding="utf-8")
      proof_manifest = root / "proof.json"
      proof_manifest.write_text("{}\n", encoding="utf-8")
      write_bundle(
        root,
        pipeline_manifest="config/other.json",
        proof_manifest=str(proof_manifest.resolve()),
      )

      with self.assertRaisesRegex(RuntimeError, "pipeline manifest mismatch"):
        validate_prepared_verity_artifact_bundle(
          root,
          require_bin=True,
          require_rewrite=True,
          parity_target_path=parity_target,
          pipeline_manifest_path=pipeline_manifest,
          proof_manifest_path=proof_manifest,
        )

  def test_rejects_rewritten_yul_without_report(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      parity_target = root / "parity-target.json"
      parity_target.write_text('{"verity":{"parityPackId":"test-pack"}}\n', encoding="utf-8")
      pipeline_manifest = root / "pipeline.json"
      pipeline_manifest.write_text("{}\n", encoding="utf-8")
      proof_manifest = root / "proof.json"
      proof_manifest.write_text("{}\n", encoding="utf-8")
      bundle = write_bundle(
        root,
        include_rewrite=False,
        pipeline_manifest=str(pipeline_manifest.resolve()),
        proof_manifest=str(proof_manifest.resolve()),
      )
      (bundle / "Morpho.rewritten.yul").write_text("rewritten-yul\n", encoding="utf-8")

      with self.assertRaisesRegex(RuntimeError, "rewrite-report"):
        validate_prepared_verity_artifact_bundle(
          root,
          require_bin=True,
          require_rewrite=False,
          parity_target_path=parity_target,
          pipeline_manifest_path=pipeline_manifest,
          proof_manifest_path=proof_manifest,
        )


if __name__ == "__main__":
  unittest.main()
