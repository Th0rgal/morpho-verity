#!/usr/bin/env python3
"""Unit tests for the Yul rewrite pipeline helper."""

from __future__ import annotations

import json
import hashlib
import pathlib
import subprocess
import tempfile
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from apply_yul_rewrite_pipeline import (  # noqa: E402
  RewritePipelineError,
  apply_rewrite_pipeline_text,
  apply_rewrite_pipeline_to_file,
  load_rewrite_pipeline_manifest,
  load_rewrite_proof_manifest,
  validate_pipeline_against_proof_manifest,
)


class ApplyYulRewritePipelineTests(unittest.TestCase):
  def test_load_rewrite_pipeline_manifest_validates_schema(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "pipeline.json"
      path.write_text(
        """{
  "version": "rewrite-pipeline-v1",
  "stages": [
    {
      "rewritePass": "rename-normalization",
      "families": ["renameOnly"],
      "proofRefs": ["rewrite.rename_only.alpha_equiv"],
      "implemented": false
    }
  ]
}
""",
        encoding="utf-8",
      )
      manifest = load_rewrite_pipeline_manifest(path)
    self.assertEqual(manifest["version"], "rewrite-pipeline-v1")
    self.assertEqual(manifest["stages"][0]["rewritePass"], "rename-normalization")

  def test_load_rewrite_pipeline_manifest_rejects_duplicate_rewrite_pass(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "pipeline.json"
      path.write_text(
        """{
  "version": "rewrite-pipeline-v1",
  "stages": [
    {
      "rewritePass": "rename-normalization",
      "families": ["renameOnly"],
      "proofRefs": ["rewrite.rename_only.alpha_equiv"]
    },
    {
      "rewritePass": "rename-normalization",
      "families": ["renameAmbiguous"],
      "proofRefs": ["rewrite.rename_ambiguous.disambiguate"]
    }
  ]
}
""",
        encoding="utf-8",
      )
      with self.assertRaisesRegex(RuntimeError, "duplicate rewritePass `rename-normalization`"):
        load_rewrite_pipeline_manifest(path)

  def test_load_rewrite_pipeline_manifest_rejects_invalid_json(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "pipeline.json"
      path.write_text("{not json", encoding="utf-8")
      with self.assertRaisesRegex(RuntimeError, "invalid JSON in"):
        load_rewrite_pipeline_manifest(path)

  def test_load_rewrite_pipeline_manifest_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "pipeline.json"
      path.write_bytes(b"\xff\xfe")
      with self.assertRaisesRegex(RewritePipelineError, "failed to decode UTF-8 JSON file"):
        load_rewrite_pipeline_manifest(path)

  def test_load_rewrite_proof_manifest_rejects_non_object_root(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "proof.json"
      path.write_text("[]", encoding="utf-8")
      with self.assertRaisesRegex(RuntimeError, "rewrite proof manifest must be a JSON object"):
        load_rewrite_proof_manifest(path)

  def test_load_rewrite_proof_manifest_wraps_schema_errors(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "proof.json"
      path.write_text(
        json.dumps(
          {
            "defaults": [],
            "families": [],
          }
        ),
        encoding="utf-8",
      )
      with self.assertRaisesRegex(RuntimeError, "invalid rewrite proof manifest"):
        load_rewrite_proof_manifest(path)

  def test_apply_rewrite_pipeline_to_file_wraps_json_report_write_failure(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"
      proof_path = tmp / "proof.json"
      json_out = tmp / "occupied" / "report.json"

      input_path.write_text('object "M" { code { } }\n', encoding="utf-8")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )
      proof_path.write_text(
        json.dumps(
          {
            "defaults": {
              "renameOnly": {
                "rewritePass": "rename-normalization",
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
              }
            },
            "families": [],
          }
        ),
        encoding="utf-8",
      )
      json_out.parent.write_text("not a directory", encoding="utf-8")

      with self.assertRaisesRegex(RewritePipelineError, "failed to write text file"):
        apply_rewrite_pipeline_to_file(
          input_path,
          output_path,
          pipeline_manifest_path=pipeline_path,
          proof_manifest_path=proof_path,
          json_out=json_out,
        )

  def test_validate_pipeline_against_proof_manifest_rejects_missing_pass(self) -> None:
    pipeline = {
      "version": "rewrite-pipeline-v1",
      "stages": [
        {
          "rewritePass": "rename-normalization",
          "families": ["renameOnly"],
          "proofRefs": ["rewrite.rename_only.alpha_equiv"],
          "implemented": False,
        }
      ],
    }
    proof_manifest = {
      "defaults": {
        "renameOnly": {
          "rewritePass": "rename-normalization",
          "proofRefs": ["rewrite.rename_only.alpha_equiv"],
        }
      },
      "families": [
        {
          "family": "checked_add",
          "rewritePass": "checked-arith-width-alignment",
          "proofRefs": ["rewrite.checked_add.width_alignment"],
        }
      ],
    }
    with self.assertRaisesRegex(RuntimeError, "missing passes tracked by the proof manifest"):
      validate_pipeline_against_proof_manifest(pipeline, proof_manifest)

  def test_apply_rewrite_pipeline_text_reports_noop_stages(self) -> None:
    rewritten, report = apply_rewrite_pipeline_text(
      'object "M" { code { } }\n',
      {
        "version": "rewrite-pipeline-v1",
        "stages": [
          {
            "rewritePass": "rename-normalization",
            "families": ["renameOnly"],
            "proofRefs": ["rewrite.rename_only.alpha_equiv"],
            "implemented": False,
          }
        ],
      },
    )
    self.assertEqual(rewritten, 'object "M" { code { } }\n')
    self.assertEqual(report["stageCount"], 1)
    self.assertEqual(report["implementedStageCount"], 0)
    self.assertEqual(report["changedStageCount"], 0)
    self.assertFalse(report["stages"][0]["changed"])

  def test_apply_rewrite_pipeline_to_file_writes_output_and_json_report(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"
      proof_path = tmp / "proof.json"
      json_out = tmp / "report.json"

      input_path.write_text('object "M" { code { } }\n', encoding="utf-8")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )
      proof_path.write_text(
        json.dumps(
          {
            "defaults": {
              "renameOnly": {
                "rewritePass": "rename-normalization",
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
              }
            },
            "families": [],
          }
        ),
        encoding="utf-8",
      )

      report = apply_rewrite_pipeline_to_file(
        input_path,
        output_path,
        pipeline_manifest_path=pipeline_path,
        proof_manifest_path=proof_path,
        json_out=json_out,
      )

      self.assertEqual(output_path.read_text(encoding="utf-8"), input_path.read_text(encoding="utf-8"))
      self.assertEqual(report["stageCount"], 1)
      self.assertTrue(json_out.is_file())
      json_report = json.loads(json_out.read_text(encoding="utf-8"))
      self.assertEqual(json_report["output"], str(output_path))
      self.assertEqual(
        json_report["pipelineManifestSha256"],
        hashlib.sha256(pipeline_path.read_text(encoding="utf-8").encode("utf-8")).hexdigest(),
      )
      self.assertEqual(
        json_report["proofManifestSha256"],
        hashlib.sha256(proof_path.read_text(encoding="utf-8").encode("utf-8")).hexdigest(),
      )

  def test_apply_rewrite_pipeline_to_file_allows_missing_proof_manifest(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"

      input_path.write_text('object "M" { code { } }\n', encoding="utf-8")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )

      report = apply_rewrite_pipeline_to_file(
        input_path,
        output_path,
        pipeline_manifest_path=pipeline_path,
        proof_manifest_path=None,
      )

      self.assertEqual(output_path.read_text(encoding="utf-8"), input_path.read_text(encoding="utf-8"))
      self.assertIsNone(report["proofManifest"])
      self.assertIsNone(report["proofManifestSha256"])

  def test_cli_rejects_invalid_proof_manifest_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"
      proof_path = tmp / "proof.json"

      input_path.write_text('object "M" { code { } }\n', encoding="utf-8")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )
      proof_path.write_text("{not json", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "apply_yul_rewrite_pipeline.py"),
          "--input",
          str(input_path),
          "--output",
          str(output_path),
          "--pipeline-manifest",
          str(pipeline_path),
          "--proof-manifest",
          str(proof_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("yul-rewrite-pipeline failed: invalid JSON in", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_rejects_invalid_utf8_input_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"
      proof_path = tmp / "proof.json"

      input_path.write_bytes(b"\xff\xfe")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )
      proof_path.write_text(
        json.dumps(
          {
            "defaults": {
              "renameOnly": {
                "rewritePass": "rename-normalization",
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
              }
            },
            "families": [],
          }
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "apply_yul_rewrite_pipeline.py"),
          "--input",
          str(input_path),
          "--output",
          str(output_path),
          "--pipeline-manifest",
          str(pipeline_path),
          "--proof-manifest",
          str(proof_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("yul-rewrite-pipeline failed: failed to decode UTF-8 text file", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_rejects_json_out_write_failure_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      tmp = pathlib.Path(d)
      input_path = tmp / "Morpho.yul"
      output_path = tmp / "Morpho.rewritten.yul"
      pipeline_path = tmp / "pipeline.json"
      proof_path = tmp / "proof.json"
      json_out = tmp / "occupied" / "report.json"

      input_path.write_text('object "M" { code { } }\n', encoding="utf-8")
      pipeline_path.write_text(
        json.dumps(
          {
            "version": "rewrite-pipeline-v1",
            "stages": [
              {
                "rewritePass": "rename-normalization",
                "families": ["renameOnly"],
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                "implemented": False,
              }
            ],
          }
        ),
        encoding="utf-8",
      )
      proof_path.write_text(
        json.dumps(
          {
            "defaults": {
              "renameOnly": {
                "rewritePass": "rename-normalization",
                "proofRefs": ["rewrite.rename_only.alpha_equiv"],
              }
            },
            "families": [],
          }
        ),
        encoding="utf-8",
      )
      json_out.parent.write_text("not a directory", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "apply_yul_rewrite_pipeline.py"),
          "--input",
          str(input_path),
          "--output",
          str(output_path),
          "--pipeline-manifest",
          str(pipeline_path),
          "--proof-manifest",
          str(proof_path),
          "--json-out",
          str(json_out),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("yul-rewrite-pipeline failed: failed to write text file", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
