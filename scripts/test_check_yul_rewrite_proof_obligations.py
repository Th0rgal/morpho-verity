#!/usr/bin/env python3
"""Unit tests for Yul rewrite proof obligation checker."""

from __future__ import annotations

import pathlib
import json
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_yul_rewrite_proof_obligations import (  # noqa: E402
    RewriteProofError,
    build_report,
    display_path,
    extract_declared_proof_obligations,
    extract_manifest_proof_plans,
    extract_declared_proof_refs,
    extract_manifest_proof_refs,
    load_manifest,
    read_json,
    read_text,
    validate_manifest_against_proofs,
    write_json_report,
)


SAMPLE_LEAN = """\
namespace Morpho.Proofs.YulRewriteProofs

namespace rewrite
namespace checked_add
axiom width_alignment : RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end checked_add

namespace rename_only
theorem alpha_equiv : RewriteProofObligation "rewrite.rename_only.alpha_equiv" "rename-pass" "renameOnly" := by
  trivial
end rename_only
end rewrite

end Morpho.Proofs.YulRewriteProofs
"""


class ExtractManifestProofRefsTests(unittest.TestCase):
    def test_extracts_defaults_and_family_refs(self) -> None:
        manifest = {
            "defaults": {
                "renameOnly": {
                    "rewritePass": "rename-pass",
                    "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                }
            },
            "families": [
                {
                    "family": "checked_add",
                    "rewritePass": "pass",
                    "proofRefs": ["rewrite.checked_add.width_alignment"],
                }
            ],
        }
        self.assertEqual(
            extract_manifest_proof_refs(manifest),
            [
                "rewrite.rename_only.alpha_equiv",
                "rewrite.checked_add.width_alignment",
            ],
        )

    def test_rejects_empty_plan_proof_refs(self) -> None:
        manifest = {
            "defaults": {},
            "families": [{"family": "checked_add", "rewritePass": "pass", "proofRefs": []}],
        }
        with self.assertRaisesRegex(RewriteProofError, "must list at least one proof ref"):
            extract_manifest_proof_refs(manifest)

    def test_rejects_blank_proof_refs(self) -> None:
        manifest = {
            "defaults": {
                "renameOnly": {
                    "rewritePass": "rename-pass",
                    "proofRefs": [""],
                }
            },
            "families": [],
        }
        with self.assertRaisesRegex(RewriteProofError, "non-empty strings"):
            extract_manifest_proof_refs(manifest)

    def test_rejects_duplicate_proof_refs_across_manifest_entries(self) -> None:
        manifest = {
            "defaults": {
                "renameOnly": {
                    "rewritePass": "rename-pass",
                    "proofRefs": ["rewrite.checked_add.width_alignment"],
                }
            },
            "families": [
                {
                    "family": "checked_add",
                    "rewritePass": "pass",
                    "proofRefs": ["rewrite.checked_add.width_alignment"],
                }
            ],
        }
        with self.assertRaisesRegex(RewriteProofError, "duplicated across entries"):
            extract_manifest_proof_plans(manifest)


class LoadManifestTests(unittest.TestCase):
    def test_rejects_missing_manifest_with_checker_error(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = pathlib.Path(tmpdir) / "missing.json"
            with self.assertRaisesRegex(RewriteProofError, "failed to read"):
                load_manifest(path)

    def test_rejects_invalid_json_with_checker_error(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = pathlib.Path(tmpdir) / "manifest.json"
            path.write_text("{\n", encoding="utf-8")
            with self.assertRaisesRegex(RewriteProofError, "invalid JSON"):
                load_manifest(path)

    def test_rejects_invalid_utf8_with_checker_error(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = pathlib.Path(tmpdir) / "manifest.json"
            path.write_bytes(b"\xff")
            with self.assertRaisesRegex(RewriteProofError, "failed to decode"):
                read_json(path)

    def test_rejects_non_object_root(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = pathlib.Path(tmpdir) / "manifest.json"
            path.write_text("[]\n", encoding="utf-8")
            with self.assertRaisesRegex(RewriteProofError, "must be a JSON object"):
                load_manifest(path)


class ExtractDeclaredProofRefsTests(unittest.TestCase):
    def test_extracts_rewrite_namespace_refs(self) -> None:
        self.assertEqual(
            extract_declared_proof_refs(SAMPLE_LEAN),
            [
                "rewrite.checked_add.width_alignment",
                "rewrite.rename_only.alpha_equiv",
            ],
        )

    def test_extracts_declared_obligation_metadata(self) -> None:
        self.assertEqual(
            extract_declared_proof_obligations(SAMPLE_LEAN),
            {
                "rewrite.checked_add.width_alignment": {
                    "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.width_alignment",
                    "rewritePass": "pass",
                    "family": "checked_add",
                },
                "rewrite.rename_only.alpha_equiv": {
                    "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.rename_only.alpha_equiv",
                    "rewritePass": "rename-pass",
                    "family": "renameOnly",
                },
            },
        )

    def test_ignores_other_namespaces(self) -> None:
        lean = """\
namespace Other.Namespace
axiom rewrite_fake : True
end Other.Namespace
"""
        self.assertEqual(extract_declared_proof_refs(lean), [])

    def test_rejects_declared_ref_mismatch(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.other_name" "pass" "checked_add"
end checked_add
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        with self.assertRaisesRegex(RewriteProofError, "must reference proof ref"):
            extract_declared_proof_obligations(lean)

    def test_ignores_non_obligation_helper_theorems(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
theorem helper : True := by
  trivial
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end checked_add
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        self.assertEqual(
            extract_declared_proof_obligations(lean),
            {
                "rewrite.checked_add.width_alignment": {
                    "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.width_alignment",
                    "rewritePass": "pass",
                    "family": "checked_add",
                }
            },
        )

    def test_section_scopes_do_not_pop_namespace_qualifiers(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
section helper
theorem helper : True := by
  trivial
end
axiom second :
  RewriteProofObligation "rewrite.checked_add.second" "pass" "checked_add"
end checked_add
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        self.assertEqual(
            extract_declared_proof_obligations(lean),
            {
                "rewrite.checked_add.second": {
                    "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.second",
                    "rewritePass": "pass",
                    "family": "checked_add",
                }
            },
        )

    def test_rejects_unmatched_end_without_scope_opener(self) -> None:
        lean = """\
end rewrite
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end checked_add
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        with self.assertRaisesRegex(RewriteProofError, "unexpected `end` without matching scope opener"):
            extract_declared_proof_obligations(lean)

    def test_rejects_mismatched_named_end_for_namespace(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end rename_only
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        with self.assertRaisesRegex(RewriteProofError, "mismatched `end rename_only`"):
            extract_declared_proof_obligations(lean)

    def test_rejects_named_end_for_anonymous_section(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
section
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end helper
end checked_add
end rewrite
end Morpho.Proofs.YulRewriteProofs
"""
        with self.assertRaisesRegex(RewriteProofError, "cannot close anonymous section"):
            extract_declared_proof_obligations(lean)

    def test_rejects_unterminated_namespace_at_eof(self) -> None:
        lean = """\
namespace Morpho.Proofs.YulRewriteProofs
namespace rewrite
namespace checked_add
axiom width_alignment :
  RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end checked_add
end rewrite
"""
        with self.assertRaisesRegex(RewriteProofError, "unterminated namespace `Morpho.Proofs.YulRewriteProofs`"):
            extract_declared_proof_obligations(lean)

    def test_read_text_rejects_invalid_utf8(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = pathlib.Path(tmpdir) / "YulRewriteProofs.lean"
            path.write_bytes(b"\xff")
            with self.assertRaisesRegex(RewriteProofError, "failed to decode"):
                read_text(path)


class ValidateManifestAgainstProofsTests(unittest.TestCase):
    def test_matching_sets_pass(self) -> None:
        validate_manifest_against_proofs(
            {
                "rewrite.checked_add.width_alignment": {
                    "rewritePass": "pass",
                    "family": "checked_add",
                }
            },
            {
                "rewrite.checked_add.width_alignment": {
                    "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.width_alignment",
                    "rewritePass": "pass",
                    "family": "checked_add",
                }
            },
        )

    def test_missing_proof_placeholder_fails(self) -> None:
        with self.assertRaisesRegex(RewriteProofError, "not declared"):
            validate_manifest_against_proofs(
                {
                    "rewrite.checked_add.width_alignment": {
                        "rewritePass": "pass",
                        "family": "checked_add",
                    }
                },
                {},
            )

    def test_extra_proof_placeholder_fails(self) -> None:
        with self.assertRaisesRegex(RewriteProofError, "not tracked by manifest"):
            validate_manifest_against_proofs(
                {},
                {
                    "rewrite.checked_add.width_alignment": {
                        "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.width_alignment",
                        "rewritePass": "pass",
                        "family": "checked_add",
                    }
                },
            )

    def test_metadata_mismatch_fails(self) -> None:
        with self.assertRaisesRegex(RewriteProofError, "metadata mismatch"):
            validate_manifest_against_proofs(
                {
                    "rewrite.checked_add.width_alignment": {
                        "rewritePass": "pass",
                        "family": "checked_add",
                    }
                },
                {
                    "rewrite.checked_add.width_alignment": {
                        "declaration": "Morpho.Proofs.YulRewriteProofs.rewrite.checked_add.width_alignment",
                        "rewritePass": "other-pass",
                        "family": "checked_add",
                    }
                },
            )


class BuildReportTests(unittest.TestCase):
    def test_build_report_summarizes_refs(self) -> None:
        report = build_report(
            [
                "rewrite.rename_only.alpha_equiv",
                "rewrite.checked_add.width_alignment",
            ],
            manifest_path=pathlib.Path("config/yul-rewrite-proof-obligations.json"),
            proof_path=pathlib.Path("Morpho/Proofs/YulRewriteProofs.lean"),
        )
        self.assertEqual(report["trackedProofRefCount"], 2)
        self.assertEqual(
            report["byFamily"],
            [
                {"family": "checked_add", "count": 1},
                {"family": "rename_only", "count": 1},
            ],
        )

    def test_build_report_uses_actual_external_input_paths(self) -> None:
        manifest_path = pathlib.Path("/tmp/external-manifest.json")
        proof_path = pathlib.Path("/tmp/external-proof.lean")
        report = build_report(
            ["rewrite.checked_add.width_alignment"],
            manifest_path=manifest_path,
            proof_path=proof_path,
        )
        self.assertEqual(report["manifest"], str(manifest_path))
        self.assertEqual(report["proofFile"], str(proof_path))


class DisplayPathTests(unittest.TestCase):
    def test_display_path_keeps_external_absolute_paths(self) -> None:
        path = pathlib.Path("/tmp/external-proof.lean")
        self.assertEqual(display_path(path), str(path))


class IoHelperTests(unittest.TestCase):
    def test_write_json_report_wraps_os_errors(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            report_path = pathlib.Path(tmpdir) / "missing-parent" / "child" / "report.json"
            report_path.parent.parent.mkdir()
            report_path.parent.write_text("not a directory", encoding="utf-8")
            with self.assertRaisesRegex(RewriteProofError, "failed to write JSON report"):
                write_json_report(report_path, {"ok": True})


class CliTests(unittest.TestCase):
    def test_cli_reports_invalid_manifest_json_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            manifest = pathlib.Path(tmpdir) / "manifest.json"
            proof_file = pathlib.Path(tmpdir) / "YulRewriteProofs.lean"
            manifest.write_text("{\n", encoding="utf-8")
            proof_file.write_text(SAMPLE_LEAN, encoding="utf-8")

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_yul_rewrite_proof_obligations.py"),
                    "--manifest",
                    str(manifest),
                    "--proof-file",
                    str(proof_file),
                ],
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(proc.returncode, 1)
        self.assertIn("yul-rewrite-proof-obligations check failed:", proc.stderr)
        self.assertIn("invalid JSON", proc.stderr)
        self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_missing_proof_file_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            manifest = pathlib.Path(tmpdir) / "manifest.json"
            proof_file = pathlib.Path(tmpdir) / "missing.lean"
            manifest.write_text(
                '{'
                '"defaults":{"renameOnly":{"rewritePass":"rename-pass","proofRefs":["rewrite.rename_only.alpha_equiv"]}},'
                '"families":[{"family":"checked_add","rewritePass":"pass","proofRefs":["rewrite.checked_add.width_alignment"]}]'
                '}\n',
                encoding="utf-8",
            )

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_yul_rewrite_proof_obligations.py"),
                    "--manifest",
                    str(manifest),
                    "--proof-file",
                    str(proof_file),
                ],
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(proc.returncode, 1)
        self.assertIn("yul-rewrite-proof-obligations check failed:", proc.stderr)
        self.assertIn("failed to read", proc.stderr)
        self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_invalid_utf8_manifest_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            manifest = pathlib.Path(tmpdir) / "manifest.json"
            proof_file = pathlib.Path(tmpdir) / "YulRewriteProofs.lean"
            manifest.write_bytes(b"\xff")
            proof_file.write_text(SAMPLE_LEAN, encoding="utf-8")

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_yul_rewrite_proof_obligations.py"),
                    "--manifest",
                    str(manifest),
                    "--proof-file",
                    str(proof_file),
                ],
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(proc.returncode, 1)
        self.assertIn("yul-rewrite-proof-obligations check failed:", proc.stderr)
        self.assertIn("failed to decode", proc.stderr)
        self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_json_out_write_failure_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            manifest = pathlib.Path(tmpdir) / "manifest.json"
            proof_file = pathlib.Path(tmpdir) / "YulRewriteProofs.lean"
            json_out = pathlib.Path(tmpdir) / "missing-parent" / "child" / "report.json"
            manifest.write_text(
                "{"
                "\"defaults\":{\"renameOnly\":{\"rewritePass\":\"rename-pass\",\"proofRefs\":[\"rewrite.rename_only.alpha_equiv\"]}},"
                "\"families\":[{\"family\":\"checked_add\",\"rewritePass\":\"pass\",\"proofRefs\":[\"rewrite.checked_add.width_alignment\"]}]"
                "}\n",
                encoding="utf-8",
            )
            proof_file.write_text(SAMPLE_LEAN, encoding="utf-8")
            json_out.parent.parent.mkdir()
            json_out.parent.write_text("not a directory", encoding="utf-8")

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_yul_rewrite_proof_obligations.py"),
                    "--manifest",
                    str(manifest),
                    "--proof-file",
                    str(proof_file),
                    "--json-out",
                    str(json_out),
                ],
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(proc.returncode, 1)
        self.assertIn("yul-rewrite-proof-obligations check failed:", proc.stderr)
        self.assertIn("failed to write JSON report", proc.stderr)
        self.assertNotIn("Traceback", proc.stderr)

    def test_cli_json_report_uses_actual_input_paths(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            manifest = pathlib.Path(tmpdir) / "manifest.json"
            proof_file = pathlib.Path(tmpdir) / "YulRewriteProofs.lean"
            json_out = pathlib.Path(tmpdir) / "report.json"
            manifest.write_text(
                "{"
                "\"defaults\":{\"renameOnly\":{\"rewritePass\":\"rename-pass\",\"proofRefs\":[\"rewrite.rename_only.alpha_equiv\"]}},"
                "\"families\":[{\"family\":\"checked_add\",\"rewritePass\":\"pass\",\"proofRefs\":[\"rewrite.checked_add.width_alignment\"]}]"
                "}\n",
                encoding="utf-8",
            )
            proof_file.write_text(SAMPLE_LEAN, encoding="utf-8")

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_yul_rewrite_proof_obligations.py"),
                    "--manifest",
                    str(manifest),
                    "--proof-file",
                    str(proof_file),
                    "--json-out",
                    str(json_out),
                ],
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(proc.returncode, 0)
            report = json.loads(json_out.read_text(encoding="utf-8"))
            self.assertEqual(report["manifest"], str(manifest))
            self.assertEqual(report["proofFile"], str(proof_file))


if __name__ == "__main__":
    unittest.main()
