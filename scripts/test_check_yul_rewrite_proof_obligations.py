#!/usr/bin/env python3
"""Unit tests for Yul rewrite proof obligation checker."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_yul_rewrite_proof_obligations import (  # noqa: E402
    RewriteProofError,
    build_report,
    extract_declared_proof_refs,
    extract_manifest_proof_refs,
    validate_manifest_against_proofs,
)


SAMPLE_LEAN = """\
namespace Morpho.Proofs.YulRewriteProofs

namespace rewrite
namespace checked_add
axiom width_alignment : RewriteProofObligation "rewrite.checked_add.width_alignment" "pass" "checked_add"
end checked_add

namespace rename_only
theorem alpha_equiv : True := by
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
                    "proofRefs": ["rewrite.rename_only.alpha_equiv"],
                }
            },
            "families": [
                {
                    "family": "checked_add",
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
            "families": [{"family": "checked_add", "proofRefs": []}],
        }
        with self.assertRaisesRegex(RewriteProofError, "must list at least one proof ref"):
            extract_manifest_proof_refs(manifest)

    def test_rejects_blank_proof_refs(self) -> None:
        manifest = {
            "defaults": {
                "renameOnly": {
                    "proofRefs": [""],
                }
            },
            "families": [],
        }
        with self.assertRaisesRegex(RewriteProofError, "non-empty strings"):
            extract_manifest_proof_refs(manifest)


class ExtractDeclaredProofRefsTests(unittest.TestCase):
    def test_extracts_rewrite_namespace_refs(self) -> None:
        self.assertEqual(
            extract_declared_proof_refs(SAMPLE_LEAN),
            [
                "rewrite.checked_add.width_alignment",
                "rewrite.rename_only.alpha_equiv",
            ],
        )

    def test_ignores_other_namespaces(self) -> None:
        lean = """\
namespace Other.Namespace
axiom rewrite_fake : True
end Other.Namespace
"""
        self.assertEqual(extract_declared_proof_refs(lean), [])


class ValidateManifestAgainstProofsTests(unittest.TestCase):
    def test_matching_sets_pass(self) -> None:
        validate_manifest_against_proofs(
            ["rewrite.checked_add.width_alignment"],
            ["rewrite.checked_add.width_alignment"],
        )

    def test_missing_proof_placeholder_fails(self) -> None:
        with self.assertRaisesRegex(RewriteProofError, "not declared"):
            validate_manifest_against_proofs(
                ["rewrite.checked_add.width_alignment"],
                [],
            )

    def test_extra_proof_placeholder_fails(self) -> None:
        with self.assertRaisesRegex(RewriteProofError, "not tracked by manifest"):
            validate_manifest_against_proofs(
                [],
                ["rewrite.checked_add.width_alignment"],
            )


class BuildReportTests(unittest.TestCase):
    def test_build_report_summarizes_refs(self) -> None:
        report = build_report(
            [
                "rewrite.rename_only.alpha_equiv",
                "rewrite.checked_add.width_alignment",
            ]
        )
        self.assertEqual(report["trackedProofRefCount"], 2)
        self.assertEqual(
            report["byFamily"],
            [
                {"family": "checked_add", "count": 1},
                {"family": "rename_only", "count": 1},
            ],
        )


if __name__ == "__main__":
    unittest.main()
