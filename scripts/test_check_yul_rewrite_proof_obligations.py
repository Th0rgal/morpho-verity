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
    extract_declared_proof_obligations,
    extract_manifest_proof_plans,
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
