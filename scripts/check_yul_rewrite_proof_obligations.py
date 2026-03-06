#!/usr/bin/env python3
"""Fail-closed sync check for Yul rewrite proof obligations.

Validates that config/yul-rewrite-proof-obligations.json references proof
placeholders declared in Morpho/Proofs/YulRewriteProofs.lean and that every
placeholder is still tracked by the manifest.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
MANIFEST_PATH = ROOT / "config" / "yul-rewrite-proof-obligations.json"
PROOF_PATH = ROOT / "Morpho" / "Proofs" / "YulRewriteProofs.lean"
PROOF_NAMESPACE = "Morpho.Proofs.YulRewriteProofs."
DECL_RE = re.compile(r"^\s*(?:axiom|theorem)\s+([A-Za-z0-9_']+)\b")
NAMESPACE_RE = re.compile(r"^\s*namespace\s+([A-Za-z0-9_.']+)\s*$")
END_RE = re.compile(r"^\s*end(?:\s+[A-Za-z0-9_.']+)?\s*$")


class RewriteProofError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


def read_json(path: pathlib.Path) -> Any:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def extract_manifest_proof_refs(manifest: dict[str, Any]) -> list[str]:
    refs: list[str] = []

    defaults = manifest.get("defaults", {})
    if not isinstance(defaults, dict):
        raise RewriteProofError("manifest key `defaults` must be an object")
    for kind, plan in defaults.items():
        refs.extend(_extract_plan_refs(plan, where=f"defaults.{kind}"))

    families = manifest.get("families", [])
    if not isinstance(families, list):
        raise RewriteProofError("manifest key `families` must be a list")
    for i, plan in enumerate(families):
        refs.extend(_extract_plan_refs(plan, where=f"families[{i}]"))

    return refs


def _extract_plan_refs(plan: Any, *, where: str) -> list[str]:
    if not isinstance(plan, dict):
        raise RewriteProofError(f"manifest entry `{where}` must be an object")
    proof_refs = plan.get("proofRefs")
    if not isinstance(proof_refs, list):
        raise RewriteProofError(f"manifest entry `{where}` must provide `proofRefs` as a list")
    if not proof_refs:
        raise RewriteProofError(f"manifest entry `{where}` must list at least one proof ref")
    if not all(isinstance(ref, str) and ref.strip() for ref in proof_refs):
        raise RewriteProofError(
            f"manifest entry `{where}` must use non-empty strings in `proofRefs`"
        )
    if len(proof_refs) != len(set(proof_refs)):
        raise RewriteProofError(f"manifest entry `{where}` contains duplicate proof refs")
    return proof_refs


def extract_declared_proof_refs(lean_text: str) -> list[str]:
    refs: list[str] = []
    namespace_stack: list[str] = []

    for line in lean_text.splitlines():
        namespace_match = NAMESPACE_RE.match(line)
        if namespace_match:
            namespace_stack.append(namespace_match.group(1))
            continue

        if END_RE.match(line):
            if namespace_stack:
                namespace_stack.pop()
            continue

        decl_match = DECL_RE.match(line)
        if not decl_match:
            continue

        full_name = ".".join([*namespace_stack, decl_match.group(1)])
        if not full_name.startswith(PROOF_NAMESPACE):
            continue
        proof_ref = full_name.removeprefix(PROOF_NAMESPACE)
        if proof_ref.startswith("rewrite."):
            refs.append(proof_ref)

    return refs


def validate_manifest_against_proofs(
    manifest_proof_refs: list[str], declared_proof_refs: list[str]
) -> None:
    manifest_set = set(manifest_proof_refs)
    declared_set = set(declared_proof_refs)

    missing = sorted(manifest_set - declared_set)
    extra = sorted(declared_set - manifest_set)
    if missing:
        raise RewriteProofError(
            "proof refs in manifest but not declared in YulRewriteProofs.lean: "
            + ", ".join(missing)
        )
    if extra:
        raise RewriteProofError(
            "proof placeholders declared in YulRewriteProofs.lean but not tracked by manifest: "
            + ", ".join(extra)
        )


def build_report(manifest_proof_refs: list[str]) -> dict[str, Any]:
    by_prefix: dict[str, int] = {}
    for ref in manifest_proof_refs:
        parts = ref.split(".")
        family = parts[1] if len(parts) > 1 else ref
        by_prefix[family] = by_prefix.get(family, 0) + 1

    return {
        "manifest": str(MANIFEST_PATH.relative_to(ROOT)),
        "proofFile": str(PROOF_PATH.relative_to(ROOT)),
        "trackedProofRefCount": len(manifest_proof_refs),
        "proofRefs": manifest_proof_refs,
        "byFamily": [
            {"family": family, "count": count}
            for family, count in sorted(by_prefix.items())
        ],
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Validate Yul rewrite proof manifest against Lean proof placeholders"
    )
    p.add_argument("--manifest", type=pathlib.Path, default=MANIFEST_PATH)
    p.add_argument("--proof-file", type=pathlib.Path, default=PROOF_PATH)
    p.add_argument("--json-out", type=pathlib.Path)
    return p


def main() -> None:
    args = parser().parse_args()

    manifest = read_json(args.manifest)
    if not isinstance(manifest, dict):
        raise RewriteProofError("rewrite proof manifest must be a JSON object")
    manifest_proof_refs = extract_manifest_proof_refs(manifest)
    declared_proof_refs = extract_declared_proof_refs(read_text(args.proof_file))
    validate_manifest_against_proofs(manifest_proof_refs, declared_proof_refs)

    report = build_report(manifest_proof_refs)
    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        args.json_out.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")

    print("yul-rewrite-proof-obligations check: OK")
    print(f"trackedProofRefs: {report['trackedProofRefCount']}")


if __name__ == "__main__":
    try:
        main()
    except RewriteProofError as exc:
        print(f"yul-rewrite-proof-obligations check failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
