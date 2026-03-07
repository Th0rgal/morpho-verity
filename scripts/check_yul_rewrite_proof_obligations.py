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
SECTION_RE = re.compile(r"^\s*section(?:\s+[A-Za-z0-9_.']+)?\s*$")
END_RE = re.compile(r"^\s*end(?:\s+[A-Za-z0-9_.']+)?\s*$")
OBLIGATION_RE = re.compile(
    r'RewriteProofObligation\s*"([^"]+)"\s*"([^"]+)"\s*"([^"]+)"',
    re.MULTILINE,
)


class RewriteProofError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


def read_json(path: pathlib.Path) -> Any:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def extract_manifest_proof_plans(manifest: dict[str, Any]) -> dict[str, dict[str, str]]:
    plans: dict[str, dict[str, str]] = {}

    defaults = manifest.get("defaults", {})
    if not isinstance(defaults, dict):
        raise RewriteProofError("manifest key `defaults` must be an object")
    for kind, plan in defaults.items():
        _add_plan_refs(
            plans,
            plan,
            where=f"defaults.{kind}",
            family=kind,
        )

    families = manifest.get("families", [])
    if not isinstance(families, list):
        raise RewriteProofError("manifest key `families` must be a list")
    for i, plan in enumerate(families):
        if not isinstance(plan, dict):
            raise RewriteProofError(f"manifest entry `families[{i}]` must be an object")
        family = plan.get("family")
        if not isinstance(family, str) or not family.strip():
            raise RewriteProofError(
                f"manifest entry `families[{i}]` must provide a non-empty `family` string"
            )
        _add_plan_refs(
            plans,
            plan,
            where=f"families[{i}]",
            family=family,
        )

    return plans


def extract_manifest_proof_refs(manifest: dict[str, Any]) -> list[str]:
    return list(extract_manifest_proof_plans(manifest))


def _add_plan_refs(
    plans: dict[str, dict[str, str]],
    plan: Any,
    *,
    where: str,
    family: str,
) -> None:
    if not isinstance(plan, dict):
        raise RewriteProofError(f"manifest entry `{where}` must be an object")
    rewrite_pass = plan.get("rewritePass")
    if not isinstance(rewrite_pass, str) or not rewrite_pass.strip():
        raise RewriteProofError(
            f"manifest entry `{where}` must provide a non-empty `rewritePass` string"
        )
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
    for proof_ref in proof_refs:
        if proof_ref in plans:
            raise RewriteProofError(
                f"manifest proof ref `{proof_ref}` is duplicated across entries "
                f"`{plans[proof_ref]['where']}` and `{where}`"
            )
        plans[proof_ref] = {
            "where": where,
            "rewritePass": rewrite_pass,
            "family": family,
        }


def extract_declared_proof_obligations(lean_text: str) -> dict[str, dict[str, str]]:
    obligations: dict[str, dict[str, str]] = {}
    namespace_stack: list[str] = []
    scope_stack: list[tuple[bool, int, str | None]] = []
    lines = lean_text.splitlines()
    i = 0

    while i < len(lines):
        line = lines[i]
        namespace_match = NAMESPACE_RE.match(line)
        if namespace_match:
            namespace_name = namespace_match.group(1)
            namespace_stack.append(namespace_name)
            scope_stack.append((True, i + 1, namespace_name))
            i += 1
            continue

        if SECTION_RE.match(line):
            scope_stack.append((False, i + 1, None))
            i += 1
            continue

        if END_RE.match(line):
            if not scope_stack:
                raise RewriteProofError(
                    f"unexpected `end` without matching scope opener at line {i + 1}"
                )
            is_namespace, _, _ = scope_stack.pop()
            if is_namespace:
                namespace_stack.pop()
            i += 1
            continue

        decl_match = DECL_RE.match(line)
        if not decl_match:
            i += 1
            continue

        full_name = ".".join([*namespace_stack, decl_match.group(1)])
        block_lines = [line]
        i += 1
        while i < len(lines):
            next_line = lines[i]
            if NAMESPACE_RE.match(next_line) or END_RE.match(next_line) or DECL_RE.match(next_line):
                break
            block_lines.append(next_line)
            i += 1

        if not full_name.startswith(PROOF_NAMESPACE):
            continue
        declared_ref = full_name.removeprefix(PROOF_NAMESPACE)
        if not declared_ref.startswith("rewrite."):
            continue

        obligation_match = OBLIGATION_RE.search("\n".join(block_lines))
        if not obligation_match:
            continue

        proof_ref, rewrite_pass, family = obligation_match.groups()
        if proof_ref != declared_ref:
            raise RewriteProofError(
                f"proof declaration `{full_name}` must reference proof ref `{declared_ref}`, "
                f"found `{proof_ref}`"
            )
        if proof_ref in obligations:
            raise RewriteProofError(
                f"proof ref `{proof_ref}` is declared more than once in YulRewriteProofs.lean"
            )
        obligations[proof_ref] = {
            "declaration": full_name,
            "rewritePass": rewrite_pass,
            "family": family,
        }

    if scope_stack:
        is_namespace, opened_line, opened_name = scope_stack[-1]
        if is_namespace:
            raise RewriteProofError(
                f"unterminated namespace `{opened_name}` opened at line {opened_line}"
            )
        raise RewriteProofError(
            f"unterminated section opened at line {opened_line}"
        )

    return obligations


def extract_declared_proof_refs(lean_text: str) -> list[str]:
    return list(extract_declared_proof_obligations(lean_text))


def validate_manifest_against_proofs(
    manifest_proof_plans: dict[str, dict[str, str]],
    declared_proof_obligations: dict[str, dict[str, str]],
) -> None:
    manifest_set = set(manifest_proof_plans)
    declared_set = set(declared_proof_obligations)

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

    mismatches: list[str] = []
    for proof_ref in sorted(manifest_set):
        manifest_plan = manifest_proof_plans[proof_ref]
        declared = declared_proof_obligations[proof_ref]
        if manifest_plan["rewritePass"] != declared["rewritePass"]:
            mismatches.append(
                f"{proof_ref} rewritePass manifest=`{manifest_plan['rewritePass']}` "
                f"lean=`{declared['rewritePass']}`"
            )
        if manifest_plan["family"] != declared["family"]:
            mismatches.append(
                f"{proof_ref} family manifest=`{manifest_plan['family']}` "
                f"lean=`{declared['family']}`"
            )
    if mismatches:
        raise RewriteProofError(
            "manifest/Lean proof metadata mismatch: " + "; ".join(mismatches)
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
    manifest_proof_plans = extract_manifest_proof_plans(manifest)
    validate_manifest_against_proofs(
        manifest_proof_plans,
        extract_declared_proof_obligations(read_text(args.proof_file)),
    )

    report = build_report(list(manifest_proof_plans))
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
