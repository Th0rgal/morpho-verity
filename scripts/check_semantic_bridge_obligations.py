#!/usr/bin/env python3
"""Fail-closed tracker for semantic bridge equivalence obligations.

Validates that config/semantic-bridge-obligations.json matches the set of
*SemEq definitions in Morpho/Proofs/SolidityBridge.lean and reports
assumed-vs-discharged status.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
BRIDGE_PATH = ROOT / "Morpho" / "Proofs" / "SolidityBridge.lean"
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"

VALID_STATUSES = {"assumed", "in_progress", "discharged"}

SEM_EQ_RE = re.compile(r"^def\s+(\w+SemEq)\b", re.MULTILINE)


class ObligationError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


def extract_sem_eq_definitions(bridge_text: str) -> list[str]:
    """Extract all *SemEq definition names from SolidityBridge.lean."""
    return [m.group(1) for m in SEM_EQ_RE.finditer(bridge_text)]


def load_config(path: pathlib.Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def validate_config(config: dict[str, Any], bridge_hypotheses: list[str]) -> None:
    """Validate config structure and consistency with SolidityBridge.lean."""
    obligations = config.get("obligations")
    if not isinstance(obligations, list):
        raise ObligationError("config missing 'obligations' array")

    # Check each obligation has required fields
    for i, obl in enumerate(obligations):
        for field in ("id", "hypothesis", "operation", "status"):
            if field not in obl:
                raise ObligationError(
                    f"obligation[{i}] missing required field '{field}'"
                )
        if obl["status"] not in VALID_STATUSES:
            raise ObligationError(
                f"obligation[{i}] has invalid status '{obl['status']}' "
                f"(valid: {sorted(VALID_STATUSES)})"
            )

    # Check no duplicate IDs
    ids = [o["id"] for o in obligations]
    if len(ids) != len(set(ids)):
        dupes = sorted(x for x in ids if ids.count(x) > 1)
        raise ObligationError(f"duplicate obligation IDs: {dupes}")

    # Check no duplicate hypotheses
    hyps = [o["hypothesis"] for o in obligations]
    if len(hyps) != len(set(hyps)):
        dupes = sorted(x for x in hyps if hyps.count(x) > 1)
        raise ObligationError(f"duplicate hypotheses: {dupes}")

    # Cross-reference against SolidityBridge.lean
    config_hyps = set(hyps)
    bridge_hyps = set(bridge_hypotheses)

    missing = sorted(bridge_hyps - config_hyps)
    extra = sorted(config_hyps - bridge_hyps)

    if missing:
        raise ObligationError(
            f"hypotheses in SolidityBridge.lean but not in config: {missing}"
        )
    if extra:
        raise ObligationError(
            f"hypotheses in config but not in SolidityBridge.lean: {extra}"
        )


def build_report(config: dict[str, Any]) -> dict[str, Any]:
    obligations = config["obligations"]
    by_status: dict[str, int] = {}
    for obl in obligations:
        s = obl["status"]
        by_status[s] = by_status.get(s, 0) + 1

    return {
        "source": str(BRIDGE_PATH.relative_to(ROOT)),
        "config": str(CONFIG_PATH.relative_to(ROOT)),
        "total": len(obligations),
        "byStatus": dict(sorted(by_status.items())),
        "obligations": [
            {
                "id": o["id"],
                "hypothesis": o["hypothesis"],
                "operation": o["operation"],
                "status": o["status"],
            }
            for o in obligations
        ],
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Validate semantic bridge obligations against SolidityBridge.lean"
    )
    p.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
    p.add_argument("--bridge", type=pathlib.Path, default=BRIDGE_PATH)
    p.add_argument("--json-out", type=pathlib.Path)
    return p


def main() -> None:
    args = parser().parse_args()

    bridge_text = read_text(args.bridge)
    bridge_hypotheses = extract_sem_eq_definitions(bridge_text)

    config = load_config(args.config)
    validate_config(config, bridge_hypotheses)

    report = build_report(config)

    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        with args.json_out.open("w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, sort_keys=True)
            f.write("\n")

    print("semantic bridge obligations check: OK")
    print(f"source: {report['source']}")
    print(f"total obligations: {report['total']}")
    for status, count in sorted(report["byStatus"].items()):
        print(f"  {status}: {count}")


if __name__ == "__main__":
    try:
        main()
    except ObligationError as e:
        print(f"semantic bridge obligations check failed: {e}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"semantic bridge obligations check failed: {e}", file=sys.stderr)
        sys.exit(1)
