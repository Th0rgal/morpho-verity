#!/usr/bin/env python3
"""Fail-closed sync check for the proof-facing summary in SemanticBridgeReadiness.lean."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from typing import Any

from check_semantic_bridge_readiness_sync import build_config_projection, load_config


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
READINESS_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeReadiness.lean"


class SemanticBridgeReadinessSummaryError(RuntimeError):
  pass


TOTAL_COMMENT_RE = re.compile(
  r"/--\s+All\s+(?P<count>\d+)\s+semantic equivalence obligations from SolidityBridge\.lean\.",
  re.DOTALL,
)
LINK1_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+of\s+(?P<total>\d+)\s+operations have Link 1 proven\.",
  re.DOTALL,
)
LINK1_LIST_RE = re.compile(
  r"These\s+(?P<count>\d+)\s+Link 1 operations are:\s+(?P<ops>[^.]+)\.",
  re.DOTALL,
)
ASSUMED_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+operations still have assumed status \(Link 1 not yet proven\)\.\s+-/",
  re.DOTALL,
)
MIGRATED_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+of\s+(?P<total>\d+)\s+operations have full \(non-stub\) macro implementations\.",
  re.DOTALL,
)
PENDING_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+operations still need macro migration before discharge\.\s+-/",
  re.DOTALL,
)
THEOREM_PATTERNS = {
  "obligation_count": re.compile(
    r"theorem\s+obligation_count\s*:\s*obligations\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "link1_proven_count": re.compile(
    r"theorem\s+link1_proven_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.status != \.assumed\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "assumed_count": re.compile(
    r"theorem\s+assumed_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.status == \.assumed\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "macro_migrated_count": re.compile(
    r"theorem\s+macro_migrated_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.macroMigrated\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "macro_pending_count": re.compile(
    r"theorem\s+macro_pending_count\s*:\s*\(obligations\.filter\s*\(fun o => !o\.macroMigrated\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
}


def derive_summary(config: dict[str, Any]) -> dict[str, Any]:
  entries = build_config_projection(load_config(config) if isinstance(config, pathlib.Path) else config)
  total = len(entries)
  link1_operations = [entry["operation"] for entry in entries if entry["status"] != "assumed"]
  assumed_count = sum(1 for entry in entries if entry["status"] == "assumed")
  macro_migrated_count = sum(1 for entry in entries if entry["macroMigrated"])
  return {
    "total": total,
    "link1_count": len(link1_operations),
    "link1_operations": link1_operations,
    "assumed_count": assumed_count,
    "macro_migrated_count": macro_migrated_count,
    "macro_pending_count": total - macro_migrated_count,
  }


def require_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  match = pattern.search(text)
  if match is None:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to locate {description} in SemanticBridgeReadiness.lean"
    )
  return match


def parse_operation_list(raw_ops: str) -> list[str]:
  operations = [item.strip().strip("`") for item in raw_ops.replace("\n", " ").split(",")]
  normalized = [item for item in operations if item and item.lower() != "none"]
  return normalized


def require_count(
  actual: int,
  expected: int,
  description: str,
) -> None:
  if actual != expected:
    raise SemanticBridgeReadinessSummaryError(
      f"{description} drift: expected {expected}, found {actual}"
    )


def validate_summary(text: str, summary: dict[str, Any]) -> None:
  total_comment = require_match(TOTAL_COMMENT_RE, text, "obligation summary comment")
  require_count(int(total_comment.group("count")), summary["total"], "obligation summary comment count")

  link1_comment = require_match(LINK1_COMMENT_RE, text, "Link 1 summary comment")
  require_count(int(link1_comment.group("count")), summary["link1_count"], "Link 1 summary comment count")
  require_count(int(link1_comment.group("total")), summary["total"], "Link 1 summary total")

  link1_list = require_match(LINK1_LIST_RE, text, "Link 1 operation list")
  require_count(int(link1_list.group("count")), summary["link1_count"], "Link 1 operation list count")
  actual_operations = parse_operation_list(link1_list.group("ops"))
  if not summary["link1_operations"] and actual_operations:
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list drift: expected no operations; found "
      + ", ".join(actual_operations)
    )
  if len(actual_operations) != len(set(actual_operations)):
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list contains duplicate operations"
    )
  if set(actual_operations) != set(summary["link1_operations"]):
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list drift: expected "
      + ", ".join(summary["link1_operations"])
      + "; found "
      + ", ".join(actual_operations)
    )

  assumed_comment = require_match(ASSUMED_COMMENT_RE, text, "assumed-count comment")
  require_count(int(assumed_comment.group("count")), summary["assumed_count"], "assumed-count comment")

  migrated_comment = require_match(MIGRATED_COMMENT_RE, text, "macro-migrated comment")
  require_count(
    int(migrated_comment.group("count")),
    summary["macro_migrated_count"],
    "macro-migrated comment count",
  )
  require_count(
    int(migrated_comment.group("total")),
    summary["total"],
    "macro-migrated comment total",
  )

  pending_comment = require_match(PENDING_COMMENT_RE, text, "macro-pending comment")
  require_count(
    int(pending_comment.group("count")),
    summary["macro_pending_count"],
    "macro-pending comment",
  )

  theorem_expectations = {
    "obligation_count": summary["total"],
    "link1_proven_count": summary["link1_count"],
    "assumed_count": summary["assumed_count"],
    "macro_migrated_count": summary["macro_migrated_count"],
    "macro_pending_count": summary["macro_pending_count"],
  }
  for theorem_name, expected in theorem_expectations.items():
    match = require_match(THEOREM_PATTERNS[theorem_name], text, f"{theorem_name} theorem")
    require_count(int(match.group("count")), expected, f"{theorem_name} theorem")


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the proof-facing summary in SemanticBridgeReadiness.lean"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--readiness", type=pathlib.Path, default=READINESS_PATH)
  args = parser.parse_args()

  summary = derive_summary(load_config(args.config))
  text = args.readiness.read_text(encoding="utf-8")
  validate_summary(text, summary)

  print("semantic-bridge-readiness-summary check: OK")
  print(
    "counts: "
    f"total={summary['total']} "
    f"link1={summary['link1_count']} "
    f"assumed={summary['assumed_count']} "
    f"macro_migrated={summary['macro_migrated_count']} "
    f"macro_pending={summary['macro_pending_count']}"
  )
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeReadinessSummaryError as e:
    print(f"semantic-bridge-readiness-summary check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"semantic-bridge-readiness-summary check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
