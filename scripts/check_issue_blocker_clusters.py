#!/usr/bin/env python3
"""Fail-closed tracker for issue-cluster macro blocker inventories."""

from __future__ import annotations

import argparse
import json
import pathlib
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"


class IssueClusterError(RuntimeError):
  pass


def load_config(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def load_obligations_by_operation(config: dict[str, Any]) -> dict[str, dict[str, Any]]:
  obligations = config.get("obligations")
  if not isinstance(obligations, list):
    raise IssueClusterError("config missing 'obligations' array")

  by_operation: dict[str, dict[str, Any]] = {}
  for i, item in enumerate(obligations):
    if not isinstance(item, dict):
      raise IssueClusterError(f"obligations[{i}] is not an object")
    operation = item.get("operation")
    if not isinstance(operation, str) or not operation:
      raise IssueClusterError(f"obligations[{i}] missing non-empty 'operation'")
    if operation in by_operation:
      raise IssueClusterError(f"duplicate obligation operation '{operation}'")
    by_operation[operation] = item
  return by_operation


def load_obligations(config: dict[str, Any]) -> list[dict[str, Any]]:
  obligations = config.get("obligations")
  if not isinstance(obligations, list):
    raise IssueClusterError("config missing 'obligations' array")
  for i, item in enumerate(obligations):
    if not isinstance(item, dict):
      raise IssueClusterError(f"obligations[{i}] is not an object")
  return obligations


def derive_cluster_blockers(
  operations: list[str],
  obligations_by_operation: dict[str, dict[str, Any]],
) -> tuple[list[str], dict[str, int]]:
  counts: dict[str, int] = {}
  for operation in operations:
    obligation = obligations_by_operation[operation]
    blockers = obligation.get("macroSurfaceBlockers", [])
    if not isinstance(blockers, list) or not all(isinstance(b, str) for b in blockers):
      raise IssueClusterError(
        f"operation '{operation}' has invalid macroSurfaceBlockers; expected string list"
      )
    for blocker in blockers:
      counts[blocker] = counts.get(blocker, 0) + 1
  return sorted(counts), dict(sorted(counts.items()))


def validate_issue_clusters(config: dict[str, Any]) -> list[dict[str, Any]]:
  obligations_by_operation = load_obligations_by_operation(config)
  obligations = load_obligations(config)
  raw_clusters = config.get("issueClusters")
  if not isinstance(raw_clusters, list):
    raise IssueClusterError("config missing 'issueClusters' array")

  seen_issues: set[int] = set()
  cluster_meta: dict[int, dict[str, Any]] = {}
  for i, item in enumerate(raw_clusters):
    if not isinstance(item, dict):
      raise IssueClusterError(f"issueClusters[{i}] is not an object")

    issue = item.get("issue")
    title = item.get("title")

    if not isinstance(issue, int):
      raise IssueClusterError(f"issueClusters[{i}] missing integer 'issue'")
    if issue in seen_issues:
      raise IssueClusterError(f"duplicate issue cluster '{issue}'")
    seen_issues.add(issue)

    if not isinstance(title, str) or not title:
      raise IssueClusterError(f"issueClusters[{i}] missing non-empty 'title'")
    cluster_meta[issue] = {"issue": issue, "title": title}

  operations_by_issue: dict[int, list[str]] = {issue: [] for issue in cluster_meta}
  for i, obligation in enumerate(obligations):
    operation = obligation["operation"]
    issue = obligation.get("issue")
    if issue is None:
      continue
    if not isinstance(issue, int):
      raise IssueClusterError(f"obligations[{i}] has non-integer 'issue'")
    if issue not in cluster_meta:
      raise IssueClusterError(
        f"obligations[{i}] references unknown issue cluster '{issue}'"
      )
    if obligation.get("macroMigrated") is not False:
      raise IssueClusterError(
        f"obligation '{operation}' cannot reference open issue #{issue} once macroMigrated!=false"
      )
    blockers = obligation.get("macroSurfaceBlockers")
    if not isinstance(blockers, list) or not blockers or not all(isinstance(b, str) for b in blockers):
      raise IssueClusterError(
        f"obligation '{operation}' missing non-empty string-list 'macroSurfaceBlockers'"
      )
    if operation not in obligations_by_operation:
      raise IssueClusterError(f"obligation '{operation}' missing from operation index")
    operations_by_issue[issue].append(operation)

  reports: list[dict[str, Any]] = []
  for issue, meta in sorted(cluster_meta.items()):
    operations = operations_by_issue[issue]
    if not operations:
      raise IssueClusterError(f"issue #{issue} does not cover any obligations")
    if len(operations) != len(set(operations)):
      raise IssueClusterError(f"issue #{issue} contains duplicate operations")
    derived_blockers, derived_counts = derive_cluster_blockers(operations, obligations_by_operation)
    reports.append({
      "issue": issue,
      "title": meta["title"],
      "operations": operations,
      "macroSurfaceBlockers": derived_blockers,
      "blockerCoverageCounts": derived_counts,
    })

  return sorted(reports, key=lambda item: item["issue"])


def parser() -> argparse.ArgumentParser:
  p = argparse.ArgumentParser(
    description="Validate issue-cluster macro blocker inventories"
  )
  p.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  p.add_argument("--json-out", type=pathlib.Path)
  return p


def main() -> None:
  args = parser().parse_args()
  config = load_config(args.config)
  clusters = validate_issue_clusters(config)

  report = {
    "config": str(args.config.relative_to(ROOT) if args.config.is_absolute() else args.config),
    "clusterCount": len(clusters),
    "issueClusters": clusters,
  }

  if args.json_out:
    args.json_out.parent.mkdir(parents=True, exist_ok=True)
    with args.json_out.open("w", encoding="utf-8") as f:
      json.dump(report, f, indent=2, sort_keys=True)
      f.write("\n")

  print("issue blocker clusters check: OK")
  print(f"clusters: {len(clusters)}")
  for cluster in clusters:
    print(
      f"  #{cluster['issue']}: "
      f"{len(cluster['operations'])} ops, "
      f"{len(cluster['macroSurfaceBlockers'])} blocker families"
    )


if __name__ == "__main__":
  try:
    main()
  except IssueClusterError as e:
    print(f"issue blocker clusters check failed: {e}", file=sys.stderr)
    sys.exit(1)
  except FileNotFoundError as e:
    print(f"issue blocker clusters check failed: {e}", file=sys.stderr)
    sys.exit(1)
