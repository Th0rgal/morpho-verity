#!/usr/bin/env python3
"""Validate no-arg Python check scripts succeed from another working directory."""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPTS_DIR = ROOT / "scripts"
PARITY_TARGET_PATH = ROOT / "config" / "parity-target.json"
SELF_NAME = pathlib.Path(__file__).name
DEFAULT_SKIP_SCRIPTS = (
  "check_no_arg_check_scripts.py",
  "check_prepared_verity_artifact_bundle.py",
)


class NoArgCheckScriptsError(RuntimeError):
  """Raised when the no-arg Python check-script guard cannot complete safely."""


def fail(message: str) -> None:
  print(f"no-arg-check-scripts check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_parity_target(path: pathlib.Path) -> tuple[str, str]:
  try:
    data = json.loads(path.read_text(encoding="utf-8"))
  except OSError as exc:
    raise NoArgCheckScriptsError(f"failed to read parity target {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise NoArgCheckScriptsError(f"failed to decode parity target {path} as UTF-8") from exc
  except json.JSONDecodeError as exc:
    raise NoArgCheckScriptsError(f"failed to parse parity target {path} as JSON") from exc
  if not isinstance(data, dict):
    raise NoArgCheckScriptsError(f"parity target root must be a JSON object: {path}")
  solc = data.get("solc")
  if not isinstance(solc, dict):
    raise NoArgCheckScriptsError(f"parity target missing `solc` object: {path}")
  version = solc.get("version")
  commit = solc.get("commit")
  if not isinstance(version, str) or not version:
    raise NoArgCheckScriptsError(f"parity target missing non-empty `solc.version`: {path}")
  if not isinstance(commit, str) or not commit:
    raise NoArgCheckScriptsError(f"parity target missing non-empty `solc.commit`: {path}")
  return version, commit


def collect_no_arg_check_scripts(scripts_dir: pathlib.Path, *, skip_scripts: set[str]) -> list[pathlib.Path]:
  scripts: list[pathlib.Path] = []
  for path in sorted(scripts_dir.glob("check_*.py")):
    if not path.is_file():
      continue
    if path.name in skip_scripts:
      continue
    scripts.append(path)
  return scripts


def build_script_env(
  script_path: pathlib.Path,
  *,
  parity_target_path: pathlib.Path,
  base_env: dict[str, str],
  temp_dir: pathlib.Path,
) -> dict[str, str]:
  env = dict(base_env)
  if script_path.name != "check_parity_target.py":
    return env

  version, commit = read_parity_target(parity_target_path)
  fake_bin = temp_dir / "fake-bin"
  fake_bin.mkdir(parents=True)
  solc_path = fake_bin / "solc"
  solc_path.write_text(
    "\n".join([
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "if [[ \"${1:-}\" != \"--version\" ]]; then",
      "  echo \"fake solc only supports --version\" >&2",
      "  exit 64",
      "fi",
      "echo 'solc, the solidity compiler commandline interface'",
      f"echo 'Version: {version}+commit.{commit}.Linux.g++'",
      "",
    ]),
    encoding="utf-8",
  )
  solc_path.chmod(0o755)
  existing_path = env.get("PATH", "")
  env["PATH"] = str(fake_bin) if not existing_path else f"{fake_bin}{os.pathsep}{existing_path}"
  return env


def run_check_script(
  script_path: pathlib.Path,
  *,
  parity_target_path: pathlib.Path,
  base_env: dict[str, str],
  runner_cwd: pathlib.Path,
  temp_dir: pathlib.Path,
) -> subprocess.CompletedProcess[str]:
  env = build_script_env(
    script_path,
    parity_target_path=parity_target_path,
    base_env=base_env,
    temp_dir=temp_dir,
  )
  return subprocess.run(
    [sys.executable, str(script_path)],
    cwd=runner_cwd,
    env=env,
    capture_output=True,
    text=True,
    check=False,
  )


def main(argv: list[str] | None = None) -> int:
  parser = argparse.ArgumentParser(
    description="Validate Python check scripts with no required args remain cwd-independent"
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=SCRIPTS_DIR,
    help="Path to the scripts directory",
  )
  parser.add_argument(
    "--parity-target",
    type=pathlib.Path,
    default=PARITY_TARGET_PATH,
    help="Path to config/parity-target.json used to build the fake solc shim",
  )
  parser.add_argument(
    "--skip-script",
    action="append",
    default=[],
    help="Script basename to skip (may be repeated)",
  )
  args = parser.parse_args(argv)

  scripts_dir = args.scripts_dir.resolve()
  parity_target_path = args.parity_target.resolve()
  skip_scripts = set(DEFAULT_SKIP_SCRIPTS)
  skip_scripts.update(args.skip_script)
  skip_scripts.add(SELF_NAME)

  if not scripts_dir.is_dir():
    fail(f"scripts directory does not exist: {scripts_dir}")

  scripts = collect_no_arg_check_scripts(scripts_dir, skip_scripts=skip_scripts)
  if not scripts:
    fail(f"no runnable no-arg check scripts found under {scripts_dir}")

  failures: list[tuple[str, int, str, str]] = []
  base_env = dict(os.environ)
  with tempfile.TemporaryDirectory() as temp_dir_name:
    temp_dir = pathlib.Path(temp_dir_name)
    runner_cwd = temp_dir / "runner"
    runner_cwd.mkdir()
    for script_path in scripts:
      proc = run_check_script(
        script_path,
        parity_target_path=parity_target_path,
        base_env=base_env,
        runner_cwd=runner_cwd,
        temp_dir=temp_dir / script_path.stem,
      )
      if proc.returncode == 0:
        continue
      failures.append((script_path.name, proc.returncode, proc.stdout.strip(), proc.stderr.strip()))

  if failures:
    lines = []
    for name, code, stdout, stderr in failures:
      detail = [f"{name} exited with status {code}"]
      if stdout:
        detail.append(f"stdout={stdout}")
      if stderr:
        detail.append(f"stderr={stderr}")
      lines.append("; ".join(detail))
    fail(" | ".join(lines))

  print(f"no-arg-check-scripts: checked={len(scripts)} skipped={len(skip_scripts)}")
  print("no-arg-check-scripts check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
