#!/usr/bin/env python3
"""Validate no-arg shell check scripts succeed from another working directory."""

from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPTS_DIR = ROOT / "scripts"
DEFAULT_SKIP_SCRIPTS = (
  "check_input_mode_parity.sh",
)

TOOLCHAIN_SHIM_SCRIPTS = {
  "lean": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'Lean (version 4.22.0)'",
      "",
    ]
  ),
  "lake": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'Lake version 5.0.0'",
      "",
    ]
  ),
  "forge": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'forge 1.4.0'",
      "",
    ]
  ),
  "anvil": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'anvil 1.4.0'",
      "",
    ]
  ),
  "solc": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'Version: 0.8.28'",
      "",
    ]
  ),
  "solc-select": "\n".join(
    [
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "if [[ \"${1:-}\" == \"versions\" ]]; then",
      "  echo '0.8.28'",
      "else",
      "  echo 'solc-select'",
      "fi",
      "",
    ]
  ),
}


class NoArgShellCheckScriptsError(RuntimeError):
  """Raised when the no-arg shell check-script guard cannot complete safely."""


def fail(message: str) -> None:
  print(f"no-arg-shell-check-scripts check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def collect_no_arg_shell_check_scripts(
  scripts_dir: pathlib.Path,
  *,
  skip_scripts: set[str],
) -> list[pathlib.Path]:
  scripts: list[pathlib.Path] = []
  for path in sorted(scripts_dir.glob("check_*.sh")):
    if not path.is_file():
      continue
    if path.name in skip_scripts:
      continue
    scripts.append(path)
  return scripts


def build_script_env(
  script_path: pathlib.Path,
  *,
  base_env: dict[str, str],
  temp_dir: pathlib.Path,
) -> dict[str, str]:
  env = dict(base_env)
  if script_path.name != "check_toolchain_readiness.sh":
    return env

  fake_bin = temp_dir / "fake-bin"
  fake_bin.mkdir(parents=True)
  for name, body in TOOLCHAIN_SHIM_SCRIPTS.items():
    shim = fake_bin / name
    shim.write_text(body, encoding="utf-8")
    shim.chmod(0o755)
  existing_path = env.get("PATH", "")
  env["PATH"] = str(fake_bin) if not existing_path else f"{fake_bin}{os.pathsep}{existing_path}"
  return env


def run_check_script(
  script_path: pathlib.Path,
  *,
  base_env: dict[str, str],
  runner_cwd: pathlib.Path,
  temp_dir: pathlib.Path,
) -> subprocess.CompletedProcess[str]:
  env = build_script_env(script_path, base_env=base_env, temp_dir=temp_dir)
  return subprocess.run(
    [str(script_path)],
    cwd=runner_cwd,
    env=env,
    capture_output=True,
    text=True,
    check=False,
  )


def main(argv: list[str] | None = None) -> int:
  parser = argparse.ArgumentParser(
    description="Validate shell check scripts with no required args remain cwd-independent"
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=SCRIPTS_DIR,
    help="Path to the scripts directory",
  )
  parser.add_argument(
    "--skip-script",
    action="append",
    default=[],
    help="Script basename to skip (may be repeated)",
  )
  args = parser.parse_args(argv)

  scripts_dir = args.scripts_dir.resolve()
  skip_scripts = set(DEFAULT_SKIP_SCRIPTS)
  skip_scripts.update(args.skip_script)

  if not scripts_dir.is_dir():
    fail(f"scripts directory does not exist: {scripts_dir}")

  scripts = collect_no_arg_shell_check_scripts(scripts_dir, skip_scripts=skip_scripts)
  if not scripts:
    fail(f"no runnable no-arg shell check scripts found under {scripts_dir}")

  failures: list[tuple[str, int, str, str]] = []
  base_env = dict(os.environ)
  with tempfile.TemporaryDirectory() as temp_dir_name:
    temp_dir = pathlib.Path(temp_dir_name)
    runner_cwd = temp_dir / "runner"
    runner_cwd.mkdir()
    for script_path in scripts:
      proc = run_check_script(
        script_path,
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

  print(f"no-arg-shell-check-scripts: checked={len(scripts)} skipped={len(skip_scripts)}")
  print("no-arg-shell-check-scripts check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
