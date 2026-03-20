#!/usr/bin/env python3
"""Unit tests for uniquify_yul_shadows.py."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import textwrap
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent / "uniquify_yul_shadows.py"


def run_uniquify(source: str) -> str:
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yul", delete=False) as inp:
        inp.write(source)
        inp.flush()
        out_path = inp.name + ".out"
        result = subprocess.run(
            [sys.executable, str(SCRIPT), "--input", inp.name, "--output", out_path],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0, f"uniquify failed: {result.stderr}"
        return Path(out_path).read_text(encoding="utf-8")


def test_no_shadows():
    src = textwrap.dedent("""\
        {
            let x := 1
            let y := 2
        }
    """)
    out = run_uniquify(src)
    assert "let x := 1" in out
    assert "let y := 2" in out


def test_simple_shadow():
    src = textwrap.dedent("""\
        {
            let __ite_cond := gt(a, 0)
            if __ite_cond {
                {
                    let __ite_cond := gt(b, 0)
                    if __ite_cond {
                        mstore(0, 1)
                    }
                }
            }
        }
    """)
    out = run_uniquify(src)
    # The second __ite_cond should be renamed
    lines = out.splitlines()
    ite_decls = [l for l in lines if "let __ite_cond" in l]
    names = set()
    for decl in ite_decls:
        # Extract variable name after "let "
        part = decl.strip().split(":=")[0].replace("let ", "").strip()
        names.add(part)
    assert len(names) == 2, f"Expected 2 unique names, got {names}"


def test_dead_ignored_lets_removed():
    src = textwrap.dedent("""\
        {
            let marketParams_0 := calldataload(4)
            let _ignoredMarketParams := marketParams
        }
    """)
    out = run_uniquify(src)
    assert "_ignoredMarketParams" not in out
    assert "marketParams_0" in out


def test_sibling_blocks_no_rename():
    src = textwrap.dedent("""\
        {
            {
                let x := 1
            }
            {
                let x := 2
            }
        }
    """)
    out = run_uniquify(src)
    # Sibling blocks can reuse names — no rename needed
    lines = [l for l in out.splitlines() if "let x" in l]
    assert len(lines) == 2


def main() -> int:
    tests = [
        test_no_shadows,
        test_simple_shadow,
        test_dead_ignored_lets_removed,
        test_sibling_blocks_no_rename,
    ]
    passed = 0
    failed = 0
    for test in tests:
        try:
            test()
            print(f"  ✓ {test.__name__}")
            passed += 1
        except AssertionError as e:
            print(f"  ✗ {test.__name__}: {e}")
            failed += 1
        except Exception as e:
            print(f"  ✗ {test.__name__}: {type(e).__name__}: {e}")
            failed += 1
    print(f"test_uniquify_yul_shadows: {passed} passed, {failed} failed")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
