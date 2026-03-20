#!/usr/bin/env python3
"""Post-process Morpho.yul to replace inline keccak256(0,160) market params hashing
with a safe function that avoids clobbering the free memory pointer at 0x40."""

import re
import sys

def fix_yul(content: str) -> str:
    # Pattern: 5 consecutive mstore at offsets 0,32,64,96,128 followed by keccak256(0, 160)
    pattern = (
        r'mstore\(0, (marketParams_0)\)\s*\n'
        r'\s*mstore\(32, (marketParams_1)\)\s*\n'
        r'\s*mstore\(64, (marketParams_2)\)\s*\n'
        r'\s*mstore\(96, (marketParams_3)\)\s*\n'
        r'\s*mstore\(128, (marketParams_4)\)\s*\n'
        r'\s*let (id\w*) := keccak256\(0, 160\)'
    )

    def replacement(m):
        p0, p1, p2, p3, p4, var = m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        return f'let {var} := keccakMarketParams({p0}, {p1}, {p2}, {p3}, {p4})'

    content = re.sub(pattern, replacement, content)

    # Add keccakMarketParams function if not present
    if 'function keccakMarketParams(' not in content:
        func_def = '''            function keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv) -> id {
                mstore(0x240, loanToken)
                mstore(0x260, collateralToken)
                mstore(0x280, oracle)
                mstore(0x2a0, irm)
                mstore(0x2c0, lltv)
                id := keccak256(0x240, 0xa0)
            }'''
        # Insert after the last existing helper function (before the main dispatch)
        # Find the first "switch" or "if" after the function definitions
        # Insert right before the main code block
        content = content.replace(
            '            switch shr(224, calldataload(0))',
            func_def + '\n            switch shr(224, calldataload(0))',
            1
        )

    return content


if __name__ == '__main__':
    path = sys.argv[1]
    with open(path, 'r') as f:
        content = f.read()
    fixed = fix_yul(content)
    with open(path, 'w') as f:
        f.write(fixed)
    # Count replacements
    import re as re2
    orig_count = content.count('keccak256(0, 160)')
    new_count = fixed.count('keccak256(0, 160)')
    print(f"Replaced {orig_count - new_count} inline keccak256(0,160) patterns with keccakMarketParams calls")
