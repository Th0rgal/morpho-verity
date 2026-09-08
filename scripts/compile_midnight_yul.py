#!/usr/bin/env python3
"""Pinned Midnight backend policy, not a general memory-safety verifier.

Reserve all statically addressed scratch/return words below the optimizer's
spill region. Dynamic writes must belong to the reviewed monotonic ABI allocator
contract (see the Midnight section of docs/TRUST_BOUNDARIES.md). This remains an explicit
consumer-owned assumption, not a proof supplied by Verity.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

SOLC_SHA = 'd40adc6f9fdbb22a97d32a02fa05688bf2ee7886affc48c9851b0afd4a726b39'

def digest(data):
    return hashlib.sha256(data).hexdigest()

def prepare(text):
    if 'memoryguard(' in text:
        raise ValueError('expected fresh unguarded compiler output')
    # New raw memory mechanisms require a new review, never inherit this policy.
    if re.search(r'\b(?:mstore8|mcopy|codecopy)\s*\(', text):
        raise ValueError('unreviewed memory-writing mechanism')
    constants = [int(x, 0) + 32 for x in re.findall(r'\bmstore\((0x[0-9a-fA-F]+|[0-9]+),', text)]
    scratch = ((max([128, *constants]) + 31) // 32) * 32
    if scratch > 4096:
        raise ValueError('unexpected static scratch expansion: review required')
    text, count = re.subn(r'\bmstore\(64, 128\)', f'mstore(64, memoryguard({scratch}))', text)
    if not count:
        raise ValueError('missing reviewed allocator initializer')
    # The deployment copy used to overwrite the entire scratch/spill region.
    # It terminates immediately and can instead use the current free pointer.
    old = 'datacopy(0, dataoffset("runtime"), datasize("runtime"))\n        return(0, datasize("runtime"))'
    if text.count(old) != 1 or len(re.findall(r'\bdatacopy\(', text)) != 1:
        raise ValueError('unreviewed deployment copy shape')
    text = text.replace(old, 'datacopy(mload(64), dataoffset("runtime"), datasize("runtime"))\n        return(mload(64), datasize("runtime"))')
    return text, scratch, count

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input', required=True)
    parser.add_argument('--output-dir', required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    solc = root / '.cache/solc-0.8.34+commit.80d5c536'
    if digest(solc.read_bytes()) != SOLC_SHA:
        raise ValueError('solc checksum mismatch')
    source = Path(args.input).read_text()
    transformed, scratch, count = prepare(source)
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    yul = out / 'Midnight.stack.yul'
    yul.write_text(transformed)
    cmd = [str(solc), '--strict-assembly', '--optimize', '--yul-optimizations', 'u', '--evm-version', 'osaka', '--bin', str(yul)]
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    if result.stderr:
        print(result.stderr)
    matches = re.findall(r'Binary representation:\s*\n([0-9a-fA-F]+)', result.stdout)
    if len(matches) != 1:
        raise ValueError('missing or ambiguous bytecode output')
    raw = bytes.fromhex(matches[0])
    if not raw:
        raise ValueError('empty bytecode')
    (out / 'Midnight.bin').write_text(matches[0] + '\n')
    (out / 'Midnight.bin.raw').write_bytes(raw)
    report = dict(policy='midnight-reviewed-allocator-solc-stack-v1', proofStatus='assumed-memory-contract-not-a-theorem',
                  optimizerSteps='u', evmVersion='osaka', scratchBytes=scratch, initializers=count,
                  inputYulSha256=digest(source.encode()), loweredYulSha256=digest(transformed.encode()),
                  solcSha256=SOLC_SHA, bytecodeSha256=digest(raw), creationBytes=len(raw),
                  standardEthereumDeploymentSizeCompliant=False,
                  note='Size and gas limits are not established by Foundry parity. Dynamic allocation correctness remains a translation obligation.')
    (out / 'Midnight.backend.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))

if __name__ == '__main__':
    main()
