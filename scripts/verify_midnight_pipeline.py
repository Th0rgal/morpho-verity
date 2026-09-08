#!/usr/bin/env python3
"""Run Midnight checks and save portable, hash-bound evidence. Never imports old logs."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
from typing import Any

ROOT = Path(__file__).resolve().parent.parent

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def parse_suite(text):
    rows = re.findall(r'Ran \d+ test suites[^\n]*?: (\d+) tests passed, (\d+) failed, (\d+) skipped \((\d+) total tests\)', text)
    if not rows:
        raise ValueError('missing completed Foundry suite summary')
    passed, failed, skipped, total = map(int, rows[-1])
    if failed or skipped or passed != total or total < 373:
        raise ValueError(f'incomplete/failing Midnight suite: {rows[-1]}')
    return dict(passed=passed, failed=failed, skipped=skipped, total=total)

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out-dir', default='out/midnight')
    args = parser.parse_args()
    out = Path(args.out_dir).resolve()
    out.mkdir(parents=True, exist_ok=True)
    evidence: dict[str, Any] = dict(schemaVersion=1, complete=False, commands=[], equivalenceProved=False,
                    revision=subprocess.check_output(['git','rev-parse','HEAD'], cwd=ROOT, text=True).strip())
    def save():
        target = out/'evidence.json'
        temporary = out/'evidence.json.tmp'
        temporary.write_text(json.dumps(evidence,indent=2)+'\n')
        temporary.replace(target)
    save()  # Replace stale success before any work, including interrupted runs.
    env = os.environ.copy()
    env.pop('CI', None)  # Same Mathlib says-mode policy as existing Lean CI job.
    def run(name, command, extra=None, timeout=5100):
        log = out / f'{name}.log'
        item: dict[str, Any] = dict(name=name, command=command, log=log.name, returncode=None)
        evidence['commands'].append(item)
        save()
        try:
            with log.open('w') as sink:
                result = subprocess.run(command, cwd=ROOT, env={**env, **(extra or {})}, stdout=sink, stderr=subprocess.STDOUT, timeout=timeout)
            item['returncode'] = result.returncode
            if result.returncode:
                raise RuntimeError(f'{name} failed; see {log}')
        finally:
            if log.exists():
                item['logSha256'] = sha(log)
            save()
        return log.read_text()
    try:
        run('generation-check', ['node','scripts/import_midnight_full.mjs','--check'])
        run('support', ['node','scripts/report_midnight_support.mjs','--output',str(out/'support.json')])
        run('lean-build', ['lake','build'])
        artifact_dir = out
        run('prepare', ['bash','scripts/prepare_midnight_artifact.sh'], {'MORPHO_MIDNIGHT_OUT_DIR':str(artifact_dir)})
        manifest=json.loads((ROOT/'morpho-midnight-verity/Midnight/Generated/FullModel.manifest.json').read_text())
        evidence['sourceCommit']=manifest['sourceCommit']
        evidence['compiler']=manifest['compiler']
        evidence['generatedLeanSha256']=manifest['generatedLeanSha256']
        evidence['backend']=json.loads((artifact_dir/'Midnight.backend.json').read_text())
        raw = artifact_dir/'Midnight.bin.raw'
        before=sha(raw)
        # Runner discovers solc on PATH. Use the exact compiler materialized and
        # checksum-verified by preparation, avoiding unrelated global versions.
        with tempfile.TemporaryDirectory(prefix='midnight-solc-') as tools:
            (Path(tools)/'solc').symlink_to(ROOT/'.cache/solc-0.8.34+commit.80d5c536')
            log=run('parity',['bash','scripts/run_morpho_midnight_parity.sh'],{
                'MORPHO_MIDNIGHT_PARITY_MODE':'verity','MORPHO_MIDNIGHT_ARTIFACT_RAW':str(raw),
                'PATH':tools+os.pathsep+env.get('PATH','')})
        evidence['suite']=parse_suite(log)
        if sha(raw)!=before or sha(ROOT/'artifacts/midnight/Midnight.bin.raw')!=before:
            raise ValueError('tested bytecode changed or deployment selection differs')
        evidence['testedBytecodeSha256']=before
        evidence['artifacts']={p.name:sha(p) for p in artifact_dir.glob('Midnight.*') if p.is_file()}
        evidence['supportReportSha256']=sha(out/'support.json')
        evidence['complete']=True
    except Exception as exc:
        evidence['error']=str(exc)
        raise
    finally:
        # JSON and referenced logs are uploaded together, even on a failing run.
        save()

if __name__ == '__main__':
    main()
