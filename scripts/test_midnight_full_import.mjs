#!/usr/bin/env node
// Full-generation tests. Compiler/parity checks remain separate mandatory gates.
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { mkdtempSync, readFileSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve } from 'node:path'
import { spawnSync } from 'node:child_process'
import { generateFull, fullLowering } from './import_midnight_full.mjs'
const sha = b => createHash('sha256').update(b).digest('hex')
let count = 0
const test = (name, fn) => { fn(); count++; console.log(`ok ${name}`) }
const first = generateFull(), second = generateFull()

test('complete generation and provenance are byte deterministic', () => {
  assert.equal(first.result.lean, second.result.lean)
  assert.equal(JSON.stringify(first.manifest), JSON.stringify(second.manifest))
  assert.equal(first.manifest.generatedLeanSha256, sha(first.result.lean))
  assert.equal(first.manifest.sourceCommit, first.config.midnightCommit)
  assert.doesNotMatch(first.result.lean, /Midnight\.Contract|Contract\.Midnight\.spec|AdminSliceHybrid/)
})
test('all generated Lean declarations have validated source-span provenance', () => {
  const names = [...first.result.lean.matchAll(/^def ([A-Za-z_][A-Za-z0-9_]*)\b/gm)].map(m => m[1]).sort()
  assert.deepEqual(first.manifest.declarations.map(d => d.generatedDeclaration).sort(), names)
  for (const d of [...first.manifest.declarations, ...first.manifest.fields, first.manifest.constructor]) {
    const o = d.origin, bytes = Buffer.from(first.source.compilation.input.sources[o.file].content)
    const [start, length, sourceId] = o.span.split(':').map(Number)
    assert.equal(sourceId, o.sourceId)
    assert.equal(first.source.files.get(sourceId), o.file)
    assert.equal(o.sourceSha256, sha(bytes))
    assert.equal(o.spanSha256, sha(bytes.subarray(start, start + length)))
    assert.equal(first.source.declaration(o.astNodeId).src, o.span)
  }
})
test('every reachable source function and public getter is emitted', () => {
  const entries = first.result.functionEntries
  const emitted = new Set([...entries.map(e => e.declaration), first.manifest.constructor.declaration])
  for (const row of first.source.callGraph()) assert(emitted.has(row.declaration))
  const abi = first.source.surface().filter(row => row.abi.type === 'function')
  const publics = entries.filter(e => !e.internal)
  assert.deepEqual(publics.map(e => e.declaration).sort((a,b)=>a-b), abi.map(e => e.declaration).sort((a,b)=>a-b))
  const selectors = first.manifest.publicSelectorsInCompilationOrder
  assert.equal(selectors.length, publics.length)
  publics.forEach((f, i) => {
    const row = abi.find(a => a.declaration === f.declaration)
    assert.equal(selectors[i].selector, row.selector)
    assert.equal(BigInt(selectors[i].value), BigInt('0x' + row.selector))
  })
})
test('generated fields are attached to solc storage entries or explicit immutable policy', () => {
  assert.equal(first.result.fields.length, first.manifest.fields.length)
  for (const field of first.manifest.fields) {
    if (field.sourceLayout) {
      assert.equal(field.sourceLayout.declaration, field.declaration)
      assert.equal(field.sourceLayout.slot, first.source.state.get(field.declaration).slot)
    } else {
      assert.equal(first.source.declaration(field.declaration).mutability, 'immutable')
      assert.equal(field.origin.syntheticPolicy, 'immutable-to-reserved-storage-1024-v1')
    }
  }
})
test('unsupported AST statements fail closed during complete generation', () => {
  const source = second.source
  const row = source.callGraph().find(r => source.declaration(r.declaration).kind === 'function')
  const original = source.declaration(row.declaration), changed = structuredClone(original)
  changed.body.statements.unshift({ nodeType: 'TryStatement', id: -999, src: changed.body.src })
  source.nodes.set(changed.id, changed)
  try { assert.throws(() => fullLowering(source).generate(), /unsupported statement TryStatement/) }
  finally { source.nodes.set(original.id, original) }
})
test('CLI --check detects drift and never overwrites it; bad arguments do not touch output', () => {
  const dir = mkdtempSync(resolve(tmpdir(), 'midnight-full-test-'))
  try {
    const file = resolve(dir, 'Full.lean')
    const run = args => spawnSync(process.execPath, ['scripts/import_midnight_full.mjs', ...args], { encoding: 'utf8', timeout: 60000 })
    let r = run(['--output', file]); assert.equal(r.status, 0, r.stderr)
    r = run(['--check', '--output', file]); assert.equal(r.status, 0, r.stderr)
    writeFileSync(file, 'deliberately stale\n')
    r = run(['--check', '--output', file]); assert.notEqual(r.status, 0); assert.match(r.stderr, /artifact drift/)
    assert.equal(readFileSync(file, 'utf8'), 'deliberately stale\n')
    r = run(['--output', file, '--unknown']); assert.notEqual(r.status, 0)
    assert.equal(readFileSync(file, 'utf8'), 'deliberately stale\n')
  } finally { rmSync(dir, { recursive: true, force: true }) }
})
console.log(`${count} full-generation tests passed; no compiler/parity success is implied`)
