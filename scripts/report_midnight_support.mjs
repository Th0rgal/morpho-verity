#!/usr/bin/env node
// Explicit emission-policy ledger, not an AST-keyword colour heuristic.
import { generateFull } from './import_midnight_full.mjs'
import { readFileSync, writeFileSync, existsSync } from 'node:fs'
import { resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import assert from 'node:assert/strict'

export function buildSupportReport(root, generated, registry) {
  const { source, manifest } = generated
  const features = registry.features
  assert.equal(registry.schemaVersion, 1)
  assert(Array.isArray(features))
  const known = new Map(features.map(f => [f.id, f]))
  assert.equal(known.size, features.length, 'duplicate policy ID')
  const backend = readFileSync(resolve(root, 'scripts/compile_midnight_yul.py'), 'utf8').match(/policy='([^']+)'/)
  assert(backend, 'backend must declare a policy ID')
  const required = [...manifest.policies, backend[1]]
  for (const id of required) assert(known.has(id), `unregistered emitted policy: ${id}`)
  for (const f of features) {
    assert(['native','adapted','unsupported'].includes(f.translation), 'unknown translation grade')
    assert(['unproved','assumed','semantics-gap','not-applicable'].includes(f.proof), 'unreviewed proof claim')
    assert(['model-wide','cei-exception','mutability-adaptation','capability','rejection'].includes(f.scope), 'unknown scope')
    if (f.translation === 'adapted') assert(required.includes(f.id), `policy no longer emitted: ${f.id}`)
    assert(f.reason && f.nextStep && f.implementation.length && f.tests.length)
    for (const file of [...f.implementation, ...f.tests]) {
      assert(!file.startsWith('/') && !file.split('/').includes('..'), 'nonportable evidence reference')
      assert(existsSync(resolve(root, file)), `missing evidence reference: ${file}`)
    }
  }
  const entries = [...manifest.declarations.filter(d => d.kind === 'function'), manifest.constructor]
  const graph = new Map(source.callGraph().map(f => [f.declaration, f.calls]))
  const sourceIds = new Set(entries.map(e => e.declaration))
  const own = new Map([...sourceIds].map(id => [id, new Set()]))
  const global = features.filter(f => f.scope === 'model-wide').map(f => f.id)
  for (const e of entries) {
    if (e.ceiPolicy?.allowPostInteractionWrites) own.get(e.declaration).add(e.ceiPolicy.policy)
    if (e.mutability?.sourceMutability !== e.mutability?.modelMutability)
      if (e.mutability) own.get(e.declaration).add(e.mutability.policy)
  }
  const functions = entries.map(e => {
    const visited = new Set()
    const visit = id => {
      if (visited.has(id)) return
      visited.add(id)
      for (const call of graph.get(id) ?? []) {
        if (call.kind === 'FunctionDefinition' && graph.has(call.target)) {
          assert(sourceIds.has(call.target), `unrepresented source dependency ${call.target}`)
          visit(call.target)
        }
      }
    }
    visit(e.declaration)
    const local = [...new Set([...visited].flatMap(id => [...(own.get(id) ?? [])]))].sort()
    for (const id of local) assert(known.has(id), `unregistered local policy ${id}`)
    return { name: e.compilationModelName, declaration: e.declaration, origin: e.origin,
      internal: e.internal, dependencies: [...visited].filter(id => id !== e.declaration).sort((a,b)=>a-b),
      inheritedModelPolicies: global, sourceDependencyPolicies: local,
      translation: 'adapted', proof: 'not-established' }
  })
  return { schemaVersion: 2, sourceCommit: manifest.sourceCommit, compiler: manifest.compiler,
    scopeNote: registry.scopeNote, proofEquivalenceEstablished: false,
    generatedLeanSha256: manifest.generatedLeanSha256,
    features: features.map(f => ({ ...f, affectedEntries: f.translation === 'adapted' ? functions.filter(e => [...e.inheritedModelPolicies,...e.sourceDependencyPolicies].includes(f.id)).map(e=>e.name) : [],
      applicability: ['capability','rejection'].includes(f.scope) ? 'capability/rejection catalogue, not function certification' : f.scope })),
    functions, syntheticDeclarations: manifest.declarations.filter(d=>d.kind !== 'function').length }
}

function main(args) {
  let output = null, json = false
  while (args.length) {
    const arg = args.shift()
    if (arg === '--json' && !json) json = true
    else if (arg === '--output' && !output && args[0] && !args[0].startsWith('--')) output = args.shift()
    else throw Error('usage: report_midnight_support.mjs [--json] [--output PATH]')
  }
  const root = process.cwd()
  const report = buildSupportReport(root, generateFull(root), JSON.parse(readFileSync(resolve(root,'config/midnight-support-policies.json'),'utf8')))
  const text = json || output ? JSON.stringify(report,null,2)+'\n' : [
    'FEATURE | TRANSLATION | PROOF | APPLICABILITY',
    ...report.features.map(f => `${f.feature} | ${f.translation} | ${f.proof} | ${f.scope==='model-wide' ? 'shared model contract' : f.translation==='adapted' ? `${f.affectedEntries.length} entries including callers` : 'catalogue'}`),
    '', report.scopeNote,
    'Use --json for source origins, callers, implementation/test links and follow-up actions.',
  ].join('\n')+'\n'
  if (output) writeFileSync(output,text)
  else process.stdout.write(text)
}
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) main(process.argv.slice(2))
