#!/usr/bin/env node
// Conservative review inventory, not a semantic proof or a new compiler.
import { generateFull } from './import_midnight_full.mjs'
import { walk } from './lib/midnight-source.mjs'
import { writeFileSync } from 'node:fs'
import assert from 'node:assert/strict'
const { source, manifest } = generateFull()
const graph = source.callGraph()
const ids = new Set(graph.map(x => x.declaration))
const rows = graph.map(row => {
  const decl = source.declaration(row.declaration)
  const reasons = new Set()
  walk(decl, node => {
    if (node.nodeType === 'InlineAssembly') reasons.add('inline-Yul semantics boundary')
    if (node.nodeType === 'WhileStatement') reasons.add('specialized bounded-loop justification')
    if (node.nodeType === 'MemberAccess' && node.memberName === 'code') reasons.add('external-code memory boundary')
  })
  for (const p of [...(decl.parameters?.parameters ?? []), ...(decl.returnParameters?.parameters ?? [])]) {
    const t = p.typeDescriptions?.typeString ?? ''
    if (/\[|\bstruct\b|\bbytes\b|\bstring\b/.test(t)) reasons.add('composite ABI/memory representation')
  }
  for (const call of row.calls) {
    if (call.target != null && !ids.has(call.target) && call.kind === 'FunctionDefinition') reasons.add('external typed-call boundary')
    if (call.kind === 'builtin-or-low-level') reasons.add('builtin/low-level call requires individual review')
  }
  return { declaration: row.declaration, name: decl.name || '<constructor>', origin: row.origin,
    dependencies: [...new Set(row.calls.filter(x => ids.has(x.target)).map(x => x.target))].sort((a,b)=>a-b),
    directReviewReasons: [...reasons].sort() }
})
const byId = new Map(rows.map(r => [r.declaration,r]))
for (const row of rows) {
  const seen = new Set()
  const visit = id => { if (seen.has(id)) return; seen.add(id); for (const d of byId.get(id).dependencies) visit(d) }
  visit(row.declaration)
  row.boundaryDependencies = [...seen].filter(id => byId.get(id).directReviewReasons.length).sort((a,b)=>a-b)
  row.status = row.boundaryDependencies.length ? 'excluded-from-unconditional-proof-scope-pending-boundary-review' : 'candidate-for-focused-review-not-yet-proved'
}
assert.equal(rows.length, ids.size)
for (const row of rows) for (const id of row.dependencies) assert(byId.has(id), 'unresolved source dependency')
const report = { schemaVersion: 1, sourceCommit: manifest.sourceCommit, compiler: manifest.compiler,
  policy: 'Conservative source-AST triage. No approved proof scope yet. No claim that excluded functions are unmodelable. Bytecode stack limits do not determine semantic scope.',
  fullReplacementApproved: false, sourceToModelEquivalenceProved: false, functions: rows }
const text = JSON.stringify(report, null, 2) + '\n'
const args = process.argv.slice(2)
if (args.length === 2 && args[0] === '--output') writeFileSync(args[1], text)
else if (args.length) throw Error('usage: report_midnight_support.mjs [--output PATH]')
else process.stdout.write(text)
