#!/usr/bin/env node
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { generateFull } from './import_midnight_full.mjs'
import { buildSupportReport } from './report_midnight_support.mjs'
const root = process.cwd(), g = generateFull(root)
const registry = JSON.parse(readFileSync('config/midnight-support-policies.json','utf8'))
const run = (data=registry, generated=g) => buildSupportReport(root,generated,data)
const report=run()
assert.deepEqual(report,run())
assert.equal(report.functions.length,g.manifest.declarations.filter(x=>x.kind==='function').length+1)
assert(report.functions.every(f=>f.inheritedModelPolicies.includes('midnight-reviewed-allocator-solc-stack-v1')))
assert(report.functions.some(f=>f.sourceDependencyPolicies.includes('source-pure-backend-conservative-view-v1')))
assert.throws(()=>run({...registry,features:registry.features.slice(1)}),/unregistered/)
assert.throws(()=>run({...registry,features:[...registry.features,registry.features[0]]}),/duplicate/)
const broken=structuredClone(registry);broken.features[0].tests=['scripts/not-a-real-test.mjs']
assert.throws(()=>run(broken),/missing evidence/)
const future={...g,manifest:{...g.manifest,policies:[...g.manifest.policies,'new-unreviewed-adapter']}}
assert.throws(()=>run(registry,future),/unregistered/)
const sourceClosure = report.functions.find(f=>f.dependencies.length && f.sourceDependencyPolicies.length)
assert(sourceClosure,'callee obligations must propagate to at least one caller')
console.log('PASS: deterministic policy report, all entries/constructor, conservative shared boundaries, dependency propagation, missing/duplicate/unknown policy and missing test rejection')
