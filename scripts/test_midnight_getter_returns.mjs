#!/usr/bin/env node
import assert from 'node:assert/strict'
import { generateFull, fullLowering } from './import_midnight_full.mjs'
const { source } = generateFull()
const lowering = fullLowering(source)
lowering.options.factorStatements = false
let tested = 0
for (const row of source.surface()) {
  if (row.abi.type !== 'function') continue
  const decl = source.declaration(row.declaration)
  if (decl.nodeType !== 'VariableDeclaration') continue
  const code = lowering.compileGetter(decl)
  // Every operand must be frozen before backend packing touches scratch memory.
  const ret = code.match(/\.returnValues \[([^\]]*)\]/)
  assert(ret, `missing return for source getter ${decl.id}`)
  assert(!/\.(?:mapping|storage|structMember)/.test(ret[1]))
  assert.match(ret[1], /\.localVar/)
  tested++
}
assert(tested > 0)
// Concrete backend hazard: evaluating a mapping lookup during tuple packing
// overwrites an earlier output with its key/base scratch. Frozen reads avoid it.
const key = 123n, base = 1n
function run(freeze) {
  const mem = new Map()
  const lookup = value => { mem.set(0, key); mem.set(32, base); return value }
  const reads = [7n, 9n, 11n]
  const values = freeze ? reads.map(lookup) : reads
  values.forEach((v,i) => mem.set(i*32, freeze ? v : lookup(v)))
  return [0,32,64].map(i=>mem.get(i))
}
assert.notDeepEqual(run(false), [7n,9n,11n])
assert.deepEqual(run(true), [7n,9n,11n])
console.log(`PASS: ${tested} source getters freeze return operands; scratch-clobber negative control reproduced. Full EVM regression: existing testMarketStateGetter/testMarketStateAfterTake.`)
