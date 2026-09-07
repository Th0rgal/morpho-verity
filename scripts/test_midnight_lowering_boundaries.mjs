#!/usr/bin/env node
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { compilePinnedSource, MidnightSource, walk } from './lib/midnight-source.mjs'
import { MidnightLowering, FunctionContext } from './lib/midnight-lowering.mjs'
const config = JSON.parse(readFileSync('config/midnight-admin-import.json', 'utf8'))
const source = new MidnightSource(compilePinnedSource(process.cwd(), config), config)
const lowering = new MidnightLowering(source)
const loops = []
for (const row of source.callGraph()) {
  const decl = source.declaration(row.declaration)
  walk(decl.body, n => { if (n.nodeType === 'WhileStatement') loops.push({ decl, node: n }) })
}
assert(loops.length > 0)
let negatives = 0
for (const { decl, node } of loops) {
  const ctx = new FunctionContext(lowering, decl)
  assert.equal(ctx.bitmapBound(node), 128)
  const bitmap = node.condition.leftExpression.referencedDeclaration
  const bit = node.body.statements[0].declarations[0].id
  for (const target of [bitmap, bit]) {
    for (const op of ['=', '++', '--', 'delete']) {
      const changed = structuredClone(node)
      const tuple = { nodeType: 'TupleExpression', components: [null, { nodeType: 'TupleExpression', components: [{ nodeType: 'Identifier', referencedDeclaration: target }] }] }
      changed.body.statements.splice(1, 0, { nodeType: 'ExpressionStatement', expression: op === '=' ? { nodeType: 'Assignment', operator: op, leftHandSide: tuple, rightHandSide: { nodeType: 'Literal', kind: 'number', value: '0' } } : { nodeType: 'UnaryOperation', operator: op, subExpression: tuple } })
      assert.throws(() => ctx.bitmapBound(changed), /mutation|additional writes/)
      negatives++
    }
  }
  const first = node.body.statements[0]
  const pick = source.resolve(first.initialValue.expression, 'FunctionDefinition')
  const clear = source.resolve(node.body.statements.at(-1).expression.rightHandSide.expression, 'FunctionDefinition')
  for (const target of [first.declarations[0], pick.returnParameters.parameters[0], clear.parameters.parameters[1]]) {
    const originalType = source.type.bind(source)
    source.type = n => n.id === target.id ? { kind: 'scalar', abi: 'uint4', bits: 4, signed: false } : originalType(n)
    try { assert.throws(() => ctx.bitmapBound(node), /full index range/); negatives++ } finally { delete source.type }
  }
}
const ctx = new FunctionContext(lowering, loops[0].decl)
ctx.emit('.mstore (.literal 64) (.literal 128)')
ctx.emit('.letVar "ordinary" (.literal 1)')
ctx.emit('.ite (.mload (.literal 128)) [.setStorage "business" (.literal 1)] []')
assert.match(ctx.body[0], /^\.unsafeBlock .*source-low-level-boundary-v1/)
assert.equal(ctx.body[1], '.letVar "ordinary" (.literal 1)')
assert.match(ctx.body[2], /^\.letVar .*\(\.literal 0\)$/)
assert.match(ctx.body[3], /^\.unsafeBlock .*\.assignVar /)
assert.match(ctx.body[4], /^\.ite \(\.localVar /)
assert(!ctx.body[4].includes('unsafeBlock'))
console.log(`PASS: ${loops.length} pinned bitmap loops; ${negatives} recursive-lvalue/index-width negative mutations; statement-local unsafe memory boundaries`)
