#!/usr/bin/env node
// Core-only regression probe. Never changes source/frontend/integration artifacts.
import assert from 'node:assert/strict'
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'
import { compilePinnedSource, MidnightSource } from './lib/midnight-source.mjs'
import { MidnightLowering, FunctionContext } from './lib/midnight-lowering.mjs'
const config = JSON.parse(readFileSync('config/midnight-admin-import.json', 'utf8'))
const source = new MidnightSource(compilePinnedSource(process.cwd(), config), config)
const lowering = new MidnightLowering(source)
const declarations = source.callGraph().map(r => source.declaration(r.declaration))
const decl = name => declarations.find(d => d.name === name)
const adminNames = ['setRoleSetter', 'setFeeSetter', 'setFeeClaimer', 'setTickSpacingSetter']
const admin = adminNames.map(n => lowering.compileFunction(decl(n)))
const constructor = lowering.compileFunction(declarations.find(d => d.kind === 'constructor'))
assert.equal(lowering.fields.length, 12)
assert.match(constructor, /\.chainid/)
assert.match(constructor, /\.emit "Constructor"/)
assert.equal((constructor.match(/\.setStorage(?:Addr)? /g) ?? []).length, 2)
admin.forEach((code, i) => {
  assert.match(code, /\.requireError .*OnlyRoleSetter/)
  assert.match(code, new RegExp(`\\.emit "${adminNames[i][0].toUpperCase() + adminNames[i].slice(1)}"`))
  assert(code.indexOf('.requireError') < code.indexOf('.setStorageAddr'))
  assert(code.indexOf('.setStorageAddr') < code.indexOf('.emit'))
})
const collateral = lowering.compileFunction(decl('collateral'))
assert.match(collateral, /"Panic" \[\(\.literal 50\)\]/)
assert.match(collateral, /\.literal 128/)
assert.equal((collateral.match(/\.structMember2/g) ?? []).length, 128)
assert(lowering.fields.some(f => /offset := 128, width := 128/.test(f)))
const consumed = lowering.compileFunction(decl('setConsumed'))
assert.match(consumed, /lazy/)
assert.match(consumed, /\.ite/)
const mul = lowering.compileFunction(decl('mulDivDown'))
assert.match(mul, /\.div/)
assert.match(mul, /"Panic" \[\(\.literal 17\)\]/)
assert.match(mul, /"Panic" \[\(\.literal 18\)\]/)
const literalLookup = lowering.compileFunction(decl('maxSettlementFee'))
assert.match(literalLookup, /isPure := true/)
assert.match(literalLookup, /"Panic" \[\(\.literal 50\)\]/)
assert(!/\.mload|\.mstore|\.calldataload/.test(literalLookup), 'immediate scalar array lookup must not introduce memory scaffolding')
assert.throws(() => lowering.generate(), /not installed|unsupported/)
// Mutation test uses an in-memory AST clone, never pinned Solidity on disk.
const changed = structuredClone(decl('setRoleSetter'))
const guard = changed.body.statements[0].expression.arguments[0]
assert.equal(guard.operator, '=='); guard.operator = '!='
assert.notEqual(lowering.compileFunction(changed), admin[0])
assert.match(lowering.compileFunction(changed), /\.logicalNot \(\.eq/)
const errors = source.surface().filter(r => r.abi.type === 'error').map(r => lowering.errorDef(source.declaration(r.declaration)))
errors.push('{ name := "Panic", params := [.uint256] }')
const eventNames = ['Constructor', 'SetRoleSetter', 'SetFeeSetter', 'SetFeeClaimer', 'SetTickSpacingSetter']
const events = source.surface().filter(r => r.abi.type === 'event' && eventNames.includes(r.abi.name)).map(r => lowering.eventDef(source.declaration(r.declaration)))
const list = xs => `[${xs.map(x => x.replaceAll('\n', ' ')).join(', ')}]`
const getters = source.surface().filter(r => r.abi.type === 'function' && source.declaration(r.declaration).nodeType === 'VariableDeclaration').map(r => lowering.compileGetter(source.declaration(r.declaration)))
assert.equal(getters.length, 12)
const boundary = new FunctionContext(lowering, decl('setRoleSetter'))
const memoryWord = boundary.bind('(.mload (.literal 128))', 'read')
boundary.emit(`.returnValues [${memoryWord}]`)
const lean = `import Compiler.CompilationModel
open Compiler.CompilationModel
def coreProbe : CompilationModel := { name := "CoreProbe", fields := ${list(lowering.fields)}, constructor := some (${constructor.replaceAll('\n', ' ')}), functions := ${list([...admin, ...getters, lowering.compileFunction(decl('collateral')), lowering.compileFunction(decl('mulDivDown')), literalLookup])}, errors := ${list(errors)}, events := ${list(events)} }
#eval coreProbe.functions.length
#eval match Compiler.CompilationModel.compile coreProbe ${list([...adminNames, ...source.surface().filter(r => r.abi.type === 'function' && source.declaration(r.declaration).nodeType === 'VariableDeclaration').map(r => r.abi.name), 'collateral'].map(name => BigInt('0x' + source.surface().find(r => r.abi.type === 'function' && r.abi.name === name).selector).toString()))} with
  | .ok _ => "CORE_COMPILE_OK"
  | .error err => panic! err
#eval match Compiler.CompilationModel.compile { coreProbe with functions := [{ name := "scopedMemoryRead", params := [], returnType := none, returns := [.uint256], body := ${list(boundary.body)} }] } [1] with
  | .ok _ => "SCOPED_MEMORY_COMPILE_OK"
  | .error err => panic! err
`
const dir = mkdtempSync(join(tmpdir(), 'midnight-core-'))
try {
  const path = join(dir, 'CoreProbe.lean'); writeFileSync(path, lean)
  const result = spawnSync('lake', ['env', 'lean', path], { encoding: 'utf8', timeout: 120000, maxBuffer: 10000000 })
  if (result.status !== 0 || !/CORE_COMPILE_OK/.test(result.stdout)) writeFileSync('/tmp/MidnightCoreFailure.lean', lean)
  assert.equal(result.status, 0, `${result.error ?? ''}\n${result.stdout}\n${result.stderr}`)
  assert(!/PANIC at/.test(result.stdout + result.stderr), 'Lean panic is not successful compilation')
  assert.match(result.stdout, /CORE_COMPILE_OK/, 'compiler must return actual success, not an inhabited panic fallback')
  assert.match(result.stdout, /SCOPED_MEMORY_COMPILE_OK/, 'unsafe memory bindings must remain usable after the boundary')
  console.log('PASS: scalar/admin constructor, 12 getters, packed 128-index dispatch, lazy guard, checked arithmetic, AST mutation, fail-closed full generation; Lean:', result.stdout.trim())
} finally { rmSync(dir, { recursive: true, force: true }) }
