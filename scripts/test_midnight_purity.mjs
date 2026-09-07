#!/usr/bin/env node
import assert from 'node:assert/strict'
import { mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'
import { generateFull, fullLowering } from './import_midnight_full.mjs'
import { FunctionContext } from './lib/midnight-lowering.mjs'

const { source, result, manifest } = generateFull()
const entries = manifest.declarations.filter(d => d.kind === 'function')
assert.equal(entries.length, 85)
assert.equal(result.selectors.length, 55)
const pure = entries.filter(d => d.mutability.sourceMutability === 'pure')
assert(pure.length > 0)
for (const entry of entries) {
  const m = entry.mutability
  assert(m)
  if (m.sourceMutability !== m.modelMutability) {
    assert.equal(m.sourceMutability, 'pure')
    assert.equal(m.modelMutability, 'view')
    assert(m.reasons.length)
    assert.match(m.claim, /NOT a proof/)
  }
  const code = result.functions[Number(entry.generatedDeclaration.split('_').at(-1))]
  assert(code.includes(`isPure := ${m.modelMutability === 'pure'}`) || entry.policy === 'solidity-public-state-getter-v1')
}
const id = entries.find(d => d.compilationModelName === 'fn_5399')
assert.equal(id.mutability.modelMutability, 'view')
assert(id.mutability.reasons.some(r => r.startsWith('raw-revert-mechanics:')))
assert.equal(entries.find(d => d.compilationModelName === 'fn_5082').mutability.modelMutability, 'pure')
const helpers = source.callGraph().map(r => source.declaration(r.declaration))
for (const decl of helpers.filter(d => ['min', 'max'].includes(d.name) && d.stateMutability === 'pure')) {
  assert.equal(entries.find(d => d.compilationModelName === `fn_${decl.id}`).mutability.modelMutability, 'pure')
}
assert(helpers.some(d => d.name === 'min'))
// Source compiler ABI, never model metadata, remains the ABI authority.
for (const row of source.surface().filter(r => r.abi.type === 'function')) {
  const entry = entries.find(d => !d.internal && d.declaration === row.declaration)
  assert.equal(entry.mutability.sourceMutability, row.abi.stateMutability)
}
// Exercise capture before factoring, direct/nested helper expressions, and reject
// unrelated reads even when an acknowledged boundary has already been seen.
const owner = fullLowering(source), decl = source.declaration(5399)
owner.statementDefinitions = []; owner.statementOrigins = []
const ctx = new FunctionContext(owner, decl)
ctx.capture(() => owner.options.abiRequire('(.literal 0)', ctx))
assert(ctx.backendPurityReasons.size)
ctx.emit('.letVar "x" (.internalCall "fn_5872" [(.literal 1), (.literal 2), (.literal 3)])')
assert([...ctx.backendPurityReasons].some(r => r.includes('fn_5872')))
assert.throws(() => ctx.emit('.letVar "bad" (.storage "s")'), /unacknowledged/)
assert.throws(() => ctx.emit('.unsafeYul unknownFragment'), /unacknowledged/)
assert.throws(() => ctx.emit('.letVar "bad" (.caller)'), /unacknowledged/)
const plain = new FunctionContext(owner, decl)
plain.emit('.require (.literal 1) "not an opcode: .caller and UnsafeYulFragment.rawRevert"')
assert.equal(plain.backendPurityReasons.size, 0)

// Native validator controls: rawRevert stays an actual mechanics read; view
// still rejects direct and transitive storage writes. No upstream edits.
const dir = mkdtempSync(join(tmpdir(), 'midnight-purity-'))
const file = join(dir, 'Probe.lean')
writeFileSync(file, `import Compiler.CompilationModel
open Compiler.CompilationModel
private def raw : Stmt := .unsafeYul (UnsafeYulFragment.rawRevert (.lit 0) (.lit 0)
  { name := "purity_probe", obligation := "exact empty revert", proofStatus := .assumed })
private def p : FunctionSpec := { name := "p", params := [], returnType := none, body := [raw], isPure := true }
private def v : FunctionSpec := { p with isPure := false, isView := true }
private def w : FunctionSpec := { v with body := [raw, .setStorage "s" (.literal 1)] }
private def callee : FunctionSpec := { name := "callee", params := [], returnType := none, isInternal := true, body := [.setStorage "s" (.literal 1)] }
private def caller : FunctionSpec := { v with body := [.internalCall "callee" []] }
#eval do
  let reject := fun (r : Except String Unit) => match r with | .error _ => true | .ok _ => false
  let accept := fun (r : Except String Unit) => match r with | .ok _ => true | .error _ => false
  if stmtReadsStateOrEnv raw &&
     reject (validateFunctionSpecMutability (inferFunctionEffects [p]) p) &&
     accept (validateFunctionSpecMutability (inferFunctionEffects [v]) v) &&
     reject (validateFunctionSpecMutability (inferFunctionEffects [w]) w) &&
     reject (validateFunctionSpecMutability (inferFunctionEffects [callee, caller]) caller) then
    IO.println "PURITY_NATIVE_CONTROLS_OK"
  else throw (IO.userError "purity controls failed")
`)
const lean = spawnSync('lake', ['env', 'lean', file], { encoding: 'utf8', timeout: 120000 })
assert.equal(lean.status, 0, lean.stdout + lean.stderr)
assert(!/PANIC|sorry/.test(lean.stdout + lean.stderr))
assert.match(lean.stdout, /PURITY_NATIVE_CONTROLS_OK/)
console.log(`PASS: ${pure.length} source-pure entries; ${pure.filter(d => d.mutability.modelMutability === 'view').length} explicit model-view boundaries; source ABI intact; pre-factoring capture; native direct/transitive writes rejected; ${file}`)
