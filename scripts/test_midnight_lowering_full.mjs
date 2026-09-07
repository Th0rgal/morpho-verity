#!/usr/bin/env node
// Full native CompilationModel gate; retains the Lean artifact/log on failure.
import assert from 'node:assert/strict'
import { readFileSync, writeFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { fullLowering } from './import_midnight_full.mjs'
import { MidnightSource, compilePinnedSource } from './lib/midnight-source.mjs'
const config = JSON.parse(readFileSync('config/midnight-full-import.json', 'utf8'))
const source = new MidnightSource(compilePinnedSource(process.cwd(), config), config)
const lowering = fullLowering(source)
lowering.options.factorStatements = true
const result = lowering.generate()
// Exact expanded-text equivalence guards optional elaboration factoring.
const plainLowering = fullLowering(source)
plainLowering.options.factorStatements = false
const plain = plainLowering.generate()
const statements = new Map()
const expand = text => text.replace(/\bsourceStatement_\d+\b/g, name => statements.get(name) ?? assert.fail(`Missing ${name}`))
for (const match of result.lean.matchAll(/^def (sourceStatement_\d+) : Stmt := (.*)$/gm)) statements.set(match[1], expand(match[2]))
const normalize = text => text.replaceAll('\n', ' ')
result.functions.forEach((f, i) => assert.equal(normalize(expand(f)), normalize(plain.functions[i])))
assert.equal(normalize(expand(result.constructor)), normalize(plain.constructor))
assert.equal(result.statementOrigins.length, statements.size)
const abi = new Map(source.surface().filter(r => r.abi.type === 'function').map(r => [r.abi.name, r]))
const external = result.functions.filter(f => !f.includes('isInternal := true'))
const selectors = external.map(f => {
  const name = /^\{ name := "([^"]+)"/.exec(f)?.[1]
  assert(abi.has(name), `Missing authoritative source ABI selector: ${name}`)
  return BigInt('0x' + abi.get(name).selector).toString()
})
assert.equal(result.functions.length, 85)
assert.equal(selectors.length, 55)
const path = '/tmp/MidnightFullNativeCompile.lean'
writeFileSync(path, result.lean + `\n#eval match Compiler.CompilationModel.compile Midnight.Generated.Full.spec [${selectors.join(',')}] .osaka with\n  | .ok _ => "FULL_NATIVE_COMPILE_OK: 85 entries / 55 source selectors"\n  | .error err => "FULL_NATIVE_COMPILE_ERROR: " ++ err\n`)
console.log(`Generated ${path}: ${result.functions.length} entries, ${selectors.length} source selectors`)
const lean = spawnSync('lake', ['env', 'lean', '-s', '65536', '--profile', path], { encoding: 'utf8', timeout: 540000, maxBuffer: 10000000 })
writeFileSync('/tmp/midnight-full-native-compile.log', (lean.stdout ?? '') + (lean.stderr ?? ''))
console.log(lean.stdout, lean.stderr)
assert.equal(lean.status, 0, String(lean.error ?? 'Lean failed'))
assert(!/PANIC|sorry/.test(lean.stdout + lean.stderr))
assert.match(lean.stdout, /FULL_NATIVE_COMPILE_OK:/)
