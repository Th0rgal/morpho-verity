#!/usr/bin/env node
/** Tiny actual native CM compile regression, not a full-model compile or proof.
 * Intrinsic EVM semantics are assumed; this checks fork and CEI classification.
 */
import assert from 'node:assert/strict'
import {spawnSync} from 'node:child_process'
import {mkdtempSync,writeFileSync,rmSync} from 'node:fs'
import {tmpdir} from 'node:os'
import {resolve} from 'node:path'
import {walk} from './lib/midnight-source.mjs'
import {lowerAssembly,supportDefinitions} from './lib/midnight-builtins.mjs'
const root=resolve(import.meta.dirname,'..')
const input={language:'Solidity',sources:{'CEI.sol':{content:`pragma solidity ^0.8.34;
contract CEI {
 function pureOpcodes() external view { assembly { let g := gas() let c := clz(g) } }
 function terminal() external pure { assembly { revert(128, 32) } }
}`}},settings:{evmVersion:'osaka',outputSelection:{'*':{'':['ast']}}}}
const solc=spawnSync(process.env.MORPHO_SOLC_0_8_34??resolve(root,'.cache/solc-0.8.34+commit.80d5c536'),['--standard-json'],{input:JSON.stringify(input),encoding:'utf8',timeout:30000})
assert.equal(solc.status,0,solc.stderr)
const output=JSON.parse(solc.stdout)
assert.deepEqual((output.errors??[]).filter(e=>e.severity==='error'),[])
let counter=0
const bodies=[]
walk(output.sources['CEI.sol'].ast,n=>{
 if(n.nodeType!=='InlineAssembly')return
 const c={stmts:[],fresh(h){return h+'_'+counter++},emit(s){this.stmts.push(s)},
 bind(e,h){const name=this.fresh(h);this.emit(`.letVar ${JSON.stringify(name)} ${e}`);return `(.localVar ${JSON.stringify(name)})`},fail(m){throw Error(m)}}
 lowerAssembly(n,c);bodies.push(c.stmts)
})
assert.equal(bodies.length,2)
const [pure,revert]=bodies.map(xs=>xs.join(', '))
assert.match(pure,/\.intrinsic "gas" \(\.builtin "gas"\) \.cancun/)
assert.match(pure,/\.intrinsic "clz" \(\.verbatim 1 1 "1e"\) \.osaka/)
assert.doesNotMatch(pure+revert,/\.ecm/)
assert.match(revert,/rawRevert \(Compiler.Yul.YulExpr.ident "revert_offset_/)
assert.match(revert,/proofStatus := \.assumed/)
const gasOnly=bodies[0].slice(0,2).join(', ')
const lean=`import Compiler.CompilationModel
open Compiler.CompilationModel
${supportDefinitions()}
def builtinCEI : CompilationModel := { name := "BuiltinCEI", fields := [{ name := "value", ty := .uint256 }], constructor := none, functions := [{ name := "probe", params := [], returnType := none, body := [${gasOnly}, .ite (.literal 0) [${revert}] [], .setStorage "value" (.literal 1)] }] }
#eval match compile builtinCEI [1] .osaka with
 | .ok _ => "BUILTIN_CEI_NATIVE_COMPILE_OK"
 | .error err => "BUILTIN_CEI_NATIVE_COMPILE_ERROR: " ++ err
-- Keep CLZ minimum fork honest, and expose pinned builtin-table limitations.
def builtinCLZ : CompilationModel := { builtinCEI with functions := [{ name := "probe", params := [], returnType := none, body := [${pure}, .setStorage "value" (.literal 1)] }] }
#eval match compile builtinCLZ [1] .osaka with
 | .ok _ => "CLZ_NATIVE_COMPILE_OK"
 | .error err => "CLZ_NATIVE_COMPILE_BLOCKED: " ++ err
#eval match compile builtinCLZ [1] .cancun with
 | .error err => "BUILTIN_CANCUN_REJECTED: " ++ err
 | .ok _ => "PINNED_BACKEND_IGNORES_VERBATIM_MIN_FORK"
-- The old gas ECM followed by a write is a real negative CEI control.
def oldGasCEI : CompilationModel := { builtinCEI with functions := [{
 name := "probe", params := [], returnType := none,
 body := [.ecm (midnightOpcode_gas "old_gas") [], .setStorage "value" (.literal 1)] }] }
#eval match compile oldGasCEI [1] .osaka with
 | .error err => "OLD_GAS_ECM_REJECTED: " ++ err
 | .ok _ => "UNEXPECTED_OLD_GAS_SUCCESS"
`
const dir=mkdtempSync(resolve(tmpdir(),'midnight-builtin-cei-'))
try {
 const path=resolve(dir,'BuiltinCEI.lean');writeFileSync(path,lean)
 const r=spawnSync('lake',['env','lean',path],{cwd:root,encoding:'utf8',timeout:120000,maxBuffer:10000000})
 assert.equal(r.status,0,`${r.error??''}\n${r.stdout}\n${r.stderr}\n${lean}`)
 assert.doesNotMatch(r.stdout+r.stderr,/PANIC|UNEXPECTED_|NATIVE_COMPILE_ERROR/)
 assert.match(r.stdout,/BUILTIN_CEI_NATIVE_COMPILE_OK/)
 // Pinned ExpressionCompile deliberately ignores _minFork for verbatim.
 // Record this gap, never claim a lower-fork rejection that did not happen.
 assert.match(r.stdout,/PINNED_BACKEND_IGNORES_VERBATIM_MIN_FORK/)
 assert.match(r.stdout,/CLZ_NATIVE_COMPILE_OK/)
 assert.match(r.stdout,/OLD_GAS_ECM_REJECTED:.*(?:CEI|[Ii]nteraction)/)
 console.log(r.stdout.trim())
 console.log('PASS: tiny actual native CM compile; gas and terminal raw revert before storage write; old-ECM negative control. CLZ result reported separately.')
} finally {rmSync(dir,{recursive:true,force:true})}
