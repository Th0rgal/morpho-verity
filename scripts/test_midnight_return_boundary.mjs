#!/usr/bin/env node
// Run after generating/building Midnight.Generated.FullModel. This exercises
// the real pinned compiler, including a negative exhaustive-return-path gate.
import assert from 'node:assert/strict'
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'
const dir=mkdtempSync(join(tmpdir(),'midnight-return-boundary-'))
try {
  const file=join(dir,'Probe.lean')
  writeFileSync(file,`import Midnight.Generated.FullModel
open Compiler.CompilationModel
partial def eraseReturnBoundary : Stmt → Stmt
  | .unsafeYul fragment =>
      if fragment.controlFlow.mayReturn then
        .unsafeYul { fragment with termination := .mayTerminate, controlFlow := .unknown }
      else .unsafeYul fragment
  | .unsafeBlock reason body => .unsafeBlock reason (body.map eraseReturnBoundary)
  | .ite condition yes no => .ite condition (yes.map eraseReturnBoundary) (no.map eraseReturnBoundary)
  | .forEach index count body => .forEach index count (body.map eraseReturnBoundary)
  | other => other
#eval show Except String String from do
  let complete := Midnight.Generated.Full.spec
  let some target := complete.functions.find? (·.name == "toMarket")
    | throw "missing generated toMarket"
  let isolated := { complete with constructor := none, functions := [target] }
  let _ ← compile isolated [1] .osaka
  let invalid := { target with body := target.body.map eraseReturnBoundary }
  match compile { isolated with functions := [invalid] } [1] .osaka with
  | .ok _ => throw "missing return metadata was incorrectly accepted"
  | .error error =>
    if (error.splitOn "not all control-flow paths").length <= 1 then
      throw ("wrong negative failure: " ++ error)
  return "RETURN_BOUNDARY_TEST_OK"
`)
  const result=spawnSync('lake',['env','lean','-s','65536',file],{encoding:'utf8',timeout:120000,maxBuffer:1000000})
  assert.equal(result.status,0,`${result.error??''}\n${result.stdout}\n${result.stderr}`)
  assert(!/PANIC|Except.error/.test(result.stdout+result.stderr),result.stdout+result.stderr)
  assert.match(result.stdout,/RETURN_BOUNDARY_TEST_OK/)
  console.log('PASS: generated composite return compiles; erasing its terminal metadata is rejected by exhaustive return-path validation')
} finally { rmSync(dir,{recursive:true,force:true}) }
