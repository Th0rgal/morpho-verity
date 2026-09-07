import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { factorGeneratedLeanStatements } from './lib/midnight-lean-render.mjs';

function checkExpansion(source) {
  const { lean, definitions } = factorGeneratedLeanStatements(source);
  const terms = new Map();
  const expand = text => text.replace(/"(?:\\.|[^"\\])*"|\b[A-Za-z_][A-Za-z_0-9]*\b/g, token => terms.get(token) ?? token);
  const declarations = new Set();
  for (const { name, term } of definitions) {
    declarations.add(`def ${name} : Stmt := ${term}\n`);
    terms.set(name, expand(term));
  }
  const expanded = expand(lean.split(/(?<=\n)/).filter(line => !declarations.has(line)).join(''));
  assert.equal(expanded, source, 'transparent expansion must recover exact input');
  return definitions;
}
const source = 'import Compiler.CompilationModel\nopen Compiler.CompilationModel\n' +
  'def sourceFunction_0 : FunctionSpec := { name := "f [,.ite] \\\"x\\\"", returns := [.uint256], body := [.ite (.literal 1) [.letVar "x" (.literal 2), .returnValues [(.localVar "x")]] [], .stop] }\n';
const definitions = checkExpansion(source);
assert.equal(definitions.length, 4);
assert.ok(definitions.every(d => d.owner === 'sourceFunction_0'));
assert.equal(factorGeneratedLeanStatements(source, { threshold: 100000 }).lean, source);
assert.throws(() => factorGeneratedLeanStatements('def sourceFunction_0 := [\n.stop]\n'), /Multiline/);
assert.throws(() => factorGeneratedLeanStatements('def sourceFunction_0 := [ .stop }\n'), /Mismatched/);
assert.throws(() => factorGeneratedLeanStatements('def sourceFunction_0 := [.stop] -- comment\n'), /Comments/);
assert.throws(() => factorGeneratedLeanStatements(source, { prefix: 'not.valid' }), /prefix/);
assert.throws(() => factorGeneratedLeanStatements(source, { threshold: -1 }), /threshold/);
assert.throws(() => factorGeneratedLeanStatements('def renderStatement_0 := 1'), /already present/);
assert.equal(checkExpansion('def sourceStatement_0 : Stmt := .ite (.literal 1) [.stop] []\n').length, 1);
if (process.argv[2]) {
  const full = readFileSync(process.argv[2], 'utf8');
  const defs = checkExpansion(full);
  console.log(`Exact full-source expansion passed (${defs.length} transparent Stmt definitions)`);
}
console.log('midnight Lean rendering tests passed');
