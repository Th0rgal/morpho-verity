#!/usr/bin/env node
/** Execute actual generated native CompilationModel statements with byte memory.
 * External outcomes are supplied adversarial inputs, not executed callee code.
 * Tracks copy bytes/memory high water, NOT EVM gas or Compiler.compile coverage.
 */
import assert from 'node:assert/strict'
import {spawnSync} from 'node:child_process'
import {readFileSync} from 'node:fs'
import {resolve} from 'node:path'
import {MidnightSource,walk} from './lib/midnight-source.mjs'
import {lowerCall} from './lib/midnight-builtins.mjs'
const MASK=(1n<<256n)-1n
const L=n=>`(.literal ${n})`
function parse(text) {
  const ts = text.match(/"(?:[^"\\]|\\.)*"|[()\[\],]|[^\s()\[\],]+/g) ?? []; let i = 0
  function one() {
    const t = ts[i++]
    if (t === '(') { const x = []; while (ts[i] !== ')') x.push(one()); i++; return x }
    if (t === '[') { const x = []; while (ts[i] !== ']') { if (ts[i] === ',') i++; else x.push(stmt()) } i++; return x }
    if (t.startsWith('"')) return JSON.parse(t)
    return /^\d+$/.test(t) ? BigInt(t) : t
  }
  function stmt() { const x = [one()]; while (i < ts.length && ![',', ']'].includes(ts[i])) x.push(one()); return x }
  return one()
}
class VM {
  constructor(data = Buffer.alloc(0)) { this.data = data; this.mem = Buffer.alloc(1 << 20, 0xa5); this.vars = new Map(); this.store(64n, 4096n) }
  word(buf, p) { let n = 0n; for (let i = 0; i < 32; i++) n = (n << 8n) | BigInt(buf[Number(p) + i] ?? 0); return n }
  store(p, n) { assert(p >= 0 && p + 32n <= BigInt(this.mem.length), 'test memory capacity'); this.mem.set(Buffer.from((n & MASK).toString(16).padStart(64, '0'), 'hex'), Number(p)) }
  ev(x) {
    if (typeof x === 'bigint') return x
    if (Array.isArray(x) && x.length === 1 && Array.isArray(x[0])) return this.ev(x[0])
    const [op, ...args] = x
    if (op === '.literal') return args[0]
    if (op === '.localVar') { assert(this.vars.has(args[0]), `unbound ${args[0]}`); return this.vars.get(args[0]) }
    if (op === '.calldatasize') return BigInt(this.data.length)
    const [a, b] = args.map(y => this.ev(y))
    switch (op) {
      case '.mload': return this.word(this.mem, a)
      case '.calldataload': return this.word(this.data, a)
      case '.add': return (a + b) & MASK
      case '.sub': return (a - b) & MASK
      case '.mul': return (a * b) & MASK
      case '.div': return b ? a / b : 0n
      case '.mod': return b ? a % b : 0n
      case '.ceilDiv': return a === 0n ? 0n : (a - 1n) / b + 1n
      case '.min': return a < b ? a : b
      case '.bitAnd': return a & b
      case '.bitOr': return a | b
      case '.bitNot': return a ^ MASK
      case '.shl': return a >= 256n ? 0n : (b << a) & MASK
      case '.shr': return a >= 256n ? 0n : b >> a
      case '.byte': return a >= 32n ? 0n : (b >> ((31n - a) * 8n)) & 255n
      case '.signextend': { const bits = (a + 1n) * 8n; if (bits >= 256n) return b; const m = (1n << bits) - 1n; return b & (1n << (bits - 1n)) ? b | (MASK ^ m) : b & m }
      case '.logicalNot': return a ? 0n : 1n
      case '.eq': return BigInt(a === b)
      case '.lt': return BigInt(a < b)
      case '.le': return BigInt(a <= b)
      case '.ge': return BigInt(a >= b)
      default: throw Error(`unknown expression ${op}`)
    }
  }
  run(stmts) {
    for (const [op, ...args] of stmts) {
      if (op === '.letVar') this.vars.set(args[0], this.ev(args[1]))
      else if (op === '.require') assert(this.ev(args[0]) !== 0n, 'ABI guard')
      else if (op === '.requireError') {
        if (this.ev(args[0]) === 0n) throw Error(`${args[1]}(${args[2].map(a => this.ev(a)).join(',')})`)
      }
      else if (op === '.mstore') this.store(this.ev(args[0]), this.ev(args[1]))
      else if (op === '.calldatacopy') {
        const [d, s, n] = args.map(a => Number(this.ev(a)))
        for (let i = 0; i < n; i++) this.mem[d + i] = this.data[s + i] ?? 0
      } else if (op === '.forEach') {
        const n = this.ev(args[1]); assert(n < 10000n)
        for (let i = 0n; i < n; i++) { this.vars.set(args[0], i); this.run(args[2]) }
      } else throw Error(`unknown statement ${op}`)
    }
  }
  execute(c) { this.run(parse(`[${c.statements.join(',')}]`)) }
  result(v) { const p = this.ev(parse(v.ptr)), n = this.word(this.mem, p); return this.mem.subarray(Number(p + 32n), Number(p + 32n + n)) }
}
class CallVM extends VM {
  constructor(ret,success=true,code=true) {super();Object.assign(this,{ret,success,code,calls:[],copies:[],outputCopied:0,captures:0})}
  touch(p,n) {if(n) {assert(p>=0&&p+n<=this.mem.length,'test memory capacity');this.high=Math.max(this.high??0,p+n)}}
  store(p,n) {this.touch(Number(p),32);super.store(p,n)}
  ev(x) {
    if(Array.isArray(x)&&x.length===1&&Array.isArray(x[0]))return this.ev(x[0])
    const [op,...args]=Array.isArray(x)?x:[]
    if(op==='.intrinsic') {
      assert.deepEqual(args,['gas',['.builtin','gas'],'.cancun',[]]);return 1000000n // supplied gas, not EVM metering
    }
    if(op==='.returndataSize')return BigInt(this.ret.length)
    if(op==='.extcodesize')return this.code?1n:0n
    if(op==='.mload')this.touch(Number(this.ev(args[0])),32)
    if(op==='.gt')return BigInt(this.ev(args[0])>this.ev(args[1]))
    if(['.call','.delegatecall','.staticcall'].includes(op)) {
      const xs=args.map(a=>this.ev(a)),p=Number(xs.at(-2)),n=Number(xs.at(-1))
      this.calls.push({op,outputSize:n});this.touch(p,n)
      const copied=Math.min(n,this.ret.length);this.mem.set(this.ret.subarray(0,copied),p);this.outputCopied+=copied
      return this.success?1n:0n
    }
    return super.ev(x)
  }
  run(stmts) {
    for(const s of stmts) {
      const [op,...args]=s
      if(op==='.ite')this.run(this.ev(args[0])?args[1]:args[2])
      else if(op==='.returndataCopy') {
        const [p,off,n]=args.map(a=>Number(this.ev(a)))
        assert(off+n<=this.ret.length,'returndatacopy bounds');this.touch(p,n)
        this.copies.push(n);this.mem.set(this.ret.subarray(off,off+n),p)
      } else if(op==='.unsafeYul') {
        const [kind,offset,size]=args[0]
        assert.equal(kind,'Compiler.CompilationModel.UnsafeYulFragment.rawRevert')
        const [p,n]=[offset,size].map(([op,v])=>{
          if(op==='.lit')return Number(v)
          assert.equal(op,'Compiler.Yul.YulExpr.ident');assert(this.vars.has(v));return Number(this.vars.get(v))
        })
        this.reverted=Buffer.from(this.mem.subarray(p,p+n));throw Error('REVERT')
      } else super.run([s])
    }
  }
}
const root=resolve(import.meta.dirname,'..')
// Real pinned interface plus a separate typed Solidity regression fixture.
// No changes to pinned Solidity and no manually invented return AST/types.
const input={language:'Solidity',sources:{
  'IOracle.sol':{content:readFileSync(resolve(root,'morpho-midnight/src/interfaces/IOracle.sol'),'utf8')},
  'Fixture.sol':{content:`pragma solidity ^0.8.34;
import './IOracle.sol';
interface Shapes {
 function empty() external;
 function widths() external view returns(uint8, int8, bool, address, bytes4);
 function dynamicResult() external view returns(bytes memory);
 struct Pair { uint8 a; bool b; }
 function aggregate() external view returns(Pair memory, uint8[2] memory);
}
contract Fixture {
 function price(IOracle oracle) external view returns(uint256) { return oracle.price(); }
 function empty(Shapes s) external { s.empty(); }
 function widths(Shapes s) external view returns(uint8,int8,bool,address,bytes4) {return s.widths();}
 function aggregate(Shapes s) external view returns(Shapes.Pair memory,uint8[2] memory) {return s.aggregate();}
 function dynamicResult(Shapes s) external view returns(bytes memory) {return s.dynamicResult();}
 function low(address a) external returns(bool,bytes memory) {return a.call(hex'');}
 function delegated(address a) external returns(bool,bytes memory) {return a.delegatecall(hex'');}
 function decoded(bytes memory b) external pure returns(bool) {return abi.decode(b,(bool));}
}`}},settings:{evmVersion:'osaka',outputSelection:{'*':{'':['ast'],'*':['abi','storageLayout','evm.methodIdentifiers']}}}}
const r=spawnSync(process.env.MORPHO_SOLC_0_8_34??'/home/claudine/solidity_to_verity/tools/bin/solc-0.8.34-linux-amd64',['--standard-json'],{input:JSON.stringify(input),encoding:'utf8',maxBuffer:10000000})
assert.equal(r.status,0,r.stderr);const output=JSON.parse(r.stdout)
assert.deepEqual((output.errors??[]).filter(e=>e.severity==='error'),[])
const source=new MidnightSource({input,output},{entrySource:'Fixture.sol',contract:'Fixture'})
const nodes=new Map()
walk(output.sources['Fixture.sol'].ast,n=>{if(n.nodeType==='FunctionCall'&&n.expression.nodeType==='MemberAccess')nodes.set(n.expression.memberName,n)})
function generated(name) {
 const c={source,statements:[],counter:0,captureCount:0,emit(s){this.statements.push(s)},fresh(h){return h+'_'+this.counter++},
 bind(e,h){const n=this.fresh(h);this.emit(`.letVar ${JSON.stringify(n)} ${e}`);return `(.localVar ${JSON.stringify(n)})`},
 abiRequire(e){this.emit(`.ite ${e} [] [.unsafeYul (Compiler.CompilationModel.UnsafeYulFragment.rawRevert (.lit 0) (.lit 0) { name := "test_abi_failure", obligation := "Assumed empty-payload terminal revert", proofStatus := .assumed })]`)},fail(m){throw Error(m)},
 capture(fn){this.captureCount++;const old=this.statements;this.statements=[];try{fn();return {body:this.statements}}finally{this.statements=old}},
 eval(n){if(n.nodeType==='Literal')return {type:{kind:'bytes',abi:'bytes'},place:'literal',hex:n.hexValue};
 const type=source.type(source.resolve(n));return type.kind==='scalar'?{type,place:'scalar',expr:L(123)}:{type,place:'memory',ptr:L(128)}}}
 const value=lowerCall(nodes.get(name),c);return {c,value}
}
const word=n=>Buffer.from((BigInt(n)&MASK).toString(16).padStart(64,'0'),'hex')
function execute(g,ret,success=true,code=true) {const vm=new CallVM(ret,success,code);vm.execute(g.c);return vm}
function rejects(g,ret,success=true,code=true) {const vm=new CallVM(ret,success,code);assert.throws(()=>vm.execute(g.c),/REVERT/);return vm}
const price=generated('price'),huge=Buffer.concat([word(42),Buffer.alloc(262144,0xab)])
assert(price.c.captureCount>0,'builtin capture must honor ctx.capture')
const normal=execute(price,word(42)),large=execute(price,huge)
assert.equal(large.ev(parse(price.value.expr)),42n)
assert.deepEqual(large.calls,[{op:'.staticcall',outputSize:32}]);assert.deepEqual(large.copies,[])
assert.equal(large.outputCopied,32);assert.equal(large.high,normal.high,'success memory independent of trailing returndata')
for(const n of [0,1,31])assert.equal(rejects(price,Buffer.alloc(n)).reverted.length,0)
assert.equal(execute(price,Buffer.concat([word(42),Buffer.from([1])])).ev(parse(price.value.expr)),42n)
for(const ret of [Buffer.alloc(0),Buffer.from('deadbeef','hex'),huge]) {
 const vm=rejects(price,ret,false);assert.deepEqual(vm.reverted,ret);assert.deepEqual(vm.copies,[ret.length])
}
const empty=generated('empty'),voidVM=execute(empty,huge)
assert.deepEqual(voidVM.calls,[{op:'.call',outputSize:0}]);assert.deepEqual(voidVM.copies,[]);assert.equal(voidVM.outputCopied,0)
assert.equal(voidVM.high,execute(empty,Buffer.alloc(0)).high)
assert.equal(rejects(empty,huge,true,false).calls.length,0,'void rejects EOA before calling')
assert.deepEqual(rejects(empty,huge,false).reverted,huge)
const widths=generated('widths'),valid=[255n,MASK,1n,123n,0x12345678n<<224n]
const encoded=xs=>Buffer.concat(xs.map(word))
const wvm=execute(widths,encoded(valid));assert.deepEqual(widths.value.values.map(v=>wvm.ev(parse(v.expr))),valid)
assert.equal(wvm.calls[0].outputSize,160)
for(const [i,bad] of [[0,256n],[1,255n],[2,2n],[3,1n<<160n],[4,(0x12345678n<<224n)|1n]]) {
 const xs=[...valid];xs[i]=bad;assert.equal(rejects(widths,encoded(xs)).reverted.length,0)
}
rejects(widths,encoded(valid).subarray(0,159))
const aggregate=generated('aggregate');assert.equal(execute(aggregate,encoded([1,1,2,3])).calls[0].outputSize,128)
rejects(aggregate,encoded([1,2,2,3]));rejects(aggregate,encoded([1,1,2,256]))
assert.throws(()=>generated('dynamicResult'),/dynamic high-level external returns unsupported/)
for(const name of ['call','delegatecall'])for(const success of [true,false]) {
 const g=generated(name),vm=execute(g,huge,success)
 assert.deepEqual(vm.result(g.value.values[1]),huge);assert.deepEqual(vm.copies,[huge.length])
 assert.equal(vm.ev(parse(g.value.values[0].expr)),success?1n:0n)
}
// Source abi.decode retains eager validation, independently of high-level calls.
const decoded=generated('decode')
for(const [n,valid] of [[0,true],[1,true],[2,false]]) {
 const vm=new CallVM(Buffer.alloc(0));vm.store(128n,32n);vm.mem.set(word(n),160)
 if(valid){vm.execute(decoded.c);assert.equal(vm.ev(parse(decoded.value.expr)),BigInt(n))}
 else assert.throws(()=>vm.execute(decoded.c),/REVERT/)
}
console.log(JSON.stringify({status:'PASS',nativeStmtVM:true,scalarOutputBytes:large.outputCopied,largeReturndataBytes:huge.length,successReturndataCopies:large.copies.length,successMemoryHighWater:large.high,gasMeasured:false,compilerCompile:false}))
