// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
function __computation$(f, c) {
  f[Symbol.toComputation] = c;
  return f
}
const __meta$0 = {
  ambient: ['Error']
}
var __runDefers = d => {
  while (d.length) d.pop()();
}
function assert(a, b) {
  if (a !== b) {
    throw new Error(`${a} !== ${b}`)
  }
}
assert[Symbol.toComputation] = () => ['function assert(a, b) {\n  if (a !== b) {\n    throw new Error(`${a} !== ${b}`)\n  }\n}', [], __meta$0];
let c = 0
const c__c = [c]
{
  c__c[0] += 1;
  assert(c__c[0], 1);
  c__c[0] -= 1;
}
assert(c__c[0], 0);
function f1() {
  c__c[0] += 1;
  assert(c__c[0], 1);
  c__c[0] -= 1;
}
f1();
f1();
assert(c__c[0], 0);
function f2() {
  c__c[0] += 1;
  assert(c__c[0], 1);
  c__c[0] += 1;
  {
    assert(c__c[0], 2);
    c__c[0] -= 1;
  }
  c__c[0] -= 1;
}
f2();
assert(c__c[0], 0);
let shouldDecrement = false
function f3() {
  c__c[0] += 1;
  {
    if (shouldDecrement) c__c[0] -= 1;
  }
}
f3();
assert(c__c[0], 1);
shouldDecrement = true;
c__c[0] -= 1;
f3();
assert(c__c[0], 0);
function maybe(ret = false) {
  if (!ret) return 
  return {
    v: 1
  }
}
{
  c__c[0] += 1;
  assert(c__c[0], 1);
  {
    const _tmp_0 = maybe(true)
    if (_tmp_0 != null) {
      const { v } = _tmp_0
      c__c[0] -= v;
    }
  }
}
assert(c__c[0], 0);
let c2 = 0
try {
  q();
  function q2() {
    c__c[0] += 1;
  }
  function q() {
    c__c[0] += 1;
  }
  const __fdefers0 = []
  try {
    __fdefers0.push(__computation$(() => {
      assert(c__c[0], 1);
      c__c[0] -= 1;
    }, () => ['() => {\n  $0($1[0], 1);\n  $1[0] -= 1;\n}', [assert, c__c]]));
    q2();
    __fdefers0.push(__computation$(() => {
      assert(c__c[0], 2);
      c__c[0] -= 1;
    }, () => ['() => {\n  $0($1[0], 2);\n  $1[0] -= 1;\n}', [assert, c__c]]));
    c2 += 1;
    assert(c__c[0], -1);
  } finally {
    __runDefers(__fdefers0);
  }
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
} finally {
  assert(c__c[0], 0);
  assert(c2, 1);
}
