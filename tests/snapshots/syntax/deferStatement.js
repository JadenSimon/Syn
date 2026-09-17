// @filename: main.syn
var __runDefers = Object.assign(d => {
  while (d.length) d.pop()();
}, {
  [Symbol.for('toComputation')]: () => ['d => {\n  while (d.length) d.pop()();\n}', []]
})
function assert(a, b) {
  if (a !== b) {
    throw new Error(`${a} !== ${b}`)
  }
}
assert[Symbol.for('toComputation')] = () => ['function assert(a, b) {\n  if (a !== b) {\n    throw new Error(`${a} !== ${b}`)\n  }\n}', []];
let c = 0
const c__c = [c]
{
  const __defers0 = []
  c__c[0] += 1;
  __defers0.push(Object.assign(() => c__c[0] -= 1, {
    [Symbol.for('toComputation')]: () => ['() => $0[0] -= 1', [c__c]]
  }));
  assert(c__c[0], 1);
  __runDefers(__defers0);
}
assert(c__c[0], 0);
function f1() {
  const __defers1 = []
  __defers1.push(Object.assign(() => c__c[0] -= 1, {
    [Symbol.for('toComputation')]: () => ['() => $0[0] -= 1', [c__c]]
  }));
  c__c[0] += 1;
  assert(c__c[0], 1);
  __runDefers(__defers1);
}
f1[Symbol.for('toComputation')] = () => ['function f1() {\n  const __defers1 = []\n  __defers1.push(() => $0[0] -= 1);\n  $0[0] += 1;\n  $1($0[0], 1);\n  $2(__defers1);\n}', [c__c, assert, __runDefers]];
f1();
f1();
assert(c__c[0], 0);
function f2() {
  const __defers2 = []
  __defers2.push(Object.assign(() => c__c[0] -= 1, {
    [Symbol.for('toComputation')]: () => ['() => $0[0] -= 1', [c__c]]
  }));
  c__c[0] += 1;
  assert(c__c[0], 1);
  c__c[0] += 1;
  __defers2.push(Object.assign(() => {
    assert(c__c[0], 2);
    c__c[0] -= 1;
  }, {
    [Symbol.for('toComputation')]: () => ['() => {\n  $0($1[0], 2);\n  $1[0] -= 1;\n}', [assert, c__c]]
  }));
  __runDefers(__defers2);
}
f2[Symbol.for('toComputation')] = () => ['function f2() {\n  const __defers2 = []\n  __defers2.push(() => $0[0] -= 1);\n  $0[0] += 1;\n  $1($0[0], 1);\n  $0[0] += 1;\n  __defers2.push(() => {\n    $1($0[0], 2);\n    $0[0] -= 1;\n  });\n  $2(__defers2);\n}', [c__c, assert, __runDefers]];
f2();
assert(c__c[0], 0);
let shouldDecrement = false
const c__shouldDecrement = [shouldDecrement]
function f3() {
  const __defers3 = []
  c__c[0] += 1;
  __defers3.push(Object.assign(() => {
    if (c__shouldDecrement[0]) c__c[0] -= 1;
  }, {
    [Symbol.for('toComputation')]: () => ['() => {\n  if ($0[0]) $1[0] -= 1;\n}', [c__shouldDecrement, c__c]]
  }));
  __runDefers(__defers3);
}
f3[Symbol.for('toComputation')] = () => ['function f3() {\n  const __defers3 = []\n  $0[0] += 1;\n  __defers3.push(() => {\n    if ($1[0]) $0[0] -= 1;\n  });\n  $2(__defers3);\n}', [c__c, c__shouldDecrement, __runDefers]];
f3();
assert(c__c[0], 1);
c__shouldDecrement[0] = true;
c__c[0] -= 1;
f3();
assert(c__c[0], 0);
function maybe(ret = false) {
  if (!ret) return 
  return {
    v: 1
  }
}
maybe[Symbol.for('toComputation')] = () => ['function maybe(ret = false) {\n  if (!ret) return \n  return {\n    v: 1\n  }\n}', []];
{
  const __defers4 = []
  c__c[0] += 1;
  __defers4.push(Object.assign(() => {
    const _tmp_0 = maybe(true)
    if (_tmp_0 != null) {
      const { v } = _tmp_0
      c__c[0] -= v;
    }
  }, {
    [Symbol.for('toComputation')]: () => ['() => {\n  const _tmp_0 = $0(true)\n  if (_tmp_0 != null) {\n    const { v } = _tmp_0\n    $1[0] -= v;\n  }\n}', [maybe, c__c]]
  }));
  assert(c__c[0], 1);
  __runDefers(__defers4);
}
assert(c__c[0], 0);
let c2 = 0
try {
  q();
  function q2() {
    c__c[0] += 1;
  }
  q2[Symbol.for('toComputation')] = () => ['function q2() {\n  $0[0] += 1;\n}', [c__c]];
  function q() {
    c__c[0] += 1;
  }
  q[Symbol.for('toComputation')] = () => ['function q() {\n  $0[0] += 1;\n}', [c__c]];
  const __fdefers5 = []
  try {
    __fdefers5.push(Object.assign(() => {
      assert(c__c[0], 1);
      c__c[0] -= 1;
    }, {
      [Symbol.for('toComputation')]: () => ['() => {\n  $0($1[0], 1);\n  $1[0] -= 1;\n}', [assert, c__c]]
    }));
    q2();
    __fdefers5.push(Object.assign(() => {
      assert(c__c[0], 2);
      c__c[0] -= 1;
    }, {
      [Symbol.for('toComputation')]: () => ['() => {\n  $0($1[0], 2);\n  $1[0] -= 1;\n}', [assert, c__c]]
    }));
    c2 += 1;
    assert(c__c[0], -1);
  } finally {
    __runDefers(__fdefers5);
  }
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
} finally {
  assert(c__c[0], 0);
  assert(c2, 1);
}
