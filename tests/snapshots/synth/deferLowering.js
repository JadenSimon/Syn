// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
var __runDefers = d => {
  while (d.length) d.pop()();
}
const log = []
function lifo() {
  log.push('lifo');
  {
    const __dret1 = 1
    log.push('b');
    log.push('a');
    return __dret1
  }
}
function beforeDefer(early) {
  if (early) return 0
  {
    const __dret2 = 2
    log.push('late');
    return __dret2
  }
}
function returnValueFirst() {
  let x = 3
  {
    const __dret3 = x
    x = 100;
    return __dret3
  }
}
function nested(flag) {
  {
    if (flag) {
      const __dret4 = 4
      log.push('inner');
      log.push('outer');
      return __dret4
    }
    log.push('inner-end');
    log.push('inner');
  }
  {
    const __dret5 = 5
    log.push('outer');
    return __dret5
  }
}
function shadowed() {
  let __dret7
  const __defers6 = []
  const name = 'outer'
  __defers6.push(Object.assign(() => log.push(name), {
    [Symbol.toComputation]: () => ['() => $0.push($1)', [log, name]]
  }));
  {
    const name = 'inner'
    log.push(name);
    return (__dret7 = 6 , __runDefers(__defers6) , __dret7)
  }
  __runDefers(__defers6);
}
function blockPayload() {
  {
    const __dret8 = 8
    {
      log.push('block-1');
      log.push('block-2');
    }
    return __dret8
  }
}
function fallThrough() {
  log.push('body');
  log.push('fell');
}
function loop() {
  for (let i = 0; i < 2; i++) {
    log.push(`body${i}`);
    log.push(`iter${i}`);
  }
}
function innerFunction() {
  const f = () => {
    return 9
  }
  {
    const __dret9 = f()
    log.push('outer-fn');
    return __dret9
  }
}
const results = [lifo(), beforeDefer(true), beforeDefer(false), returnValueFirst(), nested(true), nested(false), shadowed(), blockPayload(), innerFunction()]
fallThrough();
loop();
function equal(actual, expected) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`)
  }
}
equal(results, [1, 0, 2, 3, 4, 5, 6, 8, 9]);
equal(log, ['lifo', 'b', 'a', 'late', 'inner', 'outer', 'inner-end', 'inner', 'outer', 'inner', 'outer', 'block-1', 'block-2', 'outer-fn', 'body', 'fell', 'body0', 'iter0', 'body1', 'iter1']);
log.length = 0;
function temporaryNames() {
  const __dret = 17
  const __dret0 = 18
  {
    const __dret10 = __dret + __dret0
    log.push(`${__dret}:${__dret0}`);
    return __dret10
  }
}
equal(temporaryNames(), 35);
equal(log, ['17:18']);
log.length = 0;
function conditional(flag) {
  let __dret12
  const __defers11 = []
  __defers11.push(Object.assign(() => log.push('always'), {
    [Symbol.toComputation]: () => ['() => $0.push(\'always\')', [log]]
  }));
  if (flag) __defers11.push(Object.assign(() => log.push('conditional'), {
    [Symbol.toComputation]: () => ['() => $0.push(\'conditional\')', [log]]
  }));
  return (__dret12 = 19 , __runDefers(__defers11) , __dret12)
}
equal(conditional(false), 19);
equal(conditional(true), 19);
equal(log, ['always', 'conditional', 'always']);
log.length = 0;
function nestedConditional(flag) {
  let __dret14
  const __defers13 = []
  __defers13.push(Object.assign(() => log.push('outer'), {
    [Symbol.toComputation]: () => ['() => $0.push(\'outer\')', [log]]
  }));
  {
    if (flag) __defers13.push(Object.assign(() => log.push('conditional'), {
      [Symbol.toComputation]: () => ['() => $0.push(\'conditional\')', [log]]
    }));
  }
  return (__dret14 = 20 , __runDefers(__defers13) , __dret14)
}
equal(nestedConditional(false), 20);
equal(nestedConditional(true), 20);
equal(log, ['outer', 'conditional', 'outer']);
log.length = 0;
function deferredReturn() {
  let __dret16
  const __defers15 = []
  __defers15.push(Object.assign(() => log.push('outer'), {
    [Symbol.toComputation]: () => ['() => $0.push(\'outer\')', [log]]
  }));
  __defers15.push(Object.assign(() => {
    log.push('inner');
    return 
  }, {
    [Symbol.toComputation]: () => ['() => {\n  $0.push(\'inner\');\n  return \n}', [log]]
  }));
  return (__dret16 = 21 , __runDefers(__defers15) , __dret16)
}
equal(deferredReturn(), 21);
equal(log, ['inner', 'outer']);
log.length = 0;
function deferredVar() {
  let __dret18
  const __defers17 = []
  var value = 22
  __defers17.push(Object.assign(() => {
    var value = 100
    log.push(`${value}`);
  }, {
    [Symbol.toComputation]: () => ['() => {\n  var value = 100\n  $0.push(`${value}`);\n}', [log]]
  }));
  return (__dret18 = value , __runDefers(__defers17) , __dret18)
}
equal(deferredVar(), 22);
equal(log, ['100']);
log.length = 0;
function deferredFunction() {
  {
    const __dret19 = 23
    {
      const f = () => {
        return 'nested-function'
      }
      log.push(f());
    }
    return __dret19
  }
}
equal(deferredFunction(), 23);
equal(log, ['nested-function']);
log.length = 0;
function shadowedCatch() {
  let __dret21
  const __defers20 = []
  const name = 'outer'
  __defers20.push(Object.assign(() => log.push(name), {
    [Symbol.toComputation]: () => ['() => $0.push($1)', [log, name]]
  }));
  try {
    throw new Error('inner')
  } catch(name) {
    log.push(name.message);
    return (__dret21 = 24 , __runDefers(__defers20) , __dret21)
  }
  __runDefers(__defers20);
}
equal(shadowedCatch(), 24);
equal(log, ['inner', 'outer']);
log.length = 0;
function mixedFinally() {
  const __defers22 = []
  const __fdefers24 = []
  try {
    __fdefers24.push(Object.assign(() => {
      log.push('finally');
    }, {
      [Symbol.toComputation]: () => ['() => {\n  $0.push(\'finally\');\n}', [log]]
    }));
    __defers22.push(Object.assign(() => log.push('plain'), {
      [Symbol.toComputation]: () => ['() => $0.push(\'plain\')', [log]]
    }));
    {
      {
        const __dret25 = 25
        log.push('inner');
        __runDefers(__defers22);
        return __dret25
      }
    }
    __runDefers(__defers22);
  } finally {
    __runDefers(__fdefers24);
  }
}
equal(mixedFinally(), 25);
equal(log, ['inner', 'plain', 'finally']);
log.length = 0;
function ordinaryThrow() {
  throw new Error('ordinary')
}
function finallyThrow() {
  const __fdefers26 = []
  try {
    __fdefers26.push(Object.assign(() => {
      log.push('finally');
    }, {
      [Symbol.toComputation]: () => ['() => {\n  $0.push(\'finally\');\n}', [log]]
    }));
    throw new Error('finally')
  } finally {
    __runDefers(__fdefers26);
  }
}
try {
  ordinaryThrow();
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
}
try {
  finallyThrow();
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
}
equal(log, ['finally']);
log.length = 0;
function shadowedEnum() {
  let __dret28
  const __defers27 = []
  const value = 26
  __defers27.push(Object.assign(() => log.push(`${value}`), {
    [Symbol.toComputation]: () => ['() => $0.push(`${$1}`)', [log, value]]
  }));
  {
    const value = {
      inner: 1
    }
    return (__dret28 = value.inner , __runDefers(__defers27) , __dret28)
  }
  __runDefers(__defers27);
}
equal(shadowedEnum(), 1);
equal(log, ['26']);
log.length = 0;
function overriddenReturn() {
  let __dret30
  const __defers29 = []
  __defers29.push(Object.assign(() => log.push('once'), {
    [Symbol.toComputation]: () => ['() => $0.push(\'once\')', [log]]
  }));
  try {
    return (__dret30 = 27 , __runDefers(__defers29) , __dret30)
  } finally {
    return (__dret30 = 28 , __runDefers(__defers29) , __dret30)
  }
  __runDefers(__defers29);
}
equal(overriddenReturn(), 28);
equal(log, ['once']);
log.length = 0;
function caughtCleanup() {
  let __dret32
  const __defers31 = []
  let attempts = 0
  const c__attempts = [attempts]
  __defers31.push(Object.assign(() => {
    log.push('cleanup');
    if (++c__attempts[0] === 1) throw new Error('cleanup')
  }, {
    [Symbol.toComputation]: () => ['() => {\n  $0.push(\'cleanup\');\n  if (++$1[0] === 1) throw new Error(\'cleanup\')\n}', [log, c__attempts], ['Error']]
  }));
  try {
    return (__dret32 = 29 , __runDefers(__defers31) , __dret32)
  } catch(e) {
    if (!(e instanceof Error)) e = new Error(e);
    log.push('caught');
    return (__dret32 = 30 , __runDefers(__defers31) , __dret32)
  }
  __runDefers(__defers31);
}
equal(caughtCleanup(), 30);
equal(log, ['cleanup', 'caught']);
log.length = 0;
function deferredEval() {
  let __dret34
  const __defers33 = []
  const label = 'outer'
  __defers33.push(Object.assign(() => eval("log.push(label)"), {
    [Symbol.toComputation]: () => ['() => eval("log.push(label)")', [], ['eval']]
  }));
  {
    const label = 'inner'
    return (__dret34 = 31 , __runDefers(__defers33) , __dret34)
  }
  __runDefers(__defers33);
}
equal(deferredEval(), 31);
equal(log, ['outer']);
log.length = 0;
function deferredDeclaration() {
  let __dret36
  const __defers35 = []
  var value = Object.assign(() => 'outer', {
    [Symbol.toComputation]: () => ['() => \'outer\'', []]
  })
  __defers35.push(Object.assign(() => {
    function value() {
      return 'inner'
    }
    log.push(value());
  }, {
    [Symbol.toComputation]: () => ['() => {\n  function value() {\n    return \'inner\'\n  }\n  $0.push(value());\n}', [log]]
  }));
  return (__dret36 = Object.assign(() => value(), {
    [Symbol.toComputation]: () => ['() => $0()', [value]]
  }) , __runDefers(__defers35) , __dret36)
}
equal(deferredDeclaration()(), 'outer');
equal(log, ['inner']);
