// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
var __tmp$
const key = Symbol.for('toComputation')
function check(cond, message) {
  if (!cond) throw new Error(message)
}
function hasMeta(o, name) {
  const d = Object.getOwnPropertyDescriptor(o, name)
  const f = d.get ?? d.value
  return f[key] !== undefined
}
const top = (__tmp$ = {
  get a() {
    return 1
  }
} , Object.getOwnPropertyDescriptor(__tmp$, 'a').get[Symbol.toComputation] = () => ['function a() {\n  return 1\n}', []] , __tmp$)
check(top.a === 1 && hasMeta(top, 'a'), 'module literal');
function make(n) {
  var __tmp$
  return (__tmp$ = {
    get v() {
      return n
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'v').get[Symbol.toComputation] = () => ['function v() {\n  return $0\n}', [n]] , __tmp$)
}
const made = make(2)
check(made.v === 2 && hasMeta(made, 'v'), 'function literal');
check((globalThis).__tmp$ !== made, 'the module temporary holds a literal made inside a function');
function nested() {
  var __tmp$
  return (__tmp$ = {
    inner: (__tmp$ = {
      get i() {
        return 3
      }
    } , Object.getOwnPropertyDescriptor(__tmp$, 'i').get[Symbol.toComputation] = () => ['function i() {\n  return 3\n}', []] , __tmp$),
    get o() {
      return 4
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'o').get[Symbol.toComputation] = () => ['function o() {\n  return 4\n}', []] , __tmp$)
}
const n = nested()
check(n.o === 4 && n.inner.i === 3 && hasMeta(n, 'o') && hasMeta(n.inner, 'i'), 'nested literal');
function outerGetter() {
  var __tmp$
  return (__tmp$ = {
    get g() {
      var __tmp$
      return (__tmp$ = {
        get h() {
          return 5
        }
      } , Object.getOwnPropertyDescriptor(__tmp$, 'h').get[Symbol.toComputation] = () => ['function h() {\n  return 5\n}', []] , __tmp$)
    },
    m() {
      var __tmp$
      return (__tmp$ = {
        k() {
          return 6
        }
      } , __tmp$.k[Symbol.toComputation] = () => ['function k() {\n  return 6\n}', []] , __tmp$)
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'g').get[Symbol.toComputation] = () => ['function g() {\n  return {\n    get h() {\n      return 5\n    }\n  }\n}', []] , __tmp$.m[Symbol.toComputation] = () => ['function m() {\n  return {\n    k() {\n      return 6\n    }\n  }\n}', []] , __tmp$)
}
const og = outerGetter()
check(og.g.h === 5 && hasMeta(og, 'g') && hasMeta(og.g, 'h'), 'literal in a getter');
check(og.m().k() === 6 && hasMeta(og, 'm') && hasMeta(og.m(), 'k'), 'literal in a method');
const arrow = (x) => {
  var __tmp$
  return ((__tmp$ = {
    get a() {
      return x
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'a').get[Symbol.toComputation] = () => ['function a() {\n  return $0\n}', [x]] , __tmp$))
}
const ar = arrow(7)
check(ar.a === 7 && hasMeta(ar, 'a'), 'concise arrow');
const blockArrow = (x) => {
  var __tmp$
  const o = (__tmp$ = {
    get b() {
      return x
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'b').get[Symbol.toComputation] = () => ['function b() {\n  return $0\n}', [x]] , __tmp$)
  return o
}
const ba = blockArrow(8)
check(ba.b === 8 && hasMeta(ba, 'b'), 'block arrow');
function both() {
  var __tmp$
  const outer = (__tmp$ = {
    get p() {
      return 9
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'p').get[Symbol.toComputation] = () => ['function p() {\n  return 9\n}', []] , __tmp$)
  const inner = () => {
    var __tmp$
    return ((__tmp$ = {
      get q() {
        return 10
      }
    } , Object.getOwnPropertyDescriptor(__tmp$, 'q').get[Symbol.toComputation] = () => ['function q() {\n  return 10\n}', []] , __tmp$))
  }
  const q = inner()
  return {
    outer,
    q
  }
}
const b = both()
check(b.outer.p === 9 && b.q.q === 10 && hasMeta(b.outer, 'p') && hasMeta(b.q, 'q'), 'inner and outer scopes');
class Static {
  static made;
  static {
    var __tmp$
    Static.made = (__tmp$ = {
      get s() {
        return 11
      }
    } , Object.getOwnPropertyDescriptor(__tmp$, 's').get[Symbol.toComputation] = () => ['function s() {\n  return 11\n}', []] , __tmp$);
  }
  static {
    this[Symbol.toComputation] = () => ['class Static {\n  static made;\n  static {\n    Static.made = {\n      get s() {\n        return 11\n      }\n    };\n  }\n}', []];
  }
}
check(Static.made.s === 11 && hasMeta(Static.made, 's'), 'class static block');
class Field {
  f = (__tmp$ = {
    get r() {
      return 12
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'r').get[Symbol.toComputation] = () => ['function r() {\n  return 12\n}', []] , __tmp$);
  static {
    this[Symbol.toComputation] = () => ['class Field {\n  f = {\n    get r() {\n      return 12\n    }\n  };\n}', []];
  }
}
const fi = new Field()
check(fi.f.r === 12 && hasMeta(fi.f, 'r'), 'class field');
function withDefault(o = (__tmp$ = {
  get d() {
    return 13
  }
} , Object.getOwnPropertyDescriptor(__tmp$, 'd').get[Symbol.toComputation] = () => ['function d() {\n  return 13\n}', []] , __tmp$)) {
  return o
}
const wd = withDefault()
check(wd.d === 13 && hasMeta(wd, 'd'), 'parameter default');
function makeWithDefault() {
  var __tmp$
  function inner(o = (__tmp$ = {
    get e() {
      return 14
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'e').get[Symbol.toComputation] = () => ['function e() {\n  return 14\n}', []] , __tmp$)) {
    var __tmp$
    return (__tmp$ = {
      o,
      get w() {
        return 15
      }
    } , Object.getOwnPropertyDescriptor(__tmp$, 'w').get[Symbol.toComputation] = () => ['function w() {\n  return 15\n}', []] , __tmp$)
  }
  return inner()
}
const mwd = makeWithDefault()
check(mwd.o.e === 14 && mwd.w === 15 && hasMeta(mwd.o, 'e') && hasMeta(mwd, 'w'), 'parameter default in a function');
function* gen() {
  var __tmp$
  return (__tmp$ = {
    get y() {
      return 16
    }
  } , Object.getOwnPropertyDescriptor(__tmp$, 'y').get[Symbol.toComputation] = () => ['function y() {\n  return 16\n}', []] , __tmp$)
}
const gy = gen().next().value
check(gy.y === 16 && hasMeta(gy, 'y'), 'generator');
async function asy() {
  var __tmp$
  const o = (__tmp$ = {
    get u() {
      return 19
    },
    t: await 20
  } , Object.getOwnPropertyDescriptor(__tmp$, 'u').get[Symbol.toComputation] = () => ['function u() {\n  return 19\n}', []] , __tmp$)
  return o
}
