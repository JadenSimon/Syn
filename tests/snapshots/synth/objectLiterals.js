// @filename: main.syn
var __tmp$
let x = 1
const c__x = [x]
const s = Symbol()
const o = (__tmp$ = {
  foo() {
    return o
  },
  get x() {
    return c__x[0]
  },
  set x(v) {
    c__x[0] = v;
  }
} , __tmp$.foo[Symbol.for('toComputation')] = () => ['function foo() {\n  return $0\n}', [o]] , Object.getOwnPropertyDescriptor(__tmp$, 'x').get[Symbol.for('toComputation')] = () => ['function x() {\n  return $0[0]\n}', [c__x]] , Object.getOwnPropertyDescriptor(__tmp$, 'x').set[Symbol.for('toComputation')] = () => ['function x(v) {\n  $0[0] = v;\n}', [c__x]] , __tmp$)
