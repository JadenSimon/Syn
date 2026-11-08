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
  narrowedModules: [[0, ['a', 'b']]]
}
const __meta$1 = {
  narrowedModules: [[0, ['c']]]
}
const __meta$2 = {
  narrowedModules: [[0, ['a', 'c']]]
}
const __meta$3 = {
  narrowedModules: [[0, ['d']]]
}
const __meta$4 = {
  ambient: ['Math'],
  narrowedModules: [[0, ['a']], [1, ['d']]]
}
const __meta$5 = {
  ambient: ['Object']
}
import * as m from './m'
import * as other from './other'
export function reads() {
  return m.a + m.b() + m.a
}
reads[Symbol.toComputation] = () => ['function reads() {\n  return $0.a + $0.b() + $0.a\n}', [m], __meta$0];
export function nested() {
  const x = m.a
  return __computation$(() => m.c + x, () => ['() => $0.c + $1', [m, x], __meta$1])
}
nested[Symbol.toComputation] = () => ['function nested() {\n  const x = $0.a\n  return () => $0.c + x\n}', [m], __meta$2];
export function nestedValue() {
  return [other.d, __computation$(() => m, () => ['() => $0', [m]])]
}
nestedValue[Symbol.toComputation] = () => ['function nestedValue() {\n  return [$0.d, () => $1]\n}', [other, m], __meta$3];
export function withAmbient() {
  return Math.max(m.a, other.d)
}
withAmbient[Symbol.toComputation] = () => ['function withAmbient() {\n  return Math.max($0.a, $1.d)\n}', [m, other], __meta$4];
export function passed() {
  return Object.keys(m)
}
passed[Symbol.toComputation] = () => ['function passed() {\n  return Object.keys($0)\n}', [m], __meta$5];
export function computed(key) {
  return m[key]
}
computed[Symbol.toComputation] = () => ['function computed(key) {\n  return $0[key]\n}', [m]];
export function typeofUse() {
  return typeof m.a + typeof m
}
typeofUse[Symbol.toComputation] = () => ['function typeofUse() {\n  return typeof $0.a + typeof $0\n}', [m]];
export function spread() {
  return {
    ...m
  }
}
spread[Symbol.toComputation] = () => ['function spread() {\n  return {\n    ...$0\n  }\n}', [m]];
export function destructured() {
  const { a } = m
  return a
}
destructured[Symbol.toComputation] = () => ['function destructured() {\n  const { a } = $0\n  return a\n}', [m]];

// @filename: m.syn
export const a = 1
export function b() {
  return 2
}
export const c = 3

// @filename: other.syn
export const d = 4
