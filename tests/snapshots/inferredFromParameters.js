// @filename: foo.ts
export function foo() {
  return _foo1('hi')
}
export function foo2() {
  return _foo2('hi')
}
export function foo3() {
  return _foo1('hi')
}
export function foo4() {
  return _foo2('hi')
}
export function foo5() {
  return _foo2('hi')
}
export function foo6() {
  return _foo1('hi')
}
function _foo1(a) {
  return a
}
function _foo2(a) {
  return a
}
export function _foo3(a) {
  return a
}
function _mapped(a) {
  return a
}
export function mapped1() {
  return _mapped({
    hi: 'there'
  })
}
export function _mapped2(a) {
  return a
}
export function mapped2() {
  return _mapped2({
    hi: 'there'
  })
}
export function shaped(s) {
  return s.t
}
export const shaped_1 = shaped({
  t: true
})
export const shaped_2 = shaped({
  t: 1
})
function f2(a, b) {
  return a(b)
}
export const f3 = f2(wrap(s => s.length), 'a')
