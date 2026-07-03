// @filename: main.syn

export function getSpan() {
  const m = new Map()
  return m.get('aaa')
}
export function getAllSpans() {
  const m = new Map()
  return [...m.values()]
}
export function getSortedSpans() {
  return getAllSpans().sort((a, b) => a.start - b.start)
}
export function f1() {
  return new x('a')
}
export function f2() {
  return new x(true, {
    a: 1
  })
}
export function f3() {
  return new x2('a')
}
export function f4() {
  return new x3('a', 'b')
}
