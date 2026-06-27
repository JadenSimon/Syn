// @filename: main.syn

const a = []
export function f() {
  return a[0]
}
const a2 = []
export function f2() {
  return a2[0]
}
export function f3() {
  const v = f2()
  return v ? v[1] : undefined
}
export function f4() {
  return f2()?.[1]
}
