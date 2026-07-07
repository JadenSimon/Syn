// @filename: main.ts

export function f() {
  const x = 'a'
  if (typeof x === 'string') {
    return x
  }
  throw new Error('not a string')
}
export function f2() {
  const x = 'a'
  const x2 = 1
}
