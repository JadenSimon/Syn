// @filename: main.ts

export function f() {
  const x = 'a'
  if (typeof x === 'string') {
    return x
  }
  throw new Error('not a string')
}
