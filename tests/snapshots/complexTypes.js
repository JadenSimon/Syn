// @filename: main.ts

export function prop() {
  const x = (a) => typeof a === 'number' ? 1 : 0
  return x(2)
}
export function main(...args) {
  return args[0]
}
export const x = '1'
