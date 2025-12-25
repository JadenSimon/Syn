// TODO: tsc prefers to emit the initializer directly for simple types
// e.g. `export declare const x = 1, y = 2;`
export const x = 1, y = 2


// binding patterns

let { _z } = { _z: x + y }
export const z = _z

const [a,,_c] = ['a', 'b', 'c']
export const c = _c

const [{ nested: { _n } }] = [{ nested: { _n: true } }]
export const n = _n

const { 1: _one } = [0,1]
export const one = _one