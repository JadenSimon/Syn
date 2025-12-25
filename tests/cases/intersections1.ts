type A = { prop: string }
type B = { prop: string | number }
export type C = (A & B)['prop']

export function f() {
    const x: C = 'a'
    if (typeof x === 'string') {
        return x
    }
    throw new Error('not a string')
}
